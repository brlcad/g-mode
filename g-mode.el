;;; g-mode.el --- Major mode for BRL-CAD .g files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christopher Sean Morrison
;;
;; Author: Christopher Sean Morrison
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files, tools, bin
;; URL: https://github.com/brlcad/g-mode

;;; Commentary:

;; A major mode for inspecting, browsing, and editing BRL-CAD `.g` binary database files natively.
;; It requires Emacs 28.1+ to leverage the modern `bindat` API.
;; This mode implements its own file handling standalone, without relying on BRL-CAD libraries.

;;; Code:

(require 'cl-lib)
(require 'bindat)
(require 'tabulated-list)

(defgroup g-mode nil
  "Major mode for reading and editing BRL-CAD .g geometry database files."
  :group 'data)

(defconst g-mode-magic1 #x76 "First magic number byte for a database object.")
(defconst g-mode-magic2 #x35 "Last magic number byte for a database object.")

(defconst g-mode-db-header
  '((magic1     u8)
    (hflags     u8)
    (aflags     u8)
    (bflags     u8)
    (major-type u8)
    (minor-type u8)
    (length     u8)
    (magic2     u8))
  "Structure for the fixed 8-byte initial database header of a .g file.")

(defun g-mode--parse-header ()
  "Parse the 8-byte database header and return its alist structure.
Returns nil if magic numbers do not match or buffer is too small."
  (when (>= (- (point-max) (point-min)) 8)
    (let* ((bytes (string-as-unibyte (buffer-substring-no-properties (point-min) (+ (point-min) 8))))
           (header (bindat-unpack g-mode-db-header bytes)))
      (if (and (= (cdr (assq 'magic1 header)) g-mode-magic1)
               (= (cdr (assq 'magic2 header)) g-mode-magic2)
               (= (cdr (assq 'hflags header)) 1)) ;; DLI=1 for Db header
          header
        nil))))

(defconst g-mode-object-fixed-header
  '((magic1     u8)
    (hflags     u8)
    (aflags     u8)
    (bflags     u8)
    (major-type u8)
    (minor-type u8))
  "Fixed 6-byte prefix common to all generic database objects.")

(defun g-mode--decode-width (wid)
  "Convert width code WID (0..3) to bytes (1, 2, 4, 8)."
  (ash 1 wid))

(defun g-mode--read-uint (bytes)
  "Read big-endian unsigned integer from BYTES (unibyte string)."
  (let ((val 0))
    (dotimes (i (length bytes))
      (setq val (+ (ash val 8) (aref bytes i))))
    val))

(defun g-mode--parse-object (start-pos)
  "Parse an object starting at START-POS.
Returns an alist of metadata including 'length in bytes, and 'name if present."
  (save-excursion
    (goto-char start-pos)
    (when (= (char-after) g-mode-magic1)
      (let* ((hbuf (string-as-unibyte (buffer-substring-no-properties (point) (+ (point) 6))))
             (obj (bindat-unpack g-mode-object-fixed-header hbuf))
             (hflags (cdr (assq 'hflags obj)))
             (owid (ash (logand hflags #xC0) -6))
             (np (not (zerop (logand hflags #x20))))
             (nwid (ash (logand hflags #x18) -3))
             (olen-bytes (g-mode--decode-width owid)))
        (forward-char 6)
        
        ;; Read Object_Length
        (let* ((olen-buf (buffer-substring-no-properties (point) (+ (point) olen-bytes)))
               (olen-chunks (g-mode--read-uint olen-buf)))
          (forward-char olen-bytes)
          (setq obj (nconc obj `((length . ,(* olen-chunks 8)))))
          
          ;; Read Name if present
          (when np
            (let* ((nlen-bytes (g-mode--decode-width nwid))
                   (nlen-buf (buffer-substring-no-properties (point) (+ (point) nlen-bytes)))
                   (nlen (g-mode--read-uint nlen-buf)))
              (forward-char nlen-bytes)
              ;; Read name data. It includes a null byte, so we use (1- nlen)
              (let ((name-str (buffer-substring-no-properties (point) (+ (point) (1- nlen)))))
                (forward-char nlen)
                (setq obj (nconc obj `((name . ,name-str)))))))
          
          (setq obj (nconc obj `((pos . ,start-pos) (interior-pos . ,(point)))))
          obj)))))

(defun g-mode--scan-buffer ()
  "Scan the entire unibyte buffer for .g objects.
Returns a list of parsed object metadata alists."
  (let ((objects nil)
        (pos (+ (point-min) 8))) ;; Skip 8-byte db header
    (save-excursion
      (goto-char pos)
      (while (< (point) (point-max))
        (let ((obj (g-mode--parse-object (point))))
          (if obj
              (progn
                (push obj objects)
                (goto-char (+ (point) (cdr (assq 'length obj)))))
            (error "Failed to parse object at byte %d" (point))))))
    (nreverse objects)))

(defun g-mode--parse-attributes (obj)
  "Parse attributes out of OBJ at its `interior-pos` in the current buffer.
Returns an alist of (KEY . VALUE) strings, or nil."
  (save-excursion
    (goto-char (cdr (assq 'interior-pos obj)))
    (let* ((aflags (cdr (assq 'aflags obj)))
           (awid (ash (logand aflags #xC0) -6))
           (ap (not (zerop (logand aflags #x20))))
           (az (logand aflags #x07)))
      (when ap
        (let* ((alen-bytes (g-mode--decode-width awid))
               (alen-buf (buffer-substring-no-properties (point) (+ (point) alen-bytes)))
               (alen (g-mode--read-uint alen-buf)))
          (forward-char alen-bytes)
          (if (not (zerop az))
              '((compressed . "true")) ;; Unimplemented for now
            (let* ((attr-data (buffer-substring-no-properties (point) (+ (point) alen)))
                   (parts (split-string attr-data "\0" t))
                   (attrs nil))
              (while (>= (length parts) 2)
                (push (cons (pop parts) (pop parts)) attrs))
              (nreverse attrs))))))))

(defvar-local g-mode--binary-buffer nil
  "Reference to the hidden unibyte buffer containing the raw .g file data.")

(defvar-local g-mode--objects nil
  "List of parsed objects from the binary database.")

(defun g-mode--refresh-entries ()
  "Populate `tabulated-list-entries' from the binary buffer."
  (let ((objs (with-current-buffer g-mode--binary-buffer
                (g-mode--scan-buffer))))
    (setq g-mode--objects objs)
    (setq tabulated-list-entries
          (mapcar (lambda (obj)
                    (let* ((name (cdr (assq 'name obj)))
                           (major (cdr (assq 'major-type obj)))
                           (minor (cdr (assq 'minor-type obj)))
                           (len (cdr (assq 'length obj)))
                           (hflags (cdr (assq 'hflags obj)))
                           (type-str (format "%02X:%02X" major minor)))
                      (list obj ;; Use obj alist as the entry ID
                            (vector (or name "<unnamed>")
                                    type-str
                                    (number-to-string len)
                                    (format "%02X" hflags)))))
                  objs))))

(defun g-mode-view-object ()
  "Open a detailed view of the object at point."
  (interactive)
  (let ((obj (tabulated-list-get-id))
        (src-buf g-mode--binary-buffer))
    (unless obj
      (user-error "No object under point"))
    (let* ((name (cdr (assq 'name obj)))
           (buf-name (format "*g-mode: %s*" (or name "unnamed")))
           (attrs (with-current-buffer src-buf
                    (g-mode--parse-attributes obj))))
      (with-current-buffer (get-buffer-create buf-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Object: %s\n" (or name "<unnamed>")))
          (insert (format "Size:   %d bytes\n" (cdr (assq 'length obj))))
          (insert (format "Type:   %02X:%02X\n"
                          (cdr (assq 'major-type obj))
                          (cdr (assq 'minor-type obj))))
          (insert (format "HFlags: %02X\n" (cdr (assq 'hflags obj))))
          (insert (format "AFlags: %02X\n" (cdr (assq 'aflags obj))))
          (insert (format "BFlags: %02X\n" (cdr (assq 'bflags obj))))
          (insert "\n-- Attributes --\n")
          (if attrs
              (dolist (attr attrs)
                (insert (format "%s: %s\n" (car attr) (cdr attr))))
            (insert "None.\n"))
          (goto-char (point-min)))
        (special-mode)
        (display-buffer (current-buffer))))))

(defun g-mode--write-byte (pos byte)
  "Write a single BYTE at POS in the current buffer, overwriting 1 char."
  (save-excursion
    (goto-char pos)
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert byte))))

(defun g-mode-delete-object ()
  "Mark the object at point as Free Storage (DLI=2) natively in the .g file."
  (interactive)
  (let ((obj (tabulated-list-get-id))
        (bin-buf g-mode--binary-buffer))
    (unless obj
      (user-error "No object under point"))
    (let* ((pos (cdr (assq 'pos obj)))
           (hflags (cdr (assq 'hflags obj)))
           (owid (ash (logand hflags #xC0) -6))
           ;; New HFlags: keep OWid, set DLI=2
           (new-hflags (logior (ash owid 6) #x02))
           ;; New AFlags: AP=0, AWid=0
           (new-aflags #x00)
           ;; New BFlags: BP=1, BWid=0
           (new-bflags #x20))
      
      (when (= (logand hflags #x03) #x02)
        (user-error "Object is already marked as Free Space"))
      
      ;; Write directly into the unibyte binary buffer
      (with-current-buffer bin-buf
        (g-mode--write-byte (+ pos 1) new-hflags)
        (g-mode--write-byte (+ pos 2) new-aflags)
        (g-mode--write-byte (+ pos 3) new-bflags))
      
      (message "Marked object '%s' as Free Space." (or (cdr (assq 'name obj)) "unnamed"))
      ;; Refresh the view
      (g-mode--refresh-entries)
      (tabulated-list-print t))))

(defvar g-mode-ui-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") 'g-mode-view-object)
    (define-key map (kbd "RET") 'g-mode-view-object)
    (define-key map (kbd "d") 'g-mode-delete-object)
    map)
  "Keymap for `g-mode-ui-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . g-mode))

;;;###autoload
(add-to-list 'file-coding-system-alist '("\\.g\\'" . no-conversion))

(define-derived-mode g-mode-ui-mode tabulated-list-mode "g-mode-UI"
  "UI mode for browsing BRL-CAD database objects.
\\{g-mode-ui-map}"
  (setq tabulated-list-format [("Name" 30 t)
                               ("Type" 10 t)
                               ("Size"  8 t)
                               ("Flags" 6 nil)])
  (setq buffer-read-only t))

(defun g-mode ()
  "Major mode wrapper for BRL-CAD .g files.
Maintains the binary file buffer and creates a UI interface buffer."
  (interactive)
  ;; Ensure the binary buffer is pristine and protected
  (set-buffer-multibyte nil)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  
  (if (not (g-mode--parse-header))
      (error "Not a valid BRL-CAD .g geometry database (magic missing)")
    ;; Create or get UI buffer
    (let* ((bin-buf (current-buffer))
           (ui-buf-name (format "*g: %s*" (buffer-name)))
           (ui-buf (get-buffer-create ui-buf-name)))
      (with-current-buffer ui-buf
        (g-mode-ui-mode)
        (setq g-mode--binary-buffer bin-buf)
        (g-mode--refresh-entries)
        (tabulated-list-init-header)
        (tabulated-list-print))
      (pop-to-buffer ui-buf))))

(provide 'g-mode)

;;; g-mode.el ends here
