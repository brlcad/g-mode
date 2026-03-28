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
Returns nil if magic numbers do not match."
  (let* ((bytes (buffer-substring-no-properties (point-min) (+ (point-min) 8)))
         (header (bindat-unpack g-mode-db-header bytes)))
    (if (and (= (cdr (assq 'magic1 header)) g-mode-magic1)
             (= (cdr (assq 'magic2 header)) g-mode-magic2)
             (= (cdr (assq 'hflags header)) 1)) ;; DLI=1 for Db header
        header
      nil)))

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
      (let* ((hbuf (buffer-substring-no-properties (point) (+ (point) 6)))
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . g-mode))

(define-derived-mode g-mode tabulated-list-mode "g-mode"
  "Major mode for browsing and editing BRL-CAD .g binary files.
\\{g-mode-map}"
  (setq tabulated-list-format [("Name" 30 t)
                               ("Type" 10 t)
                               ("Size"  8 t)
                               ("Flags" 6 nil)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print))

(provide 'g-mode)

;;; g-mode.el ends here
