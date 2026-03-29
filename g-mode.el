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
              ;; Read name data. It includes a null byte, so we use (1- nlen).
              ;; Strip any trailing NUL padding for inline-renamed shorter names.
              (let* ((raw-name (buffer-substring-no-properties (point) (+ (point) (1- nlen))))
                     (name-str (replace-regexp-in-string "\0+\\'" "" raw-name)))
                (forward-char nlen)
                (setq obj (nconc obj `((name . ,name-str)))))))
          
          (setq obj (nconc obj `((pos . ,start-pos) (interior-pos . ,(point)))))
          obj)))))

(defun g-mode--scan-buffer ()
  "Scan the entire unibyte buffer for .g objects.
Returns a list of parsed object metadata alists.
Corrupt or unparseable regions are recorded with a `corrupt' flag
and skipped over by scanning for the next valid magic byte."
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
            ;; Corrupt region — record it and try to recover
            (let ((corrupt-start (point)))
              (forward-char 1)
              (while (and (< (point) (point-max))
                          (not (and (= (char-after) g-mode-magic1)
                                    (g-mode--parse-object (point)))))
                (forward-char 1))
              (push `((corrupt . t)
                      (name . nil)
                      (length . ,(- (point) corrupt-start))
                      (hflags . 0) (aflags . 0) (bflags . 0)
                      (major-type . 0) (minor-type . 0) (magic1 . 0)
                      (pos . ,corrupt-start)
                      (interior-pos . ,corrupt-start))
                    objects)
              (message "Warning: corrupt region at byte %d (%d bytes)"
                       corrupt-start (- (point) corrupt-start)))))))
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

(defconst g-mode-type-names
  '(((1 . 1) . "Torus (TOR)")
    ((1 . 2) . "Truncated General Cone (TGC)")
    ((1 . 3) . "Ellipsoid (ELL)")
    ((1 . 4) . "Arb8 (ARB8)")
    ((1 . 5) . "Waterline (ARS)")
    ((1 . 6) . "Halfspace (HLF)")
    ((1 . 7) . "Right Elliptical Cylinder (REC)")
    ((1 . 8) . "Polysolid (POLY)")
    ((1 . 9) . "B-spline (NURB)")
    ((1 . 10) . "Sphere (SPH)")
    ((1 . 11) . "NMG (NMG)")
    ((1 . 12) . "Extruded Bitmap (EBM)")
    ((1 . 13) . "Voxels (VOL)")
    ((1 . 14) . "ARBN (ARBN)")
    ((1 . 15) . "Pipe (PIPE)")
    ((1 . 16) . "Particle (PART)")
    ((1 . 17) . "Right Parabolic Cylinder (RPC)")
    ((1 . 18) . "Right Hyperbolic Cylinder (RHC)")
    ((1 . 19) . "Elliptical Paraboloid (EPA)")
    ((1 . 20) . "Elliptical Hyperboloid (EHY)")
    ((1 . 21) . "Elliptical Torus (ETO)")
    ((1 . 22) . "Grip (GRP)")
    ((1 . 23) . "Joint (JOINT)")
    ((1 . 24) . "Displacement Map (Height Field) (HF)")
    ((1 . 25) . "Displacement Map (Height Field) (DSP)")
    ((1 . 26) . "Sketch (SKETCH)")
    ((1 . 27) . "Extrusion (EXTRUDE)")
    ((1 . 28) . "Submodel (SUBMODEL)")
    ((1 . 29) . "Cline (CLINE)")
    ((1 . 30) . "Bag of Triangles (BOT)")
    ((1 . 31) . "Combination (COMB)")
    ((1 . 35) . "Superell (SUPERELL)")
    ((1 . 36) . "Metaball (METABALL)")
    ((1 . 37) . "Boundary Representation (BREP)")
    ((1 . 38) . "Hyp (HYP)")
    ((1 . 39) . "Constraint (CONSTRNT)")
    ((1 . 40) . "Revolution (REVOLVE)")
    ((1 . 41) . "Pnts (PNTS)")
    ((1 . 42) . "Annotation (ANNOT)")
    ((1 . 43) . "Heart (HRT)")
    ((1 . 44) . "Datum (DATUM)")
    ((1 . 45) . "Script (SCRIPT)")
    ((1 . 46) . "Material (MATERIAL)")
    ((2 . 0) . "Attribute Only (ATTRIBONLY)")
    ((9 . 0) . "Uniform-Array Binary (BINUNIF)")
    ((9 . 2) . "Array of Floats (FLOAT)")
    ((9 . 3) . "Array of Doubles (DOUBLE)")
    ((9 . 4) . "Array of Unsigned 8-bit Ints (U8)")
    ((9 . 5) . "Array of Unsigned 16-bit Ints (U16)")
    ((9 . 6) . "Array of Unsigned 32-bit Ints (U32)")
    ((9 . 7) . "Array of Unsigned 64-bit Ints (U64)")
    ((9 . 12) . "Array of 8-bit Ints (I8)")
    ((9 . 13) . "Array of 16-bit Ints (I16)")
    ((9 . 14) . "Array of 32-bit Ints (I32)")
    ((9 . 15) . "Array of 64-bit Ints (I64)")
    ((10 . 0) . "Binary MIME (MIME)"))
  "Alist mapping (major . minor) type pairs to human-readable names.")

(defun g-mode--get-type-name (major minor)
  "Return human-readable name for MAJOR/MINOR type, or 'Unknown'."
  (or (cdr (assoc (cons major minor) g-mode-type-names))
      "Unknown"))

(defvar-local g-mode--binary-buffer nil
  "Reference to the hidden unibyte buffer containing the raw .g file data.")

(defvar-local g-mode--objects nil
  "List of parsed objects from the binary database.")

(defvar-local g-mode-show-deleted nil
  "If non-nil, show Free Space (deleted) and invalid objects in the list.")

(defun g-mode-toggle-show-deleted ()
  "Toggle visibility of deleted/Free Space objects in the database."
  (interactive)
  (setq g-mode-show-deleted (not g-mode-show-deleted))
  (g-mode--refresh-entries)
  (tabulated-list-print t)
  (message "Deleted objects are now %s." (if g-mode-show-deleted "visible" "hidden")))

(defun g-mode--refresh-entries ()
  "Populate `tabulated-list-entries' from the binary buffer."
  (let ((objs (with-current-buffer g-mode--binary-buffer
                (g-mode--scan-buffer))))
    (setq g-mode--objects objs)
    
    (let ((entries nil))
      (dolist (obj objs)
        (let* ((hflags (cdr (assq 'hflags obj)))
               (dli (logand hflags #x03))
               (is-deleted (= dli 2))
               (is-corrupt (cdr (assq 'corrupt obj))))
          (when (or g-mode-show-deleted (and (not is-deleted) (not is-corrupt)))
            (let* ((name (cdr (assq 'name obj)))
                   (major (cdr (assq 'major-type obj)))
                   (minor (cdr (assq 'minor-type obj)))
                   (len (cdr (assq 'length obj)))
                   (type-name (g-mode--get-type-name major minor))
                   (type-str (format "%s (%d,%d)" type-name major minor))
                   (display-name (cond (is-corrupt "<corrupt>")
                                       (is-deleted "<Free Space>")
                                       (t (or name "<unnamed>")))))
              (push (list obj
                          (vector display-name
                                  type-str
                                  (number-to-string len)
                                  (format "%02X" hflags)))
                    entries)))))
      (setq tabulated-list-entries (nreverse entries)))))

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

(defun g-mode--free-object-at (pos bin-buf)
  "Mark object at POS as Free Storage natively in BIN-BUF.
This implements a 'soft delete' that only modifies the object header
flags to set DLI=2 (Free Space). It does NOT zero out the interior
data or fill it with a null body as the full specification suggests,
which allows for easier data recovery if an object was deleted
accidentally."
  (with-current-buffer bin-buf
    (let* ((hflags (char-after (+ pos 1)))
           (owid (ash (logand hflags #xC0) -6))
           (new-hflags (logior (ash owid 6) #x02))
           (new-aflags #x00)
           (new-bflags #x20))
      (g-mode--write-byte (+ pos 1) new-hflags)
      (g-mode--write-byte (+ pos 2) new-aflags)
      (g-mode--write-byte (+ pos 3) new-bflags))))

(defun g-mode-delete-object ()
  "Mark the object at point as Free Storage (DLI=2) natively in the .g file."
  (interactive)
  (let ((obj (tabulated-list-get-id))
        (bin-buf g-mode--binary-buffer))
    (unless obj
      (user-error "No object under point"))
    ;; Read hflags live from the binary buffer, not the stale alist
    (let ((live-hflags (with-current-buffer bin-buf
                         (char-after (+ (cdr (assq 'pos obj)) 1)))))
      (when (= (logand live-hflags #x03) #x02)
        (user-error "Object is already marked as Free Space"))
      
      (g-mode--free-object-at (cdr (assq 'pos obj)) bin-buf)
      
      (message "Marked object '%s' as Free Space." (or (cdr (assq 'name obj)) "unnamed"))
      (g-mode--refresh-entries)
      (tabulated-list-print t))))

(defun g-mode--interior-size (obj bin-buf)
  "Calculate exact byte size of Interior Data for OBJ.
Includes Attribute_Length + Attribute_Data and Body_Length + Body_Data.
Does not include padding or Magic2 footer.  Result is clamped to the
maximum possible interior span to guard against corrupt length fields."
  (with-current-buffer bin-buf
    (save-excursion
      (goto-char (cdr (assq 'interior-pos obj)))
      (let* ((aflags (cdr (assq 'aflags obj)))
             (ap (not (zerop (logand aflags #x20))))
             (awid (ash (logand aflags #xC0) -6))
             (bflags (cdr (assq 'bflags obj)))
             (bp (not (zerop (logand bflags #x20))))
             (bwid (ash (logand bflags #xC0) -6))
             (obj-end (+ (cdr (assq 'pos obj)) (cdr (assq 'length obj))))
             (max-interior (max 0 (- obj-end (point) 1))) ;; up to magic2
             (total 0))
        (when ap
          (let* ((alen-bytes (g-mode--decode-width awid))
                 (alen (g-mode--read-uint (buffer-substring-no-properties (point) (+ (point) alen-bytes)))))
            (if (> (+ total alen-bytes alen) max-interior)
                (setq total max-interior)
              (forward-char (+ alen-bytes alen))
              (cl-incf total (+ alen-bytes alen)))))
        (when (and bp (<= total max-interior))
          (let* ((blen-bytes (g-mode--decode-width bwid))
                 (blen (g-mode--read-uint (buffer-substring-no-properties (point) (+ (point) blen-bytes)))))
            (if (> (+ total blen-bytes blen) max-interior)
                (setq total max-interior)
              (forward-char (+ blen-bytes blen))
              (cl-incf total (+ blen-bytes blen)))))
        (min total max-interior)))))

(defun g-mode--uint-to-bytes (val wid-bytes)
  "Convert integer VAL to a big-endian raw string of WID-BYTES."
  (let ((s (make-string wid-bytes 0)))
    (dotimes (i wid-bytes)
      (aset s (- wid-bytes 1 i) (logand val #xFF))
      (setq val (ash val -8)))
    s))

(defun g-mode--calc-width-prefix (val)
  "Return (wid . bytes) for numeric VAL."
  (cond ((<= val #xFF) '(0 . 1))
        ((<= val #xFFFF) '(1 . 2))
        ((<= val #xFFFFFFFF) '(2 . 4))
        (t '(3 . 8))))

(defun g-mode-rename-object ()
  "Rename the object at point. Modifies the buffer natively."
  (interactive)
  (let ((obj (tabulated-list-get-id))
        (bin-buf g-mode--binary-buffer))
    (unless obj (user-error "No object under point"))
    (let* ((old-name (cdr (assq 'name obj)))
           (hflags (cdr (assq 'hflags obj))))
      (unless old-name (user-error "Cannot rename an unnamed or Free Space object"))
      (let ((new-name (read-string (format "Rename '%s' to: " old-name) old-name)))
        (when (string= old-name new-name)
          (user-error "Name unchanged"))
        
        (let* ((old-nlen (1+ (length old-name)))
               (new-nlen (1+ (length new-name)))
               (name-pos (- (cdr (assq 'interior-pos obj)) old-nlen)))
          (if (<= new-nlen old-nlen)
              ;; Inline overwrite: update Name_Length, write shorter name,
              ;; shift interior data forward to close the gap, and pad.
              ;; Object_Length is unchanged so no other objects move.
              (with-current-buffer bin-buf
                (save-excursion
                  (let* ((inhibit-read-only t)
                         (nwid (ash (logand hflags #x18) -3))
                         (nlen-bytes (g-mode--decode-width nwid))
                         (nlen-field-pos (- name-pos nlen-bytes))
                         (obj-end (+ (cdr (assq 'pos obj)) (cdr (assq 'length obj))))
                         (magic2-pos (1- obj-end))
                         (int-size (g-mode--interior-size obj bin-buf))
                         (interior-data (buffer-substring-no-properties
                                         (cdr (assq 'interior-pos obj))
                                         (+ (cdr (assq 'interior-pos obj)) int-size)))
                         (new-name-data (concat new-name (make-string 1 0)))
                         (available (- magic2-pos name-pos))
                         (padding-size (- available new-nlen int-size))
                         (replacement (concat new-name-data interior-data
                                             (make-string padding-size 0))))
                    ;; Update Name_Length field (same width, no position shift)
                    (goto-char nlen-field-pos)
                    (delete-char nlen-bytes)
                    (insert (g-mode--uint-to-bytes new-nlen nlen-bytes))
                    ;; Replace name + interior + padding region in one shot
                    (delete-region name-pos magic2-pos)
                    (goto-char name-pos)
                    (insert replacement))))
            ;; Append & Free
            (let* ((n-res (g-mode--calc-width-prefix new-nlen))
                   (new-nwid (car n-res))
                   (new-nlen-bytes (cdr n-res))
                   (nlen-str (g-mode--uint-to-bytes new-nlen new-nlen-bytes))
                   (name-str (concat new-name (make-string 1 0)))
                   (int-size (g-mode--interior-size obj bin-buf))
                   (int-str (with-current-buffer bin-buf 
                              (buffer-substring-no-properties 
                               (cdr (assq 'interior-pos obj))
                               (+ (cdr (assq 'interior-pos obj)) int-size))))
                   (base-size (+ 6 new-nlen-bytes new-nlen int-size 1))
                   (new-owid 0) (new-olen-bytes 1) (pad-bytes 0) (olen-chunks 0) (done nil))
              
              (while (not done)
                (let* ((raw-size (+ base-size new-olen-bytes))
                       (rem (% raw-size 8))
                       (local-pad (if (= rem 0) 0 (- 8 rem)))
                       (total-bytes (+ raw-size local-pad))
                       (chunks (/ total-bytes 8))
                       (req-owid (car (g-mode--calc-width-prefix chunks))))
                  (if (<= req-owid new-owid)
                      (setq pad-bytes local-pad olen-chunks chunks done t)
                    (setq new-owid req-owid new-olen-bytes (cdr (g-mode--calc-width-prefix chunks))))))
              
              (let* ((olen-str (g-mode--uint-to-bytes olen-chunks new-olen-bytes))
                     (hflags-no-wids (logand hflags #x07))
                     (new-hflags (char-to-string (logior hflags-no-wids #x20 (ash new-owid 6) (ash new-nwid 3))))
                     (header-str (concat (char-to-string g-mode-magic1) new-hflags
                                         (char-to-string (cdr (assq 'aflags obj)))
                                         (char-to-string (cdr (assq 'bflags obj)))
                                         (char-to-string (cdr (assq 'major-type obj)))
                                         (char-to-string (cdr (assq 'minor-type obj)))))
                     (pad-str (make-string pad-bytes 0))
                     (magic2-str (char-to-string g-mode-magic2))
                     (full-str (concat header-str olen-str nlen-str name-str int-str pad-str magic2-str)))
                
                (with-current-buffer bin-buf
                  (save-excursion
                    (goto-char (point-max))
                    (let ((inhibit-read-only t))
                      (insert full-str))))
                
                (g-mode--free-object-at (cdr (assq 'pos obj)) bin-buf))))
        (message "Renamed '%s' to '%s'." old-name new-name)
        (g-mode--refresh-entries)
        (tabulated-list-print t))))))

(defun g-mode-garbage-collect ()
  "Compact the database by reclaiming Free Space (deleted) objects.
Uses a fault-resilient multi-phase approach:
  Phase 1: Copy deleted objects to end of file (safety backup).
  Phase 2: Shift active objects forward to close gaps.
  Phase 3: Remove the tail containing backed-up deleted data."
  (interactive)
  (let* ((bin-buf g-mode--binary-buffer)
         (objects (with-current-buffer bin-buf (g-mode--scan-buffer)))
         (deleted nil)
         (active nil)
         (reclaimed 0))

    ;; Categorize objects
    (dolist (obj objects)
      (if (= (logand (cdr (assq 'hflags obj)) #x03) 2)
          (progn (push obj deleted)
                 (cl-incf reclaimed (cdr (assq 'length obj))))
        (push obj active)))
    (setq deleted (nreverse deleted))
    (setq active (nreverse active))

    (if (null deleted)
        (message "No deleted objects to compact.")
      (when (yes-or-no-p
             (format "Compact %d deleted objects, reclaiming %d bytes? "
                     (length deleted) reclaimed))
        (with-current-buffer bin-buf
          (let ((inhibit-read-only t)
                (original-end (point-max)))

            ;; Phase 1: Safety backup - append deleted objects to end
            (message "GC Phase 1/3: Backing up %d deleted objects..." (length deleted))
            (save-excursion
              (goto-char original-end)
              (dolist (obj deleted)
                (insert (buffer-substring-no-properties
                         (cdr (assq 'pos obj))
                         (+ (cdr (assq 'pos obj)) (cdr (assq 'length obj)))))))

            ;; Phase 2: Collect active object data and rewrite compacted region.
            ;; Reading from original positions is safe because Phase 1 only appended.
            ;; We read all active data first, then replace, so no position confusion.
            (message "GC Phase 2/3: Compacting %d active objects..." (length active))
            (let ((active-data (mapconcat
                                (lambda (obj)
                                  (buffer-substring-no-properties
                                   (cdr (assq 'pos obj))
                                   (+ (cdr (assq 'pos obj)) (cdr (assq 'length obj)))))
                                active "")))
              (delete-region (+ (point-min) 8) original-end)
              (goto-char (+ (point-min) 8))
              (insert active-data))

            ;; Phase 3: Remove the tail (backed-up deleted copies)
            (message "GC Phase 3/3: Removing backup data...")
            (let ((new-end (+ (point-min) 8
                              (cl-reduce #'+ active
                                         :key (lambda (o) (cdr (assq 'length o)))
                                         :initial-value 0))))
              (when (< new-end (point-max))
                (delete-region new-end (point-max))))))

        (message "GC complete: reclaimed %d bytes from %d deleted objects."
                 reclaimed (length deleted))
        (g-mode--refresh-entries)
        (tabulated-list-print t)))))

(defvar g-mode-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") 'g-mode-view-object)
    (define-key map (kbd "RET") 'g-mode-view-object)
    (define-key map (kbd "d") 'g-mode-delete-object)
    (define-key map (kbd "R") 'g-mode-rename-object)
    (define-key map (kbd "G") 'g-mode-garbage-collect)
    (define-key map (kbd "h") 'g-mode-toggle-show-deleted)
    (define-key map (kbd "s") 'g-mode-save)
    (define-key map (kbd "C-x C-s") 'g-mode-save)
    (define-key map (kbd "?") 'g-mode-help)
    map)
  "Keymap for `g-mode-ui-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . g-mode))

;;;###autoload
(add-to-list 'file-coding-system-alist '("\\.g\\'" . no-conversion))

(define-derived-mode g-mode-ui-mode tabulated-list-mode "g-mode-UI"
  "UI mode for browsing BRL-CAD database objects.
\\{g-mode-ui-mode-map}"
  (setq tabulated-list-format [("Name" 30 t)
                               ("Type" 25 t)
                               ("Size"  8 t)
                               ("Flags" 6 nil)])
  (setq header-line-format " v:view  d:delete  R:rename  G:gc  h:toggle-deleted  s:save  ?:help")
  (setq buffer-read-only t))

(defun g-mode-save ()
  "Save the changes in the hidden binary buffer to its file."
  (interactive)
  (if (not (buffer-live-p g-mode--binary-buffer))
      (error "Binary buffer is no longer live")
    (with-current-buffer g-mode--binary-buffer
      (save-buffer))
    (message "Database saved.")))

(defun g-mode-help ()
  "Display a brief summary of g-mode keybindings."
  (interactive)
  (message "v/RET:view  d:delete  R:rename  G:gc  h:toggle-deleted  s:save  ?:help"))

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
           (ui-name (format "*g: %s*" (buffer-name)))
           (ui-buf (get-buffer-create (generate-new-buffer-name ui-name))))
      (with-current-buffer ui-buf
        (g-mode-ui-mode)
        (setq g-mode--binary-buffer bin-buf)
        (g-mode--refresh-entries)
        (tabulated-list-init-header)
        (tabulated-list-print))
      (pop-to-buffer ui-buf)
      ui-buf)))

(provide 'g-mode)

;;; g-mode.el ends here
