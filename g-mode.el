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
(require 'button)
(require 'tabulated-list)

(defgroup g-mode nil
  "Major mode for reading and editing BRL-CAD .g geometry database files."
  :group 'data)

(defface g-mode-deleted-face
  '((t :inherit shadow :strike-through t))
  "Face for deleted (Free Space) objects in the tabulated list."
  :group 'g-mode)

(defface g-mode-corrupt-face
  '((t :inherit error :weight bold))
  "Face for corrupt or unparseable regions in the tabulated list."
  :group 'g-mode)

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

(defun g-mode--make-diagnostic (level code message &rest props)
  "Create a diagnostic alist with LEVEL, CODE, MESSAGE, and PROPS."
  (append `((level . ,level)
            (code . ,code)
            (message . ,message))
          props))

(defun g-mode--diagnostics-have-errors-p (diagnostics)
  "Return non-nil when DIAGNOSTICS contains at least one error entry."
  (cl-some (lambda (diag) (eq (cdr (assq 'level diag)) 'error)) diagnostics))

(defun g-mode--diagnostic-level-label (diag)
  "Return a user-facing label for diagnostic DIAG."
  (upcase (symbol-name (or (cdr (assq 'level diag)) 'info))))

(defun g-mode--get-diagnostics (record)
  "Return the diagnostics list carried by RECORD."
  (cdr (assq 'diagnostics record)))

(defun g-mode--safe-bytes (start len &optional limit)
  "Return LEN bytes starting at START, or nil if out of bounds.
LIMIT defaults to `point-max' and is treated as an exclusive bound."
  (let* ((limit (or limit (point-max)))
         (end (+ start len)))
    (when (and (<= start limit)
               (<= end limit))
      (buffer-substring-no-properties start end))))

(defun g-mode--read-uint-at (pos wid-bytes &optional limit)
  "Read an unsigned integer at POS using WID-BYTES bytes.
Return nil if the requested span falls outside LIMIT."
  (let ((bytes (g-mode--safe-bytes pos wid-bytes limit)))
    (when bytes
      (g-mode--read-uint bytes))))

(defun g-mode--canonical-header-bytes ()
  "Return the canonical 8-byte DB5 header as a unibyte string."
  (unibyte-string #x76 #x01 #x00 #x00 #x00 #x00 #x01 #x35))

(defun g-mode--analyze-header ()
  "Analyze the database header and return a recovery-oriented record."
  (save-excursion
    (let* ((pos (point-min))
           (raw (g-mode--safe-bytes pos 8))
           (diagnostics nil)
           (fields nil))
      (if (not raw)
          `((kind . header)
            (pos . ,pos)
            (length . ,(- (point-max) (point-min)))
            (valid . nil)
            (diagnostics . ,(list (g-mode--make-diagnostic
                                   'error 'truncated-header
                                   "Database header is truncated."))))
        (setq fields (bindat-unpack g-mode-db-header raw))
        (unless (= (cdr (assq 'magic1 fields)) g-mode-magic1)
          (push (g-mode--make-diagnostic
                 'error 'bad-header-magic1
                 (format "Header Magic1 is 0x%02X, expected 0x%02X."
                         (cdr (assq 'magic1 fields)) g-mode-magic1))
                diagnostics))
        (unless (= (cdr (assq 'magic2 fields)) g-mode-magic2)
          (push (g-mode--make-diagnostic
                 'error 'bad-header-magic2
                 (format "Header Magic2 is 0x%02X, expected 0x%02X."
                         (cdr (assq 'magic2 fields)) g-mode-magic2))
                diagnostics))
        (unless (= (logand (cdr (assq 'hflags fields)) #x03) 1)
          (push (g-mode--make-diagnostic
                 'error 'bad-header-dli
                 (format "Header DLI bits are 0x%X, expected 0x1."
                         (logand (cdr (assq 'hflags fields)) #x03)))
                diagnostics))
        (when (not (zerop (logand (cdr (assq 'hflags fields)) #x20)))
          (push (g-mode--make-diagnostic
                 'error 'header-name-present
                 "Header object should not have a name-present bit set.")
                diagnostics))
        (unless (zerop (logand (cdr (assq 'aflags fields)) #x20))
          (push (g-mode--make-diagnostic
                 'error 'header-attributes-present
                 "Header object should not have attributes.")
                diagnostics))
        (unless (zerop (logand (cdr (assq 'bflags fields)) #x20))
          (push (g-mode--make-diagnostic
                 'error 'header-body-present
                 "Header object should not have a body.")
                diagnostics))
        (unless (= (ash (logand (cdr (assq 'hflags fields)) #xC0) -6) 0)
          (push (g-mode--make-diagnostic
                 'error 'header-object-width
                 "Header object length width should be 8-bit.")
                diagnostics))
        (unless (= (ash (logand (cdr (assq 'aflags fields)) #xC0) -6) 0)
          (push (g-mode--make-diagnostic
                 'error 'header-attribute-width
                 "Header attribute length width should be 8-bit.")
                diagnostics))
        (unless (= (ash (logand (cdr (assq 'bflags fields)) #xC0) -6) 0)
          (push (g-mode--make-diagnostic
                 'error 'header-body-width
                 "Header body length width should be 8-bit.")
                diagnostics))
        (unless (zerop (logand (cdr (assq 'aflags fields)) #x07))
          (push (g-mode--make-diagnostic
                 'error 'header-attribute-compression
                 "Header attribute compression flags should be zero.")
                diagnostics))
        (unless (zerop (logand (cdr (assq 'bflags fields)) #x07))
          (push (g-mode--make-diagnostic
                 'error 'header-body-compression
                 "Header body compression flags should be zero.")
                diagnostics))
        (unless (= (cdr (assq 'major-type fields)) 0)
          (push (g-mode--make-diagnostic
                 'error 'header-major-type
                 "Header major type should be 0.")
                diagnostics))
        (unless (= (cdr (assq 'minor-type fields)) 0)
          (push (g-mode--make-diagnostic
                 'error 'header-minor-type
                 "Header minor type should be 0.")
                diagnostics))
        (unless (= (cdr (assq 'length fields)) 1)
          (push (g-mode--make-diagnostic
                 'error 'header-length
                 (format "Header object length field is %d, expected 1."
                         (cdr (assq 'length fields))))
                diagnostics))
        (append `((kind . header)
                  (name . "<database header>")
                  (type-label . "Database Header")
                  (pos . ,pos)
                  (length . 8)
                  (valid . ,(not (g-mode--diagnostics-have-errors-p diagnostics)))
                  (diagnostics . ,(nreverse diagnostics)))
                fields)))))

(defun g-mode--parse-header ()
  "Parse the 8-byte database header and return its header fields, or nil."
  (let ((header (g-mode--analyze-header)))
    (when (cdr (assq 'valid header))
      (list (cons 'magic1 (cdr (assq 'magic1 header)))
            (cons 'hflags (cdr (assq 'hflags header)))
            (cons 'aflags (cdr (assq 'aflags header)))
            (cons 'bflags (cdr (assq 'bflags header)))
            (cons 'major-type (cdr (assq 'major-type header)))
            (cons 'minor-type (cdr (assq 'minor-type header)))
            (cons 'length (cdr (car (last (cl-remove-if-not (lambda (cell) (eq (car cell) 'length))
                                                           header)))))
            (cons 'magic2 (cdr (assq 'magic2 header)))))))

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

(defun g-mode--analyze-object-interior (start-pos obj-end aflags bflags diagnostics)
  "Analyze object interior data between START-POS and OBJ-END.
Return an alist describing spans and updated DIAGNOSTICS.
Signal failure via `throw' when a span extends past the object boundary."
  (save-excursion
    (goto-char start-pos)
    (let* ((limit (1- obj-end))
           (cursor start-pos)
           (ap (not (zerop (logand aflags #x20))))
           (awid (ash (logand aflags #xC0) -6))
           (bp (not (zerop (logand bflags #x20))))
           (bwid (ash (logand bflags #xC0) -6))
           (attribute-data-pos nil)
           (attribute-length nil)
           (body-data-pos nil)
           (body-length nil))
      (when ap
        (let* ((alen-bytes (g-mode--decode-width awid))
               (alen (g-mode--read-uint-at cursor alen-bytes obj-end)))
          (unless alen
            (throw 'g-mode-invalid-object
                   (cons (g-mode--make-diagnostic
                          'error 'truncated-attribute-length
                          "Attribute length field extends past the object boundary.")
                         diagnostics)))
          (setq cursor (+ cursor alen-bytes)
                attribute-data-pos cursor
                attribute-length alen)
          (when (> (+ cursor alen) limit)
            (throw 'g-mode-invalid-object
                   (cons (g-mode--make-diagnostic
                          'error 'truncated-attribute-data
                          "Attribute payload extends past the object boundary."
                          `(expected-length . ,alen))
                         diagnostics)))
          (goto-char cursor)
          (forward-char alen)
          (setq cursor (point))))
      (when bp
        (let* ((blen-bytes (g-mode--decode-width bwid))
               (blen (g-mode--read-uint-at cursor blen-bytes obj-end)))
          (unless blen
            (throw 'g-mode-invalid-object
                   (cons (g-mode--make-diagnostic
                          'error 'truncated-body-length
                          "Body length field extends past the object boundary.")
                         diagnostics)))
          (setq cursor (+ cursor blen-bytes)
                body-data-pos cursor
                body-length blen)
          (when (> (+ cursor blen) limit)
            (throw 'g-mode-invalid-object
                   (cons (g-mode--make-diagnostic
                          'error 'truncated-body-data
                          "Body payload extends past the object boundary."
                          `(expected-length . ,blen))
                         diagnostics)))
          (goto-char cursor)
          (forward-char blen)
          (setq cursor (point))))
      `((diagnostics . ,diagnostics)
        (attribute-data-pos . ,attribute-data-pos)
        (attribute-length . ,attribute-length)
        (body-data-pos . ,body-data-pos)
        (body-length . ,body-length)
        (interior-size . ,(- cursor start-pos))
        (padding-size . ,(max 0 (- limit cursor)))))))

(defun g-mode--analyze-object (start-pos &optional limit)
  "Analyze the object candidate at START-POS up to LIMIT.
Return a valid object record or an invalid candidate record with diagnostics."
  (save-excursion
    (let ((limit (or limit (point-max)))
          diagnostics
          obj
          len
          obj-end)
      (cl-labels
          ((warn (code message &rest props)
             (push (apply #'g-mode--make-diagnostic 'warning code message props)
                   diagnostics))
           (fail (code message &rest props)
             (let ((all (nreverse
                         (cons (apply #'g-mode--make-diagnostic 'error code message props)
                               diagnostics))))
               (throw 'g-mode-invalid-object
                      (append `((kind . corrupt-candidate)
                                (valid . nil)
                                (pos . ,start-pos)
                                (diagnostics . ,all))
                              (when len `((candidate-length . ,len)))
                              (when obj-end `((candidate-end . ,obj-end)))
                              obj)))))
        (catch 'g-mode-invalid-object
          (unless (= (char-after start-pos) g-mode-magic1)
            (fail 'bad-magic1
                  (format "Magic1 at offset %d is 0x%02X, expected 0x%02X."
                          start-pos (or (char-after start-pos) 0) g-mode-magic1)))
          (unless (g-mode--safe-bytes start-pos 6 limit)
            (fail 'truncated-fixed-header
                  "Object fixed header is truncated."))
          (setq obj (bindat-unpack g-mode-object-fixed-header
                                   (g-mode--safe-bytes start-pos 6 limit)))
          (let* ((hflags (cdr (assq 'hflags obj)))
                 (aflags (cdr (assq 'aflags obj)))
                 (bflags (cdr (assq 'bflags obj)))
                 (owid (ash (logand hflags #xC0) -6))
                 (np (not (zerop (logand hflags #x20))))
                 (nwid (ash (logand hflags #x18) -3))
                 (olen-bytes (g-mode--decode-width owid))
                 (olen-pos (+ start-pos 6))
                 (olen-chunks (g-mode--read-uint-at olen-pos olen-bytes limit)))
            (unless olen-chunks
              (fail 'truncated-object-length
                    "Object length field is truncated."))
            (setq len (* olen-chunks 8))
            (setq obj (append obj `((length . ,len))))
            (when (< len 8)
              (fail 'object-too-short
                    (format "Object length %d is shorter than the 8-byte minimum." len)))
            (setq obj-end (+ start-pos len))
            (when (> obj-end limit)
              (fail 'object-overruns-buffer
                    (format "Object claims %d bytes, extending past end of buffer." len)
                    `(expected-end . ,obj-end)
                    `(buffer-end . ,limit)))
            (unless (= (or (char-after (1- obj-end)) 0) g-mode-magic2)
              (fail 'bad-magic2
                    (format "Object footer Magic2 is 0x%02X, expected 0x%02X."
                            (or (char-after (1- obj-end)) 0) g-mode-magic2)))
            (goto-char (+ olen-pos olen-bytes))
            (when (and (= (logand hflags #x03) 0) (not np))
              (warn 'application-object-missing-name
                    "Application-data object is missing a name field."))
            (when np
              (let* ((nlen-bytes (g-mode--decode-width nwid))
                     (nlen (g-mode--read-uint-at (point) nlen-bytes obj-end)))
                (unless nlen
                  (fail 'truncated-name-length
                        "Name length field is truncated."))
                (forward-char nlen-bytes)
                (when (< nlen 1)
                  (warn 'empty-name
                        "Name length is zero; treating name as empty."))
                (when (> (+ (point) nlen) obj-end)
                  (fail 'name-overruns-object
                        "Name data extends past the end of the object."
                        `(name-length . ,nlen)))
                (let* ((name-end (max (point) (+ (point) nlen -1)))
                       (raw-name (if (> nlen 0)
                                     (buffer-substring-no-properties (point) name-end)
                                   ""))
                       (terminator (and (> nlen 0)
                                        (char-after (1- (+ (point) nlen))))))
                  (unless (or (null terminator) (zerop terminator))
                    (warn 'name-not-null-terminated
                          "Name field is not NUL-terminated."))
                  (setq obj (append obj `((name . ,(replace-regexp-in-string "\0+\\'" "" raw-name)))))
                  (forward-char nlen))))
            (when (and (= (logand hflags #x03) 2) np)
              (warn 'free-object-has-name
                    "Free-space object retains a name field."))
            (when (and (= (logand hflags #x03) 2)
                       (not (zerop (logand aflags #x20))))
              (warn 'free-object-has-attributes
                    "Free-space object has attributes, which is non-canonical."))
            (let* ((interior-pos (point))
                   (interior (catch 'g-mode-invalid-object
                               (g-mode--analyze-object-interior
                                interior-pos obj-end aflags bflags diagnostics))))
              (when (and (consp interior) (eq (caar interior) 'level))
                (throw 'g-mode-invalid-object
                       (append `((kind . corrupt-candidate)
                                 (valid . nil)
                                 (pos . ,start-pos)
                                 (candidate-length . ,len)
                                 (candidate-end . ,obj-end)
                                 (diagnostics . ,(nreverse interior)))
                               obj)))
              (setq diagnostics (cdr (assq 'diagnostics interior)))
              (append `((kind . object)
                        (valid . t)
                        (pos . ,start-pos)
                        (interior-pos . ,interior-pos)
                        (diagnostics . ,(nreverse diagnostics)))
                      obj
                      (cl-remove-if (lambda (cell) (eq (car cell) 'diagnostics))
                                    interior)))))))))

(defun g-mode--parse-object (start-pos)
  "Parse an object starting at START-POS.
Return object metadata when START-POS holds a valid object candidate."
  (let ((analysis (g-mode--analyze-object start-pos)))
    (when (cdr (assq 'valid analysis))
      analysis)))

(defun g-mode--find-next-object-candidate (start-pos &optional limit)
  "Return the next plausible object start at or after START-POS.
LIMIT defaults to `point-max'.  Plausibility requires a structurally valid
object candidate, not just a matching magic byte."
  (let ((limit (or limit (point-max)))
        (pos start-pos)
        found)
    (while (and (not found) (< pos limit))
      (when (= (or (char-after pos) 0) g-mode-magic1)
        (let ((analysis (g-mode--analyze-object pos limit)))
          (when (cdr (assq 'valid analysis))
            (setq found pos))))
      (setq pos (1+ pos)))
    found))

(defun g-mode--scan-buffer ()
  "Scan the entire unibyte buffer for .g objects.
Returns a list of parsed object metadata alists.
Corrupt or unparseable regions are recorded with a `corrupt' flag
and skipped over by scanning for the next valid magic byte."
  (let* ((header (g-mode--analyze-header))
         (objects nil)
         (pos (if (cdr (assq 'valid header))
                  (+ (point-min) 8)
                (point-min))))
    (save-excursion
      (goto-char pos)
      (while (< (point) (point-max))
        (let* ((start (point))
               (analysis (g-mode--analyze-object start (point-max))))
          (if (cdr (assq 'valid analysis))
              (progn
                (push analysis objects)
                (goto-char (+ start (cdr (assq 'length analysis)))))
            ;; Corrupt region — record it and try to recover
            (let ((corrupt-start (point)))
              (goto-char (or (g-mode--find-next-object-candidate (1+ corrupt-start) (point-max))
                             (point-max)))
              (push `((corrupt . t)
                      (kind . corrupt)
                      (name . nil)
                      (length . ,(- (point) corrupt-start))
                      (valid . nil)
                      (diagnostics . ,(g-mode--get-diagnostics analysis))
                      (hflags . 0) (aflags . 0) (bflags . 0)
                      (major-type . 0) (minor-type . 0) (magic1 . 0)
                      ,@(when (assq 'candidate-length analysis)
                          `((candidate-length . ,(cdr (assq 'candidate-length analysis)))))
                      ,@(when (assq 'candidate-end analysis)
                          `((candidate-end . ,(cdr (assq 'candidate-end analysis)))))
                      (pos . ,corrupt-start)
                      (interior-pos . ,corrupt-start))
                    objects)
              (message "Warning: corrupt region at byte %d (%d bytes)"
                       corrupt-start (- (point) corrupt-start)))))))
    (nreverse objects)))

(defun g-mode--parse-attributes (obj)
  "Parse attributes out of OBJ at its `interior-pos` in the current buffer.
Returns an alist of (KEY . VALUE) strings, or nil."
  (let* ((aflags (cdr (assq 'aflags obj)))
         (az (logand aflags #x07))
         (attr-pos (cdr (assq 'attribute-data-pos obj)))
         (attr-len (cdr (assq 'attribute-length obj))))
    (when (and attr-pos attr-len (zerop az))
      (save-excursion
        (goto-char attr-pos)
        (let* ((attr-data (buffer-substring-no-properties (point) (+ (point) attr-len)))
               (parts (split-string attr-data "\0" t))
               (attrs nil))
          (while (>= (length parts) 2)
            (push (cons (pop parts) (pop parts)) attrs))
          (nreverse attrs))))))

(defconst g-mode-type-names
  '(((1 . 1) . "TOR (Torus)")
    ((1 . 2) . "TGC (Trunc Cone)")
    ((1 . 3) . "ELL (Ellipsoid)")
    ((1 . 4) . "ARB8 (Arb Poly)")
    ((1 . 5) . "ARS (Waterline)")
    ((1 . 6) . "HALF (Halfspace)")
    ((1 . 7) . "REC (Rt Ell Cyl)")
    ((1 . 8) . "POLY (Polysolid)")
    ((1 . 9) . "BSPLINE (B-spline)")
    ((1 . 10) . "SPH (Sphere)")
    ((1 . 11) . "NMG (N-Manifold)")
    ((1 . 12) . "EBM (Ext Bitmap)")
    ((1 . 13) . "VOL (Volume)")
    ((1 . 14) . "ARBN (N-Face Poly)")
    ((1 . 15) . "PIPE (Pipe)")
    ((1 . 16) . "PART (Particle)")
    ((1 . 17) . "RPC (Rt Parab Cyl)")
    ((1 . 18) . "RHC (Rt Hyper Cyl)")
    ((1 . 19) . "EPA (Ell Parab)")
    ((1 . 20) . "EHY (Ell Hyper)")
    ((1 . 21) . "ETO (Ell Torus)")
    ((1 . 22) . "GRIP (Grip)")
    ((1 . 23) . "JOINT (Joint)")
    ((1 . 24) . "HF (Height Field)")
    ((1 . 25) . "DSP (Disp Map)")
    ((1 . 26) . "SKETCH (Sketch)")
    ((1 . 27) . "EXTRUDE (Extrusion)")
    ((1 . 28) . "SUBMODEL (Submodel)")
    ((1 . 29) . "CLINE (Cline)")
    ((1 . 30) . "BOT (Bag of Tri)")
    ((1 . 31) . "COMB (Combination)")
    ((1 . 35) . "SUPERELL (Superell)")
    ((1 . 36) . "METABALL (Metaball)")
    ((1 . 37) . "BREP (B-Rep NURBS)")
    ((1 . 38) . "HYP (Hyp)")
    ((1 . 39) . "CONSTRNT (Constraint)")
    ((1 . 40) . "REVOLVE (Revolution)")
    ((1 . 41) . "PNTS (Pnts)")
    ((1 . 42) . "ANNOT (Annotation)")
    ((1 . 43) . "HRT (Heart)")
    ((1 . 44) . "DATUM (Datum)")
    ((1 . 45) . "SCRIPT (Script)")
    ((1 . 46) . "MATERIAL (Material)")
    ((2 . 0) . "ATTRIBONLY (Attribute)")
    ((9 . 0) . "BINUNIF (Unif Binary)")
    ((9 . 2) . "FLOAT (Floats)")
    ((9 . 3) . "DOUBLE (Doubles)")
    ((9 . 4) . "U8 (Unsigned 8)")
    ((9 . 5) . "U16 (Unsigned 16)")
    ((9 . 6) . "U32 (Unsigned 32)")
    ((9 . 7) . "U64 (Unsigned 64)")
    ((9 . 12) . "I8 (Signed 8)")
    ((9 . 13) . "I16 (Signed 16)")
    ((9 . 14) . "I32 (Signed 32)")
    ((9 . 15) . "I64 (Signed 64)")
    ((10 . 0) . "MIME (Binary MIME)"))
  "Alist mapping (major . minor) type pairs to human-readable names.")

(defun g-mode--get-type-name (major minor)
  "Return human-readable name for MAJOR/MINOR type, or \"Unknown\"."
  (or (cdr (assoc (cons major minor) g-mode-type-names))
      "Unknown"))

(defvar-local g-mode--binary-buffer nil
  "Reference to the hidden unibyte buffer containing the raw .g file data.")

(defvar-local g-mode--header-info nil
  "Structured analysis of the database header for this UI buffer.")

(defvar-local g-mode--objects nil
  "List of parsed objects from the binary database.")

(defvar-local g-mode--marked-objects nil
  "List of object positions (IDs) currently marked with `*`.")

(defvar-local g-mode--session-deleted-objects nil
  "List of object positions explicitly soft-deleted during this session.")

(defvar-local g-mode-show-deleted t
  "If non-nil, show Free Space (deleted) and invalid objects in the list.")

(defvar-local g-mode-filter-regexp nil
  "Regular expression used to filter the list of objects. Nil means no filter.")

(defun g-mode-toggle-show-deleted ()
  "Toggle visibility of deleted/Free Space objects in the database."
  (interactive)
  (setq g-mode-show-deleted (not g-mode-show-deleted))
  (g-mode--update-ui)
  (message "Deleted objects are now %s." (if g-mode-show-deleted "visible" "hidden")))

(defun g-mode-filter (regexp)
  "Filter the object list to only show items matching REGEXP."
  (interactive "sFilter regexp (empty to clear): ")
  (if (string-empty-p regexp)
      (setq g-mode-filter-regexp nil)
    (setq g-mode-filter-regexp regexp))
  (g-mode--update-ui)
  (message "Filter %s" (if g-mode-filter-regexp (format "set to '%s'" regexp) "cleared")))

(defun g-mode--update-ui ()
  "Refresh entries, print the UI, and restore visual marks."
  (g-mode--refresh-entries)
  (let ((max-name-len 4)
        (max-type-len 4)
        (max-id-len 2)
        (max-offset-len 6))
    (dolist (entry tabulated-list-entries)
      (let* ((vec (cadr entry))
             (name (aref vec 1))
             (type-str (aref vec 2))
             (id-str (aref vec 3))
             (offset-str (aref vec 4)))
        (setq max-name-len (max max-name-len (length name)))
        (setq max-type-len (max max-type-len (length type-str)))
        (setq max-id-len (max max-id-len (length id-str)))
        (setq max-offset-len (max max-offset-len (length offset-str)))))
    (setq tabulated-list-format (vector '("#" 4 t)
                                        (list "Name" (+ 3 max-name-len) t)
                                        (list "Type" (+ 3 max-type-len) t)
                                        (list "Id" (+ 3 max-id-len) t)
                                        (list "Offset" (+ 3 max-offset-len) t)
                                        '("Size" 8 t)
                                        '("Flags" 6 nil)))
    (tabulated-list-init-header))
  (tabulated-list-print t)
  (when (buffer-live-p g-mode--binary-buffer)
    (set-buffer-modified-p (buffer-modified-p g-mode--binary-buffer)))
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id)))
          (when (member id g-mode--marked-objects)
            (tabulated-list-put-tag "*")))
        (forward-line 1)))))

(defun g-mode--refresh-entries ()
  "Populate `tabulated-list-entries' from the binary buffer."
  (let* ((header-info (with-current-buffer g-mode--binary-buffer
                        (g-mode--analyze-header)))
         (objs (with-current-buffer g-mode--binary-buffer
                 (g-mode--scan-buffer))))
    (setq g-mode--header-info header-info)
    (setq g-mode--objects objs)
    
    (let ((entries nil)
          (idx 1))
      (when g-mode--header-info
        (let* ((valid (cdr (assq 'valid g-mode--header-info)))
               (face (if valid nil 'g-mode-corrupt-face))
               (name (if valid "<database header>" "<invalid header>")))
          (push (list :header
                      (vector "0"
                              (if face (propertize name 'face face) name)
                              "Database Header"
                              "-"
                              "0"
                              (number-to-string (cdr (assq 'length g-mode--header-info)))
                              "HDR"))
                entries)))
      (dolist (obj objs)
        (let* ((hflags (cdr (assq 'hflags obj)))
               (dli (logand hflags #x03))
               (is-deleted (= dli 2))
               (is-corrupt (cdr (assq 'corrupt obj)))
               (name (cdr (assq 'name obj))))
          (when (and (or g-mode-show-deleted (and (not is-deleted) (not is-corrupt)))
                     (or (null g-mode-filter-regexp)
                         (and name (string-match-p g-mode-filter-regexp name))))
            (let* ((major (cdr (assq 'major-type obj)))
                   (minor (cdr (assq 'minor-type obj)))
                   (len (cdr (assq 'length obj)))
                   (type-name (g-mode--get-type-name major minor))
                   (id-str (format "%d,%d" major minor))
                   (pos-str (number-to-string (1- (cdr (assq 'pos obj)))))
                   (face (cond (is-corrupt 'g-mode-corrupt-face)
                               (is-deleted 'g-mode-deleted-face)
                               (t nil)))
                   (display-name (cond (is-corrupt "<corrupt>")
                                       (is-deleted "<Free Space>")
                                       (t (or name "<unnamed>")))))
              ;; The ID is the object's position in the binary buffer.
              (push (list (cdr (assq 'pos obj))
                          (vector (number-to-string idx)
                                  (if face (propertize display-name 'face face) display-name)
                                  type-name
                                  id-str
                                  pos-str
                                  (number-to-string len)
                                  (format "%02X" hflags)))
                    entries)))
          (cl-incf idx)))
      (setq tabulated-list-entries (nreverse entries)))))

(defvar-local g-mode--inspector-source-buffer nil
  "The `g-mode-ui-mode' buffer that owns the current inspector.")

(defvar-local g-mode--inspector-record-id nil
  "The current record ID shown in the inspector.")

(define-derived-mode g-mode-inspector-mode special-mode "BRL-CAD Object"
  "Inspector buffer for database objects and recovery actions.")

(defun g-mode--lookup-record (id)
  "Look up record ID in the current `g-mode-ui-mode' buffer."
  (cond
   ((eq id :header) g-mode--header-info)
   (t (cl-find id g-mode--objects :key (lambda (o) (cdr (assq 'pos o)))))))

(defun g-mode--goto-record (id)
  "Move point to the row with ID in the current tabulated list buffer."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (when (equal (tabulated-list-get-id) id)
        (setq found t))
      (unless found
        (forward-line 1)))
    found))

(defun g-mode--with-record-at-point (ui-buf id fn)
  "In UI-BUF, move to record ID and call FN."
  (with-current-buffer ui-buf
    (unless (g-mode--goto-record id)
      (user-error "Record is no longer visible"))
    (funcall fn)))

(defun g-mode--write-bytes (pos bytes)
  "Overwrite BYTES at POS in the current buffer."
  (save-excursion
    (goto-char pos)
    (let ((inhibit-read-only t))
      (delete-region pos (+ pos (length bytes)))
      (insert bytes))))

(defun g-mode--make-free-object-bytes (length)
  "Return a canonical free-space object of LENGTH bytes."
  (unless (and (>= length 8) (zerop (% length 8)))
    (error "Free-space rewrite requires a length that is a multiple of 8"))
  (let* ((buf (make-string length 0))
         (chunks (/ length 8))
         (wid (g-mode--calc-width-prefix chunks))
         (owid (car wid))
         (olen-bytes (cdr wid))
         (olen-str (g-mode--uint-to-bytes chunks olen-bytes)))
    (aset buf 0 g-mode-magic1)
    (aset buf 1 (logior (ash owid 6) 2))
    (dotimes (idx olen-bytes)
      (aset buf (+ 6 idx) (aref olen-str idx)))
    (aset buf (1- length) g-mode-magic2)
    buf))

(defun g-mode--inspector-current-record ()
  "Return the latest record shown by the current inspector."
  (let ((ui-buf g-mode--inspector-source-buffer)
        (id g-mode--inspector-record-id))
    (when (buffer-live-p ui-buf)
      (with-current-buffer ui-buf
      (g-mode--refresh-entries)
        (g-mode--lookup-record id)))))

(defun g-mode--inspector-refresh ()
  "Re-render the current inspector buffer."
  (let ((record (g-mode--inspector-current-record)))
    (unless record
      (user-error "Record is no longer available"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (g-mode--render-inspector record)
      (goto-char (point-min)))))

(defun g-mode--inspector-apply-change (fn)
  "Run FN against the binary buffer, then refresh UI and inspector."
  (let ((ui-buf g-mode--inspector-source-buffer))
    (unless (buffer-live-p ui-buf)
      (user-error "Source g-mode buffer is no longer live"))
    (with-current-buffer ui-buf
      (funcall fn)
      (g-mode--update-ui))
    (g-mode--inspector-refresh)))

(defun g-mode--inspector-run-ui-command (fn)
  "Run existing UI command FN on the inspector's current record, then refresh."
  (g-mode--with-record-at-point
   g-mode--inspector-source-buffer
   g-mode--inspector-record-id
   fn)
  (g-mode--inspector-refresh))

(defun g-mode--prompt-byte (prompt current)
  "Prompt for a hex byte with PROMPT and CURRENT default."
  (let* ((text (read-string prompt (format "%02X" current)))
         (value (string-to-number text 16)))
    (unless (and (>= value 0) (<= value #xFF))
      (user-error "Expected a hex byte value between 00 and FF"))
    value))

(defun g-mode--edit-byte-at-offset (record offset label)
  "Prompt to edit byte LABEL at OFFSET within RECORD."
  (let ((pos (cdr (assq 'pos record)))
        (bin-buf (with-current-buffer g-mode--inspector-source-buffer
                   g-mode--binary-buffer)))
    (g-mode--inspector-apply-change
     (lambda ()
       (with-current-buffer bin-buf
         (let* ((byte-pos (+ pos offset))
                (current (or (char-after byte-pos) 0))
                (value (g-mode--prompt-byte
                        (format "%s (hex): " label) current)))
           (g-mode--write-byte byte-pos value)))))))

(defun g-mode--inspector-edit-type (record)
  "Prompt for new major/minor type bytes for RECORD."
  (let ((pos (cdr (assq 'pos record)))
        (bin-buf (with-current-buffer g-mode--inspector-source-buffer
                   g-mode--binary-buffer)))
    (g-mode--inspector-apply-change
     (lambda ()
       (with-current-buffer bin-buf
         (let ((major (g-mode--prompt-byte
                       "Major type (hex): "
                       (or (char-after (+ pos 4)) 0)))
               (minor (g-mode--prompt-byte
                       "Minor type (hex): "
                       (or (char-after (+ pos 5)) 0))))
           (g-mode--write-byte (+ pos 4) major)
           (g-mode--write-byte (+ pos 5) minor)))))))

(defun g-mode--inspector-repair-header ()
  "Rewrite the database header to the canonical DB5 form."
  (let ((bin-buf (with-current-buffer g-mode--inspector-source-buffer
                   g-mode--binary-buffer)))
    (g-mode--inspector-apply-change
     (lambda ()
       (with-current-buffer bin-buf
         (if (< (- (point-max) (point-min)) 8)
             (user-error "Buffer is too short to hold a database header")
           (g-mode--write-bytes (point-min) (g-mode--canonical-header-bytes))))))))

(defun g-mode--inspector-repair-magic2 (record)
  "Repair the trailing Magic2 byte described by RECORD."
  (let ((candidate-end (cdr (assq 'candidate-end record)))
        (bin-buf (with-current-buffer g-mode--inspector-source-buffer
                   g-mode--binary-buffer)))
    (unless candidate-end
      (user-error "No candidate footer position is available for this record"))
    (g-mode--inspector-apply-change
     (lambda ()
       (with-current-buffer bin-buf
         (when (> candidate-end (point-max))
           (user-error "Candidate footer position lies beyond the end of buffer"))
         (g-mode--write-byte (1- candidate-end) g-mode-magic2))))))

(defun g-mode--inspector-rewrite-free (record)
  "Rewrite RECORD's span as a canonical free-space object."
  (let* ((pos (cdr (assq 'pos record)))
         (length (cdr (assq 'length record)))
         (bin-buf (with-current-buffer g-mode--inspector-source-buffer
                    g-mode--binary-buffer)))
    (g-mode--inspector-apply-change
     (lambda ()
       (with-current-buffer bin-buf
         (g-mode--write-bytes pos (g-mode--make-free-object-bytes length)))))))

(defun g-mode--insert-button (label action &optional help)
  "Insert a text button with LABEL, ACTION, and optional HELP."
  (insert-text-button label
                      'action (lambda (_button) (funcall action))
                      'follow-link t
                      'help-echo help)
  (insert " "))

(defun g-mode--insert-inspector-line (label value)
  "Insert LABEL and VALUE on one line."
  (insert (format "%-12s %s\n" label value)))

(defun g-mode--render-diagnostics (diagnostics)
  "Insert DIAGNOSTICS into the current inspector buffer."
  (insert "Diagnostics\n")
  (insert "-----------\n")
  (if diagnostics
      (dolist (diag diagnostics)
        (insert (format "%s: %s\n"
                        (g-mode--diagnostic-level-label diag)
                        (cdr (assq 'message diag)))))
    (insert "No diagnostics.\n"))
  (insert "\n"))

(defun g-mode--render-inspector-actions (record)
  "Insert contextual repair/edit actions for RECORD."
  (insert "Actions\n")
  (insert "-------\n")
  (cond
   ((eq (cdr (assq 'kind record)) 'header)
    (g-mode--insert-button
     "[Rewrite Canonical Header]"
     #'g-mode--inspector-repair-header
     "Repair the header bytes to the canonical DB5 form."))
   ((cdr (assq 'corrupt record))
    (when (cdr (assq 'candidate-end record))
      (g-mode--insert-button
       "[Repair Magic2]"
       (lambda () (g-mode--inspector-repair-magic2 record))
       "Set the candidate footer byte to the DB5 Magic2 value."))
    (when (and (>= (cdr (assq 'length record)) 8)
               (zerop (% (cdr (assq 'length record)) 8)))
      (g-mode--insert-button
       "[Rewrite As Free Object]"
       (lambda () (g-mode--inspector-rewrite-free record))
       "Rewrite this span as a canonical free-space object.")))
   (t
   (when (cdr (assq 'name record))
      (g-mode--insert-button
       "[Rename]"
       (lambda () (g-mode--inspector-run-ui-command #'g-mode-rename-object))))
    (g-mode--insert-button
     (if (= (logand (cdr (assq 'hflags record)) #x03) 2)
         "[Undelete]"
       "[Delete]")
     (lambda () (g-mode--inspector-run-ui-command #'g-mode-delete-object)))
    (g-mode--insert-button
     "[Edit HFlags]"
     (lambda () (g-mode--edit-byte-at-offset record 1 "HFlags"))
     "Edit HFlags directly as a raw hex byte.")
    (g-mode--insert-button
     "[Edit AFlags]"
     (lambda () (g-mode--edit-byte-at-offset record 2 "AFlags"))
     "Edit AFlags directly as a raw hex byte.")
    (g-mode--insert-button
     "[Edit BFlags]"
     (lambda () (g-mode--edit-byte-at-offset record 3 "BFlags"))
     "Edit BFlags directly as a raw hex byte.")
    (g-mode--insert-button
     "[Edit Type]"
     (lambda () (g-mode--inspector-edit-type record))
     "Edit major/minor type bytes directly.")))
  (insert "\n\n"))

(defun g-mode--render-inspector (record)
  "Render RECORD in the current inspector buffer."
  (let* ((name (or (cdr (assq 'name record))
                   (if (eq (cdr (assq 'kind record)) 'header)
                       "<database header>"
                     "<unnamed>")))
         (major (cdr (assq 'major-type record)))
         (minor (cdr (assq 'minor-type record))))
    (insert (format "Record: %s\n\n" name))
    (g-mode--insert-inspector-line "Kind" (symbol-name (or (cdr (assq 'kind record)) 'object)))
    (g-mode--insert-inspector-line "Offset" (number-to-string (cdr (assq 'pos record))))
    (g-mode--insert-inspector-line "Length" (number-to-string (cdr (assq 'length record))))
    (when (assq 'candidate-length record)
      (g-mode--insert-inspector-line "Candidate Len"
                                     (number-to-string (cdr (assq 'candidate-length record)))))
    (when (assq 'candidate-end record)
      (g-mode--insert-inspector-line "Candidate End"
                                     (number-to-string (cdr (assq 'candidate-end record)))))
    (when (assq 'hflags record)
      (g-mode--insert-inspector-line "HFlags" (format "%02X" (cdr (assq 'hflags record)))))
    (when (assq 'aflags record)
      (g-mode--insert-inspector-line "AFlags" (format "%02X" (cdr (assq 'aflags record)))))
    (when (assq 'bflags record)
      (g-mode--insert-inspector-line "BFlags" (format "%02X" (cdr (assq 'bflags record)))))
    (when (and major minor)
      (g-mode--insert-inspector-line "Type"
                                     (format "%02X:%02X %s"
                                             major minor
                                             (g-mode--get-type-name major minor))))
    (when (cdr (assq 'name record))
      (g-mode--insert-inspector-line "Name" (cdr (assq 'name record))))
    (when (assq 'attribute-length record)
      (g-mode--insert-inspector-line "Attrs"
                                     (if (cdr (assq 'attribute-length record))
                                         (number-to-string (cdr (assq 'attribute-length record)))
                                       "none")))
    (when (assq 'body-length record)
      (g-mode--insert-inspector-line "Body"
                                     (if (cdr (assq 'body-length record))
                                         (number-to-string (cdr (assq 'body-length record)))
                                       "none")))
    (insert "\n")
    (g-mode--render-diagnostics (g-mode--get-diagnostics record))
    (when (and (eq (cdr (assq 'kind record)) 'object)
               (not (cdr (assq 'corrupt record))))
      (let ((attrs (with-current-buffer
                       (with-current-buffer g-mode--inspector-source-buffer
                         g-mode--binary-buffer)
                     (g-mode--parse-attributes record))))
        (insert "Attributes\n")
        (insert "----------\n")
        (if attrs
            (dolist (attr attrs)
              (insert (format "%s: %s\n" (car attr) (cdr attr))))
          (insert "None.\n"))
        (insert "\n")))
    (g-mode--render-inspector-actions record)))

(defun g-mode-view-object ()
  "Display an inspector for the record at point."
  (interactive)
  (let* ((ui-buf (current-buffer))
         (id (tabulated-list-get-id))
         (record (g-mode--lookup-record id)))
    (unless record
      (user-error "No object under point"))
    (let* ((name (or (cdr (assq 'name record))
                     (if (eq id :header) "header" "unnamed")))
           (buf-name (format "*g-mode: %s*" name)))
      (with-current-buffer (get-buffer-create buf-name)
        (g-mode-inspector-mode)
        (setq g-mode--inspector-source-buffer ui-buf)
        (setq g-mode--inspector-record-id id)
        (g-mode--inspector-refresh)
        (display-buffer (current-buffer))))))

(defun g-mode--write-byte (pos byte)
  "Write a single BYTE at POS in the current buffer, overwriting 1 char."
  (save-excursion
    (goto-char pos)
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert byte))))

(defun g-mode--set-dli-at (pos bin-buf dli)
  "Set DLI bits for object at POS in BIN-BUF to DLI (0, 1, or 2).
This implements a non-destructive `soft' status change that only
modifies the DLI bits (0-1) of the hflags byte, ensuring other
metadata like name-presence and width codes are preserved."
  (with-current-buffer bin-buf
    (let* ((hflags (char-after (+ pos 1)))
           (new-hflags (logior (logand hflags #xFC) (logand dli #x03))))
      (g-mode--write-byte (+ pos 1) new-hflags))))

(defun g-mode-delete-object ()
  "Toggle the `Deleted' (Free Space) status of the object at point."
  (interactive)
  (let* ((pos (tabulated-list-get-id))
         (obj (cl-find pos g-mode--objects :key (lambda (o) (cdr (assq 'pos o)))))
         (bin-buf g-mode--binary-buffer))
    (unless obj
      (user-error "No object under point"))
    ;; Read hflags live from the binary buffer
    (let* ((pos (cdr (assq 'pos obj)))
           (live-hflags (with-current-buffer bin-buf
                          (char-after (+ pos 1))))
           (dli (logand live-hflags #x03)))
      (cond
       ((= dli 1)
        (user-error "Cannot delete/undelete the database header object"))
       ((= dli 2)
        ;; Undelete: set DLI back to 0 (Application Data)
        (g-mode--set-dli-at pos bin-buf 0)
        (setq g-mode--session-deleted-objects (delete pos g-mode--session-deleted-objects))
        (message "Undeleted object '%s'." (or (cdr (assq 'name obj)) "unnamed")))
       (t
        ;; Delete: set DLI to 2 (Free Store)
        (g-mode--set-dli-at pos bin-buf 2)
        (cl-pushnew pos g-mode--session-deleted-objects)
        (message "Marked object '%s' as Free Space." (or (cdr (assq 'name obj)) "unnamed"))))
      (g-mode--update-ui)
      (forward-line 1))))

(defun g-mode--interior-size (obj bin-buf)
  "Calculate exact byte size of Interior Data for OBJ.
Includes Attribute_Length + Attribute_Data and Body_Length + Body_Data.
Does not include padding or Magic2 footer.  Result is clamped to the
maximum possible interior span to guard against corrupt length fields."
  (ignore bin-buf)
  (or (cdr (assq 'interior-size obj)) 0))

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

(defun g-mode--get-targets ()
  "Return list of marked object IDs in order, or just the ID at point if none."
  (or (and g-mode--marked-objects (reverse g-mode--marked-objects))
      (let ((id (tabulated-list-get-id)))
        (if (integerp id) (list id) nil))))

(defun g-mode-mark ()
  "Mark the object at point for bulk operations."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (integerp id)
      (cl-pushnew id g-mode--marked-objects)
      (tabulated-list-put-tag "*")
      (forward-line 1))))

(defun g-mode-unmark ()
  "Unmark the object at point. If soft-deleted in this session, undeletes it."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (integerp id)
      (setq g-mode--marked-objects (delete id g-mode--marked-objects))
      (tabulated-list-put-tag " ")
      ;; Check undelete
      (when (member id g-mode--session-deleted-objects)
        (let* ((objects (with-current-buffer g-mode--binary-buffer (g-mode--scan-buffer)))
               (obj (cl-find id objects :key (lambda (o) (cdr (assq 'pos o))))))
          (when (and obj (= (logand (cdr (assq 'hflags obj)) #x03) 2))
             (g-mode--set-dli-at id g-mode--binary-buffer 0)
             (setq g-mode--session-deleted-objects (delete id g-mode--session-deleted-objects))
             (message "Undeleted object '%s'." (or (cdr (assq 'name obj)) "unnamed"))
             (g-mode--update-ui)
             (forward-line -1))))
      (forward-line 1))))

(defun g-mode-unmark-backward ()
  "Move up one line and unmark/undelete."
  (interactive)
  (forward-line -1)
  (g-mode-unmark))

(defun g-mode-unmark-all-marks ()
  "Clear all marks. Undeletes any objects soft-deleted in this session."
  (interactive)
  (setq g-mode--marked-objects nil)
  (let ((undeleted 0))
    (dolist (id g-mode--session-deleted-objects)
      (let* ((objects (with-current-buffer g-mode--binary-buffer (g-mode--scan-buffer)))
             (obj (cl-find id objects :key (lambda (o) (cdr (assq 'pos o))))))
        (when (and obj (= (logand (cdr (assq 'hflags obj)) #x03) 2))
          (g-mode--set-dli-at id g-mode--binary-buffer 0)
          (cl-incf undeleted))))
    (setq g-mode--session-deleted-objects nil)
    (g-mode--update-ui)
    (message "Cleared all marks%s" 
             (if (> undeleted 0) (format " and undeleted %d objects." undeleted) "."))))

(defun g-mode-toggle-marks ()
  "Toggle the `*` mark on all visible objects."
  (interactive)
  (let ((new-marks nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id)))
          (when (and (integerp id) (not (member id g-mode--marked-objects)))
            (cl-pushnew id new-marks)))
        (forward-line 1)))
    (setq g-mode--marked-objects (nreverse new-marks))
    (g-mode--update-ui)))

(defun g-mode-mark-regexp (regexp)
  "Mark all objects whose name matches REGEXP."
  (interactive "sMark (regexp): ")
  (let ((marked 0)
        (objects (with-current-buffer g-mode--binary-buffer (g-mode--scan-buffer))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id)))
          (when (integerp id)
            (let* ((obj (cl-find id objects :key (lambda (o) (cdr (assq 'pos o)))))
                   (name (cdr (assq 'name obj))))
               (when (and name (string-match regexp name)
                          (not (member id g-mode--marked-objects)))
                 (cl-pushnew id g-mode--marked-objects)
                 (cl-incf marked)))))
        (forward-line 1)))
    (g-mode--update-ui)
    (message "Marked %d objects." marked)))

(defun g-mode-view-object-other-window ()
  "View object in other window."
  (interactive)
  (let ((pop-up-windows t))
    (g-mode-view-object)))

(defun g-mode-revert ()
  "Revert the binary buffer, discarding changes, and refresh UI."
  (interactive)
  (let ((file buffer-file-name))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (when (or (not (buffer-modified-p))
              (y-or-n-p (format "Discard changes to %s? " (file-name-nondirectory file))))
      (with-current-buffer g-mode--binary-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (set-buffer-modified-p nil)
          (buffer-disable-undo)
          (buffer-enable-undo)))
      (setq g-mode--marked-objects nil)
      (setq g-mode--session-deleted-objects nil)
      (set-buffer-modified-p nil)
      (g-mode--update-ui)
      (message "Reverted database from disk."))))

(defun g-mode-undo ()
  "Undo the last mutation in the binary buffer and refresh."
  (interactive)
  (with-current-buffer g-mode--binary-buffer
    (undo))
  (g-mode--update-ui)
  (message "Undo successful."))

(defun g-mode-rename-object ()
  "Rename marked objects (or the object at point). If shorter or same length,
rename in-place. If longer, append a new copy and mark old as Free Space."
  (interactive)
  (let ((targets (g-mode--get-targets))
        (bin-buf g-mode--binary-buffer))
    (unless targets (user-error "No objects selected"))
    (dolist (pos targets)
      (let* ((objects (with-current-buffer bin-buf (g-mode--scan-buffer)))
             (obj (cl-find pos objects :key (lambda (o) (cdr (assq 'pos o))))))
        (when obj
          (let* ((old-name (cdr (assq 'name obj)))
                 (hflags (cdr (assq 'hflags obj))))
            (unless old-name (user-error "Cannot rename an unnamed or Free Space object"))
            (let ((new-name (read-string (format "Rename '%s' to: " old-name) old-name)))
              (unless (string= old-name new-name)
                
                (let* ((old-nlen (1+ (length old-name)))
                       (new-nlen (1+ (length new-name)))
                       (name-pos (- (cdr (assq 'interior-pos obj)) old-nlen)))
                  (if (<= new-nlen old-nlen)
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
                            (goto-char nlen-field-pos)
                            (delete-char nlen-bytes)
                            (insert (g-mode--uint-to-bytes new-nlen nlen-bytes))
                            (delete-region name-pos magic2-pos)
                            (goto-char name-pos)
                            (insert replacement))))
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
                        
                        (g-mode--set-dli-at (cdr (assq 'pos obj)) bin-buf 2)
                        (cl-pushnew (cdr (assq 'pos obj)) g-mode--session-deleted-objects))))
                  (message "Renamed '%s' to '%s'." old-name new-name))))))))
    (setq g-mode--marked-objects nil)
    (g-mode--update-ui)))

(defun g-mode-copy-object ()
  "Copy marked objects (or the object at point). Prompts for new names."
  (interactive)
  (let ((targets (g-mode--get-targets))
        (bin-buf g-mode--binary-buffer))
    (unless targets (user-error "No objects selected"))
    (dolist (pos targets)
      (let* ((objects (with-current-buffer bin-buf (g-mode--scan-buffer)))
             (obj (cl-find pos objects :key (lambda (o) (cdr (assq 'pos o))))))
        (when obj
          (let* ((old-name (cdr (assq 'name obj)))
                 (hflags (cdr (assq 'hflags obj))))
            (unless old-name (user-error "Cannot copy unnamed or Free Space objects"))
            (let ((new-name (read-string (format "Copy '%s' to: " old-name) old-name)))
              (when (string= old-name new-name)
                (user-error "Cannot copy to the same name"))
              
              (let* ((new-nlen (1+ (length new-name)))
                     (n-res (g-mode--calc-width-prefix new-nlen))
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
                  (message "Copied '%s' to '%s'." old-name new-name))))))))
    (setq g-mode--marked-objects nil)
    (g-mode--update-ui)))

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
        (g-mode--update-ui)))))

(defun g-mode--swap-adjacent (idx)
  "Swap object at IDX with object at IDX+1 in the binary buffer and memory."
  (let* ((obj-a (nth idx g-mode--objects))
         (obj-b (nth (1+ idx) g-mode--objects))
         (pos-a (cdr (assq 'pos obj-a)))
         (len-a (cdr (assq 'length obj-a)))
         (pos-b (cdr (assq 'pos obj-b)))
         (len-b (cdr (assq 'length obj-b)))
         (bin-buf g-mode--binary-buffer))
    (with-current-buffer bin-buf
      (let ((inhibit-read-only t))
        (let ((data-a (buffer-substring-no-properties pos-a (+ pos-a len-a))))
          (delete-region pos-a (+ pos-a len-a))
          (goto-char (+ pos-a len-b))
          (insert data-a))))
    
    (let* ((new-pos-b pos-a)
           (new-pos-a (+ pos-a len-b))
           (a-marked (member pos-a g-mode--marked-objects))
           (b-marked (member pos-b g-mode--marked-objects))
           (a-deleted (member pos-a g-mode--session-deleted-objects))
           (b-deleted (member pos-b g-mode--session-deleted-objects)))
      (setq g-mode--marked-objects (delete pos-a (delete pos-b g-mode--marked-objects)))
      (setq g-mode--session-deleted-objects (delete pos-a (delete pos-b g-mode--session-deleted-objects)))
      (when a-marked (push new-pos-a g-mode--marked-objects))
      (when b-marked (push new-pos-b g-mode--marked-objects))
      (when a-deleted (push new-pos-a g-mode--session-deleted-objects))
      (when b-deleted (push new-pos-b g-mode--session-deleted-objects))
      
      (setcdr (assq 'pos obj-a) new-pos-a)
      (setcdr (assq 'interior-pos obj-a) (+ new-pos-a (- (cdr (assq 'interior-pos obj-a)) pos-a)))
      (setcdr (assq 'pos obj-b) new-pos-b)
      (setcdr (assq 'interior-pos obj-b) (+ new-pos-b (- (cdr (assq 'interior-pos obj-b)) pos-b)))
      
      (when (cdr (assq 'attribute-data-pos obj-a))
        (setcdr (assq 'attribute-data-pos obj-a) (+ new-pos-a (- (cdr (assq 'attribute-data-pos obj-a)) pos-a))))
      (when (cdr (assq 'body-data-pos obj-a))
        (setcdr (assq 'body-data-pos obj-a) (+ new-pos-a (- (cdr (assq 'body-data-pos obj-a)) pos-a))))
      (when (cdr (assq 'attribute-data-pos obj-b))
        (setcdr (assq 'attribute-data-pos obj-b) (+ new-pos-b (- (cdr (assq 'attribute-data-pos obj-b)) pos-b))))
      (when (cdr (assq 'body-data-pos obj-b))
        (setcdr (assq 'body-data-pos obj-b) (+ new-pos-b (- (cdr (assq 'body-data-pos obj-b)) pos-b))))
      
      (let ((cell-a (nthcdr idx g-mode--objects)))
        (setcar cell-a obj-b)
        (setcar (cdr cell-a) obj-a)))))

(defun g-mode-move-up ()
  "Move selected objects UP one logical position."
  (interactive)
  (let* ((targets (g-mode--get-targets))
         (focus-id (tabulated-list-get-id))
         (focus-obj (and focus-id (cl-find focus-id g-mode--objects :key (lambda (o) (cdr (assq 'pos o)))))))
    (unless targets (user-error "No objects selected"))
    (let* ((marked-objs (cl-remove-if-not (lambda (o) (member (cdr (assq 'pos o)) targets)) g-mode--objects)))
      (with-current-buffer g-mode--binary-buffer (undo-boundary))
      (dolist (obj marked-objs)
        (let ((idx (cl-position obj g-mode--objects)))
          (when (and idx (> idx 0))
            (let ((prev-obj (nth (1- idx) g-mode--objects)))
              (unless (memq prev-obj marked-objs)
                (g-mode--swap-adjacent (1- idx)))))))
      (with-current-buffer g-mode--binary-buffer (undo-boundary))
      (g-mode--update-ui)
      (when focus-obj
        (g-mode--goto-record (cdr (assq 'pos focus-obj)))))))

(defun g-mode-move-down ()
  "Move selected objects DOWN one logical position."
  (interactive)
  (let* ((targets (g-mode--get-targets))
         (focus-id (tabulated-list-get-id))
         (focus-obj (and focus-id (cl-find focus-id g-mode--objects :key (lambda (o) (cdr (assq 'pos o)))))))
    (unless targets (user-error "No objects selected"))
    (let* ((target-objs (cl-remove-if-not (lambda (o) (member (cdr (assq 'pos o)) targets)) g-mode--objects))
           (marked-objs (reverse target-objs)))
      (with-current-buffer g-mode--binary-buffer (undo-boundary))
      (dolist (obj marked-objs)
        (let ((idx (cl-position obj g-mode--objects)))
          (when (and idx (< idx (1- (length g-mode--objects))))
            (let ((next-obj (nth (1+ idx) g-mode--objects)))
              (unless (memq next-obj marked-objs)
                (g-mode--swap-adjacent idx))))))
      (with-current-buffer g-mode--binary-buffer (undo-boundary))
      (g-mode--update-ui)
      (when focus-obj
        (g-mode--goto-record (cdr (assq 'pos focus-obj)))))))

(defvar g-mode-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'g-mode-mark)
    (define-key map (kbd "u") 'g-mode-unmark)
    (define-key map (kbd "U") 'g-mode-unmark-all-marks)
    (define-key map (kbd "DEL") 'g-mode-unmark-backward)
    (define-key map (kbd "t") 'g-mode-toggle-marks)
    (define-key map (kbd "% m") 'g-mode-mark-regexp)
    (define-key map (kbd "C") 'g-mode-copy-object)
    (define-key map (kbd "g") 'g-mode-revert)
    (define-key map (kbd "C-_") 'g-mode-undo)
    (define-key map (kbd "C-x u") 'g-mode-undo)
    (define-key map (kbd "<undo>") 'g-mode-undo)
    (define-key map (kbd "o") 'g-mode-view-object-other-window)
    (define-key map (kbd "x") 'g-mode-garbage-collect)
    
    (define-key map (kbd "v") 'g-mode-view-object)
    (define-key map (kbd "h") 'g-mode-toggle-show-deleted)
    (define-key map (kbd "f") 'g-mode-filter)
    (define-key map (kbd "/") 'g-mode-filter)
    (define-key map (kbd "RET") 'g-mode-view-object)
    (define-key map (kbd "d") 'g-mode-delete-object)
    (define-key map (kbd "R") 'g-mode-rename-object)
    (define-key map (kbd "G") 'g-mode-garbage-collect)
    (define-key map (kbd "s") 'save-buffer)
    (define-key map (kbd "C-x C-s") 'save-buffer)
    (define-key map (kbd "<M-up>") 'g-mode-move-up)
    (define-key map (kbd "M-<up>") 'g-mode-move-up)
    (define-key map (kbd "ESC <up>") 'g-mode-move-up)
    (define-key map (kbd "<M-down>") 'g-mode-move-down)
    (define-key map (kbd "M-<down>") 'g-mode-move-down)
    (define-key map (kbd "ESC <down>") 'g-mode-move-down)
    (define-key map (kbd "?") 'g-mode-help)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `g-mode-ui-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.g\\'" . g-mode))

;;;###autoload
(add-to-list 'file-coding-system-alist '("\\.g\\'" . no-conversion))

(define-derived-mode g-mode-ui-mode tabulated-list-mode "BRL-CAD"
  "UI mode for browsing BRL-CAD database objects.
\\{g-mode-ui-mode-map}"
  (setq tabulated-list-format [("#" 4 t)
                               ("Name" 30 t)
                               ("Type" 25 t)
                               ("Id" 6 t)
                               ("Offset" 8 t)
                               ("Size"  8 t)
                               ("Flags" 6 nil)])
  (setq tabulated-list-padding 2)
  (setq header-line-format " m:mark u:unmk U:unmk-all d:del x:gc C:copy C-_:undo g:revert ?:help M-up:move-up M-dn:move-dn")
  (setq buffer-read-only t))

(defun g-mode--write-contents ()
  "Save the changes in the hidden binary buffer to the visited file."
  (let ((file buffer-file-name)
        (file-precious-flag t))
    (if (not (buffer-live-p g-mode--binary-buffer))
        (error "Binary buffer is no longer live")
      (with-current-buffer g-mode--binary-buffer
        ;; Use 'quiet to avoid changing the binary buffer's visited file or spamming messages
        (write-region (point-min) (point-max) file nil 'quiet)))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    (message "Database saved.")
    t))

(defun g-mode--kill-binary-buffer ()
  "Kill the hidden binary buffer when the UI buffer is killed."
  (when (buffer-live-p g-mode--binary-buffer)
    (kill-buffer g-mode--binary-buffer)))

(defun g-mode-help ()
  (message "v:view d:del m:mark u/U:unmk x:gc C:copy C-_:undo g:rev ?:help"))

(defun g-mode ()
  "Major mode wrapper for BRL-CAD .g files.
Maintains a hidden binary buffer and uses the current buffer as the UI."
  (interactive)
  (when (= (buffer-size) 0)
    (error "Buffer is empty; there is no database content to inspect"))
  (let* ((original-buf (current-buffer))
         (file-name (buffer-name))
         (hidden-name (generate-new-buffer-name (format " *g-binary %s*" file-name)))
         (bin-buf (generate-new-buffer hidden-name)))
    (with-current-buffer bin-buf
      (set-buffer-multibyte nil)
      (insert-buffer-substring-no-properties original-buf)
      (set-buffer-modified-p nil)
      (buffer-disable-undo)
      (buffer-enable-undo))
    (set-buffer-multibyte t)
    (g-mode-ui-mode)
    (setq g-mode--binary-buffer bin-buf)
    (add-hook 'write-contents-functions #'g-mode--write-contents nil t)
    (add-hook 'kill-buffer-hook #'g-mode--kill-binary-buffer nil t)
    (auto-save-mode -1)
    (buffer-disable-undo)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (set-buffer-modified-p nil)
    (g-mode--update-ui)
    (current-buffer)))

(provide 'g-mode)

;;; g-mode.el ends here
