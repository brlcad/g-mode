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
