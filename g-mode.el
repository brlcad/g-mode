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
