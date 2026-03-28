;;; g-mode-test.el --- Tests for g-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christopher Sean Morrison

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Unit tests for `g-mode' parsing and functionality.

;;; Code:

(require 'ert)
(require 'g-mode (expand-file-name "../g-mode.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest g-mode-basic-test ()
  "Ensure the g-mode feature loads correctly and provides tabulated-list capabilities."
  (should (fboundp 'g-mode))
  (with-temp-buffer
    (g-mode)
    (should (eq major-mode 'g-mode))))

(ert-deftest g-mode-parse-header-test ()
  "Test that the db header is correctly identified using the bindat type."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "references/geometry/moss.g")
    (let ((header (g-mode--parse-header)))
      (should header)
      (should (eq (cdr (assq 'magic1 header)) #x76))
      (should (eq (cdr (assq 'magic2 header)) #x35))
      (should (eq (cdr (assq 'length header)) 1))
      (should (eq (cdr (assq 'hflags header)) 1)))))

(provide 'g-mode-test)
;;; g-mode-test.el ends here
