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

(ert-deftest g-mode-parse-object-test ()
  "Test that generic objects are correctly parsed."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "references/geometry/moss.g")
    
    ;; First object after header is Free space (at byte 9)
    ;; Note: Emacs points are 1-indexed, so byte index 8 is point 9.
    (let ((obj1 (g-mode--parse-object 9)))
      (should obj1)
      (should (eq (cdr (assq 'magic1 obj1)) #x76))
      (should (eq (cdr (assq 'length obj1)) 96))
      (should (not (assq 'name obj1))))
    
    ;; Second object starts at 9 + 96 = 105
    (let ((obj2 (g-mode--parse-object 105)))
      (should obj2)
      (should (eq (cdr (assq 'magic1 obj2)) #x76))
      (should (eq (cdr (assq 'length obj2)) 80))
      (should (equal (cdr (assq 'name obj2)) "tor")))))

(ert-deftest g-mode-scan-buffer-test ()
  "Test scanning an entire file."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "references/geometry/moss.g")
    (let ((objects (g-mode--scan-buffer)))
      (should (> (length objects) 10))
      (should (eq (cdr (assq 'magic1 (car objects))) #x76))
      (should (equal (cdr (assq 'length (car objects))) 96)))))

(ert-deftest g-mode-ui-test ()
  "Test the tabulated-list UI initialization."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "references/geometry/moss.g")
    (g-mode)
    (should g-mode--objects)
    (should tabulated-list-entries)
    (should (> (length tabulated-list-entries) 10))
    ;; Ensure that the buffer text has been modified to show tabulated-list
    (should (string-match-p "tor" (buffer-string)))))

(provide 'g-mode-test)
;;; g-mode-test.el ends here
