;;; g-mode-test.el --- Tests for g-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christopher Sean Morrison

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Unit tests for `g-mode' parsing and functionality.

;;; Code:

(require 'ert)
(require 'g-mode (expand-file-name "../g-mode.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest g-mode-basic-test ()
  "Ensure the g-mode feature loads correctly and intercepts invalid magically."
  (should (fboundp 'g-mode))
  (with-temp-buffer
    (should-error (g-mode))))

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

(defmacro with-g-mode-test-setup (filename &rest body)
  "Set up a g-mode test environment with FILENAME and clean up afterwards."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally ,filename)
     (let* ((bin-buf (current-buffer))
            (ui-buf (g-mode)))
       (unwind-protect
           (with-current-buffer ui-buf
             ,@body)
         (when (buffer-live-p ui-buf) (kill-buffer ui-buf))
         ;; bin-buf is handled by with-temp-buffer normally,
         ;; but g-mode sets buffer-read-only, which might cause issues
         ;; if with-temp-buffer tries to erase it.
         (with-current-buffer bin-buf (setq buffer-read-only nil))))))

(ert-deftest g-mode-ui-test ()
  "Test the tabulated-list UI initialization."
  (with-g-mode-test-setup "references/geometry/moss.g"
    (should g-mode--objects)
    (should tabulated-list-entries)
    (should (> (length tabulated-list-entries) 10))
    (should (string-match-p "tor" (buffer-string)))))

(ert-deftest g-mode-attributes-test ()
  "Test attribute parsing on an object with attributes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "references/geometry/moss.g")
    (let* ((objects (g-mode--scan-buffer))
           (global-obj (cl-find "_GLOBAL" objects :key (lambda (o) (cdr (assq 'name o))) :test 'equal)))
      (should global-obj)
      (let ((attrs (g-mode--parse-attributes global-obj)))
        (should attrs)
        ;; typically there is a 'title' or 'units' attribute
        (should (assoc "title" attrs))))))

(ert-deftest g-mode-delete-undelete-test ()
  "Test marking an object as Free Space and then undeleting it."
  (with-g-mode-test-setup "references/geometry/moss.g"
    (let ((orig-len (length g-mode--objects)))
      ;; Find "tor"
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal "tor" (aref (tabulated-list-get-entry (point)) 0))))
        (forward-line 1))
      (should-not (eobp))
      
      ;; 1. Execute delete
      (g-mode-delete-object)
      ;; In UI, it should now show as <Free Space>
      (should (equal "<Free Space>" (aref (tabulated-list-get-entry (point)) 0)))
      
      ;; 2. Execute undelete
      (g-mode-delete-object)
      ;; It should be back to "tor"
      (should (equal "tor" (aref (tabulated-list-get-entry (point)) 0)))
      (should (= (length g-mode--objects) orig-len))
      
      ;; Verify binary buffer is modified
      (with-current-buffer g-mode--binary-buffer
        (should (buffer-modified-p))))))

(ert-deftest g-mode-ui-toggle-test ()
  "Test toggling of deleted items in UI."
  (with-g-mode-test-setup "references/geometry/moss.g"
    ;; By default, show-deleted is now t.
    (should g-mode-show-deleted)
    (let ((initial-entries (length tabulated-list-entries)))
      ;; Toggle visibility (was bound to 'v', now 'h')
      (g-mode-toggle-show-deleted)
      (should-not g-mode-show-deleted)
      ;; Now it should be smaller, as it hides the Free Space (deleted) objects!
      (should (< (length tabulated-list-entries) initial-entries)))))

(ert-deftest g-mode-rename-inline-test ()
  "Test in-place rename logic for smaller names."
  (with-g-mode-test-setup "references/geometry/moss.g"
    ;; Force simulated point to "tor" row by searching the UI list
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal "tor" (aref (tabulated-list-get-entry (point)) 0))))
      (forward-line 1))
    (should-not (eobp))
    ;; execute rename inline (shorter)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "to")))
      (g-mode-rename-object))
    ;; Verify it was renamed in UI
    (should (cl-find "to" g-mode--objects :key (lambda (o) (cdr (assq 'name o))) :test 'equal))
    ;; Verify binary buffer is modified
    (with-current-buffer g-mode--binary-buffer
      (should (buffer-modified-p)))))

(ert-deftest g-mode-rename-append-test ()
  "Test append rename logic for longer names."
  (with-g-mode-test-setup "references/geometry/moss.g"
    (let ((orig-objects (length g-mode--objects)))
      ;; Force simulated point to "tor" row by searching the UI list
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal "tor" (aref (tabulated-list-get-entry (point)) 0))))
        (forward-line 1))
      (should-not (eobp))
      ;; execute rename append (longer)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "tor_modified")))
        (g-mode-rename-object))
      ;; Verify it was renamed (new object added)
      (should (cl-find "tor_modified" g-mode--objects :key (lambda (o) (cdr (assq 'name o))) :test 'equal))
      ;; Verify old one was marked Free (DLI=2) but is still in objects list (soft-delete)
      (let ((old-tor (cl-find "tor" g-mode--objects :key (lambda (o) (cdr (assq 'name o))) :test 'equal)))
        (should old-tor)
        (should (= (logand (cdr (assq 'hflags old-tor)) #x03) 2)))
      ;; We should have exactly 1 more object overall
      (should (= (length g-mode--objects) (1+ orig-objects)))
      ;; Verify binary buffer is modified
      (with-current-buffer g-mode--binary-buffer
        (should (buffer-modified-p))))))

(ert-deftest g-mode-garbage-collect-test ()
  "Test fault-resilient garbage collection compaction."
  (with-g-mode-test-setup "references/geometry/moss.g"
    (let ((original-size (with-current-buffer g-mode--binary-buffer (buffer-size))))
      ;; Delete "tor"
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal "tor" (aref (tabulated-list-get-entry (point)) 0))))
        (forward-line 1))
      (should-not (eobp))
      (g-mode-delete-object)

      ;; Count objects before GC
      (let* ((pre-gc-total (length g-mode--objects))
             (pre-gc-active (cl-count-if (lambda (o)
                                          (not (or (cdr (assq 'corrupt o))
                                                   (= (logand (cdr (assq 'hflags o)) #x03) 2))))
                                        g-mode--objects)))

        ;; Run GC (mock yes-or-no-p to auto-confirm)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (g-mode-garbage-collect))

        ;; Buffer should have shrunk
        (should (< (with-current-buffer g-mode--binary-buffer (buffer-size))
                   original-size))

        ;; The deleted object should be gone from both the full scan and UI
        (should-not (cl-find "tor" g-mode--objects
                             :key (lambda (o) (cdr (assq 'name o)))
                             :test 'equal))
        ;; Active object count should be unchanged!
        ;; Since show-deleted is t, tabulated-list-entries now reflects ALL objects...
        ;; wait, after GC, there are NO deleted objects, so tabulated-list-entries should match pre-gc-active.
        (should (= (length tabulated-list-entries) pre-gc-active))
        ;; Total object count should have decreased (deleted objects removed)
        (should (< (length g-mode--objects) pre-gc-total))
        ;; Remaining objects should still be parseable
        (should (cl-find "_GLOBAL" g-mode--objects
                         :key (lambda (o) (cdr (assq 'name o)))
                         :test 'equal))))))

(provide 'g-mode-test)
;;; g-mode-test.el ends here
