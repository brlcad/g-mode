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
    (insert-file-contents-literally "test/moss.g")
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
    (insert-file-contents-literally "test/moss.g")
    
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
    (insert-file-contents-literally "test/moss.g")
    (let ((objects (g-mode--scan-buffer)))
      (should (> (length objects) 10))
      (should (eq (cdr (assq 'magic1 (car objects))) #x76))
      (should (equal (cdr (assq 'length (car objects))) 96)))))

(defmacro with-g-mode-test-setup (filename &rest body)
  "Set up a g-mode test environment with FILENAME and clean up afterwards."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (buffer-enable-undo)
     (set-buffer-multibyte nil)
     (insert-file-contents-literally ,filename)
     (let* ((bin-buf (current-buffer))
            (ui-buf (g-mode)))
       (unwind-protect
           (with-current-buffer ui-buf
             ,@body)
         ;; bin-buf is handled by with-temp-buffer normally,
         ;; but g-mode sets buffer-read-only, which might cause issues
         ;; if with-temp-buffer tries to erase it.
         (when (buffer-live-p bin-buf)
           (with-current-buffer bin-buf (setq buffer-read-only nil)))
         (when (buffer-live-p ui-buf)
           (kill-buffer ui-buf))))))

(ert-deftest g-mode-ui-test ()
  "Test the tabulated-list UI initialization."
  (with-g-mode-test-setup "test/moss.g"
                          (should g-mode--objects)
                          (should tabulated-list-entries)
                          (should (> (length tabulated-list-entries) 10))
                          (should (string-match-p "tor" (buffer-string)))))

(ert-deftest g-mode-attributes-test ()
  "Test attribute parsing on an object with attributes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "test/moss.g")
    (let* ((objects (g-mode--scan-buffer))
           (global-obj (cl-find "_GLOBAL" objects :key (lambda (o) (cdr (assq 'name o))) :test 'equal)))
      (should global-obj)
      (let ((attrs (g-mode--parse-attributes global-obj)))
        (should attrs)
        ;; typically there is a 'title' or 'units' attribute
        (should (assoc "title" attrs))))))

(ert-deftest g-mode-delete-undelete-test ()
  "Test marking an object as Free Space and then undeleting it."
  (with-g-mode-test-setup "test/moss.g"
                          (let ((orig-len (length g-mode--objects)))
                            ;; Find "tor"
                            (goto-char (point-min))
                            (while (and (not (eobp))
                                        (not (equal "tor" (aref (tabulated-list-get-entry (point)) 1))))
                              (forward-line 1))
                            (should-not (eobp))
                            
                            ;; 1. Execute delete
                            (g-mode-delete-object)
                            ;; It moved down, so move back up
                            (forward-line -1)
                            ;; In UI, it should now show as <Free Space>
                            (should (equal "<Free Space>" (aref (tabulated-list-get-entry (point)) 1)))
                            
                            ;; 2. Execute undelete
                            (g-mode-delete-object)
                            (forward-line -1)
                            ;; It should be back to "tor"
                            (should (equal "tor" (aref (tabulated-list-get-entry (point)) 1)))
                            (should (= (length g-mode--objects) orig-len))
                            
                            ;; Verify binary buffer is modified
                            (with-current-buffer g-mode--binary-buffer
                              (should (buffer-modified-p))))))

(ert-deftest g-mode-ui-toggle-test ()
  "Test toggling of deleted items in UI."
  (with-g-mode-test-setup "test/moss.g"
                          ;; By default, show-deleted is now t.
                          (should g-mode-show-deleted)
                          (let ((initial-entries (length tabulated-list-entries)))
                            ;; Toggle visibility (was bound to 'v', now 'h')
                            (g-mode-toggle-show-deleted)
                            (should-not g-mode-show-deleted)
                            ;; Now it should be smaller, as it hides the Free Space (deleted) objects!
                            (should (< (length tabulated-list-entries) initial-entries)))))

(ert-deftest g-mode-simple-filter-test ()
  "Test filtering the object list by a simple name match."
  (with-g-mode-test-setup "test/moss.g"
                          (let ((initial-entries (length tabulated-list-entries)))
                            ;; Filter by "tor"
                            (g-mode-filter "tor.r")
                            ;; The header is always kept, plus the "tor.r" object
                            (should (= (length tabulated-list-entries) 2))
                            (should (string-match-p "tor" (aref (cadr (nth 1 tabulated-list-entries)) 1)))
                            
                            ;; Clear filter
                            (g-mode-filter "")
                            (should (= (length tabulated-list-entries) initial-entries)))))

(ert-deftest g-mode-filter-test ()
  "Test filtering the object list by a regular expression."
  (with-g-mode-test-setup "test/moss.g"
                          (let ((initial-entries (length tabulated-list-entries)))
                            ;; Filter by "^to[r]$"
                            (g-mode-filter "^to[r]$")
                            ;; The header is always kept, plus the "tor" object
                            (should (= (length tabulated-list-entries) 2))
                            (should (string-match-p "tor" (aref (cadr (nth 1 tabulated-list-entries)) 1)))
                            
                            ;; Clear filter
                            (g-mode-filter "")
                            (should (= (length tabulated-list-entries) initial-entries)))))

(ert-deftest g-mode-rename-inline-test ()
  "Test in-place rename logic for smaller names."
  (with-g-mode-test-setup "test/moss.g"
                          ;; Force simulated point to "tor" row by searching the UI list
                          (goto-char (point-min))
                          (while (and (not (eobp))
                                      (not (equal "tor" (aref (tabulated-list-get-entry (point)) 1))))
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
  (with-g-mode-test-setup "test/moss.g"
                          (let ((orig-objects (length g-mode--objects)))
                            ;; Force simulated point to "tor" row by searching the UI list
                            (goto-char (point-min))
                            (while (and (not (eobp))
                                        (not (equal "tor" (aref (tabulated-list-get-entry (point)) 1))))
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
  (with-g-mode-test-setup "test/moss.g"
                          (let ((original-size (with-current-buffer g-mode--binary-buffer (buffer-size))))
                            ;; Delete "tor"
                            (goto-char (point-min))
                            (while (and (not (eobp))
                                        (not (equal "tor" (aref (tabulated-list-get-entry (point)) 1))))
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
                              ;; Wait, after GC, there are NO deleted objects, so tabulated-list-entries should match pre-gc-active + 1 for header.
                              (should (= (length tabulated-list-entries) (1+ pre-gc-active)))
                              ;; Total object count should have decreased (deleted objects removed)
                              (should (< (length g-mode--objects) pre-gc-total))
                              ;; Remaining objects should still be parseable
                              (should (cl-find "_GLOBAL" g-mode--objects
                                               :key (lambda (o) (cdr (assq 'name o)))
                                               :test 'equal))))))

(ert-deftest g-mode-mark-unmark-test ()
  "Test marking and unmarking in UI."
  (with-g-mode-test-setup "test/moss.g"
                          (goto-char (point-min))
                          (forward-line 1)
                          (g-mode-mark)
                          (should (= (length g-mode--marked-objects) 1))
                          (forward-line -1)
                          (g-mode-unmark)
                          (should (= (length g-mode--marked-objects) 0))))

(ert-deftest g-mode-copy-test ()
  "Test copying an object."
  (with-g-mode-test-setup "test/moss.g"
                          (let ((orig-objects (length g-mode--objects)))
                            (goto-char (point-min))
                            (while (and (not (eobp))
                                        (not (equal "tor" (aref (tabulated-list-get-entry (point)) 1))))
                              (forward-line 1))
                            (should-not (eobp))
                            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "tor_copied")))
                              (g-mode-copy-object))
                            (should (cl-find "tor_copied" g-mode--objects :key (lambda (o) (cdr (assq 'name o))) :test 'equal))
                            (should (= (length g-mode--objects) (1+ orig-objects))))))

(ert-deftest g-mode-move-up-down-test ()
  "Test moving objects up and down."
  (with-g-mode-test-setup "test/moss.g"
                          (let* ((nameA (cdr (assq 'name (nth 0 g-mode--objects))))
                                 (nameB (cdr (assq 'name (nth 1 g-mode--objects))))
                                 (nameC (cdr (assq 'name (nth 2 g-mode--objects))))
                                 (nameD (cdr (assq 'name (nth 3 g-mode--objects))))
                                 (nameE (cdr (assq 'name (nth 4 g-mode--objects)))))
                            
                            (g-mode--goto-record (cdr (assq 'pos (nth 0 g-mode--objects))))
                            
                            ;; 1. Move A UP -> Should be blocked because it's at index 0
                            (g-mode-move-up)
                            (should (equal (cdr (assq 'name (nth 0 g-mode--objects))) nameA))
                            
                            ;; 2. Move A DOWN by 1
                            (g-mode-move-down)
                            (should (equal (cdr (assq 'name (nth 0 g-mode--objects))) nameB))
                            (should (equal (cdr (assq 'name (nth 1 g-mode--objects))) nameA))
                            
                            ;; 3. Move A DOWN by 3 more
                            (g-mode-unmark-all-marks)
                            (g-mode--goto-record (cdr (assq 'pos (nth 1 g-mode--objects)))) ;; A is at 1
                            (g-mode-mark)
                            (g-mode-move-down)
                            (g-mode-move-down)
                            (g-mode-move-down)
                            (should (equal (cdr (assq 'name (nth 4 g-mode--objects))) nameA))
                            
                            ;; Now order is B, C, D, E, A
                            ;; 4. Move E UP by 1 (E is at index 3)
                            (g-mode-unmark-all-marks)
                            (should (equal (cdr (assq 'name (nth 3 g-mode--objects))) nameE))
                            (g-mode--goto-record (cdr (assq 'pos (nth 3 g-mode--objects))))
                            (g-mode-mark)
                            (g-mode-move-up)
                            (should (equal (cdr (assq 'name (nth 2 g-mode--objects))) nameE))
                            
                            ;; Now order is B, C, E, D, A
                            ;; 5. Test multi-object mark move
                            (g-mode-unmark-all-marks)
                            (g-mode--goto-record (cdr (assq 'pos (nth 0 g-mode--objects)))) ;; B
                            (g-mode-mark)
                            (g-mode--goto-record (cdr (assq 'pos (nth 1 g-mode--objects)))) ;; C
                            (g-mode-mark)
                            
                            ;; Move them DOWN
                            (g-mode-move-down)
                            
                            ;; Order becomes E, B, C, D, A
                            (should (equal (cdr (assq 'name (nth 0 g-mode--objects))) nameE))
                            (should (equal (cdr (assq 'name (nth 1 g-mode--objects))) nameB))
                            (should (equal (cdr (assq 'name (nth 2 g-mode--objects))) nameC))
                            
                            ;; Move out of bounds test
                            (g-mode-unmark-all-marks)
                            (let* ((last-idx (1- (length g-mode--objects)))
                                   (last-name (cdr (assq 'name (nth last-idx g-mode--objects)))))
                              (g-mode--goto-record (cdr (assq 'pos (nth last-idx g-mode--objects))))
                              (g-mode-move-down) ;; should do nothing
                              (should (equal (cdr (assq 'name (nth (1- (length g-mode--objects)) g-mode--objects))) last-name)))
                            
                            (with-current-buffer g-mode--binary-buffer
                              (should (buffer-modified-p))))))

(ert-deftest g-mode-undo-test ()
  "Test that EMACS undo reverses file mutations and refreshes UI."
  (with-g-mode-test-setup "test/moss.g"
                          (goto-char (point-min))
                          (while (and (not (eobp))
                                      (not (equal "tor" (aref (tabulated-list-get-entry (point)) 1))))
                            (forward-line 1))
                          (should-not (eobp))
                          (with-current-buffer g-mode--binary-buffer (undo-boundary))
                          (g-mode-delete-object)
                          (with-current-buffer g-mode--binary-buffer (undo-boundary))
                          (forward-line -1)
                          (should (equal "<Free Space>" (aref (tabulated-list-get-entry (point)) 1)))
                          (g-mode-undo)
                          (should (equal "tor" (aref (tabulated-list-get-entry (point)) 1)))))

(ert-deftest g-mode-invalid-header-open-test ()
  "Invalid headers should still open in recovery mode."
  (with-temp-buffer
    (buffer-enable-undo)
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "test/moss.g")
    (g-mode--write-byte (point-min) 0)
    (let* ((bin-buf (current-buffer))
           (ui-buf (g-mode)))
      (unwind-protect
          (with-current-buffer ui-buf
            (should-not (cdr (assq 'valid g-mode--header-info)))
            (should (g-mode--lookup-record :header))
            (should (> (length g-mode--objects) 0)))
        (when (buffer-live-p ui-buf) (with-current-buffer ui-buf (setq buffer-read-only nil)))
        (when (buffer-live-p ui-buf) (kill-buffer ui-buf))
        (when (buffer-live-p bin-buf) (with-current-buffer bin-buf (setq buffer-read-only nil)))))))

(ert-deftest g-mode-corrupt-object-diagnostics-test ()
  "Malformed objects should surface structured diagnostics."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "test/moss.g")
    (let* ((obj (g-mode--parse-object 105))
           (magic2-pos (1- (+ 105 (cdr (assq 'length obj))))))
      (should obj)
      (g-mode--write-byte magic2-pos 0)
      (let* ((objects (g-mode--scan-buffer))
             (corrupt (cl-find-if (lambda (o) (cdr (assq 'corrupt o))) objects))
             (diag (car (g-mode--get-diagnostics corrupt))))
        (should corrupt)
        (should (eq (cdr (assq 'code diag)) 'bad-magic2))
        (should (assq 'candidate-end corrupt))))))

(ert-deftest g-mode-inspector-repair-header-test ()
  "The inspector should offer and apply header repair in place."
  (with-temp-buffer
    (buffer-enable-undo)
    (set-buffer-multibyte nil)
    (insert-file-contents-literally "test/moss.g")
    (g-mode--write-byte (point-min) 0)
    (let* ((bin-buf (current-buffer))
           (ui-buf (g-mode))
           inspector-buf)
      (unwind-protect
          (progn
            ;; Try parsing the header directly in bin buffer to show it's invalid
            (with-current-buffer g-mode--binary-buffer
              (should-not (cdr (assq 'valid (g-mode--parse-header)))))
            
            ;; Open inspector for the header
            (with-current-buffer ui-buf
              (goto-char (point-min))
              (should (equal :header (tabulated-list-get-id)))
              (g-mode-view-object))
            (setq inspector-buf (get-buffer "*g-mode: <database header>*"))
            (should inspector-buf)
            
            ;; Emulate pressing 'r' (repair) in inspector
            (with-current-buffer inspector-buf
              (should (string-match-p "Rewrite Canonical Header" (buffer-string)))
              (g-mode--inspector-repair-header))
            
            ;; Verify it is now valid in the bin buffer
            (with-current-buffer g-mode--binary-buffer
              (should (g-mode--parse-header)))
            
            (with-current-buffer ui-buf
              (should (cdr (assq 'valid g-mode--header-info)))
              (let ((header-entry (cl-find :header tabulated-list-entries :key #'car :test #'equal)))
                (should header-entry)
                (should (equal "<database header>" (aref (cadr header-entry) 1))))
              (when (buffer-live-p inspector-buf) (kill-buffer inspector-buf))))
        (when (buffer-live-p ui-buf) (with-current-buffer ui-buf (setq buffer-read-only nil)))
        (when (buffer-live-p ui-buf) (kill-buffer ui-buf))
        (when (buffer-live-p bin-buf) (with-current-buffer bin-buf (setq buffer-read-only nil)))))))

(provide 'g-mode-test)
;;; g-mode-test.el ends here
