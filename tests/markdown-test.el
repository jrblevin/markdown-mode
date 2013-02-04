;;;; markdown-test.el --- Tests for markdown-mode

;; Copyright (C) 2013 Jason R. Blevins <jrblevin@sdf.org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains the `markdown-mode' test suite.  To run the tests:
;;
;;     M-x load-file RET markdown-test.el RET
;;     M-x markdown-test RET

;;; Code:

(unless (featurep 'markdown-mode)
  (require 'markdown-mode))

(defconst markdown-test-dir
   (expand-file-name (file-name-directory
                      (or load-file-name buffer-file-name))))

(defmacro markdown-test-string (string &rest body)
  "Run body in a temporary buffer containing STRING."
  `(with-temp-buffer
    (markdown-mode)
    (insert ,string)
    (goto-char (point-min))
    (prog1 ,@body (kill-buffer))))
(def-edebug-spec markdown-test-string (form body))

(defmacro markdown-test-file (file &rest body)
  "Open FILE from `markdown-test-dir' and execute body."
  `(let ((fn (concat markdown-test-dir ,file)))
     (save-window-excursion
       (with-temp-buffer
         (insert-file-contents fn)
         (markdown-mode)
         (goto-char (point-min))
         (font-lock-fontify-buffer)
         ,@body))))
(def-edebug-spec markdown-test-file (form body))

(defmacro markdown-test-temp-file (file &rest body)
  "Open FILE from `markdown-test-dir' visiting temp file and execute body.
This file is not saved."
  `(let ((fn (concat markdown-test-dir ,file))
         (tmp (concat (make-temp-file "markdown-test") ".text"))
         buf)
     (save-window-excursion
       (setq buf (find-file tmp))
       (insert-file-contents fn)
       (markdown-mode)
       (goto-char (point-min))
       (font-lock-fontify-buffer)
       ,@body
       (kill-buffer buf))))
(def-edebug-spec markdown-test-temp-file (form body))

(defun markdown-test-range-has-face (begin end face)
  (let (loc)
    (dolist (loc (number-sequence begin end))
      (should (eq (get-text-property loc 'face) face)))))

(defun markdown-test ()
  "Run all defined tests for `markdown-mode'."
  (interactive)
  (ert "markdown"))

;;; Example tests:

(ert-deftest test-markdown-example/string ()
  "An example string test using the `ert' framework."
  (markdown-test-string "foo *bar* baz"
   (goto-char 5)
   (delete-char 1)
   (should (looking-at "bar"))))

(ert-deftest test-markdown-example/file ()
  "An example file test using the `ert' framework."
  (markdown-test-file "inline.text"
   (goto-char 9)
   (should (looking-at "\*"))))

;;; Basic mode tests:

(ert-deftest test-markdown-mode/variables ()
  "Test `markdown-mode' variables."
  (markdown-test-file "inline.text"
   (should (= tab-width 4))
   (should (eq font-lock-multiline t))
   (should (eq major-mode 'markdown-mode))))

;;; Font lock tests:

(ert-deftest test-markdown-font-lock/italics-1 ()
  "A simple italics test."
  (markdown-test-file "inline.text"
   (goto-char 9)
   (should (looking-at "\*"))
   ;; Check face of char before leading asterisk
   (should (eq (get-text-property 8 'face) nil))
   ;; Check face of leading asterisk
   (should (eq (get-text-property 9 'face) markdown-italic-face))
   ;; Check face of trailing asterisk
   (should (eq (get-text-property 17 'face) markdown-italic-face))
   ;; Check face of point past leading asterisk
   (should (eq (get-text-property 18 'face) nil))))

(ert-deftest test-markdown-font-lock/bold-1 ()
  "A simple bold test."
  (markdown-test-file "inline.text"
   (goto-char 27)
   (should (looking-at "\*\*"))
   ;; Check face of char before leading asterisk
   (should (eq (get-text-property 26 'face) nil))
   ;; Check face of leading asterisk
   (should (eq (get-text-property 27 'face) markdown-bold-face))
   ;; Check face of trailing asterisk
   (should (eq (get-text-property 35 'face) markdown-bold-face))
   ;; Check face of point past leading asterisk
   (should (eq (get-text-property 36 'face) nil))))

(ert-deftest test-markdown-font-lock/code-1 ()
  "A simple inline code test."
  (markdown-test-file "inline.text"
   (goto-char 45)
   (should (looking-at "`"))
   (markdown-test-range-has-face 45 50 markdown-inline-code-face)
   (markdown-test-range-has-face 61 89 markdown-inline-code-face)
   ;; These tests are known to fail:
   ;; (markdown-test-range-has-face 122 128 markdown-inline-code-face)
   ;; (markdown-test-range-has-face 288 296 markdown-inline-code-face)
   ))

;;; Wiki link tests:

(ert-deftest test-markdown-wiki-link/aliasing ()
  "Test filename extraction for aliased wiki links."
  (markdown-test-file "wiki-links.text"
   ;; Confirm location of first wiki link
   (should (eq (markdown-next-wiki-link) 8))
   ;; Confirm location of second wiki link
   (should (eq (markdown-next-wiki-link) 73))
   ;; Test predicate function
   (should (markdown-wiki-link-p))
   ;; Test alias-first filename extraction
   (setq markdown-wiki-link-alias-first t)
   (should (string-equal (markdown-wiki-link-link) "second"))
   ;; Test alias-second filename extraction
   (setq markdown-wiki-link-alias-first nil)
   (should (string-equal (markdown-wiki-link-link) "first"))))

(ert-deftest test-markdown-wiki-link/navigation ()
  "Test wiki link navigation."
  (markdown-test-file "wiki-links.text"
   ;; Advance to first link
   (should (eq (markdown-next-wiki-link) 8))
   ;; Advance to second link
   (should (eq (markdown-next-wiki-link) 73))
   ;; Avance to final link
   (should (eq (markdown-next-wiki-link) 155))
   ;; Return nil and don't advance point
   (should (eq (markdown-next-wiki-link) nil))
   (should (eq (point) 155))
   ;; Move back to second link
   (should (eq (markdown-previous-wiki-link) 73))
   ;; Move back to first link
   (should (eq (markdown-previous-wiki-link) 8))
   ;; Return nil and don't move point
   (should (eq (markdown-previous-wiki-link) nil))
   (should (eq (point) 8))))

(ert-deftest test-markdown-wiki-link/font-lock ()
  "Test font lock faces for wiki links."
  (markdown-test-temp-file "wiki-links.text"
   (let* ((fn (concat (file-name-directory buffer-file-name)
                     "inline.text")))
     ;; Create inline.text in the same temp directory, refontify
     (write-region "" nil fn nil 1)
     (markdown-fontify-buffer-wiki-links)
     ;; Confirm location of first wiki link
     (should (eq (markdown-next-wiki-link) 8))
     ;; First wiki link doesn't have a corresponding file
     (should (eq (get-char-property 8 'font-lock-face) markdown-missing-link-face))
     ;; Second wiki link doesn't have a corresponding file
     (should (eq (markdown-next-wiki-link) 73))
     (should (eq (get-char-property 8 'font-lock-face) markdown-missing-link-face))
     ;; Move to third wiki link, and create the missing file
     (should (eq (markdown-next-wiki-link) 155))
     (should (string-equal (markdown-wiki-link-link) "inline"))
     (should (eq (get-char-property 155 'font-lock-face) markdown-link-face))
     ;; Remove temporary files
     (delete-file fn)
     )))

(provide 'markdown-test)

;;; markdown-test.el ends here
