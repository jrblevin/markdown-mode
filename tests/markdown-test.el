;;;; markdown-test.el --- Tests for markdown-mode

;; Copyright (C) 2013 Jason R. Blevins <jrblevin@sdf.org>
;; Copyright (C) 2013 Makoto Motohashi <mkt.motohashi@gmail.com>

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

(defmacro markdown-test-file-gfm (file &rest body)
  "Open FILE from `markdown-test-dir' and execute body."
  `(let ((fn (concat markdown-test-dir ,file)))
     (save-window-excursion
       (with-temp-buffer
         (insert-file-contents fn)
         (gfm-mode)
         (goto-char (point-min))
         (font-lock-fontify-buffer)
         ,@body))))
(def-edebug-spec markdown-test-file-gfm (form body))

(defmacro markdown-test-temp-file (file &rest body)
  "Open FILE from `markdown-test-dir' visiting temp file and execute body.
This file is not saved."
  `(let ((fn (concat markdown-test-dir ,file))
         (tmp (make-temp-file "markdown-test"))
         tmp-text
         buf)
     (save-window-excursion
       (setq tmp-text (concat tmp ".text"))
       (setq buf (find-file tmp-text))
       (insert-file-contents fn)
       (markdown-mode)
       (goto-char (point-min))
       (font-lock-fontify-buffer)
       ,@body
       (kill-buffer buf)
       (delete-file tmp))))
(def-edebug-spec markdown-test-temp-file (form body))

(defun markdown-test-range-has-property (begin end prop value)
  "Verify that the range from BEGIN to END has property PROP equal to VALUE."
  (let (loc)
    (dolist (loc (number-sequence begin end))
      (should (eq (get-char-property loc prop) value)))))

(defun markdown-test-range-has-face (begin end face)
  "Verify that the range from BEGIN to END has face equal to FACE."
  (markdown-test-range-has-property begin end 'face face))

(defun markdown-test-goto-heading (title)
  "Move the point to section with TITLE."
  (let ((regexp (format "\\(^#+ %s\\( #+\\)?\\|^%s\n[=-]+\n\\)" title title)))
    (if (re-search-forward regexp nil t)
        (goto-char (match-end 0)))))

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
   (markdown-test-range-has-face 8 8 nil)
   ;; Check face of italic range
   (markdown-test-range-has-face 9 17 markdown-italic-face)
   ;; Check face of point past leading asterisk
   (markdown-test-range-has-face 18 18 nil)))

(ert-deftest test-markdown-font-lock/bold-1 ()
  "A simple bold test."
  (markdown-test-file "inline.text"
   (goto-char 27)
   (should (looking-at "\*\*"))
   ;; Check face of char before leading asterisk
   (markdown-test-range-has-face 26 26 nil)
   ;; Check face of bold range
   (markdown-test-range-has-face 27 35 markdown-bold-face)
   ;; Check face of point past leading asterisk
   (markdown-test-range-has-face 36 36 nil)))

(ert-deftest test-markdown-font-lock/code-1 ()
  "A simple inline code test."
  (markdown-test-file "inline.text"
   (goto-char 45)
   (should (looking-at "`"))
   (markdown-test-range-has-face 45 50 markdown-inline-code-face)
   (markdown-test-range-has-face 61 89 markdown-inline-code-face)
   ;; These tests are known to fail:
   (markdown-test-range-has-face 119 125 nil)
   (markdown-test-range-has-face 228 239 markdown-inline-code-face)
   (markdown-test-range-has-face 341 351 markdown-inline-code-face)
   (markdown-test-range-has-face 460 468 markdown-inline-code-face)
   (markdown-test-range-has-face 586 592 nil)
   (markdown-test-range-has-face 597 603 nil)
   (markdown-test-range-has-face 652 656 nil)
   (markdown-test-range-has-face 657 666 markdown-inline-code-face)
   ))

(ert-deftest test-markdown-font-lock/lists-1 ()
  "A simple list marker font lock test."
  (markdown-test-file "lists.text"
   (dolist (loc (list 1063 1283 1659 1830 1919 2150 2393 2484
                      2762 2853 3097 3188 3700 3903 4009))
     (goto-char loc)
     (should (looking-at "[*+-]"))
     (markdown-test-range-has-face loc loc markdown-list-face))))

(ert-deftest test-markdown-font-lock/pre-1 ()
  "Nested list and pre block font lock test."
  (markdown-test-file "nested-list.text"
    (dolist (loc (list 4 29 194 224 491 525))
      (markdown-test-range-has-face loc loc markdown-list-face))
    (markdown-test-range-has-face 6 25 nil)
    (markdown-test-range-has-face 31 83 nil)
    (markdown-test-range-has-face 85 155 markdown-pre-face)
    (markdown-test-range-has-face 157 189 nil)
    (markdown-test-range-has-face 196 215 nil)
    (markdown-test-range-has-face 226 403 nil)
    (markdown-test-range-has-face 405 482 markdown-pre-face)
    (markdown-test-range-has-face 493 512 nil)
    (markdown-test-range-has-face 527 546 nil)
    (markdown-test-range-has-face 548 581 markdown-pre-face)))

;;; Lists:

(ert-deftest test-markdown-lists/bounds-1 ()
  "Test list item bounds function `markdown-cur-list-item-bounds'."
  (markdown-test-file "lists.text"
    (markdown-test-goto-heading "Case 9")
    (forward-line)
    (should (eq (point) 3699))
    (markdown-next-list-item 4)
    (should (eq (point) 3700))
    (should (equal (markdown-cur-list-item-bounds)
                   (list 3700 3901 0 4)))
    (markdown-next-list-item 4)
    (should (eq (point) 3903))
    (should (equal (markdown-cur-list-item-bounds)
                   (list 3903 3937 0 4)))))

;;; Outline minor mode tests:

(ert-deftest test-markdown-outline/navigation ()
  "Test outline navigation functions."
  (markdown-test-file "outline.text"
   ;; Navigate to the first visible heading
   (outline-next-visible-heading 1)
   (should (eq (point) 19))
   (should (looking-at "^# A top-level header"))
   ;; Navigate forward at the same level
   (outline-forward-same-level 1)
   (should (eq (point) 377))
   (should (looking-at "^=+$"))
   ;; Navigate backward by four visible headings
   (outline-previous-visible-heading 4)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))))

(ert-deftest test-markdown-outline/visibility-atx ()
  "Test outline visibility cycling for ATX-style headers."
  (markdown-test-file "outline.text"
   ;; Navigate to the second visible heading
   (outline-next-visible-heading 2)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))
   ;; Cycle visibility of this subtree
   (markdown-cycle)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))
   ;; Test that the entire subtree is invisible
   (markdown-test-range-has-property 93 349 'invisible 'outline)
   ;; Cycle visibility of this subtree again
   (markdown-cycle)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))
   ;; Test that text is visible
   (markdown-test-range-has-property 95 121 'invisible nil)
   ;; Test that subheadings are visible
   (markdown-test-range-has-property 123 141 'invisible nil)
   ;; Cycle visibility of this subtree again
   (markdown-cycle)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))
   ;; Verify that entire subtree is visible
   (markdown-test-range-has-property 93 349 'invisible nil)))

(ert-deftest test-markdown-outline/visibility-setext ()
  "Test outline visibility cycling for setext-style headers."
  (markdown-test-file "outline.text"
   ;; Navigate to the sixth visible heading
   (outline-next-visible-heading 7)
   (outline-previous-visible-heading 1)
   (should (looking-at markdown-regex-header))
   (should (string-equal (match-string-no-properties 1) "An underline-style header"))
   (should (string-equal (match-string-no-properties 2) "========================="))
   ;; Cycle visibility subtree, test that it's invisible
   (markdown-cycle)
   (markdown-test-range-has-property 404 515 'invisible 'outline)
   ;; Cycle visibility subtree, test that text and headers are visible
   (markdown-cycle)
   (markdown-test-range-has-property 404 417 'invisible nil)
   (markdown-test-range-has-property 420 451 'invisible nil)))

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
     (markdown-test-range-has-property 8 20 'font-lock-face markdown-missing-link-face)
     ;; Second wiki link doesn't have a corresponding file
     (should (eq (markdown-next-wiki-link) 73))
     (markdown-test-range-has-property 73 88 'font-lock-face markdown-missing-link-face)
     ;; Move to third wiki link, and create the missing file
     (should (eq (markdown-next-wiki-link) 155))
     (should (string-equal (markdown-wiki-link-link) "inline"))
     (markdown-test-range-has-property 155 164 'font-lock-face markdown-link-face)
     ;; Remove temporary files
     (delete-file fn)
     )))

;;; Hook tests:

(ert-deftest test-markdown-hook/before-export ()
  "Test hook run before export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((before-hook-run nil)
          (ofile (concat buffer-file-name ".html"))
          (func (lambda (fname)
                  (setq before-hook-run t)
                  (should (string-equal fname ofile)))))
     ;; Register function
     (add-hook 'markdown-before-export-hooks func)
     ;; Export XHTML
     (markdown-export ofile)
     (should (eq before-hook-run t))
     ;; Clean
     (remove-hook 'markdown-before-export-hooks func)
     (delete-file ofile))))

(ert-deftest test-markdown-hook/after-export ()
  "Test hook run after export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((after-hook-run nil)
          (ofile (concat buffer-file-name ".html"))
          (func (lambda (fname)
                  (setq after-hook-run t)
                  (should (string-equal fname ofile)))))
     ;; Register function
     (add-hook 'markdown-after-export-hooks func)
     ;; Export XHTML
     (markdown-export ofile)
     (should (eq after-hook-run t))
     ;; Clean
     (remove-hook 'markdown-after-export-hooks func)
     (delete-file ofile))))

;;; gfm-mode tests:

(ert-deftest test-markdown-gfm/pre-1 ()
  "GFM pre block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2626 2637 nil)
    (markdown-test-range-has-face 2639 2641 markdown-pre-face)
    (markdown-test-range-has-face 2642 2645 markdown-language-keyword-face)
    (markdown-test-range-has-face 2647 2728 markdown-pre-face)
    (markdown-test-range-has-face 2730 2732 markdown-pre-face)))

(provide 'markdown-test)

;;; markdown-test.el ends here
