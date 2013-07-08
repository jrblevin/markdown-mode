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
    (setq-default indent-tabs-mode nil)
    (insert ,string)
    (goto-char (point-min))
    (font-lock-fontify-buffer)
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

(defmacro markdown-test-string-gfm (string &rest body)
  "Run body in a temporary buffer containing STRING in `gfm-mode'."
  `(with-temp-buffer
    (gfm-mode)
    (setq-default indent-tabs-mode nil)
    (insert ,string)
    (goto-char (point-min))
    (font-lock-fontify-buffer)
    (prog1 ,@body (kill-buffer))))
(def-edebug-spec markdown-test-string-gfm (form body))

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
         (tmp (make-temp-file "markdown-test" nil ".text"))
         buf)
     (save-window-excursion
       (setq buf (find-file tmp))
       (insert-file-contents fn)
       (markdown-mode)
       (goto-char (point-min))
       (font-lock-fontify-buffer)
       ,@body
       (set-buffer-modified-p nil)
       (kill-buffer buf)
       (delete-file tmp))))
(def-edebug-spec markdown-test-temp-file (form body))

(defun markdown-test-range-has-property (begin end prop value)
  "Verify that the range from BEGIN to END has property PROP equal to VALUE."
  (let (loc props)
    (dolist (loc (number-sequence begin end))
      (setq props (get-char-property loc prop))
      (cond ((and props (listp props))
             (should (memq value props)))
            (t
             (should (eq props value)))))))

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

;;; Element insertion tests:

(ert-deftest test-markdown-insertion/blank-line-before-1 ()
  "Test function `markdown-ensure-blank-line-before' at beginning of line."
  (markdown-test-file "syntax.text"
   (search-forward "as plain text")
   (should (= (point) 1556))
   (beginning-of-line)
   (should (= (point) 1505))
   (should (looking-back "A Markdown-formatted\n"))
   (should (not (markdown-prev-line-blank-p)))
   (markdown-ensure-blank-line-before)
   (should (looking-back "A Markdown-formatted\n\n"))
   (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-2 ()
  "Test function `markdown-ensure-blank-line-before' in middle of line."
  (markdown-test-file "syntax.text"
   (search-forward "as plain text")
   (should (= (point) 1556))
   (should (looking-back "as plain text"))
   (should (not (markdown-prev-line-blank-p)))
   (markdown-ensure-blank-line-before)
   (should (looking-back "as plain text\n\n"))
   (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-3 ()
  "Test function `markdown-ensure-blank-line-before' with blank line before."
  (markdown-test-file "syntax.text"
   (search-forward "web.\n\nMarkdown is not a replacement for HTML")
   (beginning-of-line)
   (should (= (point) 2704))
   (should (looking-back "web.\n\n"))
   (should (markdown-prev-line-blank-p))
   (markdown-ensure-blank-line-before)
   (should (= (point) 2704))
   (should (looking-back "web.\n\n"))
   (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-4 ()
  "Test function `markdown-ensure-blank-line-before' at beginning of buffer."
  (markdown-test-string "first line"
   (beginning-of-line)
   (should (bobp))
   (should (= (point-max) 11))
   (markdown-ensure-blank-line-before)
   (should (= (point-max) 11))
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "first line"))
   (forward-word)
   (markdown-ensure-blank-line-before)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "first\n\n line"))))

(ert-deftest test-markdown-insertion/blank-line-after-1 ()
  "Test function `markdown-ensure-blank-line-after' at end of line."
  (markdown-test-file "syntax.text"
   (search-forward "as plain text")
   (should (= (point) 1556))
   (end-of-line)
   (should (= (point) 1573))
   (should (looking-at "\nlike it's been"))
   (should (not (markdown-next-line-blank-p)))
   (markdown-ensure-blank-line-after)
   (should (looking-at "\n\nlike it's been"))
   (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-after-2 ()
  "Test function `markdown-ensure-blank-line-after' in middle of line."
  (markdown-test-file "syntax.text"
   (search-forward "as plain text")
   (should (= (point) 1556))
   (should (looking-at ", without looking"))
   (should (not (markdown-next-line-blank-p)))
   (markdown-ensure-blank-line-after)
   (should (looking-at "\n\n, without looking"))
   (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-after-3 ()
  "Test function `markdown-ensure-blank-line-after' with blank line after."
  (markdown-test-file "syntax.text"
   (search-forward "*writing* for the web.")
   (should (= (point) 2702))
   (should (looking-at "\n\nMarkdown is not a replacement for HTML"))
   (should (markdown-next-line-blank-p))
   (markdown-ensure-blank-line-after)
   (should (= (point) 2702))
   (should (looking-at "\n\nMarkdown is not a replacement for HTML"))
   (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-after-4 ()
  "Test function `markdown-ensure-blank-line-after' at end of buffer."
  (markdown-test-string "last line"
   (end-of-line)
   (should (eobp))
   (should (= (point-max) 10))
   (markdown-ensure-blank-line-after)
   (should (= (point-max) 10))
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "last line"))
   (backward-word)
   (markdown-ensure-blank-line-after)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "last \n\nline"))))

(ert-deftest test-markdown-insertion/point-after-unwrap ()
  "Test new point position calculations after unwrap operations."
  (markdown-test-string "line **one**\n"
   (let ((prefix (cons 6 8)) (suffix (cons 11 13)))
     ;; Prefix
     (should (eq (markdown-point-after-unwrap 6 prefix suffix) 6))
     (should (eq (markdown-point-after-unwrap 7 prefix suffix) 6))
     ;; Word
     (should (eq (markdown-point-after-unwrap 8 prefix suffix) 6))
     (should (eq (markdown-point-after-unwrap 9 prefix suffix) 7))
     (should (eq (markdown-point-after-unwrap 10 prefix suffix) 8))
     ;; Suffix
     (should (eq (markdown-point-after-unwrap 11 prefix suffix) 9))
     (should (eq (markdown-point-after-unwrap 12 prefix suffix) 9))
     ;; Immediately after
     (should (eq (markdown-point-after-unwrap 13 prefix suffix) 9))))
  (markdown-test-string "line _one_\n"
   (let ((prefix (cons 6 7)) (suffix (cons 10 11)))
     ;; Prefix
     (should (eq (markdown-point-after-unwrap 6 prefix suffix) 6))
     ;; Word
     (should (eq (markdown-point-after-unwrap 7 prefix suffix) 6))
     (should (eq (markdown-point-after-unwrap 8 prefix suffix) 7))
     (should (eq (markdown-point-after-unwrap 9 prefix suffix) 8))
     ;; Suffix
     (should (eq (markdown-point-after-unwrap 10 prefix suffix) 9))
     ;; Immediately after
     (should (eq (markdown-point-after-unwrap 10 prefix suffix) 9)))))

(ert-deftest test-markdown-insertion/unwrap-thing-at-point-italic ()
  "Test function `markdown-unwrap-thing-at-point' on italics."
  (markdown-test-file "syntax.text"
   ;; Unwrap *not*
   (goto-char 2859)
   (should (thing-at-point-looking-at markdown-regex-italic))
   (should (equal (markdown-unwrap-thing-at-point
                   markdown-regex-italic 2 4)
                  (cons 2859 2862)))
   (should (= (point) 2859))
   ;; Unwrap *publishing*
   (goto-char 3064)
   (should (thing-at-point-looking-at markdown-regex-italic))
   (should (equal (markdown-unwrap-thing-at-point
                   markdown-regex-italic 2 4)
                  (cons 3060 3070)))
   (should (= (point) 3063))
   ;; Unwrap *writing*
   (goto-char 3101)
   (should (thing-at-point-looking-at markdown-regex-italic))
   (should (equal (markdown-unwrap-thing-at-point
                   markdown-regex-italic 2 4)
                  (cons 3093 3100)))
   (should (= (point) 3100))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-italic ()
  "Test function `markdown-unwrap-things-in-region' on italics."
  (markdown-test-file "syntax.text"
   (should (equal (markdown-unwrap-things-in-region
                   2704 3207 markdown-regex-italic 2 4)
                  (cons 2704 3201)))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-bound ()
  "Ensure that `markdown-unwrap-things-in-region' respects end bound"
  (markdown-test-string "**a** **b** **c** **d** **e** **f**"
   ;; Set region to unrwap a, b, c, and d only.  If endpoint is not
   ;; respected (i.e, not adjusted for character removal), the
   ;; function will unwrap e and f also.
   (should (equal (markdown-unwrap-things-in-region
                   1 24 markdown-regex-bold 2 4)
                  (cons 1 8)))
   (should (string-equal (buffer-string) "a b c d **e** **f**"))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-links ()
  "Test function `markdown-unwrap-things-in-region' on inline links."
  (markdown-test-string "a [link](http://jblevins.org/) or [two](/).\n"
   (should (equal (markdown-unwrap-things-in-region
                   (point-min) (point-max) markdown-regex-link-inline 0 3)
                  (cons 1 16)))
   (should (string-equal (buffer-string) "a link or two.\n"))))

(ert-deftest test-markdown-insertion/toggle-bold ()
  "Test toggling functionality of `markdown-insert-bold'."
  (markdown-test-string "one **two** three"
   (forward-word 2)
   (markdown-insert-bold)
   (should (string-equal (buffer-string) "one two three"))
   (should (= (point) 8))
   (forward-word)
   (markdown-insert-bold)
   (should (= (point) 16))
   (should (string-equal (buffer-string) "one two **three**"))))

(ert-deftest test-markdown-insertion/toggle-italic ()
  "Test toggling functionality of `markdown-insert-italic'."
  (markdown-test-string "one *two* three"
   (forward-word 2)
   (markdown-insert-italic)
   (should (string-equal (buffer-string) "one two three"))
   (should (= (point) 8))
   (forward-word)
   (markdown-insert-italic)
   (should (string-equal (buffer-string) "one two *three*"))
   (should (= (point) 15))))

(ert-deftest test-markdown-insertion/toggle-code ()
  "Test toggling functionality of `markdown-insert-code'."
  (markdown-test-string "one `two` three"
   (forward-word 2)
   (markdown-insert-code)
   (should (string-equal (buffer-string) "one two three"))
   (should (= (point) 8))
   (forward-word)
   (markdown-insert-code)
   (should (string-equal (buffer-string) "one two `three`"))
   (should (= (point) 15))))

(ert-deftest test-markdown-insertion/toggle-wiki-link-alias-first ()
  "Test toggling of `markdown-insert-wiki-link' with alias first.
Test point position upon removal and insertion."
  (let ((markdown-wiki-link-alias-first t))
    (markdown-test-string "[[text|page]]"
     (goto-char 5) ; point in interior of alias text, at 'x'
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 3)) ; leave point at, at 'x'
     (should (string-equal (buffer-string) "text"))
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 5)) ; leave point at, at 'x'
     (should (string-equal (buffer-string) "[[text]]")))
    (markdown-test-string "[[text|page]]"
     (goto-char 10) ; point in interior of link text, at 'g'
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 5)) ; leave point at end of alias text
     (should (string-equal (buffer-string) "text"))
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 7)) ; leave point at end of alias text
     (should (string-equal (buffer-string) "[[text]]")))))

(ert-deftest test-markdown-insertion/toggle-wiki-link-alias-last ()
  "Test toggling of `markdown-insert-wiki-link' with alias last.
Test point position upon removal and insertion."
  (let ((markdown-wiki-link-alias-first nil))
    (markdown-test-string "[[page|text]]"
     (goto-char 10) ; point in interior of alias text, at 'x'
     (call-interactively 'markdown-insert-wiki-link)
     (goto-char 3) ; leave point at, at 'x'
     (should (string-equal (buffer-string) "text"))
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 5)) ; leave point at, at 'x'
     (should (string-equal (buffer-string) "[[text]]")))
    (markdown-test-string "[[page|text]]"
     (goto-char 3) ; point in interior of link text, at 'g'
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 1)) ; leave point at beginning of alias text
     (should (string-equal (buffer-string) "text"))
     (call-interactively 'markdown-insert-wiki-link)
     (should (= (point) 3)) ; leave point at beginning of alias text
     (should (string-equal (buffer-string) "[[text]]")))))

(ert-deftest test-markdown-insertion/bold-region ()
  "Test region functionality of `markdown-insert-bold'."
  (markdown-test-string "one two three"
   (push-mark (point) t t)
   (forward-word 2)
   (markdown-insert-bold)
   (should (string-equal (buffer-string) "**one two** three"))
   (should (= (point) 10))))

(ert-deftest test-markdown-insertion/italic-region ()
  "Test region functionality of `markdown-insert-italic'."
  (markdown-test-string "one two three"
   (transient-mark-mode)
   (push-mark (point) t t)
   (forward-word 2)
   (markdown-insert-italic)
   (should (string-equal (buffer-string) "*one two* three"))
   (should (= (point) 9))))

(ert-deftest test-markdown-insertion/code-region ()
  "Test region functionality of `markdown-insert-code'."
  (markdown-test-string "one two three"
   (transient-mark-mode)
   (push-mark (point) t t)
   (forward-word 2)
   (markdown-insert-code)
   (should (string-equal (buffer-string) "`one two` three"))
   (should (= (point) 9))))

(ert-deftest test-markdown-insertion/atx-line ()
  "Test ATX header insertion without region."
  (markdown-test-string "line one\nline two\n"
   (forward-word)
   (markdown-insert-header-atx-1)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "# line one #\n\nline two\n"))
   (forward-line 2)
   (markdown-insert-header-atx-2)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "# line one #\n\n## line two ##\n\n"))))

(ert-deftest test-markdown-insertion/atx-region ()
  "Test ATX header insertion with region."
  (markdown-test-string "line one\nline two\n"
   (transient-mark-mode)
   (forward-char 5)
   (push-mark (point) t t)
   (forward-word)
   (should (string-equal (buffer-substring (region-beginning) (region-end))
                         "one"))
   (markdown-insert-header-atx-4)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line \n\n#### one ####\n\nline two\n"))))

(ert-deftest test-markdown-insertion/atx-blank ()
  "Test ATX header insertion on blank line."
  (markdown-test-string "line one\n\nline two\n"
   (forward-line)
   (markdown-insert-header-atx-3)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line one\n\n###  ###\n\nline two\n"))
   (should (= (point) 15))
   (should (looking-at " ###\n"))))

(ert-deftest test-markdown-insertion/atx-region-whitespace ()
  "Test ATX header insertion using a region with whitespace."
  (markdown-test-string "  line one\n\nline two\n  \n"
   (transient-mark-mode)
   (push-mark (point) t t)
   (goto-char (point-max))
   (markdown-insert-header-atx-2)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "## line one line two ##"))
   (should (= (point) 21))
   (should (looking-at " ##"))))

(ert-deftest test-markdown-insertion/atx-line-whitespace ()
  "Test ATX header insertion using current line with whitespace."
  (markdown-test-string "  line one  \n\nline two\n"
   (goto-char (line-end-position))
   (markdown-insert-header-atx-3)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "### line one ###\n\nline two\n"))
   (should (= (point) 13))
   (should (looking-at " ###\n"))))

(ert-deftest test-markdown-insertion/atx-replace-atx ()
  "Test ATX header insertion when replacing an existing ATX header."
  (markdown-test-string "## replace ##\n"
   (markdown-insert-header-atx-4)
   (should (string-equal (buffer-string) "#### replace ####\n\n"))
   (should (looking-at " ####\n"))))

(ert-deftest test-markdown-insertion/atx-replace-setext-1 ()
  "Test ATX header insertion when replacing an existing setext header."
  (markdown-test-string "replace\n=======\n"
   (markdown-insert-header-atx-2)
   (should (string-equal (buffer-string) "## replace ##\n\n"))
   (should (looking-at " ##\n"))))

(ert-deftest test-markdown-insertion/atx-replace-setext-2 ()
  "Test ATX header insertion when replacing an existing setext header."
  (markdown-test-string "replace\n-------\n"
   (markdown-insert-header-atx-5)
   (should (string-equal (buffer-string) "##### replace #####\n\n"))
   (should (looking-at " #####\n"))))

(ert-deftest test-markdown-insertion/setext-line ()
  "Test setext header insertion without region."
  (markdown-test-string "line one\nline two\n"
   (forward-word)
   (markdown-insert-header-setext-1)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line one\n========\n\nline two\n"))
   (forward-line 3)
   (markdown-insert-header-setext-2)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line one\n========\n\nline two\n--------\n\n"))))

(ert-deftest test-markdown-insertion/setext-region ()
  "Test setext header insertion with region."
  (markdown-test-string "line one\nline two\n"
   (transient-mark-mode)
   (forward-char 5)
   (push-mark (point) t t)
   (forward-word)
   (should (string-equal (buffer-substring (region-beginning) (region-end))
                         "one"))
   (markdown-insert-header-setext-1)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line \n\none\n===\n\nline two\n"))))

(ert-deftest test-markdown-insertion/setext-blank ()
  "Test setext header insertion on blank line."
  (markdown-test-string "line one\n\nline two\n"
   (forward-line)
   (markdown-insert-header 2 "foo" t)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line one\n\nfoo\n---\n\nline two\n"))
   (should (= (point) 14))
   (should (looking-at "\n---"))))

(ert-deftest test-markdown-insertion/setext-region-whitespace ()
  "Test setext header insertion using a region with whitespace."
  (markdown-test-string "  line one\n\nline two\n  \n"
   (transient-mark-mode)
   (push-mark (point) t t)
   (goto-char (point-max))
   (markdown-insert-header-setext-1)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line one line two\n================="))
   (should (= (point) 18))
   (should (looking-at "\n===="))))

(ert-deftest test-markdown-insertion/setext-line-whitespace ()
  "Test setext header insertion using current line with whitespace."
  (markdown-test-string "  line one  \n\nline two\n"
   (goto-char (line-end-position))
   (markdown-insert-header-setext-2)
   (should (string-equal (buffer-substring (point-min) (point-max))
                         "line one\n--------\n\nline two\n"))
   (should (= (point) 9))
   (should (looking-at "\n---"))))

(ert-deftest test-markdown-insertion/setext-replace-atx ()
  "Test setext header insertion when replacing an existing ATX header."
  (markdown-test-string "## replace ##\n"
   (markdown-insert-header-setext-1)
   (should (string-equal (buffer-string) "replace\n=======\n\n"))
   (should (looking-at "\n==="))))

(ert-deftest test-markdown-insertion/setext-replace-setext-1 ()
  "Test setext header insertion when replacing an existing setext title."
  (markdown-test-string "replace\n=======\n"
   (markdown-insert-header-setext-2)
   (should (string-equal (buffer-string) "replace\n-------\n\n"))
   (should (looking-at "\n---"))))

(ert-deftest test-markdown-insertion/setext-replace-setext-2 ()
  "Test setext header insertion when replacing an existing setext section."
  (markdown-test-string "replace\n-------\n"
   (markdown-insert-header-setext-1)
   (should (string-equal (buffer-string) "replace\n=======\n\n"))
   (should (looking-at "\n==="))))

(ert-deftest test-markdown-insertion/header-dwim ()
  "Test 'do what I mean' header insertion."
  (markdown-test-file "outline.text"
   (call-interactively 'markdown-insert-header-dwim)
   (should (looking-at " #$"))
   (end-of-defun 2)
   (call-interactively 'markdown-insert-header-dwim)
   (beginning-of-line)
   (should (looking-at "^#  #$"))
   (end-of-defun 3)
   (call-interactively 'markdown-insert-header-dwim)
   (beginning-of-line)
   (should (looking-at "^###  ###$"))))

(ert-deftest test-markdown-insertion/header-dwim-prefix ()
  "Test 'do what I mean' header insertion with prefix arguments."
  (let ((tests (list '(nil . "## abc ##")
                     '(1 . "# abc #")
                     '(2 . "## abc ##")
                     '(3 . "### abc ###")
                     '(4 . "#### abc ####")
                     '(5 . "##### abc #####")
                     '(6 . "###### abc ######")
                     '((4) . "# abc #")
                     '((16) . "### abc ###"))))
    (dolist (test tests)
      (markdown-test-string "## atx\n\nabc"
       (goto-char (point-max))
       (let ((current-prefix-arg (car test)))
         (call-interactively 'markdown-insert-header-dwim)
         (should (string-equal
                  (buffer-substring (line-beginning-position) (line-end-position))
                  (cdr test))))))))

(ert-deftest test-markdown-insertion/header-setext-dwim-prefix ()
  "Test 'do what I mean' header insertion with prefix arguments."
  (let ((tests (list '(nil . "abc\n---")
                     '(1 . "abc\n===")
                     '(2 . "abc\n---")
                     '(3 . "### abc ###")
                     '(4 . "#### abc ####")
                     '(5 . "##### abc #####")
                     '(6 . "###### abc ######")
                     '((4) . "abc\n===")
                     '((16) . "### abc ###"))))
    (dolist (test tests)
      (markdown-test-string "atx\n---\n\nabc"
       (goto-char (point-max))
       (let ((current-prefix-arg (car test)))
         (call-interactively 'markdown-insert-header-setext-dwim)
         (should (string-equal
                  (buffer-substring (line-beginning-position) (line-end-position 2))
                  (cdr test))))))))

(ert-deftest test-markdown-insertion/remove-header ()
  "Test ATX and setext header."
  (markdown-test-string
   "# atx1\n\n## atx2 ##\n\nsetext1\n=======\n\nsetext2\n-------\n"
   (should (equal (markdown-remove-header) (cons 1 5)))
   (forward-line)
   (should (not (markdown-remove-header)))
   (forward-line)
   (should (equal (markdown-remove-header) (cons 7 11)))
   (forward-line)
   (should (not (markdown-remove-header)))
   (forward-line)
   (should (equal (markdown-remove-header) (cons 13 20)))
   (forward-line)
   (should (not (markdown-remove-header)))
   (forward-line)
   (should (equal (markdown-remove-header) (cons 22 29)))
   (should (string-equal (buffer-string)
                         "atx1\n\natx2\n\nsetext1\n\nsetext2\n"))))

(ert-deftest test-markdown-insertion/italic-unwrap-region ()
  "A test of inserting italics with italic text in the region."
  (markdown-test-string "*foo* bar *baz*"
   (transient-mark-mode)
   (push-mark (point) t t)
   (end-of-line)
   (markdown-insert-italic)
   (should (string-equal (buffer-string) "*foo bar baz*"))))

(ert-deftest test-markdown-insertion/bold-unwrap-region ()
  "A test of inserting bold with italic text in the region."
  (markdown-test-string "*foo* **bar** *baz*"
   (transient-mark-mode)
   (push-mark (point) t t)
   (end-of-line)
   (markdown-insert-bold)
   (should (string-equal (buffer-string) "***foo* bar *baz***"))))

(ert-deftest test-markdown-insertion/code-unwrap-region ()
  "A test of inserting code with code already in the region."
  (markdown-test-string "`foo` *bar* `baz`"
   (transient-mark-mode)
   (push-mark (point) t t)
   (end-of-line)
   (markdown-insert-code)
   (should (string-equal (buffer-string) "`foo *bar* baz`"))))

(ert-deftest test-markdown-insertion/hr-order ()
  "Test inserting horizontal rules."
  (dotimes (n (length markdown-hr-strings))
    (markdown-test-string ""
     (let ((current-prefix-arg n))
       (call-interactively 'markdown-insert-hr))
     (should (string-equal (buffer-string) (nth (1- n) markdown-hr-strings))))))

(ert-deftest test-markdown-insertion/hr-prefix ()
  "Test inserting horizontal rule with C-u prefix."
  (markdown-test-string ""
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-hr))
   (should (string-equal (buffer-string) (car (last markdown-hr-strings))))))

(ert-deftest test-markdown-insertion/hr-bob ()
  "Test inserting horizontal rule at beginning of buffer."
  (markdown-test-string "one line\n"
   (call-interactively 'markdown-insert-hr)
   (should (string-equal (buffer-string)
                         (concat (car markdown-hr-strings)
                                 "\n\none line\n")))))

(ert-deftest test-markdown-insertion/hr-eob ()
  "Test inserting horizontal rule at end of buffer."
  (markdown-test-string "one line\n"
   (forward-line)
   (call-interactively 'markdown-insert-hr)
   (should (string-equal (buffer-string)
                         (concat "one line\n\n" (car markdown-hr-strings))))))

(ert-deftest test-markdown-insertion/hr-mob ()
  "Test inserting horizontal rule in middle of buffer."
  (markdown-test-string "one line\n"
   (forward-word)
   (let ((markdown-hr-strings '("----------")))
     (call-interactively 'markdown-insert-hr)
     (should (string-equal (buffer-string)
                           (concat "one\n\n" (car markdown-hr-strings)
                                   "\n\n line\n"))))))

(ert-deftest test-markdown-insertion/pre-region-1 ()
  "Test `markdown-pre-region'."
  ;; Simple test as non-interactive command
  (markdown-test-string "line one\nline two\n"
   (markdown-pre-region (line-beginning-position) (line-end-position))
   (should (string-equal (buffer-string) "    line one\n\nline two\n")))
  ;; Test removal of whitespace before and after region
  (markdown-test-string "line one abc\nline two\n"
   (markdown-pre-region 6 9)
   (should (string-equal (buffer-string) "line\n\n    one\n\nabc\nline two\n")))
  ;; Simple test as interactive command
  (markdown-test-string "line one\nline two\n"
   (push-mark (point) t t)
   (forward-line 2)
   (call-interactively 'markdown-pre-region)
   (should (string-equal (buffer-string) "    line one\n    line two\n\n"))))

(ert-deftest test-markdown-insertion/blockquote-region-1 ()
  "Test `markdown-blockquote-region'."
  ;; Simple test as non-interactive command
  (markdown-test-string "line one\nline two\n"
   (markdown-blockquote-region (line-beginning-position) (line-end-position))
   (should (string-equal (buffer-string) "> line one\n\nline two\n")))
  ;; Test removal of whitespace before and after region
  (markdown-test-string "line one abc\nline two\n"
   (markdown-blockquote-region 6 9)
   (should (string-equal (buffer-string) "line\n\n> one\n\nabc\nline two\n")))
  ;; Simple test as interactive command
  (markdown-test-string "line one\nline two\n"
   (push-mark (point) t t)
   (forward-line 2)
   (call-interactively 'markdown-blockquote-region)
   (should (string-equal (buffer-string) "> line one\n> line two\n\n"))))

(ert-deftest test-markdown-insertion/pre-nested-lists ()
  "Test `markdown-pre-indentation' and `markdown-insert-pre' with nested list."
  (markdown-test-string "* item\n    * item\n"
   ;; before the first item
   (should (string-equal (markdown-pre-indentation (point)) "    "))
   (markdown-insert-pre)
   (beginning-of-line)
   (should (markdown-prev-line-blank-p))
   (should (looking-at "^    $"))
   (should (markdown-next-line-blank-p))
   ;; before the second item
   (forward-line 3)
   (should (string-equal (markdown-pre-indentation (point)) "        "))
   (markdown-insert-pre)
   (beginning-of-line)
   (should (markdown-prev-line-blank-p))
   (should (looking-at "^        $"))
   (should (markdown-next-line-blank-p))
   ;; after the second item
   (forward-line 3)
   (should (string-equal (markdown-pre-indentation (point)) "            "))
   (markdown-insert-pre)
   (beginning-of-line)
   (should (markdown-prev-line-blank-p))
   (should (looking-at "^            $"))
   (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/pre-faux-list ()
  "Test `markdown-pre-indentation' following a list-marker in a pre block."
  (markdown-test-string "    * pre block, not a list item\n"
   (should (string-equal (markdown-pre-indentation (point-max)) "    "))))

(ert-deftest test-markdown-insertion/blockquote-nested-lists ()
  "Test blockquote insertion in a nested list context."
  (markdown-test-string "* item\n    * item\n"
   ;; before the first item
   (should (string-equal (markdown-blockquote-indentation (point)) ""))
   (markdown-insert-blockquote)
   (beginning-of-line)
   (should (markdown-prev-line-blank-p))
   (should (looking-at "^> $"))
   (should (markdown-next-line-blank-p))
   ;; before the second item
   (forward-line 3)
   (should (string-equal (markdown-blockquote-indentation (point)) "    "))
   (markdown-insert-blockquote)
   (beginning-of-line)
   (should (markdown-prev-line-blank-p))
   (should (looking-at "^    > $"))
   (should (markdown-next-line-blank-p))
   ;; after the second item
   (forward-line 3)
   (should (string-equal (markdown-blockquote-indentation (point)) "        "))
   (markdown-insert-blockquote)
   (beginning-of-line)
   (should (markdown-prev-line-blank-p))
   (should (looking-at "^        > $"))
   (should (markdown-next-line-blank-p))))

(ert-deftest test-markdown-insertion/empty-italic ()
  "Test `markdown-insert-italic' with no word at point and no region."
  (markdown-test-string ""
   (call-interactively 'markdown-insert-italic)
   (should (string-equal (buffer-string) "**"))
   (should (= (point) 2))))

(ert-deftest test-markdown-insertion/empty-bold ()
  "Test `markdown-insert-bold' with no word at point and no region."
  (markdown-test-string ""
   (call-interactively 'markdown-insert-bold)
   (should (string-equal (buffer-string) "****"))
   (should (= (point) 3))))

(ert-deftest test-markdown-insertion/uri ()
  "Test `markdown-insert-uri'."
  (markdown-test-string "http://jblevins.org/projects/markdown-mode/"
   (call-interactively 'markdown-insert-uri)
   (should (string-equal (buffer-string) "<http://jblevins.org/projects/markdown-mode/>"))
   (should (= (point) 2))
   (call-interactively 'markdown-insert-uri)
   (should (string-equal (buffer-string) "http://jblevins.org/projects/markdown-mode/"))
   (should (= (point) 1))
   (erase-buffer)
   (call-interactively 'markdown-insert-uri)
   (should (string-equal (buffer-string) "<>"))
   (should (= (point) 2))))

(ert-deftest test-markdown-insertion/list-item ()
  "Test `markdown-insert-list-item' on several lists."
  ;; No existing list
  (markdown-test-string "abc"
   (goto-char (point-max))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "abc\n* "))
   (should (= (point) 7)))
  ;; Following a list item, on the same line
  (markdown-test-string "  * foo"
   (goto-char (point-max))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "  * foo\n  * ")))
  ;; Following a list item, on the next line
  (markdown-test-string "- foo\n"
   (goto-char (point-max))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "- foo\n- ")))
  ;; Following a list item, after a blank line
  (markdown-test-string "- foo\n\n"
   (goto-char (point-max))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "- foo\n\n- ")))
  ;; Preceding a list item
  (markdown-test-string "- foo\n"
   (goto-char (point-min))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "- \n- foo\n")))
  ;; Preceding a list item and a blank line
  (markdown-test-string "\n\n- foo\n"
   (goto-char (point-min))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "- \n\n- foo\n")))
  ;; In the middle of a list item
  (markdown-test-string "- foo bar\n"
   (forward-word)
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "- foo\n-  bar\n")))
  ;; Before a list marker, but not at beginning of line
  (markdown-test-string "   - foo\n"
   (forward-char 2)
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "   - \n   - foo\n")))
  ;; Following an ordered list item
  (markdown-test-string "6. foo"
   (goto-char (point-max))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "6. foo\n7. ")))
  ;; Following a nested ordered list item
  (markdown-test-string "6. foo\n    1. bar"
   (goto-char (point-max))
   (call-interactively 'markdown-insert-list-item)
   (should (string-equal (buffer-string) "6. foo\n    1. bar\n    2. "))))

(ert-deftest test-markdown-insertion/reference-link ()
  "Basic tests for `markdown-insert-reference-link'."
   ;; Test optional parameters (leave point after link)
  (markdown-test-string ""
   (markdown-insert-reference-link "abc" "1")
   (should (string-equal (buffer-string) "[abc][1]"))
   (should (= (point) 9)))
   ;; Full link without title (leave point after link)
  (markdown-test-string ""
   (markdown-insert-reference-link "link" "label" "http://jblevins.org/")
   (should (string-equal (buffer-string) "[link][label]\n\n[label]: http://jblevins.org/\n"))
   (should (= (point) 14)))
   ;; Full link without label or title (leave point after link)
  (markdown-test-string ""
   (markdown-insert-reference-link "link" "" "http://jblevins.org/")
   (should (string-equal (buffer-string) "[link][]\n\n[link]: http://jblevins.org/\n"))
   (should (= (point) 9)))
   ;; Link only with no label, URL, or title (leave point after link)
  (markdown-test-string ""
   (markdown-insert-reference-link "link" "")
   (should (string-equal (buffer-string) "[link][]"))
   (should (= (point) 9))))

(ert-deftest test-markdown-insertion/reference-link-end ()
  "Basic reference link insertion test for 'end location."
  (let ((markdown-reference-location 'end))
    (markdown-test-string "first para\n\nsecond para\n"
     (end-of-line)
     (markdown-insert-reference-link "link" "" "http://jblevins.org/")
     (should (= (point) 19))
     (goto-line 5)
     (should (looking-at "\\[link\\]: http://jblevins.org/")))))

(ert-deftest test-markdown-insertion/reference-link-immediately ()
  "Basic reference link insertion test for 'immediately location."
  (let ((markdown-reference-location 'immediately))
    (markdown-test-string "first para\n\nsecond para\n"
     (end-of-line)
     (markdown-insert-reference-link "link" "" "http://jblevins.org/")
     (should (= (point) 19))
     (goto-line 3)
     (should (looking-at "\\[link\\]: http://jblevins.org/")))))

(ert-deftest test-markdown-insertion/reference-link-header ()
  "Basic reference link insertion test for 'header location."
  (let ((markdown-reference-location 'header))
    (markdown-test-string "par one\n\npar two\n\n### header\n"
     (end-of-line)
     (markdown-insert-reference-link "link" "" "")
     (should (= (point) 35))
     (should (looking-back "\\[link\\]: ")))))

(ert-deftest test-markdown-insertion/inline-link ()
  "Basic tests for `markdown-insert-link'."
   ;; Test empty markup insertion (leave point in square brackets)
  (markdown-test-string "abc "
   (end-of-line)
   (call-interactively 'markdown-insert-link)
   (should (string-equal (buffer-string) "abc []()"))
   (should (= (point) 6)))
  ;; Test with word at point (leave point in parentheses)
  (markdown-test-string "abc def ghi"
   (forward-word 2)
   (call-interactively 'markdown-insert-link)
   (should (string-equal (buffer-string) "abc [def]() ghi"))
   (should (= (point) 11)))
  ;; Test with region (leave point in parentheses)
  (markdown-test-string "abc def ghi"
   (transient-mark-mode)
   (push-mark (point) t t)
   (forward-word 2)
   (call-interactively 'markdown-insert-link)
   (should (string-equal (buffer-string) "[abc def]() ghi"))
   (should (= (point) 11))))

;;; Footnote tests:

(ert-deftest test-markdown-footnote/basic-end ()
  "Basic footnote insertion and deletion tests for 'end location."
  (let ((markdown-footnote-location 'end))
    (markdown-test-string "first line\nsecond line\n"
     ;; new buffer with no footnotes
     (should (= markdown-footnote-counter 0))
     ;; footnote insertion
     (end-of-line)
     (markdown-insert-footnote)
     (should (= (point) 35))
     (should (= markdown-footnote-counter 1))
     (should (looking-back "\\[^1\\]: "))
     ;; kill with point in footnote definition
     (insert "footnote text")
     (let (kill-ring)
       (markdown-footnote-kill))
     (should (= (point) 24))
     (should (bolp))
     (should (string-equal (buffer-string) "first line\nsecond line\n"))
     ;; insertion, counter should increment
     (goto-char (point-min))
     (end-of-line)
     (markdown-insert-footnote)
     (should (= (point) 35))
     (should (= markdown-footnote-counter 2))
     (should (looking-back "\\[^2\\]: "))
     (insert "footnote text")
     ;; return to marker
     (markdown-footnote-return)
     (should (= (point) 15))
     (should (looking-back "\\[^2\\]"))
     ;; kill with point at marker
     (let (kill-ring)
       (markdown-footnote-kill))
     (should (= (point) 11))
     (should (eolp))
     (should (string-equal (buffer-string) "first line\nsecond line\n")))))

(ert-deftest test-markdown-footnote/basic-immediately ()
  "Basic footnote insertion and deletion tests for 'immediately location."
  (let ((markdown-footnote-location 'immediately))
    (markdown-test-string "first paragraph\n\nsecond paragraph\n"
     ;; new buffer with no footnotes
     (should (= markdown-footnote-counter 0))
     ;; footnote insertion
     (end-of-line)
     (markdown-insert-footnote)
     (should (= (point) 28))
     (should (= markdown-footnote-counter 1))
     (should (looking-back "\\[^1\\]: "))
     ;; kill with point in footnote definition
     (insert "footnote text")
     (let (kill-ring)
       (markdown-footnote-kill))
     (should (= (point) 18))
     (should (bolp))
     (should (string-equal (buffer-string)
                           "first paragraph\n\nsecond paragraph\n")))))

(ert-deftest test-markdown-footnote/basic-header ()
  "Basic footnote insertion and deletion tests for 'header location."
  (let ((markdown-footnote-location 'header))
    (markdown-test-string "par one\n\npar two\n\n### header\n"
     ;; new buffer with no footnotes
     (should (= markdown-footnote-counter 0))
     ;; footnote insertion
     (end-of-line)
     (markdown-insert-footnote)
     (should (= (point) 29))
     (should (= markdown-footnote-counter 1))
     (should (looking-back "\\[^1\\]: "))
     ;; kill with point in footnote definition
     (insert "footnote text")
     (let (kill-ring)
       (markdown-footnote-kill))
     (should (= (point) 19))
     (should (bolp))
     (should (string-equal (buffer-string)
                           "par one\n\npar two\n\n### header\n"))
     ;; insertion, counter should increment
     (goto-char (point-min))
     (end-of-line)
     (markdown-insert-footnote)
     (should (= (point) 29))
     (should (= markdown-footnote-counter 2))
     (should (looking-back "\\[^2\\]: "))
     (insert "footnote text")
     ;; return to marker
     (markdown-footnote-return)
     (should (= (point) 12))
     (should (looking-back "\\[^2\\]"))
     ;; kill with point at marker
     (let (kill-ring)
       (markdown-footnote-kill))
     (should (= (point) 8))
     (should (eolp))
     (should (string-equal (buffer-string)
                           "par one\n\npar two\n\n### header\n")))))

(ert-deftest test-markdown-footnote/kill-empty-text ()
  "Test killing a footnote with marker but no text."
  (markdown-test-string "no text[^1]\n\n[^1]: \n"
   (end-of-line)
   (markdown-footnote-goto-text)
   (should (looking-back "\\[^1\\]: "))
   (let (kill-ring)
     (markdown-footnote-kill))
   (should (string-equal (buffer-string) "no text\n"))))

;;; Element removal tests:

(ert-deftest test-markdown-kill/simple ()
  "Simple tests for `markdown-kill-thing-at-point'."
  (let ((kill-ring nil)
        (tests (list '("`foo`" . "foo")
                     '("## foo ##" . "foo")
                     '("## foo" . "foo")
                     '("foo\n---" . "foo")
                     '("foo\n===" . "foo")
                     '("* * * * *" . "* * * * *")
                     '("[foo](http://bar.com/)" . "foo")
                     '("![foo](http://bar.com/)" . "foo")
                     '("[foo][bar]" . "foo")
                     '("![foo][bar]" . "foo")
                     '("<http://foo.com/>" . "http://foo.com/")
                     '("<foo@bar.com>" . "foo@bar.com")
                     '("[[foo]]" . "foo")
                     '("[[foo|bar]]" . "foo")
                     '("**foo**" . "foo")
                     '("__foo__" . "foo")
                     '("*foo*" . "foo")
                     '("_foo_" . "foo")
                     '("  [foo]: http://bar.com/" . "http://bar.com/")
                     '("  [foo]: http://bar.com/ \"title\"" . "http://bar.com/")
                     '("foo[^bar]\n\n[^bar]: baz" . "baz")
                     '("[^bar]: baz" . "baz")
                     '("  * foo\n  bar" . "  * foo\n  bar"))))
    (dolist (test tests)
      ;; Load test string (the car), move to end of first line, kill
      ;; thing at point, and then verify that the kill ring contains cdr.
      (markdown-test-string (car test)
       (end-of-line)
       (call-interactively 'markdown-kill-thing-at-point)
       (should (string-equal (current-kill 0) (cdr test)))))))

(ert-deftest test-markdown-kill/footnote-text ()
  "Test killing a footnote with point at footnote text."
  (markdown-test-string "some text[^1]\n\n[^1]: footnote\n"
   (end-of-line)
   (markdown-footnote-goto-text)
   (let (kill-ring)
     (markdown-footnote-kill))
   (should (string-equal (buffer-string) "some text\n"))))

(ert-deftest test-markdown-kill/code ()
  "Test killing with code regex.."
  (let ((kill-ring nil))
   (markdown-test-string "Lorem `ipsum` dolor `sit` `amet`."
    (goto-char 22) ; position point at s in `sit`
    (call-interactively 'markdown-kill-thing-at-point)
    (should (string-equal (current-kill 0) "sit")))))

;;; Completion:

(ert-deftest test-markdown-complete/atx-header-incomplete ()
  "Test `markdown-incomplete-atx-p'."
  (markdown-test-string "###  ###"
   (should (looking-at markdown-regex-header-atx))
   (should-not (markdown-incomplete-atx-p)))
  (markdown-test-string "###abc###"
   (should (looking-at markdown-regex-header-atx))
   (should (markdown-incomplete-atx-p)))
  (markdown-test-string "###   ###"
   (should (looking-at markdown-regex-header-atx))
   (should (markdown-incomplete-atx-p))))

(ert-deftest test-markdown-complete/atx-header ()
  "Test `markdown-complete' for atx headers."
  (markdown-test-string "##### test"
   (call-interactively 'markdown-complete)
   (should (string-equal (buffer-string) "##### test #####"))))

(ert-deftest test-markdown-complete/setext-header-incomplete ()
  "Test `markdown-incomplete-setext-p'."
  (markdown-test-string "abc\n===\n"
   (should (looking-at markdown-regex-header-setext))
   (should-not (markdown-incomplete-setext-p)))
  (markdown-test-string "abc\n==\n"
   (should (looking-at markdown-regex-header-setext))
   (should (markdown-incomplete-setext-p)))
  (markdown-test-string "abc\n====\n"
   (should (looking-at markdown-regex-header-setext))
   (should (markdown-incomplete-setext-p))))

(ert-deftest test-markdown-complete/setext-header ()
  "Test `markdown-complete' for setext headers."
  (markdown-test-string " test  \n=="
   (call-interactively 'markdown-complete)
   (should (string-equal (buffer-string) "test\n===="))))

(ert-deftest test-markdown-complete/hr-incomplete ()
  "Test `markdown-incomplete-hr-p'."
  (dolist (i (number-sequence 0 (1- (length markdown-hr-strings))))
    (markdown-test-string (nth i markdown-hr-strings)
     (should (looking-at markdown-regex-hr))
     (should-not (markdown-incomplete-hr-p))
     (should-error (call-interactively 'markdown-complete)))))

(ert-deftest test-markdown-complete/hr ()
  "Test completion via `markdown-complete' for horizontal rules."
  (markdown-test-string "- - - - -"
   (call-interactively 'markdown-complete)
   (should (string-equal (buffer-string) (car markdown-hr-strings)))))

(ert-deftest test-markdown-complete/buffer-setext-2 ()
  "Test `markdown-complete-buffer' for level two setext heading."
  ;; Ensure markdown-complete-buffer doesn't mistake this for a horizontal rule
  (markdown-test-string "Subheading\n--\n"
   (call-interactively 'markdown-complete-buffer)
   (should (string-equal (buffer-string) "Subheading\n----------\n\n")))
  (markdown-test-string "Abc\n--\n\nDef\n--\n"
   (call-interactively 'markdown-complete-buffer)
   (should (string-equal (buffer-string) "Abc\n---\n\nDef\n---\n\n"))))

;;; Promotion and demotion tests:

(ert-deftest test-markdown-promote/atx-header ()
  "Test `markdown-promote' for atx headers."
  (markdown-test-string "###### test ######"
   (markdown-promote)
   (should (string-equal (buffer-string) "##### test #####"))
   (markdown-promote)
   (should (string-equal (buffer-string) "#### test ####"))
   (markdown-promote)
   (should (string-equal (buffer-string) "### test ###"))
   (markdown-promote)
   (should (string-equal (buffer-string) "## test ##"))
   (markdown-promote)
   (should (string-equal (buffer-string) "# test #"))))

(ert-deftest test-markdown-demote/atx-header ()
  "Test `markdown-demote' for atx headers."
  (markdown-test-string "# test #"
   (markdown-demote)
   (should (string-equal (buffer-string) "## test ##"))
   (markdown-demote)
   (should (string-equal (buffer-string) "### test ###"))
   (markdown-demote)
   (should (string-equal (buffer-string) "#### test ####"))
   (markdown-demote)
   (should (string-equal (buffer-string) "##### test #####"))
   (markdown-demote)
   (should (string-equal (buffer-string) "###### test ######"))))

(ert-deftest test-markdown-promote/setext-header ()
  "Test `markdown-promote' for setext headers."
  (markdown-test-string "test\n----"
   (markdown-promote)
   (should (string-equal (buffer-string) "test\n===="))))

(ert-deftest test-markdown-demote/setext-header ()
  "Test `markdown-demote' for setext headers."
  (markdown-test-string "test\n===="
   (markdown-demote)
   (should (string-equal (buffer-string) "test\n----"))
   (markdown-demote)
   (should (string-equal (buffer-string) "### test ###"))
   (markdown-demote)
   (should (string-equal (buffer-string) "#### test ####"))
   (markdown-demote)
   (should (string-equal (buffer-string) "##### test #####"))
   (markdown-demote)
   (should (string-equal (buffer-string) "###### test ######"))))

(ert-deftest test-markdown-promote/hr ()
  "Test `markdown-promote' for horizontal rules."
  (markdown-test-string (car (reverse markdown-hr-strings))
    (dolist (n (number-sequence 4 0 -1))
      (markdown-promote)
      (should (string-equal (buffer-string) (nth n markdown-hr-strings))))))

(ert-deftest test-markdown-demote/hr ()
  "Test `markdown-demote' for horizontal rules."
  (markdown-test-string (car markdown-hr-strings)
    (dolist (n (number-sequence 1 5))
      (markdown-demote)
      (should (string-equal (buffer-string) (nth n markdown-hr-strings))))))

(ert-deftest test-markdown-promote/bold ()
  "Test `markdown-promote' for bold markup."
  (markdown-test-string "__bold__"
  (call-interactively 'markdown-promote)
  (should (string-equal (buffer-string) "**bold**"))))

(ert-deftest test-markdown-demote/bold ()
  "Test `markdown-demote' for bold markup."
  (markdown-test-string "**bold**"
  (call-interactively 'markdown-promote)
  (should (string-equal (buffer-string) "__bold__"))))

(ert-deftest test-markdown-promote/italic ()
  "Test `markdown-promote' for italic markup."
  (markdown-test-string "_italic_"
  (call-interactively 'markdown-promote)
  (should (string-equal (buffer-string) "*italic*"))))

(ert-deftest test-markdown-demote/italic ()
  "Test `markdown-demote' for italic markup."
  (markdown-test-string "*italic*"
  (call-interactively 'markdown-promote)
  (should (string-equal (buffer-string) "_italic_"))))

;;; Cycling:

(ert-deftest test-markdown-cycle/atx-header ()
  "Test `markdown-demote' cycling for atx headers."
  (markdown-test-string "##### test"
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "###### test ######"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "# test #"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "## test ##"))))

(ert-deftest test-markdown-cycle/setext-header ()
  "Test `markdown-demote' cycling for setext headers."
  (markdown-test-string "test\n===="
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "test\n----"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "### test ###"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "#### test ####"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "##### test #####"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "###### test ######"))
   (call-interactively 'markdown-demote)
   (should (string-equal (buffer-string) "# test #"))))

(ert-deftest test-markdown-cycle/hr ()
  "Test cycling of horizontal rules."
  ;; Cycle using markdown-demote
  (markdown-test-string (car markdown-hr-strings)
    (dolist (n (number-sequence 1 5))
      (call-interactively 'markdown-demote)
      (should (string-equal (buffer-string) (nth n markdown-hr-strings))))
    (call-interactively 'markdown-demote)
    (should (string-equal (buffer-string) (car markdown-hr-strings))))
  ;; Cycle using markdown-promote
  (markdown-test-string (car (reverse markdown-hr-strings))
    (dolist (n (number-sequence 4 0 -1))
      (call-interactively 'markdown-promote)
      (should (string-equal (buffer-string) (nth n markdown-hr-strings))))
    (call-interactively 'markdown-promote)
    (should (string-equal (buffer-string) (car (reverse markdown-hr-strings))))))

(ert-deftest test-markdown-cycle/bold ()
  "Test cycling of bold markup."
  (markdown-test-string "**bold**"
  (call-interactively 'markdown-demote)
  (should (string-equal (buffer-string) "__bold__"))
  (call-interactively 'markdown-demote)
  (should (string-equal (buffer-string) "**bold**"))))

(ert-deftest test-markdown-cycle/italic ()
  "Test cycling of italic markup."
  (markdown-test-string "*italic*"
  (call-interactively 'markdown-demote)
  (should (string-equal (buffer-string) "_italic_"))
  (call-interactively 'markdown-demote)
  (should (string-equal (buffer-string) "*italic*"))))

;;; Indentation tests:

(ert-deftest test-markdown-indentation/calc-indents ()
  "Test `markdown-calc-indents' a nested list context."
  (markdown-test-file "nested-list.text"
   (goto-char (point-max))
   (let ((indents (markdown-calc-indents)))
     (should (= (car indents) 17)) ; indentation of previous line first
     (should (equal (sort indents '<)
                    (list
                     0 ; beginning of line
                     3 ; first-level list marker
                     7 ; second-level list marker
                     11 ; third-level list marker
                     13 ; previous list item text
                     16 ; pre-block indentation
                     17 ; indentation of previous line
                     21 ; previous line plus tab-width
                     ))))))

(ert-deftest test-markdown-indentation/indent-region ()
  "Test `markdown-indent-region'."
  ;; Basic test with multiple lines
  (markdown-test-string "abc\ndef\nghi\n"
    (markdown-indent-region (point-min) (point-max) nil)
    (should (string-equal (buffer-string) "    abc\n    def\n    ghi\n")))
  ;; Following a list item
  (markdown-test-string "  * abc\ndef\n"
    (forward-line)
    (markdown-indent-region (line-beginning-position) (line-end-position) nil)
    (should (string-equal (buffer-string) "  * abc\n  def\n"))
    (markdown-indent-region (line-beginning-position) (line-end-position) nil)
    (should (string-equal (buffer-string) "  * abc\n    def\n"))))

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

(ert-deftest test-markdown-font-lock/italics-2 ()
  "Test space after leading asterisk or underscore."
  (markdown-test-string
   "This is * not italic*, nor _ is this_."
   (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/italics-3 ()
  "Test that slash inside asterisks is not italic."
  (markdown-test-string
   "not italic *\\*"
   (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/italics-4 ()
  "Test that escaped asterisk inside italics is not bold."
  (markdown-test-string
   "italic **\\**"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 12 markdown-italic-face)))

(ert-deftest test-markdown-font-lock/italics-5 ()
  "Test italic single letter."
  (markdown-test-string
   "*a*"
   (markdown-test-range-has-face 1 3 markdown-italic-face)))

(ert-deftest test-markdown-font-lock/italics-after-hr ()
  "Test italics after a horizontal rule with asterisks."
  (markdown-test-string "* * *\n\n*italic*\n"
   (markdown-test-range-has-face 1 5 markdown-header-face)
   (markdown-test-range-has-face 8 15 markdown-italic-face)))

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

(ert-deftest test-markdown-font-lock/bold-2 ()
  "Test space after leading asterisks or underscores."
  (markdown-test-string
   "This is ** not bold**, nor __ is this__ (but they match italics)."
   (markdown-test-range-has-face 1 8 nil)
   (markdown-test-range-has-face 9 20 markdown-italic-face)
   (markdown-test-range-has-face 21 27 nil)
   (markdown-test-range-has-face 28 38 markdown-italic-face)
   (markdown-test-range-has-face 39 (point-max) nil)))

(ert-deftest test-markdown-font-lock/bold-3 ()
  "Test escaped asterisk inside bold."
  (markdown-test-string
   "bold **\\***"
   (markdown-test-range-has-face 6 11 markdown-bold-face)))

(ert-deftest test-markdown-font-lock/bold-4 ()
  "Test bold single letter."
  (markdown-test-string
   "**a**"
   (markdown-test-range-has-face 1 5 markdown-bold-face)))

(ert-deftest test-markdown-font-lock/bold-after-hr ()
  "Test bold after a horizontal rule with asterisks."
  (markdown-test-string "* * *\n\n**bold**\n"
   (markdown-test-range-has-face 1 5 markdown-header-face)
   (markdown-test-range-has-face 8 15 markdown-bold-face)))

(ert-deftest test-markdown-font-lock/code-1 ()
  "A simple inline code test."
  (markdown-test-file "inline.text"
   (goto-char 45)
   (should (looking-at "`"))
   ;; Regular code span
   (markdown-test-range-has-face 45 50 markdown-inline-code-face)
   ;; Code containing backticks
   (markdown-test-range-has-face 61 89 markdown-inline-code-face)
   ;; Seven backquotes in a row
   (markdown-test-range-has-face 119 125 nil)
   ;; Backquotes at beginning or end
   (markdown-test-range-has-face 228 239 markdown-inline-code-face)
   (markdown-test-range-has-face 341 351 markdown-inline-code-face)
   ;; Backslash as final character
   (markdown-test-range-has-face 460 468 markdown-inline-code-face)
   ;; Escaping of leading backquotes
   (markdown-test-range-has-face 586 592 nil)
   (markdown-test-range-has-face 597 603 nil)
   ;; A code span crossing lines
   (markdown-test-range-has-face 652 656 nil)
   (markdown-test-range-has-face 657 666 markdown-inline-code-face)
   ;; Three backquotes: same line, across lines, not across blocks
   (markdown-test-range-has-face 695 748 nil)
   (markdown-test-range-has-face 749 757 markdown-inline-code-face)
   (markdown-test-range-has-face 758 805 nil)
   (markdown-test-range-has-face 806 814 markdown-inline-code-face)
   (markdown-test-range-has-face 815 891 nil)
   ))

(ert-deftest test-markdown-font-lock/code-2 ()
  "Multiple code spans in a row and on different lines."
  (markdown-test-string "`foo` `bar` `baz`"
   (markdown-test-range-has-face 1 5 markdown-inline-code-face)
   (markdown-test-range-has-face 6 6 nil)
   (markdown-test-range-has-face 7 11 markdown-inline-code-face)
   (markdown-test-range-has-face 12 12 nil)
   (markdown-test-range-has-face 13 17 markdown-inline-code-face))
  (markdown-test-string "`a`\n`b`\n`c`\n"
   (markdown-test-range-has-face 1 3 markdown-inline-code-face)
   (markdown-test-range-has-face 4 4 nil)
   (markdown-test-range-has-face 5 7 markdown-inline-code-face)
   (markdown-test-range-has-face 8 8 nil)
   (markdown-test-range-has-face 9 11 markdown-inline-code-face)
   (markdown-test-range-has-face 12 12 nil))
  (markdown-test-string "a`foo`b`bar`c`baz`d"
   (markdown-test-range-has-face 1 1 nil)
   (markdown-test-range-has-face 2 6 markdown-inline-code-face)
   (markdown-test-range-has-face 7 7 nil)
   (markdown-test-range-has-face 8 12 markdown-inline-code-face)
   (markdown-test-range-has-face 13 13 nil)
   (markdown-test-range-has-face 14 18 markdown-inline-code-face)
   (markdown-test-range-has-face 19 19 nil)))

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

(ert-deftest test-markdown-font-lock/pre-2 ()
  (markdown-test-string "* item\n\nreset baseline\n\n    pre block\n"
   (markdown-test-range-has-face 2 24 nil)
   (markdown-test-range-has-face 29 37 markdown-pre-face)))

(ert-deftest test-markdown-font-lock/pre-3 ()
  (markdown-test-string "It is interesting to see what happens when one queries
`social upheaval` and `protopalatial era`.

* `social upheaval`: the follwing queries have been tried:

    social upheaval subClassOf"
   (markdown-test-range-has-face 160 190 nil)))

(ert-deftest test-markdown-font-lock/atx-no-spaces ()
  "Test font-lock for atx headers with no spaces."
  (markdown-test-string "##abc##"
   (markdown-test-range-has-face 1 2 markdown-header-delimiter-face)
   (markdown-test-range-has-face 3 5 markdown-header-face-2)
   (markdown-test-range-has-face 6 7 markdown-header-delimiter-face))
  (markdown-test-string "##"
   (markdown-test-range-has-face 1 1 markdown-header-delimiter-face)
   (markdown-test-range-has-face 2 2 markdown-header-face-1))
  (markdown-test-string "###"
   (markdown-test-range-has-face 1 2 markdown-header-delimiter-face)
   (markdown-test-range-has-face 3 3 markdown-header-face-2)))

(ert-deftest test-markdown-font-lock/setext-1-letter ()
  "An edge case for level-one setext headers."
  (markdown-test-string "a\n=\n"
   (markdown-test-range-has-face 1 1 markdown-header-face-1)
   (markdown-test-range-has-face 3 3 markdown-header-rule-face)))

(ert-deftest test-markdown-font-lock/setext-2-letter ()
  "An edge case for level-two setext headers."
  (markdown-test-string "b\n-\n"
   (markdown-test-range-has-face 1 1 markdown-header-face-2)
   (markdown-test-range-has-face 3 3 markdown-header-rule-face)))

(ert-deftest test-markdown-font-lock/inline-links ()
  "Test font lock for inline links."
  (markdown-test-file "inline.text"
   (markdown-test-range-has-face 925 930 markdown-link-face)
   (markdown-test-range-has-face 931 950 markdown-url-face)
   (markdown-test-range-has-face 951 957 markdown-link-title-face)
   (markdown-test-range-has-face 958 958 markdown-url-face)))

(ert-deftest test-markdown-font-lock/pre-comment ()
  "Test comments inside of a pre block."
  (markdown-test-string "    <!-- pre, not comment -->"
   (markdown-test-range-has-face (point-min) (1- (point-max)) markdown-pre-face)))

(ert-deftest test-markdown-font-lock/footnote-markers-links ()
  "Test an edge case involving footnote markers and inline reference links."
  (markdown-test-string "Harvard[^1] [tuition][]"
   (markdown-test-range-has-face 8 11 markdown-footnote-face)
   (markdown-test-range-has-face 13 21 markdown-link-face)
   (markdown-test-range-has-face 22 23 markdown-reference-face)))

(ert-deftest test-markdown-font-lock/mmd-metadata ()
  "Basic MultMarkdown metadata tests."
  (markdown-test-string "Title: peg-multimarkdown User's Guide  
Author: Fletcher T. Penney  
Base Header Level: 2  "
   (markdown-test-range-has-face 1 5 markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 nil)
   (markdown-test-range-has-face 8 37 markdown-metadata-value-face)
   (markdown-test-range-has-face 41 46 markdown-metadata-key-face)
   (markdown-test-range-has-face 47 47 nil)
   (markdown-test-range-has-face 49 66 markdown-metadata-value-face)
   (markdown-test-range-has-face 70 86 markdown-metadata-key-face)
   (markdown-test-range-has-face 87 87 nil)
   (markdown-test-range-has-face 89 89 markdown-metadata-value-face))
  ;; Avoid triggering when a title contains a colon (e.g., Markdown: Syntax)
  (markdown-test-file "syntax.text"
   (markdown-test-range-has-face 1 16 markdown-header-face-1)))

(ert-deftest test-markdown-font-lock/mmd-metadata-after-header ()
  "Ensure that similar lines are not matched after the header."
  (markdown-test-string "Title: peg-multimarkdown User's Guide  

Author: Fletcher T. Penney  
Base Header Level: 2  "
   (markdown-test-range-has-face 1 5 markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 nil)
   (markdown-test-range-has-face 8 37 markdown-metadata-value-face)
   (markdown-test-range-has-face 40 67 nil)
   (markdown-test-range-has-face 71 90 nil)))

(ert-deftest test-markdown-font-lock/pandoc-metadata ()
  "Basic Pandoc metadata tests."
  (markdown-test-string "% title
  two-line title
% first author;
  second author
% date

body"
   (markdown-test-range-has-face 1 1 markdown-comment-face)
   (markdown-test-range-has-face 3 24 markdown-metadata-value-face)
   (markdown-test-range-has-face 26 26 markdown-comment-face)
   (markdown-test-range-has-face 28 56 markdown-metadata-value-face)
   (markdown-test-range-has-face 58 58 markdown-comment-face)
   (markdown-test-range-has-face 60 63 markdown-metadata-value-face)
   (markdown-test-range-has-face 64 69 nil)))

(ert-deftest test-markdown-font-lock/line-break ()
  "Basic line break tests."
  (markdown-test-string "    \nasdf  \n"
   (markdown-test-range-has-face 1 9 nil)
   (markdown-test-range-has-face 10 11 markdown-line-break-face)))

;;; Markdown Parsing Functions:

(ert-deftest test-markdown-parsing/reference-definition-basic ()
  "Test reference definition function."
  (markdown-test-file "syntax.text"
   ;; Test accuracy of returned text and bounds
   (should (equal (markdown-reference-definition "[1]")
                  (list "http://docutils.sourceforge.net/mirror/setext.html" 1942 1992)))
   (should (equal (markdown-reference-definition "[2]")
                  (list "http://www.aaronsw.com/2002/atx/" 2000 2032)))
   ;; Test that match data remains intact
   (should (string-equal (match-string 2) "http://www.aaronsw.com/2002/atx/"))
   ;; Test anchor-only relative URL
   (should (equal (markdown-reference-definition "[bq]")
                  (list "#blockquote" 7536 7547)))
   ;; Example references that appear in pre blocks in the text
   (should (not (markdown-reference-definition "[]")))
   (should (not (markdown-reference-definition "[id]")))
   (should (not (markdown-reference-definition "[foo]")))
   (should (not (markdown-reference-definition "[A]")))
   (should (not (markdown-reference-definition "[Google]")))
   ;; Test that we don't pick up other text in square brackets
   (should (not (markdown-reference-definition "[blockquoting]")))
   (should (not (markdown-reference-definition "[square brackets]")))
   ;; Test case insensitivity
   (should (equal (markdown-reference-definition "[SRC]")
                  (list "/projects/markdown/syntax.text" 1245 1275)))))

(ert-deftest test-markdown-parsing/get-defined-references ()
  "Test `markdown-get-defined-references'."
  (markdown-test-file "syntax.text"
   (should (equal (markdown-get-defined-references)
                  '("[src]" "[1]" "[2]" "[3]" "[4]" "[5]" "[6]" "[bq]" "[l]"))))
  (markdown-test-file "outline.text"
   (should (equal (markdown-get-defined-references) nil)))
  (markdown-test-file "wiki-links.text"
   (should (equal (markdown-get-defined-references) nil))))

(ert-deftest test-markdown-parsing/code-at-point-inline ()
  "Test `markdown-code-at-point-p'."

  (defun test-region (beg end)
    (goto-char (1- beg))
    (should-not (markdown-code-at-point-p))
    (goto-char (1+ end))
    (should-not (markdown-code-at-point-p))
    (dolist (loc (number-sequence beg end))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-beginning 0) beg))
      (should (equal (match-end 0) end))))

  (markdown-test-file "inline.text"
   (test-region 45 51) ; Regular code span
   (test-region 61 90) ; Code containing backticks
   (test-region 228 240) ; Backquotes at beginning
   (test-region 341 352) ; Backquotes at end
   (test-region 460 469) ; Backslash as final character
   (test-region 657 667) ; A code span crossing lines
   (test-region 749 758) ; Three backquotes on same line
   (test-region 806 815) ; Three backquotes across lines
   ))

(ert-deftest test-markdown-parsing/code-at-point-one-space ()
  "Test `markdown-code-at-point-p' with multiple code spans in a row."
  (markdown-test-string "`foo` `bar` `baz`"
    (dolist (loc (number-sequence 1 6))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 1 6 2 5))))
    (dolist (loc (number-sequence 7 12))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 7 12 8 11))))
    (dolist (loc (number-sequence 13 18))
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 13 18 14 17))))))

(ert-deftest test-markdown-parsing/code-at-point-no-space ()
  "Test `markdown-code-at-point-p' with multiple code spans in a row."
  (markdown-test-string "a`foo`b`bar`c`baz`d"
    (goto-char 1)                       ; "a"
    (should-not (markdown-code-at-point-p))
    (dolist (loc (number-sequence 2 7)) ; "`foo`b"
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 2 7 3 6))))
    (dolist (loc (number-sequence 8 13)) ; "`bar`c"
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 8 13 9 12))))
    (dolist (loc (number-sequence 14 19)) ; "`baz`d"
      (goto-char loc)
      (should (markdown-code-at-point-p))
      (should (equal (match-data) (list 14 19 15 18))))))

(ert-deftest test-markdown-parsing/code-at-point-blank-line ()
  "Test `markdown-code-at-point-p' at beginning of block."
  (markdown-test-string "----------\n\n## foo\n"
   (should-not (markdown-code-at-point-p))
   (forward-line)
   (should-not (markdown-code-at-point-p))
   (forward-line)
   (should-not (markdown-code-at-point-p))))


;;; Reference Checking:

(ert-deftest test-markdown-references/goto-line-button ()
  "Create and test a goto line button."
  (markdown-test-string "line 1\nline 2\n"
   ;; Store the temporary buffer with the text
   (let ((target (current-buffer)))
     ;; Create a new buffer for inserting
     (with-temp-buffer
       ;; Verify that point is in a different buffer
       (should (not (equal (current-buffer) target)))
       ;; Insert and press the button
       (insert-button "goto line 2"
                      :type 'markdown-goto-line-button
                      'target-buffer target
                      'target-line 2)
       (should (string-equal (buffer-string) "goto line 2"))
       (backward-button 1)
       (call-interactively 'push-button)
       ;; Verify that point is on line 2 of target buffer
       (should (= (line-number-at-pos) 2))
       (should (looking-at "line 2"))
       (should (equal (current-buffer) target))))))

(ert-deftest test-markdown-references/button-map ()
  "Verify that button-buffer-map is used for check references buffer."
  (markdown-test-string "[undefined][ref]\n"
   (let* ((target (buffer-name))
          (check (format "*Undefined references for %s*" target)))
   (markdown-check-refs)
   (with-current-buffer (get-buffer check)
     (should (equal (local-key-binding (kbd "TAB")) 'forward-button))
     (should (equal (local-key-binding (kbd "<backtab>")) 'backward-button))))))

;;; Lists:

(ert-deftest test-markdown-lists/levels-1 ()
  "Test list levels function `markdown-calculate-list-levels'."
  (markdown-test-file "nested-list.text"
   (let ((values '(((1 . 1) . nil) ((2 . 13) . (3)) ((14 . 23) . (7 3))
                   ((24 . 26) . (11 7 3)))))
     (loop for (range . value) in values
           do (goto-char (point-min))
              (forward-line (1- (car range)))
              (dotimes (n (- (cdr range) (car range)))
                (should (equal (markdown-calculate-list-levels) value))
                (forward-line))))))

(ert-deftest test-markdown-lists/levels-2 ()
  "Test list levels function `markdown-calculate-list-levels'."
  (markdown-test-file "syntax.text"
   (let ((values '(((1 . 13) . nil) ((14 . 14) . (0)) ((15 . 17) . (4 0))
                   ((18 . 18) . (0)) ((19 . 24) . (4 0)) ((25 . 25) . (0))
                   ((26 . 29) . (4 0)) ((30 . 30) . (0)) ((31 . 33) . (4 0))
                   ((34 . 588) . nil) ((589 . 595) . (0)) ((596 . 814) . nil)
                   ((815 . 820) . (0)) ((821 . 898) . nil))))
     (loop for (range . value) in values
           do (goto-char (point-min))
              (forward-line (1- (car range)))
              (dotimes (n (- (cdr range) (car range)))
                (should (equal (markdown-calculate-list-levels) value))
                (forward-line))))))

(ert-deftest test-markdown-lists/levels-interior ()
  "Test `markdown-calculate-list-levels' from inside a list item."
  (markdown-test-file "nested-list.text"
    (goto-char 36)
    (should (equal (markdown-calculate-list-levels) (list 3)))
    (goto-char 267)
    (should (equal (markdown-calculate-list-levels) (list 7 3)))
    (goto-char 540)
    (should (equal (markdown-calculate-list-levels) (list 11 7 3)))))

(ert-deftest test-markdown-lists/bounds-1 ()
  "Test list item bounds function `markdown-cur-list-item-bounds'."
  (markdown-test-file "lists.text"
    (markdown-test-goto-heading "Case 9")
    (forward-line)
    (should (eq (point) 3699))
    (markdown-next-list-item 4)
    (should (eq (point) 3700))
    (should (equal (markdown-cur-list-item-bounds)
                   (list 3700 3901 0 4 "-   ")))
    (markdown-next-list-item 4)
    (should (eq (point) 3903))
    (should (equal (markdown-cur-list-item-bounds)
                   (list 3903 3937 0 4 "*   ")))))

(ert-deftest test-markdown-lists/bounds-2 ()
  "Function `markdown-cur-list-item-bounds' should return nil outside of list items."
  (markdown-test-string "line one\n\n* item\n"
    (should (null (markdown-cur-list-item-bounds)))
    (forward-line)
    (should (null (markdown-cur-list-item-bounds)))
    (forward-line)
    (should (markdown-cur-list-item-bounds))))

(ert-deftest test-markdown-lists/promotion-and-demotion ()
  "Test function `markdown-promote-list-item'."
  (markdown-test-file "nested-list.text"
    (forward-line)
    (should (looking-at "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (markdown-demote-list-item)
    (should (looking-at "       - List level 1 item 2

         Second paragraph of item 2

            Nested pre block in item 2
            Four spaces past the marker

         Another paragraph of item 2"))
    (markdown-promote-list-item)
    (should (looking-at "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (goto-line 23)
    (should (looking-at "           - List level 3 item 1

                 Nested pre block"))
    (markdown-demote-list-item)
    (should (looking-at "               - List level 3 item 1

                     Nested pre block"))
    (markdown-promote-list-item)
    (should (looking-at "           - List level 3 item 1

                 Nested pre block"))))

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
   (let (last-command this-command)
     ;; Navigate to the second visible heading
     (outline-next-visible-heading 2)
     (should (eq (point) 69))
     (should (looking-at "^## A second-level header$"))
     ;; Cycle visibility of this subtree
     (setq this-command 'markdown-cycle)
     (markdown-cycle)
     (setq last-command 'markdown-cycle)
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
     (markdown-test-range-has-property 93 349 'invisible nil))))

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

;;; Movement tests:

(ert-deftest test-markdown-movement/defun ()
  "Test defun navigation."
  (markdown-test-file "outline.text"
   ;; end-of-defun should go to point-max
   (end-of-defun 10)
   (should (= (point) (point-max)))
   ;; end-of-defun should stop just before the next header
   (goto-char (point-min))
   (end-of-defun)
   (should (looking-at "\n# A top-level header"))
   (end-of-defun)
   (should (looking-at "\n## A second-level header"))
   (end-of-defun)
   (should (looking-at "\n### Third level ###"))
   (end-of-defun)
   (should (looking-at "\n### Third level number two ###"))
   ;; beginning-of-defun should move to the start of the previous header
   (beginning-of-defun)
   (should (looking-at "### Third level ###"))
   (beginning-of-defun)
   (should (looking-at "## A second-level header"))
   (beginning-of-defun)
   (should (looking-at "# A top-level header"))
   (beginning-of-defun)
   ;; beginning-of-defun should move up to point-min
   (should (= (point) (point-min)))))

(ert-deftest test-markdown-movement/block ()
  "Test block movement."
  (markdown-test-file "outline.text"
   (markdown-end-of-block)
   (should (looking-at "\n# A top-level header"))
   (markdown-end-of-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-end-of-block)
   (should (looking-at "\n## A second-level header"))
   (markdown-end-of-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-end-of-block)
   (should (looking-at "\n### Third level ###"))
   (markdown-end-of-block)
   (should (looking-at "\n\\* A list item"))
   (markdown-end-of-block)
   (should (looking-at "\n### Third level number two ###"))
   (markdown-end-of-block)
   (should (looking-at "\n### Level two again"))
   (markdown-end-of-block)
   (should (looking-at "\nfollowed by some body text"))

   (markdown-test-goto-heading "Level two")
   (markdown-end-of-block)
   (should (looking-at "\nbar"))
   (markdown-end-of-block)
   (should (= (point) (point-max)))
   (markdown-beginning-of-block)
   (should (looking-at "bar"))
   (markdown-beginning-of-block)
   (should (looking-at "## Level two"))
   (markdown-beginning-of-block)
   (should (looking-at "foo"))
   (markdown-beginning-of-block)
   (should (looking-at "# Level one"))
   (markdown-beginning-of-block)
   (should (looking-at "* With"))
   (markdown-beginning-of-block)
   (should (looking-at "And a level two underline header"))

   (goto-char (point-min))
   (markdown-test-goto-heading "A top-level header")
   (beginning-of-line)
   (markdown-beginning-of-block)
   (should (= (point) (point-min)))))

(ert-deftest test-markdown-movement/reference-definition ()
  "Test jumping to reference definitions."
  ;; Jumping to explicit reference definition
  (markdown-test-string "[a][ref]\n\n[ref]: gopher://localhost/\n"
   (markdown-reference-goto-definition)
   (should (= (point) 18)))
  ;; Jumping to implicit reference definition
  (markdown-test-string "[a][]\n\n[a]: ftp://localhost/\n"
   (markdown-reference-goto-definition)
   (should (= (point) 13)))
  ;; Creating non-existent reference definition
  (markdown-test-string "[a][]\n"
   (markdown-reference-goto-definition)
   (should (= (point) 13))
   (should (string-equal (buffer-string) "[a][]\n\n[a]: "))))

;;; Wiki link tests:

(ert-deftest test-markdown-wiki-link/aliasing ()
  "Test filename extraction for aliased wiki links."
  (markdown-test-file "wiki-links.text"
   ;; Confirm location of first wiki link
   (should (eq (markdown-next-link) 8))
   ;; Confirm location of second wiki link
   (should (eq (markdown-next-link) 73))
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
   (should (eq (markdown-next-link) 8))
   ;; Advance to second link
   (should (eq (markdown-next-link) 73))
   ;; Avance to final link
   (should (eq (markdown-next-link) 155))
   ;; Return nil and don't advance point
   (should (eq (markdown-next-link) nil))
   (should (eq (point) 155))
   ;; Move back to second link
   (should (eq (markdown-previous-link) 73))
   ;; Move back to first link
   (should (eq (markdown-previous-link) 8))
   ;; Return nil and don't move point
   (should (eq (markdown-previous-link) nil))
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
     (should (eq (markdown-next-link) 8))
     ;; First wiki link doesn't have a corresponding file
     (markdown-test-range-has-property 8 20 'font-lock-face markdown-missing-link-face)
     ;; Second wiki link doesn't have a corresponding file
     (should (eq (markdown-next-link) 73))
     (markdown-test-range-has-property 73 88 'font-lock-face markdown-missing-link-face)
     ;; Move to third wiki link, and create the missing file
     (should (eq (markdown-next-link) 155))
     (should (string-equal (markdown-wiki-link-link) "inline"))
     (markdown-test-range-has-property 155 164 'font-lock-face markdown-link-face)
     ;; Remove temporary files
     (delete-file fn)
     )))

;;; Filling tests:

(ert-deftest test-markdown-filling/blockquote ()
  "Test filling of blockquotes.
See `adaptive-fill-first-line-regexp'."
  (markdown-test-string "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n> eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/list-item-plus ()
  "Test filling of list items with plus sign markers.
See `adaptive-fill-regexp'."
  (markdown-test-string "  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) "  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n    eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/list-item-plus-in-blockquote ()
  "Test filling of list items with plus sign markers inside blockquote.
See `adaptive-fill-regexp'."
  (markdown-test-string ">  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) ">  + Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n>    eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/line-break ()
  "Test filling of paragraphs with hard line breaks.
See `paragraph-separate'."
  (markdown-test-string "Lorem ipsum dolor sit amet,  \nconsectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (let ((fill-column 70))
     (fill-paragraph)
     (should (string-equal (buffer-string) "Lorem ipsum dolor sit amet,  \nconsectetur adipisicing elit, sed do eiusmod tempor incididunt ut\nlabore et dolore magna aliqua.")))))

(ert-deftest test-markdown-filling/decimal-number-at-beginning ()
  "Test filling when a number with a decimal appears at the beginning of a line."
  (markdown-test-string "The circumference of a circle divided by it's radius is around\n3.14."
   (fill-paragraph)
   (should (string-equal (buffer-string) "The circumference of a circle divided by it's radius is around 3.14."))))

;;; Export tests:

(ert-deftest test-markdown-hook/xhtml-standalone ()
  "Test `markdown-xhtml-standalone-regexp' and `markdown-output-standalone-p'."
  (should (string-match markdown-xhtml-standalone-regexp
                        "<?xml version='1.0' encoding='UTF-8'?>"))
  (should (string-match markdown-xhtml-standalone-regexp
                        "<!DOCTYPE html>"))
  (should (string-match markdown-xhtml-standalone-regexp
                        "<html>"))
  (should-not (string-match markdown-xhtml-standalone-regexp
                            "<h1>title</h1>"))
  (should-not (string-match markdown-xhtml-standalone-regexp
                            "<div id=\"name\">")))

;;; Hook tests:

(ert-deftest test-markdown-hook/before-export ()
  "Test hook run before export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((before-hook-run nil)
          (orig-point (point))
          (func (lambda ()
                  ;; Change value of a variable
                  (setq before-hook-run t)
                  ;; Insert some text
                  (goto-char (point-min))
                  (insert "#")
                  ;; Deliberately move the point
                  (end-of-line)
                  ;; Verify changes
                  (should (looking-back "^## List Cases"))
                  (should-not (= (point) orig-point))))
          (ofile (progn
                   ;; Register hook
                   (add-hook 'markdown-before-export-hook func)
                   ;; Export XHTML and return filename
                   (markdown-export)))
          (obuffer (get-file-buffer ofile)))
     ;; Test side effects of hook
     (should (eq before-hook-run t))
     ;; Test position of point
     (should (= (point) orig-point))
     ;; Test that buffer was restored to original state
     (goto-char (point-min))
     (should (looking-at "^# List Cases"))
     ;; Clean
     (remove-hook 'markdown-before-export-hook func)
     (kill-buffer obuffer)
     (delete-file ofile))))

(ert-deftest test-markdown-hook/after-export ()
  "Test hook run after export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((after-hook-run nil)
          (func (lambda ()
                  ;; Change variable value
                  (setq after-hook-run t)
                  ;; Add comment to output buffer
                  (goto-char (point-min))
                  (insert "<!-- after-export-hook -->\n")))
          (ofile (progn
                   ;; Register hook
                   (add-hook 'markdown-after-export-hook func)
                   ;; Export XHTML and return filename
                   (markdown-export)))
          (obuffer (get-file-buffer ofile)))
     (message "obuffer = %S" obuffer)
     ;; Test that variable was changed
     (should (eq after-hook-run t))
     ;; Test that output buffer remains open
     (should (get-buffer obuffer))
     ;; Test that output buffer modification remains
     (with-current-buffer obuffer
       (goto-char (point-min))
       (should (looking-at "<!-- after-export-hook -->\n")))
     ;; Test that buffer modification was saved
     (should-not (buffer-modified-p obuffer))
     ;; Clean up
     (remove-hook 'markdown-after-export-hook func)
     (kill-buffer obuffer)
     (delete-file ofile))))

;;; Extension: math support

(ert-deftest test-markdown-math/file-local-variable ()
  "Test enabling math mode via `hack-local-variables-hook'."
  (markdown-test-file "math.text"
   (should-not markdown-enable-math)
   (hack-local-variables)
   (should markdown-enable-math)))

(ert-deftest test-markdown-math/reload ()
  "Test enabling math mode via function `markdown-enable-math'."
  (markdown-test-file "math.text"
    (markdown-enable-math t)
    ;; Flag should be set to t
    (should markdown-enable-math)
    ;; Font-lock keywords should be updated
    (should (member (cons markdown-regex-math-display 'markdown-math-face)
                    markdown-mode-font-lock-keywords))))

(ert-deftest test-markdown-math/font-lock ()
  "Test markdown math mode."
  (markdown-test-file "math.text"
   (markdown-enable-math t)
   (font-lock-fontify-buffer)
   (markdown-test-range-has-face 1 32 nil)
   (markdown-test-range-has-face 33 46 markdown-math-face)
   (markdown-test-range-has-face 47 49 nil)
   (markdown-test-range-has-face 50 65 markdown-math-face)
   (markdown-test-range-has-face 66 98 nil)
   (markdown-test-range-has-face 99 114 markdown-math-face)))

;;; gfm-mode tests:

(ert-deftest test-markdown-gfm/pre-1 ()
  "GFM pre block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2626 2637 nil)
    (markdown-test-range-has-face 2639 2641 markdown-pre-face)
    (markdown-test-range-has-face 2642 2645 markdown-language-keyword-face)
    (markdown-test-range-has-face 2647 2728 markdown-pre-face)
    (markdown-test-range-has-face 2730 2732 markdown-pre-face)))

(ert-deftest test-markdown-gfm/italic-1 ()
  "GFM italic font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 1483 1488 markdown-italic-face)
    (markdown-test-range-has-face 1729 1790 nil)))

(ert-deftest test-markdown-gfm/insert-code-block ()
  "GFM code block insertion test."
  ;; Test empty markup insertion
  (markdown-test-string-gfm "line 1\nline 2\n"
   (end-of-line)
   (markdown-insert-gfm-code-block "elisp")
   (should (string-equal (buffer-string)
                         "line 1\n\n```elisp\n\n```\n\nline 2\n")))
  ;; Test with active region
  (markdown-test-string-gfm "line 1\nline 2\nline 3\n"
   (forward-line)
   (transient-mark-mode)
   (push-mark (point) t t)
   (end-of-line)
   (should (markdown-use-region-p))
   (markdown-insert-gfm-code-block "elisp")
   (should (string-equal (buffer-string)
                         "line 1\n\n```elisp\nline 2\n```\n\nline 3\n"))))

(ert-deftest test-markdown-gfm/code-block-font-lock ()
  "GFM code block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2639 2641 markdown-pre-face) ; ```
    (markdown-test-range-has-face 2642 2645 markdown-language-keyword-face) ; lang
    (markdown-test-range-has-face 2647 2728 markdown-pre-face) ; code
    (markdown-test-range-has-face 2730 2732 markdown-pre-face))) ; ```

(ert-deftest test-markdown-gfm/code-block-font-lock-2 ()
  "GFM code block font lock test without language identifier."
  (markdown-test-string-gfm "Plain code block:\n\n```\nfoo\n```\n"
    (markdown-test-range-has-face 20 22 markdown-pre-face)
    (markdown-test-range-has-face 24 26 markdown-pre-face)
    (markdown-test-range-has-face 28 30 markdown-pre-face)))

(provide 'markdown-test)

;;; markdown-test.el ends here
