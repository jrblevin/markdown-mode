;;;; markdown-test.el --- Tests for markdown-mode

;; Copyright (C) 2013-2017 Jason R. Blevins <jblevins@xbeta.org>
;; Copyright (C) 2013 Makoto Motohashi <mkt.motohashi@gmail.com>
;; Copyright (C) 2015 Google, Inc. (Contributor: Samuel Freilich <sfreilich@google.com>)

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains the `markdown-mode' test suite.  To run the tests:
;;
;;     M-x load-file RET markdown-test.el RET
;;     M-x markdown-test RET

;;; Code:

(require 'markdown-mode)
(require 'ert)
(require 'cl-lib)

(defconst markdown-test-dir
  (expand-file-name (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst markdown-test-font-lock-function
  (if (and noninteractive (fboundp 'font-lock-ensure))
      #'font-lock-ensure #'font-lock-fontify-buffer))

(defmacro markdown-test-string-mode (mode string &rest body)
  "Run BODY in a temporary buffer containing STRING in MODE."
  (declare (indent 2))
  `(let ((win (selected-window)))
     (unwind-protect
         (with-temp-buffer
           (set-window-buffer win (current-buffer) t)
           (erase-buffer)
           (funcall ,mode)
           (setq-default indent-tabs-mode nil)
           (insert ,string)
           (goto-char (point-min))
           (funcall markdown-test-font-lock-function)
           (prog1 ,@body (kill-buffer))))))

(defmacro markdown-test-file-mode (mode file &rest body)
  "Open FILE from `markdown-test-dir' in MODE and execute BODY."
  (declare (indent 2))
  `(let ((fn (concat markdown-test-dir ,file)))
     (save-window-excursion
       (with-temp-buffer
         (insert-file-contents fn)
         (funcall ,mode)
         (goto-char (point-min))
         (funcall markdown-test-font-lock-function)
         ,@body))))

(defmacro markdown-test-string (string &rest body)
  "Run BODY in a temporary buffer containing STRING in `markdown-mode'."
  (declare (indent 1))
  `(markdown-test-string-mode 'markdown-mode ,string ,@body))
(def-edebug-spec markdown-test-string (form body))

(defmacro markdown-test-file (file &rest body)
  "Open FILE from `markdown-test-dir' in `markdown-mode' and execute BODY."
  (declare (indent 1))
  `(markdown-test-file-mode 'markdown-mode ,file ,@body))
(def-edebug-spec markdown-test-file (form body))

(defmacro markdown-test-string-gfm (string &rest body)
  "Run BODY in a temporary buffer containing STRING in `gfm-mode'."
  (declare (indent 1))
  `(markdown-test-string-mode 'gfm-mode ,string ,@body))
(def-edebug-spec markdown-test-string-gfm (form body))

(defmacro markdown-test-file-gfm (file &rest body)
  "Open FILE from `markdown-test-dir' in `gfm-mode' and execute BODY."
  (declare (indent 1))
  `(markdown-test-file-mode 'gfm-mode ,file ,@body))
(def-edebug-spec markdown-test-file-gfm (form body))

(defmacro markdown-test-temp-file (file &rest body)
  "Open FILE from `markdown-test-dir' visiting temp file and execute BODY.
This file is not saved."
  (declare (indent 1))
  `(let ((fn (concat markdown-test-dir ,file))
         (tmp (make-temp-file "markdown-test" nil ".text"))
         buf)
     (save-window-excursion
       (unwind-protect
           (progn
             (setq buf (find-file tmp))
             (insert-file-contents fn)
             (markdown-mode)
             (goto-char (point-min))
             (funcall markdown-test-font-lock-function)
             ,@body
             (set-buffer-modified-p nil))
         (when (buffer-live-p buf) (kill-buffer buf))
         (delete-file tmp)))))
(def-edebug-spec markdown-test-temp-file (form body))

(defun markdown-test-report-property-range (begin end prop)
  "Report buffer substring and property PROP from BEGIN to END."
  (message "Buffer substring: %s" (buffer-substring begin (1+ end)))
  (message "Properties in range are as follows:")
  (dolist (loc (number-sequence begin end))
    (message "%d: %s" loc (get-char-property loc prop))))

(defun markdown-test-range-has-property (begin end prop value)
  "Verify that range BEGIN to END has PROP equal to or containing VALUE."
  (let (vals fail-loc)
    (setq fail-loc
          (catch 'fail
            (dolist (loc (number-sequence begin end))
              (setq vals (get-char-property loc prop))
              (if (and vals (listp vals))
                  (unless (memq value vals)
                    (throw 'fail loc))
                (unless (eq vals value)
                  (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (markdown-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun markdown-test-range-property-equals (begin end prop value)
  "Verify that range BEGIN to END has property PROP equal to VALUE."
  (let ((fail-loc
         (catch 'fail
           (dolist (loc (number-sequence begin end))
             (unless (eq (get-char-property loc prop) value)
               (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (markdown-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun markdown-test-range-has-face (begin end face)
  "Verify that the range from BEGIN to END has face FACE."
  (markdown-test-range-has-property begin end 'face face))

(defun markdown-test-range-face-equals (begin end face)
  "Verify that the range from BEGIN to END has face equal to FACE."
  (markdown-test-range-property-equals begin end 'face face))

(defun markdown-test-goto-heading (title)
  "Move the point to section with TITLE."
  (let ((regexp (format "\\(^#+ %s\\( #+\\)?\\|^%s\n[=-]+\n\\)" title title)))
    (if (re-search-forward regexp nil t)
        (goto-char (match-end 0)))))

(defun markdown-command-identity (begin end output-buffer)
  "A placeholder `markdown-command' function for testing.
Extracts region from BEGIN to END and inserts in OUTPUT-BUFFER."
  (let ((text (buffer-substring-no-properties begin end)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert text))))

(defun markdown-test ()
  "Run all defined test cases for `markdown-mode'."
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
                      (should (looking-back "A Markdown-formatted\n" nil))
                      (should (not (markdown-prev-line-blank-p)))
                      (markdown-ensure-blank-line-before)
                      (should (looking-back "A Markdown-formatted\n\n" nil))
                      (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-2 ()
  "Test function `markdown-ensure-blank-line-before' in middle of line."
  (markdown-test-file "syntax.text"
                      (search-forward "as plain text")
                      (should (= (point) 1556))
                      (should (looking-back "as plain text" nil))
                      (should (not (markdown-prev-line-blank-p)))
                      (markdown-ensure-blank-line-before)
                      (should (looking-back "as plain text\n\n" nil))
                      (should (markdown-prev-line-blank-p))))

(ert-deftest test-markdown-insertion/blank-line-before-3 ()
  "Test function `markdown-ensure-blank-line-before' with blank line before."
  (markdown-test-file "syntax.text"
                      (search-forward "web.\n\nMarkdown is not a replacement for HTML")
                      (beginning-of-line)
                      (should (= (point) 2704))
                      (should (looking-back "web.\n\n" nil))
                      (should (markdown-prev-line-blank-p))
                      (markdown-ensure-blank-line-before)
                      (should (= (point) 2704))
                      (should (looking-back "web.\n\n" nil))
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
                                      markdown-regex-italic 1 3)
                                     (cons 2859 2862)))
                      (should (= (point) 2859))
                      ;; Unwrap *publishing*
                      (goto-char 3064)
                      (should (thing-at-point-looking-at markdown-regex-italic))
                      (should (equal (markdown-unwrap-thing-at-point
                                      markdown-regex-italic 1 3)
                                     (cons 3060 3070)))
                      (should (= (point) 3063))
                      ;; Unwrap *writing*
                      (goto-char 3101)
                      (should (thing-at-point-looking-at markdown-regex-italic))
                      (should (equal (markdown-unwrap-thing-at-point
                                      markdown-regex-italic 1 3)
                                     (cons 3093 3100)))
                      (should (= (point) 3100))))

(ert-deftest test-markdown-insertion/unwrap-things-in-region-italic ()
  "Test function `markdown-unwrap-things-in-region' on italics."
  (markdown-test-file "syntax.text"
                      (should (equal (markdown-unwrap-things-in-region
                                      2704 3207 markdown-regex-italic 1 3)
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

(ert-deftest test-markdown-insertion/toggle-kbd ()
  "Test toggling functionality of `markdown-insert-code'."
  (markdown-test-string "test <kbd>C-c C-s k</kbd> toggle"
                        (forward-word 2)
                        (markdown-insert-kbd)
                        (should (string-equal (buffer-string) "test C-c C-s k toggle"))
                        (should (= (point) 6))
                        (backward-word)
                        (markdown-insert-kbd)
                        (should (string-equal (buffer-string) "<kbd>test</kbd> C-c C-s k toggle"))
                        (should (= (point) 6))))

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

(ert-deftest test-markdown-insertion/kbd-region ()
  "Test region functionality of `markdown-insert-kbd'."
  (markdown-test-string "one two three"
                        (transient-mark-mode)
                        (push-mark (point) t t)
                        (forward-word 2)
                        (markdown-insert-kbd)
                        (should (string-equal (buffer-string) "<kbd>one two</kbd> three"))
                        (should (= (point) 13))))

(ert-deftest test-markdown-insertion/atx-line ()
  "Test ATX header insertion without region."
  (markdown-test-string "line one\nline two\n"
                        (forward-word)
                        (markdown-insert-header-atx-1)
                        (should (= (point) 11))
                        (should (string-equal (buffer-substring (point-min) (point-max))
                                              "# line one #\n\nline two\n"))
                        (forward-line 2)
                        (markdown-insert-header-atx-2)
                        (should (= (point) 26))
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
                        (should (= (point) 16))
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

(ert-deftest test-markdown-insertion/atx-asymmetric-point ()
  "Test point after ATX header insertion with `markdown-asymmetric-header'."
  (markdown-test-string
   "Test"
   (let ((markdown-asymmetric-header t))
     (markdown-insert-header-atx-5)
     (should (= (point) 11))
     (should (string-equal (buffer-string) "##### Test")))))

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

(ert-deftest test-markdown-insertion/header-setext-dwim ()
  "Test 'do what I mean' header insertion with setext headers."
  (markdown-test-string
   "asdfasfasfdsadfasdfasdf\n======="
   (goto-char 12)
   (call-interactively 'markdown-insert-header-dwim)
   (should (string-equal
            (buffer-string)
            "asdfasfasfdsadfasdfasdf\n======================="))))

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

(ert-deftest test-markdown-insertion/blockquote-region-with-newline ()
  (markdown-test-string "a\n\nb\n"
    (markdown-blockquote-region 1 (point-max))
    (should (equal (buffer-string) "> a\n>\n> b\n\n"))))

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
                        (should (string-equal (buffer-string) "abc\n  * "))
                        (should (= (point) 9)))
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
  ;; Following a fancy list item, on the next line
  (markdown-test-string "#. foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "#. foo\n#. ")))
  ;; Following a nested ordered list item
  (markdown-test-string "6. foo\n    1. bar"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "6. foo\n    1. bar\n    2. ")))
  ;; Preceding an ordered list item
  (markdown-test-string "\n1. foo\n2. bar"
                        (goto-char (point-min))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string) "1. \n1. foo\n2. bar")))
  ;; Preserve previous spacing in ordered list
  (markdown-test-string "1.        foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "1.        foo\n2.        ")))
  ;; Adjust spacing for number width changes (e.g., 9 -> 10)
  (markdown-test-string "9.  foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "9.  foo\n10. ")))
  ;; Don't adjust spacing for number width changes if no extra whitespace
  (markdown-test-string "99. foo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "99. foo\n100. ")))
  ;; Don't adjust spacing if tabs are used as whitespace
  (markdown-test-string "9.\tfoo"
                        (goto-char (point-max))
                        (call-interactively 'markdown-insert-list-item)
                        (should (string-equal (buffer-string)  "9.\tfoo\n10.\t"))))

(ert-deftest test-markdown-insertion/nested-list-marker ()
  "Test marker detection for `markdown-insert-list-item'."
  (markdown-test-string
   "1. A\n    * AA\n        1. AAA"
   (goto-char (point-max))
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-list-item))
   (should (eq (point) 36))
   (should (looking-back "\* "))
   (should (string-equal
            (buffer-string)
            "1. A\n    * AA\n        1. AAA\n    * "))
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-list-item))
   (should (eq (point) 40))
   (should (looking-back "2\. "))
   (should (string-equal
            (buffer-string)
            "1. A\n    * AA\n        1. AAA\n    * \n2. "))
   (let ((current-prefix-arg '(4)))
     (call-interactively 'markdown-insert-list-item))
   (should (eq (point) 44))
   (should (looking-back "3\. "))
   (should (string-equal
            (buffer-string)
            "1. A\n    * AA\n        1. AAA\n    * \n2. \n3. "))))

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
                          (goto-char (point-min))
                          (forward-line 4)
                          (should (looking-at "\\[link\\]: http://jblevins.org/")))))

(ert-deftest test-markdown-insertion/reference-link-immediately ()
  "Basic reference link insertion test for 'immediately location."
  (let ((markdown-reference-location 'immediately))
    (markdown-test-string "first para\n\nsecond para\n"
                          (end-of-line)
                          (markdown-insert-reference-link "link" "" "http://jblevins.org/")
                          (should (= (point) 19))
                          (goto-char (point-min))
                          (forward-line 2)
                          (should (looking-at "\\[link\\]: http://jblevins.org/")))))

(ert-deftest test-markdown-insertion/reference-link-header ()
  "Basic reference link and definition insertion test for 'header location."
  (let ((markdown-reference-location 'header))
    (markdown-test-string "par one\n\npar two\n\n### header\n"
                          (end-of-line)
                          (markdown-insert-reference-link "link" "")
                          (markdown-insert-reference-definition "link")
                          (should (= (point) 35))
                          (should (looking-back "\\[link\\]: " nil)))))

(ert-deftest test-markdown-insertion/reference-definition-block ()
  "Test whitespace when inserting a reference definition among others"
  (let ((markdown-reference-location 'header))
    (markdown-test-string "text

[1]: https://www.gnu.org/

### header
"
      (markdown-insert-reference-definition "2")
      (should (= (point) 38))
      (should (looking-back "https://www.gnu.org/\n\\[2\\]: " nil)))))

(ert-deftest test-markdown-insertion/reference-link-before-file-locals ()
  "Test inserting a reference link before file-local variables."
  (markdown-test-string "

<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->
"
    (markdown-insert-reference-link "link" "" "http://jblevins.org/")
    (should (equal (buffer-substring-no-properties 1 (point-max))
                   "[link][]

\[link]: http://jblevins.org/

<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->
"))
    (should (equal (point) 9))))

(ert-deftest test-markdown-insertion/inline-to-reference-link ()
  "Inline link to reference link conversion with tab completion."
  (markdown-test-string "[text](http://jblevins.org/ \"title\")"
                        (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-link RET M-DEL M-DEL M-DEL [1] RET RET h TAB RET RET"))
                        (should (string-equal (buffer-string) "[text][1]\n\n[1]: http://jblevins.org/ \"title\"\n"))))

(ert-deftest test-markdown-insertion/inline-to-reference-link-2 ()
  "Inline link to reference link conversion with existing reference links."
  (markdown-test-string "[text](http://jblevins.org/ \"title\")\n\n[1]: https://www.gnu.org"
                        (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-link RET M-DEL M-DEL M-DEL [1] RET RET"))
                        (should (string-equal (buffer-string) "[text][1]\n\n[1]: https://www.gnu.org"))))

(ert-deftest test-markdown-insertion/inline-link-angle-url-at-point ()
  "Test `markdown-insert-link' with angle URL at point."
  (markdown-test-string "<https://www.gnu.org/>"
    (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-link RET RET GNU RET RET"))
    (should (string-equal (buffer-string) "[GNU](https://www.gnu.org/)"))))

(ert-deftest test-markdown-insertion/inline-link-plain-url-at-point ()
  "Test `markdown-insert-link' with plain URL at point."
  (markdown-test-string "https://www.gnu.org/"
    (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-link RET RET GNU RET RET"))
    (should (string-equal (buffer-string) "[GNU](https://www.gnu.org/)"))))

(ert-deftest test-markdown-insertion/inline-link-reference-link-at-point ()
  "Test `markdown-insert-link' with reference link at point."
  (markdown-test-string ""
    (markdown-insert-reference-link "link" "label" "http://jblevins.org/")
    (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-link RET DEL DEL DEL DEL DEL DEL DEL http://example.com/ RET RET RET"))
    (should (string-equal (buffer-substring 1 28) "[link](http://example.com/)"))
    (should (= (point) 28))))

(ert-deftest test-markdown-insertion/inline-link-active-region ()
  "Test `markdown-insert-link' with active region."
  (markdown-test-string "abc def ghi"
    (let ((tmm-orig transient-mark-mode))
      (transient-mark-mode 1)
      (push-mark (point) t t)
      (forward-word 2)
      (execute-kbd-macro (read-kbd-macro "M-x markdown-insert-link RET http://example.com/ RET RET RET"))
      (should (string-equal (buffer-string) "[abc def](http://example.com/) ghi"))
      (should (= (point) 31))
      (transient-mark-mode tmm-orig))))

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
                          (should (looking-back "\\[^1\\]: " nil))
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
                          (should (looking-back "\\[^2\\]: " nil))
                          (insert "footnote text")
                          ;; return to marker
                          (markdown-footnote-return)
                          (should (= (point) 15))
                          (should (looking-back "\\[^2\\]" nil))
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
                          (should (looking-back "\\[^1\\]: " nil))
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
                          (should (looking-back "\\[^1\\]: " nil))
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
                          (should (looking-back "\\[^2\\]: " nil))
                          (insert "footnote text")
                          ;; return to marker
                          (markdown-footnote-return)
                          (should (= (point) 12))
                          (should (looking-back "\\[^2\\]" nil))
                          ;; kill with point at marker
                          (let (kill-ring)
                            (markdown-footnote-kill))
                          (should (= (point) 8))
                          (should (eolp))
                          (should (string-equal (buffer-string)
                                                "par one\n\npar two\n\n### header\n")))))

(ert-deftest test-markdown-footnote/basic-subtree ()
  "Basic footnote insertion and deletion tests for 'subtree location."
  (let ((markdown-footnote-location 'subtree))
    (markdown-test-string "# h1\n\nfoo\n\n## h2\n\nbar\n"
      ;; new buffer with no footnotes
      (should (= markdown-footnote-counter 0))
      ;; footnote insertion
      (forward-line 2)
      (end-of-line)
      (markdown-insert-footnote)
      (should (= (point) 34))
      (should (= markdown-footnote-counter 1))
      (should (looking-back "\\[^1\\]: " nil)))))

(ert-deftest test-markdown-footnote/kill-empty-text ()
  "Test killing a footnote with marker but no text."
  (markdown-test-string "no text[^1]\n\n[^1]: \n"
                        (end-of-line)
                        (markdown-footnote-goto-text)
                        (should (looking-back "\\[^1\\]: " nil))
                        (let (kill-ring)
                          (markdown-footnote-kill))
                        (should (string-equal (buffer-string) "no text\n"))))

(ert-deftest test-markdown-footnote/kill-empty-after ()
  "Test killing an empty footnote after one with text (previously killed the
footnote with text above)."
  (markdown-test-string "[^with-text][^no-text]\n\n[^with-text]: Text\n[^no-text]:"
                        (let (kill-ring)
                          (forward-line 3)
                          (should (looking-at "\\[\\^no-text\\]:$"))
                          (markdown-footnote-kill)
                          (should (string-equal (current-kill 0) "")))))

(ert-deftest test-markdown-footnote/kill-hanging-paras ()
  "Test killing a footnote where block text starts after the label (previously
killed the footnote above)."
  (markdown-test-string "[^1][^2]\n\n[^1]: Foo\n\n[^2]:\n    Text\n\n    More text\n\n\nNot indented"
                        (let (kill-ring)
                          (forward-line 4)
                          (should (looking-at "\\[\\^2\\]:$"))
                          (markdown-footnote-kill)
                          ;; We want to include the leading space on hanging footnote paragraphs,
                          ;; even if a hanging paragraph is the first item in the footnote.
                          (should (string-equal (current-kill 0) "Text\n\n    More text\n")))))

(ert-deftest test-markdown-footnote/text-positions-buffer-top ()
  "Test markdown-footnote-text-positions on footnote adjacent to buffer top
(was infinite loop)."
  (markdown-test-string "[^label]: text\n    more text"
   (should (equal (markdown-footnote-text-positions) (list "^label" 1 29)))))

(ert-deftest test-markdown-footnote/text-positions-buffer-top-one-line ()
  "Test markdown-footnote-text-positions on one-line footnote adjacent to
buffer top (failed to find positions)."
  (markdown-test-string "[^label]: text\n"
                        (should (equal (markdown-footnote-text-positions) (list "^label" 1 16)))))

(ert-deftest test-markdown-footnote/text-positions-buffer-top-not-footnote ()
  "Test markdown-footnote-text-positions on plain paragraph adjacent to buffer
top (was infinite loop)."
  (markdown-test-string "text\n    more text\n"
                        (should (eq (markdown-footnote-text-positions) nil))))

(ert-deftest test-markdown-footnote/text-positions-buffer-bottom ()
  "Test markdown-footnote-text-positions on footnote adjacent to buffer bottom
(was infinite loop)."
  (markdown-test-string "\n[^label]: text\n    more text"
   (forward-line 1)
   (should (equal (markdown-footnote-text-positions) (list "^label" 2 30)))))

(ert-deftest test-markdown-footnote/kill-adjacent-footnote ()
  "Test killing a footnote adjacent to other one-line footnotes (previously
killed the wrong one)."
  (markdown-test-string "Text[^1] with[^2] footnotes[^3]\n\n[^1]: foo\n[^2]: bar\n[^3]: baz"
                        (let (kill-ring)
                          (forward-line 3)
                          (should (looking-at "\\[\\^2\\]: bar"))
                          (markdown-footnote-kill)
                          (should (string-equal (current-kill 0) "bar\n")))))

(ert-deftest test-markdown-footnote/kill-adjacent-markers ()
  "Test killing a footnote where the labels are adjacent (previously, the wrong
footnote would be killed because the attempt to jump to the marker would jump to
the opening bracket of [^2], and then subsequent functions would kill [^2])."
  (markdown-test-string "Text with footnotes[^1][^2]\n\n[^1]: foo\n\n[^2]: bar\n"
                        (let (kill-ring)
                          (forward-line 2)
                          (should (looking-at "\\[\\^1\\]: foo"))
                          (markdown-footnote-kill)
                          (should (string-equal (current-kill 0) "foo\n")))))

(ert-deftest test-markdown-footnote-reference/jump ()
  "Test `markdown-do' for footnotes and reference links."
  (markdown-test-string
      "body[^1], [link 1][ref],
[link 2][ref]

[^1]: footnote

[ref]: https://duckduckgo.com/"
   (goto-char 5) ; start of [^1]
   (markdown-do) ; markdown-footnote-goto-text
   (should (looking-at "footnote"))
   (markdown-do) ; markdown-footnote-return
   (should (= (point) 9)) ; just after [^1]
   (markdown-next-link) ; beginning of [link 1][]
   (markdown-do)
   (should (looking-at "https://duckduckgo.com/"))
   (should (equal (markdown-reference-find-links "ref")
                  (list (list "link 2" 26 2) (list "link 1" 11 1))))
   (markdown-do) ; opens a reference link buffer
   (should (string= (buffer-string) "Links using reference ref:\n\nlink 1 (line 1)\nlink 2 (line 2)\n"))
   (should (looking-at "link 1")) ; in reference link popop buffer
   (execute-kbd-macro (read-kbd-macro "RET")) ; jump to "link 1"
   (should (looking-at "\\[link 1\\]")) ; back in main buffer
   (should (= (point) 11))))

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
                        (should-not (looking-at markdown-regex-header-atx)))
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
  (markdown-test-string "test  \n=="
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

;;; Subtree editing tests:

(ert-deftest test-markdown-subtree/promote ()
  "Test `markdown-promote-subtree'."
  (markdown-test-string "# h1 #\n\n## h2 ##\n\n### h3 ###\n\n## h2 ##\n\n# h1 #\n"
                        ;; The first h1 should get promoted away.
                        ;; The second h1 should not be promoted.
                        (markdown-promote-subtree)
                        (should (string-equal (buffer-string) "h1\n\n# h2 #\n\n## h3 ##\n\n# h2 #\n\n# h1 #\n"))
                        ;; Second call should do nothing since point is no longer at a heading.
                        (markdown-promote-subtree)
                        (should (string-equal (buffer-string) "h1\n\n# h2 #\n\n## h3 ##\n\n# h2 #\n\n# h1 #\n"))
                        ;; Move to h2 and promote again.
                        (forward-line 2)
                        (markdown-promote-subtree)
                        (should (string-equal (buffer-string) "h1\n\nh2\n\n# h3 #\n\n# h2 #\n\n# h1 #\n"))))

(ert-deftest test-markdown-subtree/promote-single-section ()
  "Test `markdown-promote-subtree' on a single or last section.
Should not cause an infinite loop."
  (markdown-test-string "foo\n\n## h2 ##\n\nbar\n"
    ;; The h2 should get promoted to h1 away.
    (markdown-test-goto-heading "h2")
    (markdown-promote-subtree)
    (should (string-equal (buffer-string) "foo\n\n# h2 #\n\nbar\n"))))

(ert-deftest test-markdown-subtree/demote ()
  "Test `markdown-demote-subtree'."
  (markdown-test-string "# h1 #\n\n## h2 ##\n\n### h3 ###\n\n## h2 ##\n\n# h1 #\n"
                        ;; The second h1 should not be demoted
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "## h1 ##\n\n### h2 ###\n\n#### h3 ####\n\n### h2 ###\n\n# h1 #\n"))
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "### h1 ###\n\n#### h2 ####\n\n##### h3 #####\n\n#### h2 ####\n\n# h1 #\n"))
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "#### h1 ####\n\n##### h2 #####\n\n###### h3 ######\n\n##### h2 #####\n\n# h1 #\n"))
                        ;; Stop demoting at level six
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "##### h1 #####\n\n###### h2 ######\n\n###### h3 ######\n\n###### h2 ######\n\n# h1 #\n"))
                        (markdown-demote-subtree)
                        (should (string-equal (buffer-string) "###### h1 ######\n\n###### h2 ######\n\n###### h3 ######\n\n###### h2 ######\n\n# h1 #\n"))))

(ert-deftest test-markdown-subtree/move-up ()
  "Test `markdown-move-subtree-up'."
  ;; Note that prior to Emacs 24.5, this does not work for the last subtree in
  ;; the buffer due to Emacs bug #19102:
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19102
  ;; https://github.com/emacs-mirror/emacs/commit/b3910f
  ;; That also corrects the type of the "Cannot move pase superior level" error
  ;; from 'error to 'user-error.
  (markdown-test-string "# 1 #\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n# 2 #\n# Extra\n"
                        (re-search-forward "^# 2")
                        (markdown-move-subtree-up)
                        (should (string-equal (buffer-string) "# 2 #\n# 1 #\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n# Extra\n"))
                        ;; Second attempt should fail, leaving buffer unchanged.
                        ;; (This way of asserting the contents of the error
                        ;; message is a bit convoluted and more fragile than
                        ;; ideal. But prior to Emacs 24.5, the type of this
                        ;; error is just 'error, and a bare "should-error" is
                        ;; really overly broad.)
                        (should (string-equal
                                 "Cannot move past superior level"
                                 (cl-second (should-error (markdown-move-subtree-up)))))))

(ert-deftest test-markdown-subtree/move-down ()
  "Test `markdown-move-subtree-down'."
  (markdown-test-string "# 1 #\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n# 2 #\n"
                        (re-search-forward "^## 1\.1")
                        (markdown-move-subtree-down)
                        (should (string-equal (buffer-string) "# 1 #\n\n## 1.2 ##\n\n### 1.2.1 ###\n\n## 1.1 ##\n\n### 1.1.1 ###\n\n# 2 #\n"))))

(ert-deftest test-markdown-subtree/mark ()
  "Test `markdown-mark-subtree'."
  (markdown-test-file "outline.text"
    (markdown-next-visible-heading 1)
    (should-not mark-active)
    (markdown-mark-subtree)
    (should (= (point) 19))
    (should (= (mark) 349))
    (should mark-active)
    (deactivate-mark)
    (should-not mark-active)
    (markdown-forward-same-level 1)
    (markdown-mark-subtree)
    (should (= (point) 351))
    (should (= (mark) 515))
    (should mark-active)))

(ert-deftest test-markdown-subtree/narrow ()
  "Test `markdown-narrow-to-subtree'."
  (markdown-test-file "outline.text"
    (markdown-next-visible-heading 1)
    (markdown-forward-same-level 1)
    (widen)
    (should (= (point-min) 1))
    (should (= (point-max) 553))
    (markdown-narrow-to-subtree)
    (should (= (point-min) 351))
    (should (= (point-max) 515))))

;;; Cycling:

(ert-deftest test-markdown-cycle/atx-header ()
  "Test `markdown-demote' cycling for atx headers."
  (markdown-test-string "# test"
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "## test ##"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "### test ###"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "#### test ####"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "##### test #####"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "###### test ######"))
                        (call-interactively 'markdown-demote)
                        (should (string-equal (buffer-string) "###### test ######"))))

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
                        (should (string-equal (buffer-string) "###### test ######"))))

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

(ert-deftest test-markdown-indentation/indent-list-hanging ()
  "Test `markdown-indent-line' with hanging list item."
  (markdown-test-string
   "- list
  - nested list with long lines which need to be
    hard wrapped"
   (goto-char (point-max))
   (markdown-enter-key)
   (should (eq (point) 78))))

(ert-deftest test-markdown-indentation/indent-list-single ()
  "Test `markdown-indent-line' with single list item."
  (let ((markdown-indent-on-enter 'indent-and-new-item))
    (markdown-test-string "  * item 1"
      (end-of-line)
      (call-interactively #'markdown-enter-key)
      (should (string-equal (buffer-string) "  * item 1\n  * "))
      (should (eq (point) 16))
      (call-interactively #'markdown-enter-key)
      (should (string-equal (buffer-string) "  * item 1\n\n"))
      (should (eq (point) 13)))))

(ert-deftest test-markdown-indentation/indent-nested-list ()
  "Test `markdown-enter-key' with a nested list item."
  (let ((markdown-indent-on-enter 'indent-and-new-item))
    (markdown-test-string "* foo\n* bar\n  * baz"
      (goto-char (point-max))
      (call-interactively #'markdown-enter-key)
      (should (string-equal (buffer-string) "* foo\n* bar\n  * baz\n  * "))
      (should (eq (point) 25))
      (call-interactively #'markdown-enter-key)
      (should (string-equal (buffer-string) "* foo\n* bar\n  * baz\n\n"))
      (should (eq (point) 22)))))

(ert-deftest test-markdown-indentation/indent-pre ()
  "Test `markdown-indent-line' with a pre block."
  (markdown-test-string
   "I'm gonna write a code block:

    my first line of code"
   (goto-char (point-max))
   (markdown-enter-key)
   (should (eq (point) 62))
   (should (looking-back "^    "))))

(ert-deftest test-markdown-indentation/indent-hanging-line ()
  "Test `markdown-indent-line' with hanging indentation.
See GH-245."
  (markdown-test-string "Stuff
  More"
    (forward-line)
    (should (looking-at "^  More"))
    (should (= (current-column) 0))
    (should (= (current-indentation) 2))
    (let ((last-command this-command)
          (this-command 'markdown-cycle))
      (call-interactively #'markdown-cycle))
    (should (= (current-column) 0))
    (should (= (current-indentation) 0))))

(ert-deftest test-markdown-indentation/continue-gfm-task-lists ()
  (markdown-test-string "   -   [X] item"
    (end-of-line)
    (let ((markdown-indent-on-enter 'indent-and-new-item))
      (call-interactively #'markdown-enter-key))
    (should (string-equal (buffer-string) "   -   [X] item\n   -   [ ] "))
    (should (= (point) 28))))

;;; Markup hiding tests:

(ert-deftest test-markdown-markup-hiding/italics-1 ()
  "Test hiding markup for italics."
  (markdown-test-file "inline.text"
    (goto-char 9)
    (should (looking-at "\*italic\*"))
    (markdown-test-range-has-property (point) (point) 'invisible 'markdown-markup)
    (should-not (invisible-p (point)))
    (should-not (invisible-p (+ 1 (point))))
    (markdown-toggle-markup-hiding t)
    (should (invisible-p (point)))
    (should-not (invisible-p (+ 1 (point))))))

(ert-deftest test-markdown-markup-hiding/bold-1 ()
  "Test hiding markup for bold."
  (markdown-test-file "inline.text"
    (goto-char 27)
    (should (looking-at "\*\*bold\*\*"))
    (markdown-test-range-has-property (point) (1+ (point)) 'invisible 'markdown-markup)
    (should-not (invisible-p (point)))
    (should-not (invisible-p (+ 1 (point))))
    (should-not (invisible-p (+ 2 (point))))
    (markdown-toggle-markup-hiding t)
    (should (invisible-p (point)))
    (should (invisible-p (+ 1 (point))))
    (should-not (invisible-p (+ 2 (point))))))

(ert-deftest test-markdown-markup-hiding/code-1 ()
  "Test hiding markup for inline code."
  (markdown-test-file "inline.text"
    (goto-char 45)
    (should (looking-at "`code`"))
    (markdown-test-range-has-property (point) (point) 'invisible 'markdown-markup)
    (should-not (invisible-p (point)))
    (should-not (invisible-p (1+ (point))))
    (markdown-toggle-markup-hiding t)
    (should (invisible-p (point)))
    (should-not (invisible-p (1+ (point))))))

(ert-deftest test-markdown-markup-hiding/kbd-1 ()
  "Test hiding markup for <kbd> tags."
  (markdown-test-string "<kbd>C-c C-x C-m</kbd>"
    (markdown-test-range-has-property (point) (+ 4 (point)) 'invisible 'markdown-markup)
    (should-not (invisible-p (point))) ;; part of <kbd>
    (should-not (invisible-p (+ 4 (point)))) ;; part of <kbd>
    (should-not (invisible-p (+ 5 (point)))) ;; inside <kbd>
    (markdown-toggle-markup-hiding t)
    (should (invisible-p (point))) ;; part of <kbd>
    (should (invisible-p (+ 4 (point)))) ;; part of <kbd>
    (should-not (invisible-p (+ 5 (point)))))) ;; inside <kbd>

(ert-deftest test-markdown-markup-hiding/inline-links ()
  "Test hiding markup for inline links."
  (markdown-test-file "inline.text"
    (goto-char 925)
    (should (looking-at "\\[text\\](http://www.w3.org/ \"title\")"))
    (markdown-test-range-has-property 925 925 'invisible 'markdown-markup) ; [
    (markdown-test-range-has-property 930 958 'invisible 'markdown-markup) ; ](...)
    (should-not (invisible-p 925))
    (should-not (invisible-p 958))
    (markdown-toggle-markup-hiding t)
    (should (invisible-p 925))
    (should-not (invisible-p 926))
    (should (invisible-p 958))))

(ert-deftest test-markdown-markup-hiding/reference-links ()
  "Test hiding markup for reference links."
  (markdown-test-string "[text][ref]"
    (markdown-test-range-has-property 1 1 'invisible 'markdown-markup) ; [
    (markdown-test-range-has-property 6 11 'invisible 'markdown-markup) ; ][ref]
    (should-not (invisible-p 1))
    (should-not (invisible-p 6))
    (markdown-toggle-markup-hiding t)
    (should (invisible-p 1))
    (should-not (invisible-p 2))
    (should (invisible-p 6))))

(ert-deftest test-markdown-markup-hiding/angle-urls ()
  "Test hiding markup for angle urls."
  (markdown-test-string "<http://jblevins.org/projects/markdown-mode/>"
    (markdown-test-range-has-property 1 1 'invisible 'markdown-markup) ; <
    (markdown-test-range-has-property 45 45 'invisible 'markdown-markup) ; >
    (should-not (invisible-p 1))
    (should-not (invisible-p 2))
    (should-not (invisible-p 45))
    (markdown-toggle-markup-hiding t)
    (should (invisible-p 1))
    (should-not (invisible-p 2))
    (should (invisible-p 45))))

(ert-deftest test-markdown-markup-hiding/list-items ()
  "Test hiding markup for list items."
  (let ((markdown-hide-markup t))
    (markdown-test-file "nested-list.text"
      (markdown-test-range-has-property 4 4 'display (nth 0 markdown-list-item-bullets))
      (markdown-test-range-has-property 194 194 'display (nth 0 markdown-list-item-bullets))
      (markdown-test-range-has-property 224 224 'display (nth 1 markdown-list-item-bullets))
      (markdown-test-range-has-property 525 525 'display (nth 2 markdown-list-item-bullets)))))

(ert-deftest test-markdown-markup-hiding/gfm-code-blocks ()
  "Test hiding markup for GFM code blocks."
  (let ((markdown-hide-markup t))
    (markdown-test-file "GFM.md"
      (markdown-test-range-has-property 1548 1552 'invisible 'markdown-markup)
      (should (invisible-p 1548))
      (should (invisible-p 1552))
      (markdown-test-range-has-property 1607 1609 'invisible 'markdown-markup)
      (should (invisible-p 1607))
      (should (invisible-p 1609)))))

(ert-deftest test-markdown-markup-hiding/fenced-code-blocks ()
  "Test hiding markup for tilde fenced code blocks."
  (let ((markdown-hide-markup t))
    (markdown-test-file "outline-code.text"
      (markdown-test-range-has-property 83 93 'invisible 'markdown-markup)
      (should (invisible-p 83))
      (should (invisible-p 93))
      (markdown-test-range-has-property 154 156 'invisible 'markdown-markup)
      (should (invisible-p 154))
      (should (invisible-p 156)))))

;;; Font lock tests:

(ert-deftest test-markdown-font-lock/italics-1 ()
  "A simple italics test."
  (markdown-test-file "inline.text"
                      (goto-char 9)
                      (should (looking-at "\*"))
                      ;; Check face of char before leading asterisk
                      (markdown-test-range-has-face 8 8 nil)
                      ;; Check face of italic range
                      (markdown-test-range-has-face 9 9 'markdown-markup-face)
                      (markdown-test-range-has-face 10 16 'markdown-italic-face)
                      (markdown-test-range-has-face 17 17 'markdown-markup-face)
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
  "Test escaped asterisk inside italics."
  (markdown-test-string
   "italic *\\**"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 8 'markdown-markup-face)
   (markdown-test-range-has-face 9 10 'markdown-italic-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-5 ()
  "Test italic single letter."
  (markdown-test-string
   "*a*"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 2 'markdown-italic-face)
   (markdown-test-range-has-face 3 3 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-6 ()
  "Test multiline italics across list items."
  (markdown-test-string
   "* something about function foo_bar
* something else about foo_bar"
   (markdown-test-range-has-face 31 34 nil)
   (markdown-test-range-has-face 38 62 nil)))

(ert-deftest test-markdown-font-lock/italics-8 ()
  "Test multiline italics across list items."
  (markdown-test-string
   "* something about function
  foo_bar
* something else about
  foo_bar"
   (markdown-test-range-has-face 30 36 nil)
   (markdown-test-range-has-face 63 69 nil)))

(ert-deftest test-markdown-font-lock/italics-9 ()
  "Test multiline italics across list items."
  (markdown-test-string
   "foo_bar
* foo_bar"
   (markdown-test-range-has-face 4 7 nil)
   (markdown-test-range-has-face 11 14 nil)))

(ert-deftest test-markdown-font-lock/italics-10 ()
  "Underscores in URLs should not trigger italics."
  (markdown-test-string
   "<http://jblevins.org/research/centroid/cd_z_path.m>"
   (markdown-test-range-face-equals 2 50 'markdown-plain-url-face)
   (should-not (markdown-range-property-any 43 43 'face '(markdown-italic-face)))))

(ert-deftest test-markdown-font-lock/italics-11 ()
  "Underscores in URLs should not trigger italics."
  (markdown-test-string
   "[1]: http://jblevins.org/research/centroid/cd_z_path.m"
   (markdown-test-range-face-equals 6 54 'markdown-url-face)))

(ert-deftest test-markdown-font-lock/italics-12 ()
  "Underscores in URLs should not trigger italics."
  (markdown-test-string
   "[cd\\_z\\_path.m](http://jblevins.org/research/centroid/cd_z_path.m)"
   (markdown-test-range-face-equals 17 65 'markdown-url-face)))

(ert-deftest test-markdown-font-lock/italics-after-hr ()
  "Test italics after a horizontal rule with asterisks."
  (markdown-test-string "* * *\n\n*italic*\n"
                        (markdown-test-range-has-face 1 5 'markdown-hr-face)
                        (markdown-test-range-has-face 8 8 'markdown-markup-face)
                        (markdown-test-range-has-face 9 14 'markdown-italic-face)
                        (markdown-test-range-has-face 15 15 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-heading ()
  "Test italic overlay in a heading."
  (markdown-test-string
   "# *Italics* in a Heading"
   (markdown-test-range-has-face 3 3 'markdown-markup-face)
   (markdown-test-range-has-face 4 10 'markdown-italic-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-link ()
  "Test italic overlay in an inline link."
  (markdown-test-string
   "*[italic link](http://www.link.com/)*"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 36 'markdown-italic-face)
   (markdown-test-range-has-face 37 37 'markdown-markup-face))
  (markdown-test-string
   "[*italic link*](http://www.link.com/)"
   (markdown-test-range-has-face 2 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 13 'markdown-italic-face)
   (markdown-test-range-has-face 14 14 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-blockquote ()
  "Test italics overlay in a blockquote."
  (markdown-test-string
   "> *italics* inside a blockquote"
   (markdown-test-range-has-face 3 3 'markdown-markup-face)
   (markdown-test-range-has-face 4 10 'markdown-italic-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-pre ()
  "Test italics overlay in a blockquote."
  (markdown-test-string
   "    *italics* inside a pre block"
   (markdown-test-range-has-face (point-min) (1- (point-max))
                                 'markdown-pre-face)))

(ert-deftest test-markdown-font-lock/italics-and-code ()
  "Test seeming italics mixed with code."
  (markdown-test-string
   "define `var_1` and `var_2` inline code"
   (markdown-test-range-has-face 9 13 'markdown-inline-code-face)
   (markdown-test-range-has-face 21 25 'markdown-inline-code-face))
  (markdown-test-string
   "`var_1` and var_2"
   (markdown-test-range-has-face 2 6 'markdown-inline-code-face)
   (markdown-test-range-has-face 8 17 nil))
  (markdown-test-string
   "var_1 and `var_2`"
   (markdown-test-range-has-face 1 10 nil)
   (markdown-test-range-has-face 12 16 'markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/code-in-italics ()
  "Test inline code inside italics.
See GH-275."
  (markdown-test-string
   "*text `code` text*"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 17 'markdown-italic-face)
   (markdown-test-range-has-face 7 7 'markdown-markup-face)
   (markdown-test-range-has-face 8 11 'markdown-inline-code-face)
   (markdown-test-range-has-face 12 12 'markdown-markup-face)
   (markdown-test-range-has-face 18 18 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/italics-in-reference-definitions ()
  "Test not matching italics in reference definitions across lines."
  (markdown-test-string
   "[lg]: twilight_sm.png\n[sm]: twilight_lg.png"
   (markdown-test-range-has-face 7 21 'markdown-url-face)
   (markdown-test-range-has-face 22 22 nil)
   (markdown-test-range-has-face 29 43 'markdown-url-face)
   (markdown-test-range-has-face 28 28 nil)))

(ert-deftest test-markdown-font-lock/italics-in-comment ()
  "Test not matching italics in comments."
  (markdown-test-string
   "<!-- -*- coding: utf-8 -*- -->"
   (markdown-test-range-has-face 1 30 'markdown-comment-face)
   (should-not (markdown-range-property-any 1 30 'face '(markdown-italic-face)))))

(ert-deftest test-markdown-font-lock/italics-after-bold ()
  "Test bold and italics on the same line.
See GH-223."
  (markdown-test-string
   "**foo** is a *bar*"
   (markdown-test-range-has-face 1 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 5 'markdown-bold-face)
   (markdown-test-range-has-face 6 7 'markdown-markup-face)
   (should-not
    (markdown-range-property-any 8 13 'face '(markdown-italic-face)))
   (markdown-test-range-has-face 14 14 'markdown-markup-face)
   (markdown-test-range-has-face 15 17 'markdown-italic-face)
   (markdown-test-range-has-face 18 18 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-1 ()
  "A simple bold test."
  (markdown-test-file "inline.text"
                      (goto-char 27)
                      (should (looking-at "\*\*"))
                      ;; Check face of char before leading asterisk
                      (markdown-test-range-has-face 26 26 nil)
                      ;; Check face of opening asterisks
                      (markdown-test-range-has-face 27 28 'markdown-markup-face)
                      ;; Check face of bold range
                      (markdown-test-range-has-face 29 33 'markdown-bold-face)
                      ;; Check face of closing asterisks
                      (markdown-test-range-has-face 34 35 'markdown-markup-face)
                      ;; Check face of point past leading asterisk
                      (markdown-test-range-has-face 36 36 nil)))

(ert-deftest test-markdown-font-lock/bold-2 ()
  "Test space after leading asterisks or underscores."
  (markdown-test-string
   "This is ** not bold**, nor __ is this__."
   (should-not
    (markdown-range-property-any
     (point-min) (point-max) 'face '(markdown-bold-face)))))

(ert-deftest test-markdown-font-lock/bold-3 ()
  "Test escaped asterisk inside bold."
  (markdown-test-string
   "bold **\\***"
   (markdown-test-range-has-face 1 5 nil)
   (markdown-test-range-has-face 6 7 'markdown-markup-face)
   (markdown-test-range-has-face 8 9 'markdown-bold-face)
   (markdown-test-range-has-face 10 11 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-4 ()
  "Test bold single letter."
  (markdown-test-string
   "**a**"
   (markdown-test-range-has-face 1 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 3 'markdown-bold-face)
   (markdown-test-range-has-face 4 5 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-after-hr ()
  "Test bold after a horizontal rule with asterisks."
  (markdown-test-string "* * *\n\n**bold**\n"
   (markdown-test-range-has-face 1 5 'markdown-hr-face)
   (markdown-test-range-has-face 8 9 'markdown-markup-face)
   (markdown-test-range-has-face 10 13 'markdown-bold-face)
   (markdown-test-range-has-face 14 15 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-link ()
  "Test bold overlay in an inline link."
  (markdown-test-string
   "**[bold link](http://www.link.com/)**"
   (markdown-test-range-has-face 1 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 35 'markdown-bold-face)
   (markdown-test-range-has-face 36 37 'markdown-markup-face))
  (markdown-test-string
   "[**bold link**](http://www.link.com/)"
   (markdown-test-range-has-face 2 3 'markdown-markup-face)
   (markdown-test-range-has-face 4 12 'markdown-bold-face)
   (markdown-test-range-has-face 13 14 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-in-blockquote ()
  "Test bold overlay in a blockquote."
  (markdown-test-string
   "> **bold** inside a blockquote"
   (markdown-test-range-has-face 3 4 'markdown-markup-face)
   (markdown-test-range-has-face 5 8 'markdown-bold-face)
   (markdown-test-range-has-face 9 10 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-in-pre ()
  "Test bold overlay in a blockquote."
  (markdown-test-string
   "    **bold** inside a pre block"
   (markdown-test-range-has-face (point-min) (1- (point-max))
                                 'markdown-pre-face)))

(ert-deftest test-markdown-font-lock/no-bold-in-code ()
  "Bold markers in inline code should not trigger bold."
  (markdown-test-string
   "`def __init__(self):`"
   (markdown-test-range-has-face 8 11 'markdown-inline-code-face)
   (should-not (markdown-range-property-any
                (point-min) (point-max) 'face '(markdown-bold-face))))
  (markdown-test-string
   "`**foo` bar `baz**`"
   (markdown-test-range-has-face 2 6 'markdown-inline-code-face)
   (markdown-test-range-face-equals 9 11 nil)
   (markdown-test-range-has-face 14 18 'markdown-inline-code-face)
   (should-not (markdown-range-property-any
                (point-min) (point-max) 'face '(markdown-bold-face)))))

(ert-deftest test-markdown-font-lock/code-in-bold ()
  "Test inline code inside bold."
  (markdown-test-string
   "**text `code` text**"
   (markdown-test-range-has-face 1 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 18 'markdown-bold-face)
   (markdown-test-range-has-face 8 8 'markdown-markup-face)
   (markdown-test-range-has-face 9 12 'markdown-inline-code-face)
   (markdown-test-range-has-face 13 13 'markdown-markup-face)
   (markdown-test-range-has-face 19 20 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/bold-in-comment ()
  "Test not matching bold in comments."
  (markdown-test-string
   "<!-- **not bold** -->"
   (markdown-test-range-has-face 1 21 'markdown-comment-face)
   (should-not
    (markdown-range-property-any 1 21 'face '(markdown-bold-face)))))

(ert-deftest test-markdown-font-lock/no-bold-in-url ()
  "Test not matching bold in plain URL links."
  (markdown-test-string
   "<https://example.com/__not-bold__>"
   (should-not (markdown-range-property-any 23 30 'face '(markdown-bold-face)))))

(ert-deftest test-markdown-font-lock/code-1 ()
  "A simple inline code test."
  (markdown-test-file "inline.text"
   (goto-char 45)
   (should (looking-at "`"))
   ;; Regular code span
   (markdown-test-range-has-face 45 45 'markdown-markup-face)
   (markdown-test-range-has-face 46 49 'markdown-inline-code-face)
   (markdown-test-range-has-face 50 50 'markdown-markup-face)
   ;; Code containing backticks
   (markdown-test-range-has-face 61 62 'markdown-markup-face)
   (markdown-test-range-has-face 63 87 'markdown-inline-code-face)
   (markdown-test-range-has-face 88 89 'markdown-markup-face)
   ;; Seven backquotes in a row
   (markdown-test-range-has-face 119 125 nil)
   ;; Backquotes at beginning or end
   (markdown-test-range-has-face 228 229 'markdown-markup-face)
   (markdown-test-range-has-face 230 237 'markdown-inline-code-face)
   (markdown-test-range-has-face 238 239 'markdown-markup-face)
   (markdown-test-range-has-face 341 342 'markdown-markup-face)
   (markdown-test-range-has-face 343 349 'markdown-inline-code-face)
   (markdown-test-range-has-face 350 351 'markdown-markup-face)
   ;; Backslash as final character
   (markdown-test-range-has-face 460 460 'markdown-markup-face)
   (markdown-test-range-has-face 461 467 'markdown-inline-code-face)
   (markdown-test-range-has-face 468 468 'markdown-markup-face)
   ;; Escaping of leading backquotes
   (markdown-test-range-has-face 586 592 nil)
   (markdown-test-range-has-face 597 603 nil)
   ;; A code span crossing lines
   (markdown-test-range-has-face 652 656 nil)
   (markdown-test-range-has-face 657 657 'markdown-markup-face)
   (markdown-test-range-has-face 658 665 'markdown-inline-code-face)
   (markdown-test-range-has-face 666 666 'markdown-markup-face)
   ;; Three backquotes: same line, across lines, not across blocks
   (markdown-test-range-has-face 695 748 nil)
   (markdown-test-range-has-face 749 750 'markdown-markup-face)
   (markdown-test-range-has-face 751 755 'markdown-inline-code-face)
   (markdown-test-range-has-face 756 757 'markdown-markup-face)
   (markdown-test-range-has-face 758 805 nil)
   (markdown-test-range-has-face 806 807 'markdown-markup-face)
   (markdown-test-range-has-face 808 812 'markdown-inline-code-face)
   (markdown-test-range-has-face 813 814 'markdown-markup-face)
   (markdown-test-range-has-face 815 891 nil)
   ))

(ert-deftest test-markdown-font-lock/code-2 ()
  "Multiple code spans in a row and on different lines."
  (markdown-test-string "`foo` `bar` `baz`"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 4 'markdown-inline-code-face)
   (markdown-test-range-has-face 5 5 'markdown-markup-face)
   (markdown-test-range-has-face 6 6 nil)
   (markdown-test-range-has-face 7 7 'markdown-markup-face)
   (markdown-test-range-has-face 8 10 'markdown-inline-code-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)
   (markdown-test-range-has-face 12 12 nil)
   (markdown-test-range-has-face 13 13 'markdown-markup-face)
   (markdown-test-range-has-face 14 16 'markdown-inline-code-face)
   (markdown-test-range-has-face 17 17 'markdown-markup-face))
  (markdown-test-string "`a`\n`b`\n`c`\n"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 2 'markdown-inline-code-face)
   (markdown-test-range-has-face 3 3 'markdown-markup-face)
   (markdown-test-range-has-face 4 4 nil)
   (markdown-test-range-has-face 5 5 'markdown-markup-face)
   (markdown-test-range-has-face 6 6 'markdown-inline-code-face)
   (markdown-test-range-has-face 7 7 'markdown-markup-face)
   (markdown-test-range-has-face 8 8 nil)
   (markdown-test-range-has-face 9 9 'markdown-markup-face)
   (markdown-test-range-has-face 10 10 'markdown-inline-code-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)
   (markdown-test-range-has-face 12 12 nil))
  (markdown-test-string "a`foo`b`bar`c`baz`d"
   (markdown-test-range-has-face 1 1 nil)
   (markdown-test-range-has-face 2 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 5 'markdown-inline-code-face)
   (markdown-test-range-has-face 6 6 'markdown-markup-face)
   (markdown-test-range-has-face 7 7 nil)
   (markdown-test-range-has-face 8 8 'markdown-markup-face)
   (markdown-test-range-has-face 9 11 'markdown-inline-code-face)
   (markdown-test-range-has-face 12 12 'markdown-markup-face)
   (markdown-test-range-has-face 13 13 nil)
   (markdown-test-range-has-face 14 14 'markdown-markup-face)
   (markdown-test-range-has-face 15 17 'markdown-inline-code-face)
   (markdown-test-range-has-face 18 18 'markdown-markup-face)
   (markdown-test-range-has-face 19 19 nil)))

(ert-deftest test-markdown-font-lock/code-3 ()
  "Backslashes don't escape backticks inside of inline code strings."
  (markdown-test-string
   "`foo\\`bar`"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 5 'markdown-inline-code-face)
   (markdown-test-range-has-face 6 6 'markdown-markup-face)
   (markdown-test-range-has-face 7 10 nil)))

(ert-deftest test-markdown-font-lock/code-link-precedence ()
  "Test that inline code takes precedence over inline links."
  (markdown-test-string
   "[not a `link](/foo`)"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 8 'markdown-markup-face)
   (markdown-test-range-has-face 9 18 'markdown-inline-code-face)
   (markdown-test-range-has-face 19 19 'markdown-markup-face)
   (markdown-test-range-has-face 20 20 nil)))

(ert-deftest test-markdown-font-lock/code-in-link-text ()
  "Test that inline code in link text is fontified properly"
  (markdown-test-string
   "[`this is a` link](/foo)"
   (markdown-test-range-has-face 1 2 'markdown-markup-face)
   (markdown-test-range-has-face 3 11 'markdown-inline-code-face)
   (markdown-test-range-has-face 12 12 'markdown-markup-face)
   (markdown-test-range-has-face 3 17 'markdown-link-face)
   (markdown-test-range-has-face 18 19 'markdown-markup-face)
   (markdown-test-range-has-face 20 23 'markdown-url-face)
   (markdown-test-range-has-face 24 24 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/code-italics-precedence ()
  "Test that inline code takes precedence over italics.
Test currently fails because this case isn't handled properly."
  (markdown-test-string
   "*text `code* text`"
   (markdown-test-range-has-face 1 6 nil)
   (markdown-test-range-has-face 7 7 'markdown-markup-face)
   (markdown-test-range-has-face 8 17 'markdown-inline-code-face)
   (markdown-test-range-has-face 18 18 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/code-in-comment ()
  "Test that inline code is not matched inside a comment."
  (markdown-test-string
   "<!-- `not code` -->"
   (markdown-test-range-has-face 1 19 'markdown-comment-face)
   (should-not (markdown-range-property-any 1 19 'face '(markdown-inline-code-face)))))

(ert-deftest test-markdown-font-lock/kbd ()
  "Test font lock for <kbd> tags."
  (markdown-test-string "<kbd>C-c <</kbd>"
   (markdown-test-range-has-face 1 5 'markdown-markup-face)
   (markdown-test-range-has-face 6 10 'markdown-inline-code-face)
   (markdown-test-range-has-face 11 16 'markdown-markup-face))
  (markdown-test-string "To quit Emacs, press <kbd>C-x C-c</kbd>."
   (markdown-test-range-has-face 1 21 nil)
   (markdown-test-range-has-face 22 26 'markdown-markup-face)
   (markdown-test-range-has-face 27 33 'markdown-inline-code-face)
   (markdown-test-range-has-face 34 39 'markdown-markup-face)
   (markdown-test-range-has-face 40 40 nil)))

(ert-deftest test-markdown-font-lock/lists-1 ()
  "A simple list marker font lock test."
  (markdown-test-file "lists.text"
   (dolist (loc (list 1063 1283 1659 1830 1919 2150 2393 2484
                      2762 2853 3097 3188 3700 3903 4009))
     (goto-char loc)
     (should (looking-at "[*+-]"))
     (markdown-test-range-has-face loc loc 'markdown-list-face))))

(ert-deftest test-markdown-font-lock/definition-list ()
  "A simple definition list marker font lock test."
  (markdown-test-file "definition-list.text"
    (markdown-test-range-has-face 7 7 'markdown-list-face)
    (markdown-test-range-has-face 29 52 'markdown-pre-face)
    (markdown-test-range-has-face 55 55 'markdown-list-face)))

(ert-deftest test-markdown-font-lock/pre-1 ()
  "Nested list and pre block font lock test."
  (markdown-test-file "nested-list.text"
    (dolist (loc (list 4 29 194 224 491 525))
      (markdown-test-range-has-face loc loc 'markdown-list-face))
    (markdown-test-range-has-face 6 25 nil)
    (markdown-test-range-has-face 31 83 nil)
    (markdown-test-range-has-face 85 154 'markdown-pre-face)
    (markdown-test-range-has-face 157 189 nil)
    (markdown-test-range-has-face 196 215 nil)
    (markdown-test-range-has-face 226 403 nil)
    (markdown-test-range-has-face 405 481 'markdown-pre-face)
    (markdown-test-range-has-face 493 512 nil)
    (markdown-test-range-has-face 527 546 nil)
    (markdown-test-range-has-face 548 580 'markdown-pre-face)))

(ert-deftest test-markdown-font-lock/pre-2 ()
  (markdown-test-string "* item\n\nreset baseline\n\n    pre block\n"
   (markdown-test-range-has-face 1 1 'markdown-list-face)
   (markdown-test-range-has-face 2 23 nil)
   (markdown-test-range-has-face 29 37 'markdown-pre-face)))

(ert-deftest test-markdown-font-lock/pre-3 ()
  (markdown-test-string "It is interesting to see what happens when one queries
`social upheaval` and `protopalatial era`.

* `social upheaval`: the follwing queries have been tried:

    social upheaval subClassOf"
   (markdown-test-range-has-face 160 190 nil)))

(ert-deftest test-markdown-font-lock/pre-4 ()
  "Pre blocks must be preceded by a blank line"
  (markdown-test-string "Paragraph
    for (var i = 0; i < 10; i++) {
        console.log(i);
    }"
    (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/fenced-1 ()
  "Test fenced code blocks containing four-space indents."
  (markdown-test-string "Fenced code block

~~~
if (x)
    foo();

if (y)
    bar();
~~~
"
   (markdown-test-range-has-face 1 19 nil)
   (markdown-test-range-has-face 20 22 'markdown-markup-face)
   (markdown-test-range-has-face 24 59 'markdown-pre-face)
   (markdown-test-range-has-face 61 63 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/gfm-fenced-1 ()
  "Test GFM-style fenced code blocks (1)."
  (let ((markdown-fontify-code-blocks-natively t))
    (markdown-test-string "```ruby
require 'redcarpet'
markdown = Redcarpet.new('Hello World!')
puts markdown.to_html
```"
      (markdown-test-range-has-face 1 3 'markdown-markup-face) ; ```
      (markdown-test-range-has-face 4 7 'markdown-language-keyword-face) ; ruby
      (markdown-test-range-has-face 9 90 'markdown-code-face) ; entire code block
      (markdown-test-range-has-face 9 15 'font-lock-builtin-face) ; require
      (markdown-test-range-has-face 17 27 'font-lock-string-face) ; 'redcarpet'
      (markdown-test-range-has-face 40 48 'font-lock-type-face) ; Redcarpet
      (markdown-test-range-has-face 70 72 'font-lock-builtin-face) ; puts
      (markdown-test-range-has-face 92 94 'markdown-markup-face)))) ; ```

(ert-deftest test-markdown-font-lock/gfm-fenced-2 ()
  "Test GFM-style fenced code blocks (2)."
  (markdown-test-string "```{r sum}\n2+2\n```"
   (markdown-test-range-has-face 1 3 'markdown-markup-face) ; ```
   (markdown-test-range-has-face 4 4 'markdown-markup-face) ; {
   (markdown-test-range-has-face 5 5 'markdown-language-keyword-face) ; r
   (markdown-test-range-has-face 7 9 'markdown-language-info-face) ; sum
   (markdown-test-range-has-face 10 10 'markdown-markup-face) ; }
   (markdown-test-range-has-face 12 14 'markdown-pre-face) ; 2+2
   (markdown-test-range-has-face 16 18 'markdown-markup-face))) ; ```

(ert-deftest test-markdown-font-lock/gfm-fenced-3 ()
  "GFM-style code blocks need not be preceded by a blank line."
  (markdown-test-string "Paragraph
```js
for (var i = 0; i < 10; i++) {
    console.log(i);
}
```"
    (markdown-test-range-has-face 1 10 nil) ; Paragraph
    (markdown-test-range-has-face 11 13 'markdown-markup-face) ; ```
    (markdown-test-range-has-face 14 15 'markdown-language-keyword-face) ; js
    (markdown-test-range-has-face 17 68 'markdown-pre-face)
    (markdown-test-range-has-face 70 72 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/gfm-fenced-4 ()
  "Test GFM-style fenced code blocks (2)."
  (markdown-test-string "```scalaFiddle libraries=\"Java8 Time-0.1.0\"\nimport java.time._\n\nval hour = LocalTime.now().getHour()\n\nprintln(hour)\n```"
   (markdown-test-range-has-face 1 3 'markdown-markup-face) ; ```
   (markdown-test-range-has-face 4 14 'markdown-language-keyword-face) ; scalaFiddle
   (markdown-test-range-has-face 16 43 'markdown-language-info-face) ; libraries="Java8 Time-0.1.0"
   (markdown-test-range-has-face 45 115 'markdown-pre-face) ; [code]
   (markdown-test-range-has-face 117 119 'markdown-markup-face))) ; ```

(ert-deftest test-markdown-font-lock/tilde-fenced-1 ()
  "Test native fontification of tilde fenced code blocks."
  (let ((markdown-fontify-code-blocks-natively t))
    (markdown-test-string "~~~ruby
require 'redcarpet'
markdown = Redcarpet.new('Hello World!')
puts markdown.to_html
~~~"
      (markdown-test-range-has-face 1 3 'markdown-markup-face) ; ```
      (markdown-test-range-has-face 4 7 'markdown-language-keyword-face) ; ruby
      (markdown-test-range-has-face 9 90 'markdown-code-face) ; entire code block
      (markdown-test-range-has-face 9 15 'font-lock-builtin-face) ; require
      (markdown-test-range-has-face 17 27 'font-lock-string-face) ; 'redcarpet'
      (markdown-test-range-has-face 40 48 'font-lock-type-face) ; Redcarpet
      (markdown-test-range-has-face 70 72 'font-lock-builtin-face) ; puts
      (markdown-test-range-has-face 92 94 'markdown-markup-face)))) ; ```

(ert-deftest test-markdown-font-lock/atx-no-spaces ()
  "Test font-lock for atx headers with no spaces."
  (markdown-test-string "##abc##"
   (markdown-test-range-has-face 1 7 nil))
  (markdown-test-string "##"
   (markdown-test-range-has-face 1 2 nil))
  (markdown-test-string "###"
   (markdown-test-range-has-face 1 3 nil)))

(ert-deftest test-markdown-font-lock/setext-1-letter ()
  "An edge case for level-one setext headers."
  (markdown-test-string "a\n=\n"
   (markdown-test-range-has-face 1 1 'markdown-header-face-1)
   (markdown-test-range-has-face 3 3 'markdown-header-rule-face)))

(ert-deftest test-markdown-font-lock/setext-2-letter ()
  "An edge case for level-two setext headers."
  (markdown-test-string "b\n-\n"
   (markdown-test-range-has-face 1 1 'markdown-header-face-2)
   (markdown-test-range-has-face 3 3 'markdown-header-rule-face)))

(ert-deftest test-markdown-font-lock/inline-links ()
  "Test font lock for inline links."
  (let ((markdown-hide-urls nil))
    (markdown-test-file "inline.text"
      (markdown-test-range-has-face 925 925 'markdown-markup-face)
      (markdown-test-range-has-face 926 929 'markdown-link-face)
      (markdown-test-range-has-face 930 931 'markdown-markup-face)
      (markdown-test-range-has-face 932 949 'markdown-url-face)
      (markdown-test-range-has-face 951 957 'markdown-link-title-face)
      (markdown-test-range-has-face 958 958 'markdown-markup-face))))

(ert-deftest test-markdown-font-lock/inline-links-with-parentheses ()
  "Test font lock for inline links with nested parentheses.
See <https://github.com/jrblevin/markdown-mode/issues/170>."
  (let ((markdown-hide-urls nil))
    (markdown-test-string "[foo](bar(baz)qux)"
      (markdown-test-range-has-face 1 1 'markdown-markup-face)
      (markdown-test-range-has-face 2 4 'markdown-link-face)
      (markdown-test-range-has-face 5 6 'markdown-markup-face)
      (markdown-test-range-has-face 7 17 'markdown-url-face)
      (markdown-test-range-has-face 18 18 'markdown-markup-face))))

(ert-deftest test-markdown-font-lock/pre-comment ()
  "Test comments inside of a pre block."
  (markdown-test-string "    <!-- pre, not comment -->"
   (markdown-test-range-has-face (point-min) (1- (point-max)) 'markdown-pre-face)))

(ert-deftest test-markdown-font-lock/inline-code-comment ()
  "Test comments inside of inline code."
  (markdown-test-string "`<h1> <!-- HTML comment inside inline code -->`"
   (markdown-test-range-has-face (1+ (point-min)) (- (point-max) 2) 'markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/inline-code-link ()
  "Test links inside of inline code."
  (markdown-test-string "`[text](url)`"
   (markdown-test-range-has-face (1+ (point-min)) (- (point-max) 2) 'markdown-inline-code-face)
   (should-not (markdown-range-property-any
                (1+ (point-min)) (- (point-max) 2) 'face
                '(markdown-markup-face markdown-link-face markdown-url-face)))))

(ert-deftest test-markdown-font-lock/comment-hanging-indent ()
  "Test comments with hanging indentation."
  (markdown-test-string "<!-- This comment has\n    hanging indentation -->"
   (markdown-test-range-has-face (point-min) (1- (point-max)) 'markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-multiple ()
  "Test multiple single-line comments in arow."
  (markdown-test-string "<!-- This is a comment -->\n<!-- And so is this -->"
   (markdown-test-range-has-face
    (point-at-bol) (1- (point-at-eol)) 'markdown-comment-face)
   (forward-line)
   (markdown-test-range-has-face
    (point-at-bol) (1- (point-at-eol)) 'markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-list-items ()
  "Test comment with list inside."
  (markdown-test-string
   "<!--
  - note 1;
  - note 2.
-->"
   (markdown-test-range-face-equals (point-min) (1- (point-max))
                                    'markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-angle-bracket ()
  "Regression test for GH-117."
  (markdown-test-string "<!-- > test -->"
   (markdown-test-range-face-equals (point-min) (1- (point-max))
                                    'markdown-comment-face)))

(ert-deftest test-markdown-font-lock/comment-link ()
  "Test links inside of comments."
  (markdown-test-string "<!-- [text](url) -->"
   (markdown-test-range-has-face (+ (point-min) 5) (- (point-max) 4) 'markdown-comment-face)
   (should-not (markdown-range-property-any
                (+ (point-min) 5) (- (point-max) 4) 'face
                '(markdown-markup-face markdown-link-face markdown-url-face)))))

(ert-deftest test-markdown-font-lock/footnote-markers-links ()
  "Test an edge case involving footnote markers and inline reference links."
  (markdown-test-string "Harvard[^1] [tuition][]"
   (markdown-test-range-has-face 1 7 nil)
   (markdown-test-range-has-face 8 8 'markdown-markup-face)
   (markdown-test-range-has-face 10 10 'markdown-footnote-marker-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)
   (markdown-test-range-has-face 12 12 nil)
   (markdown-test-range-has-face 13 13 'markdown-markup-face)
   (markdown-test-range-has-face 14 20 'markdown-link-face)
   (markdown-test-range-has-face 21 21 'markdown-markup-face)
   (markdown-test-range-has-face 22 23 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/mmd-metadata ()
  "Basic MultMarkdown metadata tests."
  (markdown-test-string "Title: peg-multimarkdown User's Guide
Author: Fletcher T. Penney
Base Header Level: 2"
   (markdown-test-range-has-face 1 5 'markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 'markdown-markup-face)
   (markdown-test-range-has-face 8 37 'markdown-metadata-value-face)
   (markdown-test-range-has-face 39 44 'markdown-metadata-key-face)
   (markdown-test-range-has-face 46 46 'markdown-markup-face)
   (markdown-test-range-has-face 47 64 'markdown-metadata-value-face)
   (markdown-test-range-has-face 66 82 'markdown-metadata-key-face)
   (markdown-test-range-has-face 83 83 'markdown-markup-face)
   (markdown-test-range-has-face 85 85 'markdown-metadata-value-face))
  ;; Avoid triggering when a title contains a colon (e.g., Markdown: Syntax)
  (markdown-test-file "syntax.text"
   (markdown-test-range-has-face 1 16 'markdown-header-face-1)))

(ert-deftest test-markdown-font-lock/mmd-metadata-after-header ()
  "Ensure that similar lines are not matched after the header."
  (markdown-test-string "Title: peg-multimarkdown User's Guide

Author: Fletcher T. Penney
Base Header Level: 2"
   (markdown-test-range-has-face 1 5 'markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 'markdown-markup-face)
   (markdown-test-range-has-face 8 37 'markdown-metadata-value-face)
   (markdown-test-range-has-face 40 65 nil)
   (markdown-test-range-has-face 67 86 nil)))

(ert-deftest test-markdown-font-lock/mmd-metadata-after-header-with-whitespace ()
  "Ensure that similar lines are not matched after the header.
The blank line here has two spaces, which should not change how
it is parsed."
  (markdown-test-string
   "Title: peg-multimarkdown User's Guide\n  \nAuthor: Fletcher T. Penney\nBase Header Level: 2\n"
   (markdown-test-range-has-face 1 5 'markdown-metadata-key-face)
   (markdown-test-range-has-face 6 6 'markdown-markup-face)
   (markdown-test-range-has-face 8 37 'markdown-metadata-value-face)
   (markdown-test-range-has-face 42 67 nil)
   (markdown-test-range-has-face 69 88 nil)))

(ert-deftest test-markdown-font-lock/pandoc-metadata ()
  "Basic Pandoc metadata tests."
  (markdown-test-string "% title
  two-line title
% first author;
  second author
% date

body"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 3 24 'markdown-metadata-value-face)
   (markdown-test-range-has-face 26 26 'markdown-markup-face)
   (markdown-test-range-has-face 28 56 'markdown-metadata-value-face)
   (markdown-test-range-has-face 58 58 'markdown-markup-face)
   (markdown-test-range-has-face 60 63 'markdown-metadata-value-face)
   (markdown-test-range-has-face 64 69 nil)))

(ert-deftest test-markdown-font-lock/yaml-metadata ()
  "Basic YAML metadata tests."
  (markdown-test-string
   "---
layout: post
date: 2015-08-13 11:35:25 EST
---
"
   (markdown-test-range-has-face 1 3 'markdown-markup-face)
   (markdown-test-range-has-face 5 10 'markdown-metadata-key-face)
   (markdown-test-range-has-face 11 11 'markdown-markup-face)
   (markdown-test-range-has-face 13 16 'markdown-metadata-value-face)
   (markdown-test-range-has-face 18 21 'markdown-metadata-key-face)
   (markdown-test-range-has-face 22 22 'markdown-markup-face)
   (markdown-test-range-has-face 24 46 'markdown-metadata-value-face)
   (markdown-test-range-has-face 48 50 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/toml-metadata ()
  "Basic TOML metadata tests."
  (markdown-test-string
   "---
layout = post
date = 2015-08-13 11:35:25 EST
---
"
   (markdown-test-range-has-face 1 3 'markdown-markup-face)
   (markdown-test-range-has-face 5 10 'markdown-metadata-key-face)
   (markdown-test-range-has-face 12 12 'markdown-markup-face)
   (markdown-test-range-has-face 14 17 'markdown-metadata-value-face)
   (markdown-test-range-has-face 19 22 'markdown-metadata-key-face)
   (markdown-test-range-has-face 24 24 'markdown-markup-face)
   (markdown-test-range-has-face 26 48 'markdown-metadata-value-face)
   (markdown-test-range-has-face 50 52 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/pandoc-yaml-metadata ()
  "Basic yaml metadata tests, with pandoc syntax."
  (let ((markdown-use-pandoc-style-yaml-metadata t))
    (markdown-test-string
     "some text

---
layout: post
date: 2015-08-13 11:35:25 EST
...

more text

---
layout: post
date: 2015-08-13 11:35:25 EST
---

But this is merely a code block

```
---
layout: post
date: 2015-08-13 11:35:25 EST
---
```
"
     ;; first section
     (markdown-test-range-has-face 12 14 'markdown-markup-face)
     (markdown-test-range-has-face 16 21 'markdown-metadata-key-face)
     (markdown-test-range-has-face 22 22 'markdown-markup-face)
     (markdown-test-range-has-face 24 27 'markdown-metadata-value-face)
     (markdown-test-range-has-face 29 32 'markdown-metadata-key-face)
     (markdown-test-range-has-face 33 33 'markdown-markup-face)
     (markdown-test-range-has-face 35 57 'markdown-metadata-value-face)
     (markdown-test-range-has-face 59 61 'markdown-markup-face)
     ;; second section
     (markdown-test-range-has-face 75 77 'markdown-markup-face)
     (markdown-test-range-has-face 79 84 'markdown-metadata-key-face)
     (markdown-test-range-has-face 85 85 'markdown-markup-face)
     (markdown-test-range-has-face 87 90 'markdown-metadata-value-face)
     (markdown-test-range-has-face 92 95 'markdown-metadata-key-face)
     (markdown-test-range-has-face 96 96 'markdown-markup-face)
     (markdown-test-range-has-face 98 120 'markdown-metadata-value-face)
     (markdown-test-range-has-face 122 124 'markdown-markup-face)
     ;; third section
     (markdown-test-range-has-face 160 162 'markdown-markup-face)
     (markdown-test-range-has-face 164 213 'markdown-pre-face)
     (markdown-test-range-has-face 215 217 'markdown-markup-face))))

(ert-deftest test-markdown-font-lock/line-break ()
  "Basic line break tests."
  (markdown-test-string "    \nasdf  \n"
   (markdown-test-range-has-face 1 9 nil)
   (markdown-test-range-has-face 10 11 'markdown-line-break-face)))

(ert-deftest test-markdown-font-lock/blockquote-bold ()
  "Test font lock for bold inside of a blockquote."
  (markdown-test-string
   "> **bold**"
   (markdown-test-range-has-face 1 10 'markdown-blockquote-face)
   (markdown-test-range-has-face 5 8 'markdown-bold-face)))

(ert-deftest test-markdown-font-lock/blockquote-italic ()
  "Test font lock for italic inside of a blockquote."
  (markdown-test-string
   "> *italic*"
   (markdown-test-range-has-face 1 10 'markdown-blockquote-face)
   (markdown-test-range-has-face 4 9 'markdown-italic-face)))

(ert-deftest test-markdown-font-lock/blockquote-code ()
  "Test font lock for inline code inside of a blockquote."
  (markdown-test-string
   "> `code`"
   (markdown-test-range-has-face 1 8 'markdown-blockquote-face)
   (markdown-test-range-has-face 3 3 'markdown-markup-face)
   (markdown-test-range-has-face 4 7 'markdown-inline-code-face)
   (markdown-test-range-has-face 8 8 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/blockquote-link ()
  "Test font lock for links inside of a blockquote.
This test will fail until font lock for inline links inside
blockquotes is implemented (at present, the blockquote face
takes precedence)."
  (markdown-test-string
   "> [link](url)"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 2 13 'markdown-blockquote-face)
   (markdown-test-range-has-face 3 3 'markdown-markup-face)
   (markdown-test-range-has-face 4 7 'markdown-link-face)
   (markdown-test-range-has-face 8 9 'markdown-markup-face)
   (markdown-test-range-has-face 10 12 'markdown-url-face)
   (markdown-test-range-has-face 13 13 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/blockquote-comment ()
  "Test font lock for comments inside of a blockquote."
  (markdown-test-string
   "> <!-- comment -->"
   (markdown-test-range-has-face 1 1 'markdown-markup-face)
   (markdown-test-range-has-face 3 18 'markdown-comment-face)))

(ert-deftest test-markdown-font-lock/pre-override ()
  "Test that font lock for pre blocks overrides everything else."
  (markdown-test-string
   "    **bold**
    _italic_
    <!-- comment -->
    [link](url)
    * list"
   (markdown-test-range-has-face 1 73 'markdown-pre-face)))

(ert-deftest test-markdown-font-lock/gfm-code-block-font-lock ()
  "GFM code block font lock test. Now in base markdown-mode as well!"
  (markdown-test-file "gfm.text"
    (markdown-test-range-has-face 2639 2641 'markdown-markup-face) ; ```
    (markdown-test-range-has-face 2642 2645 'markdown-language-keyword-face) ; lang
    (markdown-test-range-has-face 2647 2728 'markdown-pre-face) ; code
    (markdown-test-range-has-face 2730 2732 'markdown-markup-face))) ; ```

(ert-deftest test-markdown-font-lock/reference-definition ()
  "Reference definitions should not include ]."
  (let ((markdown-hide-urls nil))
    (markdown-test-string "[1]: http://daringfireball.net/ \"title\""
      (markdown-test-range-has-face 2 2 'markdown-reference-face) ; 1
      (markdown-test-range-has-face 6 31 'markdown-url-face) ; URL
      (markdown-test-range-has-face 34 38 'markdown-link-title-face)) ; title
    (markdown-test-string "[foo][1] and [bar][2]: not a reference definition"
      (markdown-test-range-has-face 2 4 'markdown-link-face) ; foo
      (markdown-test-range-has-face 7 7 'markdown-reference-face) ; 1
      (markdown-test-range-has-face 9 13 nil) ; [ ]and[ ]
      (markdown-test-range-has-face 15 17 'markdown-link-face) ; bar
      (markdown-test-range-has-face 20 20 'markdown-reference-face) ; 2
      (markdown-test-range-has-face 22 49 nil)))) ; [ ]and[ ]

(ert-deftest test-markdown-font-lock/subscripts ()
  "Test font lock for subscripts."
  (markdown-test-string "H~2~0"
    (markdown-test-range-has-face 2 2 'markdown-markup-face) ; First ~
    (markdown-test-range-has-face 3 3 nil) ; 2
    (markdown-test-range-has-face 4 4 'markdown-markup-face))) ; Second ~

(ert-deftest test-markdown-font-lock/superscripts ()
  "Test font lock for subscripts."
  (markdown-test-string "334^10^"
    (markdown-test-range-has-face 1 3 nil) ; 334
    (markdown-test-range-has-face 4 4 'markdown-markup-face) ; First ^
    (markdown-test-range-has-face 5 6 nil) ; 10
    (markdown-test-range-has-face 7 7 'markdown-markup-face))) ; Second ^

(ert-deftest test-markdown-font-lock/hidden-urls-inline ()
  "Test URL hiding and toggling."
  (let ((markdown-hide-urls t))
    (markdown-test-file "inline.text"
      (markdown-test-range-has-face 925 925 'markdown-markup-face)
      (markdown-test-range-has-face 926 929 'markdown-link-face)
      (markdown-test-range-has-face 930 931 'markdown-markup-face)
      (markdown-test-range-has-face 932 949 'markdown-url-face)
      (markdown-test-range-has-face 951 957 'markdown-link-title-face)
      (markdown-test-range-has-face 958 958 'markdown-markup-face)
      (should (get-text-property 932 'composition)))))

(ert-deftest test-markdown-font-lock/hidden-urls-reference ()
  "Test URL hiding and toggling."
  (let ((markdown-hide-urls t))
    (markdown-test-string "[link][15]"
      ;; Two-character reference labels shouldn't get composed.
      (markdown-test-range-has-face 1 1 'markdown-markup-face)
      (markdown-test-range-has-face 2 5 'markdown-link-face)
      (markdown-test-range-has-face 6 7 'markdown-markup-face)
      (markdown-test-range-has-face 8 9 'markdown-reference-face)
      (markdown-test-range-has-face 10 10 'markdown-markup-face)
      (should-not (get-text-property 8 'composition)))
    (markdown-test-string "[link][long-reference-label]"
      ;; Longer reference labels should be composed
      (markdown-test-range-has-face 1 1 'markdown-markup-face)
      (markdown-test-range-has-face 2 5 'markdown-link-face)
      (markdown-test-range-has-face 6 7 'markdown-markup-face)
      (markdown-test-range-has-face 8 27 'markdown-reference-face)
      (markdown-test-range-has-face 28 28 'markdown-markup-face)
      (should (get-text-property 8 'composition)))))

(ert-deftest test-markdown-font-lock/snake-case-code-in-heading ()
  "Test underscores in inline code in headings."
  (markdown-test-string "# Title with `snake_case_code`"
    (should-not (markdown-range-property-any 21 24 'face '(markdown-italic-face)))
    (markdown-test-range-has-face 15 29 'markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/stars-in-code-in-heading ()
  "Test asterisks in inline code in headings."
  (markdown-test-string "# Title with `char** foo, int* bar`"
    (should-not (markdown-range-property-any 20 29 'face '(markdown-italic-face)))
    (markdown-test-range-has-face 15 34 'markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/stars-in-code-in-blockquote ()
  "Test asterisks in inline code in blockquote."
  (markdown-test-string "> Quote with `**stars**`"
    (should-not (markdown-range-property-any
                 17 21 'face '(markdown-italic-face markdown-bold-face)))
    (markdown-test-range-has-face 15 23 'markdown-inline-code-face)))

(ert-deftest test-markdown-font-lock/two-bold-words-after-list ()
  "Test two bold words after a list marker."
  (markdown-test-string "- **foo** **bar**"
    (should-not (markdown-range-property-any
                 (point-min) (point-max) 'face '(markdown-italic-face)))))

(ert-deftest test-markdown-font-lock/heading-with-italics-and-bold ()
  "Test two bold words after a list marker."
  (markdown-test-string "# Title with *italics* and **bold**"
    (markdown-test-range-has-face 15 21 'markdown-italic-face)
    (markdown-test-range-has-face 30 33 'markdown-bold-face)
    (should-not (markdown-range-property-any 30 33 'face '(markdown-italic-face)))))

(ert-deftest test-markdown-font-lock/heading-with-italics-and-bold ()
  "Test that HRs are distinguished from setext H2 markup."
  (markdown-test-file "outline.text"
    (goto-char 485)
    (should (markdown-on-heading-p))
    (beginning-of-line)
    (should (markdown-on-heading-p))
    (should-not (markdown-range-property-any 453 484 'face '(markdown-hr-face)))))

(ert-deftest test-markdown-font-lock/heading-code-block-no-whitespace ()
  "Headings immediately before code blocks should be identified correctly.
See GH-234."
  (markdown-test-string
   "#### code snippet
```javascript
const styles = require('gadgets/dist/styles.css');
```"
   (goto-char (point-min))
   (forward-word)
   (should (markdown-on-heading-p))
   (should (markdown-match-propertized-text 'markdown-heading (point-at-eol)))
   (goto-char (match-beginning 0))
   (should (markdown-outline-level))
   (should (= (markdown-outline-level) 4))
   (markdown-test-range-has-face 6 17 'markdown-header-face-4)
   (end-of-line)
   (should-not (markdown-code-block-at-point-p))))

(ert-deftest test-markdown-font-lock/hr-underscore-with-spaces ()
  "Test font-lock for HR with spaced underscores."
  (markdown-test-string "_ _ _ _ _ _ _\n"
    (markdown-test-range-face-equals (point-min) (- (point-max) 2) 'markdown-hr-face)))

(ert-deftest test-markdown-font-lock/hr-underscore-no-spaces ()
  "Test font-lock for HR with underscores and no spaces."
  (markdown-test-string "_____________\n"
    (markdown-test-range-face-equals (point-min) (- (point-max) 2) 'markdown-hr-face)))

(ert-deftest test-markdown-font-lock/inline-attributes ()
  "Test inline attributes before a fenced code block."
  (markdown-test-file "Leanpub.md"
    ;; Inline attributes for a heading
    (markdown-test-range-has-face 38 42 'markdown-markup-face)
    ;; Inline attributes inside an aside block
    (markdown-test-range-has-face 123 141 'markdown-markup-face)
    ;; Inline attributes before a fenced code block
    (markdown-test-range-has-face 632 696 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/leanpub-sections ()
  "Test Leanpub section markers."
  (markdown-test-file "Leanpub.md"
    ;; {frontmatter}
    (markdown-test-range-has-face 12 24 'markdown-markup-face)
    ;; {mainmatter}
    (markdown-test-range-has-face 69 80 'markdown-markup-face)
    ;; {pagebreak}
    (markdown-test-range-has-face 427 437 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/leanpub-include ()
  "Test Leanpub include syntax."
  (markdown-test-file "Leanpub.md"
    ;; no title
    (markdown-test-range-has-face 561 563 'markdown-markup-face)
    (markdown-test-range-has-face 564 577 'markdown-url-face)
    (markdown-test-range-has-face 578 578 'markdown-markup-face)
    ;; title
    (markdown-test-range-has-face 581 583 'markdown-markup-face)
    (markdown-test-range-has-face 584 611 'markdown-link-title-face)
    (markdown-test-range-has-face 612 613 'markdown-markup-face)
    (markdown-test-range-has-face 614 628 'markdown-url-face)
    (markdown-test-range-has-face 629 629 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/curly-brace-include ()
  "Test curly brace include syntax."
  (markdown-test-string "<<{file}"
    (markdown-test-range-has-face 1 3 'markdown-markup-face)
    (markdown-test-range-has-face 4 7 'markdown-url-face)
    (markdown-test-range-has-face 8 8 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/square-bracket-include ()
  "Test square bracket include syntax."
  (markdown-test-string "<<[file]"
    (markdown-test-range-has-face 1 3 'markdown-markup-face)
    (markdown-test-range-has-face 4 7 'markdown-url-face)
    (markdown-test-range-has-face 8 8 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/pandoc-inline-footnote ()
  "Test font lock for Pandoc inline footnotes."
  (markdown-test-string "Here is an inline note.^[Inline notes are easier to write, since
you don't have to pick an identifier and move down to type the
note.]  And then you can close it and continue writing."
    (markdown-test-range-has-face 1 23 nil)
    (markdown-test-range-has-face 24 25 'markdown-markup-face)
    (markdown-test-range-has-face 26 133 'markdown-footnote-text-face)
    (markdown-test-range-has-face 134 134 'markdown-markup-face)))

(ert-deftest test-markdown-font-lock/pandoc-inline-footnote-across-block ()
  "Test font lock for Pandoc inline footnotes."
  (markdown-test-string "Inline notes should not^[match

across blocks]"
    (markdown-test-range-has-face (point-min) (point-max) nil)))

(ert-deftest test-markdown-font-lock/html-entity-named ()
  "Test basic font-lock support for named HTML entities."
  (markdown-test-string "&nbsp;"
    (markdown-test-range-has-face 1 6 'markdown-html-entity-face)))

(ert-deftest test-markdown-font-lock/html-entity-hex ()
  "Test basic font-lock support for hexadecimal HTML entities."
  (markdown-test-string "&#x272a;"
    (markdown-test-range-has-face 1 8 'markdown-html-entity-face)))

(ert-deftest test-markdown-font-lock/html-entity-decimal ()
  "Test basic font-lock support for decimal HTML entities."
  (markdown-test-string "&#9;"
    (markdown-test-range-has-face 1 4 'markdown-html-entity-face)))

(ert-deftest test-markdown-font-lock/html-entity-in-inline-code ()
  "Test that HTML entities are not matched inside inline code."
  (markdown-test-string "`&#9;`"
    (markdown-test-range-has-face 1 1 'markdown-markup-face)
    (markdown-test-range-has-face 2 5 'markdown-inline-code-face)
    (markdown-test-range-has-face 6 6 'markdown-markup-face)
    (should-not (markdown-range-property-any 1 6 'face '(markdown-html-entity-face)))))

(ert-deftest test-markdown-font-lock/html-entity-in-gfm-code-block ()
  "Test that HTML entities are not matched inside GFM code blocks."
  (markdown-test-string "```\n&nbsp;\n&#x272a;\n&#9;\n```"
    (should-not
     (markdown-range-property-any
      (point-min) (point-max) 'face '(markdown-html-entity-face)))))

(ert-deftest test-markdown-font-lock/html-tags-in-syntax-file ()
  "Test matching HTML tags in syntax.text."
  (markdown-test-file "syntax.text"
    ;; <ul id="ProjectSubmenu">
    (markdown-test-range-has-face 36 36 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 37 38 'markdown-html-tag-name-face)
    (markdown-test-range-has-face 40 41 'markdown-html-attr-name-face)
    (markdown-test-range-has-face 42 42 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 43 58 'markdown-html-attr-value-face)
    (markdown-test-range-has-face 59 59 'markdown-html-tag-delimiter-face)
    ;; <li>
    (markdown-test-range-has-face 65 65 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 66 67 'markdown-html-tag-name-face)
    (markdown-test-range-has-face 68 68 'markdown-html-tag-delimiter-face)
    ;; <a href="/projects/markdown/" title="Markdown Project Page">
    (markdown-test-range-has-face 69 69 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 70 70 'markdown-html-tag-name-face)
    (markdown-test-range-has-face 72 75 'markdown-html-attr-name-face)
    (markdown-test-range-has-face 76 76 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 77 97 'markdown-html-attr-value-face)
    (markdown-test-range-has-face 99 103 'markdown-html-attr-name-face)
    (markdown-test-range-has-face 104 104 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 105 127 'markdown-html-attr-value-face)
    (markdown-test-range-has-face 128 128 'markdown-html-tag-delimiter-face)))

(ert-deftest test-markdown-font-lock/html-tag-in-gfm-code-block ()
  "Test that HTML tags are not matched inside GFM code blocks."
  (markdown-test-string "```\n<ul id=\"ProjectSubmenu\">\n```"
    (should-not
     (markdown-range-property-any
      (point-min) (point-max) 'face
      '(markdown-html-tag-name-face
        markdown-html-tag-delimiter-face
        markdown-html-attr-name-face
        markdown-html-attr-value-face)))))

(ert-deftest test-markdown-font-lock/html-tag-in-code-block ()
  "Test that HTML tags are not matched inside code blocks."
  (markdown-test-string "    <ul id=\"ProjectSubmenu\">"
    (should-not
     (markdown-range-property-any
      (point-min) (point-max) 'face
      '(markdown-html-tag-name-face
        markdown-html-tag-delimiter-face
        markdown-html-attr-name-face
        markdown-html-attr-value-face)))))

(ert-deftest test-markdown-font-lock/html-tag-in-inline-code ()
  "Test that HTML tags are not matched inside inline code spans."
  (markdown-test-string "`<ul id=\"ProjectSubmenu\">`"
    (should-not
     (markdown-range-property-any
      (point-min) (point-max) 'face
      '(markdown-html-tag-name-face
        markdown-html-tag-delimiter-face
        markdown-html-attr-name-face
        markdown-html-attr-value-face)))))

(ert-deftest test-markdown-font-lock/html-disabled ()
  "Test disabling font-lock for HTML tags"
  (let ((markdown-enable-html nil))
    (markdown-test-file "syntax.text"
      (should-not
       (markdown-range-property-any
        (point-min) (point-max) 'face
        '(markdown-html-tag-name-face
          markdown-html-tag-delimiter-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face))))))

(ert-deftest test-markdown-font-lock/html-tag-angle-bracket ()
  "Test a hard to parse HTML attribute with an angle bracket."
  (markdown-test-string "<img title=\"displays >\" src=\"big.gif\">"
    (markdown-test-range-has-face 1 1 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 2 4 'markdown-html-tag-name-face)
    (markdown-test-range-has-face 6 10 'markdown-html-attr-name-face)
    (markdown-test-range-has-face 11 11 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 12 23 'markdown-html-attr-value-face)
    (markdown-test-range-has-face 25 27 'markdown-html-attr-name-face)
    (markdown-test-range-has-face 28 28 'markdown-html-tag-delimiter-face)
    (markdown-test-range-has-face 29 37 'markdown-html-attr-value-face)
    (markdown-test-range-has-face 38 38 'markdown-html-tag-delimiter-face)))

;;; Markdown Parsing Functions:

(ert-deftest test-markdown-parsing/extend-region-function ()
  "Test `markdown-syntax-propertize-extend-region'.
Should return a cons (NEW-START . NEW-END) or nil if no
adjustment should be made. Function is called repeatedly until it
returns nil."
  (markdown-test-file
   "inline.text"
   (should (equal (markdown-syntax-propertize-extend-region 1 17)
                  (cons 1 91)))
   (should (equal (markdown-syntax-propertize-extend-region 2 17)
                  (cons 1 91)))
   (should (equal (markdown-syntax-propertize-extend-region 1 91)
                  nil))
   (should (equal (markdown-syntax-propertize-extend-region 93 157)
                  nil))
   (should (equal (markdown-syntax-propertize-extend-region 496 502)
                  (cons 486 510)))
   (should (equal (markdown-syntax-propertize-extend-region 486 510)
                  nil))
   ;; Region that begins and ends with \n\n should not be extended
   (should (equal (markdown-syntax-propertize-extend-region 157 355)
                  nil))))

(defun markdown-test-check-match-limits (prop num begin end &optional pos)
  (let* ((posn (or pos (point)))
         (props (get-text-property posn prop)))
    (save-match-data
      (set-match-data props)
      (should (match-beginning num))
      (should (match-end num))
      (should (= (match-beginning num) begin))
      (should (= (match-end num) end)))))

(ert-deftest test-markdown-parsing/syntax-with-adjacent-code-blocks ()
  "Test `markdown-syntax-propertize-fenced-code-blocks' with adjacent blocks."
  (markdown-test-string
   "~~~ shell
#!/bin/sh

echo \"Hello, world!\"
~~~

~~~ shell
#!/bin/sh

echo \"Hello, world v2!\"
~~~
"
   (let ((start-top-1 (make-marker)) (end-top-1 (make-marker))
         (start-lang-1 (make-marker)) (end-lang-1 (make-marker))
         (start-mid-1 (make-marker)) (end-mid-1 (make-marker))
         (start-bottom-1 (make-marker)) (end-bottom-1 (make-marker))
         (between (make-marker))
         (start-top-2 (make-marker)) (end-top-2 (make-marker))
         (start-lang-2 (make-marker)) (end-lang-2 (make-marker))
         (start-mid-2 (make-marker)) (end-mid-2 (make-marker))
         (start-bottom-2 (make-marker)) (end-bottom-2 (make-marker)))
     ;; First code block
     (set-marker start-top-1 1)
     (set-marker end-top-1 4)
     (set-marker start-lang-1 5)
     (set-marker end-lang-1 10)
     (set-marker start-mid-1 11)
     (set-marker end-mid-1 43)
     (set-marker start-bottom-1 43)
     (set-marker end-bottom-1 46)
     ;; check top tildes
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 1 (marker-position start-top-1)
      (marker-position end-top-1) (marker-position start-top-1))
     ;; check top language specifier
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 3 (marker-position start-lang-1)
      (marker-position end-lang-1) (marker-position start-lang-1))
     ;; check text in between
     (markdown-test-check-match-limits
      'markdown-fenced-code 0 (marker-position start-mid-1)
      (marker-position end-mid-1) (marker-position start-mid-1))
     ;; check bottom tildes
     (markdown-test-check-match-limits
      'markdown-tilde-fence-end 1 (marker-position start-bottom-1)
      (marker-position end-bottom-1) (marker-position start-bottom-1))
     ;; Point between code blocks
     (set-marker between 47)
     (should (equal (get-text-property between 'markdown-fenced-code)
                    nil))
     ;; Second code block
     (set-marker start-top-2 48)
     (set-marker end-top-2 51)
     (set-marker start-lang-2 52)
     (set-marker end-lang-2 57)
     (set-marker start-mid-2 58)
     (set-marker end-mid-2 93)
     (set-marker start-bottom-2 93)
     (set-marker end-bottom-2 96)
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 1 (marker-position start-top-2)
      (marker-position end-top-2) (marker-position start-top-2))
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 3 (marker-position start-lang-2)
      (marker-position end-lang-2) (marker-position start-lang-2))
     (markdown-test-check-match-limits
      'markdown-fenced-code 0 (marker-position start-mid-2)
      (marker-position end-mid-2) (marker-position start-mid-2))
     (markdown-test-check-match-limits
      'markdown-tilde-fence-end 1 (marker-position start-bottom-2)
      (marker-position end-bottom-2) (marker-position start-bottom-2))
     ;; ;; Move point between code blocks and insert a character
     (goto-char between)
     (insert "x")
     ;; Re-propertize region after change
     (let ((range (markdown-syntax-propertize-extend-region (1- between) (point-max))))
       (markdown-syntax-propertize (car range) (cdr range)))
     ;; Re-check first code block
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 1 (marker-position start-top-1)
      (marker-position end-top-1) (marker-position start-top-1))
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 3 (marker-position start-lang-1)
      (marker-position end-lang-1) (marker-position start-lang-1))
     (markdown-test-check-match-limits
      'markdown-fenced-code 0 (marker-position start-mid-1)
      (marker-position end-mid-1) (marker-position start-mid-1))
     (markdown-test-check-match-limits
      'markdown-tilde-fence-end 1 (marker-position start-bottom-1)
      (marker-position end-bottom-1) (marker-position start-bottom-1))
     ;; Re-check point between code blocks
     (should (equal (get-text-property between 'markdown-fenced-code)
                    nil))
     ;; Re-check second code block
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 1 (marker-position start-top-2)
      (marker-position end-top-2) (marker-position start-top-2))
     (markdown-test-check-match-limits
      'markdown-tilde-fence-begin 3 (marker-position start-lang-2)
      (marker-position end-lang-2) (marker-position start-lang-2))
     (markdown-test-check-match-limits
      'markdown-fenced-code 0 (marker-position start-mid-2)
      (marker-position end-mid-2) (marker-position start-mid-2))
     (markdown-test-check-match-limits
      'markdown-tilde-fence-end 1 (marker-position start-bottom-2)
      (marker-position end-bottom-2)
      (marker-position start-bottom-2)))))

(ert-deftest test-markdown-parsing/propertize-fenced-in-between ()
  "Test whether `markdown-syntax-propertize-fenced-block-constructs' handles the
case when it can't propertize both the start and end of a fenced block within a
single pass (the end of the block is past the END argument)."
  (markdown-test-string
      "~~~ shell
#!/bin/sh

echo \"Hello, world!\"
~~~
"
    (set-text-properties (point-min) (point-max) nil)
    ;; syntax-propertize up to right after hashbang
    (markdown-syntax-propertize-fenced-block-constructs (point-min) 21)
    ;; ~~~ shell should be propertized, but nothing else
    ;; check tildes
    (markdown-test-check-match-limits 'markdown-tilde-fence-begin 1 1 4 1)
    ;; check language
    (markdown-test-check-match-limits 'markdown-tilde-fence-begin 3 5 10 5)
    ;; middle should not be propertized
    (should-not (get-text-property 11 'markdown-fenced-code))
    ;; neither should end
    (should-not (get-text-property 43 'markdown-tilde-fence-end))
    (markdown-syntax-propertize-fenced-block-constructs 21 (point-max))
    ;; everything should be propertized now
    ;; re-check top
    (markdown-test-check-match-limits 'markdown-tilde-fence-begin 1 1 4 1)
    (markdown-test-check-match-limits 'markdown-tilde-fence-begin 3 5 10 5)
    ;; check middle
    (markdown-test-check-match-limits 'markdown-fenced-code 0 10 43 10)
    ;; check ending tildes
    (markdown-test-check-match-limits 'markdown-tilde-fence-end 1 43 46 43)))

(ert-deftest test-markdown-parsing/get-code-block-at-pos ()
  "Test whether `markdown-code-block-at-pos' works in all situations. All
  situations are:
1. pre block
2. tilde block
3. gfm block
4. yaml metadata block"
  (let ((markdown-use-pandoc-style-yaml-metadata t))
    (markdown-test-string
        "
~~~ ruby
some_ruby_fun()
~~~

---
a: b
---

``` {.bash}
#!/bin/sh
echo hey
```

    pre code
    random stuff
    more preformatted code

---
data: pandoc
...
"
      ;; start/mid/end at tilde block
      (should (equal (markdown-code-block-at-pos 2) (list 2 30)))
      (should (equal (markdown-code-block-at-pos 11) (list 2 30)))
      (should (equal (markdown-code-block-at-pos 27) (list 2 30)))
      ;; yaml metadata block
      (should (equal (markdown-code-block-at-pos 32) (list 32 44)))
      (should (equal (markdown-code-block-at-pos 36) (list 32 44)))
      (should (equal (markdown-code-block-at-pos 41) (list 32 44)))
      ;; gfm block
      (should (equal (markdown-code-block-at-pos 46) (list 46 80)))
      (should (equal (markdown-code-block-at-pos 58) (list 46 80)))
      (should (equal (markdown-code-block-at-pos 77) (list 46 80)))
      ;; pre block
      (should (equal (markdown-code-block-at-pos 82) (list 82 138)))
      (should (equal (markdown-code-block-at-pos 99) (list 82 138)))
      (should (equal (markdown-code-block-at-pos 137) (list 82 138)))
      ;; pandoc yaml metadata block (should work if yaml above works)
      (should (equal (markdown-code-block-at-pos 140) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 142) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 144) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 157) (list 140 160)))
      (should (equal (markdown-code-block-at-pos 159) (list 140 160))))))

(ert-deftest test-markdown-parsing/syntax-get-fenced-blocks ()
  "Test whether *-get-fenced-block-* functions work in the case where a block is
only partially propertized."
  (save-match-data
    (markdown-test-string
     "~~~
"
     (should (equal (markdown-syntax-propertize-extend-region
                     (point-min) (point-max))
                    nil))
     (goto-char 1)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-begin))
     (should (equal (markdown-get-fenced-block-from-start
                     'markdown-tilde-fence-begin)
                    nil)))
    (markdown-test-string
     "~~~
~~~"
     (goto-char 1)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-begin))
     (should (equal (markdown-get-fenced-block-from-start
                     'markdown-tilde-fence-begin)
                    (list 1 8)))
     (should (equal (markdown-code-block-at-pos (point)) (list 1 8)))

     ;; markdown-code-block-at-point-p should not modify match data
     (set-match-data (list 1 2 3 4))
     (should (markdown-code-block-at-point-p))
     (should (equal (match-data) (list 1 2 3 4)))

     (goto-char 5)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-end))
     (should (equal (markdown-get-fenced-block-from-end
                     'markdown-tilde-fence-end)
                    (list 1 8)))
     (should (equal (markdown-code-block-at-pos (point)) (list 1 8))))
    (markdown-test-string
     "~~~

~~~"
     (goto-char 1)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-begin))
     (should (equal (markdown-get-fenced-block-from-start
                     'markdown-tilde-fence-begin)
                    (list 1 9)))
     (should (equal (markdown-code-block-at-pos (point)) (list 1 9)))
     (goto-char 5)
     (set-match-data (markdown-text-property-at-point 'markdown-fenced-code))
     (should (equal (markdown-get-fenced-block-from-middle
                     'markdown-fenced-code)
                    (list 1 9)))
     (should (equal (markdown-code-block-at-pos (point)) (list 1 9)))
     (goto-char 6)
     (set-match-data (markdown-text-property-at-point
                      'markdown-tilde-fence-end))
     (should (equal (markdown-get-fenced-block-from-end
                     'markdown-tilde-fence-end)
                    (list 1 9)))
     (should (equal (markdown-code-block-at-pos (point)) (list 1 9))))))

(ert-deftest test-markdown-parsing/reference-definition-basic ()
  "Test reference definition function."
  (markdown-test-file "syntax.text"
   ;; Test accuracy of returned text and bounds
   (should (equal (markdown-reference-definition "1")
                  (list "http://docutils.sourceforge.net/mirror/setext.html" 1942 1992)))
   (should (equal (markdown-reference-definition "2")
                  (list "http://www.aaronsw.com/2002/atx/" 2000 2032)))
   ;; Test that match data remains intact
   (should (string-equal (match-string 5) "http://www.aaronsw.com/2002/atx/"))
   ;; Test anchor-only relative URL
   (should (equal (markdown-reference-definition "bq")
                  (list "#blockquote" 7536 7547)))
   ;; Example references that appear in pre blocks in the text
   (should (not (markdown-reference-definition "")))
   (should (not (markdown-reference-definition "id")))
   (should (not (markdown-reference-definition "foo")))
   (should (not (markdown-reference-definition "A")))
   (should (not (markdown-reference-definition "Google")))
   ;; Test that we don't pick up other text in square brackets
   (should (not (markdown-reference-definition "blockquoting")))
   (should (not (markdown-reference-definition "square brackets")))
   ;; Test case insensitivity
   (should (equal (markdown-reference-definition "SRC")
                  (list "/projects/markdown/syntax.text" 1245 1275)))))

(ert-deftest test-markdown-parsing/get-defined-references ()
  "Test `markdown-get-defined-references'."
  (markdown-test-file "syntax.text"
   (should (equal (markdown-get-defined-references)
                  '("src" "1" "2" "3" "4" "5" "6" "bq" "l"))))
  (markdown-test-file "outline.text"
   (should (equal (markdown-get-defined-references) nil)))
  (markdown-test-file "wiki-links.text"
   (should (equal (markdown-get-defined-references) nil))))

(ert-deftest test-markdown-parsing/get-used-uris ()
  "Test `markdown-get-used-uris'."
  (markdown-test-file "syntax.text"
    (let ((uris (markdown-get-used-uris)))
      (should (equal (nth 0 uris) "#overview"))
      (should (equal (nth 20 uris) "http://www.aaronsw.com/2002/atx/"))
      (should-not (member "http://example.com/" uris))
      (should-not (member "address@example.com" uris)))))

(defun markdown-test-test-region (beg end)
  (goto-char (1- beg))
  (should-not (markdown-inline-code-at-point-p))
  (goto-char (1+ end))
  (should-not (markdown-inline-code-at-point-p))
  (dolist (loc (number-sequence beg end))
    (goto-char loc)
    (should (markdown-inline-code-at-point))
    (should (equal (match-beginning 0) beg))
    (should (equal (match-end 0) end))))

(ert-deftest test-markdown-parsing/inline-code-at-point ()
  "Test `markdown-inline-code-at-point'."
  (markdown-test-file "inline.text"
    (markdown-test-test-region 45 51) ; Regular code span
    (markdown-test-test-region 61 90) ; Code containing backticks
    (markdown-test-test-region 228 240) ; Backquotes at beginning
    (markdown-test-test-region 341 352) ; Backquotes at end
    (markdown-test-test-region 460 469) ; Backslash as final character
    (markdown-test-test-region 657 667) ; A code span crossing lines
    (markdown-test-test-region 749 758) ; Three backquotes on same line
    (markdown-test-test-region 806 815) ; Three backquotes across lines
    ))

(ert-deftest test-markdown-parsing/inline-code-at-point-one-space ()
  "Test `markdown-inline-code-at-point' with multiple code spans in a row."
  (markdown-test-string "`foo` `bar` `baz`"
    (dolist (loc (number-sequence 1 6))
      (goto-char loc)
      ;; markdown-inline-code-at-point should set match data
      (should (markdown-inline-code-at-point))
      (should (equal (match-data) (list 1 6 1 2 2 5 5 6)))
      ;; markdown-inline-code-at-point-p should not modify match data
      (set-match-data (list 1 2 3 4))
      (should (markdown-inline-code-at-point-p))
      (should (equal (match-data) (list 1 2 3 4))))
    (dolist (loc (number-sequence 7 12))
      (goto-char loc)
      (should (markdown-inline-code-at-point))
      (should (equal (match-data) (list 7 12 7 8 8 11 11 12))))
    (dolist (loc (number-sequence 13 18))
      (goto-char loc)
      (should (markdown-inline-code-at-point))
      (should (equal (match-data) (list 13 18 13 14 14 17 17 18))))))

(ert-deftest test-markdown-parsing/inline-code-at-point-no-space ()
  "Test `markdown-inline-code-at-point' with multiple code spans in a row.."
  (markdown-test-string "a`foo`b`bar`c`baz`d"
    (goto-char 1)                       ; "a"
    (should-not (markdown-inline-code-at-point-p))
    (dolist (loc (number-sequence 2 7)) ; "`foo`b"
      (goto-char loc)
      (should (markdown-inline-code-at-point))
      (should (equal (match-data) (list 2 7 2 3 3 6 6 7))))
    (dolist (loc (number-sequence 8 13)) ; "`bar`c"
      (goto-char loc)
      (should (markdown-inline-code-at-point))
      (should (equal (match-data) (list 8 13 8 9 9 12 12 13))))
    (dolist (loc (number-sequence 14 19)) ; "`baz`d"
      (goto-char loc)
      (should (markdown-inline-code-at-point))
      (should (equal (match-data) (list 14 19 14 15 15 18 18 19))))))

(ert-deftest test-markdown-parsing/code-at-point-blank-line ()
  "Test `markdown-inline-code-at-point-p' at beginning of block."
  (markdown-test-string "----------\n\n## foo\n"
   (should-not (markdown-inline-code-at-point-p))
   (forward-line)
   (should-not (markdown-inline-code-at-point-p))
   (forward-line)
   (should-not (markdown-inline-code-at-point-p))))

(ert-deftest test-markdown-parsing/in-comment-p-position ()
  "Test `markdown-in-comment-p'."
  (markdown-test-string
   "HTML <!-- foo --> comment"
   (should (eq (point) (point-min)))
   (should-not (markdown-in-comment-p (point-max)))
   (should (eq (point) (point-min)))))

(ert-deftest test-markdown-parsing/match-comments ()
  "Test `markdown-match-comments'."
  (markdown-test-string
   "HTML <!-- foo --> comment"
   (should (markdown-match-comments (point-max)))
   (should (eq (point) 18))
   (should (equal (match-data) (list 6 18)))
   (should-not (markdown-match-comments (point-max)))))

(ert-deftest test-markdown-parsing/range-property-any ()
  "Test behavior of `markdown-range-property-any'."
  (markdown-test-file
   "inline.text"
   (should (markdown-range-property-any
            (point-min) (point-at-eol)
            'face '(markdown-markup-face markdown-italic-face)))
   (should-not (markdown-range-property-any
            (point-min) (point-at-eol)
            'face '(markdown-bold-face)))))

(ert-deftest test-markdown-parsing/inline-code ()
  "Don't cause infinite loop for inline code just after metadata block
Detail: https://github.com/jrblevin/markdown-mode/issues/115"
  (markdown-test-string "---
x: x
---
`x`
"
    (should (markdown-match-code (point-max)))
    (should (= (point) 18))
    (should (equal (match-data t) '(14 17 14 15 15 16 16 17)))
    (should-not (markdown-match-code (point-max)))))

(ert-deftest test-markdown-parsing/list-item-at-point ()
  "Test `markdown-list-item-at-point-p'."
  (markdown-test-file "lists.text"
    (let ((orig-match-data '(1 2 3 4))
          (not-list-points '(273 399 512 3615))
          (list-points '(1063 1063 1176 1283 1659 1830 1919 2150
                              2393 2484 2762 2853 3097 3188 3700
                              3903 4009)))
      ;; markdown-inline-code-at-point-p should not modify match data
      (set-match-data orig-match-data)
      ;; Not list items
      (dolist (pos not-list-points)
        (goto-char pos)
        (should-not (markdown-list-item-at-point-p))
        (should (equal (match-data) orig-match-data)))
      ;; List items
      (dolist (pos list-points)
        (goto-char pos)
        (should (markdown-list-item-at-point-p))
        (should (equal (match-data) orig-match-data))))))

(ert-deftest test-markdown-parsing/heading-at-point ()
  "Test `markdown-heading-at-point'."
  (save-match-data
    (markdown-test-file "outline.text"
      (should-not (markdown-heading-at-point))
      (markdown-test-goto-heading "An underline-style header")
      (forward-line -1)
      (should (markdown-heading-at-point))
      (should (equal (match-data t) (get-text-property (point) 'markdown-heading)))
      (should (equal (match-data t) (get-text-property (point) 'markdown-heading-1-setext))))))

(ert-deftest test-markdown-parsing/inline-link-in-code-block ()
  "Test `markdown-match-generic-links'."
  (markdown-test-string "    **bold**
    _italic_
    <!-- comment -->
    [link](url)
    * list"
    (goto-char (point-min))
    ;; The link inside the pre block should not match.
    (should-not (markdown-match-generic-links (point-max) nil))
    ;; Point should be left at limit.
    (should (= (point) (point-max)))))

(ert-deftest test-markdown-parsing/broken-inline-link ()
  "Test `markdown-match-generic-links' with an invalid link."
  (markdown-test-string "[site1](http://site1.com
[site2](http://site2.com)
[site3](http://site3.com)"
    (goto-char (point-min))
    (let ((limit (point-at-eol)))
      ;; The first link is broken and shouldn't match.
      (should-not (markdown-match-generic-links limit nil))
      ;; Subsequent search shouldn't match, so point should move to limit.
      (should (= (point) limit)))
    ;; The second link should still match, starting from (point-min).
    (let ((limit (point-at-eol 2)))
      (should (markdown-match-generic-links limit nil))
      (should (= (point) (match-end 0))))
    ;; The third link should match when starting past the second one.
    (goto-char (match-end 0))
    (should (markdown-match-generic-links (point-max) nil))
    (should (= (point) (match-end 0)))))

(ert-deftest test-markdown-parsing/code-block-lang ()
  "Test `markdown-code-block-lang'."
  ;; Test with GFM code blocks.
  (markdown-test-file "GFM.md"
    ;; Test a call with the optional argument.
    (should (string-equal
             (markdown-code-block-lang
              '(1455 . markdown-gfm-block-begin)) "js"))
    ;; Test a call without the optional argument.
    (goto-char 1504) ;; middle of a GFM code block
    (should (string-equal (markdown-code-block-lang) "js")))
  ;; Test with tilde-fenced cdoe blocks.
  (markdown-test-file "outline-code.text"
    (goto-char 107) ;; middle of a tilde fenced code block
    (should (string-equal (markdown-code-block-lang
                           '(83 . markdown-tilde-fence-begin)) "bash"))))

(ert-deftest test-markdown-parsing/code-block-lang-period ()
  "Test `markdown-code-block-lang' when language name begins with a period."
  (markdown-test-string "~~~ { .ruby }
puts 'hello, world'
~~~
"
    (should (string-equal (markdown-code-block-lang) "ruby"))))

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

(ert-deftest test-markdown-lists/nested-list-file ()
  "Test list item propertization for a nested list."
  (markdown-test-file "nested-list.text"
    (let ((values '(((1 25 3 5 "- " nil))
                    ((26 189 3 5 "- " nil))
                    ((191 581 3 5 "- " nil))
                    ((217 482 7 9 "- " nil)
                     (191 581 3 5 "- " nil))
                    ((484 581 7 9 "- " nil)
                     (191 581 3 5 "- " nil))
                    ((514 581 11 13 "- " nil)
                     (484 581 7 9 "- " nil)
                     (191 581 3 5 "- " nil)))))
      (cl-loop
       for value in values
       do (should
           (equal (mapcar #'butlast (get-text-property (point) 'markdown-list-item)) value))
          (markdown-outline-next)))))

(ert-deftest test-markdown-lists/levels-1 ()
  "Test list levels function `markdown-calculate-list-levels'."
  (markdown-test-file "nested-list.text"
   (let ((values '(((1 . 1) . nil) ((2 . 13) . (3)) ((14 . 23) . (7 3))
                   ((24 . 26) . (11 7 3)))))
     (cl-loop for (range . value) in values
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
     (cl-loop for (range . value) in values
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
    (should (equal (butlast (markdown-cur-list-item-bounds))
                   (list 3700 3901 0 4 "-   " nil)))
    (markdown-next-list-item 4)
    (should (eq (point) 3903))
    (should (equal (butlast (markdown-cur-list-item-bounds))
                   (list 3903 3937 0 4 "*   " nil)))))

(ert-deftest test-markdown-lists/bounds-2 ()
  "Function `markdown-cur-list-item-bounds' should return nil outside of list items."
  (markdown-test-string "line one\n\n* item\n"
    (should (null (markdown-cur-list-item-bounds)))
    (forward-line)
    (should (null (markdown-cur-list-item-bounds)))
    (forward-line)
    (should (markdown-cur-list-item-bounds))))

(ert-deftest test-markdown-lists/bounds-prev ()
  "Test list item bounds function `markdown-prev-list-item-bounds'."
  (markdown-test-file "lists.text"
    (markdown-test-goto-heading "Case 9")
    (markdown-next-list-item 4)
    (markdown-next-list-item 4)
    (should (eq (point) 3903))
    (should (equal (butlast (markdown-prev-list-item-bounds))
                   (list 3700 3901 0 4 "-   " nil)))))

(ert-deftest test-markdown-lists/bounds-next ()
  "Test list item bounds function `markdown-next-list-item-bounds'."
  (markdown-test-file "lists.text"
    (markdown-test-goto-heading "Case 2")
    (goto-char 1283)
    (should-not (markdown-next-list-item-bounds))
    (markdown-test-goto-heading "Case 9")
    (markdown-next-list-item 4)
    (should (eq (point) 3700))
    (should (equal (butlast (markdown-next-list-item-bounds))
                   (list 3903 3937 0 4 "*   " nil)))))

(ert-deftest test-markdown-lists/bounds-gfm-task-list-item ()
  "Test `markdown-cur-list-item-bounds' with a GFM task list item."
  (markdown-test-string "  - [ ] task name"
    (should (equal (butlast (markdown-cur-list-item-bounds))
                   '(1 18 2 4 "- " "[ ] ")))))

(ert-deftest test-markdown-lists/gfm-task-list-item-at-point-1 ()
  "Test `markdown-gfm-task-list-item-at-point' with regular list items."
  (markdown-test-file "nested-list.text"
    (dolist (pos '(1 26 36 267 514 540))
      (goto-char pos)
      (should-not (markdown-gfm-task-list-item-at-point)))))

(ert-deftest test-markdown-lists/gfm-task-list-item-at-point-2 ()
  "Test `markdown-gfm-task-list-item-at-point' with a task list item."
  (markdown-test-string "  - [ ] task"
    (should (markdown-gfm-task-list-item-at-point))))

(ert-deftest test-markdown-insertion/insert-gfm-task-list-item ()
  "Test `markdown-insert-list-item' in a GFM task list."
  (markdown-test-string "  - [ ] task"
    (goto-char (point-max))
    (call-interactively 'markdown-insert-list-item)
    (should (string-equal (buffer-string) "  - [ ] task\n  - [ ] "))))

(ert-deftest test-markdown-lists/promotion-and-demotion ()
  "Test function `markdown-promote-list-item'."
  (markdown-test-file "nested-list.text"
    (forward-line)
    (should (looking-at-p "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (markdown-demote-list-item)
    (should (looking-at-p "       - List level 1 item 2

         Second paragraph of item 2

            Nested pre block in item 2
            Four spaces past the marker

         Another paragraph of item 2"))
    (markdown-promote-list-item)
    (should (looking-at-p "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (goto-char (point-min))
    (forward-line 22)
    (should (looking-at-p "           - List level 3 item 1

                 Nested pre block"))
    (markdown-demote-list-item)
    (should (looking-at-p "               - List level 3 item 1

                     Nested pre block"))
    (markdown-promote-list-item)
    (should (looking-at-p "           - List level 3 item 1

                 Nested pre block"))))

(ert-deftest test-markdown-lists/promotion-and-demotion-custom ()
  "Test custom variable `markdown-list-indent-width'."
  (markdown-test-file "nested-list.text"
    (forward-line)
    (should (looking-at "   - List level 1 item 2

     Second paragraph of item 2

        Nested pre block in item 2
        Four spaces past the marker

     Another paragraph of item 2"))
    (let ((markdown-list-indent-width 2))
      (markdown-demote-list-item))
    (should (looking-at "     - List level 1 item 2

       Second paragraph of item 2

          Nested pre block in item 2
          Four spaces past the marker

       Another paragraph of item 2"))))

(ert-deftest test-markdown-lists/add-gfm-checkbox ()
  (markdown-test-file "check-items.text"
    (goto-char (point-min))
    (end-of-line)
    (should (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 1))
    (should (eolp))
    (should (string-equal (buffer-substring-no-properties (line-beginning-position) (point))
                          "  * [ ] "))

    (forward-line 2)
    (back-to-indentation)
    (should (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 3))
    (should (string-equal (buffer-substring-no-properties (line-beginning-position) (point))
                          "  * [ ] "))
    (should (string-equal (buffer-substring-no-properties (point) (line-end-position))
                          "item1"))

    (forward-line 2)
    (back-to-indentation)
    (forward-char 1)
    (should (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 5))
    (should (string-equal (buffer-substring-no-properties (line-beginning-position) (point))
                          "  * [ ] i"))
    (should (string-equal (buffer-substring-no-properties (point) (line-end-position))
                          "tem2"))

    (forward-line 2)
    (back-to-indentation)
    (forward-char 2)
    (should (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 7))
    (should (string-equal (buffer-substring-no-properties (line-beginning-position) (point))
                          "- [ ] "))
    (should (string-equal (buffer-substring-no-properties (point) (line-end-position))
                          "item3"))

    (forward-line 2)
    (back-to-indentation)
    (forward-char 3)
    (should (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 9))
    (should (string-equal (buffer-substring-no-properties (line-beginning-position) (point))
                          "- [ ] i"))
    (should (string-equal (buffer-substring-no-properties (point) (line-end-position))
                          "tem4"))

    (forward-line 2)
    (end-of-line)
    (should-not (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 11))
    (should (eolp))
    (should (string-equal (buffer-substring-no-properties (line-beginning-position) (point))
                          "*   [ ] item5"))

    (forward-line 1)
    (back-to-indentation)
    (should (markdown-insert-gfm-checkbox))
    (should (= (line-number-at-pos (point)) 12))
    (should (eolp))
    (should (string-equal (buffer-substring-no-properties
                           (line-beginning-position)
                           (point))
                          "*   [ ] "))))

(ert-deftest test-markdown-lists/toggle-gfm-checkbox ()
  (markdown-test-string "   -   [X] GFM task list item"
    (should (string-equal (markdown-toggle-gfm-checkbox) "[ ]"))
    (should (string-equal (buffer-string) "   -   [ ] GFM task list item"))
    (should (string-equal (markdown-toggle-gfm-checkbox) "[x]"))
    (should (string-equal (buffer-string) "   -   [x] GFM task list item"))))

(ert-deftest test-markdown-lists/beginning-of-list ()
  "Test `markdown-beginning-of-list'."
  (markdown-test-file "lists.text"
    ;; Case 1: not in a list
    (goto-char 399)
    (should-not (markdown-beginning-of-list))
    (should (= (point) 399))
    ;; Case 2
    (goto-char 1281)
    (should (= (markdown-beginning-of-list) 1063))
    (should (= (point) 1063))
    (goto-char 1395)
    (should (= (markdown-beginning-of-list) 1063))
    (should (= (point) 1063))
    ;; Case 3
    (goto-char 1848)
    (should (= (markdown-beginning-of-list) 1659))
    (should (= (point) 1659))
    ;; Case 4
    (goto-char 2041)
    (should (= (markdown-beginning-of-list) 1919))
    (should (= (point) 1919))
    ;; Case 8
    (goto-char 3553)
    (should (= (markdown-beginning-of-list) 3096))
    (should (= (point) 3096))))

(ert-deftest test-markdown-lists/end-of-list ()
  "Test `markdown-end-of-list'."
  (markdown-test-file "lists.text"
    ;; Case 1: not in a list
    (goto-char 399)
    (should-not (markdown-end-of-list))
    (should (= (point) 399))
    ;; Case 2
    (goto-char 1281)
    (should (= (markdown-end-of-list) 1396))
    (should (= (point) 1396))
    (goto-char 1395)
    (should (= (markdown-end-of-list) 1396))
    (should (= (point) 1396))
    ;; Case 3
    (goto-char 1659)
    (should (= (markdown-end-of-list) 1849))
    (should (= (point) 1849))
    ;; Case 4
    (goto-char 2041)
    (should (= (markdown-end-of-list) 2092))
    (should (= (point) 2092))
    ;; Case 8
    (goto-char 3553)
    (should (= (markdown-end-of-list) 3614))
    (should (= (point) 3614))))

(ert-deftest test-markdown-lists/up-list ()
  "Test `markdown-up-list'."
  (markdown-test-file "nested-list.text"
    (goto-char 581)
    (should (= (markdown-up-list) 484))
    (should (= (point) 484))
    (should (= (markdown-up-list) 191))
    (should (= (point) 191))
    ;; Return nil upon failure, but move out of list.
    (should-not (markdown-up-list))
    (should (= (point) (point-min)))))

;;; Outline minor mode tests:

(ert-deftest test-markdown-outline/navigation ()
  "Test outline navigation functions."
  (markdown-test-file "outline.text"
   ;; Navigate to the first visible heading
   (markdown-next-visible-heading 1)
   (should (eq (point) 19))
   (should (looking-at "^# A top-level header"))
   ;; Navigate forward at the same level
   (markdown-forward-same-level 1)
   (should (eq (point) 351))
   (should (looking-at "^An underline-style header$"))
   ;; Navigate backward by four visible headings
   (markdown-previous-visible-heading 4)
   (should (eq (point) 69))
   (should (looking-at "^## A second-level header$"))
   ;; Navigate up the hierarchy (atx)
   (call-interactively #'markdown-up-heading)
   (should (looking-at "^# A top-level header"))
   (should (eq (mark) 69))
   ;; Navigate up the hierarchy (setext)
   (goto-char 516)
   (call-interactively #'markdown-up-heading)
   (should (looking-at "^An underline-style header$"))
   (should (eq (mark) 516))
   ;; Navigate back in the outline (setext to atx)
   (forward-line) ;; move to setext underline
   (markdown-backward-same-level 1)
   (should (looking-at "^# A top-level header"))))

(ert-deftest test-markdown-outline/navigation-with-code ()
  "Test outline navigation functions with code blocks."
  (markdown-test-file "outline-code.text"
   ;; Navigate forward at the same level
   (markdown-forward-same-level 1)
   (should (eq (point) 159))
   (should (looking-at "^# Level one again"))))

(ert-deftest test-markdown-outline/back-to-heading-over-code-block ()
  "Test `markdown-back-to-heading-over-code-block' over."
  (markdown-test-file "outline-code.text"
    ;; Initialize match data to known quantity.
    (set-match-data '(1 2 3 4))
    (should (equal (match-data t) '(1 2 3 4)))
    ;; Function should navigate back over code blocks.
    (re-search-forward "^# In a code block")
    (should (= (markdown-back-to-heading-over-code-block) 69))
    ;; Match data should be set for markdown-regex-header.
    (should (equal (match-data t) (get-text-property (point) 'markdown-heading)))
    ;; Function should return t when at a heading.
    (should (equal (markdown-back-to-heading-over-code-block) t))
    ;; Insert some text before the first heading.
    (goto-char (point-min))
    (save-excursion (insert "foo\n\n"))
    ;; Function should throw an error if no previous heading.
    (should-error (markdown-back-to-heading-over-code-block))
    ;; Function should return nil without error if NO-ERROR is non-nil.
    (should-not (markdown-back-to-heading-over-code-block t t))))

(ert-deftest test-markdown-outline/visibility-atx ()
  "Test outline visibility cycling for ATX-style headers."
  (markdown-test-file "outline.text"
   (let (last-command this-command)
     ;; Navigate to the second visible heading
     (markdown-next-visible-heading 2)
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
   (markdown-next-visible-heading 7)
   (markdown-previous-visible-heading 1)
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

(ert-deftest test-markdown-outline/visibility-with-code ()
  "Test outline visibility cycling with code blocks."
  (markdown-test-file "outline-code.text"
   (let (last-command this-command)
     ;; Cycle global visibility to "overview" mode
     (setq this-command 'markdown-cycle)
     (markdown-cycle t)
     (setq last-command 'markdown-cycle)
     (should (eq (point) (point-min)))
     (should (looking-at "^# Level one"))
     ;; Test that the code block is invisible
     (markdown-test-range-has-property 83 157 'invisible 'outline)
     ;; Check subsequent headings
     (outline-next-visible-heading 1)
     (should (eq (point) 69))
     (should (looking-at "^## Level two"))
     (outline-next-visible-heading 1)
     (should (eq (point) 159))
     (should (looking-at "^# Level one again")))))

(ert-deftest test-markdown-outline/visibility-with-metadata ()
  "Test outline visibility cycling with metadata blocks."
  (markdown-test-string
   "---
layout = post
date = 2015-08-13 11:35:25 EST
---
"
   (let (last-command this-command)
     ;; Cycle global visibility to "overview" mode
     (setq this-command 'markdown-cycle)
     (markdown-cycle t)
     ;; Check that text is visible
     (markdown-test-range-has-property (point-min) (point-max) 'invisible nil))))

(ert-deftest test-markdown-outline/level ()
  "Test `markdown-outline-level'."
  (markdown-test-file "outline.text"
    (markdown-next-heading)
    (should (= (markdown-outline-level) 1))
    (markdown-forward-same-level 1)
    (should (= (markdown-outline-level) 1))
    (markdown-next-heading)
    (should (= (markdown-outline-level) 2))
    (markdown-next-heading)
    (should (= (markdown-outline-level) 1))
    (markdown-next-heading)
    (should (= (markdown-outline-level) 2))))

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
   (should (= (point) (point-min)))
   ;; (beginning-of-defun -1)  should move to the start of the next header
   (forward-line 2)
   (beginning-of-defun -1)
   (should (looking-at "## A second-level header"))
   (beginning-of-defun -1)
   (should (looking-at "### Third level ###"))
   (beginning-of-defun -1)
   (should (looking-at "### Third level number two ###"))))

(ert-deftest test-markdown-movement/beginning-of-defun-at-point-max ()
  "Test beginning of defun navigation at point-max."
  (markdown-test-file "outline.text"
    (goto-char (point-max))
    (beginning-of-defun)))

(ert-deftest test-markdown-movement/text-block ()
  "Test plain text block movement."
  (markdown-test-file "outline.text"
   (markdown-end-of-text-block)
   (should (looking-at "\n# A top-level header"))
   (markdown-end-of-text-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-end-of-text-block)
   (should (looking-at "\n## A second-level header"))
   (markdown-end-of-text-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-end-of-text-block)
   (should (looking-at "\n### Third level ###"))
   (markdown-end-of-text-block)
   (should (looking-at "\n\\* A list item"))
   (markdown-end-of-text-block)
   (should (looking-at "\n### Third level number two ###"))
   (markdown-end-of-text-block)
   (should (looking-at "\n### Level two again"))
   (markdown-end-of-text-block)
   (should (looking-at "\nfollowed by some body text"))

   (markdown-test-goto-heading "Level two")
   (markdown-end-of-text-block)
   (should (looking-at "\nbar"))
   (markdown-end-of-text-block)
   (should (= (point) (point-max)))
   (markdown-beginning-of-text-block)
   (should (looking-at "bar"))
   (markdown-beginning-of-text-block)
   (should (looking-at "## Level two"))
   (markdown-beginning-of-text-block)
   (should (looking-at "foo"))
   (markdown-beginning-of-text-block)
   (should (looking-at "# Level one"))
   (markdown-beginning-of-text-block)
   (should (looking-at "* With"))
   (markdown-beginning-of-text-block)
   (should (looking-at "And a level two underline header"))

   (goto-char (point-min))
   (markdown-test-goto-heading "A top-level header")
   (beginning-of-line)
   (markdown-beginning-of-text-block)
   (should (= (point) (point-min)))))

(ert-deftest test-markdown-movement/mark-text-block ()
  "Test `markdown-mark-text-block'."
  (markdown-test-file "outline.text"
    ;; Start in middle of nested list with no separating whitespace.
    (goto-char 193)
    (markdown-mark-text-block)
    (should (= (point) 143))
    (should (= (mark) 269))))

(ert-deftest test-markdown-movement/paragraph ()
  "Test Markdown paragraph movement."
  (markdown-test-file "outline.text"
   (markdown-forward-paragraph)
   (should (looking-at "\n# A top-level header"))
   (markdown-forward-paragraph)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-forward-paragraph)
   (should (looking-at "\n## A second-level header"))
   (markdown-forward-paragraph)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-forward-paragraph)
   (should (looking-at "\n### Third level ###"))
   (markdown-forward-paragraph)
   (should (looking-at "\n\\* A list item"))
   (markdown-forward-paragraph)
   (should (looking-at "\\* and another"))
   (markdown-forward-paragraph)
   (should (looking-at "   \\+ and a sublist"))
   (markdown-forward-paragraph)
   (should (looking-at "- And a third"))
   (markdown-forward-paragraph)
   (should (looking-at "\n### Third level number two ###"))
   (markdown-forward-paragraph)
   (should (looking-at "\n### Level two again"))
   (markdown-forward-paragraph)
   (should (looking-at "\nfollowed by some body text"))

   (markdown-test-goto-heading "Level two")
   (markdown-forward-paragraph)
   (should (looking-at "\nbar"))
   (markdown-forward-paragraph)
   (should (= (point) (point-max)))
   (markdown-backward-paragraph)
   (should (looking-at "bar"))
   (markdown-backward-paragraph)
   (should (looking-at "## Level two"))
   (markdown-backward-paragraph)
   (should (looking-at "foo"))
   (markdown-backward-paragraph)
   (should (looking-at "# Level one"))
   (markdown-backward-paragraph)
   (should (looking-at "\\* List"))
   (markdown-backward-paragraph)
   (should (looking-at "\\* an unordered"))
   (markdown-backward-paragraph)
   (should (looking-at "\\* With"))
   (markdown-backward-paragraph)
   (should (looking-at "And a level two underline header"))

   (goto-char (point-min))
   (markdown-test-goto-heading "A top-level header")
   (beginning-of-line)
   (markdown-backward-paragraph)
   (should (= (point) (point-min)))))

(ert-deftest test-markdown-movement/forward-paragraph-with-whitespace ()
  "Test Markdown paragraph movement."
  (markdown-test-file "blocks.md"
    (markdown-test-goto-heading "With Whitespace")
    (dolist (pos '(58 67 78 94 109 114 123 131 135 147 157 170 184 199))
      (markdown-forward-paragraph)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/backward-paragraph-with-whitespace ()
  "Test Markdown paragraph movement."
  (markdown-test-file "blocks.md"
    (markdown-test-goto-heading "With Whitespace")
    (markdown-next-heading)
    (should (= (point) 200))
    (dolist (pos '(185 172 158 148 136 132 124 115 110 94 78 67 59))
      (markdown-backward-paragraph)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/forward-paragraph-without-whitespace ()
  "Test Markdown paragraph movement."
  (markdown-test-file "blocks.md"
    (markdown-test-goto-heading "Without Whitespace")
    (dolist (pos '(222 230 240 255 270 275 283 291 294 305 314 326 340 354))
      (markdown-forward-paragraph)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/backward-paragraph-without-whitespace ()
  "Test Markdown paragraph movement."
  (markdown-test-file "blocks.md"
    (goto-char (point-max))
    (dolist (pos '(340 328 314 305 294 291 284 275 271 255 240 230 223 200))
      (markdown-backward-paragraph)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/block ()
  "Test Markdown block movement."
  (markdown-test-file "outline.text"
   (markdown-forward-block)
   (should (looking-at "\n# A top-level header"))
   (markdown-forward-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-forward-block)
   (should (looking-at "\n## A second-level header"))
   (markdown-forward-block)
   (should (looking-at "\nfollowed by some body text"))
   (markdown-forward-block)
   (should (looking-at "\n### Third level ###"))
   (markdown-forward-block)
   (should (looking-at "\n\\* A list item"))
   (markdown-forward-block)
   (should (looking-at "\n### Third level number two ###"))
   (markdown-forward-block)
   (should (looking-at "\n### Level two again"))
   (markdown-forward-block)
   (should (looking-at "\nfollowed by some body text"))

   (markdown-test-goto-heading "Level two")
   (markdown-forward-block)
   (should (looking-at "\nbar"))
   (markdown-forward-block)
   (should (= (point) (point-max)))
   (markdown-backward-block)
   (should (looking-at "bar"))
   (markdown-backward-block)
   (should (looking-at "## Level two"))
   (markdown-backward-block)
   (should (looking-at "foo"))
   (markdown-backward-block)
   (should (looking-at "# Level one"))
   (markdown-backward-block)
   (should (looking-at "\\* With"))
   (markdown-backward-block)
   (should (looking-at "And a level two underline header"))

   (goto-char (point-min))
   (markdown-test-goto-heading "A top-level header")
   (beginning-of-line)
   (markdown-backward-block)
   (should (= (point) (point-min)))))

(ert-deftest test-markdown-movement/forward-block-with-whitespace ()
  "Test Markdown block movement."
  (markdown-test-file "blocks.md"
    (markdown-test-goto-heading "With Whitespace")
    (dolist (pos '(58 109 114 131 135 147 157 184 199))
      (markdown-forward-block)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/backward-block-with-whitespace ()
  "Test Markdown block movement."
  (markdown-test-file "blocks.md"
    (markdown-test-goto-heading "With Whitespace")
    (markdown-next-heading)
    (dolist (pos '(185 158 148 136 132 115 110 59))
      (markdown-backward-block)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/forward-block-without-whitespace ()
  "Test Markdown block movement."
  (markdown-test-file "blocks.md"
    (markdown-test-goto-heading "Without Whitespace")
    (dolist (pos '(222 270 275 291 294 305 314 340 354))
      (markdown-forward-block)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/backward-block-without-whitespace ()
  "Test Markdown block movement."
  (markdown-test-file "blocks.md"
    (goto-char (point-max))
    (dolist (pos '(340 314 305 294 291 275 271 223 200))
      (markdown-backward-block)
      (should (= (point) pos)))))

(ert-deftest test-markdown-movement/page ()
  "Test Markdown page movement."
  (markdown-test-file "outline.text"
   (markdown-forward-page)
   (should (looking-at "# A top-level header"))
   (markdown-forward-page)
   (should (looking-at "An underline-style header"))
   (markdown-forward-page)
   (should (looking-at "# Level one"))
   (markdown-forward-page)
   (should (eobp))
   (markdown-backward-page)
   (should (looking-at "# Level one"))
   (markdown-backward-page)
   (should (looking-at "An underline-style header"))
   (markdown-backward-page)
   (should (looking-at "# A top-level header"))
   (markdown-backward-page)
   (should (bobp))))

(ert-deftest test-markdown-movement/blockquote-paragraphs ()
  "Test filling of blockquotes containing multiple paragraphs."
  (markdown-test-string "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n>\n> Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n"
    (forward-paragraph)
    (should (looking-at "^>$"))
    (should (= (point) 128))
    (forward-paragraph)
    (should (= (point) (point-max)))))

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
   (should (string-equal (buffer-string) "[a][]\n\n[a]: \n"))))

(ert-deftest test-markdown-movement/back-to-same-level-over-code-block ()
  "`markdown-backward-same-level' over code block which contains header
like statement. Detail: https://github.com/jrblevin/markdown-mode/issues/75"
  (markdown-test-string "
## Header 2-1

## Header 2-2

```R
# Header Like Statement
```

## Header 2-3
"
    (search-forward "## Header 2-3")
    (let ((last-header-pos (point)))
      (forward-line -1)
      (call-interactively #'markdown-backward-same-level)
      (should (looking-at-p "## Header 2-1"))

      (goto-char last-header-pos)
      (call-interactively #'markdown-backward-same-level)
      (should (looking-at-p "## Header 2-2"))

      (goto-char last-header-pos)
      (markdown-backward-same-level 2)
      (should (looking-at-p "## Header 2-1"))

      (search-forward "# Header Like Statement")
      (call-interactively #'markdown-backward-same-level)
      (should (looking-at-p "## Header 2-1")))))

;;; Link tests:

(ert-deftest test-markdown-link/follow ()
  "Test link following in a browser and in Emacs."
  (markdown-test-string "[text](http://path?query=foo#id)"
    (let* ((opened-url nil)
           (browse-url-browser-function
            (lambda (url &rest args) (setq opened-url url))))
      (markdown-follow-thing-at-point nil)
      (should (equal opened-url "http://path?query=foo#id"))))
  (when (featurep 'url-parse)
    (markdown-test-string "[text](path?query=foo#id)"
      (markdown-follow-thing-at-point nil)
      (should (equal (file-name-nondirectory (buffer-file-name)) "path"))
      (kill-buffer))))

(ert-deftest test-markdown-link/inline-link-at-pos ()
  "Test `markdown-link-at-pos' return values with an inline link."
  (markdown-test-string "[text](url \"title\")"
    (should (equal (markdown-link-at-pos (point)) '(1 20 "text" "url" nil "title" nil)))))

(ert-deftest test-markdown-link/inline-image-at-pos ()
  "Test `markdown-link-at-pos' return values with an inline image."
  (markdown-test-string "![text](url \"title\")"
    (should (equal (markdown-link-at-pos (point)) '(1 21 "text" "url" nil "title" "!")))))

(ert-deftest test-markdown-link/reference-link-at-pos ()
  "Test `markdown-link-at-pos' return values with a reference link."
  (markdown-test-string "[text][ref]"
    (should (equal (markdown-link-at-pos (point)) '(1 12 "text" nil "ref" nil nil)))))

(ert-deftest test-markdown-link/reference-image-at-pos ()
  "Test `markdown-link-at-pos' return values with a reference image."
  (markdown-test-string "![text][ref]"
    (should (equal (markdown-link-at-pos (point)) '(1 13 "text" nil "ref" nil "!")))))

(ert-deftest test-markdown-link/angle-uri-at-pos ()
  "Test `markdown-link-at-pos' return values with an angle bracket inline link."
  (markdown-test-string "<http://jblevins.org/projects/markdown-mode/>"
    (should (equal (markdown-link-at-pos (point)) '(1 46 nil "http://jblevins.org/projects/markdown-mode/" nil nil nil)))))

(ert-deftest test-markdown-link/plain-uri-at-pos ()
  "Test `markdown-link-at-pos' return values with a plain URI."
  (markdown-test-string "http://jblevins.org/projects/markdown-mode/"
    (should (equal (markdown-link-at-pos (point)) '(1 44 nil "http://jblevins.org/projects/markdown-mode/" nil nil nil)))))

(ert-deftest test-markdown-link/follow-filename ()
  "Test that `markdown-follow-thing-at-pos' uses
`markdown-translate-filename-function' to translate filenames."
  (markdown-test-string "[text](/foo/bar/baz)"
    (cl-letf* ((visited-files ())
               ((symbol-function #'find-file)
                (lambda (filename)
                  (push filename visited-files)))
               (translated-files ())
               (markdown-translate-filename-function
                (lambda (filename)
                  (push filename translated-files)
                  (format "/root%s.md" filename))))
      (markdown-follow-thing-at-point nil)
      (should (equal translated-files '("/foo/bar/baz")))
      (should (equal visited-files '("/root/foo/bar/baz.md"))))))

;;; Wiki link tests:

(ert-deftest test-markdown-wiki-link/file-local-variables ()
  "Test enabling wiki links via file-local variables."
  (markdown-test-file "wiki-links.text"
   (should-not markdown-enable-wiki-links)
   (hack-local-variables)
   (should markdown-enable-wiki-links)))

(ert-deftest test-markdown-wiki-link/aliasing ()
  "Test filename extraction for aliased wiki links."
  (let ((markdown-enable-wiki-links t))
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
      (should (string-equal (markdown-wiki-link-link) "first")))))

(ert-deftest test-markdown-wiki-link/navigation ()
  "Test wiki link navigation."
  (let ((markdown-enable-wiki-links t))
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
      (should (eq (point) 8)))))

(ert-deftest test-markdown-wiki-link/font-lock ()
  "Test font lock faces for wiki links."
  ;; If `temporary-file-directory' contains an inaccessible
  ;; subdirectory, `markdown-fontify-buffer-wiki-links' fails because
  ;; it calls `directory-files-recursively' on the directory, which
  ;; fails because of
  ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28567>.  To fix
  ;; this, we run the entire test in a new subdirectory of
  ;; `temporary-file-directory', which is guaranteed to not contain
  ;; any inaccessible directories.
  (let ((temporary-file-directory
         (file-name-as-directory (make-temp-file "markdown-test" :dir-flag))))
    (markdown-test-temp-file "wiki-links.text"
      (let* ((fn (concat (file-name-directory buffer-file-name)
                         "inline.text"))
             (markdown-enable-wiki-links t))
        ;; Create inline.text in the same temp directory, refontify
        (write-region "" nil fn nil 1)
        (markdown-fontify-buffer-wiki-links)
        ;; Confirm location of first wiki link
        (should (eq (markdown-next-link) 8))
        ;; First wiki link doesn't have a corresponding file
        (markdown-test-range-has-property 8 20 'font-lock-face 'markdown-missing-link-face)
        ;; Second wiki link doesn't have a corresponding file
        (should (eq (markdown-next-link) 73))
        (markdown-test-range-has-property 73 88 'font-lock-face 'markdown-missing-link-face)
        ;; Move to third wiki link, and create the missing file
        (should (eq (markdown-next-link) 155))
        (should (string-equal (markdown-wiki-link-link) "inline"))
        (markdown-test-range-has-property 155 164 'font-lock-face 'markdown-link-face)
        ;; Check wiki links in code blocks
        (markdown-test-range-has-face 360 395 'markdown-pre-face)
        ;; Remove temporary files
        (delete-file fn)))
    (delete-directory temporary-file-directory)))

(ert-deftest test-markdown-wiki-link/kill ()
  "Simple tests for `markdown-kill-thing-at-point' for wiki links."
  (let ((kill-ring nil)
        (markdown-enable-wiki-links t)
        (tests (list '("[[foo]]" . "foo")
                     '("[[foo|bar]]" . "bar"))))
    (dolist (test tests)
      ;; Load test string (the car), move to end of first line, kill
      ;; thing at point, and then verify that the kill ring contains cdr.
      (markdown-test-string (car test)
                            (end-of-line)
                            (call-interactively 'markdown-kill-thing-at-point)
                            (should (string-equal (current-kill 0) (cdr test)))))))

;;; Filling tests:

(ert-deftest test-markdown-filling/blockquote ()
  "Test filling of blockquotes.
See `adaptive-fill-first-line-regexp'."
  (markdown-test-string "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\n> eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/blockquote-paragraphs ()
  "Test filling of blockquotes containing multiple paragraphs."
  (markdown-test-string "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n>\n> Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n"
   (forward-paragraph)
   (fill-paragraph)
   (should (string-equal (buffer-string) "> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n>\n> Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris\n> nisi ut aliquip ex ea commodo consequat.\n"))))

(ert-deftest test-markdown-filling/leanpub-block ()
  "Test adaptive filling of Leanpub blocks."
  (markdown-test-string "A> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
   (fill-paragraph)
   (should (string-equal (buffer-string) "A> Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do\nA> eiusmod tempor incididunt ut labore et dolore magna aliqua."))))

(ert-deftest test-markdown-filling/space-after-list-marker ()
  "`fill-paragraph' should preserve more than one space after a list marker,
since users may wish to indent their lists more than one space more than the
width of the marker. The examples on the Markdown Syntax page have three
spaces after the list marker for a total indentation of four."
  (let ((str "\n\n*   List item indented four spaces.\n*   Also four spaces."))
   (markdown-test-string str
    (forward-line 2)
    (fill-paragraph)
    (should (string-equal (buffer-string) str)))))

(ert-deftest test-markdown-filling/multi-line-list-with-more-space ()
  "`fill-paragraph' should preserve more than one space after a list marker
(see `test-preserve-space-after-list-marker')."
  (let ((str "*   This list item is continued on\n    the next line"))
   (markdown-test-string str
    ;; The first line is exactly 35 columns
    (let ((fill-column 35))
      (fill-paragraph)
      (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/definition-list-add-leading-spaces ()
  "`fill-paragraph' should adapt to spaces after list marker."
  (markdown-test-string
   ":   This list item is continued on the next line"
   (let ((fill-column 35))
     (fill-paragraph)
     (should (string-equal
              (buffer-string)
              ":   This list item is continued on\n    the next line")))))

(ert-deftest test-markdown-filling/definition-list-preserve-leading-spaces ()
  "`fill-paragraph' should preserve spaces after list marker."
  (let ((str ":   This list item is continued on\n    the next line")
        (fill-column 35))
    (markdown-test-string
     str (fill-paragraph)
     (should (string-equal (buffer-string) str)))))

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

(ert-deftest test-markdown-filling/avoid-unintended-list-item ()
  "Avoid breaking lines where it would result in an unintended list item."
  (markdown-test-string "Lorem ipsum dolor sit 4. amet"
   (let ((fill-column 22))
     (fill-paragraph)
     (should (string-equal (buffer-string) "Lorem ipsum dolor\nsit 4. amet")))))

(ert-deftest test-markdown-filling/no-break-link-reference ()
  "Shouldn't break line between label and url, or combine two link references."
  (let ((str "[label1]: http://long-url.example.com\n[label2]: http://another-long-url.example.com/"))
    (markdown-test-string str
     (let ((fill-column 15)) ; after end of label, before end of URL
       (fill-paragraph)
       (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/no-break-before-list-item ()
  "There's no point in putting the first item of a list on the next line,
indented the same amount."
  (let ((str "*   [Link](http://way-too-long.example.com)\n"))
    (markdown-test-string str
      (auto-fill-mode 1)
      (let ((fill-column 10))
        (end-of-line)
        (funcall auto-fill-function)
        ;; This test was known to fail in Emacs 25 and earlier.
        (if (version< emacs-version "26.0")
            (should-not (string-equal (buffer-string) str))
          (should (string-equal (buffer-string) str)))))))

(ert-deftest test-markdown-filling/break-within-list-item ()
  "This doesn't suppress auto-fill within a multi-word list item."
  :expected-result :failed
  (markdown-test-string "*   [Link](http://example.com/) more text"
    (auto-fill-mode 1)
    (let ((fill-column 10))
      (end-of-line)
      (funcall auto-fill-function)
      (should (string-equal
               (buffer-string)
               "*   [Link](http://example.com/)\n    more text")))))

(ert-deftest test-markdown-filling/preserve-next-line-footnote ()
  "Footnote block can be after label"
  (let ((str "[^label1]:\n    Footnote block\n    more footnote")) ; six spaces
    (markdown-test-string str
     (let ((fill-column 20)) ; could fit "footnote" after label, but shouldn't
       (fill-paragraph)
       (should (string-equal (buffer-string) str))))))

(ert-deftest test-markdown-filling/wrap-same-line-footnote ()
  "Additional lines must be indented one level (four spaces) when wrapped."
  (markdown-test-string "[^label]: Long line should be wrapped"
     (let ((fill-column 25)) ; wrap before end of "should"
       (fill-paragraph)
       (should (string-equal (buffer-string) "[^label]: Long line\n    should be wrapped")))))

(ert-deftest test-markdown-filling/wrap-extra-hanging-indentation ()
  "Additional lines must be indented one level (four spaces) when wrapped."
  (markdown-test-string "[^label]: Long line\n      should be wrapped"
     (let ((fill-column 25)) ; wrap before end of "should"
       (fill-paragraph)
       (should (string-equal (buffer-string) "[^label]: Long line\n      should be wrapped")))))

(ert-deftest test-markdown-filling/full-justification ()
  "Test paragraph detection with lines with lots of whitespace."
  (markdown-test-string "Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Dolor Sit Amet Consectetur http://very-long-url.lorem.ipsum.sic.dolor.sit.amet.com"
     (setq default-justification 'full)
     (fill-paragraph)
     (should (string-equal (buffer-string) "Lorem  Ipsum Lorem  Ipsum Lorem  Ipsum Lorem  Ipsum Lorem  Ipsum Lorem\nDolor                Sit               Amet                Consectetur\nhttp://very-long-url.lorem.ipsum.sic.dolor.sit.amet.com"))
     (backward-paragraph)
     (forward-paragraph)
     (should (= (point) 198))))

(ert-deftest test-markdown-filling/list-line ()
  "Test fill-paragraph for list line. Don't insert bullet automatically.
Detail: https://github.com/jrblevin/markdown-mode/issues/79"
  (markdown-test-string "* foo foo *foo* foo foo foo foo foo foo"
    (let ((fill-column 10))
      (fill-paragraph)
      (fill-paragraph)
      (forward-line 2)
      (back-to-indentation)
      (should-not (looking-at-p "\\*foo"))
      (forward-line 1)
      (back-to-indentation)
      (should-not (looking-at-p "\\*foo")))))

(ert-deftest test-markdown-filling/ignore-header ()
  "# Test fill-paragraph for containing header line paragraph.
https://github.com/jrblevin/markdown-mode/issues/159"
  (markdown-test-string "# this is header line
this is not header line
"
    (let ((fill-column 10))
      (fill-paragraph)
      (should (string= (buffer-substring (point) (line-end-position)) "# this is header line")))))

(ert-deftest test-markdown-filling/unclosed-square-bracket ()
  "Test fill-paragraph following an unclosed square bracket."
  (markdown-test-string "```\n[3\n```\n\naaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbb"
    (let ((fill-column 20))
      (forward-line 4)
      (fill-paragraph)
      (should (looking-at "aaaaaaaaaaaaaaaa\nbbbbbbbbbbbbbbbb")))))

(ert-deftest test-markdown-filling/skip-code-blocks ()
  "Test `markdown-fill-paragraph' on code blocks."
  (let ((text "test\n\n```\nhello\nworld\n```"))
    (markdown-test-string text
      (dotimes (n 5)
        ;; Fill at each line; buffer should not change.
        (fill-paragraph)
        (should (string-equal (buffer-string) text))))))

(ert-deftest test-markdown-filling/fill-region-skip-code-blocks ()
  "Test `fill-region' on code blocks."
  (let ((text "testing\n\n```\nhello\nworld\n```\n\n123"))
    (markdown-test-string text
      ;; Fill entire buffer; buffer should not change.
      (fill-region (point-min) (point-max))
      (should (string-equal (buffer-string) text)))))

(ert-deftest test-markdown-filling/fill-region-skip-code-blocks-2 ()
  "Test `fill-region' on a buffer with a code block with long paragraphs."
  (markdown-test-string "long unwrapped paragraph 1

```
code
block

foo
bar
```

long unwrapped paragraph 2"
    ;; Test markdown-fill-forward-paragraph movement.
    (should (= (markdown-fill-forward-paragraph 1) 0))
    (should (= (point) 28)) ;; Point just after par. 1.
    (should (= (markdown-fill-forward-paragraph 1) 0))
    (should (= (point) 84)) ;; Point at end of par. 2.
    ;; Test filling the entire buffer with `fill-region'.
    (let ((fill-column 12))
      (fill-region (point-min) (point-max))
      (should (string-equal (buffer-string)
                            "long
unwrapped
paragraph 1

```
code
block

foo
bar
```

long
unwrapped
paragraph 2")))))

(ert-deftest test-markdown-filling/fill-region-skip-code-blocks-3 ()
  "Test `fill-region' on a lone code block with no surrounding text."
  (let ((text "```\ncode\nblock\n```\n"))
    (markdown-test-string text
      ;; Fill entire buffer; buffer should not change.
      (fill-region (point-min) (point-max))
      (should (string-equal (buffer-string) text)))))

(ert-deftest test-markdown-filling/long-paragraph-with-link ()
  "Test `fill-paragraph' on a long paragraph with a long link.
See GH-173."
  (markdown-test-string
   "aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa [aaa aaa aaa aaa](aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) aaa aaa aaa aaa aaa."
   (let ((fill-column 79)) (fill-paragraph))
   (should (string-equal (buffer-string) "aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa
aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa aaa [aaa aaa aaa
aaa](aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) aaa aaa aaa aaa aaa."))))

(ert-deftest test-markdown-filling/pandoc-line-blocks ()
  "Filling should leave Pandoc line blocks undisturbed.
This includes preserving whitespace after the pipe."
  (let ((text "| The limerick packs laughs anatomical
| In space that is quite economical.
|    But the good ones I've seen
|    So seldom are clean
| And the clean ones so seldom are comical

| 200 Main St.
| Berkeley, CA 94718"))
    (markdown-test-string text
      (fill-region (point-min) (point-max))
      (should (string-equal (buffer-string) text)))))

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

(ert-deftest test-markdown-export/kill-buffer-nil ()
  "Test `markdown-export-kill-buffer' equal to nil."
  (markdown-test-temp-file "inline.text"
    (let* ((markdown-export-kill-buffer nil)
           (markdown-command #'markdown-command-identity)
           (export-file (markdown-export))
           (export-buffer (get-file-buffer export-file)))
      ;; Output buffer should remain open.
      (should (member export-buffer (buffer-list))))))

(ert-deftest test-markdown-export/kill-buffer-t ()
  "Test `markdown-export-kill-buffer' equal to t."
  (markdown-test-temp-file "inline.text"
    (let* ((markdown-export-kill-buffer t)
           (markdown-command #'markdown-command-identity)
           (export-file (markdown-export))
           (export-buffer (get-file-buffer export-file)))
      ;; Output buffer should be killed.
      (should-not export-buffer))))

(ert-deftest test-markdown-export/body-preamble ()
  "Test `markdown-xhtml-body-preamble'."
  (markdown-test-temp-file "inline.text"
    (let* ((str "<!-- body preamble test -->")
           (markdown-xhtml-body-preamble str)
           (markdown-command #'markdown-command-identity)
           (markdown-export-kill-buffer nil)
           (ofile (markdown-export))
           (obuffer (get-file-buffer ofile)))
      (with-current-buffer obuffer
        (goto-char (point-min))
        (should (search-forward str)))
      (kill-buffer obuffer)
      (delete-file ofile))))

(ert-deftest test-markdown-export/body-epilogue ()
  "Test `markdown-xhtml-body-epilogue'."
  (markdown-test-temp-file "inline.text"
    (let* ((str "<!-- body epilogue test -->")
           (markdown-xhtml-body-epilogue str)
           (markdown-command #'markdown-command-identity)
           (markdown-export-kill-buffer nil)
           (ofile (markdown-export))
           (obuffer (get-file-buffer ofile)))
      (with-current-buffer obuffer
        (goto-char (point-min))
        (should (search-forward str)))
      (kill-buffer obuffer)
      (delete-file ofile))))

;;; Hook tests:

(ert-deftest test-markdown-hook/before-export ()
  "Test hook run before export XHTML."
  (markdown-test-temp-file "lists.text"
   (let* ((before-hook-run nil)
          (orig-point (point))
          (markdown-command #'markdown-command-identity)
          (func (lambda ()
                  ;; Change value of a variable
                  (setq before-hook-run t)
                  ;; Insert some text
                  (goto-char (point-min))
                  (insert "#")
                  ;; Deliberately move the point
                  (end-of-line)
                  ;; Verify changes
                  (should (looking-back "^## List Cases" nil))
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
          (markdown-command #'markdown-command-identity)
          (markdown-export-kill-buffer nil)
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
  "Test enabling math mode via variable `markdown-enable-math'."
  (let ((markdown-enable-math t))
    (markdown-test-file "math.text"
      ;; Flag should be set to t
      (should markdown-enable-math)
      ;; Font-lock keywords should be updated.
      (should (member (car markdown-mode-font-lock-keywords-math)
                      (cadr font-lock-keywords))))))

(ert-deftest test-markdown-math/preserve-user-keywords ()
  "Test preserving user-specified font-lock keywords."
  (let ((user-keyword '("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
    ;; Add user font-lock keyword using `font-lock-add-keywords'.
    (font-lock-add-keywords 'markdown-mode (list user-keyword))
    ;; Visit a file using `markdown-mode'.
    (markdown-test-file "math.text"
      ;; User keyword should be present initially.
      (should (member user-keyword (cadr font-lock-keywords)))
      ;; User keyword should persist after calling `markdown-reload-extensions'.
      (markdown-reload-extensions)
      (should (member user-keyword (cadr font-lock-keywords))))
    ;; Remove the user keyword using `font-lock-remove-keywords'.
    (font-lock-remove-keywords 'markdown-mode (list user-keyword))
    ;; Visit a file using `markdown-mode'.
    (markdown-test-file "inline.text"
      ;; User keyword should not be present after removal.
      (should-not (member user-keyword (cadr font-lock-keywords))))))

(ert-deftest test-markdown-math/preserve-local-user-keywords ()
  "Test preserving buffer-specific user-specified font-lock keywords."
  (let ((user-keyword '("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
    ;; Visit a file using `markdown-mode'.
    (markdown-test-file "math.text"
      ;; Add user font-lock keyword using `font-lock-add-keywords'.
      (font-lock-add-keywords nil (list user-keyword))
      ;; User keyword should be present initially.
      (should (member user-keyword (cadr font-lock-keywords)))
      ;; User keyword should persist after calling `markdown-reload-extensions'.
      (markdown-reload-extensions)
      (should (member user-keyword (cadr font-lock-keywords)))
      ;; Remove the user keyword using `font-lock-remove-keywords'.
      (font-lock-remove-keywords nil (list user-keyword))
      ;; User keyword should not be present after removal.
      (should-not (member user-keyword (cadr font-lock-keywords))))))

(ert-deftest test-markdown-math/font-lock ()
  "Test markdown math mode."
  (let ((markdown-enable-math t))
    (markdown-test-file "math.text"
      (markdown-test-range-has-face 1 32 nil)
      (markdown-test-range-has-face 33 33 'markdown-markup-face)
      (markdown-test-range-has-face 34 45 'markdown-math-face)
      (markdown-test-range-has-face 46 46 'markdown-markup-face)
      (markdown-test-range-has-face 47 49 nil)
      (markdown-test-range-has-face 50 51 'markdown-markup-face)
      (markdown-test-range-has-face 52 63 'markdown-math-face)
      (markdown-test-range-has-face 64 65 'markdown-markup-face)
      (markdown-test-range-has-face 66 98 nil)
      (markdown-test-range-has-face 99 100 'markdown-markup-face)
      (markdown-test-range-has-face 101 112 'markdown-math-face)
      (markdown-test-range-has-face 113 114 'markdown-markup-face)
      (markdown-test-range-has-face 113 114 'markdown-markup-face)
      (markdown-test-range-has-face 117 117 'markdown-header-delimiter-face)
      (markdown-test-range-has-face 119 152 'markdown-header-face-1)
      (markdown-test-range-has-face 129 129 'markdown-markup-face)
      (markdown-test-range-has-face 136 136 'markdown-markup-face)

      (markdown-test-range-has-face 174 177 'markdown-markup-face)
      (markdown-test-range-has-face 179 179 'markdown-markup-face)
      (markdown-test-range-has-face 180 187 'markdown-language-keyword-face)
      (markdown-test-range-has-face 188 188 'markdown-markup-face)
      (markdown-test-range-has-face 190 210 'markdown-pre-face)
      (markdown-test-range-has-face 212 215 'markdown-markup-face)

      (markdown-test-range-has-face 218 218 'markdown-markup-face)
      (markdown-test-range-has-face 219 223 'markdown-math-face)
      (markdown-test-range-has-face 224 224 'markdown-markup-face)
      (markdown-test-range-has-face 350 351 'markdown-markup-face)
      (markdown-test-range-has-face 352 356 'markdown-math-face)
      (markdown-test-range-has-face 357 358 'markdown-markup-face)
      (markdown-test-range-has-face 359 391 nil)
      (markdown-test-range-has-face 392 393 'markdown-markup-face)
      (markdown-test-range-has-face 394 398 'markdown-math-face)
      (markdown-test-range-has-face 399 400 'markdown-markup-face))))

(ert-deftest test-markdown-math/double-slash-display-math ()
  "Test double slash display math font lock."
  (let ((markdown-enable-math t))
    (markdown-test-file "math.text"
      (markdown-test-range-has-face 403 474 nil)
      (markdown-test-range-has-face 475 477 'markdown-markup-face)
      (markdown-test-range-has-face 478 543 'markdown-math-face)
      (markdown-test-range-has-face 544 546 'markdown-markup-face))))

(ert-deftest test-markdown-math/indented-double-slash-display-math ()
  "Test font lock for indented double slash display math.
See GH-288."
  (let ((markdown-enable-math t))
    (markdown-test-string "- Einstein's equation:

    \\\\[ E = m c^2 \\\\]"
      (markdown-test-range-has-face 29 31 'markdown-markup-face)
      (markdown-test-range-has-face 32 42 'markdown-math-face)
      (markdown-test-range-has-face 43 45 'markdown-markup-face))))

(ert-deftest test-markdown-math/font-lock-italics ()
  "Test markdown math mode with underscores."
  (let ((markdown-enable-math t))
    (markdown-test-file "math.text"
      (markdown-test-range-has-face 227 227 'markdown-markup-face)
      (markdown-test-range-has-face 228 233 'markdown-math-face)
      (markdown-test-range-has-face 234 234 'markdown-markup-face)
      (markdown-test-range-has-face 235 270 nil)
      (markdown-test-range-has-face 271 271 'markdown-markup-face)
      (markdown-test-range-has-face 272 274 'markdown-math-face)
      (markdown-test-range-has-face 275 275 'markdown-markup-face))))

(ert-deftest test-markdown-math/font-lock-no-bold ()
  "Bold markers in math should not trigger bold."
  (let ((markdown-enable-math t))
    (markdown-test-file "math.text"
      (markdown-test-range-has-face 279 299 'markdown-math-face)
      (markdown-test-range-has-face 301 308 nil)
      (markdown-test-range-has-face 310 312 'markdown-math-face))))

;;; Extension: pipe table editing

(ert-deftest test-markdown-table/table-begin-top-of-file ()
  "Test beginning of table detection at top of file."
  (markdown-test-string "\n| 1 | 2 |\n"
    (should-not (markdown-table-at-point-p))
    (forward-line)
    (should (markdown-table-at-point-p))
    (should (= (markdown-table-begin) 2))))


;;; gfm-mode tests:

(ert-deftest test-markdown-gfm/pre-1 ()
  "GFM pre block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2626 2637 nil)
    (markdown-test-range-has-face 2639 2641 'markdown-markup-face)
    (markdown-test-range-has-face 2642 2645 'markdown-language-keyword-face)
    (markdown-test-range-has-face 2647 2728 'markdown-pre-face)
    (markdown-test-range-has-face 2730 2732 'markdown-markup-face)))

(ert-deftest test-markdown-gfm/italic-1 ()
  "GFM italic font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 1483 1483 'markdown-markup-face)
    (markdown-test-range-has-face 1484 1487 'markdown-italic-face)
    (markdown-test-range-has-face 1488 1488 'markdown-markup-face)
    (markdown-test-range-has-face 1729 1790 nil)))

(ert-deftest test-markdown-gfm/strike-through-1 ()
  "GFM strike through font lock test."
  (markdown-test-string-gfm "one ~~two~~ three"
    (markdown-test-range-has-face 1 4 nil)
    (markdown-test-range-has-face 5 6 'markdown-markup-face)
    (markdown-test-range-has-face 7 9 'markdown-strike-through-face)
    (markdown-test-range-has-face 10 11 'markdown-markup-face)
    (markdown-test-range-has-face 12 17 nil)))

(ert-deftest test-markdown-gfm/toggle-strike-through ()
  "Test toggling functionality of `markdown-insert-strike-through'."
  (markdown-test-string-gfm "one ~~two~~ three"
   (forward-word 2)
   (markdown-insert-strike-through)
   (should (string-equal (buffer-string) "one two three"))
   (should (= (point) 8))
   (forward-word)
   (markdown-insert-strike-through)
   (should (= (point) 16))
   (should (string-equal (buffer-string) "one two ~~three~~"))))

(ert-deftest test-markdown-gfm/insert-code-block-empty-markup ()
  "Test GFM code block insertion with empty code section."
  (markdown-test-string-gfm "line 1\nline 2\n"
   (end-of-line)
   (markdown-insert-gfm-code-block "elisp")
   (should (equal (car markdown-gfm-used-languages) "elisp"))
   (should (equal (car (markdown-gfm-get-corpus)) "elisp"))
   (should (string-equal (buffer-string)
                         "line 1\n\n``` elisp\n\n```\n\nline 2\n"))))

(ert-deftest test-markdown-gfm/markdown-spaces-after-code-fence ()
  "Test `markdown-spaces-after-code-fence'."
  (markdown-test-string-gfm ""
    (let ((markdown-spaces-after-code-fence 0))
      (markdown-insert-gfm-code-block "elisp")
      (should (equal (buffer-string) "```elisp\n\n```")))))

(ert-deftest test-markdown-gfm/insert-code-block-active-region ()
  "Test GFM code block insertion with active region."
  (markdown-test-string-gfm "line 1\nline 2\nline 3\n"
   (forward-line)
   (transient-mark-mode)
   (push-mark (point) t t)
   (end-of-line)
   (should (markdown-use-region-p))
   (markdown-insert-gfm-code-block "elisp")
   (should (string-equal (buffer-string)
                         "line 1\n\n``` elisp\nline 2\n```\n\nline 3\n"))))

(ert-deftest test-markdown-gfm/insert-code-block-indented-list-item ()
  "Test GFM code block insertion with an indented list item."
  (markdown-test-string-gfm "1. foo\n   "
    (goto-char (point-max))
    (markdown-insert-gfm-code-block "elisp")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "1. foo\n\n   ``` elisp\n   \n   ```"))
    (should (equal (buffer-substring-no-properties (point) (point-max))
                   "\n   ```"))))

(ert-deftest test-markdown-gfm/insert-code-block-indented-list-item-active-region ()
  "Test GFM code block insertion with an indented list item and active region."
  (markdown-test-string-gfm "1.  foo\n    bar\n"
    (let ((transient-mark-mode t))
      (forward-line)
      (push-mark nil :nomsg :activate)
      (end-of-line)
      (should (markdown-use-region-p))
      (markdown-insert-gfm-code-block "elisp"))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "1.  foo\n\n    ``` elisp\n    bar\n    ```\n\n"))
    (should (equal (buffer-substring-no-properties (point) (point-max))
                   "\n    bar\n    ```\n\n"))))

(ert-deftest test-markdown-gfm/gfm-parse-buffer-for-languages ()
  "Parse buffer for existing languages for `markdown-gfm-used-languages' test."
  (markdown-test-string-gfm "``` MADEUP\n\n```\n``` LANGUAGES\n\n```\n```MaDeUp\n\n```\n```\n\n```\n``` \n\n```\n"
    (markdown-gfm-parse-buffer-for-languages)
    (should (equal markdown-gfm-used-languages
                   (list "MaDeUp" "LANGUAGES" "MADEUP")))
    (should (equal (car markdown-gfm-used-languages) "MaDeUp"))
    (should (equal (car (markdown-gfm-get-corpus)) "MaDeUp"))
    (goto-char (point-max))
    (markdown-insert-gfm-code-block "newlang")
    (should (equal markdown-gfm-used-languages
                   (list "newlang" "MaDeUp" "LANGUAGES" "MADEUP")))
    (should (equal (car markdown-gfm-used-languages) "newlang"))
    (should (equal (car (markdown-gfm-get-corpus)) "newlang"))
    (let ((markdown-gfm-downcase-languages nil))
      (should
       (equal (markdown-gfm-get-corpus)
              (append markdown-gfm-used-languages
                      markdown-gfm-additional-languages
                      markdown-gfm-recognized-languages))))
    (let ((markdown-gfm-downcase-languages t))
      (should
       (equal
        (markdown-gfm-get-corpus)
        (append markdown-gfm-used-languages
                (cl-mapcar #'downcase
                           (append markdown-gfm-additional-languages
                                   markdown-gfm-recognized-languages))))))))

(ert-deftest test-markdown-gfm/code-block-font-lock ()
  "GFM code block font lock test."
  (markdown-test-file-gfm "gfm.text"
    (markdown-test-range-has-face 2639 2641 'markdown-markup-face) ; ```
    (markdown-test-range-has-face 2642 2645 'markdown-language-keyword-face) ; lang
    (markdown-test-range-has-face 2647 2728 'markdown-pre-face) ; code
    (markdown-test-range-has-face 2730 2732 'markdown-markup-face))) ; ```

(ert-deftest test-markdown-gfm/code-block-font-lock-2 ()
  "GFM code block font lock test without language identifier."
  (markdown-test-string-gfm "Plain code block:\n\n```\nfoo\n```\n"
    (markdown-test-range-has-face 20 22 'markdown-markup-face)
    (markdown-test-range-has-face 24 26 'markdown-pre-face)
    (markdown-test-range-has-face 28 30 'markdown-markup-face)))

;;; Extension: GFM simplified tables

(ert-deftest test-markdown-gfm/false-table-first-line ()
  "Test beginning of table detection at beginning of buffer.
Based on GH-298."
  (markdown-test-string
   "[|"
   (should-not (gfm--table-at-point-p))))

(ert-deftest test-markdown-gfm/true-table-first-line ()
  "Test beginning of table detection at beginning of buffer."
  (markdown-test-string
   "[|
-|-
x|"
   (dotimes (x 3)
     (should (gfm--table-at-point-p))
     (forward-line))))

(ert-deftest test-markdown-gfm/table-begin-top-of-file ()
  "Test beginning of table detection at top of file."
  (markdown-test-string "[|"
    (should-not (gfm--table-at-point-p))))

;;; Tests for other extensions:

(ert-deftest test-markdown-ext/pandoc-fancy-lists ()
  "Test basic support for font lock and filling of Pandoc 'fancy lists'."
  (markdown-test-string " #. abc\ndef\n"
    ;; font lock
    (markdown-test-range-has-face 1 1 nil)
    (markdown-test-range-has-face 2 3 'markdown-list-face)
    (markdown-test-range-has-face 4 11 nil)
    ;; filling
    (forward-line)
    (markdown-indent-region (line-beginning-position) (line-end-position) nil)
    (should (string-equal (buffer-string) " #. abc\n def\n"))
    (markdown-indent-region (line-beginning-position) (line-end-position) nil)
    (should (string-equal (buffer-string) " #. abc\n    def\n"))))

(ert-deftest test-markdown-ext/wiki-link-rules ()
  "Test wiki link search rules and font lock for missing pages."
  (let ((markdown-enable-wiki-links t)
        (markdown-wiki-link-fontify-missing t)
        (markdown-wiki-link-search-subdirectories t)
        (markdown-wiki-link-search-parent-directories t))
    (progn
      (find-file "wiki/root")
      (unwind-protect
          (progn
            (markdown-mode)
            ;; search rules
            (should (string-match-p
                     "/sub/foo$"
                     (markdown-convert-wiki-link-to-filename "foo")))
            (should (string-equal
                     (markdown-convert-wiki-link-to-filename "doesnotexist")
                     "doesnotexist"))
            ;; font lock
            (markdown-test-range-has-property 1 11 'font-lock-face 'markdown-link-face)
            (markdown-test-range-has-property 14 33 'font-lock-face 'markdown-missing-link-face)
            (markdown-test-range-has-property 36 42 'font-lock-face 'markdown-link-face)
            (markdown-test-range-has-property 45 60 'font-lock-face 'markdown-missing-link-face))
        (kill-buffer)))
    (progn
      (find-file "wiki/sub/foo")
      (unwind-protect
          (progn
            (markdown-mode)
            ;; search rules
            (should (string-match-p
                     "/wiki/root$"
                     (markdown-convert-wiki-link-to-filename "root")))
            (should (string-equal
                     (markdown-convert-wiki-link-to-filename "doesnotexist")
                     "doesnotexist"))
            ;; font lock
            (markdown-test-range-has-property 1 16 'font-lock-face 'markdown-missing-link-face)
            (markdown-test-range-has-property 19 26 'font-lock-face 'markdown-link-face))
        (kill-buffer)))))

(defadvice markdown-live-preview-window-eww
    (around markdown-test-create-fake-eww disable)
  (setq ad-return-value (get-buffer-create "*eww*")))

(defmacro markdown-test-fake-eww (&rest body)
  `(progn
     ,@(if (and (fboundp 'libxml-parse-html-region) (require 'eww nil t)) body
         `((ad-enable-advice #'markdown-live-preview-window-eww
                             'around 'markdown-test-create-fake-eww)
           (ad-activate #'markdown-live-preview-window-eww)
           ,@body
           (ad-disable-advice #'markdown-live-preview-window-eww
                              'around 'markdown-test-create-fake-eww)
           (ad-activate #'markdown-live-preview-window-eww)))))

(defmacro markdown-test-eww-or-nothing (test &rest body)
  (if (and (fboundp 'libxml-parse-html-region) (require 'eww nil t)
           (executable-find markdown-command))
      `(progn ,@body)
    (message "no eww, no libxml2, or no %s found: skipping %s" markdown-command test)
    nil))

(ert-deftest test-markdown-ext/live-preview-no-file ()
  "Live-preview a `markdown-mode' buffer without a file."
  (with-temp-buffer
    (markdown-mode)

    ;; Activating `markdown-live-preview-mode' signals error
    (let ((data (should-error (markdown-live-preview-mode) :type 'user-error)))
      (should (string-match-p
               (rx bos "Buffer " (+ nonl) " does not visit a file" eos)
               (error-message-string data))))

    ;; After trying to activate live preview mode, mode is not activated
    (should-not markdown-live-preview-mode)

    ;; `markdown-live-preview-export' does nothing
    (should-not (markdown-live-preview-export))

    ;; `markdown-live-preview-remove' does nothing
    (should-not (markdown-live-preview-remove))))

(ert-deftest test-markdown-ext/live-preview-exports ()
  (let ((markdown-command #'markdown-command-identity))
    (markdown-test-temp-file "inline.text"
      (unless (and (fboundp 'libxml-parse-html-region) (require 'eww nil t))
        (should-error (markdown-live-preview-mode)))
      (markdown-test-fake-eww
       (markdown-live-preview-mode)
       (should (buffer-live-p markdown-live-preview-buffer))
       (should (eq (current-buffer)
                   (with-current-buffer markdown-live-preview-buffer
                     markdown-live-preview-source-buffer)))
       (kill-buffer markdown-live-preview-buffer)
       (should (null markdown-live-preview-buffer))
       (set-buffer-modified-p t)
       (save-buffer) ;; should create new export
       (should (buffer-live-p markdown-live-preview-buffer))))))

(ert-deftest test-markdown-ext/live-preview-delete-exports ()
  (markdown-test-fake-eww
   (let ((markdown-live-preview-delete-export 'delete-on-destroy)
         (markdown-command #'markdown-command-identity)
         file-output)
     (markdown-test-temp-file "inline.text"
       (markdown-live-preview-mode)
       (setq file-output (markdown-export-file-name)))
     (should-not (file-exists-p file-output)))
   (let ((markdown-live-preview-delete-export 'delete-on-export)
         (markdown-command #'markdown-command-identity)
         file-output)
     (markdown-test-temp-file "inline.text"
       (markdown-live-preview-mode)
       (setq file-output (markdown-export-file-name))
       (should-not (file-exists-p file-output))))
   (let ((markdown-live-preview-delete-export nil)
         (markdown-command #'markdown-command-identity)
         file-output)
     (unwind-protect
         (markdown-test-temp-file "inline.text"
           (markdown-live-preview-mode)
           (setq file-output (markdown-export-file-name))
           (should (file-exists-p file-output)))
       (delete-file file-output)))))

(ert-deftest test-markdown-ext/live-preview-follow-min-max ()
  (markdown-test-eww-or-nothing "live-preview-follow-min-max"
   (markdown-test-temp-file "inline.text"
     (markdown-live-preview-mode)
     (should (buffer-live-p markdown-live-preview-buffer))
     (should (window-live-p (get-buffer-window markdown-live-preview-buffer)))
     (with-selected-window (get-buffer-window markdown-live-preview-buffer)
       (goto-char (point-min)))
     (goto-char (point-min))
     (insert "a test ")
     (markdown-live-preview-export)
     (let (final-pt final-win-st-diff)
       ;; test that still starts at point-min
       (with-selected-window (get-buffer-window markdown-live-preview-buffer)
         (should (= (window-point) 1))
         (should (= (markdown-visual-lines-between-points
                     (window-start) (window-point))
                    0))
         (set-window-point (selected-window) (point-max))
         (setq final-pt (window-point)
               final-win-st-diff (markdown-visual-lines-between-points
                                  (window-start) (window-point))))
       (goto-char (point-min))
       (insert "this is ")
       (markdown-live-preview-export)
       (with-selected-window (get-buffer-window markdown-live-preview-buffer)
         (should (= (window-point) (+ final-pt (length "this is "))))
         (should (= (markdown-visual-lines-between-points
                     (window-start) (window-point))
                    final-win-st-diff))
         ;; test that still starts at point-max, with correct line difference
         (goto-char (floor (/ (float (- (point-max) (point-min))) 2)))
         (setq final-pt (window-point)
               final-win-st-diff (markdown-visual-lines-between-points
                                  (window-start) final-pt)))
       (markdown-live-preview-export)
       ;; test that still starts at same point, with correct line difference
       (with-selected-window (get-buffer-window markdown-live-preview-buffer)
         (should (= (window-point) final-pt))
         (should (= (markdown-visual-lines-between-points
                     (window-start) (window-point))
                    final-win-st-diff)))))))

;; Tests for imenu

(ert-deftest test-markdown-imenu/metadata ()
  "Don't correct header like statement in metadata.
https://github.com/jrblevin/markdown-mode/issues/145"
  (markdown-test-string "---
title = \"Blah\"
comments = false
---

# Header1

## Header2
"
    (let ((headers (mapcar #'car (markdown-imenu-create-flat-index))))
      (should (member "Header1" headers))
      (should (member "Header2" headers))
      (should-not (member "comments = false" headers)))))

(ert-deftest test-markdown-imenu/include-footnotes ()
  "Check that footnotes are added to the imenu.
https://github.com/jrblevin/markdown-mode/issues/235"
  (markdown-test-string "# H1

[^fn1]: footnote 1

## H2

[^fn2]: footnote 2
"
    (let* ((markdown-add-footnotes-to-imenu t)
           (entries (mapcar #'car (markdown-imenu-create-flat-index))))
      (should (member "^fn1" entries))
      (should (member "^fn2" entries)))))

(ert-deftest test-markdown-imenu/no-footnotes ()
  "Check that footnotes are not added to the imenu.
https://github.com/jrblevin/markdown-mode/issues/235"
  (markdown-test-string "# H1

[^fn1]: footnote 1

## H2

[^fn2]: footnote 2
"
    (let* ((markdown-add-footnotes-to-imenu nil)
           (entries (mapcar #'car (markdown-imenu-create-flat-index))))
      (should-not (member "^fn1" entries))
      (should-not (member "^fn2" entries)))))

(ert-deftest test-markdown-command/function ()
  "Test ‘markdown’ with ‘markdown-command’ being a function."
  (markdown-test-string "foo"
    (let* ((calls ())
           (markdown-command (lambda (&rest args) (push args calls)))
           (buffer-name (markdown))
           (buffer (get-buffer buffer-name)))
      (should (stringp buffer-name))
      (should (buffer-live-p buffer))
      (should (equal calls `((1 4 ,buffer)))))))

(ert-deftest test-markdown-command/does-not-exist ()
  "Test ‘markdown’ with a non-existing ‘markdown-command’."
  (skip-unless (not (file-executable-p "/does/not/exist")))
  (markdown-test-string "foo"
    (let* ((markdown-command "/does/not/exist")
           (data (should-error (markdown) :type 'user-error)))
      (should (string-prefix-p "/does/not/exist failed with exit code "
                               (error-message-string data))))))

(ert-deftest test-markdown-command/fails ()
  "Test ‘markdown’ with a failing ‘markdown-command’."
  (skip-unless (executable-find "false"))
  (markdown-test-string "foo"
    (let* ((markdown-command "false")
           (data (should-error (markdown) :type 'user-error)))
      (should (string-prefix-p "false failed with exit code "
                               (error-message-string data))))))

(ert-deftest test-markdown-open-command/function ()
  "Test ‘markdown-open’ with ‘markdown-open-command’ being a function."
  (markdown-test-string ""
    (let* ((calls 0)
           (markdown-open-command (lambda () (cl-incf calls))))
      (markdown-open)
      (should (equal calls 1)))))

(ert-deftest test-markdown-open-command/does-not-exist ()
  "Test ‘markdown-open’ with a non-existing ‘markdown-open-command’."
  (skip-unless (not (file-executable-p "/does/not/exist")))
  (markdown-test-string "foo"
    (let ((buffer-file-name
           (expand-file-name "bar.md" temporary-file-directory))
          (markdown-open-command "/does/not/exist"))
      (should-error (markdown-open) :type 'file-error))))

(ert-deftest test-markdown-open-command/fails ()
  "Test ‘markdown-open’ with a failing ‘markdown-open-command’."
  (skip-unless (executable-find "false"))
  (markdown-test-string "foo"
    (let* ((buffer-file-name
            (expand-file-name "bar.md" temporary-file-directory))
           (markdown-open-command "false")
           (data (should-error (markdown-open) :type 'user-error)))
      (should (string-prefix-p "false failed with exit code "
                               (error-message-string data))))))

(provide 'markdown-test)

;;; markdown-test.el ends here
