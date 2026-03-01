;;;; markdown-indent-test.el --- Tests for markdown-indent  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains the `markdown-indent-mode' test suite.

;;; Code:

(require 'ert)
(require 'markdown-indent)

(defmacro markdown-indent-test-string (string &rest body)
  "Execute BODY in a temp buffer containing STRING.
`markdown-indent-mode' is enabled before BODY runs."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,string)
     (markdown-indent-mode 1)
     ,@body))

(defun markdown-indent-test-line-prefix (line-number)
  "Return the plain string of the line-prefix property at LINE-NUMBER (1-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let ((prop (get-text-property (point) 'line-prefix)))
      (if prop (substring-no-properties prop) ""))))

;;; Heading indentation tests:

(ert-deftest test-markdown-indent-heading/none ()
  "Test that content with no heading has no indentation."
  (markdown-indent-test-string "no heading\nmore content\n"
    (should (string= "" (markdown-indent-test-line-prefix 1)))
    (should (string= "" (markdown-indent-test-line-prefix 2)))))

(ert-deftest test-markdown-indent-heading/level-1 ()
  "Test that heading line has no prefix and content under level-1 is indented by 2."
  (markdown-indent-test-string "# Heading\ncontent\n"
    (should (string= "" (markdown-indent-test-line-prefix 1)))
    (should (string= "  " (markdown-indent-test-line-prefix 2)))))

(ert-deftest test-markdown-indent-heading/level-2 ()
  "Test that level-2 heading is indented by 1 and content under it by 4."
  (markdown-indent-test-string "## Heading\ncontent\n"
    (should (string= " " (markdown-indent-test-line-prefix 1)))
    (should (string= "    " (markdown-indent-test-line-prefix 2)))))

(ert-deftest test-markdown-indent-heading/level-3 ()
  "Test that level-3 heading is indented by 2 and content under it by 6."
  (markdown-indent-test-string "### Heading\ncontent\n"
    (should (string= "  " (markdown-indent-test-line-prefix 1)))
    (should (string= "      " (markdown-indent-test-line-prefix 2)))))

(ert-deftest test-markdown-indent-heading/nested ()
  "Test indentation of nested headings and their content."
  (markdown-indent-test-string "# H1\ncontent1\n## H2\ncontent2\n### H3\ncontent3\n"
    (should (string= "" (markdown-indent-test-line-prefix 1)))      ; # H1: 0 spaces
    (should (string= "  " (markdown-indent-test-line-prefix 2)))    ; content1: 2 spaces
    (should (string= " " (markdown-indent-test-line-prefix 3)))     ; ## H2: 1 space
    (should (string= "    " (markdown-indent-test-line-prefix 4)))  ; content2: 4 spaces
    (should (string= "  " (markdown-indent-test-line-prefix 5)))    ; ### H3: 2 spaces
    (should (string= "      " (markdown-indent-test-line-prefix 6))))) ; content3: 6 spaces

(ert-deftest test-markdown-indent-heading/content-before ()
  "Test that content before the first heading is not indented."
  (markdown-indent-test-string "preamble\n# Heading\ncontent\n"
    (should (string= "" (markdown-indent-test-line-prefix 1)))    ; preamble
    (should (string= "" (markdown-indent-test-line-prefix 2)))    ; # Heading
    (should (string= "  " (markdown-indent-test-line-prefix 3))))) ; content

(ert-deftest test-markdown-indent-heading/invalid-no-space ()
  "Test that `###text' without a space is not treated as a heading."
  (markdown-indent-test-string "###NoSpace\ncontent\n"
    (should (string= "" (markdown-indent-test-line-prefix 1)))
    (should (string= "" (markdown-indent-test-line-prefix 2)))))

;;; Code fence tests:

(ert-deftest test-markdown-indent-fence/indentation ()
  "Test that fence delimiters and content are indented like regular content.
Fake headings inside a fence do not change the indentation level."
  (markdown-indent-test-string "# Heading\n```\n# not a heading\ncontent\n```\n"
    (should (string= "" (markdown-indent-test-line-prefix 1)))    ; # Heading (heading line)
    (should (string= "  " (markdown-indent-test-line-prefix 2)))  ; ``` (indented like content)
    (should (string= "  " (markdown-indent-test-line-prefix 3)))  ; # not a heading (indented, not a real heading)
    (should (string= "  " (markdown-indent-test-line-prefix 4)))  ; content
    (should (string= "  " (markdown-indent-test-line-prefix 5))))) ; closing ``` (still under # Heading)

(ert-deftest test-markdown-indent-fence/resumes ()
  "Test that indentation resumes correctly after a code fence closes."
  (markdown-indent-test-string "# Heading\n```\ncode\n```\nafter fence\n"
    (should (string= "  " (markdown-indent-test-line-prefix 5))))) ; after fence

;;; Dynamic update tests:

(ert-deftest test-markdown-indent-dynamic/heading-past-fence ()
  "Test that modifying a heading level re-indents content after a fence with fake headings."
  (markdown-indent-test-string "## Heading\n```\n# fake\n```\nafter\n"
    (should (string= "    " (markdown-indent-test-line-prefix 5))) ; initially under ## (4 spaces)
    (goto-char (point-min))
    (delete-char 1)                                                 ; ## → #
    (should (string= "  " (markdown-indent-test-line-prefix 5))))) ; now under # (2 spaces)

(ert-deftest test-markdown-indent-dynamic/delete-hash ()
  "Test that deleting a # reduces heading level and updates content indentation."
  (markdown-indent-test-string "## Heading\ncontent\n"
    (should (string= "    " (markdown-indent-test-line-prefix 2))) ; initially 4 spaces (level 2)
    (goto-char (point-min))
    (delete-char 1)                                                 ; ## → #
    (should (string= "  " (markdown-indent-test-line-prefix 2))))) ; now 2 spaces (level 1)

(ert-deftest test-markdown-indent-dynamic/delete-space ()
  "Test that deleting the space after ### makes it invalid and removes indentation."
  (markdown-indent-test-string "### Heading\ncontent\n"
    (should (string= "      " (markdown-indent-test-line-prefix 2))) ; initially 6 spaces (level 3)
    (goto-char (point-min))
    (search-forward "### ")
    (delete-char -1)                                                  ; delete the space
    (should (string= "" (markdown-indent-test-line-prefix 2)))))     ; no longer a heading

;;; Minor mode tests:

(ert-deftest test-markdown-indent-mode/disable ()
  "Test that disabling `markdown-indent-mode' removes all line-prefix properties."
  (markdown-indent-test-string "# Heading\ncontent\n"
    (should (string= "  " (markdown-indent-test-line-prefix 2)))   ; properties are set
    (markdown-indent-mode -1)
    (should (null (get-text-property                                ; properties removed
                   (save-excursion (goto-char (point-min)) (forward-line 1) (point))
                   'line-prefix)))))

(provide 'markdown-indent-test)

;;; markdown-indent-test.el ends here
