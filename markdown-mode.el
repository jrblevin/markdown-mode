;;; markdown-mode.el --- Major mode to edit Markdown files in Emacs

;; Copyright (C) 2007-2008 Jason Blevins

;; Version: 1.6-dev
;; Keywords: Markdown major mode
;; Author: Jason Blevins <jrblevin@sdf.lonestar.org>
;; URL: http://jblevins.org/projects/markdown-mode

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; markdown-mode is a major mode for editing [Markdown][]-formatted
;; text files in GNU Emacs.  markdown-mode is free software, licensed
;; under the GNU GPL.
;;
;;  [Markdown]: http://daringfireball.net/projects/markdown/
;;
;; The latest version is markdown-mode 1.5, released on October 11, 2007:
;;
;;  * [markdown-mode.el][]
;;  * [Screenshot][]
;;  * [Release notes][]
;;
;; markdown-mode is also available in the Debian `emacs-goodies-el`
;; package (beginning with revision 27.0-1).
;;
;;  [markdown-mode.el]: http://code.jblevins.org/markdown-mode/markdown-mode.el
;;  [screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20071011-001.png
;;  [release notes]: http://jblevins.org/projects/markdown-mode/rev-1-5

;;; Dependencies:

;; markdown-mode requires easymenu, a standard package since GNU Emacs
;; 19 and XEmacs 19, which provides a uniform interface for creating
;; menus in GNU Emacs and XEmacs.

;;; Installation:

;; Make sure to place `markdown-mode.el` somewhere in the load-path and add
;; the following lines to your `.emacs` file to associate markdown-mode
;; with `.text` files:
;;
;;     (autoload 'markdown-mode "markdown-mode.el"
;;        "Major mode for editing Markdown files" t)
;;     (setq auto-mode-alist
;;        (cons '("\\.text" . markdown-mode) auto-mode-alist))
;;
;; There is no consensus on an official file extension so change `.text` to
;; `.mdwn`, `.md`, `.mdt`, or whatever you call your markdown files.

;;; Usage:

;; Although no configuration is necessary there are a few things that can
;; be customized (`M-x customize-mode`).
;;
;; Keybindings are grouped by prefixes based on their function.  For
;; example, commands dealing with headers begin with `C-c C-t`.  The
;; primary commands in each group will are described below.  You can
;; obtain a list of all keybindings by pressing `C-c C-h`.
;;
;; * Anchors: `C-c C-a`
;;
;;   `C-c C-a l` inserts inline links of the form [text](url).  If there
;;   is an active region, text in the region is used for the link text.
;;
;; * Commands: `C-c C-c`
;;
;;   `C-c C-c m` will run Markdown on the current buffer and preview the
;;   output in another buffer while `C-c C-c p` runs Markdown on the
;;   current buffer and previews the output in a browser.
;;
;; * Images: `C-c C-i`
;;
;;   `C-c C-i i` inserts an image, using the active region (if any) as
;;   the alt text.
;;
;; * Physical styles: `C-c C-p`
;;
;;   These commands all act on text in the active region, if any, and
;;   insert empty markup fragments otherwise.  `C-c C-p b` makes the
;;   selected text bold, `C-c C-p f` formats the region as fixed-width
;;   text, and `C-c C-p i` is used for italic text.
;;
;; * Logical styles: `C-c C-s`
;;
;;   These commands all act on text in the active region, if any, and
;;   insert empty markup fragments otherwise.  Logical styles include
;;   blockquote (`C-c C-s b`), preformatted (`C-c C-s p`), code (`C-c C-s c`),
;;   emphasis (`C-c C-s e`), and strong (`C-c C-s s`).
;;
;; * Headers: `C-c C-t`
;;
;;   All header commands use text in the active region, if any, as the
;;   header text.  To insert an atx or hash style level-n header, press
;;   `C-c C-t n` where n is between 1 and 5.  For a top-level setext or
;;   underline style header press `C-c C-t t` (mnemonic: title) and for
;;   a second-level underline-style header press `C-c C-t s`
;;   (mnemonic: section).
;;
;; * Other commands
;;
;;   `C-c -` inserts a horizontal rule.
;;
;; Many of the commands described above behave differently depending on
;; whether Transient Mark mode is enabled or not.  When it makes sense,
;; if Transient Mark mode is on and a region is active, the command
;; applies to the text in the region (e.g., `C-c C-p b` makes the region
;; bold).  For users who prefer to work outside of Transient Mark mode,
;; in Emacs 22 it can be enabled temporarily by pressing `C-SPC C-SPC`.
;;
;; When applicable, commands that specifically act on the region even
;; outside of Transient Mark mode have the same keybinding as the with
;; the exception of an additional `C-` prefix.  For example,
;; `markdown-insert-blockquote` is bound to `C-c C-s b` and only acts on
;; the region in Transient Mark mode while `markdown-blockquote-region`
;; is bound to `C-c C-s C-b` and always applies to the region (when
;; nonempty).

;;; Extensions:

;; Besides supporting the basic Markdown syntax, markdown-mode also
;; includes syntax highlighting for `[[Wiki Links]]` by default.
;; Syntax highlighting for mathematical expressions written in LaTeX
;; (only expressions denoted by `$..$`, `$$..$$`, or `\[..\]`) can be
;; enabled by editing `markdown-mode.el` and changing
;; `(defvar markdown-enable-itex nil)` to `(defvar markdown-enable-itex t)`.

;;; Thanks:

;; * Cyril Brulebois <cyril.brulebois@enst-bretagne.fr> for Debian packaging.
;; * Conal Elliott <conal@conal.net> for a font-lock regexp patch.
;; * Edward O'Connor <hober0@gmail.com> for a font-lock regexp fix.
;; * Greg Bognar <greg_bognar@hms.harvard.edu> for menus and a patch.
;; * Daniel Burrows <dburrows@debian.org> for filing Debian bug #456592.
;; * Peter S. Galbraith <psg@debian.org> for maintaining emacs-goodies-el.

;;; Bugs:

;; This mode has only been tested on Emacs 21.4 and 22.0.  Please let me
;; know if there are problems on other versions.  If you find any bugs,
;; such as syntax highlighting issues, please construct a test case and
;; email me at <jrblevin@sdf.lonestar.org>.



;;; Code:

(require 'easymenu)

;;; User Customizable Variables ===============================================

;; To enable LaTeX/itex syntax highlighting, change to
;; (defvar markdown-enable-itex t)
(defvar markdown-enable-itex nil)


;;; Customizable variables ====================================================

;; Current revision
(defconst markdown-mode-version "1.6-dev")

;; A hook for users to run their own code when the mode is loaded.
(defvar markdown-mode-hook nil)


;;; Customizable variables ====================================================

(defgroup markdown nil
  "Major mode for editing text files in Markdown format."
  :prefix "markdown-"
  :group 'wp
  :link '(url-link "http://jblevins.org/projects/markdown-mode/"))

(defcustom markdown-command "markdown"
  "Command to run markdown."
  :group 'markdown
  :type 'string)

(defcustom markdown-hr-length 5
  "Length of horizonal rules."
  :group 'markdown
  :type 'integer)

(defcustom markdown-bold-underscore nil
  "Use two underscores for bold instead of two asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-italic-underscore nil
  "Use underscores for italic instead of asterisks."
  :group 'markdown
  :type 'boolean)


;;; Font lock =================================================================

(require 'font-lock)

;; From Emacs 22.1
(unless (boundp 'nobreak-space)
  (defface nobreak-space
    '((((class color) (min-colors 88)) :inherit escape-glyph :underline t)
      (((class color) (min-colors 8)) :background "magenta")
      (t :inverse-video t))
    "Face for displaying nobreak space."
    :group 'basic-faces
    :version 22.1))

(defgroup markdown-faces nil
  "Faces used in Markdown Mode"
  :group 'markdown
  :group 'faces)

(defcustom markdown-italic-face 'font-lock-variable-name-face
  "Italic text."
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-bold-face 'font-lock-type-face
  "Bold text"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-header-face 'font-lock-function-name-face
  "Headers"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-inline-code-face 'font-lock-builtin-face
  "Inline code"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-list-face 'font-lock-variable-name-face
  "List item markers"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-blockquote-face 'font-lock-comment-face
  "Blockquote sections and preformatted text"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-link-face 'font-lock-constant-face
  "Link text"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-reference-face 'font-lock-type-face
  "Link references"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-url-face 'font-lock-string-face
  "URLs"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-math-face 'font-lock-builtin-face
  "LaTeX expressions"
  :group 'markdown-faces
  :type '(face))

(defcustom markdown-line-break-face 'nobreak-space
  "Line breaks"
  :group 'markdown-faces
  :type '(face))


(defconst markdown-regex-link-inline
  "\\(!?\\[.*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)")

(defconst markdown-regex-link-reference
  "\\(!?\\[.+?\\]\\)[ ]?\\(\\[.*?\\]\\)"
  "Regular expression for a reference link [text][id]")

(defconst markdown-regex-reference-definition
  "^ \\{0,3\\}\\(\\[.+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst markdown-regex-header-atx
  "^\\(#+ \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for atx-style (hash mark) headers")

(defconst markdown-regex-header-setext
  "^\\(.*\\)\n\\(===+\\|---+\\)$"
  "Regular expression for setext-style (underline) headers")

(defconst markdown-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Markdown horizontal rules")

(defconst markdown-regex-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ].*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments")

(defconst markdown-regex-pre
  "^    .*$"
  "Regular expression for matching preformatted text sections")

(defconst markdown-regex-list
  "^[ \t]*\\([0-9]+\\.\\|[\\*\\+-]\\) "
  "Regular expression for matching list markers")

(defconst markdown-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text")

(defconst markdown-regex-italic
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching italic text")

(defconst markdown-regex-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines")

(defconst markdown-regex-line-break
  "  $"
  "Regular expression for matching line breaks")

(defconst markdown-regex-wiki-link
  "\\[\\[[^]]+\\]\\]"
  "Regular expression for matching wiki links")

(defconst markdown-regex-uri
  "<\\(acap\\|cid\\|data\\|dav\\|fax\\|file\\|ftp\\|gopher\\|http\\|https\\|imap\\|ldap\\|mailto\\|mid\\|modem\\|news\\|nfs\\|nntp\\|pop\\|prospero\\|rtsp\\|service\\|sip\\|tel\\|telnet\\|tip\\|urn\\|vemmi\\|wais\\)://[^>]*>"
  "Regular expression for matching inline URIs")

(defconst markdown-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses")

(defconst markdown-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions")

(defconst markdown-regex-latex-display
    "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions")

(defconst markdown-mode-font-lock-keywords-basic
  (list
   (cons markdown-regex-code markdown-inline-code-face)
   (cons markdown-regex-pre markdown-blockquote-face)
   (cons markdown-regex-header-setext markdown-header-face)
   (cons markdown-regex-header-atx markdown-header-face)
   (cons markdown-regex-list markdown-list-face)
   (cons markdown-regex-hr markdown-header-face)
   (cons markdown-regex-link-inline
         '((1 markdown-link-face t)
           (2 markdown-url-face t)))
   (cons markdown-regex-link-reference
         '((1 markdown-link-face t)
           (2 markdown-reference-face t)))
   (cons markdown-regex-reference-definition
         '((1 markdown-reference-face t)
           (2 markdown-url-face t)
           (3 markdown-link-face t)))
   (cons markdown-regex-bold '(2 markdown-bold-face))
   (cons markdown-regex-italic '(2 markdown-italic-face))
   (cons markdown-regex-blockquote markdown-blockquote-face)
   (cons markdown-regex-wiki-link markdown-link-face)
   (cons markdown-regex-uri markdown-link-face)
   (cons markdown-regex-email markdown-link-face))
  "Syntax highlighting for Markdown files.")


;; Includes additional Latex/itex/Instiki font lock keywords
(defconst markdown-mode-font-lock-keywords-itex
  (append
    (list
     ;; itex math mode $..$ or $$..$$
     (cons markdown-regex-latex-expression '(2 markdown-math-face))
     ;; Display mode equations with brackets: \[ \]
     (cons markdown-regex-latex-display markdown-math-face)
     ;; Equation reference (eq:foo)
     (cons "(eq:\\w+)" markdown-reference-face)
     ;; Equation reference \eqref
     (cons "\\\\eqref{\\w+}" markdown-reference-face))
    markdown-mode-font-lock-keywords-basic)
  "Syntax highlighting for Markdown, itex, and wiki expressions.")


(defvar markdown-mode-font-lock-keywords
  (if markdown-enable-itex 
      markdown-mode-font-lock-keywords-itex
    markdown-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Markdown mode")



;;; Syntax Table ==============================================================

(defvar markdown-mode-syntax-table
  (let ((markdown-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "w" markdown-mode-syntax-table)
    markdown-mode-syntax-table)
  "Syntax table for markdown-mode")



;;; Element Insertion =========================================================

(defun markdown-wrap-or-insert (s1 s2)
 "Insert the strings S1 and S2.
If Transient Mark mode is on and a region is active, wrap the strings S1
and S2 around the region."
 (if (and transient-mark-mode mark-active)
     (let ((a (region-beginning)) (b (region-end)))
       (goto-char a)
       (insert s1)
       (goto-char (+ b (length s1)))
       (insert s2))
   (insert s1 s2)))

(defun markdown-insert-hr ()
  "Inserts a horizonal rule."
  (interactive)
  (let (hr)
    (dotimes (count (- markdown-hr-length 1) hr)        ; Count to n - 1
      (setq hr (concat "* " hr)))                       ; Build HR string
    (setq hr (concat hr "*\n"))                         ; Add the n-th *
    (insert hr)))

(defun markdown-insert-bold ()
  "Inserts markup for a bold word or phrase.
If Transient Mark mode is on and a region is active, it is made bold."
  (interactive)
  (if markdown-bold-underscore
      (markdown-wrap-or-insert "__" "__")
    (markdown-wrap-or-insert "**" "**"))
  (backward-char 2))

(defun markdown-insert-italic ()
  "Inserts markup for an italic word or phrase.
If Transient Mark mode is on and a region is active, it is made italic."
  (interactive)
  (if markdown-italic-underscore
      (markdown-wrap-or-insert "_" "_")
    (markdown-wrap-or-insert "*" "*"))
  (backward-char 1))

(defun markdown-insert-code ()
  "Inserts markup for an inline code fragment.
If Transient Mark mode is on and a region is active, it is marked
as inline code."
  (interactive)
  (markdown-wrap-or-insert "`" "`")
  (backward-char 1))

(defun markdown-insert-link ()
  "Inserts an inline link of the form []().
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (markdown-wrap-or-insert "[" "]")
  (insert "()")
  (backward-char 1))

(defun markdown-insert-image ()
  "Inserts an inline image tag of the form ![]().
If Transient Mark mode is on and a region is active, it is used
as the alt text of the image."
  (interactive)
  (markdown-wrap-or-insert "![" "]")
  (insert "()")
  (backward-char 1))

(defun markdown-insert-header-1 ()
  "Inserts a first level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 1))

(defun markdown-insert-header-2 ()
  "Inserts a second level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 2))

(defun markdown-insert-header-3 ()
  "Inserts a third level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 3))

(defun markdown-insert-header-4 ()
  "Inserts a fourth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 4))

(defun markdown-insert-header-5 ()
  "Inserts a fifth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 5))

(defun markdown-insert-header (n)
  "Inserts an atx-style (hash mark) header.
With no prefix argument, insert a level-1 header.  With prefix N,
insert a level-N header.  If Transient Mark mode is on and the
region is active, it is used as the header text."
  (interactive "p")
  (unless n                             ; Test to see if n is defined
    (setq n 1))                         ; Default to level 1 header
  (let (hdr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))      ; Build a hash mark header string
    (setq hdrl (concat hdr " "))
    (setq hdrr (concat " " hdr))
    (markdown-wrap-or-insert hdrl hdrr))
  (backward-char (+ 1 n)))

(defun markdown-insert-title ()
  "Insert a setext-style (underline) first level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "=" hdr)))  ; Build a === title underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n==========\n")
    (backward-char 12)))

(defun markdown-insert-section ()
  "Insert a setext-style (underline) second level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "-" hdr)))  ; Build a --- section underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n----------\n")
    (backward-char 12)))

(defun markdown-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (markdown-blockquote-region)
    (insert "> ")))

(defun markdown-blockquote-region (beg end &optional arg)
  "Blockquote the region."
  (interactive "*r\nP")
  (if mark-active
      (perform-replace "^" "> " nil 1 nil nil nil beg end)))

(defun markdown-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (markdown-pre-region)
    (insert "    ")))

(defun markdown-pre-region (beg end &optional arg)
  "Format the region as preformatted text."
  (interactive "*r\nP")
  (if mark-active
      (perform-replace "^" "    " nil 1 nil nil nil beg end)))




;;; Keymap ====================================================================

(defvar markdown-mode-map
  (let ((markdown-mode-map (make-keymap)))
    ;; Element insertion
    (define-key markdown-mode-map "\C-c\C-al" 'markdown-insert-link)
    (define-key markdown-mode-map "\C-c\C-ii" 'markdown-insert-image)
    (define-key markdown-mode-map "\C-c\C-t1" 'markdown-insert-header-1)
    (define-key markdown-mode-map "\C-c\C-t2" 'markdown-insert-header-2)
    (define-key markdown-mode-map "\C-c\C-t3" 'markdown-insert-header-3)
    (define-key markdown-mode-map "\C-c\C-t4" 'markdown-insert-header-4)
    (define-key markdown-mode-map "\C-c\C-t5" 'markdown-insert-header-5)
    (define-key markdown-mode-map "\C-c\C-pb" 'markdown-insert-bold)
    (define-key markdown-mode-map "\C-c\C-ss" 'markdown-insert-bold)
    (define-key markdown-mode-map "\C-c\C-pi" 'markdown-insert-italic)
    (define-key markdown-mode-map "\C-c\C-se" 'markdown-insert-italic)
    (define-key markdown-mode-map "\C-c\C-pf" 'markdown-insert-code)
    (define-key markdown-mode-map "\C-c\C-sc" 'markdown-insert-code)
    (define-key markdown-mode-map "\C-c\C-sb" 'markdown-insert-blockquote)
    (define-key markdown-mode-map "\C-c\C-s\C-b" 'markdown-blockquote-region)
    (define-key markdown-mode-map "\C-c\C-sp" 'markdown-insert-pre)
    (define-key markdown-mode-map "\C-c\C-s\C-p" 'markdown-pre-region)
    (define-key markdown-mode-map "\C-c-" 'markdown-insert-hr)
    (define-key markdown-mode-map "\C-c\C-tt" 'markdown-insert-title)
    (define-key markdown-mode-map "\C-c\C-ts" 'markdown-insert-section)
    ;; Markdown functions
    (define-key markdown-mode-map "\C-c\C-cm" 'markdown)
    (define-key markdown-mode-map "\C-c\C-cp" 'markdown-preview)
    markdown-mode-map)
  "Keymap for Markdown major mode")

;;; Menu ==================================================================

(easy-menu-define markdown-mode-menu markdown-mode-map
  "Menu for Markdown mode"
  '("Markdown"
    ["Compile" markdown]
    ["Preview" markdown-preview]
    "---"
    ("Headers (setext)"
     ["Insert Title" markdown-insert-title]
     ["Insert Section" markdown-insert-section])
    ("Headers (atx)"
     ["First level" markdown-insert-header-1]
     ["Second level" markdown-insert-header-2]
     ["Third level" markdown-insert-header-3]
     ["Fourth level" markdown-insert-header-4]
     ["Fifth level" markdown-insert-header-5])
    "---"
    ["Bold" markdown-insert-bold]
    ["Italic" markdown-insert-italic]
    ["Blockquote" markdown-insert-blockquote]
    ["Preformatted" markdown-insert-pre]
    ["Code" markdown-insert-code]
    "---"
    ["Insert inline link" markdown-insert-link]
    ["Insert image" markdown-insert-image]
    ["Insert horizontal rule" markdown-insert-hr]
    "---"
    ["Version" markdown-show-version]
    ))



;;; Commands ==================================================================

(defun markdown ()
  "Run markdown on the current buffer and preview the output in another buffer."
  (interactive)
    (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
        (shell-command-on-region (region-beginning) (region-end) markdown-command
                                 "*markdown-output*" nil)
      (shell-command-on-region (point-min) (point-max) markdown-command
                               "*markdown-output*" nil)))

(defun markdown-preview ()
  "Run markdown on the current buffer and preview the output in a browser."
  (interactive)
  (markdown)
  (browse-url-of-buffer "*markdown-output*"))



;;; Mode definition  ==========================================================

(defun markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "markdown-mode, version %s" markdown-mode-version))

(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; For menu support in XEmacs
  (easy-menu-add markdown-mode-menu markdown-mode-map))

;(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))

(provide 'markdown-mode)

;;; markdown-mode.el ends here
