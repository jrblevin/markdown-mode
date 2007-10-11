;;; markdown-mode.el --- Major mode to edit Markdown files in Emacs
;;
;; Author: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Created: May 24, 2007
;; $Id: markdown-mode.el,v 1.5 2007/10/11 16:43:23 jrblevin Exp $
;; Keywords: Markdown major mode
;;
;; Copyright (C) 2007 Jason Blevins
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;; Emacs markdown-mode
;; ===================
;; 
;; [markdown-mode][] is a major mode for [GNU Emacs][] which provides syntax
;; highlighting and supporting commands for editing [Markdown][] files.  It
;; provides keybindings and commands for inserting Markdown elements and to
;; assist in calling Markdown to parse the source code or preview the document
;; in a browser.  It also, optionally, provides syntax highlighting for
;; [wiki links](#itex) and embedded [itex](#itex) mathematical expressions.
;; 
;; Markdown is a simple plain-text-based markup language for documents designed
;; to be translated into HTML.  It is designed to be as readable as possible.
;; Markdown is also the name of the original text-to-HTML conversion tool
;; written in Perl.  Since its creation it has become very popular and has been
;; integrated into a number of wiki and weblog packages (see the [links](#links)
;; below).
;; 
;; markdown-mode is free software, licensed under the [GNU GPL][].
;; 
;;  [markdown-mode]: http://jrblevin.freeshell.org/software/markdown-mode
;;  [GNU Emacs]:     http://www.gnu.org/software/emacs
;;  [Markdown]:      http://daringfireball.net/projects/markdown
;;  [GNU GPL]:       http://www.gnu.org/copyleft/gpl.html
;; 
;; Download
;; --------
;; 
;; The latest version is [markdown-mode 1.5][current], released on October 11,
;; 2007.  This [documentation](readme.txt) is also available in its original
;; Markdown form.
;; 
;; This release contains significant changes from the previous version,
;; particularly in the area of syntax highlighting.  There are still a few
;; highlighting quirks, but these are mostly side effects of using Emacs'
;; multi-line font lock rather than using a custom function to parse the
;; document.  All [previous versions][archive], such as
;; [markdown-mode-1.4.el](archive/markdown-mode-1.4.el), are still available as
;; well.
;; 
;; This mode has only been tested on Emacs 21.4 and 22.0.  Please let me know
;; if there are problems on other versions.  If you find any bugs, such as
;; syntax highlighting issues that aren't already acknowledged in the TODO
;; list, please [email](mailto:jrblevin@sdf.lonestar.org) me a test case to
;; look at.
;; 
;; markdown-mode is also available in the Debian [emacs-goodies-el][]
;; package, as of the 27.0-1 revision.
;; 
;; [current]: http://jrblevin.freeshell.org/software/markdown-mode/markdown-mode.el
;; [archive]: http://jrblevin.freeshell.org/software/markdown-mode/archive
;; [emacs-goodies-el]: http://packages.debian.org/emacs%2Dgoodies%2Del
;; 
;; Installation
;; ------------
;; 
;; Add the following lines to your `.emacs` file to associate markdown-mode with
;; `.mdml` files.  There doesn't seem to be a consensus on an official file
;; extension so you can change this to `.text`, `.md`, `.mdt`, or whatever you
;; call your markdown files.
;; 
;;     (autoload 'markdown-mode "markdown-mode.el"
;;        "Major mode for editing Markdown files" t)
;;     (setq auto-mode-alist
;;        (cons '("\\.mdml$" . markdown-mode) auto-mode-alist))
;; 
;; Make sure to place this file somewhere in the load-path.
;; 
;; Instiki and itex
;; ----------------
;; 
;; Besides supporting the basic Markdown syntax, this mode also includes support
;; for documents including embedded mathematics written [itex][] and syntax
;; highlighting for `[[Wiki Links]]`.  These features are designed for editing
;; pages on math-enabled wikis such as [Instiki][].  One way to do this is to simply
;; copy the text from your browser to Emacs, edit the page using markdown-mode,
;; and then copy it back when you are finished.  The other way is to use a
;; plugin such as [Mozex][] for Mozilla/Firefox which allows you to call an
;; external editor such as Emacs to edit textareas.  You can also tell [Mozex][]
;; to give the temporary file a particular extension, such as `.mdml`, so that
;; markdown-mode will be loaded automatically.
;; 
;; To enable syntax highlighting for itex equations and wiki links, edit
;; `markdown-mode.el` and change `(defvar markdown-enable-itex nil)` to
;; `(defvar markdown-enable-itex t)`.
;; 
;;  [itex]:    http://golem.ph.utexas.edu/~distler/blog/itex2MMLcommands.html
;;  [Instiki]: http://golem.ph.utexas.edu/instiki
;;  [Mozex]:   http://mozex.mozdev.org
;; 
;; 
;; Usage
;; -----
;; 
;; No configuration is necessary, although there are a few things that can be
;; customized (<kbd>M-x customize-mode</kbd>).
;; 
;; The element insertion keybindings are based on those of [html-helper-mode][].
;; Commands are grouped by prefixes based on their function.  For example,
;; commands dealing with headers begin with <kdb>C-c C-t</kbd>.  You can obtain
;; a list of keybindings by pressing <kbd>C-c C-h</kbd> in Emacs.
;; 
;; Some commands behave differently depending on whether there is an active
;; selection.  For example, if there is no active selection <kbd>C-c C-a l</kbd>
;; will simply insert an empty `[]()` link.  Otherwise, it will use the selected
;; text as the link text.  Most other formatting commands behave similarly.
;; 
;; TODO
;; ----
;; 
;; * Highlight inline HTML.
;; * itex: Separate font locking for `\label{}` elements inside equations.
;; * When inserting links (and maybe other elements), the selected should not
;;   remain in the kill ring.
;; * Indentation and filling of list items.
;; * A complete syntax table.
;; * Hanging indents of list items are highlighted as preformatted text.
;; * Use abbrev for quick entry of itex math mode symbols.
;; * Multi-line font lock in Emacs can be unreliable.  What we really need is
;;   a custom syntax-highlighting function.


;;; User Customizable Variables ===============================================

;; To enable itex/wiki syntax highlighting, change to
;; (defvar markdown-enable-itex t)
(defvar markdown-enable-itex nil)


;;; Customizable variables ====================================================

;; Current revision
(defconst markdown-mode-version "$Revision: 1.5 $")

;; A hook for users to run their own code when the mode is loaded.
(defvar markdown-mode-hook nil)


;;; Customizable variables ====================================================

(defgroup markdown nil
  "Markdown mode."
  :prefix "markdown-"
  :group 'languages)

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


;;; Font lock faces ==========================================================

;; Make new faces based on existing ones

;;; This is not available in Emacs 21 so it has been disabled until 
;;; something can be built from scratch.  If you are running Emacs 22 and
;;; want to underline line breaks, uncomment this face and the associated
;;; regular expression below.

;(copy-face 'nobreak-space 'markdown-font-lock-line-break-face)

(defface markdown-font-lock-bold-face '((t (:inherit bold)))
  "`markdown-mode' face used to highlight **bold** and __bold__ text.")
(defface markdown-font-lock-italic-face '((t (:inherit italic)))
  "`markdown-mode' face used to highlight *italic* and _italic_ text.")
(defface markdown-font-lock-inline-code-face '((t (:inherit fixed-pitch)))
  "`markdown-mode' face used to highlight `inline code` fragments.")

;;; If you prefer to highlight italic/bold/code using colors, rather than
;;; with italic and bold and fixed faces, uncomment the following lines.

;(copy-face 'font-lock-variable-name-face 'markdown-font-lock-italic-face)
;(copy-face 'font-lock-type-face 'markdown-font-lock-bold-face)
;(copy-face 'font-lock-builtin-face 'markdown-font-lock-inline-code-face)

(copy-face 'font-lock-function-name-face 'markdown-font-lock-header-face)
(copy-face 'font-lock-variable-name-face 'markdown-font-lock-list-face)
(copy-face 'font-lock-comment-face 'markdown-font-lock-blockquote-face)
(copy-face 'font-lock-constant-face 'markdown-font-lock-link-face)
(copy-face 'font-lock-type-face 'markdown-font-lock-reference-face)
(copy-face 'font-lock-string-face 'markdown-font-lock-url-face)
(copy-face 'font-lock-builtin-face 'markdown-font-lock-math-face)

;; Define the extra font lock faces
;(defvar markdown-font-lock-line-break-face 'markdown-font-lock-line-break-face
;  "Face name to use for line breaks.")
(defvar markdown-font-lock-italic-face 'markdown-font-lock-italic-face
  "Face name to use for italics.")
(defvar markdown-font-lock-bold-face 'markdown-font-lock-bold-face
  "Face name to use for bold.")
(defvar markdown-font-lock-header-face 'markdown-font-lock-header-face
  "Face name to use for headers.")
(defvar markdown-font-lock-inline-code-face 'markdown-font-lock-inline-code-face
  "Face name to use for inline code.")
(defvar markdown-font-lock-list-face 'markdown-font-lock-list-face
  "Face name to use for list items.")
(defvar markdown-font-lock-blockquote-face 'markdown-font-lock-blockquote-face
  "Face name to use for blockquotes and code blocks.")
(defvar markdown-font-lock-link-face 'markdown-font-lock-link-face
  "Face name to use for links.")
(defvar markdown-font-lock-reference-face 'markdown-font-lock-reference-face
  "Face name to use for references.")
(defvar markdown-font-lock-url-face 'markdown-font-lock-url-face
  "Face name to use for URLs.")
(defvar markdown-font-lock-math-face 'markdown-font-lock-math-face
  "Face name to use for itex expressions.")


;;; Regular expressions =======================================================

;; Links
(defconst regex-link-inline "\\(!?\\[.*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)")
(defconst regex-link-reference "\\(!?\\[.+?\\]\\)[ ]?\\(\\[.*?\\]\\)"
  "Regular expression for a reference link [text][id]")
(defconst regex-reference-definition
  "^ \\{0,3\\}\\(\\[.+?\\]\\):[ ]?\\(.*?\\)\\(\"[^\"]+?\"\\)?$"
  "Regular expression for a link definition [id]: ...")

;; itex/LaTeX
(defconst markdown-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions")

(defconst markdown-regex-latex-display
    "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions")


;;; Font lock =================================================================

(defconst markdown-mode-font-lock-keywords-basic
  (list
   ;;;
   ;;; Code ----------------------------------------------------------
   ;;;
   ;; Double backtick style ``inline code``
   (cons "``.+?``" 'markdown-font-lock-inline-code-face)
   ;; Single backtick style `inline code`
   (cons "`.+?`" 'markdown-font-lock-inline-code-face)
   ;; Four-space indent style code block
   (cons "^    .*$" 'markdown-font-lock-blockquote-face)
   ;;;
   ;;; Headers and Horizontal Rules ----------------------------------
   ;;;
   ;; Equals style headers (===)
   (cons ".*\n===+" 'markdown-font-lock-header-face)
   ;; Hyphen style headers (---)
   (cons ".*\n---+" 'markdown-font-lock-header-face)
   ;; Hash style headers (###)
   (cons "^#+ .*$" 'markdown-font-lock-header-face)
   ;; Asterisk style horizontal rules (* * *)
   (cons "^\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*$" 'markdown-font-lock-header-face)
   ;; Hyphen style horizontal rules (- - -)
   (cons "^-[ ]?-[ ]?-[--- ]*$" 'markdown-font-lock-header-face)
   ;;;
   ;;; Special cases -------------------------------------------------
   ;;;
   ;; List item including bold
   (cons "^\\s *\\* .*?[^\\\n]?\\(\\*\\*.*?[^\n\\]\\*\\*\\).*$"
         '(1 'markdown-font-lock-bold-face))
   ;; List item including italics
   (cons "^\\* .*?[^\\\n]?\\(\\*.*?[^\n\\]\\*\\).*$"
         '(1 'markdown-font-lock-italic-face))
   ;;;
   ;;; Lists ---------------------------------------------------------
   ;;;
   ;; Numbered lists (1. List item)
   (cons "^[0-9]+\\.\\s " 'markdown-font-lock-list-face)
   ;; Level 1 list item (no indent) (* List item)
   (cons "^\\(\\*\\|\\+\\|-\\) " '(1 'markdown-font-lock-list-face))
   ;; Level 2 list item (two or more spaces) (   * Second level list item)
   (cons "^  [ ]*\\(\\*\\|\\+\\|-\\) " 'markdown-font-lock-list-face)
   ;;;
   ;;; Links ---------------------------------------------------------
   ;;;
   (cons regex-link-inline '(1 'markdown-font-lock-link-face t))
   (cons regex-link-inline '(2 'markdown-font-lock-url-face t))
   (cons regex-link-reference '(1 'markdown-font-lock-link-face t))
   (cons regex-link-reference '(2 'markdown-font-lock-reference-face t))
   (cons regex-reference-definition '(1 'markdown-font-lock-reference-face t))
   (cons regex-reference-definition '(2 'markdown-font-lock-url-face t))
   (cons regex-reference-definition '(3 'markdown-font-lock-link-face t))
   ;;;
   ;;; Bold ----------------------------------------------------------
   ;;;
   ;; **Asterisk** and _underscore_ style bold
   (cons "[^\\]\\(\\(\\*\\*\\|__\\)\\(.\\|\n\\)*?[^\\]\\2\\)"
         '(1 'markdown-font-lock-bold-face))
   ;;;
   ;;; Italic --------------------------------------------------------
   ;;;
   ;; *Asterisk* and _underscore_ style italic
   (cons "[^\\]\\(\\(\\*\\|_\\)\\(.\\|\n\\)*?[^\\]\\2\\)"
         '(1 'markdown-font-lock-italic-face))
   ;;;
   ;;; Blockquotes ---------------------------------------------------
   ;;;
   (cons "^>.*$" 'markdown-font-lock-blockquote-face)
   ;;;
   ;;; Hard line breaks ----------------------------------------------
   ;;;
   ;; Trailing whitespace (two spaces at end of line)
;   (cons "  $" 'markdown-font-lock-line-break-face)
   )
  "Syntax highlighting for Markdown files.")


;; Includes additional Latex/itex/Instiki font lock keywords
(defconst markdown-mode-font-lock-keywords-itex
  (append
    (list
     ;;;
     ;;; itex expressions --------------------------------------------
     ;;;
     ;; itex math mode $..$ or $$..$$
     (cons markdown-regex-latex-expression '(2 markdown-font-lock-math-face))
     ;; Display mode equations with brackets: \[ \]
     (cons markdown-regex-latex-display 'markdown-font-lock-math-face)
     ;;;
     ;;; itex equation references ------------------------------------
     ;;;
     ;; Equation reference (eq:foo)
     (cons "(eq:\\w+)" 'markdown-font-lock-reference-face)
     ;; Equation reference \eqref
     (cons "\\\\eqref{\\w+}" 'markdown-font-lock-reference-face)
     ;; Wiki links
     (cons "\\[\\[[^]]+\\]\\]" 'markdown-font-lock-link-face))
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

(defun wrap-or-insert (s1 s2)
 "Insert the strings s1 and s2 around the current region or just insert them
if there is no region selected."
 (if (and transient-mark-mode mark-active)
     (let ((a (region-beginning)) (b (region-end)))
       (kill-region a b)
       (insert s1)
       (yank)
       (insert s2))
   (insert s1 s2)))

(defun markdown-insert-hr ()
  "Insert a horizonal rule."
  (interactive)
  (let (hr)
    (dotimes (count (- markdown-hr-length 1) hr)        ; Count to n - 1
      (setq hr (concat "* " hr)))                       ; Build HR string
    (setq hr (concat hr "*\n"))                         ; Add the n-th *
    (insert hr)))

(defun markdown-insert-bold ()
  "Make the active region bold or insert an empty bold word."
  (interactive)
  (if markdown-bold-underscore
      (wrap-or-insert "__" "__")
    (wrap-or-insert "**" "**"))
  (backward-char 2))

(defun markdown-insert-italic ()
  "Make the active region italic or insert an empty italic word."
  (interactive)
  (if markdown-italic-underscore
      (wrap-or-insert "_" "_")
    (wrap-or-insert "*" "*"))
  (backward-char 1))

(defun markdown-insert-code ()
  "Format the active region as inline code or insert an empty inline code
fragment."
  (interactive)
  (wrap-or-insert "`" "`")
  (backward-char 1))

(defun markdown-insert-link ()
  "Creates an empty link of the form []().  If there is an active region,
this text will be used for the link text."
  (interactive)
  (wrap-or-insert "[" "]")
  (insert "()")
  (backward-char 1))

(defun markdown-insert-image ()
  "Creates an empty image of the form ![]().  If there is an active region,
this text will be used for the alternate text for the image."
  (interactive)
  (wrap-or-insert "![" "]")
  (insert "()")
  (backward-char 1))

(defun markdown-insert-header-1 ()
  "Creates a level 1 header"
  (interactive)
  (markdown-insert-header 1))

(defun markdown-insert-header-2 ()
  "Creates a level 2 header"
  (interactive)
  (markdown-insert-header 2))

(defun markdown-insert-header-3 ()
  "Creates a level 3 header"
  (interactive)
  (markdown-insert-header 3))

(defun markdown-insert-header-4 ()
  "Creates a level 4 header"
  (interactive)
  (markdown-insert-header 4))

(defun markdown-insert-header-5 ()
  "Creates a level 5 header"
  (interactive)
  (markdown-insert-header 5))

(defun markdown-insert-header (n)
  "Creates a level n header.  If there is an active region, it is used as the
header text."
  (interactive "p")
  (unless n                             ; Test to see if n is defined
    (setq n 1))                         ; Default to level 1 header
  (let (hdr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))      ; Build a ### header string
    (setq hdrl (concat hdr " "))
    (setq hdrr (concat " " hdr))
    (wrap-or-insert hdrl hdrr))
  (backward-char (+ 1 n)))

(defun markdown-insert-title ()
  "Use the active region to create an \"equals\" style title or insert
a blank title and move the cursor to the required position in order to
insert a title."
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
  "Use the active region to create a dashed style section or insert
a blank section and move the cursor to the required position in order to
insert a section."
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
  "Start a blank blockquote section unless there is an active region, in
which case it is turned into a blockquote region."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (blockquote-region)
    (insert "> ")))


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
    (define-key markdown-mode-map "\C-c-" 'markdown-insert-hr)
    (define-key markdown-mode-map "\C-c\C-tt" 'markdown-insert-title)
    (define-key markdown-mode-map "\C-c\C-ts" 'markdown-insert-section)
    ;; Markdown functions
    (define-key markdown-mode-map "\C-c\C-cm" 'markdown)
    (define-key markdown-mode-map "\C-c\C-cp" 'markdown-preview)
    markdown-mode-map)
  "Keymap for Markdown major mode")


;;; Markdown ==================================================================

(defun markdown ()
  "Run markdown on the current buffer and preview the output in another buffer."
  (interactive)
    (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
        (shell-command-on-region region-beginning region-end markdown-command
                                 "*markdown-output*" nil)
      (shell-command-on-region (point-min) (point-max) markdown-command
                               "*markdown-output*" nil)))

(defun markdown-preview ()
  "Run markdown on the current buffer and preview the output in a browser."
  (interactive)
  (markdown)
  (browse-url-of-buffer "*markdown-output*"))


;;; Utilities =================================================================

(defun markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "markdown-mode, version %s" markdown-mode-version))

(defun blockquote-region ()
  "Blockquote an entire region."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (perform-replace "^" "> " nil 1 nil nil nil (region-beginning) (region-end))))


;;; Mode definition  ==========================================================

(define-derived-mode markdown-mode fundamental-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t))

;(add-to-list 'auto-mode-alist '("\\.mdml$" . markdown-mode))

(provide 'markdown-mode)

;;; markdown-mode.el ends here
