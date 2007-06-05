;;; markdown-mode.el --- Major mode to edit Markdown files in Emacs
;;
;; Author: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Maintainer: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Created: May 24, 2007
;; $Id: markdown-mode.el,v 1.3 2007/06/05 03:29:43 jrblevin Exp $
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
;; Supported Emacsen:
;; ==================
;; This mode has only been tested on Emacs 22.0.  Please let me know
;; if there are problems on other versions.
;;
;; Installation:
;; =============
;; Add the following lines to your .emacs file to associate
;; markdown-mode with .mdml files.   There doesn't seem to be
;; a consensus on an official file extension so you can change
;; this to .text, .md, .mdt, or whatever you call your markdown
;; files.
;;
;;   (autoload 'markdown-mode "markdown-mode.el"
;;   	"Major mode for editing Markdown files" t)
;;   (setq auto-mode-alist
;;     (cons '("\\.mdml$" . markdown-mode) auto-mode-alist))
;;
;; Make sure to place this file somewhere in the load-path.
;;
;; Description:
;; ============
;; This mode provides basic syntax highlighting, element insertion
;; commands, and preview commands for Markdown files.  The latest version
;; should always be available from
;; http://jrblevin.freeshell.org/software/markdown-mode.
;;
;; TODO:
;; =====
;; * Recognize inline HTML.
;; * Bold at the beginning of a line is mistaken to be a list item.
;; * itex font lock support:
;;   + Equation references: (eq:reference) or \eqref{reference}.
;;   + Separate font locking for \label{} elements in side \[ \] equations.

(defconst markdown-mode-version "$Revision: 1.3 $")

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


;;; Regular expressions =======================================================

;; Links
(defconst regex-link-inline "\\(!?\\[.+?\\]\\)\\((.*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)")
(defconst regex-link-reference "\\(!?\\[.+?\\]\\)[ ]?\\(\\[.*?\\]\\)"
  "Regular expression for a reference link [text][id]")
(defconst regex-reference-definition
  "^\s*\\(\\[.+?\\]\\):\s*\\([^\s\n]+\\).*$"
  "Regular expression for a link definition [id]: ...")


;;; Font lock =================================================================

(defconst markdown-mode-font-lock-keywords
  (list
   ;; Latex/itex
;   (cons "\\\\\\[[^$]+\\\\\\]" 'font-lock-string-face)
;   (cons "\\$\\$[^$]+\\$\\$" 'font-lock-string-face)
;   (cons "\\$[^$]+\\$" 'font-lock-string-face)
   ;; Headers and (Horizontal Rules)
   (cons ".*\n?===*" 'font-lock-function-name-face)     ; === headers
   (cons ".*\n?---*" 'font-lock-function-name-face)     ; --- headers
   (cons "^#+ .*$" 'font-lock-function-name-face)	; ### Headers
   (cons "^\\*[\\*\s]*$" 'font-lock-function-name-face) ; * * * style HRs
   (cons "^-[-\s]*$" 'font-lock-function-name-face)	; - - - style HRs
   ;; Blockquotes
   (cons "^>.*$" 'font-lock-comment-face)            ; > blockquote
   ;; Bold
   (cons "[^\\]?\\*\\*.+?\\*\\*" 'font-lock-type-face)     ; **bold**
   (cons "[^\\]?__.+?__" 'font-lock-type-face)             ; __bold__
   ;; Italic
   (cons "[^\\]?\\*.+?\\*" 'font-lock-variable-name-face)  ; *italic*
   (cons "[^\\]?_.+?_" 'font-lock-variable-name-face)      ; _italic_
   ;; Lists
   (cons "^[0-9]+\\." 'font-lock-variable-name-face) ; Numbered list
   (cons "^\\*" 'font-lock-variable-name-face)	     ; Level 1 (no indent)
   (cons "^\\+" 'font-lock-variable-name-face)	     ; Level 1 (no indent)
   (cons "^\\-" 'font-lock-variable-name-face)	     ; Level 1 (no indent)
   (cons "^  [ ]*\\*" 'font-lock-variable-name-face) ; Level 2 (two or more)
   (cons "^  [ ]*\\+" 'font-lock-variable-name-face) ; Level 2 (two or more)
   (cons "^  [ ]*\\-" 'font-lock-variable-name-face) ; Level 2 (two or more)
   ;; Links
   (cons regex-link-inline '(1 'font-lock-string-face t))
   (cons regex-link-inline '(2 'font-lock-constant-face t))
   (cons regex-link-reference '(1 'font-lock-string-face t))
   (cons regex-link-reference '(2 'font-lock-comment-face t))
   (cons regex-reference-definition '(1 'font-lock-comment-face t))
   (cons regex-reference-definition '(2 'font-lock-constant-face t))
   ;; Wiki links
;   (cons "\\[\\[\\w+\\]\\]" 'font-lock-string-face)  ; Standard wiki link
   (cons "\\[\\[.+\\]\\]" 'font-lock-string-face)
   ;; Code
   (cons "``.+?``" 'font-lock-constant-face)         ; ``inline code``
   (cons "`.+?`" 'font-lock-constant-face)           ; `inline code`
   (cons "^    .*$" 'font-lock-constant-face)        ;     code block
   )
  "Syntax highlighting for Markdown files.")


;;; Element Insertion ==========================================================

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
    (dotimes (count (- markdown-hr-length 1) hr)	; Count to n - 1
      (setq hr (concat "* " hr)))	                ; Build HR string
    (setq hr (concat hr "*\n"))				; Add the n-th *
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
  (unless n				; Test to see if n is defined
    (setq n 1))				; Default to level 1 header
  (let (hdr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))	; Build a ### header string
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
	  (setq hdr (concat "=" hdr)))	; Build a === title underline
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
	  (setq hdr (concat "-" hdr)))	; Build a --- section underline
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
      (replace-regexp "^" "> ")))


;; Mode definition  ===========================================================

(define-derived-mode markdown-mode fundamental-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t))

;(add-to-list 'auto-mode-alist '("\\.mdml$" . markdown-mode))

(provide 'markdown-mode)

;;; Change log
;; 2007-05-29 Jason Blevins <jrblevin@sdf.lonestar.org>
;;   * Added support for equals and dash style headings.
;;   * Added markdown-show-version.
;;   * Ability to preview markdown output in a buffer (markdown) or
;;     in a browser (markdown-preview).  Markdown command is customizable.
;;   * Made HR length customizable through markdown-hr-length.
;;   * Made bold and italic style customizable through markdown-bold-underscore
;;     and markdown-italic-underscore.
;;   * Made keybindings more like those of html-helper-mode.
;;   * Added image insertion (markdown-insert-image).
;;   * Font lock for code fragments with double backticks.
;;   * Added blockquote-region function and insert-blockquote keybinding.
;;   * Don't highlight escaped literals such as \* or \_.
;;   * Added header insertion commands for H1-H5 (markdown-insert-header-n).
;;
;; 2007-05-25 Jason Blevins <jrblevin@sdf.lonestar.org>
;;   * Added element insertion commands and keys for links, horizontal rules,
;;     headers, inline code, and bold and italic text.
;;   * Revision 1.2.
;;
;; 2007-05-24 Jason Blevins <jrblevin@sdf.lonestar.org>
;;   * Initial revision.
;;   * Basic syntax highlighting support.
;;   * Revision 1.1.

;;; markdown-mode.el ends here
