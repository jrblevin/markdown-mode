;;; markdown-mode.el --- Major mode to edit Markdown files in Emacs

;; Author: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Maintainer: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Created: May 24, 2007
;; $Id: markdown-mode.el,v 1.2 2007/05/25 15:47:20 jrblevin Exp $
;; Keywords: Markdown major mode

;; Copyright (C) 2007 Jason Blevins

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

;; Supported Emacsen:
;; ==================
;; This mode has only been tested on Emacs 22.0.  Please let me know
;; if there are problems on other versions.

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

;; Description:
;; ============
;; This mode provides basic syntax highlighting and element insertion
;; commands for Markdown files.  The latest version should be available
;; from http://jrblevin.freeshell.org/software/markdown-mode.

;; Changelog:
;; ==========
;; Revision 1.2:
;;   * Added element insertion commands and keys for links, horizontal rules,
;;     headers, inline code, and bold and italic text.
;;
;; Revision 1.1:
;;   * Initial revision.
;;   * Basic syntax highlighting support.

;; TODO:
;; =====
;; * Font locking for equation references: (eq:reference) or \eqref{reference}.
;; * Separate font locking for \label{} elements in side \[ \] equations.
;; * Handle inline HTML better.
;; * Don't highlight escaped literals such as \* or \_.
;; * Allow for code fragments with double backticks.
;; * Add customizable variables for HR style and length, bold and
;;   italic style, etc.

(defconst markdown-mode-version "$Revision: 1.2 $")

;; A hook for users to run their own code when the mode is loaded.
(defvar markdown-mode-hook nil)

;;-------------------------------------
;; Regular expressions

;; Links
(defconst regex-link-inline "\\(!?\\[.+?\\]\\)\\((.*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)")
(defconst regex-link-reference "\\(!?\\[.+?\\]\\)[ ]?\\(\\[.*?\\]\\)"
  "Regular expression for a reference link [text][id]")
(defconst regex-reference-definition
  "^\s*\\(\\[.+?\\]\\):\s*\\([^\s\n]+\\).*$"
  "Regular expression for a link definition [id]: ...")

;;-------------------------------------
;; Font lock

(defconst markdown-mode-font-lock-keywords
  (list
   ;; Latex
;   (cons "\\$[^$]+\\$" 'font-lock-string-face)
;   (cons "\\$\\$\n.*\n\\$\\$" 'font-lock-string-face)
   ;; Headers and (Horizontal Rules)
   (cons ".*\n?===*" 'font-lock-builtin-face)        ; === headers
   (cons ".*\n?---*" 'font-lock-builtin-face)        ; --- headers
   (cons "^#+ .*$" 'font-lock-builtin-face)	     ; ### Headers
   (cons "^\\*[\\*\s]*$" 'font-lock-builtin-face)    ; * * * style HRs
   (cons "^-[-\s]*$" 'font-lock-builtin-face)	     ; - - - style HRs
   ;; Blockquotes
   (cons "^>.*$" 'font-lock-comment-face)            ; > blockquote
   ;; Bold
   (cons "\\*\\*.+?\\*\\*" 'font-lock-type-face)     ; **bold**
   (cons "__.+?__" 'font-lock-type-face)             ; __bold__
   ;; Italic
   (cons "\\*.+?\\*" 'font-lock-variable-name-face)  ; *italic*
   (cons "_.+?_" 'font-lock-variable-name-face)      ; _italic_
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
   (cons "`.+?`" 'font-lock-constant-face)           ; `inline code`
   (cons "^    .*$" 'font-lock-constant-face)        ;     code block
   )
  "Syntax highlighting for Markdown files.")

;;-------------------------------------
;; Element Insertion

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
  (insert "* * * * *\n"))

(defun markdown-insert-bold ()
  "Make the active region bold or insert an empty bold word."
  (interactive)
  (wrap-or-insert "**" "**")
  (backward-char 2))

(defun markdown-insert-italic ()
  "Make the active region italic or insert an empty italic word."
  (interactive)
  (wrap-or-insert "*" "*")
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

(defun markdown-insert-header (n)
  "Creates a level n header.  If there is an active region, it is used as the
header text."
  (interactive "P")
  (unless n				; Test to see if n is defined
    (setq n 1))				; Default to level 1 header
  (let (hdr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))	; Build a ### header string
    (setq hdrl (concat hdr " "))
    (setq hdrr (concat " " hdr))
    (wrap-or-insert hdrl hdrr))
  (backward-char (+ 1 n)))

;;-------------------------------------
;; Keymap

(defvar markdown-mode-map
  (let ((markdown-mode-map (make-keymap)))
    (define-key markdown-mode-map "\C-cl" 'markdown-insert-link)
    (define-key markdown-mode-map "\C-ch" 'markdown-insert-header)
    (define-key markdown-mode-map "\C-cb" 'markdown-insert-bold)
    (define-key markdown-mode-map "\C-ci" 'markdown-insert-italic)
    (define-key markdown-mode-map "\C-cc" 'markdown-insert-code)
    (define-key markdown-mode-map "\C-cr" 'markdown-insert-hr)
    markdown-mode-map)
  "Keymap for Markdown major mode")

;;-------------------------------------
;; Mode definition

(define-derived-mode markdown-mode fundamental-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t))

;(add-to-list 'auto-mode-alist '("\\.mdml$" . markdown-mode))

(provide 'markdown-mode)

;; end markdown-mode.el
