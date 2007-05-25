;;; markdown-mode.el --- Major mode to edit Markdown files in Emacs

;; Author: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Maintainer: Jason Blevins <jrblevin@sdf.lonestar.org>
;; Created: May 24, 2007
;; $Id: markdown-mode.el,v 1.1 2007/05/25 15:46:44 jrblevin Exp $
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
;; * Element insertion commands.

(defconst markdown-mode-version "$Revision: 1.1 $")

;;;-------------------------------------
;;; Regular expressions

;;; Links
(defconst regex-link-inline "\\(!?\\[.+?\\]\\)\\((.+?)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)")
(defconst regex-link-reference "\\(!?\\[.+?\\]\\)[ ]?\\(\\[.*?\\]\\)"
  "Regular expression for a reference link [text][id]")
(defconst regex-reference-definition
  "^\s*\\(\\[.+?\\]\\):\s*\\([^\s\n]+\\).*$"
  "Regular expression for a link definition [id]: ...")

;;;-------------------------------------
;;; Font lock

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

;;;-------------------------------------
;;; Mode definition

(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t))

(provide 'markdown-mode)

;;; end markdown-mode.el
