;;; markdown-indent.el --- Dynamic indentation for Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2026 Jason R. Blevins and markdown-mode
;; contributors (see the commit log for details).

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

;; Dynamic virtual indentation for Markdown, similar to org-indent-mode.
;; Indents content based on heading level and hides leading hash symbols.
;;
;; Usage:
;;   (require 'markdown-indent)
;;   (add-hook 'markdown-mode-hook 'markdown-indent-mode)
;;
;; See the README.md file for details.

;;; Code:

(defgroup markdown-indent nil
  "Options concerning dynamic virtual outline indentation for Markdown."
  :tag "Markdown Indent"
  :group 'markdown)

(defface markdown-indent '((t (:inherit default)))
  "Face for outline indentation.
The default is to make it look like whitespace."
  :group 'markdown-indent)

(defface markdown-indent-hide-hash
  '((((background light)) (:foreground "white"))
    (((background dark)) (:foreground "black"))
    (t (:inherit default)))
  "Face used to hide leading hash symbols in headings.
The foreground color matches the background to make hashes invisible."
  :group 'markdown-indent)


(defconst markdown-indent--heading-regexp "^\\(#+\\)[ \t]+"
  "Regular expression matching Markdown headings.")

(defconst markdown-indent--fence-regexp "^\\(`\\{3,\\}\\|~\\{3,\\}\\)"
  "Regular expression matching the start or end of a code fence.")

(defconst markdown-indent--hide-hash-keywords
  '((markdown-indent--search-heading-hashes
     (1 'markdown-indent-hide-hash prepend)
     (2 nil prepend)))
  "Font-lock keywords to hide leading hash symbols outside code fences.")

(defun markdown-indent--state-at (pos)
  "Return (LEVEL . IN-FENCE) representing the indentation state at POS.
Scans forward from `point-min' tracking both heading level and fence state."
  (save-excursion
    (save-match-data
      (let ((level 0)
            (in-fence nil))
        (goto-char (point-min))
        (while (< (point) pos)
          (when (looking-at markdown-indent--fence-regexp)
            (setq in-fence (not in-fence)))
          (when (and (not in-fence) (looking-at markdown-indent--heading-regexp))
            (setq level (length (match-string 1))))
          (forward-line))
        (cons level in-fence)))))

(defun markdown-indent--search-heading-hashes (limit)
  "Font-lock search function: find heading hashes outside code fences."
  (let ((in-fence (cdr (markdown-indent--state-at (point))))
        found)
    (while (and (not found) (< (point) limit))
      (cond
       ((looking-at markdown-indent--fence-regexp)
        (setq in-fence (not in-fence))
        (forward-line))
       ((and (not in-fence) (looking-at "^\\(#*\\)\\(#\\)[ \t]"))
        (goto-char (match-end 0))
        (setq found t))
       (t (forward-line))))
    found))

(defun markdown-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
                          '(line-prefix nil wrap-prefix nil) string)
  string)

(defun markdown-indent--next-heading-pos (pos)
  "Return the position of the next heading outside a fence after POS.
Return `point-max' if no such heading exists."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (forward-line 1)
      (let ((in-fence (cdr (markdown-indent--state-at (point)))))
        (catch 'found
          (while (not (eobp))
            (when (looking-at markdown-indent--fence-regexp)
              (setq in-fence (not in-fence)))
            (when (and (not in-fence) (looking-at markdown-indent--heading-regexp))
              (throw 'found (line-beginning-position)))
            (forward-line))
          (point-max))))))

(defun markdown-indent-set-line-properties (level indentation)
  "Set prefix properties on current line at LEVEL with wrap INDENTATION.
Advance to the next line."
  (let* ((line (propertize (make-string (* 2 level) ?\s) 'face 'markdown-indent))
         (wrap (propertize (make-string (+ (* 2 level) indentation) ?\s) 'face 'markdown-indent)))
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
                         `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))

(defun markdown-indent-add-properties (beg end)
  "Add indentation properties between BEG and END."
  (save-match-data
    (save-excursion
      (goto-char beg)
      (forward-line 0)
      (let* ((state (markdown-indent--state-at beg))
             (level (car state))
             (in-fence (cdr state)))
        (with-silent-modifications
          (while (and (<= (point) end) (not (eobp)))
            (when (looking-at markdown-indent--fence-regexp)
              (setq in-fence (not in-fence)))
            (cond
             ((and (not in-fence) (looking-at markdown-indent--heading-regexp))
              (let* ((nstars (length (match-string 1)))
                     (prefix (propertize (make-string (1- nstars) ?\s) 'face 'markdown-indent)))
                (add-text-properties (line-beginning-position) (line-beginning-position 2)
                                     `(line-prefix ,prefix wrap-prefix ,prefix))
                (setq level nstars)
                (forward-line)))
             ((zerop level)
              (remove-text-properties (line-beginning-position) (line-beginning-position 2)
                                      '(line-prefix nil wrap-prefix nil))
              (forward-line))
             (t
              (markdown-indent-set-line-properties level (current-indentation))))))))))

(defun markdown-indent-refresh-maybe (beg end _)
  "Refresh indentation properties after a buffer change.
This function is meant to be called by `after-change-functions'."
  (when markdown-indent-mode
    (markdown-indent-add-properties beg (markdown-indent--next-heading-pos end))))

(defun markdown-indent-indent-buffer ()
  "Add indentation properties to the accessible part of the buffer."
  (interactive)
  (message "Setting buffer indentation...")
  (markdown-indent-add-properties (point-min) (point-max))
  (message "Indentation of buffer set."))

;;;###autoload
(define-minor-mode markdown-indent-mode
  "When active, indent text according to Markdown outline structure.

Internally this works by adding `line-prefix' and `wrap-prefix'
properties, after each buffer modification, on the modified zone."
  :lighter " MdInd"
  (cond
   (markdown-indent-mode
    (setq-local indent-tabs-mode nil)
    (font-lock-add-keywords nil markdown-indent--hide-hash-keywords 'append)
    (when font-lock-mode (font-lock-flush))
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'markdown-indent-remove-properties-from-string)
    (add-hook 'after-change-functions 'markdown-indent-refresh-maybe nil 'local)
    (markdown-indent-add-properties (point-min) (point-max)))
   (t
    (font-lock-remove-keywords nil markdown-indent--hide-hash-keywords)
    (when font-lock-mode (font-lock-flush))
    (remove-function (local 'filter-buffer-substring-function)
                     #'markdown-indent-remove-properties-from-string)
    (remove-hook 'after-change-functions 'markdown-indent-refresh-maybe 'local)
    (save-excursion
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max) '(line-prefix nil wrap-prefix nil))))
    (redraw-display))))

(provide 'markdown-indent)

;;; markdown-indent.el ends here
