;;; markdown-mode-expansions.el --- Expansions for expand-region to be used in markdown-mode

;; Copyright (C) 2017 Jason R. Blevins

;; Author: Jason R. Blevins <jrblevin@sdf.org>
;; Keywords: marking region

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

;;; Code:

(require 'expand-region-core)

(declare-function markdown-mark-page "markdown-mode")
(declare-function markdown-mark-block "markdown-mode")

(defun er/mark-markdown-parent ()
  "Marks a heading 1 level up from current subheading"
  (interactive)
  (mark-defun)
  (markdown-back-to-heading))

(defun er/add-markdown-mode-expansions ()
  "Adds markdown-specific expansions for buffers in org-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append
        er/try-expand-list
        '(er/markdown-mark-parent
          markdown-mark-block
          markdown-mark-page))))

(er/enable-mode-expansions 'markdown-mode 'er/add-markdown-mode-expansions)

(provide 'markdown-mode-expansions)
