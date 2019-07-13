;;; markdown-changelog.el --- maintain changelog entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Paul Landes

;; Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: markdown changelog
;; URL: https://github.com/plandes/markdown-changelog
;; Package-Requires: ((emacs "26") (dash "2.13.0"))

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

;; Create and maintain Keep a Changelog based entries.
;; See http://keepachangelog.com/ for this specific change log format.  A
;; nascent changelog is created with `markdown-changelog-new' and
;; `markdown-changelog-add-release' is used to add a new entry.

;;; Code:

(require 'subr-x)
(require 'dash)

(defvar markdown-changelog-buffer-name "CHANGELOG.md"
  "The default buffer name for the changelog when not the current buffer.")

(defvar markdown-changelog-version-prefix "v"
  "The prefix of the version, which defaults to `v'.")

(defvar markdown-changelog-unreleased-entry-regexp
  "^#+[ \t]*\\[Unreleased\\]$"
  "The regular expression of the unreleased entry \(near top).")

(defvar markdown-changelog-unreleased-regex-format
  "^\\[Unreleased\\]: \\(.+\\)\\/%s\\(.+\\)\\.\\.\\.HEAD$"
  "The format of the regular expression for the unlreleased link entry.")

(defvar markdown-changelog-release-regex
  "^\\[%s\\]: \\(.+\\)$"
  "The regular expression link entries in the change log.")

(defvar markdown-changelog-release-format
  "[%s]: %s/%s%s...%s%s"
  "The format to use to generate release link entries.")

(defun markdown-changelog-date-string ()
  "Return a year, month, date format that confirms with `Keep a Changelog'."
  (format-time-string "%Y-%m-%d"))

(defun markdown-changelog-first-sha (&optional default)
  "Return the first SHA commit of the repo dervied from `default-directory'.
If the version can not be determined, return DEFAULT or raise an error."
  (let ((res (->> (shell-command-to-string "git rev-list --max-parents=0 HEAD")
		  string-trim)))
    (if (string-match "^fatal" res)
	(or default (error "Can't determine git first SHA: %s" res))
      res)))

;;;###autoload
(defun markdown-changelog-new (url)
  "Create a new changelog buffer with project URL."
  (interactive "sProject URL: ")
  (if (string-match "\\/$" url)
      (setq url (substring url 0 (1- (length url)))))
  (setq url (format "%s/current/" url))
  (let ((first-content (-> "\
# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]

## [0.1] - %s\n"
			   (format (markdown-changelog-date-string))))
	(second-content (-> "\
\n\n### Added
- Initial version


<!-- links -->
\[Unreleased]: %s%s0.0.1...HEAD
\[0.0.1]: %s%s%s...%s0.0.1\n"
			 (format url
				 markdown-changelog-version-prefix
				 url
				 markdown-changelog-version-prefix
				 (markdown-changelog-first-sha "FIRST_HASH")
				 markdown-changelog-version-prefix)))
	(buf (generate-new-buffer markdown-changelog-buffer-name))
	pos)
    (with-current-buffer buf
      (insert first-content)
      (setq pos (point))
      (insert second-content)
      (goto-char pos)
      (if (fboundp 'markdown-mode)
	  (markdown-mode)))
    (display-buffer buf)))

(defun markdown-changelog-unreleased-regex ()
  "Return the regular expression for the unrleased link entry."
  (format markdown-changelog-unreleased-regex-format
	  markdown-changelog-version-prefix))

(defun markdown-changelog-buffer ()
  "Return the changelog buffer."
  (if (eq major-mode 'markdown-mode)
      (current-buffer)
    (get-buffer markdown-changelog-buffer-name)))

(defun markdown-changelog-release-info ()
  "Return the latest version using the `Unreleased' link entry."
  (with-current-buffer (markdown-changelog-buffer)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp (markdown-changelog-unreleased-regex) nil t)
	  (->> '(1 2)
	       (-map #'match-string)
	       (-map #'substring-no-properties)
	       (append (list (point))))
	(error "Couldn't find the [Unreleased] entry")))))

(defun markdown-changelog-version-increment (ver)
  "Increment the minor version of string VER."
  (if (string-match "^\\(.*\\.?\\)\\([0-9]+\\)$" ver)
      (concat (match-string 1 ver)
	      (->> (match-string 2 ver)
		   string-to-number
		   1+
		   prin1-to-string))
    ver))

;;;###autoload
(defun markdown-changelog-insert-release ()
  "Add a new release to the change log."
  (interactive)
  (let* ((rinfo (markdown-changelog-release-info))
	 (pos (cl-first rinfo))
	 (url (cl-second rinfo))
	 (prev-ver (cl-third rinfo))
	 (next-ver (markdown-changelog-version-increment prev-ver))
	 (regex (format markdown-changelog-release-regex prev-ver))
	 user-pos)
    (with-current-buffer (markdown-changelog-buffer)
      (save-excursion
	(goto-char (point-min))
	(when (search-forward-regexp markdown-changelog-unreleased-entry-regexp
				     nil t)
	  (insert "\n\n\n"))
	(insert (format "## [%s] - %s" next-ver
			(markdown-changelog-date-string)))
	(beginning-of-line)
	(forward-line 1)
	(setq user-pos (point))
	(goto-char (point-min))
	(if (search-forward-regexp regex nil t)
	    (substring-no-properties (match-string 1))
	  (error "Couldn't find release %s" prev-ver))
	(beginning-of-line)
	(insert (format markdown-changelog-release-format
			next-ver url
			markdown-changelog-version-prefix
			prev-ver
			markdown-changelog-version-prefix
			next-ver))
	(newline)
	(goto-char pos)
	(beginning-of-line)
	(kill-line)
	(insert (format markdown-changelog-release-format
			"Unreleased" url
			markdown-changelog-version-prefix
			next-ver "" "HEAD")))
      (goto-char user-pos))))

(provide 'markdown-changelog)

;;; markdown-changelog.el ends here
