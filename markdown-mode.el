;;; markdown-mode.el --- Emacs Major mode for Markdown-formatted text files

;; Copyright (C) 2007-2013 Jason R. Blevins <jrblevin@sdf.org>
;; Copyright (C) 2007, 2009 Edward O'Connor <ted@oconnor.cx>
;; Copyright (C) 2007 Conal Elliott <conal@conal.net>
;; Copyright (C) 2008 Greg Bognar <greg_bognar@hms.harvard.edu>
;; Copyright (C) 2008 Dmitry Dzhus <mail@sphinx.net.ru>
;; Copyright (C) 2008 Bryan Kyle <bryan.kyle@gmail.com>
;; Copyright (C) 2008 Ben Voui <intrigeri@boum.org>
;; Copyright (C) 2009 Ankit Solanki <ankit.solanki@gmail.com>
;; Copyright (C) 2009 Hilko Bengen <bengen@debian.org>
;; Copyright (C) 2009 Peter Williams <pezra@barelyenough.org>
;; Copyright (C) 2010 George Ogata <george.ogata@gmail.com>
;; Copyright (C) 2011 Eric Merritt <ericbmerritt@gmail.com>
;; Copyright (C) 2011 Philippe Ivaldi <pivaldi@sfr.fr>
;; Copyright (C) 2011 Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Copyright (C) 2011 Christopher J. Madsen <cjm@cjmweb.net>
;; Copyright (C) 2011 Shigeru Fukaya <shigeru.fukaya@gmail.com>
;; Copyright (C) 2011 Joost Kremers <joostkremers@fastmail.fm>
;; Copyright (C) 2011-2012 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012 Akinori Musha <knu@idaemons.org>
;; Copyright (C) 2012 Zhenlei Jia <zhenlei.jia@gmail.com>
;; Copyright (C) 2012 Peter Jones <pjones@pmade.com>

;; Author: Jason R. Blevins <jrblevin@sdf.org>
;; Maintainer: Jason R. Blevins <jrblevin@sdf.org>
;; Created: May 24, 2007
;; Version: 1.9
;; Keywords: Markdown, GitHub Flavored Markdown, itex
;; URL: http://jblevins.org/projects/markdown-mode/

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

;; markdown-mode is a major mode for editing [Markdown][]-formatted
;; text files in GNU Emacs.  markdown-mode is free software, licensed
;; under the GNU GPL.
;;
;;  [Markdown]: http://daringfireball.net/projects/markdown/
;;
;; The latest stable version is markdown-mode 1.9, released on January 25, 2013:
;;
;;    * [markdown-mode.el][]
;;    * [Screenshot][]
;;    * [Release notes][]
;;
;;  [markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el
;;  [screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20110812-001.png
;;  [release notes]: http://jblevins.org/projects/markdown-mode/rev-1-9
;;
;; markdown-mode is also available in several package managers, including:
;;
;;    * Debian and Ubuntu Linux: [emacs-goodies-el][]
;;    * RedHat and Fedora Linux: [emacs-goodies][]
;;    * OpenBSD: [textproc/markdown-mode][]
;;    * Arch Linux (AUR): [emacs-markdown-mode-git][]
;;    * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
;;
;;  [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
;;  [emacs-goodies]: https://admin.fedoraproject.org/pkgdb/acls/name/emacs-goodies
;;  [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
;;  [emacs-markdown-mode-git]: http://aur.archlinux.org/packages.php?ID=30389
;;  [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
;;  [macports-ticket]: http://trac.macports.org/ticket/35716
;;
;; The latest development version can be downloaded directly
;; ([markdown-mode.el][devel.el]) or it can be obtained from the
;; (browsable and clonable) Git repository at
;; <http://jblevins.org/git/markdown-mode.git>.  The entire repository,
;; including the full project history, can be cloned via the Git protocol
;; by running
;;
;;     git clone git://jblevins.org/git/markdown-mode.git
;;
;;  [devel.el]: http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el

;;; Dependencies:

;; markdown-mode requires easymenu, a standard package since GNU Emacs
;; 19 and XEmacs 19, which provides a uniform interface for creating
;; menus in GNU Emacs and XEmacs.

;;; Installation:

;; Make sure to place `markdown-mode.el` somewhere in the load-path and add
;; the following lines to your `.emacs` file to associate markdown-mode
;; with `.text` files:
;;
;;     (autoload 'markdown-mode "markdown-mode"
;;        "Major mode for editing Markdown files" t)
;;     (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;;
;; There is no consensus on an official file extension so change `.text` to
;; `.mdwn`, `.md`, `.mdt`, or whatever you call your markdown files.

;;; Customization:

;; Although no configuration is *necessary* there are a few things
;; that can be customized.  The `M-x customize-mode` command
;; provides an interface to all of the possible customizations:
;;
;;   * `markdown-command' - the command used to run Markdown (default:
;;     `markdown').  This variable may be customized to pass
;;     command-line options to your Markdown processor of choice, but
;;     this command must accept input from `stdin`.  If it does not, a
;;     simple wrapper script can be used to write `stdin` to a file
;;     and then pass that file to your Markdown interpreter.  Ideally,
;;     this command will produce an XHTML fragment around which
;;     markdown-mode will wrap a header and footer (which can be
;;     further customized).  However, it attempts to detect whether
;;     the command produces standalone XHTML output (via
;;     `markdown-xhtml-standalone-regexp'), in which case no header
;;     and footer content will be added.
;;
;;   * `markdown-command-needs-filename' - set to non-nil if
;;     `markdown-command' does not accept input from stdin (default: nil).
;;      Instead, it will be passed a filename as the final command-line
;;      option.  As a result, you will only be able to run Markdown
;;      from buffers which are visiting a file.
;;
;;   * `markdown-open-command' - the command used for calling a standalone
;;     Markdown previewer which is capable of opening Markdown source files
;;     directly (default: `nil').  This command will be called
;;     with a single argument, the filename of the current buffer.
;;     A representative program is the Mac app [Marked][], a
;;     live-updating MultiMarkdown previewer which has a command line
;;     utility at `/usr/local/bin/mark`.
;;
;;   * `markdown-hr-string' - string to use when inserting horizontal
;;     rules (default: `* * * * *').
;;
;;   * `markdown-bold-underscore' - set to a non-nil value to use two
;;     underscores for bold instead of two asterisks (default: `nil').
;;
;;   * `markdown-italic-underscore' - set to a non-nil value to use
;;     underscores for italic instead of asterisks (default: `nil').
;;
;;   * `markdown-indent-function' - the function to use for automatic
;;     indentation (default: `markdown-indent-line').
;;
;;   * `markdown-indent-on-enter' - set to a non-nil value to
;;     automatically indent new lines when the enter key is pressed
;;     (default: `t')
;;
;;   * `markdown-follow-wiki-link-on-enter' - set to a non-nil value
;;     to automatically open a linked document in a new buffer if the
;;     cursor is an wiki link
;;     (default: `t')
;;
;;   * `markdown-wiki-link-alias-first' - set to a non-nil value to
;;     treat aliased wiki links like `[[link text|PageName]]`.
;;     When set to nil, they will be treated as `[[PageName|link text]]'.
;;
;;   * `markdown-uri-types' - a list of protocols for URIs that
;;     `markdown-mode' should highlight.
;;
;;   * `markdown-enable-math' - syntax highlighting for
;;     LaTeX fragments (default: `nil').
;;
;;   * `markdown-css-path' - CSS file to link to in XHTML output.
;;
;;   * `markdown-content-type' - when set to a nonempty string, an
;;     `http-equiv` attribute will be included in the XHTML `<head>`
;;     block.  If needed, the suggested values are
;;     `application/xhtml+xml` or `text/html`.
;;
;;   * `markdown-coding-system' - used for specifying the character
;;     set identifier in the `http-equiv` attribute (see
;;     `markdown-content-type').  When set to `nil',
;;     `buffer-file-coding-system' will be used (and falling back to
;;     `iso-8859-1' when unavailable).  Common settings are `utf-8'
;;     and `iso-latin-1'.
;;
;;   * `markdown-xhtml-header-content' - additional content to include
;;     in the XHTML `<head>` block.
;;
;;   * `markdown-xhtml-standalone-regexp' - a regular expression which
;;     indicates whether the output of `markdown-command' is standalone
;;     XHTML (default: `^\\(\<\?xml\\|\<!DOCTYPE\\|\<html\\)`).  If
;;     this is not matched, we assume this output is a fragment and add
;;     our own header and footer.
;;
;;   * `markdown-link-space-sub-char' - a character to replace spaces
;;     when mapping wiki links to filenames (default: `_`).
;;     For example, use an underscore for compatibility with the
;;     Python Markdown WikiLinks extension or a hyphen for compatibility
;;     with Github wiki links.
;;
;; Additionally, the faces used for syntax highlighting can be modified to
;; your liking by issuing `M-x customize-group RET markdown-faces`
;; or by using the "Markdown Faces" link at the bottom of the mode
;; customization screen.
;;
;; [Marked]: https://itunes.apple.com/us/app/marked/id448925439?ls=1&mt=12&partnerId=30&siteID=GpHp3Acs1Yo

;;; Usage:

;; Keybindings are grouped by prefixes based on their function.  For
;; example, commands dealing with headers begin with `C-c C-t`.  The
;; primary commands in each group will are described below.  You can
;; obtain a list of all keybindings by pressing `C-c C-h`.
;;
;;   * Anchors: `C-c C-a`
;;
;;     `C-c C-a l` inserts inline links of the form `[text](url)`.
;;     `C-c C-a r` inserts reference links of the form `[text][label]`.
;;     The label definition will be placed at the end of the current
;;     block. `C-c C-a w` acts similarly for wiki links of the form
;;     `[[WikiLink]]`. In all cases, if there is an active region, the
;;     text in the region is used as the link text.
;;
;;   * Commands: `C-c C-c`
;;
;;     *Compile:* `C-c C-c m` will run Markdown on the current buffer
;;     and show the output in another buffer.  *Preview*: `C-c C-c p`
;;     runs Markdown on the current buffer and previews, stores the
;;     output in a temporary file, and displays the file in a browser.
;;     *Export:* `C-c C-c e` will run Markdown on the current buffer
;;     and save the result in the file `basename.html`, where
;;     `basename` is the name of the Markdown file with the extension
;;     removed.  *Export and View:* press `C-c C-c v` to export the
;;     file and view it in a browser.  **For both export commands, the
;;     output file will be overwritten without notice.**
;;     *Open:* `C-c C-c o` will open the Markdown source file directly
;;     using `markdown-open-command'.
;;
;;     To summarize:
;;
;;       - `C-c C-c m`: `markdown-command' > `*markdown-output*` buffer.
;;       - `C-c C-c p`: `markdown-command' > temporary file > browser.
;;       - `C-c C-c e`: `markdown-command' > `basename.html`.
;;       - `C-c C-c v`: `markdown-command' > `basename.html` > browser.
;;       - `C-c C-c o`: `markdown-open-command'.
;;
;;     `C-c C-c c` will check for undefined references.  If there are
;;     any, a small buffer will open with a list of undefined
;;     references and the line numbers on which they appear.  In Emacs
;;     22 and greater, selecting a reference from this list and
;;     pressing `RET` will insert an empty reference definition at the
;;     end of the buffer.  Similarly, selecting the line number will
;;     jump to the corresponding line.
;;
;;     `C-c C-c n` will clean up the numbering of ordered lists.

;;   * Images: `C-c C-i`
;;
;;     `C-c C-i i` inserts an image, using the active region (if any)
;;     as the alt text.
;;
;;   * Physical styles: `C-c C-p`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  `C-c C-p b` makes
;;     the selected text bold, `C-c C-p f` formats the region as
;;     fixed-width text, and `C-c C-p i` is used for italic text.
;;
;;   * Logical styles: `C-c C-s`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  Logical styles
;;     include blockquote (`C-c C-s b`), preformatted (`C-c C-s p`),
;;     code (`C-c C-s c`), emphasis (`C-c C-s e`), and strong
;;     (`C-c C-s s`).
;;
;;   * Headers: `C-c C-t`
;;
;;     All header commands use text in the active region, if any, as
;;     the header text.  To insert an atx or hash style level-n
;;     header, press `C-c C-t n` where n is between 1 and 6.  For a
;;     top-level setext or underline style header press `C-c C-t t`
;;     (mnemonic: title) and for a second-level underline-style header
;;     press `C-c C-t s` (mnemonic: section).
;;
;;   * Footnotes: `C-c C-f`
;;
;;     To create a new footnote at the point, press `C-c C-f n`.
;;     Press `C-c C-f g` with the point at a footnote to jump to the
;;     location where the footnote text is defined.  Then, press
;;     `C-c C-f b` to return to the footnote marker in the main text.
;;     When the point is at a footnote marker or in the body of a
;;     footnote, press `C-c C-f k` to kill the footnote and add the
;;     text to the kill ring.
;;
;;   * Other elements:
;;
;;     `C-c -` inserts a horizontal rule.
;;
;;   * Links:
;;
;;     Press `C-c C-o` when the point is on an inline or reference
;;     link to open the URL in a browser.
;;
;;   * Wiki-Link Navigation:
;;
;;     Use `M-p` and `M-n` to quickly jump to the previous and next
;;     wiki links, respectively.
;;
;;   * Outline Navigation:
;;
;;     Navigation between headings is possible using `outline-mode'.
;;     Use `C-M-n` and `C-M-p` to move between the next and previous
;;     visible headings.  Similarly, `C-M-f` and `C-M-b` move to the
;;     next and previous visible headings at the same level as the one
;;     at the point.  Finally, `C-M-u` will move up to a lower-level
;;     (more inclusive) visible heading.
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
;; `markdown-insert-blockquote' is bound to `C-c C-s b` and only acts on
;; the region in Transient Mark mode while `markdown-blockquote-region'
;; is bound to `C-c C-s C-b` and always applies to the region (when
;; nonempty).
;;
;; markdown-mode attempts to be flexible in how it handles
;; indentation.  When you press `TAB` repeatedly, the point will cycle
;; through several possible indentation levels corresponding to things
;; you might have in mind when you press `RET` at the end of a line or
;; `TAB`.  For example, you may want to start a new list item,
;; continue a list item with hanging indentation, indent for a nested
;; pre block, and so on.
;;
;; markdown-mode supports outline-minor-mode as well as org-mode-style
;; visibility cycling for atx- or hash-style headers.  There are two
;; types of visibility cycling: Pressing `S-TAB` cycles globally between
;; the table of contents view (headers only), outline view (top-level
;; headers only), and the full document view.  Pressing `TAB` while the
;; point is at a header will cycle through levels of visibility for the
;; subtree: completely folded, visible children, and fully visible.
;; Note that mixing hash and underline style headers will give undesired
;; results.

;;; Extensions:

;; Besides supporting the basic Markdown syntax, markdown-mode also
;; includes syntax highlighting for `[[Wiki Links]]` by default. Wiki
;; links may be followed automatically by hitting the enter key when
;; your curser is on a wiki link or by hitting `C-c C-w`. The
;; autofollowing on enter key may be controlled with the
;; `markdown-follow-wiki-link-on-enter' customization.  Use `M-p` and
;; `M-n` to quickly jump to the previous and next wiki links,
;; respectively.  Aliased or piped wiki links of the form
;; `[[link text|PageName]]` are also supported.  Since some wikis
;; reverse these components, set `markdown-wiki-link-alias-first'
;; to nil to treat them as `[[PageName|link text]]`.
;;
;; [SmartyPants][] support is possible by customizing `markdown-command'.
;; If you install `SmartyPants.pl` at, say, `/usr/local/bin/smartypants`,
;; then you can set `markdown-command' to `"markdown | smartypants"`.
;; You can do this either by using `M-x customize-group markdown`
;; or by placing the following in your `.emacs` file:
;;
;;     (defun markdown-custom ()
;;       "markdown-mode-hook"
;;       (setq markdown-command "markdown | smartypants"))
;;     (add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))
;;
;; [SmartyPants]: http://daringfireball.net/projects/smartypants/
;;
;; Experimental syntax highlighting for mathematical expressions written
;; in LaTeX (only expressions denoted by `$..$`, `$$..$$`, or `\[..\]`)
;; can be enabled by setting `markdown-enable-math' to a non-nil value,
;; either via customize or by placing `(setq markdown-enable-itex t)`
;; in `.emacs`, and restarting Emacs.
;;
;; A [GitHub Flavored Markdown](http://github.github.com/github-flavored-markdown/)
;; mode, `gfm-mode', is also available.  The GitHub implementation of
;; differs slightly from standard Markdown.  Most importantly, newlines are
;; significant and trigger hard line breaks.  As such, `gfm-mode' turns off
;; `auto-fill-mode' and turns on `visual-line-mode' (or `longlines-mode' if
;; `visual-line-mode' is not available).  Wiki links in this mode will be
;; treated as on GitHub, with hyphens replacing spaces in filenames and
;; where the first letter of the filename capitalized.  For example,
;; `[[wiki link]]' will map to a file named `Wiki-link` with the same
;; extension as the current file.

;;; Acknowledgments:

;; markdown-mode has benefited greatly from the efforts of the
;; following people:
;;
;;   * Cyril Brulebois <cyril.brulebois@enst-bretagne.fr> for Debian packaging.
;;   * Conal Elliott <conal@conal.net> for a font-lock regexp patch.
;;   * Edward O'Connor <hober0@gmail.com> for a font-lock regexp fix and
;;     GitHub Flavored Markdown mode (`gfm-mode').
;;   * Greg Bognar <greg_bognar@hms.harvard.edu> for menus and running
;;     `markdown' with an active region.
;;   * Daniel Burrows <dburrows@debian.org> for filing Debian bug #456592.
;;   * Peter S. Galbraith <psg@debian.org> for maintaining emacs-goodies-el.
;;   * Dmitry Dzhus <mail@sphinx.net.ru> for reference checking functions.
;;   * Bryan Kyle <bryan.kyle@gmail.com> for indentation code.
;;   * Ben Voui <intrigeri@boum.org> for font-lock face customizations.
;;   * Ankit Solanki <ankit.solanki@gmail.com> for longlines.el
;;     compatibility and custom CSS.
;;   * Hilko Bengen <bengen@debian.org> for proper XHTML output.
;;   * Jose A. Ortega Ruiz <jao@gnu.org> for Emacs 23 fixes.
;;   * Alec Resnick <alec@sproutward.org> for bug reports.
;;   * Joost Kremers <joostkremers@fastmail.fm> for footnote-handling
;;     functions and bug reports regarding indentation.
;;   * Peter Williams <pezra@barelyenough.org> for fill-paragraph
;;     enhancements.
;;   * George Ogata <george.ogata@gmail.com> for fixing several
;;     byte-compilation warnings.
;;   * Eric Merritt <ericbmerritt@gmail.com> for wiki link features.
;;   * Philippe Ivaldi <pivaldi@sfr.fr> for XHTML preview
;;     customizations and XHTML export.
;;   * Jeremiah Dodds <jeremiah.dodds@gmail.com> for supporting
;;     Markdown processors which do not accept input from stdin.
;;   * Werner Dittmann <werner.dittmann@t-online.de> for bug reports
;;     regarding the cl dependency and auto-fill-mode and indentation.
;;   * Scott Pfister <scott.pfister@gmail.com> for generalizing the space
;;     substitution character for mapping wiki links to filenames.
;;   * Marcin Kasperski <marcin.kasperski@mekk.waw.pl> for a patch to
;;     escape shell commands.
;;   * Christopher J. Madsen <cjm@cjmweb.net> for patches to fix a match
;;     data bug and to prefer `visual-line-mode' in `gfm-mode'.
;;   * Shigeru Fukaya <shigeru.fukaya@gmail.com> for better adherence to
;;     Emacs Lisp coding conventions.
;;   * Donald Ephraim Curtis <dcurtis@milkbox.net> for fixing the `paragraph-fill'
;;     regexp, refactoring the compilation and preview functions,
;;     heading font-lock generalizations, list renumbering,
;;     and kill ring save.
;;   * Kevin Porter <kportertx@gmail.com> for wiki link handling in `gfm-mode'.
;;   * Max Penet <max.penet@gmail.com> and Peter Eisentraut <peter_e@gmx.net>
;;     for an autoload token for `gfm-mode'.
;;   * Ian Yang <me@iany.me> for improving the reference definition regex.
;;   * Akinori Musha <knu@idaemons.org> for an imenu index function.
;;   * Michael Sperber <sperber@deinprogramm.de> for XEmacs fixes.
;;   * Francois Gannaz <francois.gannaz@free.fr> for suggesting charset
;;     declaration in XHTML output.
;;   * Zhenlei Jia <zhenlei.jia@gmail.com> for smart (dedention)
;;     un-indentation function.
;;   * Matus Goljer <dota.keys@gmail.com> for improved wiki link following.
;;   * Peter Jones <pjones@pmade.com> for link following functions.
;;   * Bryan Fink <bryan.fink@gmail.com> for a bug report regarding
;;     externally modified files.
;;   * Vegard Vesterheim <vegard.vesterheim@uninett.no> and Carsten Dominik
;;     for a bug fix related to orgtbl-mode.

;;; Bugs:

;; Although markdown-mode is developed and tested primarily using
;; GNU Emacs 24, compatibility with earlier Emacsen is also a
;; priority.
;;
;; If you find any bugs in markdown-mode, please construct a test case
;; or a patch and email me at <jrblevin@sdf.org>.

;;; History:

;; markdown-mode was written and is maintained by Jason Blevins.  The
;; first version was released on May 24, 2007.
;;
;;   * 2007-05-24: Version 1.1
;;   * 2007-05-25: Version 1.2
;;   * 2007-06-05: [Version 1.3][]
;;   * 2007-06-29: Version 1.4
;;   * 2008-05-24: [Version 1.5][]
;;   * 2008-06-04: [Version 1.6][]
;;   * 2009-10-01: [Version 1.7][]
;;   * 2011-08-12: [Version 1.8][]
;;   * 2011-08-15: [Version 1.8.1][]
;;   * 2013-01-25: [Version 1.9][]
;;
;; [Version 1.3]: http://jblevins.org/projects/markdown-mode/rev-1-3
;; [Version 1.5]: http://jblevins.org/projects/markdown-mode/rev-1-5
;; [Version 1.6]: http://jblevins.org/projects/markdown-mode/rev-1-6
;; [Version 1.7]: http://jblevins.org/projects/markdown-mode/rev-1-7
;; [Version 1.8]: http://jblevins.org/projects/markdown-mode/rev-1-8
;; [Version 1.8.1]: http://jblevins.org/projects/markdown-mode/rev-1-8-1
;; [Version 1.9]: http://jblevins.org/projects/markdown-mode/rev-1-9


;;; Code:

(require 'easymenu)
(require 'outline)
(eval-when-compile (require 'cl))

;;; Constants =================================================================

(defconst markdown-mode-version "1.9"
  "Markdown mode version number.")

(defconst markdown-output-buffer-name "*markdown-output*"
  "Name of temporary buffer for markdown command output.")

;;; Customizable variables ====================================================

(defvar markdown-mode-hook nil
  "Hook run when entering Markdown mode.")

(defgroup markdown nil
  "Major mode for editing text files in Markdown format."
  :prefix "markdown-"
  :group 'wp
  :link '(url-link "http://jblevins.org/projects/markdown-mode/"))

(defcustom markdown-command "markdown"
  "Command to run markdown."
  :group 'markdown
  :type 'string)

(defcustom markdown-command-needs-filename nil
  "Set to non-nil if `markdown-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command-line
option.  As a result, you will only be able to run Markdown from
buffers which are visiting a file."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-open-command nil
  "Command used for opening Markdown files directly.
For example, a standalone Markdown previewer.  This command will
be called with a single argument: the filename of the current
buffer."
  :group 'markdown
  :type 'string)

(defcustom markdown-hr-string "* * * * *"
  "String to use for horizonal rules."
  :group 'markdown
  :type 'string)

(defcustom markdown-bold-underscore nil
  "Use two underscores for bold instead of two asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-italic-underscore nil
  "Use underscores for italic instead of asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-indent-function 'markdown-indent-line
  "Function to use to indent."
  :group 'markdown
  :type 'function)

(defcustom markdown-indent-on-enter t
  "Automatically indent new lines when enter key is pressed."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-follow-wiki-link-on-enter t
  "Follow wiki link at point (if any) when the enter key is pressed."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-wiki-link-alias-first t
  "When non-nil, treat aliased wiki links like [[alias text|PageName]].
Otherwise, they will be treated as [[PageName|alias text]]."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'markdown
  :type 'list)

(defcustom markdown-enable-math nil
  "Syntax highlighting for inline LaTeX expressions.
This will not take effect until Emacs is restarted."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-css-path ""
  "URL of CSS file to link to in the output XHTML."
  :group 'markdown
  :type 'string)

(defcustom markdown-content-type ""
  "Content type string for the http-equiv header in XHTML output.
When set to a non-empty string, insert the http-equiv attribute.
Otherwise, this attribute is omitted."
  :group 'markdown
  :type 'string)

(defcustom markdown-coding-system nil
  "Character set string for the http-equiv header in XHTML output.
Defaults to `buffer-file-coding-system' (and falling back to
`iso-8859-1' when not available).  Common settings are `utf-8'
and `iso-latin-1'.  Use `list-coding-systems' for more choices."
  :group 'markdown
  :type 'coding-system)

(defcustom markdown-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'markdown
  :type 'string)

(defcustom markdown-xhtml-standalone-regexp
  "^\\(\<\?xml\\|\<!DOCTYPE\\|\<html\\)"
  "Regexp indicating whether `markdown-command' output is standalone XHTML."
  :group 'markdown
  :type 'regexp)

(defcustom markdown-link-space-sub-char
  "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'markdown
  :type 'string)

(defcustom markdown-footnote-location 'end
  "Position where new footnotes are inserted in the document."
  :group 'markdown
  :type '(choice (const :tag "At the end of the document" end)
		 (const :tag "Immediately after the paragraph" immediately)
		 (const :tag "Before next header" header)))

;;; Font lock =================================================================

(require 'font-lock)

(defvar markdown-italic-face 'markdown-italic-face
  "Face name to use for italic text.")

(defvar markdown-bold-face 'markdown-bold-face
  "Face name to use for bold text.")

(defvar markdown-header-delimiter-face 'markdown-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar markdown-header-rule-face 'markdown-header-rule-face
  "Face name to use as a base for header rules.")

(defvar markdown-header-face 'markdown-header-face
  "Face name to use as a base for headers.")

(defvar markdown-header-face-1 'markdown-header-face-1
  "Face name to use for level-1 headers.")

(defvar markdown-header-face-2 'markdown-header-face-2
  "Face name to use for level-2 headers.")

(defvar markdown-header-face-3 'markdown-header-face-3
  "Face name to use for level-3 headers.")

(defvar markdown-header-face-4 'markdown-header-face-4
  "Face name to use for level-4 headers.")

(defvar markdown-header-face-5 'markdown-header-face-5
  "Face name to use for level-5 headers.")

(defvar markdown-header-face-6 'markdown-header-face-6
  "Face name to use for level-6 headers.")

(defvar markdown-inline-code-face 'markdown-inline-code-face
  "Face name to use for inline code.")

(defvar markdown-list-face 'markdown-list-face
  "Face name to use for list markers.")

(defvar markdown-blockquote-face 'markdown-blockquote-face
  "Face name to use for blockquote.")

(defvar markdown-pre-face 'markdown-pre-face
  "Face name to use for preformatted text.")

(defvar markdown-link-face 'markdown-link-face
  "Face name to use for links.")

(defvar markdown-missing-link-face 'markdown-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar markdown-reference-face 'markdown-reference-face
  "Face name to use for reference.")

(defvar markdown-footnote-face 'markdown-footnote-face
  "Face name to use for footnote identifiers.")

(defvar markdown-url-face 'markdown-url-face
  "Face name to use for URLs.")

(defvar markdown-link-title-face 'markdown-link-title-face
  "Face name to use for reference link titles.")

(defvar markdown-comment-face 'markdown-comment-face
  "Face name to use for HTML comments.")

(defvar markdown-math-face 'markdown-math-face
  "Face name to use for LaTeX expressions.")

(defgroup markdown-faces nil
  "Faces used in Markdown Mode"
  :group 'markdown
  :group 'faces)

(defface markdown-italic-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for italic text."
  :group 'markdown-faces)

(defface markdown-bold-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for bold text."
  :group 'markdown-faces)

(defface markdown-header-rule-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers rules."
  :group 'markdown-faces)

(defface markdown-header-delimiter-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers hash delimiter."
  :group 'markdown-faces)

(defface markdown-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'markdown-faces)

(defface markdown-header-face-1
  '((t (:inherit markdown-header-face)))
  "Face for level-1 headers."
  :group 'markdown-faces)

(defface markdown-header-face-2
  '((t (:inherit markdown-header-face)))
  "Face for level-2 headers."
  :group 'markdown-faces)

(defface markdown-header-face-3
  '((t (:inherit markdown-header-face)))
  "Face for level-3 headers."
  :group 'markdown-faces)

(defface markdown-header-face-4
  '((t (:inherit markdown-header-face)))
  "Face for level-4 headers."
  :group 'markdown-faces)

(defface markdown-header-face-5
  '((t (:inherit markdown-header-face)))
  "Face for level-5 headers."
  :group 'markdown-faces)

(defface markdown-header-face-6
  '((t (:inherit markdown-header-face)))
  "Face for level-6 headers."
  :group 'markdown-faces)

(defface markdown-inline-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'markdown-faces)

(defface markdown-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'markdown-faces)

(defface markdown-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'markdown-faces)

(defface markdown-pre-face
  '((t (:inherit font-lock-constant-face)))
  "Face for preformatted text."
  :group 'markdown-faces)

(defface markdown-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'markdown-faces)

(defface markdown-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'markdown-faces)

(defface markdown-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'markdown-faces)

(defface markdown-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'markdown-faces)

(defface markdown-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'markdown-faces)

(defface markdown-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'markdown-faces)

(defface markdown-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'markdown-faces)

(defface markdown-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'markdown-faces)

(defconst markdown-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst markdown-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ ]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst markdown-regex-reference-definition
  "^ \\{0,3\\}\\(\\[[^^\n]+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst markdown-regex-footnote
  "\\(\\[\\^.+?\\]\\)"
  "Regular expression for a footnote marker [^fn].")

(defconst markdown-regex-header
  "^\\(?:\\(.+\\)\n\\(===+\\)\\|\\(.+\\)\n\\(---+\\)\\|\\(#+\\)\\s-*\\(.*?\\)\\s-*?\\(#*\\)\\)$"
  "Regexp identifying Markdown headers.")

(defconst markdown-regex-header-1-atx
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst markdown-regex-header-2-atx
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst markdown-regex-header-3-atx
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst markdown-regex-header-4-atx
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst markdown-regex-header-5-atx
  "^\\(##### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst markdown-regex-header-6-atx
  "^\\(###### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst markdown-regex-header-1-setext
  "^\\(.*\\)\n\\(===+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst markdown-regex-header-2-setext
  "^\\(.*\\)\n\\(---+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst markdown-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Markdown horizontal rules.")

(defconst markdown-regex-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ]\\(.\\|\n[^\n]\\)*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")

(defconst markdown-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst markdown-regex-list
  "^[ \t]*\\([0-9]+\\.\\|[\\*\\+-]\\) "
  "Regular expression for matching list markers.")

(defconst markdown-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst markdown-regex-italic
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n[^\n]\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching italic text.")

(defconst markdown-regex-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst markdown-regex-line-break
  "  $"
  "Regular expression for matching line breaks.")

(defconst markdown-regex-wiki-link
  "\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]"
  "Regular expression for matching wiki links.
This matches typical bracketed [[WikiLinks]] as well as 'aliased'
wiki links of the form [[PageName|link text]].  In this regular
expression, #1 matches the page name and #3 matches the link
text.")

(defconst markdown-regex-uri
  (concat
   "\\(" (mapconcat 'identity markdown-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst markdown-regex-angle-uri
  (concat
   "\\(<\\)\\(\\(?:"
   (mapconcat 'identity markdown-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst markdown-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst markdown-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions.")

(defconst markdown-regex-latex-display
  "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions.")

(defconst markdown-regex-list-indent
  "^\\(\\s *\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\(\\s +\\)"
  "Regular expression for matching indentation of list items.")

(defvar markdown-mode-font-lock-keywords-basic
  (list
   '(markdown-match-pre-blocks 0 markdown-pre-face t t)
   '(markdown-match-fenced-code-blocks 0 markdown-pre-face t t)
   (cons markdown-regex-blockquote 'markdown-blockquote-face)
   (cons markdown-regex-header-1-setext '((1 markdown-header-face-1)
                                          (2 markdown-header-rule-face)))
   (cons markdown-regex-header-2-setext '((1 markdown-header-face-2)
                                          (2 markdown-header-rule-face)))
   (cons markdown-regex-header-1-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-1)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-2-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-2)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-3-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-3)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-4-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-4)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-5-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-5)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-header-6-atx '((1 markdown-header-delimiter-face)
                                       (2 markdown-header-face-6)
                                       (3 markdown-header-delimiter-face)))
   (cons markdown-regex-hr 'markdown-header-face)
   '(markdown-match-comments 0 markdown-comment-face t t)
   (cons markdown-regex-code '(2 markdown-inline-code-face))
   (cons markdown-regex-angle-uri 'markdown-link-face)
   (cons markdown-regex-uri 'markdown-link-face)
   (cons markdown-regex-email 'markdown-link-face)
   (cons markdown-regex-list 'markdown-list-face)
   (cons markdown-regex-link-inline
         '((1 markdown-link-face t)
           (2 markdown-url-face t)))
   (cons markdown-regex-link-reference
         '((1 markdown-link-face t)
           (2 markdown-reference-face t)))
   (cons markdown-regex-reference-definition
         '((1 markdown-reference-face t)
           (2 markdown-url-face t)
           (3 markdown-link-title-face t)))
   (cons markdown-regex-footnote 'markdown-footnote-face)
   (cons markdown-regex-bold '(2 markdown-bold-face))
   (cons markdown-regex-italic '(2 markdown-italic-face))
   )
  "Syntax highlighting for Markdown files.")

(defconst markdown-mode-font-lock-keywords-latex
  (list
   ;; Math mode $..$ or $$..$$
   (cons markdown-regex-latex-expression '(2 markdown-math-face))
   ;; Display mode equations with brackets: \[ \]
   (cons markdown-regex-latex-display 'markdown-math-face)
   ;; Equation reference (eq:foo)
   (cons "(eq:\\w+)" 'markdown-reference-face)
   ;; Equation reference \eqref{foo}
   (cons "\\\\eqref{\\w+}" 'markdown-reference-face))
  "Syntax highlighting for LaTeX fragments.")

(defvar markdown-mode-font-lock-keywords
  (append
   (if markdown-enable-math
       markdown-mode-font-lock-keywords-latex)
   markdown-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Markdown mode.")

;; Footnotes
(defvar markdown-footnote-counter 0
  "Counter for footnote numbers.")
(make-variable-buffer-local 'markdown-footnote-counter)

(defconst markdown-footnote-chars
  "[[:alnum:]-]"
  "Regular expression maching any character that is allowed in a footnote identifier.")



;;; Compatibility =============================================================

;; Handle replace-regexp-in-string in XEmacs 21
(defun markdown-replace-regexp-in-string (regexp rep string)
  "Compatibility wrapper to provide `replace-regexp-in-string'."
  (if (featurep 'xemacs)
      (replace-in-string string regexp rep)
    (replace-regexp-in-string regexp rep string)))

(eval-and-compile
  (if (boundp 'mark-active)
      (defun markdown-mark-active ()    ; Emacs
	mark-active)
    (defalias 'markdown-mark-active 'region-exists-p))) ; XEmacs

(defun markdown-transient-mark-mode-active ()
  (cond
   ((boundp 'transient-mark-mode)
    (and transient-mark-mode (markdown-mark-active)))
   ((boundp 'zmacs-regions)
    (and zmacs-regions (markdown-mark-active)))))



;;; Markdown parsing functions ================================================

(defun markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun markdown-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-min))
        t
      (forward-line -1)
      (markdown-cur-line-blank-p))))

(defun markdown-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-max))
        t
      (forward-line 1)
      (markdown-cur-line-blank-p))))

(defun markdown-prev-line-indent-p ()
  "Return t if the previous line is indented and nil otherwise."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun markdown-cur-line-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "^\\s +" (point-at-eol) t)
    (current-column)))

(defun markdown-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (markdown-cur-line-indent)))

(defun markdown-next-line-indent ()
  "Return the number of leading whitespace characters in the next line."
  (save-excursion
    (forward-line 1)
    (markdown-cur-line-indent)))

(defun markdown-cur-non-list-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward markdown-regex-list-indent (point-at-eol) t)
      (current-column))))

(defun markdown-prev-non-list-indent ()
  "Return position of the first non-list-marker on the previous line."
  (save-excursion
    (forward-line -1)
    (markdown-cur-non-list-indent)))

(defun markdown--next-block ()
  "Move the point to the start of the next text block."
  (forward-line)
  (while (and (or (not (markdown-prev-line-blank-p))
                  (markdown-cur-line-blank-p))
              (not (eobp)))
    (forward-line)))

(defun markdown--end-of-level (level)
  "Move the point to the end of region with indentation at least LEVEL."
  (let (indent)
    (while (and (not (< (setq indent (markdown-cur-line-indent)) level))
                (not (>= indent (+ level 4)))
                (not (eobp)))
      (markdown--next-block))
    (unless (eobp)
      ;; Move back before any trailing blank lines
      (while (and (markdown-prev-line-blank-p)
                  (not (bobp)))
        (forward-line -1))
      (forward-line -1)
      (end-of-line))))

;; From html-helper-mode
(defun markdown-match-comments (last)
  "Match HTML comments from the point to LAST."
  (cond ((search-forward "<!--" last t)
         (backward-char 4)
         (let ((beg (point)))
           (cond ((search-forward-regexp "--[ \t]*>" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun markdown-match-pre-blocks (last)
  "Match Markdown pre blocks from point to LAST.
A region matches as if it is indented at least four spaces
relative to the nearest previous block of lesser non-list-marker
indentation."

  (let (cur-begin cur-end cur-indent prev-indent prev-list stop match found)
    ;; Don't start in the middle of a block
    (unless (and (bolp)
                 (markdown-prev-line-blank-p)
                 (not (markdown-cur-line-blank-p)))
      (markdown--next-block))

    ;; Move to the first full block in the region with indent 4 or more
    (while (and (not (>= (setq cur-indent (markdown-cur-line-indent)) 4))
                (not (>= (point) last)))
      (markdown--next-block))
    (setq cur-begin (point))
    (markdown--end-of-level cur-indent)
    (setq cur-end (point))
    (setq match nil)
    (setq stop (> cur-begin cur-end))

    (while (and (<= cur-end last) (not stop) (not match))
      ;; Move to the nearest preceding block of lesser (non-marker) indentation
      (setq prev-indent (+ cur-indent 1))
      (goto-char cur-begin)
      (setq found nil)
      (while (and (>= prev-indent cur-indent)
                  (not (and prev-list
                            (eq prev-indent cur-indent)))
                  (not (bobp)))

        ;; Move point to the last line of the previous block.
        (forward-line -1)
        (while (and (markdown-cur-line-blank-p)
                    (not (bobp)))
          (forward-line -1))

        ;; Update the indentation level using either the
        ;; non-list-marker indentation, if the previous line is the
        ;; start of a list, or the actual indentation.
        (setq prev-list (markdown-cur-non-list-indent))
        (setq prev-indent (or prev-list
                              (markdown-cur-line-indent)))
        (setq found t))

      ;; If the loop didn't execute
      (unless found
        (setq prev-indent 0))

      ;; Compare with prev-indent minus its remainder mod 4
      (setq prev-indent (- prev-indent (mod prev-indent 4)))

      ;; Set match data and return t if we have a match
      (if (>= cur-indent (+ prev-indent 4))
          ;; Match
          (progn
            (setq match t)
            (set-match-data (list cur-begin cur-end))
            ;; Leave point at end of block
            (goto-char cur-end)
            (forward-line))

        ;; Move to the next block (if possible)
        (goto-char cur-end)
        (markdown--next-block)
        (setq cur-begin (point))
        (setq cur-indent (markdown-cur-line-indent))
        (markdown--end-of-level cur-indent)
        (setq cur-end (point))
        (setq stop (equal cur-begin cur-end))))
    match))

(defun markdown-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\([~]\\{3,\\}\\)" last t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line)
           (cond ((search-forward-regexp
                   (concat "^" (match-string 1) "~*") last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun markdown-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (re-search-backward "\n\n" nil t)))
      (when found
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (beginning-of-line)
          (setq font-lock-end (point)))
        (setq font-lock-beg found)))))



;;; Syntax Table ==============================================================

(defvar markdown-mode-syntax-table
  (let ((markdown-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "w" markdown-mode-syntax-table)
    markdown-mode-syntax-table)
  "Syntax table for `markdown-mode'.")



;;; Element Insertion =========================================================

(defun markdown-wrap-or-insert (s1 s2)
  "Insert the strings S1 and S2.
If Transient Mark mode is on and a region is active, wrap the strings S1
and S2 around the region."
  (if (markdown-transient-mark-mode-active)
      (let ((a (region-beginning)) (b (region-end)))
        (goto-char a)
        (insert s1)
        (goto-char (+ b (length s1)))
        (insert s2))
    (insert s1 s2)))

(defun markdown-insert-hr ()
  "Insert a horizonal rule using `markdown-hr-string'."
  (interactive)
  ;; Leading blank line
  (when (and (>= (point) (+ (point-min) 2))
             (not (looking-back "\n\n" 2)))
    (insert "\n"))
  ;; Insert custom HR string
  (insert (concat markdown-hr-string "\n"))
  ;; Following blank line
  (backward-char)
  (unless (looking-at "\n\n")
    (insert "\n")))

(defun markdown-insert-bold ()
  "Insert markup for a bold word or phrase.
If Transient Mark mode is on and a region is active, it is made bold."
  (interactive)
  (if markdown-bold-underscore
      (markdown-wrap-or-insert "__" "__")
    (markdown-wrap-or-insert "**" "**"))
  (backward-char 2))

(defun markdown-insert-italic ()
  "Insert markup for an italic word or phrase.
If Transient Mark mode is on and a region is active, it is made italic."
  (interactive)
  (if markdown-italic-underscore
      (markdown-wrap-or-insert "_" "_")
    (markdown-wrap-or-insert "*" "*"))
  (backward-char 1))

(defun markdown-insert-code ()
  "Insert markup for an inline code fragment.
If Transient Mark mode is on and a region is active, it is marked
as inline code."
  (interactive)
  (markdown-wrap-or-insert "`" "`")
  (backward-char 1))

(defun markdown-insert-link ()
  "Insert an inline link of the form []().
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (markdown-wrap-or-insert "[" "]")
  (insert "()")
  (backward-char 1))

(defun markdown-insert-reference-link-dwim ()
  "Insert a reference link of the form [text][label] at point.
If Transient Mark mode is on and a region is active, the region
is used as the link text. Otherwise, the link text will be read
from the minibuffer. The link URL, label, and title will be read
from the minibuffer. The link label definition is placed at the
end of the current paragraph."
  (interactive)
  (if (markdown-transient-mark-mode-active)
      (call-interactively 'markdown-insert-reference-link-region)
    (call-interactively 'markdown-insert-reference-link)))

(defun markdown-insert-reference-link-region (url label title)
  "Insert a reference link at point using the region as the link text."
  (interactive "sLink URL: \nsLink Label (optional): \nsLink Title (optional): ")
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (markdown-insert-reference-link text url label title)))

(defun markdown-insert-reference-link (text url label title)
  "Insert a reference link at point.
The link label definition is placed at the end of the current
paragraph."
  (interactive "sLink Text: \nsLink URL: \nsLink Label (optional): \nsLink Title (optional): ")
  (let (end)
    (insert (concat "[" text "][" label "]"))
    (setq end (point))
    (forward-paragraph)
    (insert "\n[")
    (if (> (length label) 0)
        (insert label)
      (insert text))
    (insert (concat "]: " url))
    (unless (> (length url) 0)
      (setq end (point)))
    (when (> (length title) 0)
      (insert (concat " \"" title "\"")))
    (insert "\n")
    (unless (looking-at "\n")
      (insert "\n"))
    (goto-char end)))

(defun markdown-insert-wiki-link ()
  "Insert a wiki link of the form [[WikiLink]].
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (markdown-wrap-or-insert "[[" "]]")
  (backward-char 2))

(defun markdown-insert-image ()
  "Insert an inline image tag of the form ![]().
If Transient Mark mode is on and a region is active, it is used
as the alt text of the image."
  (interactive)
  (markdown-wrap-or-insert "![" "]")
  (insert "()")
  (backward-char 1))

(defun markdown-insert-header-1 ()
  "Insert a first level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 1))

(defun markdown-insert-header-2 ()
  "Insert a second level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 2))

(defun markdown-insert-header-3 ()
  "Insert a third level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 3))

(defun markdown-insert-header-4 ()
  "Insert a fourth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 4))

(defun markdown-insert-header-5 ()
  "Insert a fifth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 5))

(defun markdown-insert-header-6 ()
  "Insert a sixth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (markdown-insert-header 6))

(defun markdown-insert-header (n)
  "Insert an atx-style (hash mark) header.
With no prefix argument, insert a level-1 header.  With prefix N,
insert a level-N header.  If Transient Mark mode is on and the
region is active, it is used as the header text."
  (interactive "p")
  (unless n                             ; Test to see if n is defined
    (setq n 1))                         ; Default to level 1 header
  (let (hdr hdrl hdrr)
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
  (if (markdown-transient-mark-mode-active)
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
  (if (markdown-transient-mark-mode-active)
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
  (if (markdown-transient-mark-mode-active)
      (markdown-blockquote-region (region-beginning) (region-end))
    (insert "> ")))

(defun markdown-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.  The characters PREFIX will appear at the beginning
of each line."
  (if mark-active
      (save-excursion
        ;; Ensure that there is a leading blank line
        (goto-char beg)
        (when (and (>= (point) (+ (point-min) 2))
                   (not (looking-back "\n\n" 2)))
          (insert "\n")
          (setq beg (1+ beg))
          (setq end (1+ end)))
        ;; Move back before any blank lines at the end
        (goto-char end)
        (while (and (looking-back "\n" 1)
                    (not (equal (point) (point-min))))
          (backward-char)
          (setq end (1- end)))
        ;; Ensure that there is a trailing blank line
        (goto-char end)
        (if (not (or (looking-at "\n\n")
                     (and (equal (1+ end) (point-max)) (looking-at "\n"))))
            (insert "\n"))
        ;; Insert PREFIX
        (goto-char beg)
        (beginning-of-line)
        (while (< (point-at-bol) end)
          (insert prefix)
          (setq end (+ (length prefix) end))
          (forward-line)))))

(defun markdown-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (markdown-block-region beg end "> "))

(defun markdown-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (markdown-transient-mark-mode-active)
      (markdown-pre-region (region-beginning) (region-end))
    (insert "    ")))

(defun markdown-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (markdown-block-region beg end "    "))

;;; Footnotes ======================================================================

(defun markdown-footnote-counter-inc ()
  "Increment markdown-footnote-counter and return the new value."
  (when (= markdown-footnote-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\[\\^\\(" markdown-footnote-chars "*?\\)\\]:")
				(point-max) t)
	(let ((fn (string-to-number (match-string 1))))
	  (when (> fn markdown-footnote-counter)
	    (setq markdown-footnote-counter fn))))))
  (incf markdown-footnote-counter))

(defun markdown-footnote-new ()
  "Insert a footnote with a new number and jump to a position to enter the
footnote text."
  (interactive)
  (let ((fn (markdown-footnote-counter-inc)))
    (insert (format "[^%d]" fn))
    (markdown-footnote-text-find-new-location)
    (insert (format "[^%d]: " fn))))

(defun markdown-footnote-text-find-new-location ()
  "Position the cursor at the proper location for a new footnote text."
  (cond
   ((eq markdown-footnote-location 'end) (goto-char (point-max)))
   ((eq markdown-footnote-location 'immediately) (forward-paragraph))
   ((eq markdown-footnote-location 'header)
    ;; search for a header. if none is found, go to the end of the document.
    (catch 'eof
      (while (progn
	       (forward-paragraph)
	       (unless (re-search-forward markdown-regex-header nil t)
		 (throw 'eof nil))
	       (backward-paragraph)
               (forward-line)
	       (not (looking-at markdown-regex-header)))))))
  (forward-line -1)
  ;; make sure we're on an empty line:
  (unless (markdown-cur-line-blank-p)
    (insert "\n"))
  ;; and make sure the previous line is empty:
  (unless (markdown-prev-line-blank-p)
    (insert "\n"))
  ;; then make sure there's an empty line following the footnote:
  (unless (markdown-next-line-blank-p)
    (insert "\n")
    (forward-line -1)))

(defun markdown-footnote-kill ()
  "Kill the footnote at point.
The footnote text is killed (and added to the kill ring), the
footnote marker is deleted. Point has to be either at the
footnote marker or in the footnote text."
  (interactive)
  (let (return-pos)
    (when (markdown-footnote-text-positions) ; if we're in a footnote text
      (markdown-footnote-return) ; we first move to the marker
      (setq return-pos 'text)) ; and remember our return position
    (let ((marker (markdown-footnote-delete-marker)))
      (unless marker
	(error "Not at a footnote"))
      (let ((text-pos (markdown-footnote-find-text (car marker))))
	(unless text-pos
	  (error "No text for footnote `%s'" (car marker)))
	(goto-char text-pos)
	(let ((pos (markdown-footnote-kill-text)))
	  (setq return-pos
		(if (and pos (eq return-pos 'text))
		    pos
		  (cadr marker))))))
    (goto-char return-pos)))

(defun markdown-footnote-delete-marker ()
  "Delete a footnote marker at point.
Returns a list (ID START) containing the footnote ID and the
start position of the marker before deletion. If no footnote
marker was deleted, this function returns NIL."
  (let ((marker (markdown-footnote-marker-positions)))
    (when marker
      (delete-region (second marker) (third marker))
      (butlast marker))))

(defun markdown-footnote-kill-text ()
  "Kill footnote text at point.
Returns the start position of the footnote text before deletion,
or NIL if point was not inside a footnote text.

The killed text is placed in the kill ring (without the footnote
number)."
  (let ((fn (markdown-footnote-text-positions)))
    (when fn
      (let ((text (delete-and-extract-region (second fn) (third fn))))
	(string-match (concat "\\[\\" (first fn) "\\]:[[:space:]]*\\(\\(.*\n?\\)*\\)") text)
	(kill-new (match-string 1 text))
	(second fn)))))

(defun markdown-footnote-goto-text ()
  "Jump to the text of the footnote at point."
  (interactive)
  (let ((fn (car (markdown-footnote-marker-positions))))
    (unless fn
      (error "Not at a footnote marker"))
    (let ((new-pos (markdown-footnote-find-text fn)))
      (unless new-pos
	(error "No definition found for footnote `%s'" fn))
      (goto-char new-pos))))

(defun markdown-footnote-return ()
  "Return from a footnote to its footnote number in the main text."
  (interactive)
  (let ((fn (save-excursion
	      (car (markdown-footnote-text-positions)))))
    (unless fn
      (error "Not in a footnote"))
    (let ((new-pos (markdown-footnote-find-marker fn)))
      (unless new-pos
	(error "Footnote marker `%s' not found" fn))
      (goto-char new-pos))))

(defun markdown-footnote-find-marker (id)
  "Find the location of the footnote marker with ID.
The actual buffer position returned is the position directly
following the marker's closing bracket. If no marker is found,
NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "\\[" id "\\]\\([^:]\\|\\'\\)") nil t)
      (skip-chars-backward "^]")
      (point))))

(defun markdown-footnote-find-text (id)
  "Find the location of the text of footnote ID.
The actual buffer position returned is the position of the first
character of the text, after the footnote's identifier. If no
footnote text is found, NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\[" id "\\]:") nil t)
      (skip-chars-forward "[:space:]")
      (point))))

(defun markdown-footnote-marker-positions ()
  "Return the position and ID of the footnote marker point is on.
The return value is a list (ID START END). If point is not on a
footnote, NIL is returned."
  ;; first make sure we're at a footnote marker
  (if (or (looking-back (concat "\\[\\^" markdown-footnote-chars "*\\]?") (point-at-bol))
	  (looking-at (concat "\\[?\\^" markdown-footnote-chars "*?\\]")))
      (save-excursion
	;; move point between [ and ^:
	(if (looking-at "\\[")
	    (forward-char 1)
	  (skip-chars-backward "^["))
	(looking-at (concat "\\(\\^" markdown-footnote-chars "*?\\)\\]"))
	(list (match-string 1) (1- (match-beginning 1)) (1+ (match-end 1))))))

(defun markdown-footnote-text-positions ()
  "Return the start and end positions of the footnote text point is in.
The exact return value is a list of three elements: (ID START
END). The start position is the position of the opening bracket
of the footnote id. The end position is directly after the
newline that ends the footnote. If point is not in a footnote,
NIL is returned instead."
  (save-excursion
    (let ((fn (progn
		(backward-paragraph)
		;; if we're in a multiparagraph footnote, we need to back up further
		(while (>= (markdown-next-line-indent) 4)
		  (backward-paragraph))
		(forward-line)
		(if (looking-at (concat "^\\[\\(\\^" markdown-footnote-chars "*?\\)\\]:"))
		    (list (match-string 1) (point))))))
      (when fn
	(while (progn
		 (forward-paragraph)
		 (>= (markdown-next-line-indent) 4)))
	(append fn (list (point)))))))

;;; Indentation ====================================================================

(defun markdown-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun markdown-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `markdown-enter-key' or
`markdown-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `markdown-enter-key', by an initial call of
`markdown-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position."
  (interactive)
  (let ((positions (markdown-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'markdown-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (markdown-indent-find-next-position cur-pos positions)))))

(defun markdown-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level."
  (let (pos prev-line-pos positions)

    ;; Previous line indent
    (setq prev-line-pos (markdown-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq pos (markdown-prev-non-list-indent))
    (when pos
      (setq positions (cons pos positions))
      (setq positions (cons (+ pos tab-width) positions)))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward markdown-regex-list-indent (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if (and pos (not (eq pos prev-line-pos)))
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    (reverse positions)))

(defun markdown-do-normal-return ()
  "Insert a newline and optionally indent the next line."
  (newline)
  (if markdown-indent-on-enter
      (funcall indent-line-function)))

(defun markdown-enter-key ()
  "Handle RET according to context.
If there is a wiki link at the point, follow it unless
`markdown-follow-wiki-link-on-enter' is nil.  Otherwise, process
it in the usual way."
  (interactive)
  (if (and markdown-follow-wiki-link-on-enter (markdown-wiki-link-p))
      (markdown-follow-wiki-link-at-point)
    (markdown-do-normal-return)))

(defun markdown-dedent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then dedent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (let ((cur-pos (current-column))
        (start-of-indention (save-excursion
                              (back-to-indentation)
                              (current-column))))
    (if (and (> cur-pos 0) (= cur-pos start-of-indention))
        (let ((result 0))
          (dolist (i (markdown-calc-indents))
            (when (< i cur-pos)
              (setq result (max result i))))
          (indent-line-to result))
      (backward-delete-char-untabify arg))))



;;; Keymap ====================================================================

(defvar markdown-mode-map
  (let ((map (make-keymap)))
    ;; Element insertion
    (define-key map "\C-c\C-al" 'markdown-insert-link)
    (define-key map "\C-c\C-ar" 'markdown-insert-reference-link-dwim)
    (define-key map "\C-c\C-aw" 'markdown-insert-wiki-link)
    (define-key map "\C-c\C-ii" 'markdown-insert-image)
    (define-key map "\C-c\C-t1" 'markdown-insert-header-1)
    (define-key map "\C-c\C-t2" 'markdown-insert-header-2)
    (define-key map "\C-c\C-t3" 'markdown-insert-header-3)
    (define-key map "\C-c\C-t4" 'markdown-insert-header-4)
    (define-key map "\C-c\C-t5" 'markdown-insert-header-5)
    (define-key map "\C-c\C-t6" 'markdown-insert-header-6)
    (define-key map "\C-c\C-pb" 'markdown-insert-bold)
    (define-key map "\C-c\C-ss" 'markdown-insert-bold)
    (define-key map "\C-c\C-pi" 'markdown-insert-italic)
    (define-key map "\C-c\C-se" 'markdown-insert-italic)
    (define-key map "\C-c\C-pf" 'markdown-insert-code)
    (define-key map "\C-c\C-sc" 'markdown-insert-code)
    (define-key map "\C-c\C-sb" 'markdown-insert-blockquote)
    (define-key map "\C-c\C-s\C-b" 'markdown-blockquote-region)
    (define-key map "\C-c\C-sp" 'markdown-insert-pre)
    (define-key map "\C-c\C-s\C-p" 'markdown-pre-region)
    (define-key map "\C-c-" 'markdown-insert-hr)
    (define-key map "\C-c\C-tt" 'markdown-insert-title)
    (define-key map "\C-c\C-ts" 'markdown-insert-section)
    ;; Footnotes
    (define-key map "\C-c\C-fn" 'markdown-footnote-new)
    (define-key map "\C-c\C-fg" 'markdown-footnote-goto-text)
    (define-key map "\C-c\C-fb" 'markdown-footnote-return)
    (define-key map "\C-c\C-fk" 'markdown-footnote-kill)
    ;; Regular Link Following
    (define-key map "\C-c\C-o" 'markdown-follow-link-at-point)
    ;; WikiLink Following
    (define-key map "\C-c\C-w" 'markdown-follow-wiki-link-at-point)
    (define-key map "\M-n" 'markdown-next-wiki-link)
    (define-key map "\M-p" 'markdown-previous-wiki-link)
    ;; Indentation
    (define-key map "\C-m" 'markdown-enter-key)
    (define-key map (kbd "<backspace>") 'markdown-dedent-or-delete)
    ;; Visibility cycling
    (define-key map (kbd "<tab>") 'markdown-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'markdown-shifttab)
    (define-key map (kbd "<S-tab>")  'markdown-shifttab)
    (define-key map (kbd "<backtab>") 'markdown-shifttab)
    ;; Header navigation
    (define-key map (kbd "C-M-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-M-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-M-f") 'outline-forward-same-level)
    (define-key map (kbd "C-M-b") 'outline-backward-same-level)
    (define-key map (kbd "C-M-u") 'outline-up-heading)
    ;; Markdown functions
    (define-key map "\C-c\C-cm" 'markdown-other-window)
    (define-key map "\C-c\C-cp" 'markdown-preview)
    (define-key map "\C-c\C-ce" 'markdown-export)
    (define-key map "\C-c\C-cv" 'markdown-export-and-preview)
    (define-key map "\C-c\C-co" 'markdown-open)
    (define-key map "\C-c\C-cw" 'markdown-kill-ring-save)
    ;; References
    (define-key map "\C-c\C-cc" 'markdown-check-refs)
    ;; Lists
    (define-key map "\C-c\C-cn" 'markdown-cleanup-list-numbers)
    map)
  "Keymap for Markdown major mode.")

;;; Menu ==================================================================

(easy-menu-define markdown-mode-menu markdown-mode-map
  "Menu for Markdown mode"
  '("Markdown"
    ("Show/Hide"
     ["Cycle visibility" markdown-cycle (outline-on-heading-p)]
     ["Cycle global visibility" markdown-shifttab])
    "---"
    ["Compile" markdown-other-window]
    ["Preview" markdown-preview]
    ["Export" markdown-export]
    ["Export & View" markdown-export-and-preview]
    ["Open" markdown-open]
    ["Kill ring save" markdown-kill-ring-save]
    "---"
    ("Headers (setext)"
     ["Insert Title" markdown-insert-title]
     ["Insert Section" markdown-insert-section])
    ("Headers (atx)"
     ["First level" markdown-insert-header-1]
     ["Second level" markdown-insert-header-2]
     ["Third level" markdown-insert-header-3]
     ["Fourth level" markdown-insert-header-4]
     ["Fifth level" markdown-insert-header-5]
     ["Sixth level" markdown-insert-header-6])
    "---"
    ["Bold" markdown-insert-bold]
    ["Italic" markdown-insert-italic]
    ["Blockquote" markdown-insert-blockquote]
    ["Preformatted" markdown-insert-pre]
    ["Code" markdown-insert-code]
    "---"
    ["Insert inline link" markdown-insert-link]
    ["Insert reference link" markdown-insert-reference-link-dwim]
    ["Insert image" markdown-insert-image]
    ["Insert horizontal rule" markdown-insert-hr]
    "---"
    ("Footnotes"
     ["Insert footnote" markdown-footnote-new]
     ["Jump to footnote text" markdown-footnote-goto-text]
     ["Return from footnote" markdown-footnote-return])
    "---"
    ["Check references" markdown-check-refs]
    ["Clean up list numbering" markdown-cleanup-list-numbers]
    "---"
    ["Version" markdown-show-version]
    ))



;;; imenu =====================================================================

(defun markdown-imenu-create-index ()
  (let* ((root '(nil . nil))
	 cur-alist
	 (cur-level 0)
	 (empty-heading "-")
	 (self-heading ".")
	 hashes pos level heading)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-header (point-max) t)
	(cond
	 ((setq heading (match-string-no-properties 1))
	  (setq pos (match-beginning 1)
		level 1))
	 ((setq heading (match-string-no-properties 3))
	  (setq pos (match-beginning 3)
		level 2))
	 ((setq hashes (match-string-no-properties 5))
	  (setq heading (match-string-no-properties 6)
		pos (match-beginning 5)
		level (length hashes))))
	(let ((alist (list (cons heading pos))))
	  (cond
	   ((= cur-level level)		; new sibling
	    (setcdr cur-alist alist)
	    (setq cur-alist alist))
	   ((< cur-level level)		; first child
	    (dotimes (i (- level cur-level 1))
	      (setq alist (list (cons empty-heading alist))))
	    (if cur-alist
		(let* ((parent (car cur-alist))
		       (self-pos (cdr parent)))
		  (setcdr parent (cons (cons self-heading self-pos) alist)))
	      (setcdr root alist))	; primogenitor
	    (setq cur-alist alist)
	    (setq cur-level level))
	   (t				; new sibling of an ancestor
	    (let ((sibling-alist (last (cdr root))))
	      (dotimes (i (1- level))
		(setq sibling-alist (last (cdar sibling-alist))))
	      (setcdr sibling-alist alist)
	      (setq cur-alist alist))
	    (setq cur-level level)))))
      (cdr root))))

;;; References ================================================================

;;; Undefined reference checking code by Dmitry Dzhus <mail@sphinx.net.ru>.

(defconst markdown-refcheck-buffer
  "*Undefined references for %buffer%*"
  "Pattern for name of buffer for listing undefined references.
The string %buffer% will be replaced by the corresponding
`markdown-mode' buffer name.")

(defun markdown-has-reference-definition (reference)
  "Find out whether Markdown REFERENCE is defined.

REFERENCE should include the square brackets, like [this]."
  (let ((reference (downcase reference)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward markdown-regex-reference-definition nil t)
          (when (string= reference (downcase (match-string-no-properties 1)))
            (throw 'found (match-string-no-properties 2))))))))

(defun markdown-get-undefined-refs ()
  "Return a list of undefined Markdown references.

Result is an alist of pairs (reference . occurencies), where
occurencies is itself another alist of pairs (label .
line-number).

For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"[emacs]\" (\"[Nice editor]\" . 12) (\"[GNU Emacs]\" . 45)) (\"[elisp]\" (\"[manual]\" . 127)))."
  (let ((missing))
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward markdown-regex-link-reference nil t)
        (let* ((label (match-string-no-properties 1))
               (reference (match-string-no-properties 2))
               (target (downcase (if (string= reference "[]") label reference))))
          (unless (markdown-has-reference-definition target)
            (let ((entry (assoc target missing)))
              (if (not entry)
                  (add-to-list 'missing (cons target
                                              (list (cons label (markdown-line-number-at-pos)))) t)
                (setcdr entry
                        (append (cdr entry) (list (cons label (markdown-line-number-at-pos))))))))))
      missing)))

(defun markdown-add-missing-ref-definition (ref buffer &optional recheck)
  "Add blank REF definition to the end of BUFFER.

REF is a Markdown reference in square brackets, like \"[lisp-history]\".

When RECHECK is non-nil, BUFFER gets rechecked for undefined
references so that REF disappears from the list of those links."
  (with-current-buffer buffer
    (when (not (eq major-mode 'markdown-mode))
      (error "Not available in current mode"))
    (goto-char (point-max))
    (indent-new-comment-line)
    (insert (concat ref ": ")))
  (switch-to-buffer-other-window buffer)
  (goto-char (point-max))
  (when recheck
    (markdown-check-refs t)))

;; Button which adds an empty Markdown reference definition to the end
;; of buffer specified as its 'target-buffer property. Reference name
;; is button's label
(when (>= emacs-major-version 22)
  (define-button-type 'markdown-ref-button
    'help-echo "Push to create an empty reference definition"
    'face 'bold
    'action (lambda (b)
              (markdown-add-missing-ref-definition
               (button-label b) (button-get b 'target-buffer) t))))

;; Button jumping to line in buffer specified as its 'target-buffer
;; property. Line number is button's 'line property.
(when (>= emacs-major-version 22)
  (define-button-type 'goto-line-button
    'help-echo "Push to go to this line"
    'face 'italic
    'action (lambda (b)
              (message (button-get b 'buffer))
              (switch-to-buffer-other-window (button-get b 'target-buffer))
              ;; use call-interactively to silence compiler
              (call-interactively 'goto-line (button-get b 'target-line)))))

(defun markdown-check-refs (&optional silent)
  "Show all undefined Markdown references in current `markdown-mode' buffer.

If SILENT is non-nil, do not message anything when no undefined
references found.

Links which have empty reference definitions are considered to be
defined."
  (interactive "P")
  (when (not (eq major-mode 'markdown-mode))
    (error "Not available in current mode"))
  (let ((oldbuf (current-buffer))
        (refs (markdown-get-undefined-refs))
        (refbuf (get-buffer-create (markdown-replace-regexp-in-string
                                 "%buffer%" (buffer-name)
                                 markdown-refcheck-buffer))))
    (if (null refs)
        (progn
          (when (not silent)
            (message "No undefined references found"))
          (kill-buffer refbuf))
      (with-current-buffer refbuf
        (when view-mode
          (View-exit-and-edit))
        (erase-buffer)
        (insert "Following references lack definitions:")
        (newline 2)
        (dolist (ref refs)
          (let ((button-label (format "%s" (car ref))))
            (if (>= emacs-major-version 22)
                ;; Create a reference button in Emacs 22
                (insert-text-button button-label
                                    :type 'markdown-ref-button
                                    'target-buffer oldbuf)
              ;; Insert reference as text in Emacs < 22
              (insert button-label)))
          (insert " (")
          (dolist (occurency (cdr ref))
            (let ((line (cdr occurency)))
              (if (>= emacs-major-version 22)
                  ;; Create a line number button in Emacs 22
                  (insert-button (number-to-string line)
                                 :type 'goto-line-button
                                 'target-buffer oldbuf
                                 'target-line line)
                ;; Insert line number as text in Emacs < 22
                (insert (number-to-string line)))
              (insert " "))) (delete-char -1)
          (insert ")")
          (newline))
        (view-buffer-other-window refbuf)
        (goto-char (point-min))
        (forward-line 2)))))


;;; Lists =====================================================================

(defun markdown--cleanup-list-numbers-level (&optional pfx)
  "Update the numbering for level PFX (as a string of spaces).

Assume that the previously found match was for a numbered item in
a list."
  (let ((cpfx pfx)
        (idx 0)
        (continue t)
        (step t)
        (sep nil))
    (while (and continue (not (eobp)))
      (setq step t)
      (cond
       ((looking-at "^\\([\s-]*\\)[0-9]+\\. ")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ((string= cpfx pfx)
          (replace-match
           (concat pfx (number-to-string  (setq idx (1+ idx))) ". "))
          (setq sep nil))
         ;; indented a level
         ((string< pfx cpfx)
          (setq sep (markdown--cleanup-list-numbers-level cpfx))
          (setq step nil))
         ;; exit the loop
         (t
          (setq step nil)
          (setq continue nil))))

       ((looking-at "^\\([\s-]*\\)[^	 \n\r].*$")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ;; reset if separated before
         ((string= cpfx pfx) (when sep (setq idx 0)))
         ((string< cpfx pfx)
          (setq step nil)
          (setq continue nil))))
       (t (setq sep t)))

      (when step
        (beginning-of-line)
        (setq continue (= (forward-line) 0))))
    sep))

;;;###autoload
(defun markdown-cleanup-list-numbers ()
  "Update the numbering of ordered lists."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (markdown--cleanup-list-numbers-level "")))


;;; Outline ===================================================================

;; The following visibility cycling code was taken from org-mode
;; by Carsten Dominik and adapted for markdown-mode.

(defvar markdown-cycle-global-status 1)
(defvar markdown-cycle-subtree-status nil)

;; Based on org-end-of-subtree from org.el
(defun markdown-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

;; Based on org-cycle from org.el.
(defun markdown-cycle (&optional arg)
  "Visibility cycling for Markdown mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'."
  (interactive "P")
  (cond
   ((eq arg t) ;; Global cycling
    (cond
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 2))
      ;; Move from overview to contents
      (hide-sublevels 1)
      (message "CONTENTS")
      (setq markdown-cycle-global-status 3))

     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 3))
      ;; Move from contents to all
      (show-all)
      (message "SHOW ALL")
      (setq markdown-cycle-global-status 1))

     (t
      ;; Defaults to overview
      (hide-body)
      (message "OVERVIEW")
      (setq markdown-cycle-global-status 2))))

   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (outline-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (outline-end-of-heading)   (setq eoh (point))
        (markdown-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
        ;; Nothing is hidden behind this heading
        (message "EMPTY ENTRY")
        (setq markdown-cycle-subtree-status nil))
       ((>= eol eos)
        ;; Entire subtree is hidden in one line: open it
        (show-entry)
        (show-children)
        (message "CHILDREN")
        (setq markdown-cycle-subtree-status 'children))
       ((and (eq last-command this-command)
             (eq markdown-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        (show-subtree)
        (message "SUBTREE")
        (setq markdown-cycle-subtree-status 'subtree))
       (t
        ;; Default action: hide the subtree.
        (hide-subtree)
        (message "FOLDED")
        (setq markdown-cycle-subtree-status 'folded)))))

   (t
    (indent-for-tab-command))))

;; Based on org-shifttab from org.el.
(defun markdown-shifttab ()
  "Global visibility cycling.
Calls `markdown-cycle' with argument t."
  (interactive)
  (markdown-cycle t))

(defun markdown-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((match-end 1) 1)
   ((match-end 2) 2)
   ((- (match-end 0) (match-beginning 0)))))

;;; Commands ==================================================================

(defun markdown (&optional output-buffer-name)
  "Run `markdown' on current buffer and insert output in buffer given by
`output-buffer-name' (defaults to `markdown-output-buffer-name').  Return the
OUTPUT-BUFFER used."
  (interactive)
  (save-window-excursion
    (let ((begin-region)
          (end-region))
      (if (markdown-transient-mark-mode-active)
          (setq begin-region (region-beginning)
                end-region (region-end))
        (setq begin-region (point-min)
              end-region (point-max)))

      (unless output-buffer-name
        (setq output-buffer-name markdown-output-buffer-name))

      (cond
       ;; Handle case when `markdown-command' does not read from stdin
       (markdown-command-needs-filename
        (if (not buffer-file-name)
            (error "Must be visiting a file")
          (shell-command (concat markdown-command " "
                                 (shell-quote-argument buffer-file-name))
                         output-buffer-name)))
       ;; Pass region to `markdown-command' via stdin
       (t
        (shell-command-on-region begin-region end-region markdown-command
                                 output-buffer-name))))
    output-buffer-name))

(defun markdown-standalone (&optional output-buffer-name)
  "special function to provide html standalone output"
  (interactive)
  (setq output-buffer-name (markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (goto-char (point-min))
    (unless (markdown-output-standalone-p)
      (markdown-add-xhtml-header-and-footer output-buffer-name))
    (html-mode))
  output-buffer-name)

(defun markdown-other-window (&optional output-buffer-name)
  " Run `markdown' on current buffer and display in other window"
  (interactive)
  (display-buffer (markdown-standalone output-buffer-name)))

(defun markdown-output-standalone-p ()
  "Determine whether `markdown-command' output is standalone XHTML.
Standalone XHTML output is identified by an occurrence of
`markdown-xhtml-standalone-regexp' in the first five lines of output."
  (re-search-forward
   markdown-xhtml-standalone-regexp
   (save-excursion (goto-char (point-min)) (forward-line 4) (point))
   t))

(defun markdown-add-xhtml-header-and-footer (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
          "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
          "<head>\n<title>")
  (insert title)
  (insert "</title>\n")
  (when (> (length markdown-content-type) 0)
    (insert
     (format
      "<meta http-equiv=\"Content-Type\" content=\"%s;charset=%s\"/>\n"
      markdown-content-type
      (or (and markdown-coding-system
               (fboundp 'coding-system-get)
               (coding-system-get markdown-coding-system
                                  'mime-charset))
          (and (fboundp 'coding-system-get)
               (coding-system-get buffer-file-coding-system
                                  'mime-charset))
          "iso-8859-1"))))
  (if (> (length markdown-css-path) 0)
      (insert "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
              markdown-css-path
              "\"  />\n"))
  (when (> (length markdown-xhtml-header-content) 0)
    (insert markdown-xhtml-header-content))
  (insert "\n</head>\n\n"
          "<body>\n\n")
  (goto-char (point-max))
  (insert "\n"
          "</body>\n"
          "</html>\n"))

(defun markdown-preview (&optional output-buffer-name)
  "Run `markdown' on the current buffer and preview the output in a browser."
  (interactive)
  (browse-url-of-buffer (markdown markdown-output-buffer-name)))

(defun markdown-export-file-name (&optional extension)
  "Attempt to generate a filename for Markdown output.
If the current buffer is visiting a file, we construct a new
output filename based on that filename.  Otherwise, return nil."
  (when (buffer-file-name)
    (unless extension
      (setq extension ".html"))
    (concat
     (cond
      ((buffer-file-name)
       (file-name-sans-extension (buffer-file-name)))
      (t (buffer-name)))
     extension)))

(defun markdown-export (&optional output-file)
  "Run Markdown on the current buffer, save to a file, and return the filename.
The resulting filename will be constructed using the current filename, but
with the extension removed and replaced with .html."
  (interactive)
  (unless output-file
    (setq output-file (markdown-export-file-name ".html")))
  (when output-file
    (let ((output-buffer-name))
      (setq output-buffer-name (buffer-name (find-file-noselect output-file)))
      (markdown-standalone output-buffer-name)
      (with-current-buffer output-buffer-name
        (save-buffer))
      output-file)))

(defun markdown-export-and-preview ()
  "Export to XHTML using `markdown-export' and browse the resulting file."
  (interactive)
  (browse-url (markdown-export)))

(defun markdown-open ()
  "Open file for the current buffer with `markdown-open-command'."
  (interactive)
  (if (not markdown-open-command)
      (error "Variable `markdown-open-command' must be set")
    (if (not buffer-file-name)
        (error "Must be visiting a file")
      (call-process markdown-open-command
                    nil nil nil buffer-file-name))))

(defun markdown-kill-ring-save ()
  "Run Markdown on file and store output in the kill ring."
  (interactive)
  (save-window-excursion
    (markdown)
    (with-current-buffer markdown-output-buffer-name
      (kill-ring-save (point-min) (point-max)))))


;;; Links =====================================================================

(require 'thingatpt)

(defun markdown-link-p ()
  "Return non-nil when `point' is at a non-wiki link.
See `markdown-wiki-link-p' for more information."
  (let ((case-fold-search nil))
    (and (not (markdown-wiki-link-p))
         (or (thing-at-point-looking-at markdown-regex-link-inline)
             (thing-at-point-looking-at markdown-regex-link-reference)
             (thing-at-point-looking-at markdown-regex-uri)
             (thing-at-point-looking-at markdown-regex-angle-uri)))))

(defun markdown-link-link ()
  "Return the link part of the regular (non-wiki) link at point.
Works with both inline and reference style links.  If point is
not at a link or the link reference is not defined returns nil."
  (cond
   ((thing-at-point-looking-at markdown-regex-link-inline)
    (substring-no-properties (match-string 2) 1 -1))
   ((thing-at-point-looking-at markdown-regex-link-reference)
    (let* ((label (match-string-no-properties 1))
           (reference (match-string-no-properties 2))
           (target (downcase (if (string= reference "[]") label reference))))
      (markdown-has-reference-definition target)))
   ((thing-at-point-looking-at markdown-regex-uri)
    (match-string-no-properties 0))
   ((thing-at-point-looking-at markdown-regex-angle-uri)
    (match-string-no-properties 2))
   (t nil)))

(defun markdown-follow-link-at-point ()
  "Open the current non-wiki link in a browser."
  (interactive)
  (if (markdown-link-p) (browse-url (markdown-link-link))
    (error "Point is not at a Markdown link or URI")))


;;; WikiLink Following/Markup =================================================

(defun markdown-wiki-link-p ()
  "Return non-nil when `point' is at a true wiki link.
A true wiki link name matches `markdown-regex-wiki-link' but does not
match the current file name after conversion.  This modifies the data
returned by `match-data'.  Note that the potential wiki link name must
be available via `match-string'."
  (let ((case-fold-search nil))
    (and (thing-at-point-looking-at markdown-regex-wiki-link)
	 (or (not buffer-file-name)
	     (not (string-equal (buffer-file-name)
				(markdown-convert-wiki-link-to-filename
                                 (markdown-wiki-link-link)))))
	 (not (save-match-data
		(save-excursion))))))

(defun markdown-wiki-link-link ()
  "Return the link part of the wiki link using current match data.
The location of the link component depends on the value of
`markdown-wiki-link-alias-first'."
  (if markdown-wiki-link-alias-first
      (or (match-string 3) (match-string 1))
    (match-string 1)))

(defun markdown-convert-wiki-link-to-filename (name)
  "Generate a filename from the wiki link NAME.
Spaces in NAME are replaced with `markdown-link-space-sub-char'.
When in `gfm-mode', follow GitHub's conventions where [[Test Test]]
and [[test test]] both map to Test-test.ext."
  (let ((basename (markdown-replace-regexp-in-string
                   "[[:space:]\n]" markdown-link-space-sub-char name)))
    (when (eq major-mode 'gfm-mode)
      (setq basename (concat (upcase (substring basename 0 1))
                             (downcase (substring basename 1 nil)))))
    (concat basename
            (if (buffer-file-name)
                (concat "."
                        (file-name-extension (buffer-file-name)))))))

(defun markdown-follow-wiki-link (name &optional other)
  "Follow the wiki link NAME.
Convert the name to a file name and call `find-file'.  Ensure that
the new buffer remains in `markdown-mode'."
  (let ((filename (markdown-convert-wiki-link-to-filename name))
        (wp (file-name-directory buffer-file-name)))
    (when other (other-window 1))
    (find-file (concat wp filename)))
  (when (not (eq major-mode 'markdown-mode))
    (markdown-mode)))

(defun markdown-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument C-u open the file in other window.

See `markdown-wiki-link-p' and `markdown-follow-wiki-link'."
  (interactive "P")
  (if (markdown-wiki-link-p)
      (markdown-follow-wiki-link (markdown-wiki-link-link) arg)
    (error "Point is not at a Wiki Link")))

(defun markdown-next-wiki-link ()
  "Jump to next wiki link.
See `markdown-wiki-link-p'."
  (interactive)
  (if (markdown-wiki-link-p)
      ;; At a wiki link already, move past it.
      (goto-char (+ 1 (match-end 0))))
  (save-match-data
    ;; Search for the next wiki link and move to the beginning.
    (re-search-forward markdown-regex-wiki-link nil t)
    (goto-char (match-beginning 0))))

(defun markdown-previous-wiki-link ()
  "Jump to previous wiki link.
See `markdown-wiki-link-p'."
  (interactive)
  (re-search-backward markdown-regex-wiki-link nil t))

(defun markdown-highlight-wiki-link (from to face)
  "Highlight the wiki link in the region between FROM and TO using FACE."
  (put-text-property from to 'font-lock-face face))

(defun markdown-unfontify-region-wiki-links (from to)
  "Remove wiki link faces from the region specified by FROM and TO."
  (interactive "nfrom: \nnto: ")
  (remove-text-properties from to '(font-lock-face markdown-link-face))
  (remove-text-properties from to '(font-lock-face markdown-missing-link-face)))

(defun markdown-fontify-region-wiki-links (from to)
  "Search region given by FROM and TO for wiki links and fontify them.
If a wiki link is found check to see if the backing file exists
and highlight accordingly."
  (goto-char from)
  (save-match-data
    (while (re-search-forward markdown-regex-wiki-link to t)
      (let ((highlight-beginning (match-beginning 0))
	    (highlight-end (match-end 0))
	    (file-name
	     (markdown-convert-wiki-link-to-filename (match-string 1))))
	(if (file-exists-p file-name)
	    (markdown-highlight-wiki-link
	     highlight-beginning highlight-end markdown-link-face)
	  (markdown-highlight-wiki-link
	   highlight-beginning highlight-end markdown-link-face)
          (markdown-highlight-wiki-link
           highlight-beginning highlight-end markdown-missing-link-face))))))

(defun markdown-extend-changed-region (from to)
  "Extend region given by FROM and TO so that we can fontify all links.
The region is extended to the first newline before and the first
newline after."
  ;; start looking for the first new line before 'from
  (goto-char from)
  (re-search-backward "\n" nil t)
  (let ((new-from (point-min))
	(new-to (point-max)))
    (if (not (= (point) from))
	(setq new-from (point)))
    ;; do the same thing for the first new line after 'to
    (goto-char to)
    (re-search-forward "\n" nil t)
    (if (not (= (point) to))
	(setq new-to (point)))
    (values new-from new-to)))

(defun markdown-check-change-for-wiki-link (from to change)
  "Check region between FROM and TO for wiki links and re-fontfy as needed.
Designed to be used with the `after-change-functions' hook.
CHANGE is the number of bytes of pre-change text replaced by the
given range."
  (interactive "nfrom: \nnto: \nnchange: ")
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t)
         (inhibit-point-motion-hooks t)
         deactivate-mark
         buffer-file-truename)
     (unwind-protect
         (save-excursion
           (save-match-data
             (save-restriction
               ;; Extend the region to fontify so that it starts
               ;; and ends at safe places.
               (multiple-value-bind (new-from new-to)
                   (markdown-extend-changed-region from to)
                 ;; Unfontify existing fontification (start from scratch)
                 (markdown-unfontify-region-wiki-links new-from new-to)
                 ;; Now do the fontification.
                 (markdown-fontify-region-wiki-links new-from new-to)))))
       (and (not modified)
            (buffer-modified-p)
            (set-buffer-modified-p nil)))))

(defun markdown-fontify-buffer-wiki-links ()
  "Refontify all wiki links in the buffer."
  (interactive)
  (markdown-check-change-for-wiki-link (point-min) (point-max) 0))

;;; Miscellaneous =============================================================

(defun markdown-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun markdown-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*"))



;;; Mode definition  ==========================================================

(defun markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "markdown-mode, version %s" markdown-mode-version))

;;;###autoload
(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  ;; Natural Markdown tab width
  (setq tab-width 4)
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(markdown-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; For imenu support
  (setq imenu-create-index-function 'markdown-imenu-create-index)
  ;; For menu support in XEmacs
  (easy-menu-add markdown-mode-menu markdown-mode-map)
  (set (make-local-variable 'beginning-of-defun-function)
       'markdown-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'markdown-end-of-defun)
  ;; Make filling work with lists (unordered, ordered, and definition)
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t]*[0-9]+\\.\\|^[ \t]*: ")
  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp markdown-regex-header)
  (make-local-variable 'outline-level)
  (setq outline-level 'markdown-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p)
  (setq indent-line-function markdown-indent-function)

  ;; Prepare hooks for XEmacs compatibility
  (when (featurep 'xemacs)
    (make-local-hook 'after-change-functions)
    (make-local-hook 'font-lock-extend-region-functions)
    (make-local-hook 'window-configuration-change-hook))

  ;; Multiline font lock
  (add-hook 'font-lock-extend-region-functions
            'markdown-font-lock-extend-region)

  ;; Anytime text changes make sure it gets fontified correctly
  (add-hook 'after-change-functions 'markdown-check-change-for-wiki-link t t)

  ;; If we left the buffer there is a really good chance we were
  ;; creating one of the wiki link documents. Make sure we get
  ;; refontified when we come back.
  (add-hook 'window-configuration-change-hook
	    'markdown-fontify-buffer-wiki-links t t)

  ;; do the initial link fontification
  (markdown-fontify-buffer-wiki-links))

;;(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))

;;; GitHub Flavored Markdown Mode  ============================================

;;;###autoload
(define-derived-mode gfm-mode markdown-mode "GFM"
  "Major mode for editing GitHub Flavored Markdown files."
  (setq markdown-link-space-sub-char "-")
  (auto-fill-mode 0)
  ;; Use visual-line-mode if available, fall back to longlines-mode:
  (if (fboundp 'visual-line-mode)
      (visual-line-mode 1)
    (longlines-mode 1))
  ;; do the initial link fontification
  (markdown-fontify-buffer-wiki-links))


(provide 'markdown-mode)

;;; markdown-mode.el ends here
