# Emacs Markdown Mode [![MELPA badge][melpa-badge]][melpa-link] [![MELPA stable badge][melpa-stable-badge]][melpa-stable-link] [![Travis CI Build Status][travis-badge]][travis-link] [![Guide to Markdown Mode for Emacs][leanpub-badge]][leanpub-link]

  [melpa-link]: https://melpa.org/#/markdown-mode
  [melpa-stable-link]: https://stable.melpa.org/#/markdown-mode
  [melpa-badge]: https://melpa.org/packages/markdown-mode-badge.svg
  [melpa-stable-badge]: https://stable.melpa.org/packages/markdown-mode-badge.svg
  [travis-link]: https://travis-ci.org/jrblevin/markdown-mode
  [travis-badge]: https://travis-ci.org/jrblevin/markdown-mode.svg?branch=master
  [leanpub-link]: https://leanpub.com/markdown-mode
  [leanpub-badge]: https://img.shields.io/badge/leanpub-guide-orange.svg

markdown-mode is a major mode for editing [Markdown][]-formatted
text.  The latest stable version is markdown-mode 2.3, released on
August 31, 2017.  See the [release notes][] for details.
markdown-mode is free software, licensed under the GNU GPL,
version 3 or later.

![Markdown Mode Screenshot](https://jblevins.org/projects/markdown-mode/screenshots/20170818-001.png)

[Markdown]: http://daringfireball.net/projects/markdown/
[release notes]: https://jblevins.org/projects/markdown-mode/rev-2-3

## Documentation

<a href="https://leanpub.com/markdown-mode">
<img src="https://jblevins.org/projects/markdown-mode/guide-v2.3.png" align="right" height="350" width="231">
</a>

The primary documentation for Markdown Mode is available below, and
is generated from comments in the source code.  For a more in-depth
treatment, the [_Guide to Markdown Mode for Emacs_][guide] covers
Markdown syntax, advanced movement and editing in Emacs,
extensions, configuration examples, tips and tricks, and a survey
of other packages that work with Markdown Mode.  Finally, Emacs is
also a self-documenting editor.  This means that the source code
itself contains additional documentation: each function has its own
docstring available via <kbd>C-h f</kbd> (`describe-function`), individual
keybindings can be investigated with <kbd>C-h k</kbd> (`describe-key`), and
a complete list of keybindings is available using <kbd>C-h m</kbd>
(`describe-mode`).

 [guide]: https://leanpub.com/markdown-mode

## Installation

_Note:_ To use all of the features of `markdown-mode`, you'll need
to install the Emacs package itself and also have a local Markdown
processor installed (e.g., Markdown.pl, MultiMarkdown, or Pandoc).
The external processor is not required for editing, but will be
used for rendering HTML for preview and export. After installing
the Emacs package, be sure to configure `markdown-command` to point
to the preferred Markdown executable on your system.  See the
Customization section below for more details.

The recommended way to install `markdown-mode` is to install the package
from [MELPA Stable](https://stable.melpa.org/#/markdown-mode)
using `package.el`. First, configure `package.el` and the MELPA Stable
repository by adding the following to your `.emacs`, `init.el`,
or equivalent startup file:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
```

Then, after restarting Emacs or evaluating the above statements, issue
the following command: <kbd>M-x package-install RET markdown-mode RET</kbd>.
When installed this way, the major modes `markdown-mode` and `gfm-mode`
will be autoloaded and `markdown-mode` will be used for file names
ending in either `.md` or `.markdown`.

Alternatively, if you manage loading packages with [use-package][]
then you can automatically install and configure `markdown-mode` by
adding a declaration such as this one to your init file (as an
example; adjust settings as desired):

```lisp
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
```

[MELPA Stable]: http://stable.melpa.org/
[use-package]: https://github.com/jwiegley/use-package

**Direct Download**

Alternatively you can manually download and install markdown-mode.
First, download the [latest stable version][markdown-mode.el] and
save the file where Emacs can find it (i.e., a directory in your
`load-path`). You can then configure `markdown-mode` and `gfm-mode`
to load automatically by adding the following to your init file:

```lisp
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
```

[markdown-mode.el]: https://jblevins.org/projects/markdown-mode/markdown-mode.el

**Development Version**

To follow or contribute to markdown-mode development, you can
browse or clone the Git repository
[on GitHub](https://github.com/jrblevin/markdown-mode):

```
git clone https://github.com/jrblevin/markdown-mode.git
```

If you prefer to install and use the development version, which may
become unstable at some times, you can either clone the Git
repository as above or install markdown-mode from
[MELPA](https://melpa.org/#/markdown-mode).

If you clone the repository directly, then make sure that Emacs can
find it by adding the following line to your startup file:

```lisp
(add-to-list 'load-path "/path/to/markdown-mode/repository")
```

**Packaged Installation**

markdown-mode is also available in several package managers. You
may want to confirm that the package you install contains the
latest stable version first (and please notify the package
maintainer if not).

   * Debian Linux: [elpa-markdown-mode][] and [emacs-goodies-el][]
   * Ubuntu Linux: [elpa-markdown-mode][elpa-ubuntu] and [emacs-goodies-el][emacs-goodies-el-ubuntu]
   * RedHat and Fedora Linux: [emacs-goodies][]
   * NetBSD: [textproc/markdown-mode][]
   * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
   * FreeBSD: [textproc/markdown-mode.el][freebsd-port]

 [elpa-markdown-mode]: https://packages.debian.org/sid/lisp/elpa-markdown-mode
 [elpa-ubuntu]: http://packages.ubuntu.com/search?keywords=elpa-markdown-mode
 [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
 [emacs-goodies-el-ubuntu]: http://packages.ubuntu.com/search?keywords=emacs-goodies-el
 [emacs-goodies]: https://apps.fedoraproject.org/packages/emacs-goodies
 [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
 [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
 [macports-ticket]: http://trac.macports.org/ticket/35716
 [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el

**Dependencies**

To enable editing of code blocks in indirect buffers using <kbd>C-c '</kbd>,
you will need to install the [`edit-indirect`][ei] package.

  [ei]: https://github.com/Fanael/edit-indirect/

## Usage

Keybindings are grouped by prefixes based on their function.  For
example, the commands for styling text are grouped under <kbd>C-c C-s</kbd>
and toggle commands begin with <kbd>C-c C-x</kbd>.  The primary commands in
each group will are described below.  You can obtain a list of all
keybindings by pressing <kbd>C-c C-h</kbd>.  Movement and shifting commands
tend to be associated with paired delimiters such as <kbd>M-{</kbd> and
<kbd>M-}</kbd> or <kbd>C-c <</kbd> and <kbd>C-c ></kbd>.  Outline navigation keybindings the
same as in `org-mode`.  Finally, commands for running Markdown or
doing maintenance on an open file are grouped under the <kbd>C-c C-c</kbd>
prefix.  The most commonly used commands are described below. You
can obtain a list of all keybindings by pressing <kbd>C-c C-h</kbd>.

  * Links and Images: <kbd>C-c C-l</kbd> and <kbd>C-c C-i</kbd>

    <kbd>C-c C-l</kbd> (`markdown-insert-link`) is a general command for
    inserting new link markup or editing existing link markup. This
    is especially useful when markup or URL hiding is enabled, so
    that URLs can't easily be edited directly.  This command can be
    used to insert links of any form: either inline links,
    reference links, or plain URLs in angle brackets.  The URL or
    `[reference]` label, link text, and optional title are entered
    through a series of interactive prompts.  The type of link is
    determined by which values are provided:

    *   If both a URL and link text are given, insert an inline link:
        `[text](url)`.
    *   If both a `[reference]` label and link text are given, insert
        a reference link: `[text][reference]`.
    *   If only link text is given, insert an implicit reference link:
        `[text][]`.
    *   If only a URL is given, insert a plain URL link:
        `<url>`.

    Similarly, <kbd>C-c C-i</kbd> (`markdown-insert-image`) is a general
    command for inserting or editing image markup.  As with the link
    insertion command, through a series interactive prompts you can
    insert either an inline or reference image:

    *   If both a URL and alt text are given, insert an inline
        image: `![alt text](url)`.
    *   If both a `[reference]` label and alt text are given,
        insert a reference link: `![alt text][reference]`.

    If there is an existing link or image at the point, these
    command will edit the existing markup rather than inserting new
    markup.  Otherwise, if there is an active region, these commands
    use the region as either the default URL (if it seems to be a
    URL) or link text value otherwise.  In that case, the region
    will be deleted and replaced by the link.

    Note that these functions can be used to convert links and
    images from one type to another (inline, reference, or plain
    URL) by selectively adding or removing properties via the
    interactive prompts.

    If a reference label is given that is not yet defined, you
    will be prompted for the URL and optional title and the
    reference will be inserted according to the value of
    `markdown-reference-location`.  If a title is given, it will be
    added to the end of the reference definition and will be used
    to populate the title attribute when converted to HTML.

    Local images associated with image links may be displayed
    inline in the buffer by pressing <kbd>C-c C-x C-i</kbd>
    (`markdown-toggle-inline-images`).  This is a toggle command, so
    pressing this once again will remove inline images.  Large
    images may be scaled down to fit in the buffer using
    `markdown-max-image-size`, a cons cell of the form
    `(max-width . max-height)`.  Resizing requires Emacs to be
    built with ImageMagick support.

  * Text Styles: <kbd>C-c C-s</kbd>

    <kbd>C-c C-s i</kbd> inserts markup to make a region or word italic. If
    there is an active region, make the region italic.  If the point
    is at a non-italic word, make the word italic.  If the point is
    at an italic word or phrase, remove the italic markup.
    Otherwise, simply insert italic delimiters and place the point
    in between them.  Similarly, use <kbd>C-c C-s b</kbd> for bold, <kbd>C-c C-s c</kbd>
    for inline code, and <kbd>C-c C-s k</kbd> for inserting `<kbd>` tags.

    <kbd>C-c C-s q</kbd> inserts a blockquote using the active region, if
    any, or starts a new blockquote. <kbd>C-c C-s Q</kbd> is a variation
    which always operates on the region, regardless of whether it
    is active or not (i.e., when `transient-mark-mode` is off but
    the mark is set).  The appropriate amount of indentation, if
    any, is calculated automatically given the surrounding context,
    but may be adjusted later using the region indentation
    commands.

    <kbd>C-c C-s p</kbd> behaves similarly for inserting preformatted code
    blocks (with <kbd>C-c C-s P</kbd> being the region-only counterpart)
    and <kbd>C-c C-s C</kbd> inserts a GFM style backquote fenced code block.

  * Headings: <kbd>C-c C-s</kbd>

    To insert or replace headings, there are two options.  You can
    insert a specific level heading directly or you can have
    `markdown-mode` determine the level for you based on the previous
    heading.  As with the other markup commands, the heading
    insertion commands use the text in the active region, if any,
    as the heading text.  Otherwise, if the current line is not
    blank, they use the text on the current line.  Finally, the
    setext commands will prompt for heading text if there is no
    active region and the current line is blank.

    <kbd>C-c C-s h</kbd> inserts a heading with automatically chosen type and
    level (both determined by the previous heading).  <kbd>C-c C-s H</kbd>
    behaves similarly, but uses setext (underlined) headings when
    possible, still calculating the level automatically.
    In cases where the automatically-determined level is not what
    you intended, the level can be quickly promoted or demoted
    (as described below).  Alternatively, a <kbd>C-u</kbd> prefix can be
    given to insert a heading _promoted_ (lower number) by one
    level or a <kbd>C-u C-u</kbd> prefix can be given to insert a heading
    demoted (higher number) by one level.

    To insert a heading of a specific level and type, use <kbd>C-c C-s 1</kbd>
    through <kbd>C-c C-s 6</kbd> for atx (hash mark) headings and <kbd>C-c C-s !</kbd> or
    <kbd>C-c C-s @</kbd> for setext headings of level one or two, respectively.
    Note that <kbd>!</kbd> is <kbd>S-1</kbd> and <kbd>@</kbd> is <kbd>S-2</kbd>.

    If the point is at a heading, these commands will replace the
    existing markup in order to update the level and/or type of the
    heading.  To remove the markup of the heading at the point,
    press <kbd>C-c C-k</kbd> to kill the heading and press <kbd>C-y</kbd> to yank the
    heading text back into the buffer.

  * Horizontal Rules: <kbd>C-c C-s -</kbd>

    <kbd>C-c C-s -</kbd> inserts a horizontal rule.  By default, insert the
    first string in the list `markdown-hr-strings` (the most
    prominent rule).  With a <kbd>C-u</kbd> prefix, insert the last string.
    With a numeric prefix <kbd>N</kbd>, insert the string in position <kbd>N</kbd>
    (counting from 1).

  * Footnotes: <kbd>C-c C-s f</kbd>

    <kbd>C-c C-s f</kbd> inserts a footnote marker at the point, inserts a
    footnote definition below, and positions the point for
    inserting the footnote text.  Note that footnotes are an
    extension to Markdown and are not supported by all processors.

  * Wiki Links: <kbd>C-c C-s w</kbd>

    <kbd>C-c C-s w</kbd> inserts a wiki link of the form `[[WikiLink]]`.  If
    there is an active region, use the region as the link text.  If the
    point is at a word, use the word as the link text.  If there is
    no active region and the point is not at word, simply insert
    link markup.  Note that wiki links are an extension to Markdown
    and are not supported by all processors.

  * Markdown and Maintenance Commands: <kbd>C-c C-c</kbd>

    *Compile:* <kbd>C-c C-c m</kbd> will run Markdown on the current buffer
    and show the output in another buffer.  *Preview*: <kbd>C-c C-c p</kbd>
    runs Markdown on the current buffer and previews, stores the
    output in a temporary file, and displays the file in a browser.
    *Export:* <kbd>C-c C-c e</kbd> will run Markdown on the current buffer
    and save the result in the file `basename.html`, where
    `basename` is the name of the Markdown file with the extension
    removed.  *Export and View:* press <kbd>C-c C-c v</kbd> to export the
    file and view it in a browser.  *Open:* <kbd>C-c C-c o</kbd> will open
    the Markdown source file directly using `markdown-open-command`.
    *Live Export*: Press <kbd>C-c C-c l</kbd> to turn on
    `markdown-live-preview-mode` to view the exported output
    side-by-side with the source Markdown. **For all export commands,
    the output file will be overwritten without notice.**
    `markdown-live-preview-window-function` can be customized to open
    in a browser other than `eww`.  If you want to force the
    preview window to appear at the bottom or right, you can
    customize `markdown-split-window-direction`.

    To summarize:

      - <kbd>C-c C-c m</kbd>: `markdown-command` > `*markdown-output*` buffer.
      - <kbd>C-c C-c p</kbd>: `markdown-command` > temporary file > browser.
      - <kbd>C-c C-c e</kbd>: `markdown-command` > `basename.html`.
      - <kbd>C-c C-c v</kbd>: `markdown-command` > `basename.html` > browser.
      - <kbd>C-c C-c w</kbd>: `markdown-command` > kill ring.
      - <kbd>C-c C-c o</kbd>: `markdown-open-command`.
      - <kbd>C-c C-c l</kbd>: `markdown-live-preview-mode` > `*eww*` buffer.

    <kbd>C-c C-c c</kbd> will check for undefined references.  If there are
    any, a small buffer will open with a list of undefined
    references and the line numbers on which they appear.  In Emacs
    22 and greater, selecting a reference from this list and
    pressing <kbd>RET</kbd> will insert an empty reference definition at the
    end of the buffer.  Similarly, selecting the line number will
    jump to the corresponding line.

    <kbd>C-c C-c n</kbd> renumbers any ordered lists in the buffer that are
    out of sequence.

    <kbd>C-c C-c ]</kbd> completes all headings and normalizes all horizontal
    rules in the buffer.

  * Following Links: <kbd>C-c C-o</kbd>

    Press <kbd>C-c C-o</kbd> when the point is on an inline or reference
    link to open the URL in a browser.  When the point is at a
    wiki link, open it in another buffer (in the current window,
    or in the other window with the <kbd>C-u</kbd> prefix).  Use <kbd>M-p</kbd> and
    <kbd>M-n</kbd> to quickly jump to the previous or next link of any type.

  * Doing Things: <kbd>C-c C-d</kbd>

    Use <kbd>C-c C-d</kbd> to do something sensible with the object at the point:

      - Jumps between reference links and reference definitions.
        If more than one link uses the same reference label, a
        window will be shown containing clickable buttons for
        jumping to each link.  Pressing <kbd>TAB</kbd> or <kbd>S-TAB</kbd> cycles
        between buttons in this window.
      - Jumps between footnote markers and footnote text.
      - Toggles the completion status of GFM task list items
        (checkboxes).
      - Re-aligns table columns.

  * Promotion and Demotion: <kbd>C-c C--</kbd> and <kbd>C-c C-=</kbd>

    Headings, horizontal rules, and list items can be promoted and
    demoted, as well as bold and italic text.  For headings,
    "promotion" means *decreasing* the level (i.e., moving from
    `<h2>` to `<h1>`) while "demotion" means *increasing* the
    level.  For horizontal rules, promotion and demotion means
    moving backward or forward through the list of rule strings in
    `markdown-hr-strings`.  For bold and italic text, promotion and
    demotion means changing the markup from underscores to asterisks.
    Press <kbd>C-c C--</kbd> or <kbd>C-c LEFT</kbd> to promote the element at the point
    if possible.

    To remember these commands, note that <kbd>-</kbd> is for decreasing the
    level (promoting), and <kbd>=</kbd> (on the same key as <kbd>+</kbd>) is for
    increasing the level (demoting).  Similarly, the left and right
    arrow keys indicate the direction that the atx heading markup
    is moving in when promoting or demoting.

  * Completion: <kbd>C-c C-]</kbd>

    Complete markup is in normalized form, which means, for
    example, that the underline portion of a setext header is the
    same length as the heading text, or that the number of leading
    and trailing hash marks of an atx header are equal and that
    there is no extra whitespace in the header text.  <kbd>C-c C-]</kbd>
    completes the markup at the point, if it is determined to be
    incomplete.

  * Editing Lists: <kbd>M-RET</kbd>, <kbd>C-c UP</kbd>, <kbd>C-c DOWN</kbd>, <kbd>C-c LEFT</kbd>, and <kbd>C-c RIGHT</kbd>

    New list items can be inserted with <kbd>M-RET</kbd> or <kbd>C-c C-j</kbd>.  This
    command determines the appropriate marker (one of the possible
    unordered list markers or the next number in sequence for an
    ordered list) and indentation level by examining nearby list
    items.  If there is no list before or after the point, start a
    new list.  As with heading insertion, you may prefix this
    command by <kbd>C-u</kbd> to decrease the indentation by one level.
    Prefix this command by <kbd>C-u C-u</kbd> to increase the indentation by
    one level.

    Existing list items (and their nested sub-items) can be moved
    up or down with <kbd>C-c UP</kbd> or <kbd>C-c DOWN</kbd> and indented or
    outdented with <kbd>C-c RIGHT</kbd> or <kbd>C-c LEFT</kbd>.

  * Editing Subtrees: <kbd>C-c UP</kbd>, <kbd>C-c DOWN</kbd>, <kbd>C-c LEFT</kbd>, and <kbd>C-c RIGHT</kbd>

    Entire subtrees of ATX headings can be promoted and demoted
    with <kbd>C-c LEFT</kbd> and <kbd>C-c RIGHT</kbd>, which are the same keybindings
    used for promotion and demotion of list items.   If the point is in
    a list item, the operate on the list item.  Otherwise, they operate
    on the current heading subtree.  Similarly, subtrees can be
    moved up and down with <kbd>C-c UP</kbd> and <kbd>C-c DOWN</kbd>.

    These commands currently do not work properly if there are
    Setext headings in the affected region.

    Please note the following "boundary" behavior for promotion and
    demotion.  Any level-six headings will not be demoted further
    (i.e., they remain at level six, since Markdown and HTML define
    only six levels) and any level-one headings will promoted away
    entirely (i.e., heading markup will be removed, since a
    level-zero heading is not defined).

  * Shifting the Region: <kbd>C-c <</kbd> and <kbd>C-c ></kbd>

    Text in the region can be indented or outdented as a group using
    <kbd>C-c ></kbd> to indent to the next indentation point (calculated in
    the current context), and <kbd>C-c <</kbd> to outdent to the previous
    indentation point.  These keybindings are the same as those for
    similar commands in `python-mode`.

  * Killing Elements: <kbd>C-c C-k</kbd>

    Press <kbd>C-c C-k</kbd> to kill the thing at point and add important
    text, without markup, to the kill ring.  Possible things to
    kill include (roughly in order of precedece): inline code,
    headings, horizonal rules, links (add link text to kill ring),
    images (add alt text to kill ring), angle URIs, email
    addresses, bold, italics, reference definitions (add URI to
    kill ring), footnote markers and text (kill both marker and
    text, add text to kill ring), and list items.

  * Outline Navigation: <kbd>C-c C-n</kbd>, <kbd>C-c C-p</kbd>, <kbd>C-c C-f</kbd>, <kbd>C-c C-b</kbd>, and <kbd>C-c C-u</kbd>

    These keys are used for hierarchical navigation in lists and
    headings.  When the point is in a list, they move between list
    items.  Otherwise, they move between headings.  Use <kbd>C-c C-n</kbd> and
    <kbd>C-c C-p</kbd> to move between the next and previous visible
    headings or list items of any level.  Similarly, <kbd>C-c C-f</kbd> and
    <kbd>C-c C-b</kbd> move to the next and previous visible headings or
    list items at the same level as the one at the point.  Finally,
    <kbd>C-c C-u</kbd> will move up to the parent heading or list item.

  * Movement by Markdown paragraph: <kbd>M-{</kbd>, <kbd>M-}</kbd>, and <kbd>M-h</kbd>

    Paragraphs in `markdown-mode` are regular paragraphs,
    paragraphs inside blockquotes, individual list items, headings,
    etc.  These keys are usually bound to `forward-paragraph` and
    `backward-paragraph`, but the built-in Emacs functions are
    based on simple regular expressions that fail in Markdown
    files.  Instead, they are bound to `markdown-forward-paragraph`
    and `markdown-backward-paragraph`.  To mark a paragraph,
    you can use <kbd>M-h</kbd> (`markdown-mark-paragraph`).

  * Movement by Markdown block: <kbd>C-M-{</kbd>, <kbd>C-M-}</kbd>, and <kbd>C-c M-h</kbd>

    Markdown blocks are regular paragraphs in many cases, but
    contain many paragraphs in other cases: blocks are considered
    to be entire lists, entire code blocks, and entire blockquotes.
    To move backward one block use <kbd>C-M-{</kbd>
    (`markdown-beginning-block`) and to move forward use <kbd>C-M-}</kbd>
    (`markdown-end-of-block`).  To mark a block, use <kbd>C-c M-h</kbd>
    (`markdown-mark-block`).

  * Movement by Defuns: <kbd>C-M-a</kbd>, <kbd>C-M-e</kbd>, and <kbd>C-M-h</kbd>

    The usual Emacs commands can be used to move by defuns
    (top-level major definitions).  In markdown-mode, a defun is a
    section.  As usual, <kbd>C-M-a</kbd> will move the point to the
    beginning of the current or preceding defun, <kbd>C-M-e</kbd> will move
    to the end of the current or following defun, and <kbd>C-M-h</kbd> will
    put the region around the entire defun.

  * Table Editing:

    Markdown Mode includes support for editing tables, which
    have the following basic format:

        | Right | Left | Center | Default |
        |------:|:-----|:------:|---------|
        |    12 | 12   | 12     | 12      |
        |   123 | 123  | 123    | 123     |
        |     1 | 1    | 1      | 1       |

    The first line contains column headers. The second line
    contains a separator line between the headers and the content.
    Each following line is a row in the table.  Columns are always
    separated by the pipe character.  The colons indicate column
    alignment.

    A table is re-aligned automatically each time you press <kbd>TAB</kbd>
    or <kbd>RET</kbd> inside the table.  <kbd>TAB</kbd> also moves to the next
    field (<kbd>RET</kbd> to the next row) and creates new table rows at
    the end of the table or before horizontal separator lines.  The
    indentation of the table is set by the first line.  Column
    centering inside Emacs is not supported.

    Beginning pipe characters are required for proper detection of
    table borders inside Emacs.  Any line starting with `|-` or `|:`
    is considered as a horizontal separator line and will be
    expanded on the next re-align to span the whole table width.  No
    padding is allowed between the beginning pipe character and
    header separator symbol.  So, to create the above table, you
    would only type

        |Right|Left|Center|Default|
        |-

    and then press <kbd>TAB</kbd> to align the table and start filling in
    cells.

    Then you can jump with <kbd>TAB</kbd> from one cell to the next or with
    <kbd>S-TAB</kbd> to the previous one.  <kbd>RET</kbd> will jump to the to the
    next cell in the same column, and create a new row if there is
    no such cell or if the next row is beyond a separator line.

    You can also convert selected region to a table. Basic editing
    capabilities include inserting, deleting, and moving of columns
    and rows, and table re-alignment, sorting, transposition:

      - <kbd>C-c UP</kbd> or <kbd>C-c DOWN</kbd> - Move the current row up or down.
      - <kbd>C-c LEFT</kbd> or <kbd>C-c RIGHT</kbd> - Move the current column left or right.
      - <kbd>C-c S-UP</kbd> - Kill the current row.
      - <kbd>C-c S-DOWN</kbd> - Insert a row above the current row. With a
        prefix argument, row line is created below the current one.
      - <kbd>C-c S-LEFT</kbd> - Kill the current column.
      - <kbd>C-c S-RIGHT</kbd> - Insert a new column to the left of the current one.
      - <kbd>C-c C-d</kbd> - Re-align the current table (`markdown-do`).
      - <kbd>C-c C-c ^</kbd> - Sort the rows of a table by a specified column.
        This command prompts you for the column number and a sort
        method (alphabetical or numerical, optionally in reverse).
      - <kbd>C-c C-c |</kbd> - Convert the region to a table.  This function
        attempts to recognize comma, tab, and space separated data
        and then splits the data into cells accordingly.
      - <kbd>C-c C-c t</kbd> - Transpose table at point.

    The table editing functions try to handle markup hiding
    correctly when calculating column widths, however, columns
    containing hidden markup may not always be aligned properly.

  * Viewing Modes:

    Read-only viewing modes, `markdown-view-mode` and `gfm-view-mode`
    are provided for viewing Markdown content.  These modes provide
    simplified keybindings for navigating the buffer.  Many of these
    are like `help-mode` and `view-mode`, such as <kbd>SPC</kbd>,
    <kbd>DEL</kbd>, <kbd><</kbd>, and <kbd>></kbd> for scrolling,
    <kbd>q</kbd> for quitting, and <kbd>?</kbd> or <kbd>h</kbd> for
    help.  Other keys are provided that mirror the outline navigation
    commands when editing: <kbd>n</kbd>, <kbd>p</kbd>, <kbd>f</kbd>,
    <kbd>b</kbd>, and <kbd>u</kbd>.  Both of these modes enable markup
    hiding by default, but this can be customized by setting
    `markdown-hide-markup-in-view-modes`.

  * Miscellaneous Commands:

    When the [`edit-indirect`][ei] package is installed, <kbd>C-c '</kbd>
    (`markdown-edit-code-block`) can be used to edit a code block
    in an indirect buffer in the native major mode. Press <kbd>C-c C-c</kbd>
    to commit changes and return or <kbd>C-c C-k</kbd> to cancel.  You can
    also give a prefix argument to the insertion command, as in
    <kbd>C-u C-c C-s C</kbd>, to edit the code block in an indirect buffer
    upon insertion.

As noted, many of the commands above behave differently depending
on whether Transient Mark mode is enabled or not.  When it makes
sense, if Transient Mark mode is on and the region is active, the
command applies to the text in the region (e.g., <kbd>C-c C-s b</kbd> makes the
region bold).  For users who prefer to work outside of Transient
Mark mode, since Emacs 22 it can be enabled temporarily by pressing
<kbd>C-SPC C-SPC</kbd>.  When this is not the case, many commands then
proceed to look work with the word or line at the point.

When applicable, commands that specifically act on the region even
outside of Transient Mark mode have the same keybinding as their
standard counterpart, but the letter is uppercase.  For example,
`markdown-insert-blockquote` is bound to <kbd>C-c C-s q</kbd> and only acts on
the region in Transient Mark mode while `markdown-blockquote-region`
is bound to <kbd>C-c C-s Q</kbd> and always applies to the region (when nonempty).

Note that these region-specific functions are useful in many
cases where it may not be obvious.  For example, yanking text from
the kill ring sets the mark at the beginning of the yanked text
and moves the point to the end.  Therefore, the (inactive) region
contains the yanked text.  So, <kbd>C-y</kbd> followed by <kbd>C-c C-s Q</kbd> will
yank text and turn it into a blockquote.

markdown-mode attempts to be flexible in how it handles
indentation.  When you press <kbd>TAB</kbd> repeatedly, the point will cycle
through several possible indentation levels corresponding to things
you might have in mind when you press <kbd>RET</kbd> at the end of a line or
<kbd>TAB</kbd>.  For example, you may want to start a new list item,
continue a list item with hanging indentation, indent for a nested
pre block, and so on.  Outdenting is handled similarly when backspace
is pressed at the beginning of the non-whitespace portion of a line.

markdown-mode supports outline-minor-mode as well as org-mode-style
visibility cycling for atx- or hash-style headings.  There are two
types of visibility cycling: Pressing <kbd>S-TAB</kbd> cycles globally between
the table of contents view (headings only), outline view (top-level
headings only), and the full document view.  Pressing <kbd>TAB</kbd> while the
point is at a heading will cycle through levels of visibility for the
subtree: completely folded, visible children, and fully visible.
Note that mixing hash and underline style headings will give undesired
results.

## Customization

Although no configuration is *necessary* there are a few things
that can be customized.  The <kbd>M-x customize-mode</kbd> command
provides an interface to all of the possible customizations:

  * `markdown-command` - the command used to run Markdown (default:
    `markdown`).  This variable may be customized to pass
    command-line options to your Markdown processor of choice.  It can
    also be a function; in this case `markdown` will call it with three
    arguments: the beginning and end of the region to process, and
    a buffer to write the output to.

  * `markdown-command-needs-filename` - set to `t` if
    `markdown-command` does not accept standard input (default:
    `nil`).  When `nil`, `markdown-mode` will pass the Markdown
    content to `markdown-command` using standard input (`stdin`).
    When set to `t`, `markdown-mode` will pass the name of the file
    as the final command-line argument to `markdown-command`.  Note
    that in the latter case, you will only be able to run
    `markdown-command` from buffers which are visiting a file.  If
    `markdown-command` is a function, `markdown-command-needs-filename`
    is ignored.

  * `markdown-open-command` - the command used for calling a standalone
    Markdown previewer which is capable of opening Markdown source files
    directly (default: `nil`).  This command will be called
    with a single argument, the filename of the current buffer.
    A representative program is the Mac app [Marked 2][], a
    live-updating Markdown previewer which can be [called from a
    simple shell script](https://jblevins.org/log/marked-2-command).
    This variable can also be a function; in this case `markdown-open`
    will call it without arguments to preview the current buffer.

  * `markdown-hr-strings` - list of strings to use when inserting
    horizontal rules.  Different strings will not be distinguished
    when converted to HTML--they will all be converted to
    `<hr/>`--but they may add visual distinction and style to plain
    text documents.  To maintain some notion of promotion and
    demotion, keep these sorted from largest to smallest.

  * `markdown-bold-underscore` - set to a non-nil value to use two
    underscores when inserting bold text instead of two asterisks
    (default: `nil`).

  * `markdown-italic-underscore` - set to a non-nil value to use
    underscores when inserting italic text instead of asterisks
    (default: `nil`).

  * `markdown-asymmetric-header` - set to a non-nil value to use
    asymmetric header styling, placing header characters only on
    the left of headers (default: `nil`).

  * `markdown-header-scaling` - set to a non-nil value to use
    a variable-pitch font for headings where the size corresponds
    to the level of the heading (default: `nil`).

  * `markdown-header-scaling-values` - list of scaling values,
    relative to baseline, for headers of levels one through six,
    used when `markdown-header-scaling` is non-nil
    (default: `(2.0 1.7 1.4 1.1 1.0 1.0)`).

  * `markdown-marginalize-headers` - put opening atx header markup
    in the left margin when non-nil (default: `nil`).

  * `markdown-marginalize-headers-margin-width` - width of margin
    used for marginalized headers (default: 6).

  * `markdown-list-indent-width` - depth of indentation for lists
    when inserting, promoting, and demoting list items (default: 4).

  * `markdown-indent-function` - the function to use for automatic
    indentation (default: `markdown-indent-line`).

  * `markdown-indent-on-enter` - Set to a non-nil value to
    automatically indent new lines when <kbd>RET</kbd> is pressed.
    Set to `indent-and-new-item` to additionally continue lists
    when <kbd>RET</kbd> is pressed (default: `t`).

  * `markdown-enable-wiki-links` - syntax highlighting for wiki
    links (default: `nil`).  Set this to a non-nil value to turn on
    wiki link support by default.  Wiki link support can be toggled
    later using the function `markdown-toggle-wiki-links`."

  * `markdown-wiki-link-alias-first` - set to a non-nil value to
    treat aliased wiki links like `[[link text|PageName]]`
    (default: `t`).  When set to nil, they will be treated as
    `[[PageName|link text]]`.

  * `markdown-uri-types` - a list of protocol schemes (e.g., "http")
    for URIs that `markdown-mode` should highlight.

  * `markdown-enable-math` - font lock for inline and display LaTeX
    math expressions (default: `nil`).  Set this to `t` to turn on
    math support by default.  Math support can be toggled
    interactively later using <kbd>C-c C-x C-e</kbd>
    (`markdown-toggle-math`).

  * `markdown-enable-html` - font lock for HTML tags and attributes
    (default: `t`).

  * `markdown-css-paths` - CSS files to link to in XHTML output
    (default: `nil`).

  * `markdown-content-type` - when set to a nonempty string, an
    `http-equiv` attribute will be included in the XHTML `<head>`
    block (default: `""`).  If needed, the suggested values are
    `application/xhtml+xml` or `text/html`.  See also:
    `markdown-coding-system`.

  * `markdown-coding-system` - used for specifying the character
    set identifier in the `http-equiv` attribute when included
    (default: `nil`).  See `markdown-content-type`, which must
    be set before this variable has any effect.  When set to `nil`,
    `buffer-file-coding-system` will be used to automatically
    determine the coding system string (falling back to
    `iso-8859-1` when unavailable).  Common settings are `utf-8`
    and `iso-latin-1`.

  * `markdown-xhtml-header-content` - additional content to include
    in the XHTML `<head>` block (default: `""`).

  * `markdown-xhtml-body-preamble` - additional content to include in
    the XHTML <body> block, before the output (default: `""`).  This
    is useful for enclosing additional elements around the Markdown
    output.

  * `markdown-xhtml-body-epilogue` - additional content to include in
    the XHTML <body> block, after the output (default: `""`).  This is
    useful for enclosing additional elements around the Markdown
    output.

  * `markdown-xhtml-standalone-regexp` - a regular expression which
    `markdown-mode` uses to determine whether the output of
    `markdown-command` is a standalone XHTML document or an XHTML
    fragment (default: `"^\\(<\\?xml\\|<!DOCTYPE\\|<html\\)"`).  If
    this regular expression not matched in the first five lines of
    output, `markdown-mode` assumes the output is a fragment and
    adds a header and footer.

  * `markdown-link-space-sub-char` - a character to replace spaces
    when mapping wiki links to filenames (default: `"_"`).
    For example, use an underscore for compatibility with the
    Python Markdown WikiLinks extension.  In `gfm-mode`, this is
    set to `"-"` to conform with GitHub wiki links.

  * `markdown-reference-location` - where to insert reference
    definitions (default: `header`).  The possible locations are
    the end of the document (`end`), after the current block
    (`immediately`), the end of the current subtree (`subtree`),
    or before the next header (`header`).

  * `markdown-footnote-location` - where to insert footnote text
    (default: `end`).  The set of location options is the same as
    for `markdown-reference-location`.

  * `markdown-nested-imenu-heading-index` - Use nested imenu
    heading instead of a flat index (default: `t`).  A nested
    index may provide more natural browsing from the menu, but a
    flat list may allow for faster keyboard navigation via tab
    completion.

  * `markdown-add-footnotes-to-imenu` - Add footnote definitions to
    the end of the imenu index (default: `t`).

  * `comment-auto-fill-only-comments` - variable is made
    buffer-local and set to `nil` by default.  In programming
    language modes, when this variable is non-nil, only comments
    will be filled by auto-fill-mode.  However, comments in
    Markdown documents are rare and the most users probably intend
    for the actual content of the document to be filled.  Making
    this variable buffer-local allows `markdown-mode` to override
    the default behavior induced when the global variable is non-nil.

  * `markdown-gfm-additional-languages`, - additional languages to
    make available, aside from those predefined in
    `markdown-gfm-recognized-languages`, when inserting GFM code
    blocks (default: `nil`). Language strings must have be trimmed
    of whitespace and not contain any curly braces. They may be of
    arbitrary capitalization, though.

  * `markdown-gfm-use-electric-backquote` - use
    `markdown-electric-backquote` for interactive insertion of GFM
    code blocks when backquote is pressed three times (default: `t`).

  * `markdown-make-gfm-checkboxes-buttons` - Whether GitHub
    Flavored Markdown style task lists (checkboxes) should be
    turned into buttons that can be toggled with mouse-1 or RET. If
    non-nil (default), then buttons are enabled.  This works in
    `markdown-mode` as well as `gfm-mode`.

  * `markdown-hide-urls` - Determines whether URL and reference
    labels are hidden for inline and reference links (default: `nil`).
    When non-nil, inline links will appear in the buffer as
    `[link](∞)` instead of
    `[link](http://perhaps.a/very/long/url/)`.  To change the
    placeholder (composition) character used, set the variable
    `markdown-url-compose-char`.  URL hiding can be toggled
    interactively using <kbd>C-c C-x C-l</kbd> (`markdown-toggle-url-hiding`)
    or from the Markdown | Links & Images menu.

  * `markdown-hide-markup` - Determines whether all possible markup
    is hidden or otherwise beautified (default: `nil`).   The actual
    buffer text remains unchanged, but the display will be altered.
    Brackets and URLs for links will be hidden, asterisks and
    underscores for italic and bold text will be hidden, text
    bullets for unordered lists will be replaced by Unicode
    bullets, and so on.  Since this includes URLs and reference
    labels, when non-nil this setting supersedes `markdown-hide-urls`.
    Markup hiding can be toggled using <kbd>C-c C-x C-m</kbd>
    (`markdown-toggle-markup-hiding`) or from the Markdown | Show &
    Hide menu.

    Unicode bullets are used to replace ASCII list item markers.
    The list of characters used, in order of list level, can be
    specified by setting the variable `markdown-list-item-bullets`.
    The placeholder characters used to replace other markup can
    be changed by customizing the corresponding variables:
    `markdown-blockquote-display-char`,
    `markdown-hr-display-char`, and
    `markdown-definition-display-char`.

  * `markdown-fontify-code-blocks-natively` - Whether to fontify
    code in code blocks using the native major mode.  This only
    works for fenced code blocks where the language is specified
    where we can automatically determine the appropriate mode to
    use.  The language to mode mapping may be customized by setting
    the variable `markdown-code-lang-modes`.  This can be toggled
    interactively by pressing <kbd>C-c C-x C-f</kbd>
    (`markdown-toggle-fontify-code-blocks-natively`).

  * `markdown-gfm-uppercase-checkbox` - When non-nil, complete GFM
    task list items with `[X]` instead of `[x]` (default: `nil`).
    This is useful for compatibility with `org-mode`, which doesn't
    recognize the lowercase variant.

  * `markdown-translate-filename-function` - A function to be used to
    translate filenames in links.

Additionally, the faces used for syntax highlighting can be modified to
your liking by issuing <kbd>M-x customize-group RET markdown-faces</kbd>
or by using the "Markdown Faces" link at the bottom of the mode
customization screen.

[Marked 2]: https://itunes.apple.com/us/app/marked-2/id890031187?mt=12&uo=4&at=11l5Vs&ct=mm

## Extensions

Besides supporting the basic Markdown syntax, Markdown Mode also
includes syntax highlighting for `[[Wiki Links]]`.  This can be
enabled by setting `markdown-enable-wiki-links` to a non-nil value.
Wiki links may be followed by pressing <kbd>C-c C-o</kbd> when the point
is at a wiki link.  Use <kbd>M-p</kbd> and <kbd>M-n</kbd> to quickly jump to the
previous and next links (including links of other types).
Aliased or piped wiki links of the form `[[link text|PageName]]`
are also supported.  Since some wikis reverse these components, set
`markdown-wiki-link-alias-first` to nil to treat them as
`[[PageName|link text]]`.  If `markdown-wiki-link-fontify-missing`
is also non-nil, Markdown Mode will highlight wiki links with
missing target file in a different color.  By default, Markdown
Mode only searches for target files in the current directory.
Search in subdirectories can be enabled by setting
`markdown-wiki-link-search-subdirectories` to a non-nil value.
Sequential parent directory search (as in [Ikiwiki][]) can be
enabled by setting `markdown-wiki-link-search-parent-directories`
to a non-nil value.

[Ikiwiki]: https://ikiwiki.info

[SmartyPants][] support is possible by customizing `markdown-command`.
If you install `SmartyPants.pl` at, say, `/usr/local/bin/smartypants`,
then you can set `markdown-command` to `"markdown | smartypants"`.
You can do this either by using <kbd>M-x customize-group markdown</kbd>
or by placing the following in your `.emacs` file:

```lisp
(setq markdown-command "markdown | smartypants")
```

[SmartyPants]: http://daringfireball.net/projects/smartypants/

Syntax highlighting for mathematical expressions written
in LaTeX (only expressions denoted by `$..$`, `$$..$$`, or `\[..\]`)
can be enabled by setting `markdown-enable-math` to a non-nil value,
either via customize or by placing `(setq markdown-enable-math t)`
in `.emacs`, and then restarting Emacs or calling
`markdown-reload-extensions`.

## GitHub Flavored Markdown (GFM)

A [GitHub Flavored Markdown][GFM] mode, `gfm-mode`, is also
available.  The GitHub implementation differs slightly from
standard Markdown in that it supports things like different
behavior for underscores inside of words, automatic linking of
URLs, strikethrough text, and fenced code blocks with an optional
language keyword.

The GFM-specific features above apply to `README.md` files, wiki
pages, and other Markdown-formatted files in repositories on
GitHub.  GitHub also enables [additional features][GFM comments] for
writing on the site (for issues, pull requests, messages, etc.)
that are further extensions of GFM.  These features include task
lists (checkboxes), newlines corresponding to hard line breaks,
auto-linked references to issues and commits, wiki links, and so
on.  To make matters more confusing, although task lists are not
part of [GFM proper][GFM], [since 2014][] they are rendered (in a
read-only fashion) in all Markdown documents in repositories on the
site.  These additional extensions are supported to varying degrees
by `markdown-mode` and `gfm-mode` as described below.

* **URL autolinking:** Both `markdown-mode` and `gfm-mode` support
  highlighting of URLs without angle brackets.

* **Multiple underscores in words:** You must enable `gfm-mode` to
  toggle support for underscores inside of words. In this mode
  variable names such as `a_test_variable` will not trigger
  emphasis (italics).

* **Fenced code blocks:** Code blocks quoted with backquotes, with
  optional programming language keywords, are highlighted in
  both `markdown-mode` and `gfm-mode`.  They can be inserted with
  <kbd>C-c C-s C</kbd>.  If there is an active region, the text in the
  region will be placed inside the code block.  You will be
  prompted for the name of the language, but may press enter to
  continue without naming a language.

* **Strikethrough:** Strikethrough text is supported in both
  `markdown-mode` and `gfm-mode`.  It can be inserted (and toggled)
  using <kbd>C-c C-s s</kbd>.

* **Task lists:** GFM task lists will be rendered as checkboxes
  (Emacs buttons) in both `markdown-mode` and `gfm-mode` when
  `markdown-make-gfm-checkboxes-buttons` is set to a non-nil value
  (and it is set to t by default).  These checkboxes can be
  toggled by clicking `mouse-1`, pressing <kbd>RET</kbd> over the button,
  or by pressing <kbd>C-c C-d</kbd> (`markdown-do`) with the point anywhere
  in the task list item.  A normal list item can be turned to a
  check list item by the same command, or more specifically
  <kbd>C-c C-s [</kbd> (`markdown-insert-gfm-checkbox`).

* **Wiki links:** Generic wiki links are supported in
  `markdown-mode`, but in `gfm-mode` specifically they will be
  treated as they are on GitHub: spaces will be replaced by hyphens
  in filenames and the first letter of the filename will be
  capitalized.  For example, `[[wiki link]]` will map to a file
  named `Wiki-link` with the same extension as the current file.
  If a file with this name does not exist in the current directory,
  the first match in a subdirectory, if any, will be used instead.

* **Newlines:** Neither `markdown-mode` nor `gfm-mode` do anything
  specifically with respect to newline behavior.  If you use
  `gfm-mode` mostly to write text for comments or issues on the
  GitHub site--where newlines are significant and correspond to
  hard line breaks--then you may want to enable `visual-line-mode`
  for line wrapping in buffers.  You can do this with a
  `gfm-mode-hook` as follows:

    ```lisp
    ;; Use visual-line-mode in gfm-mode
    (defun my-gfm-mode-hook ()
      (visual-line-mode 1))
    (add-hook 'gfm-mode-hook 'my-gfm-mode-hook)
    ```

* **Preview:** GFM-specific preview can be powered by setting
  `markdown-command` to use [Docter][].  This may also be
  configured to work with [Marked 2][] for `markdown-open-command`.

[GFM]: http://github.github.com/github-flavored-markdown/
[GFM comments]: https://help.github.com/articles/writing-on-github/
[since 2014]: https://github.com/blog/1825-task-lists-in-all-markdown-documents
[Docter]: https://github.com/alampros/Docter

## Acknowledgments

markdown-mode has benefited greatly from the efforts of the many
volunteers who have sent patches, test cases, bug reports,
suggestions, helped with packaging, etc.  Thank you for your
contributions!  See the [contributors graph][contrib] for details.

 [contrib]: https://github.com/jrblevin/markdown-mode/graphs/contributors

## Bugs

markdown-mode is developed and tested primarily for compatibility
with GNU Emacs 24.4 and later.  If you find any bugs in
markdown-mode, please construct a test case or a patch and open a
ticket on the [GitHub issue tracker][issues].  See the
contributing guidelines in `CONTRIBUTING.md` for details on
creating pull requests.

 [issues]: https://github.com/jrblevin/markdown-mode/issues

## History

markdown-mode was written and is maintained by Jason Blevins.  The
first version was released on May 24, 2007.

  * 2007-05-24: [Version 1.1][]
  * 2007-05-25: [Version 1.2][]
  * 2007-06-05: [Version 1.3][]
  * 2007-06-29: [Version 1.4][]
  * 2007-10-11: [Version 1.5][]
  * 2008-06-04: [Version 1.6][]
  * 2009-10-01: [Version 1.7][]
  * 2011-08-12: [Version 1.8][]
  * 2011-08-15: [Version 1.8.1][]
  * 2013-01-25: [Version 1.9][]
  * 2013-03-24: [Version 2.0][]
  * 2016-01-09: [Version 2.1][]
  * 2017-05-26: [Version 2.2][]
  * 2017-08-31: [Version 2.3][]

[Version 1.1]: https://jblevins.org/projects/markdown-mode/rev-1-1
[Version 1.2]: https://jblevins.org/projects/markdown-mode/rev-1-2
[Version 1.3]: https://jblevins.org/projects/markdown-mode/rev-1-3
[Version 1.4]: https://jblevins.org/projects/markdown-mode/rev-1-4
[Version 1.5]: https://jblevins.org/projects/markdown-mode/rev-1-5
[Version 1.6]: https://jblevins.org/projects/markdown-mode/rev-1-6
[Version 1.7]: https://jblevins.org/projects/markdown-mode/rev-1-7
[Version 1.8]: https://jblevins.org/projects/markdown-mode/rev-1-8
[Version 1.8.1]: https://jblevins.org/projects/markdown-mode/rev-1-8-1
[Version 1.9]: https://jblevins.org/projects/markdown-mode/rev-1-9
[Version 2.0]: https://jblevins.org/projects/markdown-mode/rev-2-0
[Version 2.1]: https://jblevins.org/projects/markdown-mode/rev-2-1
[Version 2.2]: https://jblevins.org/projects/markdown-mode/rev-2-2
[Version 2.3]: https://jblevins.org/projects/markdown-mode/rev-2-3
