#!/bin/sh

DATE=$(date +"%B %e, %Y %H:%M %Z")

echo "title: Markdown Mode for Emacs
description: A major mode for GNU Emacs for editing Markdown-formatted text files.
markup: markdown
city: Columbus
created: May 24, 2007 23:47 GMT
modified: $DATE
style: #badges { margin-bottom: 1.5rem; }

<div id=\"badges\">
<a href=\"https://github.com/jrblevin/markdown-mode\"><img src=\"https://img.shields.io/github/stars/jrblevin/markdown-mode.svg?style=social&label=GitHub\" alt=\"GitHub\"/></a>
<a href=\"https://elpa.nongnu.org/nongnu/markdown-mode.html\"><img src=\"https://elpa.nongnu.org/nongnu/markdown-mode.svg\" alt=\"NonGNU ELPA badge\"/></a>
<a href=\"https://melpa.org/#/markdown-mode\"><img src=\"https://melpa.org/packages/markdown-mode-badge.svg\" alt=\"MELPA badge\"/></a>
<a href=\"https://stable.melpa.org/#/markdown-mode\"><img src=\"https://stable.melpa.org/packages/markdown-mode-badge.svg\" alt=\"MELPA stable badge\"/></a>
<a href=\"https://travis-ci.org/jrblevin/markdown-mode\"><img src=\"https://travis-ci.org/jrblevin/markdown-mode.svg?branch=master\" alt=\"Travis CI Build Status\"/></a>
<a href=\"https://leanpub.com/markdown-mode\"><img src=\"https://img.shields.io/badge/leanpub-guide-orange.svg\" alt=\"Guide to Markdown Mode for Emacs\"/></a>
</div>" > index.text

cat README.md >> index.text
