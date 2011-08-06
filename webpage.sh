#!/bin/sh

STARTRE='^;;; Commentary:$'
STOPRE='^$'
DATE=$(date +"%B %e, %Y %H:%M %Z")

echo "title: Emacs Markdown Mode
description: A major mode for GNU Emacs for editing Markdown-formatted text files.
markup: markdown
icon: emacs
city: Columbus
created: May 24, 2007 23:47 GMT
modified: $DATE" > index.text

cat markdown-mode.el |\
    # Keep only the Commentary section
    awk "/$STARTRE/,/$STOPRE/" |\
    # Remove the start and step regexps
    grep -v "$STARTRE" | grep -v "$STOPRE" |\
    # Convert headers
    sed -e 's/^;;; \(.*\):$/\n## \1/' |\
    # Remove leading spaces (but don't disturb pre blocks)
    sed -e 's/^;;[ ]\{0,1\}//' |\
    # Escape wiki links
    #sed -e 's/\(\[\[[^]\n]*\]\]\)/\\\1/g' |\
    # Use Markdown-style backticks for single-quoted lisp code
    sed -e 's/`\([^'\'']*\)'\''/`\1`/g' |\
    # Use <kbd> tags for keybindings
    sed -e 's/`\([CM]-[^`]*\)`/<kbd>\1<\/kbd>/g' |\
    # Remove email addresses
    sed -e 's/ <.*@.*> / /g' \
    >> index.text
