EMACS=emacs
EASK=eask

PACKAGE=markdown-mode

SOURCE=markdown-mode.el
COMPILED=markdown-mode.elc

VERSION=$(shell cat $(SOURCE) | sed -n 's/^;; Version: \(.*\)/\1/p')

TEST_FILES=tests/Makefile tests/*.el tests/*.text tests/*.md

.el.elc:
	$(EMACS) -q -no-site-file -no-init-file -batch -f batch-byte-compile $<

all: $(COMPILED)

.PHONY: dist test

build:
	$(EASK) package
	$(EASK) install

test-unix:
	$(EASK) compile
	$(EASK) test ert ./tests/markdown-test.el
	$(EASK) lint checkdoc

test-windows:
	$(EASK) compile
	#$(EASK) test ert ./tests/markdown-test.el
	$(EASK) lint checkdoc

clean:
	$(EASK) clean all

dist:
	$(EASK) package

update: $(COMPILED)
	cp -a $(SOURCE) $(COMPILED) $(HOME)/.emacs.d/site-lisp
