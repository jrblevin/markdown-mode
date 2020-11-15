EMACS=emacs
MARKDOWN=Markdown.pl

.SUFFIXES: .text .html
.PHONY: test

TEST_SRCS = $(wildcard *.text)
TEST_HTML = $(TEST_SRCS:.text=.html)

.text.html:
	$(MARKDOWN) < $< > $@

test: compile-all checkdoc
	$(EMACS) -Q --batch \
	    -l ert -l ../markdown-mode.elc -l markdown-test.elc \
	    --eval '(ert-run-tests-batch-and-exit $(SELECTOR))'

checkdoc:
	$(EMACS) -Q --batch -l checkdoc-batch.el 2>&1 | \
	    grep -E "markdown-mode.el:[1-9]+" && exit 1 || exit 0

compile-all:
	$(EMACS) -Q --batch \
	    --eval '(setq byte-compile-error-on-warn t)' \
	    -l ../markdown-mode.el \
	    -f batch-byte-compile ../markdown-mode.el markdown-test.el

html: $(TEST_HTML)

clean:
	rm -f $(TEST_HTML) *.elc ../*.elc

evm:
	$(MAKE) test EMACS="$(shell evm bin emacs-25.1)"
	$(MAKE) test EMACS="$(shell evm bin emacs-25.2)"
	$(MAKE) test EMACS="$(shell evm bin emacs-25.3)"
	$(MAKE) test EMACS="$(shell evm bin emacs-26.1)"
	$(MAKE) test EMACS="$(shell evm bin emacs-26.2)"
	$(MAKE) test EMACS="$(shell evm bin emacs-26.3)"
