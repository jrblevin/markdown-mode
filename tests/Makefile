EMACS=emacs
MARKDOWN=Markdown.pl

.SUFFIXES: .text .html
.PHONY: test

TEST_SRCS = $(wildcard *.text)
TEST_HTML = $(TEST_SRCS:.text=.html)

.text.html:
	$(MARKDOWN) < $< > $@

test:
	$(EMACS) -Q --batch \
	    --eval '(setq byte-compile-error-on-warn t)' \
	    -l ../markdown-mode.el \
	    -f batch-byte-compile ../markdown-mode.el markdown-test.el
	$(EMACS) -Q --batch \
	    -l ert -l ../markdown-mode.elc -l markdown-test.elc \
	    -f ert-run-tests-batch-and-exit

html: $(TEST_HTML)

clean:
	rm -f $(TEST_HTML) *.elc ../*.elc
