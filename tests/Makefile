EMACS=emacs
MARKDOWN=Markdown.pl

.SUFFIXES: .text .html
.PHONY: test

TEST_SRCS = $(wildcard *.text)
TEST_HTML = $(TEST_SRCS:.text=.html)

.text.html:
	$(MARKDOWN) < $< > $@

test:
	$(EMACS) -batch -l ert -l ../markdown-mode.el -l markdown-test.el \
	    -f ert-run-tests-batch-and-exit

html: $(TEST_HTML)

clean:
	rm -f $(TEST_HTML)
