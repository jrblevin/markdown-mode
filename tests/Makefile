EMACS=emacs
MARKDOWN=Markdown.pl

.SUFFIXES: .text .html
.PHONY: test

TEST_SRCS = $(wildcard *.text)
TEST_HTML = $(TEST_SRCS:.text=.html)

.text.html:
	$(MARKDOWN) < $< > $@

test: compile-all
	$(EMACS) -Q --batch $(CL) \
	    -l ert -l ../markdown-mode.elc -l markdown-test.elc \
	    -f ert-run-tests-batch-and-exit

compile-all:
	$(EMACS) -Q --batch $(CL) \
	    --eval '(setq byte-compile-error-on-warn t)' \
	    -l ../markdown-mode.el \
	    -f batch-byte-compile ../markdown-mode.el markdown-test.el

html: $(TEST_HTML)

clean:
	rm -f $(TEST_HTML) *.elc ../*.elc

# add cl compatibility for emacs 24.1,2
CL_ARG := -l cl-lib-0.5.el \
	--eval '(setq byte-compile-warnings (quote (not cl-functions)))'
evm:
	$(MAKE) test EMACS="$(shell evm bin emacs-24.1)" CL="$(CL_ARG)"
	$(MAKE) test EMACS="$(shell evm bin emacs-24.2)" CL="$(CL_ARG)"
	$(MAKE) test EMACS="$(shell evm bin emacs-24.3)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-24.4)" CL=""
	$(MAKE) test EMACS="$(shell evm bin emacs-24.5)" CL=""
