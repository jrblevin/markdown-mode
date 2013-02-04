EMACS=emacs

.PHONY: test

test:
	$(EMACS) -batch -l ert -l ../markdown-mode.el -l markdown-test.el \
	    -f ert-run-tests-batch-and-exit
