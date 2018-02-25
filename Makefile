#!/usr/bin/make -f

CASK ?= cask
EMACS ?= $(CASK) emacs -Q --batch

SRC := gitlab-ci-mode.el gitlab-ci-mode-flycheck.el gitlab-ci-mode-lint.el
OBJ := $(SRC:.el=.elc)
TESTS := gitlab-ci-mode.test.stamp

.INTERMEDIATE: $(TESTS)

all:
	$(CASK) install
	$(EMACS) -l tests/init.el -f package-lint-batch-and-exit gitlab-ci-mode.el
	$(MAKE) $(OBJ) $(TESTS)

clean:
	$(RM) $(OBJ)
	$(RM) $(TESTS)
	$(RM) -r .cask

test: $(TESTS)

%.elc: %.el
	$(EMACS) -eval "(checkdoc-file \"$*.el\")"
	$(EMACS) -L . -f batch-byte-compile $<

%.test.stamp: tests/%-test.el %.elc
	$(EMACS) -L . -l tests/$*-test.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test
