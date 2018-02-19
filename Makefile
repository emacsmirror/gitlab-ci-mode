#!/usr/bin/make -f

CASK ?= cask
EMACS ?= $(CASK) emacs -Q --batch

SRC := gitlab-ci-mode.el
OBJ := $(SRC:.el=.elc)
TESTS := $(SRC:.el=.test.stamp)

.INTERMEDIATE: $(TESTS)

all:
	$(CASK) install
	$(MAKE) $(OBJ) $(TESTS)

clean:
	$(RM) $(OBJ)
	$(RM) $(TESTS)
	$(RM) -r .cask

test: $(TESTS)

%.elc: %.el
	$(EMACS) -eval "(checkdoc-file \"$*.el\")"
	$(EMACS) -l test-init.el -f package-lint-batch-and-exit $*.el
	$(EMACS) -L . -f batch-byte-compile $<

%.test.stamp: %-test.el %.elc
	$(EMACS) -L . -l $*-test.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test
