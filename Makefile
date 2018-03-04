#!/usr/bin/make -f

CASK ?= cask
EMACS ?= $(CASK) emacs -Q --batch

SRC := gitlab-ci-mode.el
OBJ := $(SRC:.el=.elc)
TESTS := $(SRC:.el=.test.stamp)

.INTERMEDIATE: $(TESTS)

all: $(OBJ) $(TESTS)

.cask/stamp: Cask $(SRC)
	$(CASK) install
	touch $@

clean:
	$(RM) $(OBJ)
	$(RM) $(TESTS)
	$(RM) -r .cask

test: $(TESTS)

%.elc: %.el .cask/stamp
	$(EMACS) -eval "(checkdoc-file \"$<\")"
	$(EMACS) -l tests/init.el -f package-lint-batch-and-exit $<
	$(EMACS) -L . -f batch-byte-compile $<

%.test.stamp: tests/%-test.el %.elc
	$(EMACS) -L . -l $< -f ert-run-tests-batch-and-exit

.PHONY: all clean test
