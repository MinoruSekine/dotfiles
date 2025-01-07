all:

lint: elisp-lint-all sh-lint-all

testrun: testrun-emacs

test: test-elisp

EMACS_CMD=emacs

# Rules for unit tests of utility functions in my-init.el.
TARGET_ELISPS=$(wildcard .emacs.d/test/*.el)
TESTS_TARGET_ELISP=$(addsuffix .test-elisp, $(TARGET_ELISPS))
test-elisp: $(TESTS_TARGET_ELISP)

%.test-elisp:
	$(EMACS_CMD) -Q --batch -l $* --eval '(setq debug-on-error t)' --eval '(ert-run-tests-batch-and-exit (quote t))'

# Rules for Emacs Lisp linter.
elisp-lint-all: .emacs.d/my-init.el
	.emacs.d/elisp-lint.sh "$(realpath $+)"

TESTRUN_EMACS_CMD=$(EMACS_CMD) -nw --batch --eval '(setq debug-on-error t)' --eval "(setq my-default-install-missing-packages t)" -l $(realpath $+) -f kill-emacs
testrun-emacs: .emacs.d/my-init.el
	$(TESTRUN_EMACS_CMD)

# Rules for shell script linter.
DIRS_WHICH_HAVE_SH_FILES := .emacs.d .profile
SH_FILES := $(foreach dir, $(DIRS_WHICH_HAVE_SH_FILES), $(wildcard $(dir)/*.sh))
SH_CHECK := $(addsuffix .shellcheck, $(SH_FILES))

sh-lint-all: $(SH_CHECK)

%.shellcheck:
	shellcheck --shell=dash $*
