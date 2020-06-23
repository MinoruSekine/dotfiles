all:

lint: elisp-lint-all

testrun: testrun-emacs

EMACS_CMD=emacs
ELISP_LINT_PATH=$(shell find $(HOME)/.emacs.d -name elisp-lint.el)
ELISP_LINT_OPTION=--no-byte-compile --no-checkdoc --no-package-format
ELISP_LINT_CMD=$(EMACS_CMD) -Q --batch -l $(ELISP_LINT_PATH) -f elisp-lint-files-batch $(ELISP_LINT_OPTION)
elisp-lint-all: .emacs.d/my-init.el
	$(ELISP_LINT_CMD) $(realpath $+)

TESTRUN_EMACS_CMD=$(EMACS_CMD) -nw --batch -l $(realpath $+) -f kill-emacs
testrun-emacs: .emacs.d/my-init.el
	$(TESTRUN_EMACS_CMD)
