all:

lint: elisp-lint-all sh-lint-all

testrun: testrun-emacs

# Rules for Emacs.
EMACS_CMD=emacs
elisp-lint-all: .emacs.d/my-init.el
	$(EMACS_CMD) -Q --batch --eval "(progn(package-initialize)(require 'elisp-lint)(elisp-lint-file \"$(realpath $+)\"))"

TESTRUN_EMACS_CMD=$(EMACS_CMD) -nw --batch --eval "(setq my-default-install-missing-packages t)" -l $(realpath $+) -f kill-emacs
testrun-emacs: .emacs.d/my-init.el
	$(TESTRUN_EMACS_CMD)

# Rules for shell script.
DIRS_WHICH_HAVE_SH_FILES := .profile
SH_FILES := $(foreach dir, $(DIRS_WHICH_HAVE_SH_FILES), $(wildcard $(dir)/*.sh))
SH_CHECK := $(addsuffix .shellcheck, $(SH_FILES))

sh-lint-all: $(SH_CHECK)

%.shellcheck:
	shellcheck --shell=dash $*
