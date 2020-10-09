all:

lint: elisp-lint-all

testrun: testrun-emacs

EMACS_CMD=emacs
elisp-lint-all: .emacs.d/my-init.el
	$(EMACS_CMD) -Q --batch --eval "(progn(package-initialize)(require 'elisp-lint)(elisp-lint-file \"$(realpath $+)\"))"

TESTRUN_EMACS_CMD=$(EMACS_CMD) -nw --batch --eval "(setq my-default-install-missing-packages t)" -l $(realpath $+) -f kill-emacs
testrun-emacs: .emacs.d/my-init.el
	$(TESTRUN_EMACS_CMD)
