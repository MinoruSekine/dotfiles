all:

lint: elisp-lint-all

EMACS_CMD=emacs
ELISP_LINT_PATH=$(shell find $(HOME)/.emacs.d -name elisp-lint.el)
PACKAGE_LINT_PATH=$(shell find $(HOME)/.emacs.d -name package-lint.el)
ELISP_LINT_OPTION=--no-byte-compile --no-checkdoc --no-package-format
ELISP_LINT_CMD=$(EMACS_CMD) --batch -l $(ELISP_LINT_PATH) -l $(PACKAGE_LINT_PATH) -f elisp-lint-files-batch $(ELISP_LINT_OPTION)
elisp-lint-all: .emacs.d/my-init.el
	$(ELISP_LINT_CMD) $(realpath $+)
