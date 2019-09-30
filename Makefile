$(HOME)/.emacs.d/my-init.el: .emacs.d/my-init.el
	mkdir -p $(HOME)/.emacs.d
	ln -s $(abspath $<) $@
