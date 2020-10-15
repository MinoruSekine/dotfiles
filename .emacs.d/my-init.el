(defun my-install-missing-packages ()
  "Install missing packages."
  (defvar my-packages '(beacon
			color-identifiers-mode
			color-moccur
			elisp-lint
			flycheck
			google-c-style
			magit
			markdown-mode
			package-utils
			powershell
			rainbow-delimiters
			yaml-mode
			yasnippet
			yasnippet-snippets))

  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   '("melpa-stable" . "https://stable.melpa.org/packages/")
   )
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (require 'cl)  ;; To use remove-if-not.
  (defvar my-not-yet-installed-packages
    (remove-if-not (lambda (p) (not (package-installed-p p)))
		   my-packages))

  (when (and my-not-yet-installed-packages
	     (if noninteractive
		 (if (boundp 'my-default-install-missing-packages) my-default-install-missing-packages nil)
	       (y-or-n-p (concat "Install missing packages? : "
				 (format "%s" my-not-yet-installed-packages)))))
    (package-refresh-contents)
    (dolist (p my-not-yet-installed-packages)
      (package-install p))
    )
  )

(my-install-missing-packages)

(add-hook 'after-init-hook
	  (lambda ()
	    (my-environment-variable-setup)
	    (my-language-setup)
	    (my-general-visibility-setup)
	    (my-gui-setup)
	    (my-general-mode-line-setup)
	    (my-font-lock-setup)
	    (global-color-identifiers-mode t)
	    ))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (my-emacs-server-setup)
	    (my-backup-directory-setup)
	    (my-default-directory-to-home-setup)
	    (my-completion-case-sensitivity-setup)
	    (my-ede-and-semantic-mode-setup)
	    (my-c++-mode-setup)
	    (my-compilation-mode-setup)
	    (my-rainbow-delimiters-mode-setup)
	    (my-flycheck-mode-setup)
	    (my-yasnippet-setup)
	    (my-mocuur-setup)
	    ))

(defun my-environment-variable-setup ()
  "Set up environment variables."
  ;;; Pager in Emacs (eshell, terms, ...)
  (setenv "PAGER" "")
  )

(defun my-language-setup ()
  "Set up about language."
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  )

(defun my-general-visibility-setup ()
  "Setup visibility for both -nw and GUI."
  (show-paren-mode t)
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (beacon-mode t)
  (setq beacon-color "yellow")
  (setq beacon-blink-duration 0.1)
  )

(defun my-general-mode-line-setup ()
  "Set up modeline."
  (setq column-number-mode t)
  (setq line-number-mode t)
  (display-time-mode 1)
  (which-function-mode 1)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda()
	      (setq mode-name "Elisp")))
  )

(defun my-gui-setup ()
  "Setup if Emacs is running on GUI."
  (when (window-system)
    (tool-bar-mode 0)
    (load-theme 'tsdh-light t)
    (setq-default indicate-buffer-boundaries 'left)
    )
  )

(defun my-c++-mode-setup ()
  "Setup c++ mode."
  (add-to-list
   'magic-mode-alist
   '("\\(.\\|\n\\)*\n@\\(implementation\\|interface\\|protocol\\)"
     . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
  (font-lock-add-keywords 'c++-mode
			  '(("[ \t]+$" . 'trailing-whitespace)))
  (add-hook 'c++-mode-hook
	    '(lambda()
	       (setq indent-tabs-mode nil)
	       ))
  )

(defun my-emacs-server-setup ()
  "Setup Emacs server."
  (require 'server)
  (unless (server-running-p)
    (server-start))
  )

(defun my-ede-and-semantic-mode-setup ()
  "Setup ede and semantic mode."
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (global-ede-mode t)
  (semantic-mode 1)
  (when (file-directory-p "/usr/local/include/")
    (semantic-add-system-include "/usr/local/include/" 'c-mode)
    (semantic-add-system-include "/usr/local/include/" 'c++-mode))
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-summary-mode 1)
  )

(defun my-backup-directory-setup ()
  "Setup directories for backup files."
  (setq backup-directory-alist '((".*" . "~/.emacs-backup-files")))
  )

(defun my-default-directory-to-home-setup ()
  "Setup default directories."
  (setq default-directory "~/")
  (setq command-line-default-directory "~/")
  )

(defun my-completion-case-sensitivity-setup ()
  "Setup case sensitivity on completion."
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  )

(defun my-yasnippet-setup ()
  "Setup yasnippet mode."
  (yas-global-mode 1)
  )

(defun my-compilation-mode-setup ()
  "Setup compilation mode."
  (setq compilation-scroll-output t)
  (setq compile-command "time nice make -k -j ")
  )

(defun my-font-lock-setup ()
  "Setup font-lock."
  (setq jit-lock-defer-time 0.05)
  )

(defun my-rainbow-delimiters-mode-setup ()
  "Setup rainbow-delimiters-mode."
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (require 'cl-lib)
  (require 'color)
  (with-eval-after-load 'rainbow-delimiters
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face"
				 index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  )

(defun my-flycheck-mode-setup ()
  "Setup flycheck mode."
  (setq flycheck-cppcheck-standards '("c++11"))
  (setq flycheck-clang-language-standard "c++11")
  (setq flycheck-gcc-language-standard "c++11")
  (global-flycheck-mode t)
  )

(defun my-mocuur-setup ()
  "Setup moccur."
  (with-eval-after-load 'color-moccur
    (add-to-list 'dmoccur-exclusion-mask "\\.o$")
    (add-to-list 'dmoccur-exclusion-mask "\\.[a-z]?obj$")
    (add-to-list 'dmoccur-exclusion-mask "\\..?pch\\(\\.[a-z]+\\)?$")
    (add-to-list 'dmoccur-exclusion-mask "\\.p?db$")
    )
  )
