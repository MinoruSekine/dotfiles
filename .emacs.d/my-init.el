;; Setup packages if not installed yet.
(defvar my-packages '(color-identifiers-mode
		      color-moccur
		      elisp-lint
		      flycheck
		      google-c-style
		      magit
		      markdown-mode
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

(when (and my-not-yet-installed-packages)
  (package-refresh-contents))

(dolist (p my-not-yet-installed-packages)
  (package-install p))

(add-hook 'after-init-hook
	  (lambda ()
	    (my-general-visibility-setup)
	    (my-gui-setup)
	    (my-general-mode-line-setup)
	    (my-semantic-mode-setup)
	    (my-c++-mode-setup)
	    (my-compilation-mode-setup)
	    (my-font-lock-set-up)
	    (my-rainbow-delimiters-mode-setup)
	    (my-flycheck-mode-setup)
	    (global-color-identifiers-mode t)
	    ))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (my-emacs-server-setup)
	    (my-backup-directory-setup)
	    (my-default-directory-to-home-setup)
	    (my-completion-case-sensitivity-setup)
	    (my-yasnippet-setup)
	    (my-mocuur-setup)
	    ))

(defun my-general-visibility-setup ()
  (show-paren-mode t)
  )

(defun my-general-mode-line-setup ()
  (setq column-number-mode t)
  (setq line-number-mode t)
  (display-time-mode 1)
  (which-function-mode 1)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda()
	      (setq mode-name "Elisp")))
  )

(defun my-gui-setup ()
  "Setup if runs on GUI."
  (when (window-system)
    (tool-bar-mode 0)
    (load-theme 'tsdh-light t)
    (setq-default indicate-buffer-boundaries 'left)
    )
  )

(defun my-c++-mode-setup ()
  "Setup c++ mode."
  (add-to-list 'magic-mode-alist
	       '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
  (add-to-list 'magic-mode-alist
	       '("\\(.\\|\n\\)*\n@interface" . objc-mode))
  (add-to-list 'magic-mode-alist
	       '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
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

(defun my-semantic-mode-setup ()
  "Setup semantic mode."
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (semantic-mode 1)
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
  (setq default-directry "~/")
  (setq command-line-default-directory "~/")
  )

(defun my-completion-case-sensitivity-setup ()
  "Setup case sensitivity on completion."
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  )

(defun my-yasnippet-setup ()
  "Setup yasnippet mode"
  (yas-global-mode 1)
  )

(defun my-compilation-mode-setup ()
  "Setup compilation mode"
  (setq compilation-scroll-output t)
  )

(defun my-font-lock-set-up ()
  "Setup font-lock"
  (setq jit-lock-defer-time 0.05)
  )

(defun my-rainbow-delimiters-mode-setup ()
  "Setup rainbow-delimiters-mode"
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
  (setq flycheck-cppcheck-standards '("c++11"))
  (setq flycheck-clang-language-standard "c++11")
  (setq flycheck-gcc-language-standard "c++11")
  (global-flycheck-mode t)
  )

(defun my-mocuur-setup ()
  (with-eval-after-load 'color-moccur
    (add-to-list 'dmoccur-exclusion-mask "\\.o$")
    (add-to-list 'dmoccur-exclusion-mask "\\.[a-z]?obj$")
    (add-to-list 'dmoccur-exclusion-mask "\\..?pch\\(\\.[a-z]+\\)?$")
    (add-to-list 'dmoccur-exclusion-mask "\\.p?db$")
    )
  )
