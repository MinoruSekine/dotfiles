;; Setup packages if not installed yet.
(defvar my-packages '(elisp-lint
		      flycheck
		      magit
		      yaml-mode
		      yasnippet
		      yasnippet-snippets))

(require 'package)
(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'after-init-hook
	  (lambda ()
	    (my-gui-setup)
	    (my-semantic-mode-setup)
	    (my-c++-mode-setup)
	    ))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (my-emacs-server-setup)
	    (my-backup-directory-setup)
	    (my-default-directory-to-home-setup)
	    (my-completion-case-sensitivity-setup)
	    (my-yasnippet-setup)
	    ))

(defun my-gui-setup ()
  "Setup if runs on GUI."
  (when (window-system)
    (tool-bar-mode 0)
    )
  )

(defun my-c++-mode-setup ()
  "Setup c++ mode."
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )

(defun my-emacs-server-setup ()
  "Setup Emacs server."
  (require 'server)
  (unless (server-running-p)
    (server-start))
  )

(defun my-semantic-mode-setup ()
  "Setup semantic mode."
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
