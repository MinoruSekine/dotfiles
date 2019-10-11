;; Setup packages if not installed yet.
(defvar my-packages '(flycheck magit elisp-lint yaml-mode))

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

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (my-emacs-server-setup)
	    (my-semantic-mode-setup)
	    (my-backup-directory-setup)
	    (my-default-directory-to-home-setup)
	    ))

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
