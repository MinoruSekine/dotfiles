;; Make backup files not into original file dir,
;; but into specified single dir.
(setq backup-directory-alist '((".*" . "~/.emacs-backup-files")))

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

;; Semantic mode
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-summary-mode 1)

;; Start server for emacsclient, if not running yet.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Default directory
(setq default-directry "~/")
(setq command-line-default-directory "~/")
