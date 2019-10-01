;; Make backup files not into original file dir,
;; but into specified single dir.
(setq backup-directory-alist '((".*" . "~/.emacs-backup-files")))

;; Setup packages if not installed yet.
(defvar my-packages '(flycheck magit elisp-lint))

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

;; Start server for emacsclient, if not running yet.
(require 'server)
(unless (server-running-p)
  (server-start))
