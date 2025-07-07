;;; my-init.el --- My Emacs initialization codes. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file includes elisp codes to initialize Emacs settings.
;;   - Install necessary elisp packages if missing
;;   - Configurations
;;     - Fonts
;;     - Visibility
;;     - Key bindings
;;     - ...
;; It will be loaded from ~/.emacs.d/init.el.

;;; Code:

;; Internal helper functions.
(defun my-get-default-plantuml-jar-path ()
  "Get path to plantuml.jar on running environment."
  (let* ((my-plantuml-jar-path
          (cond ((equal system-type 'darwin)
                 (let* ((my-plantuml-executable-installed-by-homebrew-path
                         (executable-find "plantuml")))
                   (when my-plantuml-executable-installed-by-homebrew-path
                     (let*
                         ((my-plantuml-jar-wildcard
                           (string-replace
                            "/bin/plantuml"
                            "/Cellar/plantuml/*/libexec/plantuml.jar"
                            my-plantuml-executable-installed-by-homebrew-path)))
                       (substring (shell-command-to-string
                                   (combine-and-quote-strings
                                    (list
                                     "echo"
                                     my-plantuml-jar-wildcard)
                                    " "))
                                  0
                                  -1)))))
                ((equal system-type 'windows-nt)
                 (expand-file-name (replace-regexp-in-string
                                    "plantuml.\\(cmd\\|jar\\)\n"
                                    "plantuml.jar"
                                    (shell-command-to-string
                                     (mapconcat
                                      #'shell-quote-argument
                                      (list "scoop" "which" "plantuml")
                                      " ")))))
                ((equal system-type 'gnu/linux)
                 "/usr/share/plantuml/plantuml.jar")
                )))
    (if (and my-plantuml-jar-path
             (file-readable-p my-plantuml-jar-path))
        my-plantuml-jar-path
      nil)))

(defun my-add-hooks (hook-to-add my-list)
  "Add each function in MY-LIST into HOOK-TO-ADD ."
  (dolist (itr my-list)
    (add-hook hook-to-add itr)))

(defun my-network-connection-available-p ()
  "Get network connection is available or not."
  ;; The local loop back device may always be included
  ;; in return of network-interface-list.
  (>= (length (network-interface-list)) 2))

(defun my-join-path (dir file-name)
  "Join FILE-NAME string into DIR with right path delimiter."
  (concat (file-name-as-directory dir) file-name))

(defsubst my-get-file-size (file-path)
  "Get size of FILE-PATH."
  (nth 7 (file-attributes file-path)))

(defun my-get-elc-path (el-path)
  "Get .elc path from .el path specified by EL-PATH."
  (concat (file-name-sans-extension el-path) ".elc"))

(defsubst my-emacs-wiki-elisp-file-name (elisp-name)
  "Filename of ELISP-NAME installed from Emacs Wiki."
  (concat elisp-name ".el"))

(defsubst my-emacs-wiki-elisp-dir (elisp-name)
  "Directory for ELISP-NAME installed from Emacs Wiki."
  (defconst my-emacs-wiki-elisp-dir-root
    (expand-file-name "~/.emacs.d/emacs-wiki")
    "Path to install elisps from Emacs Wiki.")
  (my-join-path my-emacs-wiki-elisp-dir-root elisp-name))

(defsubst my-emacs-wiki-elisp-path (elisp-name)
  "Path for ELISP-NAME installed from Emacs Wiki."
  (my-join-path (my-emacs-wiki-elisp-dir elisp-name)
                (my-emacs-wiki-elisp-file-name elisp-name)))

(defsubst my-emacs-wiki-elisp-installed-p (elisp-name)
  "Get ELISP-NAME form Emacs Wiki installed or not."
  (defconst elisp-path (my-emacs-wiki-elisp-path elisp-name))
  (and (file-exists-p elisp-path) (> (my-get-file-size elisp-path) 0)))

(defun my-setup-elisp-from-emacs-wiki ()
  "Install missing elisp from Emacs Wiki and set `load-path`."
  (defsubst my-emacs-wiki-elisp-url (elisp-name)
    "URL for ELISP-NAME installed from Emacs Wiki."
    (concat "https://www.emacswiki.org/emacs/download/"
            (my-emacs-wiki-elisp-file-name elisp-name)))
  (defconst my-elisp-from-emacs-wiki '("tempbuf"))
  (defcustom my-init-emacs-wiki-download-error-level
    (if (getenv "GITHUB_WORKFLOW")
        :warning
      :error)
    "If non-nil, ignore error while downloading from Emacs Wiki.
On GitHub Actions workflow, t is default.
On other environments nil is default.
Please see also https://github.com/MinoruSekine/dotfiles/issues/200 ."
    :type 'boolean
    :group 'my-init)
  (dolist (p my-elisp-from-emacs-wiki)
    (defconst this-elisp-dir (my-emacs-wiki-elisp-dir p))
    (unless (my-emacs-wiki-elisp-installed-p p)
      (defconst this-elisp-file (my-emacs-wiki-elisp-path p))
      (unless (file-directory-p this-elisp-dir)
        (make-directory this-elisp-dir t))
      (if (= (my-curl-copy-file
              (my-emacs-wiki-elisp-url p) this-elisp-file t 4 8)
             0)
          (byte-compile-file this-elisp-file)
        (lwarn 'my-init
               my-init-emacs-wiki-download-error-level
               "Downloading %s by `curl` is failed" p)))
    (add-to-list 'load-path this-elisp-dir)))

;; Functions for my-init.el.
(defun my-install-missing-packages ()
  "Install missing packages."
  (defvar my-packages '(color-identifiers-mode
                        color-moccur
                        elisp-lint
                        flycheck
                        google-c-style
                        highlight-doxygen
                        magit
                        markdown-mode
                        minions
                        package-utils
                        plantuml-mode
                        powershell
                        rainbow-delimiters
                        realgud
                        yaml-mode))
  (when (executable-find "lldb")
    (add-to-list 'my-packages 'realgud-lldb))
  (when (executable-find "wakatime-cli")
    (add-to-list 'my-packages 'wakatime-mode))
  (when (equal system-type 'windows-nt)
    (add-to-list 'my-packages 'ssh-agency))

  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   '("melpa-stable" . "https://stable.melpa.org/packages/")
   )
  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (unless (package-installed-p 'gnu-elpa-keyring-update)
    (let (prev-package-check-signature package-check-signature)
      (setq package-check-signature nil)
      (package-refresh-contents)
      (package-install 'gnu-elpa-keyring-update)
      (setq package-check-signature prev-package-check-signature)))

  (defconst my-not-yet-installed-packages
    (cl-remove-if (lambda (p) (package-installed-p p))
                  my-packages))

  (when (and my-not-yet-installed-packages
             (if (or noninteractive (and (fboundp 'daemonp) (daemonp)))
                 (if (boundp 'my-default-install-missing-packages)
                     my-default-install-missing-packages nil)
               (y-or-n-p (concat "Install missing packages? : "
                                 (format "%s"
                                         my-not-yet-installed-packages)))))
    (package-refresh-contents)
    (dolist (p my-not-yet-installed-packages)
      (package-install p))
    )
  )

(defun my-curl-copy-file (url
                          newname
                          ok-if-already-exists
                          retry-times
                          retry-interval-sec)
  "Download URL to NEWNAME.
This function use curl if available,
and fallback to use my-url-copy-file if curl is unavailable on system.
OK-IF-ALREADY-EXISTS, RETRY-TIMES, and RETRY-INTERVAL-SEC is only used
when fallback to my-url-copy-file,
and they will be ignored if using curl."
  ;; url-copy-file from EmacsWiki sometime randomly fails
  ;; on GitHub Actions runner.
  ;; But the reason has not been clarified. So added retry.
  ;; See https://github.com/MinoruSekine/dotfiles/issues/200 for details.
  (defsubst my-url-copy-file (url
                              newname
                              ok-if-already-exists
                              retry-times
                              retry-interval-sec)
    "URL-COPY-FILE wrapper to download URL to NEWNAME.
Overwrite existing NEWNAME file when OK-IF-ALREADY-EXISTS is non-nil.
If error occured in url-copyfile,
retry RETRY-TIMES times with RETRY-INTERVAL-SEC sec interval."
    (let* ((remaining-retry-count retry-times)
           (is-url-copy-file-succeeded nil))
      (while (and (> remaining-retry-count 0)
                  (not is-url-copy-file-succeeded))
        (defconst is-url-copy-file-succeeded
          (ignore-errors (url-copy-file url newname ok-if-already-exists)))
        (setq remaining-retry-count (1- remaining-retry-count))
        (when (not is-url-copy-file-succeeded)
          (if (> remaining-retry-count 0)
              (progn (display-warning
                      'my-init
                      (format
                       "Downloading from %s failed. Retrying %s more time(s)"
                       url remaining-retry-count))
                     (sit-for retry-interval-sec))
            (error "url-copy-file for %s failed even with %s times retry"
                   url retry-times))))))
  (defconst my-curl-path (executable-find "curl"))
  (if my-curl-path
      (progn (unless (file-exists-p newname)
               (message "curl found. Download %s by curl." url)
               (call-process-shell-command
                (mapconcat
                 #'shell-quote-argument
                 (list "curl" "-f" "-s" "-o" newname url)
                 " "))))
    (message "curl not found. Fall back to url-copy-file to download %s." url)
    (my-url-copy-file
     url newname ok-if-already-exists retry-times retry-interval-sec)))

;;; Functions for auto upgrade packages.
(defconst my-upgrade-interval-days 7)
(defconst my-default-last-upgrade-time
  (time-subtract (current-time)
                 (* 60 60 24 my-upgrade-interval-days)))
(define-multisession-variable my-last-upgrade-time my-default-last-upgrade-time)

(defun my-auto-upgrade-packages ()
  "Auto upgrade packages.
This function works if interval expired, interactive, and network available."
  (defsubst my-auto-upgrade-packages-interval-expired-p ()
    "Return t if it is necessary to upgrade packages."
    (defconst last-upgrade-time (multisession-value my-last-upgrade-time))
    (defconst days-from-last-upgrade
      (/ (time-convert (time-subtract (current-time) last-upgrade-time)
                       'integer)
         (* 60 60 24)))
    (> days-from-last-upgrade my-upgrade-interval-days))
  (when (and (not noninteractive)
             (my-auto-upgrade-packages-interval-expired-p)
             (my-network-connection-available-p)
             (y-or-n-p "Upgrade packages now?"))
    (package-initialize)
    (package-refresh-contents)
    (package-upgrade-all)
    (setf (multisession-value my-last-upgrade-time)
          (current-time))))

;;; Main processes.
(if (my-network-connection-available-p)
    (progn (my-install-missing-packages)
           (my-setup-elisp-from-emacs-wiki))
  (display-warning 'my-init "Network connection may not be available."))

(defconst my-after-init-func-list '(my-gc-setup
                                    my-environment-variable-setup
                                    my-language-setup
                                    my-general-visibility-setup
                                    my-gui-setup
                                    my-general-mode-line-setup
                                    my-font-lock-setup
                                    my-color-identifiers-mode-setup
                                    my-adjust-font-size-setup
                                    ))
(my-add-hooks 'after-init-hook my-after-init-func-list)

(defconst my-init-el-path load-file-name)
(defconst my-emacs-startup-func-list
  '(my-emacs-server-setup
    my-backup-directory-setup
    my-default-directory-to-home-setup
    my-completion-case-sensitivity-setup
    my-indent-tabs-mode-for-file-without-hard-tab
    my-ede-and-semantic-mode-setup
    my-editorconfig-mode-setup
    my-c++-mode-setup
    my-compilation-mode-setup
    my-rainbow-delimiters-mode-setup
    my-flycheck-mode-setup
    my-mocuur-setup
    my-plantuml-mode-setup
    my-dired-setup
    my-eshell-setup
    my-emacs-lisp-mode-setup
    my-tempbuf-mode-setup
    my-realgud-setup
    my-ssh-agency-setup
    my-global-set-key-toggle-input-method
    my-backslash-key-setup
    my-javascript-setup
    my-typescript-setup
    my-wakatime-setup
    my-magit-setup
    my-whitespace-mode-setup
    my-init-el-byte-compile))
(my-add-hooks 'emacs-startup-hook my-emacs-startup-func-list)

(defconst my-kill-emacs-func-list
  '(my-auto-upgrade-packages
    my-init-el-byte-compile))
(my-add-hooks 'kill-emacs-hook my-kill-emacs-func-list)

;;; Functions for initializing Emacs.
(defun my-gc-setup ()
  "Settings for garbage collection."
  (custom-set-variables
   '(gc-cons-threshold (* 256 1024 1024))
   '(garbage-collection-messages t))
  (run-with-idle-timer 120 nil #'garbage-collect))

(defun my-environment-variable-setup ()
  "Set up environment variables."
  ;;; Pager in Emacs (eshell, terms, ...)
  (setenv "PAGER" "")

  (custom-set-variables
   '(exec-path (append exec-path
                       (parse-colon-path (getenv "PATH")))))
  )

(defun my-language-setup ()
  "Set up about language."
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (cond ((equal system-type 'windows-nt)
         (setq default-process-coding-system '(undecided-dos . utf-8-unix)))
        (t
         (setq default-process-coding-system '(undecided . utf-8-unix)))))

(defun my-general-visibility-setup ()
  "Setup visibility for both -nw and GUI."
  (show-paren-mode t)
  (custom-set-variables
   '(show-paren-style 'mixed)
   '(show-paren-when-point-inside-paren t)
   '(show-paren-when-point-in-periphery t))
  (highlight-doxygen-global-mode 1)
  (use-package beacon
    :ensure t
    :custom
    (beacon-color "yellow")
    (beacon-blink-duration 0.1)
    (blink-cursor-blinks 0)  ;; 0 means "blink ever".
    :config
    (beacon-mode t))
  )

(defun my-general-mode-line-setup ()
  "Set up modeline."
  (custom-set-variables
   '(column-number-mode t)
   '(line-number-mode t))
  (display-time-mode 1)
  (which-function-mode 1)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq mode-name "Elisp")))
  (use-package minions
    :ensure t
    :config
    (minions-mode)))

(defun my-after-make-frame-func (frame)
  "Called just after making each frame with FRAME as made frame."
  (with-selected-frame frame
    (cond ((display-graphic-p)
           (tool-bar-mode -1)
           (when (find-font (font-spec :name "VL ゴシック"))
             (set-frame-font "VL ゴシック-10")))
          (t
           (menu-bar-mode -1))))
  (blink-cursor-mode t))


;; Functions to adjust font size for display.
(defun my-get-display-pixel-width ()
  "Get width of display in pixel."
  (nth 3
       (cl-loop for itr in (display-monitor-attributes-list)
                when (> (length (assoc 'frames itr)) 1)
                return (assoc 'workarea itr))))

(defun my-get-display-mm-width ()
  "Get width of display in mm."
  (nth 1
       (cl-loop for itr in (display-monitor-attributes-list)
                when (> (length (assoc 'frames itr)) 1)
                return (assoc 'mm-size itr))))

(defun my-get-display-inch-width ()
  "Get width of display in inch."
  (/ (my-get-display-mm-width) 25.4))

(defun my-get-display-dpi ()
  "Get DPI of display."
  (/ (my-get-display-pixel-width) (my-get-display-inch-width)))

(defun my-enabled-adjust-font-size-setup-p ()
  "Indicated necessary to adjust font size."
  ;; NTEmacs seems to natively support high DPI awareness.
  (and (not (equal system-type 'windows-nt))
       (display-graphic-p)
       (display-supports-face-attributes-p :height)))

(defun my-get-font-zoom-ratio-for-display ()
  "Get font zoom ratio for display."
  (if (my-enabled-adjust-font-size-setup-p)
      (max (/ (my-get-display-dpi) 72) 1)
    1))

(defun my-adjust-font-size (&optional frame)
  "Adjust font size for current display which has FRAME."
  (defconst my-default-face-height 100 "Default face height.")
  (defconst my-adjusted-face-height (truncate
                                     (* my-default-face-height
                                        (my-get-font-zoom-ratio-for-display))))
  (set-face-attribute 'default frame :height my-adjusted-face-height))

(defun my-get-relative-frame-size-zoom-ratio ()
  "Get zoom ratio for frame size relative to display work area size."
  (/ 1.0 (my-get-font-zoom-ratio-for-display)))

(defun my-gui-setup ()
  "Setup if Emacs is running on GUI."
  (setq-default indicate-buffer-boundaries 'left)
  (add-to-list 'default-frame-alist
               '(foreground-color . "black"))
  (add-to-list 'default-frame-alist
               '(background-color . "ghost white"))
  (add-to-list 'default-frame-alist
               '(cursor-color . "forest green"))
  (when (find-font (font-spec :name "VL ゴシック"))
    (add-to-list 'default-frame-alist '(font . "VL ゴシック-10"))
    )
  (add-to-list 'default-frame-alist
               `(width . ,(* 0.3 (my-get-relative-frame-size-zoom-ratio))))
  (add-to-list 'default-frame-alist
               `(height . ,(* 0.8 (my-get-relative-frame-size-zoom-ratio))))
  (add-to-list 'default-frame-alist
               `(top . 0.1))
  (add-to-list 'default-frame-alist
               `(left . 0.2 ))
  ;; Defer following settings if not GUI yet.
  (add-hook 'after-make-frame-functions
            'my-after-make-frame-func)
  (my-after-make-frame-func (selected-frame))
  )

(defun my-switch-mode-for-header-file ()
  "Switch to detailed mode for .h files."
  (save-excursion
    (goto-char (point-min))
    (let ((objc-found-p
           (re-search-forward
            "\\_<@\\(interface\\|implementation\\)"
            magic-mode-regexp-match-limit t))
          (c++-found-p
           (re-search-forward
            "\\_<\\(namespace\\|template\\|class\\|std::\\)"
            magic-mode-regexp-match-limit t)))
      (cond
       ((and objc-found-p c++-found-p) (objc++-mode))
       (objc-found-p (objc-mode))
       (c++-found-p (c++-mode))
       (t (c-mode))))))

(defun my-c++-mode-setup ()
  "Setup c++ mode."
  (use-package cc-mode
    :ensure t
    :mode (("\\.h\\'" . my-switch-mode-for-header-file)
           ("\\.mm\\'" . objc++-mode))))

(defun my-emacs-server-setup ()
  "Setup Emacs server."
  (use-package server
    :ensure t
    :config
    (unless (server-running-p)
      (server-start)))
  )

(defcustom my-after-ede-setup-hook nil
  "List of functions to call after setup for EDE."
  :type 'hook
  :group 'my-init)

(defun my-ede-and-semantic-mode-setup ()
  "Setup ede and semantic mode."
  (custom-set-variables
   '(semantic-idle-work-parse-neighboring-files-flag t))
  (global-ede-mode t)
  (use-package semantic
    :ensure t
    :config
    (semantic-mode 1)
    (when (file-directory-p "/usr/local/include/")
      (semantic-add-system-include "/usr/local/include/" 'c-mode)
      (semantic-add-system-include "/usr/local/include/" 'c++-mode))
    (global-semantic-idle-scheduler-mode 1)
    (global-semantic-idle-completions-mode 1)
    (global-semantic-idle-summary-mode 1)
    (global-semantic-highlight-func-mode)
    (run-hooks 'my-after-ede-setup-hook))
  )

(defun my-indent-tabs-mode-for-file-without-hard-tab ()
  "Set INDENT-TABS-MODE as nil for files without hard tab."
  (defun my-set-indent-tabs-mode ()
    "Disable INDENT-TABS-MODE when .editorconfig is unavailable (PROPS is nil)
 and existing file includes no hard tab."
    (defsubst my-buffer-contains-hard-tab-p (bytes-to-check)
      "Return first BYTES-TO-CHECK bytes of current buffer
 containing hard tab or not."
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (let ((limit (min (point-max) bytes-to-check)))
            (re-search-forward "\t" limit t)))))
    (defconst my-hard-tab-check-bytes 16384)
    (when (and buffer-file-name
               (not (and (fboundp 'editorconfig-core-get-properties)
                         (editorconfig-core-get-properties)))
               (or (not (file-exists-p buffer-file-name))
                   (not (my-buffer-contains-hard-tab-p
                         my-hard-tab-check-bytes))))
      (setq indent-tabs-mode nil)))
  (add-hook 'find-file-hook #'my-set-indent-tabs-mode)
  (add-hook 'after-change-major-mode-hook #'my-set-indent-tabs-mode))

(defun my-editorconfig-mode-setup ()
  "Setup for .editorconfig file."
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1)))

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
  (custom-set-variables
   '(read-buffer-completion-ignore-case t)
   '(read-file-name-completion-ignore-case t))
  )

(defun my-compilation-mode-setup ()
  "Setup compilation mode."
  (custom-set-variables
   '(compilation-scroll-output t)
   '(compile-command "time nice make -k -j "))
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(defun my-font-lock-setup ()
  "Setup font-lock."
  (custom-set-variables
   '(jit-lock-defer-time 0.05))
  )

(defun my-rainbow-delimiters-mode-setup ()
  "Setup \"rainbow-delimiters-mode\"."
  (use-package rainbow-delimiters
    :ensure t
    :requires (cl-lib color)
    :hook (prog-mode . rainbow-delimiters-mode)
    :config
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face"
                                 index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))))

(defun my-flycheck-mode-setup ()
  "Setup flycheck mode."
  (custom-set-variables
   '(flycheck-cppcheck-standards '("c++11"))
   '(flycheck-clang-language-standard "c++11")
   '(flycheck-gcc-language-standard "c++11"))
  (global-flycheck-mode t)
  )

(defun my-mocuur-setup ()
  "Setup moccur."
  (use-package color-moccur
    :ensure t
    :config
    (add-to-list 'dmoccur-exclusion-mask "\\.o$")
    (add-to-list 'dmoccur-exclusion-mask "\\.[a-z]?obj$")
    (add-to-list 'dmoccur-exclusion-mask "\\..?pch\\(\\.[a-z]+\\)?$")
    (add-to-list 'dmoccur-exclusion-mask "\\.p?db$")))

(defun my-plantuml-mode-setup ()
  "Setup \"plantuml-mode\"."
  (use-package plantuml-mode
    :ensure t
    :mode ("\\.pu\\'" "\\.puml\\'" "\\.plantuml\\'")
    :custom
    (plantuml-default-exec-mode 'jar)
    (plantuml-options "-charset UTF-8")
    (plantuml-output-type "png")
    (plantuml-indent-level 2)
    (plantuml-indent-regexp-activate-start
     "^\s*\\(activate\s+.+\\|[^\\+]+\\+\\+.*\\)$")
    (plantuml-indent-regexp-activate-end
     "^\s*\\(deactivate\s+.+\\|return\\(\s+.+\\)?\\)$")
    :config
    (when (equal system-type 'darwin)
      (defconst java-path-installed-by-brew
        "/usr/local/opt/openjdk/bin/java")
      (when (executable-find java-path-installed-by-brew)
        (setq plantuml-java-command java-path-installed-by-brew)))
    (defconst my-plantuml-jar-path (my-get-default-plantuml-jar-path)
      "Path of plantuml.jar.")
    (when (file-exists-p my-plantuml-jar-path)
      (setq plantuml-jar-path my-plantuml-jar-path))
    ;; To avoid "No Java runtime present, requesting install." message
    ;; even if installed OpenJDK by brew.
    (setq plantuml-java-args
          (delete "-Djava.awt.headless=true" plantuml-java-args))))

(defun my-dired-setup ()
  "Setup DIRED."
  (when (equal system-type 'darwin)
    (custom-set-variables
     '(dired-use-ls-dired nil))
    )
  )

(defun my-color-identifiers-mode-setup ()
  "Setup \"color-identifiers-mode\"."
  (global-color-identifiers-mode t)
  (run-with-idle-timer 1 t #'color-identifiers:refresh)
  )

(defun my-get-bash-aliases-with-file (&optional path)
  "Get bash aliases defined by PATH (if specified)."
  (let* ((my-prev-bash-env (getenv "BASH_ENV")))
    (when path
      (setenv "BASH_ENV" path))
    (let* ((my-bash-aliases (process-lines "bash" "-c" "alias")))
      (when path
        (setenv "BASH_ENV" my-prev-bash-env))
      my-bash-aliases)))

(defun my-eshell-import-bash-aliases ()
  "Import aliases defined for bash into eshell."
  (let* ((my-bash-aliases-list (my-get-bash-aliases-with-file "~/.bashrc")))
    (dolist (itr my-bash-aliases-list nil)
      (string-match "alias \\([^=]+\\)='\\([^']+\\)'" itr)
      (eshell/alias (match-string 1 itr) (match-string 2 itr)))))

(defun my-eshell-setup ()
  "Setup eshell."
  (with-eval-after-load 'em-term
    ;;; It is extra necessary to disable pager if you need.
    (add-to-list 'eshell-visual-subcommands
                 '("git" "diff" "help" "log" "show")))
  (with-eval-after-load 'em-alias
    (my-eshell-import-bash-aliases)))

(defun my-emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  nil)

(defun my-tempbuf-mode-setup ()
  "Setup tempbuf-mode."
  (if (my-emacs-wiki-elisp-installed-p "tempbuf")
      (progn (require 'tempbuf)
             (custom-set-variables '(tempbuf-kill-message nil)
                                   '(tempbuf-minimum-timeout 300))
             (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
             (add-hook 'magit-mode-hook 'turn-on-tempbuf-mode))
    (warn "Skipped configurations for tempbuf because not found.")))

(defun my-init-el-byte-compile ()
  "Byte compile this file if newer than elc."
  (defsubst my-update-byte-compile (el-path)
    "Byte compile EL-PATH if it is newer than its .elc."
    (defconst my-elc-path (my-get-elc-path el-path))
    (if (file-newer-than-file-p el-path my-elc-path)
        (byte-compile-file el-path)))
  (save-window-excursion
    (my-update-byte-compile my-init-el-path)))

(defun my-realgud-setup ()
  "Setup realgud."
  (when (and (executable-find "lldb") (package-installed-p 'realgud-lldb))
    (use-package realgud-lldb
      :ensure t)))

(defsubst my-ssh-key-exists-p ()
  "Check current user's ssh key(s) exists or not."
  (and (file-directory-p "~/.ssh")
       (file-expand-wildcards "~/.ssh/*.pub")))

(defun my-ssh-agency-setup ()
  "Setup ssh-agency."
  (when (package-installed-p 'ssh-agency)
    (with-eval-after-load 'ssh-agency
      (unless (my-ssh-key-exists-p)
        (remove-hook 'magit-credential-hook 'ssh-agency-ensure)))))

(defun my-wakatime-setup ()
  "Setup \"wakatime-mode\" for Emacs if wakatime available."
  (when (package-installed-p 'wakatime-mode)
    (global-wakatime-mode t)))

(defun my-adjust-font-size-setup ()
  "Set up hooks to adjust font size when necessary."
  (when (my-enabled-adjust-font-size-setup-p)
    (add-function :after after-focus-change-function #'my-adjust-font-size)
    (add-function :after after-focus-change-function #'my-adjust-font-size)))

;; Add "Accesibility" privilege to Emacs and /usr/bin/osascript
;; in order to use this function.
(defun my-toggle-input-method-darwin ()
  "Toggle macOS input method by sending key stroke via AppleScript."
  (interactive)
  ;; This elisp funtion sends Cmd + SPC key stroke.
  ;; If your environment has another key binding to toggle input method,
  ;; you must modify this.
  (call-process-shell-command
   (mapconcat
    #'shell-quote-argument
    (list "osascript"
          "-e" "tell application \"System Events\""
          "-e" "key code 49 using command down"
          "-e" "end tell")
    " ")))

(defun my-global-set-key-toggle-input-method ()
  "Set C-¥ key binding as toggling system's input method."
  (when (equal system-type 'darwin)
    (global-set-key (kbd "C-\\") nil)
    (global-set-key (kbd "C-\\") 'my-toggle-input-method-darwin)))

(defun my-backslash-key-setup ()
  "Enable to input backslash by opt+¥ on macOS."
  (when (and (equal system-type 'darwin) (equal mac-option-modifier 'meta))
    (define-key global-map [?\M-\\] [?\\])))

(defun my-javascript-setup ()
  "Setup for JavaScript."
  (custom-set-variables '(js-indent-level 2)))

(defun my-typescript-setup ()
  "Setup for TypeScript."
  (use-package typescript-ts-mode
    :mode (("\\.tsx\\'" . tsx-ts-mode)
           ("\\.ts\\'" . tsx-ts-mode))
    :config
    (setq typescript-ts-mode-indent-offset 2))
  (use-package treesit
    :config
    (setq treesit-font-lock-level 4))
  (use-package treesit-auto
    :ensure t
    :config
    (global-treesit-auto-mode)
    (setq treesit-auto-install t))
  (use-package tree-sitter
    :ensure t
    :hook ((typescript-ts-mode . tree-sitter-hl-mode)
           (tsx-ts-mode . tree-sitter-hl-mode))
    :config
    (global-tree-sitter-mode))
  (use-package tree-sitter-langs
    :ensure t
    :after tree-sitter
    :config
    (tree-sitter-require 'tsx)
    (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))
  (use-package tide
    :ensure t
    :hook (tsx-ts-mode . setup-tide-mode)
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))
    (setq company-tooltip-align-annotations t)))

(defun my-magit-setup ()
  "Setup for magit."
  (delete 'Git vc-handled-backends)
  (custom-set-variables
   '(magit-refresh-status-buffer nil)))

(defun my-disable-whitespace-mode-if-non-file ()
  "Disable whitespace mode if current buffer is not file."
  (unless buffer-file-name
    (whitespace-mode -1)))

(defun my-whitespace-mode-setup ()
  "Setup for whitespace mode."
  (use-package whitespace
    :ensure t
    :custom
    (whitespace-style '(face trailing tabs))
    :config
    (global-whitespace-mode)
    :hook
    (after-change-major-mode . my-disable-whitespace-mode-if-non-file)))

;; Utility functions for users.
(defun my-semanticdb-update-for-directory (dir-path)
  "Update semanticdb for files under specified DIR-PATH."
  (interactive "Directory which has files for updating semanticdb: ")
  (let* ((my-semanticdb-update-dir-entries
          (cl-remove-if (lambda (str) (string-match str "\.\.?"))
                        (directory-files dir-path))))
    (dolist (itr my-semanticdb-update-dir-entries)
      (let* ((my-semanticdb-update-dir-entry (my-join-path dir-path itr)))
        (if (file-directory-p my-semanticdb-update-dir-entry)
            (my-semanticdb-update-for-directory my-semanticdb-update-dir-entry)
          (semanticdb-file-table-object my-semanticdb-update-dir-entry))))))

;;; my-init.el ends here
