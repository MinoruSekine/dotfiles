;;; my-init.el --- My Emacs initialization codes.

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
  (cond ((equal system-type 'darwin)
         (substring (shell-command-to-string
                     "echo /usr/local/Cellar/plantuml/*/libexec/plantuml.jar")
                    0
                    -1))
        ((equal system-type 'windows-nt)
         (expand-file-name (replace-regexp-in-string
                            "plantuml.\\(cmd\\|jar\\)\n"
                            "plantuml.jar"
                            (shell-command-to-string "scoop which plantuml"))))
        ((equal system-type 'gnu/linux)
         "/usr/share/plantuml/plantuml.jar")
        ))

(defun my-add-hooks (hook-to-add my-list)
  "Add each function in MY-LIST into HOOK-TO-ADD ."
  (dolist (itr my-list)
    (add-hook hook-to-add itr)))

(defun my-is-network-connection-available ()
  "Get network connection is available or not."
  ;; The local loop back device may always be included
  ;; in return of network-interface-list.
  (>= (length (network-interface-list)) 2))

(defun my-join-path (dir file-name)
  "Join FILE-NAME string into DIR with right path delimiter."
  (concat (file-name-as-directory dir) file-name))

(defun my-get-elc-path (el-path)
  "Get .elc path from .el path specified by EL-PATH."
  (concat (file-name-sans-extension el-path) ".elc"))


(defun my-update-byte-compile (el-path)
  "Byte compile EL-PATH if it is newer than its .elc."
  (defconst my-elc-path (my-get-elc-path el-path))
  (if (file-newer-than-file-p el-path my-elc-path)
      (byte-compile-file el-path)))

(defun my-get-gcc-system-include-paths ()
  "Get system header include path used by gcc."
  (when (and (executable-find "gcc")
	     (executable-find "sed"))
    (split-string (shell-command-to-string "gcc -x c++ -v -E /dev/null 2>&1 > /dev/null | sed -e '1,/> search starts here:/d' | sed -n '/End of search list./q;p' | sed -e 's/^ *//g'") "\n" t)))

(defun my-install-missing-packages ()
  "Install missing packages."
  (defvar my-packages '(beacon
                        color-identifiers-mode
                        color-moccur
                        elisp-lint
                        flycheck
                        google-c-style
                        highlight-doxygen
                        magit
                        markdown-mode
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

  (unless (fboundp 'remove-if-not)
    (require 'cl))
  (defvar my-not-yet-installed-packages
    (remove-if-not (lambda (p) (not (package-installed-p p)))
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

(defun my-emacs-wiki-elisp-file-name (elisp-name)
  "Filename of ELISP-NAME installed from Emacs Wiki."
  (concat elisp-name ".el"))

(defvar my-emacs-wiki-elisp-dir-root (expand-file-name "~/.emacs.d/emacs-wiki")
  "Path to install elisps from Emacs Wiki.")

(defun my-emacs-wiki-elisp-dir (elisp-name)
  "Directory for ELISP-NAME installed from Emacs Wiki."
  (my-join-path my-emacs-wiki-elisp-dir-root elisp-name))

(defun my-emacs-wiki-elisp-path (elisp-name)
  "Path for ELISP-NAME installed from Emacs Wiki."
  (my-join-path (my-emacs-wiki-elisp-dir elisp-name) (my-emacs-wiki-elisp-file-name elisp-name)))

(defun my-emacs-wiki-elisp-url (elisp-name)
  "URL for ELISP-NAME installed from Emacs Wiki."
  (concat "https://www.emacswiki.org/emacs/download/" (my-emacs-wiki-elisp-file-name elisp-name)))

(defun my-emacs-wiki-is-elisp-installed (elisp-name)
  "Get ELISP-NAME form Emacs Wiki installed or not."
  (file-exists-p (my-emacs-wiki-elisp-path elisp-name)))

(defun my-setup-elisp-from-emacs-wiki ()
  "Install missing elisp from Emacs Wiki and set 'load-path'."
  (defvar my-elisp-from-emacs-wiki '("tempbuf"))
  (dolist (p my-elisp-from-emacs-wiki)
    (defconst this-elisp-dir (my-emacs-wiki-elisp-dir p))
    (unless (my-emacs-wiki-is-elisp-installed p)
      (defconst this-elisp-file (my-emacs-wiki-elisp-path p))
      (unless (file-directory-p this-elisp-dir)
        (make-directory this-elisp-dir t))
      (url-copy-file (my-emacs-wiki-elisp-url p) this-elisp-file t)
      (byte-compile-file this-elisp-file))
    (add-to-list 'load-path this-elisp-dir)))

(defun my-gc-setup ()
  "Settings for garbage collection."
  (setq gc-cons-threshold (* 256 1024 1024))
  (setq garbage-collection-messages t)
  (run-with-idle-timer 120 nil #'garbage-collect))

;;; Functions for auto upgrade packages.
(defconst my-upgrade-interval-days 7)
(defconst my-default-last-upgrade-time
  (time-subtract (current-time)
                 (* 60 60 24 my-upgrade-interval-days)))
(define-multisession-variable my-last-upgrade-time my-default-last-upgrade-time)

(defun my-auto-upgrade-packages-interval-expired-p ()
  "Return t if it is necessary to upgrade packages."
  (defconst last-upgrade-time (multisession-value my-last-upgrade-time))
  (defconst days-from-last-upgrade
    (/ (time-convert (time-subtract (current-time) last-upgrade-time)
		     'integer)
       (* 60 60 24)))
  (> days-from-last-upgrade my-upgrade-interval-days))

(defun my-auto-upgrade-packages ()
  "Auto upgrade packages if interval expired, interactive, and network available."
  (when (and (not noninteractive)
             (my-auto-upgrade-packages-interval-expired-p)
             (my-is-network-connection-available)
             (y-or-n-p "Upgrade packages now?"))
    (progn (package-initialize)
           (package-refresh-contents)
           (package-upgrade-all)
           (setf (multisession-value my-last-upgrade-time)
                 (current-time)))))

;;; Main processes.
(if (my-is-network-connection-available)
    (progn (my-install-missing-packages)
           (my-setup-elisp-from-emacs-wiki))
  (display-warning 'my-init "Network connection may not be available."))

(defvar my-after-init-func-list '(my-gc-setup
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

(defvar my-init-el-path load-file-name)
(defvar my-emacs-startup-func-list
  '(my-emacs-server-setup
    my-backup-directory-setup
    my-default-directory-to-home-setup
    my-completion-case-sensitivity-setup
    my-ede-and-semantic-mode-setup
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
    my-wakatime-setup
    my-magit-setup
    my-init-el-byte-compile))
(my-add-hooks 'emacs-startup-hook my-emacs-startup-func-list)

(defvar my-kill-emacs-func-list
  '(my-auto-upgrade-packages))
(my-add-hooks 'kill-emacs-hook my-kill-emacs-func-list)

;;; Functions for initializing Emacs.
(defun my-environment-variable-setup ()
  "Set up environment variables."
  ;;; Pager in Emacs (eshell, terms, ...)
  (setenv "PAGER" "")

  (setq exec-path (append exec-path
                          (parse-colon-path (getenv "PATH"))))
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
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (beacon-mode t)
  (setq beacon-color "yellow")
  (setq beacon-blink-duration 0.1)
  (setq blink-cursor-blinks 0)
  (highlight-doxygen-global-mode 1)
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
	     when (> (length (assoc 'frames itr)) 1) return (assoc 'workarea itr))))

(defun my-get-display-mm-width ()
  "Get width of display in mm."
  (nth 1
       (cl-loop for itr in (display-monitor-attributes-list)
	     when (> (length (assoc 'frames itr)) 1) return (assoc 'mm-size itr))))

(defun my-get-display-inch-width ()
  "Get width of display in inch."
  (/ (my-get-display-mm-width) 25.4))

(defun my-get-display-dpi ()
  "Get DPI of display."
  (/ (my-get-display-pixel-width) (my-get-display-inch-width)))

(defun my-is-enabled-adjust-font-size-setup ()
  "Indicated necessary to adjust font size."
  (and (not (equal system-type 'windows-nt))  ;; NTEmacs seems to support high DPI awareness.
       (display-graphic-p)
       (display-supports-face-attributes-p :height)))

(defun my-get-font-zoom-ratio-for-display ()
  "Get font zoom ratio for display."
  (if (my-is-enabled-adjust-font-size-setup)
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

(defun my-c++-mode-setup ()
  "Setup c++ mode."
  (defun my-c++-keyword-match-function ()
    "Find C++ keyword(s)."
    (re-search-forward "\\<\\(class\\|constexpr\\|namespace\\|template\\|auto\\)\\>"
                       magic-mode-regexp-match-limit t))
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (my-c++-keyword-match-function)))
                 . c++-mode))
  (add-to-list 'magic-fallback-mode-alist
               `(,(lambda ()
                    (my-c++-keyword-match-function))
                 . c++-mode))
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (re-search-forward "@\\<\\(implementation\\|interface\\|protocol\\)\\>"
                                            magic-mode-regexp-match-limit t)))
                 . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
  (font-lock-add-keywords 'c++-mode
                          '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face prepend)))
  (font-lock-add-keywords 'c++-mode
                          '(("[ \t]+$" . 'trailing-whitespace)))
  (font-lock-add-keywords 'c-mode
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

(defvar my-after-ede-setup-hook nil
  "List of functions to call after setup for EDE.")

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
  (global-semantic-highlight-func-mode)
  (run-hooks 'my-after-ede-setup-hook)
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

(defun my-compilation-mode-setup ()
  "Setup compilation mode."
  (with-eval-after-load 'compile
    (setq compilation-scroll-output t)
    (setq compile-command "time nice make -k -j ")))

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

(defvar my-plantuml-jar-path (my-get-default-plantuml-jar-path)
  "Path of plantuml.jar.")

(defun my-plantuml-mode-setup ()
  "Setup plantuml-mode."
  (add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; To avoid "No Java runtime present, requesting install." message
  ;; even if installed OpenJDK by brew.
  (when (equal system-type 'darwin)
    (defconst java-path-installed-by-brew
      "/usr/local/opt/openjdk/bin/java")
    (when (executable-find java-path-installed-by-brew)
      (setq plantuml-java-command java-path-installed-by-brew)))
  (when (file-exists-p my-plantuml-jar-path)
    (setq plantuml-jar-path my-plantuml-jar-path))
  (setq plantuml-default-exec-mode 'jar)
  (with-eval-after-load 'plantuml
    (setq plantuml-java-args
          (delete "-Djava.awt.headless=true" plantuml-java-args)))
  (setq plantuml-options "-charset UTF-8")
  (setq plantuml-output-type "png")
  (setq plantuml-indent-level 2)
  (setq plantuml-indent-regexp-activate-start "^\s*\\(activate\s+.+\\|[^\\+]+\\+\\+.*\\)$")
  (setq plantuml-indent-regexp-activate-end "^\s*\\(deactivate\s+.+\\|return\\(\s+.+\\)?\\)$")
  (add-hook 'plantuml-mode-hook
            '(lambda()
               (setq indent-tabs-mode nil)
               ))

  )

(defun my-dired-setup ()
  "Setup dired."
  (when (equal system-type 'darwin)
    (setq dired-use-ls-dired nil)
    )
  )

(defun my-color-identifiers-mode-setup ()
  "Setup color-identifiers-mode."
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
  (with-eval-after-load 'eshell
    (require 'em-term)
    ;;; It is extra necessary to disable pager if you need.
    (add-to-list 'eshell-visual-subcommands
                 '("git" "diff" "help" "log" "show")))
  (with-eval-after-load 'em-alias
    (my-eshell-import-bash-aliases)))

(defun my-emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  (add-hook 'emacs-lisp-mode-hook
            '(lambda()
               (setq indent-tabs-mode nil)
               )
            )
  )

(defun my-tempbuf-mode-setup ()
  "Setup tempbuf-mode."
  (require 'tempbuf)
  (custom-set-variables '(tempbuf-kill-message nil)
                        '(tempbuf-minimum-timeout 300))
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'magit-mode-hook 'turn-on-tempbuf-mode))

(defun my-init-el-byte-compile ()
  "Byte compile this file if newer than elc."
  (save-window-excursion
   (my-update-byte-compile my-init-el-path)))

(defun my-realgud-setup ()
  "Setup realgud."
  (when (package-installed-p 'realgud-lldb)
    (require 'realgud-lldb)))

(defun my-ssh-key-exists-p ()
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
  "Setup wakatime-mode for Emacs if wakatime available."
  (when (package-installed-p 'wakatime-mode)
    (global-wakatime-mode t)))

(defun my-adjust-font-size-setup ()
  "Set up hooks to adjust font size when necessary."
  (when (my-is-enabled-adjust-font-size-setup)
    (add-function :after after-focus-change-function #'my-adjust-font-size)
    (add-function :after after-focus-change-function #'my-adjust-font-size)))

;; For this function, add "Accesibility" privilege to Emacs and /usr/bin/osascript.
(defun my-toggle-input-method-darwin ()
  "Toggle macOS input method by sending key stroke via AppleScript."
  (interactive)
  ;; This elisp funtion sends Cmd + SPC key stroke.
  ;; If your environment has another key binding to toggle input method,
  ;; you must modify this.
  (shell-command-to-string
   "osascript -e 'tell application \"System Events\"' -e 'key code 49 using command down' -e 'end tell'"))

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
  (add-hook 'js-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq js-indent-level 2))))

(defun my-magit-setup ()
  "Setup for magit."
  (setq magit-refresh-status-buffer nil))

;; Utility functions for users.
(defun my-semanticdb-update-for-directory (dir-path)
  "Update semanticdb for files under specified DIR-PATH."
  (interactive "DDirectory which has files for updating semanticdb: ")
  (let* ((my-semanticdb-update-dir-entries
          (cl-remove-if (lambda (str) (string-match str "\.\.?")) (directory-files dir-path))))
    (dolist (itr my-semanticdb-update-dir-entries)
      (let* ((my-semanticdb-update-dir-entry (my-join-path dir-path itr)))
	(if (file-directory-p my-semanticdb-update-dir-entry)
            (my-semanticdb-update-for-directory my-semanticdb-update-dir-entry)
	  (semanticdb-file-table-object my-semanticdb-update-dir-entry))))))
;;; my-init.el ends here
