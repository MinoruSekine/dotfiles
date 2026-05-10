;;; my-early-init.el --- My Emacs early initialization codes. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file includes elisp codes to initialize Emacs settings.
;;   - Enable package-quickstart
;;   - Relax gc-cons-threshold while launching Emacs
;; It will be loaded from ~/.emacs.d/early-init.el.

;;; Code:

;; Customizable variables.
(defcustom my-gc-cons-threshold-launching (* 128 1024 1024)
  "GC-CONS-THRESHOLD while launching Emacs."
  :type 'integer
  :group 'my-init)

;;; Main processes.

;; Reduce invoking GC on launching.
(setq gc-cons-threshold my-gc-cons-threshold-launching)

(setq package-quickstart t)

;;; my-early-init.el ends here
