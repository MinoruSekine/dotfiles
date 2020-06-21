(defun install-elisp-lint-batch ()
  "Batch install elisp-lint on the command line."
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install 'package-lint)
  (package-install 'elisp-lint))
