;;; Unit tests of functions in my-init.el.
(require 'ert)

(defconst this-dir (file-name-directory (or load-file-name buffer-file-name)))
(defconst parent-dir (file-name-directory (directory-file-name this-dir)))
(load-file (concat parent-dir "my-init.el"))

(ert-deftest my-get-default-plantuml-jar-path-test ()
  (should (< 0 (length (my-get-default-plantuml-jar-path)))))
