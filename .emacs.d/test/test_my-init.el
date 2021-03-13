;;; Unit tests of functions in my-init.el.
(require 'ert)

(defconst this-dir (file-name-directory (or load-file-name buffer-file-name)))
(defconst parent-dir (file-name-directory (directory-file-name this-dir)))
(load-file (concat parent-dir "my-init.el"))

(ert-deftest my-get-default-plantuml-jar-path-test ()
  (should (< 0 (length (my-get-default-plantuml-jar-path)))))

(ert-deftest my-join-path-test ()
  (defconst my-test-dir "/foo")
  (defconst my-test-file-name "bar")
  (defconst my-joined-path (my-join-path my-test-dir my-test-file-name))
  (should (< (+ (length my-test-dir) (length my-test-file-name))
             (length my-joined-path)))
  (should (<= 0
              (string-match (concat "^" my-test-dir) my-joined-path)))
  (should (< (length my-test-dir)
             (string-match (concat my-test-file-name "$") my-joined-path))))

(ert-deftest my-get-elc-path-test ()
  (defconst my-test-elisp-path "/foo/bar.el")
  (defconst my-test-elc-path (my-get-elc-path my-test-elisp-path))
  (should (string= (file-name-sans-extension my-test-elisp-path)
                   (file-name-sans-extension my-test-elc-path)))
  (should (string= ".elc"
                   (file-name-extension my-test-elc-path t))))




;;
