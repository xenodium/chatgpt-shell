(require 'ert)

;;; chatgpt-shell--append-system-info

(ert-deftest test-chatgpt-shell--append-system-info-smoke-test ()
  (let ((output (chatgpt-shell--append-system-info "abc")))
    (should-not (null output))))

(defun mock-shell-command-to-string (command)
  "A mock version of `shell-command-to-string` for testing purposes."
  "mocked system info")

(defun mock-emacs-version ()
  "A mock version of `emacs-version` for testing purposes."
  "mocked emacs version")

(ert-deftest test-chatgpt-shell--append-system-info-with-mock ()
  (cl-letf (((symbol-function 'shell-command-to-string) #'mock-shell-command-to-string)
            ((symbol-function 'emacs-version) #'mock-emacs-version))
    (should (let ((system-type 'darwin))
              (equal (chatgpt-shell--append-system-info "foo")
                     "foo\n# System info\n\n## OS details\nmocked system info\n## Editor\nmocked emacs version")))
    (should (let ((system-type 'gnu/linux))
              (equal (chatgpt-shell--append-system-info "foo")
                     "foo\n# System info\n\n## OS details\nmocked system info\n## Editor\nmocked emacs version")))))
