
(require 'ert)

;;; chatgpt-shell-model-version

(ert-deftest test-chatgpt-shell-model-version ()
  "Tests for `chatgpt-shell-model-version` function."
  (let ((chatgpt-shell-model-version "gpt-3.5")
        (chatgpt-shell-model-versions '("gpt-3" "gpt-3.5" "gpt-4")))
    ;; Test for string version
    (should (equal (chatgpt-shell-model-version) "gpt-3.5")))
  (let ((chatgpt-shell-model-version 1)
        (chatgpt-shell-model-versions '("gpt-3" "gpt-3.5" "gpt-4")))
    ;; Test for integer index
    (should (equal (chatgpt-shell-model-version) "gpt-3.5")))
  (let ((chatgpt-shell-model-version nil)
        (chatgpt-shell-model-versions '("gpt-3" "gpt-3.5" "gpt-4")))
    ;; Test for nil, expecting nil
    (should (equal (chatgpt-shell-model-version) nil))))

;;; chatgpt-shell--append-system-info

(ert-deftest test-chatgpt-shell--append-system-info-smoke-test ()
  (let ((output (chatgpt-shell--append-system-info "abc")))
    (should-not (null output))
    ))

(defun mock-shell-command-to-string (command)
  "A mock version of `shell-command-to-string` for testing purposes."
  (concat "mocked system info"))

(defun mock-emacs-version ()
  "A mock version of `emacs-version` for testing purposes."
  "mocked emacs version")

(ert-deftest test-chatgpt-shell--append-system-info-with-mock ()
  (cl-letf (((symbol-function 'shell-command-to-string) #'mock-shell-command-to-string)
            ((symbol-function 'emacs-version) #'mock-emacs-version)
            )
    (should (let ((system-type 'darwin))
              (equal (chatgpt-shell--append-system-info "foo")
                     "foo\n# System info\n\n## OS details\nmocked system info\n## Editor\nmocked emacs version")))
    (should (let ((system-type 'gnu/linux))
              (equal (chatgpt-shell--append-system-info "foo")
                     "foo\n# System info\n\n## OS details\nmocked system info\n## Editor\nmocked emacs version")))
            ))
