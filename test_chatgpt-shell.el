;;; test_chatgpt-shell.el --- Tests for chatgpt-shell  -*- lexical-binding: t -*-
(require 'ert)
(require 'chatgpt-shell)
(require 'chatgpt-shell-google)

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

(ert-deftest test-chatgpt-shell-google-interactions-url ()
  (let ((model (chatgpt-shell-google-make-interactions-model
                :version "gemini-3.6-flash"
                :short-version "gemini-3.6-flash"
                :token-width 4
                :context-window 1000)))
    (should (equal (chatgpt-shell-google--make-interactions-url :model model :settings nil)
                   "https://generativelanguage.googleapis.com/v1beta/interactions"))
    (should (equal (chatgpt-shell-google--make-interactions-url :model model :settings '((:streaming . t)))
                   "https://generativelanguage.googleapis.com/v1beta/interactions?alt=sse"))))

(ert-deftest test-chatgpt-shell-google-interactions-payload ()
  (let ((model (chatgpt-shell-google-make-interactions-model
                :version "gemini-3.6-flash"
                :short-version "gemini-3.6-flash"
                :token-width 4
                :context-window 1000)))
    (let ((payload (chatgpt-shell-google--make-interactions-payload
                    :prompt "Hello world"
                    :context nil
                    :settings '((:streaming . t))
                    :model model)))
      (should (equal (map-elt payload 'model) "gemini-3.6-flash"))
      (should (equal (map-elt payload 'input) "Hello world"))
      (should (equal (map-elt payload 'stream) t))
      (should (equal (map-elt payload 'tools) [((type . "google_search")) ((type . "url_context"))])))))

(ert-deftest test-chatgpt-shell-google-parse-interactions-json ()
  (let ((json '((id . "v1_123")
                (status . "completed")
                (steps . [((type . "model_output")
                           (content . [((type . "text") (text . "AI is machine learning."))]))]))))
    (should (equal (chatgpt-shell-google--parse-interactions-json json)
                   "AI is machine learning."))))

(ert-deftest test-chatgpt-shell-google-effective-fallback-models ()
  (let ((chatgpt-shell-google-fallback-models '("gemini-3.5-flash" "gemini-2.5-flash" "gemini-2.0-flash"))
        (model '((:version . "gemini-3.6-flash"))))
    (should (equal (chatgpt-shell-google--effective-fallback-models model)
                   '("gemini-3.5-flash" "gemini-2.5-flash" "gemini-2.0-flash")))))

(ert-deftest test-chatgpt-shell-google-previous-interaction-id ()
  (let ((model (chatgpt-shell-google-make-interactions-model
                :version "gemini-3.6-flash"
                :short-version "gemini-3.6-flash"
                :token-width 4
                :context-window 1000))
        (chatgpt-shell-google--last-interaction-id nil))
    ;; Turn 1: No previous interaction ID
    (let ((json '((id . "v1_INTERACTION_123")
                  (status . "completed")
                  (steps . [((type . "model_output")
                             (content . [((type . "text") (text . "Response 1"))]))]))))
      (chatgpt-shell-google--parse-interactions-json json)
      (should (equal chatgpt-shell-google--last-interaction-id "v1_INTERACTION_123")))
    ;; Turn 2: Should include previous_interaction_id in payload
    (let ((payload (chatgpt-shell-google--make-interactions-payload
                    :prompt "How many paws?"
                    :context '(("I have 2 dogs." . "Response 1"))
                    :settings nil
                    :model model)))
      (should (equal (map-elt payload 'previous_interaction_id) "v1_INTERACTION_123"))
      (should (equal (map-elt payload 'input) "How many paws?")))))
