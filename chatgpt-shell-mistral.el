;;; chatgpt-shell-mistral.el --- Mistral support for `chatgpt-shell' -*- lexical-binding: t; -*-

;;; Commentary:

;; Adds Mistral specifics for `chatgpt-shell'.
;;
;; Talks to the Mistral API at https://api.mistral.ai/v1/chat/completions
;; using an OpenAI-compatible request/response format, so most of the
;; request building and header logic is shared with `chatgpt-shell-openai'.
;;
;; Supports standard chat models (mistral-large/medium/small, codestral,
;; devstral, nemo, ministral-*) and Magistral reasoning models.  Magistral
;; models return content as an array of typed blocks rather than a plain
;; string; `chatgpt-shell-mistral--content-text' handles both forms.
;;
;; API key is read from `chatgpt-shell-mistral-key'.
;; See https://docs.mistral.ai/getting-started/models/models_overview/ for
;; the current model list and context-window sizes.

;;; Code:

(require 'map)

(cl-defun chatgpt-shell-mistral-make-model (&key label version short-version token-width context-window validate-command other-params)
  "Create a Mistral model.

Set LABEL, VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW,
VALIDATE-COMMAND and OTHER-PARAMS."
  (chatgpt-shell-openai-make-model
   :label label
   :version version
   :short-version short-version
   :token-width token-width
   :context-window context-window
   :other-params other-params
   :validate-command #'chatgpt-shell-mistral--validate-command
   :url-base 'chatgpt-shell-mistral-api-url-base
   :path "/chat/completions"
   :provider "Mistral"
   :key #'chatgpt-shell-mistral-key
   :headers #'chatgpt-shell-mistral--make-headers
   :handler #'chatgpt-shell-mistral--handle-command
   :filter #'chatgpt-shell-mistral--filter-output
   :icon "mistral-color.png"))

(defun chatgpt-shell-mistral-models ()
  "Build a list of Mistral LLM models."
  ;; See https://docs.mistral.ai/getting-started/models/models_overview/
  (list (chatgpt-shell-mistral-make-model
         :version "mistral-large-latest"
         :short-version "large"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "mistral-medium-latest"
         :short-version "medium"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "mistral-small-latest"
         :short-version "small"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "codestral-latest"
         :short-version "codestral"
         :label "Mistral"
         :token-width 3
         :context-window 262144)
        (chatgpt-shell-mistral-make-model
         :version "devstral-latest"
         :short-version "devstral"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "magistral-medium-latest"
         :short-version "magistral-medium"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "magistral-small-latest"
         :short-version "magistral-small"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "open-mistral-nemo"
         :short-version "nemo"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "ministral-14b-latest"
         :short-version "ministral-14b"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "ministral-8b-latest"
         :short-version "ministral-8b"
         :label "Mistral"
         :token-width 3
         :context-window 131072)
        (chatgpt-shell-mistral-make-model
         :version "ministral-3b-latest"
         :short-version "ministral-3b"
         :label "Mistral"
         :token-width 3
         :context-window 131072)))

(defcustom chatgpt-shell-mistral-api-url-base "https://api.mistral.ai/v1"
  "Mistral API's base URL.

API url = base + path.

If you use Mistral through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-mistral-key nil
  "Mistral key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defun chatgpt-shell-mistral-key ()
  "Get the Mistral key."
  (cond ((stringp chatgpt-shell-mistral-key)
         chatgpt-shell-mistral-key)
        ((functionp chatgpt-shell-mistral-key)
         (condition-case _err
             (funcall chatgpt-shell-mistral-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-mistral--handle-command (&key model command context shell settings)
  "Handle Mistral COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (chatgpt-shell-openai--handle-chatgpt-command
   :model model
   :command command
   :context context
   :shell shell
   :settings settings
   :key #'chatgpt-shell-mistral-key
   :filter #'chatgpt-shell-mistral--filter-output
   :missing-key-msg "Your chatgpt-shell-mistral-key is missing"))

(defun chatgpt-shell-mistral--content-text (content)
  "Extract text from CONTENT, which may be a string or array of content blocks.

Magistral reasoning models return content as an array of typed blocks:
  [{\"type\": \"thinking\", ...}, {\"type\": \"text\", \"text\": \"...\"}]

Standard Mistral models return a plain string.  Thinking blocks are discarded;
only text blocks are returned."
  (cond
   ((and content (not (eq content :null)) (stringp content))
    content)
   ((and (sequencep content) (not (stringp content)))
    (mapconcat (lambda (block)
                 (let-alist block
                   (if (equal .type "text") (or .text "") "")))
               content ""))
   (t "")))

(defun chatgpt-shell-mistral--filter-output (output)
  "Process pending OUTPUT for Mistral models including Magistral reasoning models.

OUTPUT is always of the form:
  ((:function-calls . ...)
   (:pending . ...)
   (:filtered . ...))

Handles Magistral's array-based content blocks in addition to the standard
string content used by other Mistral models."
  (cond ((stringp output)
         (error "Please upgrade shell-maker to 0.79.1 or newer"))
        ((equal (string-trim (map-elt output :pending))
                "data: [DONE]")
         (setf (map-elt output :pending) "")))
  (if-let* ((whole (shell-maker--json-parse-string (map-elt output :pending)))
            (response-text
             (or (let-alist whole .error.message)
                 (let-alist whole
                   (mapconcat (lambda (choice)
                                (let-alist choice
                                  (chatgpt-shell-mistral--content-text
                                   (or .delta.content .message.content))))
                              .choices "")))))
      (list (cons :filtered response-text))
    (when-let ((chunks (shell-maker--split-text (map-elt output :pending))))
      (let ((response-text "")
            (pending))
        (mapc
         (lambda (chunk)
           (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                     (obj (shell-maker--json-parse-string (map-elt chunk :value))))
               (let-alist obj
                 (let ((text (mapconcat
                              (lambda (choice)
                                (let-alist choice
                                  (chatgpt-shell-mistral--content-text
                                   (or (and (not (eq .delta.content :null))
                                            .delta.content)
                                       .message.content))))
                              .choices "")))
                   (unless (string-empty-p text)
                     (setq response-text (concat response-text text)))))
             (setq pending (concat pending
                                   (or (map-elt chunk :key) "")
                                   (map-elt chunk :value)))))
         chunks)
        (setf (map-elt output :filtered)
              (unless (string-empty-p response-text) response-text))
        (setf (map-elt output :pending) (or pending ""))
        output))))

(defun chatgpt-shell-mistral--make-headers (&rest args)
  "Create Mistral API headers.

Passes ARGS through to the underlying OpenAI-compatible header builder."
  (apply #'chatgpt-shell-openai--make-headers
         :key #'chatgpt-shell-mistral-key
         args))

(defun chatgpt-shell-mistral--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-mistral-key
    "Variable `chatgpt-shell-mistral-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-mistral-key

or

(setq chatgpt-shell-mistral-key \"my-key\")"))

(provide 'chatgpt-shell-mistral)
;;; chatgpt-shell-mistral.el ends here
