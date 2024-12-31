;;; chatgpt-shell-openrouter.el --- OpenRouter-specific logic -*- lexical-binding: t; -*-

(cl-defun chatgpt-shell-openrouter-make-model (&rest args &key label version short-version token-width context-window validate-command other-params)
  (apply #'chatgpt-shell-openai-make-model
         :validate-command #'chatgpt-shell-openrouter--validate-command
         :url-base 'chatgpt-shell-openrouter-api-url-base
         :path "/v1/chat/completions"
         :provider "OpenRouter"
         :key #'chatgpt-shell-openrouter-key
         :headers #'chatgpt-shell-openrouter--make-headers
         :handler #'chatgpt-shell-openrouter--handle-chatgpt-command
         :filter #'chatgpt-shell-openrouter--filter-output
         args))

(defun chatgpt-shell-openrouter-models ()
  "Build a list of OpenRouter LLM models."
  (list (chatgpt-shell-openrouter-make-model
         :version "meta-llama/llama-3.3-70b-instruct"
         :short-version "llama-3.3-70b"
         :label "Llama"
         :token-width 16
         ;; See https://openrouter.ai/meta-llama/llama-3.3-70b-instruct.
         :context-window 131072
         ;; Multiple quantizations are offered for this model by different
         ;; providers so we restrict to one for consistency. Note that the sense
         ;; in which provider is used here means the providers available through
         ;; OpenRouter. This is different from the meaning of the :provider
         ;; argument.
         ;;
         ;; See https://openrouter.ai/docs/provider-routing#quantization
         :other-params '((provider (quantizations . ["bf16"]))))
        (chatgpt-shell-openrouter-make-model
         :version "qwen/qwq-32b-preview"
         :short-version "qwq-32b-preview"
         :label "Qwen"
         :token-width 16
         ;; See
         :context-window 32768
         ;; Multiple quantizations are offered for this model by different
         ;; providers so we restrict to one for consistency. Note that the sense
         ;; in which provider is used here means the providers available through
         ;; OpenRouter. This is different from the meaning of the :provider
         ;; argument.
         ;;
         ;; See https://openrouter.ai/qwen/qwq-32b-preview
         :other-params '((provider (quantizations . ["bf16"]))))
        (chatgpt-shell-openrouter-make-model
         :version "openai/o1"
         :short-version "o1"
         :label "ChatGPT"
         :token-width 3
         ;; See https://openrouter.ai/openai/o1-2024-12-17
         :context-window 200000
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openrouter-make-model
         :version "qwen/qwen-2.5-coder-32b-instruct"
         :short-version "qwen-2.5-coder-32b"
         :label "Qwen"
         :token-width 16
         ;; See
         :context-window 32768
         ;; Multiple quantizations are offered for this model by different
         ;; providers so we restrict to one for consistency. Note that the sense
         ;; in which provider is used here means the providers available through
         ;; OpenRouter. This is different from the meaning of the :provider
         ;; argument.
         ;;
         ;; See https://openrouter.ai/qwen/qwen-2.5-coder-32b-instruct
         :other-params '((provider (quantizations . ["bf16"]))))))

(defcustom chatgpt-shell-openrouter-api-url-base "https://openrouter.ai/api"
  "OpenRouter API's base URL.

API url = base + path.

If you use OpenRouter through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-openrouter-key nil
  "OpenRouter key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defun chatgpt-shell-openrouter-key ()
  "Get the OpenRouter key."
  (cond ((stringp chatgpt-shell-openrouter-key)
         chatgpt-shell-openrouter-key)
        ((functionp chatgpt-shell-openrouter-key)
         (condition-case _err
             (funcall chatgpt-shell-openrouter-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(defun chatgpt-shell-openrouter--handle-chatgpt-command (&rest args &key model command context shell settings)
  (apply #'chatgpt-shell-openai--handle-chatgpt-command
         :key #'chatgpt-shell-openrouter-key
         :filter #'chatgpt-shell-openrouter--filter-output
         :missing-key-msg "Your chatgpt-shell-openrouter-key is missing"
         args))

(defun chatgpt-shell-openrouter--filter-output (raw-response)
  (unless (string= (string-trim raw-response) ": OPENROUTER PROCESSING")
    (chatgpt-shell-openai--filter-output raw-response)))

(defun chatgpt-shell-openrouter--make-headers (&rest args)
  (apply #'chatgpt-shell-openai--make-headers
         :key #'chatgpt-shell-openrouter-key
         args))

(defun chatgpt-shell-openrouter--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-openrouter-key
    "Variable `chatgpt-shell-openrouter-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openrouter-key

or

(setq chatgpt-shell-openrouter-key \"my-key\")"))

(provide 'chatgpt-shell-openrouter)
;;; chatgpt-shell-openrouter.el ends here
