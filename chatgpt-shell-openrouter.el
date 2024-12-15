;;; chatgpt-shell-openrouter.el --- OpenRouter-specific logic -*- lexical-binding: t; -*-

(cl-defun chatgpt-shell-openrouter-make-model (&rest args &key version short-version token-width context-window validate-command other-params)
  (apply #'chatgpt-shell-openai-make-model
         :provider "OpenRouter"
         :key #'chatgpt-shell-openrouter-key
         args))

(defun chatgpt-shell-openrouter-models ()
  "Build a list of OpenRouter LLM models."
  (list (chatgpt-shell-openrouter-make-model
         :version "meta-llama/llama-3.3-70b-instruct"
         :short-version "llama-3.3-70b"
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
         :version "qwen/qwen-2.5-coder-32b-instruct"
         :short-version "qwen-2.5-coder-32b"
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

(provide 'chatgpt-shell-openrouter)
;;; chatgpt-shell-openrouter.el ends here
