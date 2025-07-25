;;; chatgpt-shell-openrouter.el --- OpenRouter-specific logic -*- lexical-binding: t; -*-

;; Author: David J. Rosenbaum <djr7c4@gmail.com>
;; URL: https://github.com/xenodium/chatgpt-shell
;; Package-Requires: ((emacs "28.1") (shell-maker "0.72.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Adds OpenRouter specifics for `chatgpt-shell'.

;;; Code:

(declare-function chatgpt-shell-validate-no-system-prompt "chatgpt-shell")

(cl-defun chatgpt-shell-openrouter-make-model (&key label version short-version token-width context-window reasoning-effort validate-command other-params)
  "Create an OpenRouter model.

Set LABEL, VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW,
VALIDATE-COMMAND and OTHER-PARAMS for `chatgpt-shell-openai-make-model'."
  (chatgpt-shell-openai-make-model
   :label label
   :version version
   :short-version short-version
   :token-width token-width
   :context-window context-window
   :other-params other-params
   :validate-command #'chatgpt-shell-openrouter--validate-command
   :url-base 'chatgpt-shell-openrouter-api-url-base
   :path "/v1/chat/completions"
   :provider "OpenRouter"
   :reasoning-effort reasoning-effort
   :validate-command validate-command
   :key #'chatgpt-shell-openrouter-key
   :headers #'chatgpt-shell-openrouter--make-headers
   :handler #'chatgpt-shell-openrouter--handle-command
   :filter #'chatgpt-shell-openrouter--filter-output
   :icon "openrouter.png"))

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
         :other-params '((provider (quantizations . ["bf16"])
                                   (require_parameters . t))))
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
         :other-params '((provider (quantizations . ["bf16"])
                                   (require_parameters . t))))
        (chatgpt-shell-openrouter-make-model
         :version "openai/o3-mini"
         :short-version "o3-mini"
         :label "ChatGPT"
         :token-width 3
         ;; See https://openrouter.ai/openai/o1-2024-12-17
         :context-window 200000
         :reasoning-effort chatgpt-shell-openai-reasoning-effort
         :validate-command #'chatgpt-shell-validate-no-system-prompt
         :other-params '((provider (require_parameters . t))))
        (chatgpt-shell-openrouter-make-model
         :version "openai/o1"
         :short-version "o1"
         :label "ChatGPT"
         :token-width 3
         ;; See https://openrouter.ai/openai/o1-2024-12-17
         :context-window 200000
         ;; Do not enable :reasoning-effort. Not supported by this model.
         :reasoning-effort nil
         :validate-command #'chatgpt-shell-validate-no-system-prompt
         :other-params '((provider (require_parameters . t))))
        (chatgpt-shell-openrouter-make-model
         :version "qwen/qwen-2.5-coder-32b-instruct"
         :short-version "qwen-2.5-coder-32b"
         :label "Qwen"
         :token-width 16
         ;; See https://openrouter.ai/qwen/qwen-2.5-coder-32b-instruct
         :context-window 33000
         ;; Multiple quantizations are offered for this model by different
         ;; providers so we restrict to one for consistency. Note that the sense
         ;; in which provider is used here means the providers available through
         ;; OpenRouter. This is different from the meaning of the :provider
         ;; argument.
         ;;
         ;; See https://openrouter.ai/qwen/qwen-2.5-coder-32b-instruct
         :other-params '((provider (quantizations . ["bf16"]))))
        (chatgpt-shell-openrouter-make-model
         :version "anthropic/claude-3.7-sonnet"
         :short-version "claude-3.7-sonnet"
         :label "Claude"
         :token-width 4
         ;; See https://openrouter.ai/anthropic/claude-3.7-sonnet
         :context-window 200000)))

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

(cl-defun chatgpt-shell-openrouter--handle-command (&key model command context shell settings)
  "Handle ChatGPT COMMAND (prompt) using ARGS, MODEL, CONTEXT, SHELL, and SETTINGS."
  (chatgpt-shell-openai--handle-chatgpt-command
   :model model
   :command command
   :context context
   :shell shell
   :settings settings
   :key #'chatgpt-shell-openrouter-key
   :filter #'chatgpt-shell-openrouter--filter-output
   :missing-key-msg "Your chatgpt-shell-openrouter-key is missing"))


(defun chatgpt-shell-openrouter--filter-output (raw-response)
  "Filter RAW-RESPONSE when processing responses are sent."
  (let ((pending (if (and (listp raw-response) (plist-member raw-response :pending))
                     (plist-get raw-response :pending)
                   ""))
        (input (if (stringp raw-response) raw-response ""))
        (result ""))

    (setq pending (concat pending input))

    ;; Extract all content from complete data lines
    (with-temp-buffer
      (insert pending)
      (goto-char (point-min))
      (let ((new-pending ""))
        (while (not (eobp))
          (if (looking-at "^data: \\({.*}\\)$")
              (let ((json-str (match-string 1)))
                (condition-case nil
                    (let* ((json (json-read-from-string json-str))
                           (content (cdr (assoc-string "content"
                                                      (cdr (assoc-string "delta"
                                                                        (elt (cdr (assoc-string "choices" json)) 0)))))))
                      (when content
                        (setq result (concat result content))))
                  (error nil))
                (forward-line))
            (if (looking-at "^:")
                ;; Skip comment lines like ": OPENROUTER PROCESSING"
                (forward-line)
              ;; Keep incomplete lines as pending
              (setq new-pending (concat new-pending (buffer-substring (point) (line-end-position)) "\n"))
              (forward-line))))
        (setq pending new-pending)))

    (list (cons :filtered result)
          (cons :pending pending))))

(defun chatgpt-shell-openrouter--make-headers (&rest args)
  "Create the API headers.

ARGS are the same as for `chatgpt-shell-openai--make-headers'."
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
