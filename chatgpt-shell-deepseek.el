;;; chatgpt-shell-deepseek.el --- Deepseek-specific logic -*- lexical-binding: t; -*-

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Adds DeepSeek specifics for `chatgpt-shell'.

;;; Code:

(declare-function chatgpt-shell-validate-no-system-prompt "chatgpt-shell")

(cl-defun chatgpt-shell-deepseek-make-model (&key label version short-version token-width context-window validate-command other-params)
  "Create an DeepSeek model.

Set LABEL, VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW,
VALIDATE-COMMAND and OTHER-PARAMS for `chatgpt-shell-openai-make-model'."
  (chatgpt-shell-openai-make-model
   :label label
   :version version
   :short-version short-version
   :token-width token-width
   :context-window context-window
   :other-params other-params
   :validate-command #'chatgpt-shell-deepseek--validate-command
   :url-base 'chatgpt-shell-deepseek-api-url-base
   :path "/chat/completions"
   :provider "DeepSeek"
   :validate-command validate-command
   :key #'chatgpt-shell-deepseek-key
   :headers #'chatgpt-shell-deepseek--make-headers
   :handler #'chatgpt-shell-deepseek--handle-command
   :filter #'chatgpt-shell-deepseek--filter-output
   :icon "deepseek-color.png"))

(defun chatgpt-shell-deepseek-models ()
  "Build a list of DeepSeek LLM models."
  (list (chatgpt-shell-deepseek-make-model
         :version "deepseek-reasoner"
         :short-version "reasoner"
         :label "DeepSeek"
         ;; See https://api-docs.deepseek.com/quick_start/token_usage
         :token-width 3
         ;; See https://api-docs.deepseek.com/quick_start/pricing.
         :context-window 65536)
        (chatgpt-shell-deepseek-make-model
         :version "deepseek-chat"
         :short-version "chat"
         :label "DeepSeek"
         ;; See https://api-docs.deepseek.com/quick_start/token_usage
         :token-width 3
         ;; See https://api-docs.deepseek.com/quick_start/pricing.
         :context-window 65536)))

(defcustom chatgpt-shell-deepseek-api-url-base "https://api.deepseek.com"
  "DeepSeek API's base URL.

API url = base + path.

If you use DeepSeek through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-deepseek-key nil
  "DeepSeek key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defun chatgpt-shell-deepseek-key ()
  "Get the DeepSeek key."
  (cond ((stringp chatgpt-shell-deepseek-key)
         chatgpt-shell-deepseek-key)
        ((functionp chatgpt-shell-deepseek-key)
         (condition-case _err
             (funcall chatgpt-shell-deepseek-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-deepseek--handle-command (&key model command context shell settings)
  "Handle ChatGPT COMMAND (prompt) using ARGS, MODEL, CONTEXT, SHELL, and SETTINGS."
  (chatgpt-shell-openai--handle-chatgpt-command
   :model model
   :command command
   :context context
   :shell shell
   :settings settings
   :key #'chatgpt-shell-deepseek-key
   :filter #'chatgpt-shell-deepseek--filter-output
   :missing-key-msg "Your chatgpt-shell-deepseek-key is missing"))

(defun chatgpt-shell-deepseek--filter-output (object)
  "Process OBJECT to extract response output."
  (chatgpt-shell-openai--filter-output object))

(defun chatgpt-shell-deepseek--make-headers (&rest args)
  "Create the API headers.

ARGS are the same as for `chatgpt-shell-openai--make-headers'."
  (apply #'chatgpt-shell-openai--make-headers
         :key #'chatgpt-shell-deepseek-key
         args))

(defun chatgpt-shell-deepseek--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-deepseek-key
    "Variable `chatgpt-shell-deepseek-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-deepseek-key

or

(setq chatgpt-shell-deepseek-key \"my-key\")"))

(provide 'chatgpt-shell-deepseek)
;;; chatgpt-shell-deepseek.el ends here
