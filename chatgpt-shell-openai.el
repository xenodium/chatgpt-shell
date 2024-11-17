;;; chatgpt-shell.el --- ChatGPT shell + buffer insert commands  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 1.23.1
;; Package-Requires: ((emacs "28.1") (shell-maker "0.62.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds OpenAI specifics for `chatgpt-shell'.

;;; Code:

(defcustom chatgpt-shell-openai-models
  '(((:version . "chatgpt-4o-latest")
     (:short-version . "4o-latest")
     (:label . "ChatGPT")
     (:provider . "OpenAI")
     (:handler . chatgpt-shell-openai--handle-chatgpt-command)
     (:filter . chatgpt-shell-openai--filter-output)
     (:payload . chatgpt-shell-openai--make-payload)
     (:headers . chatgpt-shell-openai--make-headers)
     (:url . chatgpt-shell-openai--make-url)
     (:key . chatgpt-shell-openai-key)
     (:path . "/v1/chat/completions")
     (:url-base . chatgpt-shell-api-url-base)
     (:token-width . 3)
     ;; https://platform.openai.com/docs/models/gpt-4o
     (:context-window . 128000)))
  "List of OpenAI LLM models available."
  :type '(alist :key-type (symbol :tag "Attribute") :value-type (sexp))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-api-url-base "https://api.openai.com"
  "OpenAI API's base URL.

API url = base + path.

If you use ChatGPT through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(cl-defun chatgpt-shell-openai--make-chatgpt-messages (&key model system-prompt prompt prompt-url context)
  "Create ChatGPT messages using MODEL.

SYSTEM-PROMPT: string.

PROMPT: string.

PROMPT-URL: string.

CONTEXT: Excludes PROMPT."
  (when prompt-url
    (setq prompt-url (chatgpt-shell--make-chatgpt-url prompt-url)))
  (vconcat
   (when system-prompt
     `(((role . "system")
        (content . ,system-prompt))))
   (when context
     (chatgpt-shell-openai--user-assistant-messages
      (if model
          (chatgpt-shell-crop-context
           :model model
           :command prompt
           :context context)
        context)))
   `(((role . "user")
      (content . ,(vconcat
                   (append
                    (when prompt
                      `(((type . "text")
                         (text . ,prompt))))
                    (when prompt-url
                      `(((type . "image_url")
                         (image_url . ,prompt-url)))))))))))

(defun chatgpt-shell-openai-key ()
  "Get the ChatGPT key."
  (cond ((stringp chatgpt-shell-openai-key)
         chatgpt-shell-openai-key)
        ((functionp chatgpt-shell-openai-key)
         (condition-case _err
             (funcall chatgpt-shell-openai-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-openai-make-chatgpt-request-data (&key system-prompt prompt prompt-url context version temperature streaming other-params)
  "Make request data with MESSAGES.

Optionally set PROMPT, VERSION, TEMPERATURE, STREAMING, SYSTEM-PROMPT,
and OTHER-PARAMS (list)."
  (unless version
    (error "Missing mandatory :version param"))
  (append
   `((model . ,version)
     (messages . ,(vconcat (chatgpt-shell-openai--make-chatgpt-messages
                            :system-prompt system-prompt
                            :prompt prompt
                            :prompt-url prompt-url
                            :context context))))
   (when temperature
     `((temperature . ,temperature)))
   (when streaming
     `((stream . t)))
   other-params))

(defun chatgpt-shell-openai--filter-output (raw-response)
  "Extract ChatGPT response from RAW-RESPONSE.

When ChatGPT responses are streamed, they arrive in the form:

  data: {...json...}
  data: {...jdon...}

Otherwise:

  {...json...}."
  (if-let* ((whole (shell-maker--json-parse-string raw-response))
            (response (or (let-alist whole
                            .error.message)
                          (let-alist whole
                            (mapconcat (lambda (choice)
                                         (let-alist choice
                                           (or .delta.content
                                               .message.content)))
                                       .choices)))))
      response
    (when-let ((chunks (shell-maker--split-text raw-response)))
      (let ((response)
            (pending)
            (result))
        (mapc (lambda (chunk)
                ;; Response chunks come in the form:
                ;;   data: {...}
                ;;   data: {...}
                (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                          (obj (shell-maker--json-parse-string (map-elt chunk :value)))
                          (text (or
                                 ;; .choices[i].message.content
                                 ;; .choices[i].delta.content
                                 (let-alist obj
                                   (mapconcat (lambda (choice)
                                                (let-alist choice
                                                  (or .delta.content
                                                      .message.content)))
                                              .choices)))))
                    (unless (string-empty-p text)
                      (setq response (concat response text)))
                  (setq pending (concat pending
                                        (or (map-elt chunk :key) "")
                                        (map-elt chunk :value)))))
              chunks)
        (setq result
              (list (cons :filtered (unless (string-empty-p response)
                                      response))
                    (cons :pending pending)))
        result))))

(cl-defun chatgpt-shell-openai--make-url (&key model _settings)
  "Create the API URL using MODEL."
  (concat (symbol-value (or (map-elt model :url-base)
                            (error "Model :url-base not found")))
          (or (map-elt model :path)
              (error "Model :path not found"))))

(cl-defun chatgpt-shell-openai--make-headers (&key _model _settings)
  "Create the API headers."
  (list "Content-Type: application/json; charset=utf-8"
        (format "Authorization: Bearer %s" (chatgpt-shell-openai-key))))

(cl-defun chatgpt-shell-openai--make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (chatgpt-shell-openai-make-chatgpt-request-data
   :system-prompt (map-elt settings :system-prompt)
   :context context
   :version (map-elt model :version)
   :temperature (map-elt settings :temperature)
   :streaming (map-elt settings :streaming)))

(cl-defun chatgpt-shell-openai--handle-chatgpt-command (&key model command context shell settings)
  "Handle ChatGPT COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (concat chatgpt-shell-api-url-base
                (or (map-elt model :path)
                    (error "Model :path not found")))
   :data (chatgpt-shell-openai-make-chatgpt-request-data
          :prompt command
          :system-prompt (map-elt settings :system-prompt)
          :context context
          :version (map-elt model :version)
          :temperature (map-elt settings :temperature)
          :streaming (map-elt settings :streaming))
   :headers (list "Content-Type: application/json; charset=utf-8"
                  (format "Authorization: Bearer %s" (chatgpt-shell-openai-key)))
   :filter #'chatgpt-shell-openai--filter-output
   :shell shell))

(defun chatgpt-shell-openai--user-assistant-messages (history)
  "Convert HISTORY to ChatGPT format.

Sequence must be a vector for json serialization.

For example:

 [
   ((role . \"user\") (content . \"hello\"))
   ((role . \"assistant\") (content . \"world\"))
 ]"
  (let ((result))
    (mapc
     (lambda (item)
       (when (car item)
         (push (list (cons 'role "user")
                     (cons 'content (car item))) result))
       (when (cdr item)
         (push (list (cons 'role "assistant")
                     (cons 'content (cdr item))) result)))
     history)
    (nreverse result)))

(provide 'chatgpt-shell-openai)

;;; chatgpt-shell-openai.el ends here
