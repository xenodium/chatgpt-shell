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

(defcustom chatgpt-shell-api-url-base "https://api.openai.com"
  "OpenAI API's base URL.

`chatgpt-shell--chatgpt-api-url' =
   `chatgpt-shell--chatgpt-api-url-base' + `chatgpt-shell--chatgpt-api-url-path'

If you use ChatGPT through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-api-url-path "/v1/chat/completions"
  "OpenAI API's URL path.

`chatgpt-shell--chatgpt-api-url' =
   `chatgpt-shell--chatgpt-api-url-base' + `chatgpt-shell--chatgpt-api-url-path'"
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

;; TODO: Inline in chatgpt-shell--handle-chatgpt-command and remove?
(defcustom chatgpt-shell-auth-header
  (lambda ()
    (format "Authorization: Bearer %s" (chatgpt-shell-openai-key)))
  "Function to generate the request's `Authorization' header string."
  :type '(function :tag "Function")
  :group 'chatgpt-shell)

(cl-defun chatgpt-shell--make-chatgpt-payload (&key prompt context version temperature streaming system-prompt)
  "Create a ChatGPT request payload.

PROMPT: The new prompt (should not be in CONTEXT).
VERSION: The model version.
CONTEXT: All previous interactions.
TEMPERATURE: Model temperature.
STREAMING: When non-nil, request streamed response.
SYSTEM-PROMPT: Optional system prompt."
  (chatgpt-shell-make-chatgpt-request-data
   :messages (vconcat
              (when system-prompt
                `(((role . "system")
                   (content . ,system-prompt))))
              (chatgpt-shell--user-assistant-messages context)
              (when prompt
                `(((role . "user")
                   (content . ,prompt)))))
   :version version
   :temperature temperature
   :streaming streaming))

(cl-defun chatgpt-shell--make-chatgpt-messages (&key model system-prompt prompt prompt-url context)
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
     (chatgpt-shell--user-assistant-messages
      (chatgpt-shell-crop-context
       :model model
       :command prompt
       :context context)))
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

(cl-defun chatgpt-shell-make-chatgpt-request-data (&key messages version temperature streaming other-params)
  "Make request data with MESSAGES.

Optionally set VERSION, TEMPERATURE, STREAMING, and OTHER-PARAMS (list)."
  (unless messages
    (error "Missing mandatory :messages param"))
  (append
   `((model . ,version)
     (messages . ,(vconcat messages)))
   (when temperature
     `((temperature . ,temperature)))
   (when streaming
     `((stream . t)))
   other-params))

(defun chatgpt-shell-filter-chatgpt-output (raw-response)
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
    (when-let ((chunks (chatgpt-shell--split-response raw-response)))
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

(cl-defun chatgpt-shell--handle-chatgpt-command (&key model command context shell settings)
  "Handle ChatGPT COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (concat chatgpt-shell-api-url-base
                (or (map-elt model :path)
                    (error "Model :path not found")))
   :data (chatgpt-shell--make-chatgpt-payload
          :prompt command
          :version (map-elt model :name)
          :context context
          :streaming (map-elt settings :streaming)
          :temperature (map-elt settings :temperature)
          :system-prompt (map-elt settings :system-prompt))
   :headers (list "Content-Type: application/json; charset=utf-8"
                  (funcall chatgpt-shell-auth-header))
   :filter #'chatgpt-shell-filter-chatgpt-output
   :shell shell))

(defun chatgpt-shell--user-assistant-messages (history)
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
