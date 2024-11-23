;;; chatgpt-shell-anthropic.el --- Anthropic-specific logic  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

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

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds Anthropic specifics for `chatgpt-shell'.

;;; Code:

(defcustom chatgpt-shell-anthropic-key nil
  "Anthropic API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-anthropic-api-url-base "https://api.anthropic.com"
  "Anthropic API's base URL.

API url = base + path.

If you use Claude through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(cl-defun chatgpt-shell-anthropic--make-model (&key version
                                                    short-version
                                                    token-width
                                                    max-tokens
                                                    context-window)
  "Create an Anthropic model configuration.

VERSION: Mandatory. The version of the model as a string.
SHORT-VERSION: Optional. A shortened version identifier as a string.
TOKEN-WIDTH: Mandatory. Approximate token width (in chars) limit as integer.
MAX-TOKENS: Mandatory. The maximum number of tokens to generate before stopping.
CONTEXT-WINDOW: Mandatory. The context window size as an integer."
  (unless version
    (error "Missing mandatory :version param"))
  (unless token-width
    (error "Missing mandatory :token-width param"))
  (unless max-tokens
    (error "Missing mandatory :max-tokens param"))
  (unless context-window
    (error "Missing mandatory :context-window param"))
  (unless (integerp token-width)
    (error ":token-width must be an integer"))
  (unless (integerp context-window)
    (error ":context-window must be an integer"))
  `((:provider . "Anthropic")
    (:label . "Claude")
    (:path . "/v1/messages")
    (:version . ,version)
    (:max-tokens . ,max-tokens)
    (:short-version . ,short-version)
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:handler . chatgpt-shell-anthropic--handle-claude-command)
    (:filter . chatgpt-shell-anthropic--extract-claude-response)
    (:payload . chatgpt-shell-anthropic--make-payload)
    (:url . chatgpt-shell-anthropic--make-url)
    (:headers . chatgpt-shell-anthropic--make-headers)
    (:url-base . chatgpt-shell-anthropic-api-url-base)
    (:key . chatgpt-shell-anthropic-key)
    (:validate-command . chatgpt-shell-anthropic--validate-command)))

(defun chatgpt-shell-anthropic-models ()
  "Build a list of Anthropic LLM models available."
  (list
   ;; https://docs.anthropic.com/en/docs/about-claude/models#model-comparison-table
   ;; A token is equivalent to _about_ 4 characters.
   (chatgpt-shell-anthropic--make-model :version "claude-3-5-sonnet-20240620"
                                        :short-version "3-5-sonnet-20240620"
                                        :token-width  4
                                        :max-tokens 8192
                                        :context-window 200000)))

(cl-defun chatgpt-shell-anthropic--make-url (&key model _settings)
  "Create the API URL using MODEL and SETTINGS."
  (concat (symbol-value (or (map-elt model :url-base)
                            (error "Model :url-base not found")))
          (or (map-elt model :path)
              (error "Model :path not found"))))

(defun chatgpt-shell-anthropic--validate-command (_command)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-anthropic-key
    "Variable `chatgpt-shell-anthropic-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-anthropic-key

or

(setq chatgpt-shell-anthropic-key \"my-key\")"))

(defun chatgpt-shell-anthropic-key ()
  "Get the Anthropic API key."
  (cond ((stringp chatgpt-shell-anthropic-key)
         chatgpt-shell-anthropic-key)
        ((functionp chatgpt-shell-anthropic-key)
         (condition-case _err
             (funcall chatgpt-shell-anthropic-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-anthropic--make-headers (&key _model _settings)
  "Create the API headers."
  (unless (chatgpt-shell-anthropic-key)
    (error "Your chatgpt-shell-anthropic-key is missing"))
  (list "Content-Type: application/json; charset=utf-8"
        (concat "x-api-key: " (chatgpt-shell-anthropic-key))
        "anthropic-version: 2023-06-01"))

(cl-defun chatgpt-shell-anthropic--make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (let ((context (mapcan (lambda (l)
                           (when (cdr l)
                             `(((role . "user")
                                (content . ,(car l)))
                               ((role . "assistant")
                                (content . ,(cdr l))))))
                         context))
        (command `(((role . "user")
                    (content . ,(caar (last context)))))))
    (append
     (when (map-elt settings :system-prompt)
       `((system . ,(map-elt settings :system-prompt))))
     `((max_tokens . ,(or (map-elt model :max-tokens)
                          (error "Missing %s :max-tokens" (map-elt model :name))))
       (model . ,(map-elt model :version))
       (stream . ,(if (map-elt settings :streaming) 't :false))
       (messages . ,(vconcat
                     (append
                      context
                      command)))))))

(cl-defun chatgpt-shell-anthropic--handle-claude-command (&key model command context shell settings)
  "Handle Claude shell COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-anthropic--make-url :model model
                                           :settings settings)
   :data (chatgpt-shell-anthropic--make-payload :model model
                                                :context
                                                (append
                                                 context
                                                 (list (cons command nil)))
                                                :settings settings)
   :headers (chatgpt-shell-anthropic--make-headers)
   :filter #'chatgpt-shell-anthropic--extract-claude-response
   :shell shell))

(defun chatgpt-shell-anthropic--extract-claude-response (raw-response)
  "Extract Claude response from RAW-RESPONSE."
  (if-let* ((whole (shell-maker--json-parse-string raw-response))
            (response (or (let-alist whole
                            .error.message)
                          (let-alist whole
                            (mapconcat (lambda (content)
                                         (let-alist content
                                           .text))
                                       .content)))))
      response
    (if-let ((chunks (shell-maker--split-text raw-response)))
      (let ((response)
            (pending)
            (result))
        (mapc (lambda (chunk)
                ;; Response chunks come in the form:
                ;; event: message_start
                ;; data: {...}
                ;; event: content_block_start
                ;; data: {...}
                (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                          (obj (shell-maker--json-parse-string (map-elt chunk :value)))
                          (text (let-alist obj
                                  (or .text
                                      .content_block.text
                                      .delta.text
                                      .error.message
                                      ""))))
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
        result)
      (list (cons :filtered nil)
            (cons :pending raw-response)))))

(provide 'chatgpt-shell-anthropic)

;;; chatgpt-shell-anthropic.el ends here
