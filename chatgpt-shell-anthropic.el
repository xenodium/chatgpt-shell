;;; chatgpt-shell-anthropic.el --- Anthropic-specific logic  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
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

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds Anthropic specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'map)

(defvar chatgpt-shell-proxy)
(declare-function chatgpt-shell--unsorted-collection "chatgpt-shell")
(declare-function chatgpt-shell-previous-source-block "chatgpt-shell")

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

(defcustom chatgpt-shell-anthropic-thinking nil
  "When non-nil enable model thinking if available."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-anthropic-thinking-budget-tokens nil
  "The token budget allocated for Anthropic model thinking.

nil means to use the maximum number of thinking tokens allowed."
  :type '(choice integer (const nil))
  :group 'chatgpt-shell)

(defun chatgpt-shell-anthropic-reasoning-effort-selector (model)
  "Select the reasoning effort for the Anthropic MODEL."
  (let* ((min (map-elt model :thinking-budget-min))
         (max (1- (map-elt model :max-tokens)))
         (response (completing-read (format "Thinking budget tokens (%d-%d): " min max)
                                    (chatgpt-shell--unsorted-collection
                                     '("disable" "max"))))
         (budget (cond
                  ((equal response "disable")
                   0)
                  ((equal response "max")
                   nil)
                  (t
                   (string-to-number response)))))
    (unless (or (not budget)
                (eql budget 0)
                (and (integerp budget) (<= min budget max)))
      (user-error "Thinking budget tokens must be in the range %d-%d" min max))
    `(((:symbol . chatgpt-shell-anthropic-thinking-budget-tokens)
       (:value . ,(if (eql budget 0)
                      chatgpt-shell-anthropic-thinking-budget-tokens
                    budget))
       (:kind . thinking-budget)
       (:max .  ,(null budget)))
      ((:symbol . chatgpt-shell-anthropic-thinking)
       (:value . ,(not (eql budget 0)))
       (:kind . thinking-toggle)))))

(cl-defun chatgpt-shell-anthropic--make-model (&key version
                                                    short-version
                                                    token-width
                                                    max-tokens
                                                    context-window
                                                    thinking-budget-min
                                                    reasoning-effort-selector)
  "Create an Anthropic model.

Set VERSION, SHORT-VERSION, TOKEN-WIDTH, MAX-TOKENS,
CONTEXT-WINDOW, THINKING-BUDGET-MIN and
REASONING-EFFORT-SELECTOR."
  (unless version
    (error "Missing mandatory :version param"))
  (unless token-width
    (error "Missing mandatory :token-width param"))
  (unless max-tokens
    (error "Missing mandatory :max-tokens param"))
  (unless context-window
    (error "Missing mandatory :context-window param"))
  (unless token-width
    (error "Missing mandatory :token-width param for %s" version))
  (unless context-window
    (error "Missing mandatory :context-window param for %s" version))
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
    (:thinking-budget-min . ,thinking-budget-min)
    (:reasoning-effort-selector . ,reasoning-effort-selector)
    (:validate-command . chatgpt-shell-anthropic--validate-command)
    (:icon . "anthropic.png")))

(defun chatgpt-shell-anthropic-toggle-thinking ()
  "Toggle Anthropic model, as per `chatgpt-shell-anthropic-thinking'."
  (interactive)
  (setq chatgpt-shell-anthropic-thinking (not chatgpt-shell-anthropic-thinking))
  (message "Anthropic thinking %s"
           (if chatgpt-shell-anthropic-thinking
               "enabled"
             "disabled")))

(defun chatgpt-shell-anthropic-models ()
  "Build a list of Anthropic LLM models available."
  (list
   ;; https://docs.anthropic.com/en/docs/about-claude/models#model-comparison-table
   ;; A token is equivalent to _about_ 4 characters.
   ;;
   ;; claude-4-sonnet-latest and claude-4-sonnet-latest are not supported yet.
   (chatgpt-shell-anthropic--make-model :version "claude-opus-4-5"
                                        :short-version "opus-4.5"
                                        :token-width  4
                                        :thinking-budget-min 1024
                                        :reasoning-effort-selector #'chatgpt-shell-anthropic-reasoning-effort-selector
                                        :max-tokens 32000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-opus-4-1-20250805"
                                        :short-version "opus-4.1"
                                        :token-width  4
                                        :thinking-budget-min 1024
                                        :reasoning-effort-selector #'chatgpt-shell-anthropic-reasoning-effort-selector
                                        :max-tokens 32000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-opus-4-20250514"
                                        :short-version "opus-4"
                                        :token-width  4
                                        :thinking-budget-min 1024
                                        :reasoning-effort-selector #'chatgpt-shell-anthropic-reasoning-effort-selector
                                        :max-tokens 32000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-sonnet-4-5"
                                        :short-version "sonnet-4.5"
                                        :token-width  4
                                        :thinking-budget-min 1024
                                        :reasoning-effort-selector #'chatgpt-shell-anthropic-reasoning-effort-selector
                                        :max-tokens 64000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-sonnet-4-20250514"
                                        :short-version "sonnet-4"
                                        :token-width  4
                                        :thinking-budget-min 1024
                                        :reasoning-effort-selector #'chatgpt-shell-anthropic-reasoning-effort-selector
                                        :max-tokens 64000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-3-7-sonnet-latest"
                                        :short-version "3.7-sonnet"
                                        :token-width  4
                                        :thinking-budget-min 1024
                                        :reasoning-effort-selector #'chatgpt-shell-anthropic-reasoning-effort-selector
                                        :max-tokens 64000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-3-5-sonnet-latest"
                                        :short-version "3.5-sonnet"
                                        :token-width  4
                                        :max-tokens 8192
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-haiku-4-5-20251001"
                                        :short-version "haiku-4.5"
                                        :token-width  4
                                        :max-tokens 64000
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-3-5-haiku-latest"
                                        :short-version "3.5-haiku"
                                        :token-width  4
                                        :max-tokens 8192
                                        :context-window 200000)
   (chatgpt-shell-anthropic--make-model :version "claude-3-opus-latest"
                                        :short-version "3-opus"
                                        :token-width  4
                                        :max-tokens 4096
                                        :context-window 200000)))

(cl-defun chatgpt-shell-anthropic--make-url (&key _command model _settings)
  "Create the API URL using MODEL and SETTINGS."
  (concat (symbol-value (or (map-elt model :url-base)
                            (error "Model :url-base not found")))
          (or (map-elt model :path)
              (error "Model :path not found"))))

(defun chatgpt-shell-anthropic--validate-command (_command _model _settings)
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
        "anthropic-version: 2023-06-01"
        "anthropic-beta: output-128k-2025-02-19"))

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
     (when chatgpt-shell-anthropic-thinking
       (when (and chatgpt-shell-anthropic-thinking-budget-tokens
                  (>= chatgpt-shell-anthropic-thinking-budget-tokens (map-elt model :max-tokens)))
         (error "Error: chatgpt-shell-anthropic-thinking-budget-tokens must be smaller than %d"
                (map-elt model :max-tokens)))
       (let ((chatgpt-shell-anthropic-thinking-budget-tokens
              (if chatgpt-shell-anthropic-thinking-budget-tokens
                  chatgpt-shell-anthropic-thinking-budget-tokens
                (1- (map-elt model :max-tokens)))))
         `((thinking . ((type . "enabled")
                        (budget_tokens . ,chatgpt-shell-anthropic-thinking-budget-tokens))))))
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
   :proxy chatgpt-shell-proxy
   :data (chatgpt-shell-anthropic--make-payload :model model
                                                :context
                                                (append
                                                 context
                                                 (list (cons command nil)))
                                                :settings settings)
   :headers (chatgpt-shell-anthropic--make-headers)
   :filter #'chatgpt-shell-anthropic--extract-claude-response
   :shell shell))

(defun chatgpt-shell-anthropic--extract-claude-response (output)
  "Process pending OUTPUT to extract Claude response.

OUTPUT is always of the form:

  ((:function-calls . ...)
   (:pending . ...)
   (:filtered . ...))

and must be returned in the same form."
  (when (stringp output)
    (error "Please upgrade shell-maker to 0.79.1 or newer"))
  (if-let* ((whole (shell-maker--json-parse-string (map-elt output :pending)))
            (response (or (let-alist whole
                            .error.message)
                          (let-alist whole
                            (mapconcat (lambda (content)
                                         (let-alist content
                                           .text))
                                       .content "")))))
      response
    (if-let ((chunks (shell-maker--split-text (map-elt output :pending))))
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
      output)))

(provide 'chatgpt-shell-anthropic)

;;; chatgpt-shell-anthropic.el ends here
