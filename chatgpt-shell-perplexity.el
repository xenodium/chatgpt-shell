;;; chatgpt-shell-perplexity.el --- Perplexity-specific logic  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Package-Requires: ((emacs "29.1") (shell-maker "0.72.1"))

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

;; Adds Perplexity specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'let-alist)
(require 'shell-maker)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar chatgpt-shell-proxy)
(declare-function chatgpt-shell-openai--make-payload "chatgpt-shell-openai")

(cl-defun chatgpt-shell-perplexity-make-model (&key version
                                                    short-version
                                                    token-width
                                                    context-window)
  "Create an Perplexity model.

Set VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW and
VALIDATE-COMMAND handler."
  (unless version
    (error "Missing mandatory :version param"))
  (unless token-width
    (error "Missing mandatory :token-width param for %s" version))
  (unless context-window
    (error "Missing mandatory :context-window param for %s" version))
  (unless (integerp token-width)
    (error ":token-width must be an integer"))
  (unless (integerp context-window)
    (error ":context-window must be an integer"))
  `((:provider . "Perplexity")
    (:label . "Perplexity")
    (:version . ,version)
    (:short-version . ,short-version)
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:headers . chatgpt-shell-perplexity--make-headers)
    (:handler . chatgpt-shell-perplexity--handle-perplexity-command)
    (:filter . chatgpt-shell-perplexity--extract-perplexity-response)
    (:payload . chatgpt-shell-openai--make-payload)
    (:url . chatgpt-shell-perplexity--make-url)
    (:validate-command . chatgpt-shell-perplexity--validate-command)
    (:icon . "perplexity-color.png")))

(defcustom chatgpt-shell-perplexity-key nil
  "Perplexity API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-perplexity-api-url-base "https://api.perplexity.ai"
  "Perplexity API's base URL.

API url = base + path.

If you use Perplexity through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defun chatgpt-shell-perplexity-models ()
  "Build a list of Perplexity LLM models available."
  (list (chatgpt-shell-perplexity-make-model
         :version "sonar"
         :short-version "sonar"
         ;; TODO: Find a reference.
         :token-width 4
         ;; https://openrouter.ai/perplexity/llama-3.1-sonar-small-128k-online
         :context-window 127072)))

(defun chatgpt-shell-perplexity-key ()
  "Get the perplexity API key."
  (cond ((stringp chatgpt-shell-perplexity-key)
         chatgpt-shell-perplexity-key)
        ((functionp chatgpt-shell-perplexity-key)
         (condition-case _err
             (funcall chatgpt-shell-perplexity-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-perplexity--make-headers (&key _model _settings)
  "Create the API headers."
  (unless (chatgpt-shell-perplexity-key)
    (error "Your chatgpt-shell-perplexity-key is missing"))
  (list "Content-Type: application/json; charset=utf-8"
        (format "Authorization: Bearer %s" (chatgpt-shell-perplexity-key))))

(cl-defun chatgpt-shell-perplexity--handle-perplexity-command (&key model command context shell settings)
  "Handle Perplexity shell COMMAND (prompt).

Uses MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-perplexity--make-url :model model
                                            :settings settings)
   :proxy chatgpt-shell-proxy
   :data (chatgpt-shell-openai--make-payload :model model
                                             :context
                                             (append
                                              context
                                              (list (cons command nil)))
                                             :settings settings)
   :headers (chatgpt-shell-perplexity--make-headers)
   :filter #'chatgpt-shell-perplexity--extract-perplexity-response
   :shell shell))

(cl-defun chatgpt-shell-perplexity--make-url (&key _command _model _settings)
  "Create the API URL using MODEL and SETTINGS."
  (concat chatgpt-shell-perplexity-api-url-base
          "/chat/completions"))

(defun chatgpt-shell-perplexity--extract-perplexity-response (output)
  "Extract Perplexity response from OUTPUT.

When Perplexity responses are streamed, they arrive in the form:

  data: {...json...}
  data: {...jdon...}

Otherwise:

  {...json...}."
  (when (stringp output)
    (error "Please upgrade shell-maker to 0.79.1 or newer"))
  ;; Non-streamed
  (if-let* ((whole (shell-maker--json-parse-string (map-elt output :pending)))
            (response (or (let-alist whole
                            .error.message)
                          (let-alist whole
                            (mapconcat (lambda (choice)
                                         (let-alist choice
                                           (cond ((not (string-empty-p .delta.content))
                                                  .delta.content)
                                                 ((not (string-empty-p .message.content))
                                                  .message.content)
                                                 (t
                                                  ""))))
                                       .choices "")))))
      (let ((citations (let-alist whole
                         .citations)))
        (chatgpt-shell-perplexity--expand-citations
         response citations))
    ;; Streamed
    (when-let ((chunks (shell-maker--split-text (map-elt output :pending))))
      (let ((response)
            (pending)
            (result))
        (mapc (lambda (chunk)
                (let ((citations))
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
                                                .choices "")))))
                      (progn
                        (setq citations (let-alist obj
                                          .citations))
                        (unless (string-empty-p text)
                          (setq response (concat response
                                                 (chatgpt-shell-perplexity--expand-citations
                                                  text citations)))))
                    (setq pending (concat pending
                                          (or (map-elt chunk :key) "")
                                          (map-elt chunk :value))))))
              chunks)
        (setq result
              (list (cons :filtered (unless (string-empty-p response)
                                      response))
                    (cons :pending pending)))
        result))))

(defun chatgpt-shell-perplexity--expand-citations (text citations)
  "Expand citation placeholders in TEXT using CITATIONS."
  (if (and text citations)
      (replace-regexp-in-string
       "\\[\\([0-9]+\\)\\]"
       (lambda (match)
         (let* ((index (string-to-number (match-string 1 match)))
                (url (nth (1- index)
                          ;; Append converts arrays to lists.
                          (append citations nil))))
           (when url
             (format " [%d](%s)" index url))))
       text)
    text))

(defun chatgpt-shell-perplexity--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-perplexity-key
    "Variable `chatgpt-shell-perplexity-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-perplexity-key

or

(setq chatgpt-shell-perplexity-key \"my-key\")"))

(provide 'chatgpt-shell-perplexity)

;;; chatgpt-shell-perplexity.el ends here
