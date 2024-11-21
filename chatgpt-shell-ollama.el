;;; chatgpt-shell-ollama.el --- Ollama-specific logic  -*- lexical-binding: t -*-

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

;; Adds Ollama specifics for `chatgpt-shell'.

;;; Code:

(defun chatgpt-shell-ollama-models ()
  "Build a list of Ollama LLM models available."
  '(((:provider . "Ollama")
     (:label . "Ollama")
     (:version . "llama3.2")
     (:short-version . "llama3.2")
     (:token-width . 3)
     (:context-window . 131072)
     (:handler . chatgpt-shell-ollama--handle-ollama-command)
     (:filter . chatgpt-shell-ollama--extract-ollama-response)
     (:payload . chatgpt-shell-ollama-make-payload)
     (:url . chatgpt-shell-ollama--make-url))))

(cl-defun chatgpt-shell-ollama--handle-ollama-command (&key model command context shell settings)
  "Handle Ollama shell COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-ollama--make-url :model model
                                           :settings settings)
   :data (chatgpt-shell-ollama-make-payload :model model
                                                 :context
                                                 (append
                                                  context
                                                  (list (cons command nil)))
                                                :settings settings)
   :filter #'chatgpt-shell-ollama--extract-ollama-response
   :shell shell))

(cl-defun chatgpt-shell-ollama--make-url (&key _model _settings)
  "Create the API URL using MODEL and SETTINGS."
  ;; TODO: Make configurable.
  "http://localhost:11434/api/chat")

(defun chatgpt-shell-ollama--extract-ollama-response (raw-response)
  "Extract Claude response from RAW-RESPONSE."
  (if-let* ((whole (shell-maker--json-parse-string raw-response))
            (response (let-alist whole
                        .response)))
      response
    (if-let ((chunks (string-split raw-response "\n")))
        (let ((response))
          (mapc (lambda (chunk)
                  (let-alist (shell-maker--json-parse-string chunk)
                    (unless (string-empty-p .message.content)
                      (setq response (concat response .message.content)))))
                chunks)
          (or response raw-response))
      raw-response)))

(cl-defun chatgpt-shell-ollama-make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (unless (map-elt model :version)
    (error "Missing mandatory :version param"))
  (append
   `((model . ,(map-elt model :version))
     (messages . ,(vconcat (chatgpt-shell-ollama--make-messages
                            :system-prompt (map-elt settings :system-prompt)
                            :context context))))
   (when (map-elt settings :temperature)
     `((temperature . ,(map-elt settings :temperature))))
   (when (map-elt settings :streaming)
     `((stream . t)))))

(cl-defun chatgpt-shell-ollama--make-messages (&key model system-prompt prompt prompt-url context)
  "Create messages using MODEL.

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
   (when (or prompt
             prompt-url)
     `(((role . "user")
        (content . ,(vconcat
                     (append
                      (when prompt
                        `(((type . "text")
                           (text . ,prompt))))
                      (when prompt-url
                        `(((type . "image_url")
                           (image_url . ,prompt-url))))))))))))

(provide 'chatgpt-shell-ollama)

;;; chatgpt-shell-ollama.el ends here
