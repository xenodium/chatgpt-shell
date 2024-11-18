;;; chatgpt-shell-google.el --- Google-specific logic  -*- lexical-binding: t -*-

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

;; Adds OpenAI specifics for `chatgpt-shell'.

;;; Code:

(defcustom chatgpt-shell-google-key nil
  "Google API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-google-api-url-base "https://generativelanguage.googleapis.com"
  "Google API's base URL.

API url = base + path.

If you use Gemini through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-google-models
  '(((:version . "gemini-1.5-pro-latest")
     (:short-version . "1.5-pro-latest")
     (:label . "Gemini")
     (:provider . "Google")
     (:handler . chatgpt-shell-google--handle-gemini-command)
     (:filter . chatgpt-shell-google--extract-gemini-response)
     (:payload . chatgpt-shell-google--make-payload)
     (:url . chatgpt-shell-google--make-url)
     (:headers . chatgpt-shell-google--make-headers)
     (:key . chatgpt-shell-google-key)
     (:path . "/v1beta/models/gemini-1.5-pro-latest")
     ;; https://ai.google.dev/gemini-api/docs/tokens?lang=python
     ;; A token is equivalent to _about_ 4 characters.
     (:url-base . chatgpt-shell-google-api-url-base)
     (:token-width . 4)
     (:context-window . 2097152)))
  "List of Google LLM models available."
  :type '(alist :key-type (symbol :tag "Attribute") :value-type (sexp))
  :group 'chatgpt-shell)

(defun chatgpt-shell-google-key ()
  "Get the Google API key."
  (cond ((stringp chatgpt-shell-google-key)
         chatgpt-shell-google-key)
        ((functionp chatgpt-shell-google-key)
         (condition-case _err
             (funcall chatgpt-shell-google-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-google--make-url (&key model settings)
  "Create the API URL using MODEL and SETTINGS."
  (unless model
    (error "Missing mandatory :model param"))
  (unless settings
    (error "Missing mandatory :settings param"))
  (concat chatgpt-shell-google-api-url-base
          (or (map-elt model :path)
              (error "Provider :path not found"))
          (if (map-elt settings :streaming)
              ":streamGenerateContent"
            ":generateContent")
          "?key="
          (or (chatgpt-shell-google-key)
              (error "Your chatgpt-shell-google-key is missing"))
          "&alt=sse")) ;; Needed or streaming doesn't work.

(cl-defun chatgpt-shell-google--make-headers (&key _model _settings)
  "Create the API headers."
  (list "Content-Type: application/json; charset=utf-8"))

(cl-defun chatgpt-shell-google--make-payload (&key _model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (chatgpt-shell-google--make-gemini-payload
   :context context
   :temperature (map-elt settings :temperature)
   :system-prompt (map-elt settings :system-prompt)))

(cl-defun chatgpt-shell-google--handle-gemini-command (&key model command context shell settings)
  "Handle ChatGPT COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-google--make-url :model model
                                        :settings settings)
   :data (chatgpt-shell-google--make-gemini-payload
          :prompt command
          :context context
          :temperature (map-elt settings :temperature)
          :system-prompt (map-elt settings :system-prompt))
   :headers (list "Content-Type: application/json; charset=utf-8")
   :filter #'chatgpt-shell-google--extract-gemini-response
   :shell shell))

(cl-defun chatgpt-shell-google--make-gemini-payload (&key prompt context system-prompt temperature)
  "Create the request payload.

 Compose using PROMPT, CONTEXT, SYSTEM-PROMPT and TEMPERATURE."
  (append
   (when system-prompt
     `((system_instruction . ((parts . ((text . ,system-prompt)))))))
   `((contents . ,(vconcat
                   (chatgpt-shell-google--gemini-user-model-messages
                    (append context
                            (when prompt
                              (list (cons prompt nil)))))))
     (generation_config . ((temperature . ,(or temperature 1))
                           ;; 1 is most diverse output.
                           (topP . 1))))))

(defun chatgpt-shell-google--gemini-user-model-messages (context)
  "Convert CONTEXT to gemini messages.

Sequence must be a vector for json serialization.

For example:

 [
   ((role . \"user\") (parts . ((text . \"hello\"))))
   ((role . \"model\") (parts . ((text . \"world\"))))
 ]"
  (let ((result))
    (mapc
     (lambda (item)
       (when (car item)
         (push (list (cons 'role "user")
                     (cons 'parts (vconcat ;; Vector for json
                                   (list (list (cons 'text (car item))))))) result))
       (when (cdr item)
         (push (list (cons 'role "model")
                     (cons 'parts (vconcat ;; Vector for json
                                   (list (list (cons 'text (cdr item))))))) result)))
     context)
    (nreverse result)))

(defun chatgpt-shell-google--extract-gemini-response (raw-response)
  "Extract Gemini response from RAW-RESPONSE."
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
    (if-let ((chunks (shell-maker--split-text raw-response)))
      (let ((response)
            (pending)
            (result))
        (mapc (lambda (chunk)
                ;; Response chunks come in the form:
                ;;   data: {...}
                ;;   data: {...}
                (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                          (obj (shell-maker--json-parse-string (map-elt chunk :value)))
                          (text (let-alist obj
                                  (or (let-alist (seq-first .candidates)
                                        (cond ((seq-first .content.parts)
                                               (let-alist (seq-first .content.parts)
                                                 .text))
                                              ((equal .finishReason "RECITATION")
                                               "")
                                              ((equal .finishReason "STOP")
                                               "")
                                              ((equal .finishReason "CANCELLED")
                                               "Error: Request cancellled.")
                                              ((equal .finishReason "CRASHED")
                                               "Error: An error occurred. Try again.")
                                              ((equal .finishReason "END_OF_PROMPT")
                                               "Error: Couldn't generate a response. Try rephrasing.")
                                              ((equal .finishReason "LENGTH")
                                               "Error: Response is too big. Try rephrasing.")
                                              ((equal .finishReason "TIME")
                                               "Error: Timed out.")
                                              ((equal .finishReason "SAFETY")
                                               "Error: Flagged for safety.")
                                              ((equal .finishReason "LANGUAGE")
                                               "Error: Flagged for language.")
                                              ((equal .finishReason "BLOCKLIST")
                                               "Error: Flagged for forbidden terms.")
                                              ((equal .finishReason "PROHIBITED_CONTENT")
                                               "Error: Flagged for prohibited content.")
                                              ((equal .finishReason "SPII")
                                               "Error: Flagged for sensitive personally identifiable information.")
                                              (.finishReason
                                               (format "\n\nError: Something's up (%s)" .finishReason))))
                                      .error.message))))
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

(provide 'chatgpt-shell-google)

;;; chatgpt-shell-google.el ends here
