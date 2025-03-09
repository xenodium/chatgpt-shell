;;; chatgpt-shell-google.el --- Google-specific logic  -*- lexical-binding: t -*-

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

;; Adds Google specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'map)
(require 'json)

(defvar chatgpt-shell-proxy)

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

;; https://ai.google.dev/gemini-api/docs/tokens
;; A token is equivalent to _about_ 4 characters.
(cl-defun chatgpt-shell-google-make-model (&key version short-version path token-width context-window grounding-search)
  "Create a Google model.

Set VERSION, SHORT-VERSION, PATH, TOKEN-WIDTH, CONTEXT-WINDOW,
VALIDATE-COMMAND, and GROUNDING-SEARCH handler."
  (unless version
    (error "Missing mandatory :version param"))
  (unless short-version
    (error "Missing mandatory :short-version param"))
  (unless path
    (error "Missing mandatory :path param"))
  (unless token-width
    (error "Missing mandatory :token-width param for %s" version))
  (unless context-window
    (error "Missing mandatory :context-window param for %s" version))
  `((:version . ,version)
    (:short-version . ,short-version)
    (:label . "Gemini")
    (:provider . "Google")
    (:path . ,path)
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:grounding-search . ,grounding-search)
    (:url-base . chatgpt-shell-google-api-url-base)
    (:handler . chatgpt-shell-google--handle-gemini-command)
    (:filter . chatgpt-shell-google--extract-gemini-response)
    (:payload . chatgpt-shell-google--make-payload)
    (:url . chatgpt-shell-google--make-url)
    (:headers . chatgpt-shell-google--make-headers)
    (:key . chatgpt-shell-google-key)
    (:validate-command . chatgpt-shell-google--validate-command)))

(defun chatgpt-shell-google--current-generative-model-p (api-response)
  "This is a predicate that looks at a model description within
API-RESPONSE.

It returns non-nil if the model described in API-RESPONSE is current and
supports \"generateContent\".

This is used to filter the list of models returned from
https://generativelanguage.googleapis.com"
  (when-let* ((description (gethash "description" api-response))
              ((not (string-match-p (rx (or "discontinued" "deprecated")) description)))
              (supported-methods (gethash "supportedGenerationMethods" api-response)))
    (seq-contains-p supported-methods "generateContent")))

(defun chatgpt-shell-google--fetch-model-versions ()
  "Retrieves the list of generative models from the Google API."
  (let ((url (concat chatgpt-shell-google-api-url-base "/v1beta/models?key="
                     (chatgpt-shell-google-key))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (if (boundp 'url-http-end-of-headers)
                     url-http-end-of-headers
                   (error "`url-http-end-of-headers' marker is not defined")))
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
        (let ((parsed-response
               (json-read-from-string
                (buffer-substring-no-properties (point) (point-max)))))
          (seq-filter #'chatgpt-shell-google--current-generative-model-p
                      (gethash "models" parsed-response)))))))

(defun chatgpt-shell-google--convert-model (api-response)
  "Convert the API-RESPONSE returned by Gemini into a
the model description needed by `chatgpt-shell'."
  (let ((model-name (gethash "name" model))
        (model-cwindow (gethash "inputTokenLimit" model)))
    (let ((model-version (string-remove-prefix "models/" model-name)))
      (let ((model-shortversion (string-remove-prefix "gemini-" model-version))
            (model-urlpath (concat "/v1beta/" model-name))
            ;; The model descriptor does not stipulate whether grounding is supported.
            ;; So this logic just checks the name.
            (model-supports-grounding (or
                                       (string-prefix-p "gemini-1.5" model-version)
                                       (string-prefix-p "gemini-2.0" model-version))))
        (chatgpt-shell-google-make-model :version model-version
                                         :short-version model-shortversion
                                         :grounding-search model-supports-grounding
                                         :path model-urlpath
                                         :token-width 4
                                         :context-window model-cwindow)))))

(cl-defun chatgpt-shell-google-load-models (&key override)
  "Query Google for the list of Gemini LLM models available.

The data is retrieved from
https://ai.google.dev/gemini-api/docs/models/gemini.  This fn then the
models retrieved to `chatgpt-shell-models' unless a model with the same
name is already present.

By default, replace the existing Google models in `chatgpt-shell-models'
with the newly retrieved models.  When OVERRIDE is non-nil (interactively
with a prefix argument), replace all the Google models with those
retrieved."
  (interactive (list :override current-prefix-arg))
  (let* ((goog-predicate (lambda (model)
                           (string= (map-elt model :provider) "Google")))
         (goog-index (or (cl-position-if goog-predicate chatgpt-shell-models)
                         (length chatgpt-shell-models))))
    (setq chatgpt-shell-models (and (not override)
                                    (cl-remove-if goog-predicate chatgpt-shell-models)))
    (let* ((existing-gemini-models (mapcar (lambda (model)
                                             (map-elt model :version))
                                           (cl-remove-if-not goog-predicate
                                                             chatgpt-shell-models)))
           (new-gemini-models
            (mapcar #'chatgpt-shell-google--convert-model (chatgpt-shell-google--fetch-model-versions))))
      (setq chatgpt-shell-models
            (append (seq-take chatgpt-shell-models goog-index)
                    new-gemini-models
                    (seq-drop chatgpt-shell-models goog-index)))
      (message "Added %d Gemini model(s); kept %d existing Gemini model(s)"
               (length new-gemini-models)
               (length existing-gemini-models)))))

(defun chatgpt-shell-google-toggle-grounding ()
  "Toggle the `:grounding-search' boolean for the currently-selected model.

Google's documentation states that All Gemini 1.5 and 2.0 models support
grounding, and `:grounding-search' will be `t' for those models. For
models that support grounding, this package will include a

  (tools .((google_search . ())))

in the request payload for 2.0+ models, or

  (tools .((google_search_retrieval . ())))

for 1.5-era models.

But some of the experimental models may not support grounding.  If
`chatgpt-shell' tries to send a tools parameter as above to a model that
does not support grounding, the API returns an error.  In that case, the
user can use this function to toggle grounding on the model, so that
this package does not send the tools parameter in subsequent outbound
requests to that model.

Returns the newly toggled value of `:grounding-search'."
  (interactive)
  (when-let* ((current-model (chatgpt-shell--resolved-model))
              (is-google (string= (map-elt current-model :provider) "Google"))
              (current-grounding-cons (assq :grounding-search current-model)))
    (setf (cdr current-grounding-cons) (not (cdr current-grounding-cons)))))

(defun chatgpt-shell-google--get-grounding-tool-keyword (model)
  "Retrieves the keyword for the grounding tool.

This gets set once for each model, based on a heuristic."
  (when-let* ((current-model model)
              (is-google (string= (map-elt current-model :provider) "Google"))
              (version (map-elt current-model :version)))
    (save-match-data
      (if (string-match "1\\.5" version) "google_search_retrieval" "google_search"))))

(defun chatgpt-shell-google-models ()
  "Build a list of Google LLM models available."
  ;; Context windows have been verified as of 11/26/2024. See
  ;; https://ai.google.dev/gemini-api/docs/models/gemini.
  (list (chatgpt-shell-google-make-model :version "gemini-2.0-flash"
                                         :short-version "2.0-flash"
                                         :path "/v1beta/models/gemini-2.0-flash"
                                         :grounding-search t
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-1.5-pro-latest"
                                         :short-version "1.5-pro-latest"
                                         :path "/v1beta/models/gemini-1.5-pro-latest"
                                         :token-width 4
                                         :context-window 2097152)
        (chatgpt-shell-google-make-model :version "gemini-1.5-flash-latest"
                                         :short-version "1.5-flash-latest"
                                         :path "/v1beta/models/gemini-1.5-flash-latest"
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-2.0-flash-thinking-exp-01-21"
                                         :short-version "2.0-flash-thinking-exp"
                                         :path "/v1beta/models/gemini-2.0-flash-thinking-exp-01-21"
                                         :token-width 4
                                         :context-window 32767)))

(defun chatgpt-shell-google--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-google-key
    "Variable `chatgpt-shell-google-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-google-key

or

(setq chatgpt-shell-google-key \"my-key\")"))

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

(cl-defun chatgpt-shell-google--make-url (&key _command model settings)
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

(cl-defun chatgpt-shell-google--make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (chatgpt-shell-google--make-gemini-payload
   :context context
   :model model
   :settings settings))

(cl-defun chatgpt-shell-google--handle-gemini-command (&key model command context shell settings)
  "Handle Gemini COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-google--make-url :model model
                                        :settings settings)
   :proxy chatgpt-shell-proxy
   :data (chatgpt-shell-google--make-gemini-payload
          :prompt command
          :context context
          :model model
          :settings settings)
   :headers (list "Content-Type: application/json; charset=utf-8")
   :filter #'chatgpt-shell-google--extract-gemini-response
   :shell shell))

(cl-defun chatgpt-shell-google--make-gemini-payload (&key prompt context settings model)
  "Create the request payload.

 Compose using PROMPT, CONTEXT, SETTINGS and MODEL."
  (append
   (when (map-elt settings :system-prompt)
     `((system_instruction . ((parts . ((text . ,(map-elt settings :system-prompt))))))))
   `((contents . ,(vconcat
                   (chatgpt-shell-google--gemini-user-model-messages
                    (append context
                            (when prompt
                              (list (cons prompt nil))))))))
   (when (map-elt model :grounding-search)
     ;; Google's docs say that grounding is supported for all Gemini 1.5 and 2.0 models.
     ;; But the API is slightly different between them. This uses the correct tool name.
     `((tools . ((,(intern (chatgpt-shell-google--get-grounding-tool-keyword model)) . ())))))
   `((generation_config . ((temperature . ,(or (map-elt settings :temperature) 1))
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
                                       .choices "")))))
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
