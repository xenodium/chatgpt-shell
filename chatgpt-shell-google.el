;;; chatgpt-shell-google.el --- Google-specific logic  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Alvaro Ramirez

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

(require 'cl-lib)
(require 'let-alist)
(require 'shell-maker)
(require 'map)
(require 'rx)
(require 'json)

(defvar chatgpt-shell-proxy)
(defvar chatgpt-shell-models)
(defvar-local chatgpt-shell-google--last-interaction-id nil
  "The interaction ID returned by the previous Gemini Interactions API call.")
(declare-function chatgpt-shell--unsorted-collection "chatgpt-shell")

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

(defcustom chatgpt-shell-google-thinking-budget-tokens 'dynamic
  "The token budget allocated for Google model thinking.

nil means to use the maximum number of thinking tokens allowed.
Set this to 0 to disable thinking on models that support no
thinking. =\\'dynamic means to let the model decide how many
thinking tokens to use based on the complexity of the query. See
https://ai.google.dev/gemini-api/docs/thinking."
  :type '(choice integer (const nil) (const dynamic))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-google-fallback-enabled nil
  "Whether to fallback to secondary Google models if an API call fails or returns non-200.
When non-nil and the active model request fails (e.g. non-200 HTTP code or error),
chatgpt-shell will attempt each fallback model in `chatgpt-shell-google-fallback-models'
one by one."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-google-fallback-models '("gemini-3.5-flash" "gemini-2.5-flash" "gemini-2.0-flash")
  "List of Google model version strings to try sequentially as fallbacks when an API call fails.
Models are tried in order. If all fallback models fail, the request fails."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defun chatgpt-shell-google-reasoning-effort-selector (model)
  "Select the reasoning effort for the Google MODEL."
  (let* ((min (map-elt model :thinking-budget-min))
         (max (map-elt model :thinking-budget-max))
         (response (completing-read (format "Thinking budget tokens (%d-%d): " min max)
                                    (chatgpt-shell--unsorted-collection
                                     (append (and (= min 0) (list "disable"))
                                             (list "dynamic" "max")))))
         (budget (cond
                  ((equal response "disable")
                   0)
                  ((equal response "dynamic")
                   'dynamic)
                  ((equal response "max")
                   nil)
                  (t
                   (string-to-number response)))))
    (unless (or (memq budget '(dynamic nil))
                (and (integerp budget) (<= min budget max)))
      (user-error "Thinking budget tokens must be in the range %d-%d" min max))
    `(((:symbol . chatgpt-shell-google-thinking-budget-tokens)
       (:value . ,budget)
       (:kind . thinking-budget)
       (:max . ,(null budget))))))

;; https://ai.google.dev/gemini-api/docs/tokens
;; A token is equivalent to _about_ 4 characters.
(cl-defun chatgpt-shell-google-make-model (&key version short-version path token-width context-window grounding-search url-context thinking-budget-min thinking-budget-max reasoning-effort-selector)
  "Create a Google model.

Set VERSION, SHORT-VERSION, PATH, TOKEN-WIDTH, CONTEXT-WINDOW,
GROUNDING-SEARCH handler, URL-CONTEXT, THINKING-BUDGET-MIN,
THINKING-BUDGET-MAX and REASONING-EFFORT-SELECTOR."
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
    (:url-context . ,url-context)
    (:thinking-budget-min . ,thinking-budget-min)
    (:thinking-budget-max . ,thinking-budget-max)
    (:reasoning-effort-selector . ,reasoning-effort-selector)
    (:url-base . chatgpt-shell-google-api-url-base)
    (:handler . chatgpt-shell-google--handle-gemini-command)
    (:filter . chatgpt-shell-google--extract-gemini-response)
    (:payload . chatgpt-shell-google--make-payload)
    (:url . chatgpt-shell-google--make-url)
    (:headers . chatgpt-shell-google--make-headers)
    (:key . chatgpt-shell-google-key)
    (:validate-command . chatgpt-shell-google--validate-command)
    (:icon . "gemini-color.png")))

;; https://ai.google.dev/gemini-api/docs/text-generation
(cl-defun chatgpt-shell-google-make-interactions-model (&key version short-version token-width context-window grounding-search url-context thinking-budget-min thinking-budget-max reasoning-effort-selector)
  "Create a Google Gemini model using the Interactions API (/v1beta/interactions)."
  (unless version
    (error "Missing mandatory :version param"))
  (unless short-version
    (error "Missing mandatory :short-version param"))
  (unless token-width
    (error "Missing mandatory :token-width param for %s" version))
  (unless context-window
    (error "Missing mandatory :context-window param for %s" version))
  `((:version . ,version)
    (:short-version . ,short-version)
    (:label . "Gemini")
    (:provider . "Google")
    (:path . "/v1beta/interactions")
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:grounding-search . ,grounding-search)
    (:url-context . ,url-context)
    (:thinking-budget-min . ,thinking-budget-min)
    (:thinking-budget-max . ,thinking-budget-max)
    (:reasoning-effort-selector . ,reasoning-effort-selector)
    (:url-base . chatgpt-shell-google-api-url-base)
    (:handler . chatgpt-shell-google--handle-interactions-command)
    (:filter . chatgpt-shell-google--extract-interactions-response)
    (:payload . chatgpt-shell-google--make-interactions-payload)
    (:url . chatgpt-shell-google--make-interactions-url)
    (:headers . chatgpt-shell-google--make-headers)
    (:key . chatgpt-shell-google-key)
    (:validate-command . chatgpt-shell-google--validate-command)
    (:icon . "gemini-color.png")))

(defun chatgpt-shell-google--current-generative-model-p (api-response)
  "Determine if model in API-RESPONSE is generative.

It returns non-nil if the model described in API-RESPONSE is current and
supports \"generateContent\".

This is used to filter the list of models returned from
https://generativelanguage.googleapis.com"
  (let-alist api-response
    (and .supportedGenerationMethods
         (not (and .description (string-match-p (rx (or "discontinued" "deprecated")) .description)))
         (seq-contains-p .supportedGenerationMethods "generateContent"))))

(defun chatgpt-shell-google--fetch-model-versions ()
  "Retrieves the list of generative models from the Google API."
  (unless (chatgpt-shell-google-key)
    (user-error "Please set your `chatgpt-shell-google-key'"))
  (with-current-buffer (url-retrieve-synchronously
                        (concat chatgpt-shell-google-api-url-base "/v1beta/models?key="
                                (chatgpt-shell-google-key)))
    (goto-char (if (boundp 'url-http-end-of-headers)
                   url-http-end-of-headers
                 (error "`url-http-end-of-headers' marker is not defined")))
    (if-let* ((parsed-response
               (shell-maker--json-parse-string
                (buffer-substring-no-properties (point) (point-max)))))
        (let-alist parsed-response
          (seq-filter #'chatgpt-shell-google--current-generative-model-p .models))
      (error "No response from Google"))))

(defun chatgpt-shell-google--parse-model (api-response)
  "Parse Google API-RESPONSE and return a `chatgpt-shell' model."
  (let-alist api-response
    (let* ((model-version (string-remove-prefix "models/" .name))
           (model-shortversion (string-remove-prefix "gemini-" model-version))
           (model-urlpath (concat "/v1beta/" .name))
           ;; The api-response descriptor does not stipulate whether grounding is supported.
           ;; This logic applies a heuristic based on the model name (aka version).
           (model-supports-grounding (string-match-p (rx bol (or "gemini-1.5" "gemini-2.0" "gemini-2.5")) model-version)))
      (chatgpt-shell-google-make-model :version model-version
                                       :short-version model-shortversion
                                       :grounding-search model-supports-grounding
                                       :path model-urlpath
                                       :token-width 4
                                       :context-window .inputTokenLimit))))

(cl-defun chatgpt-shell-google-load-models (&key override)
  "Query Google for the list of Gemini LLM models available.

By default, this package adds a list of statically-defined models, as
returned from `chatgpt-shell-google-models', into `chatgpt-shell-models'.
But some users may want to choose from a fresher set of available models.

This function retrieves data from
https://ai.google.dev/gemini-api/docs/models/gemini, and
appends the models retrieved to the `chatgpt-shell-models' list, unless
a model with the same name is already present.

By default, replace the existing Google models in `chatgpt-shell-models'
with the newly retrieved models.  When OVERRIDE is non-nil, which
happens when the function is invoked interactively with a prefix
argument, replace all the Google models with those retrieved.

One note: models loaded this way do not get a
`thinking-budget-min' or `thinking-budget-max'."

  (interactive (list :override current-prefix-arg))
  (let* ((goog-predicate (lambda (model)
                           (string= (map-elt model :provider) "Google")))
         (goog-index (or (cl-position-if goog-predicate chatgpt-shell-models)
                         (length chatgpt-shell-models))))
    (setq chatgpt-shell-models (and (not override)
                                    (cl-remove-if goog-predicate chatgpt-shell-models)))
    (let* ((existing-gemini-models
            (mapcar (lambda (model) (map-elt model :version))
                    (cl-remove-if-not goog-predicate chatgpt-shell-models)))
           (new-gemini-models
            (mapcar #'chatgpt-shell-google--parse-model (chatgpt-shell-google--fetch-model-versions))))
      (setq chatgpt-shell-models
            (append (seq-take chatgpt-shell-models goog-index)
                    new-gemini-models
                    (seq-drop chatgpt-shell-models goog-index)))
      (message "Added %d Gemini model(s); kept %d existing Gemini model(s)"
               (length new-gemini-models)
               (length existing-gemini-models)))))


(defun chatgpt-shell-google-toggle-grounding-with-google-search ()
  "Toggle the `:grounding-search' boolean for the currently-selected model.

Google's documentation states that All Gemini 1.5 and 2.0 models support
grounding with Google search, and `:grounding-search' will be t for
those models.  For models that support grounding, this package will
include a

  (tools .((google_search . ())))

in the request payload for 2.0+ models, or

  (tools .((google_search_retrieval . ())))

for 1.5-era models.

But some of the experimental models of those versions may not support
grounding.  If `chatgpt-shell' tries to send a tools parameter as above
to a model that does not support grounding, the API returns an error.

And in some cases users may wish to not _use_ grounding in Search, even
though it is available.

In either case, the user can invoke this function to toggle
grounding-in-google-search on the model.  This package will send the
tools parameter in subsequent outbound requests to that model, when
grounding is enabled.

Returns the new boolean value of `:grounding-search'."
  (interactive)
  (when-let* ((current-model (chatgpt-shell--resolved-model))
              (is-google (string= (map-elt current-model :provider) "Google"))
              (current-grounding-cons (assq :grounding-search current-model)))
    (let ((toggled (not (cdr current-grounding-cons))))
      (setf (cdr current-grounding-cons) toggled)
      (message "Grounding in Google search: %s" (if toggled "ON" "OFF"))
      toggled)))

(defun chatgpt-shell-google-models ()
  "Build a list of Google LLM models available."
  ;; Context windows have been verified as of 11/26/2024. See
  ;; https://ai.google.dev/gemini-api/docs/models/gemini.
  (list (chatgpt-shell-google-make-interactions-model :version "gemini-3.6-flash"
                                                       :short-version "gemini-3.6-flash"
                                                       :grounding-search t
                                                       :url-context t
                                                       :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                                       :token-width 4
                                                       :context-window 1048576)
        (chatgpt-shell-google-make-interactions-model :version "gemini-3.5-flash"
                                                       :short-version "gemini-3.5-flash"
                                                       :grounding-search t
                                                       :url-context t
                                                       :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                                       :token-width 4
                                                       :context-window 1048576)
        (chatgpt-shell-google-make-interactions-model :version "gemini-3.5-flash-lite"
                                                       :short-version "gemini-3.5-flash-lite"
                                                       :grounding-search t
                                                       :url-context t
                                                       :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                                       :token-width 4
                                                       :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-3.1-pro-preview"
                                         :short-version "gemini-3.1-pro-preview"
                                         :path "/v1beta/models/gemini-3.1-pro-preview"
                                         :grounding-search t
                                         :url-context t
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-3-pro-preview"
                                         :short-version "gemini-3-pro-preview"
                                         :path "/v1beta/models/gemini-3-pro-preview"
                                         :grounding-search t
                                         :url-context t
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-3-flash-preview"
                                         :short-version "gemini-3-flash-preview"
                                         :path "/v1beta/models/gemini-3-flash-preview"
                                         :grounding-search t
                                         :url-context t
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-flash-latest"
                                         :short-version "flash-latest"
                                         :path "/v1beta/models/gemini-flash-latest"
                                         :thinking-budget-min 0
                                         :thinking-budget-max 24576
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :grounding-search t
                                         :url-context t
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-flash-lite-latest"
                                         :short-version "flash-lite-latest"
                                         :path "/v1beta/models/gemini-flash-lite-latest"
                                         :thinking-budget-min 0
                                         :thinking-budget-max 24576
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :grounding-search t
                                         :url-context t
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-pro-latest"
                                         :short-version "pro-latest"
                                         :path "/v1beta/models/gemini-pro-latest"
                                         :grounding-search t
                                         :url-context t
                                         :thinking-budget-min 128
                                         :thinking-budget-max 32768
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-2.5-flash"
                                         :short-version "gemini-2.5-flash"
                                         :path "/v1beta/models/gemini-2.5-flash"
                                         :thinking-budget-min 0
                                         :thinking-budget-max 24576
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :grounding-search t
                                         :url-context t
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-2.5-pro"
                                         :short-version "gemini-2.5-pro"
                                         :path "/v1beta/models/gemini-2.5-pro"
                                         :grounding-search t
                                         :url-context t
                                         :thinking-budget-min 128
                                         :thinking-budget-max 32768
                                         :reasoning-effort-selector #'chatgpt-shell-google-reasoning-effort-selector
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-2.0-flash"
                                         :short-version "2.0-flash"
                                         :path "/v1beta/models/gemini-2.0-flash"
                                         :grounding-search t
                                         :url-context t
                                         :token-width 4
                                         :context-window 1048576)
        (chatgpt-shell-google-make-model :version "gemini-2.0-flash-lite"
                                         :short-version "2.0-flash-lite"
                                         :path "/v1beta/models/gemini-2.0-flash-lite"
                                         :grounding-search t
                                         :url-context t
                                         :token-width 4
                                         :context-window 1048576)))

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
          "?alt=sse")) ;; Needed or streaming doesn't work.

(cl-defun chatgpt-shell-google--make-headers (&key _model _settings)
  "Create the API headers."
  (list "Content-Type: application/json; charset=utf-8"
        (concat "x-goog-api-key: "
                (or (chatgpt-shell-google-key)
                    (error "Your chatgpt-shell-google-key is missing")))))

(cl-defun chatgpt-shell-google--make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (chatgpt-shell-google--make-gemini-payload
   :context context
   :model model
   :settings settings))

(defun chatgpt-shell-google--effective-fallback-models (current-model)
  "Return a list of model version strings to try as fallbacks for CURRENT-MODEL."
  (let ((models chatgpt-shell-google-fallback-models))
    (cl-remove (map-elt current-model :version) models :test #'string=)))

(cl-defun chatgpt-shell-google--execute-request (&key model command context shell settings url payload headers filter remaining-fallbacks)
  "Execute an HTTP request for Google MODEL with sequential fallback list."
  (let ((fallbacks (or remaining-fallbacks
                       (when chatgpt-shell-google-fallback-enabled
                         (chatgpt-shell-google--effective-fallback-models model)))))
    (shell-maker-make-http-request
     :async t
     :url url
     :proxy chatgpt-shell-proxy
     :data payload
     :headers headers
     :filter filter
     :shell shell
     :on-finished
     (lambda (result)
       (let ((exit-code (map-elt result :exit-status))
             (http-status (map-elt result :http-status)))
         (when (and fallbacks
                    (or (and exit-code (not (zerop exit-code)))
                        (and http-status (integerp http-status) (not (= http-status 200)))))
           (let* ((next-version (car fallbacks))
                  (rest-fallbacks (cdr fallbacks))
                  (fallback-model (cl-find-if (lambda (m)
                                                (and (string= (map-elt m :provider) "Google")
                                                     (string= (map-elt m :version) next-version)))
                                              chatgpt-shell-models)))
             (if fallback-model
                 (progn
                   (message "Google API call failed for %s (HTTP %s / exit %s). Trying fallback (%d remaining): %s..."
                            (map-elt model :version)
                            (or http-status "N/A")
                            (or exit-code "N/A")
                            (length fallbacks)
                            next-version)
                   (when-let* ((handler (map-elt fallback-model :handler)))
                     (funcall handler
                              :model fallback-model
                              :command command
                              :context context
                              :shell shell
                              :settings settings
                              :remaining-fallbacks rest-fallbacks)))
               (message "Fallback model %s not found in `chatgpt-shell-models'." next-version)))))))))

(cl-defun chatgpt-shell-google--handle-gemini-command (&key model command context shell settings remaining-fallbacks)
  "Handle Gemini COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (chatgpt-shell-google--execute-request
   :model model
   :command command
   :context context
   :shell shell
   :settings settings
   :url (chatgpt-shell-google--make-url :model model :settings settings)
   :payload (chatgpt-shell-google--make-gemini-payload
             :prompt command :context context :model model :settings settings)
   :headers (chatgpt-shell-google--make-headers :model model :settings settings)
   :filter #'chatgpt-shell-google--extract-gemini-response
   :remaining-fallbacks remaining-fallbacks))

(cl-defun chatgpt-shell-google--make-interactions-url (&key _command model settings)
  "Create the Interactions API URL using MODEL and SETTINGS."
  (unless model
    (error "Missing mandatory :model param"))
  (concat chatgpt-shell-google-api-url-base
          "/v1beta/interactions"
          (when (map-elt settings :streaming)
            "?alt=sse")))

(cl-defun chatgpt-shell-google--make-interactions-payload (&key prompt context settings model)
  "Create the request payload for Gemini Interactions API.

Composes using PROMPT, CONTEXT, SETTINGS and MODEL."
  (unless context
    (setq chatgpt-shell-google--last-interaction-id nil))
  (let* ((version (map-elt model :version))
         (system-prompt (map-elt settings :system-prompt))
         (is-streaming (map-elt settings :streaming))
         (prev-id chatgpt-shell-google--last-interaction-id)
         (grounding-search (map-elt model :grounding-search))
         (url-context (map-elt model :url-context))
         (tools-list (append (when grounding-search '(((type . "google_search"))))
                             (when url-context '(((type . "url_context"))))))
         (formatted-input
          (cond
           ((or (listp prompt) (vectorp prompt))
            prompt)
           ((and (not prev-id) context prompt)
            (let ((history-str
                   (mapconcat
                    (lambda (item)
                      (concat (when (car item) (format "User: %s\n" (car item)))
                              (when (cdr item) (format "Model: %s\n" (cdr item)))))
                    context
                    "")))
              (if (string-empty-p history-str)
                  prompt
                (format "%sUser: %s" history-str prompt))))
           (prompt prompt)
           (t "")))
         (payload `((model . ,version)
                    (input . ,formatted-input))))
    (when prev-id
      (push (cons 'previous_interaction_id prev-id) payload))
    (when tools-list
      (push (cons 'tools (vconcat tools-list)) payload))
    (when system-prompt
      (push (cons 'system_instruction system-prompt) payload))
    (when is-streaming
      (push (cons 'stream t) payload))
    payload))

(defun chatgpt-shell-google--parse-interactions-json (obj)
  "Extract response text from an Interactions API JSON object OBJ."
  (let-alist obj
    (when .id
      (setq chatgpt-shell-google--last-interaction-id .id))
    (or .error.message
        .output_text
        (when .steps
          (let ((texts nil))
            (mapc
             (lambda (step)
               (let-alist step
                 (when (or (equal .type "model_output") (equal .type "text"))
                   (mapc
                    (lambda (item)
                      (let-alist item
                        (when .text (push .text texts))))
                    .content))))
             .steps)
            (when texts
              (mapconcat #'identity (nreverse texts) ""))))
        (when .content
          (if (stringp .content)
              .content
            (mapconcat (lambda (item) (let-alist item .text)) .content "")))
        .text
        .delta.text)))

(defun chatgpt-shell-google--extract-interactions-response (output)
  "Process pending OUTPUT to extract Gemini Interactions response."
  (if-let* ((whole (shell-maker--json-parse-string (map-elt output :pending)))
            (response (chatgpt-shell-google--parse-interactions-json whole)))
      (list (cons :filtered response))
    (if-let ((chunks (shell-maker--split-text (map-elt output :pending))))
        (let ((response)
              (pending)
              (result))
          (mapc (lambda (chunk)
                  (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                            (obj (shell-maker--json-parse-string (map-elt chunk :value)))
                            (text (chatgpt-shell-google--parse-interactions-json obj)))
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

(cl-defun chatgpt-shell-google--handle-interactions-command (&key model command context shell settings remaining-fallbacks)
  "Handle Gemini COMMAND using MODEL via the Interactions API."
  (chatgpt-shell-google--execute-request
   :model model
   :command command
   :context context
   :shell shell
   :settings settings
   :url (chatgpt-shell-google--make-interactions-url :model model :settings settings)
   :payload (chatgpt-shell-google--make-interactions-payload
             :prompt command :context context :model model :settings settings)
   :headers (chatgpt-shell-google--make-headers :model model :settings settings)
   :filter #'chatgpt-shell-google--extract-interactions-response
   :remaining-fallbacks remaining-fallbacks))

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
   `((tools . ,(append (when (map-elt model :grounding-search) '((google_search . nil)))
                       (when (map-elt model :url-context) '((url_context . nil))))))
   `((generation_config . ,(append
                            `((temperature . ,(or (map-elt settings :temperature) 1)))
                            ;; 1 is most diverse output.
                            '((topP . 1))
                            ;; Include thinking parameters if it is supported for
                            ;; this model.
                            (let ((min (map-elt model :thinking-budget-min))
                                  (max (map-elt model :thinking-budget-max)))
                              (when (or min max)
                                (let ((chatgpt-shell-google-thinking-budget-tokens
                                       (cond
                                        ((not chatgpt-shell-google-thinking-budget-tokens)
                                         max)
                                        ;; -1 is always valid and indicates dynamic thinking. See
                                        ;; https://ai.google.dev/gemini-api/docs/thinking.
                                        ((eq chatgpt-shell-google-thinking-budget-tokens 'dynamic)
                                         -1)
                                        ((<= min chatgpt-shell-google-thinking-budget-tokens max)
                                         chatgpt-shell-google-thinking-budget-tokens)
                                        (t
                                         (error "Error: chatgpt-shell-google-thinking-budget-tokens must be between %d and %d (inclusive) or 'dynamic" min max)))))
                                  `((thinkingConfig . ((thinkingBudget . ,chatgpt-shell-google-thinking-budget-tokens))))))))))))

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

(defun chatgpt-shell-google--extract-gemini-response (output)
  "Process pending OUTPUT to extract Gemini response.

OUTPUT is always of the form:

  ((:function-calls . ...)
   (:pending . ...)
   (:filtered . ...))

and must be returned in the same form.

Processing means processing :pending content into :filtered."
  (when (stringp output)
    (error "Please upgrade shell-maker to 0.79.1 or newer"))
  (if-let* ((whole (shell-maker--json-parse-string (map-elt output :pending)))
            (response (or (let-alist whole
                            .error.message)
                          (let-alist whole
                            (mapconcat (lambda (choice)
                                         (let-alist choice
                                           (or .delta.content
                                               .message.content)))
                                       .choices "")))))
      (list (cons :filtered response))
    (if-let ((chunks (shell-maker--split-text (map-elt output :pending))))
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
      output)))

(provide 'chatgpt-shell-google)

;;; chatgpt-shell-google.el ends here
