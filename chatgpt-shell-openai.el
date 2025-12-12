;;; chatgpt-shell-openai.el --- OpenAI-specific logic  -*- lexical-binding: t -*-

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

;; Adds OpenAI specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'shell-maker)

(declare-function chatgpt-shell--unsorted-collection "chatgpt-shell")
(declare-function chatgpt-shell-crop-context "chatgpt-shell")
(declare-function chatgpt-shell--make-chatgpt-url "chatgpt-shell")
(declare-function chatgpt-shell-validate-no-system-prompt "chatgpt-shell")

;; See https://platform.openai.com/docs/guides/reasoning
(defcustom chatgpt-shell-openai-reasoning-effort "medium"
  "The amount of reasoning effort to use for OpenAI reasoning models.

 It can be \"minimal\", \"low\", \"medium\" or \"high\". Lower
values are faster and cheaper but higher values may work better
for more difficult problems."
  :type 'string
  :safe #'stringp
  :options '("minimal" "low" "medium" "high")
  :group 'chatgpt-shell)

(defun chatgpt-shell-openai-make-reasoning-effort-selector (choices)
  "Create a function for the :reasoning-effort-selector parameter using CHOICES."
  (lambda (_model)
    (let ((effort (completing-read "Reasoning effort: " (chatgpt-shell--unsorted-collection choices) nil t)))
      `(((:symbol . chatgpt-shell-openai-reasoning-effort)
         (:value . ,effort)
         (:kind . thinking-budget))))))

(cl-defun chatgpt-shell-openai-make-model (&key version short-version token-width context-window validate-command (headers #'chatgpt-shell-openai--make-headers) (key chatgpt-shell-openai-key) (url-base 'chatgpt-shell-api-url-base) (path "/v1/chat/completions") (provider "OpenAI") (label "ChatGPT") (handler #'chatgpt-shell-openai--handle-chatgpt-command) (filter #'chatgpt-shell-openai--filter-output) reasoning-effort reasoning-effort-selector icon function-calling other-params)
  "Create an OpenAI model.

Set VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW,
VALIDATE-COMMAND, HEADERS, KEY, URL-BASE, PATH, PROVIDER, LABEL,
HANDLER, REASONING-EFFORT, FILTER, ICON, FUNCTION-CALLING, and OTHER-PARAMS."
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
  (append `((:version . ,version)
            (:short-version . ,short-version)
            (:label . ,label)
            (:provider . ,provider)
            (:path . ,path)
            (:token-width . ,token-width)
            (:context-window . ,context-window)
            (:handler . ,handler)
            (:filter . ,filter)
            (:payload . chatgpt-shell-openai--make-payload)
            (:headers . ,headers)
            (:url . chatgpt-shell-openai--make-url)
            (:key . ,key)
            (:reasoning-effort . ,reasoning-effort)
            (:reasoning-effort-selector . ,reasoning-effort-selector)
            (:url-base . ,url-base)
            (:validate-command . ,(or validate-command 'chatgpt-shell-openai--validate-command))
            (:other-params . ,other-params)
            (:icon . ,(or icon "openai.png"))
            (:function-calling . ,function-calling))))

(defun chatgpt-shell-openai-models ()
  "Build a list of all OpenAI LLM models available."
  ;; Context windows have been verified as of 11/26/2024.
  (list (chatgpt-shell-openai-make-model
         :version "gpt-5.2"
         :function-calling t
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-5.2
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("none" "low" "medium" "high"))
         :context-window 400000)
        (chatgpt-shell-openai-make-model
         :version "gpt-5.1"
         :function-calling t
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-5.1
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("none" "low" "medium" "high"))
         :context-window 400000)
        (chatgpt-shell-openai-make-model
         :version "gpt-5"
         :function-calling t
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-5
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("minimal" "low" "medium" "high"))
         :context-window 400000)
        (chatgpt-shell-openai-make-model
         :version "gpt-5-mini"
         :function-calling t
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-5-mini
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("minimal" "low" "medium" "high"))
         :context-window 400000)
        (chatgpt-shell-openai-make-model
         :version "gpt-5-nano"
         :function-calling t
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-5-nano
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("minimal" "low" "medium" "high"))
         :context-window 400000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.1"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/gpt-4.1
         :context-window 1047576)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.1-mini"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/gpt-4.1-mini
         :context-window 1047576)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.1-nano"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/gpt-4.1-nano
         :context-window 1047576)
        (chatgpt-shell-openai-make-model
         :version "chatgpt-4o-latest"
         :token-width 3
         ;; https://platform.openai.com/docs/models/chatgpt-4o-latest
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/gpt-4o
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o-search-preview"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4o-search-preview
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o-mini"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/gpt-4o-mini
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-4o-mini-search-preview"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-4o-mini-search-preview
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "o4-mini"
         :token-width 3
         :function-calling t
         :context-window 200000
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("low" "medium" "high"))
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o3"
         :token-width 3
         :function-calling t
         :context-window 200000
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("low" "medium" "high"))
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o3-mini"
         :token-width 3
         :function-calling t
         :context-window 200000
         :reasoning-effort t
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o1"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/o1
         :context-window 200000
         :reasoning-effort t
         :reasoning-effort-selector (chatgpt-shell-openai-make-reasoning-effort-selector '("low" "medium" "high"))
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o1-preview"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models/gpt-01
         :context-window 128000
         ;; Reasoning effort is only supported for o1-pro, o1 and o3-mini.
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "o1-mini"
         :token-width 3
         ;; https://platform.openai.com/docs/models/gpt-01-mini
         :context-window 128000
         ;; Reasoning effort is only supported for o1 and o3-mini.
         :validate-command #'chatgpt-shell-validate-no-system-prompt)
        (chatgpt-shell-openai-make-model
         :version "gpt-4.5-preview"
         :token-width 3
         :function-calling t
         ;; https://platform.openai.com/docs/models#gpt-4-5
         :context-window 128000)
        (chatgpt-shell-openai-make-model
         :version "gpt-3.5-turbo"
         :token-width 4
         ;; https://platform.openai.com/docs/models/gpt-3.5-turbo#gpt-3-5-turbo
         :context-window 16385)))

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

(defvar chatgpt-shell-openai--use-function-calling nil
  "Tool/function calling is highly experimental.

When non-nil, attempt to use `chatgpt-shell-openai--tools'.

Note that `chatgpt-shell-openai--use-function-calling' and
`chatgpt-shell-openai--tools' are bound to change.

Only models with :function-calling t wil attempt to use
this feature.")

(defvar chatgpt-shell-openai--tools
  '((:name "get_current_weather"
     :function chatgpt-shell-apply-fetch-weather
     :description "Get the current weather for given coordinate. Requires cities or other locations as coordinate."
     :async t
     :args ((:name "lat"
             :type string
             :description "Location latitude")
            (:name "lon"
             :type string
             :description "Location longitude")
            (:name "unit"
             :type string
             :enum ["celsius" "fahrenheit"]
             :optional t))))
  "Known tools for function calling.

Note: Tools are created using plists unlike the rest of the codebase to be
compatible with gptel.el and llm.el tools as per agreed spec:

https://github.com/ahyatt/llm/discussions/124#discussioncomment-11877109")

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
                           (image_url . ((url . ,prompt-url))))))))))))))

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

(cl-defun chatgpt-shell-openai-make-chatgpt-request-data (&key system-prompt prompt prompt-url context messages version temperature reasoning-effort streaming tools other-params)
  "Make request data.

Optionally set PROMPT, VERSION, TEMPERATURE, STREAMING, SYSTEM-PROMPT,
and OTHER-PARAMS (list)."
  (unless version
    (error "Missing mandatory :version param"))
  (when (and messages (or prompt prompt-url system-prompt context))
    (error ":messages cannot be used with either :prompt :prompt-url :system-prompt or :context"))
  (append
   `((model . ,version)
     (messages . ,(vconcat (if messages
                               messages
                             (chatgpt-shell-openai--make-chatgpt-messages
                              :system-prompt system-prompt
                              :prompt prompt
                              :prompt-url prompt-url
                              :context context))))
     )
   (when temperature
     `((temperature . ,temperature)))
   (when tools
     `((tools . ,(vconcat
                  (mapcar (lambda (tool)
                            ;; TODO: This is ChatGPT-specific. Generalize.
                            (chatgpt-shell-openai--tool-to-openai-format tool))
                          tools)))))
   (when tools
     '((tool_choice . "auto")))
   (when reasoning-effort
     `((reasoning_effort . ,reasoning-effort)))
   (when streaming
     `((stream . t)))
   other-params))

(defun chatgpt-shell-openai--tool-to-openai-format (tool)
  "Convert TOOL from generic schema to OpenAI format."
  `((type . "function")
    (function . ((name . ,(plist-get tool :name))
                 (description . ,(plist-get tool :description))
                 (parameters . ,(chatgpt-shell-openai--args-to-openai-params
                                 (plist-get tool :args)))))))

(defun chatgpt-shell-openai--args-to-openai-params (args)
  "Convert ARGS list to OpenAI parameters format."
  (if (null args)
      ;; No arguments case
      `((type . "object")
        (properties . ()))
    ;; Has arguments
    (let ((properties '())
          (required '()))
      (dolist (arg args)
        (let ((name (plist-get arg :name))
              (type (symbol-name (plist-get arg :type)))
              (desc (plist-get arg :description))
              (enum (plist-get arg :enum))
              (optional (plist-get arg :optional)))
          (push `(,(intern name) . ((type . ,type)
                                    ,@(when desc `((description . ,desc)))  ; Only add if desc exists
                                    ,@(when enum `((enum . ,enum)))))
                properties)
          (unless optional
            (push name required))))
      `((type . "object")
        (properties . ,(nreverse properties))
        ,@(when required `((required . ,(vconcat (nreverse required)))))))))

(defun chatgpt-shell-apply-fetch-weather (args on-finished)
  "Fetch weather data from MET Norway API using ARGS and invoking ON-FINISHED.

ARGS is of the form:

\((lat . \"1\")
  (lon . \"1\"))

ON-FINISHED param is of the form:

\((error . \"some error\"))

or

\((content . \"30C and Sunny\"))"
  (cond ((null args)
         (funcall on-finished '((error . "Error: Missing lat and long arguments"))))
        ((string-empty-p (string-trim (or args "")))
         (funcall on-finished '((error . "Error: Missing lat and long arguments"))))
        (t
         (if-let* ((obj (condition-case nil
                            (json-read-from-string args)
                          (error nil)))
                   (lat (string-to-number (map-elt obj 'lat "")))
                   (lon (string-to-number (map-elt obj 'lon ""))))
             (cond ((equal lat 0)
                    (funcall on-finished '((error . "Error: Invalid lat"))))
                   ((equal lon 0)
                    (funcall on-finished '((error . "Error: Invalid lon"))))
                   (t
                    ;; TODO: Move to a separate file.
                    (let ((weather (chatgpt-shell-fetch-weather lat lon)))
                      (funcall on-finished `((content . ,(condition-case nil
                                                             (json-encode weather)
                                                           (error nil))))))))
           (funcall on-finished '((error . "Error: Couldn't parse arguments")))))))

(defun chatgpt-shell-fetch-weather (lat lon)
  "Fetch weather data from MET Norway API for LAT and LON.

  Return the parsed JSON object."
  (when-let* ((url (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s" lat lon))
              (args (list "-s" url))
              (data (with-temp-buffer
                      (apply #'call-process "curl" nil t nil args)
                      (goto-char (point-min))
                      (json-parse-buffer :object-type 'alist)))
              (now (current-time))
              (entry (seq-find
                      (lambda (entry)
                        (let-alist entry
                          (time-less-p now (date-to-time .time))))
                      (let-alist data
                        .properties.timeseries)))
              (unit (let-alist data
                      .properties.meta.units.air_temperature)))
    (let-alist entry
      `((temperature . ,(format "%.1f%s"
                                .data.instant.details.air_temperature
                                (cond
                                 ((string= unit "celsius") "°C")
                                 ((string= unit "fahrenheit") "°F")
                                 (t (concat " " unit)))))
        (symbol . ,(alist-get 'symbol_code .data.next_1_hours.summary))))))

(defun chatgpt-shell-openai--filter-output (output)
  "Process pending OUTPUT to extract ChatGPT response.

OUTPUT is always of the form:

  ((:function-calls . ...)
   (:pending . ...)
   (:filtered . ...))

and must be returned in the same form.

Processing means processing :pending content into :filtered.

When ChatGPT responses are streamed, :pending arrives as:
  data: {...json...}
  data: {...json...}
Otherwise:
  {...json...}."
  (cond ((stringp output)
         (error "Please upgrade shell-maker to 0.79.1 or newer")
         (setq output (list (cons :pending output))))
        ((equal (string-trim (map-elt output :pending))
                "data: [DONE]")
         (setf (map-elt output :pending) "")))
  (if-let* ((whole (shell-maker--json-parse-string (map-elt output :pending)))
            (response-text (or (let-alist whole
                                 .error.message)
                               (let-alist whole
                                 (mapconcat (lambda (choice)
                                              (let-alist choice
                                                (or .delta.content
                                                    .message.content)))
                                            .choices "")))))
      (list (cons :filtered response-text))
    (when-let ((chunks (shell-maker--split-text (map-elt output :pending))))
      (let ((response-text)
            (pending)
            (function-calls (map-elt output :function-calls)))
        (mapc
         (lambda (chunk)
           (if-let* ((is-data (equal (map-elt chunk :key) "data:"))
                     (obj (shell-maker--json-parse-string (map-elt chunk :value))))
               (let-alist obj
                 ;; Extract content
                 (let ((text (mapconcat (lambda (choice)
                                          (let-alist choice
                                            (or (and (not (eq .delta.content :null))
                                                     .delta.content)
                                                .message.content
                                                "")))
                                        .choices "")))
                   (unless (string-empty-p text)
                     (setq response-text (concat response-text text))))
                 ;; Extract function calls/arguments
                 (mapc (lambda (choice)
                         (let-alist choice
                           (mapc
                            (lambda (tool-call)
                              ;; Function/tool call is of the form:
                              ;; '((:name . ...)
                              ;;    (:id . ...)
                              ;;    (:index . ...)
                              ;;    (:arguments . ...))
                              (let-alist tool-call
                                (when .index ;; LOOKS LIKE ID IS NOT ACCESSIBLE HERE!!!! or MAYBE we should be resolving by index
                                  (let ((call (map-elt function-calls .index)))
                                    (when .index
                                      (setf (map-elt call :index) .index))
                                    (when .id
                                      (setf (map-elt call :id) .id))
                                    (when .function.name
                                      (setf (map-elt call :name) .function.name))
                                    (when .function.arguments
                                      (setf (map-elt call :arguments)
                                            (concat (map-elt call :arguments)
                                                    .function.arguments)))
                                    (setf (map-elt function-calls .index) call)))))
                            .delta.tool_calls)
                           (when (equal .finish_reason
                                        "tool_calls")
                             (setf (map-elt output :incoming-requests)
                                   (map-values function-calls)))))
                       .choices))
             (setq pending (concat pending
                                   (or (map-elt chunk :key) "")
                                   (map-elt chunk :value)))))
         chunks)
        (setf (map-elt output :function-calls) function-calls)
        (setf (map-elt output :filtered) (unless (string-empty-p response-text)
                                           response-text))
        (setf (map-elt output :pending) pending)
        output))))

(cl-defun chatgpt-shell-openai--make-url (&key _command model _settings)
  "Create the API URL using MODEL."
  (concat (symbol-value (or (map-elt model :url-base)
                            (error "Model :url-base not found")))
          (or (map-elt model :path)
              (error "Model :path not found"))))

(cl-defun chatgpt-shell-openai--make-headers (&key _model _settings (key #'chatgpt-shell-openai-key))
  "Create the API headers using KEY as the API KEY."
  (list "Content-Type: application/json; charset=utf-8"
        (format "Authorization: Bearer %s" (funcall key))))

(defun chatgpt-shell-openai--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-openai-key
    "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")"))

(cl-defun chatgpt-shell-openai--make-payload (&key model prompt-url context settings)
  "Create the API payload using MODEL PROMPT-URL CONTEXT and SETTINGS."
  (chatgpt-shell-openai-make-chatgpt-request-data
   :system-prompt (map-elt settings :system-prompt)
   :prompt-url prompt-url
   :context context
   :version (map-elt model :version)
   :temperature (map-elt settings :temperature)
   :reasoning-effort (and (map-elt model :reasoning-effort)
                          chatgpt-shell-openai-reasoning-effort)
   :streaming (map-elt settings :streaming)
   :other-params (map-elt model :other-params)))

(cl-defun chatgpt-shell-openai--make-http-request (&key messages async url model settings proxy headers filter shell)
  "Like `shell-maker-make-http-request' but with OpenAI function calling support.

MESSAGES, ASYNC, URL, MODEL, SETTINGS, PROXY, HEADERS, FILTER,
and SHELL are all the same."
  (shell-maker-make-http-request
   :async async
   :url url
   :proxy proxy
   :data (chatgpt-shell-openai-make-chatgpt-request-data
          :messages messages
          :version (map-elt model :version)
          :temperature (map-elt settings :temperature)
          :reasoning-effort (and (map-elt model :reasoning-effort)
                                 chatgpt-shell-openai-reasoning-effort)
          :tools (when (and (map-elt model :function-calling)
                            chatgpt-shell-openai--use-function-calling)
                   chatgpt-shell-openai--tools)
          :streaming (map-elt settings :streaming)
          :other-params (map-elt model :other-params))
   :headers headers
   :on-incoming-requests
   (lambda (incoming-requests)
     (chatgpt-shell-openai--service-incoming-requests
      incoming-requests
      (when (map-elt model :function-calling)
        chatgpt-shell-openai--tools)
      (lambda (serviced-requests)
        (chatgpt-shell-openai--make-http-request
         :messages
         (vconcat (append (vconcat messages)
                          `[((role . "assistant")
                             (tool_calls . ,(vconcat
                                             (mapcar
                                              (lambda (incoming-request)
                                                `((id . ,(map-elt incoming-request :id))
                                                  (type . "function")
                                                  (function . ((name . ,(map-elt incoming-request :name))
                                                               (arguments . ,(map-elt incoming-request :arguments))))))
                                              incoming-requests))))]
                          serviced-requests))
         :async async
         :url url
         :model model
         :settings settings
         :proxy proxy
         :headers headers
         :filter filter
         :shell shell))))
   :filter filter
   :shell shell))

(cl-defun chatgpt-shell-openai--handle-chatgpt-command
    (&key model command context shell settings
          (key #'chatgpt-shell-openai-key)
          (filter #'chatgpt-shell-openai--filter-output)
          (missing-key-msg "Your chatgpt-shell-openai-key is missing"))
  "Handle ChatGPT COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (unless (funcall key)
    (funcall (map-elt shell :write-output) missing-key-msg)
    (funcall (map-elt shell :finish-output) nil))
  (chatgpt-shell-openai--make-http-request
   :messages (chatgpt-shell-openai--make-chatgpt-messages
              :model model
              :system-prompt (map-elt settings :system-prompt)
              :prompt command
              :context context)
   :async t
   :url (chatgpt-shell-openai--make-url :model model)
   :proxy chatgpt-shell-proxy
   :model model
   :settings settings
   :headers (list "Content-Type: application/json; charset=utf-8"
                  (format "Authorization: Bearer %s" (funcall key)))
   :filter filter
   :shell shell))

(defun chatgpt-shell-openai--service-incoming-requests (incoming-requests tools on-finished)
  "Service all INCOMING-REQUESTS using TOOLS, invoking ON-FINISHED when finished."
  (let ((serviced-requests nil))
    (mapc (lambda (incoming-request)
            (if-let* ((tool (seq-find (lambda (tool)
                                        (equal (format "%s" (map-elt incoming-request :name))
                                               (plist-get tool :name)))
                                      tools))
                      (request-handler (plist-get tool :function))
                      (request-id (map-elt incoming-request :id)))
                (funcall request-handler
                         (map-elt incoming-request :arguments)
                         (lambda (result)
                           (setq serviced-requests
                                 (append serviced-requests
                                         ;; TODO: This is ChatGPT-specific. Generalize.
                                         `(((role . "tool")
                                            (tool_call_id . ,request-id)
                                            (content . ,(json-encode (or (map-elt result 'error)
                                                                         (map-elt result 'content)
                                                                         "")))))))
                           (when (equal (length incoming-requests)
                                        (length serviced-requests))
                             (funcall on-finished serviced-requests))))))
          incoming-requests)))

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
