;;; chatgpt-shell-ollama.el --- Ollama-specific logic  -*- lexical-binding: t -*-

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

;; Adds Ollama specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'let-alist)
(require 'shell-maker)
(require 'map)
(require 'seq)
(require 'subr-x)

;; Muffle warning about free variable.
(defvar chatgpt-shell-models)
(defvar chatgpt-shell-request-timeout)
(declare-function chatgpt-shell-crop-context "chatgpt-shell")
(declare-function chatgpt-shell--make-chatgpt-url "chatgpt-shell")
(declare-function chatgpt-shell-openai--user-assistant-messages "chatgpt-shell-openai")

(cl-defun chatgpt-shell-ollama-make-model (&key version
                                                short-version
                                                token-width
                                                context-window)
  "Create an Ollama model.

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
  `((:provider . "Ollama")
    (:label . "Ollama")
    (:version . ,version)
    (:short-version . ,short-version)
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:handler . chatgpt-shell-ollama--handle-ollama-command)
    (:filter . chatgpt-shell-ollama--extract-ollama-response)
    (:payload . chatgpt-shell-ollama-make-payload)
    (:url . chatgpt-shell-ollama--make-url)
    (:validate-command . chatgpt-shell-ollama--validate-command)
    (:icon . "ollama.png")))

(defcustom chatgpt-shell-ollama-api-url-base "http://localhost:11434"
  "Ollama API's base URL.

API url = base + path.

If you use Ollama through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defun chatgpt-shell-ollama-models ()
  "Build a list of Ollama LLM models available."
  ;; Context windows have been verified via ollama show <model> as of
  ;; 11/26/2024.
  (list (chatgpt-shell-ollama-make-model
         :version "gemma2:2b"
         :token-width 4
         :context-window 8192)
        (chatgpt-shell-ollama-make-model
         :version "llama3.2"
         :token-width 4
         :context-window 131072)
        (chatgpt-shell-ollama-make-model
         :version "llama3.2:1b"
         :token-width 4
         :context-window 131072)
        (chatgpt-shell-ollama-make-model
         :version "qwen2.5-coder"
         :token-width 4
         :context-window 32768)))

(defun chatgpt-shell-ollama--fetch-model-versions ()
  "Fetch available Ollama model versions (installed locally)."
  (mapcar (lambda (model)
            (string-remove-suffix ":latest" (map-elt model 'name)))
          (map-elt (shell-maker--json-parse-string
                    (map-elt (shell-maker-make-http-request
                              :async nil
                              :url (concat chatgpt-shell-ollama-api-url-base "/api/tags"))
                             :output))
                   'models)))

(defun chatgpt-shell-ollama--parse-token-width (quantization)
  "Parse token using QUANTIZATION."
  ;; e.g. "F16", "Q4_0", "MXFP4"
  (when (string-match "^[A-Z]+\\([1-9][0-9]*\\)" quantization)
    (string-to-number (match-string 1 quantization))))

(defun chatgpt-shell-ollama--fetch-model (version)
  "Fetch Ollama model details with VERSION."
  (let* ((data (shell-maker--json-parse-string
                (map-elt (shell-maker-make-http-request
                          :async nil
                          :url (concat chatgpt-shell-ollama-api-url-base "/api/show")
                          :data `((model . ,version)))
                         :output)))
         (token-width (let-alist data
                        (chatgpt-shell-ollama--parse-token-width
                         .details.quantization_level)))
         ;; The context length key depends on the name of the model. For qwen2,
         ;; it's at: model_info -> qwen2.context_length.
         (context-window (cdr (cl-find-if (lambda (cell)
                                            (string-suffix-p "context_length" (symbol-name (car cell))))
                                          (map-elt data 'model_info)))))
    (chatgpt-shell-ollama-make-model
     :version version
     :token-width token-width
     :context-window context-window)))

(cl-defun chatgpt-shell-ollama-load-models (&key override)
  "Query ollama for the locally installed models.

Queried models are added to `chatgpt-shell-models' unless a model
with the same name is already present.

By default, replace the ollama models in `chatgpt-shell-models'
locally installed ollama models.  When OVERRIDE is non-nil (interactively
with a prefix argument), replace all models with
locally installed ollama models."
  (interactive (list :override current-prefix-arg))
  (let* ((ollama-predicate (lambda (model)
                             (string= (map-elt model :provider) "Ollama")))
         ;; Find the index of the first ollama model so that the new ones will
         ;; be placed in the same part of the list.
         (ollama-index (or (cl-position-if ollama-predicate chatgpt-shell-models)
                           (length chatgpt-shell-models))))
    (setq chatgpt-shell-models (and (not override)
                                    (cl-remove-if ollama-predicate chatgpt-shell-models)))
    (let* ((existing-ollama-versions (mapcar (lambda (model)
                                               (map-elt model :version))
                                             (cl-remove-if-not ollama-predicate
                                                               chatgpt-shell-models)))
           (new-ollama-versions (cl-remove-if (lambda (version)
                                                (member version existing-ollama-versions))
                                              (chatgpt-shell-ollama--fetch-model-versions)))
           (new-ollama-models (mapcar #'chatgpt-shell-ollama--fetch-model new-ollama-versions)))
      (setq chatgpt-shell-models
            (append (seq-take chatgpt-shell-models ollama-index)
                    new-ollama-models
                    (seq-drop chatgpt-shell-models ollama-index)))
      (message "Added %d ollama model(s); kept %d existing ollama model(s)"
               (length new-ollama-models)
               (length existing-ollama-versions)))))

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
   :timeout chatgpt-shell-request-timeout
   :shell shell))

(cl-defun chatgpt-shell-ollama--make-url (&key _command _model _settings)
  "Create the API URL using MODEL and SETTINGS."
  (concat chatgpt-shell-ollama-api-url-base
          "/api/chat"))

(defun chatgpt-shell-ollama--extract-ollama-response (object)
  "Process Ollama response from OBJECT."
  (when (stringp object)
    (error "Please upgrade shell-maker to 0.79.1 or newer"))
  (if-let* ((whole (shell-maker--json-parse-string (map-elt object :pending)))
            (response (let-alist whole
                        .response)))
      (progn
        (setf (map-elt object :filtered) response)
        (setf (map-elt object :pending) nil)
        object)
    (if-let ((chunks (string-split (map-elt object :pending) "\n")))
        (let ((response))
          (mapc (lambda (chunk)
                  (let-alist (shell-maker--json-parse-string chunk)
                    (unless (string-empty-p .message.content)
                      (setq response (concat response .message.content)))))
                chunks)
          (setf (map-elt object :filtered) response)
          (setf (map-elt object :pending) nil)
          object)
      object)))

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

(defun chatgpt-shell-ollama--validate-command (_command model _settings)
  "Return error string if MODEL isn't valid."
  (unless (seq-contains-p (chatgpt-shell-ollama--fetch-model-versions)
                          (map-elt model :version))
    (format "  Local model \"%s\" not found.

  Try installing from the command line via:

    %% ollama pull %s

  Check out the [Ollama CLI reference](https://github.com/ollama/ollama?tab=readme-ov-file#cli-reference)

  Alternatively, fetch available models from Emacs via:

    M-x chatgpt-shell-ollama-load-models

  Then swap active model with:

    M-x chatgpt-shell-swap-system-prompt"
            (map-elt model :version)
            (map-elt model :version))))

(provide 'chatgpt-shell-ollama)

;;; chatgpt-shell-ollama.el ends here
