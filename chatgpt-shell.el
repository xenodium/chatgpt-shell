;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.3
;; Package-Requires: ((emacs "27.1")
;;                    (markdown-mode "2.5"))

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

;; Note: This is very much a proof of concept (and very rough!).  Much
;; of the code is based on `ielm'.
;;
;; You must set `chatgpt-shell-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.

;;; Code:

(require 'comint)
(require 'goto-addr)
(require 'json)
(require 'map)
(require 'markdown-mode)
(require 'seq)

(eval-when-compile
  (require 'cl-lib)
  (declare-function json-pretty-print "ext:json" (begin end &optional minimize)))

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-request-maker #'chatgpt-shell--async-curl-request
  "OpenAI key as a string or a function that loads and returns it."
  :type 'function
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-language-mapping '(("elisp" . "emacs-lisp")
                                            ("objective-c" . "objc")
                                            ("objectivec" . "objc")
                                            ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '()
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-chatgpt-model-version "gpt-3.5-turbo"
  "The used ChatGPT OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-dall-e-model-version "image-alpha-001"
  "The used DALL-E OpenAI model."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-temperature nil
  "What sampling temperature to use, between 0 and 2, or nil.

Higher values like 0.8 will make the output more random, while
lower values like 0.2 will make it more focused and
deterministic.  Value of nil will not pass this configuration to
the model.

See
https://platform.openai.com/docs/api-reference/completions\
/create#completions/create-temperature
for details."
  :type '(choice integer
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-transmitted-context-length nil
  "Controls the amount of context provided to chatGPT.

This context needs to be transmitted to the API on every request.
ChatGPT reads the provided context on every request, which will
consume more and more prompt tokens as your conversation grows.
Models do have a maximum token limit, however.

A value of nil will send full chat history (the full contents of
the comint buffer), to ChatGPT.

A value of 0 will not provide any context.  This is the cheapest
option, but ChatGPT can't look back on your conversation.

A value of 1 will send only the latest prompt-completion pair as
context.

A Value >1 will send that amount of prompt-completion pairs to
ChatGPT."
  :type '(choice integer
                 (const nil))
  :group 'chatgpt-shell)

(defvar chatgpt-shell--log-buffer-name "*chatgpt-shell-log*")

(defvar chatgpt-shell--input)

(defvar chatgpt-shell--current-request-id 0)

(defvar chatgpt-shell--show-invisible-markers nil)

(cl-defstruct
    chatgpt-shell-config
  prompt
  buffer-name
  process-name
  url
  request-data-maker
  response-extrator)

(defvar chatgpt-shell--chatgpt-config
  (make-chatgpt-shell-config
   :buffer-name "*chatgpt*"
   :process-name "chatgpt"
   :prompt "ChatGPT> "
   :url "https://api.openai.com/v1/chat/completions"
   :request-data-maker
   (lambda (commands-and-responses)
     (let ((request-data `((model . ,chatgpt-shell-chatgpt-model-version)
                           (messages . ,commands-and-responses))))
       (when chatgpt-shell-model-temperature
         (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
       request-data))
   :response-extrator #'chatgpt-shell--extract-chatgpt-response))

(defvar chatgpt-shell--dall-e-config
  (make-chatgpt-shell-config
   :buffer-name "*dalle*"
   :process-name "dalle"
   :prompt "DALL-E> "
   :url "https://api.openai.com/v1/images/generations"
   :request-data-maker
   (lambda (commands-and-responses)
     (let ((request-data `((model . ,chatgpt-shell-dall-e-model-version)
                           (prompt . ,(map-elt (aref commands-and-responses
                                                     (1- (length commands-and-responses)))
                                               'content)))))
       (when chatgpt-shell-model-temperature
         (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
       request-data))
   :response-extrator #'chatgpt-shell--extract-dall-e-response))

(defvar-local chatgpt-shell--busy nil)

(defvar-local chatgpt-shell--config nil)

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-shell-map)

(defconst chatgpt-shell-font-lock-keywords
  `(;; Markdown triple backticks source blocks
    ("\\(^\\(```\\)\\([^`\n]*\\)\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(^\\(```\\)$\\)"
     ;; (2) ``` (3) language (4) body (6) ```
     (0 (progn
          ;; Hide ```
          (overlay-put (make-overlay (match-beginning 2)
                                     (match-end 2)) 'invisible t)
          ;; Language box.
          (overlay-put (make-overlay (match-beginning 3)
                                     (match-end 3)) 'face '(:box t))
          ;; Additional newline after language box.
          (overlay-put (make-overlay (match-end 3)
                                     (1+ (match-end 3))) 'display "\n\n")
          ;; Hide ```
          (overlay-put (make-overlay (match-beginning 6)
                                     (match-end 6)) 'invisible t)
          ;; Show body
          (chatgpt-shell--fontify-source-block
           (buffer-substring (match-beginning 3)
                             (match-end 3))
           ;; body
           (match-beginning 4) (match-end 4))
          nil)))
    ;; Markdown single backticks
    ("`\\([^`\n]+\\)`"
     (1 'markdown-inline-code-face))))

(defvar chatgpt-shell-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'chatgpt-shell-return)
    (define-key map "\C-c\C-c" 'chatgpt-shell-interrupt)
    map)
  "Keymap for ChatGPT mode.")

(defalias 'chatgpt-shell-clear-buffer 'comint-clear-buffer)
(defalias 'chatgpt-shell-explain-code 'chatgpt-shell-describe-code)

;;;###autoload
(defun chatgpt-shell ()
  "Start a ChatGPT shell."
  (interactive)
  (let ((old-point)
        (buf-name "*chatgpt*"))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create "*chatgpt*")
        (setq-local chatgpt-shell--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (inferior-chatgpt-mode)
        (chatgpt-shell--initialize chatgpt-shell--chatgpt-config)))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))

;;;###autoload
(defun dall-e-shell ()
  "Start a ChatGPT shell."
  (interactive)
  (let ((old-point)
        (buf-name "*dalle*"))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create "*dalle*")
        (setq-local chatgpt-shell--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (inferior-chatgpt-mode)
        (chatgpt-shell--initialize
         chatgpt-shell--dall-e-config)))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))

(define-derived-mode inferior-chatgpt-mode comint-mode "CHATGPT"
  "Major mode for interactively evaluating ChatGPT prompts.
Uses the interface provided by `comint-mode'"
  nil)

(defun chatgpt-shell--buffer (config)
  "Get buffer from CONFIG."
  (get-buffer-create (chatgpt-shell-config-buffer-name chatgpt-shell--config)))

(defun chatgpt-shell--initialize (config)
  "Initialize shell using CONFIG."
  (setq-local chatgpt-shell--config config)
  (visual-line-mode +1)
  (goto-address-mode +1)
  (setq comint-prompt-regexp
        (concat "^" (regexp-quote
                     (chatgpt-shell-config-prompt chatgpt-shell--config))))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'chatgpt-shell--input-sender)
  (setq comint-process-echoes nil)
  (setq-local chatgpt-shell--prompt-internal
              (chatgpt-shell-config-prompt chatgpt-shell--config))
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'chatgpt-shell--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (unless (or (comint-check-proc (chatgpt-shell--buffer chatgpt-shell--config))
              (get-buffer-process (chatgpt-shell--buffer chatgpt-shell--config)))
    (condition-case nil
        (start-process (chatgpt-shell-config-process-name chatgpt-shell--config)
                       (chatgpt-shell--buffer chatgpt-shell--config) "hexl")
      (file-error (start-process
                   (chatgpt-shell-config-process-name chatgpt-shell--config)
                   (chatgpt-shell--buffer chatgpt-shell--config) "cat")))
    (set-process-query-on-exit-flag (chatgpt-shell--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (chatgpt-shell--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt-internal)
    (set-marker comint-last-input-start (chatgpt-shell--pm))
    (set-process-filter (get-buffer-process
                         (chatgpt-shell--buffer chatgpt-shell--config))
                        'comint-output-filter))

  (font-lock-add-keywords nil chatgpt-shell-font-lock-keywords))

(defun chatgpt-shell--fontify-source-block (lang start end)
  "Fontify using LANG from START to END."
  (let ((lang-mode (intern (concat (or
                                    (map-elt chatgpt-shell-language-mapping
                                             (downcase (string-trim lang)))
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties start end))
        (buf (chatgpt-shell--buffer chatgpt-shell--config))
        (pos 0)
        (props)
        (overlay)
        (propertized-text))
    ;; FIXME: Find a more reliable way of highlighting syntax.
    ;; (remove-text-properties start end '(face nil))
    (if (fboundp lang-mode)
        (progn
          (setq propertized-text
                (with-current-buffer
                    (get-buffer-create
                     (format " *chatgpt-shell-fontification:%s*" lang-mode))
                  (let ((inhibit-modification-hooks nil))
                    (erase-buffer)
                    ;; Additional space ensures property change.
                    (insert string " ")
                    (funcall lang-mode)
                    (font-lock-ensure))
                  (buffer-string)))
          (while (< pos (length propertized-text))
            (setq props (text-properties-at pos propertized-text))
            (setq overlay (make-overlay (+ start pos)
                                        (+ start (1+ pos))
                                        buf))
            ;; (set-text-properties (+ start pos)
            ;;                      (+ start (1+ pos))
            ;;                      props)
            (overlay-put overlay 'face (plist-get props 'face))
            (setq pos (1+ pos)))
          ;; (overlay-put (make-overlay start end buf)
          )
      (set-text-properties start end
                               '(face 'markdown-pre-face)))))

(defun chatgpt-shell-chatgpt-prompt ()
  "Make a ChatGPT request from the minibuffer."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (read-string (chatgpt-shell-config-prompt
                 chatgpt-shell--chatgpt-config)))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (chatgpt-shell--send-input))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "What does the following code do?\n\n"
           (buffer-substring (region-beginning) (region-end))))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-eshell-whats-wrong-with-last-command ()
  "Ask ChatGPT what's wrong with the last eshell command."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "What's wrong with this command?\n\n"
           (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
           "\n\n"
           (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-eshell-summarize-last-command-output ()
  "Ask ChatGPT to summarize the last command output."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "Summarize the output of the following command: \n\n"
           (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
           "\n\n"
           (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-send-region ()
  "Send region to ChatGPT."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (buffer-substring (region-beginning) (region-end)) nil t))

(defun chatgpt-shell-send-to-buffer (text &optional submit save-excursion)
  "Send TEXT to *chatgpt* buffer.
Set SUBMIT to automatically submit to ChatGPT.
Set SAVE-EXCURSION to prevent point from moving."
  (chatgpt-shell)
  (switch-to-buffer (get-buffer-create "*chatgpt*"))
  (with-current-buffer (get-buffer-create "*chatgpt*")
    (when chatgpt-shell--busy
      (chatgpt-shell-interrupt))
    (goto-char (point-max))
    (if save-excursion
        (save-excursion
          (insert text))
      (insert text))
    (when submit
      (chatgpt-shell--send-input))))

(defun chatgpt-shell-interrupt ()
  "Interrupt current request."
  (interactive)
  (with-current-buffer (chatgpt-shell--buffer chatgpt-shell--config)
    ;; Increment id, so in-flight request is ignored.
    (chatgpt-shell--increment-request-id)
    (comint-send-input)
    (goto-char (point-max))
    (comint-output-filter (chatgpt-shell--process)
                          (concat (propertize "<gpt-end-of-prompt>\n<gpt-ignored-response>"
                                              'invisible (not chatgpt-shell--show-invisible-markers))
                                  "\n"
                                  chatgpt-shell--prompt-internal))
    (setq chatgpt-shell--busy nil)
    (message "interrupted!")))

(defun chatgpt-shell--eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (unless chatgpt-shell--busy
    (setq chatgpt-shell--busy t)
    (cond
     ((string-equal "clear" (string-trim input-string))
      (call-interactively 'comint-clear-buffer)
      (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt-internal)
      (setq chatgpt-shell--busy nil))
     ((not (chatgpt-shell--curl-version-supported))
      (chatgpt-shell--write-reply "You need curl version 7.67 or newer.")
      (setq chatgpt-shell--busy nil))
     ((not chatgpt-shell-openai-key)
      (chatgpt-shell--write-reply
       "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")" t)
      (setq chatgpt-shell--busy nil))
     ((string-empty-p (string-trim input-string))
      (comint-output-filter (chatgpt-shell--process)
                            (concat "\n" chatgpt-shell--prompt-internal))
      (setq chatgpt-shell--busy nil))
     (t
      ;; For viewing prompt delimiter (used to handle multiline prompts).
      ;; (comint-output-filter (chatgpt-shell--process) "<gpt-end-of-prompt>")
      (comint-output-filter (chatgpt-shell--process)
                            (propertize "<gpt-end-of-prompt>"
                                        'invisible (not chatgpt-shell--show-invisible-markers)))
      (when-let ((key (cond ((stringp chatgpt-shell-openai-key)
                             chatgpt-shell-openai-key)
                            ((functionp chatgpt-shell-openai-key)
                             (condition-case err
                                 (funcall chatgpt-shell-openai-key)
                               (error
                                (chatgpt-shell--write-reply (error-message-string err) t)
                                (comint-output-filter (chatgpt-shell--process)
                                                      (propertize "\n<gpt-ignored-response>"
                                                                  'invisible (not chatgpt-shell--show-invisible-markers)))
                                (setq chatgpt-shell--busy nil)
                                nil))))))
        (funcall chatgpt-shell-request-maker
                 key (chatgpt-shell-config-url chatgpt-shell--config)
                 (funcall (chatgpt-shell-config-request-data-maker chatgpt-shell--config)
                          (vconcat
                           (last (chatgpt-shell--extract-commands-and-responses)
                                 (chatgpt-shell--unpaired-length
                                  chatgpt-shell-transmitted-context-length))))
                 (chatgpt-shell-config-response-extrator chatgpt-shell--config)
                 (lambda (response)
                   (if response
                       (chatgpt-shell--write-reply response)
                     (chatgpt-shell--write-reply "Error: that's all is known" t))
                   (setq chatgpt-shell--busy nil))
                 (lambda (error)
                   (chatgpt-shell--write-reply error t)
                   (setq chatgpt-shell--busy nil))))))))

(defun chatgpt-shell--unpaired-length (length)
  "Expand LENGTH to include paired responses.

Each request has a response, so double LENGTH if set.

Add one for current request (without response).

If no LENGTH set, use 2048."
  (if length
      (1+ (* 2 length))
    2048))

(defun chatgpt-shell--async-curl-request (key url request-data response-extractor callback error-callback)
  "Make request via `curl' using KEY URL REQUEST-DATA RESPONSE-EXTRACTOR CALLBACK and ERROR-CALLBACK."
  (chatgpt-shell--async-shell-command
   (chatgpt-shell--make-curl-request-command-list
    key url request-data)
   response-extractor
   callback
   error-callback))

(defun chatgpt-shell--async-shell-command (command response-extractor callback error-callback)
  "Run shell COMMAND asynchronously.
Calls RESPONSE-EXTRACTOR to extract the response and feeds it to
CALLBACK or ERROR-CALLBACK accordingly."
  (let* ((buffer (chatgpt-shell--buffer chatgpt-shell--config))
         (request-id (chatgpt-shell--increment-request-id))
         (output-buffer (generate-new-buffer " *temp*"))
         (request-process (condition-case err
                              (apply #'start-process (append (list "ChatGPT" (buffer-name output-buffer))
                                                             command))
                            (error
                             (funcall error-callback (error-message-string err))
                             nil)))
         (process-connection-type nil))
    (when request-process
      (chatgpt-shell--write-output-to-log-buffer "// Request\n\n")
      (chatgpt-shell--write-output-to-log-buffer (string-join command " "))
      (chatgpt-shell--write-output-to-log-buffer "\n\n")
      (set-process-sentinel
       request-process
       (lambda (process _event)
         (let ((active (eq request-id chatgpt-shell--current-request-id))
               (output (with-current-buffer (process-buffer process)
                         (buffer-string))))
           (chatgpt-shell--write-output-to-log-buffer
            (format "// Response (%s)\n\n" (if active "active" "inactive")))
           (chatgpt-shell--write-output-to-log-buffer output)
           (chatgpt-shell--write-output-to-log-buffer "\n\n")
           (with-current-buffer buffer
             (when active
               (if (= (process-exit-status process) 0)
                   (funcall callback
                            (funcall response-extractor output))
                 (funcall error-callback output))
               ;; Only message if not active buffer.
               (unless (eq (chatgpt-shell--buffer chatgpt-shell--config)
                           (window-buffer (selected-window)))
                 (message "%s responded"
                          (buffer-name
                           (chatgpt-shell--buffer chatgpt-shell--config))))))
           (kill-buffer output-buffer)))))))

(defun chatgpt-shell--increment-request-id ()
  "Increment `chatgpt-shell--current-request-id'."
  (if (= chatgpt-shell--current-request-id most-positive-fixnum)
      (setq chatgpt-shell--current-request-id 0)
    (setq chatgpt-shell--current-request-id (1+ chatgpt-shell--current-request-id))))

(defun chatgpt-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark
               (get-buffer-process
                (chatgpt-shell--buffer chatgpt-shell--config))) pos))

(defun chatgpt-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process
                 (chatgpt-shell--buffer chatgpt-shell--config))))

(defun chatgpt-shell--input-sender (_proc input)
  "Set the variable `chatgpt-shell--input' to INPUT.
Used by `chatgpt-shell--send-input's call."
  (setq chatgpt-shell--input input))

(defun chatgpt-shell--send-input ()
  "Send text after the prompt."
  (interactive)
  (let (chatgpt-shell--input)
    (comint-send-input)
    (chatgpt-shell--eval-input chatgpt-shell--input)))

(defun chatgpt-shell--write-reply (reply &optional failed)
  "Write REPLY to prompt.  Set FAILED to record failure."
  (comint-output-filter (chatgpt-shell--process)
                        (concat "\n"
                                (string-trim reply)
                                (if failed
                                    (propertize "\n<gpt-ignored-response>"
                                                'invisible (not chatgpt-shell--show-invisible-markers))
                                  "")
                                "\n\n"
                                chatgpt-shell--prompt-internal)))

(defun chatgpt-shell--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun chatgpt-shell--make-curl-request-command-list (key url request-data)
  "Build ChatGPT curl command list using KEY URL and REQUEST-DATA."
  (list "curl" url
        "--fail" "--no-progress-meter" "-m" "30"
        "-H" "Content-Type: application/json"
        "-H" (format "Authorization: Bearer %s" key)
        "-d" (chatgpt-shell--json-encode request-data)))

(defun chatgpt-shell--json-encode (obj)
  "Serialize OBJ to json. Use fallback if `json-serialize' isn't available."
  (if (fboundp 'json-serialize)
      (json-serialize obj)
    (json-encode obj)))

(defun chatgpt-shell--curl-version-supported ()
  "Return t if curl version is 7.67 or newer, nil otherwise."
  (let* ((curl-error-redirect (if (eq system-type (or 'windows-nt 'ms-dos)) "2> NUL" "2>/dev/null"))
         (curl-version-string (shell-command-to-string (concat "curl --version " curl-error-redirect))))
    (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" curl-version-string)
      (let ((version (match-string 1 curl-version-string)))
        (version<= "7.67" version)))))

(defun chatgpt-shell--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (json-parse-error nil))
    (condition-case err
        (json-read-from-string json)
      (error nil))))

(defun chatgpt-shell--extract-chatgpt-response (json)
  "Extract ChatGPT response from JSON."
  (when-let (parsed (chatgpt-shell--json-parse-string json))
    (string-trim
     (map-elt (map-elt (seq-first (map-elt parsed 'choices))
                       'message)
              'content))))

(defun chatgpt-shell--find-string-in-buffer (buffer search-str)
  "Find SEARCH-STR in BUFFER and return a cons cell with start and end positions.  Return nil if not found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward search-str nil t)
        (cons (match-beginning 0) (match-end 0))))))

(defun chatgpt-shell--download-image (url filename callback error-callback)
  "Download URL to FILENAME.  Invoke CALLBACK on success.  ERROR-CALLBACK otherwise."
  ;; Ensure sync failures can be handled in next runloop.
  (run-with-idle-timer 0 nil
                       (lambda ()
                         (let* ((path (expand-file-name filename temporary-file-directory))
                                (output-buffer (generate-new-buffer " *temp*"))
                                (request-process
                                 (condition-case err
                                     (start-process "curl" (buffer-name output-buffer)
                                                    "curl" "-o" path
                                                    url)
                                   (error
                                    (funcall error-callback (error-message-string err))
                                    nil)))
                                (process-connection-type nil))
                           (when request-process
                             (set-process-sentinel
                              request-process
                              (lambda (process _event)
                                (let ((output (with-current-buffer (process-buffer process)
                                                (buffer-string))))
                                  (if (= (process-exit-status process) 0)
                                      (funcall callback path)
                                    (funcall error-callback output))
                                  (kill-buffer output-buffer)))))))))

(defun chatgpt-shell--extract-commands-and-responses ()
  "Extract all command and responses in buffer."
  (let ((result))
    (with-current-buffer (chatgpt-shell--buffer chatgpt-shell--config)
      (mapc (lambda (item)
              (let* ((values (split-string item "<gpt-end-of-prompt>"))
                     (lines (split-string item "\n"))
                     (prompt (string-trim (nth 0 values)))
                     (response (string-trim (progn
                                              (if (> (length values) 1)
                                                  (nth 1 values)
                                                (string-join
                                                 (cdr lines) "\n"))))))
                (unless (string-match "<gpt-ignored-response>" response)
                  (when (not (string-empty-p prompt))
                    (push (list (cons 'role "user")
                                (cons 'content prompt)) result))
                  (when (not (string-empty-p response))
                    (push (list (cons 'role "system")
                                (cons 'content response)) result)))))
            (split-string (substring-no-properties (buffer-string))
                          chatgpt-shell--prompt-internal)))
    (nreverse result)))

(defun chatgpt-shell--write-output-to-log-buffer (output)
  "Write curl process OUTPUT to log buffer.

Create the log buffer if it does not exist.  Pretty print output
if `json' is available."
  (let ((buffer (get-buffer chatgpt-shell--log-buffer-name)))
    (unless buffer
      ;; Create buffer
      (setq buffer (get-buffer-create chatgpt-shell--log-buffer-name))
      (with-current-buffer buffer
        ;; Use `js-json-mode' if available, fall back to `js-mode'.
        (if (fboundp #'js-json-mode)
            (js-json-mode)
          (js-mode))))
    (with-current-buffer buffer
      (let ((beginning-of-input (goto-char (point-max))))
        (insert output)
        (when (and (require 'json nil t)
                   (ignore-errors (chatgpt-shell--json-parse-string output)))
          (json-pretty-print beginning-of-input (point)))))))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (chatgpt-shell--buffer chatgpt-shell--config)))

(defun chatgpt-shell--extract-dall-e-response (json)
  "Extract DALL-E response from JSON."
  (when-let ((buffer (chatgpt-shell--buffer chatgpt-shell--config))
             (parsed (chatgpt-shell--json-parse-string json))
             (url (map-elt (seq-first (map-elt parsed 'data))
                           'url))
             (created (concat (number-to-string (map-elt parsed 'created))
                              ".png")))
    (chatgpt-shell--download-image
     url created
     (lambda (path)
       (let* ((loc (chatgpt-shell--find-string-in-buffer
                    buffer
                    created))
              (start (car loc))
              (end (cdr loc)))
         (with-current-buffer buffer
           (remove-text-properties start end '(face nil))
           (add-text-properties
            start end
            `(display ,(create-image path nil nil :width 400)))
           (put-text-property start end
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map (kbd "RET")
                                          (lambda () (interactive)
                                            (find-file path)))
                                        map)))))
     (lambda (error)
       (when-let* ((loc (chatgpt-shell--find-string-in-buffer
                         buffer
                         created))
                   (start (car loc))
                   (end (cdr loc)))
         (with-current-buffer buffer
           (remove-text-properties start end '(face nil))
           (add-text-properties start end `(display ,error))))))
    (propertize created 'display "[downloading...]")))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
