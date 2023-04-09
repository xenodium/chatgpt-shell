;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.13.1
;; Package-Requires: ((emacs "27.1"))

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

(require 'esh-mode)
(require 'eshell)
(require 'ielm)
(require 'shell-maker)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-request-timeout 60
  "How long to wait for a request to time out."
  :type 'integer
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-default-prompts
  '("Write a unit test for the following code:"
    "Refactor the following code so that "
    "Summarize the output of the following command:"
    "What's wrong with this command?"
    "Explain what the following code does:")
  "List of default prompts to choose from."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-on-command-finished-function nil
  "Function to automatically execute after last command output.

This is useful if you'd like to automatically handle or suggest things
post execution.

For example:

\(setq `chatgpt-shell-on-command-finished-function'
   (lambda (command output)
     (message \"Command: %s\" command)
     (message \"Output: %s\" output)))"
  :type 'function
  :group 'shell-maker)

(defvaralias 'chatgpt-shell-display-function 'shell-maker-display-function)

(defvaralias 'chatgpt-shell-read-string-function 'shell-maker-read-string-function)

(defalias 'chatgpt-shell-save-session-transcript 'shell-maker-save-session-transcript)

(defvar chatgpt-shell--prompt-history nil)

(defcustom chatgpt-shell-language-mapping '(("elisp" . "emacs-lisp")
                                            ("objective-c" . "objc")
                                            ("objectivec" . "objc")
                                            ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '(repeat (cons string string))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version "gpt-3.5-turbo"
  "The used ChatGPT OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
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
  :type '(choice (float :tag "Float")
                 (const :tag "Nil" nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-system-prompt nil
  "The system message helps set the behavior of the assistant.

For example: You are a helpful assistant that translates English to French.

See https://platform.openai.com/docs/guides/chat/introduction"
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-streaming t
  "Whether or not to stream ChatGPT responses (experimental)."
  :type 'boolean
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
  :type '(choice (integer :tag "Integer value")
                 (const :tag "Not set" nil))
  :group 'chatgpt-shell)

(defvar chatgpt-shell--url "https://api.openai.com/v1/chat/completions")

(defvar chatgpt-shell--config
  (make-shell-maker-config
   :name "ChatGPT"
   :validate-command
   (lambda (_command)
     (unless chatgpt-shell-openai-key
       "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")"))
   :execute-command
   (lambda (_command history callback error-callback)
     (shell-maker-async-shell-command
      (chatgpt-shell--make-curl-request-command-list
       (chatgpt-shell--make-payload history))
      chatgpt-shell-streaming
      #'chatgpt-shell--extract-chatgpt-response
      callback
      error-callback))
   :on-command-finished
   (lambda (command output)
     (chatgpt-shell--put-source-block-overlays)
     (when chatgpt-shell-on-command-finished-function
       (funcall chatgpt-shell-on-command-finished-function command output)))
   :redact-log-output
   (lambda (output)
     (if (chatgpt-shell-openai-key)
         (string-replace (chatgpt-shell-openai-key)
                         "SK-REDACTED-OPENAI-KEY"
                         output)
       output))))

(defalias 'chatgpt-shell-clear-buffer 'comint-clear-buffer)

(defalias 'chatgpt-shell-explain-code 'chatgpt-shell-describe-code)

;; Aliasing enables editing as text in babel.
(defalias 'chatgpt-shell-mode #'text-mode)

;;;###autoload
(defun chatgpt-shell ()
  "Start a ChatGPT shell."
  (interactive)
  (shell-maker-start chatgpt-shell--config)
  (define-key shell-maker-mode-map "\C-\M-h"
    #'chatgpt-shell-mark-at-point-dwim))

(defun chatgpt-shell-mark-at-point-dwim ()
  "Mark source block if at point.  Mark all output otherwise."
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (progn
        (set-mark (cdr block))
        (goto-char (car block)))
    (shell-maker-mark-output)))

(defun chatgpt-shell-markdown-block-at-point ()
  "Markdown start/end cons if point at block.  nil otherwise."
  (save-excursion
    (save-restriction
      (shell-maker-narrow-to-prompt)
      (let ((start (save-excursion
                     (when (re-search-backward "^```" nil t)
                       (end-of-line)
                       (point))))
            (end (save-excursion
                   (when (re-search-forward "^```" nil t)
                     (forward-line 0)
                     (point)))))
        (when (and start end
                   (> (point) start)
                   (< (point) end))
          (cons start end))))))

(defun chatgpt-shell--inline-codes ()
  "Get a list of all inline codess in buffer."
  (let ((codes '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "`\\([^`\n]+\\)`"
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push
           (list
            'body (cons (match-beginning 1) (match-end 1))) codes))))
    (nreverse codes)))

(defun chatgpt-shell--source-blocks ()
  "Get a list of all source blocks in buffer."
  (let ((markdown-blocks '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
                  (group (zero-or-more (or alphanumeric "-"))) ;; language
                  (zero-or-more whitespace)
                  (one-or-more "\n")
                  (group (*? anychar)) ;; body
                  (one-or-more "\n")
                  (group "```"))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push
           (list
            'start (cons (match-beginning 1)
                         (match-end 1))
            'end (cons (match-beginning 4)
                       (match-end 4))
            'language (when (and (match-beginning 2)
                                 (match-end 2))
                        (cons (match-beginning 2)
                              (match-end 2)))
            'body (cons (match-beginning 3) (match-end 3))) markdown-blocks))))
    (nreverse markdown-blocks)))

(defun chatgpt-shell-prompt ()
  "Make a ChatGPT request from the minibuffer.

If region is active, append to prompt."
  (interactive)
  (unless chatgpt-shell--prompt-history
    (setq chatgpt-shell--prompt-history
          chatgpt-shell-default-prompts))
  (let ((prompt (funcall shell-maker-read-string-function
                         (concat
                          (if (region-active-p)
                              "[appending region] "
                            "")
                          (shell-maker-prompt
                           chatgpt-shell--config))
                         'chatgpt-shell--prompt-history)))
    (when (region-active-p)
      (setq prompt (concat prompt "\n\n"
                           (buffer-substring (region-beginning) (region-end)))))
    (chatgpt-shell-send-to-buffer prompt)
    (shell-maker--send-input)))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
  (chatgpt-shell-send-to-buffer
   (concat "What does the following code do?\n\n"
           (buffer-substring (region-beginning) (region-end))))
  (shell-maker--send-input))

(defun chatgpt-shell-send-region-with-header (header)
  "Send text with HEADER from region using ChatGPT."
  (chatgpt-shell-send-to-buffer
   (concat header
           "\n\n"
           (buffer-substring (region-beginning) (region-end)))))

(defun chatgpt-shell-refactory-code ()
  "Refactoring code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header "Please help me refactor the following code. Please reply with the refactoring explanation in English, refactored code, and diff between two versions. Please ignore the comments and strings in the code during the refactoring. If the code remains unchanged after refactoring, please say 'No need to refactor'."))

(defun chatgpt-shell-generate-unit-test ()
  "Generate unit-test for the code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header "Please help me generate unit-test following function:"))

(defun chatgpt-shell-proofreading-doc ()
  "Proofread English from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header "Please help me proofread the following paragraph with English:"))

(defun chatgpt-shell-eshell-whats-wrong-with-last-command ()
  "Ask ChatGPT what's wrong with the last eshell command."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "What's wrong with this command?\n\n"
           (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
           "\n\n"
           (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))
  (shell-maker--send-input))

(defun chatgpt-shell-eshell-summarize-last-command-output ()
  "Ask ChatGPT to summarize the last command output."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "Summarize the output of the following command: \n\n"
           (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
           "\n\n"
           (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))
  (shell-maker--send-input))

(defun chatgpt-shell-send-region (review)
  "Send region to ChatGPT.
With prefix REVIEW prompt before sending to ChatGPT."
  (interactive "P")
  (unless (region-active-p)
    (user-error "No region active"))
  (chatgpt-shell-send-to-buffer
   (if review
       (concat "\n\n" (buffer-substring (region-beginning) (region-end)))
     (buffer-substring (region-beginning) (region-end))) review))

(defun chatgpt-shell-send-and-review-region ()
  "Send region to ChatGPT, review before submitting."
  (interactive)
  (chatgpt-shell-send-region t))

(defun chatgpt-shell-send-to-buffer (text &optional review)
  "Send TEXT to *chatgpt* buffer.
Set REVIEW to make changes before submitting to ChatGPT.
Set SAVE-EXCURSION to prevent point from moving."
  (chatgpt-shell)
  (with-selected-window
      (get-buffer-window (get-buffer-create "*chatgpt*"))
    (when shell-maker--busy
      (shell-maker-interrupt))
    (goto-char (point-max))
    (if review
        (save-excursion
          (insert text))
      (insert text)
      (shell-maker--send-input))))

(defun chatgpt-shell-send-to-ielm-buffer (text &optional execute save-excursion)
  "Send TEXT to *ielm* buffer.
Set EXECUTE to automatically execute.
Set SAVE-EXCURSION to prevent point from moving."
  (ielm)
  (with-current-buffer (get-buffer-create "*ielm*")
    (goto-char (point-max))
    (if save-excursion
        (save-excursion
          (insert text))
      (insert text))
    (when execute
      (ielm-return))))

(defun chatgpt-shell-parse-elisp-code (code)
  "Parse emacs-lisp CODE and return a list of expressions."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let (sexps)
      (while (not (eobp))
        (condition-case nil
            (push (read (current-buffer)) sexps)
          (error nil)))
      (reverse sexps))))

(defun chatgpt-shell-split-elisp-expressions (code)
  "Split emacs-lisp CODE into a list of stringified expressions."
  (mapcar
   (lambda (form)
     (prin1-to-string form))
   (chatgpt-shell-parse-elisp-code code)))

(defun chatgpt-shell--markdown-source-blocks (text)
  "Find Markdown code blocks with language labels in TEXT."
  (let (blocks)
    (while (string-match
            (rx bol "```" (zero-or-more space) (group (one-or-more (or alpha "-")))
                (group (*? anything))
                "```") text)
      (setq blocks (cons (cons (match-string 1 text)
                               (match-string 2 text)) blocks))
      (setq text (substring text (match-end 0))))
    (reverse blocks)))

(defun chatgpt-shell-post-messages (messages &optional version callback error-callback)
  "Make a single ChatGPT request with MESSAGES.
Optionally pass model VERSION, CALLBACK, and ERROR-CALLBACK.

If CALLBACK or ERROR-CALLBACK are missing, execute synchronously.

For example:

\(chatgpt-shell-post-messages
 `(((role . \"user\")
    (content . \"hello\")))
 \"gpt-3.5-turbo\"
 (lambda (response)
   (message \"%s\" response))
 (lambda (error)
   (message \"%s\" error)))"
  (if (and callback error-callback)
      (with-temp-buffer
        (setq-local shell-maker-config
                    chatgpt-shell--config)
        (shell-maker-async-shell-command
         (chatgpt-shell--make-curl-request-command-list
          (let ((request-data `((model . ,(or version
                                              chatgpt-shell-model-version))
                                (messages . ,(vconcat ;; Vector for json
                                              messages)))))
            (when chatgpt-shell-model-temperature
              (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
            request-data))
         nil ;; streaming
         #'chatgpt-shell--extract-chatgpt-response
         callback
         error-callback))
    (with-temp-buffer
      (setq-local shell-maker-config
                  chatgpt-shell--config)
      (let* ((buffer (current-buffer))
             (command
              (chatgpt-shell--make-curl-request-command-list
               (let ((request-data `((model . ,(or version
                                                   chatgpt-shell-model-version))
                                     (messages . ,(vconcat ;; Vector for json
                                                   messages)))))
                 (when chatgpt-shell-model-temperature
                   (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
                 request-data)))
             (_status (apply #'call-process (seq-first command) nil buffer nil (cdr command))))
        (chatgpt-shell--extract-chatgpt-response
         (buffer-substring-no-properties
	  (point-min)
	  (point-max)))))))

(defun chatgpt-shell-post-prompt (prompt &optional version callback error-callback)
  "Make a single ChatGPT request with PROMPT.
Optioally pass model VERSION, CALLBACK, and ERROR-CALLBACK.

If CALLBACK or ERROR-CALLBACK are missing, execute synchronously.

For example:

\(chatgpt-shell-request-oneof-prompt
 \"hello\"
 \"gpt-3.5-turbo\"
 (lambda (response)
   (message \"%s\" response))
 (lambda (error)
   (message \"%s\" error)))"
  (chatgpt-shell-post-messages `(((role . "user")
                                  (content . ,prompt)))
                               version
                               callback error-callback))

(defun chatgpt-shell-openai-key ()
  "Get the ChatGPT key."
  (cond ((stringp chatgpt-shell-openai-key)
         chatgpt-shell-openai-key)
        ((functionp chatgpt-shell-openai-key)
         (funcall chatgpt-shell-openai-key))
        (t
         nil)))

(defun chatgpt-shell--make-curl-request-command-list (request-data)
  "Build ChatGPT curl command list using REQUEST-DATA."
  (list "curl" chatgpt-shell--url
        "--fail-with-body"
        "--no-progress-meter"
        "-m" (number-to-string chatgpt-shell-request-timeout)
        "-H" "Content-Type: application/json"
        "-H" (format "Authorization: Bearer %s"
                     (cond ((stringp chatgpt-shell-openai-key)
                            chatgpt-shell-openai-key)
                           ((functionp chatgpt-shell-openai-key)
                            (condition-case _err
                                (funcall chatgpt-shell-openai-key)
                              (error
                               "KEY-NOT-FOUND")))))
        "-d" (shell-maker--json-encode request-data)))

(defun chatgpt-shell--make-payload (history)
  "Create the request payload from HISTORY."
  (setq history
        (vconcat ;; Vector for json
         (chatgpt-shell--user-assistant-messages
          (last history
                (chatgpt-shell--unpaired-length
                 chatgpt-shell-transmitted-context-length)))))
  (let ((request-data `((model . ,chatgpt-shell-model-version)
                        (messages . ,(if chatgpt-shell-system-prompt
                                         (vconcat ;; Vector for json
                                          (list
                                           (list
                                            (cons 'role "system")
                                            (cons 'content chatgpt-shell-system-prompt)))
                                          history)
                                       history)))))
    (when chatgpt-shell-model-temperature
      (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
    (when chatgpt-shell-streaming
      (push `(stream . t) request-data))
    request-data))

(defun chatgpt-shell--extract-chatgpt-response (json)
  "Extract ChatGPT response from JSON."
  (if (eq (type-of json) 'cons)
      (let-alist json ;; already parsed
        (or (let-alist (seq-first .choices)
              (or .delta.content
                  .message.content))
            .error.message
            ""))
    (if-let (parsed (shell-maker--json-parse-string json))
        (string-trim
         (let-alist parsed
           (let-alist (seq-first .choices)
             .message.content)))
      (if-let (parsed-error (shell-maker--json-parse-string-filtering
                             json "^curl:.*\n?"))
          (let-alist parsed-error
            .error.message)))))

;; FIXME: Make shell agnostic or move to chatgpt-shell.
(defun chatgpt-shell-restore-session-from-transcript ()
  "Restore session from transcript.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let* ((path (read-file-name "Restore from: " nil nil t))
         (prompt (shell-maker-prompt shell-maker-config))
         (history (with-temp-buffer
                                   (insert-file-contents path)
                                   (chatgpt-shell--extract-history
                                    (buffer-string)
                                    prompt)))
         (execute-command (shell-maker-config-execute-command
                         shell-maker-config))
         (validate-command (shell-maker-config-validate-command
                         shell-maker-config))
         (command)
         (response)
         (failed))
    ;; Momentarily overrides request handling to replay all commands
    ;; read from file so comint treats all commands/outputs like
    ;; any other command.
    (unwind-protect
        (progn
          (setf (shell-maker-config-validate-command shell-maker-config) nil)
          (setf (shell-maker-config-execute-command shell-maker-config)
                (lambda (_command _history callback _error-callback)
                  (setq response (car history))
                  (setq history (cdr history))
                  (when response
                    (unless (string-equal (map-elt response 'role)
                                          "assistant")
                      (setq failed t)
                      (user-error "Invalid transcript"))
                    (funcall callback (map-elt response 'content) nil)
                    (setq command (car history))
                    (setq history (cdr history))
                    (when command
                      (insert (map-elt command 'content))
                      (shell-maker--send-input)))))
          (goto-char (point-max))
          (comint-clear-buffer)
          (setq command (car history))
          (setq history (cdr history))
          (when command
            (unless (string-equal (map-elt command 'role)
                                  "user")
              (setq failed t)
              (user-error "Invalid transcript"))
            (insert (map-elt command 'content))
            (shell-maker--send-input)))
      (if failed
          (setq shell-maker--file nil)
        (setq shell-maker--file path))
      (setq shell-maker--busy nil)
      (setf (shell-maker-config-validate-command shell-maker-config)
            validate-command)
      (setf (shell-maker-config-execute-command shell-maker-config)
            execute-command))))

(defun chatgpt-shell--fontify-source-block (quotes1-start quotes1-end lang
lang-start lang-end body-start body-end quotes2-start quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (overlay-put (make-overlay quotes1-start
                             quotes1-end) 'invisible 'chatgpt-shell)
  (overlay-put (make-overlay quotes2-start
                             quotes2-end) 'invisible 'chatgpt-shell)
  (unless (eq lang-start lang-end)
    (overlay-put (make-overlay lang-start
                               lang-end) 'face '(:box t))
    (overlay-put (make-overlay lang-end
                               (1+ lang-end)) 'display "\n\n"))
  (let ((lang-mode (intern (concat (or
                                    (map-elt chatgpt-shell-language-mapping
                                             (downcase (string-trim lang)))
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties body-start body-end))
        (buf (shell-maker-buffer shell-maker-config))
        (pos 0)
        (props)
        (overlay)
        (propertized-text))
    (if (fboundp lang-mode)
        (progn
          (setq propertized-text
                (with-current-buffer
                    (get-buffer-create
                     (format " *chatgpt-shell-fontification:%s*" lang-mode))
                  (let ((inhibit-modification-hooks nil)
                        (inhibit-message t))
                    (erase-buffer)
                    ;; Additional space ensures property change.
                    (insert string " ")
                    (funcall lang-mode)
                    (font-lock-ensure))
                  (buffer-string)))
          (while (< pos (length propertized-text))
            (setq props (text-properties-at pos propertized-text))
            (setq overlay (make-overlay (+ body-start pos)
                                        (+ body-start (1+ pos))
                                        buf))
            (overlay-put overlay 'face (plist-get props 'face))
            (setq pos (1+ pos))))
      (overlay-put (make-overlay body-start body-end buf)
                   'face 'font-lock-doc-markup-face))))

(defun chatgpt-shell--fontify-inline-code (body-start body-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (overlay-put (make-overlay (1- body-start)
                             body-start) 'invisible 'chatgpt-shell)
  (overlay-put (make-overlay body-end
                             (1+ body-end)) 'invisible 'chatgpt-shell)
  (overlay-put (make-overlay body-start body-end
                             (shell-maker-buffer shell-maker-config))
               'face 'font-lock-doc-markup-face))

(defun chatgpt-shell--put-source-block-overlays ()
  "Put overlays for all source blocks."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (delete-overlay overlay))
  (dolist (block (chatgpt-shell--source-blocks))
    (chatgpt-shell--fontify-source-block
     (car (map-elt block 'start))
     (cdr (map-elt block 'start))
     (buffer-substring-no-properties (car (map-elt block 'language))
                                     (cdr (map-elt block 'language)))
     (car (map-elt block 'language))
     (cdr (map-elt block 'language))
     (car (map-elt block 'body))
     (cdr (map-elt block 'body))
     (car (map-elt block 'end))
     (cdr (map-elt block 'end))))
  (dolist (block (chatgpt-shell--inline-codes))
    (chatgpt-shell--fontify-inline-code
     (car (map-elt block 'body))
     (cdr (map-elt block 'body)))))

(defun chatgpt-shell--unpaired-length (length)
  "Expand LENGTH to include paired responses.

Each request has a response, so double LENGTH if set.

Add one for current request (without response).

If no LENGTH set, use 2048."
  (if length
      (1+ (* 2 length))
    2048))

(defun chatgpt-shell-view-at-point ()
  "View prompt and putput at point in a separate buffer."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
		      (point)))
        (buf))
    (save-excursion
      (when (>= (point) prompt-pos)
        (goto-char prompt-pos)
        (forward-line -1)
        (end-of-line))
      (let* ((items (chatgpt-shell--user-assistant-messages
                     (shell-maker--command-and-response-at-point)))
             (command (string-trim (or (map-elt (seq-first items) 'content) "")))
             (response (string-trim (or (map-elt (car (last items)) 'content) ""))))
        (setq buf (generate-new-buffer (if command
                                           (concat
                                            (shell-maker-prompt shell-maker-config)
                                            ;; Only the first line of prompt.
                                            (seq-first (split-string command "\n")))
                                         (concat (shell-maker-prompt shell-maker-config)
                                                 "(no prompt)"))))
        (when (seq-empty-p items)
          (user-error "Nothing to view"))
        (with-current-buffer buf
          (save-excursion
            (insert (propertize (or command "") 'face font-lock-doc-face))
            (when (and command response)
              (insert "\n\n"))
            (insert (or response "")))
          (view-mode +1)
          (setq view-exit-action 'kill-buffer))))
    (switch-to-buffer buf)
    buf))

(defun chatgpt-shell--extract-history (text prompt-regexp)
  "Extract all command and responses in TEXT with PROMPT-REGEXP."
  (chatgpt-shell--user-assistant-messages
   (shell-maker--extract-history text prompt-regexp)))

(defun chatgpt-shell--user-assistant-messages (history)
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

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
