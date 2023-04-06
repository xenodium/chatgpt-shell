;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.11.1
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
(require 'mk-shell)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-request-timeout 60
  "How long to wait for a request to time out."
  :type 'integer
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-chatgpt-default-prompts
  '("Write a unit test for the following code:"
    "Refactor the following code so that "
    "Summarize the output of the following command:"
    "What's wrong with this command?"
    "Explain what the following code does:")
  "List of default prompts to choose from."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defvar chatgpt-shell--chatgpt-prompt-history nil)

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

(defcustom chatgpt-shell-chatgpt-model-version "gpt-3.5-turbo"
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

(defcustom chatgpt-shell-chatgpt-system-prompt nil
  "The system message helps set the behavior of the assistant.

For example: You are a helpful assistant that translates English to French.

See https://platform.openai.com/docs/guides/chat/introduction"
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-chatgpt-streaming nil
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

(defvar chatgpt-shell--chatgpt-config
  (make-chatgpt-shell-config
   :buffer-name "*chatgpt*"
   :process-name "chatgpt"
   :prompt "ChatGPT> "
   :url "https://api.openai.com/v1/chat/completions"
   :invalid-input
   (lambda (_input)
     (unless chatgpt-shell-openai-key
       "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")"))
   :request-maker
   (lambda (url request-data response-extractor callback error-callback)
     (chatgpt-shell--async-shell-command
      (chatgpt-shell--make-curl-request-command-list
       chatgpt-shell-openai-key
       url request-data)
      t ;; streaming
      response-extractor
      callback
      error-callback))
   :request-data-maker
   (lambda (commands-and-responses)
     (let ((request-data `((model . ,chatgpt-shell-chatgpt-model-version)
                           (messages . ,(if chatgpt-shell-chatgpt-system-prompt
                                            (vconcat
                                             (list
                                              (list
                                               (cons 'role "system")
                                               (cons 'content chatgpt-shell-chatgpt-system-prompt)))
                                             commands-and-responses)
                                          commands-and-responses)))))
       (when chatgpt-shell-model-temperature
         (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
       (when chatgpt-shell-chatgpt-streaming
         (push `(stream . t) request-data))
       request-data))
   :response-extractor #'chatgpt-shell--extract-chatgpt-response
   :response-post-processor
   (lambda (response)
     (when chatgpt-shell-chatgpt-on-response-function
       (funcall chatgpt-shell-chatgpt-on-response-function response)))))

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
        (chatgpt-shell-mode)
        (chatgpt-shell--initialize chatgpt-shell--chatgpt-config)))
    (funcall chatgpt-shell-display-function buf-name)
    (when old-point
      (push-mark old-point))))

(defun chatgpt-shell--fontify-source-block (quotes1-start quotes1-end lang
lang-start lang-end body-start body-end quotes2-start quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (overlay-put (make-overlay quotes1-start
                             quotes1-end) 'invisible t)
  (overlay-put (make-overlay quotes2-start
                             quotes2-end) 'invisible t)
  (when (and lang-start lang-end)
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
        (buf (chatgpt-shell--buffer chatgpt-shell--config))
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
                  (let ((inhibit-modification-hooks nil))
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
            ;; (set-text-properties (+ body-start pos)
            ;;                      (+ body-start (1+ pos))
            ;;                      props)
            (overlay-put overlay 'face (plist-get props 'face))
            (setq pos (1+ pos)))
          ;; (overlay-put (make-overlay body-start body-end buf)
          )
      (set-text-properties body-start body-end
                           '(face 'font-lock-doc-markup-face)))))

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

(defun chatgpt-shell-save-session-transcript ()
  "Save shell transcript to file.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (if chatgpt-shell--file
      (let ((content (buffer-string))
            (path chatgpt-shell--file))
        (with-temp-buffer
          (insert content)
          (write-file path nil)))
    (when-let ((path (read-file-name "Write file: "
                                     nil nil nil "transcript.txt"))
               (content (buffer-string)))
      (with-temp-buffer
        (insert content)
        (write-file path t))
      (setq chatgpt-shell--file path))))

(defun chatgpt-shell-restore-session-from-transcript ()
  "Restore session from transcript.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let* ((path (read-file-name "Restore from: " nil nil t))
         (prompt (chatgpt-shell-config-prompt chatgpt-shell--config))
         (commands-and-responses (with-temp-buffer
                                   (insert-file-contents path)
                                   (chatgpt-shell--extract-commands-and-responses
                                    (buffer-string)
                                    prompt)))
         (response-extractor (chatgpt-shell-config-response-extractor
                              chatgpt-shell--config))
         (request-maker (chatgpt-shell-config-request-maker
                         chatgpt-shell--config))
         (invalid-input (chatgpt-shell-config-invalid-input
                         chatgpt-shell--config))
         (command)
         (response)
         (failed))
    ;; Momentarily overrides request handling to replay all commands
    ;; read from file so comint treats all commands/responses like
    ;; any other command.
    (unwind-protect
        (progn
          (setf (chatgpt-shell-config-response-extractor chatgpt-shell--config)
                (lambda (json)
                  json))
          (setf (chatgpt-shell-config-invalid-input chatgpt-shell--config) nil)
          (setf (chatgpt-shell-config-request-maker chatgpt-shell--config)
                (lambda (_url _request-data _response-extractor callback _error-callback)
                  (setq response (car commands-and-responses))
                  (setq commands-and-responses (cdr commands-and-responses))
                  (when response
                    (unless (string-equal (map-elt response 'role)
                                          "assistant")
                      (setq failed t)
                      (user-error "Invalid transcript"))
                    (funcall callback (map-elt response 'content) nil)
                    (setq command (car commands-and-responses))
                    (setq commands-and-responses (cdr commands-and-responses))
                    (when command
                      (insert (map-elt command 'content))
                      (chatgpt-shell--send-input)))))
          (goto-char (point-max))
          (comint-clear-buffer)
          (setq command (car commands-and-responses))
          (setq commands-and-responses (cdr commands-and-responses))
          (when command
            (unless (string-equal (map-elt command 'role)
                                  "user")
              (setq failed t)
              (user-error "Invalid transcript"))
            (insert (map-elt command 'content))
            (chatgpt-shell--send-input)))
      (if failed
          (setq chatgpt-shell--file nil)
        (setq chatgpt-shell--file path))
      (setq chatgpt-shell--busy nil)
      (setf (chatgpt-shell-config-response-extractor chatgpt-shell--config)
            response-extractor)
      (setf (chatgpt-shell-config-invalid-input chatgpt-shell--config)
            invalid-input)
      (setf (chatgpt-shell-config-request-maker chatgpt-shell--config)
            request-maker))))

(defun chatgpt-shell-chatgpt-prompt ()
  "Make a ChatGPT request from the minibuffer.

If region is active, append to prompt."
  (interactive)
  (unless chatgpt-shell--chatgpt-prompt-history
    (setq chatgpt-shell--chatgpt-prompt-history
          chatgpt-shell-chatgpt-default-prompts))
  (let ((prompt (funcall chatgpt-shell-read-string-function
                         (concat
                          (if (region-active-p)
                              "[appending region] "
                            "")
                          (chatgpt-shell-config-prompt
                           chatgpt-shell--chatgpt-config))
                         'chatgpt-shell--chatgpt-prompt-history)))
    (when (region-active-p)
      (setq prompt (concat prompt "\n\n"
                           (buffer-substring (region-beginning) (region-end)))))
    (chatgpt-shell-send-to-buffer prompt)
    (chatgpt-shell--send-input)))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
  (chatgpt-shell-send-to-buffer
   (concat "What does the following code do?\n\n"
           (buffer-substring (region-beginning) (region-end))))
  (chatgpt-shell--send-input))

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
    (when chatgpt-shell--busy
      (chatgpt-shell-interrupt))
    (goto-char (point-max))
    (if review
        (save-excursion
          (insert text))
      (insert text)
      (chatgpt-shell--send-input))))

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

;; FIXME: Delete?
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

(defun chatgpt-shell-post-chatgpt-messages (messages &optional version callback error-callback)
  "Make a single ChatGPT request with MESSAGES.
Optionally pass model VERSION, CALLBACK, and ERROR-CALLBACK.

If CALLBACK or ERROR-CALLBACK are missing, execute synchronously.

For example:

\(chatgpt-shell-post-chatgpt-messages
 `(((role . \"user\")
    (content . \"hello\")))
 \"gpt-3.5-turbo\"
 (lambda (response)
   (message \"%s\" response))
 (lambda (error)
   (message \"%s\" error)))"
  (if (and callback error-callback)
      (with-temp-buffer
        (setq-local chatgpt-shell--config
                    chatgpt-shell--chatgpt-config)
        (chatgpt-shell--async-shell-command
         (chatgpt-shell--make-curl-request-command-list
          chatgpt-shell-openai-key
          (chatgpt-shell-config-url chatgpt-shell--config)
          (let ((request-data `((model . ,(or version
                                              chatgpt-shell-chatgpt-model-version))
                                (messages . ,(vconcat
                                              messages)))))
            (when chatgpt-shell-model-temperature
              (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
            request-data))
         nil ;; streaming
         (chatgpt-shell-config-response-extractor chatgpt-shell--config)
         callback
         error-callback))
    (with-temp-buffer
      (setq-local chatgpt-shell--config
                  chatgpt-shell--chatgpt-config)
      (let* ((buffer (current-buffer))
             (command
              (chatgpt-shell--make-curl-request-command-list
               chatgpt-shell-openai-key
               (chatgpt-shell-config-url chatgpt-shell--config)
               (let ((request-data `((model . ,(or version
                                                   chatgpt-shell-chatgpt-model-version))
                                     (messages . ,(vconcat
                                                   messages)))))
                 (when chatgpt-shell-model-temperature
                   (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
                 request-data)))
             (_status (apply #'call-process (seq-first command) nil buffer nil (cdr command))))
        (chatgpt-shell--extract-chatgpt-response
             (buffer-substring-no-properties
	      (point-min)
	      (point-max)))))))

(defun chatgpt-shell-post-chatgpt-prompt (prompt &optional version callback error-callback)
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
  (chatgpt-shell-post-chatgpt-messages `(((role . "user")
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

(defun chatgpt-shell--make-curl-request-command-list (key url request-data)
  "Build ChatGPT curl command list using KEY URL and REQUEST-DATA."
  (list "curl" url
        "--fail-with-body"
        "--no-progress-meter"
        "-m" (number-to-string chatgpt-shell-request-timeout)
        "-H" "Content-Type: application/json"
        "-H" (format "Authorization: Bearer %s"
                     (cond ((stringp key)
                            key)
                           ((functionp key)
                            (condition-case _err
                                (funcall key)
                              (error
                               "KEY-NOT-FOUND")))))
        "-d" (chatgpt-shell--json-encode request-data)))

(defun chatgpt-shell--extract-chatgpt-response (json)
  "Extract ChatGPT response from JSON."
  (if (eq (type-of json) 'cons)
      (let-alist json ;; already parsed
        (or (let-alist (seq-first .choices)
              (or .delta.content
                  .message.content))
            .error.message
            ""))
    (if-let (parsed (chatgpt-shell--json-parse-string json))
        (string-trim
         (let-alist parsed
           (let-alist (seq-first .choices)
             .message.content)))
      (if-let (parsed-error (chatgpt-shell--json-parse-string-filtering
                             json "^curl:.*\n?"))
          (let-alist parsed-error
            .error.message)))))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
