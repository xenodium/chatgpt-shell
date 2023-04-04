;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.11.1
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
(require 'esh-mode)
(require 'eshell)
(require 'goto-addr)
(require 'ielm)
(require 'json)
(require 'map)
(require 'markdown-mode)
(require 'seq)
(require 'shell)
(require 'view)

(eval-when-compile
  (require 'cl-lib)
  (declare-function json-pretty-print "ext:json" (begin end &optional minimize)))

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-request-timeout 60
  "How long to wait for a request to time out."
  :type 'integer
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-display-function #'pop-to-buffer-same-window
  "Function to display new shell.  Can be set to `display-buffer' or similar."
  :type 'function
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-read-string-function (lambda (prompt history)
                                                (read-string prompt nil history))
  "Function to read strings from user.

To use `completing-read', it can be done with something like:

\(setq `chatgpt-shell-read-string-function'
      (lambda (prompt history)
        (completing-read prompt (symbol-value history) nil nil nil history)))"
  :type 'function
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-chatgpt-on-response-function nil
  "Function to automatically execute after last command output.

This is useful if you'd like to automatically handle or suggest things."
  :type 'function
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
  :type '(choice (float :tag "Float")
                 (const :tag "Nil" nil))
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

(defvar chatgpt-shell--log-buffer-name "*chatgpt-shell-log*")

(defvar chatgpt-shell--input nil)

(defvar chatgpt-shell--current-request-id 0)

(defvar chatgpt-shell--show-invisible-markers nil)

(defvar chatgpt-shell--prompt-internal nil)

(cl-defstruct
    chatgpt-shell-config
  prompt
  buffer-name
  process-name
  url
  invalid-input
  request-maker
  request-data-maker
  response-extractor
  response-post-processor)

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
                           (messages . ,commands-and-responses))))
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

(defvar chatgpt-shell--dall-e-config
  (make-chatgpt-shell-config
   :buffer-name "*dalle*"
   :process-name "dalle"
   :prompt "DALL-E> "
   :url "https://api.openai.com/v1/images/generations"
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
      nil ;; no streaming
      response-extractor
      callback
      error-callback))
   :request-data-maker
   (lambda (commands-and-responses)
     (let ((request-data `((model . ,chatgpt-shell-dall-e-model-version)
                           (prompt . ,(map-elt (aref commands-and-responses
                                                     (1- (length commands-and-responses)))
                                               'content)))))
       request-data))
   :response-extractor #'chatgpt-shell--extract-dall-e-response))

(defvar-local chatgpt-shell--busy nil)

(defvar-local chatgpt-shell--config nil)

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-shell-map)

(defvar-local chatgpt-shell--file nil)

(defvar chatgpt-shell-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'chatgpt-shell-return)
    (define-key map "\C-c\C-c" 'chatgpt-shell-interrupt)
    (define-key map "\C-x\C-s" 'chatgpt-shell-save-session-transcript)
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
    (funcall chatgpt-shell-display-function buf-name)
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
    (funcall chatgpt-shell-display-function buf-name)
    (when old-point
      (push-mark old-point))))

(define-derived-mode inferior-chatgpt-mode comint-mode "CHATGPT"
  "Major mode for interactively evaluating ChatGPT prompts.
Uses the interface provided by `comint-mode'"
  nil)

(defun chatgpt-shell--buffer (config)
  "Get buffer from CONFIG."
  (get-buffer-create (chatgpt-shell-config-buffer-name config)))

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
                        'comint-output-filter)))

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
     (cdr (map-elt block 'end)))))

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
                           '(face 'markdown-pre-face)))))

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
  (unless (eq major-mode 'inferior-chatgpt-mode)
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

(defun chatgpt-shell--list-to-pairs (items)
  "Return a list of pairs from the input ITEMS."
  (let ((pairs '()))
    (while items
      (push (cons (car items) (cadr items)) pairs)
      (setq items (cddr items)))
    (reverse pairs)))

(defun chatgpt-shell-restore-session-from-transcript ()
  "Restore session from transcript.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'inferior-chatgpt-mode)
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

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (unless (eq major-mode 'inferior-chatgpt-mode)
    (user-error "Not in a shell"))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
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
  "Send region to ChatGPT."
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

(defun chatgpt-shell-last-output ()
  "Get the last command output from the shell."
  (let ((proc (get-buffer-process (current-buffer))))
    (save-excursion
      (let* ((pmark (progn (goto-char (process-mark proc))
			   (forward-line 0)
			   (point-marker)))
             (output (buffer-substring comint-last-input-end pmark))
             (items (split-string output "<gpt-end-of-prompt>")))
        (if (> (length items) 1)
            (nth 1 items)
          (nth 0 items))))))

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

(defun chatgpt-shell-interrupt ()
  "Interrupt current request."
  (interactive)
  (unless (eq major-mode 'inferior-chatgpt-mode)
    (user-error "Not in a shell"))
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
  (let ((buffer (chatgpt-shell--buffer chatgpt-shell--config))
        (prefix-newline "")
        (suffix-newline "\n\n")
        (response-count 0))
   (unless chatgpt-shell--busy
    (setq chatgpt-shell--busy t)
    (cond
     ((string-equal "clear" (string-trim input-string))
      (call-interactively 'comint-clear-buffer)
      (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt-internal)
      (setq chatgpt-shell--busy nil))
     ((not (chatgpt-shell--curl-version-supported))
      (chatgpt-shell--write-reply "\nYou need curl version 7.76 or newer.\n\n")
      (setq chatgpt-shell--busy nil))
     ((and (chatgpt-shell-config-invalid-input
            chatgpt-shell--config)
           (funcall (chatgpt-shell-config-invalid-input
                     chatgpt-shell--config) input-string))
      (chatgpt-shell--write-reply
       (concat "\n"
               (funcall (chatgpt-shell-config-invalid-input
                         chatgpt-shell--config) input-string)
               "\n\n"))
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
      (funcall (chatgpt-shell-config-request-maker chatgpt-shell--config)
                 (chatgpt-shell-config-url chatgpt-shell--config)
                 (funcall (chatgpt-shell-config-request-data-maker chatgpt-shell--config)
                          (vconcat
                           (last (chatgpt-shell--extract-commands-and-responses
                                  (with-current-buffer buffer
                                    (buffer-string))
                                  (chatgpt-shell-config-prompt chatgpt-shell--config))
                                 (chatgpt-shell--unpaired-length
                                  chatgpt-shell-transmitted-context-length))))
                 (chatgpt-shell-config-response-extractor chatgpt-shell--config)
                 (lambda (response partial)
                   (setq response-count (1+ response-count))
                   (setq prefix-newline (if (> response-count 1)
                                            ""
                                          "\n"))
                   (if response
                       (if partial
                           (progn
                             (chatgpt-shell--write-partial-reply (concat prefix-newline response))
                             (setq chatgpt-shell--busy partial))
                         (progn
                           (chatgpt-shell--write-reply (concat prefix-newline response suffix-newline))
                           (chatgpt-shell--announce-response buffer)
                           (setq chatgpt-shell--busy nil)
                           (when (chatgpt-shell-config-response-post-processor chatgpt-shell--config)
                             ;; FIXME use (concat prefix-newline response suffix-newline) if not streaming.
                             (funcall (chatgpt-shell-config-response-post-processor chatgpt-shell--config)
                                      (chatgpt-shell-last-output)))))
                     (chatgpt-shell--write-reply "Error: that's all is known" t) ;; comeback
                     (setq chatgpt-shell--busy nil)
                     (chatgpt-shell--announce-response buffer)))
                 (lambda (error)
                   (chatgpt-shell--write-reply (concat (string-trim error) suffix-newline) t)
                   (setq chatgpt-shell--busy nil)
                   (chatgpt-shell--announce-response buffer))))))))

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
             (status (apply #'call-process (seq-first command) nil buffer nil (cdr command))))
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

(defun chatgpt-shell-post-dall-e-prompt (prompt &optional version)
  "Make a single DALL-E request with PROMPT.

Optionally provide model VERSION."
  (with-temp-buffer
    (setq-local chatgpt-shell--config
                chatgpt-shell--dall-e-config)
    (let* ((api-buffer (current-buffer))
           (command
            (chatgpt-shell--make-curl-request-command-list
             chatgpt-shell-openai-key
             (chatgpt-shell-config-url chatgpt-shell--config)
             (let ((request-data `((model . ,(or version
                                                 chatgpt-shell-dall-e-model-version))
                                   (prompt . ,prompt))))
               request-data)))
           (status (condition-case err
                       (apply #'call-process (seq-first command)
                              nil api-buffer nil (cdr command))
                     (error
                      (insert (error-message-string err))
                      1)))
           (response (chatgpt-shell--extract-dall-e-response
                      (buffer-substring-no-properties
	               (point-min)
	               (point-max))
                      t)))
      (if (and (map-elt response 'url)
               (map-elt response 'path)
               (map-elt response 'created))
          (with-temp-buffer
            (let* ((download-buffer (current-buffer))
                   (status (condition-case err
                               (call-process "curl" nil download-buffer
                                             "curl" "--no-progress-meter"
                                             "-o" (map-elt response 'path)
                                             (map-elt response 'url))
                             (error
                              (insert (error-message-string err))
                              1)))
                   (output (with-current-buffer download-buffer
                             (buffer-string))))
              (message "outcome: %s" output)
              (if (= status 0)
                  (map-elt response 'path)
                output)))
        (or response (with-current-buffer api-buffer
                       (buffer-string)))))))

(defun chatgpt-shell--announce-response (buffer)
  "Announce response if BUFFER is not active."
  (unless (eq buffer (window-buffer (selected-window)))
    (message "%s responded" (buffer-name buffer))))

(defun chatgpt-shell-openai-key ()
  "Get the ChatGPT key."
  (cond ((stringp chatgpt-shell-openai-key)
         chatgpt-shell-openai-key)
        ((functionp chatgpt-shell-openai-key)
         (funcall chatgpt-shell-openai-key))
        (t
         nil)))

(defun chatgpt-shell--unpaired-length (length)
  "Expand LENGTH to include paired responses.

Each request has a response, so double LENGTH if set.

Add one for current request (without response).

If no LENGTH set, use 2048."
  (if length
      (1+ (* 2 length))
    2048))

(defun chatgpt-shell--async-shell-command (command streaming response-extractor callback error-callback)
  "Run shell COMMAND asynchronously.
Set STREAMING to enable it.  Calls RESPONSE-EXTRACTOR to extract the
response and feeds it to CALLBACK or ERROR-CALLBACK accordingly."
  (let* ((buffer (chatgpt-shell--buffer chatgpt-shell--config))
         (request-id (chatgpt-shell--increment-request-id))
         (output-buffer (generate-new-buffer " *temp*"))
         (request-process (condition-case err
                              (apply #'start-process (append (list "ChatGPT" (buffer-name output-buffer))
                                                             command))
                            (error
                             (with-current-buffer buffer
                              (funcall error-callback (error-message-string err)))
                             nil)))
         (preparsed)
         (remaining-text)
         (process-connection-type nil))
    (when request-process
      (chatgpt-shell--write-output-to-log-buffer "// Request\n\n")
      (chatgpt-shell--write-output-to-log-buffer (string-join command " "))
      (chatgpt-shell--write-output-to-log-buffer "\n\n")
      (when streaming
        (set-process-filter
         request-process
         (lambda (process output)
           (when (eq request-id chatgpt-shell--current-request-id)
             (chatgpt-shell--write-output-to-log-buffer
              (format "// Filter output\n\n%s\n\n" output))
             (setq remaining-text (concat remaining-text output))
             (setq preparsed (chatgpt-shell--preparse-json remaining-text))
             (if (car preparsed)
                 (mapc (lambda (obj)
                         (with-current-buffer buffer
                           (funcall callback (funcall response-extractor obj) t)))
                       (car preparsed))
               (with-current-buffer buffer
                 (funcall callback (cdr preparsed) t)))
             (setq remaining-text (cdr preparsed))))))
      (set-process-sentinel
       request-process
       (lambda (process _event)
         (let ((active (eq request-id chatgpt-shell--current-request-id))
               (output (with-current-buffer (process-buffer process)
                         (buffer-string)))
               (exit-status (process-exit-status process)))
           (chatgpt-shell--write-output-to-log-buffer
            (format "// Response (%s)\n\n" (if active "active" "inactive")))
           (chatgpt-shell--write-output-to-log-buffer
            (format "Exit status: %d\n\n" exit-status))
           (chatgpt-shell--write-output-to-log-buffer output)
           (chatgpt-shell--write-output-to-log-buffer "\n\n")
           (with-current-buffer buffer
             (when active
               (if (= exit-status 0)
                   (funcall callback
                            (if (string-empty-p (string-trim output))
                                output
                              (funcall response-extractor output))
                            nil)
                 (if-let ((error (if (string-empty-p (string-trim output))
                                     output
                                   (funcall response-extractor output))))
                     (funcall error-callback error)
                   (funcall error-callback output)))))
           (kill-buffer output-buffer)))))))

(defun chatgpt-shell--json-parse-string-filtering (json regexp)
  "Attempt to parse JSON.  If unsuccessful, attempt after removing REGEXP."
  (let ((json-object nil)
        (curl-lines-removed-str json))
    ;; Try parsing JSON string as is
    (condition-case nil
        (setq json-object (json-read-from-string json))
      (error nil))
    ;; If parsing fails, remove curl lines and try again
    (when (null json-object)
      (setq curl-lines-removed-str (replace-regexp-in-string regexp "" json))
      (condition-case nil
          (setq json-object (json-read-from-string curl-lines-removed-str))
        (error nil)))
    json-object))

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
  (let (chatgpt-shell--input)
    (comint-send-input)
    (chatgpt-shell--eval-input chatgpt-shell--input)))

(defun chatgpt-shell--write-reply (reply &optional failed)
  "Write REPLY to prompt.  Set FAILED to record failure."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (comint-output-filter (chatgpt-shell--process)
                          (concat reply
                                  (if failed
                                      (propertize "<gpt-ignored-response>"
                                                  'invisible (not chatgpt-shell--show-invisible-markers))
                                    "")
                                  chatgpt-shell--prompt-internal))
    (chatgpt-shell--put-source-block-overlays)))

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

(defun chatgpt-shell--json-encode (obj)
  "Serialize OBJ to json.  Use fallback if `json-serialize' isn't available."
  (if (fboundp 'json-serialize)
      (json-serialize obj)
    (json-encode obj)))

(defun chatgpt-shell--curl-version-supported ()
  "Return t if curl version is 7.76 or newer, nil otherwise."
  (let* ((curl-error-redirect (if (eq system-type (or 'windows-nt 'ms-dos)) "2> NUL" "2>/dev/null"))
         (curl-version-string (shell-command-to-string (concat "curl --version " curl-error-redirect))))
    (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" curl-version-string)
      (let ((version (match-string 1 curl-version-string)))
        (version<= "7.76" version)))))

(defun chatgpt-shell--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (json-parse-error nil))
    (condition-case _err
        (json-read-from-string json)
      (error nil))))

(defun chatgpt-shell--write-partial-reply (reply)
  "Write partial REPLY to prompt."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (delete-overlay overlay))
    (comint-output-filter (chatgpt-shell--process) reply)))

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

(defun chatgpt-shell--preparse-json (json)
  "Preparse JSON and return a cons of parsed objects vs unparsed text."
  (let ((parsed)
        (remaining)
        (loc))
    (setq json (replace-regexp-in-string (rx bol "data:") "" json))
    (with-temp-buffer ;; with-current-buffer (get-buffer-create "*preparse*")
      (erase-buffer)
      (insert json)
      (goto-char (point-min))
      (setq loc (point))
      (while (when-let
                 ((data (ignore-errors (json-read))))
               (setq parsed (append parsed (list data)))
               (setq loc (point))))
      (setq remaining (buffer-substring-no-properties loc (point-max)))
      (cons parsed
            (string-trim remaining)))))


(defun chatgpt-shell--find-string-in-buffer (buffer search-str)
  "Find SEARCH-STR in BUFFER and return a cons with start/end.
Return nil if not found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward search-str nil t)
        (cons (match-beginning 0) (match-end 0))))))

(defun chatgpt-shell--download-image (url path callback error-callback)
  "Download URL to PATH.  Invoke CALLBACK on success.
ERROR-CALLBACK otherwise."
  ;; Ensure sync failures can be handled in next runloop.
  (run-with-idle-timer 0 nil
                       (lambda ()
                         (let* ((output-buffer (generate-new-buffer " *temp*"))
                                (request-process
                                 (condition-case err
                                     (start-process "curl" (buffer-name output-buffer)
                                                    "curl" "--no-progress-meter"
                                                    "-o" path
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

(defun chatgpt-shell--extract-commands-and-responses (text prompt-regexp)
  "Extract all command and responses in TEXT with PROMPT-REGEXP."
  (let ((result))
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
                  (push (list (cons 'role "assistant")
                              (cons 'content response)) result)))))
          (split-string text prompt-regexp))
    (nreverse result)))

(defun chatgpt-shell--extract-current-command-and-response ()
  "Extract the current command and response in buffer."
  (save-excursion
    (save-restriction
      (shell-narrow-to-prompt)
      (let ((items (chatgpt-shell--extract-commands-and-responses
                    (buffer-string)
                    (chatgpt-shell-config-prompt chatgpt-shell--config))))
        (cl-assert (or (seq-empty-p items)
                       (eq (length items) 1)
                       (eq (length items) 2)))
        items))))

(defun chatgpt-shell-view-current ()
  "View current entry in a separate buffer."
  (interactive)
  (unless (eq major-mode 'inferior-chatgpt-mode)
    (user-error "Not in a shell"))
  (let* ((items (chatgpt-shell--extract-current-command-and-response))
         (command (map-elt (seq-find (lambda (item)
                                       (and (string-equal
                                             (map-elt item 'role)
                                             "user")
                                            (not (string-empty-p
                                                  (string-trim (map-elt item 'content))))))
                                     items)
                           'content))
         (response (map-elt (seq-find (lambda (item)
                                        (and (string-equal
                                              (map-elt item 'role)
                                              "assistant")
                                             (not (string-empty-p
                                                   (string-trim (map-elt item 'content))))))
                                      items)
                            'content))
         (buf (generate-new-buffer (if command
                                       (concat
                                        (chatgpt-shell-config-prompt chatgpt-shell--config)
                                        ;; Only the first line of prompt.
                                        (seq-first (split-string command "\n")))
                                     (concat (chatgpt-shell-config-prompt chatgpt-shell--config)
                                             "(no prompt)")))))
    (when (seq-empty-p items)
      (user-error "Nothing to view"))
    (with-current-buffer buf
      (save-excursion
        (insert (propertize (or command "") 'face font-lock-doc-face))
        (when (and command response)
          (insert "\n\n"))
        (insert (or response "")))
      (view-mode +1)
      (setq view-exit-action 'kill-buffer))
    (switch-to-buffer buf)
    buf))

(defun chatgpt-shell--write-output-to-log-buffer (output)
  "Write OUTPUT to log buffer."
  (when (chatgpt-shell-openai-key)
    (setq output (string-replace (chatgpt-shell-openai-key) "SK-REDACTED-OPENAI-KEY"
                                 output)))
  (let ((buffer (get-buffer chatgpt-shell--log-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create chatgpt-shell--log-buffer-name)))
    (with-current-buffer buffer
      (let ((beginning-of-input (goto-char (point-max))))
        (insert output)
        (when (and (require 'json nil t)
                   (ignore-errors (chatgpt-shell--json-parse-string output)))
          (json-pretty-print beginning-of-input (point)))))))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (chatgpt-shell--buffer chatgpt-shell--config)))

(defun chatgpt-shell--extract-dall-e-response (json &optional no-download)
  "Extract DALL-E response from JSON.
Set NO-DOWNLOAD to skip automatic downloading."
  (if-let ((parsed (chatgpt-shell--json-parse-string-filtering
                    json "^curl:.*\n?"))
           (buffer (chatgpt-shell--buffer chatgpt-shell--config)))
      (if-let* ((url (let-alist parsed
                       (let-alist (seq-first .data)
                         .url)))
                (created (number-to-string (let-alist parsed
                                             .created)))
                (path (expand-file-name (concat created ".png") temporary-file-directory)))
          (if no-download
              `((url . ,url)
                (created . ,created)
                (path . ,path))
            (progn
              (chatgpt-shell--download-image
               url path
               (lambda (path)
                 (let* ((loc (chatgpt-shell--find-string-in-buffer
                              buffer
                              path))
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
                                   path))
                             (start (car loc))
                             (end (cdr loc)))
                   (with-current-buffer buffer
                     (remove-text-properties start end '(face nil))
                     (add-text-properties start end `(display ,error))))))
              (propertize path 'display "[downloading...]")))
        (let-alist parsed
          .error.message))))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
