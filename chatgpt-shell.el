;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.25.1
;; Package-Requires: ((emacs "27.1") (shell-maker "0.17.1"))

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

;; `chatgpt-shell' is a comint-based ChatGPT shell for Emacs.
;;
;; You must set `chatgpt-shell-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.
;;
;; Note: This is young package still.  Please report issues or send
;; patches to https://github.com/xenodium/chatgpt-shell
;;
;; Support the work https://github.com/sponsors/xenodium

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

(defcustom chatgpt-shell-additional-curl-options nil
  "Additional options for `curl' command."
  :type '(repeat (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-request-timeout 180
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

(defcustom chatgpt-shell-insert-queries-inline t
  "When making queries in non-shell buffers, insert responses inline."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-after-command-functions nil
  "Abnormal hook (i.e. with parameters) invoked after each command.

This is useful if you'd like to automatically handle or suggest things
post execution.

For example:

\(add-hook `chatgpt-shell-after-command-functions'
   (lambda (command output)
     (message \"Command: %s\" command)
     (message \"Output: %s\" output)))"
  :type 'hook
  :group 'shell-maker)

(defvaralias 'chatgpt-shell-display-function 'shell-maker-display-function)

(defvaralias 'chatgpt-shell-read-string-function 'shell-maker-read-string-function)

(defvaralias 'chatgpt-shell-logging 'shell-maker-logging)

(defvaralias 'chatgpt-shell-history-path 'shell-maker-history-path)

(defalias 'chatgpt-shell-save-session-transcript #'shell-maker-save-session-transcript)

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

(defcustom chatgpt-shell-babel-headers '(("dot" . ((:file . "<temp-file>.png")))
                                         ("ditaa" . ((:file . "<temp-file>.png")))
                                         ("objc" . ((:results . "output")))
                                         ("python" . ((:python . "python3")))
                                         ("swiftui" . ((:results . "file")))
                                         ("c++" . ((:results . "raw")))
                                         ("c" . ((:results . "raw"))))
  "Additional headers to make babel blocks work.

Please submit contributions so more things work out of the box."
  :type '(repeat (cons string string))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-source-block-actions
  nil
  "Block actions for known languages.

Can be used compile or run source block at point."
  :type '(list (cons string
                     (list (cons 'primary-action-confirmation string)
                           (cons 'primary-action function))))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-versions
  '("gpt-3.5-turbo"
    "gpt-4")
  "The list of ChatGPT OpenAI models to swap from.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version (nth 0 chatgpt-shell-model-versions)
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

(defcustom chatgpt-shell-system-prompts
  '("Always show code snippets in markdown blocks with language labels.")
  "List of default prompts to choose from."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-system-prompt (nth 0 chatgpt-shell-system-prompts)
  "The system message helps set the behavior of the assistant.

For example: You are a helpful assistant that translates English to French.

See https://platform.openai.com/docs/guides/chat/introduction"
  :type 'string
  :group 'chatgpt-shell)

(defun chatgpt-shell-swap-system-prompt ()
  "Swap system prompt from `chatgpt-shell-system-prompts'."
  (interactive)
  (let ((choice (completing-read "System prompt: "
                                 (append
                                  (list "None")
                                  chatgpt-shell-system-prompts))))
    (if (or (string-equal choice "None")
            (string-empty-p (string-trim choice)))
        (setq chatgpt-shell-system-prompt nil)
      (setq chatgpt-shell-system-prompt choice))))

(defun chatgpt-shell-swap-model-version ()
  "Swap model version from `chatgpt-shell-model-versions'."
  (interactive)
  (setq chatgpt-shell-model-version
        (completing-read "Model version: "
                         chatgpt-shell-model-versions nil t)))

(defcustom chatgpt-shell-streaming t
  "Whether or not to stream ChatGPT responses (show chunks as they arrive)."
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

(defcustom chatgpt-shell-api-url-base "https://api.openai.com"
  "OpenAI API's base URL.

`chatgpt-shell--api-url' =
   `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'

If you use ChatGPT through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-api-url-path "/v1/chat/completions"
  "OpenAI API's URL path.

`chatgpt-shell--api-url' =
   `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'"
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

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
     (run-hook-with-args 'chatgpt-shell-after-command-functions
                         command output))
   :redact-log-output
   (lambda (output)
     (if (chatgpt-shell-openai-key)
         (replace-regexp-in-string (regexp-quote (chatgpt-shell-openai-key))
                                   "SK-REDACTED-OPENAI-KEY"
                                   output)
       output))))

(defalias 'chatgpt-shell-clear-buffer #'comint-clear-buffer)

(defalias 'chatgpt-shell-explain-code #'chatgpt-shell-describe-code)

;; Aliasing enables editing as text in babel.
(defalias 'chatgpt-shell-mode #'text-mode)

;;;###autoload
(defun chatgpt-shell (&optional no-focus)
  "Start a ChatGPT shell.

With NO-FOCUS, start the shell without focus."
  (interactive)
  (shell-maker-start chatgpt-shell--config no-focus)
  (define-key shell-maker-mode-map (kbd "C-M-h")
    #'chatgpt-shell-mark-at-point-dwim)
  (define-key shell-maker-mode-map (kbd "C-c C-c")
    #'chatgpt-shell-ctrl-c-ctrl-c)
  (define-key shell-maker-mode-map (kbd "C-c C-p")
    #'chatgpt-shell-previous-item)
  (define-key shell-maker-mode-map (kbd "C-c C-n")
    #'chatgpt-shell-next-item))

(defun chatgpt-shell-ctrl-c-ctrl-c ()
  "Ctrl-C Ctrl-C DWIM binding.

If on a block with primary action, execute it.

Otherwise interrupt if busy."
  (interactive)
  (cond ((chatgpt-shell-primary-block-action-at-point)
         (chatgpt-shell-execute-primary-block-action-at-point))
        ((chatgpt-shell-markdown-block-at-point)
         (user-error "No action available"))
        ((and shell-maker--busy
              (eq (line-number-at-pos (point-max))
                  (line-number-at-pos (point))))
         (shell-maker-interrupt)
         (chatgpt-shell--put-source-block-overlays))
        (t
         (shell-maker-interrupt))))

(defun chatgpt-shell-mark-at-point-dwim ()
  "Mark source block if at point.  Mark all output otherwise."
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (progn
        (set-mark (map-elt block 'end))
        (goto-char (map-elt block 'start)))
    (shell-maker-mark-output)))

(defun chatgpt-shell-markdown-block-language (text)
  "Get the language label of a Markdown TEXT code block."
  (when (string-match (rx bol "```" (0+ space) (group (+ (not (any "\n"))))) text)
    (match-string 1 text)))

(defun chatgpt-shell-markdown-block-at-point ()
  "Markdown start/end cons if point at block.  nil otherwise."
  (save-excursion
    (save-restriction
      (shell-maker-narrow-to-prompt)
      (let* ((language)
             (language-start)
             (language-end)
             (start (save-excursion
                      (when (re-search-backward "^```" nil t)
                        (setq language (chatgpt-shell-markdown-block-language (thing-at-point 'line)))
                        (save-excursion
                          (forward-char 3) ; ```
                          (setq language-start (point))
                          (end-of-line)
                          (setq language-end (point)))
                        language-end)))
             (end (save-excursion
                    (when (re-search-forward "^```" nil t)
                      (forward-line 0)
                      (point)))))
        (when (and start end
                   (> (point) start)
                   (< (point) end))
          (list (cons 'language language)
                (cons 'language-start language-start)
                (cons 'language-end language-end)
                (cons 'start start)
                (cons 'end end)))))))

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

(defvar chatgpt-shell--source-block-regexp
  (rx  bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
       (group (zero-or-more (or alphanumeric "-" "+"))) ;; language
       (zero-or-more whitespace)
       (one-or-more "\n")
       (group (*? anychar)) ;; body
       (one-or-more "\n")
       (group "```") (or "\n" eol)))

(defun chatgpt-shell-next-source-block ()
  "Move point to previous source block."
  (interactive)
  (when-let
      ((next-block
        (save-excursion
          (when-let ((current (chatgpt-shell-markdown-block-at-point)))
            (goto-char (map-elt current 'end))
            (end-of-line))
          (when (re-search-forward chatgpt-shell--source-block-regexp nil t)
            (chatgpt-shell--match-source-block)))))
    (goto-char (car (map-elt next-block 'body)))))

(defun chatgpt-shell-previous-item ()
  "Go to previous item.

Could be a prompt or a source block."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (when (comint-next-prompt (- 1))
                        (point))))
        (block-pos (save-excursion
                     (when (chatgpt-shell-previous-source-block)
                       (point)))))
    (cond ((and block-pos prompt-pos)
           (goto-char (max prompt-pos
                           block-pos)))
          (block-pos
           (goto-char block-pos))
          (prompt-pos
           (goto-char prompt-pos)))))

(defun chatgpt-shell-next-item ()
  "Go to next item.

Could be a prompt or a source block."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (when (comint-next-prompt 1)
                        (point))))
        (block-pos (save-excursion
                     (when (chatgpt-shell-next-source-block)
                       (point)))))
    (cond ((and block-pos prompt-pos)
           (goto-char (min prompt-pos
                           block-pos)))
          (block-pos
           (goto-char block-pos))
          (prompt-pos
           (goto-char prompt-pos)))))

(defun chatgpt-shell-previous-source-block ()
  "Move point to previous source block."
  (interactive)
  (when-let
      ((previous-block
        (save-excursion
          (when-let ((current (chatgpt-shell-markdown-block-at-point)))
            (goto-char (map-elt current 'start))
            (forward-line 0))
          (when (re-search-backward chatgpt-shell--source-block-regexp nil t)
            (chatgpt-shell--match-source-block)))))
    (goto-char (car (map-elt previous-block 'body)))))

(defun chatgpt-shell--match-source-block ()
  "Return a matched source block by the previous search/regexp operation."
  (list
   'start (cons (match-beginning 1)
                (match-end 1))
   'end (cons (match-beginning 4)
              (match-end 4))
   'language (when (and (match-beginning 2)
                        (match-end 2))
               (cons (match-beginning 2)
                     (match-end 2)))
   'body (cons (match-beginning 3) (match-end 3))))

(defun chatgpt-shell--source-blocks ()
  "Get a list of all source blocks in buffer."
  (let ((markdown-blocks '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              chatgpt-shell--source-block-regexp
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push (chatgpt-shell--match-source-block)
                markdown-blocks))))
    (nreverse markdown-blocks)))

(defun chatgpt-shell-prompt (prefix)
  "Make a ChatGPT request from the minibuffer.

If region is active, append to prompt.

With PREFIX, invert `chatgpt-shell-insert-queries-inline' choice."
  (interactive "P")
  (unless chatgpt-shell--prompt-history
    (setq chatgpt-shell--prompt-history
          chatgpt-shell-default-prompts))
  (let ((overlay-blocks (derived-mode-p 'prog-mode))
        (prompt (funcall shell-maker-read-string-function
                         (concat
                          (if (region-active-p)
                              "[appending region] "
                            "")
                          (shell-maker-prompt
                           chatgpt-shell--config))
                         'chatgpt-shell--prompt-history)))
    (when (region-active-p)
      (setq prompt (concat prompt "\n\n"
                           (if overlay-blocks
                               (format "``` %s\n"
                                       (string-remove-suffix "-mode" (format "%s" major-mode)))
                             "")
                           (buffer-substring (region-beginning) (region-end))
                           (if overlay-blocks
                               "\n```"
                             ""))))
    (chatgpt-shell-send-to-buffer prompt nil prefix)))

(defun chatgpt-shell-describe-code (prefix)
  "Describe code from region using ChatGPT.

With PREFIX, invert `chatgpt-shell-insert-queries-inline' choice."
  (interactive "P")
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((overlay-blocks (derived-mode-p 'prog-mode)))
    (chatgpt-shell-send-to-buffer
     (concat "What does the following code do?\n\n"
             (if overlay-blocks
                 (format "``` %s\n"
                         (string-remove-suffix "-mode" (format "%s" major-mode)))
               "")
             (buffer-substring (region-beginning) (region-end))
             (if overlay-blocks
                 "\n```"
               ""))
     nil prefix)
    (when overlay-blocks
      (with-current-buffer (get-buffer-create "*chatgpt*")
        (chatgpt-shell--put-source-block-overlays)))))

(defun chatgpt-shell-send-region-with-header (header invert-insert-inline)
  "Send text with HEADER from region using ChatGPT.

With INVERT-INSERT-INLINE, invert `chatgpt-shell-insert-queries-inline' choice."
  (chatgpt-shell-send-to-buffer
   (concat header
           "\n\n"
           (buffer-substring (region-beginning) (region-end)))
   nil invert-insert-inline))

(defun chatgpt-shell-refactor-code (prefix)
  "Refactor code from region using ChatGPT.

With PREFIX, invert `chatgpt-shell-insert-queries-inline' choice."
  (interactive "P")
  (chatgpt-shell-send-region-with-header "Please help me refactor the following code. Please reply with the refactoring explanation in English, refactored code, and diff between two versions. Please ignore the comments and strings in the code during the refactoring. If the code remains unchanged after refactoring, please say 'No need to refactor'." prefix))

(defun chatgpt-shell-generate-unit-test (prefix)
  "Generate unit-test for the code from region using ChatGPT.

With PREFIX, invert `chatgpt-shell-insert-queries-inline' choice."
  (interactive "P")
  (chatgpt-shell-send-region-with-header
   "Please help me generate unit-test following function:" prefix))

(defun chatgpt-shell-proofread-region (prefix)
  "Proofread English from region using ChatGPT.

With PREFIX, invert `chatgpt-shell-insert-queries-inline' choice."
  (interactive "P")
  (chatgpt-shell-send-region-with-header
   "Please help me proofread the following text with English:" prefix))

(defun chatgpt-shell-eshell-whats-wrong-with-last-command ()
  "Ask ChatGPT what's wrong with the last eshell command."
  (interactive)
  (let ((chatgpt-shell-insert-queries-inline nil))
    (chatgpt-shell-send-to-buffer
     (concat "What's wrong with this command?\n\n"
             (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
             "\n\n"
             (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))))

(defun chatgpt-shell-eshell-summarize-last-command-output ()
  "Ask ChatGPT to summarize the last command output."
  (interactive)
  (let ((chatgpt-shell-insert-queries-inline nil))
    (chatgpt-shell-send-to-buffer
     (concat "Summarize the output of the following command: \n\n"
             (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
             "\n\n"
             (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))))

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

(defun chatgpt-shell-command-line-from-prompt-file (file-path)
  "Send prompt in FILE-PATH and output to standard output."
  (let ((prompt (with-temp-buffer
                  (insert-file-contents file-path)
                  (buffer-string))))
    (if (string-empty-p (string-trim prompt))
        (princ (format "Could not read prompt from %s" file-path)
               #'external-debugging-output)
      (chatgpt-shell-command-line prompt))))

(defun chatgpt-shell-command-line (prompt)
  "Send PROMPT and output to standard output."
  (let ((chatgpt-shell-insert-queries-inline nil)
        (worker-done nil)
        (buffered ""))
    (chatgpt-shell-send-to-buffer
     prompt nil nil
     (lambda (_command output _error finished)
       (setq buffered (concat buffered output))
       (when finished
         (setq worker-done t))))
    (while buffered
      (unless (string-empty-p buffered)
        (princ buffered #'external-debugging-output))
      (setq buffered "")
      (when worker-done
        (setq buffered nil))
      (sleep-for 0.1))
    (princ "\n")))

(defun chatgpt-shell--eshell-last-last-command ()
  "Get second to last eshell command."
  (save-excursion
    (if (string= major-mode "eshell-mode")
        (let ((cmd-start)
              (cmd-end))
          ;; Find command start and end positions
          (goto-char eshell-last-output-start)
          (re-search-backward eshell-prompt-regexp nil t)
          (setq cmd-start (point))
          (goto-char eshell-last-output-start)
          (setq cmd-end (point))

          ;; Find output start and end positions
          (goto-char eshell-last-output-start)
          (forward-line 1)
          (re-search-forward eshell-prompt-regexp nil t)
          (forward-line -1)
          (concat "What's wrong with this command?\n\n"
                  (buffer-substring-no-properties cmd-start cmd-end)))
      (message "Current buffer is not an eshell buffer."))))

;; Based on https://emacs.stackexchange.com/a/48215
(defun chatgpt-shell--source-eshell-string (string)
  "Execute eshell command in STRING."
  (interactive "sString to be evaluated as eshell script: ")
  (let ((orig (point))
        (here (point-max))
        (inhibit-point-motion-hooks t))
    (goto-char (point-max))
    (with-silent-modifications
      ;; FIXME: Use temporary buffer and avoid insert/delete.
      (insert string)
      (goto-char (point-max))
      (throw 'eshell-replace-command
             (prog1
                 (list 'let
                       (list (list 'eshell-command-name (list 'quote "source-string"))
                             (list 'eshell-command-arguments '()))
                       (eshell-parse-command (cons here (point))))
               (delete-region here (point))
               (goto-char orig))))))

(defun chatgpt-shell-add-??-command-to-eshell ()
  "Add `??' command to `eshell'."

  (defun eshell/?? (&rest _args)
    "Implements `??' eshell command."
    (interactive)
    (let ((prompt (concat
                   "What's wrong with the following command execution?\n\n"
                   (chatgpt-shell--eshell-last-last-command)))
          (prompt-file (concat temporary-file-directory
                               "chatgpt-shell-command-line-prompt")))
      (when (file-exists-p prompt-file)
        (delete-file prompt-file))
      (with-temp-file prompt-file nil nil t
                      (insert prompt))
      (chatgpt-shell--source-eshell-string
       (concat
        (file-truename (expand-file-name invocation-name invocation-directory)) " "
        "--quick --batch --eval "
        "'"
        (prin1-to-string
         `(progn
            (interactive)
            (load ,(find-library-name "chatgpt-shell") nil t)
            (require (intern "chatgpt-shell") nil t)
            (setq chatgpt-shell-model-temperature 0)
            (setq chatgpt-shell-openai-key ,(chatgpt-shell-openai-key))
            (chatgpt-shell-command-line-from-prompt-file ,prompt-file)))
        "'"))))

  (add-hook 'eshell-post-command-hook
            (defun chatgpt-shell--eshell-post-??-execution ()
              (when (string-match (symbol-name #'chatgpt-shell-command-line-from-prompt-file)
                                  (string-join eshell-last-arguments " "))
                (save-excursion
                  (save-restriction
                    (narrow-to-region (eshell-beginning-of-output)
                                      (eshell-end-of-output))
                    (chatgpt-shell--put-source-block-overlays))))))

  (require 'esh-cmd)

  (add-to-list 'eshell-complex-commands "??"))

(defun chatgpt-shell-send-to-buffer (text &optional review invert-insert-inline handler)
  "Send TEXT to *chatgpt* buffer.
Set REVIEW to make changes before submitting to ChatGPT.

When INVERT-INSERT-INLINE, invert `chatgpt-shell-insert-queries-inline' choice.

If passing HANDLER function, use it instead of inserting inline."
  (let* ((insert-inline (if invert-insert-inline
                            (not chatgpt-shell-insert-queries-inline)
                          chatgpt-shell-insert-queries-inline))
         (buffer (current-buffer))
         (point (point))
         (marker (copy-marker (point)))
         (orig-region-active (region-active-p)))
    (when (region-active-p)
      (setq marker (copy-marker (max (region-beginning)
                                     (region-end)))))
    (chatgpt-shell insert-inline)
    (cl-flet ((send ()
                    (when shell-maker--busy
                      (shell-maker-interrupt))
                    (goto-char (point-max))
                    (if review
                        (save-excursion
                          (insert text))
                      (insert text)
                      (shell-maker--send-input
                       (if insert-inline
                           (lambda (_command output error _finished)
                             (setq output (or output ""))
                             (with-current-buffer buffer
                               (if error
                                   (unless (string-empty-p (string-trim output))
                                     (message "%s" output))
                                 (save-excursion
                                   (if orig-region-active
                                       (progn
                                         (goto-char marker)
                                         (when (eq (marker-position marker)
                                                   point)
                                           (insert "\n\n")
                                           (set-marker marker (+ 2 (marker-position marker))))
                                         (insert output)
                                         (set-marker marker (+ (length output)
                                                               (marker-position marker))))
                                     (goto-char marker)
                                     (insert output)
                                     (set-marker marker (+ (length output)
                                                           (marker-position marker))))))))
                         (or handler (lambda (_command _output _error _finished))))
                       t))))
      (if insert-inline
          (with-current-buffer (shell-maker-buffer chatgpt-shell--config)
            (goto-char (point-max))
            (send))
        (with-selected-window
            (get-buffer-window (get-buffer-create "*chatgpt*"))
          (send))))))

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

(defun chatgpt-shell--api-url ()
  "The complete URL OpenAI's API.

`chatgpt-shell--api-url' =
   `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'"
  (concat chatgpt-shell-api-url-base chatgpt-shell-api-url-path))

(defun chatgpt-shell--make-curl-request-command-list (request-data)
  "Build ChatGPT curl command list using REQUEST-DATA."
  (append (list "curl" (chatgpt-shell--api-url))
          chatgpt-shell-additional-curl-options
          (list "--fail-with-body"
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
                "-d" (shell-maker--json-encode request-data))))

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
                                    (chatgpt-shell--resolve-internal-language lang)
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties body-start body-end))
        (buf (if (and (boundp 'shell-maker-config)
                      shell-maker-config)
                 (shell-maker-buffer shell-maker-config)
               (current-buffer)))
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
                             (if (and (boundp 'shell-maker-config)
                                      shell-maker-config)
                                 (shell-maker-buffer shell-maker-config)
                               (current-buffer)))
               'face 'font-lock-doc-markup-face))

(defun chatgpt-shell-rename-block-at-point ()
  "Rename block at point (perhaps a different language)."
  (interactive)
  (save-excursion
    (if-let ((block (chatgpt-shell-markdown-block-at-point)))
        (if (map-elt block 'language)
            (perform-replace (map-elt block 'language)
                             (read-string "Name: " nil nil "") nil nil nil nil nil
                             (map-elt block 'language-start) (map-elt block 'language-end))
          (let ((new-name (read-string "Name: " nil nil "")))
            (goto-char (map-elt block 'language-start))
            (insert new-name)
            (chatgpt-shell--put-source-block-overlays)))
      (user-error "No block at point"))))

(defun chatgpt-shell-remove-block-overlays ()
  "Remove block overlays.  Handy for renaming blocks."
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (delete-overlay overlay)))

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

(defun chatgpt-shell-run-command (command callback)
  "Run COMMAND list asynchronously and call CALLBACK function.

CALLBACK can be like:

\(lambda (success output)
  (message \"%s\" output))"
  (let* ((buffer (generate-new-buffer "*run command*"))
         (proc (apply #'start-process
                      (append `("exec" ,buffer) command))))
    (set-process-sentinel
     proc
     (lambda (proc _)
       (with-current-buffer buffer
         (funcall callback
                  (equal (process-exit-status proc) 0)
                  (buffer-string))
         (kill-buffer buffer))))))

(defun chatgpt-shell--resolve-internal-language (language)
  "Resolve external LANGUAGE to internal.

For example \"elisp\" -> \"emacs-lisp\"."
  (when language
    (or (map-elt chatgpt-shell-language-mapping
                 (downcase (string-trim language)))
        (when (intern (concat (downcase (string-trim language))
                              "-mode"))
          (downcase (string-trim language))))))

(defun chatgpt-shell-primary-block-action-at-point ()
  "Return t if block at point has primary action.  nil otherwise."
  (let* ((source-block (chatgpt-shell-markdown-block-at-point))
         (language (chatgpt-shell--resolve-internal-language
                    (map-elt source-block 'language)))
         (actions (chatgpt-shell--get-block-actions language)))
    actions
    (if actions
        actions
      (chatgpt-shell--org-babel-command language))))

(defun chatgpt-shell--get-block-actions (language)
  "Get block actions for LANGUAGE."
  (map-elt chatgpt-shell-source-block-actions
           (chatgpt-shell--resolve-internal-language
            language)))

(defun chatgpt-shell--org-babel-command (language)
  "Resolve LANGUAGE to org babel command."
  (require 'ob)
  (when language
    (ignore-errors
      (or (require (intern (concat "ob-" (capitalize language))) nil t)
          (require (intern (concat "ob-" (downcase language))) nil t)))
    (let ((f (intern (concat "org-babel-execute:" language)))
          (f-cap (intern (concat "org-babel-execute:" (capitalize language)))))
      (if (fboundp f)
          f
        (if (fboundp f-cap)
            f-cap)))))

(defun chatgpt-shell-execute-primary-block-action-at-point ()
  "Execute primary action for known block.

Actions are defined in `chatgpt-shell-languages-primary-action'.s"
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (if-let ((actions (chatgpt-shell--get-block-actions (map-elt block 'language)))
               (action (map-elt actions 'primary-action))
               (confirmation (map-elt actions 'primary-action-confirmation))
               (default-directory "/tmp"))
          (when (y-or-n-p confirmation)
            (funcall action (buffer-substring-no-properties
                             (map-elt block 'start)
                             (map-elt block 'end))))
        (if (and (map-elt block 'language)
                 (chatgpt-shell--org-babel-command
                  (chatgpt-shell--resolve-internal-language
                   (map-elt block 'language))))
            (chatgpt-shell-execute-babel-block-action-at-point)
          (user-error "No primary action for %s blocks" (map-elt block 'language))))
    (user-error "No block at point")))

(defun chatgpt-shell--override-language-params (language params)
  "Override PARAMS for LANGUAGE if found in `chatgpt-shell-babel-headers'."
  (if-let* ((overrides (map-elt chatgpt-shell-babel-headers
                                language))
            (temp-dir (file-name-as-directory
                       (make-temp-file "chatgpt-shell-" t)))
            (temp-file (concat temp-dir "source-block-" language)))
      (if (cdr (assq :file overrides))
          (append (list
                   (cons :file
                         (replace-regexp-in-string (regexp-quote "<temp-file>")
                                                   temp-file
                                                   (cdr (assq :file overrides)))))
                  (assq-delete-all :file overrides)
                  params)
        (append
         overrides
         params))
    params))

(defun chatgpt-shell-execute-babel-block-action-at-point ()
  "Execute block as org babel."
  (interactive)
  (require 'ob)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (if-let* ((language (chatgpt-shell--resolve-internal-language
                           (map-elt block 'language)))
                (babel-command (chatgpt-shell--org-babel-command language))
                (lang-headers (intern
                               (concat "org-babel-default-header-args:" language)))
                (bound (fboundp babel-command))
                (default-directory "/tmp"))
          (when (y-or-n-p (format "Execute %s ob block?" (capitalize language)))
            (message "Executing %s block..." (capitalize language))
            (let* ((params (org-babel-process-params
                            (chatgpt-shell--override-language-params
                             language
                             (org-babel-merge-params
                              org-babel-default-header-args
                              (and (boundp
                                    (intern
                                     (concat "org-babel-default-header-args:" language)))
                                   (eval (intern
                                          (concat "org-babel-default-header-args:" language)) t))))))
                   (output (progn
                             (when (get-buffer org-babel-error-buffer-name)
                               (kill-buffer (get-buffer org-babel-error-buffer-name)))
                             (funcall babel-command
                                      (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end)) params)))
                   (buffer))
              (if (and output (not (stringp output)))
                  (setq output (format "%s" output))
                (when (and (cdr (assq :file params))
                           (file-exists-p (cdr (assq :file params))))
                  (setq output (cdr (assq :file params)))))
              (if (and output (not (string-empty-p output)))
                  (progn
                    (setq buffer (get-buffer-create (format "*%s block output*" (capitalize language))))
                    (with-current-buffer buffer
                      (save-excursion
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (setq output (when output (string-trim output)))
                          (if (file-exists-p output) ;; Output was a file.
                              ;; Image? insert image.
                              (if (member (downcase (file-name-extension output))
                                          '("jpg" "jpeg" "png" "gif" "bmp" "webp"))
                                  (progn
                                    (insert "\n")
                                    (insert-image (create-image output)))
                                ;; Insert content of all other file types.
                                (insert-file-contents output))
                            ;; Just text output, insert that.
                            (insert output))))
                      (view-mode +1)
                      (setq view-exit-action 'kill-buffer))
                    (message "")
                    (select-window (display-buffer buffer)))
                (if (get-buffer org-babel-error-buffer-name)
                    (select-window (display-buffer org-babel-error-buffer-name))
                  (setq buffer (get-buffer-create (format "*%s block output*" (capitalize language))))
                  (message "No output. Check %s blocks work in your .org files." language)))))
        (user-error "No primary action for %s blocks" (map-elt block 'language)))
    (user-error "No block at point")))

(defun chatgpt-shell-eval-elisp-block-in-ielm (text)
  "Run elisp source in TEXT."
  (chatgpt-shell-send-to-ielm-buffer text t))

(defun chatgpt-shell-compile-swift-block (text)
  "Compile Swift source in TEXT."
  (when-let* ((source-file (chatgpt-shell-write-temp-file text ".swift"))
              (default-directory (file-name-directory source-file)))
    (chatgpt-shell-run-command
     `("swiftc" ,(file-name-nondirectory source-file))
     (lambda (success output)
       (if success
           (message
            (concat (propertize "Compiles cleanly" 'face '(:foreground "green"))
                    " :)"))
         (let ((buffer (generate-new-buffer "*block error*")))
           (with-current-buffer buffer
             (save-excursion
               (insert
                (chatgpt-shell--remove-compiled-file-names
                 (file-name-nondirectory source-file)
                 (ansi-color-apply output))))
             (compilation-mode)
             (view-mode +1)
             (setq view-exit-action 'kill-buffer))
           (select-window (display-buffer buffer)))
         (message
          (concat (propertize "Compilation failed" 'face '(:foreground "orange"))
                  " :(")))))))

(defun chatgpt-shell-write-temp-file (content extension)
  "Create a temporary file with EXTENSION and write CONTENT to it.

Return the file path."
  (let* ((temp-dir (file-name-as-directory
                    (make-temp-file "chatgpt-shell-" t)))
         (temp-file (concat temp-dir "source-block" extension)))
    (with-temp-file temp-file
      (insert content)
      (let ((inhibit-message t))
        (write-file temp-file)))
    temp-file))

(defun chatgpt-shell--remove-compiled-file-names (filename text)
  "Remove lines starting with FILENAME in TEXT.

Useful to remove temp file names from compilation output when
compiling source blocks."
  (replace-regexp-in-string
   (rx-to-string `(: bol ,filename (one-or-more (not (any " "))) " ") " ")
   "" text))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
