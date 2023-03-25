;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.2
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

(defcustom chatgpt-shell-prompt "ChatGPT> "
  "Prompt text."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version "gpt-3.5-turbo"
  "The used OpenAI model.

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

(defvar chatgpt-shell--busy)

(defvar chatgpt-shell--prompt-internal "ChatGPT> ")

(defvar chatgpt-shell--current-request-id 0)

(defvar chatgpt-shell--show-invisible-markers nil)

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-shell-map)

(defconst chatgpt-shell-font-lock-keywords
  '(;; Markdown triple backticks
    ("\\(^\\(```\\)[^`\n]*\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(^\\(```\\)$\\)"
     (3 'markdown-pre-face))
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

;;;###autoload
(defun chatgpt-shell ()
  "Start a ChatGPT shell."
  (interactive)
  (let ((old-point)
        (buf-name "*chatgpt*"))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create "*chatgpt*")
        (setq chatgpt-shell--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (inferior-chatgpt-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))

(define-derived-mode inferior-chatgpt-mode comint-mode "CHATGPT"
  "Major mode for interactively evaluating ChatGPT prompts.
Uses the interface provided by `comint-mode'"
  (visual-line-mode +1)
  (setq comint-prompt-regexp (concat "^" (regexp-quote chatgpt-shell-prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'chatgpt-shell--input-sender)
  (setq comint-process-echoes nil)
  (setq-local chatgpt-shell--prompt-internal chatgpt-shell-prompt)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'chatgpt-shell--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "chatgpt" (current-buffer) "hexl")
      (file-error (start-process "chatgpt" (current-buffer) "cat")))
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
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))

  (font-lock-add-keywords nil chatgpt-shell-font-lock-keywords))

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (chatgpt-shell--send-input))

(defun chatgpt-shell-prompt ()
  "Make a ChatGPT request from the minibuffer."
  (interactive)
  (chatgpt-shell-send-to-buffer (read-string chatgpt-shell-prompt))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "What does the following code do?\n\n"
           (buffer-substring (region-beginning) (region-end))))
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
  (switch-to-buffer (chatgpt-shell--buffer))
  (with-current-buffer (chatgpt-shell--buffer)
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
  (with-current-buffer (chatgpt-shell--buffer)
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
  (unless chatgpt-shell--busy
    (setq chatgpt-shell--busy t)
    (cond
     ((string-equal "clear" (string-trim input-string))
      (call-interactively #'comint-clear-buffer)
      (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt-internal)
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
        (chatgpt-shell--request-completion key)
        (setq chatgpt-shell--busy nil))))))

;; Maybe I should be a macro (get rid of callback), maybe not
(defun chatgpt-shell--request-completion (key)
  "Request a completion.

KEY is API key.  CALLBACK is called with a parsed response body,
where objects are converted into alists."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " key))
            ("Content-Type" . "application/json")))
         (messages
          (vconcat
           (last (chatgpt-shell--extract-commands-and-responses)
                 (if (null chatgpt-shell-transmitted-context-length)
                     ;; If variable above is nil, send "full" context.
                     ;; Arbitrarily chosen big number here to signify
                     ;; it
                     2048
                   ;; Send in pairs of prompt and completion by
                   ;; multiplying by 2
                   (1+ (* 2 chatgpt-shell-transmitted-context-length))))))
         (url-request-data `((model . ,chatgpt-shell-model-version)
                             (messages . ,messages)))
         ;; Add temperature parameter if it is not nil
         (url-request-data (if chatgpt-shell-model-temperature
                               (append url-request-data
                                       `((temperature . chatgpt-shell-model-temperature)))
                             url-request-data))
                                        ; newline not strictly
                                        ; necessary here, but it makes
                                        ; for easier logging for now
         (url-request-data (concat (json-encode url-request-data) "\n")))
    ;; Advice around `url-http-create-request' to get the raw request
    ;; message
    (advice-add #'url-http-create-request :filter-return #'chatgpt-shell--log-request)
    ;; implement timeouts using me
    (setq processing-buffer
          (url-retrieve "https://api.openai.com/v1/chat/completions"
                        #'chatgpt-shell--url-retrieve-callback))
    ;; (switch-to-buffer chatgpt-shell--url-processing-buffer)
    ))

(defun chatgpt-shell--url-retrieve-callback (status &optional cbargs)
  ""
  ;; move me somewhere else
  (advice-remove #'url-http-create-request #'chatgpt-shell--log-request)
  (chatgpt-shell--write-reply (format "%s" status) t)

  (let ((buffer-string (buffer-string))
        (status (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))))
    (chatgpt-shell--write-output-to-log-buffer buffer-string)
    ;; Something went wrong in the request, either here or on the
    ;; server, but at least we got a response
    (unless (= status 200)
      (chatgpt-shell--write-reply buffer-string t)))
  ;; straight to the body, who cares about content-types or content-lengths
  (let ((headers
         (url-http-symbol-value-in-buffer 'url-http-extra-headers (current-buffer))))
    (message "%s" headers))

  (search-forward "\n\n")
  (chatgpt-shell--write-reply
   (string-trim
    (map-elt
     (map-elt
      (seq-first (map-elt (json-parse-buffer :object-type 'alist) 'choices))
      'message) 'content))))

(defun chatgpt-shell--log-request (request)
  "Write REQUEST to log buffer and return REQUEST."
  (prog1
      request
    (chatgpt-shell--write-output-to-log-buffer request)))

(defun chatgpt-shell--increment-request-id ()
  "Increment `chatgpt-shell--current-request-id'."
  (if (= chatgpt-shell--current-request-id most-positive-fixnum)
      (setq chatgpt-shell--current-request-id 0)
    (setq chatgpt-shell--current-request-id (1+ chatgpt-shell--current-request-id))))

(defun chatgpt-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (chatgpt-shell--buffer))) pos))

(defun chatgpt-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (chatgpt-shell--buffer))))

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

(defun chatgpt-shell--extract-commands-and-responses ()
  "Extract all command and responses in buffer."
  (let ((result))
    (with-current-buffer (chatgpt-shell--buffer)
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
  "Write OUTPUT to log buffer.

Create the log buffer if it does not exist."
  (with-current-buffer (get-buffer-create chatgpt-shell--log-buffer-name)
    (let ((beginning-of-input (goto-char (point-max))))
      (insert output))))

(defun chatgpt-shell--buffer ()
  "Get *chatgpt* buffer."
  (get-buffer-create "*chatgpt*"))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (chatgpt-shell--buffer)))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
