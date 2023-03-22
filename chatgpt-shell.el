;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.1
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

(require 'map)
(require 'cl-lib)
(require 'comint)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt "ChatGPT> "
  "Prompt text."
  :type 'string
  :group 'chatgpt-shell)

(defvar chatgpt-shell--input)

(defvar chatgpt-shell--busy)

(defvar chatgpt-shell--prompt-internal "ChatGPT> ")

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-shell-map)

(defconst chatgpt-shell-font-lock-keywords
  '(;; Markdown triple backticks
    ("\\(^\\(```\\)[^`\n]*\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(^\\(```\\)$\\)"
     (3 'markdown-pre-face))
    ;; Markdown single backticks
    ("`\\([^`\n]+\\)`"
     (1 'markdown-inline-code-face))))

(defvar chatgpt-shell-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'chatgpt-shell-return)
    map)
  "Keymap for ChatGPT mode.")

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

(defun chatgpt-shell--eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (unless chatgpt-shell--busy
    (setq chatgpt-shell--busy t)
    (cond
     ((string-equal "clear" (string-trim input-string))
      (call-interactively 'comint-clear-buffer)
      (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt-internal)
      (setq chatgpt-shell--busy nil))
     ((not chatgpt-shell-openai-key)
      (chatgpt-shell--write-reply
       "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")")
      (setq chatgpt-shell--busy nil))
     ((string-empty-p (string-trim input-string))
      (comint-output-filter (chatgpt-shell--process)
                            (concat "\n" chatgpt-shell--prompt-internal))
      (setq chatgpt-shell--busy nil))
     (t
      ;; For viewing prompt delimiter (used to handle multiline prompts).
      ;; (comint-output-filter (chatgpt-shell--process) "<gpt-end-of-prompt>")
      (comint-output-filter (chatgpt-shell--process)
                            (propertize "<gpt-end-of-prompt>" 'invisible t))
      (when-let ((key (cond ((stringp chatgpt-shell-openai-key)
                             chatgpt-shell-openai-key)
                            ((functionp chatgpt-shell-openai-key)
                             (condition-case err
                                 (funcall chatgpt-shell-openai-key)
                               (error
                                (chatgpt-shell--write-reply (error-message-string err))
                                (setq chatgpt-shell--busy nil)
                                nil))))))
        (chatgpt-shell--async-shell-command (chatgpt-shell--make-request-command-list
                                             ;; For no-context queries (ie. no history).
                                             ;; (vector (list (cons 'role "user")
                                             ;;             (cons 'content input-string)))
                                             (vconcat (chatgpt-shell--extract-commands-and-responses))
                                             key)
                                            (lambda (response)
                                              (if-let ((content (chatgpt-shell--extract-content response)))
                                                  (chatgpt-shell--write-reply content)
                                                (chatgpt-shell--write-reply "Error: that's all I know"))
                                              (setq chatgpt-shell--busy nil))
                                            (lambda (error)
                                              (chatgpt-shell--write-reply error)
                                              (setq chatgpt-shell--busy nil)))) ))))

(defun chatgpt-shell--async-shell-command (command callback error-callback)
  "Run shell COMMAND asynchronously.
Calls CALLBACK and ERROR-CALLBACK with its output when finished."
  (let ((output-buffer (generate-new-buffer " *temp*"))
        (process-connection-type nil))
    (set-process-sentinel
     (apply #'start-process (append (list "ChatGPT" (buffer-name output-buffer))
                                    command))
     (lambda (process _event)
       (let ((output (with-current-buffer (process-buffer process)
                       (buffer-string))))
         (if (= (process-exit-status process) 0)
             (funcall callback output)
           (funcall error-callback output))
         (kill-buffer output-buffer))))))

(defun chatgpt-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (chatgpt-shell--buffer))) pos))

(defun chatgpt-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (chatgpt-shell--buffer))))

(defun chatgpt-shell--input-sender (_proc input)
  "Set the variable chatgpt-shell--input to INPUT.
Used by `chatgpt-shell--send-input's call."
  (setq chatgpt-shell--input input))

(defun chatgpt-shell--send-input ()
  "Send text after the prompt."
  (interactive)
  (let (chatgpt-shell--input)
    (comint-send-input)
    (chatgpt-shell--eval-input chatgpt-shell--input)))

(defun chatgpt-shell--write-reply (reply)
  "Write REPLY to prompt."
  (comint-output-filter (chatgpt-shell--process)
                        (concat "\n" reply "\n\n" chatgpt-shell--prompt-internal)))

(defun chatgpt-shell--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun chatgpt-shell--make-request-command-list (messages key)
  "Build ChatGPT curl command list using MESSAGES and KEY."
  (cl-assert chatgpt-shell-openai-key nil "`chatgpt-shell-openai-key' needs to be set with your key")
  (list "curl"
        "https://api.openai.com/v1/chat/completions"
        "--fail" "--no-progress-meter" "-m" "30"
        "-H" "Content-Type: application/json"
        "-H" (format "Authorization: Bearer %s" key)
        "-d" (json-serialize `((model . "gpt-3.5-turbo")
                               (messages . ,messages)
                               (temperature . 0.7)))))

(defun chatgpt-shell--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (condition-case nil
      (json-parse-string json :object-type 'alist)
    (json-parse-error nil)))

(defun chatgpt-shell--extract-content (json)
  "Extract ChatGPT response from JSON."
  (when-let (parsed (chatgpt-shell--json-parse-string json))
    (string-trim
     (map-elt (map-elt (seq-first (map-elt parsed 'choices))
                       'message)
              'content))))

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
                (when (not (string-empty-p prompt))
                  (push (list (cons 'role "user")
                              (cons 'content prompt)) result))
                (when (not (string-empty-p response))
                  (push (list (cons 'role "system")
                              (cons 'content response)) result))))
            (split-string (substring-no-properties (buffer-string))
                          chatgpt-shell--prompt-internal)))
    (nreverse result)))

(defun chatgpt-shell--buffer ()
  "Get *chatgpt* buffer."
  (get-buffer-create "*chatgpt*"))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (chatgpt-shell--buffer)))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
