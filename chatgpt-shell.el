;;; chatgpt-shell.el --- interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.1

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

;; Note: This is very much a proof of concept (and very rough!). Much
;; of the code is based on `ielm'.
;;
;; You must set `chatgpt-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.

;;; Code:

(defcustom chatgpt-openai-key nil "Default major mode" :type  'boolean)

(defcustom chatgpt-prompt "ChatGPT> " nil :type 'string)

(defvar chatgpt--input)

(defvar chatgpt--busy)

(defvar chatgpt--prompt-internal "ChatGPT> ")

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-map)

(defvar chatgpt-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'chatgpt-return)
    map)
  "Keymap for ChatGPT mode.")

(defun chatgpt-shell ()
  "Start a ChatGPT shell."
  (interactive)
  (let ((old-point)
        (buf-name "*chatgpt*"))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create "*chatgpt*")
        (setq chatgpt--busy nil)
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
  (setq comint-prompt-regexp (concat "^" (regexp-quote chatgpt-prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'chatgpt--input-sender)
  (setq comint-process-echoes nil)
  (setq-local chatgpt--prompt-internal chatgpt-prompt)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'chatgpt--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "chatgpt" (current-buffer) "hexl")
      (file-error (start-process "chatgpt" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (chatgpt--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (chatgpt--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (chatgpt--process) chatgpt--prompt-internal)
    (set-marker comint-last-input-start (chatgpt--pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun chatgpt-return (&optional for-effect)
  "RET binding."
  (interactive)
  (let ((state
         (save-excursion
           (end-of-line)
           (parse-partial-sexp (chatgpt--pm)
                               (point)))))
    (if (and (< (car state) 1) (not (nth 3 state)))
        (chatgpt--send-input for-effect)
      (when (save-excursion
              (beginning-of-line)
              (looking-at-p comint-prompt-regexp))
        (save-excursion
          (goto-char (chatgpt--pm))
          (newline 1)))
      (newline-and-indent))))

(defun chatgpt--eval-input (input-string &optional for-effect)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (let ((pmark (chatgpt--pm)))
    (unless chatgpt--busy
      (setq chatgpt--busy t)
      (cond
       ((string-equal "clear" (string-trim input-string))
        (call-interactively 'comint-clear-buffer)
        (comint-output-filter (chatgpt--process) chatgpt--prompt-internal)
        (setq chatgpt--busy nil))
       ((not chatgpt-openai-key)
        (chatgpt--write-reply "`chatgpt-openai-key' needs to be set to your key")
        (setq chatgpt--busy nil))
       ((string-empty-p (string-trim input-string))
        (comint-output-filter (chatgpt--process)
                              (concat "\n" chatgpt--prompt-internal))
        (setq chatgpt--busy nil))
       (t
        ;; For viewing prompt delimiter (used to handle multiline prompts).
        ;; (comint-output-filter (chatgpt--process) "<gpt-end-of-prompt>")
        (comint-output-filter (chatgpt--process)
                              (propertize "<gpt-end-of-prompt>" 'invisible t))
        (chatgpt--async-shell-command (chatgpt--make-request-command-list
                                       (vconcat (chatgpt--extract-commands-and-responses))
                                       ;; For no-context queries (ie. no history).
                                       ;; (vector (list (cons 'role "user")
                                       ;;             (cons 'content input-string)))
                                       )
                                      (lambda (response)
                                        (if-let ((content (chatgpt--extract-content response)))
                                            (chatgpt--write-reply content)
                                          (chatgpt--write-reply "Error: that's all I know"))
                                        (setq chatgpt--busy nil))
                                      (lambda (error)
                                        (chatgpt--write-reply error)
                                        (setq chatgpt--busy nil))))))))

(defun chatgpt--async-shell-command (command callback error-callback)
  "Run shell COMMAND asynchronously and call CALLBACK with its output when finished."
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

(defun chatgpt--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (chatgpt--buffer))) pos))

(defun chatgpt--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (chatgpt--buffer))))

(defun chatgpt--input-sender (_proc input)
  "Set the variable chatgpt--input for `chatgpt--send-input's call."
  (setq chatgpt--input input))

(defun chatgpt--send-input (&optional for-effect)
  "Send text after the prompt."
  (interactive)
  (let (chatgpt--input)
    (comint-send-input)
    (chatgpt--eval-input chatgpt--input for-effect)))

(defun chatgpt--write-reply (reply)
  (comint-output-filter (chatgpt--process)
                        (concat "\n" reply "\n\n" chatgpt--prompt-internal)))

(defun chatgpt--get-old-input nil
  "Return the previous input surrounding point"
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun chatgpt--make-request-command-list (messages)
  "Build ChatGPT curl command list using MESSAGES."
  (cl-assert chatgpt-openai-key nil "`chatgpt-openai-key' needs to be set with your key")
  (list "curl"
        "https://api.openai.com/v1/chat/completions"
        "--fail" "--no-progress-meter" "-m" "30"
        "-H" "Content-Type: application/json"
        "-H" (format "Authorization: Bearer %s" chatgpt-openai-key)
        "-d" (json-serialize `((model . "gpt-3.5-turbo")
                               (messages . ,messages)
                               (temperature . 0.7)))))

(defun chatgpt--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (condition-case nil
      (json-parse-string json :object-type 'alist)
    (json-parse-error nil)))

(defun chatgpt--extract-content (json)
  "Extract ChatGPT response from JSON."
  (when-let (parsed (chatgpt--json-parse-string json))
    (string-trim
     (map-elt (map-elt (seq-first (map-elt parsed 'choices))
                       'message)
              'content))))

(defun chatgpt--extract-commands-and-responses ()
  (let ((result))
    (with-current-buffer (chatgpt--buffer)
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
                          chatgpt--prompt-internal)))
    (nreverse result)))

(defun chatgpt--buffer ()
  (get-buffer-create "*chatgpt*"))

(defun chatgpt--process nil
  (get-buffer-process (chatgpt--buffer)))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
