;;; shell-maker.el --- Interaction mode for making comint shells  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.22.1
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

;; This is a comint-based generic package used for building concrete
;; shells.
;;
;; Much inspiration comes from IELM
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Interaction.html

;;; Code:

(require 'comint)
(require 'goto-addr)
(require 'json)
(require 'map)
(require 'seq)
(require 'shell)
(require 'view)

(eval-when-compile
  (require 'cl-lib)
  (declare-function json-pretty-print "ext:json" (begin end &optional minimize)))

(defcustom shell-maker-display-function #'pop-to-buffer-same-window
  "Function to display the shell.  Set to `display-buffer' or custom function."
  :type 'function
  :group 'shell-maker)

(defcustom shell-maker-read-string-function (lambda (prompt history)
                                              (read-string prompt nil history))
  "Function to read strings from user.

To use `completing-read', it can be done with something like:

\(setq `shell-maker-read-string-function'
      (lambda (prompt history)
        (completing-read prompt (symbol-value history) nil nil nil history)))"
  :type 'function
  :group 'shell-maker)

(defcustom shell-maker-logging nil
  "Logging disabled by default (slows things down).

Enable it for troubleshooting issues."
  :type 'boolean
  :group 'shell-maker)

(defcustom shell-maker-history-path user-emacs-directory
  "Root path to the location for storing history files."
  :type 'directory
  :group 'shell-maker)

(defvar shell-maker--input nil)

(defvar shell-maker--current-request-id 0)

(defvar shell-maker--show-invisible-markers nil)

(defconst shell-maker--prompt-rear-nonsticky
  '(field inhibit-line-move-field-capture read-only font-lock-face)
  "Text properties set on the prompt and don't want to leak past it.")

(cl-defstruct
    shell-maker-config
  name
  prompt
  prompt-regexp
  validate-command
  execute-command
  on-command-finished
  redact-log-output)

(defvar-local shell-maker--busy nil)

(defvar-local shell-maker-config nil)

(defvar-local shell-maker--file nil)

(defvar-local shell-maker--request-process nil)

(defvaralias 'shell-maker-mode-map 'shell-maker-map)

(defvar shell-maker-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map [remap comint-send-input] 'shell-maker-return)
    (define-key map [remap comint-interrupt-subjob] 'shell-maker-interrupt)
    (define-key map (kbd "C-x C-s") 'shell-maker-save-session-transcript)
    (define-key map (kbd "C-M-h") 'shell-maker-mark-output)
    (define-key map [remap comint-history-isearch-backward-regexp] 'shell-maker-search-history)
    map)
  "Keymap for `shell-maker' shells.")

(declare-function shell-maker-mode "shell-maker" ())

(defun shell-maker-start (config &optional no-focus)
  "Start a shell with CONFIG.

Specify NO-FOCUS if started shell should not be focused."
  (define-derived-mode shell-maker-mode comint-mode
    (shell-maker-config-name config)
    "Major mode for interactively evaluating shell-maker prompts.
Uses the interface provided by `comint-mode'"
    nil)

  (make-local-variable 'shell-maker-map)

  (let ((old-point)
        (buf-name (shell-maker-buffer-name config)))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create buf-name)
        (setq-local shell-maker--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (shell-maker-mode)
        (shell-maker--initialize config)))
    (unless no-focus
      (funcall shell-maker-display-function buf-name))
    (when old-point
      (push-mark old-point))))

(defun shell-maker--initialize (config)
  "Initialize shell using CONFIG."
  (setq-local shell-maker-config config)
  (visual-line-mode +1)
  (goto-address-mode +1)
  ;; Prevents fontifying streamed response as prompt.
  (setq comint-prompt-regexp
        (shell-maker-prompt-regexp config))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'shell-maker--input-sender)
  (setq comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'shell-maker--get-old-input)
  (setq-local comint-completion-addsuffix nil)
  (setq-local imenu-generic-expression
              `((nil ,(concat (shell-maker-prompt-regexp shell-maker-config) "\\(.*\\)") 1)))
  (shell-maker--read-input-ring-history config)
  (unless (or (comint-check-proc (shell-maker-buffer shell-maker-config))
              (get-buffer-process (shell-maker-buffer shell-maker-config)))
    (condition-case nil
        (start-process (shell-maker-process-name shell-maker-config)
                       (shell-maker-buffer shell-maker-config) "hexl")
      (file-error (start-process
                   (shell-maker-process-name shell-maker-config)
                   (shell-maker-buffer shell-maker-config) "cat")))
    (set-process-query-on-exit-flag (shell-maker--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (shell-maker--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (shell-maker--output-filter (shell-maker--process)
                                (shell-maker-prompt shell-maker-config))
    (set-marker comint-last-input-start (shell-maker--pm))
    (set-process-filter (get-buffer-process
                         (shell-maker-buffer shell-maker-config))
                        'shell-maker--output-filter)))

(defun shell-maker--write-reply (reply &optional failed)
  "Write REPLY to prompt.  Set FAILED to record failure."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (comint-output-filter (shell-maker--process)
                            (concat reply
                                    (if failed
                                        (propertize "<shell-maker-failed-command>"
                                                    'invisible (not shell-maker--show-invisible-markers))
                                      "")
                                    (shell-maker-prompt shell-maker-config))))))

(defun shell-maker-return ()
  "RET binding."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (shell-maker--send-input))

(defun shell-maker-search-history ()
  "Search comint input ring."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((candidate (completing-read
                    "History: "
                    (delete-dups
                     (seq-filter
                      (lambda (item)
                        (not (string-empty-p item)))
                      (ring-elements comint-input-ring))) nil t)))
    (delete-region (comint-line-beginning-position) (point-max))
    (insert candidate)))

(defun shell-maker-last-output ()
  "Get the last command output from the shell."
  (let ((proc (get-buffer-process (current-buffer))))
    (save-excursion
      (let* ((pmark (progn (goto-char (process-mark proc))
                           (forward-line 0)
                           (point-marker)))
             (output (buffer-substring comint-last-input-end pmark))
             (items (split-string output "<shell-maker-end-of-prompt>")))
        (if (> (length items) 1)
            (nth 1 items)
          (nth 0 items))))))

;; Thanks to https://www.n16f.net/blog/making-ielm-more-comfortable
(defun shell-maker--read-input-ring-history (config)
  "Read input ring history from file using CONFIG."
  (let ((path (shell-maker-history-file-path config)))
    (make-directory
     (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (comint-read-input-ring t))

(defun shell-maker--write-input-ring-history ()
  "Write input ring history to file."
  (with-file-modes #o600
    (comint-write-input-ring)))

(defun shell-maker--output-at-point ()
  "Output at point range with cons of start and end."
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((current-pos (point))
        (revert-pos)
        (start)
        (end)
        (prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
                      (point))))
    (when (>= (point) prompt-pos)
      (goto-char prompt-pos)
      (forward-line 0))
    (save-excursion
      (unless
          (cond
           ((re-search-backward "<shell-maker-end-of-prompt>" nil t)
            (forward-char (length "<shell-maker-end-of-prompt>"))
            t)
           ((re-search-backward
             (shell-maker-prompt-regexp shell-maker-config) nil t)
            (if (re-search-forward "<shell-maker-end-of-prompt>" nil t)
                t
              (end-of-line))
            t)
           (t
            nil))
        (setq revert-pos t))
      (setq start (point)))
    (save-excursion
      (unless (re-search-forward
               (shell-maker-prompt-regexp shell-maker-config)  nil t)
        (goto-char current-pos)
        (setq revert-pos t))
      (beginning-of-line)
      (setq end (point)))
    (when revert-pos
      (goto-char current-pos)
      (user-error "Not available"))
    (cons start end)))

(defun shell-maker-narrow-to-prompt ()
  "Narrow buffer to the command line (and any following command output) at point."
  (interactive)
  (let ((begin (shell-maker--prompt-begin-position)))
    (narrow-to-region
     begin
     (save-excursion
       (goto-char (shell-maker--prompt-end-position))
       (re-search-forward (shell-maker-prompt-regexp shell-maker-config) nil t)
       (if (= begin (shell-maker--prompt-begin-position))
           (point-max)
         (shell-maker--prompt-begin-position))))))

(defun shell-maker--prompt-end-position ()
  "Based on `shell--prompt-end-position'."
  (save-excursion
    (goto-char (shell-maker--prompt-begin-position))
    (comint-next-prompt 1)
    (point)))

(defun shell-maker-mark-output ()
  "If at latest prompt, mark last output.
Otherwise mark current output at location."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((current-pos (point))
        (revert-pos)
        (start)
        (end)
        (prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
                      (point))))
    (when (>= (point) prompt-pos)
      (goto-char prompt-pos)
      (forward-line -1)
      (end-of-line))
    (save-excursion
      (save-restriction
        (shell-maker-narrow-to-prompt)
        (unless
            (cond
             ((re-search-backward "<shell-maker-end-of-prompt>" nil t)
              (forward-char (length "<shell-maker-end-of-prompt>"))
              t)
             ((re-search-backward
               (shell-maker-prompt-regexp shell-maker-config) nil t)
              (if (re-search-forward "<shell-maker-end-of-prompt>" nil t)
                  t
                (end-of-line))
              t)
             (t
              nil))
          (setq revert-pos t))
        (setq start (point))))
    (save-excursion
      (save-restriction
        (shell-maker-narrow-to-prompt)
        (setq end (point-max))))
    (when revert-pos
      (goto-char current-pos)
      (user-error "Not available"))
    (set-mark (1- end))
    (goto-char (1+ start))))

(defun shell-maker--prompt-begin-position ()
  "Based on `shell--prompt-begin-position'."
  (save-excursion
    (let ((old-point (point)))
      (max
       (save-excursion
         (call-interactively #'comint-previous-prompt)
         (re-search-backward comint-prompt-regexp nil t)
         (point))
       (save-excursion
         (re-search-backward comint-prompt-regexp nil t)
         (point))
       (save-excursion
         (call-interactively #'comint-next-prompt)
         (re-search-backward comint-prompt-regexp nil t)
         (if (<= (point) old-point)
             (point)
           (point-min)))))))

(defun shell-maker-save-output ()
  "If at latest prompt, save last output.
Otherwise save current output at location."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (let ((orig-point (point))
        (orig-region-active (region-active-p))
        (orig-region-start (region-beginning))
        (orig-region-end (region-end)))
    (unwind-protect
        (progn
          (shell-maker-mark-output)
          (write-region (region-beginning)
                        (region-end)
                        (read-file-name "Write file: ")))
      (if orig-region-active
          (progn
            (set-mark orig-region-start)
            (goto-char orig-region-end))
        (setq mark-active nil)
        (goto-char orig-point)))))

(defun shell-maker-interrupt ()
  "Interrupt current request."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (with-current-buffer (shell-maker-buffer shell-maker-config)
    ;; Increment id, so in-flight request is ignored.
    (shell-maker--increment-request-id)
    (comint-send-input)
    (goto-char (point-max))
    (shell-maker--output-filter (shell-maker--process)
                                (concat (propertize "<shell-maker-failed-command>"
                                                    'invisible (not shell-maker--show-invisible-markers))
                                        "\n"
                                        (shell-maker-prompt shell-maker-config)))
    (when (process-live-p shell-maker--request-process)
      (kill-process shell-maker--request-process))
    (when shell-maker--busy
      (message "interrupted!"))
    (setq shell-maker--busy nil)))

(defun shell-maker--eval-input (input-string &optional on-output no-announcement)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result.

Use ON-OUTPUT function to handle outcome.

For example:

\(lambda (command output error finished)
   (message \"Command: %s\" command)
   (message \"Output: %s\" output)
   (message \"Has error: %s\" output)
   (message \"Is finished: %s\" finished))

NO-ANNOUNCEMENT skips announcing response when in background."
  (let ((buffer (shell-maker-buffer shell-maker-config))
        (prefix-newline "")
        (suffix-newline "\n\n")
        (response-count 0)
        (errored))
    (unless shell-maker--busy
      (setq shell-maker--busy t)
      (cond
       ((string-equal "clear" (string-trim input-string))
        (call-interactively #'comint-clear-buffer)
        (shell-maker--output-filter (shell-maker--process) (shell-maker-prompt shell-maker-config))
        (setq shell-maker--busy nil))
       ((not (shell-maker--curl-version-supported))
        (shell-maker--write-reply "\nYou need curl version 7.76 or newer.\n\n")
        (setq shell-maker--busy nil))
       ((and (shell-maker-config-validate-command
              shell-maker-config)
             (funcall (shell-maker-config-validate-command
                       shell-maker-config) input-string))
        (shell-maker--write-reply
         (concat "\n"
                 (funcall (shell-maker-config-validate-command
                           shell-maker-config) input-string)
                 "\n\n"))
        (setq shell-maker--busy nil))
       ((string-empty-p (string-trim input-string))
        (shell-maker--output-filter (shell-maker--process)
                                    (concat "\n" (shell-maker-prompt shell-maker-config)))
        (setq shell-maker--busy nil))
       (t
        ;; For viewing prompt delimiter (used to handle multiline prompts).
        ;; (shell-maker--output-filter (shell-maker--process) "<shell-maker-end-of-prompt>")
        (shell-maker--output-filter (shell-maker--process)
                                    (propertize "<shell-maker-end-of-prompt>"
                                                'invisible (not shell-maker--show-invisible-markers)))
        (funcall (shell-maker-config-execute-command shell-maker-config)
                 input-string
                 (shell-maker--extract-history
                  (with-current-buffer buffer
                    (buffer-string))
                  (shell-maker-prompt-regexp shell-maker-config))
                 (lambda (response partial)
                   (setq response-count (1+ response-count))
                   (setq prefix-newline (if (> response-count 1)
                                            ""
                                          "\n"))
                   (if response
                       (if partial
                           (progn
                             (shell-maker--write-partial-reply (concat prefix-newline response))
                             (setq shell-maker--busy partial)
                             (when on-output
                               (funcall on-output
                                        input-string response nil partial)))
                         (shell-maker--write-reply (concat prefix-newline response suffix-newline))
                         (unless no-announcement
                           (shell-maker--announce-response buffer))
                         (setq shell-maker--busy nil)
                         (shell-maker--write-input-ring-history)
                         (when (shell-maker-config-on-command-finished shell-maker-config)
                           ;; FIXME use (concat prefix-newline response suffix-newline) if not streaming.
                           (when on-output
                             (funcall on-output
                                      input-string response nil t))
                           (funcall (shell-maker-config-on-command-finished shell-maker-config)
                                    input-string
                                    (shell-maker-last-output))))
                     (shell-maker--write-reply "Error: that's all is known" t) ;; comeback
                     (setq shell-maker--busy nil)
                     (unless no-announcement
                       (shell-maker--announce-response buffer))
                     (when on-output
                       (funcall on-output
                                input-string (shell-maker-last-output) t t))))
                 (lambda (error)
                   (unless errored
                     (shell-maker--write-reply (concat (string-trim error) suffix-newline) t)
                     (setq errored t))
                   (setq shell-maker--busy nil)
                   (unless no-announcement
                     (shell-maker--announce-response buffer))
                   (when on-output
                     (funcall on-output
                              input-string error t t)))))))))

(defun shell-maker--announce-response (buffer)
  "Announce response if BUFFER is not active."
  (unless (eq buffer (window-buffer (selected-window)))
    (message "%s responded" (buffer-name buffer))))

(defun shell-maker--curl-exit-status-from-error-string (string)
  "Extract exit status from curl error STRING."
  (when (string-match (rx "curl: (" (group (one-or-more digit)) ")") string)
    (string-to-number (match-string 1 string))))

(defun shell-maker-async-shell-command (command streaming response-extractor callback error-callback)
  "Run shell COMMAND asynchronously.
Set STREAMING to enable it.  Calls RESPONSE-EXTRACTOR to extract the
response and feeds it to CALLBACK or ERROR-CALLBACK accordingly."
  (let* ((buffer (shell-maker-buffer shell-maker-config))
         (request-id (shell-maker--increment-request-id))
         (output-buffer (generate-new-buffer " *temp*"))
         (config shell-maker-config)
         (request-process (condition-case err
                              (apply #'start-process (append (list
                                                              (shell-maker-buffer-name shell-maker-config)
                                                              (buffer-name output-buffer))
                                                             command))
                            (error
                             (with-current-buffer buffer
                               (funcall error-callback (error-message-string err)))
                             nil)))
         (preparsed)
         (remaining-text)
         (process-connection-type nil))
    (when request-process
      (setq shell-maker--request-process request-process)
      (shell-maker--write-output-to-log-buffer "// Request\n\n" config)
      (shell-maker--write-output-to-log-buffer (string-join command " ") config)
      (shell-maker--write-output-to-log-buffer "\n\n" config)
      (when streaming
        (set-process-filter
         request-process
         (lambda (_process output)
           (when (and (eq request-id shell-maker--current-request-id)
                      (buffer-live-p buffer))
             (shell-maker--write-output-to-log-buffer
              (format "// Filter output\n\n%s\n\n" output) config)
             (setq remaining-text (concat remaining-text output))
             (setq preparsed (shell-maker--preparse-json remaining-text))
             (if (car preparsed)
                 (mapc (lambda (obj)
                         (with-current-buffer buffer
                           (funcall callback (funcall response-extractor obj) t)))
                       (car preparsed))
               (with-current-buffer buffer
                 (let ((curl-exit-code (shell-maker--curl-exit-status-from-error-string (cdr preparsed))))
                   (cond ((eq 0 curl-exit-code)
                          (funcall callback (cdr preparsed) t))
                         ((numberp curl-exit-code)
                          (funcall error-callback (string-trim (cdr preparsed))))
                         (t
                          (funcall callback (cdr preparsed) t))))))
             (setq remaining-text (cdr preparsed))))))
      (set-process-sentinel
       request-process
       (lambda (process _event)
         (when-let ((active (and (eq request-id shell-maker--current-request-id)
                                 (buffer-live-p buffer)))
                    (output (with-current-buffer (process-buffer process)
                              (buffer-string)))
                    (exit-status (process-exit-status process)))
           (shell-maker--write-output-to-log-buffer
            (format "// Response (%s)\n\n" (if active "active" "inactive")) config)
           (shell-maker--write-output-to-log-buffer
            (format "Exit status: %d\n\n" exit-status) config)
           (shell-maker--write-output-to-log-buffer output config)
           (shell-maker--write-output-to-log-buffer "\n\n" config)
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
                   (funcall error-callback output))))))
         (kill-buffer output-buffer))))))

(defun shell-maker--json-parse-string-filtering (json regexp)
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

(defun shell-maker--increment-request-id ()
  "Increment `shell-maker--current-request-id'."
  (if (= shell-maker--current-request-id most-positive-fixnum)
      (setq shell-maker--current-request-id 0)
    (setq shell-maker--current-request-id (1+ shell-maker--current-request-id))))

(defun shell-maker--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark
               (get-buffer-process
                (shell-maker-buffer shell-maker-config))) pos))

(defun shell-maker--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process
                 (shell-maker-buffer shell-maker-config))))

(defun shell-maker--input-sender (_proc input)
  "Set the variable `shell-maker--input' to INPUT.
Used by `shell-maker--send-input's call."
  (setq shell-maker--input input))

(defun shell-maker--send-input (&optional on-output no-announcement)
  "Send text after the prompt.

Use ON-OUTPUT function to handle outcome.

For example:

\(lambda (command output error finished)
   (message \"Command: %s\" command)
   (message \"Output: %s\" output)
   (message \"Has error: %s\" output)
   (message \"Is finished: %s\" finished))

NO-ANNOUNCEMENT skips announcing response when in background."
  (let (shell-maker--input)
    (comint-send-input)
    (shell-maker--eval-input shell-maker--input on-output no-announcement)))

(defun shell-maker--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun shell-maker--json-encode (obj)
  "Serialize OBJ to json.  Use fallback if `json-serialize' isn't available."
  (if (fboundp 'json-serialize)
      (json-serialize obj)
    (json-encode obj)))

(defun shell-maker--curl-version-supported ()
  "Return t if curl version is 7.76 or newer, nil otherwise."
  (let* ((curl-error-redirect (if (eq system-type (or 'windows-nt 'ms-dos)) "2> NUL" "2>/dev/null"))
         (curl-version-string (shell-command-to-string (concat "curl --version " curl-error-redirect))))
    (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" curl-version-string)
      (let ((version (match-string 1 curl-version-string)))
        (version<= "7.76" version)))))

(defun shell-maker--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (json-parse-error nil))
    (condition-case _err
        (json-read-from-string json)
      (error nil))))

(defun shell-maker--write-partial-reply (reply)
  "Write partial REPLY to prompt."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (shell-maker--output-filter (shell-maker--process) reply))))

(defun shell-maker--preparse-json (json)
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

(defun shell-maker--command-and-response-at-point ()
  "Extract the current command and response in buffer."
  (save-excursion
    (save-restriction
      (shell-maker-narrow-to-prompt)
      (let ((items (shell-maker--extract-history
                    (buffer-string)
                    (shell-maker-prompt shell-maker-config))))
        (cl-assert (or (seq-empty-p items)
                       (eq (length items) 1)))
        items))))

(defun shell-maker--write-output-to-log-buffer (output config)
  "Write OUTPUT to log buffer using CONFIG."
  (when (and shell-maker-logging config)
    (when (shell-maker-config-redact-log-output config)
      (setq output
            (funcall (shell-maker-config-redact-log-output config) output)))
    (with-current-buffer (get-buffer-create (format "*%s-log*"
                                                    (shell-maker-process-name config)))
      (let ((beginning-of-input (goto-char (point-max))))
        (insert output)
        (when (and (require 'json nil t)
                   (ignore-errors (shell-maker--json-parse-string output)))
          (json-pretty-print beginning-of-input (point)))))))

(defun shell-maker--process nil
  "Get shell buffer process."
  (get-buffer-process (shell-maker-buffer shell-maker-config)))

(defun shell-maker-save-session-transcript ()
  "Save shell transcript to file.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (if shell-maker--file
      (let ((content (buffer-string))
            (path shell-maker--file))
        (with-temp-buffer
          (insert content)
          (write-file path nil)))
    (when-let ((path (read-file-name "Write file: "
                                     nil nil nil "transcript.txt"))
               (content (buffer-string)))
      (with-temp-buffer
        (insert content)
        (write-file path t))
      (setq shell-maker--file path))))

(defun shell-maker--extract-history (text prompt-regexp)
  "Extract all commands and respective output in TEXT with PROMPT-REGEXP.

Returns a list of (command . output) cons."
  (setq text (substring-no-properties text))
  (let ((result))
    (mapc (lambda (item)
            (let* ((values (split-string item "<shell-maker-end-of-prompt>"))
                   (lines (split-string item "\n"))
                   (prompt (string-trim (nth 0 values)))
                   (response (string-trim (progn
                                            (if (> (length values) 1)
                                                (nth 1 values)
                                              (string-join
                                               (cdr lines) "\n"))))))
              (unless (string-match "<shell-maker-failed-command>" response)
                (when (or (not (string-empty-p prompt))
                          (not (string-empty-p response)))
                  (push (cons (if (string-empty-p prompt)
                                  nil
                                prompt)
                              (if (string-empty-p response)
                                  nil
                                response))
                        result)))))
          (split-string text prompt-regexp))
    (nreverse result)))

(defun shell-maker--output-filter (process string)
  "Copy of `comint-output-filter' but avoids fontifying non-prompt text.

Uses PROCESS and STRING same as `comint-output-filter'."
  (when-let ((oprocbuf (process-buffer process)))
    (with-current-buffer oprocbuf
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (goto-char (process-mark process))
          (set-marker comint-last-output-start (point))
          (insert string)
          (set-marker (process-mark process) (point))
          (goto-char (process-mark process))
          (unless comint-use-prompt-regexp
            (with-silent-modifications
              (add-text-properties comint-last-output-start (point)
                                   `(rear-nonsticky
                                     ,shell-maker--prompt-rear-nonsticky
                                     front-sticky
                                     (field inhibit-line-move-field-capture)
                                     field output
                                     inhibit-line-move-field-capture t))))
          (when-let* ((prompt-start (save-excursion (forward-line 0) (point)))
                      (inhibit-read-only t)
                      (prompt (string-match
                               comint-prompt-regexp
                               (buffer-substring prompt-start (point)))))
            (with-silent-modifications
              (or (= (point-min) prompt-start)
                  (get-text-property (1- prompt-start) 'read-only)
                  (put-text-property (1- prompt-start)
                                     prompt-start 'read-only 'fence))
              (add-text-properties prompt-start (point)
                                   '(read-only t front-sticky (read-only))))
            (when comint-last-prompt
              (font-lock--remove-face-from-text-property
               (car comint-last-prompt)
               (cdr comint-last-prompt)
               'font-lock-face
               'comint-highlight-prompt))
            (setq comint-last-prompt
                  (cons (copy-marker prompt-start) (point-marker)))
            (font-lock-append-text-property prompt-start (point)
                                            'font-lock-face
                                            'comint-highlight-prompt)
            (add-text-properties prompt-start (point)
                                 `(rear-nonsticky
                                   ,shell-maker--prompt-rear-nonsticky))))))))

(defun shell-maker-buffer (config)
  "Get buffer from CONFIG."
  (get-buffer-create (shell-maker-buffer-name config)))

(defun shell-maker-buffer-name (config)
  "Get buffer name from CONFIG."
  (concat "*"
          (downcase (shell-maker-config-name config))
          "*"))

(defun shell-maker-process-name (config)
  "Get process name from CONFIG."
  (downcase (shell-maker-config-name config)))

(defun shell-maker-history-file-path (config)
  "Get process name from CONFIG."
  (expand-file-name (concat
                     (file-name-as-directory
                      (downcase (shell-maker-config-name config)))
                     "history") shell-maker-history-path))

(defun shell-maker-prompt (config)
  "Get prompt from CONFIG."
  (if (shell-maker-config-prompt config)
      (shell-maker-config-prompt config)
    (concat (shell-maker-config-name config) "> ")))

(defun shell-maker-set-prompt (prompt prompt-regexp)
  "Set internal config's PROMPT and PROMPT-REGEXP."
  (unless (eq major-mode 'shell-maker-mode)
    (user-error "Not in a shell"))
  (setf (shell-maker-config-prompt shell-maker-config)
        prompt)
  (setf (shell-maker-config-prompt-regexp shell-maker-config)
        prompt-regexp)
  ;; Prevents fontifying streamed response as prompt.
  (setq comint-prompt-regexp prompt-regexp)
  (setq-local imenu-generic-expression
              `((nil ,(concat "\\(" prompt-regexp "\\)" "\\(.*\\)") 2))))

(defun shell-maker-prompt-regexp (config)
  "Get prompt regexp from CONFIG."
  (if (shell-maker-config-prompt-regexp config)
      (shell-maker-config-prompt-regexp config)
    (concat "^" (shell-maker-prompt config))))

(provide 'shell-maker)

;;; shell-maker.el ends here
