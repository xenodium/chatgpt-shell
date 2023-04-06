;;; mk-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

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

(defvar-local chatgpt-shell--busy nil)

(defvar-local chatgpt-shell--config nil)

(defvaralias 'chatgpt-shell-mode-map 'chatgpt-shell-map)

(defvar-local chatgpt-shell--file nil)

(defvar-local chatgpt-shell--request-process nil)

(defvar chatgpt-shell-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'chatgpt-shell-return)
    (define-key map "\C-c\C-c" 'chatgpt-shell-interrupt)
    (define-key map "\C-x\C-s" 'chatgpt-shell-save-session-transcript)
    (define-key map "\C-\M-h" 'chatgpt-shell-mark-output)
    map)
  "Keymap for ChatGPT mode.")

(define-derived-mode chatgpt-shell-mode comint-mode "CHATGPT"
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
  (setq-local imenu-generic-expression
              `(("Prompt" ,(concat "^" (regexp-quote
                                        (chatgpt-shell-config-prompt chatgpt-shell--config))
                                   "\\(.*\\)") 1)))
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

;; FIXME: Move to chatgpt-shell.
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
    ;; FIXME: Move to chatgpt-shell.
    (chatgpt-shell--put-source-block-overlays)))

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (chatgpt-shell--send-input))

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

(defun chatgpt-shell-mark-output ()
  "If at latest prompt, mark last output.
Otherwise mark current output at location."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
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
           ((re-search-backward "<gpt-end-of-prompt>" nil t)
            (forward-char (length "<gpt-end-of-prompt>"))
            t)
           ((re-search-backward
             (concat "^"
                     (chatgpt-shell-config-prompt chatgpt-shell--config))nil t)
            (if (re-search-forward "<gpt-end-of-prompt>" nil t)
                t
              (end-of-line))
            t)
           (t
            nil))
        (setq revert-pos t))
      (setq start (point)))
    (save-excursion
      (unless (re-search-forward
               (concat "^"
                       (chatgpt-shell-config-prompt chatgpt-shell--config)) nil t)
        (goto-char current-pos)
        (setq revert-pos t))
      (backward-char (length (chatgpt-shell-config-prompt chatgpt-shell--config)))
      (setq end (point)))
    (when revert-pos
      (goto-char current-pos)
      (user-error "Not available"))
    (set-mark (1- end))
    (goto-char (1+ start))))

(defun chatgpt-shell-save-output ()
  "If at latest prompt, save last output.
Otherwise save current output at location."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let ((orig-point (point))
        (orig-region-active (region-active-p))
        (orig-region-start (region-beginning))
        (orig-region-end (region-end)))
    (unwind-protect
        (progn
          (chatgpt-shell-mark-output)
          (write-region (region-beginning)
                        (region-end)
                        (read-file-name "Write file: ")))
      (if orig-region-active
          (progn
            (set-mark orig-region-start)
            (goto-char orig-region-end))
        (setq mark-active nil)
        (goto-char orig-point)))))

(defun chatgpt-shell-interrupt ()
  "Interrupt current request."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (with-current-buffer (chatgpt-shell--buffer chatgpt-shell--config)
    ;; Increment id, so in-flight request is ignored.
    (chatgpt-shell--increment-request-id)
    (comint-send-input)
    (goto-char (point-max))
    (comint-output-filter (chatgpt-shell--process)
                          (concat (propertize "<gpt-ignored-response>"
                                              'invisible (not chatgpt-shell--show-invisible-markers))
                                  "\n"
                                  chatgpt-shell--prompt-internal))
    (when (process-live-p chatgpt-shell--request-process)
      (kill-process chatgpt-shell--request-process))
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
                           ;; FIXME: Move to chatgpt-shell.
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

(defun chatgpt-shell--announce-response (buffer)
  "Announce response if BUFFER is not active."
  (unless (eq buffer (window-buffer (selected-window)))
    (message "%s responded" (buffer-name buffer))))

;; FIXME: Move to chatgpt-shell.
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
      (setq chatgpt-shell--request-process request-process)
      (chatgpt-shell--write-output-to-log-buffer "// Request\n\n")
      (chatgpt-shell--write-output-to-log-buffer (string-join command " "))
      (chatgpt-shell--write-output-to-log-buffer "\n\n")
      (when streaming
        (set-process-filter
         request-process
         (lambda (_process output)
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

(defun chatgpt-shell--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

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

;; FIXME: Use invisible markers to extract text.
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
  (unless (eq major-mode 'chatgpt-shell-mode)
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

(provide 'mk-shell)
