;;; mk-shell.el --- Interaction mode for making comint shells  -*- lexical-binding: t -*-

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

(defcustom mk-shell-display-function #'pop-to-buffer-same-window
  "Function to display new shell.  Can be set to `display-buffer' or similar."
  :type 'function
  :group 'mk-shell)

(defcustom mk-shell-read-string-function (lambda (prompt history)
                                                (read-string prompt nil history))
  "Function to read strings from user.

To use `completing-read', it can be done with something like:

\(setq `mk-shell-read-string-function'
      (lambda (prompt history)
        (completing-read prompt (symbol-value history) nil nil nil history)))"
  :type 'function
  :group 'mk-shell)

(defvar mk-shell--input nil)

(defvar mk-shell--current-request-id 0)

(defvar mk-shell--show-invisible-markers nil)

(defvar mk-shell--prompt-internal nil)

(cl-defstruct
    mk-shell-config
  prompt
  buffer-name
  process-name
  url
  invalid-input
  request-maker
  request-data-maker
  response-extractor
  response-post-processor)

(defvar-local mk-shell--busy nil)

(defvar-local mk-shell-config nil)

(defvaralias 'mk-shell-mode-map 'mk-shell-map)

(defvar-local mk-shell--file nil)

(defvar-local mk-shell--request-process nil)

(defvar mk-shell-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'mk-shell-return)
    (define-key map "\C-c\C-c" 'mk-shell-interrupt)
    (define-key map "\C-x\C-s" 'mk-shell-save-session-transcript)
    (define-key map "\C-\M-h" 'mk-shell-mark-output)
    map)
  "Keymap for mk-shell-mode.")

(define-derived-mode mk-shell-mode comint-mode "mk-shell"
  "Major mode for interactively evaluating mk-shell prompts.
Uses the interface provided by `comint-mode'"
  nil)

(defun mk-shell-buffer (config)
  "Get buffer from CONFIG."
  (get-buffer-create (mk-shell-config-buffer-name config)))

(defun mk-start-shell (config)
  (let ((old-point)
        (buf-name (mk-shell-config-buffer-name config)))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create buf-name)
        (setq-local mk-shell--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (mk-shell-mode)
        (mk-shell--initialize config)))
    (funcall mk-shell-display-function buf-name)
    (when old-point
      (push-mark old-point))))

(defun mk-shell--initialize (config)
  "Initialize shell using CONFIG."
  (setq-local mk-shell-config config)
  (visual-line-mode +1)
  (goto-address-mode +1)
  (setq comint-prompt-regexp
        (concat "^" (regexp-quote
                     (mk-shell-config-prompt mk-shell-config))))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'mk-shell--input-sender)
  (setq comint-process-echoes nil)
  (setq-local mk-shell--prompt-internal
              (mk-shell-config-prompt mk-shell-config))
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'mk-shell--get-old-input)
  (setq-local comint-completion-addsuffix nil)
  (setq-local imenu-generic-expression
              `(("Prompt" ,(concat "^" (regexp-quote
                                        (mk-shell-config-prompt mk-shell-config))
                                   "\\(.*\\)") 1)))
  (unless (or (comint-check-proc (mk-shell-buffer mk-shell-config))
              (get-buffer-process (mk-shell-buffer mk-shell-config)))
    (condition-case nil
        (start-process (mk-shell-config-process-name mk-shell-config)
                       (mk-shell-buffer mk-shell-config) "hexl")
      (file-error (start-process
                   (mk-shell-config-process-name mk-shell-config)
                   (mk-shell-buffer mk-shell-config) "cat")))
    (set-process-query-on-exit-flag (mk-shell--process) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (mk-shell--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (mk-shell--process) mk-shell--prompt-internal)
    (set-marker comint-last-input-start (mk-shell--pm))
    (set-process-filter (get-buffer-process
                         (mk-shell-buffer mk-shell-config))
                        'comint-output-filter)))

(defun mk-shell--write-reply (reply &optional failed)
  "Write REPLY to prompt.  Set FAILED to record failure."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (comint-output-filter (mk-shell--process)
                          (concat reply
                                  (if failed
                                      (propertize "<gpt-ignored-response>"
                                                  'invisible (not mk-shell--show-invisible-markers))
                                    "")
                                  mk-shell--prompt-internal))))

(defun mk-shell-return ()
  "RET binding."
  (interactive)
  (unless (eq major-mode 'mk-shell-mode)
    (user-error "Not in a shell"))
  (mk-shell--send-input))

(defun mk-shell-last-output ()
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

(defun mk-shell-mark-output ()
  "If at latest prompt, mark last output.
Otherwise mark current output at location."
  (interactive)
  (unless (eq major-mode 'mk-shell-mode)
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
        (mk-shell-narrow-to-prompt)
        (unless
            (cond
             ((re-search-backward "<gpt-end-of-prompt>" nil t)
              (forward-char (length "<gpt-end-of-prompt>"))
              t)
             ((re-search-backward
               (concat "^"
                       (mk-shell-config-prompt mk-shell-config))nil t)
              (if (re-search-forward "<gpt-end-of-prompt>" nil t)
                  t
                (end-of-line))
              t)
             (t
              nil))
          (setq revert-pos t))
        (setq start (point))))
    (save-excursion
      (save-restriction
        (shell-narrow-to-prompt)
        (setq end (point-max))))
    (when revert-pos
      (goto-char current-pos)
      (user-error "Not available"))
    (set-mark (1- end))
    (goto-char (1+ start))))

(defun mk-shell-save-output ()
  "If at latest prompt, save last output.
Otherwise save current output at location."
  (interactive)
  (unless (eq major-mode 'mk-shell-mode)
    (user-error "Not in a shell"))
  (let ((orig-point (point))
        (orig-region-active (region-active-p))
        (orig-region-start (region-beginning))
        (orig-region-end (region-end)))
    (unwind-protect
        (progn
          (mk-shell-mark-output)
          (write-region (region-beginning)
                        (region-end)
                        (read-file-name "Write file: ")))
      (if orig-region-active
          (progn
            (set-mark orig-region-start)
            (goto-char orig-region-end))
        (setq mark-active nil)
        (goto-char orig-point)))))

(defun mk-shell-interrupt ()
  "Interrupt current request."
  (interactive)
  (unless (eq major-mode 'mk-shell-mode)
    (user-error "Not in a shell"))
  (with-current-buffer (mk-shell-buffer mk-shell-config)
    ;; Increment id, so in-flight request is ignored.
    (mk-shell--increment-request-id)
    (comint-send-input)
    (goto-char (point-max))
    (comint-output-filter (mk-shell--process)
                          (concat (propertize "<gpt-ignored-response>"
                                              'invisible (not mk-shell--show-invisible-markers))
                                  "\n"
                                  mk-shell--prompt-internal))
    (when (process-live-p mk-shell--request-process)
      (kill-process mk-shell--request-process))
    (setq mk-shell--busy nil)
    (message "interrupted!")))

(defun mk-shell--eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (let ((buffer (mk-shell-buffer mk-shell-config))
        (prefix-newline "")
        (suffix-newline "\n\n")
        (response-count 0))
   (unless mk-shell--busy
    (setq mk-shell--busy t)
    (cond
     ((string-equal "clear" (string-trim input-string))
      (call-interactively 'comint-clear-buffer)
      (comint-output-filter (mk-shell--process) mk-shell--prompt-internal)
      (setq mk-shell--busy nil))
     ((not (mk-shell--curl-version-supported))
      (mk-shell--write-reply "\nYou need curl version 7.76 or newer.\n\n")
      (setq mk-shell--busy nil))
     ((and (mk-shell-config-invalid-input
            mk-shell-config)
           (funcall (mk-shell-config-invalid-input
                     mk-shell-config) input-string))
      (mk-shell--write-reply
       (concat "\n"
               (funcall (mk-shell-config-invalid-input
                         mk-shell-config) input-string)
               "\n\n"))
      (setq mk-shell--busy nil))
     ((string-empty-p (string-trim input-string))
      (comint-output-filter (mk-shell--process)
                            (concat "\n" mk-shell--prompt-internal))
      (setq mk-shell--busy nil))
     (t
      ;; For viewing prompt delimiter (used to handle multiline prompts).
      ;; (comint-output-filter (mk-shell--process) "<gpt-end-of-prompt>")
      (comint-output-filter (mk-shell--process)
                            (propertize "<gpt-end-of-prompt>"
                                        'invisible (not mk-shell--show-invisible-markers)))
      (funcall (mk-shell-config-request-maker mk-shell-config)
                 (mk-shell-config-url mk-shell-config)
                 (funcall (mk-shell-config-request-data-maker mk-shell-config)
                          (vconcat
                           ;; FIXME: Move to chatgpt-shell.
                           (last (chatgpt-shell--extract-commands-and-responses
                                  (with-current-buffer buffer
                                    (buffer-string))
                                  (mk-shell-config-prompt mk-shell-config))
                                 ;; FIXME: Move to chatgpt-shell.
                                 (chatgpt-shell--unpaired-length
                                  ;; FIXME: Move to chatgpt-shell.
                                  chatgpt-shell-transmitted-context-length))))
                 (mk-shell-config-response-extractor mk-shell-config)
                 (lambda (response partial)
                   (setq response-count (1+ response-count))
                   (setq prefix-newline (if (> response-count 1)
                                            ""
                                          "\n"))
                   (if response
                       (if partial
                           (progn
                             (mk-shell--write-partial-reply (concat prefix-newline response))
                             (setq mk-shell--busy partial))
                         (progn
                           (mk-shell--write-reply (concat prefix-newline response suffix-newline))
                           (mk-shell--announce-response buffer)
                           (setq mk-shell--busy nil)
                           (when (mk-shell-config-response-post-processor mk-shell-config)
                             ;; FIXME use (concat prefix-newline response suffix-newline) if not streaming.
                             (funcall (mk-shell-config-response-post-processor mk-shell-config)
                                      (mk-shell-last-output)))))
                     (mk-shell--write-reply "Error: that's all is known" t) ;; comeback
                     (setq mk-shell--busy nil)
                     (mk-shell--announce-response buffer)))
                 (lambda (error)
                   (mk-shell--write-reply (concat (string-trim error) suffix-newline) t)
                   (setq mk-shell--busy nil)
                   (mk-shell--announce-response buffer))))))))

(defun mk-shell--announce-response (buffer)
  "Announce response if BUFFER is not active."
  (unless (eq buffer (window-buffer (selected-window)))
    (message "%s responded" (buffer-name buffer))))

(defun mk-shell--async-shell-command (command streaming response-extractor callback error-callback)
  "Run shell COMMAND asynchronously.
Set STREAMING to enable it.  Calls RESPONSE-EXTRACTOR to extract the
response and feeds it to CALLBACK or ERROR-CALLBACK accordingly."
  (let* ((buffer (mk-shell-buffer mk-shell-config))
         (request-id (mk-shell--increment-request-id))
         (output-buffer (generate-new-buffer " *temp*"))
         (request-process (condition-case err
                              (apply #'start-process (append (list
                                                              (mk-shell-config-buffer-name mk-shell-config)
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
      (setq mk-shell--request-process request-process)
      (mk-shell--write-output-to-log-buffer "// Request\n\n")
      (mk-shell--write-output-to-log-buffer (string-join command " "))
      (mk-shell--write-output-to-log-buffer "\n\n")
      (when streaming
        (set-process-filter
         request-process
         (lambda (_process output)
           (when (eq request-id mk-shell--current-request-id)
             (mk-shell--write-output-to-log-buffer
              (format "// Filter output\n\n%s\n\n" output))
             (setq remaining-text (concat remaining-text output))
             (setq preparsed (mk-shell--preparse-json remaining-text))
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
         (let ((active (eq request-id mk-shell--current-request-id))
               (output (with-current-buffer (process-buffer process)
                         (buffer-string)))
               (exit-status (process-exit-status process)))
           (mk-shell--write-output-to-log-buffer
            (format "// Response (%s)\n\n" (if active "active" "inactive")))
           (mk-shell--write-output-to-log-buffer
            (format "Exit status: %d\n\n" exit-status))
           (mk-shell--write-output-to-log-buffer output)
           (mk-shell--write-output-to-log-buffer "\n\n")
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

(defun mk-shell--json-parse-string-filtering (json regexp)
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

(defun mk-shell--increment-request-id ()
  "Increment `mk-shell--current-request-id'."
  (if (= mk-shell--current-request-id most-positive-fixnum)
      (setq mk-shell--current-request-id 0)
    (setq mk-shell--current-request-id (1+ mk-shell--current-request-id))))

(defun mk-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark
               (get-buffer-process
                (mk-shell-buffer mk-shell-config))) pos))

(defun mk-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process
                 (mk-shell-buffer mk-shell-config))))

(defun mk-shell--input-sender (_proc input)
  "Set the variable `mk-shell--input' to INPUT.
Used by `mk-shell--send-input's call."
  (setq mk-shell--input input))

(defun mk-shell--send-input ()
  "Send text after the prompt."
  (let (mk-shell--input)
    (comint-send-input)
    (mk-shell--eval-input mk-shell--input)))

(defun mk-shell--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun mk-shell--json-encode (obj)
  "Serialize OBJ to json.  Use fallback if `json-serialize' isn't available."
  (if (fboundp 'json-serialize)
      (json-serialize obj)
    (json-encode obj)))

(defun mk-shell--curl-version-supported ()
  "Return t if curl version is 7.76 or newer, nil otherwise."
  (let* ((curl-error-redirect (if (eq system-type (or 'windows-nt 'ms-dos)) "2> NUL" "2>/dev/null"))
         (curl-version-string (shell-command-to-string (concat "curl --version " curl-error-redirect))))
    (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" curl-version-string)
      (let ((version (match-string 1 curl-version-string)))
        (version<= "7.76" version)))))

(defun mk-shell--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (if (fboundp 'json-parse-string)
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (json-parse-error nil))
    (condition-case _err
        (json-read-from-string json)
      (error nil))))

(defun mk-shell--write-partial-reply (reply)
  "Write partial REPLY to prompt."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (delete-overlay overlay))
    (comint-output-filter (mk-shell--process) reply)))

(defun mk-shell--preparse-json (json)
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

;; FIXME: Use invisible markers to extract text.
(defun chatgpt-shell--command-and-response-at-point ()
  "Extract the current command and response in buffer."
  (save-excursion
    (save-restriction
      (mk-shell-narrow-to-prompt)
      (let ((items (chatgpt-shell--extract-commands-and-responses
                    (buffer-string)
                    (mk-shell-config-prompt mk-shell-config))))
        (cl-assert (or (seq-empty-p items)
                       (eq (length items) 1)
                       (eq (length items) 2)))
        items))))

(defun mk-shell--write-output-to-log-buffer (output)
  "Write OUTPUT to log buffer."
  ;; FIXME: Make redacting generic.
  (when (chatgpt-shell-openai-key)
    (setq output (string-replace (chatgpt-shell-openai-key) "SK-REDACTED-OPENAI-KEY"
                                 output)))
  (with-current-buffer (get-buffer-create (format "*%s-log*"
                                           (mk-shell-config-process-name mk-shell-config)))
      (let ((beginning-of-input (goto-char (point-max))))
        (insert output)
        (when (and (require 'json nil t)
                   (ignore-errors (mk-shell--json-parse-string output)))
          (json-pretty-print beginning-of-input (point))))))

(defun mk-shell--process nil
  "Get shell buffer process."
  (get-buffer-process (mk-shell-buffer mk-shell-config)))

(defun mk-shell-save-session-transcript ()
  "Save shell transcript to file.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'mk-shell-mode)
    (user-error "Not in a shell"))
  (if mk-shell--file
      (let ((content (buffer-string))
            (path mk-shell--file))
        (with-temp-buffer
          (insert content)
          (write-file path nil)))
    (when-let ((path (read-file-name "Write file: "
                                     nil nil nil "transcript.txt"))
               (content (buffer-string)))
      (with-temp-buffer
        (insert content)
        (write-file path t))
      (setq mk-shell--file path))))

(provide 'mk-shell)
