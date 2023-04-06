;;; dall-e-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

(defcustom chatgpt-shell-dall-e-image-size nil
  "The default size of the requested image as a string.

For example: \"1024x1024\""
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-dall-e-model-version "image-alpha-001"
  "The used DALL-E OpenAI model."
  :type 'string
  :group 'chatgpt-shell)

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
       (when chatgpt-shell-dall-e-image-size
         (push `(size . ,chatgpt-shell-dall-e-image-size) request-data))
       request-data))
   :response-extractor #'chatgpt-shell--extract-dall-e-response))

;;;###autoload
(defun dall-e-shell ()
  "Start a DALL-E shell."
  (interactive)
  (let ((old-point)
        (buf-name "*dalle*"))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create "*dalle*")
        (setq-local chatgpt-shell--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (chatgpt-shell-mode)
        (chatgpt-shell--initialize
         chatgpt-shell--dall-e-config)))
    (funcall chatgpt-shell-display-function buf-name)
    (when old-point
      (push-mark old-point))))

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

(defun chatgpt-shell-post-dall-e-prompt (prompt &optional version image-size)
  "Make a single DALL-E request with PROMPT.

Optionally provide model VERSION or IMAGE-SIZE."
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
               (when (or image-size chatgpt-shell-dall-e-image-size)
                 (push `(size . ,(or image-size chatgpt-shell-dall-e-image-size))
                       request-data))
               request-data)))
           (_status (condition-case err
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
