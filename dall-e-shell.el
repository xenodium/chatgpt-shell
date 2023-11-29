;;; dall-e-shell.el --- Interaction mode for DALL-E  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.37.1
;; Package-Requires: ((emacs "27.1") (shell-maker "0.44.1"))

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

;; `dall-e-shell' is a comint-based DALL-E shell for Emacs.
;;
;; You must set `dall-e-shell-openai-key' to your key before using.
;;
;; Run `dall-e-shell' to get a DALL-E shell.
;;
;; Note: This is young package still.  Please report issues or send
;; patches to https://github.com/xenodium/chatgpt-shell
;;
;; Support the work https://github.com/sponsors/xenodium

(require 'shell-maker)

;;; Code:

(defcustom dall-e-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'dall-e-shell)

(defcustom dall-e-shell-additional-curl-options nil
  "Additional options for `curl' command."
  :type '(repeat (string :tag "String"))
  :group 'dall-e-shell)

(defcustom dall-e-shell-image-size nil
  "The default size of the requested image as a string.

For example: \"1024x1024\""
  :type 'string
  :group 'dall-e-shell)

(defcustom dall-e-shell-model-version nil
  "The used DALL-E OpenAI model.  For Dall-E 3, use \"dall-e-3\"."
  :type 'string
  :group 'dall-e-shell)

(defcustom dall-e-shell-request-timeout 60
  "How long to wait for a request to time out."
  :type 'integer
  :group 'dall-e-shell)

(defcustom dall-e-shell-image-output-directory temporary-file-directory
  "Output directory for the generated image."
  :type 'directory
  :group 'dall-e-shell)

(defcustom dall-e-shell-welcome-function #'shell-maker-welcome-message
  "Function returning welcome message or nil for no message.

See `shell-maker-welcome-message' as an example."
  :type 'function
  :group 'dall-e-shell)

(defvaralias 'dall-e-shell-display-function 'shell-maker-display-function)

(defvaralias 'dall-e-shell-read-string-function 'shell-maker-read-string-function)

;; Aliasing enables editing as text in babel.
(defalias 'dall-e-shell-mode #'text-mode)

(defvar dall-e-shell--url "https://api.openai.com/v1/images/generations")

(defvar dall-e-shell--config
  (make-shell-maker-config
   :name "DALL-E"
   :validate-command
   (lambda (_command)
     (unless dall-e-shell-openai-key
       "Variable `dall-e-shell-openai-key' needs to be set to your key.

Try M-x set-variable dall-e-shell-openai-key

or

(setq dall-e-shell-openai-key \"my-key\")"))
   :execute-command
   (lambda (_command history callback error-callback)
     (shell-maker-async-shell-command
      (dall-e-shell--make-curl-request-command-list
       (dall-e-shell--make-payload history))
      nil ;; no streaming
      #'dall-e-shell--extract-response
      callback
      error-callback))))

(shell-maker-define-major-mode dall-e-shell--config)

;;;###autoload
(defun dall-e-shell ()
  "Start a DALL-E shell."
  (interactive)
  (shell-maker-start dall-e-shell--config nil dall-e-shell-welcome-function))

(defun dall-e-shell--make-payload (history)
  "Create the request payload from HISTORY."
  (let ((request-data `((prompt . ,(car (car (last history)))))))
    (when dall-e-shell-image-size
      (push `(size . ,dall-e-shell-image-size) request-data))
    (when dall-e-shell-model-version
      (push `(model . ,dall-e-shell-model-version)
            request-data))
    request-data))

(defun dall-e-shell--extract-response (json &optional no-download)
  "Extract DALL-E response from JSON.
Set NO-DOWNLOAD to skip automatic downloading."
  (if-let ((parsed (shell-maker--json-parse-string-filtering
                    json "^curl:.*\n?"))
           (buffer (shell-maker-buffer shell-maker--config)))
      (if-let* ((url (let-alist parsed
                       (let-alist (seq-first .data)
                         .url)))
                (created (number-to-string (let-alist parsed
                                             .created)))
                (path (expand-file-name (concat created ".png")
                                        dall-e-shell-image-output-directory))
                (revised-prompt (or (let-alist parsed
                                      (let-alist (seq-first .data)
                                        .revised_prompt))
                                    "")))
          (if no-download
              `((url . ,url)
                (created . ,created)
                (path . ,path)
                (revised_prompt . ,revised-prompt))
            (progn
              (dall-e-shell--download-image
               url path
               (lambda (path)
                 (let* ((loc (dall-e-shell--find-string-in-buffer
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
                 (when-let* ((loc (dall-e-shell--find-string-in-buffer
                                   buffer
                                   path))
                             (start (car loc))
                             (end (cdr loc)))
                   (with-current-buffer buffer
                     (remove-text-properties start end '(face nil))
                     (add-text-properties start end `(display ,error))))))
              (if (string-empty-p revised-prompt)
                  (propertize path 'display "[downloading...]")
                (concat (propertize path 'display "[downloading...]")
                        (format "\n\n%s" revised-prompt)))))
        (let-alist parsed
          .error.message))))

(defun dall-e-shell-post-prompt (prompt &optional version image-size show-revised-prompt)
  "Make a single DALL-E request with PROMPT.

Optionally provide model VERSION or IMAGE-SIZE.

Set SHOW-REVISED-PROMPT to include in returned value."
  (with-temp-buffer
    (setq-local shell-maker--config
                dall-e-shell--config)
    (let* ((api-buffer (current-buffer))
           (command
            (dall-e-shell--make-curl-request-command-list
             (let ((request-data `((prompt . ,prompt))))
               (when (or image-size dall-e-shell-image-size)
                 (push `(size . ,(or image-size dall-e-shell-image-size))
                       request-data))
               (when (or version dall-e-shell-model-version)
                 (push `(model . ,(or version dall-e-shell-model-version))
                       request-data))
               request-data)))
           (_status (condition-case err
                        (apply #'call-process (seq-first command)
                               nil api-buffer nil (cdr command))
                      (error
                       (insert (error-message-string err))
                       1)))
           (response (dall-e-shell--extract-response
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
                  (if (and show-revised-prompt
                           (map-elt response 'revised_prompt))
                      (concat (map-elt response 'path)
                              (format "\n\n%s" (map-elt response 'revised_prompt)))
                    (map-elt response 'path))
                output)))
        (or response (with-current-buffer api-buffer
                       (buffer-string)))))))

(defun dall-e-shell--find-string-in-buffer (buffer search-str)
  "Find SEARCH-STR in BUFFER and return a cons with start/end.
Return nil if not found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward search-str nil t)
        (cons (match-beginning 0) (match-end 0))))))

(defun dall-e-shell--download-image (url path callback error-callback)
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

(defun dall-e-shell--make-curl-request-command-list (request-data)
  "Build DALL-E curl command list using REQUEST-DATA."
  (append (list "curl" dall-e-shell--url)
          dall-e-shell-additional-curl-options
          (list "--fail-with-body"
                "--no-progress-meter"
                "-m" (number-to-string dall-e-shell-request-timeout)
                "-H" "Content-Type: application/json; charset=utf-8"
                "-H" (format "Authorization: Bearer %s"
                             (cond ((stringp dall-e-shell-openai-key)
                                    dall-e-shell-openai-key)
                                   ((functionp dall-e-shell-openai-key)
                                    (condition-case _err
                                        (funcall dall-e-shell-openai-key)
                                      (error
                                       "KEY-NOT-FOUND")))))
                "-d" (shell-maker--json-encode request-data))))

(provide 'dall-e-shell)

;;; dall-e-shell.el ends here
