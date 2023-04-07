;;; dall-e-shell.el --- Interaction mode for DALL-E  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
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

;; You must set `dall-e-shell-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.

(require 'mk-shell)

;;; Code:

(defcustom dall-e-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'dall-e-shell)

(defcustom dall-e-shell-image-size nil
  "The default size of the requested image as a string.

For example: \"1024x1024\""
  :type 'string
  :group 'dall-e-shell)

(defcustom dall-e-shell-model-version "image-alpha-001"
  "The used DALL-E OpenAI model."
  :type 'string
  :group 'dall-e-shell)

(defcustom dall-e-shell-request-timeout 60
  "How long to wait for a request to time out."
  :type 'integer
  :group 'dall-e-shell)

(defvaralias 'dall-e-shell-display-function 'mk-shell-display-function)

(defvaralias 'dall-e-shell-read-string-function 'mk-shell-read-string-function)

;; Aliasing enables editing as text in babel.
(defalias 'dall-e-shell-mode #'text-mode)

(defvar dall-e-shell--config
  (make-mk-shell-config
   :buffer-name "*dalle*"
   :process-name "dalle"
   :prompt "DALL-E> "
   :url "https://api.openai.com/v1/images/generations"
   :invalid-input
   (lambda (_input)
     (unless dall-e-shell-openai-key
       "Variable `dall-e-shell-openai-key' needs to be set to your key.

Try M-x set-variable dall-e-shell-openai-key

or

(setq dall-e-shell-openai-key \"my-key\")"))
   :request-maker
   (lambda (url request-data response-extractor callback error-callback)
     (mk-shell--async-shell-command
      (dall-e-shell--make-curl-request-command-list
       dall-e-shell-openai-key
       url request-data)
      nil ;; no streaming
      response-extractor
      callback
      error-callback))
   :request-data-maker #'dall-e-shell--make-data
   :response-extractor #'dall-e-shell--extract-response))

;;;###autoload
(defun dall-e-shell ()
  "Start a DALL-E shell."
  (interactive)
  (mk-start-shell dall-e-shell--config))

(defun dall-e-shell--make-data (commands-and-responses)
  "Create the request payload from COMMANDS-AND-RESPONSES."
  (let ((request-data `((model . ,dall-e-shell-model-version)
                        (prompt . ,(car (car (last commands-and-responses)))))))
    (when dall-e-shell-image-size
      (push `(size . ,dall-e-shell-image-size) request-data))
    request-data))

(defun dall-e-shell--extract-response (json &optional no-download)
  "Extract DALL-E response from JSON.
Set NO-DOWNLOAD to skip automatic downloading."
  (if-let ((parsed (mk-shell--json-parse-string-filtering
                    json "^curl:.*\n?"))
           (buffer (mk-shell-buffer mk-shell-config)))
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
              (propertize path 'display "[downloading...]")))
        (let-alist parsed
          .error.message))))

(defun dall-e-shell-post-prompt (prompt &optional version image-size)
  "Make a single DALL-E request with PROMPT.

Optionally provide model VERSION or IMAGE-SIZE."
  (with-temp-buffer
    (setq-local mk-shell-config
                dall-e-shell--config)
    (let* ((api-buffer (current-buffer))
           (command
            (dall-e-shell--make-curl-request-command-list
             dall-e-shell-openai-key
             (mk-shell-config-url mk-shell-config)
             (let ((request-data `((model . ,(or version
                                                 dall-e-shell-model-version))
                                   (prompt . ,prompt))))
               (when (or image-size dall-e-shell-image-size)
                 (push `(size . ,(or image-size dall-e-shell-image-size))
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
                  (map-elt response 'path)
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

(defun dall-e-shell--make-curl-request-command-list (key url request-data)
  "Build DALL-E curl command list using KEY URL and REQUEST-DATA."
  (list "curl" url
        "--fail-with-body"
        "--no-progress-meter"
        "-m" (number-to-string dall-e-shell-request-timeout)
        "-H" "Content-Type: application/json"
        "-H" (format "Authorization: Bearer %s"
                     (cond ((stringp key)
                            key)
                           ((functionp key)
                            (condition-case _err
                                (funcall key)
                              (error
                               "KEY-NOT-FOUND")))))
        "-d" (mk-shell--json-encode request-data)))

(provide 'dall-e-shell)

;;; dall-e-shell.el ends here
