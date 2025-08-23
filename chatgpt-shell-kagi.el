;;; chatgpt-shell-kagi.el --- Kagi-specific logic  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Package-Requires: ((emacs "29.1") (shell-maker "0.72.1"))

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

;; Adds Kagi specifics for `chatgpt-shell'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'let-alist)
(require 'shell-maker)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar chatgpt-shell-proxy)

(cl-defun chatgpt-shell-kagi-make-summarizer-model ()
  "Create an Kagi model.

Set VERSION, SHORT-VERSION, TOKEN-WIDTH, CONTEXT-WINDOW and
VALIDATE-COMMAND handler."
  `((:provider . "Kagi")
    (:label . "Kagi")
    (:version . "muriel-summarizer")
    (:icon . "https://upload.wikimedia.org/wikipedia/commons/1/1b/Kagi_Search_Engine_Icon.png")
    (:headers . chatgpt-shell-kagi--make-summarizer-headers)
    (:handler . chatgpt-shell-kagi--handle-summarizer-command)
    (:ignore-context . t)
    (:filter . chatgpt-shell-kagi--extract-summarizer-response)
    (:url . chatgpt-shell-kagi--make-summarizer-url)
    (:validate-command . chatgpt-shell-kagi--validate-command)))

(defcustom chatgpt-shell-kagi-key nil
  "Kagi API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-kagi-api-url-base "https://kagi.com/api/v0/summarize"
  "Kagi API's base URL.

API url = base + path.

If you use Kagi through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defun chatgpt-shell-kagi-models ()
  "Build a list of Kagi LLM models available."
  (list (chatgpt-shell-kagi-make-summarizer-model)))

(defun chatgpt-shell-kagi-key ()
  "Get the kagi API key."
  (cond ((stringp chatgpt-shell-kagi-key)
         chatgpt-shell-kagi-key)
        ((functionp chatgpt-shell-kagi-key)
         (condition-case _err
             (funcall chatgpt-shell-kagi-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-kagi--make-summarizer-headers (&key _model _settings)
  "Create the API headers."
  (unless (chatgpt-shell-kagi-key)
    (error "Your chatgpt-shell-kagi-key is missing"))
  (list (format "Authorization: Bot %s" (chatgpt-shell-kagi-key))))

(cl-defun chatgpt-shell-kagi--handle-summarizer-command (&key model command _context shell settings)
  "Handle Kagi shell COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-kagi--make-summarizer-url :model model
                                                 :settings settings
                                                 :command command)
   :proxy chatgpt-shell-proxy
   :headers (chatgpt-shell-kagi--make-summarizer-headers)
   :filter #'chatgpt-shell-kagi--extract-summarizer-response
   :shell shell))

(cl-defun chatgpt-shell-kagi--make-summarizer-url (&key _model _settings command)
  "Create Kagi summarizer API URL using COMMAND."
  (concat chatgpt-shell-kagi-api-url-base
          "?engine=muriel"
          "&url="
          (url-hexify-string (string-trim command))))

(defun chatgpt-shell-kagi--extract-summarizer-response (object)
  "Extract Kagi summarizer response from OBJECT.

Responses are never streamed."
  (when (stringp object)
    (error "Please upgrade shell-maker to 0.79.1 or newer"))
  ;; Non-streamed
  (if-let* ((whole (shell-maker--json-parse-string (map-elt object :pending)))
            (response (cond ((and (map-elt whole 'error)
                                  (seq-first (map-elt whole 'error)))
                             (map-elt (seq-first (map-elt whole 'error)) 'msg))
                            ((and (map-elt whole 'data)
                                  (not (eq (map-elt whole 'data) :null)))
                             (map-elt (map-elt whole 'data) 'object)))))
      (progn
        (setf (map-elt object :filtered) response)
        (setf (map-elt object :pending) nil)
        object)
    object))

(cl-defun chatgpt-shell-kagi--extract-url (&key text fail)
  "Trim TEXT URL found.

Optionally FAIL if none found."
  (let* ((trimmed (string-trim text))
         (urls (split-string trimmed "\\s-+" t))
         (valid-url (seq-filter (lambda (url)
                                  (or (string-match-p "^https?://" url)
                                      (string-match-p "^[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\(/.*\\)?$" url))) urls)))
    (if (and (= 1 (length valid-url)) (= 1 (length urls)))
        (let ((url (car valid-url)))
          (if (string-match-p "^[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\(/.*\\)?$" url)
              (concat "https://" url)
            url))
      (if fail
          (error "Input must contain exactly one URL")
        nil))))

(defun chatgpt-shell-kagi--validate-command (command model settings)
  "Return error string if command/setup isn't valid.

Needs COMMAND, MODEL and SETTINGS."
  (cond ((not chatgpt-shell-kagi-key)
         "Variable `chatgpt-shell-kagi-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-kagi-key

or

(setq chatgpt-shell-kagi-key \"my-key\")")
        ((not (chatgpt-shell-kagi--extract-url :text command))
         "Needs one and only one URL")
        ((map-elt settings :system-prompt)
         (format "Model \"%s\" does not support system prompts. Please unset via \"M-x chatgpt-shell-swap-system-prompt\" by selecting None."
                 (map-elt model :version)))))

(provide 'chatgpt-shell-kagi)

;;; chatgpt-shell-kagi.el ends here
