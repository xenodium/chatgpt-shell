;;; ob-chatgpt-shell.el --- Org babel functions for ChatGPT evaluation -*- lexical-binding: t; -*-

;; Copyright (C) Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.19.1
;; Package-Requires: ((emacs "27.1") (chatgpt-shell "0.39.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Run and get responses from ChatGPT blocks using org babel.
;;
;; Install with:
;;
;;   (require 'ob-chatgpt-shell)
;;   (ob-chatgpt-shell-setup)
;;
;; Usage:
;;
;;     #+begin_src chatgpt-shell
;;       Hello
;;     #+end_src

;;; Requirements:

;;; Code:
(require 'chatgpt-shell)
(require 'map)
(require 'ob)
(require 'org-element)

(defvar org-babel-default-header-args:chatgpt-shell '((:results . "raw")
                                                      (:version . nil)
                                                      (:system . nil)
                                                      (:context . nil)
                                                      (:temperature . nil)))

(defun org-babel-execute:chatgpt-shell(body params)
  "Execute a block of ChatGPT prompt in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (message "executing ChatGPT source code block")
  (chatgpt-shell-post-messages
   (vconcat ;; Vector for json
    (when (map-elt params :system)
      (list
       (list
        (cons 'role "system")
        (cons 'content (map-elt params :system)))))
    (when (map-elt params :context)
      (ob-chatgpt-shell--context))
    `(((role . "user")
       (content . ,body))))
   (map-elt params :version)
   nil nil
   (map-elt params :temperature)))

(defun ob-chatgpt-shell--context ()
  "Return the context (what was asked and responded) in all previous blocks."
  (let ((context '()))
    (mapc
     (lambda (src-block)
       (add-to-list
        'context
        (list
         (cons 'role "user")
         (cons 'content (map-elt src-block 'body))))
       (add-to-list
        'context
        (list
         (cons 'role "assistant")
         (cons 'content (map-elt src-block 'result)))))
     (ob-chatgpt--relevant-source-blocks-before-current))
    (nreverse context)))

(defun ob-chatgpt-shell-setup ()
  "Set up babel ChatGPT support."
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((chatgpt-shell . t))))
  (add-to-list 'org-src-lang-modes '("chatgpt-shell" . text)))

(defun ob-chatgpt--relevant-source-blocks-before-current ()
  "Return all previous source blocks from current one."
  (when-let ((current-block-pos (let ((element (org-element-context)))
                                  (when (eq (org-element-type element) 'src-block)
                                    (org-element-property :begin element)))))
    (seq-filter (lambda (src)
                  (and (string-equal (map-elt src 'language)
                                     "chatgpt-shell")
                       (< (map-elt src 'start) current-block-pos)))
                (ob-chatgpt--all-source-blocks))))

(defun ob-chatgpt--all-source-blocks ()
  "Return all source blocks in buffer."
  (org-element-map (org-element-parse-buffer) '(src-block fixed-width)
    (lambda (element)
      (cond ((eq (org-element-type element) 'src-block)
             (list 'start (org-element-property :begin element)
                   'body (when (org-element-property :value element)
                           (string-trim (org-element-property :value element)))
                   'language (when (org-element-property :language element)
                               (string-trim (org-element-property :language element)))
                   'result (save-restriction
                             (goto-char (org-element-property :begin element))
                             (when (org-babel-where-is-src-block-result)
                               (goto-char (org-babel-where-is-src-block-result))
                               (org-babel-read-result)))))))))

(provide 'ob-chatgpt-shell)

;;; ob-chatgpt-shell.el ends here
