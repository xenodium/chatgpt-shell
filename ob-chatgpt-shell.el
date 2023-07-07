;;; ob-chatgpt-shell.el --- Org babel functions for ChatGPT evaluation -*- lexical-binding: t; -*-

;; Copyright (C) Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.30.1
;; Package-Requires: ((emacs "27.1") (chatgpt-shell "0.60.1"))

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
                                                      (:temperature . nil)
                                                      (:preflight . nil)))

(defun org-babel-execute:chatgpt-shell (body params)
  "Execute a block of ChatGPT prompt in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (message "executing ChatGPT source code block")
  (let* ((context
          (when-let ((context-name (map-elt params :context)))
            (if (string-equal context-name "t")
                ;; If the context is `t' then collect all previous contexts
                (ob-chatgpt-shell--context)
              ;; Otherwise only collect contexts with matching context-name
              (ob-chatgpt-shell--context context-name))))
         (messages
          (vconcat ;; Convert to vector for json
           (append
            (when (and (map-elt params :system)
                       (not (map-elt params :context)))
              `(((role . "system")
                 (content . ,(map-elt params :system)))))
            context
            `(((role . "user")
               (content . ,body)))))))
    (if (map-elt params :preflight)
        (pp (chatgpt-shell-make-request-data
             messages
             (map-elt params :version)
             (map-elt params :temperature)))
      (chatgpt-shell-post-messages
       messages
       (map-elt params :version)
       nil nil
       (map-elt params :temperature)))))

(defun ob-chatgpt-shell--context (&optional context-name)
  "Return the context (what was asked and responded) for matching
previous src blocks. If CONTEXT-NAME is provided each src block
have a :context arg with a value matching the CONTEXT-NAME."
  (let ((context '()))
    (mapc
     (lambda (src-block)
       (when-let ((system (and (seq-empty-p context) ;; Add system only if first item.
                               (or (map-elt (map-elt src-block 'parameters '()) :system)
                                   (map-elt org-babel-default-header-args:chatgpt-shell :system)))))
         (push
          (list
           (cons 'role "system")
           (cons 'content system))
          context))
       (push
        (list
         (cons 'role "user")
         (cons 'content (map-elt src-block 'body)))
        context)
       (push
        (list
         (cons 'role "assistant")
         (cons 'content (map-elt src-block 'result)))
        context))
     (ob-chatgpt--relevant-source-blocks-before-current context-name))
    (nreverse context)))

(defun ob-chatgpt-shell-setup ()
  "Set up babel ChatGPT support."
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((chatgpt-shell . t))))
  (add-to-list 'org-src-lang-modes '("chatgpt-shell" . text)))

(defun ob-chatgpt--relevant-source-blocks-before-current (context-name)
  "Return all previous source blocks relative to the current block with a
:context arg with a value matching CONTEXT-NAME. If CONTEXT-NAME
is nil then return all previous source blocks."
  (when-let ((current-block-pos (let ((element (org-element-context)))
                                  (when (eq (org-element-type element) 'src-block)
                                    (org-element-property :begin element)))))
    (seq-filter (lambda (src)
                  (and (string-equal (map-elt src 'language)
                                     "chatgpt-shell")
                       (or (not context-name)
                           (string-equal (map-elt (map-elt src 'parameters '()) :context)
                                         context-name))
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
                   'parameters (when (org-element-property :parameters element)
                                 (org-babel-parse-header-arguments
                                  (string-trim (org-element-property :parameters element))))
                   'result (save-restriction
                             (goto-char (org-element-property :begin element))
                             (when (org-babel-where-is-src-block-result)
                               (goto-char (org-babel-where-is-src-block-result))
                               (org-babel-read-result)))))))))

(provide 'ob-chatgpt-shell)

;;; ob-chatgpt-shell.el ends here
