;;; ob-chatgpt-shell.el --- Org babel functions for SwiftUI evaluation -*- lexical-binding: t; -*-

;; Copyright (C) Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell

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

;; Run and render SwiftUI blocks using org babel.
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
(require 'ob)
(require 'org)
(require 'chatgpt-shell)
(require 'map)

;; Aliasing enables block syntax highlighting.
(defalias 'chatgpt-shell-mode #'text-mode)

(defvar org-babel-default-header-args:chatgpt-shell '((:results . "raw")
                                                      (:version . nil)
                                                      (:preface . nil)))

(defun org-babel-execute:chatgpt-shell(body params)
  "Execute a block of ChatGPT prompt in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (message "executing ChatGPT source code block")
  (if (map-elt params :preface)
      (chatgpt-shell-post-chatgpt-messages
       (append preface `(((role . "user")
                          (content . ,body))))
       (map-elt params :version))
      (chatgpt-shell-post-chatgpt-prompt body (map-elt params :version))))

(defun ob-chatgpt-shell-setup ()
  "Set up babel ChatGPT support."
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((chatgpt-shell . t))))
  (add-to-list 'org-src-lang-modes '("chatgpt-shell" . text)))

(provide 'ob-chatgpt-shell)

;;; ob-chatgpt-shell.el ends here
