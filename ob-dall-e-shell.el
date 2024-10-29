;;; ob-dall-e-shell.el --- Org babel functions for DALL-E evaluation -*- lexical-binding: t; -*-

;; Copyright (C) Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.32.1
;; Package-Requires: ((emacs "27.1") (dall-e-shell "0.42.1"))

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

;; Run and render DALL-E blocks using org babel.
;;
;; Install with:
;;
;;   (require 'ob-dall-e-shell)
;;   (ob-dall-e-shell-setup)
;;
;; Usage:
;;
;;     #+begin_src dall-e-shell
;;       Hello
;;     #+end_src

;;; Requirements:

;;; Code:
(require 'ob)
(require 'dall-e-shell)
(require 'map)

(defvar org-babel-default-header-args:dall-e-shell '((:results . "file")
                                                     (:version . nil)))

(defun org-babel-execute:dall-e-shell(body params)
  "Execute a block of DALL-E prompt in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (message "executing DALL-E source code block")
  (dall-e-shell-post-prompt body
                            (map-elt params :version) nil
                            (seq-contains-p (map-elt params :result-params) "both")))

(defun ob-dall-e-shell-setup ()
  "Set up babel DALL-E support."
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((dall-e-shell . t))))
  (add-to-list 'org-src-lang-modes '("dall-e-shell" . text))

  ;; Automatically refresh inline images.
  (add-hook 'org-babel-after-execute-hook
            (defun ob-dall-e--refresh-inline-images ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images)))))

(provide 'ob-dall-e-shell)

;;; ob-dall-e-shell.el ends here
