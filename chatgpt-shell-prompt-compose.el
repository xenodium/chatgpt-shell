;;; chatgpt-shell-prompt-compose.el --- A shell prompt compose buffer  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell

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

;; Prompt compose buffers enable crafting more involved queries and
;; simplify both response navigation and follow-up queries.
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'ring)
(require 'flymake)
(require 'shell-maker)

(declare-function chatgpt-shell-previous-source-block "chatgpt-shell")
(declare-function chatgpt-shell-next-source-block "chatgpt-shell")
(declare-function chatgpt-shell-swap-model "chatgpt-shell")
(declare-function chatgpt-shell-swap-system-prompt "chatgpt-shell")
(declare-function chatgpt-shell--minibuffer-prompt "chatgpt-shell")
(declare-function chatgpt-shell--put-source-block-overlays "chatgpt-shell")
(declare-function chatgpt-shell-send-to-buffer "chatgpt-shell")
(declare-function chatgpt-shell-execute-block-action-at-point "chatgpt-shell")
(declare-function chatgpt-shell-block-action-at-point "chatgpt-shell")
(declare-function chatgpt-shell-clear-buffer "chatgpt-shell")
(declare-function chatgpt-shell--primary-buffer "chatgpt-shell")
(declare-function chatgpt-shell--eshell-last-last-command "chatgpt-shell")
(declare-function chatgpt-shell-mark-block "chatgpt-shell")
(declare-function chatgpt-shell--region "chatgpt-shell")

(defvar-local chatgpt-shell-prompt-compose--exit-on-submit nil
  "Whether or not compose buffer should close after submission.

This is typically used to craft prompts and immediately jump over to
the shell to follow the response.")

(defvar-local chatgpt-shell-prompt-compose--last-known-region nil
  "Last known region details.

Of the form

\((:buffer . buffer)
 (:start . start)
 (:end . end)
 (:text . text))")

(defvar-local chatgpt-shell-prompt-compose--transient-frame-p nil
  "Identifies whether or not buffer is running on a dedicated frame.

t if invoked from a transient frame (quitting closes the frame).")

(defvar chatgpt-shell-prompt-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chatgpt-shell-prompt-compose-send-buffer)
    (define-key map (kbd "C-c C-k") #'chatgpt-shell-prompt-compose-cancel)
    (define-key map (kbd "C-c C-s") #'chatgpt-shell-prompt-compose-swap-system-prompt)
    (define-key map (kbd "C-c C-v") #'chatgpt-shell-prompt-compose-swap-model-version)
    (define-key map (kbd "C-c C-o") #'chatgpt-shell-prompt-compose-other-buffer)
    (define-key map (kbd "M-r") #'chatgpt-shell-prompt-compose-search-history)
    (define-key map (kbd "M-p") #'chatgpt-shell-prompt-compose-previous-history)
    (define-key map (kbd "M-n") #'chatgpt-shell-prompt-compose-next-history)
    map))

(define-derived-mode chatgpt-shell-prompt-compose-mode fundamental-mode "ChatGPT Compose"
  "Major mode for composing ChatGPT prompts from a dedicated buffer."
  :keymap chatgpt-shell-prompt-compose-mode-map)

(defvar chatgpt-shell-prompt-compose-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'chatgpt-shell-prompt-compose-retry)
    (define-key map (kbd "C-M-h") #'chatgpt-shell-mark-block)
    (define-key map (kbd "n") #'chatgpt-shell-prompt-compose-next-block)
    (define-key map (kbd "p") #'chatgpt-shell-prompt-compose-previous-block)
    (define-key map (kbd "<tab>") #'chatgpt-shell-prompt-compose-next-block)
    (define-key map (kbd "<backtab>") #'chatgpt-shell-prompt-compose-previous-block)
    (define-key map (kbd "r") #'chatgpt-shell-prompt-compose-reply)
    (define-key map (kbd "q") #'chatgpt-shell-prompt-compose-quit-and-close-frame)
    (define-key map (kbd "e") #'chatgpt-shell-prompt-compose-request-entire-snippet)
    (define-key map (kbd "i") #'chatgpt-shell-prompt-compose-insert-block-at-point)
    (define-key map (kbd "m") #'chatgpt-shell-prompt-compose-request-more)
    (define-key map (kbd "o") #'chatgpt-shell-prompt-compose-other-buffer)
    (set-keymap-parent map view-mode-map)
    map)
  "Keymap for `chatgpt-shell-prompt-compose-view-mode'.")

(define-minor-mode chatgpt-shell-prompt-compose-view-mode
  "Like `view-mode`, but extended for ChatGPT Compose."
  :lighter " ChatGPT view"
  :keymap chatgpt-shell-prompt-compose-view-mode-map
  (setq buffer-read-only chatgpt-shell-prompt-compose-view-mode))

;;;###autoload
(defun chatgpt-shell-prompt-compose (prefix)
  "Compose and send prompt from a dedicated buffer.

With PREFIX, clear existing history (wipe asociated shell history).

Whenever `chatgpt-shell-prompt-compose' is invoked, appends any active
region (or flymake issue at point) to compose buffer.

Additionally, if point is at an error/warning raised by flymake,
automatically add context (error/warning + code) to expedite ChatGPT
for help to fix the issue.

The compose buffer always shows the latest interaction, but it's
backed by the shell history.  You can always switch to the shell buffer
to view the history.

Editing: While compose buffer is in in edit mode, it offers a couple
of magit-like commit buffer bindings.

 `\\[chatgpt-shell-prompt-compose-send-buffer]` to send the buffer query.
 `\\[chatgpt-shell-prompt-compose-cancel]` to cancel compose buffer.
 `\\[chatgpt-shell-prompt-compose-search-history]` search through history.
 `\\[chatgpt-shell-prompt-compose-previous-history]` cycle through previous
item in history.
 `\\[chatgpt-shell-prompt-compose-next-history]` cycle through next item in
history.

Read-only: After sending a query, the buffer becomes read-only and
enables additional key bindings.

 `\\[chatgpt-shell-prompt-compose-send-buffer]` After sending offers to abort
query in-progress.
 `\\[View-quit]` Exits the read-only buffer.
 `\\[chatgpt-shell-prompt-compose-retry]` Refresh (re-send the query).  Useful
to retry on disconnects.
 `\\[chatgpt-shell-prompt-compose-next-block]` Jump to next source block.
 `\\[chatgpt-shell-prompt-compose-previous-block]` Jump to next previous block.
 `\\[chatgpt-shell-prompt-compose-reply]` Reply to follow-up with additional questions.
 `\\[chatgpt-shell-prompt-compose-request-entire-snippet]` Send \"Show entire snippet\" query.
 `\\[chatgpt-shell-prompt-compose-insert-block-at-point]` Insert block at point at last known location.
 `\\[chatgpt-shell-prompt-compose-request-more]` Send \"Show me more\" query.
 `\\[chatgpt-shell-prompt-compose-other-buffer]` Jump to other buffer (ie. the shell itself).
 `\\[chatgpt-shell-mark-block]` Mark block at point."
  (interactive "P")
  (chatgpt-shell-prompt-compose-show-buffer :clear-history prefix))

(defvar-local chatgpt-shell--ring-index nil)

(cl-defun chatgpt-shell-prompt-compose-show-buffer (&key content clear-history transient-frame-p)
  "Show a prompt compose buffer.

Prepopulate buffer with optional CONTENT.

Set CLEAR-HISTORY to wipe any existing shell history.

Set TRANSIENT-FRAME-P to also close frame on exit."
  (let* ((exit-on-submit (derived-mode-p 'chatgpt-shell-mode))
         (region-details)
         (input-text (or content
                     (when-let ((region-active (region-active-p))
                                (region (buffer-substring (region-beginning)
                                                          (region-end))))
                       (setq region-details (chatgpt-shell--region))
                       (deactivate-mark)
                       (concat (if-let ((buffer-file-name (buffer-file-name))
                                        (name (file-name-nondirectory buffer-file-name))
                                        (is-key-file (seq-contains-p '(".babelrc"
                                                                       ".editorconfig"
                                                                       ".eslintignore"
                                                                       ".eslintrc"
                                                                       ".eslintrc.json"
                                                                       ".mocharc.json"
                                                                       ".prettierrc"
                                                                       "package.json"
                                                                       "tsconfig.json"
                                                                       "wrangler.toml")
                                                                     name)))
                                   (format "%s: \n\n" name)
                                 "")
                               "```"
                               (cond ((listp mode-name)
                                      (downcase (car mode-name)))
                                     ((stringp mode-name)
                                      (downcase mode-name))
                                     (t
                                      ""))
                               "\n"
                               region
                               "\n"
                               "```"))
                     (when (derived-mode-p 'eshell-mode)
                       (chatgpt-shell--eshell-last-last-command))
                     (when-let* ((diagnostic (flymake-diagnostics (point)))
                                 (line-start (line-beginning-position))
                                 (line-end (line-end-position))
                                 (top-context-start (max (line-beginning-position 1) (point-min)))
                                 (top-context-end (max (line-beginning-position -5) (point-min)))
                                 (bottom-context-start (min (line-beginning-position 2) (point-max)))
                                 (bottom-context-end (min (line-beginning-position 7) (point-max)))
                                 (current-line (buffer-substring line-start line-end)))
                       (concat
                        "Fix this code and only show me a diff without explanation\n\n"
                        (mapconcat #'flymake-diagnostic-text diagnostic "\n")
                        "\n\n"
                        (buffer-substring top-context-start top-context-end)
                        (buffer-substring line-start line-end)
                        " <--- issue is here\n"
                        (buffer-substring bottom-context-start bottom-context-end)))))
         ;; TODO: Consolidate, but until then keep in sync with
         ;; inlined instructions from `chatgpt-shell-prompt-compose-send-buffer'.
         (instructions (concat "Type "
                               (propertize "C-c C-c" 'face 'help-key-binding)
                               " to send prompt. "
                               (propertize "C-c C-k" 'face 'help-key-binding)
                               " to cancel and exit. "))
         (erase-buffer (or clear-history
                           (not input-text)
                           ;; view-mode = old query, erase for new one.
                           (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                             chatgpt-shell-prompt-compose-view-mode))))
    (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
      (chatgpt-shell-prompt-compose-mode)
      (setq-local chatgpt-shell-prompt-compose--exit-on-submit exit-on-submit)
      (setq-local chatgpt-shell-prompt-compose--transient-frame-p transient-frame-p)
      (setq-local chatgpt-shell-prompt-compose--last-known-region region-details)
      (visual-line-mode +1)
      (when erase-buffer
        (chatgpt-shell-prompt-compose-view-mode -1)
        (erase-buffer))
      (when input-text
        (save-excursion
          (goto-char (point-max))
          (insert "\n\n")
          (insert input-text)
          (chatgpt-shell--put-source-block-overlays)))
      (when clear-history
        (with-current-buffer (chatgpt-shell--primary-buffer)
          (chatgpt-shell-clear-buffer)))
      ;; TODO: Find a better alternative to prevent clash.
      ;; Disable "n"/"p" for region-bindings-mode-map, so it doesn't
      ;; clash with "n"/"p" selection binding.
      (when (boundp 'region-bindings-mode-disable-predicates)
        (add-to-list 'region-bindings-mode-disable-predicates
                     (lambda () buffer-read-only)))
      (setq chatgpt-shell--ring-index nil)
      (message instructions))
    (unless transient-frame-p
      (select-window (display-buffer (chatgpt-shell-prompt-compose-buffer))))
    (chatgpt-shell-prompt-compose-buffer)))

(defun chatgpt-shell-prompt-compose-search-history ()
  "Search prompt history, select, and insert to current compose buffer."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((candidate (with-current-buffer (chatgpt-shell--primary-buffer)
                     (completing-read
                      "History: "
                      (delete-dups
                       (seq-filter
                        (lambda (item)
                          (not (string-empty-p item)))
                        (ring-elements comint-input-ring))) nil t))))
    (insert candidate)))

(defun chatgpt-shell-prompt-compose-quit-and-close-frame ()
  "Quit compose and close frame if it's the last window."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((transient-frame-p chatgpt-shell-prompt-compose--transient-frame-p))
    (quit-restore-window (get-buffer-window (current-buffer)) 'kill)
    (when (and transient-frame-p
               (< (chatgpt-shell-prompt-compose-frame-window-count) 2))
      (delete-frame))))

(defun chatgpt-shell-prompt-compose-frame-window-count ()
  "Get the number of windows per current frame."
  (if-let ((window (get-buffer-window (current-buffer)))
           (frame (window-frame window)))
      (length (window-list frame))
    0))

(defun chatgpt-shell-prompt-compose-previous-history ()
  "Insert previous prompt from history into compose buffer."
  (interactive)
  (unless chatgpt-shell-prompt-compose-view-mode
    (let* ((ring (with-current-buffer (chatgpt-shell--primary-buffer)
                   (seq-filter
                    (lambda (item)
                      (not (string-empty-p item)))
                    (ring-elements comint-input-ring))))
           (next-index (unless (seq-empty-p ring)
                         (if chatgpt-shell--ring-index
                             (1+ chatgpt-shell--ring-index)
                           0))))
      (let ((prompt (buffer-string)))
        (with-current-buffer (chatgpt-shell--primary-buffer)
          (unless (ring-member comint-input-ring prompt)
            (ring-insert comint-input-ring prompt))))
      (if next-index
          (if (>= next-index (seq-length ring))
              (setq chatgpt-shell--ring-index (1- (seq-length ring)))
            (setq chatgpt-shell--ring-index next-index))
        (setq chatgpt-shell--ring-index nil))
      (when chatgpt-shell--ring-index
        (erase-buffer)
        (insert (seq-elt ring chatgpt-shell--ring-index))))))

(defun chatgpt-shell-prompt-compose-next-history ()
  "Insert next prompt from history into compose buffer."
  (interactive)
  (unless chatgpt-shell-prompt-compose-view-mode
    (let* ((ring (with-current-buffer (chatgpt-shell--primary-buffer)
                   (seq-filter
                    (lambda (item)
                      (not (string-empty-p item)))
                    (ring-elements comint-input-ring))))
           (next-index (unless (seq-empty-p ring)
                         (if chatgpt-shell--ring-index
                             (1- chatgpt-shell--ring-index)
                           0))))
      (if next-index
          (if (< next-index 0)
              (setq chatgpt-shell--ring-index nil)
            (setq chatgpt-shell--ring-index next-index))
        (setq chatgpt-shell--ring-index nil))
      (when chatgpt-shell--ring-index
        (erase-buffer)
        (insert (seq-elt ring chatgpt-shell--ring-index))))))

(defun chatgpt-shell-prompt-compose-send-buffer ()
  "Send compose buffer content to shell for processing."
  (interactive)
  (catch 'exit
    (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
      (user-error "Not in a shell compose buffer"))
    (with-current-buffer (chatgpt-shell--primary-buffer)
      (when shell-maker--busy
        (unless (y-or-n-p "Abort?")
          (throw 'exit nil))
        (shell-maker-interrupt t)
        (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
          (progn
            (chatgpt-shell-prompt-compose-view-mode -1)
            (erase-buffer)))
        (user-error "Aborted")))
    (when (chatgpt-shell-block-action-at-point)
      (chatgpt-shell-execute-block-action-at-point)
      (throw 'exit nil))
    (when (string-empty-p
           (string-trim
            (buffer-substring-no-properties
             (point-min) (point-max))))
      (erase-buffer)
      (user-error "Nothing to send"))
    (if chatgpt-shell-prompt-compose-view-mode
        (progn
          (chatgpt-shell-prompt-compose-view-mode -1)
          (erase-buffer)
          ;; TODO: Consolidate, but until then keep in sync with
          ;; instructions from `chatgpt-shell-prompt-compose-show-buffer'.
          (message (concat "Type "
                           (propertize "C-c C-c" 'face 'help-key-binding)
                           " to send prompt. "
                           (propertize "C-c C-k" 'face 'help-key-binding)
                           " to cancel and exit. ")))
      (let ((prompt (string-trim
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))
        (erase-buffer)
        (insert (propertize (concat prompt "\n") 'face font-lock-doc-face))
        (insert (concat (propertize (concat (make-string 20 ? ) "")
                                    'face '((:underline t) font-lock-doc-face)) "\n\n\n"))
        (chatgpt-shell--put-source-block-overlays)
        (chatgpt-shell-prompt-compose-view-mode +1)
        (setq view-exit-action 'kill-buffer)
        (when (string-equal prompt "clear")
          (view-mode -1)
          (erase-buffer))
        (if chatgpt-shell-prompt-compose--exit-on-submit
            (let ((view-exit-action nil))
              (quit-window t (get-buffer-window (chatgpt-shell-prompt-compose-buffer)))
              (chatgpt-shell-send-to-buffer prompt nil nil nil 'shell))
          (chatgpt-shell-send-to-buffer prompt nil nil
                                        (lambda (_input _output _success)
                                          (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                                            (chatgpt-shell--put-source-block-overlays)))
                                        'inline))))))

(defun chatgpt-shell-prompt-compose-next-interaction (&optional backwards)
  "Show next interaction (request / response).

If BACKWARDS is non-nil, go to previous interaction."
  (interactive)
  (unless (eq (current-buffer) (chatgpt-shell-prompt-compose-buffer))
    (error "Not in a compose buffer"))
  (when-let ((shell-buffer (chatgpt-shell--primary-buffer))
             (compose-buffer (chatgpt-shell-prompt-compose-buffer))
             (next (with-current-buffer (chatgpt-shell--primary-buffer)
                     (shell-maker-next-command-and-response backwards))))
    (chatgpt-shell-prompt-compose-replace-interaction
     (car next) (cdr next))
    next))

(defun chatgpt-shell-prompt-compose-previous-interaction ()
  "Show previous interaction (request / response)."
  (interactive)
  (chatgpt-shell-prompt-compose-next-interaction t))

(defun chatgpt-shell-prompt-compose-replace-interaction (prompt &optional response)
  "Replace the current compose's buffer interaction with PROMPT and RESPONSE."
  (unless (eq (current-buffer) (chatgpt-shell-prompt-compose-buffer))
    (error "Not in a compose buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
      (when response
        (insert response))
      (chatgpt-shell--put-source-block-overlays))
    (chatgpt-shell-prompt-compose-view-mode +1)))

;; TODO: Delete and use chatgpt-shell-prompt-compose-quit-and-close-frame instead.
(defun chatgpt-shell-prompt-compose-cancel ()
  "Cancel and close compose buffer."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (chatgpt-shell-prompt-compose-quit-and-close-frame))

(defun chatgpt-shell-prompt-compose-buffer-name ()
  "Generate compose buffer name."
  (concat (chatgpt-shell--minibuffer-prompt) "compose"))

(defun chatgpt-shell-prompt-compose-swap-system-prompt ()
  "Swap the compose buffer's system prompt."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (chatgpt-shell-swap-system-prompt))
  (rename-buffer (chatgpt-shell-prompt-compose-buffer-name)))

(defun chatgpt-shell-prompt-compose-swap-model-version ()
  "Swap the compose buffer's model version."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (chatgpt-shell-swap-model))
  (rename-buffer (chatgpt-shell-prompt-compose-buffer-name)))

(defun chatgpt-shell-prompt-compose-buffer ()
  "Get the available shell compose buffer."
  (unless (chatgpt-shell--primary-buffer)
    (error "No shell to compose to"))
  (let* ((buffer (get-buffer-create (chatgpt-shell-prompt-compose-buffer-name))))
    (unless buffer
      (error "No compose buffer available"))
    buffer))

(defun chatgpt-shell-prompt-compose-retry ()
  "Retry sending request to shell.

Useful if sending a request failed, perhaps from failed connectivity."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (when-let ((prompt (with-current-buffer (chatgpt-shell--primary-buffer)
                       (seq-first (delete-dups
                                   (seq-filter
                                    (lambda (item)
                                      (not (string-empty-p item)))
                                    (ring-elements comint-input-ring))))))
             (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt nil nil
                                  (lambda (_input _output _success)
                                    (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                                      (chatgpt-shell--put-source-block-overlays)))
                                  'inline)))

(defun chatgpt-shell-prompt-compose-insert-block-at-point ()
  "Insert block at point at last known location."
  (interactive)
  (save-excursion
    (let* ((block (or (chatgpt-shell-markdown-block-at-point)
                      (error "No block at point")))
           (body (buffer-substring-no-properties (or (map-elt block 'start)
                                                     (error "No block body found"))
                                                 (or (map-elt block 'end)
                                                     (error "No block body found"))))
           (origin (or chatgpt-shell-prompt-compose--last-known-region
                       (user-error "Nowhere to insert to")))
           (window-config (current-window-configuration)))
      (switch-to-buffer-other-window (map-elt origin :buffer))
      (with-current-buffer (map-elt origin :buffer)
        (if (eq ?y (chatgpt-shell--pretty-smerge-insert
                    :text body
                    :start (map-elt origin :start)
                    :end (map-elt origin :end)
                    :buffer (map-elt origin :buffer)))
            (progn
              (map-put! origin :end (+ (map-elt origin :start)
                                       (length body)))
              (map-put! origin :text body)
              (setq chatgpt-shell-prompt-compose--last-known-region origin))
          (set-window-configuration window-config))))))

(defun chatgpt-shell-prompt-compose-next-block ()
  "Jump to and select next code block."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((before (point)))
    (if (call-interactively #'chatgpt-shell-next-source-block)
        (call-interactively #'chatgpt-shell-mark-block)
      (chatgpt-shell-prompt-compose-next-interaction)
      (when (eq before (point))
        (user-error "No more left")))))

(defun chatgpt-shell-prompt-compose-previous-block ()
  "Jump to and select previous code block."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((before (point)))
    (if (call-interactively #'chatgpt-shell-previous-source-block)
        (call-interactively #'chatgpt-shell-mark-block)
      (unless (chatgpt-shell-prompt-compose-previous-interaction)
        (deactivate-mark)
        (goto-char (point-min)))
      (when (eq before (point))
        (user-error "No more left")))))

(defun chatgpt-shell-prompt-compose-reply ()
  "Reply as a follow-up and compose another query."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait")))
  (chatgpt-shell-prompt-compose-view-mode -1)
  (erase-buffer))

(defun chatgpt-shell-prompt-compose-request-entire-snippet ()
  "If the response code is incomplete, request the entire snippet."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait")))
  (let ((prompt "show entire snippet")
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt nil nil nil 'inline)))

(defun chatgpt-shell-prompt-compose-request-more ()
  "Request more data.  This is useful if you already requested examples."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait")))
  (let ((prompt "give me more")
        (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt nil nil nil 'inline)))

(defun chatgpt-shell-prompt-compose-other-buffer ()
  "Jump to the shell buffer (compose's other buffer)."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (switch-to-buffer (chatgpt-shell--primary-buffer)))

(provide 'chatgpt-shell-prompt-compose)

;;; chatgpt-shell-prompt-compose.el ends here
