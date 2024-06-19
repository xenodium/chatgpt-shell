;;; chatgpt-shell-compose.el --- Compose ChatGPT shell prompts in a dedicated buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (chatgpt-shell "1.0.12"))

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

;; `chatgpt-shell-compose' is dedicated prompt buffer compose for
;; `chatgpt-shell'.
;;
;; Run `chatgpt-shell-compose' to get a ChatGPT shell compose buffer.
;;
;; Note: This is young package still.  Please report issues or send
;; patches to https://github.com/xenodium/chatgpt-shell
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(defvar-local chatgpt-shell-compose--exit-on-submit nil
  "Whether or not compose buffer should close after submission.

This is typically used to craft prompts and immediately jump over to
the shell to follow the response.")

(defvar-local chatgpt-shell-compose--compose-buffer-p nil
  "Identifies whether or not buffer is a compose buffer.

Checking for major mode in buffer isn't reliable to distinguish
compose buffers as it could be either `view-mode' or
`chatgpt-shell-compose-mode'.  Instead, store in
`chatgpt-shell-compose--compose-buffer-p'.")

(defvar-local chatgpt-shell-compose--transient-frame-p nil
  "Identifies whether or not buffer is a compose buffer.

t if invoked from a transient frame (quitting closes the frame).")

(defvar chatgpt-shell-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chatgpt-shell-compose-send-buffer)
    (define-key map (kbd "C-c C-k") #'chatgpt-shell-compose-cancel)
    (define-key map (kbd "M-r") #'chatgpt-shell-compose-search-history)
    (define-key map (kbd "M-p") #'chatgpt-shell-compose-previous-history)
    (define-key map (kbd "M-n") #'chatgpt-shell-compose-next-history)
    map))

(defun chatgpt-shell-compose (prefix)
  "Compose and send prompt from a dedicated buffer.

With PREFIX, clear existing history (wipe asociated shell history).

Whenever `chatgpt-shell-compose' is invoked, appends any active
region (or flymake issue at point) to compose buffer.

Additionally, if point is at an error/warning raised by flymake,
automatically add context (error/warning + code) to expedite ChatGPT
for help to fix the issue.

The compose buffer always shows the latest interaction, but it's
backed by the shell history.  You can always switch to the shell buffer
to view the history.

Editing: While compose buffer is in in edit mode, it offers a couple
of magit-like commit buffer bindings.

 `\\[chatgpt-shell-compose-send-buffer]` to send the buffer query.
 `\\[chatgpt-shell-compose-cancel]` to cancel compose buffer.
 `\\[chatgpt-shell-compose-search-history]` search through history.
 `\\[chatgpt-shell-compose-previous-history]` cycle through previous
item in history.
 `\\[chatgpt-shell-compose-next-history]` cycle through next item in
history.

Read-only: After sending a query, the buffer becomes read-only and
enables additional key bindings.

 `\\[chatgpt-shell-compose-send-buffer]` After sending offers to abort
query in-progress.
 `\\[View-quit]` Exits the read-only buffer.
 `\\[chatgpt-shell-compose-retry]` Refresh (re-send the query).  Useful
to retry on disconnects.
 `\\[chatgpt-shell-compose-next-block]` Jump to next source block.
 `\\[chatgpt-shell-compose-previous-block]` Jump to next previous block.
 `\\[chatgpt-shell-compose-reply]` Reply to follow-up with additional questions.
 `\\[chatgpt-shell-compose-request-entire-snippet]` Send \"Show entire snippet\" query (useful to request alternative
 `\\[chatgpt-shell-compose-other-buffer]` Jump to other buffer (ie. the shell itself).
 `\\[chatgpt-shell-mark-block]` Mark block at point."
  (interactive "P")
  (chatgpt-shell-compose-show-buffer nil prefix))

(defun chatgpt-shell-compose-show-buffer (&optional content clear-history transient-frame-p)
  "Show a prompt compose buffer.

Prepopulate buffer with optional CONTENT.

Set CLEAR-HISTORY to wipe any existing shell history.

Set TRANSIENT-FRAME-P to also close frame on exit."
  (let* ((exit-on-submit (eq major-mode 'chatgpt-shell-mode))
         (region (or content
                     (when-let ((region-active (region-active-p))
                                (region (buffer-substring (region-beginning)
                                                          (region-end))))
                       (deactivate-mark)
                       region)
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
         (instructions (concat "Type "
                               (propertize "C-c C-c" 'face 'help-key-binding)
                               " to send prompt. "
                               (propertize "C-c C-k" 'face 'help-key-binding)
                               " to cancel and exit. "))
         (erase-buffer (or clear-history
                           (not region)
                           ;; view-mode = old query, erase for new one.
                           (with-current-buffer (chatgpt-shell-compose-buffer)
                             view-mode)))
         (prompt))
    (with-current-buffer (chatgpt-shell-compose-buffer)
      (chatgpt-shell-compose-mode)
      (setq-local chatgpt-shell-compose--exit-on-submit exit-on-submit)
      (setq-local chatgpt-shell-compose--transient-frame-p transient-frame-p)
      (setq-local chatgpt-shell-compose--compose-buffer-p t)
      (visual-line-mode +1)
      (when view-mode
        (view-mode -1))
      (when erase-buffer
        (erase-buffer))
      (when region
        (save-excursion
          (goto-char (point-min))
          (let ((insert-trailing-newlines (not (looking-at-p "\n\n"))))
            (insert "\n\n")
            (insert region)
            (when insert-trailing-newlines
              (insert "\n\n")))))
      (when clear-history
        (let ((chatgpt-shell-prompt-query-response-style 'inline))
          (chatgpt-shell-send-to-buffer "clear")))
      ;; TODO: Find a better alternative to prevent clash.
      ;; Disable "n"/"p" for region-bindings-mode-map, so it doesn't
      ;; clash with "n"/"p" selection binding.
      (when (boundp 'region-bindings-mode-disable-predicates)
        (add-to-list 'region-bindings-mode-disable-predicates
                     (lambda () buffer-read-only)))
      (defvar-local chatgpt-shell--ring-index nil)
      (setq chatgpt-shell--ring-index nil)
      (message instructions))
    ;; Is there a window already displaying a chatgpt compose/output buffer?
    (if-let* ((buffer-name-regex (rx (| (group "*chatgpt* " (+ nonl) "> " (+ nonl)) (group "ChatGPT> " (+ nonl)))))
              (window (get-window-with-predicate
                       (lambda (w)
                         (string-match buffer-name-regex
                                       (buffer-name (window-buffer w)))))))
        (progn
          (set-window-buffer window (chatgpt-shell-compose-buffer))
          (select-window window))
      (pop-to-buffer (chatgpt-shell-compose-buffer)))
    (chatgpt-shell-compose-buffer)))

(defun chatgpt-shell-compose-search-history ()
  "Search prompt history, select, and insert to current compose buffer."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (let ((candidate (with-current-buffer (chatgpt-shell--primary-buffer)
                     (completing-read
                      "History: "
                      (delete-dups
                       (seq-filter
                        (lambda (item)
                          (not (string-empty-p item)))
                        (ring-elements comint-input-ring))) nil t))))
    (insert candidate)))

(defun chatgpt-shell-compose-quit-and-close-frame ()
  "Quit compose and close frame if it's the last window."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (let ((transient-frame-p chatgpt-shell-compose--transient-frame-p))
    (quit-restore-window (get-buffer-window (current-buffer)) 'kill)
    (when (and transient-frame-p
               (< (chatgpt-shell-compose-frame-window-count) 2))
      (delete-frame))))

(defun chatgpt-shell-compose-frame-window-count ()
  "Get the number of windows per current frame."
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (if-let ((window (get-buffer-window (current-buffer)))
           (frame (window-frame window)))
      (length (window-list frame))
    0))

(defun chatgpt-shell-compose-previous-history ()
  "Insert previous prompt from history into compose buffer."
  (interactive)
  (unless view-mode
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

(defun chatgpt-shell-compose-next-history ()
  "Insert next prompt from history into compose buffer."
  (interactive)
  (unless view-mode
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

(defun chatgpt-shell-mark-block ()
  "Mark current block in compose buffer."
  (interactive)
  (when-let ((block (chatgpt-shell-markdown-block-at-point)))
    (set-mark (map-elt block 'end))
    (goto-char (map-elt block 'start))))

(defun chatgpt-shell-compose-send-buffer ()
  "Send compose buffer content to shell for processing."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (unless (y-or-n-p "Abort?")
        (cl-return))
      (shell-maker-interrupt t)
      (with-current-buffer (chatgpt-shell-compose-buffer)
        (progn
          (view-mode -1)
          (erase-buffer)))
      (user-error "Aborted")))
  (when (chatgpt-shell-block-action-at-point)
    (chatgpt-shell-execute-block-action-at-point)
    (cl-return))
  (when (string-empty-p
         (string-trim
          (buffer-substring-no-properties
           (point-min) (point-max))))
    (erase-buffer)
    (user-error "Nothing to send"))
  (if view-mode
      (progn
        (view-mode -1)
        (erase-buffer)
        (message instructions))
    (setq prompt
          (string-trim
           (buffer-substring-no-properties
            (point-min) (point-max))))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (view-mode +1)
    ;; Make buffer-local view-mode bindings.
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map view-mode-map)
      (define-key map (kbd "g") #'chatgpt-shell-compose-retry)
      (define-key map (kbd "C-M-h") #'chatgpt-shell-mark-block)
      (define-key map (kbd "n") #'chatgpt-shell-compose-next-block)
      (define-key map (kbd "p") #'chatgpt-shell-compose-previous-block)
      (define-key map (kbd "r") #'chatgpt-shell-compose-reply)
      (define-key map (kbd "q") #'chatgpt-shell-compose-quit-and-close-frame)
      (define-key map (kbd "e") #'chatgpt-shell-compose-request-entire-snippet)
      (define-key map (kbd "o") #'chatgpt-shell-compose-other-buffer)
      (setq-local minor-mode-overriding-map-alist
                  (list (cons 'view-mode map))))
    (setq view-exit-action 'kill-buffer)
    (when (string-equal prompt "clear")
      (view-mode -1)
      (erase-buffer))
    (if chatgpt-shell-compose--exit-on-submit
        (let ((view-exit-action nil)
              (chatgpt-shell-prompt-query-response-style 'shell))
          (quit-window t (get-buffer-window (chatgpt-shell-compose-buffer)))
          (chatgpt-shell-send-to-buffer prompt))
      (let ((chatgpt-shell-prompt-query-response-style 'inline))
        (chatgpt-shell-send-to-buffer prompt)))))

;; TODO: Delete and use chatgpt-shell-compose-quit-and-close-frame instead.
(defun chatgpt-shell-compose-cancel ()
  "Cancel and close compose buffer."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (chatgpt-shell-compose-quit-and-close-frame))

(define-derived-mode chatgpt-shell-compose-mode fundamental-mode "ChatGPT Compose"
  "Major mode for composing ChatGPT prompts from a dedicated buffer."
  :keymap chatgpt-shell-compose-mode-map)

(defun chatgpt-shell-compose-buffer ()
  "Get the available shell compose buffer."
  (unless (chatgpt-shell--primary-buffer)
    (error "No shell to compose to"))
  (let* ((buffer-name (concat (chatgpt-shell--minibuffer-prompt)
                              "compose"))
         (buffer (get-buffer-create buffer-name)))
    (unless buffer
      (error "No compose buffer available"))
    buffer))

(defun chatgpt-shell-compose-retry ()
  "Retry sending request to shell.

Useful if sending a request failed, perhaps from failed connectivity."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (when-let ((prompt (with-current-buffer (chatgpt-shell--primary-buffer)
                       (seq-first (delete-dups
                                   (seq-filter
                                    (lambda (item)
                                      (not (string-empty-p item)))
                                    (ring-elements comint-input-ring))))))
             (inhibit-read-only t)
             (chatgpt-shell-prompt-query-response-style 'inline))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt)))

(defun chatgpt-shell-compose-next-block ()
  "Jump to and select next code block."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (call-interactively #'chatgpt-shell-next-source-block)
  (when-let ((block (chatgpt-shell-markdown-block-at-point)))
    (set-mark (map-elt block 'end))
    (goto-char (map-elt block 'start))))

(defun chatgpt-shell-compose-previous-block ()
  "Jump to and select previous code block."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (call-interactively #'chatgpt-shell-previous-source-block)
  (when-let ((block (chatgpt-shell-markdown-block-at-point)))
    (set-mark (map-elt block 'end))
    (goto-char (map-elt block 'start))))

(defun chatgpt-shell-compose-reply ()
  "Reply as a follow-up and compose another query."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait")))
  (view-mode -1)
  (erase-buffer))

(defun chatgpt-shell-compose-request-entire-snippet ()
  "If the response code is incomplete, request the entire snippet."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait")))
  (let ((prompt "show entire snippet")
        (inhibit-read-only t)
        (chatgpt-shell-prompt-query-response-style 'inline))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt)))

(defun chatgpt-shell-compose-other-buffer ()
  "Jump to the shell buffer (compose's other buffer)."
  (interactive)
  (unless chatgpt-shell-compose--compose-buffer-p
    (user-error "Not in a compose buffer"))
  (switch-to-buffer (chatgpt-shell--primary-buffer)))

(provide 'chatgpt-shell-compose)

;;; chatgpt-shell-compose.el ends here
