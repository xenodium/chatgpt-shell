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
(require 'transient)

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
(declare-function chatgpt-shell--pretty-smerge-insert "chatgpt-shell")
(declare-function chatgpt-shell-markdown-block-at-point "chatgpt-shell")
(declare-function chatgpt-shell-view-block-at-point "chatgpt-shell")
(declare-function chatgpt-shell-copy-block-at-point "chatgpt-shell")

(defcustom chatgpt-shell-compose-auto-transient t
  "When non-nil automatically display transient menu post compose submission."
  :type 'boolean
  :group 'chatgpt-shell)

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
    (define-key map (kbd "n") #'chatgpt-shell-prompt-compose-next-item)
    (define-key map (kbd "b") #'chatgpt-shell-prompt-compose-previous-interaction)
    (define-key map (kbd "f") #'chatgpt-shell-prompt-compose-next-interaction)
    (define-key map (kbd "p") #'chatgpt-shell-prompt-compose-previous-item)
    (define-key map (kbd "<tab>") #'chatgpt-shell-prompt-compose-next-item)
    (define-key map (kbd "<backtab>") #'chatgpt-shell-prompt-compose-previous-item)
    (define-key map (kbd "r") #'chatgpt-shell-prompt-compose-reply)
    (define-key map (kbd "q") #'chatgpt-shell-prompt-compose-quit-and-close-frame)
    (define-key map (kbd "e") #'chatgpt-shell-prompt-compose-request-entire-snippet)
    (define-key map (kbd "v") #'chatgpt-shell-view-block-at-point)
    (define-key map (kbd "i") #'chatgpt-shell-prompt-compose-insert-block-at-point)
    (define-key map (kbd "m") #'chatgpt-shell-prompt-compose-request-more)
    (define-key map (kbd "o") #'chatgpt-shell-prompt-compose-other-buffer)
    (set-keymap-parent map view-mode-map)
    map)
  "Keymap for `chatgpt-shell-prompt-compose-view-mode'.")

(transient-define-prefix chatgpt-shell-prompt-compose-transient ()
  "ChatGPT Shell Compose Transient."
  [["Navigation"
    ("n" "Next item" chatgpt-shell-prompt-compose-next-item :transient t)
    ("p" "Previous item" chatgpt-shell-prompt-compose-previous-item :transient t)
    ("f" "Next interaction" chatgpt-shell-prompt-compose-next-interaction :transient t)
    ("b" "Previous interaction" chatgpt-shell-prompt-compose-previous-interaction :transient t)
    ("o" "View other shell buffer" chatgpt-shell-prompt-compose-other-buffer)]
   ["Prompts"
    ("g" "Retry" chatgpt-shell-prompt-compose-retry)
    ("r" "Reply with followup" chatgpt-shell-prompt-compose-reply)
    ("m" "Request more" chatgpt-shell-prompt-compose-request-more)
    ("e" "Request entire snippet" chatgpt-shell-prompt-compose-request-entire-snippet)]
   ["Blocks"
    ("v" "View block at point" chatgpt-shell-view-block-at-point)
    ("i" "Insert block at point" chatgpt-shell-prompt-compose-insert-block-at-point)
    ("w" "Copy block at point" chatgpt-shell-copy-block-at-point)
    ("C-c C-c" "Execute block at point" chatgpt-shell-execute-block-action-at-point)]
   ["Quit"
    ("q" "Quit shell compose buffer" chatgpt-shell-prompt-compose-quit-and-close-frame)]])

(define-minor-mode chatgpt-shell-prompt-compose-view-mode
  "Like `view-mode`, but extended for ChatGPT Compose."
  :lighter " ChatGPT view"
  :keymap chatgpt-shell-prompt-compose-view-mode-map
  (setq buffer-read-only chatgpt-shell-prompt-compose-view-mode))

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
                                (region (buffer-substring-no-properties (region-beginning)
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
                                 (current-line (buffer-substring-no-properties line-start line-end)))
                       (concat
                        "Fix this code and only show me a diff without explanation\n\n"
                        (mapconcat #'flymake-diagnostic-text diagnostic "\n")
                        "\n\n"
                        (buffer-substring-no-properties top-context-start top-context-end)
                        (buffer-substring-no-properties line-start line-end)
                        " <--- issue is here\n"
                        (buffer-substring-no-properties bottom-context-start bottom-context-end)))))
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
      (unless transient-frame-p
        (select-window (display-buffer (chatgpt-shell-prompt-compose-buffer))))
      (chatgpt-shell-prompt-compose-mode)
      (setq-local chatgpt-shell-prompt-compose--exit-on-submit exit-on-submit)
      (setq-local chatgpt-shell-prompt-compose--transient-frame-p transient-frame-p)
      (setq-local chatgpt-shell-prompt-compose--last-known-region region-details)
      (visual-line-mode +1)
      (when clear-history
        (with-current-buffer (chatgpt-shell--primary-buffer)
          (chatgpt-shell-clear-buffer)))
      (when (or erase-buffer
                (string-empty-p (string-trim (chatgpt-shell-prompt-compose--text))))
        (chatgpt-shell-prompt-compose-view-mode -1)
        (chatgpt-shell-prompt-compose--initialize))
      (when input-text
        (save-excursion
          (goto-char (point-max))
          (insert "\n\n")
          (insert input-text)
          (let ((inhibit-read-only t))
            (chatgpt-shell--put-source-block-overlays))))
      ;; TODO: Find a better alternative to prevent clash.
      ;; Disable "n"/"p" for region-bindings-mode-map, so it doesn't
      ;; clash with "n"/"p" selection binding.
      (when (boundp 'region-bindings-mode-disable-predicates)
        (add-to-list 'region-bindings-mode-disable-predicates
                     (lambda () buffer-read-only)))
      (setq chatgpt-shell--ring-index nil)
      (message instructions))
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
      (if next-index
          (if (>= next-index (seq-length ring))
              (setq chatgpt-shell--ring-index (1- (seq-length ring)))
            (setq chatgpt-shell--ring-index next-index))
        (setq chatgpt-shell--ring-index nil))
      (when chatgpt-shell--ring-index
        (chatgpt-shell-prompt-compose--initialize
         (seq-elt ring chatgpt-shell--ring-index))))))

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
        (chatgpt-shell-prompt-compose--initialize
         (seq-elt ring chatgpt-shell--ring-index))))))

(defun chatgpt-shell-prompt-compose--initialize (&optional prompt)
  "Initialize compose buffer.

Optionally set its PROMPT."
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (chatgpt-shell-prompt-compose--history-label))
    (when prompt
      (insert (propertize (concat prompt "\n\n")
                          'rear-nonsticky t
                          'prompt t
                          'request t
                          'face font-lock-doc-face)))))

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
            (chatgpt-shell-prompt-compose--initialize)))
        (user-error "Aborted")))
    (when (chatgpt-shell-block-action-at-point)
      (chatgpt-shell-execute-block-action-at-point)
      (throw 'exit nil))
    (when (string-empty-p
           (string-trim (chatgpt-shell-prompt-compose--text)))
      (chatgpt-shell-prompt-compose--initialize)
      (user-error "Nothing to send"))
    (if chatgpt-shell-prompt-compose-view-mode
        (progn
          (chatgpt-shell-prompt-compose-view-mode -1)
          (chatgpt-shell-prompt-compose--initialize)
          ;; TODO: Consolidate, but until then keep in sync with
          ;; instructions from `chatgpt-shell-prompt-compose-show-buffer'.
          (message (concat "Type "
                           (propertize "C-c C-c" 'face 'help-key-binding)
                           " to send prompt. "
                           (propertize "C-c C-k" 'face 'help-key-binding)
                           " to cancel and exit. ")))
      (let ((prompt (string-trim
                     (chatgpt-shell-prompt-compose--text))))
        (chatgpt-shell-prompt-compose--initialize prompt)
        (let ((inhibit-read-only t))
          (chatgpt-shell--put-source-block-overlays))
        (chatgpt-shell-prompt-compose-view-mode +1)
        (setq view-exit-action 'kill-buffer)
        (when (string-equal prompt "clear")
          (view-mode -1)
          (chatgpt-shell-prompt-compose--initialize))
        (if chatgpt-shell-prompt-compose--exit-on-submit
            (let ((view-exit-action nil))
              (quit-window t (get-buffer-window (chatgpt-shell-prompt-compose-buffer)))
              (chatgpt-shell-send-to-buffer prompt nil nil nil 'shell))
          (chatgpt-shell-send-to-buffer prompt nil nil
                                        (lambda (_input _output _success)
                                          (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                                            (let ((inhibit-read-only t))
                                              (chatgpt-shell--put-source-block-overlays)))
                                          (when chatgpt-shell-compose-auto-transient
                                            (chatgpt-shell-prompt-compose-transient)))
                                        'inline))
        ;; Point should go to beginning of response after submission.
        (goto-char (point-min))
        (text-property-search-forward 'request nil)))))

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
    (text-property-search-forward 'prompt t)
    next))

(defun chatgpt-shell-prompt-compose-refresh ()
  "Refresh compose buffer content with curernt item from shell."
  (interactive)
  (unless (eq (current-buffer) (chatgpt-shell-prompt-compose-buffer))
    (error "Not in a compose buffer"))
  (when-let ((shell-buffer (chatgpt-shell--primary-buffer))
             (compose-buffer (chatgpt-shell-prompt-compose-buffer))
             (current (with-current-buffer (chatgpt-shell--primary-buffer)
                        (or (shell-maker--command-and-response-at-point)
                            (shell-maker-next-command-and-response t)))))
    (chatgpt-shell-prompt-compose-replace-interaction
     (car current) (cdr current))
    (text-property-search-forward 'prompt t)
    current))

(defun chatgpt-shell-prompt-compose-previous-interaction ()
  "Show previous interaction (request / response)."
  (interactive)
  (chatgpt-shell-prompt-compose-next-interaction t))

(defun chatgpt-shell-prompt-compose-replace-interaction (prompt &optional response)
  "Replace the current compose's buffer interaction with PROMPT and RESPONSE."
  (unless (eq (current-buffer) (chatgpt-shell-prompt-compose-buffer))
    (error "Not in a compose buffer"))
  (let ((inhibit-read-only t))
    (save-excursion
      (chatgpt-shell-prompt-compose--initialize prompt)
      (when response
        (insert response))
      (chatgpt-shell--put-source-block-overlays))
    (chatgpt-shell-prompt-compose-view-mode +1)))

(defun chatgpt-shell-prompt-compose--history-label ()
  "Return the position in history of the primary shell buffer."
  (let ((pos (or (chatgpt-shell-prompt-compose--position)
                 (cons 1 1))))
    (propertize (format "[%d/%d]\n\n" (car pos) (cdr pos))
                 'ignore t
                 'read-only t
                 'face font-lock-comment-face
                 'rear-nonsticky t)))

(defun chatgpt-shell-prompt-compose--position ()
  "Return the position in history of the primary shell buffer."
  (let* ((current (with-current-buffer (chatgpt-shell--primary-buffer)
                    (shell-maker--command-and-response-at-point)))
         (history (with-current-buffer (chatgpt-shell--primary-buffer)
                    (shell-maker-history)))
         (pos (seq-position history current)))
    (cond ((and current history pos)
           (cons (1+ pos) (length history)))
          (history
           (cons (1+ (length history))
                 (1+ (length history)))))))

(defun chatgpt-shell-prompt-compose--text ()
  "Get the compose buffer text (excluding header)."
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((text (buffer-string))
        (result "")
        (pos 0))
    (while (< pos (length text))
      (let ((next (or (next-single-property-change pos 'ignore text)
                      (length text))))
        (unless (get-text-property pos 'ignore text)
          (setq result (concat result (substring text pos next))))
        (setq pos next)))
    (with-temp-buffer
      (insert (string-trim result))
      (buffer-substring-no-properties (point-min)
                                      (point-max)))))

;; TODO: Delete and use chatgpt-shell-prompt-compose-quit-and-close-frame instead.
(defun chatgpt-shell-prompt-compose-cancel ()
  "Cancel and close compose buffer."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (chatgpt-shell-prompt-compose-quit-and-close-frame))

(defun chatgpt-shell-prompt-compose-buffer-name ()
  "Generate compose buffer name."
  (concat "*"
          (chatgpt-shell--minibuffer-prompt) "compose"
          "*"))

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
    (chatgpt-shell-prompt-compose--initialize prompt)
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

(defun chatgpt-shell-prompt-compose-next-item ()
  "Jump to and select next item (request, response, block, link, interaction)."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let* ((request (save-excursion
                    (when (get-text-property (point) 'ignore)
                      (text-property-search-forward 'request t)
                      (point))))
         (response (save-excursion
                     (when (get-text-property (point) 'request)
                       (text-property-search-forward 'request nil)
                       (point))))
         (next-block (save-excursion
                       (call-interactively #'chatgpt-shell-next-source-block)))
         (next-link (save-excursion
                      (call-interactively #'chatgpt-shell-next-link)))
         (positions (delq nil (list next-block
                                    next-link
                                    response
                                    request)))
         (next-pos (when positions
                     (apply 'min positions))))
    (cond ((not next-pos)
           (chatgpt-shell-prompt-compose-next-interaction))
          ((eq next-pos request)
           (deactivate-mark)
           (goto-char next-pos))
          ((eq next-pos next-block)
           (deactivate-mark)
           (goto-char next-block)
           (call-interactively #'chatgpt-shell-mark-block))
          ((eq next-pos response)
           (deactivate-mark)
           (goto-char next-pos))
          ((eq next-pos next-link)
           (deactivate-mark)
           (goto-char next-link)))))

(defun chatgpt-shell-prompt-compose-previous-item ()
  "Jump to and select previous item (request, response, block, link, interaction)."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let* ((location (point))
         (response (save-excursion
                     (when (and (not (get-text-property (point) 'ignore))
                                (not (get-text-property (point) 'request)))
                       (text-property-search-backward 'request nil)
                       (text-property-search-forward 'request nil)
                       (unless (eq location (point))
                         (point)))))
         (request (unless response
                    (save-excursion
                      (goto-char (point-min))
                      (text-property-search-forward 'prompt t)
                      (point))))
         (previous-block (save-excursion
                           (call-interactively #'chatgpt-shell-previous-source-block)))
         (previous-link (save-excursion
                          (call-interactively #'chatgpt-shell-previous-link)))
         (positions (delq nil (list previous-block
                                    previous-link
                                    response
                                    request)))
         (previous-pos (when positions
                         (apply 'max positions))))
    (cond ((eq (point) (save-excursion
                         (goto-char (point-min))
                         (text-property-search-forward 'prompt t)
                         (point)))
           (deactivate-mark)
           (chatgpt-shell-prompt-compose-previous-interaction))
          ((eq previous-pos request)
           (deactivate-mark)
           (goto-char request))
          ((not previous-pos)
           (deactivate-mark)
           (goto-char (point-min))
           (text-property-search-forward 'prompt t))
          ((eq previous-pos previous-block)
           (deactivate-mark)
           (goto-char previous-block)
           (call-interactively #'chatgpt-shell-mark-block))
          ((eq previous-pos response)
           (deactivate-mark)
           (goto-char previous-pos))
          ((eq previous-pos previous-link)
           (deactivate-mark)
           (goto-char previous-link)))))

(defun chatgpt-shell-prompt-compose-reply ()
  "Reply as a follow-up and compose another query."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait"))
    (goto-char (point-max)))
  (chatgpt-shell-prompt-compose-view-mode -1)
  (chatgpt-shell-prompt-compose--initialize))

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
    (chatgpt-shell-prompt-compose--initialize prompt)
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
    (chatgpt-shell-prompt-compose--initialize prompt)
    (chatgpt-shell-send-to-buffer prompt nil nil nil 'inline)))

(defun chatgpt-shell-prompt-compose-other-buffer ()
  "Jump to the shell buffer (compose's other buffer)."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (switch-to-buffer (chatgpt-shell--primary-buffer)))

(provide 'chatgpt-shell-prompt-compose)

;;; chatgpt-shell-prompt-compose.el ends here
