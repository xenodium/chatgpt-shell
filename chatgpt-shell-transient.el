;;; chatgpt-shell-transient.el --- Transient menus for chatgpt-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides transient menus for interacting with chatgpt-shell.

;;; Code:

(require 'transient)

(defun chatgpt-shell-transient--in-shell-p ()
  "Return non-nil if the current buffer is in chatgpt-shell-mode."
  (derived-mode-p 'chatgpt-shell-mode))

;;;###autoload
(transient-define-prefix chatgpt-shell-transient--popup ()
  "Transient menu for chatgpt-shell commands."
  :transient-suffix 'chatgpt-shell-transient--popup--suffix
  [ ;; Group 1: Always available core actions
   "ChatGPT Shell actions"
   ["Core Shell & Compose"
    ("s" "Focus/Start Shell" chatgpt-shell)
    ("N" "Start New Shell" (lambda () (interactive) (chatgpt-shell t)))
    ("e" "Compose Prompt" chatgpt-shell-prompt-compose)
    ("p" "Prompt (minibuffer)"
     (lambda ()
       (interactive)
       (transient-quit-one)
       (run-with-idle-timer 0 nil #'chatgpt-shell-prompt)))
    ("P" "Prompt (append kill)"
     (lambda ()
       (interactive)
       (transient-quit-one)
       (run-with-idle-timer 0 nil #'chatgpt-shell-prompt-appending-kill-ring)))
     ("q" "Quick Insert"
      (lambda ()
        (interactive)
        (transient-quit-one)
        (run-with-idle-timer 0 nil #'chatgpt-shell-quick-insert)))]

   ;; Group 2: Region-specific actions (available if region is active)
   ["Region Actions"
    ("r" "Send Region" (lambda () (interactive) (chatgpt-shell-send-region nil)) :if region-active-p)
    ("R" "Send & Review Region" chatgpt-shell-send-and-review-region :if region-active-p)
    ("d" "Describe Code" chatgpt-shell-describe-code :if region-active-p)
    ("f" "Refactor Code" chatgpt-shell-refactor-code :if region-active-p)
    ("g" "Write Git Commit" chatgpt-shell-write-git-commit :if region-active-p)
    ("t" "Generate Unit Test" chatgpt-shell-generate-unit-test :if region-active-p)
    ("w" "Proofread Region" chatgpt-shell-proofread-region :if region-active-p)]
   ]

  [ ;; Group 3: Shell-specific actions (available only in chatgpt-shell buffer)
   "Shell Context Actions"
   ["Shell Buffer Management"
    ("C" "Clear Shell Buffer" chatgpt-shell-clear-buffer :if chatgpt-shell-transient--in-shell-p)
    ("I" "Interrupt Request" chatgpt-shell-interrupt :if chatgpt-shell-transient--in-shell-p)]

   ["Shell Navigation"
    ("h" "Search History"
     (lambda ()
       (interactive)
       (transient-quit-one)
       (run-with-idle-timer 0 nil #'chatgpt-shell-search-history)) :if chatgpt-shell-transient--in-shell-p)
    ("j" "Next Item" chatgpt-shell-next-item :if chatgpt-shell-transient--in-shell-p)
    ("k" "Previous Item" chatgpt-shell-previous-item :if chatgpt-shell-transient--in-shell-p)
    ("J" "Next Source Block" chatgpt-shell-next-source-block :if chatgpt-shell-transient--in-shell-p)
    ("K" "Previous Source Block" chatgpt-shell-previous-source-block :if chatgpt-shell-transient--in-shell-p)]

   ["Block Actions"
    ("x" "Execute Block" chatgpt-shell-execute-babel-block-action-at-point :if chatgpt-shell-transient--in-shell-p)
    ("E" "Edit Block" chatgpt-shell-edit-block-at-point :if chatgpt-shell-transient--in-shell-p)
    ("V" "View Block" chatgpt-shell-view-block-at-point :if chatgpt-shell-transient--in-shell-p)]

   ["Shell Configuration"
    ("m" "Swap Model" chatgpt-shell-swap-model :if chatgpt-shell-transient--in-shell-p)
    ("y" "Swap System Prompt" chatgpt-shell-swap-system-prompt :if chatgpt-shell-transient--in-shell-p)]

   ["Shell Session"
    ("S" "Save Transcript"
     (lambda ()
       (interactive)
       (transient-quit-one)
       (run-with-idle-timer 0 nil #'chatgpt-shell-save-session-transcript)) :if chatgpt-shell-transient--in-shell-p)
    ("O" "Restore Transcript" chatgpt-shell-restore-session-from-transcript :if chatgpt-shell-transient--in-shell-p)]
   ]

  [ ;; Group 4: General utilities (mostly always available)
   "Configuration & Utilities"
   ["Model & Configuration"
    ("L" "Reload Default Models" chatgpt-shell-reload-default-models)]

   ["Other"
    ("v" "Show Version" chatgpt-shell-version)]
   ]
  )

(transient-define-suffix chatgpt-shell-transient--popup--suffix ()
  :description "ChatGPT Shell Transient Suffix"
  :class 'transient-suffix)


;;;###autoload
(defun chatgpt-shell-transient ()
  "Invoke the main transient menu for chatgpt-shell."
  (interactive)
  (chatgpt-shell-transient--popup))

(provide 'chatgpt-shell-transient)

;;; chatgpt-shell-transient.el ends here
