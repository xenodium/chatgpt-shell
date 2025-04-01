;;; chatgpt-shell-transient.el --- Transient menus for chatgpt-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides transient menus for interacting with chatgpt-shell.

;;; Code:

(require 'transient)
(require 'chatgpt-shell) ;; Ensure the main functions are loaded

;;;###autoload
(transient-define-prefix chatgpt-shell-transient--popup ()
  "Transient menu for chatgpt-shell commands."
  [
   "ChatGPT Shell actions"
   ["Core Shell & Compose"
    ("s" "Focus/Start Shell" chatgpt-shell)
    ("N" "Start New Shell" (lambda () (interactive) (chatgpt-shell t)))
     ("C" "Clear Shell Buffer" chatgpt-shell-clear-buffer)
     ("K" "Interrupt Request" chatgpt-shell-interrupt)
     ("e" "Compose Prompt" chatgpt-shell-prompt-compose)
     ("p" "Prompt (minibuffer)" chatgpt-shell-prompt)
     ("P" "Prompt (append kill)" chatgpt-shell-prompt-appending-kill-ring)
     ("q" "Quick Insert" chatgpt-shell-quick-insert)]

    ["Region Actions"
     ("r" "Send Region" (lambda () (interactive) (chatgpt-shell-send-region nil)))
     ("R" "Send & Review Region" chatgpt-shell-send-and-review-region)
     ("d" "Describe Code" chatgpt-shell-describe-code)
     ("f" "Refactor Code" chatgpt-shell-refactor-code)
     ("g" "Write Git Commit" chatgpt-shell-write-git-commit)
     ("t" "Generate Unit Test" chatgpt-shell-generate-unit-test)
     ("w" "Proofread Region" chatgpt-shell-proofread-region)]

   ["Shell Navigation"
    ("h" "Search History" chatgpt-shell-search-history)
    ("j" "Next Item" chatgpt-shell-next-item)
    ("k" "Previous Item" chatgpt-shell-previous-item)
     ("J" "Next Source Block" chatgpt-shell-next-source-block)
     ("K" "Previous Source Block" chatgpt-shell-previous-source-block)]

    ["Block Actions"
     ("x" "Execute Block" chatgpt-shell-execute-babel-block-action-at-point)
     ("E" "Edit Block" chatgpt-shell-edit-block-at-point)
     ("V" "View Block" chatgpt-shell-view-block-at-point)]
   ]

  [
   "Configuration & Utilities"
   ["Model & Configuration"
    ("m" "Swap Model" chatgpt-shell-swap-model)
    ("y" "Swap System Prompt" chatgpt-shell-swap-system-prompt)
     ("L" "Reload Default Models" chatgpt-shell-reload-default-models)]

    ["Session & Other"
     ("S" "Save Transcript" chatgpt-shell-save-session-transcript)
     ("O" "Restore Transcript" chatgpt-shell-restore-session-from-transcript)
     ("i" "Describe Image" chatgpt-shell-describe-image)
     ("v" "Show Version" chatgpt-shell-version)]
   ]
  )


;;;###autoload
(defun chatgpt-shell-transient ()
  "Invoke the main transient menu for chatgpt-shell."
  (interactive)
  ;; Call the correctly named transient prefix command
  (chatgpt-shell-transient--popup))

(provide 'chatgpt-shell-transient)

;;; chatgpt-shell-transient.el ends here
