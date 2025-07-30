;;; chatgpt-shell-transient.el --- Transient menus for chatgpt-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides transient menus for interacting with chatgpt-shell.

;;; Code:

(require 'transient)

(with-eval-after-load 'transient
  (defun chatgpt-shell-transient--in-shell-p ()
    "Return non-nil if the current buffer is in chatgpt-shell-mode."
    (derived-mode-p 'chatgpt-shell-mode))

  (transient-define-prefix chatgpt-shell-transient--popup ()
    "Transient menu for chatgpt-shell commands."
    :transient-suffix 'chatgpt-shell-transient--popup--suffix
    :transient-non-suffix 'transient--do-stay
    [ ;; Row 1: Always available core actions
     ["Shells"
      ("b" "Switch to shell buffer" chatgpt-shell)
      ("N" "Create new shell" (lambda () (interactive) (chatgpt-shell t)))]

     ["Compose prompt via"
      ("e" "Dedicated buffer" chatgpt-shell-prompt-compose)
      ("p" "Minibuffer"
       (lambda ()
         (interactive)
         (transient-quit-one)
         (run-with-idle-timer 0 nil #'chatgpt-shell-prompt)))
      ("P" "Minibuffer (include last kill)"
       (lambda ()
         (interactive)
         (transient-quit-one)
         (run-with-idle-timer 0 nil #'chatgpt-shell-prompt-appending-kill-ring)))]

     ["Inline edit"
      ("q" "Quick insert/edit"
       (lambda ()
         (interactive)
         (transient-quit-one)
         (run-with-idle-timer 0 nil #'chatgpt-shell-quick-insert)))
      ("r" "Send region" (lambda () (interactive) (chatgpt-shell-send-region nil)) :if region-active-p)
      ("R" "Send & review region" chatgpt-shell-send-and-review-region :if region-active-p)]
     ]

    [ ;; Row 2: Session management and navigation
     ["Session"
      ("m" "Swap model" chatgpt-shell-swap-model :if chatgpt-shell-transient--in-shell-p)
      ("L" "Reload models" chatgpt-shell-reload-default-models)
      ("y" "Swap system prompt" chatgpt-shell-swap-system-prompt :if chatgpt-shell-transient--in-shell-p)]

     ["History"
      ("h" "Search"
       (lambda ()
         (interactive)
         (transient-quit-one)
         (run-with-idle-timer 0 nil #'chatgpt-shell-search-history)) :if chatgpt-shell-transient--in-shell-p)]

     ["Navigation"
      ("n" "Next item" chatgpt-shell-next-item :if chatgpt-shell-transient--in-shell-p :transient t)
      ("p" "Previous item" chatgpt-shell-previous-item :if chatgpt-shell-transient--in-shell-p :transient t)
      ("TAB" "Next source block" chatgpt-shell-next-source-block :if chatgpt-shell-transient--in-shell-p :transient t)
      ("<backtab>" "Previous source block" chatgpt-shell-previous-source-block :if chatgpt-shell-transient--in-shell-p :transient t)]
     ]

    [ ;; Row 3: Source blocks, transcript, and code actions
     ["Source blocks"
      ("C-c C-c" "Execute" chatgpt-shell-execute-babel-block-action-at-point :if chatgpt-shell-transient--in-shell-p)
      ("E" "Edit" chatgpt-shell-edit-block-at-point :if chatgpt-shell-transient--in-shell-p)
      ("V" "View" chatgpt-shell-view-block-at-point :if chatgpt-shell-transient--in-shell-p)]

     ["Transcript"
      ("S" "Save"
       (lambda ()
         (interactive)
         (transient-quit-one)
         (run-with-idle-timer 0 nil #'chatgpt-shell-save-session-transcript)) :if chatgpt-shell-transient--in-shell-p)
      ("O" "Restore" chatgpt-shell-restore-session-from-transcript :if chatgpt-shell-transient--in-shell-p)]

     ["Code Actions"
      ("d" "Describe code" chatgpt-shell-describe-code :if region-active-p)
      ("f" "Refactor code" chatgpt-shell-refactor-code :if region-active-p)
      ("g" "Write git commit" chatgpt-shell-write-git-commit :if region-active-p)
      ("t" "Generate unit test" chatgpt-shell-generate-unit-test :if region-active-p)
      ("w" "Proofread" chatgpt-shell-proofread-paragraph-or-region :if region-active-p)]
     ]

    [ ;; Row 4: Utilities and other actions
     ["Other"
      ("C" "Clear buffer" chatgpt-shell-clear-buffer :if chatgpt-shell-transient--in-shell-p)
      ("I" "Interrupt" chatgpt-shell-interrupt :if chatgpt-shell-transient--in-shell-p)
      ("v" "Show version" chatgpt-shell-version)]
     ]
    )

  (transient-define-suffix chatgpt-shell-transient--popup--suffix ()
    :description "ChatGPT Shell Transient Suffix"
    :class 'transient-suffix))

;;;###autoload
(defun chatgpt-shell-transient ()
  "Invoke the main transient menu for chatgpt-shell."
  (interactive)
  (chatgpt-shell-transient--popup))

(provide 'chatgpt-shell-transient)

;;; chatgpt-shell-transient.el ends here
