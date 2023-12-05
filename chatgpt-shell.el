;;; chatgpt-shell.el --- ChatGPT shell + buffer insert commands  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.95.1
;; Package-Requires: ((emacs "27.1") (shell-maker "0.43.1"))

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

;; `chatgpt-shell' is a comint-based ChatGPT shell for Emacs.
;;
;; You must set `chatgpt-shell-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.
;;
;; Note: This is young package still.  Please report issues or send
;; patches to https://github.com/xenodium/chatgpt-shell
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(require 'cl-lib)
(require 'esh-mode)
(require 'eshell)
(require 'find-func)
(require 'ielm)
(require 'shell-maker)

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-additional-curl-options nil
  "Additional options for `curl' command."
  :type '(repeat (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-auth-header
  (lambda ()
    (format "Authorization: Bearer %s" (chatgpt-shell-openai-key)))
  "Function to generate the request's `Authorization' header string."
  :type '(function :tag "Function")
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-request-timeout 600
  "How long to wait for a request to time out in seconds."
  :type 'integer
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-default-prompts
  '("Write a unit test for the following code:"
    "Refactor the following code so that "
    "Summarize the output of the following command:"
    "What's wrong with this command?"
    "Explain what the following code does:")
  "List of default prompts to choose from."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-describe-code
  "What does the following code do?"
  "Prompt header of `describe-code`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-write-git-commit
  "Please help me write a git commit message for the following commit:"
  "Prompt header of `git-commit`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-refactor-code
  "Please help me refactor the following code.
   Please reply with the refactoring explanation in English, refactored code, and diff between two versions.
   Please ignore the comments and strings in the code during the refactoring.
   If the code remains unchanged after refactoring, please say 'No need to refactor'."
  "Prompt header of `refactor-code`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-generate-unit-test
  "Please help me generate unit-test following function:"
  "Prompt header of `generate-unit-test`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-proofread-region
  "Please help me proofread the following text with English:"
  "Promt header of `proofread-region`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-whats-wrong-with-last-command
  "What's wrong with this command?"
  "Prompt header of `whats-wrong-with-last-command`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-eshell-summarize-last-command-output
  "Summarize the output of the following command:"
  "Prompt header of `eshell-summarize-last-command-output`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-query-response-style 'other-buffer
  "Determines the prompt style when invoking from other buffers.

`'inline' inserts responses into current buffer.
`'other-buffer' inserts responses into a transient buffer.
`'shell' inserts responses and focuses the shell

Note: in all cases responses are written to the shell to keep context."
  :type '(choice (const :tag "Inline" inline)
                 (const :tag "Other Buffer" other-buffer)
                 (const :tag "Shell" shell))
  :group 'chatgpt)

(defcustom chatgpt-shell-after-command-functions nil
  "Abnormal hook (i.e. with parameters) invoked after each command.

This is useful if you'd like to automatically handle or suggest things
post execution.

For example:

\(add-hook `chatgpt-shell-after-command-functions'
   (lambda (command output)
     (message \"Command: %s\" command)
     (message \"Output: %s\" output)))"
  :type 'hook
  :group 'shell-maker)

(defvaralias 'chatgpt-shell-display-function 'shell-maker-display-function)

(defvaralias 'chatgpt-shell-read-string-function 'shell-maker-read-string-function)

(defvaralias 'chatgpt-shell-logging 'shell-maker-logging)

(defvaralias 'chatgpt-shell-history-path 'shell-maker-history-path)

(defalias 'chatgpt-shell-save-session-transcript #'shell-maker-save-session-transcript)

(defvar chatgpt-shell--prompt-history nil)

(defcustom chatgpt-shell-language-mapping '(("elisp" . "emacs-lisp")
                                            ("objective-c" . "objc")
                                            ("objectivec" . "objc")
                                            ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '(repeat (cons string string))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-babel-headers '(("dot" . ((:file . "<temp-file>.png")))
                                         ("plantuml" . ((:file . "<temp-file>.png")))
                                         ("ditaa" . ((:file . "<temp-file>.png")))
                                         ("objc" . ((:results . "output")))
                                         ("python" . ((:python . "python3")))
                                         ("swiftui" . ((:results . "file")))
                                         ("c++" . ((:results . "raw")))
                                         ("c" . ((:results . "raw"))))
  "Additional headers to make babel blocks work.

Please submit contributions so more things work out of the box."
  :type '(repeat (cons string string))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-source-block-actions
  nil
  "Block actions for known languages.

Can be used compile or run source block at point."
  :type '(list (cons string
                     (list (cons 'primary-action-confirmation string)
                           (cons 'primary-action function))))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-versions
  '("gpt-4-1106-preview"
    "gpt-4-0613"
    "gpt-4"
    "gpt-3.5-turbo-16k-0613"
    "gpt-3.5-turbo-16k"
    "gpt-3.5-turbo-0613"
    "gpt-3.5-turbo")
  "The list of ChatGPT OpenAI models to swap from.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version 0
  "The active ChatGPT OpenAI model index.

See `chatgpt-shell-model-versions' for available model versions.

Swap using `chatgpt-shell-swap-model-version'.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type '(choice (string :tag "String")
                 (integer :tag "Integer")
                 (const :tag "Nil" nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-temperature nil
  "What sampling temperature to use, between 0 and 2, or nil.

Higher values like 0.8 will make the output more random, while
lower values like 0.2 will make it more focused and
deterministic.  Value of nil will not pass this configuration to
the model.

See
https://platform.openai.com/docs/api-reference/completions\
/create#completions/create-temperature
for details."
  :type '(choice (float :tag "Float")
                 (const :tag "Nil" nil))
  :group 'chatgpt-shell)

(defun chatgpt-shell--append-system-info (text)
  "Append system info to TEXT."
  (cond ((eq system-type 'darwin)
         (concat text
                 "\n# System info\n"
                 "\n## OS details\n"
                 (string-trim (shell-command-to-string "sw_vers"))
                 "\n## Editor\n"
                 (emacs-version)))
        ((or (eq system-type 'gnu/linux)
             (eq system-type 'gnu/kfreebsd))
         (concat text
                 "\n# System info\n"
                 "\n## OS details\n"
                 (string-trim (shell-command-to-string "uname -a"))
                 "\n## Editor\n"
                 (emacs-version)))
        ((eq system-type 'windows-nt)
         (concat text
                 "\n# System info\n"
                 "\n## OS details\n"
                 (string-trim (shell-command-to-string "ver"))
                 "\n## Editor\n"
                 (emacs-version)))
        (t
         (concat text
                 "\n# System info\n"
                 "\n## OS details\n"
                 (format "%s" system-type)
                 "\n## Editor\n"
                 (emacs-version)))))

(defcustom chatgpt-shell-system-prompts
  `(("General" . "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.")
    ;; Based on https://github.com/benjamin-asdf/dotfiles/blob/8fd18ff6bd2a1ed2379e53e26282f01dcc397e44/mememacs/.emacs-mememacs.d/init.el#L768
    ("Programming" . ,(chatgpt-shell--append-system-info
                       "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets."))
    ("Positive Programming" . ,(chatgpt-shell--append-system-info
                       "Your goal is to help the user become an amazing computer programmer.
                        You are positive and encouraging.
                        You love see them learn.
                        You do not repeat obvious things, including their query.
                        You are as concise in responses. You always guide the user go one level deeper and help them see patterns.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets.")))

  "List of system prompts to choose from.

If prompt is a cons, its car will be used as a title to display.

For example:

\(\"Translating\" . \"You are a helpful English to Spanish assistant.\")\"
\(\"Programming\" . \"The user is a programmer with very limited time...\")"
  :type '(repeat (cons string string))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-system-prompt 1 ;; Concise
  "The system prompt `chatgpt-shell-system-prompts' index.

Or nil if none."
  :type '(choice (string :tag "String")
                 (integer :tag "Integer")
                 (const :tag "No Prompt" nil))
  :group 'chatgpt-shell)

(defun chatgpt-shell-model-version ()
  "Return active model version."
  (cond ((stringp chatgpt-shell-model-version)
         chatgpt-shell-model-version)
        ((integerp chatgpt-shell-model-version)
         (nth chatgpt-shell-model-version
              chatgpt-shell-model-versions))
        (t
         nil)))

(defun chatgpt-shell-system-prompt ()
  "Return active system prompt."
  (cond ((stringp chatgpt-shell-system-prompt)
         chatgpt-shell-system-prompt)
        ((integerp chatgpt-shell-system-prompt)
         (let ((prompt (nth chatgpt-shell-system-prompt
                            chatgpt-shell-system-prompts)))
           (if (consp prompt)
               (cdr prompt)
             prompt)))
        (t
         nil)))

(defun chatgpt-shell-duplicate-map-keys (map)
  "Return duplicate keys in MAP."
  (let ((keys (map-keys map))
        (seen '())
        (duplicates '()))
    (dolist (key keys)
      (if (member key seen)
          (push key duplicates)
        (push key seen)))
    duplicates))

(defun chatgpt-shell-swap-system-prompt ()
  "Swap system prompt from `chatgpt-shell-system-prompts'."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (when-let ((duplicates (chatgpt-shell-duplicate-map-keys chatgpt-shell-system-prompts)))
    (user-error "Duplicate prompt names found %s. Please remove." duplicates))
  (let* ((choices (append (list "None")
                          (map-keys chatgpt-shell-system-prompts)))
         (choice (completing-read "System prompt: " choices))
         (choice-pos (seq-position choices choice)))
    (if (or (string-equal choice "None")
            (string-empty-p (string-trim choice))
            (not choice-pos))
        (setq-local chatgpt-shell-system-prompt nil)
      (setq-local chatgpt-shell-system-prompt
                  ;; -1 to disregard None
                  (1- (seq-position choices choice)))))
  (chatgpt-shell--update-prompt t)
  (chatgpt-shell-interrupt nil))

(defun chatgpt-shell-load-awesome-prompts ()
  "Load `chatgpt-shell-system-prompts' from awesome-chatgpt-prompts.

Downloaded from https://github.com/f/awesome-chatgpt-prompts."
  (interactive)
  (unless (fboundp 'pcsv-parse-file)
    (user-error "Please install pcsv"))
  (require 'pcsv)
  (let ((csv-path (concat (temporary-file-directory) "awesome-chatgpt-prompts.csv")))
    (url-copy-file "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv"
                   csv-path t)
    (setq chatgpt-shell-system-prompts
         (map-merge 'list
                    chatgpt-shell-system-prompts
                    ;; Based on Daniel Gomez's parsing code from
                    ;; https://github.com/xenodium/chatgpt-shell/issues/104
                    (seq-sort (lambda (rhs lhs)
                                (string-lessp (car rhs)
                                              (car lhs)))
                              (cdr
                               (mapcar
                                (lambda (row)
                                  (cons (car row)
                                        (cadr row)))
                                (pcsv-parse-file csv-path))))))
    (message "Loaded awesome-chatgpt-prompts")
    (setq chatgpt-shell-system-prompt nil)
    (chatgpt-shell--update-prompt t)
    (chatgpt-shell-interrupt nil)
    (chatgpt-shell-swap-system-prompt)))

(defun chatgpt-shell-swap-model-version ()
  "Swap model version from `chatgpt-shell-model-versions'."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (setq-local chatgpt-shell-model-version
              (completing-read "Model version: "
                               (if (> (length chatgpt-shell-model-versions) 1)
                                   (seq-remove
                                    (lambda (item)
                                      (string-equal item (chatgpt-shell-model-version)))
                                    chatgpt-shell-model-versions)
                                 chatgpt-shell-model-versions) nil t))
  (chatgpt-shell--update-prompt t)
  (chatgpt-shell-interrupt nil))

(defcustom chatgpt-shell-streaming t
  "Whether or not to stream ChatGPT responses (show chunks as they arrive)."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-highlight-blocks t
  "Whether or not to highlight source blocks."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-transmitted-context-length
  #'chatgpt-shell--approximate-context-length
  "Controls the amount of context provided to chatGPT.

This context needs to be transmitted to the API on every request.
ChatGPT reads the provided context on every request, which will
consume more and more prompt tokens as your conversation grows.
Models do have a maximum token limit, however.

A value of nil will send full chat history (the full contents of
the comint buffer), to ChatGPT.

A value of 0 will not provide any context.  This is the cheapest
option, but ChatGPT can't look back on your conversation.

A value of 1 will send only the latest prompt-completion pair as
context.

A Value > 1 will send that amount of prompt-completion pairs to
ChatGPT.

A function `(lambda (tokens-per-message tokens-per-name messages))'
returning length.  Can use custom logic to enable a shifting context
window."
  :type '(choice (integer :tag "Integer")
                 (const :tag "Not set" nil)
                 (function :tag "Function"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-api-url-base "https://api.openai.com"
  "OpenAI API's base URL.

`chatgpt-shell--api-url' =
   `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'

If you use ChatGPT through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-api-url-path "/v1/chat/completions"
  "OpenAI API's URL path.

`chatgpt-shell--api-url' =
   `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'"
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-welcome-function #'shell-maker-welcome-message
  "Function returning welcome message or nil for no message.

See `shell-maker-welcome-message' as an example."
  :type 'function
  :group 'chatgpt-shell)

(defvar chatgpt-shell--config
  (make-shell-maker-config
   :name "ChatGPT"
   :validate-command
   (lambda (_command)
     (unless chatgpt-shell-openai-key
       "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")"))
   :execute-command
   (lambda (_command history callback error-callback)
     (shell-maker-async-shell-command
      (chatgpt-shell--make-curl-request-command-list
       (chatgpt-shell--make-payload history))
      chatgpt-shell-streaming
      #'chatgpt-shell--extract-chatgpt-response
      callback
      error-callback))
   :on-command-finished
   (lambda (command output)
     (chatgpt-shell--put-source-block-overlays)
     (run-hook-with-args 'chatgpt-shell-after-command-functions
                         command output))
   :redact-log-output
   (lambda (output)
     (if (chatgpt-shell-openai-key)
         (replace-regexp-in-string (regexp-quote (chatgpt-shell-openai-key))
                                   "SK-REDACTED-OPENAI-KEY"
                                   output)
       output))))

(defalias 'chatgpt-shell-clear-buffer #'comint-clear-buffer)

(defalias 'chatgpt-shell-explain-code #'chatgpt-shell-describe-code)

;; Aliasing enables editing as text in babel.
(defalias 'chatgpt-shell-mode #'text-mode)

(shell-maker-define-major-mode chatgpt-shell--config)

;;;###autoload
(defun chatgpt-shell (&optional new-session)
  "Start a ChatGPT shell interactive command.

With NEW-SESSION, start a new session."
  (interactive "P")
  (chatgpt-shell-start nil new-session))

(defun chatgpt-shell-start (&optional no-focus new-session)
  "Start a ChatGPT shell programmatically.

Set NO-FOCUS to start in background.

Set NEW-SESSION to start a separate new session."
  (let* ((chatgpt-shell--config
          (let ((config (copy-sequence chatgpt-shell--config)))
            (setf (shell-maker-config-prompt config)
                  (car (chatgpt-shell--prompt-pair)))
            (setf (shell-maker-config-prompt-regexp config)
                  (cdr (chatgpt-shell--prompt-pair)))
            config))
         (shell-buffer
          (shell-maker-start chatgpt-shell--config
                             no-focus
                             chatgpt-shell-welcome-function
                             new-session
                             (if (chatgpt-shell--primary-buffer)
                                 (buffer-name (chatgpt-shell--primary-buffer))
                               (chatgpt-shell--make-buffer-name)))))
    (unless (chatgpt-shell--primary-buffer)
      (chatgpt-shell--set-primary-buffer shell-buffer))
    (let ((version chatgpt-shell-model-version)
          (system-prompt chatgpt-shell-system-prompt))
      (with-current-buffer shell-buffer
        (setq-local chatgpt-shell-model-version version)
        (setq-local chatgpt-shell-system-prompt system-prompt)
        (chatgpt-shell--update-prompt t)
        (chatgpt-shell--add-menus)))
    ;; Disabling advice for now. It gets in the way.
    ;; (advice-add 'keyboard-quit :around #'chatgpt-shell--adviced:keyboard-quit)
    (define-key chatgpt-shell-mode-map (kbd "C-M-h")
      #'chatgpt-shell-mark-at-point-dwim)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-c")
      #'chatgpt-shell-ctrl-c-ctrl-c)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-v")
      #'chatgpt-shell-swap-model-version)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-s")
      #'chatgpt-shell-swap-system-prompt)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-p")
      #'chatgpt-shell-previous-item)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-n")
      #'chatgpt-shell-next-item)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-e")
      #'chatgpt-shell-prompt-compose)
    shell-buffer))

(defun chatgpt-shell--shrink-model-version (model-version)
  "Shrink MODEL-VERSION.  gpt-3.5-turbo -> 3.5t."
  (replace-regexp-in-string
   "-turbo" "t"
   (string-remove-prefix
    "gpt-" (string-trim model-version))))

(defun chatgpt-shell--shrink-system-prompt (prompt)
  "Shrink PROMPT."
  (if (consp prompt)
      (car prompt)
    (if (> (length (string-trim prompt)) 6)
        (format "%s..."
                (substring (string-trim prompt) 0 15))
      (string-trim prompt))))

(defun chatgpt-shell--shell-info ()
  "Generate shell info for display."
  (concat
   (chatgpt-shell--shrink-model-version
    (chatgpt-shell-model-version))
   (cond ((and (integerp chatgpt-shell-system-prompt)
               (nth chatgpt-shell-system-prompt
                    chatgpt-shell-system-prompts))
          (concat "/" (chatgpt-shell--shrink-system-prompt (nth chatgpt-shell-system-prompt
                                                                chatgpt-shell-system-prompts))))
         ((stringp chatgpt-shell-system-prompt)
          (concat "/" (chatgpt-shell--shrink-system-prompt chatgpt-shell-system-prompt)))
         (t
          ""))))

(defun chatgpt-shell--prompt-pair ()
  "Return a pair with prompt and prompt-regexp."
  (cons
   (format "ChatGPT(%s)> " (chatgpt-shell--shell-info))
   (rx (seq bol "ChatGPT" (one-or-more (not (any "\n"))) ">" (or space "\n")))))

(defun chatgpt-shell--shell-buffers ()
  "Return a list of all shell buffers."
  (seq-filter
   (lambda (buffer)
     (eq (buffer-local-value 'major-mode buffer)
         'chatgpt-shell-mode))
   (buffer-list)))

(defun chatgpt-shell-set-as-primary-shell ()
  "Set as primary shell when there are multiple sessions."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (chatgpt-shell--set-primary-buffer (current-buffer)))

(defun chatgpt-shell--set-primary-buffer (primary-shell-buffer)
  "Set PRIMARY-SHELL-BUFFER as primary buffer."
  (mapc (lambda (shell-buffer)
          (with-current-buffer shell-buffer
            (setq chatgpt-shell--is-primary-p nil)))
        (chatgpt-shell--shell-buffers))
  (with-current-buffer primary-shell-buffer
    (setq chatgpt-shell--is-primary-p t)))

(defun chatgpt-shell--primary-buffer ()
  "Return the primary shell buffer (used for sending a prompt to in the background)."
  (let ((primary-shell-buffer (seq-find
                               (lambda (shell-buffer)
                                 (with-current-buffer shell-buffer
                                   chatgpt-shell--is-primary-p))
                               (chatgpt-shell--shell-buffers))))
    primary-shell-buffer))

(defun chatgpt-shell--make-buffer-name ()
  "Generate a buffer name using current shell config info."
  (format "%s %s"
          (shell-maker-buffer-default-name
           (shell-maker-config-name chatgpt-shell--config))
          (chatgpt-shell--shell-info)))

(defun chatgpt-shell--add-menus ()
  "Add ChatGPT shell menu items."
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (when-let ((duplicates (chatgpt-shell-duplicate-map-keys chatgpt-shell-system-prompts)))
    (user-error "Duplicate prompt names found %s. Please remove." duplicates))
  (easy-menu-define chatgpt-shell-system-prompts-menu (current-local-map) "ChatGPT"
    `("ChatGPT"
      ("Versions"
       ,@(mapcar (lambda (version)
                   `[,version
                     (lambda ()
                       (interactive)
                       (setq-local chatgpt-shell-model-version
                                   (seq-position chatgpt-shell-model-versions ,version))
                       (chatgpt-shell--update-prompt t)
                       (chatgpt-shell-interrupt nil))])
                 chatgpt-shell-model-versions))
      ("Prompts"
       ,@(mapcar (lambda (prompt)
                   `[,(car prompt)
                     (lambda ()
                       (interactive)
                       (setq-local chatgpt-shell-system-prompt
                                   (seq-position (map-keys chatgpt-shell-system-prompts) ,(car prompt)))
                       (chatgpt-shell--update-prompt t)
                       (chatgpt-shell-interrupt nil))])
                 chatgpt-shell-system-prompts))))
  (easy-menu-add chatgpt-shell-system-prompts-menu))

(defun chatgpt-shell--update-prompt (rename-buffer)
  "Update prompt and prompt regexp from `chatgpt-shell-model-versions'.

Set RENAME-BUFFER to also rename the buffer accordingly."
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (shell-maker-set-prompt
   (car (chatgpt-shell--prompt-pair))
   (cdr (chatgpt-shell--prompt-pair)))
  (when rename-buffer
    (shell-maker-set-buffer-name
     (current-buffer)
     (chatgpt-shell--make-buffer-name))))

(defun chatgpt-shell--adviced:keyboard-quit (orig-fun &rest args)
  "Advice around `keyboard-quit' interrupting active shell.

Applies ORIG-FUN and ARGS."
  (chatgpt-shell-interrupt nil)
  (apply orig-fun args))

(defun chatgpt-shell-interrupt (ignore-item)
  "Interrupt `chatgpt-shell' from any buffer.

With prefix IGNORE-ITEM, do not mark as failed."
  (interactive "P")
  (with-current-buffer
      (cond
       ((eq major-mode 'chatgpt-shell-mode)
        (current-buffer))
       (t
        (shell-maker-buffer-name chatgpt-shell--config)))
    (shell-maker-interrupt ignore-item)))

(defun chatgpt-shell-ctrl-c-ctrl-c (ignore-item)
  "If point in source block, execute it.  Otherwise interrupt.

With prefix IGNORE-ITEM, do not use interrupted item in context."
  (interactive "P")
  (cond ((chatgpt-shell-block-action-at-point)
         (chatgpt-shell-execute-block-action-at-point))
        ((chatgpt-shell-markdown-block-at-point)
         (user-error "No action available"))
        ((and shell-maker--busy
              (eq (line-number-at-pos (point-max))
                  (line-number-at-pos (point))))
         (shell-maker-interrupt ignore-item))
        (t
         (shell-maker-interrupt ignore-item))))

(defun chatgpt-shell-mark-at-point-dwim ()
  "Mark source block if at point.  Mark all output otherwise."
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (progn
        (set-mark (map-elt block 'end))
        (goto-char (map-elt block 'start)))
    (shell-maker-mark-output)))

(defun chatgpt-shell-markdown-block-language (text)
  "Get the language label of a Markdown TEXT code block."
  (when (string-match (rx bol "```" (0+ space) (group (+ (not (any "\n"))))) text)
    (match-string 1 text)))

(defun chatgpt-shell-markdown-block-at-point ()
  "Markdown start/end cons if point at block.  nil otherwise."
  (save-excursion
    (save-restriction
      (when (eq major-mode 'chatgpt-shell-mode)
        (shell-maker-narrow-to-prompt))
      (let* ((language)
             (language-start)
             (language-end)
             (start (save-excursion
                      (when (re-search-backward "^```" nil t)
                        (setq language (chatgpt-shell-markdown-block-language (thing-at-point 'line)))
                        (save-excursion
                          (forward-char 3) ; ```
                          (setq language-start (point))
                          (end-of-line)
                          (setq language-end (point)))
                        language-end)))
             (end (save-excursion
                    (when (re-search-forward "^```" nil t)
                      (forward-line 0)
                      (point)))))
        (when (and start end
                   (> (point) start)
                   (< (point) end))
          (list (cons 'language language)
                (cons 'language-start language-start)
                (cons 'language-end language-end)
                (cons 'start start)
                (cons 'end end)))))))

(defun chatgpt-shell--markdown-headers (&optional avoid-ranges)
  "Extract markdown headers with AVOID-RANGES."
  (let ((headers '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (group (one-or-more "#"))
                  (one-or-more space)
                  (group (one-or-more (not (any "\n")))) eol)
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'level (cons (match-beginning 1) (match-end 1))
              'title (cons (match-beginning 2) (match-end 2)))
             headers)))))
    (nreverse headers)))

(defun chatgpt-shell--markdown-links (&optional avoid-ranges)
  "Extract markdown links with AVOID-RANGES."
  (let ((links '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (seq "["
                       (group (one-or-more (not (any "]"))))
                       "]"
                       "("
                       (group (one-or-more (not (any ")"))))
                       ")"))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'title (cons (match-beginning 1) (match-end 1))
              'url (cons (match-beginning 2) (match-end 2)))
             links)))))
    (nreverse links)))

(defun chatgpt-shell--markdown-bolds (&optional avoid-ranges)
  "Extract markdown bolds with AVOID-RANGES."
  (let ((bolds '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or (group "**" (group (one-or-more (not (any "\n*")))) "**")
                      (group "__" (group (one-or-more (not (any "\n_")))) "__")))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'text (cons (or (match-beginning 2)
                              (match-beginning 4))
                          (or (match-end 2)
                              (match-end 4))))
             bolds)))))
    (nreverse bolds)))

(defun chatgpt-shell--markdown-strikethroughs (&optional avoid-ranges)
  "Extract markdown strikethroughs with AVOID-RANGES."
  (let ((strikethroughs '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "~~" (group (one-or-more (not (any "\n~")))) "~~")
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start begin
              'end end
              'text (cons (match-beginning 1)
                          (match-end 1)))
             strikethroughs)))))
    (nreverse strikethroughs)))

(defun chatgpt-shell--markdown-italics (&optional avoid-ranges)
  "Extract markdown italics with AVOID-RANGES."
  (let ((italics '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (or (group (or bol (one-or-more (any "\n \t")))
                             (group "*")
                             (group (one-or-more (not (any "\n*")))) "*")
                      (group (or bol (one-or-more (any "\n \t")))
                             (group "_")
                             (group (one-or-more (not (any "\n_")))) "_")))
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'start (or (match-beginning 2)
                         (match-beginning 5))
              'end end
              'text (cons (or (match-beginning 3)
                              (match-beginning 6))
                          (or (match-end 3)
                              (match-end 6))))
             italics)))))
    (nreverse italics)))

(defun chatgpt-shell--markdown-inline-codes (&optional avoid-ranges)
  "Get a list of all inline markdown code in buffer with AVOID-RANGES."
  (let ((codes '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "`\\([^`\n]+\\)`"
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (unless (seq-find (lambda (avoided)
                              (and (>= begin (car avoided))
                                   (<= end (cdr avoided))))
                            avoid-ranges)
            (push
             (list
              'body (cons (match-beginning 1) (match-end 1))) codes)))))
    (nreverse codes)))

(defvar chatgpt-shell--source-block-regexp
  (rx  bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
       (group (zero-or-more (or alphanumeric "-" "+"))) ;; language
       (zero-or-more whitespace)
       (one-or-more "\n")
       (group (*? anychar)) ;; body
       (one-or-more "\n")
       (group "```") (or "\n" eol)))

(defvar-local chatgpt-shell--is-primary-p nil)

(defun chatgpt-shell-next-source-block ()
  "Move point to previous source block."
  (interactive)
  (when-let
      ((next-block
        (save-excursion
          (when-let ((current (chatgpt-shell-markdown-block-at-point)))
            (goto-char (map-elt current 'end))
            (end-of-line))
          (when (re-search-forward chatgpt-shell--source-block-regexp nil t)
            (chatgpt-shell--match-source-block)))))
    (goto-char (car (map-elt next-block 'body)))))

(defun chatgpt-shell-previous-item ()
  "Go to previous item.

Could be a prompt or a source block."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (when (comint-next-prompt (- 1))
                        (point))))
        (block-pos (save-excursion
                     (when (chatgpt-shell-previous-source-block)
                       (point)))))
    (cond ((and block-pos prompt-pos)
           (goto-char (max prompt-pos
                           block-pos)))
          (block-pos
           (goto-char block-pos))
          (prompt-pos
           (goto-char prompt-pos)))))

(defun chatgpt-shell-next-item ()
  "Go to next item.

Could be a prompt or a source block."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (when (comint-next-prompt 1)
                        (point))))
        (block-pos (save-excursion
                     (when (chatgpt-shell-next-source-block)
                       (point)))))
    (cond ((and block-pos prompt-pos)
           (goto-char (min prompt-pos
                           block-pos)))
          (block-pos
           (goto-char block-pos))
          (prompt-pos
           (goto-char prompt-pos)))))

(defun chatgpt-shell-previous-source-block ()
  "Move point to previous source block."
  (interactive)
  (when-let
      ((previous-block
        (save-excursion
          (when-let ((current (chatgpt-shell-markdown-block-at-point)))
            (goto-char (map-elt current 'start))
            (forward-line 0))
          (when (re-search-backward chatgpt-shell--source-block-regexp nil t)
            (chatgpt-shell--match-source-block)))))
    (goto-char (car (map-elt previous-block 'body)))))

(defun chatgpt-shell--match-source-block ()
  "Return a matched source block by the previous search/regexp operation."
  (list
   'start (cons (match-beginning 1)
                (match-end 1))
   'end (cons (match-beginning 4)
              (match-end 4))
   'language (when (and (match-beginning 2)
                        (match-end 2))
               (cons (match-beginning 2)
                     (match-end 2)))
   'body (cons (match-beginning 3) (match-end 3))))

(defun chatgpt-shell--source-blocks ()
  "Get a list of all source blocks in buffer."
  (let ((markdown-blocks '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              chatgpt-shell--source-block-regexp
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push (chatgpt-shell--match-source-block)
                markdown-blocks))))
    (nreverse markdown-blocks)))

(defun chatgpt-shell--minibuffer-prompt ()
  "Construct a prompt for the minibuffer."
  (if (chatgpt-shell--primary-buffer)
      (concat (buffer-name (chatgpt-shell--primary-buffer)) "> ")
    (shell-maker-prompt
     chatgpt-shell--config)))

(defun chatgpt-shell-prompt ()
  "Make a ChatGPT request from the minibuffer.

If region is active, append to prompt."
  (interactive)
  (unless chatgpt-shell--prompt-history
    (setq chatgpt-shell--prompt-history
          chatgpt-shell-default-prompts))
  (let ((overlay-blocks (derived-mode-p 'prog-mode))
        (prompt (funcall shell-maker-read-string-function
                         (concat
                          (if (region-active-p)
                              "[appending region] "
                            "")
                          (chatgpt-shell--minibuffer-prompt))
                         'chatgpt-shell--prompt-history)))
    (when (string-empty-p (string-trim prompt))
      (user-error "Nothing to send"))
    (when (region-active-p)
      (setq prompt (concat prompt "\n\n"
                           (if overlay-blocks
                               (format "``` %s\n"
                                       (string-remove-suffix "-mode" (format "%s" major-mode)))
                             "")
                           (buffer-substring (region-beginning) (region-end))
                           (if overlay-blocks
                               "\n```"
                             ""))))
    (chatgpt-shell-send-to-buffer prompt nil)))

(defun chatgpt-shell-prompt-compose (prefix)
  "Compose and send prompt (kbd \"C-c C-c\") from a dedicated buffer.

With PREFIX, clear existing history. Appends any active region."
  (interactive "P")
  (let* ((exit-on-submit (eq major-mode 'chatgpt-shell-mode))
         (buffer-name (concat (chatgpt-shell--minibuffer-prompt)
                              "compose"))
         (buffer (get-buffer-create buffer-name))
         (map (make-sparse-keymap))
         (region (when-let ((_ (region-active-p))
                            (region (buffer-substring (region-beginning)
                                                      (region-end))))
                   (deactivate-mark)
                   region))
         (instructions (concat "Type "
                               (propertize "C-c C-c" 'face 'help-key-binding)
                               " to send prompt. "
                               (propertize "C-c C-k" 'face 'help-key-binding)
                               " to cancel and exit. "))
         (prompt))
    (add-to-list 'display-buffer-alist
                 (cons buffer
                       '((display-buffer-below-selected)
                         (split-window-sensibly))))
    (with-current-buffer buffer
      (visual-line-mode +1)
      (when view-mode
        (view-mode -1))
      (erase-buffer)
      (when region
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n\n" region))))
      (when prefix
        (let ((chatgpt-shell-prompt-query-response-style 'inline))
          (chatgpt-shell-send-to-buffer "clear")))
      (make-local-variable 'view-mode-map)
      ;; TODO: Find a better alternative to prevent clash.
      ;; Disable "n"/"p" for region-bindings-mode-map, so it doesn't
      ;; clash with "n"/"p" selection binding.
      (when (boundp 'region-bindings-mode-disable-predicates)
        (add-to-list 'region-bindings-mode-disable-predicates
                     (lambda () buffer-read-only)))
      (define-key view-mode-map (kbd "g")
                  (lambda ()
                    (interactive)
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
                      (chatgpt-shell-send-to-buffer prompt))))
      (define-key view-mode-map (kbd "n")
                  (lambda ()
                    (interactive)
                    (call-interactively #'chatgpt-shell-next-source-block)
                    (when-let ((block (chatgpt-shell-markdown-block-at-point)))
                      (set-mark (map-elt block 'end))
                      (goto-char (map-elt block 'start)))))
      (define-key view-mode-map (kbd "p")
                  (lambda ()
                    (interactive)
                    (call-interactively #'chatgpt-shell-previous-source-block)
                    (when-let ((block (chatgpt-shell-markdown-block-at-point)))
                      (set-mark (map-elt block 'end))
                      (goto-char (map-elt block 'start)))))
      (define-key view-mode-map (kbd "r") ;; reply
                  (lambda ()
                    (interactive)
                    (with-current-buffer (chatgpt-shell--primary-buffer)
                      (when shell-maker--busy
                        (user-error "Busy, please wait")))
                    (view-mode -1)
                    (erase-buffer)))
      (define-key view-mode-map (kbd "e") ;; show entire snippet
                  (lambda ()
                    (interactive)
                    (with-current-buffer (chatgpt-shell--primary-buffer)
                      (when shell-maker--busy
                        (user-error "Busy, please wait")))
                    (let ((prompt "show entire snippet")
                          (inhibit-read-only t)
                          (chatgpt-shell-prompt-query-response-style 'inline))
                      (erase-buffer)
                      (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
                      (chatgpt-shell-send-to-buffer prompt))))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (quit-window t (get-buffer-window buffer))
                       (message "exit")))
      (defvar-local chatgpt-shell--ring-index nil)
      (setq chatgpt-shell--ring-index nil)
      (local-set-key (kbd "M-p") (lambda ()
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
                                         (insert (seq-elt ring chatgpt-shell--ring-index)))))))
      (local-set-key (kbd "M-n") (lambda ()
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
                                         (insert (seq-elt ring chatgpt-shell--ring-index)))))))
      (local-set-key (kbd "C-M-h") (lambda ()
                                     (interactive)
                                     (when-let ((block (chatgpt-shell-markdown-block-at-point)))
                                       (set-mark (map-elt block 'end))
                                       (goto-char (map-elt block 'start)))))
      (local-set-key (kbd "C-c C-n") #'chatgpt-shell-next-source-block)
      (local-set-key (kbd "C-c C-p") #'chatgpt-shell-previous-source-block)
      (local-set-key (kbd "M-r")
                     (lambda ()
                       (interactive)
                       (let ((candidate (with-current-buffer (chatgpt-shell--primary-buffer)
                                          (completing-read
                                           "History: "
                                           (delete-dups
                                            (seq-filter
                                             (lambda (item)
                                               (not (string-empty-p item)))
                                             (ring-elements comint-input-ring))) nil t))))
                         (insert candidate))))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (with-current-buffer (chatgpt-shell--primary-buffer)
                         (when shell-maker--busy
                           (unless (y-or-n-p "Abort?")
                             (cl-return))
                           (shell-maker-interrupt t)
                           (with-current-buffer buffer
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
                         (setq view-exit-action 'kill-buffer)
                         (when (string-equal prompt "clear")
                           (view-mode -1)
                           (erase-buffer))
                         (if exit-on-submit
                             (let ((view-exit-action nil)
                                   (chatgpt-shell-prompt-query-response-style 'shell))
                               (quit-window t (get-buffer-window buffer))
                               (chatgpt-shell-send-to-buffer prompt))
                           (let ((chatgpt-shell-prompt-query-response-style 'inline))
                             (chatgpt-shell-send-to-buffer prompt))))))
      (message instructions))
    (pop-to-buffer buffer-name)))

(defun chatgpt-shell-prompt-appending-kill-ring ()
  "Make a ChatGPT request from the minibuffer appending kill ring."
  (interactive)
  (unless chatgpt-shell--prompt-history
    (setq chatgpt-shell--prompt-history
          chatgpt-shell-default-prompts))
  (let ((prompt (funcall shell-maker-read-string-function
                         (concat
                          "[appending kill ring] "
                          (chatgpt-shell--minibuffer-prompt))
                         'chatgpt-shell--prompt-history)))
    (chatgpt-shell-send-to-buffer
     (concat prompt "\n\n"
             (current-kill 0)) nil)))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((overlay-blocks (derived-mode-p 'prog-mode)))
    (chatgpt-shell-send-to-buffer
     (concat chatgpt-shell-prompt-header-describe-code
             "\n\n"
             (if overlay-blocks
                 (format "``` %s\n"
                         (string-remove-suffix "-mode" (format "%s" major-mode)))
               "")
             (buffer-substring (region-beginning) (region-end))
             (if overlay-blocks
                 "\n```"
               "")) nil)
    (when overlay-blocks
      (with-current-buffer
          (chatgpt-shell--primary-buffer)
        (chatgpt-shell--put-source-block-overlays)))))

(defun chatgpt-shell-send-region-with-header (header)
  "Send text with HEADER from region using ChatGPT."
  (unless (region-active-p)
    (user-error "No region active"))
  (chatgpt-shell-send-to-buffer
   (concat header
           "\n\n"
           (buffer-substring (region-beginning) (region-end)))
   nil))

(defun chatgpt-shell-refactor-code ()
  "Refactor code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-refactor-code))

(defun chatgpt-shell-write-git-commit ()
  "Write commit from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-write-git-commit))

(defun chatgpt-shell-generate-unit-test ()
  "Generate unit-test for the code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-generate-unit-test))

(defun chatgpt-shell-proofread-region ()
  "Proofread English from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-proofread-region))

(defun chatgpt-shell-eshell-whats-wrong-with-last-command ()
  "Ask ChatGPT what's wrong with the last eshell command."
  (interactive)
  (let ((chatgpt-shell-prompt-query-response-style 'other-buffer))
    (chatgpt-shell-send-to-buffer
     (concat chatgpt-shell-prompt-header-whats-wrong-with-last-command
             "\n\n"
             (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
             "\n\n"
             (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))))

(defun chatgpt-shell-eshell-summarize-last-command-output ()
  "Ask ChatGPT to summarize the last command output."
  (interactive)
  (let ((chatgpt-shell-prompt-query-response-style 'other-buffer))
    (chatgpt-shell-send-to-buffer
     (concat chatgpt-shell-prompt-header-eshell-summarize-last-command-output
             "\n\n"
             (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)
             "\n\n"
             (buffer-substring-no-properties (eshell-beginning-of-output) (eshell-end-of-output))))))

(defun chatgpt-shell-send-region (review)
  "Send region to ChatGPT.
With prefix REVIEW prompt before sending to ChatGPT."
  (interactive "P")
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((chatgpt-shell-prompt-query-response-style 'shell))
    (chatgpt-shell-send-to-buffer
     (if review
         (concat "\n\n" (buffer-substring (region-beginning) (region-end)))
       (buffer-substring (region-beginning) (region-end))) review)))

(defun chatgpt-shell-send-and-review-region ()
  "Send region to ChatGPT, review before submitting."
  (interactive)
  (chatgpt-shell-send-region t))

(defun chatgpt-shell-command-line-from-prompt-file (file-path)
  "Send prompt in FILE-PATH and output to standard output."
  (let ((prompt (with-temp-buffer
                  (insert-file-contents file-path)
                  (buffer-string))))
    (if (string-empty-p (string-trim prompt))
        (princ (format "Could not read prompt from %s" file-path)
               #'external-debugging-output)
      (chatgpt-shell-command-line prompt))))

(defun chatgpt-shell-command-line (prompt)
  "Send PROMPT and output to standard output."
  (let ((chatgpt-shell-prompt-query-response-style 'shell)
        (worker-done nil)
        (buffered ""))
    (chatgpt-shell-send-to-buffer
     prompt nil
     (lambda (_command output _error finished)
       (setq buffered (concat buffered output))
       (when finished
         (setq worker-done t))))
    (while buffered
      (unless (string-empty-p buffered)
        (princ buffered #'external-debugging-output))
      (setq buffered "")
      (when worker-done
        (setq buffered nil))
      (sleep-for 0.1))
    (princ "\n")))

(defun chatgpt-shell--eshell-last-last-command ()
  "Get second to last eshell command."
  (save-excursion
    (if (string= major-mode "eshell-mode")
        (let ((cmd-start)
              (cmd-end))
          ;; Find command start and end positions
          (goto-char eshell-last-output-start)
          (re-search-backward eshell-prompt-regexp nil t)
          (setq cmd-start (point))
          (goto-char eshell-last-output-start)
          (setq cmd-end (point))

          ;; Find output start and end positions
          (goto-char eshell-last-output-start)
          (forward-line 1)
          (re-search-forward eshell-prompt-regexp nil t)
          (forward-line -1)
          (concat "What's wrong with this command?\n\n"
                  (buffer-substring-no-properties cmd-start cmd-end)))
      (message "Current buffer is not an eshell buffer."))))

;; Based on https://emacs.stackexchange.com/a/48215
(defun chatgpt-shell--source-eshell-string (string)
  "Execute eshell command in STRING."
  (let ((orig (point))
        (here (point-max))
        (inhibit-point-motion-hooks t))
    (goto-char (point-max))
    (with-silent-modifications
      ;; FIXME: Use temporary buffer and avoid insert/delete.
      (insert string)
      (goto-char (point-max))
      (throw 'eshell-replace-command
             (prog1
                 (list 'let
                       (list (list 'eshell-command-name (list 'quote "source-string"))
                             (list 'eshell-command-arguments '()))
                       (eshell-parse-command (cons here (point))))
               (delete-region here (point))
               (goto-char orig))))))

(defun chatgpt-shell-add-??-command-to-eshell ()
  "Add `??' command to `eshell'."

  (defun eshell/?? (&rest _args)
    "Implements `??' eshell command."
    (interactive)
    (let ((prompt (concat
                   "What's wrong with the following command execution?\n\n"
                   (chatgpt-shell--eshell-last-last-command)))
          (prompt-file (concat temporary-file-directory
                               "chatgpt-shell-command-line-prompt")))
      (when (file-exists-p prompt-file)
        (delete-file prompt-file))
      (with-temp-file prompt-file nil nil t
                      (insert prompt))
      (chatgpt-shell--source-eshell-string
       (concat
        (file-truename (expand-file-name invocation-name invocation-directory)) " "
        "--quick --batch --eval "
        "'"
        (prin1-to-string
         `(progn
            (interactive)
            (load ,(find-library-name "shell-maker") nil t)
            (load ,(find-library-name "chatgpt-shell") nil t)
            (require (intern "chatgpt-shell") nil t)
            (setq chatgpt-shell-model-temperature 0)
            (setq chatgpt-shell-openai-key ,(chatgpt-shell-openai-key))
            (chatgpt-shell-command-line-from-prompt-file ,prompt-file)))
        "'"))))

  (add-hook 'eshell-post-command-hook
            (defun chatgpt-shell--eshell-post-??-execution ()
              (when (string-match (symbol-name #'chatgpt-shell-command-line-from-prompt-file)
                                  (string-join eshell-last-arguments " "))
                (save-excursion
                  (save-restriction
                    (narrow-to-region (eshell-beginning-of-output)
                                      (eshell-end-of-output))
                    (chatgpt-shell--put-source-block-overlays))))))

  (require 'esh-cmd)

  (add-to-list 'eshell-complex-commands "??"))

(defun chatgpt-shell-send-to-buffer (text &optional review handler)
  "Send TEXT to *chatgpt* buffer.
Set REVIEW to make changes before submitting to ChatGPT.

If HANDLER function is set, ignore `chatgpt-shell-prompt-query-response-style'."
  (let* ((buffer (cond (handler
                        nil)
                       ((eq chatgpt-shell-prompt-query-response-style 'inline)
                        (current-buffer))
                       ((eq chatgpt-shell-prompt-query-response-style 'other-buffer)
                        (let* ((inhibit-read-only t)
                               (other-buffer
                                (get-buffer-create
                                 (concat (chatgpt-shell--minibuffer-prompt)
                                         (truncate-string-to-width
                                          (nth 0 (split-string text "\n"))
                                          (window-body-width))))))
                          (with-current-buffer other-buffer
                            (erase-buffer))
                          other-buffer))
                       (t
                        nil)))
         (point (point))
         (marker (copy-marker (point)))
         (orig-region-active (region-active-p))
         (no-focus (or (eq chatgpt-shell-prompt-query-response-style 'inline)
                       (eq chatgpt-shell-prompt-query-response-style 'other-buffer)
                       handler)))
    (when (region-active-p)
      (setq marker (copy-marker (max (region-beginning)
                                     (region-end)))))
    (if (chatgpt-shell--primary-buffer)
        (with-current-buffer (chatgpt-shell--primary-buffer)
          (chatgpt-shell-start no-focus))
      (chatgpt-shell-start no-focus t))
    (when (eq chatgpt-shell-prompt-query-response-style 'other-buffer)
      (with-current-buffer buffer
        (view-mode +1)
        (setq view-exit-action 'kill-buffer)))
    (when (eq chatgpt-shell-prompt-query-response-style 'other-buffer)
      (unless (assoc (rx "*ChatGPT>" (zero-or-more not-newline) "*")
                     display-buffer-alist)
        (add-to-list 'display-buffer-alist
                     (cons (rx "*ChatGPT>" (zero-or-more not-newline) "*")
                           '((display-buffer-below-selected) (split-window-sensibly)))))
      (display-buffer buffer))
    (cl-flet ((send ()
                    (when shell-maker--busy
                      (shell-maker-interrupt nil))
                    (goto-char (point-max))
                    (if review
                        (save-excursion
                          (insert text))
                      (insert text)
                      (shell-maker--send-input
                       (if (or (eq chatgpt-shell-prompt-query-response-style 'other-buffer)
                               (eq chatgpt-shell-prompt-query-response-style 'inline))
                           (lambda (_command output error finished)
                             (setq output (or output ""))
                             (with-current-buffer buffer
                               (if error
                                   (unless (string-empty-p (string-trim output))
                                     (message "%s" output))
                                 (let ((inhibit-read-only t))
                                   (save-excursion
                                     (if orig-region-active
                                         (progn
                                           (goto-char marker)
                                           (when (eq (marker-position marker)
                                                     point)
                                             (insert "\n\n")
                                             (set-marker marker (+ 2 (marker-position marker))))
                                           (insert output)
                                           (set-marker marker (+ (length output)
                                                                 (marker-position marker))))
                                       (goto-char marker)
                                       (insert output)
                                       (set-marker marker (+ (length output)
                                                             (marker-position marker)))))))
                               (when (and finished
                                          (eq chatgpt-shell-prompt-query-response-style 'other-buffer))
                                 (chatgpt-shell--put-source-block-overlays))))
                         (or handler (lambda (_command _output _error _finished))))
                       t))))
      (if (or (eq chatgpt-shell-prompt-query-response-style 'inline)
              (eq chatgpt-shell-prompt-query-response-style 'other-buffer)
              handler)
          (with-current-buffer (chatgpt-shell--primary-buffer)
            (goto-char (point-max))
            (send))
        (with-selected-window (get-buffer-window (chatgpt-shell--primary-buffer))
          (send))))))

(defun chatgpt-shell-send-to-ielm-buffer (text &optional execute save-excursion)
  "Send TEXT to *ielm* buffer.
Set EXECUTE to automatically execute.
Set SAVE-EXCURSION to prevent point from moving."
  (ielm)
  (with-current-buffer (get-buffer-create "*ielm*")
    (goto-char (point-max))
    (if save-excursion
        (save-excursion
          (insert text))
      (insert text))
    (when execute
      (ielm-return))))

(defun chatgpt-shell-parse-elisp-code (code)
  "Parse emacs-lisp CODE and return a list of expressions."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let (sexps)
      (while (not (eobp))
        (condition-case nil
            (push (read (current-buffer)) sexps)
          (error nil)))
      (reverse sexps))))

(defun chatgpt-shell-split-elisp-expressions (code)
  "Split emacs-lisp CODE into a list of stringified expressions."
  (mapcar
   (lambda (form)
     (prin1-to-string form))
   (chatgpt-shell-parse-elisp-code code)))


(defun chatgpt-shell-make-request-data (messages &optional version temperature other-params)
  "Make request data from MESSAGES, VERSION, TEMPERATURE, and OTHER-PARAMS."
  (let ((request-data `((model . ,(or version
                                      (chatgpt-shell-model-version)))
                        (messages . ,(vconcat ;; Vector for json
                                      messages)))))
    (when (or temperature chatgpt-shell-model-temperature)
      (push `(temperature . ,(or temperature chatgpt-shell-model-temperature))
            request-data))
    (when other-params
      (push other-params
            request-data))
    request-data))

(defun chatgpt-shell-post-messages (messages response-extractor &optional version callback error-callback temperature other-params)
  "Make a single ChatGPT request with MESSAGES.
Optionally pass model VERSION, CALLBACK, ERROR-CALLBACK, TEMPERATURE and OTHER-PARAMS.

OTHER-PARAMS are appended to the json object at the top level.

If CALLBACK or ERROR-CALLBACK are missing, execute synchronously.

For example:

\(chatgpt-shell-post-messages
 `(((role . \"user\")
    (content . \"hello\")))
 \"gpt-3.5-turbo\"
 (lambda (response)
   (message \"%s\" response))
 (lambda (error)
   (message \"%s\" error)))"
  (if (and callback error-callback)
      (progn
        (unless (boundp 'shell-maker--current-request-id)
          (defvar-local shell-maker--current-request-id 0))
        (with-temp-buffer
          (setq-local shell-maker--config
                      chatgpt-shell--config)
          (shell-maker-async-shell-command
           (chatgpt-shell--make-curl-request-command-list
            (chatgpt-shell-make-request-data messages version temperature other-params))
           nil ;; streaming
           (or response-extractor #'chatgpt-shell--extract-chatgpt-response)
           callback
           error-callback)))
    (with-temp-buffer
      (setq-local shell-maker--config
                  chatgpt-shell--config)
      (let* ((buffer (current-buffer))
             (command
              (chatgpt-shell--make-curl-request-command-list
               (let ((request-data `((model . ,(or version
                                                   (chatgpt-shell-model-version)))
                                     (messages . ,(vconcat ;; Vector for json
                                                   messages)))))
                 (when (or temperature chatgpt-shell-model-temperature)
                   (push `(temperature . ,(or temperature chatgpt-shell-model-temperature))
                         request-data))
                 (when other-params
                   (push other-params
                         request-data))
                 request-data)))
             (config chatgpt-shell--config)
             (status (progn
                       (shell-maker--write-output-to-log-buffer "// Request\n\n" config)
                       (shell-maker--write-output-to-log-buffer (string-join command " ") config)
                       (shell-maker--write-output-to-log-buffer "\n\n" config)
                       (apply #'call-process (seq-first command) nil buffer nil (cdr command))))
             (data (buffer-substring-no-properties (point-min) (point-max)))
             (response (chatgpt-shell--extract-chatgpt-response data)))
        (shell-maker--write-output-to-log-buffer (format "// Data (status: %d)\n\n" status) config)
        (shell-maker--write-output-to-log-buffer data config)
        (shell-maker--write-output-to-log-buffer "\n\n" config)
        (shell-maker--write-output-to-log-buffer "// Response\n\n" config)
        (shell-maker--write-output-to-log-buffer response config)
        (shell-maker--write-output-to-log-buffer "\n\n" config)
        response))))

(defun chatgpt-shell-describe-image ()
  "Request OpenAI to describe image.

When visiting a buffer with an image, send that.

If in a `dired' buffer, use selection (single image only for now)."
  (interactive)
  (let* ((file (chatgpt-shell--current-file))
         (extension (downcase (file-name-extension file))))
    (unless (seq-contains-p '("jpg" "jpeg" "png" "webp" "gif") extension)
      (user-error "Must be user either .jpg, .jpeg, .png, .webp or .gif file."))
    (chatgpt-shell-vision-make-request
     (read-string "Send vision prompt (default \"What’s in this image?\"): " nil nil "What’s in this image?")
     file)))

(defun chatgpt-shell--current-file ()
  "Return buffer file (if available) or dired selected file."
  (when (use-region-p)
    (user-error "No region selection supported"))
  (if (buffer-file-name)
      (buffer-file-name)
    (let* ((dired-files (dired-get-marked-files))
           (file (seq-first dired-files)))
      (unless dired-files
        (user-error "No file selected"))
      (when (> (length dired-files) 1)
        (user-error "Only one file selection supported"))
      file)))

(cl-defun chatgpt-shell-vision-make-request (prompt url-path &key on-success on-failure)
  "Make a vision request using PROMPT and URL-PATH.

PROMPT can be somethign like: \"Describe the image in detail\".
URL-PATH can be either a local file path or an http:// URL."
  (let* ((url (if (string-prefix-p "http" url-path)
                  url-path
                (unless (file-exists-p url-path)
                  (error "file not found"))
                (concat "data:image/jpeg;base64,"
                        (with-temp-buffer
                          (insert-file-contents-literally url-path)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string)))))
         (messages
          (vconcat ;; Convert to vector for json
           (append
            `(((role . "user")
               (content . ,(vconcat
                            `(((type . "text")
                               (text . ,prompt))
                              ((type . "image_url")
                               (image_url . ,url)))))))))))
    (message "Requesting...")
    (chatgpt-shell-post-messages
     messages
     #'chatgpt-shell--extract-chatgpt-response
     "gpt-4-vision-preview"
     (if on-success
         (lambda (response _partial)
           (funcall on-success response))
       (lambda (response _partial)
         (message response)))
     (or on-failure (lambda (error)
                      (message error)))
     nil '(max_tokens . 300))))

(defun chatgpt-shell-post-prompt (prompt &optional response-extractor version callback error-callback temperature other-params)
  "Make a single ChatGPT request with PROMPT.
Optioally pass model VERSION, CALLBACK, ERROR-CALLBACK, TEMPERATURE, and OTHER-PARAMS.

If CALLBACK or ERROR-CALLBACK are missing, execute synchronously.

OTHER-PARAMS are appended to the json object at the top level.

For example:

\(chatgpt-shell-post-prompt
 \"hello\"
 \"gpt-3.5-turbo\"
 (lambda (response)
   (message \"%s\" response))
 (lambda (error)
   (message \"%s\" error)))"
  (chatgpt-shell-post-messages `(((role . "user")
                                  (content . ,prompt)))
                               (or response-extractor #'chatgpt-shell--extract-chatgpt-response)
                               version
                               callback
                               error-callback
                               temperature
                               other-params))

(defun chatgpt-shell-openai-key ()
  "Get the ChatGPT key."
  (cond ((stringp chatgpt-shell-openai-key)
         chatgpt-shell-openai-key)
        ((functionp chatgpt-shell-openai-key)
         (condition-case _err
             (funcall chatgpt-shell-openai-key)
           (error
            "KEY-NOT-FOUND")))
        (t
         nil)))

(defun chatgpt-shell--api-url ()
  "The complete URL OpenAI's API.

`chatgpt-shell--api-url' =
   `chatgpt-shell--api-url-base' + `chatgpt-shell--api-url-path'"
  (concat chatgpt-shell-api-url-base chatgpt-shell-api-url-path))

(defun chatgpt-shell--make-curl-request-command-list (request-data)
  "Build ChatGPT curl command list using REQUEST-DATA."
  (append (list "curl" (chatgpt-shell--api-url))
          chatgpt-shell-additional-curl-options
          (list "--fail-with-body"
                "--no-progress-meter"
                "-m" (number-to-string chatgpt-shell-request-timeout)
                "-H" "Content-Type: application/json; charset=utf-8"
                "-H" (funcall chatgpt-shell-auth-header)
                "-d" (shell-maker--json-encode request-data))))

(defun chatgpt-shell--make-payload (history)
  "Create the request payload from HISTORY."
  (setq history
        (vconcat ;; Vector for json
         (chatgpt-shell--user-assistant-messages
          (last history
                (chatgpt-shell--unpaired-length
                 (if (functionp chatgpt-shell-transmitted-context-length)
                     (funcall chatgpt-shell-transmitted-context-length
                              (chatgpt-shell-model-version) history)
                   chatgpt-shell-transmitted-context-length))))))
  ;; TODO: Use `chatgpt-shell-make-request-data'.
  (let ((request-data `((model . ,(chatgpt-shell-model-version))
                        (messages . ,(if (chatgpt-shell-system-prompt)
                                         (vconcat ;; Vector for json
                                          (list
                                           (list
                                            (cons 'role "system")
                                            (cons 'content (chatgpt-shell-system-prompt))))
                                          history)
                                       history)))))
    (when chatgpt-shell-model-temperature
      (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
    (when chatgpt-shell-streaming
      (push `(stream . t) request-data))
    request-data))

(defun chatgpt-shell--approximate-context-length (model messages)
  "Approximate the context length using MODEL and MESSAGES."
  (let* ((tokens-per-message)
         (max-tokens)
         (original-length (floor (/ (length messages) 2)))
         (context-length original-length))
    ;; Remove "ft:" from fine-tuned models and recognize as usual
    (setq model (string-remove-prefix "ft:" model))
    (cond
     ((string-prefix-p "gpt-3.5" model)
      (setq tokens-per-message 4
            ;; https://platform.openai.com/docs/models/gpt-3-5
            max-tokens 4096))
     ((string-prefix-p "gpt-4" model)
      (setq tokens-per-message 3
            ;; https://platform.openai.com/docs/models/gpt-4
            max-tokens 8192))
     (t
      (error "Don't know '%s', so can't approximate context length" model)))
    (while (> (chatgpt-shell--num-tokens-from-messages
               tokens-per-message messages)
              max-tokens)
      (setq messages (cdr messages)))
    (setq context-length (floor (/ (length messages) 2)))
    (unless (eq original-length context-length)
      (message "Warning: chatgpt-shell context clipped"))
    context-length))

;; Very rough token approximation loosely based on num_tokens_from_messages from:
;; https://github.com/openai/openai-cookbook/blob/main/examples/How_to_count_tokens_with_tiktoken.ipynb
(defun chatgpt-shell--num-tokens-from-messages (tokens-per-message messages)
  "Approximate number of tokens in MESSAGES using TOKENS-PER-MESSAGE."
  (let ((num-tokens 0))
    (dolist (message messages)
      (setq num-tokens (+ num-tokens tokens-per-message))
      (setq num-tokens (+ num-tokens (/ (length (cdr message)) tokens-per-message))))
    ;; Every reply is primed with <|start|>assistant<|message|>
    (setq num-tokens (+ num-tokens 3))
    num-tokens))

(defun chatgpt-shell--extract-chatgpt-response (json)
  "Extract ChatGPT response from JSON."
  (if (eq (type-of json) 'cons)
      (let-alist json ;; already parsed
        (or (let-alist (seq-first .choices)
              (or .delta.content
                  .message.content))
            .error.message
            ""))
    (if-let (parsed (shell-maker--json-parse-string json))
        (string-trim
         (let-alist parsed
           (let-alist (seq-first .choices)
             .message.content)))
      (if-let (parsed-error (shell-maker--json-parse-string-filtering
                             json "^curl:.*\n?"))
          (let-alist parsed-error
            .error.message)))))

;; FIXME: Make shell agnostic or move to chatgpt-shell.
(defun chatgpt-shell-restore-session-from-transcript ()
  "Restore session from transcript.

Very much EXPERIMENTAL."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let* ((path (read-file-name "Restore from: " nil nil t))
         (prompt-regexp (shell-maker-prompt-regexp shell-maker--config))
         (history (with-temp-buffer
                    (insert-file-contents path)
                    (chatgpt-shell--extract-history
                     (buffer-substring-no-properties
                      (point-min) (point-max))
                     prompt-regexp)))
         (execute-command (shell-maker-config-execute-command
                           shell-maker--config))
         (validate-command (shell-maker-config-validate-command
                            shell-maker--config))
         (command)
         (response)
         (failed))
    ;; Momentarily overrides request handling to replay all commands
    ;; read from file so comint treats all commands/outputs like
    ;; any other command.
    (unwind-protect
        (progn
          (setf (shell-maker-config-validate-command shell-maker--config) nil)
          (setf (shell-maker-config-execute-command shell-maker--config)
                (lambda (_command _history callback _error-callback)
                  (setq response (car history))
                  (setq history (cdr history))
                  (when response
                    (unless (string-equal (map-elt response 'role)
                                          "assistant")
                      (setq failed t)
                      (user-error "Invalid transcript"))
                    (funcall callback (map-elt response 'content) nil)
                    (setq command (car history))
                    (setq history (cdr history))
                    (when command
                      (goto-char (point-max))
                      (insert (map-elt command 'content))
                      (shell-maker--send-input)))))
          (goto-char (point-max))
          (comint-clear-buffer)
          (setq command (car history))
          (setq history (cdr history))
          (when command
            (unless (string-equal (map-elt command 'role)
                                  "user")
              (setq failed t)
              (user-error "Invalid transcript"))
            (goto-char (point-max))
            (insert (map-elt command 'content))
            (shell-maker--send-input)))
      (if failed
          (setq shell-maker--file nil)
        (setq shell-maker--file path))
      (setq shell-maker--busy nil)
      (setf (shell-maker-config-validate-command shell-maker--config)
            validate-command)
      (setf (shell-maker-config-execute-command shell-maker--config)
            execute-command)))
  (goto-char (point-max)))

(defun chatgpt-shell--fontify-source-block (quotes1-start quotes1-end lang
lang-start lang-end body-start body-end quotes2-start quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (overlay-put (make-overlay quotes1-start
                             quotes1-end) 'invisible 'chatgpt-shell)
  (overlay-put (make-overlay quotes2-start
                             quotes2-end) 'invisible 'chatgpt-shell)
  (unless (eq lang-start lang-end)
    (overlay-put (make-overlay lang-start
                               lang-end) 'face '(:box t))
    (overlay-put (make-overlay lang-end
                               (1+ lang-end)) 'display "\n\n"))
  (let ((lang-mode (intern (concat (or
                                    (chatgpt-shell--resolve-internal-language lang)
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties body-start body-end))
        (buf (if (and (boundp 'shell-maker--config)
                      shell-maker--config)
                 (shell-maker-buffer shell-maker--config)
               (current-buffer)))
        (pos 0)
        (props)
        (overlay)
        (propertized-text))
    (if (fboundp lang-mode)
        (progn
          (setq propertized-text
                (with-current-buffer
                    (get-buffer-create
                     (format " *chatgpt-shell-fontification:%s*" lang-mode))
                  (let ((inhibit-modification-hooks nil)
                        (inhibit-message t))
                    (erase-buffer)
                    ;; Additional space ensures property change.
                    (insert string " ")
                    (funcall lang-mode)
                    (font-lock-ensure))
                  (buffer-string)))
          (while (< pos (length propertized-text))
            (setq props (text-properties-at pos propertized-text))
            (setq overlay (make-overlay (+ body-start pos)
                                        (+ body-start (1+ pos))
                                        buf))
            (overlay-put overlay 'face (plist-get props 'face))
            (setq pos (1+ pos))))
      (overlay-put (make-overlay body-start body-end buf)
                   'face 'font-lock-doc-markup-face))))

(defun chatgpt-shell--fontify-link (start end title-start title-end url-start url-end)
  "Fontify a markdown link.
Use START END TITLE-START TITLE-END URL-START URL-END."
  ;; Hide markup before
  (overlay-put (make-overlay start title-start) 'invisible 'chatgpt-shell)
  ;; Show title as link
  (overlay-put (make-overlay title-start title-end) 'face 'link)
  ;; Make RET open the URL
  (define-key (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1]
                  (lambda () (interactive)
                    (browse-url (buffer-substring-no-properties url-start url-end))))
                (define-key map (kbd "RET")
                  (lambda () (interactive)
                    (browse-url (buffer-substring-no-properties url-start url-end))))
                (overlay-put (make-overlay title-start title-end) 'keymap map)
                map)
    [remap self-insert-command] 'ignore)
  ;; Hide markup after
  (overlay-put (make-overlay title-end end) 'invisible 'chatgpt-shell))

(defun chatgpt-shell--fontify-bold (start end text-start text-end)
  "Fontify a markdown bold.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (overlay-put (make-overlay start text-start) 'invisible 'chatgpt-shell)
  ;; Show title as bold
  (overlay-put (make-overlay text-start text-end) 'face 'bold)
  ;; Hide markup after
  (overlay-put (make-overlay text-end end) 'invisible 'chatgpt-shell))

(defun chatgpt-shell--fontify-header (start _end level-start level-end title-start title-end)
  "Fontify a markdown header.
Use START END LEVEL-START LEVEL-END TITLE-START TITLE-END."
  ;; Hide markup before
  (overlay-put (make-overlay start title-start) 'invisible 'chatgpt-shell)
  ;; Show title as header
  (overlay-put (make-overlay title-start title-end) 'face
               (cond ((eq (- level-end level-start) 1)
                      'org-level-1)
                     ((eq (- level-end level-start) 2)
                      'org-level-2)
                     ((eq (- level-end level-start) 3)
                      'org-level-3)
                     ((eq (- level-end level-start) 4)
                      'org-level-4)
                     ((eq (- level-end level-start) 5)
                      'org-level-5)
                     ((eq (- level-end level-start) 6)
                      'org-level-6)
                     ((eq (- level-end level-start) 7)
                      'org-level-7)
                     ((eq (- level-end level-start) 8)
                      'org-level-8)
                     (t
                      'org-level-1))))

(defun chatgpt-shell--fontify-italic (start end text-start text-end)
  "Fontify a markdown italic.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (overlay-put (make-overlay start text-start) 'invisible 'chatgpt-shell)
  ;; Show title as italic
  (overlay-put (make-overlay text-start text-end) 'face 'italic)
  ;; Hide markup after
  (overlay-put (make-overlay text-end end) 'invisible 'chatgpt-shell))

(defun chatgpt-shell--fontify-strikethrough (start end text-start text-end)
  "Fontify a markdown strikethrough.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (overlay-put (make-overlay start text-start) 'invisible 'chatgpt-shell)
  ;; Show title as strikethrough
  (overlay-put (make-overlay text-start text-end) 'face '(:strike-through t))
  ;; Hide markup after
  (overlay-put (make-overlay text-end end) 'invisible 'chatgpt-shell))

(defun chatgpt-shell--fontify-inline-code (body-start body-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (overlay-put (make-overlay (1- body-start)
                             body-start) 'invisible 'chatgpt-shell)
  (overlay-put (make-overlay body-end
                             (1+ body-end)) 'invisible 'chatgpt-shell)
  (overlay-put (make-overlay body-start body-end
                             (if (and (boundp 'shell-maker--config)
                                      shell-maker--config)
                                 (shell-maker-buffer shell-maker--config)
                               (current-buffer)))
               'face 'font-lock-doc-markup-face))

(defun chatgpt-shell-rename-block-at-point ()
  "Rename block at point (perhaps a different language)."
  (interactive)
  (save-excursion
    (if-let ((block (chatgpt-shell-markdown-block-at-point)))
        (if (map-elt block 'language)
            (perform-replace (map-elt block 'language)
                             (read-string "Name: " nil nil "") nil nil nil nil nil
                             (map-elt block 'language-start) (map-elt block 'language-end))
          (let ((new-name (read-string "Name: " nil nil "")))
            (goto-char (map-elt block 'language-start))
            (insert new-name)
            (chatgpt-shell--put-source-block-overlays)))
      (user-error "No block at point"))))

(defun chatgpt-shell-remove-block-overlays ()
  "Remove block overlays.  Handy for renaming blocks."
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (delete-overlay overlay)))

(defun chatgpt-shell-refresh-rendering ()
  "Refresh markdown rendering by re-applying to entire buffer."
  (interactive)
  (chatgpt-shell--put-source-block-overlays))

(defun chatgpt-shell--put-source-block-overlays ()
  "Put overlays for all source blocks."
  (when chatgpt-shell-highlight-blocks
    (let* ((source-blocks (chatgpt-shell--source-blocks))
           (avoid-ranges (seq-map (lambda (block)
                                    (map-elt block 'body))
                                  source-blocks)))
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (delete-overlay overlay))
      (dolist (block source-blocks)
        (chatgpt-shell--fontify-source-block
         (car (map-elt block 'start))
         (cdr (map-elt block 'start))
         (buffer-substring-no-properties (car (map-elt block 'language))
                                         (cdr (map-elt block 'language)))
         (car (map-elt block 'language))
         (cdr (map-elt block 'language))
         (car (map-elt block 'body))
         (cdr (map-elt block 'body))
         (car (map-elt block 'end))
         (cdr (map-elt block 'end))))
      (dolist (link (chatgpt-shell--markdown-links avoid-ranges))
        (chatgpt-shell--fontify-link
         (map-elt link 'start)
         (map-elt link 'end)
         (car (map-elt link 'title))
         (cdr (map-elt link 'title))
         (car (map-elt link 'url))
         (cdr (map-elt link 'url))))
      (dolist (header (chatgpt-shell--markdown-headers avoid-ranges))
        (chatgpt-shell--fontify-header
         (map-elt header 'start)
         (map-elt header 'end)
         (car (map-elt header 'level))
         (cdr (map-elt header 'level))
         (car (map-elt header 'title))
         (cdr (map-elt header 'title))))
      (dolist (bold (chatgpt-shell--markdown-bolds avoid-ranges))
        (chatgpt-shell--fontify-bold
         (map-elt bold 'start)
         (map-elt bold 'end)
         (car (map-elt bold 'text))
         (cdr (map-elt bold 'text))))
      (dolist (italic (chatgpt-shell--markdown-italics avoid-ranges))
        (chatgpt-shell--fontify-italic
         (map-elt italic 'start)
         (map-elt italic 'end)
         (car (map-elt italic 'text))
         (cdr (map-elt italic 'text))))
      (dolist (strikethrough (chatgpt-shell--markdown-strikethroughs avoid-ranges))
        (chatgpt-shell--fontify-strikethrough
         (map-elt strikethrough 'start)
         (map-elt strikethrough 'end)
         (car (map-elt strikethrough 'text))
         (cdr (map-elt strikethrough 'text))))
      (dolist (inline-code (chatgpt-shell--markdown-inline-codes avoid-ranges))
        (chatgpt-shell--fontify-inline-code
         (car (map-elt inline-code 'body))
         (cdr (map-elt inline-code 'body)))))))

(defun chatgpt-shell--unpaired-length (length)
  "Expand LENGTH to include paired responses.

Each request has a response, so double LENGTH if set.

Add one for current request (without response).

If no LENGTH set, use 2048."
  (if length
      (1+ (* 2 length))
    2048))

(defun chatgpt-shell-view-at-point ()
  "View prompt and output at point in a separate buffer."
  (interactive)
  (unless (eq major-mode 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let ((prompt-pos (save-excursion
                      (goto-char (process-mark
                                  (get-buffer-process (current-buffer))))
                      (point)))
        (buf))
    (save-excursion
      (when (>= (point) prompt-pos)
        (goto-char prompt-pos)
        (forward-line -1)
        (end-of-line))
      (let* ((items (chatgpt-shell--user-assistant-messages
                     (shell-maker--command-and-response-at-point)))
             (command (string-trim (or (map-elt (seq-first items) 'content) "")))
             (response (string-trim (or (map-elt (car (last items)) 'content) ""))))
        (setq buf (generate-new-buffer (if command
                                           (concat
                                            (buffer-name (current-buffer)) "> "
                                            ;; Only the first line of prompt.
                                            (seq-first (split-string command "\n")))
                                         (concat (buffer-name (current-buffer)) "> "
                                                 "(no prompt)"))))
        (when (seq-empty-p items)
          (user-error "Nothing to view"))
        (with-current-buffer buf
          (save-excursion
            (insert (propertize (or command "") 'face font-lock-doc-face))
            (when (and command response)
              (insert "\n\n"))
            (insert (or response "")))
          (chatgpt-shell--put-source-block-overlays)
          (view-mode +1)
          (setq view-exit-action 'kill-buffer))))
    (switch-to-buffer buf)
    buf))

(defun chatgpt-shell--extract-history (text prompt-regexp)
  "Extract all command and responses in TEXT with PROMPT-REGEXP."
  (chatgpt-shell--user-assistant-messages
   (shell-maker--extract-history text prompt-regexp)))

(defun chatgpt-shell--user-assistant-messages (history)
  "Convert HISTORY to ChatGPT format.

Sequence must be a vector for json serialization.

For example:

 [
   ((role . \"user\") (content . \"hello\"))
   ((role . \"assistant\") (content . \"world\"))
 ]"
  (let ((result))
    (mapc
     (lambda (item)
       (when (car item)
         (push (list (cons 'role "user")
                     (cons 'content (car item))) result))
       (when (cdr item)
         (push (list (cons 'role "assistant")
                     (cons 'content (cdr item))) result)))
     history)
    (nreverse result)))

(defun chatgpt-shell-run-command (command callback)
  "Run COMMAND list asynchronously and call CALLBACK function.

CALLBACK can be like:

\(lambda (success output)
  (message \"%s\" output))"
  (let* ((buffer (generate-new-buffer "*run command*"))
         (proc (apply #'start-process
                      (append `("exec" ,buffer) command))))
    (set-process-sentinel
     proc
     (lambda (proc _)
       (with-current-buffer buffer
         (funcall callback
                  (equal (process-exit-status proc) 0)
                  (buffer-string))
         (kill-buffer buffer))))))

(defun chatgpt-shell--resolve-internal-language (language)
  "Resolve external LANGUAGE to internal.

For example \"elisp\" -> \"emacs-lisp\"."
  (when language
    (or (map-elt chatgpt-shell-language-mapping
                 (downcase (string-trim language)))
        (when (intern (concat (downcase (string-trim language))
                              "-mode"))
          (downcase (string-trim language))))))

(defun chatgpt-shell-block-action-at-point ()
  "Return t if block at point has an action.  nil otherwise."
  (let* ((source-block (chatgpt-shell-markdown-block-at-point))
         (language (chatgpt-shell--resolve-internal-language
                    (map-elt source-block 'language)))
         (actions (chatgpt-shell--get-block-actions language)))
    actions
    (if actions
        actions
      (chatgpt-shell--org-babel-command language))))

(defun chatgpt-shell--get-block-actions (language)
  "Get block actions for LANGUAGE."
  (map-elt chatgpt-shell-source-block-actions
           (chatgpt-shell--resolve-internal-language
            language)))

(defun chatgpt-shell--org-babel-command (language)
  "Resolve LANGUAGE to org babel command."
  (require 'ob)
  (when language
    (ignore-errors
      (or (require (intern (concat "ob-" (capitalize language))) nil t)
          (require (intern (concat "ob-" (downcase language))) nil t)))
    (let ((f (intern (concat "org-babel-execute:" language)))
          (f-cap (intern (concat "org-babel-execute:" (capitalize language)))))
      (if (fboundp f)
          f
        (if (fboundp f-cap)
            f-cap)))))

(defun chatgpt-shell-execute-block-action-at-point ()
  "Execute block at point."
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (if-let ((actions (chatgpt-shell--get-block-actions (map-elt block 'language)))
               (action (map-elt actions 'primary-action))
               (confirmation (map-elt actions 'primary-action-confirmation))
               (default-directory "/tmp"))
          (when (y-or-n-p confirmation)
            (funcall action (buffer-substring-no-properties
                             (map-elt block 'start)
                             (map-elt block 'end))))
        (if (and (map-elt block 'language)
                 (chatgpt-shell--org-babel-command
                  (chatgpt-shell--resolve-internal-language
                   (map-elt block 'language))))
            (chatgpt-shell-execute-babel-block-action-at-point)
          (user-error "No primary action for %s blocks" (map-elt block 'language))))
    (user-error "No block at point")))

(defun chatgpt-shell--override-language-params (language params)
  "Override PARAMS for LANGUAGE if found in `chatgpt-shell-babel-headers'."
  (if-let* ((overrides (map-elt chatgpt-shell-babel-headers
                                language))
            (temp-dir (file-name-as-directory
                       (make-temp-file "chatgpt-shell-" t)))
            (temp-file (concat temp-dir "source-block-" language)))
      (if (cdr (assq :file overrides))
          (append (list
                   (cons :file
                         (replace-regexp-in-string (regexp-quote "<temp-file>")
                                                   temp-file
                                                   (cdr (assq :file overrides)))))
                  (assq-delete-all :file overrides)
                  params)
        (append
         overrides
         params))
    params))

(defun chatgpt-shell-execute-babel-block-action-at-point ()
  "Execute block as org babel."
  (interactive)
  (require 'ob)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (if-let* ((language (chatgpt-shell--resolve-internal-language
                           (map-elt block 'language)))
                (babel-command (chatgpt-shell--org-babel-command language))
                (lang-headers (intern
                               (concat "org-babel-default-header-args:" language)))
                (bound (fboundp babel-command))
                (default-directory "/tmp"))
          (when (y-or-n-p (format "Execute %s ob block?" (capitalize language)))
            (message "Executing %s block..." (capitalize language))
            (let* ((params (org-babel-process-params
                            (chatgpt-shell--override-language-params
                             language
                             (org-babel-merge-params
                              org-babel-default-header-args
                              (and (boundp
                                    (intern
                                     (concat "org-babel-default-header-args:" language)))
                                   (eval (intern
                                          (concat "org-babel-default-header-args:" language)) t))))))
                   (output (progn
                             (when (get-buffer org-babel-error-buffer-name)
                               (kill-buffer (get-buffer org-babel-error-buffer-name)))
                             (funcall babel-command
                                      (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end)) params)))
                   (buffer))
              (if (and output (not (stringp output)))
                  (setq output (format "%s" output))
                (when (and (cdr (assq :file params))
                           (file-exists-p (cdr (assq :file params))))
                  (setq output (cdr (assq :file params)))))
              (if (and output (not (string-empty-p output)))
                  (progn
                    (setq buffer (get-buffer-create (format "*%s block output*" (capitalize language))))
                    (with-current-buffer buffer
                      (save-excursion
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (setq output (when output (string-trim output)))
                          (if (file-exists-p output) ;; Output was a file.
                              ;; Image? insert image.
                              (if (member (downcase (file-name-extension output))
                                          '("jpg" "jpeg" "png" "gif" "bmp" "webp"))
                                  (progn
                                    (insert "\n")
                                    (insert-image (create-image output)))
                                ;; Insert content of all other file types.
                                (insert-file-contents output))
                            ;; Just text output, insert that.
                            (insert output))))
                      (view-mode +1)
                      (setq view-exit-action 'kill-buffer))
                    (message "")
                    (select-window (display-buffer buffer)))
                (if (get-buffer org-babel-error-buffer-name)
                    (select-window (display-buffer org-babel-error-buffer-name))
                  (setq buffer (get-buffer-create (format "*%s block output*" (capitalize language))))
                  (message "No output. Check %s blocks work in your .org files." language)))))
        (user-error "No primary action for %s blocks" (map-elt block 'language)))
    (user-error "No block at point")))

(defun chatgpt-shell-eval-elisp-block-in-ielm (text)
  "Run elisp source in TEXT."
  (chatgpt-shell-send-to-ielm-buffer text t))

(defun chatgpt-shell-compile-swift-block (text)
  "Compile Swift source in TEXT."
  (when-let* ((source-file (chatgpt-shell-write-temp-file text ".swift"))
              (default-directory (file-name-directory source-file)))
    (chatgpt-shell-run-command
     `("swiftc" ,(file-name-nondirectory source-file))
     (lambda (success output)
       (if success
           (message
            (concat (propertize "Compiles cleanly" 'face '(:foreground "green"))
                    " :)"))
         (let ((buffer (generate-new-buffer "*block error*")))
           (with-current-buffer buffer
             (save-excursion
               (insert
                (chatgpt-shell--remove-compiled-file-names
                 (file-name-nondirectory source-file)
                 (ansi-color-apply output))))
             (compilation-mode)
             (view-mode +1)
             (setq view-exit-action 'kill-buffer))
           (select-window (display-buffer buffer)))
         (message
          (concat (propertize "Compilation failed" 'face '(:foreground "orange"))
                  " :(")))))))

(defun chatgpt-shell-write-temp-file (content extension)
  "Create a temporary file with EXTENSION and write CONTENT to it.

Return the file path."
  (let* ((temp-dir (file-name-as-directory
                    (make-temp-file "chatgpt-shell-" t)))
         (temp-file (concat temp-dir "source-block" extension)))
    (with-temp-file temp-file
      (insert content)
      (let ((inhibit-message t))
        (write-file temp-file)))
    temp-file))

(defun chatgpt-shell--remove-compiled-file-names (filename text)
  "Remove lines starting with FILENAME in TEXT.

Useful to remove temp file names from compilation output when
compiling source blocks."
  (replace-regexp-in-string
   (rx-to-string `(: bol ,filename (one-or-more (not (any " "))) " ") " ")
   "" text))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
