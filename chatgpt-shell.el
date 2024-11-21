;;; chatgpt-shell.el --- A multi-llm Emacs shell (ChatGPT, Claude, Gemini) + editing integrations  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 2.0.4
;; Package-Requires: ((emacs "28.1") (shell-maker "0.69.1"))
(defconst chatgpt-shell--version "2.0.4")

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
(require 'dired)
(require 'esh-mode)
(require 'em-prompt)
(require 'eshell)
(require 'find-func)
(require 'flymake)
(require 'ielm)
(require 'shell-maker)
(require 'smerge-mode)
(require 'ob-core)
(require 'color)

(require 'chatgpt-shell-openai)
(require 'chatgpt-shell-google)
(require 'chatgpt-shell-anthropic)

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
  "Please help me proofread the following English text and only reply with fixed text:"
  "Prompt header used by `chatgpt-shell-proofread-region`."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt-header-whats-wrong-with-last-command
  "What's wrong with this command execution?"
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
   (lambda (command output success)
     (message \"Command: %s\" command)
     (message \"Output: %s\" output)))"
  :type 'hook
  :group 'shell-maker)

(defvaralias 'chatgpt-shell-swap-model-version 'chatgpt-shell-swap-model)

(defvaralias 'chatgpt-shell-display-function 'shell-maker-display-function)

(defvaralias 'chatgpt-shell-read-string-function 'shell-maker-read-string-function)

(defvaralias 'chatgpt-shell-logging 'shell-maker-logging)

(defvaralias 'chatgpt-shell-root-path 'shell-maker-root-path)

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
  :type '(alist :key-type (string :tag "Language Name/Alias")
                :value-type (string :tag "Mode Name (without -mode)"))
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

Entries are of the form (language . headers).  Headers should
conform to the types of `org-babel-default-header-args', which
see.

Please submit contributions so more things work out of the box."
  :type '(alist :key-type (string :tag "Language")
                :value-type (alist :key-type (restricted-sexp :match-alternatives (keywordp) :tag "Argument Name")
                                   :value-type (string :tag "Value")))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-source-block-actions
  nil
  "Block actions for known languages.

Can be used compile or run source block at point."
  :type '(alist :key-type (string :tag "Language")
                :value-type (list (cons (const primary-action-confirmation) (string :tag "Confirmation Prompt:"))
                                  (cons (const primary-action) (function :tag "Action:"))))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-models
  (append (chatgpt-shell-openai-models)
          (chatgpt-shell-anthropic-models)
          (chatgpt-shell-google-models))
  "The list of supported models to swap from.

See `chatgpt-shell-openai-models', `chatgpt-shell-anthropic-models'
and `chatgpt-shell-google-models' for details."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version 0
  "The active ChatGPT OpenAI model index.

See `chatgpt-shell-models' for available model versions.

Swap using `chatgpt-shell-swap-model'."
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
  (cl-labels ((chatgpt-shell--get-system-info-command
               ()
               (cond ((eq system-type 'darwin) "sw_vers")
                     ((or (eq system-type 'gnu/linux)
                          (eq system-type 'gnu/kfreebsd)) "uname -a")
                     ((eq system-type 'windows-nt) "ver")
                     (t (format "%s" system-type)))))
    (let ((system-info (string-trim
                        (shell-command-to-string
                         (chatgpt-shell--get-system-info-command)))))
      (concat text
              "\n# System info\n"
              "\n## OS details\n"
              system-info
              "\n## Editor\n"
              (emacs-version)))))

(defcustom chatgpt-shell-system-prompts
  `(("tl;dr" . "Be as succint but informative as possible and respond in tl;dr form to my queries")
    ("General" . "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.")
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
                                 Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets."))
    ("Japanese" . ,(chatgpt-shell--append-system-info
                    "The user is a beginner Japanese language learner with very limited time.
                     You treat their time as precious. You do not repeat obvious things, including their query.
                     You are as concise as possible in responses.
                     You never apologize for confusions because it would waste their time.
                     You use markdown liberally to structure responses."))
    ("SwiftUI" . ,(chatgpt-shell--append-system-info
                   "You are a helpful assistant that generates concise, idiomatic SwiftUI views. Always follow these guidelines:

1. Generate a SwiftUI ContentView.
2. Never import SwiftUI.
3. Ensure code is compatible with devices running iOS 15 or later.
4. Use the `@State`, `@Binding`, or `@ObservedObject` property wrappers where applicable.
5. Assume that all Text should be scalable  (e.g., `.font(.body)` or `.dynamicTypeSize(.large)`).
6. When creating custom views, ensure layout is responsive across screen sizes using `.frame`, `.padding()`, `.alignmentGuide()`, or `.geometryReader`.
7. Add a structured comment explaining the purpose of the view at the top.
8. Do not include ContentView_Previews or @main structs.
9. Label markdown source blocks as swiftui.
10. Omit all explanations and code comments.")))

  "List of system prompts to choose from.

If prompt is a cons, its car will be used as a title to display.

For example:

\(\"Translating\" . \"You are a helpful English to Spanish assistant.\")\"
\(\"Programming\" . \"The user is a programmer with very limited time...\")"
  :type '(alist :key-type (string :tag "Title")
                :value-type (string :tag "Prompt value"))
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
  (when (boundp 'chatgpt-shell-model-versions)
    (error (concat "\"chatgpt-shell-model-versions\" no longer supported. "
                   "Please unset or migrate to \"chatgpt-shell-models\".")))
  (cond ((stringp chatgpt-shell-model-version)
         chatgpt-shell-model-version)
        ((integerp chatgpt-shell-model-version)
         (let ((model (nth chatgpt-shell-model-version
                          chatgpt-shell-models)))
           (cond ((stringp model)
                  model)
                 ((stringp (map-elt model :version))
                  (map-elt model :version))
                 (t
                  (error "Don't know how to resolve model to version %s"
                         chatgpt-shell-model-version)))))
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

;;;###autoload
(defun chatgpt-shell-swap-system-prompt ()
  "Swap system prompt from `chatgpt-shell-system-prompts'."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (when-let ((duplicates (chatgpt-shell-duplicate-map-keys chatgpt-shell-system-prompts)))
    (user-error "Duplicate prompt names found %s. Please remove" duplicates))
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
  (chatgpt-shell-interrupt nil)
  (chatgpt-shell--save-variables))

(declare-function pcsv-parse-file "pcsv" (file &optional coding-system))

;;;###autoload
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

;;;###autoload
(defun chatgpt-shell-version ()
  "Show `chatgpt-shell' mode version."
  (interactive)
  (message "chatgpt-shell v%s" chatgpt-shell--version))

(defun chatgpt-shell-reload-models ()
  "Reload all available models."
  (interactive)
  (setq chatgpt-shell-models
        (append (chatgpt-shell-openai-models)
                (chatgpt-shell-anthropic-models)
                (chatgpt-shell-google-models)))
  (message "Reloaded %d models" (length chatgpt-shell-models)))

(defun chatgpt-shell-swap-model ()
  "Swap model version from `chatgpt-shell-models'."
  (interactive)
  (if-let* ((last-label (chatgpt-shell--model-label))
            (all-models (seq-remove
                         (lambda (item)
                           (string-equal (map-elt item :version)
                                         (chatgpt-shell-model-version)))
                         chatgpt-shell-models))
            (width (let ((width))
                     (mapc (lambda (model)
                             (unless width
                               (setq width 0))
                             (when-let ((provider (map-elt model :provider))
                                        (provider-width (length (map-elt model :provider)))
                                        (longer (> provider-width width)))
                               (setq width provider-width)))
                           all-models)
                     width))
            (models (seq-map (lambda (model)
                               (format (format "%%-%ds   %%s" width)
                                       (map-elt model :provider)
                                       (map-elt model :version)))
                             all-models))
            (selection (nth 1 (string-split (completing-read "Model version: "
                                                             models nil t)))))
      (progn
        (when (derived-mode-p 'chatgpt-shell-mode)
          (setq-local chatgpt-shell-model-version selection)
          (chatgpt-shell--update-prompt t)
          (chatgpt-shell-interrupt nil)
          (unless (equal last-label (chatgpt-shell--model-label))
            (chatgpt-shell-clear-buffer)))
        (setq-default chatgpt-shell-model-version selection))
    (error "No other providers found")))

(defcustom chatgpt-shell-streaming t
  "Whether or not to stream ChatGPT responses (show chunks as they arrive)."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-highlight-blocks t
  "Whether or not to highlight source blocks."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-insert-dividers nil
  "Whether or not to display a divider between requests and responses."
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

(defcustom chatgpt-shell-welcome-function #'shell-maker-welcome-message
  "Function returning welcome message or nil for no message.

See `shell-maker-welcome-message' as an example."
  :type 'function
  :group 'chatgpt-shell)

(defvar chatgpt-shell--config
  (make-shell-maker-config
   :name "ChatGPT"
   :validate-command
   (lambda (command)
     (when-let* ((model (chatgpt-shell--resolved-model))
                 (validate-command (map-elt model :validate-command)))
       (funcall validate-command command)))
   :execute-command
   (lambda (command shell)
     (if-let* ((model (chatgpt-shell--resolved-model))
               (handler (map-elt model :handler)))
         (funcall handler
                  :model model
                  :command command
                  :context (chatgpt-shell-crop-context
                            :model model
                            :command command
                            :context (map-elt shell :history))
                  :shell shell
                  :settings (list (cons :streaming chatgpt-shell-streaming)
                                  (cons :temperature chatgpt-shell-model-temperature)
                                  (cons :system-prompt (chatgpt-shell-system-prompt))))
       (error "%s not found" (chatgpt-shell-model-version))))
   :on-command-finished
   (lambda (command output success)
     (chatgpt-shell--put-source-block-overlays)
     (run-hook-with-args 'chatgpt-shell-after-command-functions
                         command output success))
   :redact-log-output
   (lambda (output)
     (if-let ((key (map-elt (chatgpt-shell--resolved-model) :key)))
         (replace-regexp-in-string (regexp-quote (funcall key))
                                   "SK-REDACTED-PROVIDER-KEY"
                                   output)
       output))))

(defalias 'chatgpt-shell-explain-code #'chatgpt-shell-describe-code)

;; Aliasing enables editing as text in babel.
(defalias 'chatgpt-shell-mode #'text-mode)

(shell-maker-define-major-mode chatgpt-shell--config)

(cl-defun chatgpt-shell--resolved-model (&key versioned)
  "Resolve model VERSIONED name."
  (or (seq-find (lambda (model)
                  (equal (map-elt model :version)
                         (or versioned (chatgpt-shell-model-version))))
                chatgpt-shell-models)
      (error "Service %s not found"
             (chatgpt-shell-model-version))))

(cl-defun chatgpt-shell--make-payload (&key version context streaming temperature system-prompt)
  "Create a payload for model with VERSION.

Set CONTEXT, STREAMING, TEMPERATURE, and SYSTEM-PROMPT as usual."
  (let* ((model (chatgpt-shell--resolved-model :versioned version))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (payload (or (map-elt model :payload)
                      (error "Model :payload not found"))))
    (funcall payload
             :model model
             :context context
             :settings settings)))

;;;###autoload
(defun chatgpt-shell (&optional new-session)
  "Start a ChatGPT shell interactive command.

With NEW-SESSION, start a new session."
  (interactive "P")
  (chatgpt-shell-start nil new-session))

(defvar chatgpt-shell-mode-map (make-sparse-keymap)
  "Keymap for `chatgpt-shell-mode'.")

(defun chatgpt-shell-start (&optional no-focus new-session ignore-as-primary model-version system-prompt)
  "Start a ChatGPT shell programmatically.

Set NO-FOCUS to start in background.

Set NEW-SESSION to start a separate new session.

Set IGNORE-AS-PRIMARY to avoid making new buffer the primary one.

Set MODEL-VERSION to override variable `chatgpt-shell-model-version'.

Set SYSTEM-PROMPT to override variable `chatgpt-shell-system-prompt'"
  (let* ((chatgpt-shell--config
          (let ((config (copy-sequence chatgpt-shell--config))
                (chatgpt-shell-model-version (or model-version chatgpt-shell-model-version))
                (chatgpt-shell-system-prompt (or system-prompt chatgpt-shell-system-prompt)))
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
                             (if (and (chatgpt-shell--primary-buffer)
                                      (not ignore-as-primary))
                                 (buffer-name (chatgpt-shell--primary-buffer))
                               (chatgpt-shell--make-buffer-name))
                             "LLM")))
    (when (and (not ignore-as-primary)
               (not (chatgpt-shell--primary-buffer)))
      (chatgpt-shell--set-primary-buffer shell-buffer))
    (unless model-version
      (setq model-version chatgpt-shell-model-version))
    (unless system-prompt
      (setq system-prompt chatgpt-shell-system-prompt))
    (with-current-buffer shell-buffer
      (setq-local chatgpt-shell-model-version model-version)
      (setq-local chatgpt-shell-system-prompt system-prompt)
      (chatgpt-shell--update-prompt t)
      (chatgpt-shell--add-menus))
    (define-key chatgpt-shell-mode-map (kbd "C-M-h")
      #'chatgpt-shell-mark-at-point-dwim)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-c")
      #'chatgpt-shell-ctrl-c-ctrl-c)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-v")
      #'chatgpt-shell-swap-model)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-s")
      #'chatgpt-shell-swap-system-prompt)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-p")
      #'chatgpt-shell-previous-item)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-n")
      #'chatgpt-shell-next-item)
    (define-key chatgpt-shell-mode-map (kbd "C-c C-e")
      #'chatgpt-shell-prompt-compose)
    shell-buffer))

(defun chatgpt-shell--shrink-system-prompt (prompt)
  "Shrink PROMPT."
  (if (consp prompt)
      (chatgpt-shell--shrink-system-prompt (car prompt))
    (if (> (length (string-trim prompt)) 15)
        (format "%s..."
                (substring (string-trim prompt) 0 12))
      (string-trim prompt))))

(defun chatgpt-shell--shell-info ()
  "Generate shell info for display."
  (let* ((model (chatgpt-shell--resolved-model))
         (short-version (or (map-elt model :short-version)
                            (chatgpt-shell-model-version))))
    (concat
     short-version
     (cond ((and (integerp chatgpt-shell-system-prompt)
                 (nth chatgpt-shell-system-prompt
                      chatgpt-shell-system-prompts))
            (concat "/" (chatgpt-shell--shrink-system-prompt (nth chatgpt-shell-system-prompt
                                                                  chatgpt-shell-system-prompts))))
           ((stringp chatgpt-shell-system-prompt)
            (concat "/" (chatgpt-shell--shrink-system-prompt chatgpt-shell-system-prompt)))
           (t
            "")))))

(defun chatgpt-shell--model-label ()
  "Return the current model label."
  (or (map-elt (chatgpt-shell--resolved-model) :label)
      "Unknown"))

(defun chatgpt-shell--model-short-version ()
  "Return the current model short version."
  (or (map-elt (chatgpt-shell--resolved-model) :short-version)
      "unknown"))

(defun chatgpt-shell--prompt-pair ()
  "Return a pair with prompt and prompt-regexp."
  (let* ((label (chatgpt-shell--model-label)))
    (cons
     (format "%s(%s)> " label (chatgpt-shell--shell-info))
     (chatgpt-shell--prompt-regexp))))

(defun chatgpt-shell--prompt-regexp ()
  "Return a regexp to match any model prompt."
  (rx bol
      (one-or-more alphanumeric)
      "("
      (minimal-match (one-or-more not-newline))
      ")> "))

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
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (chatgpt-shell--set-primary-buffer (current-buffer)))

(defvar-local chatgpt-shell--is-primary-p nil
  "Non-nil if shell buffer is considered primary.")

(defun chatgpt-shell--set-primary-buffer (primary-shell-buffer)
  "Set PRIMARY-SHELL-BUFFER as primary buffer."
  (unless primary-shell-buffer
    (error "No primary shell available"))
  (mapc (lambda (shell-buffer)
          (with-current-buffer shell-buffer
            (setq chatgpt-shell--is-primary-p nil)))
        (chatgpt-shell--shell-buffers))
  (with-current-buffer primary-shell-buffer
    (setq chatgpt-shell--is-primary-p t)))

(defun chatgpt-shell--primary-buffer ()
  "Return the primary shell buffer.

This is used for sending a prompt to in the background."
  (let* ((shell-buffers (chatgpt-shell--shell-buffers))
         (primary-shell-buffer (seq-find
                                (lambda (shell-buffer)
                                  (with-current-buffer shell-buffer
                                    chatgpt-shell--is-primary-p))
                                shell-buffers)))
    (unless primary-shell-buffer
      (setq primary-shell-buffer
            (or (seq-first shell-buffers)
                (shell-maker-start chatgpt-shell--config
                                   t
                                   chatgpt-shell-welcome-function
                                   t
                                   (chatgpt-shell--make-buffer-name)
                                   "LLM")))
      (chatgpt-shell--set-primary-buffer primary-shell-buffer))
    primary-shell-buffer))

(defun chatgpt-shell--make-buffer-name ()
  "Generate a buffer name using current shell config info."
  (format "*%s llm* %s"
          (downcase (chatgpt-shell--model-label))
          (chatgpt-shell--shell-info)))

(defun chatgpt-shell--add-menus ()
  "Add ChatGPT shell menu items."
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (when-let ((duplicates (chatgpt-shell-duplicate-map-keys chatgpt-shell-system-prompts)))
    (user-error "Duplicate prompt names found %s. Please remove.?" duplicates))
  (easy-menu-define chatgpt-shell-system-prompts-menu (current-local-map) "ChatGPT"
    `("ChatGPT"
      ("Versions"
       ,@(mapcar (lambda (version)
                   `[,version
                     (lambda ()
                       (interactive)
                       (setq-local chatgpt-shell-model-version
                                   (seq-position chatgpt-shell-models ,version))
                       (chatgpt-shell--update-prompt t)
                       (chatgpt-shell-interrupt nil))])
                 chatgpt-shell-models))
      ("Prompts"
       ,@(mapcar (lambda (prompt)
                   `[,(car prompt)
                     (lambda ()
                       (interactive)
                       (setq-local chatgpt-shell-system-prompt
                                   (seq-position (map-keys chatgpt-shell-system-prompts) ,(car prompt)))
                       (chatgpt-shell--save-variables)
                       (chatgpt-shell--update-prompt t)
                       (chatgpt-shell-interrupt nil))])
                 chatgpt-shell-system-prompts))))
  (easy-menu-add chatgpt-shell-system-prompts-menu))

(defun chatgpt-shell--update-prompt (rename-buffer)
  "Update prompt and prompt regexp from `chatgpt-shell-models'.

Set RENAME-BUFFER to also rename the buffer accordingly."
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (shell-maker-set-prompt
   (car (chatgpt-shell--prompt-pair))
   (cdr (chatgpt-shell--prompt-pair)))
  (when rename-buffer
    (shell-maker-set-buffer-name
     (current-buffer)
     (chatgpt-shell--make-buffer-name))))

(defun chatgpt-shell-interrupt (ignore-item)
  "Interrupt `chatgpt-shell' from any buffer.

With prefix IGNORE-ITEM, do not mark as failed."
  (interactive "P")
  (with-current-buffer
      (cond
       ((derived-mode-p 'chatgpt-shell-mode)
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
      (when (derived-mode-p 'chatgpt-shell-mode)
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
                   (>= (point) start)
                   (< (point) end))
          (list (cons 'language language)
                (cons 'language-start language-start)
                (cons 'language-end language-end)
                (cons 'start start)
                (cons 'end end)))))))

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
(defvar chatgpt-shell--source-block-regexp
  (rx  bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
       (group (zero-or-more (or alphanumeric "-" "+" "#"))) ;; languages like: emacs-lisp C++ C#
       (zero-or-more whitespace)
       (one-or-more "\n")
       (group (*? anychar)) ;; body
       (one-or-more "\n")
       (group "```") (or "\n" eol)))

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
  (unless (derived-mode-p 'chatgpt-shell-mode)
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
  (unless (derived-mode-p 'chatgpt-shell-mode)
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

;; TODO: Move to shell-maker.
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

;; TODO: Move to shell-maker.
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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
  (let ((question (concat header "\n\n" (buffer-substring (region-beginning) (region-end)))))
    (chatgpt-shell-send-to-buffer question nil)))

;;;###autoload
(defun chatgpt-shell-refactor-code ()
  "Refactor code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-refactor-code))

;;;###autoload
(defun chatgpt-shell-write-git-commit ()
  "Write commit from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-write-git-commit))

;;;###autoload
(defun chatgpt-shell-generate-unit-test ()
  "Generate unit-test for the code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-region-with-header chatgpt-shell-prompt-header-generate-unit-test))

;;;###autoload
(defun chatgpt-shell-proofread-region ()
  "Proofread text from region using ChatGPT.

See `chatgpt-shell-prompt-header-proofread-region' to change prompt or language."
  (interactive)
  (chatgpt-shell-request-and-insert-merged-response
   :system-prompt chatgpt-shell-prompt-header-proofread-region
   :query ""))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun chatgpt-shell-send-region (review)
  "Send region to ChatGPT.
With prefix REVIEW prompt before sending to ChatGPT."
  (interactive "P")
  (unless (region-active-p)
    (user-error "No region active"))
  (let ((chatgpt-shell-prompt-query-response-style 'shell)
        (region-text (buffer-substring (region-beginning) (region-end))))
    (chatgpt-shell-send-to-buffer
     (if review
         (concat "\n\n" region-text)
       region-text) review)))

;;;###autoload
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
    (chatgpt-shell-post :context (list (cons prompt nil))
                        :streaming t
                        :on-output (lambda (output)
                                     (setq buffered (concat buffered output)))
                        :on-success (lambda (_output)
                                      (setq worker-done t))
                        :on-failure (lambda (_output)
                                      (setq worker-done t)))
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
    (if (derived-mode-p 'eshell-mode)
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
          (buffer-substring-no-properties cmd-start cmd-end))
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

;;;###autoload
(defun chatgpt-shell-add-??-command-to-eshell ()
  "Add `??' command to `eshell'."

  ;; Note: Must have "eshell/" prefix to be recognized as an eshell command.
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
            (load ,(find-library-name "chatgpt-shell-openai") nil t)
            (load ,(find-library-name "chatgpt-shell-google") nil t)
            (load ,(find-library-name "chatgpt-shell-anthropic") nil t)
            (load ,(find-library-name "chatgpt-shell") nil t)
            (setq chatgpt-shell-model-temperature 0)
            (setq chatgpt-shell-openai-key ,(chatgpt-shell-openai-key))
            (chatgpt-shell-command-line-from-prompt-file ,prompt-file)))
        "'"))))

  (add-hook 'eshell-post-command-hook
            (defun chatgpt-shell--eshell-post-??-execution ()
              (when (string-match "chatgpt-shell-command-line-from-prompt-file"
                                  (string-join eshell-last-arguments " "))
                (save-excursion
                  (save-restriction
                    (narrow-to-region (eshell-beginning-of-output)
                                      (eshell-end-of-output))
                    (chatgpt-shell--put-source-block-overlays))))))

  (require 'esh-cmd)

  (add-to-list 'eshell-complex-commands "??"))

(cl-defun chatgpt-shell-request-and-insert-response (&key query
                                                          (buffer (current-buffer))
                                                          model-version
                                                          system-prompt
                                                          streaming
                                                          start
                                                          end)
  "Send a contextless request (no history) with:

QUERY: Request query text.
BUFFER (optional): Buffer to insert to or omit to insert to current buffer.
MODEL-VERSION (optional): Index from `chatgpt-shell-models' or string.
SYSTEM-PROMPT (optional): As string.
STREAMING (optional): Non-nil to stream insertion.
START (optional): Beginning of region to replace (overrides active region).
END (optional): End of region to replace (overrides active region)."
  (let* ((point (point))
         (delete-text (or
                       (and start end)
                       (region-active-p)))
         (delete-from (when delete-text
                        (or start (region-beginning))))
         (delete-to (when delete-text
                      (or end (region-end))))
         (marker (if delete-text
                     (copy-marker (max delete-from delete-to))
                   (copy-marker (point))))
         (progress-reporter (unless streaming
                              (make-progress-reporter
                               (format "%s " (chatgpt-shell--model-label))))))
    (chatgpt-shell-send-contextless-request
     :model-version model-version
     :system-prompt system-prompt
     :query query
     :streaming t
     :on-output (lambda (output)
                    (if streaming
                        (progn
                          (with-current-buffer buffer
                            (when delete-text
                              (deactivate-mark)
                              (delete-region delete-from delete-to)
                              (setq delete-text nil))
                            (save-excursion
                              (goto-char marker)
                              (insert output)
                              (set-marker marker (+ (length output)
                                                    (marker-position marker))))))
                      (progn
                        (progress-reporter-update progress-reporter))))
     :on-success (lambda (output)
                   (if streaming
                        (with-current-buffer buffer
                          (goto-char point))
                      (progress-reporter-done progress-reporter)
                      (with-current-buffer buffer
                        (when delete-text
                          (deactivate-mark)
                          (delete-region delete-from delete-to)
                          (setq delete-text nil))
                        (save-excursion
                          (goto-char marker)
                          (insert output))
                        (goto-char point))))
     :on-failure (lambda (output)
                   (if streaming
                       (with-current-buffer buffer
                         (goto-char point))
                     (progress-reporter-done progress-reporter))
                   (when (not (string-empty-p (string-trim
                                               (or output ""))))
                     (message (or output "failed")))))))

;; TODO: Review. Can it become service agnostic?
(cl-defun chatgpt-shell-send-contextless-request
    (&key model-version
          system-prompt
          query
          streaming
          on-output
          on-success
          on-failure)
  "Send a request with:

QUERY: Request query text.
ON-OUTPUT: Of the form (lambda (output))
ON-FINISHED: Of the form (lambda (success))
MODEL-VERSION (optional): Index from `chatgpt-shell-models' or string.
SYSTEM-PROMPT (optional): As string.
STREAMING (optional): non-nil to received streamed ON-OUTPUT events."
  (unless query
    (error "Missing mandatory \"query\" param"))
  (chatgpt-shell-post :context (list (cons query nil))
                      :system-prompt system-prompt
                      :version model-version
                      :streaming streaming
                      :on-output on-output
                      :on-success on-success
                      :on-failure on-failure))

(defun chatgpt-shell-send-to-buffer (input &optional review handler on-finished)
  "Send INPUT to *chatgpt* shell buffer.

Set REVIEW to make changes before submitting to ChatGPT.

If HANDLER function is set, ignore `chatgpt-shell-prompt-query-response-style',
and is of the form:

  (lambda (input output error finished))

ON-FINISHED is invoked when the entire interaction is finished and of the form:

  (lambda (input output success))."
  (if (eq chatgpt-shell-prompt-query-response-style 'other-buffer)
      (let ((buffer (chatgpt-shell-prompt-compose-show-buffer input)))
        (unless review
          (with-current-buffer buffer
            (chatgpt-shell-prompt-compose-send-buffer))))
    (let* ((response-style chatgpt-shell-prompt-query-response-style)
           (buffer (cond (handler
                          nil)
                         ((eq response-style 'inline)
                          (current-buffer))
                         (t
                          nil)))
           (marker (copy-marker (point)))
           (orig-region-active (region-active-p))
           (region-beginning (when orig-region-active
                               (region-beginning)))
           (region-end (when orig-region-active
                         (region-end)))
           (no-focus (or (eq response-style 'inline)
                         handler)))
      (when (region-active-p)
        (setq marker (copy-marker (max (region-beginning)
                                       (region-end)))))
      (if (chatgpt-shell--primary-buffer)
          (with-current-buffer (chatgpt-shell--primary-buffer)
            (chatgpt-shell-start no-focus))
        (chatgpt-shell-start no-focus t))
      (cl-flet ((send ()
                  (when shell-maker--busy
                    (shell-maker-interrupt nil))
                  (if review
                      (save-excursion
                        (goto-char (point-max))
                        (insert input))
                    (shell-maker-submit
                     :input input
                     :on-output (lambda (output)
                                  (setq output (or output ""))
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer buffer
                                      (let ((inhibit-read-only t))
                                        (save-excursion
                                          (when orig-region-active
                                            (delete-region region-beginning region-end)
                                            (setq orig-region-active nil))
                                          (goto-char marker)
                                          (insert output)
                                          (set-marker marker (+ (length output)
                                                                (marker-position marker))))))))
                     :on-finished (lambda (input output success)
                                    (when on-finished
                                      (funcall on-finished input output success)))))))
        (if (or (eq response-style 'inline)
                handler)
            (with-current-buffer (chatgpt-shell--primary-buffer)
              (goto-char (point-max))
              (send))
          (with-selected-window (get-buffer-window (chatgpt-shell--primary-buffer))
            (send)))))))

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

;;;###autoload
(defun chatgpt-shell-japanese-ocr-lookup ()
  "Select a region of the screen to OCR and look up in Japanese."
  (interactive)
  (let* ((term)
         (process (start-process "macosrec-ocr" nil "macosrec" "--ocr")))
    (if (memq window-system '(mac ns))
        (unless (executable-find "macosrec")
          (user-error "You need \"macosrec\" installed: brew install xenodium/macosrec/macosrec"))
      (user-error "Not yet supported on %s (please send a pull request)" window-system))
    (set-process-filter process (lambda (_proc text)
                                  (setq term (concat term text))))
    (set-process-sentinel process (lambda (_proc event)
                                    (when (string= event "finished\n")
                                      (chatgpt-shell-japanese-lookup term))))))

;;;###autoload
(defun chatgpt-shell-japanese-audio-lookup ()
  "Transcribe audio at current file (buffer or `dired') and look up in Japanese."
  (interactive)
  (let* ((term)
         (file (chatgpt-shell--current-file))
         (extension (downcase (file-name-extension file)))
         (process (start-process "macosrec-speechrec" nil "macosrec"
                                 "--speech-to-text" "--locale" "ja-JP" "--input" file)))
    (if (memq window-system '(mac ns))
        (unless (executable-find "macosrec")
          (user-error "You need \"macosrec\" installed: brew install xenodium/macosrec/macosrec"))
      (user-error "Not yet supported on %s (please send a pull request)" window-system))
    (unless (seq-contains-p '("mp3" "wav" "m4a" "caf") extension)
      (user-error "Must be using either .mp3, .m4a, .caf or .wav"))
    (set-process-filter process (lambda (_proc text)
                                  (setq term (concat term text))))
    (set-process-sentinel process (lambda (_proc event)
                                    (when (string= event "finished\n")
                                      (chatgpt-shell-japanese-lookup term))))))

(defun chatgpt-shell-japanese-lookup (&optional capture)
  "Look Japanese term up.

Ask user for term.  Alternatively, if point is on an image, extract Japanese
data from the image.

If command invoked with prefix, CAPTURE a screenshot.

Display result in org table of the form:

| Hiragana | Katakana | Kanji | Romaji | English | #Tags |
|----------+----------+-------+--------+---------+-------|
|          |          |       |        |         |       |"
  (interactive "P")
  (let* ((file (chatgpt-shell--current-image-file capture))
         (term (unless file
                 (chatgpt-shell--read-string :prompt "Japanese lookup: ")))
         (buffer "*Japanese lookup*"))
    (chatgpt-shell-lookup :buffer buffer
                          :prompt (if file
                                      "Use for this image to extract each row data."
                                    (format "Fill out a row for \"%s\"" term))
                          :on-success (lambda (_output)
                                        (with-current-buffer buffer
                                          (goto-char (point-min))
                                          (org-mode)
                                          (let ((inhibit-read-only t))
                                            (goto-char (point-min))
                                            (org-table-align)
                                            (when (fboundp 'valign-mode)
                                              (valign-mode +1))
                                            (visual-line-mode -1))))
                          :prompt-url file
                          :streaming t
                          :system-prompt "
1. Fill out an org mode table using this format as an example:

| Hiragana      | Katakana | Kanji | Romaji            | English | Tags              |
|---------------+----------+-------+-------------------+---------+-------------------|
| おなかすきました |          | 空腹   | onaka sukimashita | hungry  | #describe #myself |

2. ALWAYS Fill out Hiragana when appropriate.
3. ALWAYS Fill out Katakana when appropriate.
4. ALWAYS Fill out Kanji when appropriate.
5. Show long romaji vowels (i.e ō).
6. Ensure columns align.
7. Do NOT wrap anything in Markdown source blocks.
8. Do NOT add any text or explanations outside the org table.")))

;; TODO: Make service agnostic.
(cl-defun chatgpt-shell-lookup (&key buffer system-prompt prompt prompt-url streaming
                                     temperature on-success on-failure)
  "Look something up as a one-off (no shell history) and output to BUFFER.

Inputs:

Either (or all) of SYSTEM-PROMPT, PROMPT, PROMPT-URL (image file).

Optionally:

STREAMING: Non-nil streams output to BUFFER.

TEMPERATURE: Defaults to 1 otherwise.

ON-SUCCESS: (lambda (output)) for completion event.

ON-FAILURE: (lambda (output)) for completion event."
  (setq buffer (get-buffer-create buffer))
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer))
    (use-local-map (let ((map (make-sparse-keymap)))
                     (define-key map (kbd "q") 'kill-buffer-and-window)
                     map))
    (setq buffer-read-only t))
  (let* ((model (chatgpt-shell--resolved-model))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (url (funcall (map-elt model :url)
                       :model model
                       :settings settings))
         (headers (funcall (map-elt model :headers)
                           :model model
                           :settings settings)))
    (when streaming
      (display-buffer buffer))
    (unless (equal (map-elt model :provider) "OpenAI")
      (error "%s's %s not yet supported (please sponsor development)"
             (map-elt model :provider) (map-elt model :version)))
    (shell-maker-make-http-request
     :async t
     :url url
     :data (chatgpt-shell-openai-make-chatgpt-request-data
            :prompt prompt
            :prompt-url prompt-url
            :system-prompt system-prompt
            :version (map-elt model :version)
            :temperature (or temperature 1)
            :streaming streaming)
     :headers headers
     :filter #'chatgpt-shell-openai--filter-output
     :on-output
     (lambda (output)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert output))))
     :on-finished
     (lambda (result)
       (unless streaming
         (display-buffer buffer))
       (if (equal 0 (map-elt result :exit-status))
           (when on-success
             (funcall on-success (map-elt result :output)))
         (when on-failure
           (funcall on-failure (map-elt result :output))))))))

(cl-defun chatgpt-shell--read-string (&key prompt default-value)
  "Like `read-string' but disallowing empty input (unless DEFAULT-VALUE given).

Specify PROMPT to signal the user."
  (let ((input ""))
    (while (string-empty-p (string-trim input))
      (setq input (read-string (or prompt "")
                               (when (use-region-p)
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end)))
                               nil nil default-value))
      (when (string-empty-p (string-trim input))
        (setq input default-value)))
    input))

(cl-defun chatgpt-shell-post (&key context
                                   version
                                   system-prompt
                                   on-output
                                   on-success on-failure
                                   temperature streaming)
  "Make a single ChatGPT request with MESSAGES and FILTER.

`chatgpt-shell-openai--filter-output' typically used as extractor.

Optionally pass model VERSION, TEMPERATURE and OTHER-PARAMS.

OTHER-PARAMS are appended to the json object at the top level.

CONTEXT: A list of cons of the form: (command . response).

ON-OUTPUT: (lambda (output))

ON-SUCCESS: (lambda (output))

ON-FAILURE: (lambda (output))

If ON-FINISHED, ON-SUCCESS, and ON-FINISHED are missing, execute synchronously."
  (unless context
    (error "Missing mandatory \"context\" param"))
  (let* ((model (chatgpt-shell--resolved-model :versioned version))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (url (funcall (map-elt model :url)
                       :model model
                       :settings settings))
         (payload (or (map-elt model :payload)
                      (error "Model :payload not found")))
         (filter (or (map-elt model :filter)
                     (error "Model :filter not found")))
         (headers (map-elt model :headers)))
    (if (or on-output on-success on-failure)
        (progn
          (unless (boundp 'shell-maker--current-request-id)
            (setq-local shell-maker--current-request-id 0))
          ;; TODO: Remove the need to create a temporary
          ;; shell configuration when invoking `shell-maker-make-http-request'.
          (with-temp-buffer
            (setq-local shell-maker--config
                        chatgpt-shell--config)
            ;; Async exec
            (shell-maker-make-http-request
             :async t
             :url url
             :data (funcall payload
                            :model model
                            :context context
                            :settings settings)
             :headers (funcall headers
                               :model model
                               :settings settings)
             :filter filter
             :on-output on-output
             :on-finished (lambda (result)
                            (if (equal 0 (map-elt result :exit-status))
                                (when on-success
                                  (funcall on-success (map-elt result :output)))
                              (when on-failure
                                (funcall on-failure (map-elt result :output))))))))
      ;; Sync exec
      (let ((result
             (shell-maker-make-http-request
              :async nil ;; Block to return result
              :url url
              :data (funcall payload
                             :model model
                             :context context
                             :settings settings)
              :headers (funcall headers
                                :model model
                                :settings settings)
              :filter filter)))
        (map-elt result :output)))))

;;;###autoload
(defun chatgpt-shell-describe-image (&optional capture)
  "Request OpenAI to describe image.

When visiting a buffer with an image, send that.

If command invoked with prefix, CAPTURE a screenshot.

If in a `dired' buffer, use selection (single image only for now)."
  (interactive "P")
  (let ((file (chatgpt-shell--current-image-file capture))
        (description-buffer (get-buffer-create "*chatgpt image description*"))
        (prompt (chatgpt-shell--read-string
                 :prompt "Describe image (default \"What's in this image?\"): "
                 :default-value "What's in this image?")))
    (unless file
      (error "No image found"))
    (message "Requesting...")
    (chatgpt-shell-lookup :buffer description-buffer
                          :prompt prompt
                          :on-success (lambda (output)
                                        (with-current-buffer description-buffer
                                          (let ((inhibit-read-only t))
                                            (erase-buffer)
                                            (insert output)
                                            (use-local-map (let ((map (make-sparse-keymap)))
                                                             (define-key map (kbd "q") 'kill-buffer-and-window)
                                                             map)))
                                          (message "Image description ready")
                                          (read-only-mode +1))
                                        (display-buffer description-buffer))
                          :prompt-url file
                          :streaming nil)))

(defun chatgpt-shell--current-image-file (&optional capture)
  "Return buffer image file, Dired selected file, or image at point.

If optional CAPTURE is non-nil, cature a screenshot."
  (when (or (and (use-region-p) (derived-mode-p 'image-mode))
            (and (use-region-p) (derived-mode-p 'dired-mode)))
    (user-error "No region selection supported"))
  (cond (capture
         (redisplay) ;; Call process will block. Give redisplay a chance.
         (when-let ((file (make-temp-file "screenshot" nil ".png"))
                    ;; TODO: Make screenshot utility configurable.
                    (success (eq 0 (call-process "/usr/sbin/screencapture" nil nil nil "-i" file)))
                    (found (file-exists-p file))
                    (written (not (zerop (nth 7 (file-attributes file))))))
           file))
        ((derived-mode-p 'image-mode)
         (buffer-file-name))
        ((derived-mode-p 'dired-mode)
         (let* ((dired-files (dired-get-marked-files))
                (file (seq-first dired-files)))
           (when (> (length dired-files) 1)
             (user-error "Only one file selection supported"))
           file))
        (t
         (when-let* ((image (cdr (get-text-property (point) 'display)))
                     (image-file (cond ((plist-get image :file)
                                        (plist-get image :file))
                                       ((plist-get image :data)
                                        (ignore-errors
                                          (delete-file (chatgpt-shell--image-request-file)))
                                        (with-temp-file (chatgpt-shell--image-request-file)
                                          (set-buffer-multibyte nil)
                                          (insert (plist-get image :data)))
                                        (chatgpt-shell--image-request-file)))))
           image-file))))

(defun chatgpt-shell--current-file ()
  "Return buffer file, Dired selected file, or image at point."
  (when (use-region-p)
    (user-error "No region selection supported"))
  (cond ((buffer-file-name)
         (buffer-file-name))
        ((derived-mode-p 'dired-mode)
         (let* ((dired-files (dired-get-marked-files))
                (file (seq-first dired-files)))
           (unless dired-files
             (user-error "No file selected"))
           (when (> (length dired-files) 1)
             (user-error "Only one file selection supported"))
           file))
        (t
         (user-error "Nothing found to work on"))))

(defun chatgpt-shell--make-chatgpt-url (url)
  "Create ChatGPT message content for URL."
  (unless url
    (error "URL missing"))
  (let* ((extension (downcase (file-name-extension url)))
         (name (file-name-nondirectory url)))
    (unless (string-prefix-p "http" url)
      (unless (file-exists-p url)
        (error "File not found"))
      (unless (or (seq-contains-p '("jpg" "jpeg" "png" "webp" "gif") extension)
                  (equal name "image.request"))
        (user-error "Image must be either .jpg, .jpeg, .png, .webp or .gif"))
      (setq url (concat "data:image/jpeg;base64,"
                        (with-temp-buffer
                          (insert-file-contents-literally url)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string)))))
    url))

(defun chatgpt-shell--temp-dir ()
  "Get chatgpt-shell's temp directory."
  (let ((temp-dir (file-name-concat temporary-file-directory "chatgpt-shell")))
    (make-directory temp-dir t)
    temp-dir))

(defun chatgpt-shell--json-request-file ()
  "JSON request written to this file prior to sending."
  (file-name-concat (chatgpt-shell--temp-dir) "request.json"))

(defun chatgpt-shell--image-request-file ()
  "Image written to this file prior to sending."
  (file-name-concat (chatgpt-shell--temp-dir) "image.request"))

(cl-defun chatgpt-shell--write-json-request-file (&key data)
  "Encode and write DATA as json to request file."
  (unless data
    (error "Missing mandatory \"data\" param"))
  (with-temp-file (chatgpt-shell--json-request-file)
    (setq-local coding-system-for-write 'utf-8)
    (insert (shell-maker--json-encode data))))

;; TODO: replace all chatgpt-shell-crop-context
(cl-defun chatgpt-shell-crop-context (&key context model command)
  "Crop CONTEXT to fit MODEL limits appending COMMAND."
  (unless model
    (error "Missing mandatory \"model\" param"))
  ;; Temporarily appending command for context
  ;; calculation and removing with butlast.
  (let ((complete-context (append context
                                  (list (cons command nil)))))
    (butlast
     (last complete-context
           (chatgpt-shell--unpaired-length
            (if (functionp chatgpt-shell-transmitted-context-length)
                (funcall chatgpt-shell-transmitted-context-length
                         model complete-context)
              chatgpt-shell-transmitted-context-length)))
     1)))

(defun chatgpt-shell--approximate-context-length (model context)
  "Approximate the CONTEXT length using MODEL."
  (let* ((approx-chars-per-token)
         (context-window)
         (original-length (floor (/ (length context) 2)))
         (context-length original-length))
    (if (map-elt model :token-width)
        (setq approx-chars-per-token
              (map-elt model :token-width))
      (error "Don't know %s's approximate token width" model))
    (if (map-elt model :context-window)
        (setq context-window
              (map-elt model :context-window))
      (error "Don't know %s's max tokens context limit" model))
    (while (> (chatgpt-shell--num-tokens-from-context
               approx-chars-per-token context)
              context-window)
      ;; Keep dropping history until under context-window.
      (setq context (cdr context)))
    (setq context-length (floor (/ (length context) 2)))
    (unless (eq original-length context-length)
      (message "Warning: chatgpt-shell context clipped"))
    context-length))

;; Very rough token approximation loosely based on num_tokens_from_messages from:
;; https://github.com/openai/openai-cookbook/blob/main/examples/How_to_count_tokens_with_tiktoken.ipynb
(defun chatgpt-shell--num-tokens-from-context (chars-per-token context)
  "Approximate number of tokens in CONTEXT using approximate CHARS-PER-TOKEN."
  (let ((num-tokens 0))
    (dolist (message context)
      (setq num-tokens (+ num-tokens chars-per-token))
      (setq num-tokens (+ num-tokens (/ (length (cdr message)) chars-per-token))))
    ;; Every reply is primed with <|start|>assistant<|message|>
    (setq num-tokens (+ num-tokens 3))
    num-tokens))

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-source-block (quotes1-start quotes1-end lang
lang-start lang-end body-start body-end quotes2-start quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Overlay beginning "```" with a copy block button.
  (chatgpt-shell--overlay-put-all
   (make-overlay quotes1-start quotes1-end)
   'evaporate t
   'display
   (propertize "📋 "
               'pointer 'hand
               'keymap (shell-maker--make-ret-binding-map
                        (lambda ()
                          (interactive)
                          (kill-ring-save body-start body-end)
                          (message "Copied")))))
  ;; Hide end "```" altogether.
  (chatgpt-shell--overlay-put-all
   (make-overlay quotes2-start quotes2-end)
   'evaporate t
   'invisible 'chatgpt-shell)
  (unless (eq lang-start lang-end)
    (chatgpt-shell--overlay-put-all
     (make-overlay lang-start lang-end)
     'evaporate t
     'face '(:box t))
    (chatgpt-shell--overlay-put-all
     (make-overlay lang-end (1+ lang-end))
     'evaporate t
     'display "\n\n"))
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
            (chatgpt-shell--overlay-put-all
             overlay
             'evaporate t
             'face (plist-get props 'face))
            (setq pos (1+ pos))))
      (chatgpt-shell--overlay-put-all
       (make-overlay body-start body-end buf)
       'evaporate t
       'face 'font-lock-doc-markup-face))))

(defun chatgpt-shell--overlay-put-all (overlay &rest props)
  "Set multiple properties on OVERLAY via PROPS."
  (unless (= (mod (length props) 2) 0)
    (error "Props missing a property or value"))
  (while props
    (overlay-put overlay (pop props) (pop props))))

(defun chatgpt-shell--fontify-divider (start end)
  "Display text between START and END as a divider."
  (chatgpt-shell--overlay-put-all
   (make-overlay start end
                 (if (and (boundp 'shell-maker--config)
                          shell-maker--config)
                     (shell-maker-buffer shell-maker--config)
                   (current-buffer)))
   'evaporate t
   'display
   (concat (propertize (concat (make-string (window-body-width) ? ) "")
                       'face '(:underline t)) "\n")))

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-link (start end title-start title-end url-start url-end)
  "Fontify a markdown link.
Use START END TITLE-START TITLE-END URL-START URL-END."
  ;; Hide markup before
  (chatgpt-shell--overlay-put-all
   (make-overlay start title-start)
   'evaporate t
   'invisible 'chatgpt-shell)
  ;; Show title as link
  (chatgpt-shell--overlay-put-all
   (make-overlay title-start title-end)
   'evaporate t
   'face 'link)
  ;; Make RET open the URL
  (define-key (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1]
                            (lambda () (interactive)
                              (browse-url (buffer-substring-no-properties url-start url-end))))
                (define-key map (kbd "RET")
                            (lambda () (interactive)
                              (browse-url (buffer-substring-no-properties url-start url-end))))
                (chatgpt-shell--overlay-put-all
                 (make-overlay title-start title-end)
                 'evaporate t
                 'keymap map)
                map)
              [remap self-insert-command] 'ignore)
  ;; Hide markup after
  (chatgpt-shell--overlay-put-all
   (make-overlay title-end end)
   'evaporate t
   'invisible 'chatgpt-shell))

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-bold (start end text-start text-end)
  "Fontify a markdown bold.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (chatgpt-shell--overlay-put-all
   (make-overlay start text-start)
   'evaporate t
   'invisible 'chatgpt-shell)
  ;; Show title as bold
  (chatgpt-shell--overlay-put-all
   (make-overlay text-start text-end)
   'evaporate t
   'face 'bold)
  ;; Hide markup after
  (chatgpt-shell--overlay-put-all
   (make-overlay text-end end)
   'evaporate t
   'invisible 'chatgpt-shell))

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-header (start _end level-start level-end title-start title-end)
  "Fontify a markdown header.
Use START END LEVEL-START LEVEL-END TITLE-START TITLE-END."
  ;; Hide markup before
  (chatgpt-shell--overlay-put-all
   (make-overlay start title-start)
   'evaporate t
   'invisible 'chatgpt-shell)
  ;; Show title as header
  (chatgpt-shell--overlay-put-all
   (make-overlay title-start title-end)
   'evaporate t
   'face
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

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-italic (start end text-start text-end)
  "Fontify a markdown italic.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (chatgpt-shell--overlay-put-all
   (make-overlay start text-start)
   'evaporate t
   'invisible 'chatgpt-shell)
  ;; Show title as italic
  (chatgpt-shell--overlay-put-all
   (make-overlay text-start text-end)
   'evaporate t
   'face 'italic)
  ;; Hide markup after
  (chatgpt-shell--overlay-put-all
   (make-overlay text-end end)
   'evaporate t
   'invisible 'chatgpt-shell))

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-strikethrough (start end text-start text-end)
  "Fontify a markdown strikethrough.
Use START END TEXT-START TEXT-END."
  ;; Hide markup before
  (chatgpt-shell--overlay-put-all
   (make-overlay start text-start)
   'evaporate t
   'invisible 'chatgpt-shell)
  ;; Show title as strikethrough
  (chatgpt-shell--overlay-put-all
   (make-overlay text-start text-end)
   'evaporate t
   'face '(:strike-through t))
  ;; Hide markup after
  (chatgpt-shell--overlay-put-all
   (make-overlay text-end end)
   'evaporate t
   'invisible 'chatgpt-shell))

;; TODO: Move to shell-maker.
(defun chatgpt-shell--fontify-inline-code (body-start body-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Hide ```
  (chatgpt-shell--overlay-put-all
   (make-overlay (1- body-start)
                 body-start)
   'evaporate t
   'invisible 'chatgpt-shell)
  (chatgpt-shell--overlay-put-all
   (make-overlay body-end
                 (1+ body-end))
   'evaporate t
   'invisible 'chatgpt-shell)
  (chatgpt-shell--overlay-put-all
   (make-overlay body-start body-end
                 (if (and (boundp 'shell-maker--config)
                          shell-maker--config)
                     (shell-maker-buffer shell-maker--config)
                   (current-buffer)))
   'evaporate t

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

;; TODO: Move to shell-maker.
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
      (when chatgpt-shell-insert-dividers
        (dolist (divider (shell-maker--prompt-end-markers))
          (chatgpt-shell--fontify-divider (car divider) (cdr divider))))
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

;; TODO: Move to shell-maker.
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
  (unless (derived-mode-p 'chatgpt-shell-mode)
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
      (let* ((items (chatgpt-shell-openai--user-assistant-messages
                     (list (shell-maker--command-and-response-at-point))))
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
  (chatgpt-shell-openai--user-assistant-messages
   (shell-maker--extract-history text prompt-regexp)))

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

;; TODO: Move to shell-maker.
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

(defun chatgpt-shell--save-variables ()
  "Save variables across Emacs sessions."
  (setq-default chatgpt-shell-system-prompt
                chatgpt-shell-system-prompt)
  (with-temp-file (concat user-emacs-directory ".chatgpt-shell.el")
    (prin1 (list
            (cons 'chatgpt-shell-system-prompt chatgpt-shell-system-prompt)
            (cons 'chatgpt-shell-system-prompt-resolved
                  (when (integerp chatgpt-shell-system-prompt)
                    (nth chatgpt-shell-system-prompt
                         chatgpt-shell-system-prompts)))) (current-buffer))))

(with-eval-after-load 'chatgpt-shell
  (chatgpt-shell--load-variables))

(defun chatgpt-shell--load-variables ()
  "Load variables across Emacs sessions."
  (with-temp-buffer
    (condition-case nil
      ;; Try to insert the contents of .chatgpt-shell.el
      (insert-file-contents (concat user-emacs-directory ".chatgpt-shell.el"))
      (error
        ;; If an error happens, execute chatgpt-shell--save-variables
        (chatgpt-shell--save-variables)))
    (goto-char (point-min))
    (let ((vars (read (current-buffer))))
      (when (and (map-elt vars 'chatgpt-shell-system-prompt)
                 (map-elt vars 'chatgpt-shell-system-prompt-resolved)
                 (equal (map-elt vars 'chatgpt-shell-system-prompt-resolved)
                        (nth (map-elt vars 'chatgpt-shell-system-prompt)
                             chatgpt-shell-system-prompts)))
        (setq chatgpt-shell-system-prompt (map-elt vars 'chatgpt-shell-system-prompt))))))

(defun chatgpt-shell--flymake-context ()
  "Return flymake diagnostic context if available.  Nil otherwise."
  (when-let* ((point (point))
              (diagnostic (flymake-diagnostics (point)))
              (line-start (line-beginning-position))
              (line-end (line-end-position))
              (top-context-start (max (line-beginning-position -5) (point-min)))
              (top-context-end (max (line-beginning-position 1) (point-min)))
              (bottom-context-start (min (line-beginning-position 2) (point-max)))
              (bottom-context-end (min (line-beginning-position 7) (point-max)))
              (current-line (buffer-substring line-start line-end)))
    (list
     (cons :point (point))
     (cons :start top-context-start)
     (cons :end bottom-context-end)
     (cons :diagnostic (mapconcat #'flymake-diagnostic-text diagnostic "\n"))
     (cons :content (concat
                     (buffer-substring-no-properties top-context-start top-context-end)
                     (buffer-substring-no-properties line-start line-end)
                     " <--- issue is here\n"
                     (buffer-substring-no-properties bottom-context-start bottom-context-end))))))

(when-let ((flymake-context (chatgpt-shell--flymake-context)))
  (set-mark (map-elt flymake-context :start))
  (goto-char (map-elt flymake-context :end)))

;;;###autoload
(defun chatgpt-shell-fix-error-at-point ()
  "Fixes flymake error at point."
  (interactive)
  (if-let ((flymake-context (chatgpt-shell--flymake-context))
           (progress-reporter (make-progress-reporter
                               (format "%s " (chatgpt-shell--model-label))))
           (buffer (current-buffer))
           (prog-mode-p (derived-mode-p 'prog-mode)))
      (progn
        (chatgpt-shell--fader-start-fading-region
         (save-excursion
           (goto-char (map-elt flymake-context :point))
           (line-beginning-position))
         (save-excursion
           (goto-char (map-elt flymake-context :point))
           (line-end-position)))
        (progress-reporter-update progress-reporter)
        (chatgpt-shell-send-contextless-request
         :system-prompt "Fix the error highlighted in code and show the entire snippet rewritten with the fix.
Do not give explanations. Do not add comments.
Do not balance unbalanced brackets or parenthesis at beginning or end of text.
Do not wrap snippets in markdown blocks.\n\n"
         :query (concat (map-elt flymake-context :diagnostic) "\n\n"
                        "Code: \n\n"
                        (map-elt flymake-context :content))
         :streaming t
         :on-output (lambda (_chunk)
                      (progress-reporter-update progress-reporter))
         :on-success (lambda (output)
                       (with-current-buffer buffer
                         ;; In prog mode, remove unnecessary
                         ;; markdown blocks prior to insertion.
                         (when prog-mode-p
                           (setq output
                                 (chatgpt-shell--remove-source-block-markers output)))
                         (chatgpt-shell--fader-stop-fading)
                         (progress-reporter-done progress-reporter)
                         (chatgpt-shell--pretty-smerge-insert
                          :text output
                          :start (map-elt flymake-context :start)
                          :end (map-elt flymake-context :end)
                          :buffer buffer)))
         :on-failure (lambda (output)
                       (with-current-buffer buffer
                         (chatgpt-shell--fader-stop-fading)
                         (progress-reporter-done progress-reporter)
                         (when (not (string-empty-p (string-trim
                                                     (or output ""))))
                           (message (or output "failed")))))))
    (error "Nothing to fix")))

(cl-defun chatgpt-shell-request-and-insert-merged-response (&key query
                                                                 (buffer (current-buffer))
                                                                 model-version
                                                                 system-prompt
                                                                 remove-block-markers)
  "Send a contextless request (no history) and merge into BUFFER:

QUERY: Request query text.
BUFFER (optional): Buffer to insert to or omit to insert to current buffer.
MODEL-VERSION (optional): Index from `chatgpt-shell-models' or string.
SYSTEM-PROMPT (optional): As string."
  (unless query
    (error "Missing mandatory \"query\" param"))
  (unless (region-active-p)
    (error "No region selected"))
  (let ((start (save-excursion
                 ;; Always select from beginning of line.
                 (goto-char (region-beginning))
                 (line-beginning-position)))
        (end (region-end))
        (progress-reporter (make-progress-reporter
                            (format "%s " (chatgpt-shell--model-label)))))
    ;; Barf trailing space from selection.
    (when (string-match "[ \n\t]+$"
                        (buffer-substring-no-properties
                         start
                         end))
      (setq end (- end (length (match-string 0)))))
    (setq query (concat query "\n\n" (buffer-substring start end)))
    (chatgpt-shell--fader-start-fading-region start end)
    (progress-reporter-update progress-reporter)
    (chatgpt-shell-send-contextless-request
     :query query
     :model-version model-version
     :system-prompt system-prompt
     :streaming t
     :on-output (lambda (_chunk)
                    (progress-reporter-update progress-reporter))
     :on-success (lambda (output)
                   (with-current-buffer buffer
                     (when remove-block-markers
                       (setq output (chatgpt-shell--remove-source-block-markers output)))
                     (chatgpt-shell--fader-stop-fading)
                     (progress-reporter-done progress-reporter)
                     (chatgpt-shell--pretty-smerge-insert
                      :text output
                      :start start
                      :end end
                      :buffer buffer)))
     :on-failure (lambda (output)
                   (with-current-buffer buffer
                     (chatgpt-shell--fader-stop-fading)
                     (progress-reporter-done progress-reporter)
                     (when (not (string-empty-p (string-trim
                                                 (or output ""))))
                       (message (or output "failed"))))))))

(defun chatgpt-shell--remove-source-block-markers (text)
  "Remove markdown code block markers TEXT."
  (replace-regexp-in-string
   (rx (optional "\n") bol "```" (zero-or-more (or alphanumeric "-" "+"))
       (zero-or-more space) eol (optional "\n"))
   "" text t))

(defvar-local chatgpt-shell-prompt-compose--exit-on-submit nil
  "Whether or not compose buffer should close after submission.

This is typically used to craft prompts and immediately jump over to
the shell to follow the response.")

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
(defun chatgpt-shell-quick-insert()
  "Request from minibuffer and insert response into current buffer."
  (interactive)
  (let ((system-prompt "Follow my instruction and only my instruction.
Do not explain nor wrap in a markdown block.
Do not balance unbalanced brackets or parenthesis at beginning or end of text.
Write solutions in their entirety.")
        (query (read-string (format "%s (%s) insert: "
                                    (chatgpt-shell--model-label)
                                    (chatgpt-shell--model-short-version)))))
    (when (derived-mode-p 'prog-mode)
      (setq system-prompt (format "%s\nUse `%s` programming language."
                                  system-prompt
                                  (string-trim-right (symbol-name major-mode) "-mode"))))
    (if (region-active-p)
        (chatgpt-shell-request-and-insert-merged-response
         :system-prompt system-prompt
         :query (concat query
                        "\n\n"
                        "Apply my instruction to:")
         :remove-block-markers t)
      (chatgpt-shell-request-and-insert-response
       :streaming t
       :system-prompt system-prompt
       :query query))))

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
 `\\[chatgpt-shell-prompt-compose-request-more]` Send \"Show me more\" query.
 `\\[chatgpt-shell-prompt-compose-other-buffer]` Jump to other buffer (ie. the shell itself).
 `\\[chatgpt-shell-mark-block]` Mark block at point."
  (interactive "P")
  (chatgpt-shell-prompt-compose-show-buffer nil prefix))

(defvar-local chatgpt-shell--ring-index nil)

(defun chatgpt-shell-prompt-compose-show-buffer (&optional content clear-history transient-frame-p)
  "Show a prompt compose buffer.

Prepopulate buffer with optional CONTENT.

Set CLEAR-HISTORY to wipe any existing shell history.

Set TRANSIENT-FRAME-P to also close frame on exit."
  (let* ((exit-on-submit (derived-mode-p 'chatgpt-shell-mode))
         (region (or content
                     (when-let ((region-active (region-active-p))
                                (region (buffer-substring (region-beginning)
                                                          (region-end))))
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
                           (not region)
                           ;; view-mode = old query, erase for new one.
                           (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                             chatgpt-shell-prompt-compose-view-mode))))
    (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
      (chatgpt-shell-prompt-compose-mode)
      (setq-local chatgpt-shell-prompt-compose--exit-on-submit exit-on-submit)
      (setq-local chatgpt-shell-prompt-compose--transient-frame-p transient-frame-p)
      (visual-line-mode +1)
      (when erase-buffer
        (chatgpt-shell-prompt-compose-view-mode -1)
        (erase-buffer))
      (when region
        (save-excursion
          (goto-char (point-max))
          (insert "\n\n")
          (insert region)))
      (when clear-history
        (let ((chatgpt-shell-prompt-query-response-style 'inline))
          (chatgpt-shell-send-to-buffer "clear")))
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

(defun chatgpt-shell-mark-block ()
  "Mark current block in compose buffer."
  (interactive)
  (when-let ((block (chatgpt-shell-markdown-block-at-point)))
    (set-mark (map-elt block 'end))
    (goto-char (map-elt block 'start))))

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
        (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
        (chatgpt-shell-prompt-compose-view-mode +1)
        (setq view-exit-action 'kill-buffer)
        (when (string-equal prompt "clear")
          (view-mode -1)
          (erase-buffer))
        (if chatgpt-shell-prompt-compose--exit-on-submit
            (let ((view-exit-action nil)
                  (chatgpt-shell-prompt-query-response-style 'shell))
              (quit-window t (get-buffer-window (chatgpt-shell-prompt-compose-buffer)))
              (chatgpt-shell-send-to-buffer prompt))
          (let ((chatgpt-shell-prompt-query-response-style 'inline))
            (chatgpt-shell-send-to-buffer prompt nil nil
                                          (lambda (_input _output _success)
                                            (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                                              (chatgpt-shell--put-source-block-overlays))))))))))

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
     (car next) (cdr next))))

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
             (inhibit-read-only t)
             (chatgpt-shell-prompt-query-response-style 'inline))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt nil nil
                                  (lambda (_input _output _success)
                                    (with-current-buffer (chatgpt-shell-prompt-compose-buffer)
                                      (chatgpt-shell--put-source-block-overlays))))))

(defun chatgpt-shell-prompt-compose-next-block ()
  "Jump to and select next code block."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((before (point)))
    (call-interactively #'chatgpt-shell-next-source-block)
    (call-interactively #'chatgpt-shell-mark-block)
    (when (eq before (point))
      (chatgpt-shell-prompt-compose-next-interaction))))

(defun chatgpt-shell-prompt-compose-previous-block ()
  "Jump to and select previous code block."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (let ((before (point)))
    (call-interactively #'chatgpt-shell-previous-source-block)
    (call-interactively #'chatgpt-shell-mark-block)
    (when (eq before (point))
      (chatgpt-shell-prompt-compose-previous-interaction))))

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
        (inhibit-read-only t)
        (chatgpt-shell-prompt-query-response-style 'inline))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt)))

(defun chatgpt-shell-prompt-compose-request-more ()
  "Request more data.  This is useful if you already requested examples."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (with-current-buffer (chatgpt-shell--primary-buffer)
    (when shell-maker--busy
      (user-error "Busy, please wait")))
  (let ((prompt "give me more")
        (inhibit-read-only t)
        (chatgpt-shell-prompt-query-response-style 'inline))
    (erase-buffer)
    (insert (propertize (concat prompt "\n\n") 'face font-lock-doc-face))
    (chatgpt-shell-send-to-buffer prompt)))

(defun chatgpt-shell-prompt-compose-other-buffer ()
  "Jump to the shell buffer (compose's other buffer)."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-prompt-compose-mode)
    (user-error "Not in a shell compose buffer"))
  (switch-to-buffer (chatgpt-shell--primary-buffer)))

;; pretty smerge start

(cl-defun chatgpt-shell--pretty-smerge-insert (&key text start end buffer)
  "Insert TEXT, replacing content of START and END at BUFFER."
  (unless (and text (stringp text))
    (error ":text is missing or not a string"))
  (unless (and buffer (bufferp buffer))
    (error ":buffer is missing or not a buffer"))
  (unless (and start (integerp start))
    (error ":start is missing or not an integer"))
  (unless (and end (integerp end))
    (error ":end is missing or not an integer"))
  (with-current-buffer buffer
    (let* ((needs-wrapping (not visual-line-mode))
           (orig-start (copy-marker start))
           (orig-end (copy-marker end))
           (orig-text (buffer-substring-no-properties orig-start
                                                      orig-end))
           (diff (chatgpt-shell--pretty-smerge--make-merge-patch
                  :old-label "Before" :old orig-text
                  :new-label "After" :new text)))
      (delete-region orig-start orig-end)
      (when (looking-at-p "\n")
        (delete-char 1))
      (goto-char orig-start)
      (insert diff)
      (when needs-wrapping
        (visual-line-mode +1))
      (smerge-mode +1)
      (ignore-errors
        (smerge-prev))
      (chatgpt-shell--pretty-smerge-mode +1)
      (condition-case nil
          (unwind-protect
              (progn
                (if (y-or-n-p "Keep change?")
                    (smerge-keep-lower)
                  (smerge-keep-upper))
                (smerge-mode -1))
            (chatgpt-shell--pretty-smerge-mode -1)
            (when needs-wrapping
              (visual-line-mode -1)))
        (quit
         (chatgpt-shell--pretty-smerge-mode -1)
         (when needs-wrapping
           (visual-line-mode -1)))
        (error nil)))))

(cl-defun chatgpt-shell--pretty-smerge--make-merge-patch (&key old new old-label new-label)
  "Write OLD and NEW to temporary files, run diff3, and return merge patch.
OLD-LABEL (optional): To display for old text.
NEW-LABEL (optional): To display for new text."
  (let ((base-file (make-temp-file "base"))
        (old-file (make-temp-file "old"))
        (new-file (make-temp-file "new")))
    (with-temp-file old-file
      (insert old)
      (unless (string-suffix-p "\n" old)
        (insert "\n")))
    (with-temp-file new-file
      (insert new)
      (unless (string-suffix-p "\n" new)
        (insert "\n")))
    (with-temp-buffer
      (let ((retval (call-process "diff3" nil t nil "-m" old-file base-file new-file)))
        (delete-file base-file)
        (delete-file old-file)
        (delete-file new-file)
        ;; 0: No differences or no conflicts.
        ;; 1: Merge conflicts.
        ;; 2: Error occurred.
        (when (= retval 2)
          (error (buffer-substring-no-properties (point-min)
                                                 (point-max))))
        (goto-char (point-min))
        (while (search-forward old-file nil t)
          (replace-match (or old-label "old")))
        (goto-char (point-min))
        (while (search-forward new-file nil t)
          (replace-match (or new-label "new")))
        (goto-char (point-min))
        (flush-lines "^|||||||")
        (buffer-substring-no-properties (point-min)
                                        (point-max))))))

(define-minor-mode chatgpt-shell--pretty-smerge-mode
  "Minor mode to display overlays for conflict markers."
  :lighter " PrettySmerge"
  (if chatgpt-shell--pretty-smerge-mode
      (progn
        (chatgpt-shell--pretty-smerge--refresh)
        (add-hook 'after-change-functions
                  #'chatgpt-shell--pretty-smerge--autodisable
                  nil t))
    (chatgpt-shell--pretty-smerge-mode-remove--overlays)
    (remove-hook 'after-change-functions
                 #'chatgpt-shell--pretty-smerge--autodisable
                 t)))

(defun chatgpt-shell--pretty-smerge--autodisable (_beg _end _len)
  "Disable `chatgpt-shell--pretty-smerge-mode' on edit."
  (chatgpt-shell--pretty-smerge-mode -1)
  (remove-hook 'after-change-functions
               #'chatgpt-shell--pretty-smerge--autodisable
               t))

(defun chatgpt-shell--pretty-smerge--refresh ()
  "Apply overlays to conflict markers."
  (chatgpt-shell--pretty-smerge-mode-remove--overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat
             "^\\(<<<<<<<[ \t]*\\)" ;; begin marker
             "\\(.*\\)\n"           ;; begin label
             "\\(\\(?:.*\n\\)*?\\)"     ;; upper content
             "\\(=======\n\\)"      ;; maker
             "\\(\\(?:.*\n\\)*?\\)"     ;; lower content
             "\\(>>>>>>>[ \t]*\\)"  ;; end marker
             "\\(.*\\)\n")          ;; end label
            nil t)
      (let ((overlay (make-overlay (match-beginning 1)
                                   (match-end 2))))
        (overlay-put overlay 'category 'conflict-marker)
        (overlay-put overlay 'display
                     (concat (propertize (concat " " (match-string 2) " ")
                                         'face '(:inherit default :box t))
                             "\n"))
        (overlay-put overlay 'evaporate t))
      (let ((overlay (make-overlay (match-beginning 4)
                                   (match-end 4))))
        (overlay-put overlay 'category 'conflict-marker)
        (overlay-put overlay 'display
                     (concat "\n" (propertize (concat " " (match-string 7) " ")
                                              'face '(:inherit default :box t)) "\n\n"))
        (overlay-put overlay 'evaporate t))
      (let ((overlay (make-overlay (match-beginning 6)
                                   (match-end 7))))
        (overlay-put overlay 'category 'conflict-marker)
        (overlay-put overlay 'display "")
        (overlay-put overlay 'face 'warning)
        (overlay-put overlay 'evaporate t)))))

(defun chatgpt-shell--pretty-smerge-mode-remove--overlays ()
  "Remove all conflict marker overlays."
  (remove-overlays (point-min) (point-max) 'category 'conflict-marker))

;; pretty smerge end

;; fader start

(defvar-local chatgpt-shell--fader-timer nil
  "Timer object for animating the region.")

(defvar-local chatgpt-shell--fader-overlays nil
  "List of overlays for the animated regions.")

(defun chatgpt-shell--fader-start-fading-region (start end)
  "Animate the background color of the region between START and END."
  (deactivate-mark)
  (chatgpt-shell--fader-stop-fading)
  (let ((colors (append (chatgpt-shell--fader-palette)
                        (reverse (chatgpt-shell--fader-palette)))))
    (dolist (ov chatgpt-shell--fader-overlays) (delete-overlay ov))
    (setq chatgpt-shell--fader-overlays (list (make-overlay start end)))
    (setq chatgpt-shell--fader-timer
          (run-with-timer 0 0.01
                          (lambda ()
                            (let* ((color (pop colors)))
                              (if (and color
                                       chatgpt-shell--fader-overlays)
                                  (progn
                                    (overlay-put (car chatgpt-shell--fader-overlays) 'face `(:background ,color :extend t))
                                    (setq colors (append colors (list color))))
                                (chatgpt-shell--fader-stop-fading))))))))

(defun chatgpt-shell--fader-palette ()
  "Generate a gradient palette from the `region' face to the `default' face."
  (let* ((start-color (face-background 'region))
         (end-color (face-background 'default))
         (start-rgb (color-name-to-rgb start-color))
         (end-rgb (color-name-to-rgb end-color))
         (steps 50))
    (mapcar (lambda (step)
              (apply #'color-rgb-to-hex
                     (cl-mapcar (lambda (start end)
                                  (+ start (* step (/ (- end start) (1- steps)))))
                                start-rgb end-rgb)))
            (number-sequence 0 (1- steps)))))

(defun chatgpt-shell--fader-start ()
  "Start animating the currently active region."
  (if (use-region-p)
      (progn
        (deactivate-mark)
        (chatgpt-shell--fader-start-fading-region (region-beginning) (region-end)))
    (message "No active region")))

(defun chatgpt-shell--fader-stop-fading ()
  "Stop animating and remove all overlays."
  (when chatgpt-shell--fader-timer
    (cancel-timer chatgpt-shell--fader-timer)
    (setq chatgpt-shell--fader-timer nil))
  (dolist (ov chatgpt-shell--fader-overlays)
    (delete-overlay ov))
  (setq chatgpt-shell--fader-overlays nil))

;; fader end

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
