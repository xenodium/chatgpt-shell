;;; chatgpt-shell.el --- A family of utilities to interact with LLMs (ChatGPT, Claude, DeepSeek, Gemini, Kagi, Ollama, Perplexity)  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 2.30.4
;; Package-Requires: ((emacs "28.1") (shell-maker "0.82.3") (transient "0.9.3"))
(defconst chatgpt-shell--version "2.30.4")

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

;; `chatgpt-shell' provides utilities to interact with LLMS.
;;
;; At its core, it provides a comint-based shell for multiple cloud or local
;; LLM services (ChatGPT, Claude, Gemini, Kagi, Ollama, Perplexity).
;;
;; M-x `chatgpt-shell-prompt-compose'
;;
;; Compose offers a shell-hybrid interface enabling more efficient
;; LLM interactions.
;;
;; This package also provides integrations like the following (amongst others):
;;
;; M-x `chatgpt-shell-quick-insert'
;; M-x `chatgpt-shell-proofread-region'
;; M-x `chatgpt-shell-describe-image'
;; M-x `chatgpt-shell-japanese-lookup'
;;
;; You must set an API key for most cloud services.  Check out:
;;
;;   `chatgpt-shell-anthropic-key'.
;;   `chatgpt-shell-deepseek-key'
;;   `chatgpt-shell-google-key'.
;;   `chatgpt-shell-kagi-key'.
;;   `chatgpt-shell-openai-key'.
;;   `chatgpt-shell-openrouter-key'
;;   `chatgpt-shell-perplexity-key'.
;;
;; Alternatively, local services like Ollama do not require an API key.
;;
;; Run `chatgpt-shell' to open an LLM shell.
;;
;; Please report issues or send patches to
;; https://github.com/xenodium/chatgpt-shell
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dired)
(require 'esh-mode)
(require 'em-prompt)
(require 'eshell)
(require 'find-func)
(require 'flymake)
(require 'ielm)
(unless (require 'markdown-overlays nil 'noerror)
  (error "Please update 'shell-maker' to v0.77.1 or newer"))
(require 'shell-maker)
(require 'smerge-mode)
(require 'ob-core)
(require 'color)

(require 'chatgpt-shell-anthropic)
(require 'chatgpt-shell-deepseek)
(require 'chatgpt-shell-google)
(require 'chatgpt-shell-kagi)
(require 'chatgpt-shell-ollama)
(require 'chatgpt-shell-openai)
(require 'chatgpt-shell-openrouter)
(require 'chatgpt-shell-perplexity)
(require 'chatgpt-shell-prompt-compose)
(require 'chatgpt-shell-transient)

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
  "Please help me proofread the following English text and only reply with fixed text.
Output just the proofread text without any intro, comments, or explanations.
If the original text was indented on the left, preserve the same amount of spacing in your response:

"
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

(defcustom chatgpt-shell-show-model-icons t
  "When non-nil display model image icons.

For example, when swapping models."
  :type 'boolean
  :group 'chatgpt-shell)

(defvaralias 'chatgpt-shell-swap-model-version 'chatgpt-shell-swap-model)

(defvaralias 'chatgpt-shell-display-function 'shell-maker-display-function)

(defvaralias 'chatgpt-shell-read-string-function 'shell-maker-read-string-function)

(defvaralias 'chatgpt-shell-logging 'shell-maker-logging)

(defvaralias 'chatgpt-shell-root-path 'shell-maker-root-path)

(defvaralias 'chatgpt-shell-insert-dividers 'markdown-overlays-insert-dividers)

(defvaralias 'chatgpt-shell-highlight-blocks 'markdown-overlays-highlight-blocks)

(defvaralias 'chatgpt-shell-render-latex 'markdown-overlays-render-latex)

(defvaralias 'chatgpt-shell-language-mapping 'markdown-overlays-language-mapping)

(defalias 'chatgpt-shell-save-session-transcript #'shell-maker-save-session-transcript)

(defalias 'chatgpt-shell-proofread-region #'chatgpt-shell-proofread-paragraph-or-region)

(defvar chatgpt-shell--prompt-history nil)

(defcustom chatgpt-shell-babel-headers '(("dot" . ((:file . "<temp-file>.png")))
                                         ("plantuml" . ((:file . "<temp-file>.png")))
                                         ("ditaa" . ((:file . "<temp-file>.png")))
                                         ("objc" . ((:results . "output")))
                                         ("lisp" . ((:results . "output")))
                                         ("clojure" . ((:results . "output")))
                                         ("python" . ((:python . "python3")))
                                         ("swiftui" . ((:results . "file")))
                                         ("c++" . ((:results . "raw")))
                                         ("c" . ((:results . "raw"))))
  "Additional headers to make babel blocks work.

Entries are of the form (language . headers).  Headers should
conform to the types of `org-babel-default-header-args'.

Please submit contributions so more languages work out of the box."
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

(defun chatgpt-shell--make-default-models ()
  "Create a list of default models by combining models from different providers.

This function aggregates models from OpenAI, Anthropic, Google, and Ollama.
It returns a list containing all available models from these providers."
  (append (chatgpt-shell-anthropic-models)
          (chatgpt-shell-deepseek-models)
          (chatgpt-shell-google-models)
          (chatgpt-shell-kagi-models)
          (chatgpt-shell-ollama-models)
          (chatgpt-shell-openai-models)
          (chatgpt-shell-openrouter-models)
          (chatgpt-shell-perplexity-models)))

(defcustom chatgpt-shell-models
  (chatgpt-shell--make-default-models)
  "The list of supported models to swap from.

See `chatgpt-shell-openai-models',
    `chatgpt-shell-anthropic-models'
    `chatgpt-shell-ollama-models'
    `chatgpt-shell-google-models' for details."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version nil
  "The active model version as either a string.

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
    ("Mathematics" . "The user is a mathematician with very limited time.
                      You treat their time as precious. You do not repeat obvious things, including their query.
                      You are as concise as possible in responses.
                      You never apologize for confusions because it would waste their time.
                      All mathematical outputs must be in proper LaTeX format.
                      Use \\( and \\( for in-line equations, and \\[ and \\] for equation environments.
                      All mathematical delimiters must be on their own line.
                      All mathematical outputs are formally expressed in formal mathematical notation.
                      Don't give approximations or pronunciations for constants or variables.
                      You give formal definitions for all variables.
                      You keep your answers succinct and to-the-point.")
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
  (cond ((not chatgpt-shell-model-version)
         ;; No default model set, find one that's cleared for sending commands.
         (if-let* ((cleared-model (seq-find (lambda (model)
                                              (cond ((not (map-elt model :validate-command))
                                                     t)
                                                    ((and (map-elt model :validate-command)
                                                          (not (funcall (map-elt model :validate-command)
                                                                        "hello" model nil)))
                                                     t)
                                                    (t
                                                     nil)))
                                            chatgpt-shell-models))
                   (model-version (map-elt cleared-model :version)))
             model-version
           (if (map-elt (seq-first chatgpt-shell-models) :version)
               (map-elt (seq-first chatgpt-shell-models) :version)
             (error "Could not find a model.  Missing model setup?"))))
        ((stringp chatgpt-shell-model-version)
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
         (error "Could not find a model.  Missing model setup?"))))

(defun chatgpt-shell-system-prompt ()
  "Return active system prompt."
  (cond ((stringp chatgpt-shell-system-prompt)
         chatgpt-shell-system-prompt)
        ((integerp chatgpt-shell-system-prompt)
         (let ((prompt (nth chatgpt-shell-system-prompt
                            chatgpt-shell-system-prompts)))
           (if (consp prompt)
               (cdr prompt)
             prompt)))))

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

(defun chatgpt-shell-validate-no-system-prompt (command model settings)
  "Perform validation for COMMAND with MODEL and SETTINGS.
Then enforce that there is no system prompt.  This is useful for models like
OpenAI's o1 that do not allow one."
    (or (chatgpt-shell-openai--validate-command command model settings)
        (when (map-elt settings :system-prompt)
          (format "Model \"%s\" does not support system prompts. Please unset via \"M-x chatgpt-shell-swap-system-prompt\" by selecting None."
                  (map-elt model :version)))))

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

(defun chatgpt-shell--load-awesome-prompts-parse-alist ()
  "Helper function for `claude-shell-load-awesome-prompts'.

Download awesome-prompts and parse into a list of label and
prompt cons."
  (let ((url "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv")
        (collector '()))
    (with-current-buffer (url-retrieve-synchronously url)

      (goto-char (if (boundp 'url-http-end-of-headers)
                     url-http-end-of-headers
                   (error "`url-http-end-of-headers' marker is not defined")))

      (forward-line 2)
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (split (split-string line "," 'nil "\""))
               (head (car split))
               (tail (apply #'concat (cdr split))))
          (push (cons head tail) collector)
          (forward-line 1))))
    collector))

;;;###autoload
(defun chatgpt-shell-load-awesome-prompts ()
  "Load `chatgpt-shell-system-prompts' from awesome-chatgpt-prompts.

Downloaded from https://github.com/f/awesome-chatgpt-prompts."
  (interactive)
  (let ((prompts (chatgpt-shell--load-awesome-prompts-parse-alist)))
    (setq chatgpt-shell-system-prompts
          (map-merge 'list
                     chatgpt-shell-system-prompts
                     (seq-sort (lambda (lhs rhs) (string-lessp (car lhs) (car rhs))) prompts)))
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

(defun chatgpt-shell-reload-default-models ()
  "Reload all available models."
  (interactive)
  (setq chatgpt-shell-models (chatgpt-shell--make-default-models))
  (message "Reloaded %d models" (length chatgpt-shell-models)))

(defcustom chatgpt-shell-swap-model-filter nil
  "Filter models to swap from using this function as a filter.

See `chatgpt-shell-allow-model-versions' and
`chatgpt-shell-ignore-model-versions' as examples."
  :type 'function
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-swap-model-selector nil
  "Custom function to select a model during swap.

This would allow a user to sort, group, filter, present a different
selection, user experience, attach affixations, and so on.

An example:

  (setq chatgpt-shell-swap-model-selector
        (lambda (candidates)
          (completing-read
            \"New model: \"
             (my-custom-model-completion-table candidates) nil t)))

See also `chatgpt-shell-swap-model'."
  :type 'function
  :group 'chatgpt-shell)

(defun chatgpt-shell-allow-model-versions (versions)
  "Return a filter function to keep known model VERSIONS only.

Use with `chatgpt-shell-swap-model-filter'."
  (lambda (models)
    (seq-filter (lambda (model)
                  (member (map-elt model :version) versions))
                models)))

(defun chatgpt-shell-ignore-model-versions (versions)
  "Return a filter function to drop model VERSIONS.

Use with `chatgpt-shell-swap-model-filter'."
  (lambda (models)
    (seq-filter (lambda (model)
                  (not (member (map-elt model :version) versions)))
                models)))

(defun chatgpt-shell-swap-model ()
  "Swap model version from `chatgpt-shell-models'.

To select a model, it uses `chatgpt-shell-swap-model-selector' if
non-nil; otherwise `completing-read'."
  (interactive)
  (when (and (boundp 'chatgpt-shell-model-filter)
             chatgpt-shell-model-filter)
    (user-error "Variable chatgpt-shell-model-filter is retired.  Please use chatgpt-shell-swap-model-filter"))
  (if-let* ((last-label (chatgpt-shell--model-label))
            (width (let ((width))
                     (mapc (lambda (model)
                             (unless width
                               (setq width 0))
                             (when-let ((provider (map-elt model :provider))
                                        (provider-width (length (map-elt model :provider)))
                                        (longer (> provider-width width)))
                               (setq width provider-width)))
                           chatgpt-shell-models)
                     width))
            (models (seq-map (lambda (model)
                               (format (format "%%s%%-%ds   %%s" width)
                                       (if-let* ((show-icon chatgpt-shell-show-model-icons)
                                                 (icon (map-elt model :icon))
                                                 (icon-filename (chatgpt-shell--fetch-model-icon icon)))
                                           (with-temp-buffer
                                             (insert-image (create-image icon-filename nil nil
                                                                         :ascent 'center
                                                                         :height (frame-char-height)))
                                             (insert "  ")
                                             (buffer-string))
                                         "")
                                       (map-elt model :provider)
                                       (map-elt model :version)))
                             (if chatgpt-shell-swap-model-filter
                                 (funcall chatgpt-shell-swap-model-filter chatgpt-shell-models)
                               chatgpt-shell-models)))
            (selection (nth 1 (split-string (if chatgpt-shell-swap-model-selector
                                                (funcall chatgpt-shell-swap-model-selector models)
                                              (completing-read "Model version: "
                                                               models nil t))))))
      (progn
        (when (derived-mode-p 'chatgpt-shell-mode)
          (setq-local chatgpt-shell-model-version selection)
          (chatgpt-shell--update-prompt t)
          (chatgpt-shell-interrupt nil)
          (unless (equal last-label (chatgpt-shell--model-label))
            (chatgpt-shell-clear-buffer)))
        (setq-default chatgpt-shell-model-version selection))
    (error "No other providers found")))

(defun chatgpt-shell--unsorted-collection (collection)
  "Return a completion table from COLLECTION that inhibits sorting.

See `completing-read' for the types that are supported for
COLLECTION."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        (let ((current-metadata (cdr (completion-metadata (minibuffer-contents)
                                                          collection
                                                          minibuffer-completion-predicate))))
          `(metadata
            ,@(map-merge 'alist
                         current-metadata
                         '((display-sort-function . identity)
                           (cycle-sort-function . identity)))))
      (complete-with-action action collection string predicate))))
(defun chatgpt-shell-select-reasoning-effort (&optional global)
  "Interactively set the reasoning effort for the current model.

By default, this is done buffer-locally when in a
`chatgpt-shell-mode' buffer `chatgpt-shell-prompt-compose-mode'
buffer. When GLOBAL is non-nil (interactively with a prefix
argument), it is set globally."
  (interactive "P")
  (let* ((model (chatgpt-shell--resolved-model))
         (selector (map-elt model :reasoning-effort-selector)))
    (unless selector
      (user-error "No reasoning effort selector is defined for %s" (chatgpt-shell-model-version)))
    (let* ((buf (cond
                 (global
                  nil)
                 ((eq major-mode 'chatgpt-shell-mode)
                  (current-buffer))
                 ((memq major-mode '(chatgpt-shell-prompt-compose-mode chatgpt-shell-prompt-compose-view-mode))
                  (chatgpt-shell--primary-buffer))))
           ;; The call to the selector returns a list of bindings. Some models
           ;; (e.g. those by Anthropic) have multiple variables that control
           ;; reasoning so in some cases it is necessary to set more than one.
           ;; An example return value is
           ;;
           ;; '(((:symbol chatgpt-shell-anthropic-thinking-budget-tokens)
           ;;    (:value 3000)
           ;;    (:kind thinking-budget)
           ;;    ;; This indicates if the budget will be set to the max by this
           ;;    ;; binding. It is only needed when it is non-nil.
           ;;    (:max nil))
           ;;   ((:symbol chatgpt-shell-anthropic-thinking)
           ;;    (:value t)
           ;;    (:kind thinking-toggle)))
           (bindings (if buf
                         (with-current-buffer buf
                           (funcall selector model))
                       (funcall selector model))))
      (dolist (binding bindings)
        (unless (memq (map-elt binding :kind)
                      '(thinking-budget thinking-toggle))
          (error "Unknown kind %S returned by reasoning effort selector" (map-elt binding :kind)))
        (if buf
            ;; Ensure that the variable is set buffer-locally.
            (with-current-buffer buf
              (set (make-local-variable (map-elt binding :symbol))
                   (map-elt binding :value)))
          ;; Set the global value even if it has already been bound
          ;; buffer-locally. Note that using `set' on var will set the
          ;; buffer-local value if one already exists.
          (set-default (map-elt binding :symbol) (map-elt binding :value)))
        ;; Let the user know what the thinking budget was set to.
        (when (eq (map-elt binding :kind) 'thinking-budget)
          (message "Set %s to %s%s"
                   (map-elt binding :symbol)
                   (if (map-elt binding :max) "max" (map-elt binding :value))
                   (if buf " locally" " globally")))))))

(defcustom chatgpt-shell-streaming t
  "Whether or not to stream ChatGPT responses (show chunks as they arrive)."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-always-create-new t
  "Non-nil creates a new shell buffer every time `chatgpt-shell' is invoked.

Otherwise, reuse an existing chat."
  :type 'boolean
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-proxy nil
  "When non-nil, use as a proxy (for example http or socks5)."
  :type 'string
  :group 'chatgpt-shell)

(defun chatgpt-shell--model-settings ()
  "Variable model settings.

See `chatgpt-shell-streaming'
    `chatgpt-shell-model-temperature'
    variable `chatgpt-shell-system-prompt'."
  (list (cons :streaming chatgpt-shell-streaming)
        (cons :temperature chatgpt-shell-model-temperature)
        (cons :system-prompt (chatgpt-shell-system-prompt))))

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
                 (settings (chatgpt-shell--model-settings))
                 (validate-command (map-elt model :validate-command)))
       (funcall validate-command command model settings)))
   :execute-command
   (lambda (command shell)
     (if-let* ((model (chatgpt-shell--resolved-model))
               (handler (map-elt model :handler)))
         (progn
           (unless (fboundp 'markdown-overlays-expand-local-links)
             (error "Please update 'shell-maker' to v0.78.1 or newer"))
           (unless (and (boundp 'shell-maker-version)
                        (version<= "0.79.1" shell-maker-version))
             (error "Please update 'shell-maker' to v0.79.1 or newer"))
           (funcall handler
                    :model model
                    :command (if chatgpt-shell-include-local-file-link-content
                                 (markdown-overlays-expand-local-links command)
                               command)
                    :context (chatgpt-shell-crop-context
                              :model model
                              :command command
                              :context
                              (if chatgpt-shell-include-local-file-link-content
                                  (mapcar (lambda (item)
                                            (cons (markdown-overlays-expand-local-links
                                                   (car item))
                                                  (cdr item)))
                                          (map-elt shell :history))
                                (map-elt shell :history)))
                    :shell shell
                    :settings (chatgpt-shell--model-settings)))
       (error "%s not found" (chatgpt-shell-model-version))))
   :on-command-finished
   (lambda (command output success)
     (markdown-overlays-put)
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

(defvar-keymap chatgpt-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `chatgpt-shell-mode'."
  "C-M-h" #'chatgpt-shell-mark-at-point-dwim
  "C-c C-c" #'chatgpt-shell-ctrl-c-ctrl-c
  "C-c C-v" #'chatgpt-shell-swap-model
  "C-c C-s" #'chatgpt-shell-swap-system-prompt
  "C-c C-p" #'chatgpt-shell-previous-item
  "<backtab>" #'chatgpt-shell-previous-item
  "C-c C-n" #'chatgpt-shell-next-item
  "<tab>" #'chatgpt-shell-next-item
  "C-c C-e" #'chatgpt-shell-prompt-compose
  "C-c C-t" #'chatgpt-shell-transient)

(shell-maker-define-major-mode chatgpt-shell--config chatgpt-shell-mode-map)

;; Implementation generated by shell-maker
(declare-function chatgpt-shell-clear-buffer "chatgpt-shell")

(cl-defun chatgpt-shell--resolved-model (&key versioned)
  "Resolve model VERSIONED name."
  (seq-find (lambda (model)
              (equal (map-elt model :version)
                     (or versioned (chatgpt-shell-model-version))))
            chatgpt-shell-models))

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
  (chatgpt-shell-start nil (or new-session
                               chatgpt-shell-always-create-new)))

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
                             (if (and (chatgpt-shell--primary-buffer :create nil)
                                      (not ignore-as-primary))
                                 (buffer-name (chatgpt-shell--primary-buffer :create nil))
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
  (concat
   (chatgpt-shell--model-short-version)
   (if-let (system-prompt-name (chatgpt-shell--system-prompt-name))
       (concat "/" system-prompt-name)
     "")))

(defun chatgpt-shell--system-prompt-name ()
  "Get the current system prompt name."
  (cond ((and (integerp chatgpt-shell-system-prompt)
              (nth chatgpt-shell-system-prompt
                   chatgpt-shell-system-prompts))
         (chatgpt-shell--shrink-system-prompt (nth chatgpt-shell-system-prompt
                                                   chatgpt-shell-system-prompts)))
        ((stringp chatgpt-shell-system-prompt)
         (chatgpt-shell--shrink-system-prompt chatgpt-shell-system-prompt))
        (t
         nil)))

(defun chatgpt-shell--model-label ()
  "Return the current model label."
  (or (map-elt (chatgpt-shell--resolved-model) :label)
      "Unknown"))

(defun chatgpt-shell--model-short-version ()
  "Return the current model short version."
  (or (map-elt (chatgpt-shell--resolved-model) :short-version)
      (map-elt (chatgpt-shell--resolved-model) :version)
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

(cl-defun chatgpt-shell--primary-buffer (&key (create t))
  "Return the primary shell buffer.

:CREATE nil to avoid automatic buffer creation.

This is used for sending a prompt to in the background."
  (let* ((shell-buffers (chatgpt-shell--shell-buffers))
         (primary-shell-buffer (seq-find
                                (lambda (shell-buffer)
                                  (with-current-buffer shell-buffer
                                    chatgpt-shell--is-primary-p))
                                shell-buffers)))
    (when (and create (not primary-shell-buffer))
      (setq primary-shell-buffer
            (or (seq-first shell-buffers) (chatgpt-shell-start t t)))
      (chatgpt-shell--set-primary-buffer primary-shell-buffer))
    primary-shell-buffer))

(defun chatgpt-shell--make-buffer-name ()
  "Generate a buffer name using current shell config info."
  (format "*%s llm (%s)*"
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

(defcustom chatgpt-shell-include-local-file-link-content nil
  "Non-nil includes linked file content in requests.

Links must be of the form:

  `[file.txt](file:///absolute/path/to/file.txt)'"
  :type 'boolean
  :group 'chatgpt-shell)

(defun chatgpt-shell-insert-local-file-link ()
  "Select and insert a link to a local file.

Requires `chatgpt-shell-include-local-file-link-content' set."
  (interactive)
  (let* ((file (read-file-name "Select file: "))
         (link (markdown-overlays-make-local-file-link file)))
    (unless link
      (error "File not found"))
    (unless chatgpt-shell-include-local-file-link-content
      (unless (yes-or-no-p "Link file and potentially send content? ")
        (error "Aborted"))
      (customize-save-variable 'chatgpt-shell-include-local-file-link-content t))
    (save-excursion
      (insert "\n\n" link))))

(defun chatgpt-shell-insert-buffer-file-link ()
  "Select and insert a link to a buffer's local file.

Requires `chatgpt-shell-include-local-file-link-content' set."
  (interactive)
  (let* ((buffer (get-buffer
                  (completing-read
                   "Select buffer: "
                   (mapcar #'buffer-name
                           (seq-filter #'buffer-file-name (buffer-list))) nil t)))
         (file (buffer-file-name buffer))
         (link (markdown-overlays-make-local-file-link file)))
    (unless link
      (error "File not found"))
    (unless chatgpt-shell-include-local-file-link-content
      (unless (yes-or-no-p "Link file and potentially send content? ")
        (error "Aborted"))
      (customize-save-variable 'chatgpt-shell-include-local-file-link-content t))
    (save-excursion
      (insert "\n\n" link))))

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
      ;; Ensure point is within block if at bol in header.
      (move-end-of-line 1)
      (let* ((language)
             (language-start)
             (language-end)
             (start (save-excursion
                      (when (re-search-backward "^[ \t]*```" nil t)
                        (setq language (chatgpt-shell-markdown-block-language (thing-at-point 'line)))
                        (save-excursion
                          (forward-char 3) ; ```
                          (setq language-start (point))
                          (end-of-line)
                          (setq language-end (point)))
                        language-end)))
             (end (save-excursion
                    (when (re-search-forward "^[ \t]*```" nil t)
                      (forward-line 0)
                      (point)))))
        (when (and start end language
                   (>= (point) start)
                   (< (point) end))
          (list (cons 'language language)
                (cons 'language-start language-start)
                (cons 'language-end language-end)
                (cons 'start start)
                (cons 'end end)))))))

(defun chatgpt-shell-next-source-block ()
  "Move point to the next source block's body."
  (interactive)
  (let ((blocks (markdown-overlays--source-blocks))
        (pos (point)))
    (when-let ((next-block (seq-find (lambda (block)
                                       (> (car (map-elt block 'start)) pos))
                                     blocks)))
      (goto-char (car (map-elt next-block 'start)))
      (point))))

(defun chatgpt-shell-next-link ()
  "Move point to the next link."
  (interactive)
  (let ((links (markdown-overlays--markdown-links))
        (pos (point)))
    (when-let ((next-link (seq-find (lambda (link)
                                       (> (map-elt link 'start) pos))
                                     links)))
      (goto-char (map-elt next-link 'start))
      (point))))

(defun chatgpt-shell-previous-item ()
  "Go to previous item.

Could be a prompt or a source block."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let* ((prompt-pos (save-excursion
                       (when (comint-next-prompt (- 1))
                         (point))))
         (block-pos (save-excursion
                      (chatgpt-shell-previous-source-block)))
         (link-pos (save-excursion
                     (chatgpt-shell-previous-link)))
         (positions (delq nil (list prompt-pos
                                    block-pos
                                    link-pos)))
         (next-pos (when positions
                     (apply 'max positions))))
    (when next-pos
      (cond ((eq next-pos prompt-pos)
             (deactivate-mark)
             (goto-char prompt-pos))
            ((eq next-pos block-pos)
             (deactivate-mark)
             (goto-char block-pos)
             (call-interactively #'chatgpt-shell-mark-block))
            ((eq next-pos link-pos)
             (deactivate-mark)
             (goto-char link-pos))))))

(defun chatgpt-shell-next-item ()
  "Go to next item.

Could be a prompt or a source block."
  (interactive)
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Not in a shell"))
  (let* ((prompt-pos (save-excursion
                       (when (comint-next-prompt 1)
                         (point))))
         (block-pos (save-excursion
                      (chatgpt-shell-next-source-block)))
         (link-pos (save-excursion
                     (chatgpt-shell-next-link)))
         (next-pos (apply 'min (delq nil (list prompt-pos
                                               block-pos
                                               link-pos)))))
    (when next-pos
      (cond ((eq next-pos prompt-pos)
             (deactivate-mark)
             (goto-char prompt-pos))
            ((eq next-pos block-pos)
             (deactivate-mark)
             (goto-char block-pos)
             (call-interactively #'chatgpt-shell-mark-block))
            ((eq next-pos link-pos)
             (deactivate-mark)
             (goto-char link-pos))))))

(defun chatgpt-shell-previous-source-block ()
  "Move point to the previous source block's body."
  (interactive)
  (let ((blocks (markdown-overlays--source-blocks))
        (pos (point)))
    (when-let ((next-block (seq-find (lambda (block)
                                       (< (car (map-elt block 'end)) pos))
                                     (reverse blocks))))
      (goto-char (car (map-elt next-block 'start)))
      (point))))

(defun chatgpt-shell-previous-link ()
  "Move point to the previous link."
  (interactive)
  (let ((links (markdown-overlays--markdown-links))
        (pos (point)))
    (when-let ((previous-link (seq-find (lambda (link)
                                          (< (map-elt link 'end) pos))
                                        (reverse links))))
      ;; May not be on actual URL text because of overlay (not too sure).
      ;; So, pressing RET does not open link.
      ;; Work around by moving forward 1 char (not visible to user).
      (goto-char (map-elt previous-link 'start))
      (forward-char)
      (point))))

(defun chatgpt-shell--minibuffer-prompt ()
  "Construct a prompt for the minibuffer."
  (if (chatgpt-shell--primary-buffer)
      (concat (string-trim
               (replace-regexp-in-string
                "\\*" ""
                (buffer-name (chatgpt-shell--primary-buffer)))) "> ")
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
        (markdown-overlays-put)))))

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
(defun chatgpt-shell-proofread-paragraph-or-region ()
  "Proofread text from region or current paragraph using ChatGPT.

See `chatgpt-shell-prompt-header-proofread-region' to change prompt or language."
  (interactive)
  (let* ((region (if (use-region-p)
                     (chatgpt-shell--region)
                   (save-excursion
		     ;; Mark the current paragraph or org element, because org
		     ;; defines paragraphs differently from other text modes
                     (if (derived-mode-p 'org-mode)
			 (org-mark-element)
		       (mark-paragraph))
                     ;; Adjust start and end to avoid including newline characters
                     (let ((start (progn (goto-char (region-beginning))
					 (skip-chars-forward "\n")
					 (point)))
                           (end (progn (goto-char (region-end))
                                       (skip-chars-backward "\n")
                                       (point))))
                       (list (cons :start start)
                             (cons :end end)
                             (cons :text (buffer-substring-no-properties start end)))))))
         (query (map-elt region :text))
         (context nil))
    (chatgpt-shell-request-and-insert-merged-response
     :system-prompt chatgpt-shell-prompt-header-proofread-region
     :query query
     :context context
     :remove-block-markers t
     :region region
     :on-iterate (lambda (output)
                   (set-mark (map-elt region :end))
                   (goto-char (map-elt region :start))
                   (chatgpt-shell-quick-insert (append context
                                                       (list (cons query output))))))))

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
                   "What's wrong with the following command execution? Be succinct.\n\n"
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
            (load ,(find-library-name "chatgpt-shell") nil t)
            (load ,(find-library-name "chatgpt-shell-anthropic") nil t)
            (load ,(find-library-name "chatgpt-shell-deepseek") nil t)
            (load ,(find-library-name "chatgpt-shell-google") nil t)
            (load ,(find-library-name "chatgpt-shell-kagi") nil t)
            (load ,(find-library-name "chatgpt-shell-ollama") nil t)
            (load ,(find-library-name "chatgpt-shell-openai") nil t)
            (load ,(find-library-name "chatgpt-shell-openrouter") nil t)
            (load ,(find-library-name "chatgpt-shell-perplexity") nil t)
            (load ,(find-library-name "chatgpt-shell-prompt-compose") nil t)
            (load ,(find-library-name "shell-maker") nil t)
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
                    (markdown-overlays-put))))))

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
 `\\[chatgpt-shell-prompt-compose-next-item]` Jump to next source block.
 `\\[chatgpt-shell-prompt-compose-previous-item]` Jump to next previous block.
 `\\[chatgpt-shell-prompt-compose-reply]` Reply to follow-up with additional questions.
 `\\[chatgpt-shell-prompt-compose-request-entire-snippet]` Send \"Show entire snippet\" query.
 `\\[chatgpt-shell-prompt-compose-insert-block-at-point]` Insert block at point at last known location.
 `\\[chatgpt-shell-prompt-compose-request-more]` Send \"Show me more\" query.
 `\\[chatgpt-shell-prompt-compose-other-buffer]` Jump to other buffer (ie. the shell itself).
 `\\[chatgpt-shell-mark-block]` Mark block at point."
  (interactive "P")
  (chatgpt-shell-prompt-compose-show-buffer :clear-history prefix))

;; TODO: move to cl-defun and consider removing `chatgpt-shell-prompt-query-response-style'.
(defun chatgpt-shell-send-to-buffer (input &optional review handler on-finished response-style)
  "Send INPUT to *chatgpt* shell buffer.

Set REVIEW to make changes before submitting to ChatGPT.

If HANDLER function is set, ignore RESPONSE-STYLE.

RESPONSE-STYLE defaults to `chatgpt-shell-prompt-query-response-style',

and is of the form:

  (lambda (input output error finished))

ON-FINISHED is invoked when the entire interaction is finished and of the form:

  (lambda (input output success))."
  (unless response-style
    (setq response-style chatgpt-shell-prompt-query-response-style))
  (if (eq response-style 'other-buffer)
      (let ((buffer (chatgpt-shell-prompt-compose-show-buffer :content input)))
        (unless review
          (with-current-buffer buffer
            (chatgpt-shell-prompt-compose-send-buffer))))
    (let* ((buffer (cond (handler
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
                                      (funcall on-finished input output success))
                                    (with-current-buffer (chatgpt-shell--primary-buffer)
                                      (markdown-overlays-put)))))))
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

|----------+----------+-------+--------+---------+-------|
| Hiragana | Katakana | Kanji | Romaji | English | #Tags |
|----------+----------+-------+--------+---------+-------|
|          |          |       |        |         |       |
|----------+----------+-------+--------+---------+-------|"
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
                                            (when (fboundp 'org-modern-mode)
                                              (org-modern-mode +1))
                                            ;; (when (fboundp 'valign-mode)
                                            ;;   (valign-mode +1))
                                            (visual-line-mode -1))))
                          :prompt-url file
                          :streaming t
                          :system-prompt "
1. Fill out an org mode table using this format as an example:

|-------------------+----------+-------+-------------------+---------+-------------------|
| Hiragana          | Katakana | Kanji | Romaji            | English | Tags              |
|-------------------+----------+-------+-------------------+---------+-------------------|
| おなかすきました  |          | 空腹  | onaka sukimashita | hungry  | #describe #myself |
|-------------------+----------+-------+-------------------+---------+-------------------|
2. ALWAYS Fill out Hiragana when appropriate.
3. ALWAYS Fill out Katakana when appropriate.
4. ALWAYS Fill out Kanji when appropriate.
5. Show long romaji vowels (i.e ō).
6. Ensure columns align.
7. Do NOT wrap anything in Markdown source blocks.
8. Do NOT add any text or explanations outside the org table.")))

(cl-defun chatgpt-shell-lookup (&key buffer model-version system-prompt prompt prompt-url streaming
                                     temperature on-success on-failure)
  "Look something up as a one-off (no shell history) and output to BUFFER.

Inputs:

Either (or all) of MODEL-VERSION, SYSTEM-PROMPT, PROMPT,
and PROMPT-URL (image file).

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
  (let* ((model (or (chatgpt-shell--resolved-model :versioned model-version)
                    (error "Model \"%s\" not found" model-version)))
         (settings (list (cons :streaming streaming)
                         (cons :temperature temperature)
                         (cons :system-prompt system-prompt)))
         (url (funcall (map-elt model :url)
                       :model model
                       :settings settings
                       :command prompt))
         (headers (when (map-elt model :headers)
                    (funcall (map-elt model :headers)
                             :model model
                             :settings settings))))
    (when streaming
      (display-buffer buffer))
    (unless (map-elt model :payload)
      (error "%s's %s not yet supported (please sponsor development)"
             (map-elt model :provider) (map-elt model :version)))
    ;; TODO: Make file access service agnostic.
    (when (and prompt-url
               (not (equal (map-elt model :provider) "OpenAI")))
      (error "%s's %s file upload is not yet supported (please sponsor development)"
             (map-elt model :provider) (map-elt model :version)))
    (shell-maker-make-http-request
     :async t
     :url url
     :proxy chatgpt-shell-proxy
     :data (apply (map-elt model :payload)
                  (append (list :model model
                                :context (list (cons prompt nil))
                                :settings settings)
                          (when (equal (map-elt model :provider) "OpenAI")
                            (list :prompt-url prompt-url))))
     :headers headers
     :filter (map-elt model :filter)
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
                       :settings settings
                       :command (car (car (last context)))))
         (payload (map-elt model :payload))
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
             :proxy chatgpt-shell-proxy
             :data (when payload
                     (funcall payload
                              :model model
                              :context context
                              :settings settings))
             :headers (when headers
                        (funcall headers
                                 :model model
                                 :settings settings))
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
              :proxy chatgpt-shell-proxy
              :data (when payload
                      (funcall payload
                               :model model
                               :context context
                               :settings settings))
              :headers (when headers
                         (funcall headers
                                  :model model
                                  :settings settings))
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

(defcustom chatgpt-shell-screenshot-command
  (if (eq system-type 'darwin)
      '("/usr/sbin/screencapture" "-i")
    ;; ImageMagick is common on Linux and many other *nix systems.
    '("/usr/bin/import"))
  "The program to use for capturing screenshots.

Assume screenshot file path will be appended to this list."
  :type '(repeat string)
  :group 'chatgpt-shell)

(defun chatgpt-shell--current-image-file (&optional capture)
  "Return buffer image file, Dired selected file, or image at point.

If optional CAPTURE is non-nil, cature a screenshot."
  (when (or (and (use-region-p) (derived-mode-p 'image-mode))
            (and (use-region-p) (derived-mode-p 'dired-mode)))
    (user-error "No region selection supported"))
  (cond (capture
         (redisplay) ;; Call process will block. Give redisplay a chance.
         (when-let ((file (make-temp-file "screenshot" nil ".png"))
                    (success (eq 0 (apply #'call-process
                                          (append
                                           (list
                                            (car chatgpt-shell-screenshot-command)
                                            nil
                                            nil
                                            nil)
                                           (cdr chatgpt-shell-screenshot-command)
                                           (list file)))))
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
  (if (map-elt model :ignore-context)
      (list (cons command nil))
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
       1))))

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
            (markdown-overlays-put)))
      (user-error "No block at point"))))

(defun chatgpt-shell-remove-block-overlays ()
  "Remove block overlays.  Handy for renaming blocks."
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (delete-overlay overlay)))

(defun chatgpt-shell-refresh-rendering ()
  "Refresh markdown rendering by re-applying to entire buffer."
  (interactive)
  (markdown-overlays-put))

(defun chatgpt-shell--invert-ranges (ranges min max)
  "Invert a list of RANGES within the interval [MIN, MAX].
Each range is a cons of start and end integers."
  (let ((result nil)
        (start min))
    (dolist (range ranges)
      (when (< start (car range))
        (push (cons start (car range)) result))
      (setq start (cdr range)))
    (when (< start max)
      (push (cons start max) result))
    result))

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
          (markdown-overlays-put)
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

(defun chatgpt-shell-block-action-at-point ()
  "Return t if block at point has an action.  nil otherwise."
  (let* ((source-block (chatgpt-shell-markdown-block-at-point))
         (language (markdown-overlays--resolve-internal-language
                    (map-elt source-block 'language)))
         (actions (chatgpt-shell--get-block-actions language)))
    actions
    (if actions
        actions
      (chatgpt-shell--org-babel-command language))))

(defun chatgpt-shell--get-block-actions (language)
  "Get block actions for LANGUAGE."
  (map-elt chatgpt-shell-source-block-actions
           (markdown-overlays--resolve-internal-language
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
                  (markdown-overlays--resolve-internal-language
                   (map-elt block 'language))))
            (chatgpt-shell-execute-babel-block-action-at-point)
          (user-error "No primary action for %s blocks" (map-elt block 'language))))
    (user-error "No block at point")))

(defun chatgpt-shell-edit-block-at-point ()
  "Execute block at point."
  (interactive)
  (error "Not yet supported")
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (chatgpt-shell--view-code :language (map-elt block 'language)
                                :code (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end))
                                :on-finished (lambda (code)
                                               (when-let ((inhibit-read-only t)
                                                          (success code))
                                                 (deactivate-mark)
                                                 (delete-region (map-elt block 'start)
                                                                (map-elt block 'end))
                                                 (insert "\n"
                                                         (string-trim code)
                                                         "\n")
                                                 (markdown-overlays-put))))
    (user-error "No block at point")))

(defun chatgpt-shell-view-block-at-point ()
  "View code block at point (using language's major mode)."
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (chatgpt-shell--view-code :language (map-elt block 'language)
                                :code (buffer-substring-no-properties
                                       (map-elt block 'start)
                                       (map-elt block 'end))
                                :on-finished #'identity)
    (user-error "No block at point")))

(defun chatgpt-shell-copy-block-at-point ()
  "Copy code block at point to the kill ring."
  (interactive)
  (if-let ((block (chatgpt-shell-markdown-block-at-point)))
      (let ((code (buffer-substring-no-properties
                   (map-elt block 'start)
                   (map-elt block 'end))))
        (kill-new code)
        (message "Copied block to kill ring"))
    (user-error "No block at point")))

(cl-defun chatgpt-shell--view-code (&key edit language code on-finished)
  "Open a temporary buffer for editing CODE in LANGUAGE major mode.
When done, invoke the FINISHED function with the resulting code.

ARGS:
- EDIT: To enable editing CODE in own buffer.
- LANGUAGE: A string with the language name.
- CODE: A string with the initial content of the buffer.
- ON-FINISHED: Invoked with saved changes, nil if cancelled."
  (let* ((block-buffer (current-buffer))
         (edit-buffer (generate-new-buffer (format "*%s code block*"
                                                   (if edit "edit" "view"))))
         (buffer-name (buffer-name edit-buffer))
         (language-mode (intern (concat (or
                                         (markdown-overlays--resolve-internal-language language)
                                         (downcase (string-trim language)))
                                        "-mode"))))
    (switch-to-buffer edit-buffer)
    (set-visited-file-name (make-temp-file "chatgpt-shell-edit-block"))
    (rename-buffer buffer-name)
    (add-hook 'after-change-functions
              (lambda (_beg _end _len)
                (set-buffer-modified-p nil)) nil t)
    (funcall language-mode)
    (insert (or code ""))
    (set-buffer-modified-p nil)
    (if edit
        (progn
          (setq header-line-format
                (concat
                 " "
                 (propertize "C-c '" 'face 'help-key-binding)
                 " to Save. "
                 (propertize "C-c C-k" 'face 'help-key-binding)
                 " to Cancel and Discard."))
          (let ((local-map (make-sparse-keymap)))
            (define-key local-map (kbd "C-c '") (lambda ()
                                                  (interactive)
                                                  (let ((result (buffer-string)))
                                                    (with-current-buffer block-buffer
                                                      (funcall on-finished result))
                                                    (set-buffer-modified-p nil)
                                                    (kill-buffer edit-buffer))))
            (define-key local-map (kbd "C-c C-k") (lambda ()
                                                    (interactive)
                                                    (with-current-buffer block-buffer
                                                      (funcall on-finished nil))
                                                    (set-buffer-modified-p nil)
                                                    (kill-buffer edit-buffer)))
            (use-local-map local-map)))
      (setq header-line-format
            (concat
             " Press "
             (propertize "q" 'face 'help-key-binding)
             " to exit"))
      (let ((local-map (make-sparse-keymap)))
        (define-key local-map (kbd "q") (lambda ()
                                          (interactive)
                                          (quit-restore-window
                                           (get-buffer-window edit-buffer) 'kill)))
        (use-local-map local-map))
      (setq buffer-read-only t))
    (goto-char (point-min))))

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
      (if-let* ((language (markdown-overlays--resolve-internal-language
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
  (with-temp-file (expand-file-name ".chatgpt-shell.el" chatgpt-shell-root-path)
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
      (insert-file-contents (expand-file-name ".chatgpt-shell.el" chatgpt-shell-root-path))
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

(defun chatgpt-shell--region ()
  "Return region info (ensuring start is always at bol).

Of the form

\((:buffer . buffer)
 (:start . start)
 (:end . end)
 (:text . text))"
  (when (region-active-p)
    (let ((start (save-excursion
                   ;; Always select from beginning of line.
                   (goto-char (region-beginning))
                   (line-beginning-position)))
          (end (region-end)))
      ;; Barf trailing space from selection.
      (let ((text (buffer-substring-no-properties
                   start
                   end)))
        (when (string-match "[ \n\t]+$"
                            text)
          (setq end (- end (length (match-string 0 text))))))
      (list (cons :start start)
            (cons :end end)
            (cons :buffer (current-buffer))
            (cons :text (buffer-substring start end))))))

(cl-defun chatgpt-shell-request-and-insert-merged-response (&key query
                                                                 context
                                                                 (buffer (current-buffer))
                                                                 (region (chatgpt-shell--region))
                                                                 model-version
                                                                 system-prompt
                                                                 remove-block-markers
                                                                 on-iterate)
  "Send a contextless request (no history) and merge into BUFFER:

QUERY: Request query text.
CONTEXT: Any history context to include.
BUFFER (optional): Buffer to insert to or omit to insert to current buffer.
MODEL-VERSION (optional): Index from `chatgpt-shell-models' or string.
SYSTEM-PROMPT (optional): As string."
  (unless query
    (error "Missing mandatory \"query\" param"))
  (unless region
    (error "No region selected"))
  (let ((progress-reporter (make-progress-reporter
                            (format "%s " (chatgpt-shell--model-label)))))
    (chatgpt-shell--fader-start-fading-region (map-elt region :start)
                                              (map-elt region :end))
    (progress-reporter-update progress-reporter)
    (chatgpt-shell-post :context (append
                                  context
                                  (list (cons query nil)))
                        :system-prompt system-prompt
                        :version model-version
                        :streaming t
                        :on-output (lambda (_chunk)
                                     (progress-reporter-update progress-reporter))
                        :on-success (lambda (output)
                                      (with-current-buffer buffer
                                        (when remove-block-markers
                                          (setq output (chatgpt-shell--remove-source-block-markers output)))
                                        (chatgpt-shell--fader-stop-fading)
                                        (progress-reporter-done progress-reporter)
                                        (if (equal (string-trim output)
                                                   (string-trim (map-elt region :text)))
                                            (message "No change suggested")
                                          (let ((choice (chatgpt-shell--pretty-smerge-insert
                                                         :text output
                                                         :start (map-elt region :start)
                                                         :end (map-elt region :end)
                                                         :buffer buffer
                                                         :iterate on-iterate)))
                                            (cond ((and on-iterate
                                                        (eq choice ?i))
                                                   (funcall on-iterate output))
                                                  ((eq choice ?n)
                                                   (set-mark (map-elt region :end))
                                                   (goto-char (map-elt region :start))))))))
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

;;;###autoload
(defun chatgpt-shell-quick-insert(&optional context)
  "Request from minibuffer and insert response into current buffer.

Optionally include any CONTEXT to consider."
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
    (if-let ((region (chatgpt-shell--region)))
        (progn
          (setq query (concat query
                              "\n\n"
                              "Apply my instruction to:"
                              "\n\n"
                              (map-elt region :text)))
          (chatgpt-shell-request-and-insert-merged-response
           :system-prompt system-prompt
           :query query
           :context context
           :remove-block-markers t
           :region region
           :on-iterate (lambda (output)
                         (set-mark (map-elt region :end))
                         (goto-char (map-elt region :start))
                         (chatgpt-shell-quick-insert (append context
                                                             (list (cons query output)))))))
      (chatgpt-shell-request-and-insert-response
       :streaming t
       :system-prompt system-prompt
       :query query))))

(defun chatgpt-shell-mark-block ()
  "Mark current block in compose buffer."
  (interactive)
  (when-let ((block (chatgpt-shell-markdown-block-at-point)))
    (set-mark (map-elt block 'end))
    (goto-char (map-elt block 'start))))

(defun chatgpt-shell--fetch-model-icon (icon)
  "Download ICON filename from GitHub, only if it exists and save as binary.

ICON names can be found at https://github.com/lobehub/lobe-icons/tree/master/packages/static-png

ICONs starting with https:// are downloaded directly from that location."
  (when icon
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (url (if (string-prefix-p "https://" (downcase icon))
                    icon
                  (concat "https://raw.githubusercontent.com/lobehub/lobe-icons/refs/heads/master/packages/static-png/"
                          mode "/" icon)))
           (filename (file-name-nondirectory url))
           (cache-dir (file-name-concat (temporary-file-directory) "chatgpt-shell" mode))
           (cache-path (expand-file-name filename cache-dir)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((buffer (url-retrieve-synchronously url t t 5.0)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (if (re-search-forward "^HTTP/1.1 200 OK" nil t)
                  (progn
                    (re-search-forward "\r?\n\r?\n")
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region (point) (point-max) cache-path)))
                (message "Icon fetch failed: %s" url)))
            (kill-buffer buffer))))
      (when (file-exists-p cache-path)
        cache-path))))

;; pretty smerge start

(cl-defun chatgpt-shell--pretty-smerge-insert (&key text start end buffer iterate)
  "Insert TEXT, replacing content of START and END at BUFFER.

With non-nil ITERATE, ask user if they'd like to iterate further on change.

Return non-nil if either inserted or cancelled (for manual merge)."
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
                  :new-label "After" :new text))
           (outcome))
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
                (setq outcome
                      (if iterate
                          (read-char-choice (substitute-command-keys
                                             "Apply change? (\\`y' or \\`n') / Iterate? (\\`i'): ")
                                            '(?y ?n ?i))
                        (read-char-choice (substitute-command-keys
                                           "Apply change? (\\`y' or \\`n'): ")
                                          '(?y ?n))))
                (cond
                 ((eq outcome ?y)
                  (smerge-keep-lower)
                  (message "Applied"))
                 ((eq outcome ?n)
                  (smerge-keep-upper)
                  (message "Skipped"))
                 ((eq outcome ?i)
                  (smerge-keep-upper))
                 (t
                  (smerge-keep-upper)))
                (smerge-mode -1)
                outcome)
            (chatgpt-shell--pretty-smerge-mode -1)
            (when needs-wrapping
              (visual-line-mode -1))
            outcome)
        (quit
         (chatgpt-shell--pretty-smerge-mode -1)
         (when needs-wrapping
           (visual-line-mode -1))
         t)
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
  (let* ((start-color (or (face-background 'region) "#808080"))
         (end-color (or (face-background 'default) "#1a1a1a"))
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
