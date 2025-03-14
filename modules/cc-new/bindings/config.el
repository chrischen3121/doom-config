;;; cc-new/bindings/config.el -*- lexical-binding: t; -*-
;; -*- no-byte-compile: t; -*-

;; Unset global keybindings
(undefine-key! global-map
  "C-z"
  "C-x C-z"
  "C-x 8" ; emoji
  "C-h 4" ; info other window
  )

(setq!
 doom-leader-key nil
 doom-localleader-key nil
 doom-leader-alt-key "C-c M-d"
 doom-localleader-alt-key "C-c M-d l")

;; which-key configuration
(remove-hook! 'doom-first-input-hook #'which-key-mode)
(add-hook! 'doom-first-buffer-hook
           :append
           #'which-key-mode)

(after! which-key
  (setq! which-pkey-sort-order 'which-key-description-order))

(after! which-key
  (which-key-add-key-based-replacements "C-c 1" "<checker>"))

;; C-x keybindings
(map! :after which-key
      :prefix "C-x"
      :desc "ibuffer" "C-b" #'ibuffer)

;; C-c keybindings
(map! :after which-key
      :prefix "C-c"

      ;; C-c g -- global keybindings
      (:prefix-map
       ("g" . "<global>")
       (:when (modulep! :ui popup)
         :desc "Close all popups" "c" #'+popup/close-all)
       )

      ;; C-c w --- workspace
      (:prefix-map
       ("w" . "<workspace>")
       (:when (modulep! :ui workspaces)
         :desc "Make workspace" "m" #'+workspace/new-named
         :desc "Save workspace" "s" #'+workspace/save
         :desc "Load workspace" "l" #'+workspace/load
         :desc "Remove workspace" "r" #'+workspace/delete
         :desc "Switch workspace" "o" #'+workspace/switch-to
         :desc "Display workspaces" "d" #'+workspace/display)
       :desc "Kill all buffers" "k" #'doom/kill-all-buffers
       ;; session
       :desc "Load last session" "w" #'doom/quickload-session)

      ;; C-c e -- edit/writing
      (:prefix-map
       ("e" . "<edit>")
       (:when (modulep! :editor multiple-cursors)
         (:prefix ("m" . "<multicursors>")
          :desc "Edit lines" "e" #'mc/edit-lines
          :desc "Mark next like this" "n" #'mc/mark-next-like-this
          :desc "Mark previous like this" "p" #'mc/mark-previous-like-this
          :desc "Mark all like this" "a" #'mc/mark-all-like-this))
       (:when (modulep! :emacs undo)
         (:prefix ("u" . "<undo>")
          :desc "Undo" "u" #'undo-fu-only-undo
          :desc "Undo tree redo" "r" #'undo-fu-only-redo
          :desc "Undo tree redo all" "R" #'undo-fu-redo-all))
       (:when (modulep! :checkers spell)
         (:prefix ("s" . "<spell>")
          :desc "Correct this word" "c" #'+spell/correct
          :desc "Add word to dict""a" #'+spell/add-word
          :desc "Remove word" "r" #'+spell/remove-word
          (:unless (modulep! :checkers spell +flyspell)
            :desc "Toggle spell-fu" "t" #'spell-fu-mode
            :desc "Reset word cache" "k" #'spell-fu-reset
            :desc "Next error" "n" #'spell-fu-goto-next-error
            :desc "Previous error" "p" #'spell-fu-goto-previous-error)))
       (:prefix
        ("w" . "<writing>")
        (:when (modulep! :checkers grammar)
          (:desc "Grammar check" "c" #'langtool-check
           :desc "Grammar correct" "e" #'langtool-correct-buffer
           :desc "Grade level" "g" #'writegood-grade-level
           :desc "Read ease score" "r" #'writegood-reading-ease)))
       )

      ;; C-c l -- local keybindings
      ;; which related to current major mode
      (:prefix-map
       ("l" . "<local>")
       )

      ;; C-c o -- open
      (:prefix-map
       ("o" . "<open>")
       :desc "New frame" "f" #'make-frame
       (:when (modulep! :term vterm)
         :desc "vterm" "t" #'+vterm/toggle)
       (:when (modulep! :tools docker)
         :desc "Docker" "d" #'docker)
       )

      ;; C-c k -- lookup
      (:prefix-map
       ("k" . "<lookup>")
       (:when (modulep! :tools lookup)
         :desc "Jump to definition" "." #'+lookup/definition
         :desc "Jump to references" "r" #'+lookup/references
         :desc "Jump to documentation" "k" #'+lookup/documentation
         :desc "Find implementations" "i" #'+lookup/implementations
         :desc "Find type definition" "t" #'+lookup/type-definition
         :desc "Search online" "o" #'+lookup/online
         :desc "Search dictionary" "d" #'+lookup/dictionary-definition)
       (:when (and (modulep! :completion vertico)
                   (modulep! :tools lsp))
         :desc "Search symbols" "s" #'consult-lsp-symbols)
       )

      ;; C-c t -- toggle
      (:prefix-map
       ("t" . "<toggle>")
       (:when (modulep! :ui treemacs)
         :desc "Treemacs" "t" #'+treemacs/toggle)
       (:when (modulep! :ui zen)
         :desc "zen-mode" "z" #'+zen/toggle)
       (:when (modulep! :editor word-wrap)
         :desc "Visual line mode" "v" #'+word-wrap-mode)
       (:when (modulep! :checkers grammar)
         :desc "writegood-mode" "w" #'writegood-mode)
       (:when (modulep! :checkers spell)
         :desc "spell-fu-mode" "s" #'spell-fu-mode)
       (:when (and (modulep! :checkers syntax)
                   (not (modulep! :checkers syntax +flymake)))
         :desc "Flycheck" "c" #'flycheck-mode)
       )

      ;; C-c c -- code keybindings
      (:prefix-map
       ("c" . "<code>")
       (:when (modulep! :tools eval)
         :prefix ("e" . "<eval>")
         (:map prog-mode-map
          :desc "Eval buffer" "b" #'+eval/buffer
          :desc "Eval region" "r" #'+eval/region
          :desc "Eval line" "l" #'+eval/line-or-region
          :desc "Send to REPL" "s" #'+eval/send-region-to-repl
          :desc "Open REPL" "o" #'+eval/open-repl-other-window
          :desc "Open REPL here" "O" #'+eval/open-repl-same-window)
         (:map emacs-lisp-mode-map
          :desc "Eval buffer" "b" #'eval-buffer
          :desc "Eval defun" "d" #'eval-defun
          :desc "Eval region" "r" #'eval-region
          :desc "Eval last sexp" "e" #'eval-last-sexp)
         )
       (:when (and (modulep! :tools lsp)
                   (not (modulep! :tools lsp +eglot)))
         :map lsp-mode-map
         :desc "Action" "a" #'lsp-execute-code-action
         (:prefix ("l" . "<lsp>")
          :desc "Organize imports" "i" #'lsp-organize-imports
          :desc "Rename" "r" #'lsp-rename
          :desc "Inlay Hints Mode" "I" #'lsp-inlay-hints-mode)
         (:prefix ("s". "<session>")
          :desc "Describe session" "d" #'lsp-describe-session
          :desc "Disconnect" "q" #'lsp-disconnect
          :desc "Shutdown" "k" #'lsp-workspace-shutdown
          :desc "Add folder" "a" #'lsp-workspace-folders-add
          :desc "Remove folder" "r" #'lsp-workspace-folders-remove
          :desc "Remove all folders" "R" #'lsp-workspace-remove-all-folders
          :desc "Unblock folders" "b" #'lsp-workspace-blocklist-remove
          :desc "Switch client" "s" #'+lsp/switch-client)
         (:when (modulep! :ui treemacs +lsp)
           :prefix ("t" . "<treemacs-lsp>")
           :desc "Incoming call hierarchy" "i" #'lsp-treemacs-call-hierarchy
           :desc "Outgoing call hierarchy" "o" (cmd!! #'lsp-treemacs-call-hierarchy t)
           :desc "Type hierarchy" "t" #'lsp-treemacs-type-hierarchy
           :desc "References tree" "r" (cmd!! #'lsp-treemacs-references t)
           )
         )
       )


      ;; C-c d -- debugging
      (:prefix-map
       ("d" . "<debug>")
       (:when (modulep! :tools debugger)
         :map prog-mode-map
         :desc "Run" "r" #'+debugger/start
         :desc "Quit" "q" #'+debugger/quit
         :desc "Run last" "l" #'+debugger/start-last
         )
       )

      ;; C-c y -- yasnippets
      (:when (modulep! :editor snippets)
        (:prefix-map ("y" . "<snippets>")
         :desc "New snippet" "n" #'+snippets/new
         :desc "Edit snippet" "e" #'+snippets/edit
         :desc "Find snippet" "f" #'+snippets/find
         :desc "Browse snippets" "b" #'+default/browse-templates))

      ;; C-c i -- insert keybindings
      (:prefix-map
       ("i" . "<insert>")
       (:when (modulep! :completion corfu)
         :desc "From dict" "d" #'cape-dict
         :desc "Emoji" "e" #'cape-emoji
         :desc "dabbrev" "a" #'cape-dabbrev)
       )
      )
