;; -*- no-byte-compile: t; -*-
;;; cc/bindings/config.el -*- lexical-binding: t; -*-

;; persp-mode and projectile in different prefixes
(when (modulep! :ui workspaces)
  (setq! persp-keymap-prefix (kbd "C-c w")))

(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Unset global keybindings
(undefine-key! global-map
  "C-z"
  "C-x C-z")

(remove-hook! 'doom-first-input-hook #'which-key-mode)
(add-hook! 'doom-first-buffer-hook
           :append
           #'which-key-mode)

;; which-key sort by description
(after! which-key
  (setq! which-pkey-sort-order 'which-key-description-order))


;; Autoloads
(autoload 'org-capture-goto-target "org-capture" nil t)
(autoload 'recentf-open-files "recentf" nil t)

;; Adding which-key descriptions
(after! which-key
  (which-key-add-key-based-replacements
    "C-x <RET>" "coding-system"
    "M-s h" "highlight"
    "C-x 8" "emoji"
    "C-x 8 e" "emoji"
    "C-x n" "narrow/widen"
    "C-x r" "register"
    "C-x t" "tab"
    "C-x w" "win-select"
    "C-x x" "buffer-ops"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x p" "project"
    "C-x a" "abbrev"))

;; Global keybindings
(map! :after which-key
      :prefix "C-c"
      :desc "Redo" "r" #'undo-fu-only-redo
      :desc "Redo all" "R" #'undo-fu-redo-all

      ;;; C-c t --- toggle
      (:prefix-map ("t" . "<toggle>")
       :desc "Big font mode" "b" #'doom-big-font-mode
       :desc "Fullscreen" "F" #'toggle-frame-fullscreen
       :desc "Flymake" "f" #'flymake-mode ; TODO: just give it a try
       :desc "Line numbers" "l" #'doom/toggle-line-numbers
       :desc "Tab/Space switch" "t" #'doom/toggle-indent-style
       :desc "Read-only mode" "r" #'read-only-mode
       (:when (modulep! :checkers syntax)
         :desc "Flycheck" "c" #'flycheck-mode)
       (:when (modulep! :ui minimap)
         :desc "Minimap" "m" #'minimap-mode)
       (:when (modulep! :lang org +present)
         :desc "Org presentation" "p" #'org-tree-slide-mode)
       (:when (modulep! :ui zen)
         :desc "Zen mode" "z" #'+zen/toggle
         :desc "Zen mode (fullscreen)" "Z" #'+zen/toggle-fullscreen))


      ;;; C-c y --- snippets
      (:prefix-map ("y" . "<snippets>")
       :desc "New snippet" "n" #'+snippets/new
       :desc "Edit snippet" "e" #'+snippets/edit
       :desc "Find snippet" "f" #'+snippets/find
       :desc "Browse snippets" "b" #'+default/browse-templates)

      ;;; C-c c --- code
      (:prefix-map ("c" . "<code>")
       :desc "Compile" "c" #'compile
       :desc "Format buffer/region" "f" #'+format/region-or-buffer
       :desc "List errors" "e" #'+default/diagnostics

       ;; TODO: lsp bindings
       ;; (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
       ;;   :desc "LSP Code actions" "a" #'lsp-execute-code-action
       ;;   :desc "LSP Organize imports" "o" #'lsp-organize-imports
       ;;   :desc "LSP Rename" "r" #'lsp-rename)
       ;;   (:when (modulep! :completion ivy)
       ;;     :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
       ;;     :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
       ;;   (:when (modulep! :completion helm)
       ;;     :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
       ;;     :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)
       ;;   (:when (modulep! :completion vertico)
       ;;     :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols
       ;;     :desc "Jump to symbol in any workspace"     "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces))
       ;;   (:when (modulep! :ui treemacs +lsp)
       ;;     :desc "Errors list"                         "X"   #'lsp-treemacs-errors-list
       ;;     :desc "Incoming call hierarchy"             "y"   #'lsp-treemacs-call-hierarchy
       ;;     :desc "Outgoing call hierarchy"             "Y"   (cmd!! #'lsp-treemacs-call-hierarchy t)
       ;;     :desc "References tree"                     "R"   (cmd!! #'lsp-treemacs-references t)
       ;;     :desc "Symbols"                             "S"   #'lsp-treemacs-symbols))
       ;; (:when (modulep! :tools lsp +eglot)
       ;;   :desc "LSP Execute code action"              "a" #'eglot-code-actions
       ;;   :desc "LSP Rename"                           "r" #'eglot-rename
       ;;   :desc "LSP Find declaration"                 "j" #'eglot-find-declaration
       ;;   (:when (modulep! :completion vertico)
       ;;     :desc "Jump to symbol in current workspace" "j" #'consult-eglot-symbols))
       )


      ;;; C-c k --- lookup
      (:prefix-map ("k" . "<lookup>")
       :desc "Jump to definition" "l" #'+lookup/definition
       :desc "Jump to references" "r" #'+lookup/references
       :desc "Find implementations" "i" #'+lookup/implementations
       :desc "Jump to documentation" "d" #'+lookup/documentation
       :desc "Find type definition" "t" #'+lookup/type-definition
       :desc "Search online" "o" #'+lookup/online
       (:when (modulep! :tools lookup +docsets)
         :desc "Search in docsets" "s" #'+lookup/in-docsets
         :desc "Search in all docsets" "S" #'+lookup/in-all-docsets
         :desc "Install offline docsets""D" #'dash-docs-install-docset))

      ;;; C-c f --- file
      (:prefix-map ("f" . "<file>")
       :desc "Recent files" "r" #'recentf-open-files
       :desc "Copy this file" "c" #'doom/copy-this-file
       :desc "Delete this file" "D" #'doom/delete-this-file
       :desc "Move this file" "m" #'doom/move-this-file
       :desc "Find file in private config" "p"
       #'doom/find-file-in-private-config
       :desc "Browse private config" "b" #'doom/open-private-config
       :desc "Sudo this file" "s" #'doom/sudo-this-file
       :desc "Sudo find file" "S" #'doom/sudo-find-file
       :desc "Copy file path" "y" #'+default/yank-buffer-path
       :desc "Open scratch buffer" "x" #'doom/open-scratch-buffer)


      ;;; C-c o --- open
      (:prefix-map ("o" . "<open>"))


      ;;; C-c s --- search
      (:prefix-map ("s" . "<search>"))


      ;;; C-c i --- insert
      (:prefix-map ("i". "<insert>")
       :desc "Unicode" "u" #'insert-char
       :desc "Current file name" "f" #'+default/insert-file-path
       :desc "From clipboard" "y" #'+default/yank-pop)


      ;;; C-c n --- note
      (:prefix-map ("n" . "<note>")
       :desc "Browse notes" "n" #'+default/browse-notes)

      )


;; local keybindings
(map! :after which-key
      :prefix ("C-c l" . "<local>")


      ;; C-c l e --- eval
      (:when (modulep! :tools eval)
        :map prog-mode-map
        :prefix-map ("e" . "<eval/quickrun>")
        :desc "Eval buffer" "b" #'+eval/buffer-or-region
        :desc "Eval line" "r" #'+eval/line-or-region
        :desc "Send to REPL" "s" #'+eval/send-region-to-repl
        :desc "Open REPL" "i" #'+eval/open-repl-other-window))
