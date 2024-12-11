;; -*- no-byte-compile: t; -*-
;;; cc/bindings/config.el -*- lexical-binding: t; -*-

;; persp-mode and projectile in different prefixes
(when (modulep! :ui workspaces)
  (setq! persp-keymap-prefix (kbd "C-c w p"))
  (which-key-add-key-based-replacements "C-c w p" "persp-mode"))

;; Unset global keybindings
(undefine-key! global-map
  "C-z"
  "C-x C-z")

(setq!
 doom-leader-key nil
 doom-localleader-key nil
 doom-leader-alt-key "C-c M-d"
 doom-localleader-alt-key "C-c M-d l")

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
(autoload 'projectile-recentf "projectile" nil t)

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
    "C-x a" "abbrev"
    "C-h 4" "info"
    "C-c M-d" "doom/leader"
    "C-c M-d l" "doom/localleader"))

;; Global keybindings
(map! :after which-key
      :desc "ibuffer" "C-x C-b" #'ibuffer
      :desc "Switch buffer" "C-x b" #'switch-to-buffer
      :desc "Switch buffer" "C-x 4 b" #'switch-to-buffer-other-window
      (:when (modulep! :ui treemacs)
        "<f1>" #'+treemacs/toggle
        "C-x C-o" #'treemacs-select-window
        :prefix ("C-c C-w" . "tree-workspace")
        :prefix ("C-c C-p" . "tree-project")))

;; "C-c" keybindings
(map! :after which-key
      :prefix "C-c"

      ;; C-c u --- undo
      (:prefix-map ("u" . "<undo>")
       :desc "Undo" "u" #'undo-fu-only-undo
       :desc "Undo tree redo" "r" #'undo-fu-only-redo
       :desc "Undo tree redo all" "R" #'undo-fu-redo-all)

      ;; DONE C-c t --- toggle
      (:prefix-map ("t" . "<toggle>")
       :desc "Big font mode" "b" #'doom-big-font-mode
       :desc "Fullscreen" "F" #'toggle-frame-fullscreen
       :desc "Flymake" "f" #'flymake-mode ; TODO: just give it a try
       :desc "Line numbers" "l" #'doom/toggle-line-numbers
       :desc "Treemacs" "t" #'+treemacs/toggle
       :desc "Tab/Space switch" "T" #'doom/toggle-indent-style
       :desc "Read-only mode" "r" #'read-only-mode
       (:when (modulep! :checkers syntax)
         :desc "Flycheck" "c" #'flycheck-mode)
       (:when (modulep! :ui minimap)
         :desc "Minimap" "m" #'minimap-mode)
       (:when (modulep! :lang org +present)
         :desc "Org presentation" "p" #'org-tree-slide-mode)
       (:when (modulep! :ui zen)
         :desc "Zen mode" "z" #'+zen/toggle
         :desc "Zen mode (fullscreen)" "Z" #'+zen/toggle-fullscreen)
       (:when (modulep! :ui indent-guides)
         :desc "Indent guides" "i" #'indent-bars-mode))


      ;; C-c y --- snippets
      (:prefix-map ("y" . "<snippets>")
       :desc "New snippet" "n" #'+snippets/new
       :desc "Edit snippet" "e" #'+snippets/edit
       :desc "Find snippet" "f" #'+snippets/find
       :desc "Browse snippets" "b" #'+default/browse-templates)

      ;; C-c c --- code
      (:prefix-map ("c" . "<code>")
       :desc "Compile" "c" #'+default/compile
       :desc "Format buffer/region" "f" #'+format/region-or-buffer
       :desc "List errors" "e" #'+default/diagnostics
       :desc "Inlay Hints Mode" "i" #'lsp-inlay-hints-mode
       (:when (modulep! :tools make)
         :desc "Make run target" "m" #'+make/run
         :desc "Make run last" "M" #'+make/run-last)

       (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
         :desc "LSP Code actions" "a" #'lsp-execute-code-action
         :desc "LSP Organize imports" "o" #'lsp-organize-imports
         :desc "LSP Rename" "r" #'lsp-rename)
       (:when (modulep! :tools lsp +peek)
         :desc "LSP Doc glance" "g" #'lsp-ui-doc-glance)
       (:when (modulep! :completion vertico)
         :desc "Jump to symbol" "j"   #'consult-lsp-symbols))

      ;; C-c k --- lookup
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
         :desc "Install offline docsets""D" #'dash-docs-install-docset)
       (:when (modulep! :tools lsp +peek)
         :desc "LSP Doc glance" "g" #'lsp-ui-doc-glance))

      ;; DONE C-c f --- file
      (:prefix-map ("f" . "<file>")
       :desc "Recent files" "r"
       (cond ((modulep! :completion vertico) #'consult-recent-file)
             (t #'recentf-open-files))
       :desc "Copy this file" "c" #'doom/copy-this-file
       :desc "Delete this file" "d" #'doom/delete-this-file
       :desc "Move this file" "m" #'doom/move-this-file
       :desc "Locate file" "l"
       (cond ((modulep! :completion vertico) #'consult-locate)
             (t #'locate))
       :desc "Find file under here (-r)" "." #'+default/find-file-under-here
       :desc "Find roam note" "n" #'org-roam-node-find
       :desc "Find agenda file" "a" #'+default/find-in-notes
       :desc "Find in doom" "p" #'doom/find-file-in-private-config
       :desc "Browse in doom" "P" #'doom/open-private-config
       :desc "Find in emacsd" "e" #'doom/find-file-in-emacsd
       :desc "Browse in emacsd" "E" #'doom/browse-in-emacsd
       :desc "Sudo this file" "s" #'doom/sudo-this-file
       :desc "Sudo find file" "S" #'doom/sudo-find-file
       :desc "Copy file path" "y" #'+default/yank-buffer-path)

      ;; C-c p --- project
      (:prefix-map ("p" . "<project>")
       :desc "Open current editorconfig" "e" #'editorconfig-find-current-editorconfig
       :desc "Search project" "s" #'+default/search-project
       :desc "Switch project" "p" #'projectile-switch-project
       :desc "Recent files" "R" #'projectile-recentf
       :desc "List todos" "t" #'magit-todos-list
       :desc "Replace in project" "r" #'projectile-replace
       :desc "Find file" "f" #'projectile-find-file
       :desc "Project dired" "d" #'+default/browse-project
       :desc "Search symbol" "." #'+default/search-project-for-symbol-at-point)


      ;; C-c a --- agenda
      (:prefix-map ("a" . "<agenda>")
       :desc "Find agenda file" "f" #'+default/find-in-notes
       :desc "Agenda view""a" #'org-agenda
       :desc "Agenda capture" "c" #'org-capture
       (:map org-mode-map
        :prefix ("e" . "<effort>")
        :desc "Add estimate" "a" #'org-set-effort
        :desc "Edit estimate" "e" #'org-clock-modify-effort-estimate
        :desc "Clock in" "i" #'org-clock-in
        :desc "Clock out" "o" #'org-clock-out
        :desc "Cancel clock" "c" #'org-clock-cancel
        :desc "Goto clock" "g" #'org-clock-goto))


      ;; C-c o --- open
      (:prefix-map
       ("o" . "<open>")
       (:when (modulep! :app calendar)
         :desc "Calendar" "c" #'calendar)
       (:when (modulep! :term vterm)
         :desc "vterm" "t" #'+vterm/toggle
         :desc "vterm here" "T" #'+vterm/here)
       (:when (modulep! :tools ein)
         (:prefix-map ("j" . "<jupyter>")
          :desc "Jupyter run" "r" #'ein:run
          :desc "Jupyter login" "l" #'ein:login
          :desc "Jupyter stop" "s" #'ein:stop))
       (:when (modulep! :tools docker)
         :desc "Docker" "d" #'docker))

      ;; C-c w --- workspace
      (:prefix-map
       ("w" . "<workspace>")
       (:when (modulep! :ui workspaces)
         ;; workspace
         :desc "New workspace" "n" #'+workspace/new-named
         :desc "Save workspace" "s" #'+workspace/save
         :desc "Restore workspace" "r" #'+workspace/load
         :desc "Delete workspace" "d" #'+workspace/delete
         :desc "Switch workspace" "w" #'+workspace/switch-to
         :desc "Display workspaces" "l" #'+workspace/display
         :desc "Switch to last workspace" "o" #'+workspace/other

         ;; session
         :desc "[Session] Load" "L" #'doom/load-session
         :desc "[Session] Save" "S" #'doom/save-session
         :desc "[Session] Load last" "q" #'doom/quickload-session))


      ;; C-c s --- search
      (:prefix-map ("s" . "<search>")
       :desc "Search line" "l"
       (cond ((modulep! :completion vertico)   #'consult-line)
             ((modulep! :completion ivy)       #'swiper)
             ((modulep! :completion helm)      #'swiper))
       :desc "List colors" "C" #'list-colors-display
       (:when (modulep! :tools lookup)
         :desc "Word dictionary" "w" #'+lookup/dictionary-definition
         :desc "Thesaurus/θɪˈsɔːrəs/" "t" #'+lookup/synonyms
         :desc "Find file" "f" #'+lookup/file))


      ;; C-c i --- insert
      (:prefix-map ("i". "<insert>")
       :desc "Unicode" "u" #'insert-char
       :desc "Current file name" "f" #'+default/insert-file-path
       :desc "From clipboard" "y" #'+default/yank-pop
       (:when (modulep! :ui emoji)
         :desc "Emoji" "e" #'emojify-insert-emoji))


      ;; C-c n --- note
      (:prefix-map
       ("n" . "<note>")
       (:when (modulep! :lang org +roam2)
         :desc "Fleet note" "j" #'org-roam-dailies-find-today
         :desc "Find note" "f" #'org-roam-node-find
         :desc "Find ref" "r" #'org-roam-ref-find
         :desc "Show graph" "g" #'org-roam-graph
         :desc "Insert node" "i" #'org-roam-node-insert
         :desc "Capture" "c" #'org-roam-capture
         :desc "Show backlinks" "b" #'org-roam-buffer-toggle
         :desc "Show backlinks(dedicated)" "B" #'org-roam-buffer-display-dedicated
         :desc "Sync db" "s" #'org-roam-db-sync
         :desc "Refile node" "w" #'org-roam-refile
         (:prefix ("a" . "alias")
          :desc "Add alias" "a" #'org-roam-alias-add
          :desc "Remove alias" "r" #'org-roam-alias-remove)
         (:prefix ("r" . "ref")
          :desc "Add ref" "a" #'org-roam-ref-add
          :desc "Remove ref" "r" #'org-roam-ref-remove
          :desc "Find ref" "f" #'org-roam-ref-find)
         (:prefix ("t" . "tag")
          :desc "Add tag" "a" #'org-roam-tag-add
          :desc "Remove tag" "r" #'org-roam-tag-remove)
         (:prefix ("d" . "by date")
          :desc "Goto date" "d" #'org-roam-dailies-goto-date
          :desc "Capture date" "c" #'org-roam-dailies-capture-date
          :desc "Goto tomorrow" "m" #'org-roam-dailies-goto-tomorrow
          :desc "Goto today" "t" #'org-roam-dailies-goto-today
          :desc "Goto yesterday" "y" #'org-roam-dailies-goto-yesterday
          :desc "Find dir" "f" #'org-roam-dailies-find-directory)
         (:map org-roam-mode-map
          :desc "Visit node" "v" #'org-roam-node-visit)
         ))

      ;; C-c r --- remote
      (:prefix-map ("r" . "<remote>"))
      )


;; local keybindings
(map! :after which-key
      :prefix ("C-c m" . "<local>")

      ;; C-c m r --- run/eval
      (:prefix-map
       ("r" . "<run/eval>")
       (:when (modulep! :tools eval)
         :map prog-mode-map
         :desc "Eval buffer" "b" #'+eval/buffer-or-region
         :desc "Eval line" "r" #'+eval/line-or-region
         :desc "Send to REPL" "s" #'+eval/send-region-to-repl
         :desc "Open REPL" "i" #'+eval/open-repl-other-window))


      ;; C-c m t --- tmux
      (:when (modulep! :tools tmux)
        :map prog-mode-map
        :prefix-map ("t" . "<tmux>")
        :desc "Run" "r" #'+tmux/run
        :desc "Rerun" "R" #'+tmux/rerun
        :desc "cd" "c" #'+tmux/cd
        :desc "cd to here" "h" #'+tmux/cd-here
        :desc "cd to project" "p" #'+tmux/cd-project
        :desc "Send region" "s" #'+tmux/send-region))
