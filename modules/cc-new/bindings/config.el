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
         :desc "Emoji" "e" #'cape-emoji
         :desc "dabbrev" "d" #'cape-dabbrev)
       )
      )
