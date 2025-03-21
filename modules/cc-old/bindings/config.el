;; -*- no-byte-compile: t; -*-
;;; cc/bindings/config.el -*- lexical-binding: t; -*-
;; "C-c" keybindings
(map! :after which-key
      :prefix "C-c"



      ;; C-c r --- remote
      (:prefix-map ("r" . "<remote>"))
      )

;; local keybindings
(map! :after which-key
      :prefix "C-c l"

      ;; C-c l t --- tmux
      (:when (modulep! :tools tmux)
        :map prog-mode-map
        :prefix-map ("t" . "<tmux>")
        :desc "Run" "r" #'+tmux/run
        :desc "Rerun" "R" #'+tmux/rerun
        :desc "cd" "c" #'+tmux/cd
        :desc "cd to here" "h" #'+tmux/cd-here
        :desc "cd to project" "p" #'+tmux/cd-project
        :desc "Send region" "s" #'+tmux/send-region))
