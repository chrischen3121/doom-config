;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; :completion
;; company
;; Hints:
;; C-; +company/complete

;; :tools
;; debugger
(when (modulep! :tools debugger)
  (map!
   :desc "dap debug" "C-c g d" #'dap-debug
   :map dap-mode-map
   :prefix ("C-c d" . "dap-cmds")
   :desc "start" "d" #'dap-debug
   :desc "continue" "c" #'dap-continue
   :desc "next" "n" #'dap-next
   :desc "step-in" "i" #'dap-step-in
   :desc "step-out" "o" #'dap-step-out
   :desc "restart" "r" #'dap-debug-restart
   :desc "quit" "q" #'+debugger/quit
   :prefix ("C-c d b" . "breakpoints")
   :desc "toggle bp" "b" #'dap-breakpoint-toggle
   :desc "add bp" "a" #'dap-breakpoint-add
   :desc "delete bp" "d" #'dap-breakpoint-delete
   :desc "delete all bp" "D" #'dap-breakpoint-delete-all
   :desc "add condition" "c" #'dap-breakpoint-condition
   :desc "hit condition" "h" #'dap-breakpoint-hit-condition
   :desc "log message" "l" #'dap-breakpoint-log-message
   :prefix ("C-c d e" . "eval")
   :desc "eval" "e" #'dap-eval
   :desc "eval region" "r" #'dap-eval-region
   :desc "eval thing at point" "t" #'dap-eval-thing-at-point))


;; :tools
;; lsp +peek
(when (modulep! :tools lsp +peek)
  (setq! lsp-keymap-prefix (kbd "C-c ; l"))
  (map! :map overriding-local-map
        :leader
        :prefix ("; l" . "lsp"))
  (after! lsp-ui
    (setq! lsp-ui-sideline-show-diagnostics t
           lsp-ui-sideline-show-code-actions t
           lsp-ui-sideline-show-symbol t
           lsp-ui-sideline-delay 0.5
           lsp-ui-imenu-buffer-position 'left
           ;;lsp-ui-imenu-auto-refresh t
           lsp-ui-imenu-refresh-delay 2)

    (map!
     :map lsp-ui-mode-map
     [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
     [remap xref-find-references]  #'lsp-ui-peek-find-references
     :desc "Open lsp-ui imenu" "<f8>" #'lsp-ui-imenu
     :desc "Open lsp-ui imenu" "C-c t i" #'lsp-ui-imenu
     :map lsp-ui-imenu-mode-map
     :desc "Close lsp-ui imenu" "<f8>" #'lsp-ui-imenu--kill
     :desc "Close lsp-ui imenu" "C-c t i" #'lsp-ui-imenu--kill))

  (map! :leader
        :prefix "c"
        :desc "Peek doc" "K" #'lsp-ui-doc-glance))
