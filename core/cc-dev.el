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
