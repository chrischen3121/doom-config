;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; :completion
;; company
;; Hints:
;; C-; +company/complete
(when (modulep! :completion company)
  (after! company
    (when (modulep! :editor snippets)
      (map! :after yasnippet
            :map yas-minor-mode-map
            :desc "Code snippets"
            "M-/" #'company-yasnippet
            "S-<tab>" nil))
    (map!
     (:map prog-mode-map
           "M-<RET>" #'+default--newline-indent-and-continue-comments-a)
     (:map company-active-map
           "M-/" #'company-abort))

    (set-company-backend!
      '(prog-mode yaml-mode conf-mode)
      'company-capf
      '(:seperate company-yasnippet company-files))
    (set-company-backend!
      '(text-mode org-mode)
      'company-capf
      '(:separate company-dabbrev company-files company-ispell)
      'company-yasnippet)
    (setq-hook! 'org-mode-hook
      company-minimum-prefix-length 3
      ))


  (when (modulep! :completion company +childframe)
    (after! company-box
      (setq-hook! 'company-box-mode-hook company-box-doc-delay 2))))

;; :editor
;; fold (outline hide/show)
;; C-c C-f - fold commands prefix
(when (modulep! :editor fold)
  (after! outline
    (add-hook! 'outline-minor-mode-hook
      (defun set-outline-prefix-map ()
        (setq! outline-minor-mode-prefix (kbd "C-c 2 l"))
        (which-key-add-keymap-based-replacements
          outline-minor-mode-map "C-c 2 l" "outline"))))

  (map! :map (org-mode-map prog-mode-map)
        :leader
        :prefix ("2" . "fold")
        :desc "Toggle fold one" "f" #'+fold/toggle
        :desc "Open all" "O" #'+fold/open-all
        :desc "Close all" "C" #'+fold/close-all
        :desc "Open one" "o" #'+fold/open
        :desc "Close one" "c" #'+fold/close
        :desc "Delete folded one" "d" #'vimish-fold-delete
        :desc "Delete folded all" "D" #'vimish-fold-delete-all))


;; :tools
;; eval
(when (modulep! :tools eval)
  (map! :map (prog-mode-map emacs-lisp-mode-map)
        :leader
        :prefix ("; e" . "eval")
        :desc "Eval line" "l" #'+eval/line-or-region
        :desc "Eval buffer" "b" #'+eval/buffer-or-region
        :desc "Eval defun" "d" #'eval-defun
        :desc "Region and replace" "r" #'+eval/region-and-replace
        :desc "Region to REPL" "s" #'+eval/send-region-to-repl
        :desc "Open REPL same window" "c" #'+eval/open-repl-same-window
        :desc "Open REPL other window" "w" #'+eval/open-repl-other-window))

;; :tools
;; debugger
(when (modulep! :tools debugger)
  (map!
   :desc "dap debug" "C-c g d" #'dap-debug
   :map dap-mode-map
   :leader
   :prefix ("d" . "dap-cmds")
   :desc "start" "d" #'dap-debug
   :desc "continue" "c" #'dap-continue
   :desc "next" "n" #'dap-next
   :desc "step-in" "i" #'dap-step-in
   :desc "step-out" "o" #'dap-step-out
   :desc "restart" "r" #'dap-debug-restart
   :desc "quit" "q" #'+debugger/quit
   :prefix ("d b" . "breakpoints")
   :desc "toggle bp" "b" #'dap-breakpoint-toggle
   :desc "delete all bp" "d" #'dap-breakpoint-delete-all
   :desc "add condition" "c" #'dap-breakpoint-condition
   :desc "hit condition" "h" #'dap-breakpoint-hit-condition
   :desc "log message" "l" #'dap-breakpoint-log-message
   :prefix ("d e" . "eval")
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
     :desc "Open lsp-ui imenu" "C-c o i" #'lsp-ui-imenu
     :map lsp-ui-imenu-mode-map
     :desc "Close lsp-ui imenu" "<f8>" #'lsp-ui-imenu--kill
     :desc "Close lsp-ui imenu" "C-c o i" #'lsp-ui-imenu--kill))

  (map! :leader
        :prefix "c"
        :desc "Peek doc" "K" #'lsp-ui-doc-glance))


;; :ui
;; zen
(when (modulep! :ui zen)
  (after! writeroom-mode
    (setq! +zen-text-scale 0.8)
    (add-hook! 'writeroom-mode-enable-hook
      (when (modulep! :ui tabs)
        (centaur-tabs-local-mode +1))
      (display-line-numbers-mode -1))
    (add-hook! 'writeroom-mode-disable-hook
      (when (modulep! :ui tabs)
        (centaur-tabs-local-mode -1))
      (display-line-numbers-mode +1))))


;; :others
;; copilot
(use-package! copilot
  :defer t
  :init
  (add-hook! (prog-mode git-commit-setup conf-mode yaml-mode)
             :append #'copilot-mode)
  (setq-hook! copilot-mode copilot--indent-warning-printed-p t)
  :config
  (map! :map copilot-completion-map
        "<backtab>" #'copilot-accept-completion
        "M-o" #'copilot-panel-complete
        "M-l" #'copilot-accept-completion-by-line
        "M-j" #'copilot-accept-completion
        "M-n" #'copilot-next-completion
        "M-p" #'copilot-previous-completion))
