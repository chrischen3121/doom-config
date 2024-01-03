;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (map! :after yasnippet
          :map
          (org-mode-map
           prog-mode-map
           yaml-mode-map
           conf-mode-map)
          "M-/" #'company-yasnippet)
    (map! :map prog-mode-map
          "M-<RET>" #'+default--newline-indent-and-continue-comments-a
          :map company-active-map
          "M-/" #'company-abort)
    (set-company-backend!
      '(prog-mode yaml-mode conf-mode)
      'company-capf
      '(:seperate company-yasnippet company-files))
    (set-company-backend! '(text-mode org-mode)
      'company-capf
      '(:separate company-dabbrev company-files company-ispell)
      'company-yasnippet)
    )

  (after! company-box
    (setq-hook! 'company-box-mode-hook company-box-doc-delay 2)))

;; :editor
;; fold (outline hide/show)
;; C-c C-f - fold commands prefix
(when (modulep! :editor fold)
  (add-hook! 'outline-minor-mode-hook
    (setq! outline-minor-mode-prefix (kbd "C-c 2 @"))
    (which-key-add-key-based-replacements "C-c 2 @" "outline"))
  (map! :map (org-mode-map prog-mode-map)
        :prefix ("C-c 2" . "fold")
        "l" #'+fold/toggle
        "O" #'+fold/open-all
        "C" #'+fold/close-all
        "o" #'+fold/open
        "c" #'+fold/close
        "d" #'vimish-fold-delete
        "D" #'vimish-fold-delete-all))


;; :tools
;; eval
(when (modulep! :tools eval)
  (map! :map (prog-mode-map emacs-lisp-mode-map)
        :prefix ("C-c m e" . "eval")
        :desc "Eval line" "l" #'+eval/line-or-region
        :desc "Eval buffer" "b" #'+eval/buffer-or-region
        :desc "Region and replace" "r" #'+eval/region-and-replace
        :desc "Region to REPL" "s" #'+eval/send-region-to-repl
        :desc "Open REPL same window" "c" #'+eval/open-repl-same-window
        :desc "Open REPL other window" "w" #'+eval/open-repl-other-window))


;; :tools
;; lsp +peek
(when (modulep! :tools lsp +peek)
  (after! lsp-ui
    (setq! lsp-ui-sideline-show-diagnostics t
           lsp-ui-sideline-show-code-actions t
           lsp-ui-sideline-show-symbol t
           lsp-ui-sideline-delay 0.5
           lsp-ui-imenu-buffer-position 'left
           ;;lsp-ui-imenu-auto-refresh t
           lsp-ui-imenu-refresh-delay 2
           )
    (map!
     :map lsp-ui-mode-map
     [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
     [remap xref-find-references]  #'lsp-ui-peek-find-references
     :desc "Open lsp-ui imenu" "C-c 3" #'lsp-ui-imenu
     ))
  (map! :leader
        :prefix "c"
        :desc "Peek doc" "K" #'lsp-ui-doc-glance))


;; :ui
;; zen
(when (modulep! :ui zen)
  (after! writeroom-mode
    (setq! +zen-text-scale 0.8)
    (add-hook! 'writeroom-mode-enable-hook
      (centaur-tabs-local-mode +1)
      (display-line-numbers-mode -1)
      (add-hook! 'writeroom-mode-disable-hook
        (centaur-tabs-local-mode -1)
        (display-line-numbers-mode +1)))))



;; :others
;; copilot
(add-hook! (prog-mode git-commit-setup conf-mode yaml-mode) #'copilot-mode)
(map! :after copilot
      :map copilot-completion-map
      "<backtab>" #'copilot-accept-completion
      "M-j" #'copilot-accept-completion
      "M-n" #'copilot-next-completion
      "M-p" #'copilot-previous-completion
      "M-l" #'copilot-accept-completion-by-line
      "M-o" #'copilot-panel-complete)


;; TODO try ein and move to python.el
;; (after! ein
;;   (setq! ein:jupyter-server-use-subcommand "server"))
