;; -*- no-byte-compile: t; -*-
;;; cc/dev/config.el -*- lexical-binding: t; -*-

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (setq! company-tooltip-limit 12)
    (map! :prefix ("C-c l c" . "<company>")
          "f" #'company-files
          "d" #'company-dabbrev
          "D" #'company-dabbrev-code
          "y" #'company-yasnippet
          "c" #'company-complete
          "C" #'company-capf
          "s" #'company-ispell
          "g" #'company-gtags
          "e" #'company-etags
          "o" #'company-other-backend)
    (map! :map company-active-map
          "M-/" #'company-other-backend
          "S-<tab>" nil)
    (advice-add '+company-init-backends-h :around #'cc/overwrite-company-backends)
    (setq-hook! 'org-mode-hook company-minimum-prefix-length 3)))

(when (modulep! :completion company +childframe)
  (after! company-box
    (setq-hook! 'company-box-mode-hook company-box-doc-delay 2)))

;; :editor
;; fold
(when (modulep! :editor fold)
  (after! outline
    (setq-hook! 'outline-minor-mode-hook
      outline-minor-mode-prefix (kbd "C-c 2 l"))
    (which-key-add-keymap-based-replacements
      outline-minor-mode-map "C-c 2 l" "outline")
    (undefine-key! outline-minor-mode-map "C-c @"))

;;;###package vimish-fold
  (map! :map prog-mode-map
        :prefix ("C-c 2" . "fold")
        :desc "Fold/Unfold" "2" #'+fold/toggle
        :desc "Fold all" "f" #'+fold/close-all
        :desc "Unfold all" "u" #'+fold/open-all
        :desc "Delete folded" "d" #'vimish-fold-delete))


;; :editor
;; yasnippet
(when (modulep! :editor snippets)
  (map! :after yasnippet
        :map yas-minor-mode-map
        "C-c &" nil
        (:prefix "C-c i"
         :desc "Snippet" "s" #'yas-insert-snippet)
        (:prefix "C-c y"
         :desc "Reload snippets" "r" #'yas-reload-all
         :desc "Insert snippet" "i" #'yas-insert-snippet)))

;; :tools
;; editorconfig
;; Indent for languages, See: `editorconfig-indentation-alist'

;; :tools
;; ein (jupyter notebook)
(when (modulep! :tools ein)
  (after! ein
    ;; for jupyter-lab, otherwise use "notebook"
    (setq! ein:jupyter-server-use-subcommand "server")))

;; :tools
;; rgb
(when (modulep! :tools rgb)
  (add-hook! '(emacs-lisp-mode-hook html-mode-hook
               css-mode-hook scss-mode-hook)
             #'rainbow-mode)
  (after! rainbow-mode
    (add-hook! 'rainbow-mode-hook
      (hl-line-mode (if rainbow-mode -1 +1)))))

;; :tools
;; lsp +peek
(when (modulep! :tools lsp +peek)
  ;; TODO: lsp default keymap is disorganized, try some and remove this keymap
  (setq! lsp-keymap-prefix (kbd "C-c c l"))
  (after! lsp-mode
    (add-hook! 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (after! lsp-ui
    (setq! lsp-ui-sideline-show-diagnostics t
           lsp-ui-sideline-show-code-actions t
           lsp-ui-sideline-show-symbol t
           lsp-ui-sideline-delay 0.5
           lsp-ui-imenu-buffer-position 'left
           lsp-ui-imenu-auto-refresh t
           lsp-ui-imenu-refresh-delay 2)
    (map!
     :map lsp-ui-mode-map
     [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
     [remap xref-find-references]  #'lsp-ui-peek-find-references
     ;; :desc "Open lsp-ui imenu" "<f2>" #'lsp-ui-imenu
     ;; :desc "Close lsp-ui imenu" "<f2>" #'lsp-ui-imenu--kill
     )))

(when (modulep! :ui treemacs +lsp)
  (after! treemacs
    (setq! lsp-treemacs-sync-mode 1)))

;; :tools
;; debugger
(when (modulep! :tools debugger)
  (map!
   :map dap-mode-map
   (:prefix ("C-c g" . "<debugger>")
    :desc "start" "d" #'+debugger/start
    :desc "continue" "c" #'dap-continue
    :desc "next" "n" #'dap-next
    :desc "step-in" "i" #'dap-step-in
    :desc "step-out" "o" #'dap-step-out
    :desc "restart" "r" #'dap-debug-restart
    :desc "quit" "q" #'+debugger/quit)
   (:prefix ("C-c g b" . "breakpoints")
    :desc "Toggle bp" "b" #'dap-breakpoint-toggle
    :desc "Add bp" "a" #'dap-breakpoint-add
    :desc "Delete bp" "d" #'dap-breakpoint-delete
    :desc "Delete all bp" "D" #'dap-breakpoint-delete-all
    :desc "Add condition" "c" #'dap-breakpoint-condition
    :desc "Hit condition" "h" #'dap-breakpoint-hit-condition
    :desc "Log message" "l" #'dap-breakpoint-log-message)
   (:prefix "C-c l r"
    :desc "Dap eval" "e" #'dap-eval
    :desc "Dap eval region" "r" #'dap-eval-region
    :desc "Dap eval at point" "t" #'dap-eval-thing-at-point)))


;; :tools
;; upload
;; TODO: add descriptions for ssh-deploy keybindings
(when (modulep! :tools upload)
  (map! :after ssh-deploy
        :desc "<ssh-upload>" "C-c r u" #'ssh-deploy-prefix-map))

;; :ui
;; indent-guide
(when (modulep! :ui indent-guides)
;;;###package highlight-indent-guides
  (remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
    #'highlight-indent-guides-mode))

;; :tools
;; eval
;; check `quickrun--language-alist' for languages
;; to add new or overwrite, See:
;; https://github.com/emacsorphanage/quickrun?tab=readme-ov-file#user-defined-command
;; TODO check with python
;; (when (modulep! :tools eval)
;;   (map! :map (prog-mode-map emacs-lisp-mode-map)
;;         :prefix ("C-c l e" . "eval")
;;         :desc "Eval line" "l" #'+eval/line-or-region
;;         :desc "Eval buffer" "b" #'+eval/buffer-or-region
;;         :desc "Region to REPL" "s" #'+eval/send-region-to-repl
;;         :desc "Open REPL same window" "r" #'+eval/open-repl-same-window
;;         :desc "Open REPL other window" "R" #'+eval/open-repl-other-window))

;; [Packages]
;; Rainbow mode: highlight color string
;; (use-package! rainbow-mode
;;   :hook ((emacs-lisp-mode html-mode css-mode) . rainbow-mode))

;; Codeium: NOTE wait for the async company-backend
;; cape-capf-super solution is still experimental


;; Github Copilot
(use-package! copilot
  :hook ((prog-mode git-commit-setup conf-mode yaml-mode) . copilot-mode)
  :config
  (setq! copilot-indent-offset-warning-disable t)
  (map! :map copilot-completion-map
        "<backtab>" #'copilot-accept-completion
        "M-w" #'copilot-accept-completion-by-word
        "M-l" #'copilot-accept-completion-by-line
        "M-n" #'copilot-next-completion
        "M-p" #'copilot-previous-completion)
;;;###package whitespace
  ;; For Github Copilot compatibility
  ;; Cursor Jump to End of Line When Typing
  ;; If you are using whitespace-mode, make sure to remove newline-mark from whitespace-style.
  (setq! whitespace-style (delq 'newline-mark whitespace-style)))
