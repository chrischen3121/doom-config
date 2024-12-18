;; -*- no-byte-compile: t; -*-
;;; cc/dev/config.el -*- lexical-binding: t; -*-

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (setq! company-tooltip-limit 12)
    (map! :prefix ("C-c m c" . "<company>")
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

(defun merge-sort (a b)
  "Merge two sorted lists A and B."
  (if (or (null a) (null b))
      b
    (if (< (car a) (car b))
        (cons (car a) (merge-sort (cdr a) b))
      (cons (car b) (merge-sort a (cdr b))))))

;; :editor
;; fold
(when (modulep! :editor fold)
  (after! outline
    (setq-hook! 'outline-minor-mode-hook
      outline-minor-mode-prefix (kbd "C-c 2 l"))
    (which-key-add-keymap-based-replacements
      outline-minor-mode-map "C-c 2 l" "<outline>")
    (undefine-key! outline-minor-mode-map "C-c @"))

;;;###package vimish-fold
  (map! :map (prog-mode-map yaml-mode-map)
        :prefix ("C-c 2" . "<fold>")
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
;; lsp +peek
(when (modulep! :tools lsp +peek)
  ;; TODO: lsp default keymap is disorganized, try some and remove this keymap
  (setq! lsp-keymap-prefix (kbd "C-c l"))
  (after! lsp-mode
    (add-hook! 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (after! lsp-ui
    (setq! lsp-ui-sideline-show-diagnostics t
           lsp-ui-sideline-show-code-actions t
           lsp-ui-sideline-show-symbol t
           lsp-ui-sideline-delay 1
           lsp-ui-imenu-buffer-position 'left
           lsp-ui-imenu-auto-refresh t
           ;; lsp-ui-imenu-refresh-delay 2
           )
    (map!
     :map lsp-ui-mode-map
     [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
     [remap xref-find-references]  #'lsp-ui-peek-find-references
     ;; :desc "Open lsp-ui imenu" "<f2>" #'lsp-ui-imenu
     ;; :desc "Close lsp-ui imenu" "<f2>" #'lsp-ui-imenu--kill
     )))

(when (modulep! :ui treemacs +lsp)
  (after! treemacs
    (setq! lsp-treemacs-sync-mode 1)
    (map! :prefix-map ("C-c l t" . "<lsp-treemacs>")
          :desc "Browse project" "b" #'+treemacs/toggle
          :desc "Errors list" "e" #'lsp-treemacs-errors-list
          :desc "LSP UI Errors List" "E" #'lsp-ui-flycheck-list
          :desc "Incoming call hierarchy" "i" #'lsp-treemacs-call-hierarchy
          :desc "Outgoing call hierarchy" "o" (cmd!! #'lsp-treemacs-call-hierarchy t)
          :desc "Type hierarchy" "t" #'lsp-treemacs-type-hierarchy
          :desc "Implementation" "I" #'lsp-treemacs-implementations
          :desc "References tree" "r" (cmd!! #'lsp-treemacs-references t)
          :desc "Symbols" "s" #'lsp-treemacs-symbols)))

;; :tools
;; debugger
(when (modulep! :tools debugger)
  (map!
   :prefix-map ("C-c g" . "<debugger>")
   :desc "run" "s" #'+debugger/start
   :desc "quit" "q" #'+debugger/quit)
  (remove-hook! 'dap-ui-mode-hook #'dap-ui-controls-mode)
  (remove-hook! 'dap-mode-hook #'dap-tooltip-mode)
  (setq! dap-auto-configure-features
         '(sessions locals breakpoints expressions))
  (after! dap-mode
    (map!
     :map dap-mode-map
     :prefix "C-c g"
     :desc "dap-debug" "g" #'dap-debug
     :desc "dap-hydra" "h" #'dap-hydra))
  )

;; :tools
;; upload
;; TODO: add descriptions for ssh-deploy keybindings
(when (modulep! :tools upload)
  (map! :after ssh-deploy
        :desc "<ssh-upload>" "C-c r u" #'ssh-deploy-prefix-map))

;; :ui
;; indent-guide
(when (modulep! :ui indent-guides)
  (defun cc/inhibit-for-specified-modes ()
    "Inhibit indent-guides mode if the current mode is in the specified list."
    (member major-mode '(org-mode)))
;;;###package indent-bars
  (add-hook! '+indent-guides-inhibit-functions #'cc/inhibit-for-specified-modes))

;; :tools
;; eval
;; check `quickrun--language-alist' for languages
;; to add new or overwrite, See:
;; https://github.com/emacsorphanage/quickrun?tab=readme-ov-file#user-defined-command
;; TODO check with python
;; (when (modulep! :tools eval)
;;   (map! :map (prog-mode-map emacs-lisp-mode-map)
;;         :prefix ("C-c m e" . "eval")
;;         :desc "Eval line" "l" #'+eval/line-or-region
;;         :desc "Eval buffer" "b" #'+eval/buffer-or-region
;;         :desc "Region to REPL" "s" #'+eval/send-region-to-repl
;;         :desc "Open REPL same window" "r" #'+eval/open-repl-same-window
;;         :desc "Open REPL other window" "R" #'+eval/open-repl-other-window))

;; [Packages]
;; Rainbow mode: highlight color string
;; (use-package! rainbow-mode
;;   :hook ((emacs-lisp-mode html-mode css-mode) . rainbow-mode))

;; rainbow-mode
(use-package! rainbow-mode
  :hook ((emacs-lisp-mode html-mode css-mode scss-mode) . rainbow-mode)
  :config
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

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

;; TODO may try Codeium later on
;; codeium-completion-at-point should be the first in the completion-at-point-functions
;; which is imcompatible with lsp
;; enable company-preview-frontend if using codeium
;; (use-package! codeium
;;   :init
;;   (codeium-init)
;;   (add-hook! 'prog-mode-hook
;;     (defun cc/set-codeium-capf ()
;;       :local (add-to-list 'completion-at-point-functions
;;                           #'codeium-completion-at-point))))

;; Langs
(add-hook! 'sh-mode-hook
  (defun cc/set-default-shell ()
    (sh-set-shell "bash")))

(when (modulep! :tools tree-sitter)
  (setq! +tree-sitter-hl-enabled-modes
         '(python-mode
           ;; FIXME not confirmed below
           cc-mode
           json-mode
           yaml-mode
           sh-mode
           css-mode
           html-mode
           dockerfile-mode
           editorconfig-mode
           makefile-mode
           cmake-mode
           js2-mode
           typescript-mode
           )))

;; built-in treesit
;; TODO no syntax hightlight feature in built-in treesit
;; (when (>= emacs-major-version 29)
;;
;;   (use-package! treesit
;;     :preface
;;     (defun cc/install-treesit-grammars ()
;;       (interactive)
;;       (dolist (grammar
;;                '((python "https://github.com/tree-sitter/tree-sitter-python")
;;                  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;                  (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;                  (cmake "https://github.com/uyha/tree-sitter-cmake")
;;                  (html "https://github.com/tree-sitter/tree-sitter-html")
;;                  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;                  (json "https://github.com/tree-sitter/tree-sitter-json")
;;                  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;         (add-to-list 'treesit-language-source-alist grammar)
;;         (unless (treesit-language-available-p (car grammar))
;;           (treesit-install-language-grammar (car grammar)))))
;;     :config
;;     (cc/install-treesit-grammars)
;;     (dolist (mapping
;;              '((python-mode . python-ts-mode)
;;                (sh-mode . bash-ts-mode)
;;                (cmake-mode . cmake-ts-mode)
;;                (js-mode . js-ts-mode)
;;                (json-mode . json-ts-mode)
;;                (yaml-mode . yaml-ts-mode)))
;;       (add-to-list 'major-mode-remap-alist mapping))
;;     (setq! font-lock-support-mode #'tree-sitter-lock-mode)
;;     ;; TODO to modify all hooks...
;;     (setq! c++-ts-mode-hook c++-mode-hook)
;;     )
;;   (use-package combobulate
;;     :init
;;     (setq! combobulate-key-prefix "C-c ;") ; FIXME waiting for PR
;;     (add-hook! 'prog-mode-hook #'combobulate-mode))
;;   )
