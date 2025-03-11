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

;; :completion
;; corfu
(when (modulep! :completion corfu)
  )



(defun merge-sort (a b)
  "Merge two sorted lists A and B."
  (if (or (null a) (null b))
      b
    (if (< (car a) (car b))
        (cons (car a) (merge-sort (cdr a) b))
      (cons (car b) (merge-sort a (cdr b))))))


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
  (map! :map lsp-mode-map
        "s-l" nil)
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
;; upload
;; TODO: add descriptions for ssh-deploy keybindings
(when (modulep! :tools upload)
  (map! :after ssh-deploy
        :desc "<ssh-upload>" "C-c r u" #'ssh-deploy-prefix-map))

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
        "C-M-w" #'copilot-accept-completion-by-word
        "C-M-l" #'copilot-accept-completion-by-line
        "C-M-n" #'copilot-next-completion
        "C-M-p" #'copilot-previous-completion)
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
