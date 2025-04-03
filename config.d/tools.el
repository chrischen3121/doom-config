;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/tools.el
(when (modulep! :tools debugger)
  (remove-hook! 'dap-ui-mode-hook #'dap-ui-controls-mode)
  (remove-hook! 'dap-mode-hook #'dap-tooltip-mode)
  (setq! dap-auto-configure-features '(locals breakpoints)))

(when (modulep! :tools lsp)
  (add-hook! 'lsp-mode-hook
             #'lsp-enable-which-key-integration
             ;; (defun cc/lsp-ensure-copilot-server ()
             ;;   (lsp-ensure-server 'copilot-ls))
             )

  (setq! lsp-idle-delay 0.8
         lsp-copilot-enabled nil
         lsp-headerline-breadcrumb-enable t
         lsp-signature-render-documentation nil

         lsp-ui-sideline-show-diagnostics t
         lsp-ui-sideline-show-code-actions t
         lsp-ui-sideline-show-symbol nil
         lsp-ui-sideline-delay 1

         lsp-inline-completion-idle-delay 0.5

         lsp-ui-imenu-buffer-position 'left
         lsp-ui-imenu-auto-refresh t
         ;; lsp-ui-doc
         lsp-ui-doc-enable nil)
  (map! :map lsp-mode-map
        "s-l" nil
        :desc "Format buffer" "C-c c f" #'lsp-format-buffer
        :map lsp-ui-mode-map
        :desc "Open lsp-ui imenu" "C-c c i" #'lsp-ui-imenu)
  (map! :after lsp-inline-completion
        :map lsp-inline-completion-active-map
        "<backtab>" #'lsp-inline-completion-accept
        "M-<return>" #'lsp-inline-completion-accept
        "C-n" nil
        "C-p" nil
        "M-n" #'lsp-inline-completion-next
        "M-p" #'lsp-inline-completion-prev)
  )

(when (modulep! :tools pdf)
  (map! (:map pdf-view-mode-map
         :prefix ("C-c t p" . "<pdf-toggles>")
         :desc "Toggle slice mode" "s"
         #'pdf-view-auto-slice-minor-mode
         :desc "Toggle themed mode" "t"
         #'pdf-view-themed-minor-mode))
  (setq-hook! 'pdf-view-mode-hook
    pdf-view-themed-minor-mode 1))
