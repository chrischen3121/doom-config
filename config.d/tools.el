;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/tools.el
(when (modulep! :tools debugger)
  (remove-hook! 'dap-ui-mode-hook #'dap-ui-controls-mode)
  (remove-hook! 'dap-mode-hook #'dap-tooltip-mode)
  (setq! dap-auto-configure-features
         '(sessions locals breakpoints expressions))
  (map!
   :map dap-mode-map
   :prefix "C-c d"
   :desc "dap-debug" "g" #'dap-debug
   :desc "dap-hydra" "h" #'dap-hydra)
  )


(when (modulep! :tools lsp)
  (add-hook! 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (setq! lsp-idle-delay 0.8
         lsp-copilot-enabled t
         lsp-headerline-breadcrumb-enable t
         lsp-signature-render-documentation nil

         lsp-ui-sideline-show-diagnostics t
         lsp-ui-sideline-show-code-actions t
         lsp-ui-sideline-show-symbol nil

         lsp-ui-sideline-delay 1
         lsp-ui-imenu-buffer-position 'left
         lsp-ui-imenu-auto-refresh t
         ;; lsp-ui-doc
         lsp-ui-doc-enable nil
         )
  (map! :map lsp-mode-map
        "s-l" nil
        :map lsp-ui-mode-map
        :desc "Open lsp-ui imenu" "C-c c i" #'lsp-ui-imenu)
  )
