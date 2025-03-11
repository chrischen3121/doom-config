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
