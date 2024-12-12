;; no-byte-compile: t
;;; cc-langs/python/config.el -*- lexical-binding: t; -*-

(when (modulep! :lang python +poetry)
  (map! :map python-mode-map
        :desc "poetry" "C-c m p" #'poetry))

(when (modulep! :lang python +pyright)
  (map! :map lsp-mode-map
        :desc "LSP Organize imports" "C-c c o"
        #'lsp-pyright-organize-imports))


(when (modulep! :tools debugger)
  (after! dap-mode
    (setq dap-python-debugger 'debugpy)))

(setq-hook! python-mode python-indent-offset 4)
