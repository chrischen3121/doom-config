;; no-byte-compile: t
;;; cc-langs/python/config.el -*- lexical-binding: t; -*-

(defvar cc/python-indent-offset 4
  "The number of spaces to indent inside python blocks.")

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

(setq-hook! python-mode python-indent-offset
            cc/python-indent-offset)

(use-package! sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  :config
  (setq! sphinx-doc-include-types t
         sphinx-doc-python-indent cc/python-indent-offset)
  (map! :map sphinx-doc-mode-map
        :desc "Insert docstring" "C-c c d"
        #'sphinx-doc))
