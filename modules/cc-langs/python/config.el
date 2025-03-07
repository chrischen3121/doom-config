;; no-byte-compile: t
;;; cc-langs/python/config.el -*- lexical-binding: t; -*-
(when (modulep! :lang python)
  (defvar cc/python-indent-offset 4
    "The number of spaces to indent inside python blocks.")

  (setq-hook! python-mode python-indent-offset
              cc/python-indent-offset)

  (setq! python-shell-interpreter "python3")

  (map! :map python-mode-map
        "C-c <TAB> a" nil ; python-add-import
        "C-c <TAB> s" nil
        "C-c <TAB> f" nil
        "C-c <TAB> r" nil)

  (when (modulep! :lang rst)
    (use-package! sphinx-doc
      :hook (python-mode . sphinx-doc-mode)
      :config
      (setq! sphinx-doc-include-types t
             sphinx-doc-python-indent cc/python-indent-offset)
      (map! :map sphinx-doc-mode-map
            :desc "Insert docstring" "C-c c d"
            #'sphinx-doc)))
  )

(when (modulep! :lang python +poetry)
  (setq! poetry-tracking-strategy 'projectile)
  (map! :map python-mode-map
        :desc "poetry" "C-c l p" #'poetry))

(when (modulep! :lang python +pyright)
  (map! :map lsp-mode-map
        :desc "LSP Organize imports" "C-c c o"
        #'lsp-pyright-organize-imports))

(when (modulep! :tools debugger)
  (add-hook! 'dap-stopped-hook
    (defun cc/dap-hydra (arg)
      (call-interactively #'dap-hydra)))
  (after! dap-python
    ;; HACK pyvenv-tracking-mode will help find the executable
    (defun dap-python--pyenv-executable-find (command)
      (executable-find command))
    (setq! dap-python-debugger 'debugpy)))
