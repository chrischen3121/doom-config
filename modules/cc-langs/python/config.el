;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/python/config.el
(when (modulep! :lang python)
  (defvar cc/python-indent-offset 4
    "The number of spaces to indent inside python blocks.")

  (after! pyvenv
    (setq! pyvenv-activate ".venv")
    (add-hook! 'python-mode-hook #'pyvenv-tracking-mode))


  (setq-hook! 'python-mode-hook
    python-indent-offset cc/python-indent-offset)

  ;; set default python interpreter
  (setq! python-shell-interpreter "python3"
         doom-modeline-env-enable-python nil)

  (when (modulep! :ui indent-guides)
    (add-hook! 'python-mode-hook
      (defun configure-indent-guides ()
        (setq-local indent-bars-treesit-support t
                    indent-bars-treesit-wrap
                    '((python argument_list parameters
                       list list_comprehension
                       dictionary dictionary_comprehension
                       parenthesized_expression subscript)
                      (python list_comprehension))
                    indent-bars-treesit-ignore-blank-lines-types '("module")))))

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

(when (modulep! :lang python +lsp)
  (map! :map lsp-mode-map
        :desc "Organize imports" "C-c c o"
        #'lsp-organize-imports))

(when (modulep! :tools debugger)
  (add-hook! 'dap-stopped-hook
    (defun cc/dap-hydra (arg)
      (call-interactively #'dap-hydra)))
  (after! dap-python
    ;; HACK pyvenv-tracking-mode will help find the executable
    (defun dap-python--pyenv-executable-find (command)
      (executable-find command))
    (setq! dap-python-debugger 'debugpy)))
