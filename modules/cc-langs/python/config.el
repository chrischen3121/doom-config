;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/python/config.el
(when (modulep! :lang python)
  (defun cc/python-dis-region-or-buffer ()
    "Disassemble the Python code in the current region or buffer and show it in a temp buffer."
    (interactive)
    (let* ((start (if (region-active-p) (region-beginning) (point-min)))
           (end (if (region-active-p) (region-end) (point-max)))
           (code (buffer-substring-no-properties start end))
           (temp-file (make-temp-file "python-dis-" nil ".py"))
           (buffer (get-buffer-create "*Python Disassembly*")))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert code))
            (with-current-buffer buffer
              (erase-buffer)
              (call-process "python3" nil buffer nil "-m" "dis" temp-file)
              (goto-char (point-min))
              (read-only-mode 1)
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "q") 
                  (lambda () 
                    (interactive)
                    (quit-window t)))
                (use-local-map map)))
            (switch-to-buffer-other-window buffer))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))

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
        "C-c <TAB> r" nil
        :desc "Disassemble region/buffer" "C-c c d" #'cc/python-dis-region-or-buffer)

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
