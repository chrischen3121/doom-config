;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/langs/python.el

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
