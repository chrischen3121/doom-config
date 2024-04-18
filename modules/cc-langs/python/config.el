;; no-byte-compile: t
;;; cc-langs/python/config.el -*- lexical-binding: t; -*-

(when (modulep! :lang python +poetry)
  (map! :map python-mode-map
        :desc "poetry" "C-c l p" #'poetry))

(when (modulep! :tools debugger)
  (after! dap-mode
    (setq dap-python-debugger 'debugpy)))

(setq-hook! python-mode python-indent-offset 2)
