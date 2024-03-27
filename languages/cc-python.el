;;; languages/cc-python.el -*- lexical-binding: t; -*-


;; TODO try ein and move to python.el
;; (after! ein
;;   (setq! ein:jupyter-server-use-subcommand "server"))
(when (modulep! :tools debugger)
  (after! dap-mode
    (setq dap-python-debugger 'debugpy)))

(setq-hook! python-mode python-indent-offset 2)
