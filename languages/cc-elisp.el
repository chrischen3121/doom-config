;;; languages/cc-elisp.el -*- lexical-binding: t; -*-

(after! emacs-lisp
  (setq-hook! 'emacs-lisp-mode-hook tab-width 2)
  (map!
   :map emacs-lisp-mode-map
   "C-c 1 r" #'check-parens
   ))

(map! :prefix ("C-c P" . "Emacs profiling")
      :desc "Start profiling" "p" #'profiler-start
      :desc "Stop profiling" "s" #'profiler-stop
      :desc "Report profiling" "r" #'profiler-report)
