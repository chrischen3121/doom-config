;;; languages/cc-elisp.el -*- lexical-binding: t; -*-
(map! :map emacs-lisp-mode-map
      :desc "Check parens"
      "C-c 1 P" #'check-parens)

(map! :prefix ("C-c P" . "Emacs profiling")
      :desc "Start profiling" "p" #'profiler-start
      :desc "Stop profiling" "s" #'profiler-stop
      :desc "Report profiling" "r" #'profiler-report)
