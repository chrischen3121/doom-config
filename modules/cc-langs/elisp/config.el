;; no-byte-compile: t
;;; cc-langs/elisp/config.el -*- lexical-binding: t; -*-

(map! (:map emacs-lisp-mode-map
       :desc "Lookup demos" "C-c k e" #'elisp-demos-find-demo
       :desc "Byte compile" "C-c c c" #'byte-compile-file
       :desc "Check parens" "C-c 1 [" #'check-parens))
