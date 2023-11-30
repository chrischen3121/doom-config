;;; languages/cc-elisp.el -*- lexical-binding: t; -*-

(after! emacs-lisp
  (setq-hook! 'emacs-lisp-mode-hook tab-width 2))

(map!
 :after emacs-lisp-mode
 :map emacs-lisp-mode-map
 "C-c ! r" #'check-parens
 )
