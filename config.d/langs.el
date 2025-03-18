;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/langs.el

;; elisp
(map! :map emacs-lisp-mode-map
      :desc "Byte compile file" "C-c c c" #'byte-compile-file
      :desc "Check parens" "C-c 1 [" #'check-parens)
