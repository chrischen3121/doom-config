;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/langs.el

;; elisp
(setq-hook! 'emacs-lisp-mode-hook
  completion-at-point-functions
  `(cape-file
    ,(cape-capf-super #'elisp-completion-at-point #'yasnippet-capf)
    t))

(map! :map emacs-lisp-mode-map
      :desc "Byte compile file" "C-c c c" #'byte-compile-file
      :desc "Disassemble" "C-c c d" #'disassemble
      :desc "Check parens" "C-c 1 [" #'check-parens)

;; sh
(add-hook! 'sh-mode-hook
  (defun cc/set-default-shell ()
    (sh-set-shell "bash")))
