;; no-byte-compile: t
;;; cc-langs/cpp/config.el -*- lexical-binding: t; -*-

;;; References:
;; https://github.com/doomemacs/doomemacs/tree/master/modules/lang/cc
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/

(when (and (modulep! :tools lsp) (modulep! :lang cc))
  (after! lsp-clangd
    ;; [project] use compile_commands.json
    ;; [user] use ~/.config/clangd/config.yaml
    ;; TODO: don't why this doesn't work.
    (setq! lsp-clients-clangd-args
           '("-std=c++20"
             "-j=3"
             "--background-index"
             "--clang-tidy"
             "--completion-style=detailed"
             "--header-insertion=never"
             "--header-insertion-decorators=0"))
    (set-lsp-priority! 'clangd 2)))
