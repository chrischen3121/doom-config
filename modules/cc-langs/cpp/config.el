;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/cpp/config.el

;;; References:
;; https://github.com/doomemacs/doomemacs/tree/master/modules/lang/cc
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/

(when (modulep! :lang cc)
  (setq-hook! 'c++-mode-hook
    tab-width 2)

  (add-to-list 'auto-mode-alist '("conanfile\\.txt\\'" . conf-windows-mode))
  (map! :after cc-mode
        :map c++-mode-map
        :prefix "C-c l"
        :desc "Compile" "c" #'cc/cpp-compile
        :desc "Run" "r" #'cc/cpp-run
        :desc "Quick run" "q" #'cc/cpp-quick-run
        :desc "Woman" "w" #'woman ; or C-h W
        :desc "Debug(gdb)" "g" #'gdb
        :desc "Debug(dap-debug)" "d" #'dap-debug))

(when (modulep! :ui indent-guides)
  (add-hook! 'c-mode-common-hook
    (defun configure-indent-guides ()
      (setq-local indent-bars-treesit-support t
                  indent-bars-treesit-wrap
                  '((c argument_list parameter_list
                     init_declarator parenthesized_expression))))))

(when (and (modulep! :tools lsp) (modulep! :lang cc))
  (after! lsp-clangd
    ;; [project] use compile_commands.json
    ;; [user] use ~/.config/clangd/config.yaml
    (setq! lsp-clients-clangd-args
           '("-j=3"
             "--background-index"
             "--clang-tidy"
             "--limit-results=50"
             "--completion-style=detailed"
             "--header-insertion=never"
             "--header-insertion-decorators=0"))
    (set-lsp-priority! 'clangd 2)))

(setq-hook! 'cmake-mode-hook cmake-tab-width 4)
