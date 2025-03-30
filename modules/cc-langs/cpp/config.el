;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/cpp/config.el

;;; References:
;; https://github.com/doomemacs/doomemacs/tree/master/modules/lang/cc
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/


(when (modulep! :lang cc)
  (add-to-list 'auto-mode-alist '("conanfile\\.txt\\'" . conf-unix-mode))
  (setq-hook! 'c++-mode-hook tab-width 2)
  (add-hook! 'compilation-finish-functions #'cc/close-compilation-buffer-if-successful)
  (map! :after cc-mode
        :map (c++-mode-map cmake-mode-map)
        (:prefix "C-c r"
         :desc "C++ Quick run" "q" #'cc/cpp-quick-run
         )
        (:prefix "C-c c"
         :desc "Quick compile" "c" #'cc/cpp-quick-compile
         :desc "Disassemble" "d" #'disaster
         (:prefix
          ("b" . "<build>")
          (:when (modulep! :tools make)
            :desc "make command" "m" #'+make/run
            :desc "make last" "l" #'+make/run-last)
          (:when (modulep! :lang cc)
            :desc "cmake command" "c" #'cmake-command-run)
          )
         ))

  ;; cmake-mode
  (setq-hook! 'cmake-mode-hook cmake-tab-width 4)
  (map! :map cmake-mode-map
        :desc "CMake doc" "C-c k k" #'cmake-help)
  (advice-add #'cmake-help :after #'cc/focus-on-cmake-help)
  )

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

