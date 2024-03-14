;;; languages/cc-cpp.el -*- lexical-binding: t; -*-

;; For projects
;; use `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .` to generate compile_commands.json


(when (modulep! :lang cc)
  (defun cc/lsp-cpp-compile ()
    "compile c++ file and focus on compilation window"
    (interactive)
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (format "g++ -std=c++20 -Wall -g -o %s %s" (file-name-sans-extension file) file))))
    (compile compile-command))

  (defun cc/lsp-cpp-run ()
    "Open or send to vterm and run"
    (interactive)
    (let ((file (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
      (if (get-buffer "*vterm*")
          (progn
            ;; Switch to the *vterm* buffer in another window.
            (switch-to-buffer-other-window "*vterm*")
            (vterm-send-string (format "./%s\n" file)))
        (progn
          ;; If *vterm* buffer doesn't exist, open it in another window.
          (vterm-other-window)
          (vterm-send-string (format "./%s\n" file))))))
  (after! lsp-clangd
    (setq! lsp-clients-clangd-args
           '("-j=3"
             "--background-index"
             "--clang-tidy"
             "--completion-style=detailed"
             "--header-insertion=never"
             "--header-insertion-decorators=0"))
    (set-lsp-priority! 'clangd 2)))

(add-hook! 'c++-mode-hook
  (setq-local flycheck-clang-language-standard "c++20"
              flycheck-gcc-language-standard "c++20"
              tab-width 2))

(map! :map c++-mode-map
      :prefix "C-c ;"
      :desc "Compile" "c" #'cc/lsp-cpp-compile
      :desc "Man" "m" #'woman
      :desc "Run" "r" #'cc/lsp-cpp-run
      :prefix ("C-c ; d" . "Debug")
      :desc "gdb" "g" #'gdb
      :desc "dap-debug" "d" #'dap-debug)
