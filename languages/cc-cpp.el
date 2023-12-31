;;; languages/cc-cpp.el -*- lexical-binding: t; -*-


(when (modulep! :lang cc)
  (defun cc/lsp-cpp-compile ()
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
            (vterm-send-string (format "./%s\n" file))
            (vterm-send-return))
        (progn
          (vterm-other-window)
          (vterm-send-string (format "./%s\n" file))
          (vterm-send-return))))))

(map! :map c++-mode-map
      :prefix "C-c m"
      :desc "Compile" "c" #'cc/lsp-cpp-compile
      :desc "Man" "m" #'woman
      :desc "Run" "r" #'cc/lsp-cpp-run
      :desc "Debug" "d" #'gud-gdb)
