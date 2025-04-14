;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/cpp/autoload.el
(defvar cc/cpp-quick-compile-command "g++ -std=c++20 -Wall -g -o"
  "The command to compile c++ file")

;;;###autoload
(defun cc/cpp-set-default-sytle (&optional style)
  "Set the default style for C/C++ mode."
  (if style
      (c-set-style style)
    (c-set-style "gnu")))

;;;###autoload
(defun cc/cpp-set-default-capf ()
  "Set the completion-at-point-functions for C++ mode src blocks."
  (setq-local completion-at-point-functions
              '(yasnippet-capf
                cape-file
                t)))

;;;###autoload
(defun cc/cpp-set-lsp-capf ()
  "Set the completion-at-point-functions for C++ mode(with lsp-mode)."
  (when (derived-mode-p 'c++-mode)
    (setq-local completion-at-point-functions
                `(,(cape-capf-super #'lsp-completion-at-point #'yasnippet-capf)
                  cape-file
                  t))))

;;;###autoload
(defun cc/cpp-quick-compile ()
  "compile c++ file and focus on compilation window"
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s %s %s" cc/cpp-quick-compile-command
                   (file-name-sans-extension file) file))))
  (compile compile-command))

;;;###autoload
(defun cc/close-compilation-buffer-if-successful (buffer string)
  "Close the *compilation* BUFFER if it succeeded without errors."
  (when (and (string-prefix-p "*compilation*" (buffer-name buffer))
             (string-prefix-p "finished" string))
    (kill-buffer buffer)))

;;;###autoload
(defun cc/cpp-quick-run ()
  "Run compiled c++ executable"
  (interactive)
  (let ((file (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (shell-command (format "./%s" file))))

;;;###autoload
(defun cc/cpp-quick-debug ()
  "Run compiled c++ executable with gdb"
  (interactive)
  (let ((file (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (gdb (format "gdb -i mi %s" file))))

;;;###autoload
(defun cc/focus-on-cmake-help ()
  "Focus on cmake-help buffer"
  (let ((help-buffer (get-buffer "*CMake Help*")))
    (when help-buffer
      (select-window (get-buffer-window help-buffer)))))

;;;###autoload
(defun cc//find-cmake-build-dir ()
  "Find the build directory"
  (let ((build-dir (expand-file-name "build" default-directory)))
    (unless (file-directory-p build-dir)
      (make-directory build-dir))
    build-dir))

;;;###autoload
(defun cc/cmake-generate-build-files ()
  "Run 'cmake -B [build]' to generate build files"
  (interactive)
  (let ((build-dir (cc//find-cmake-build-dir)))
    (cmake-command-run (format "-B %s" build-dir))))

;; TODO: reduce duplication
;; implement a more general function, let the user input the options
;;;###autoload
(defun cc/cmake-gen-debug-build-files ()
  "Run 'cmake -DCMAKE_BUILD_TYPE=Debug -B [build]' to generate debug build files"
  (interactive)
  (let ((build-dir (cc//find-cmake-build-dir)))
    (cmake-command-run (format "-DCMAKE_BUILD_TYPE=Debug -B %s" build-dir))))

;;;###autoload
(defun cc/cmake-build ()
  "Run 'cmake --build [build]' to build project"
  (interactive)
  (let ((build-dir (cc//find-cmake-build-dir)))
    (cmake-command-run (format "--build %s" build-dir))))

;;;###autoload
(defun cc/cmake-compile-commands ()
  "Run 'cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1' to generate compile_commands.json"
  (interactive)
  (let ((build-dir (cc//find-cmake-build-dir)))
    (cmake-command-run (format "-DCMAKE_EXPORT_COMPILE_COMMANDS=on -B %s" build-dir))
    (let* ((compile-json-name "compile_commands.json")
           (compile-json-origin (expand-file-name compile-json-name build-dir)))
      (unless (file-exists-p compile-json-name)
        (make-symbolic-link compile-json-origin compile-json-name)))))

;;;###autoload
(defun cc/cmake-ctest ()
  "Run 'ctest --test-dir [build]' to test project"
  (interactive)
  (let ((build-dir (cc//find-cmake-build-dir)))
    (shell-command (format "ctest --test-dir %s" build-dir))
    (with-current-buffer "*Shell Command Output*"
      (view-mode t))))
