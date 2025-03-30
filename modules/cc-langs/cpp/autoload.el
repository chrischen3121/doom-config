;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/cpp/autoload.el
(defvar cc/cpp-quick-compile-command "g++ -std=c++20 -Wall -g -o"
  "The command to compile c++ file")

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
  (when (string-match "finished" string)
    (kill-buffer buffer)))

;;;###autoload
(defun cc/cpp-quick-run ()
  "Run compiled c++ executable"
  (interactive)
  (let ((file (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (shell-command (format "./%s" file))))

;;;###autoload
(defun cc/focus-on-cmake-help ()
  "Focus on cmake-help buffer"
  (let ((help-buffer (get-buffer "*CMake Help*")))
    (when help-buffer
      (select-window (get-buffer-window help-buffer)))))
