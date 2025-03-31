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
(defun cc/find-topmost-cmake-build-dir ()
  "Find the topmost directory containing a CMakeLists.txt, create a build/ directory if it doesn't exist, and return the build directory path."
  (let ((dir (locate-dominating-file default-directory "CMakeLists.txt")))
    (if dir
        (let ((build-dir (expand-file-name "build" dir)))
          (unless (file-directory-p build-dir)
            (make-directory build-dir))
          build-dir)
      (error "No CMakeLists.txt found in any parent directory"))))

;;;###autoload
(defun cc/cmake-configure ()
  "Run 'cmake -B build' in the topmost directory containing a CMakeLists.txt."
  (interactive)
  (let ((build-dir (cc/find-topmost-cmake-build-dir)))
    (shell-command (format "cmake -B %s" build-dir))))

;;;###autoload
(defun cc/cmake-build ()
  "Run 'cmake --build build' in the topmost directory containing a CMakeLists.txt."
  (interactive)
  (let ((build-dir (cc/find-topmost-cmake-build-dir)))
    (shell-command (format "cmake --build %s" build-dir))))

;;;###autoload
(defun cc/focus-on-cmake-help ()
  "Focus on cmake-help buffer"
  (let ((help-buffer (get-buffer "*CMake Help*")))
    (when help-buffer
      (select-window (get-buffer-window help-buffer)))))
