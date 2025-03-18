;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-langs/cpp/autoload.el


;;;###autoload
(defun cc/cpp-compile ()
  "compile c++ file and focus on compilation window"
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "g++ -std=c++20 -Wall -g -o %s %s" (file-name-sans-extension file) file))))
  (compile compile-command))


;;;###autoload
(defun cc/cpp-run ()
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

;;;###autoload
(defun cc/cpp-quick-run ()
  "Compile and run c++ file"
  (interactive)
  (cc/cpp-compile)
  (cc/cpp-run))
