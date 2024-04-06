;;; cc/better-defaults/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cc/disable-continue-comments ()
  (advice-remove 'newline-and-indent
                 '+default--newline-indent-and-continue-comments-a))
