;; -*- no-byte-compile: t; -*-
;;; cc/notes/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cc/open-pdf-note-files ()
  "Open all PDF note files in `cc/org-pdf-notes-dir'."
  (interactive)
  (let ((default-directory cc/org-pdf-notes-dir))
    (call-interactively 'find-file)))
