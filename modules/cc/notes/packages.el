;; -*- no-byte-compile: t; -*-
;;; cc/notes/packages.el

(package! anki-editor)
(package! org-download)

(when (modulep! :lang org +roam2)
  (package! org-roam-ui))
