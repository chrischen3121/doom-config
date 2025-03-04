;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/completion.el
(when (modulep! :completion vertico)
  (map! :map vertico-map
        "C-l" #'vertico-directory-delete-word
        "C-M-p" #'vertico-previous-group
        "C-M-n" #'vertico-next-group
        "C-SPC" #'+vertico/embark-preview
        :desc "Export to buffer" "C-c C-e" #'embark-export))
