;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-

(map! :after vertico
      :map vertico-map
      "C-l" #'vertico-directory-delete-char)
