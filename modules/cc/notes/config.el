;; -*- no-byte-compile: t; -*-
;;; cc/notes/config.el -*- lexical-binding: t; -*-

(defcustom cc/deft-notes-dir "~/org/notes/"
  "Deft notes directory."
  :type 'string
  :group 'cc-notes)


;; :ui
;; deft
(when (modulep! :ui deft)
  (map! :desc "Search notes" "C-c s n" #'deft)
  (after! deft
    (setq! deft-directory cc/deft-notes-dir
           deft-default-extension "org"
           deft-use-filename-as-title t
           deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")))
