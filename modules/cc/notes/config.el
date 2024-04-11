;; -*- no-byte-compile: t; -*-
;;; cc/notes/config.el -*- lexical-binding: t; -*-

(defcustom cc/deft-notes-dir "~/org/notes/"
  "Deft notes directory."
  :type 'string
  :group 'cc-notes)

;; org configuration
(remove-hook! 'org-load-hook #'+org-init-smartparens-h)
(after! org
  (remove-hook 'org-mode-hook #'org-indent-mode)
  (setq! org-startup-indented nil
         org-ellipsis " ▼"

         ;; pretty latex preview
         org-pretty-entities t
         org-pretty-entities-include-sub-superscripts nil
         org-highlight-latex-and-related '(native latex entities)

         ;; inline image
         org-startup-with-inline-images t)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  (require 'org-indent) ; fix: Invalid face reference: org-indent

  )


;; :ui
;; deft
(when (modulep! :ui deft)
  (map! :desc "Search notes" "C-c s n" #'deft)
  (after! deft
    (setq! deft-directory cc/deft-notes-dir
           deft-default-extension "org"
           deft-use-filename-as-title t
           deft-strip-summary-regex
           ":PROPERTIES:\n\\(.+\n\\)+:END:\n")))
