;;; org/cc-org.el -*- lexical-binding: t; -*-
;; TODO: org-format-latex-options
;;
;;; Hints:
;;
;; C-c C-o -- Open link at point
;; C-c C-l -- Insert a link
;; C-x n s, C-x n w -- Narrow/Unnarrow buffer
;; C-c / -- Search in an outline tree and fold others as much as possible
;; M-S-<RET> -- Add TODO outlines or add items with a checkbox
;; C-c - -- Cycle bullets (-, +, *, ...)
;; C-c <TAB> -- org-toggle-inline-images
;; C-c ; -- Toggle the "COMMENT" keyword
;; ====== Tags ==========
;; C-c C-q -- Set a tag
;; M-x org-agenda m/M -- Match tags (only TODO)
;; ====== Properties ========
;; C-c C-x p -- Set a property
;; C-c C-c d -- Remove a property
;; ====== Timestamps ========
;; C-c . -- Set a timestamp
;; S-LEFT/S-Right -- Change by one day
;; ====== Footnotes =========
;; C-c f prefix
;; C-c C-c -- jump between definition and reference
;; +strike-through+
;; =============================
;; Here is an small example
;;  : Some example from a text file.
;; =============================
;; #+INCLUDE: "~/.emacs" src emacs-lisp
;;

;; TODO may use +dragndrop
(use-package! org-download
  :after-call (org-mode-hook)
  :commands (org-download-screenshot
             org-download-clipboard)
  :init
  (map! :map org-mode-map
        :prefix ("C-c ; d" . "org-download")
        :desc "Download screenshot"
        "p" #'org-download-screenshot
        :desc "Delete downloaded image"
        "d" #'org-download-delete
        :desc "From clipboard"
        "y" #'org-download-clipboard
        :desc "Rename"
        "r" #'org-download-rename-at-point)
  :config
  (setq! org-download-image-dir "images/"
         org-download-heading-lvl 1))

;; org-tag-alist
;; '(("Learning" . ?l)
;;   ("ML" . ?L)
;;   ("Hobby" . ?h)
;;   ("Housekeeping" . ?k)
;;   ("English" . ?e)
;;   ("Child" . ?c)
;;   ("CS" . ?s)
;;   ("Finance" . ?f)
;;   ("CPP" . ?+)
;;   ("Python" . ?p)
;;   ("Math" . ?M)
;;   ("Frontend" . ?F)
;;   ("Backend" . ?b)
;;   ("Database" . ?d))
