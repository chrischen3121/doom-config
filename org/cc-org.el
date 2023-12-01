;;; org/cc-org.el -*- lexical-binding: t; -*-
;; TODO: org-download
;; TODO: anki-editor
;; TODO: org-format-latex-options
;;
;;; Hints:
;;
;; C-c C-o -- Open link at point
;; C-c C-l -- Insert a link
;; C-x n s, C-x n w -- Narrow/Unnarrow buffer
;; C-c / -- Search in an outline tree and fold others as much as possible
;; M-S-RET -- Add TODO outlines or add items with a checkbox
;; C-c - -- Cycle bullets (-, +, *, ...)
;; C-c m i -- org-toggle-inline-images
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
(after! org
  ;; org-indent-mode
  (setq org-startup-indented nil) ; Prevent org-indent-mode from being enabled by default
  (remove-hook 'org-mode-hook #'org-indent-mode) ; Remove org-indent-mode from the org-mode-hook

  ;; Org titles
  (setq! org-ellipsis " ▼") ; ▼
  (dolist (face
           '((org-level-1 . 1.3)
             (org-level-2 . 1.2)
             (org-level-3 . 1.1)))
    (set-face-attribute (car face) nil :font "Hack" :weight 'bold :height (cdr face)))

  ;; Latex preview configuation
  (setq! org-pretty-entities t
         org-pretty-entities-include-sub-superscripts nil ; show sub'_' / super '^'
         org-highlight-latex-and-related '(native latex entities))

  ;; Inline image
  (setq! org-startup-with-inline-images t)
  (which-key-add-keymap-based-replacements org-mode-map "C-c \"" "plot")

  ;; Source code block
  (dolist (lang
           '(("py" . "src python")
             ("cpp" . "src cpp")
             ("sh" . "src bash")
             ("el" . "src emacs-lisp")
             ("tex" . "src latex")
             ("js" . "src js")
             ("html" . "src html")
             ("css" . "src css")
             ("json" . "src json")
             ("yaml" . "src yaml")
             ("plantuml" . "src plantuml")
             ("dot" . "src dot")
             ("shell" . "src shell")
             ("conf" . "src conf")))
    (add-to-list 'org-structure-template-alist lang))
  (map! :map org-mode-map
        "M-S-<return>" #'org-table-copy-down
        "S-<return>" #'org-insert-todo-heading
        "C-c m s" #'org-insert-structure-template
        "C-c m p" #'org-set-property-and-value
        "C-c m l" #'org-latex-preview
        "C-c m i" #'org-toggle-inline-images))

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))
