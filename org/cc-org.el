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
  (which-key-add-keymap-based-replacements org-mode-map "C-c m I" "org-id")
  )

;; Keybindings
(map! :after org
      :map org-mode-map
      "M-S-<return>" #'org-table-copy-down
      "S-<return>" #'org-insert-todo-heading
      "C-c m s" #'org-insert-structure-template
      "C-c m p" #'org-set-property
      "C-c m I c" #'org-id-get-create
      "C-c m I g" #'org-id-goto
      "C-c m I w" #'org-id-copy
      "C-c m I l" #'org-id-store-link
      "C-c m l" #'org-latex-preview
      "C-c m i" #'org-toggle-inline-images)

;; org-protocol configuration for Logseq
(use-package! org-protocol
  :after org
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-protocol-logseq" :protocol "find-file" :function org-protocol-logseq :kill-client nil))
  (defun org-protocol-logseq (fname)
    "Process org-protocol://find-file?path= style URL."
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file f)
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))

;; anki-editor
(use-package! anki-editor
  :after org
  :config
  (setq! cc/org-anki-file (expand-file-name "anki.org" org-directory))
  (setq! anki-editor-create-decks t
         anki-editor-org-tags-as-anki-tags t
         anki-editor-use-math-jax t
         anki-editor-export-preamble nil
         anki-editor-export-index t
         anki-editor-org-file cc/org-anki-file
         anki-editor-new-notes-format "* %s :drill:\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: %s\n:END:\n** Front\n%s\n** Back\n%s"
         anki-editor-cloze-delete-func 'anki-editor-cloze-delete-blank
         anki-editor-cloze-delete-blank 'anki-editor-cloze-delete-blank
         anki-editor-cloze-delete-all 'anki-editor-cloze-delete-all
         anki-editor-cloze-delete-region 'anki-editor-cloze-delete-region)
  (which-key-add-keymap-based-replacements org-mode-map "C-c k" "anki")
  :bind (:map org-mode-map
              ("C-c k p" . anki-editor-push-notes)
              ("C-c k c" . anki-editor-cloze-dwim)
              ("C-c k i" . anki-editor-insert-note)
              ("C-c k u" . anki-editor-update-note)
              ("C-c k 0" . anki-editor-clear-cloze))
  )

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))

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
