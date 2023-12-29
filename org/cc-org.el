;;; org/cc-org.el -*- lexical-binding: t; -*-
;; TODO: auto close emphasis and latex $
;; TODO: write latex
;; TODO: anki-editor
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
(after! org
  ;; disable org-indent-mode
  (setq! org-startup-indented nil) ; Prevent org-indent-mode from being enabled by default
  (remove-hook 'org-mode-hook #'org-indent-mode) ; Remove org-indent-mode from the org-mode-hook

  ;; Org titles
  (setq! org-ellipsis " ▼") ; ▼
  (dolist (face
           '((org-level-1 . 1.3)
             (org-level-2 . 1.2)
             (org-level-3 . 1.1)))
    (set-face-attribute (car face) nil :font "Hack" :weight 'bold :height (cdr face)))

  ;; Latex preview configuration
  (setq! org-pretty-entities t
         org-pretty-entities-include-sub-superscripts nil ; show sub'_' / super '^'
         org-highlight-latex-and-related '(native latex entities))

  ;; Inline image
  (setq! org-startup-with-inline-images t)

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
  (which-key-add-keymap-based-replacements org-mode-map "C-c \""  "plot"))

;; :ui
;; deft
(after! deft
  (setq! deft-directory cc/deft-notes-dir
         deft-use-filename-as-title t
         deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"))


(after! org-noter
  (setq! org-noter-notes-search-path `(,cc/org-pdf-notes-dir)))


;; Keybindings
(map! :after org
      :map org-mode-map
      "M-S-<return>" #'org-table-copy-down
      "S-<return>" #'org-insert-todo-heading
      :prefix "C-c m"
      :desc "Insert code template"
      "s" #'org-insert-structure-template
      :desc "Set property"
      "p" #'org-set-property
      :desc "Toggle latex preview"
      "l" #'org-latex-preview
      :desc "Toggle inline images"
      "i" #'org-toggle-inline-images
      :prefix ("C-c m I" . "org-id")
      :desc "Generate org-id"
      "c" #'org-id-get-create
      "g" #'org-id-goto
      "w" #'org-id-copy
      "l" #'org-id-store-link)

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
  :defer t
  :after org
  :init
  (which-key-add-keymap-based-replacements org-mode-map "C-c k" "anki")
  :config
  (setq! anki-editor-create-decks t
         anki-editor-org-tags-as-anki-tags t
         anki-editor-use-math-jax t)
  ;; TODO
  :bind (:map org-mode-map
              ("C-c k p" . anki-editor-push-notes)
              ("C-c k c" . anki-editor-cloze-dwim)
              ("C-c k i" . anki-editor-insert-note)
              ("C-c k u" . anki-editor-update-note)
              ("C-c k 0" . anki-editor-clear-cloze))
  )

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))


;; TODO: maybe use org filename instead of 0 level heading
(use-package! org-download
  :after-call (org-mode-hook)
  :config
  (setq! org-download-image-dir "images/"
         org-download-heading-lvl 0)
  (map! :map org-mode-map
        :prefix ("C-c m d" . "org-download")
        :desc "Download screenshot"
        "p" #'cc/org-download-screenshot
        :desc "Delete downloaded image"
        "d" #'org-download-delete
        :desc "Delete all downloaded images"
        "D" #'org-download-delete-all
        :desc "Download clipboard"
        "y" #'org-download-clipboard))

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
