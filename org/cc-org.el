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
  (which-key-add-keymap-based-replacements org-mode-map "C-c m I" "org-id")
  (which-key-add-keymap-based-replacements org-mode-map "C-c \""  "plot"))

;; :ui
;; deft
(after! deft
  (setq! deft-directory cc/deft-notes-dir
         deft-use-filename-as-title t
         deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"))

;; :lang
;; org +roam +noter
;; TODO maybe org-roam-ui
(after! org-roam
  (setq! org-roam-directory cc/org-roam-directory
         org-roam-db-location cc/org-roam-db-location
         org-roam-db-gc-threshold most-positive-fixnum
         org-roam-graph-viewer cc/org-roam-graph-viewer
         org-roam-dailies-directory cc/org-roam-journal-directory
         org-roam-capture-templates
         '(("d" "default" plain "%?"
            :if-new (file+head "${slug}-%<%Y%m%d>.org"
                               "#+title: ${title}\n")
            :unnarrowed t)
           ("t" "tagged" plain "%?"
            :if-new (file+head "${slug}-%<%Y%m%d>.org"
                               "#+title: ${title}\n#+filetags: %^{filetags}\n")
            :unnarrowed t)
           ))
  (org-roam-db-autosync-mode)
  (map! :prefix "C-c n r"
        :desc "Generate org-id"
        "p" #'org-id-get-create
        :map org-mode-map
        :prefix "C-c n r"
        :desc "Add alias"
        "a" #'org-roam-alias-add
        :desc "Open org-roam buffer"
        "b" #'org-roam-buffer-toggle
        :desc "Add tag"
        "t" #'org-roam-tag-add
        :desc "Add ref"
        "r" #'org-roam-ref-add))

;; org-roam-ui
(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-mode
  :config
  (setq! org-roam-ui-sync-theme t
         org-roam-ui-follow t
         org-roam-ui-update-on-save t
         org-roam-ui-open-on-start t))

(map! :prefix "C-c n r"
      :desc "Open org-roam-ui"
      "u" #'org-roam-ui-mode
      :desc "Sync ui theme"
      "l" #'org-roam-ui-sync-theme)


(after! org-noter
  (setq! org-noter-notes-search-path `(,cc/org-pdf-notes-dir)))


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
  :init
  (which-key-add-keymap-based-replacements org-mode-map "C-c k" "anki")
  :config
  (setq! anki-editor-create-decks t
         anki-editor-org-tags-as-anki-tags t
         anki-editor-use-math-jax t)
  :bind (:map org-mode-map
              ("C-c k p" . anki-editor-push-notes)
              ("C-c k c" . anki-editor-cloze-dwim)
              ("C-c k i" . anki-editor-insert-note)
              ("C-c k u" . anki-editor-update-note)
              ("C-c k 0" . anki-editor-clear-cloze))
  )

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))


;; TODO: add org-download
;; wl-clipboard for org-download
;; pamac install --no-confirm --needed wl-clipboard


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
