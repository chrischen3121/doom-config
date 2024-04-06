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
(remove-hook! 'org-load-hook #'+org-init-smartparens-h)

(after! org
  ;; disable org-indent-mode
  (setq! org-startup-indented nil) ; Prevent org-indent-mode from being enabled by default
  (remove-hook 'org-mode-hook #'org-indent-mode) ; Remove org-indent-mode from the org-mode-hook
  (require 'org-indent) ; fix: Invalid face reference: org-indent
  ;; Org titles
  (setq! org-ellipsis " ▼") ; ▼
  (dolist (face
           '((org-level-1 . 1.3)
             (org-level-2 . 1.2)
             (org-level-3 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; Latex hints:
  ;; ` - cdlatex-math-symbol
  ;; ' - cdlatex-math-modify e.g. 'b: /mathbf


  ;; Latex preview configuration
  (setq! org-pretty-entities t
         org-pretty-entities-include-sub-superscripts nil ; show sub'_' / super '^'
         org-highlight-latex-and-related '(native latex entities))

  ;; Inline image
  (setq! org-startup-with-inline-images t)

  ;; Source code block
  ;; TODO maybe just use yasnippet
  (appendq! org-structure-template-alist
            '(("el" . "src emacs-lisp")
              ("py" . "src python")
              ("cpp" . "src cpp")
              ("sh" . "src bash")
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


  ;; smartparens for inline latex
  (sp-with-modes 'org-mode
    (sp-local-pair "$" "$"
                   :unless '(sp-point-after-word-p)))


  (map! :map org-mode-map
        :prefix ("C-c ; p" . "preview/plot")
        :desc "Preview latex fragment" "l" #'org-latex-preview
        :desc "Preview image" "i" #'org-display-inline-images
        :desc "Plot table" "p" #'org-plot/gnuplot
        "d" nil
        "u" nil))

(after! org-noter
  (setq! org-noter-notes-search-path `(,cc/org-pdf-notes-dir)
         org-noter-highlight-selected-text t
         org-noter-auto-save-last-location t
         org-noter-max-short-selected-text-length 40)
  (map! :map (org-noter-notes-mode-map org-noter-doc-mode-map)
        :prefix ("C-c ; n" . "org-noter")
        :desc "Sync next note"
        "n" #'org-noter-sync-next-note
        :desc "Sync previous note"
        "p" #'org-noter-sync-prev-note
        :desc "Sync page or chapter"
        "s" #'org-noter-sync-current-page-or-chapter
        :desc "Sync current note"
        "c" #'org-noter-sync-current-note
        :map org-noter-doc-mode-map
        :desc "Insert note" "e" #'org-noter-insert-note
        :desc "Insert precise note" "M-e" #'org-noter-insert-precise-note))


;; Keybindings
(map! :after org
      :map org-mode-map
      :desc "Insert date" "C-c !" #'org-timestamp-inactive
      "M-S-<return>" #'org-table-copy-down
      "S-<return>" #'org-insert-todo-heading

      :prefix "C-c ;"
      :desc "Insert code template"
      "s" #'org-insert-structure-template

      :prefix ("C-c ; i" . "org-id")
      :desc "Generate org-id"
      "c" #'org-id-get-create
      "g" #'org-id-goto
      "w" #'org-id-copy
      "l" #'org-id-store-link)

;; org-protocol configuration for Logseq
(use-package! org-protocol
  :after-call (org-mode-hook)
  :config
  (add-to-list
   'org-protocol-protocol-alist
   '("org-protocol-logseq"
     :protocol "find-file"
     :function org-protocol-logseq
     :kill-client nil))
  (defun org-protocol-logseq (fname)
    "Process org-protocol://find-file?path= style URL."
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file f)
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))

;; anki-editor
(use-package! anki-editor
  :after-call (org-mode-hook)
  :commands (anki-editor-push-notes
             anki-editor-insert-note
             anki-editor-update-note)
  :config
  (setq! anki-editor-create-decks t
         anki-editor-org-tags-as-anki-tags t
         anki-editor-use-math-jax t)
  :init
  (map! :map org-mode-map
        :prefix ("C-c ; k" . "anki")
        :desc "Push cards" "p" #'anki-editor-push-notes
        :desc "Cloze dwim" "c" #'anki-editor-cloze-dwim
        :desc "Insert card" "i" #'anki-editor-insert-note
        :desc "Update card" "u" #'anki-editor-update-note
        :desc "Clear cloze" "0" #'anki-editor-clear-cloze))

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))


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
