;; -*- no-byte-compile: t; -*-
;;; cc/notes/config.el -*- lexical-binding: t; -*-
;; TODO: org-format-latex-options
;; TODO: may try +jupyter +pandoc +pretty(latex highlighting)
;;
;;; Hints:
;;
;; C-c o l -- Open link at point
;; C-c i l -- Insert a link
;; C-x n s, C-x n w -- Narrow/Unnarrow buffer
;; C-c s o -- Search in an outline tree as much as possible
;; C-c - -- Cycle bullets (-, +, *, ...)
;; C-c <TAB> -- org-toggle-inline-images
;; C-c ; -- Toggle the "COMMENT" keyword
;; ====== Tags ==========
;; C-c C-c -- Set a tag
;; M-x org-agenda m/M -- Match tags (only TODO)
;; ====== Properties ========
;; C-c C-x p -- Set a property
;; ====== Timestamps ========
;; C-c . -- Set a timestamp
;; S-LEFT/S-Right -- Change by one day
;; ====== Footnotes =========
;; C-c l f footnote prefix
;; C-c C-c -- jump between definition and reference
;; +strike-through+
(defvar cc/org-home-dir "~/org/"
  "Org directory")

(defvar cc/org-agenda-dir "~/org/todos/"
  "Agenda home directory")

(defvar cc/deft-notes-dir "~/org/notes/"
  "Deft notes directory.")

(defvar cc/org-pdf-notes-dir "~/org/pdf-notes/"
  "Org pdf notes directory.")

(defvar cc/org-roam-directory "~/org/roam/"
  "Org-roam directory.")

(defvar cc/org-roam-journal-directory "~/org/roam/journal/"
  "Org-roam journal directory.")

(defvar cc/org-roam-db-location "~/org/roam/org-roam.db"
  "Org-roam database location.")

(defvar cc/org-roam-graph-viewer "google-chrome"
  "Org-roam graph viewer.")

(map! :after org
      :map org-mode-map

      ;; non prefix
      "M-S-<return>" #'org-table-copy-down
      "S-<return>" #'org-insert-todo-heading
      :desc "Open link" "C-c o l" #'org-open-at-point
      :desc "Search in outline tree" "C-c s o" #'org-sparse-tree

      ;; C-c i --- insert
      (:prefix "C-c i"
       :desc "Org insert date" "d" #'org-timestamp-inactive
       :desc "Org insert time" "t" #'org-timestamp
       :desc "Set a tag" "T" #'org-set-tags-command
       :desc "Set a tag" "p" #'org-set-property
       :desc "Insert link" "l" #'org-insert-link)


      ;; local prefix l
      (:prefix "C-c l"

       ;; i -- create org-id
       :desc "Create org-id" "i" #'org-id-get-create

       ;; p -- preview/plot
       (:prefix ("p" . "<preview/plot>")
        :desc "Preview latex fragment" "l" #'org-latex-preview
        :desc "Preview image" "i" #'org-display-inline-images
        :desc "Plot table" "p" #'org-plot/gnuplot)

       ;; f -- footnote
       :desc "Insert footnote" "f" #'org-footnote-new
       ))


;; org configuration
(remove-hook! 'org-load-hook #'+org-init-smartparens-h)
(after! org
  (remove-hook 'org-mode-hook #'org-indent-mode)
  (setq! org-startup-indented nil
         org-ellipsis " ▼"
         org-directory cc/org-home-dir

         ;; pretty latex preview
         org-pretty-entities t
         org-pretty-entities-include-sub-superscripts nil
         org-highlight-latex-and-related '(native latex entities)

         ;; inline image
         org-startup-with-inline-images t)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face))))

(when (modulep! :config default +smartparens)
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "$" "$"
                     :unless '(sp-point-after-word-p)))))

(when (modulep! :lang org +noter)
  (map! :prefix ("C-c n p" . "<pdfnotes>")
        :desc "Find pdfnotes file" "f" #'cc/open-pdf-note-files
        :desc "Org noter" "o" #'org-noter)
  (after! org-noter
    (setq! org-noter-notes-search-path `(,cc/org-pdf-notes-dir)
           org-noter-highlight-selected-text t
           org-noter-auto-save-last-location t
           org-noter-max-short-selected-text-length 40)
    (add-hook! 'org-noter-doc-mode-hook
               (setq-local pdf-view-display-size 'fit-width))
    (map! (:map (org-noter-notes-mode-map org-noter-doc-mode-map)
           :prefix "C-c n p"
           :desc "Generate TOC" "t" #'org-noter-create-skeleton
           :desc "Sync next note" "n" #'org-noter-sync-next-note
           :desc "Sync previous note" "p" #'org-noter-sync-prev-note
           :desc "Sync page or chapter" "S"
           #'org-noter-sync-current-page-or-chapter
           :desc "Sync current note" "s" #'org-noter-sync-current-note
           :desc "Exit org noter" "q" #'org-noter-kill-session
           :desc "Set start location" "l" #'org-noter-set-start-location)
          (:map org-noter-doc-mode-map
           :desc "Insert note" "e" #'org-noter-insert-note
           :desc "Insert precise note" "M-e" #'org-noter-insert-precise-note))))

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
  (org-roam-db-autosync-mode))

(when (modulep! :lang org +roam2)
  (use-package! org-roam-ui
    :commands org-roam-ui-mode
    :config
    (setq! org-roam-ui-sync-theme t
           org-roam-ui-follow t
           org-roam-ui-update-on-save t
           org-roam-ui-open-on-start t)
    :init
    ;; HACK https://github.com/org-roam/org-roam-ui/issues/289
    ;; emacs29 will set value to sqlite-builtin, only first file tag works
    (setq! org-roam-database-connector 'sqlite)
    (map! :prefix ("C-c n u" . "<org-roam-ui>")
          :desc "Start roam UI" "u" #'org-roam-ui-mode
          :desc "Open new UI page" "o" #'org-roam-ui-open
          :desc "Sync UI theme" "s" #'org-roam-ui-sync-theme
          :map org-mode-map
          :desc "Show ui node local" "g" #'org-roam-ui-node-local
          :desc "Zoom ui node" "z" #'org-roam-ui-node-zoom)))


(use-package! anki-editor
  ;;:after-call (org-mode-hook)
  :commands (anki-editor-push-notes
             anki-editor-insert-note)
  :config
  (setq! anki-editor-create-decks t
         anki-editor-org-tags-as-anki-tags t
         anki-editor-use-math-jax t)
  :init
  (map! :map org-mode-map
        :prefix ("C-c l a" . "<anki>")
        :desc "Push cards" "p" #'anki-editor-push-notes
        :desc "Cloze dwim" "c" #'anki-editor-cloze-dwim
        :desc "Cloze region" "r" #'anki-editor-cloze-region
        :desc "Insert card" "i" #'anki-editor-insert-note
        :desc "Clear cloze" "0" #'anki-editor-clear-cloze))

(use-package! org-download
  :commands (org-download-screenshot
             org-download-clipboard
             org-download-delete
             org-download-rename-at-point)
  :init
  (map! :map org-mode-map
        :prefix ("C-c l d" . "<org-download>")
        :desc "Insert screenshot" "i" #'org-download-screenshot
        :desc "Insert from clipboard" "y" #'org-download-clipboard
        :desc "Rename at point" "r" #'org-download-rename-at-point
        :desc "Delete at point" "d" #'org-download-delete)
  :config
  (setq! org-download-image-dir "images/"
         org-download-heading-lvl 1
         org-download-annotate-function (lambda (_link) "")))
