;;; org/cc-org-roam.el -*- lexical-binding: t; -*-

;; org +roam +noter
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
  (map! :map org-mode-map
        :prefix ("C-c ; r" . "Roam")
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
  :commands org-roam-ui-mode
  :config
  (setq! org-roam-ui-sync-theme t
         org-roam-ui-follow t
         org-roam-ui-update-on-save t
         org-roam-ui-open-on-start t)
  :init
  (map! :prefix "C-c ; r"
        :desc "Open org-roam-ui"
        "u" #'org-roam-ui-mode
        :desc "Sync ui theme"
        "l" #'org-roam-ui-sync-theme))
