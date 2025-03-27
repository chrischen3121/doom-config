;;; -*- lexical-binding: t; no-byte-compile: t; ---
;;; cc/notes/roam.el

(when (modulep! :lang org +roam2)
  (after! org-roam
    (when (>= emacs-major-version 29)
      ;; HACK https://github.com/org-roam/org-roam-ui/issues/289
      ;; emacs29 will set value to sqlite-builtin, only first file tag works
      (unless (functionp 'emacsql-sqlite)
        (defun emacsql-sqlite (db &rest args)
          (apply 'emacsql-sqlite-open db args)))
      (setq! org-roam-database-connector 'sqlite)
      )
    (advice-add 'org-roam-node-find :before #'cc/org-roam-choose-dir-if-not-set)
    (advice-add 'org-roam-capture :before #'cc/org-roam-choose-dir-if-not-set)
    (advice-add 'org-roam-node-insert :before #'cc/org-roam-choose-dir-if-not-set)

    (setq! org-roam-directory nil ; cc/roam-notes-dir
           org-roam-db-gc-threshold most-positive-fixnum
           org-roam-graph-viewer cc/org-roam-graph-viewer
           org-roam-dailies-directory cc/roam-journals-dir
           org-roam-capture-templates
           '(("d" "default" plain "%?"
              :if-new (file+head "${slug}-%<%Y%m%d>.org"
                                 "#+title: ${title}\n")
              :unnarrowed t)))

    ;; To export roam note correctly
    (advice-add 'org-export-dispatch
                :before
                (lambda (&rest _)
                  (require 'org-roam-export))))

  (use-package! org-roam-ui
    :commands org-roam-ui-mode
    :config
    (setq! org-roam-ui-sync-theme t
           org-roam-ui-follow t
           org-roam-ui-update-on-save t
           org-roam-ui-open-on-start t)
    :init
    (map! :prefix ("C-c n u" . "<org-roam-ui>")
          :desc "Start roam UI" "u" #'org-roam-ui-mode
          :desc "Open new UI page" "o" #'org-roam-ui-open
          :desc "Sync UI theme" "s" #'org-roam-ui-sync-theme
          :map org-mode-map
          :desc "Show ui node local" "g" #'org-roam-ui-node-local
          :desc "Zoom ui node" "z" #'org-roam-ui-node-zoom)))
