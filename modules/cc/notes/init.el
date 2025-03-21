;;; cc/notes/init.el -*- lexical-binding: t; -*-

(defvar cc/default-org-dir "~/org/"
  "Org directory")

(defvar cc/org-id-locations (expand-file-name "org-id-locations" cc/default-org-dir)
  "Org id locations file.")

(defvar cc/notes-base-dir (expand-file-name "notes/" cc/default-org-dir)
  "Org notes directory.")

(defvar cc/org-roam-graph-viewer "google-chrome"
  "Org-roam graph viewer.")
