;;; -*- lexical-binding: t; no-byte-compile: t; ---
;;; cc/notes/init.el

(defvar cc/default-org-dir "~/org/"
  "Org directory for default agenda files location.")

(defvar cc/org-id-locations '("~/org/.org-id-locations")
  "File to store org-id locations.")

(defvar cc/notes-base-dir (expand-file-name "notes/" cc/default-org-dir)
  "Org notes directory.")

(defvar cc/org-roam-graph-viewer "google-chrome"
  "Org-roam graph viewer.")
