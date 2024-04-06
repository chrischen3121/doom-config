;;; core/cc-var-define.el -*- lexical-binding: t; -*-
(defcustom cc/personal-dictionary-dir "~/dicts/"
  "Personal dictionary directory."
  :type 'string
  :group 'cc-config)

;; agenda configuration
(defcustom cc/org-home-dir "~/org/"
  "Agenda home directory"
  :type 'string
  :group 'cc-org-config)

(defcustom cc/org-pdf-notes-dir "~/org/pdf-notes/"
  "Org pdf notes directory."
  :type 'string
  :group 'cc-org-config)

;; org-roam configuration
(defcustom cc/org-roam-directory "~/org/roam/"
  "Org-roam directory."
  :type 'string
  :group 'cc-org-roam-config)

(defcustom cc/org-roam-journal-directory "~/org/roam/journal/"
  "Org-roam journal directory."
  :type 'string
  :group 'cc-org-roam-config)

(defcustom cc/org-roam-db-location "~/org/roam/org-roam.db"
  "Org-roam database location."
  :type 'string
  :group 'cc-org-roam-config)

(defcustom cc/org-roam-graph-viewer "google-chrome"
  "Org-roam graph viewer."
  :type 'string
  :group 'cc-org-roam-config)
