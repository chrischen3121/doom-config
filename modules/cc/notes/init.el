;;; -*- lexical-binding: t; no-byte-compile: t; ---
;;; cc/notes/init.el

(defcustom cc/default-org-dir "~/org/"
  "Org directory for default agenda files location."
  :group 'cc-note
  :type 'directory)

(defcustom cc/org-id-locations '("~/org/.org-id-locations")
  "File to store org-id locations."
  :group'cc-note
  :type 'file)

(defcustom cc/notes-base-dir (expand-file-name "notes/" cc/default-org-dir)
  "Org notes directory."
  :group 'cc-note
  :type 'directory)
