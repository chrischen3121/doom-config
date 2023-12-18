;;; core/cc-var-define.el -*- lexical-binding: t; -*-
(defcustom cc/default-font-size 20
  "Adjust default font size."
  :type 'integer
  :group 'cc-config)

(defcustom cc/default-unicode-font-size 20
  "Adjust unicode font size for your system."
  :type 'integer
  :group 'cc-config)

(defcustom cc/spell-personal-dictionary "~/en.pws"
  "Personal dictionary for spell checking."
  :type 'string
  :group 'cc-config)

;; agenda configiration
(defcustom cc/org-home-dir "~/org/"
  "Agenda home directory"
  :type 'string
  :group 'cc-agenda-config)
