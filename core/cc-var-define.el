;;; core/cc-var-define.el -*- lexical-binding: t; -*-
(defcustom cc/default-font-size 20
  "Adjust default font size."
  :type 'integer
  :group 'cc-config)

(defcustom cc/default-unicode-font-size 20
  "Adjust unicode font size for your system."
  :type 'integer
  :group 'cc-config)

(defcustom cc/frame-transparency '(90 . 90)
  "Make frame transparency overridable."
  :type '(cons integer integer)
  :group 'cc-config)

;; agenda configiration
(defcustom cc/agenda-home-dir "~/TODOs"
  "Agenda home directory"
  :type 'string
  :group 'cc-agenda-config)

(defcustom cc/org-todo-filename "todo.org"
  "Agenda todo filename"
  :type 'string
  :group 'cc-agenda-config)

(defcustom cc/org-habits-filename "habits.org"
  "Agenda habits filename"
  :type 'string
  :group 'cc-agenda-config)
