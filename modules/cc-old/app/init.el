;;; cc/app/init.el -*- lexical-binding: t; -*-

(defcustom cc/gcal-calendar-ids-alist nil
  "Alist of calendar ids for gcal."
  :type 'alist
  :group 'cc-app)

(defcustom cc/gcal-client-id nil
  "Google calendar client id."
  :type 'string
  :group 'cc-app)

(defcustom cc/gcal-client-secret nil
  "Google calendar client secret."
  :type 'string
  :group 'cc-app)
