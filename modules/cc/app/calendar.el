;;; cc/app/calendar.el -*- lexical-binding: t; -*-

;; Documentation:
;; calfw: https://github.com/kiwanami/emacs-calfw
;; org-gcal: https://github.com/kidd/org-gcal.el
(after! calendar
  (setq! calendar-week-start-day 1
         cfw:org-overwrite-default-keybinding t))

(after! org-gcal
  (setq! org-gcal-client-id cc/gcal-client-id
         org-gcal-client-secret cc/gcal-client-secret
         org-gcal-fetch-file-alist cc/gcal-calendar-ids-alist))
