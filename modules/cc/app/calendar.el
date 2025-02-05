;;; cc/app/calendar.el -*- lexical-binding: t; -*-

;; Documentation:
;; calfw: https://github.com/kiwanami/emacs-calfw
;; org-gcal: https://github.com/kidd/org-gcal.el
(setq! calendar-week-start-day 1)

(when (modulep! :app calendar)
  (setq! cfw:org-overwrite-default-keybinding t
         org-gcal-client-id cc/gcal-client-id
         org-gcal-client-secret cc/gcal-client-secret
         org-gcal-fetch-file-alist cc/gcal-calendar-ids-alist))
