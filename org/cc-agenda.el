;;; org/cc-agenda.el -*- lexical-binding: t; -*-
;;
;; DOOM org-todo-keywords explanation
;;
;; ----------------------------------------
;; Group 1
;; This sequence is used for projects
;;
;; TODO: A task that needs to be done.
;; PROJ: A project, which is a collection of tasks.
;; LOOP: A recurring task.
;; STRT: A task that has been started.
;; WAIT: A task that is waiting on something or someone.
;; HOLD: A task that is on hold.
;; IDEA: An idea that might be turned into a task later.
;; ---------
;; DONE: A task that has been completed.
;; KILL: A task that has been cancelled or is no longer applicable.
;;
;; ----------------------------------------
;; Group 2
;; This sequence is used for tasks
;;
;; [ ]: An incomplete checklist item.
;; [-]: A checklist item that is in progress.
;; [?]: A checklist item that is in a state of uncertainty or waiting.
;; ---------
;; [X]: A completed checklist item.
;;
;; ----------------------------------------
;; Group 3
;; This sequence is likely used for items that represent a decision or an approval process
;; OKAY: An item that is deemed okay or acceptable.
;; YES: An affirmative decision.
;; NO: A negative decision.

;; append to the global org-agenda-custom-commands list


;; ;; Agenda configuration
;; org-agenda-custom-commands
;; '(;; Daily dashboard
;;   ("d" "Dashboard"
;;    ((agenda "" ((org-deadline-warning-days 5)
;;                 (org-agenda-span 3)
;;                 (org-agenda-repeating-timestamp-show-all t)))
;;     (todo "NEXT"
;;           ((org-agenda-overriding-header "Next Tasks")
;;            (org-agenda-sorting-strategy '(priority-up effort-down))))
;;     (stuck "") ; review stuck projects as designated by org-stuck-projects
;;     ))

;;   ;; Weekly review
;;   ("r" "Weekly Review"
;;    ((agenda "" ((org-deadline-warning-days 15)
;;                 (org-agenda-span 7)
;;                 (org-agenda-sorting-strategy '(priority-up effort-down))))
;;     (stuck "")
;;     (todo "DONE")))

;;   ;; tags-todo
;;   ("g" "GTD Tagged Agenda"
;;    ((tags-todo "Work")
;;     (tags-todo "Learning")
;;     (tags-todo "Project")
;;     (tags-todo "Child")
;;     (tags-todo "English"))
;;    nil)

;;   ;; Upcoming deadlines
;;   ("D" "Upcoming Deadlines" agenda ""
;;    ((org-agenda-entry-types '(:deadline))
;;     (org-agenda-span 1)
;;     (org-deadline-warning-days 90)
;;     (org-agenda-time-grid nil)))

;;   ;; Archive search
;;   ("A" "Archive Search" search ""
;;    ((org-agenda-files (directory-files-recursively cc/org-home-dir ".org_archive$"))))

;;   ;; Effort
;;   ("t" "Effort Table" alltodo ""; "DONE"
;;    ((org-columns-default-format-for-agenda
;;      "%10CATEGORY %15ITEM(TASK) %2PRIORITY %5TODO %SCHEDULED %DEADLINE %5EFFORT(ESTIMATED){:} %5CLOCKSUM(SPENT)")
;;     (org-agenda-view-columns-initially t)
;;     (org-agenda-span 30)))

;;   ;; Low-effort next actions
;;   ("e" tags-todo "+TODO=\"NEXT\"+Effort<30&+Effort>0"
;;    ((org-agenda-overriding-header "Low Effort Tasks")
;;     (org-agenda-max-todos 15)
;;     (org-agenda-files org-agenda-files))))
;; )
