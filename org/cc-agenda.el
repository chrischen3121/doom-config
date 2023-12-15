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

(setq! org-directory cc/org-home-dir
       org-agenda-files `(,cc/org-home-dir))

(after! org
  (setq! org-log-repeat nil
         +org-capture-journal-file (expand-file-name "journal.org" org-directory)
         cc/org-capture-habits-file (expand-file-name "habits.org" org-directory)
         org-deadline-warning-days 5
         org-log-done 'time
         org-log-into-drawer t)

  (setq! org-capture-templates
         '(("t" "Quick todo" entry
            (file+headline +org-capture-todo-file "Quick Tasks")
            "* [ ] %?\n%U\n" :prepend t)
           ("s" "Quick start" entry
            (file+headline +org-capture-todo-file "Quick Tasks")
            "* [-] %?\n%U\n"
            :clock-in t :clock-keep t prepend t)
           ("n" "Quick ideas" entry
            (file+headline +org-capture-notes-file "Ideas")
            "* %u %?\n%i\n" :prepend t)
           ("c" "Start a cource" entry
            (file+headline +org-capture-todo-file "Courses")
            "* PROJ %^{Course name}\n%i\nSTARTED:%u\n\n** TODO L1%?"
            :prepend t :empty-lines 1)
           ("b" "Start a book" entry
            (file+headline +org-capture-todo-file "Books")
            "* PROJ %^{Book name}\n%i\nSTARTED:%u\n** TODO C1%?"
            :empty-lines 1)
           ("p" "Personal projects" entry
            (file+headline +org-capture-todo-file "Projects")
            "* PROJ %^{Project name}\n%i\nCREATED:%u\n** TODO Task1%?"
            :prepend t :empty-lines 1)
           ("l" "Templates for local projects")
           ("lt" "Project-local todo" entry
            (file+headline +org-capture-project-todo-file "TODOs")
            "* [ ] %?\n%i\n" :prepend t)
           ("ln" "Project-local notes" entry
            (file+headline +org-capture-project-notes-file "Notes")
            "* %U %?\n%i\n" :prepend t)
           ("lc" "Project-local changelog" entry
            (file+headline +org-capture-project-changelog-file "Unreleased")
            "* %U %?\n%i\n%a" :prepend t)
           ("j" "Journal" entry
            (file+olp+datetree +org-capture-journal-file)
            "* %u %?\n%i\n" :prepend t)
           ("h" "Create a habit")
           ("hd" "Daily habit" entry
            (file+headline cc/org-capture-habits-file "Habits")
            "* [ ] [#C] %^{Habit Name}\nSCHEDULED: <%<%Y-%m-%d %a> .+1d>\n"
            :prepend t :empty-lines 1
            )
           ("hw" "Weekly habit" entry
            (file+headline cc/org-capture-habits-file "Habits")
            "* [ ] [#C] %^{Habit Name}\nSCHEDULED: <%<%Y-%m-%d %a> .+1w>\n"
            :prepend t :empty-lines 1
            )
           ("hm" "Monthly habit" entry
            (file+headline cc/org-capture-habits-file "Habits")
            "* [ ] [#C] %^{Habit Name}\nSCHEDULED: <%<%Y-%m-%d %a> .+1m>\n"
            :prepend t :empty-lines 1
            )
           )
         )
  )

(after! org-agenda
  (setq! org-agenda-start-with-log-mode t
         org-agenda-show-future-repeats nil)
  (map! :map org-agenda-mode-map
        "C" #'org-agenda-columns))

(after! org-clock
  (defun cc/org-clock-in-switch-state (state)
    "Switch to a new task state while clocking in, depending on the current STATE."
    (cond
     ((equal state "TODO") "STRT")
     ((equal state "[ ]") "[-]")
     ;; Otherwise, do not change the state
     (t state)))

  (setq! org-clock-in-switch-to-state #'cc/org-clock-in-switch-state
         org-clock-report-include-clocking-task t)
  )

(after! org
  (which-key-add-keymap-based-replacements org-mode-map "C-c m c" "org-clock")
  (map!
   :map org-mode-map
   "C-c m c e" #'org-set-effort
   "C-c m c E" #'org-clock-modify-effort-estimate
   "C-c m c c" #'org-clock-cancel
   "C-c m c g" #'org-clock-goto
   "C-c m c i" #'org-clock-in
   "C-c m c o" #'org-clock-out))

;; append to the global org-agenda-custom-commands list
(add-to-list 'org-agenda-custom-commands
             '("d" "Daily Dashboard"
               ((todo "STRT|[-]"
                      ((org-agenda-overriding-header "In Progress")
                       (org-agenda-sorting-strategy '(priority-up effort-down))))
                (agenda "" ((org-agenda-span 3)
                            (org-deadline-warning-days 5)
                            (org-agenda-overriding-header "Today's Agenda")))
                )))

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
