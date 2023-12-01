;;; org/cc-agenda.el -*- lexical-binding: t; -*-
;; TODO: think and change

(setq! org-directory cc/agenda-home-dir
       cc/org-todo-file
       (expand-file-name cc/org-todo-filename cc/agenda-home-dir)
       cc/org-quicknotes-file
       (expand-file-name cc/org-quicknotes-filename cc/agenda-home-dir)
       cc/org-habits-file
       (expand-file-name cc/org-habits-filename cc/agenda-home-dir))

(after! recentf
  (add-to-list 'recentf-exclude cc/org-todo-file)
  (add-to-list 'recentf-exclude cc/org-quicknotes-file)
  (add-to-list 'recentf-exclude cc/org-habits-file))

(after! org-agenda
  (setq! org-agenda-files
         (directory-files-recursively cc/agenda-home-dir "\\.org$")
         org-default-notes-file cc/org-quicknotes-file
         org-capture-templates
         '(("p" "Project" entry
            (file+headline cc/org-todo-file "Projects")
            "* TODO %^{Project Name}\n%U\n\n** Tasks\n\n%?"
            :empty-lines 1)
           ("h" "Habit" entry
            (file+headline cc/org-habits-file "Habits")
            "* HABIT [#C] %^{Habit}\nSCHEDULED:%T\n" :empty-lines 1)
           ("b" "Book" entry
            (file+headline cc/org-todo-file "Books")
            "* TODO %^{Book Name}%^g\n%U\n" :empty-lines 1)
           ("c" "Course" entry
            (file+headline cc/org-todo-file "Courses")
            "* TODO %^{Course Name}%^g\n%U\n" :empty-lines 1)
           ("w" "Work" entry
            (file+headline cc/org-todo-file "Work")
            "* TODO %?%^g\n%U\n" :empty-lines 1)
           ("m" "Meeting" entry
            (file+olp cc/org-todo-file "Work" "Meetings")
            "* TODO %?\n%U\n"
            :clock-in :clock-resume
            :empty-lines 1))
         org-refile-targets
         '((nil :level . 1)
           (org-agenda-files :level . 1))
         org-export-with-todo-keywords nil
         org-todo-keywords
         '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)"  "|" "DONE(d!)" "CANCELED(c)")
           (sequence "BUG(b)" "REPORT(r)" "REVIEW(w)" "ISSUE(i)" "WARNING(w)" "|" "FIXED(f" "CANCELED(c")
           (sequence "HABIT(h)" "|" "DONE(d!)" "CANCELED(c")
           )
         org-todo-keyword-faces
         '(("TODO" . "dark salmon")
           ("NEXT" . "light blue")
           ("HOLD" . "medium purple")
           ("DONE" . "light green")
           ("CANCELED" .  "dark gray")
           ("BUG" . "red")
           ("REPORT" . "blue")
           ("REVIEW" . "purple")
           ("ISSUE" . "magenta")
           ("WARNING" . "orange")
           ("HABIT" . "dark green")
           ("FIXED" . "light green")
           ("CANCELED" . "dark gray")
           )

         org-clock-in-switch-to-state "NEXT"

         ;; Clock configuration
         org-deadline-warning-days 5
         org-clock-out-remove-zero-time-clocks t
         org-clock-out-when-done t
         org-clock-in-resume t
         org-clock-report-include-clocking-task t
         org-log-done 'time
         org-log-into-drawer t
         org-clock-persist t
         org-clock-persist-query-resume nil ; Do not prompt to resume an active clock

         ;; Agenda configuration
         org-tag-alist
         '(("Learning" . ?l)
           ("ML" . ?L)
           ("Hobby" . ?h)
           ("Housekeeping" . ?k)
           ("English" . ?e)
           ("Child" . ?c)
           ("CS" . ?s)
           ("Finance" . ?f)
           ("CPP" . ?+)
           ("Python" . ?p)
           ("Math" . ?M)
           ("Frontend" . ?F)
           ("Backend" . ?b)
           ("Database" . ?d))
         org-agenda-custom-commands
         '(;; Daily dashboard
           ("d" "Dashboard"
            ((agenda "" ((org-deadline-warning-days 5)
                         (org-agenda-span 3)
                         (org-agenda-repeating-timestamp-show-all t)))
             (todo "NEXT"
                   ((org-agenda-overriding-header "Next Tasks")
                    (org-agenda-sorting-strategy '(priority-up effort-down))))
             (stuck "") ; review stuck projects as designated by org-stuck-projects
             ))

           ;; Weekly review
           ("r" "Weekly Review"
            ((agenda "" ((org-deadline-warning-days 15)
                         (org-agenda-span 7)
                         (org-agenda-sorting-strategy '(priority-up effort-down))))
             (stuck "")
             (todo "DONE")))

           ;; tags-todo
           ("g" "GTD Tagged Agenda"
            ((tags-todo "Work")
             (tags-todo "Learning")
             (tags-todo "Project")
             (tags-todo "Child")
             (tags-todo "English"))
            nil)

           ;; Upcoming deadlines
           ("D" "Upcoming Deadlines" agenda ""
            ((org-agenda-entry-types '(:deadline))
             (org-agenda-span 1)
             (org-deadline-warning-days 90)
             (org-agenda-time-grid nil)))

           ;; Archive search
           ("A" "Archive Search" search ""
            ((org-agenda-files (directory-files-recursively cc/agenda-home-dir ".org_archive$"))))

           ;; Effort
           ("t" "Effort Table" alltodo ""; "DONE"
            ((org-columns-default-format-for-agenda
              "%10CATEGORY %15ITEM(TASK) %2PRIORITY %5TODO %SCHEDULED %DEADLINE %5EFFORT(ESTIMATED){:} %5CLOCKSUM(SPENT)")
             (org-agenda-view-columns-initially t)
             (org-agenda-span 30)))

           ;; Low-effort next actions
           ("e" tags-todo "+TODO=\"NEXT\"+Effort<30&+Effort>0"
            ((org-agenda-overriding-header "Low Effort Tasks")
             (org-agenda-max-todos 15)
             (org-agenda-files org-agenda-files))))
         org-agenda-start-with-log-mode t
         ))

(after! org-superstar
  (setq! org-superstar-special-todo-items t
         org-superstar-todo-bullet-alist
         '(("TODO" . ?☐)
           ("NEXT" . ?➡)
           ("HOLD" . ?⧖)
           ("DONE" . ?✔)
           ("CANCELED" . ?✘)
           ("BUG" . ?)
           ("REPORT" . ?)
           ("REVIEW" . ?)
           ("ISSUE" . ?)
           ("WARNING" . ?)
           ("HABIT" . ?))))
