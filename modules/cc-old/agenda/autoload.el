;;; cc/agenda/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cc/org-clock-in-switch-state (state)
  "Switch to a new task state while clocking in, depending on the current STATE."
  (cond
   ((equal state "TODO") "STRT")
   ((equal state "[ ]") "[-]")
   ;; Otherwise, do not change the state
   (t state)))
