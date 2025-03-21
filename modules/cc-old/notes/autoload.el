;; -*- no-byte-compile: t; -*-
;;; cc/notes/autoload.el -*- lexical-binding: t; -*-

;; TODO use emacs cache to store the org-roam directory, use C-c n n to switch between directories

;;;###autoload
(defun cc/open-pdf-note-files ()
  "Open all PDF note files in `cc/org-pdf-notes-dir'."
  (interactive)
  (let ((default-directory cc/org-pdf-notes-dir))
    (call-interactively 'find-file)))


;;;###autoload
(defun cc/org-roam-choose-directory ()
  "Choose a directory to use as the org-roam directory."
  (interactive)

  (let ((ignored-dirs '("public" "images"))
        (chosen-dir
         (completing-read "Choose directory: "
                          (directory-files cc/roam-notes-dir nil "^[A-Z]"))))
    (setq! org-roam-directory
           (expand-file-name chosen-dir cc/roam-notes-dir)
           org-roam-db-location
           (expand-file-name ".cache/org-roam.db" org-roam-directory)))
  (dired org-roam-directory))


;;;###autoload
(defun cc/org-roam-find-by-dir (&rest args)
  "Wrapped `org-roam-node-find' that prompts for a directory first."
  (interactive)
  (unless (bound-and-true-p org-roam-directory)
    (cc/org-roam-choose-directory))
  (apply #'org-roam-node-find args))


;;;###autoload
(defun cc/org-roam-capture-by-dir (&rest args)
  "Wrapped `org-roam-capture' that prompts for a directory first."
  (interactive)
  (unless (bound-and-true-p org-roam-directory)
    (cc/org-roam-choose-directory))
  (apply #'org-roam-capture args))
