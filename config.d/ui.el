;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/ui.el

(when(modulep! :ui popup)
  (map! :map +popup-buffer-mode-map
        :desc "Raise popup" "C-c C-p" #'+popup/raise))

(when (modulep! :ui doom-dashboard)
  (setq! +doom-dashboard-name "Happy Hacking!"))

(when (modulep! :ui treemacs)
  (map! :map treemacs-mode-map
        "C-c C-p" nil
        "C-c C-w" nil
        :desc "Select window" "C-x o" #'treemacs-select-window
        (:prefix
         ("p" . "<tree-project>")
         :desc "Switch project" "o" #'treemacs-projectile
         :desc "Add project" "a" #'treemacs-add-project-to-workspace
         :desc "Remove project" "k" #'treemacs-remove-project-from-workspace
         :desc "Unfold all" "c" #'treemacs-collapse-all-projects
         :desc "Rename project" "r" #'treemacs-rename-project)
        (:prefix
         ("w" . "<tree-workspace>")
         :desc "Create workspace" "c" #'treemacs-create-workspace
         :desc "Remove workspace" "k" #'treemacs-remove-workspace
         :desc "Edit workspaces" "e" #'treemacs-edit-workspaces
         :desc "Rename workspace" "r" #'treemacs-rename-workspace
         :desc "Switch workspace" "o" #'treemacs-switch-workspace))
  (setq! +treemacs-git-mode 'deferred))

(when (modulep! :ui window-select)
  (custom-set-faces!
    '(aw-leading-char-face :inherit 'font-lock-builtin-face :height 4.5)))

(when (and (modulep! :ui window-select)
           (modulep! :ui treemacs))
  (after! (:and treemacs ace-window)
    (setq! aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))))

(when (modulep! :ui workspaces)
  (setq! persp-keymap-prefix nil)
  (after! persp-mode
    (defun +workspace/save-current ()
      (interactive)
      (+workspace/save (persp-name (get-current-persp))))
    (map! "C-c w s" #'+workspace/save-current)))
