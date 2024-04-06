;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-




;; :tools
;; docker
;; Hints:
;; Open files in docker container:
;; C-x C-f /docker:$USER@$CONTAINER:/path/to/file


;; :ui
;; workspaces
(when (modulep! :ui workspaces)
  (map!
   :leader
   :prefix ("3" . "workspace")
   :desc "New workspace"
   "n" #'+workspace/new-named
   :desc "Load workspace"
   "l" #'+workspace/load
   :desc "Load last autosaved session"
   "L" #'doom/quickload-session
   :desc "Save workspace"
   "s" #'+workspace/save
   :desc "Save session"
   "S" #'doom/save-session
   :desc "Delete workspace"
   "d" #'+workspace/delete
   :desc "Rename workspace"
   "r" #'+workspace/rename
   :desc "Switch workspace"
   "w" #'+workspace/switch-to
   :desc "Swap left"
   "<left>" #'+workspace/swap-left
   :desc "Swap right"
   "<right>" #'+workspace/swap-right
   :desc "Display workspaces"
   "d" #'+workspace/display))


;; :ui
;; treemacs
;; enable project follow mode
(when (modulep! :ui treemacs)
  (after! treemacs
    (setq! treemacs-project-follow-mode t)))


;; :ui
;; tabs
(when (modulep! :ui tabs)
  (after! centaur-tabs
    (setq! centaur-tabs-style "wave"
           centaur-tabs-set-bar nil
           centaur-tabs-height 36
           centaur-tabs-close-button "x")
    (add-hook!
      (dired-mode special-mode vterm-mode)
      :append
      ;; maybe :local
      #'centaur-tabs-local-mode)
    (map! :leader
          :prefix ("4" . "tabs")
          :map centaur-tabs-mode-map
          :desc "Tab forward"
          "f" #'centaur-tabs-forward
          :desc "Tab backward"
          "b" #'centaur-tabs-backward
          :desc "Switch to tab"
          "j" #'centaur-tabs-ace-jump
          :desc "Tab close"
          "c" #'centaur-tabs-close-tab)))
