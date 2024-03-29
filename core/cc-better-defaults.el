;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-


;; Global configuration
(add-hook! 'doom-after-init-hook
           ;; Disable "continue comments" functionality
           (defun disable-continue-comments ()
             (advice-remove 'newline-and-indent
                            '+default--newline-indent-and-continue-comments-a))
           ;; for Github Copilot compatibility
           (setq! whitespace-style (delq 'newline-mark whitespace-style)))




;; :emacs
;; dired
;; C-c C-r Rsync to ...
;; C-c C-e Rename entries
(when (modulep! :emacs dired)
  (map! :after dired
        :map dired-mode-map
        "C-l" #'dired-up-directory))


;; :completion
;; vertico
(when (modulep! :completion vertico)
  (map! :after vertico
        :map vertico-map
        "C-l" #'vertico-directory-delete-char))


;; :emacs
;; undo
(when (modulep! :emacs undo)
  (map!
   :map overriding-local-map
   :leader
   :prefix ("; u" . "undo")
   :desc "Undo" "u" #'undo-fu-only-undo
   :desc "Redo" "r" #'undo-fu-only-redo
   :desc "Redo all" "a" #'undo-fu-only-redo-all))


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


;; :others
;; Whole line or region
(use-package! whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

;; Rainbow mode: highlight color string
(add-hook! (emacs-lisp-mode html-mode css-mode)
           #'rainbow-mode)

;; Ace jump mode
;; It can help you to move your cursor to ANY position in emacs
;; by using only 3 times key press.
(use-package! ace-jump-mode
  :commands ace-jump-mode
  :init
  (map!
   :map overriding-local-map
   :leader
   :desc "Ace jump" "j" #'ace-jump-mode
   :desc "Ace jump back" "J" #'ace-jump-mode-pop-mark))


;; Change ace-window leading char face
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "#51afef"
      :weight bold
      :height 5.0)))
