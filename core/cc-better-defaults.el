;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Global keybindings
(map! "C-z" nil)

(add-hook! 'doom-after-init-hook
           ;; Disable "continue comments" functionality
           (advice-remove 'newline-and-indent
                          '+default--newline-indent-and-continue-comments-a))

;; :app
;; +calendar
(when (modulep! :app calendar)
  (setq! calendar-week-start-day 1)
  (map! :leader
        :prefix "o"
        :desc "Calendar"
        "c" #'+calendar/open-calendar))

;; :checkers
;; syntax
(when (modulep! :checkers syntax)
  (after! flycheck
    (setq! flycheck-keymap-prefix (kbd "C-c 1"))
    (map! :prefix ("C-c 1" . "checkers"))))

;; :checkers
;; +grammar
(when (modulep! :checkers grammar)
  (map!
   :map (text-mode-map org-mode-map)
   :prefix ("C-c 1 g" . "grammar")
   :desc "Check buffer" "c" #'langtool-check
   :desc "Correct buffer" "C" #'langtool-correct-buffer))


;; :checkers
;; +spell
;; spell-fu
(when (modulep! :checkers spell)
  (add-hook! spell-fu-mode
    (setf
     (alist-get 'prog-mode +spell-excluded-faces-alist)
     '(font-lock-string-face))
    (setq! cc/en-personal-dictionary
           (file-name-concat cc/personal-dictionary-dir "en.pws"))
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary "en" cc/en-personal-dictionary)))

  (after! spell-fu
    (setq! spell-fu-idle-delay 0.5)
    (custom-set-faces!
      '(spell-fu-incorrect-face :underline (:color "cyan" :style wave)))
    (map!
     :map flycheck-mode-map
     :prefix ("C-c 1 s" . "spell-check")
     :desc "Correct word at point" "c" #'+spell/correct
     :desc "Add word at point" "a" #'+spell/add-word
     :desc "Remove word at point" "r" #'+spell/remove-word
     :desc "Goto next error" "n" #'spell-fu-goto-next-error
     :desc "Goto previous error" "p" #'spell-fu-goto-previous-error)))


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
   :localleader
   :prefix ("u" . "undo")
   :desc "Undo" "u" #'undo-fu-only-undo
   :desc "Redo" "r" #'undo-fu-only-redo
   :desc "Redo all" "a" #'undo-fu-only-redo-all))

;; :term
;; vterm
;; Install vterm-module by `M-x vterm-module-compile`
;; C-c o t/T Open vterm


;; :tools
;; docker
;; C-x C-f /docker:$USER@$CONTAINER:/path/to/file


;; :ui
;; workspace
(when (modulep! :ui workspace)
  (after! persp-mode
    (setq! persp-emacsclient-init-frame-behaviour-override "main"))
  (map!
   :map (override-global-map general-override-mode-map)
   :prefix ("C-c w w" . "workspace")
   :desc "Load workspace"
   "l" #'+workspace/load
   :desc "Load last autosaved session"
   "L" #'doom/quickload-session
   :desc "Save workspace"
   "s" #'+workspace/save
   :desc "Save session"
   "S" #'doom/save-session))


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
      #'centaur-tabs-local-mode)
    (map! :prefix ("C-c w t" . "tabs")
          :map centaur-tabs-mode-map
          :desc "Tab forword"
          "f" #'centaur-tabs-forward
          :desc "Tab backward"
          "b" #'centaur-tabs-backward
          :desc "Tab ace jump"
          "j" #'centaur-tabs-ace-jump)))


;; :others
;; Whole line or region
(use-package! whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

;; Rainbow mode: highlight color string
(add-hook! (emacs-lisp-mode html-mode css-mode) #'rainbow-mode)

;; Ace jump mode
;; It can help you to move your cursor to ANY position in emacs
;; by using only 3 times key press.
(use-package! ace-jump-mode
  :config
  (map!
   :localleader
   :prefix ("j" . "jump")
   :desc "Ace jump" "j" #'ace-jump-mode
   :desc "Ace jump backward" "b" #'ace-jump-mode-pop-mark))


;; Change ace-window leading char face
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "#51afef"
      :weight bold
      :height 3.0)))


;; Add description for keybindings
(map! :prefix ("C-x <RET>" . "coding-system")
      :prefix ("C-x a" . "abbrev")
      :prefix ("C-c m" . "Mode commands")
      :prefix ("M-s h" . "highlight")
      :prefix ("C-x 8" . "emoji")
      :prefix ("C-x 8 e" . "emoji"))
