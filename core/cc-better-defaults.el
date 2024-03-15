;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Global keybindings
(map! "C-z" nil
      "C-x C-z" nil)

(after! which-key
  ;; Add descriptions for keybindings
  (map! :prefix ("C-c g" . "<cc-global>")
        :prefix ("C-c L" . "<cc-less-used>")
        :prefix ("C-c ;" . "<cc-local>")
        :prefix ("C-x <RET>" . "coding-system")
        :prefix ("C-x a" . "abbrev")
        :prefix ("M-s h" . "highlight")
        :prefix ("C-x 8" . "emoji")
        :prefix ("C-x 8 e" . "emoji")))


;; Global configuration
(add-hook! 'doom-after-init-hook
           ;; Disable "continue comments" functionality
           (defun disable-continue-comments ()
             (advice-remove 'newline-and-indent
                            '+default--newline-indent-and-continue-comments-a))
           ;; for Github Copilot compatibility
           (setq! whitespace-style (delq 'newline-mark whitespace-style)))

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
    (which-key-add-key-based-replacements "C-c 1" "checker")))

;; :checkers
;; +grammar
(when (modulep! :checkers grammar)
  (map!
   :map text-mode-map
   :leader
   :prefix ("1 g" . "grammar")
   :desc "Check buffer" "c" #'langtool-check
   :desc "Correct buffer" "f" #'langtool-correct-buffer))


;; :checkers
;; +spell
;; spell-fu
(when (modulep! :checkers spell)
  (setq! cc/en-personal-dictionary
         (file-name-concat cc/personal-dictionary-dir "en.pws")
         ispell-dictionary "en")

  (add-hook! spell-fu-mode
    (defun add-personal-dictionary ()
      (spell-fu-dictionary-add
       (spell-fu-get-personal-dictionary "en" cc/en-personal-dictionary))))
  (after! spell-fu
    (setq! spell-fu-idle-delay 0.5)
    (setf
     (alist-get 'prog-mode +spell-excluded-faces-alist)
     '(font-lock-constant-face
       font-lock-string-face)) ; TODO: wait for official fix
    (custom-set-faces!
      '(spell-fu-incorrect-face :underline (:color "cyan" :style wave)))
    (map!
     :map overriding-local-map
     :leader
     :prefix ("1 s" . "spell-check")
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
