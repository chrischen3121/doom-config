;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-
;; TODO: May be try to use Hydra
;; TODO: May be define function cc/kill-and-del-other-window

;; :app
;; +calendar
(when (modulep! :app calendar)
  (setq! calendar-week-start-day 1)
  (map! :prefix "C-c o"
        :desc "Calendar"
        "c" #'+calendar/open-calendar))


;; :checkers
;; +grammar
(when (modulep! :checkers grammar)
  (map!
   :map (text-mode-map org-mode-map)
   :prefix ("C-c ! g" . "grammar")
   :desc "Check buffer" "c" #'langtool-check
   :desc "Correct buffer" "C" #'langtool-correct-buffer))


;; +spell
(when (modulep! :checkers spell)
  (setf
   (alist-get 'prog-mode +spell-excluded-faces-alist)
   '(font-lock-string-face))
  (setq! cc/en-personal-dictionary
         (file-name-concat cc/personal-dictionary-dir "en.pws"))
  (add-hook! spell-fu-mode
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary "en" cc/en-personal-dictionary)))
  ;; spell-fu
  (after! spell-fu
    (setq! spell-fu-idle-delay 0.5)
    (custom-set-faces!
      '(spell-fu-incorrect-face :underline (:color "cyan" :style wave)))
    (map!
     :prefix ("C-c ! s" . "spell")
     :desc "Correct word at point" "c" #'+spell/correct
     :desc "Add word at point" "a" #'+spell/add-word
     :desc "Remove word at point" "r" #'+spell/remove-word
     :desc "Goto next error" "n" #'spell-fu-goto-next-error
     :desc "Goto previous error" "p" #'spell-fu-goto-previous-error)))


;; :emacs
;; dired
;; C-c C-r Rsync to ...
;; C-c C-e Rename entries
(map! :after dired
      :map dired-mode-map
      "C-l" #'dired-up-directory)

;; :emacs
(map! :after vertico
      :map vertico-map
      "C-l" #'vertico-directory-delete-char)

;; undo
(map! :after undo-fu
      :prefix ("C-c u" . "undo")
      "u" #'undo-fu-only-undo
      "r" #'undo-fu-only-redo
      "a" #'undo-fu-only-redo-all)

;; :term
;; vterm
;; Install vterm-module by `M-x vterm-module-compile`
;; C-c o t/T Open vterm

;; :tools
;; docker
;; C-x C-f /docker:$USER@$CONTAINER:/path/to/file

;; workspace
(when (modulep! :ui workspace)
  (after! persp-mode
    (setq! persp-emacsclient-init-frame-behaviour-override "main"))
  )
(map!
 :map general-override-mode-map
 :desc "Load workspace"
 "C-c w l" #'+workspace/load
 :desc "Load last autosaved session"
 "C-c w L" #'doom/quickload-session
 :desc "Save workspace"
 "C-c w s" #'+workspace/save
 :desc "Save session"
 "C-c w S" #'doom/save-session)

;; treemacs
;; enable project follow mode
(after! treemacs
  (setq! treemacs-project-follow-mode t))

;; tabs
(when (modulep! :ui tabs)
  (after! centaur-tabs
    (setq! centaur-tabs-style "wave"
           centaur-tabs-set-bar nil
           centaur-tabs-height 36
           centaur-tabs-close-button "x")
    (add-hook! (dired-mode special-mode) #'centaur-tabs-local-mode)
    (map! :prefix ("C-x t" . "tabs")
          :map centaur-tabs-mode-map
          :desc "Tab forword" "f" #'centaur-tabs-forward
          :desc "Tab backward" "b" #'centaur-tabs-backward
          :desc "Tab ace jump" "j" #'centaur-tabs-ace-jump)))


;; :others
;; Whole line or region
(use-package! whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

(add-hook! (emacs-lisp-mode html-mode css-mode) #'rainbow-mode)

;; Ace jump mode
;; It can help you to move your cursor to ANY position in emacs
;; by using only 3 times key press.
(use-package! ace-jump-mode
  :init
  (map!
   :map general-override-mode-map
   :desc "Ace jump"
   "C-c j" #'ace-jump-mode
   :desc "Ace jump backward"
   "C-c J" #'ace-jump-mode-pop-mark))


;; Global keybindings
(add-hook! 'doom-after-init-hook
  (map! "C-z" nil ; unbind suspend-frame
        "S-<SPC>" #'set-mark-command))

;; Change ace-window leading char face
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "#51afef"
      :weight bold
      :height 3.0)))

;; Global which-key
(after! which-key
  (which-key-add-key-based-replacements "C-x <RET>" "coding-system")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-c m" "mode-cmds")
  (which-key-add-key-based-replacements "M-s h" "highlight")
  (which-key-add-key-based-replacements "C-x 8" "emoji")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji"))
