;; -*- no-byte-compile: t; -*-
;;; cc/better-defaults/config.el -*- lexical-binding: t; -*-

(defvar cc/personal-aspell-dict-dir "~/dicts/"
  "Personal aspell dictionary directory.")

;; Change newline behavior
(add-hook! 'doom-first-buffer-hook
  (defun cc/change-newline-behavior ()
    (advice-remove 'newline-and-indent
                   '+default--newline-indent-and-continue-comments-a)))

;; recentf
(after! recentf
  (setq! recentf-max-saved-items 21)
  (add-to-list 'recentf-exclude "autosave"))

;; Frame related


;; make file executable if it has shebang
(add-hook! 'after-save-hook
           #'executable-make-buffer-file-executable-if-script-p)


;; projectile keybindings
(after! projectile
  ;; TODO: need to combine the useful keybindings to "C-c p"
  (define-key projectile-mode-map (kbd "C-c l p") 'projectile-command-map)
  (after! which-key
    (which-key-add-key-based-replacements
      "C-c l p" "<projectile>"
      "C-c l p 4" "other-window"
      "C-c l p 5" "other-frame"
      "C-c l p x" "execute"
      "C-c l p s" "search"))
  (map! :map projectile-mode-map
        :prefix ("C-c p" . "<project>")
        :desc "Recent project files" "r" #'projectile-recentf
        :desc "Replace in project" "R" #'projectile-replace
        :desc "Search project" "s" #'+default/search-project
        :desc "List todos" "T" #'magit-todos-list
        :desc "Find file" "f" #'projectile-find-file
        :desc "Search symbol" "." #'+default/search-project-for-symbol-at-point
        :desc "Project dired" "D" #'+default/browse-project))

;; :app
;; calendar
(when (modulep! :app calendar)
  (after! calendar
    (setq! calendar-week-start-day 1)))

;; :app
;; everywhere
;; TODO: may be used to edit anki cards
(when (modulep! :app everywhere)
  (after! recentf
    (add-to-list 'recentf-exclude "emacs-everywhere"))
  (after! emacs-everywhere
    (setq! emacs-everywhere-major-mode-function #'org-mode)))

;; :checkers
;; syntax
(when (modulep! :checkers syntax)
  (after! flycheck
    (setq! flycheck-keymap-prefix (kbd "C-c 1"))
    (which-key-add-key-based-replacements "C-c 1" "<checker>")))

;; :checkers
;; spell
;; spell-fu
(when (modulep! :checkers spell)
  (after! spell-fu
    (setq! spell-fu-idle-delay 0.5
           cc/en-personal-dictionary
           (file-name-concat cc/personal-aspell-dict-dir "en.pws")
           ispell-dictionary "en")
    (setf
     (alist-get 'prog-mode +spell-excluded-faces-alist)
     '(font-lock-constant-face
       font-lock-string-face)) ; TODO: wait for official fix, should disable spell-fu in string
    (map!
     :map general-override-mode-map
     :prefix ("C-c 1 s" . "<spell-check>")
     :desc "Correct word at point" "c" #'+spell/correct
     :desc "Add word at point" "a" #'+spell/add-word
     :desc "Remove word at point" "r" #'+spell/remove-word
     :desc "Goto next error" "n" #'spell-fu-goto-next-error
     :desc "Goto previous error" "p" #'spell-fu-goto-previous-error)
    (add-hook! 'spell-fu-mode-hook
      (defun add-personal-dictionary ()
        (spell-fu-dictionary-add
         (spell-fu-get-personal-dictionary "en" cc/en-personal-dictionary))))))

;; :checkers
(when (modulep! :checkers grammar)
  (after! langtool
    (map!
     :map text-mode-map
     :prefix "C-c 1"
     :desc "Check grammar" "g" #'langtool-check)))

;; :completion
;; vertico
(when (modulep! :completion vertico)
;;;###package vertico
  (map! :map vertico-map
        "C-l" #'vertico-directory-delete-word))


;; :editor
;; word-wrap
(when (modulep! :editor word-wrap)
  (+global-word-wrap-mode +1)
  (after! word-wrap
    (add-to-list '+word-wrap-disabled-modes 'vterm-mode)))


;; :emacs
;; dired
(when (modulep! :emacs dired)
  (map! :after dired
        :map dired-mode-map
        "C-l" #'dired-up-directory
        "C-c C-r" nil
        "C-c C-e" nil
        (:prefix "C-c l"
         :desc "Rsync" "r" #'dired-rsync
         :desc "Edit mode" "e" #'wdired-change-to-wdired-mode
         (:when (modulep! :tools upload)
           :desc "SSH Upload" "u" #'ssh-deploy-upload-handler))))

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
    (map! :map centaur-tabs-mode-map
          :prefix ("C-c w t" . "tabs")
          :desc "Tab forward" "f" #'centaur-tabs-forward
          :desc "Tab backward" "b" #'centaur-tabs-backward
          :desc "Switch to tab" "j" #'centaur-tabs-ace-jump
          :desc "Tab close" "c" #'centaur-tabs-close-tab)))


;; :ui
;; zen
(when (modulep! :ui zen)
  (after! writeroom-mode
    (setq! +zen-text-scale 0.8)
    (add-hook! 'writeroom-mode-enable-hook
      (display-line-numbers-mode -1))
    (add-hook! 'writeroom-mode-disable-hook
      (display-line-numbers-mode +1))
    (when (modulep! :ui tabs)
      (add-hook! 'writeroom-mode-enable-hook
        (centaur-tabs-local-mode +1)
        (display-line-numbers-mode -1))
      (add-hook! 'writeroom-mode-disable-hook
        (centaur-tabs-local-mode -1)
        (display-line-numbers-mode +1)))))

;; [Packages]
;; Whole line or region
(use-package! whole-line-or-region
  :hook (doom-first-input . whole-line-or-region-global-mode))

;; Ace jump mode
;; It can help you to move your cursor to ANY position in emacs
;; by using only 3 times key press.
(use-package! ace-jump-mode
  :commands ace-jump-mode
  :init
  (map! "C-." #'ace-jump-mode))
