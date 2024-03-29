;;; cc/better-defaults/config.el -*- lexical-binding: t; -*-

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Global keybindings
(undefine-key! global-map
  "C-z" "C-x C-z")

;; disable evil leader key
;; error will be thrown if alt-key is nil
(when (not (modulep! :editor evil))
  (setq! doom-leader-alt-key "C-z"
         doom-localleader-alt-key "C-z l"))

;; which-key
(map! :prefix ("C-x <RET>" . "coding-system")
      :prefix ("C-x a" . "abbrev")
      :prefix ("M-s h" . "highlight")
      :prefix ("C-x 8" . "emoji")
      :prefix ("C-x 8 e" . "emoji")
      :prefix ("C-x n" . "narrow/widen")
      :prefix ("C-x r" . "register")
      :prefix ("C-x t" . "tab")
      :prefix ("C-x w" . "win-select")
      :prefix ("C-x x" . "buffer-ops")
      :prefix ("C-x 4" . "other-window")
      :prefix ("C-x 5" . "other-frame")
      :prefix ("C-c o" . "open")
      :prefix ("C-c p" . "project")
      :prefix ("C-c s" . "search")
      :prefix ("C-c t" . "toggle")
      :prefix ("C-c w" . "workspace")
      :prefix ("C-c f" . "file")
      :prefix ("C-h 4" . "info")
      :prefix ("C-c l" . "<cc-local>"))

(when (modulep! :editor snippets)
  (which-key-add-key-based-replacements "C-c &" "snippets"))

;; recentf
(map! :prefix "C-c f"
      :desc "Recent files" "r" #'recentf-open-files)
(after! recentf
  (setq! recentf-max-saved-items 21)
  (add-to-list 'recentf-exclude "autosave"))

;; :app
;; calendar
(when (modulep! :app calendar)
  (after! calendar
    (setq! calendar-week-start-day 1))
  (map! :prefix "C-c o"
        :desc "Calendar"
        "c" #'+calendar/open-calendar))

;; celander

;; :app
;; everywhere
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
    (which-key-add-key-based-replacements "C-c 1" "checker")))


;; :checkers
;; spell
;; spell-fu
(when (modulep! :checkers spell)
  (after! spell-fu
    (setq! spell-fu-idle-delay 0.5)
    (setf
     (alist-get 'prog-mode +spell-excluded-faces-alist)
     '(font-lock-constant-face
       font-lock-string-face)) ; TODO: wait for official fix, should disable spell-fu in string
    (custom-set-faces!
      '(spell-fu-incorrect-face :underline (:color "cyan" :style wave)))
    (map!
     :map general-override-mode-map
     :prefix ("C-c 1 s" . "spell-check")
     :desc "Correct word at point" "c" #'+spell/correct
     :desc "Add word at point" "a" #'+spell/add-word
     :desc "Remove word at point" "r" #'+spell/remove-word
     :desc "Goto next error" "n" #'spell-fu-goto-next-error
     :desc "Goto previous error" "p" #'spell-fu-goto-previous-error)))

;; :checkers
;; spell
;; +hunspell
(when (modulep! :checkers spell +hunspell)
  (setq! ispell-dictionary "en_US"))

;; :checkers
;; grammar
(when (modulep! :checkers grammar)
  (after! langtool
    (map!
     :map text-mode-map
     :prefix "C-c 1"
     :desc "Check grammar" "g" #'langtool-check)))
