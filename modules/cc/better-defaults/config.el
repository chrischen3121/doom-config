;; -*- no-byte-compile: t; -*-
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

;; global keybindings
(map! :desc "Redo" "C-c r" #'undo-fu-only-redo
      :desc "Redo all" "C-c R" #'undo-fu-only-redo-all
      :prefix ("C-x <RET>" . "coding-system")
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
      :prefix ("C-c s" . "search")
      :prefix ("C-c n" . "notes")
      :desc "Browse notes" "n" #'+default/browse-notes

      :prefix ("C-c t" . "toggle")
      :prefix ("C-c w" . "workspace")
      :prefix ("C-c c" . "code")

      ;; C-c f +file
      :prefix ("C-c f" . "file")
      :desc "Copy this file" "c" #'doom/copy-this-file
      :desc "Delete this file" "D" #'doom/delete-this-file
      :desc "Move this file" "m" #'doom/move-this-file
      :desc "Find file in private config" "p"
      #'doom/find-file-in-private-config
      :desc "Browse private config" "b" #'doom/open-private-config
      :desc "Sudo this file" "s" #'doom/sudo-this-file
      :desc "Sudo find file" "S" #'doom/sudo-find-file
      :desc "Copy file path" "y" #'+default/yank-buffer-path
      :desc "Open scratch buffer" "x" #'doom/open-scratch-buffer

      :prefix ("C-h 4" . "info")
      :prefix ("C-c l" . "<cc-local>"))

(after! projectile
  (map! :map projectile-mode-map
        :prefix ("C-c p" . "project")
        :desc "Recent project files" "r" #'projectile-recentf
        :desc "Replace in project" "R" #'projectile-replace
        :desc "Search project" "s" #'+default/search-project
        :desc "List todos" "t" #'magit-todos-list
        :desc "Find file" "f" #'projectile-find-file
        :desc "Search symbol" "." #'+default/search-project-for-symbol-at-point
        :desc "Browse project" "D" #'+default/browse-project
        :prefix ("C-c p 4" . "other-window")
        :prefix ("C-c p 5" . "other-frame")
        :prefix ("C-c p x" . "run")
        :prefix ("C-c p x 4" . "run other-window")))

(when (modulep! :editor snippets)
  (map! :prefix ("C-c &" . "snippets")
        :desc "New snippet" "n" #'+snippets/new
        :desc "Edit snippet" "e" #'+snippets/edit
        :desc "Find snippet" "f" #'+snippets/find
        :desc "Browse snippets" "b" #'+default/browse-templates)
  (after! yasnippet
    (undefine-key! yas-minor-mode-map
      "C-c & C-n" "C-c & C-v" "C-c & C-s")
    (map! :map yas-minor-mode-map
          :prefix "C-c &"
          :desc "Reload snippets" "r" #'yas-reload-all
          :desc "Insert snippet" "i" #'yas-insert-snippet)))

;; recentf
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

;; :completion
;; vertico
(when (modulep! :completion vertico)
  (map! :prefix "C-c s"
        :desc "Search Project" "p" #'+default/search-project
        :prefix "C-c f"
        :desc "Locate file" "l" #'consult-locate
        :desc "Recent files" "r" #'consult-recent-file)
  (map! :map vertico-map
        "C-l" #'vertico-directory-delete-word))


;; :editor
;; word-wrap
(when (modulep! :editor word-wrap)
  (+global-word-wrap-mode +1))


;; :emacs
;; dired
(when (modulep! :emacs dired)
  (map! :after dired
        :map dired-mode-map
        "C-l" #'dired-up-directory)
  (map! :map dired-mode-map
        "C-c C-r" nil
        "C-c C-e" nil
        :prefix "C-c l"
        :desc "Rsync" "r" #'dired-rsync
        :desc "Edit mode" "e" #'wdired-change-to-wdired-mode))

