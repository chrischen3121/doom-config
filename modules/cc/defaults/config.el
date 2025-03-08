;; -*- no-byte-compile: t; -*-
;;; cc/defaults/config.el -*- lexical-binding: t; -*-

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

;; TODO not working
(add-hook! 'after-save-hook
           ;; make file executable if it has shebang
           #'executable-make-buffer-file-executable-if-script-p)

;; projectile keybindings
(after! projectile
  (define-key projectile-mode-map (kbd "C-c M-p") 'projectile-command-map)
  (after! which-key
    (which-key-add-key-based-replacements
      "C-c M-p" "<projectile>"
      "C-c M-p 4" "other-window"
      "C-c M-p 5" "other-frame"
      "C-c M-p x" "execute"
      "C-c M-p s" "search")))

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
       font-lock-string-face))
    (setq! spell-fu-ignore-modes
           '(emacs-lisp-mode prog-mode python-mode c++-mode))
    ;; TODO: wait for official fix, should disable spell-fu in string

    (map!
     :map general-override-mode-map
     :prefix ("C-c 1 s" . "<spell-check>")
     :desc "Correct word at point" "c" #'+spell/correct
     :desc "Add word at point" "a" #'+spell/add-word
     :desc "Remove word at point" "k" #'+spell/remove-word
     :desc "Reset word cache" "r" #'spell-fu-reset
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
  (map! "C-." #'ace-jump-mode
        "C-c j" #'ace-jump-mode))
