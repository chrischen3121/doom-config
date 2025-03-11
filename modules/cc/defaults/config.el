;; -*- no-byte-compile: t; -*-
;;; cc/defaults/config.el -*- lexical-binding: t; -*-



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
  (map! "C-j" #'ace-jump-mode))
