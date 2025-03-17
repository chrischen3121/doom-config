;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-new/defaults/config.el

;; TODO Change newline behavior
;; (add-hook! 'doom-first-buffer-hook
;;   (defun cc/change-newline-behavior ()
;;     (advice-remove 'newline-and-indent
;;                    '+default--newline-indent-and-continue-comments-a)))

;; recentf
(after! recentf
  (setq! recentf-max-saved-items 21)
  (add-to-list 'recentf-exclude "autosave"))

;; make file executable if it has shebang
(add-hook! 'after-save-hook
           #'executable-make-buffer-file-executable-if-script-p)

(use-package! whole-line-or-region
  :hook (doom-first-input . whole-line-or-region-global-mode))

;; Ace jump mode
;; It can help you to move your cursor to ANY position in emacs
;; by using only 3 times key press.
(use-package! ace-jump-mode
  :commands ace-jump-mode
  :init
  (map! :desc "Ace jump" "C-c j" #'ace-jump-mode))
