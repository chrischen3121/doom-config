;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-
;; TODO: May be try to use Hydra
;; TODO: May be define function cc/kill-and-del-other-window

;; Fullscreen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(advice-remove 'newline-and-indent '+default--newline-indent-and-continue-comments-a)

;; Global which-key
(which-key-add-key-based-replacements "C-c m" "modmap")

;; Dired
(map! :after dired
      :map dired-mode-map
      "C-l" #'dired-up-directory)


;; Change ace-window leading char face
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face :foreground "#51afef" :weight bold :height 3.0)))

;; Whole line or region
(use-package! whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

;; Ace jump mode
;; It can help you to move your cursor to ANY position in emacs
;; by using only 3 times key press.
(use-package! ace-jump-mode
  :bind
  (:map mode-specific-map
        ("j" . ace-jump-mode)
        ("J" . ace-jump-mode-pop-mark)))


;; Global keybindings
(map! "C-z" nil ; unbind suspend-frame
      "S-<SPC>" #'set-mark-command
      )

(map! :after vertico
      :map vertico-map
      "C-l" #'vertico-directory-delete-char)
