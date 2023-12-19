;;; core/cc-better-defaults.el -*- lexical-binding: t; -*-
;; TODO: May be try to use Hydra
;; TODO: May be define function cc/kill-and-del-other-window

;; :app
;; +calendar
(when (modulep! :app calendar)
  (setq! calendar-week-start-day 1)
  (which-key-add-key-based-replacements "C-c o c" "Calendar")
  (map! "C-c o c" #'+calendar/open-calendar))

;; :checkers
;; langtool for grammer check
(when (modulep! :checkers grammar)
  (which-key-add-key-based-replacements "C-c ! l" "langtool")
  (map! "C-c ! l !" #'langtool-check
        "C-c ! l c" #'langtool-correct-buffer))

;; (add-hook! spell-fu-mode-hook
;;   (spell-fu-dictionary-add
;;    (spell-fu-get-personal-dictionary "en-personal" cc/spell-personal-dictionary)))

;; (after! spell-fu
;;   (defface my-spell-fu-incorrect-face
;;     '((t :underline (:color "LightBlue" :style wave)))
;;     "Face for spell-fu incorrect words."
;;     :group 'spell-fu)
;;   (setq! spell-fu-idle-delay 0.5)
;;   (which-key-add-key-based-replacements "C-c ! s" "spell")
;;   (map! :map mode-specific-map
;;         "! s n" #'spell-fu-goto-next-error
;;         "! s p" #'spell-fu-goto-previous-error
;;         "! s c" #'+spell/correct
;;         "! s a" #'+spell/add-word
;;         "! s r" #'+spell/remove-word))

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
