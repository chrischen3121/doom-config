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
         (expand-file-name "en.pws" cc/personal-dictionary-dir))
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

;; undo
(map! :after undo-fu
      :prefix ("C-c u" . "undo")
      "C-c u u" #'undo-fu-only-undo
      "C-c u r" #'undo-fu-only-redo
      "C-c u a" #'undo-fu-only-redo-all)

;; :term
;; vterm
;; Install vterm-module by `M-x vterm-module-compile`
;; C-c o t/T Open vterm

;; TODO: next
;; Global which-key
(which-key-add-key-based-replacements "C-c m" "mode-commands")



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
