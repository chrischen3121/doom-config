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
;; langtool for grammar check
(when (modulep! :checkers grammar)
  (which-key-add-key-based-replacements "C-c ! l" "langtool")
  (map! :map mode-specific-map
        "! l !" #'langtool-check
        "! l c" #'langtool-correct-buffer))

;; spell-fu
(after! spell-fu
  (custom-set-faces!
    '(spell-fu-incorrect-face :underline (:color "cyan" :style wave)))
  (setq! spell-fu-idle-delay 0.5
         spell-fu-faces-exclude '(font-lock-string-face)) ; TODO: not working
  (map! :map mode-specific-map
        "! s n" #'spell-fu-goto-next-error
        "! s p" #'spell-fu-goto-previous-error
        "! s c" #'+spell/correct
        "! s a" #'+spell/add-word
        "! s r" #'+spell/remove-word)

  ;; add personal dictionaries
  (setq! cc/en-personal-dictionary
         (expand-file-name "en.pws" cc/personal-dictionary-dir)
         cc/elisp-personal-dictionary
         (expand-file-name "elisp.pws" cc/personal-dictionary-dir)
         cc/python-personal-dictionary
         (expand-file-name "python.pws" cc/personal-dictionary-dir)
         cc/cpp-personal-dictionary
         (expand-file-name "cpp.pws" cc/personal-dictionary-dir))

  ;; TODO: the original dictionary disappear for text-mode (org and txt)
  (add-hook! text-mode
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary
      "en" cc/en-personal-dictionary)))
  (add-hook! emacs-lisp-mode
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary
      "elisp" cc/elisp-personal-dictionary)))
  (add-hook! python-mode
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary
      "python" cc/python-personal-dictionary)))
  (add-hook! c++-mode
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary
      "cpp" cc/cpp-personal-dictionary))))


;; TODO: next
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
