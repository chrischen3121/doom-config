;;; cc/better-defaults/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
      :prefix ("C-c o" . "open")
      :prefix ("C-c l" . "<cc-local>"))

(when (modulep! :editor snippets)
  (which-key-add-key-based-replacements "C-c &" "snippets"))


;; :app
;; +calendar
(when (modulep! :app calendar)
  (after! calendar
    (setq! calendar-week-start-day 1))
  (map! :prefix "C-c o"
        :desc "Calendar"
        "c" #'+calendar/open-calendar))

;; :checkers
;; syntax
(when (modulep! :checkers syntax)
  (after! flycheck
    (setq! flycheck-keymap-prefix (kbd "C-c 1"))
    (which-key-add-key-based-replacements "C-c 1" "checker")))

;; :checkers
;; +grammar
(when (modulep! :checkers grammar)
  (after! langtool
    (map!
     :map text-mode-map
     :prefix "C-c 1"
     :desc "Check grammar" "g" #'langtool-check)))
