;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; mycustom.el

;; cc-ui
(setq! cc/mono-font "Hack"
       cc/font-size 24
       ;; recommended: doom-acario-light, doom-one-light, doom-nord,  doom-oksolar-light
       cc/light-theme 'doom-one-light
       ;; recommended: doom-one, doom-peacock, doom-tomorrow-night, doom-opera
       cc/dark-theme 'doom-tomorrow-night)

;; cc-config
(setq! cc/personal-aspell-en-dict "~/dicts/spell-fu/en.pws")

;; cc-note
(setq! cc/default-org-dir "~/org/"
       cc/org-id-locations "~/org/.orgids"
       cc/notes-base-dir "~/org/notes/"
       cc/org-agenda-dir "~/org/todos/")

;; cc-dev
(setq! cc/copilot-chat-model "gpt-4o")
