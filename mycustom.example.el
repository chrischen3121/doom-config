;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; mycustom.el

;; cc-ui
(setq! cc/mono-font "Hack"
       cc/unicode-font "Source Han Sans CN"
       cc/emoji-font "Noto Color Emoji"
       cc/font-size 24
       ;; recommended: doom-one-light, doom-acario-light(cold), doom-oksolar-light(warm)
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
(setq! cc/copilot-chat-model "claude-3.7-sonnet"
       cc/cpp-default-tab-width 2)

;; cc-ai
(setq! cc/openai-key ""
       cc/anthropic-key ""
       cc/gemini-key ""
       cc/deepseek-key "")
