;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/theme/config.el

(setq! doom-font (font-spec :family cc/mono-font :size cc/font-size)
       doom-symbol-font (font-spec :family cc/unicode-font :size cc/font-size)
       doom-emoji-font (font-spec :family cc/emoji-font :size cc/font-size)
       ;; doom-variable-pitch-font (font-spec :family cc/unicode-font :size cc/font-size)
       doom-big-font-increment (+ cc/font-size (/ cc/font-size 3))
       doom-theme cc/light-theme)

(if (string-equal (getenv "XDG_SESSION_DESKTOP") "gnome")
    (set-theme-based-on-sys-style)
  (set-theme-based-on-time))

(map! "<f12>" #'cc/switch-light-dark-theme)
