;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/theme/config.el

(setq! doom-font (font-spec :family cc/mono-font :size cc/mono-font-size)
       doom-big-font
       (font-spec :family cc/mono-font
                  :size (+ cc/mono-font-size (/ cc/mono-font-size 3)))
       doom-theme cc/light-theme)

(if (string-equal (getenv "XDG_SESSION_DESKTOP") "gnome")
    (set-theme-based-on-sys-style)
  (set-theme-based-on-time))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(map! "<f12>" #'cc/switch-light-dark-theme)
