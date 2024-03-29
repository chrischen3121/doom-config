;;; core/cc-themes.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq! doom-font (font-spec :family "Hack" :size cc/default-font-size)
       doom-variable-pitch-font (font-spec :family "Hack" :size cc/default-unicode-font-size)
       doom-big-font (font-spec :family "Hack" :size (+ cc/default-font-size (/ cc/default-font-size 3)))
       doom-serif-font (font-spec :family "WenQuanyi Micro Hei" :size cc/default-unicode-font-size)
       doom-symbol-font (font-spec :family "WenQuanyi Micro Hei" :size cc/default-unicode-font-size))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.

;; Define default light/dark themes
;; light:
;; doom-acario-light, doom-one-light, doom-nord,  doom-oksolar-light

;; dark:
;; doom-peacock, doom-tomorrow-night, doom-opera

(defvar cc/light-theme 'doom-one-light)
(defvar cc/dark-theme 'doom-tomorrow-night)


;; Set default theme to light if it's daytime, dark if it's night
(defun cc/set-default-theme-by-time ()
  "Set theme based on time of day"
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (< hour 6) (> hour 19))
        (load-theme cc/dark-theme t)
      (load-theme cc/light-theme t))))


;; Set default theme in line with system style
(defun cc/set-default-theme-by-sys-style ()
  "Set theme based on system style"
  (let ((gnome-style (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")))
    (if (string-match-p "dark" gnome-style)
        (load-theme cc/dark-theme t)
      (load-theme cc/light-theme t))))


;; Automatically set default theme
(add-hook! 'doom-init-ui-hook
           ;; Set default theme by system style if it's gnome
           ;; otherwise set by time
           (defun auto-set-default-theme ()
             (interactive)
             (if (string-equal (getenv "XDG_CURRENT_DESKTOP") "GNOME")
                 (cc/set-default-theme-by-sys-style)
               (cc/set-default-theme-by-time))))


;; Switch between light and dark themes
(defun cc/switch-light-dark-theme ()
  "Switch light/dark themes"
  (interactive)
  (if (eq (car custom-enabled-themes) cc/light-theme)
      (progn (disable-theme cc/light-theme)
             (load-theme cc/dark-theme t))
    (disable-theme cc/dark-theme)
    (load-theme cc/light-theme t)))


(map! "<f12>" #'cc/switch-light-dark-theme)
