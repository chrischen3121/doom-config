;;; cc/appearance/autoload.el -*- lexical-binding: t; -*-

(defvar cc/default-font "Hack" "Adjust default font for your system.")
(defvar cc/default-font-size 24)
(defvar cc/default-unicode-font "WenQuanYi Micro Hei"
  "Adjust unicode font for your system.")
(defvar cc/default-unicode-font-size 24)
(defvar cc/light-theme 'doom-one-light)
(defvar cc/dark-theme 'doom-tomorrow-night)


;;;###autoload
(defun cc/set-theme-based-on-time ()
  "Set theme based on time of day"
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (< hour 6) (> hour 19))
        (setq! doom-theme cc/dark-theme)
      (setq! doom-theme cc/light-theme))))


;;;###autoload
(defun cc/set-theme-based-on-sys-style ()
  "Set theme based on system style"
  (let ((gnome-style (shell-command-to-string
                      "gsettings get org.gnome.desktop.interface gtk-theme")))
    (if (string-match-p "dark" gnome-style)
        (setq! doom-theme cc/dark-theme)
      (setq! doom-theme cc/light-theme))))


;;;###autoload
(defun cc/switch-light-dark-theme ()
  "Switch light/dark themes"
  (interactive)
  (if (eq doom-theme cc/light-theme)
      (load-theme cc/dark-theme t)
    (load-theme cc/light-theme t)))

;;;###autoload
(defun cc/update-doom-ui-appearance ()
  "Set up the fonts and themes for the current system."
  (setq! doom-font
         (font-spec :family cc/default-font :size cc/default-font-size)
         doom-variable-pitch-font
         (font-spec :family cc/default-font :size cc/default-unicode-font-size)
         doom-big-font
         (font-spec :family cc/default-font
                    :size (+ cc/default-font-size (/ cc/default-font-size 3)))
         doom-serif-font
         (font-spec :family cc/default-unicode-font :size cc/default-unicode-font-size)
         doom-symbol-font
         (font-spec :family cc/default-unicode-font :size cc/default-unicode-font-size))
  (if (string-equal (getenv "XDG_SESSION_TYPE") "x11")
      (cc/set-theme-based-on-sys-style)
    (cc/set-theme-based-on-time)))
