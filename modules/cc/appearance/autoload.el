;;; cc/appearance/autoload.el -*- lexical-binding: t; -*-

(defvar cc/default-font "Hack" "Adjust default font for your system.")
(defvar cc/default-font-size 24)
(defvar cc/default-unicode-font "Noto Sans CJK SC"
  "Adjust unicode font for your system.")
(defvar cc/default-unicode-font-size 24)
(defvar cc/light-theme 'doom-one-light)
(defvar cc/dark-theme 'doom-tomorrow-night)
(defvar cc/theme-dark-p nil
  "Whether the current theme is dark.")

;;;###autoload
(defun cc/switch-to-light-theme ()
  "Switch to light theme"
  (setq! doom-theme cc/light-theme
         cc/theme-dark-p nil)
  (load-theme doom-theme t))

;;;###autoload
(defun cc/switch-to-dark-theme ()
  "Switch to dark theme"
  (setq! doom-theme cc/dark-theme
         cc/theme-dark-p t)
  (load-theme doom-theme t))


;;;###autoload
(defun cc/set-theme-based-on-time ()
  "Set theme based on time of day"
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (< hour 6) (> hour 19))
        (cc/switch-to-dark-theme)
      (cc/switch-to-light-theme))))


;;;###autoload
(defun cc/set-theme-based-on-sys-style ()
  "Set theme based on system style"
  (let ((gnome-style (shell-command-to-string
                      "gsettings get org.gnome.desktop.interface gtk-theme")))
    (if (string-match-p "dark" gnome-style)
        (cc/switch-to-dark-theme)
      (cc/switch-to-light-theme))))


;;;###autoload
(defun cc/switch-light-dark-theme ()
  "Switch light/dark themes"
  (interactive)
  (if cc/theme-dark-p
      (cc/switch-to-light-theme)
    (cc/switch-to-dark-theme)))


;;;###autoload
(defun cc/set-doom-ui-appearance ()
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
  (custom-set-faces!
    ;; ace-window
    '(aw-leading-char-face :inherit 'font-lock-builtin-face :height 4.5)
    ;; spell-fu
    `(spell-fu-incorrect-face :underline (:style wave :color ,(doom-color 'blue)))
    ;; volatile-highlights
    `(vhl/default-face :inherit 'region :background ,(doom-color 'gray)))
  (if (string-equal (getenv "XDG_SESSION_DESKTOP") "gnome")
      (cc/set-theme-based-on-sys-style)
    (cc/set-theme-based-on-time)))
