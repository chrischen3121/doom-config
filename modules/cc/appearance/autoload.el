;;; cc/appearance/autoload.el -*- lexical-binding: t; -*-


;; Set default theme to light if it's daytime, dark if it's night
;;;###autoload
(defun cc/set-default-theme-by-time ()
  "Set theme based on time of day"
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (< hour 6) (> hour 19))
        (load-theme cc/dark-theme t)
      (load-theme cc/light-theme t))))


;; Set default theme in line with system style
;;;###autoload
(defun cc/set-default-theme-by-sys-style ()
  "Set theme based on system style"
  (let ((gnome-style (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme")))
    (if (string-match-p "dark" gnome-style)
        (load-theme cc/dark-theme t)
      (load-theme cc/light-theme t))))

;; Switch between light and dark themes
;;;###autoload
(defun cc/switch-light-dark-theme ()
  "Switch light/dark themes"
  (interactive)
  (if (eq (car custom-enabled-themes) cc/light-theme)
      (progn (disable-theme cc/light-theme)
             (load-theme cc/dark-theme t))
    (disable-theme cc/dark-theme)
    (load-theme cc/light-theme t)))
