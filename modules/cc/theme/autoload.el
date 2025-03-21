;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/theme/autoload.el

(autoload 'ring-insert "ring" nil t)

(defvar dark-theme-p nil
  "Whether the current theme is dark.")

;;;###autoload
(defun switch-to-light-theme ()
  "Switch to light theme"
  (when (eq doom-theme cc/dark-theme)
    (load-theme cc/light-theme t)))

;;;###autoload
(defun switch-to-dark-theme ()
  "Switch to dark theme"
  (when (eq doom-theme cc/light-theme)
    (load-theme cc/dark-theme t)))

;;;###autoload
(defun set-theme-based-on-time ()
  "Set theme based on time of day"
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (< hour 6) (> hour 19))
        (switch-to-dark-theme)
      (switch-to-light-theme))))

;;;###autoload
(defun set-theme-based-on-sys-style ()
  "Set theme based on system style"
  (let ((gnome-style (shell-command-to-string
                      "gsettings get org.gnome.desktop.interface gtk-theme")))
    (if (string-match-p "dark" gnome-style)
        (switch-to-dark-theme)
      (switch-to-light-theme))))

;;;###autoload
(defun cc/switch-light-dark-theme ()
  "Switch light/dark themes"
  (interactive)
  (if (eq doom-theme cc/dark-theme)
      (switch-to-light-theme)
    (switch-to-dark-theme)))
