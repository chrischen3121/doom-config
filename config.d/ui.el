;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/ui.el

(when(modulep! :ui popup)
  (map! :map +popup-buffer-mode-map
        :desc "Raise popup" "C-c C-p" #'+popup/raise))

(when (modulep! :ui doom-dashboard)
  (setq! +doom-dashboard-name "Happy Hacking!"))
