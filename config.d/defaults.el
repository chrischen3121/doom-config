;;; -*- lexical-binding: t; no-byte-compile: t; -*-;;;
;;; config.d/defaults.el

;; Custom widget keybindings
(map! (:map widget-keymap
            "C-M-f" #'widget-forward
            "C-M-b" #'widget-backward)
      (:map widget-field-keymap
            "M-/" #'widget-complete
            "TAB" #'widget-complete))
