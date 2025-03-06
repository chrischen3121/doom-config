;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/ui.el

(when(modulep! :ui popup)
  (map! :map +popup-buffer-mode-map
        :desc "Raise popup" "C-c C-p" #'+popup/raise))

(when (modulep! :ui doom-dashboard)
  (setq! +doom-dashboard-name "Happy Hacking!"))

(when (modulep! :ui treemacs)
  ;; TODO check on follow mode
  (map! :map treemacs-mode-map
        :desc "Select window" "C-x o" #'treemacs-select-window
        (:prefix
         ("p" . "<tree-project>")
         "p" #'treemacs-projectile)
        (:prefix
         ("w" . "<tree-workspace>")
         "w" #'treemacs-switch-workspace))
  (setq! +treemacs-git-mode 'deferred))

(when (modulep! :ui window-select)
  (custom-set-faces!
    '(aw-leading-char-face :inherit 'font-lock-builtin-face :height 4.5)))

(when (and (modulep! :ui window-select)
           (modulep! :ui treemacs))
  (after! (:and treemacs ace-window)
    (setq! aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))))
