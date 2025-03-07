;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/editor.el

(when (modulep! :editor fold)
  (map! :map (prog-mode-map
              yaml-mode-map
              org-mode-map)
        :prefix ("C-c <TAB>" . "<fold>")
        :desc "Fold/Unfold" "<TAB>" #'+fold/toggle
        :desc "Fold all" "f" #'hs-hide-all
        :desc "Unfold all" "u" #'hs-show-all))
