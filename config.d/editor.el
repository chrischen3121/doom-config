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

(when (modulep! :editor multiple-cursors)
  (map! :map prog-mode-map
        :desc "Mark previous line like this" "C-<left>"
        #'mc/mark-previous-like-this
        :desc "Mark next line like this" "C-<right>"
        #'mc/mark-next-like-this
        :map multiple-cursors-mode-map
        "<return>" nil))

(when (modulep! :editor snippets)
  (map! :map yas-minor-mode-map
        "C-c &" nil
        "C-M-/" #'yas-expand
        :desc "Reload snippets" "C-c y r" #'yas-reload-all
        :desc "Insert snippet" "C-c y i" #'yas-insert-snippet))

(when (modulep! :editor word-wrap)
  (+global-word-wrap-mode +1)
  (when (modulep! :term vterm)
    (add-to-list '+word-wrap-disabled-modes 'vterm-mode)))
