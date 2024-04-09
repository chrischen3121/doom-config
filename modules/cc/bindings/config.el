;; -*- no-byte-compile: t; -*-
;;; cc/bindings/config.el -*- lexical-binding: t; -*-

;; persp-mode and projectile in different prefixes
(when (modulep! :ui workspaces)
  (setq! persp-keymap-prefix (kbd "C-c w")))

(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Unset global keybindings
(undefine-key! global-map
  "C-z"
  "C-x C-z")

(remove-hook! 'doom-first-input-hook #'which-key-mode)
(add-hook! 'doom-first-buffer-hook
           :append
           #'which-key-mode)

;; which-key sort by description
(after! which-key
  (setq! which-key-sort-order 'which-key-description-order))


;; Autoloads
(autoload 'org-capture-goto-target "org-capture" nil t)

;; Global keybindings
(map! :after which-key
      :prefix "C-c"

      ;;; C-c t --- toggle
      (:prefix-map ("t" . "<toggle>")
       :desc "Big font mode" "b" #'doom-big-font-mode
       :desc "Fullscreen" "F" #'toggle-frame-fullscreen
       :desc "Flymake" "f" #'flymake-mode ; TODO: just give it a try
       :desc "Line numbers" "l" #'doom/toggle-line-numbers
       :desc "Tab/Space switch" "t" #'doom/toggle-indent-style
       :desc "Read-only mode" "r" #'read-only-mode
       (:when (modulep! :checkers syntax)
         :desc "Flycheck" "c" #'flycheck-mode)
       (:when (modulep! :ui minimap)
         :desc "Minimap" "m" #'minimap-mode)
       (:when (modulep! :lang org +present)
         :desc "Org presentation" "p" #'org-tree-slide-mode)
       (:when (modulep! :ui zen)
         :desc "Zen mode" "z" #'+zen/toggle
         :desc "Zen mode (fullscreen)" "Z" #'+zen/toggle-fullscreen))

      ;;; C-c y --- snippets
      (:prefix-map ("y" . "<snippets>")
       :desc "New snippet" "n" #'+snippets/new
       :desc "Edit snippet" "e" #'+snippets/edit
       :desc "Find snippet" "f" #'+snippets/find
       :desc "Browse snippets" "b" #'+default/browse-templates)
      ;; yasnippet bindings
      (:when (modulep! :editor snippets)
        (:after yasnippet
         :map yas-minor-mode-map
         "&" nil
         (:prefix "i"
          :desc "Snippet" "s" #'yas-insert-snippet)
         (:prefix "y"
          :desc "Reload snippets" "r" #'yas-reload-all
          :desc "Insert snippet" "i" #'yas-insert-snippet)))

      )
