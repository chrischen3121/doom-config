;; -*- no-byte-compile: t; -*-
;;; cc/dev/config.el -*- lexical-binding: t; -*-

(map! :map prog-mode-map
      "M-<RET>" #'+default--newline-indent-and-continue-comments-a)

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (setq! company-tooltip-limit 12)
    (map! :desc "Complete path" "C-c C-/"  #'company-files
          :map company-active-map
          "M-/" #'company-other-backend
          "S-<tab>" nil)
    (advice-add '+company-init-backends-h :around #'cc/overwrite-company-backends)
    (setq-hook! 'org-mode-hook company-minimum-prefix-length 3)))

(when (modulep! :completion company +childframe)
  (after! company-box
    (setq-hook! 'company-box-mode-hook company-box-doc-delay 2)))

;; :editor
;; format
(when (modulep! :editor format)
  (map! :map prog-mode-map
        :prefix "C-c c"
        :desc "Format buffer" "f" #'+format/region-or-buffer
        :desc "Format with apheleia" "F" #'apheleia-format-buffer))

;; :editor
;; fold
(when (modulep! :editor fold)
  (after! outline
    (setq-hook! 'outline-minor-mode-hook
      outline-minor-mode-prefix (kbd "C-c 2 l"))
    (which-key-add-keymap-based-replacements
      outline-minor-mode-map "C-c 2 l" "outline")
    (undefine-key! outline-minor-mode-map "C-c @"))

  (map! :map prog-mode-map
        :prefix ("C-c 2" . "fold")
        :desc "Fold/Unfold" "2" #'+fold/toggle
        :desc "Fold all" "f" #'+fold/close-all
        :desc "Unfold all" "u" #'+fold/open-all
        :desc "Delete folded" "d" #'vimish-fold-delete))

;; :tools
;; docker
(when (modulep! :tools docker)
  (map! :prefix "C-c o"
        :desc "Docker" "D" #'docker))

;; :tools
;; editorconfig
;; Indent for languages, See: editorconfig-indentation-alist

;; :tools
;; ein (jupyter notebook)
(when (modulep! :tools ein)
  (after! ein
    (setq! ein:jupyter-server-use-subcommand "server"))
  (map! :prefix "C-c o"
        :desc "Jupyter run" "j" #'ein:run
        :desc "Jupyter login" "J" #'ein:login
        :desc "Jupyter stop" "C-j" #'ein:stop))
