;; -*- no-byte-compile: t; -*-
;;; cc/dev/config.el -*- lexical-binding: t; -*-

(map! :map prog-mode-map
      "M-<RET>" #'+default--newline-indent-and-continue-comments-a)

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (setq! company-tooltip-limit 12)
    (map! :map company-active-map
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
        :desc "Format buffer" "f" #'+format/region-or-buffer))

;; :editor
;; fold
(when (modulep! :editor fold)
  (map! :map prog-mode-map
        :prefix "C-c C-f"
        :desc "Fold region" "C-f" #'+fold/toggle
        :desc "Unfold region" "C-u" #'+fold/open
        :desc "Fold all" "F" #'+fold/close-all
        :desc "Unfold all" "U" #'+fold/open-all
        :desc "Delete folded" "d" #'vimish-fold-delete
        :desc "Delete all folded" "D" #'vimish-fold-delete-all))
