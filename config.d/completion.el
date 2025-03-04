;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/completion.el
;; TODO try cape-dict
(when (modulep! :completion vertico)
  (map! :map vertico-map
        "C-l" #'vertico-directory-delete-word
        "C-M-p" #'vertico-previous-group
        "C-M-n" #'vertico-next-group
        "C-SPC" #'+vertico/embark-preview
        :desc "Export to buffer" "C-c C-e" #'embark-export))

(when (modulep! :completion corfu)
  (map! :map corfu-map
        "C-c C-e" #'+corfu/move-to-minibuffer
        "C-SPC" #'corfu-insert-separator
        :map corfu-popupinfo-map
        "M-p" #'corfu-popupinfo-scroll-down
        "M-n" #'corfu-popupinfo-scroll-up
        "M-<" #'corfu-popupinfo-beginning
        "M->" #'corfu-popupinfo-end)
  (after! cape
    (map! "M-/" #'cape-dabbrev
          "C-M-/" #'dabbrev-expand
          (:map mode-specific-map
           :desc "Emoji" "i e" #'cape-emoji))))

;; completion.el ends here
