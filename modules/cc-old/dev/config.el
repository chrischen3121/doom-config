;; -*- no-byte-compile: t; -*-
;;; cc/dev/config.el -*- lexical-binding: t; -*-

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (setq! company-tooltip-limit 12)
    (map! :prefix ("C-c l c" . "<company>")
          "f" #'company-files
          "d" #'company-dabbrev
          "D" #'company-dabbrev-code
          "y" #'company-yasnippet
          "c" #'company-complete
          "C" #'company-capf
          "s" #'company-ispell
          "g" #'company-gtags
          "e" #'company-etags
          "o" #'company-other-backend)
    (map! :map company-active-map
          "M-/" #'company-other-backend
          "S-<tab>" nil)
    (advice-add '+company-init-backends-h :around #'cc/overwrite-company-backends)
    (setq-hook! 'org-mode-hook company-minimum-prefix-length 3)))

(when (modulep! :completion company +childframe)
  (after! company-box
    (setq-hook! 'company-box-mode-hook company-box-doc-delay 2)))

;; :tools
;; ein (jupyter notebook)
(when (modulep! :tools ein)
  (after! ein
    ;; for jupyter-lab, otherwise use "notebook"
    (setq! ein:jupyter-server-use-subcommand "server")))


;; [Packages]
;; Rainbow mode: highlight color string
;; (use-package! rainbow-mode
;;   :hook ((emacs-lisp-mode html-mode css-mode) . rainbow-mode))

;; rainbow-mode
(use-package! rainbow-mode
  :hook ((emacs-lisp-mode html-mode css-mode scss-mode) . rainbow-mode)
  :config
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

;; Github Copilot
(use-package! copilot
  :hook ((prog-mode git-commit-setup conf-mode yaml-mode) . copilot-mode)
  :config
  (setq! copilot-indent-offset-warning-disable t)
  (map! :map copilot-completion-map
        ;; "<backtab>" #'copilot-accept-completion
        "C-M-w" #'copilot-accept-completion-by-word
        "C-M-l" #'copilot-accept-completion-by-line
        "C-M-n" #'copilot-next-completion
        "C-M-p" #'copilot-previous-completion)
;;;###package whitespace
  ;; For Github Copilot compatibility
  ;; Cursor Jump to End of Line When Typing
  ;; If you are using whitespace-mode, make sure to remove newline-mark from whitespace-style.
  (setq! whitespace-style (delq 'newline-mark whitespace-style)))

;; TODO may try Codeium later on
;; codeium-completion-at-point should be the first in the completion-at-point-functions
;; which is imcompatible with lsp
;; enable company-preview-frontend if using codeium
;; (use-package! codeium
;;   :init
;;   (codeium-init)
;;   (add-hook! 'prog-mode-hook
;;     (defun cc/set-codeium-capf ()
;;       :local (add-to-list 'completion-at-point-functions
;;                           #'codeium-completion-at-point))))

;; Langs
(add-hook! 'sh-mode-hook
  (defun cc/set-default-shell ()
    (sh-set-shell "bash")))
