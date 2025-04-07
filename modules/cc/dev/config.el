;;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; cc/dev/config.el

;; :tools
;; ein (jupyter notebook)
(when (modulep! :tools ein)
  (after! ein
    ;; for jupyter-lab, otherwise use "notebook"
    (setq! ein:jupyter-server-use-subcommand "server")))


;; [Packages]
;; Rainbow mode: highlight color string
(use-package! rainbow-mode
  :hook ((emacs-lisp-mode html-mode css-mode scss-mode) . rainbow-mode)
  :config
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

;; Github Copilot
(use-package! copilot
  :hook ((emacs-lisp-mode) . copilot-mode)
  :init
  (add-hook! (prog-mode yaml-mode conf-mode) #'copilot-mode)
  ;; (when (modulep! :tools lsp)
  ;;   (unless lsp-copilot-enabled
  ;;     (add-hook! (prog-mode yaml-mode conf-mode) #'copilot-mode)))
  :config
  (setq! copilot-indent-offset-warning-disable t)
  ;; For Github Copilot compatibility
  ;; Cursor Jump to End of Line When Typing
  ;; If you are using whitespace-mode, make sure to remove newline-mark from whitespace-style.
  ;; TODO may not be needed anymore
  ;; (setq! whitespace-style (delq 'newline-mark whitespace-style))
  (map! :desc "Copilot mode" "C-c t a" #'copilot-mode
        :map copilot-completion-map
        "<backtab>" #'copilot-accept-completion
        "M-<return>" #'copilot-accept-completion
        "C-<return>" #'copilot-accept-completion
        "M-w" #'copilot-accept-completion-by-word
        "M-l" #'copilot-accept-completion-by-line
        "M-n" #'copilot-next-completion
        "M-p" #'copilot-previous-completion))

;; Github Copilot Chat
(use-package! copilot-chat
  :commands copilot-chat-transient
  :init
  (map! :desc "Copilot chat menu" "C-c c p" #'copilot-chat-transient)
  :config
  (add-hook! 'git-commit-setup-hook #'copilot-chat-insert-commit-message)
  (setq! copilot-chat-backend 'curl
         copilot-chat-frontend 'org
         copilot-chat-default-model cc/copilot-chat-model))

;; aider
(use-package! aider
  :commands (aider-transient-menu)
  :init
  (map! :desc "Aider menu" "C-c a" #'aider-transient-menu))


;; codeium
;; (use-package! codeium
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   :config
;;   (setq use-dialog-box nil))
