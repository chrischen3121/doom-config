;;; core/cc-dev.el -*- lexical-binding: t; -*-


(use-package! copilot
  :defer t
  :config
  (setq! copilot-indent-warning-suppress t)
  :hook
  ((prog-mode . copilot-mode)
   (git-commit-setup . copilot-mode))
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("M-n" . copilot-next-completion)
        ("M-p" . copilot-previous-completion)
        ))
