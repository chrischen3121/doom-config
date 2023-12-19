;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; outline settings
(after! outline
  (which-key-add-key-based-replacements "C-c @" "outline"))

;; company settings
(map! :map prog-mode-map
      "M-/" #'company-yasnippet
      "M-<RET>" #'+default--newline-indent-and-continue-comments-a
      :map company-active-map
      "M-/" #'company-abort)


;; TODO: setting company-backends doesn't work
(defun cc/set-default-text-backends ()
  (setq-local company-backends
              '(company-capf
                company-files
                (:separate company-dabbrev company-yasnippet company-ispell))
              ))

(defun cc/set-default-code-backends ()
  (setq-local company-backends
              '((:separate company-capf company-yasnippet) company-yasnippet company-files)
              ))

(add-hook! 'prog-mode-hook #'cc/set-default-code-backends)
(add-hook! 'text-mode-hook #'cc/set-default-text-backends)


;; copilot
(use-package! copilot
  ;; :config
  ;; (setq! copilot-indent-warning-suppress t)
  :hook
  ((prog-mode . copilot-mode)
   (git-commit-setup . copilot-mode))
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("M-n" . copilot-next-completion)
        ("M-p" . copilot-previous-completion)
        ("M-l" . copilot-accept-completion-by-line)
        ))
