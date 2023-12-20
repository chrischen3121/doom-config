;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; outline settings (hide/show)
(after! outline
  (which-key-add-key-based-replacements "C-c @" "outline"))

;; company settings
(map! :map prog-mode-map
      "M-/" #'company-yasnippet
      "M-<RET>" #'+default--newline-indent-and-continue-comments-a
      :map company-active-map
      "M-/" #'company-abort)

;; TODO: not exactly same
;; setting default company backends

(set-company-backend! 'prog-mode
  '(:separate company-capf company-yasnippet)
  'company-yasnippet 'company-files)

;; (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

(set-company-backend! 'text-mode
  '(:separate company-dabbrev company-yasnippet company-ispell)
    'company-yasnippet 'company-files)


;; ;; TODO: setting company-backends doesn't work
;; (defun cc/set-default-text-backends ()
;;   (setq-local company-backends
;;               '(company-capf
;;                 company-files
;;                 (:separate company-dabbrev company-yasnippet company-ispell))
;;               ))

;; (defun cc/set-default-code-backends ()
;;   (setq-local company-backends
;;               '((:separate company-capf company-yasnippet) company-yasnippet company-files)
;;               ))

;; (add-hook! 'prog-mode-hook #'cc/set-default-code-backends)
;; (add-hook! 'text-mode-hook #'cc/set-default-text-backends)


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
