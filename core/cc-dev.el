;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; enhanced company-yasnippet
(defun cc/company-to-yasnippet ()
  "Abort company completion and start Yasnippet selection."
  (interactive)
  (when (company-manual-begin)  ; Check if company is active
    (company-abort)            ; Abort company completion
    (company-yasnippet)))      ; Start Yasnippet selection

(map! :map prog-mode-map
      "M-/" #'company-yasnippet
      :map company-active-map
      "M-/" #'cc/company-to-yasnippet)

;; TODO: cancel company-popup and pop up yasnippet menu
;; (def advice/company-yasnippet
;;   (around company-yasnippet-popup-adv activate)
;;   (let ((company-tooltip-limit 1))
;;     ad-do-it))

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
