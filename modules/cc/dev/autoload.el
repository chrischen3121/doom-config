;;; cc/dev/autoload.el -*- lexical-binding: t; -*-

;; Issue:
;; (put '+company-init-backends-h 'permanent-local-hook t)
;; the above line in doom code will overwrite the company-backends even after the major-mode-hook
;; the following advice will overwrite the company-backends after +company-init-backends-h

;;;###autoload
(defun cc/overwrite-company-backends (orig-fun &rest args)
  ;; Call the original function first
  (apply orig-fun args)

  ;; text-mode backends
  ;; Then apply your customization if in (org-mode prog-mode yaml-mode conf-mode)
  (when (member major-mode '(org-mode text-mode))
    (setq-local company-backends '(company-capf (company-ispell company-files :separate))))

  ;; prog-mode backends
  (when (member major-mode '(prog-mode yaml-mode conf-mode
                             c-mode c++-mode python-mode))
    ;; (setq-local company-backends `(,+lsp-company-backends))
    (if (modulep! :editor snippets)
        (setq-local company-backends
                    `(,+lsp-company-backends
                      (company-yasnippet company-files :separate)))
      (setq-local company-backends
                  `(,+lsp-company-backends company-files :separate)))))
