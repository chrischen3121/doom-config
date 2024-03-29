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

    ;; (put '+company-init-backends-h 'permanent-local-hook t)
    ;; the above line in doom code will overwrite the company-backends even after the major-mode-hook
    ;; the following advice will overwrite the company-backends after +company-init-backends-h
    (defun cc/overwrite-company-backends (orig-fun &rest args)
      ;; Call the original function first
      (apply orig-fun args)
      ;; Then apply your customization if in (org-mode prog-mode yaml-mode conf-mode)
      (when (member major-mode '(org-mode text-mode))
        (setq-local company-backends '(company-capf (company-ispell company-files :separate))))
      (when (member major-mode '(prog-mode yaml-mode conf-mode
                                 c-mode c++-mode python-mode))
        ;; (setq-local company-backends `(,+lsp-company-backends))
        (if (modulep! :editor snippets)
            (setq-local company-backends
                        `(,lsp-company-backends
                          (company-yasnippet company-files :separate)))
            (setq-local company-backends
                        `(,lsp-company-backends company-files :separate)))))
    (advice-add '+company-init-backends-h :around #'cc/overwrite-company-backends)

    (setq-hook! 'org-mode-hook company-minimum-prefix-length 3)))

(when (modulep! :completion company +childframe)
  (after! company-box
    (setq-hook! 'company-box-mode-hook company-box-doc-delay 2)))

;; :editor
;; format
(when (modulep! :editor format)
  (map! :after format
        :map prog-mode-map
        :prefix "C-c c"
        :desc "Format buffer" "f" #'+format/region-or-buffer))
