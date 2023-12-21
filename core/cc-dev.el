;;; core/cc-dev.el -*- lexical-binding: t; -*-

;; :completion
;; company
(when (modulep! :completion company)
  (after! company
    (map! :after yasnippet
          "M-/" #'company-yasnippet)
    (map! :map prog-mode-map
          "M-<RET>" #'+default--newline-indent-and-continue-comments-a
          :map company-active-map
          "M-/" #'company-abort)
    (set-company-backend!
      '(prog-mode yaml-mode conf-mode)
      'company-capf
      '(:seperate company-yasnippet company-files))
    (set-company-backend! '(text-mode org-mode)
      'company-capf
      '(:separate company-dabbrev company-files company-ispell)
      'company-yasnippet)
    )


  (after! company-box
    (setq-hook! 'company-box-mode-hook company-box-doc-delay 2)))


;; :editor
;; fold (outline hide/show)
;; C-c C-f - fold commands prefix
(when (modulep! :editor fold)
  (after! outline
    (which-key-add-key-based-replacements "C-c @" "outline")
    (which-key-add-key-based-replacements "C-c C-f C-a" "outline-all")
    (map! "C-c C-f o" #'+fold-open-all
          "C-c C-f c" #'+fold-close-all
          "C-c C-f d" #'+fold-toggle-all)))


;; TODO: not exactly same
;; setting default company backends

;; (set-company-backend! '(prog-mode yaml-mode conf-mode)
;;   '(:separate company-capf company-yasnippet)
;;   'company-yasnippet 'company-files)

;; (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

;; (set-company-backend! '(text-mode org-mode)
;;   '(:separate company-dabbrev company-yasnippet company-ispell)
;;   'company-yasnippet 'company-files)


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
(after! copilot
  (add-hook! (prog-mode git-commit-setup) #'copilot-mode)
  (map! :map copilot-completion-map
        "<backtab>" #'copilot-accept-completion
        "M-j" #'copilot-accept-completion
        "M-n" #'copilot-next-completion
        "M-p" #'copilot-previous-completion
        "M-l" #'copilot-accept-completion-by-line
        "M-o" #'copilot-panel-complete))
