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

;; :editor
;; format
(when (modulep! :editor format)
  (map! :map prog-mode-map
        :prefix "C-c c"
        :desc "Format buffer" "f" #'+format/region-or-buffer
        :desc "Format with apheleia" "F" #'apheleia-format-buffer))

;; :editor
;; fold
(when (modulep! :editor fold)
  (after! outline
    (setq-hook! 'outline-minor-mode-hook
      outline-minor-mode-prefix (kbd "C-c 2 l"))
    (which-key-add-keymap-based-replacements
      outline-minor-mode-map "C-c 2 l" "outline")
    (undefine-key! outline-minor-mode-map "C-c @"))

  (map! :map prog-mode-map
        :prefix ("C-c 2" . "fold")
        :desc "Fold/Unfold" "2" #'+fold/toggle
        :desc "Fold all" "f" #'+fold/close-all
        :desc "Unfold all" "u" #'+fold/open-all
        :desc "Delete folded" "d" #'vimish-fold-delete))


;; :editor
;; yasnippet
(when (modulep! :editor snippets)
  (map! :map yas-minor-mode-map
        "C-c &" nil
        (:prefix "C-c i"
         :desc "Snippet" "s" #'yas-insert-snippet)
        (:prefix "C-c y"
         :desc "Reload snippets" "r" #'yas-reload-all
         :desc "Insert snippet" "i" #'yas-insert-snippet)))

;; :tools
;; editorconfig
;; Indent for languages, See: `editorconfig-indentation-alist'

;; :tools
;; ein (jupyter notebook)
(when (modulep! :tools ein)
  (after! ein
    ;; for jupyter-lab, otherwise use "notebook"
    (setq! ein:jupyter-server-use-subcommand "server")))

;; :tools
;; eval
;; check `quickrun--language-alist' for languages
;; to add new or overwrite, See:
;; https://github.com/emacsorphanage/quickrun?tab=readme-ov-file#user-defined-command
;; TODO check with python
;; (when (modulep! :tools eval)
;;   (map! :map (prog-mode-map emacs-lisp-mode-map)
;;         :prefix ("C-c l e" . "eval")
;;         :desc "Eval line" "l" #'+eval/line-or-region
;;         :desc "Eval buffer" "b" #'+eval/buffer-or-region
;;         :desc "Region to REPL" "s" #'+eval/send-region-to-repl
;;         :desc "Open REPL same window" "r" #'+eval/open-repl-same-window
;;         :desc "Open REPL other window" "R" #'+eval/open-repl-other-window))

;; [Packages]
;; Rainbow mode: highlight color string
(use-package! rainbow-mode
  :hook ((emacs-lisp-mode html-mode css-mode) . rainbow-mode))

;; Codeium: NOTE wait for the async company-backend
;; cape-capf-super solution is still experimental

;; Github Copilot
(use-package! copilot
  :hook ((prog-mode git-commit-setup conf-mode yaml-mode) . copilot-mode)
  :config
  (setq! copilot--indent-warning-printed-p t)
  (map! :map copilot-completion-map
        "<backtab>" #'copilot-accept-completion
        (:prefix "C-c l c"
         :desc "Copilot panel" "P" #'copilot-panel-complete
         :desc "Copilot by line"  "w" #'copilot-accept-completion-by-word
         :desc "Copilot by line"  "l" #'copilot-accept-completion-by-line
         :desc "Copilot next" "n" #'copilot-next-completion
          :desc "Copilot previous" "p" #'copilot-previous-completion))
;;;###package whitespace
    ;; For Github Copilot compatibility
    ;; Cursor Jump to End of Line When Typing
    ;; If you are using whitespace-mode, make sure to remove newline-mark from whitespace-style.
    (setq! whitespace-style (delq 'newline-mark whitespace-style)))
