;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/ai/config.el

;; aider
;; (use-package! aider
;;   :commands (aider-transient-menu)
;;   :init
;;   (map! :desc "Aider menu" "C-c a" #'aider-transient-menu))

;; aidermacs
(use-package! aidermacs
  :commands aidermacs-transient-menu
  :init
  (map! :desc "Aider menu" "C-c a" #'aidermacs-transient-menu)
  :config
  (setenv "ANTHROPIC_API_KEY" cc/anthropic-key)
  (setenv "OPENAI_API_KEY" cc/openai-key)
  (setenv "GEMINI_API_KEY" cc/gemini-key)
  (setenv "DEEPSEEK_API_KEY" cc/deepseek-key)
  (setq! aidermacs-use-architect-mode t
         aidermacs-default-model "sonnet"
         ;; for architect reasoning
         aidermacs-architect-model "gemini/gemini-2.5-pro-exp-03-25"
         ;; for code generation
         aidermacs-editor-model "sonnet"
         aidermacs-auto-commits nil
         ;; aidermacs-config-file "~/.aider.conf.yml"
         ))

;; gptel
(use-package! gptel
  :commands (gptel-send
             gptel
             gptel-menu
             gptel-add
             gptel-add-file
             gptel-rewrite)
  :init
  (map! :prefix ("C-c x" . "gptel")
        :desc "gptel menu" "x" #'gptel-menu
        :desc "Open chat" "c" #'gptel
        :desc "Add region/buffer" "a" #'gptel-add
        :desc "Add file" "f" #'gptel-add-file
        :desc "Rewrite region" "r" #'gptel-rewrite
        :desc "Tools enable" "t" #'cc/gptel-enable-all-mcp-tools
        :desc "Tools disable" "T" #'cc/gptel-disable-all-mcp-tools)
  :config
  (setq! gptel-default-mode 'org-mode
         gptel-temperature 1
         gptel-max-tokens 8192
         gptel-log-level 'info
         gptel-use-tools t
         gptel-include-reasoning t
         gptel-rewrite-default-action 'diff)
  (setq! gptel-api-key cc/openai-key)
  (gptel-make-anthropic "Claude" :stream t :key cc/anthropic-key)
  (gptel-make-gemini "Gemini" :stream t :key cc/gemini-key)
  (gptel-make-deepseek "DeepSeek" :stream t :key cc/deepseek-key)
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:latest
              gemma3:12b))
  )

;; mcp servers
;; use `mcp-make-text-tool` to create a gptel tool
(use-package! mcp-hub
  :commands (mcp-hub
             mcp-hub-start-all-server
             mcp-hub-close-all-server)
  :init
  (setq mcp-hub-servers
        `(("filesystem" .
           (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,cc/mcp-fs-directory)))
          ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))
  (map! :desc "mcp hub" "C-c x m" #'mcp-hub)
  (when cc/use-mcp-p
    (after! gptel
      (cc/gptel-mcp-register-tools))
    (add-hook 'after-init-hook #'mcp-hub-start-all-server)
    (add-hook 'gptel-mode-hook #'cc/gptel-enable-all-mcp-tools)))
