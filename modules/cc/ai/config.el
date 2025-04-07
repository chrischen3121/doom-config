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
  :commands (gptel-send gptel gptel-menu)
  :init
  (map! :desc "gptel menu" "C-c g g" #'gptel-menu)
  :config
  (setq! gptel-api-key cc/openai-key)
  (gptel-make-anthropic "Claude" :stream t :key cc/anthropic-key)
  (gptel-make-gemini "Gemini" :stream t :key cc/gemini-key)
  (gptel-make-deepseek "DeepSeek" :stream t :key cc/deepseek-key))

;; mcp servers
(use-package! mcp-hub
  :commands (mcp-hub-start-all-server mcp-hub)
  :init
  (setq mcp-hub-servers
        `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,cc/mcp-fs-directory)))
          ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))
  (map! :desc "mcp hub" "C-c g m" #'mcp-hub))
