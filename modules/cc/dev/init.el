;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/dev/init.el

(defcustom cc/copilot-chat-model "gpt-4o"
  "Default model for Copilot Chat."
  :group 'cc-dev
  :type 'string)

(defcustom cc/copilot-chat-commit-model "claude-sonnet-4"
  "Default model for Copilot Chat commit messages."
  :group 'cc-dev
  :type 'string)

(defun cc/minuet--use-claude ()
  (setenv "ANTHROPIC_API_KEY" cc/anthropic-key)
  (setq! minuet-provider 'claude
         minuet-n-completions 2
         minuet-context-window 768))

(defun cc/minuet--use-ollama ()
  (setq! minuet-provider 'openai-fim-compatible
         minuet-n-completions 1
         minuet-context-window 512)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:7b"))
