;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/dev/init.el

(defcustom cc/copilot-chat-model "claude-3.7-sonnet"
  "Default model for Copilot Chat."
  :group 'cc-dev
  :type 'string)

(defun cc/minuet--use-claude ()
  (setenv "ANTHROPIC_API_KEY" cc/anthropic-key)
  (setq minuet-provider 'claude))

(defun cc/minuet--use-ollama ()
  (setq! minuet-provider 'openai-compatible)
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 64)
  (plist-put minuet-openai-compatible-options :end-point "http://localhost:11434/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :name "Ollama")
  (plist-put minuet-openai-compatible-options :api-key "Not needed")
  (plist-put minuet-openai-compatible-options :model "qwen2.5-coder"))
