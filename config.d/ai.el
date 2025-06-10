;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/ai.el

(after! aidermacs
  ;; Comment with AI! triggers aider to make changes to your code.
  ;; AI? triggers aider to answer your question.
  (setq! aidermacs-use-architect-mode t
         aidermacs-default-model "sonnet"
         aidermacs-weak-model "claude-3-5-haiku-latest"
         ;; aidermacs-architect-model "gemini/gemini-2.5-pro-preview-06-05"
         aidermacs-auto-commits nil
         ;; May modify aidermacs-common-prompts
         )
  ;; (add-to-list 'aidermacs-extra-args "--verbose")
  )

(after! gptel
  (setq! gptel-model 'claude-3-5-haiku-latest
         gptel-temperature 0.8
         gptel-max-tokens 2048))
