;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/dev/autoload.el

;;;###autoload
(defun cc/close-all-copilot-chat-buffers (&rest _)
  "Close all copilot chat buffers."
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*Copilot Chat" (buffer-name buf))
      (kill-buffer buf))))
