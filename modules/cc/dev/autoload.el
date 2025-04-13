;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/dev/autoload.el

;;;###autoload
(defun cc/close-copilot-chat-buffers (&rest _ )
  "Close all buffers with names starting with \"*Copilot Chat\""
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*Copilot Chat" (buffer-name buf))
      (kill-buffer buf))))
