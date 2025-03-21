;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc/bindings/autoload.el
(autoload 'org-capture-goto-target "org-capture" nil t)
(autoload 'recentf-open-files "recentf" nil t)
(autoload 'projectile-recentf "projectile" nil t)

(when (modulep! :tools upload)
  (dolist (handler '(ssh-deploy-upload-handler
                     ssh-deploy-download-handler
                     ssh-deploy-delete-handler
                     ssh-deploy-browse-remote-handler
                     ssh-deploy-remote-changes-handler
                     ssh-deploy-upload-handler-forced
                     ssh-deploy-open-remote-file-handler
                     ssh-deploy-diff-handler))
    (autoload handler "ssh-deploy" nil t))
  )
