;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-new/bindings/autoload.el

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
