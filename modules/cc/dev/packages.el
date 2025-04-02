;; -*- no-byte-compile: t; -*-
;;; cc/better-defaults/packages.el

(disable-packages! company-dict
                   eglot)

(package! rainbow-mode)

(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))

;; (package! codeium
;;   :recipe (:host github
;;            :repo "Exafunction/codeium.el"
;;            :files ("*.el")))

(package! aider
  :recipe (:host github
           :repo "tninja/aider.el"
           :files ("*.el")))
