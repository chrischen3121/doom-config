;; -*- no-byte-compile: t; -*-
;;; cc/better-defaults/packages.el

(disable-packages! company-dict
                   evil-vimish-fold)

(package! rainbow-mode)

(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))
