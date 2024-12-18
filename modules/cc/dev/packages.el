;; -*- no-byte-compile: t; -*-
;;; cc/better-defaults/packages.el

(disable-packages! company-dict
                   evil-vimish-fold
                   eglot)

(package! rainbow-mode)

(package! copilot
  :recipe (:host github
           :repo "copilot-emacs/copilot.el"
           :files ("*.el")))

;; TODO no syntax hightlight feature in built-in treesit
;; (package! combobulate
;;   :recipe (:host github
;;            :repo "mickeynp/combobulate"
;;            :branch "development"
;;            :files ("*.el")))
