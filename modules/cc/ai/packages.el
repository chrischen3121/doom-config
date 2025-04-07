;; -*- no-byte-compile: t; -*-
;;; cc/ai/packages.el

;; (package! aider)

(package! aidermacs)

(package! gptel :recipe (:nonrecursive t))

(package! mcp
  :recipe (:host github
           :repo "lizqwerscott/mcp.el"
           :files ("*.el")))
