;; -*- no-byte-compile: t; -*-
;;; cc/ai/packages.el

(package! transient
  :pin "13daf473d5080b91bc37f40e0f3b566738707914") ; 0.9.1
(package! aider
  :recipe (:host github :repo "tninja/aider.el" ))

(package! aidermacs)

(package! gptel :recipe (:nonrecursive t))

(package! mcp
  :recipe (:host github
           :repo "lizqwerscott/mcp.el"
           :files ("*.el")))
