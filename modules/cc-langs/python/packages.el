;; -*- no-byte-compile: t; -*-
;;; cc-langs/python/packages.el

(unless (modulep! :lang rst)
  (package! sphinx-doc))
