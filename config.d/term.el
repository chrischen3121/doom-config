;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/term.el

(when (modulep! :term vterm)
  (setq! vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
