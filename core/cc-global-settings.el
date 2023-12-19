;;; core/cc-global-settings.el -*- lexical-binding: t; -*-

;; Fullscreen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable "continue comments" functionality
(advice-remove 'newline-and-indent '+default--newline-indent-and-continue-comments-a)
