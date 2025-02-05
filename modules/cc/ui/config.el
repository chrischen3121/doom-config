;; -*- no-byte-compile: t; -*-
;;; cc/ui/config.el -*- lexical-binding: t; -*-
;;;

;; other frame settings: 'default-frame-alist
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;; Fonts Hint
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!



;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.

;; Define default light/dark themes
;; light:
;; doom-acario-light, doom-one-light, doom-nord,  doom-oksolar-light

;; dark:
;; doom-one, doom-peacock, doom-tomorrow-night, doom-opera

(map! "<f12>" #'cc/switch-light-dark-theme)


(when (modulep! :ui doom-dashboard)
  (setq! +doom-dashboard-name "Happy Hacking!"))
