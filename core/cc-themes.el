;;; core/cc-themes.el -*- lexical-binding: t; -*-

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
(setq doom-font (font-spec :family "Hack" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 22)
      doom-big-font (font-spec :family "Fira Code" :size 28 :weight 'semi-light)
      doom-serif-font (font-spec :family "Fira Code" :size 20 :weight 'semi-light)
      doom-symbol-font (font-spec :family "Fira Code" :size 20 :weight 'semi-light)
      doom-unicode-font (font-spec :family "WenQuanyi Micro Hei Mono" :size 20)
      )


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-gruvbox-light)
;; (setq doom-theme 'doom-gruvbox)

(defvar cc/light-theme 'doom-tomorrow-day)
(defvar cc/dark-theme 'doom-palenight)

;; set default theme
(setq doom-theme 'doom-tomorrow-day)

;; (defun cc/switch-light-dark-theme ()
;;   """Switch light/dark themes"""
;;   (interactive)
;;   (if (eq (car custom-enabled-themes) 'cc/light-theme)
;;       (progn (disable-theme 'cc/light-theme)
;;       (load-theme 'cc/dark-theme t))
;;     (disable-theme 'cc/dark-theme)
;;     (load-theme 'cc/light-theme t)))
