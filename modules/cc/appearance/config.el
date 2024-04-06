;; -*- no-byte-compile: t; -*-
;;; cc/appearance/config.el -*- lexical-binding: t; -*-
;;;

;; other frame settings: 'default-frame-alist
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(defcustom cc/default-font "Hack"
  "Adjust default font for your system."
  :type 'string
  :group 'cc-appearance)

(defcustom cc/default-unicode-font "WenQuanYi Micro Hei"
  "Adjust unicode font for your system."
  :type 'string
  :group 'cc-appearance)


(defcustom cc/default-font-size 20
  "Adjust default font size."
  :type 'integer
  :group 'cc-appearance)

(defcustom cc/default-unicode-font-size 20
  "Adjust unicode font size for your system."
  :type 'integer
  :group 'cc-appearance)


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

(defvar cc/light-theme 'doom-one-light)
(defvar cc/dark-theme 'doom-tomorrow-night)

;; Automatically set default theme
(add-hook! 'doom-init-ui-hook
           :append
           ;; Set default theme by system style if it's gnome
           ;; otherwise set by time
           (defun cc/after-doom-init-ui ()
             ;; Setting fonts should be done before loading theme
             ;; Or here is a workaround to reload theme after setting fonts
             (setq! doom-font
                    (font-spec :family cc/default-font :size cc/default-font-size)
                    doom-variable-pitch-font
                    (font-spec :family cc/default-font :size cc/default-unicode-font-size)
                    doom-big-font
                    (font-spec :family cc/default-font
                               :size (+ cc/default-font-size (/ cc/default-font-size 3)))
                    doom-serif-font
                    (font-spec :family cc/default-unicode-font :size cc/default-unicode-font-size)
                    doom-symbol-font
                    (font-spec :family cc/default-unicode-font :size cc/default-unicode-font-size))
             (if (string-equal (getenv "XDG_SESSION_TYPE") "x11")
                 (cc/set-default-theme-by-sys-style)
               (cc/set-default-theme-by-time))
             (doom/reload-theme)
             (map! "<f12>" #'cc/switch-light-dark-theme)))


;; Change ace-window leading char face
(after! ace-window
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "#51afef"
      :weight bold
      :height 5.0)))

(when (modulep! :ui doom-dashboard)
  (setq! +doom-dashboard-name "Happy Hacking!"))
