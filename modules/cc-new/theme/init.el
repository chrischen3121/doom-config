;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; cc-new/theme/init.el
(defcustom cc/mono-font
  (string-trim-right
   (font-get-system-font) " [0-9]+$")
  "The default monospace font for the system."
  :type 'string
  :group 'cc)

(defcustom cc/mono-font-size 16
  "The default monospace font size."
  :type 'integer
  :group 'cc)

(defcustom cc/light-theme 'doom-one-light
  "The default light theme."
  :type 'symbol
  :group 'cc)

(defcustom cc/dark-theme 'doom-tomorrow-night
  "The default dark theme."
  :type 'symbol
  :group 'cc)
