;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq! user-full-name "Chris Chen"
       user-mail-address "chrischen3121@gmail.com")

(when (modulep! :cc ui)
  (cc/set-doom-ui-appearance))

;; org-directory must be set before doom-package:org has loaded
;; set if cc/org-home-dir is bound
(when (boundp 'cc/default-org-dir)
  (setq org-directory cc/default-org-dir))

(when (boundp 'cc/notes-base-dir)
  (setq! cc/roam-notes-dir (concat cc/notes-base-dir "roamnotes/")
         cc/org-pdf-notes-dir (concat cc/notes-base-dir "pdfnotes/")
         cc/roam-journals-dir (concat cc/notes-base-dir "journals/")))

;; load config files
(let ((config-dir (expand-file-name "config.d" doom-user-dir)))
  (if (file-directory-p config-dir)
      (dolist (file (directory-files config-dir t "\\.el$"))
        (load file))
    (error "Private config dir %s does not exist" config-dir)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq! display-line-numbers-type t)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
