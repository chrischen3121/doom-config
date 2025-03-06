;;; cc-new/bindings/config.el -*- lexical-binding: t; -*-
;; -*- no-byte-compile: t; -*-

;; Unset global keybindings
(undefine-key! global-map
  "C-z"
  "C-x C-z"
  "C-x 8" ; emoji
  "C-h 4" ; info other window
  )

(setq!
 doom-leader-key nil
 doom-localleader-key nil
 doom-leader-alt-key "C-c M-d"
 doom-localleader-alt-key "C-c M-d l")

;; which-key configuration
(remove-hook! 'doom-first-input-hook #'which-key-mode)
(add-hook! 'doom-first-buffer-hook
           :append
           #'which-key-mode)

(after! which-key
  (setq! which-pkey-sort-order 'which-key-description-order))

(map! :after which-key
      :prefix "C-c"

      ;; C-c g -- global keybindings
      (:prefix-map
       ("g" . "<global>")
       (:when (modulep! :ui popup)
         :desc "Close all popups" "c" #'+popup/close-all)
       )

      ;; C-c w --- workspace
      (:prefix-map
       ("w" . "<workspace>")
       (:when (modulep! :ui workspaces)
         :desc "Make workspace" "m" #'+workspace/new-named
         :desc "Save workspace" "s" #'+workspace/save
         :desc "Load workspace" "l" #'+workspace/load
         :desc "Remove workspace" "r" #'+workspace/delete
         :desc "Switch workspace" "o" #'+workspace/switch-to
         :desc "Display workspaces" "d" #'+workspace/display
         )
       ;; session
       :desc "Load last session" "w" #'doom/quickload-session)

      ;; C-c l -- local keybindings
      ;; which related to current major mode
      (:prefix-map
       ("l" . "<local>")
       )

      ;; C-c c -- code keybindings
      (:prefix-map
       ("c" . "<code>")
       )

      ;; C-c i -- insert keybindings
      (:prefix-map
       ("i" . "<insert>")
       (:when (modulep! :completion corfu)
         :desc "Emoji" "e" #'cape-emoji
         :desc "dabbrev" "d" #'cape-dabbrev)
       )
      (:prefix-map
       ("m" . "<make>")
       :desc "Make frame" "f" #'make-frame
       ))
