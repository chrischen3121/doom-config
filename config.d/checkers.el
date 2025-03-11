;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; config.d/checkers.el

(when (modulep! :checkers spell)
  (setq! spell-fu-idle-delay 0.5)
  ;; exclude what faces to preform spellchecking on

  (setf
   (alist-get 'prog-mode +spell-excluded-faces-alist)
   '(font-lock-constant-face
     font-lock-string-face))

  (add-hook! 'spell-fu-mode-hook
    (defun add-personal-dictionary ()
      (spell-fu-dictionary-add
       (spell-fu-get-personal-dictionary "en" personal-aspell-en-dict))))
  )
