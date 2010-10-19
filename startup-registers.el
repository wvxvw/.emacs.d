;;; startup-registers.el --- Set up registers
;;
;; Part of the Emacs Starter Kit

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))
             (?b (file . ,(concat dotfiles-dir "startup-bindings.el")))
             (?r (file . ,(concat dotfiles-dir "startup-registers.el")))
             (?j (file . ,(concat dotfiles-dir "org/inbox.org")))
             (?w (file . ,(concat dotfiles-dir "org/work.org")))))
  (set-register (car r) (cadr r)))

(provide 'startup-registers)
;;; startup-registers.el ends here