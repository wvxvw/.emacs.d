;;; init.el --- Fuck it just start editing!
;;

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
 
;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "vendor"))




(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify) ;; unique buffer names
(require 'ansi-color)
(require 'recentf) ;; recently opened files menu


(require 'startup-defuns)
(require 'startup-bindings)
(require 'startup-misc)
(require 'startup-registers)
(require 'startup-eshell)
(require 'startup-lisp)
(require 'startup-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))

(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-jabber-0.8.0")
(load "jabber-autoloads")

(jabber-connect-all)

(load-file "~/.emacs.d/saws-commands.el")

;; Work tracker
(load "worktracker.el")
(global-set-key [f9] 'work-clockin)
(global-set-key [f10] 'work-clockout)
(global-set-key [f12] 'work-interrupted)
(global-set-key [f11] 'work-resume)

;;(require 'magit)