(setq find-program "C:/cygwin/bin/find.exe")

;; C#-mode

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(require 'compile)
(require 'cl)

(add-hook 'csharp-mode-hook (λ() (sln-minor-mode 1)))

(load-library "~/.emacs.d/vendor/cske/msbuild.el")
(load-library "~/.emacs.d/vendor/cske/sln-mode.el")
(load-library "~/.emacs.d/vendor/cske/csproj-mode.el")
(load-library "~/.emacs.d/vendor/cske/sln-minor-mode.el")

(add-to-list 'auto-mode-alist '("\\.sln$" . sln-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . csproj-mode))
(add-to-list 'auto-mode-alist '("\\.cs" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)) ;; csharp-mode in csproj files as well

(add-to-list 'auto-mode-alist '("\\.asp\\'" . asp-nxhtml-mumamo-mode))


(require 'compile)
(require 'cl)

;;; startup-asp.el --- Helpers for asp & asp.net coding
;;
;; Not part of Emacs Starter Kit

;; nxhtml mode
(load (concat dotfiles-dir "vendor/nxhtml/autostart.el"))

(require 'visual-basic-mode)
;;(require 'mumamo-fun)

;;;;;###autoload
;;(define-mumamo-multi-major-mode asp-html-mumamo-mode
;;  "Turn on multiple major modes for ASP with main mode tml-mode'.
;;This also covers inlined style and javascript."
;;  ("ASP Html Family" html-mode
;;   (mumamo-chunk-asp
;;    mumamo-asp-chunk-inlined-script
;;    mumamo-chunk-inlined-script
;;    mumamo-chunk-style=
;;    mumamo-chunk-onjs=
;;    )))  

(add-hook 'csharp-mode-hook (λ() (sln-minor-mode 1)))

(load-library "~/.emacs.d/vendor/cske/msbuild.el")
(load-library "~/.emacs.d/vendor/cske/sln-mode.el")
(load-library "~/.emacs.d/vendor/cske/csproj-mode.el")
(load-library "~/.emacs.d/vendor/cske/sln-minor-mode.el")

(add-to-list 'auto-mode-alist '("\\.sln$" . sln-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . csproj-mode))

(load-library "~/.emacs.d/vendor/csharp-mode-0.7.0.el")
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;;; startup-asp.el ends here      

(provide 'startup-windows)