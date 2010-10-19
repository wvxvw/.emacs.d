;;; startup-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
;;(add-hook 'espresso-mode-hook 'moz-minor-mode)
;;(add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)
;;(add-hook 'espresso-mode-hook 'run-coding-hook)
;;(add-hook 'espresso-mode-hook 'idle-highlight)
(setq espresso-indent-level 4)

(eval-after-load 'espresso
  '(progn ;;(define-key espresso-mode-map "{" 'paredit-open-curly)
          ;;(define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
          ;; fixes problem with pretty function font-lock
          (define-key espresso-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords 'espresso-mode
                        '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                           1 font-lock-warning-face t)))
          (font-lock-add-keywords
           'espresso-mode `(("\\(function *\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))))))

(provide 'startup-js)
;;; startup-js.el ends here

;; TODO consider implementing this
;; (defun compose (regex char)
;;   `(,regex
;;     (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                               ,char
;;                               nil)))))

;; (font-lock-add-keywords
;;  'javascript-mode
;;  (let ((bindings
;;         `(("[^a-zA-Z0-9$]\\(function\\>\\)" . ,(make-char
;;  'greek-iso8859-7 107))
;;           ("^\\(function\\>\\)" . ,(make-char 'greek-iso8859-7 107))
;;           ("[^a-zA-Z0-9$]\\(return\\>\\) " . ?↺)
;;           ("^\\(return \\>\\)" . ?↺)
;;           ("[^a-zA-Z0-9$]\\(return\\) " . ?↺)
;;           ("^\\(return\\) " . ?↺)
;;           ("[^a-zA-Z0-9$]\\(new \\)" . ?°)
;;           ("^\\(new\\) " . ?°)
;;           ("[^a-zA-Z0-9$]\\(var\\>\\)" . ?≡)
;;           ("^\\(var\\>\\)" . ?≡)
;;           ("[^a-zA-Z0-9$]\\(else\\>\\)" . ?↪)
;;           ("[^=]\\(=\\)[^=]" . ?←)
;;           ("[^=]\\(==\\)[^=]" . ?=)
;;           ("[^=]\\(!=\\)[^=]" . ?≠)
;;           ("[^a-zA-Z0-9$]\\(undefined\\>\\)" . ?Ø)
;;           ("[^a-zA-Z0-9$]Math.\\(sqrt\\>\\)" . ?√)
;;           ("[^a-zA-Z0-9$]\\(echo\\>\\)" . ?⇑))))
;;    (mapcar (lambda (rac) (compose (car rac) (cdr rac)))
;;         bindings)))