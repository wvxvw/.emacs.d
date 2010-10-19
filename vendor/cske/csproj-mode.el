;;; csproj-mode.el --- Mode to handle .NET C# project files

;; Copyright (C) 2009 Cecilio Pardo

;; Author: Cecilio Pardo <cpardo@imayhem.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar csproj-mode-hook nil
  "List of functions to call when entering csproj-mode")

(defvar csproj-mode-map nil
  "Keymap for csproj-mode")

(setq csproj-mode-map nil)

(unless csproj-mode-map
  (setq csproj-mode-map (make-sparse-keymap))
  (define-key csproj-mode-map [down] 'csproj-move-point-to-next-file) 
  (define-key csproj-mode-map [up] 'csproj-move-point-to-previous-file) 
  (define-key csproj-mode-map [return] 'csproj-open-file-at-point)

  (define-key csproj-mode-map [(control c) ?b] 'msbuild)
  (define-key csproj-mode-map [(control c) ?r] 'msbuild-rebuild)
  (define-key csproj-mode-map [(control c) ?c] 'msbuild-clean)

  (define-key csproj-mode-map [menu-bar csproj-build] 
    (cons "Build" (make-sparse-keymap "Build" )))
 
  (define-key csproj-mode-map [menu-bar csproj-build csproj-clean] 
    '("Clean project (Solution with C-u)" . msbuild-clean))

  (define-key csproj-mode-map [menu-bar csproj-build csproj-rebuild] 
    '("Rebuild project (Solution with C-u)" . msbuild-rebuild))

  (define-key csproj-mode-map [menu-bar csproj-build csproj-build] 
    '("Build project  (Solution with C-u)" . msbuild))

)

(defun -csproj-find-solution-candidates(filename)
  (let ((f (file-expand-wildcards "*.sln")))
    (if f f 
      (setq f (file-expand-wildcards "../*.sln"))
      (if f f
        (error "No candidates")))))

(defun csproj-find-solution()
  "Searchs all open buffers for the Solution buffer for this
project. If cannot find it, prompt the user. Returns nil if
cannot find it."
  (catch t
    (let ((pname csproj-project-name))
      (dolist (b (buffer-list))
        (with-current-buffer b
          (and (eq major-mode 'sln-mode)
               (sln-project-exists pname)
               (throw t b))))
      ;; If we get here, did not find the solution
      (when (y-or-n-p 
           (concat
            "msbuild: Can't find sln buffer. Try to find it? "))
        (let ((sol
               (ido-completing-read 
                "Solution candidates: "
                (-csproj-find-solution-candidates pname))))
          (and sol (find-file-noselect sol)))))))

(defun csproj-goto-solution(&optional dont-switch)
  (interactive)
  (let ((sol (csproj-find-solution)))
    (if sol
        (funcall (if dont-switch 'set-buffer 'switch-to-buffer) sol)
      (error "Can't find solution buffer"))))

(defconst -csproj-file-nav-regexp 
  "^.*?<Compile.*?Include=\"\\([^\"]*\\)\"")

(defun -csproj-move-point-to-matched-file()
  (goto-char (match-beginning 1)))

(defun -csproj-match-file-at-point()
  (save-excursion
    (beginning-of-line)
    (unless (looking-at -csproj-file-nav-regexp)
      (error "Not at a file line"))))

(defun csproj-open-file-at-point()
  (interactive)
  (-csproj-match-file-at-point)
  (find-file (match-string 1)))

(defun csproj-move-point-to-next-file()
  (interactive)
  (if (re-search-forward -csproj-file-nav-regexp nil t)
      (-csproj-move-point-to-matched-file)
    (message "No more files")))

(defun csproj-move-point-to-previous-file()
  (interactive)
  (if (re-search-backward -csproj-file-nav-regexp nil t)
      (-csproj-move-point-to-matched-file)
    (message "No more files")))

(defun csproj-get-file-list()
  (let (out
        (l (imenu--make-index-alist)))
    (dolist (el l)
      (add-to-list 'out (car el)))
    out))

(defun csproj-file-exists(file)
  (catch t
    (dolist (l (csproj-get-file-list))
      (if (string= file (expand-file-name l))
          (throw t t)))))


;; (defun csproj-add-source-file(&optional fname)
;;   (interactive)
;;   (if (null fname)
;;       (csproj-add-source-file (read-file-name "Add source file to project: "))
;;     (print (xml-parse-region (point-min) (point-max)))
;;     ))

(defun csproj-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))

(defun -csproj-hide-blocks(regexp)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (goto-char (match-end 0))
    (hs-hide-block)))

(define-derived-mode csproj-mode nxml-mode "C# project"
  (setq mode-name "C# project" major-mode 'csproj-mode)
  (set (make-variable-buffer-local 'csproj-project-name)
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
  (use-local-map csproj-mode-map)
  (setq imenu-generic-expression 
        '(
          ;;("Sources" "^.*?<Compile.*?Include=\"\\([^\"]*\\)\"" 1)
          ;;("Emb.Resources" 
          ;; "^.*?<EmbeddedResource.*?Include=\"\\([^\"]*\\)\"" 1)
          (nil "^.*?<Compile.*?Include=\"\\([^\"]*\\)\"" 1)
          ))

  (imenu-add-to-menubar "Sources")

  (add-to-list 
   'hs-special-modes-alist
   '(csproj-mode
     "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
     ""
     "<!--" ;; won't work on its own; uses syntax table
     (lambda (arg) (csproj-forward-element))
     nil))
  (hs-minor-mode 1)

  (save-excursion
    (-csproj-hide-blocks "<PropertyGroup.*>")
    (-csproj-hide-blocks "<Target .*>")
    (-csproj-hide-blocks "<ItemGroup.*>\n.*<Reference")
    (-csproj-hide-blocks "<ItemGroup.*>\n.*<ProjectReference")
    )
  (run-hooks 'csproj-mode-hook))
