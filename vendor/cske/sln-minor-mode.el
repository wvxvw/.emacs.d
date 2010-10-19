;;; sln-minor-mode.el --- Minor mode to allow compilation of .NET 
;;; projects from c# source files

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

(defvar sln-minor-mode nil
  "Mode variable for sln-minor-mode")
(make-variable-buffer-local 'sln-minor-mode)

(defvar sln-minor-mode-map nil
  "Keymap for sln-mode")
(setq sln-minor-mode-map nil)
(unless sln-minor-mode-map
  (setq sln-minor-mode-map (make-sparse-keymap))

  (define-key sln-minor-mode-map [(control c) ?b] 'msbuild)
  (define-key sln-minor-mode-map [(control c) ?r] 'msbuild-rebuild)
  (define-key sln-minor-mode-map [(control c) ?c] 'msbuild-clean)

  (define-key sln-minor-mode-map [menu-bar sln-minor-build] 
    (list 'menu-item "Build" (make-sparse-keymap)))
 
  (define-key sln-minor-mode-map [menu-bar sln-minor-build sln-minor-clean] 
    '("Clean project (solution with C-u)" . msbuild-clean))

  (define-key sln-minor-mode-map [menu-bar sln-minor-build sln-minor-rebuild] 
    '("Rebuild project (solutionwith C-u)" . msbuild-rebuild))

  (define-key sln-minor-mode-map [menu-bar sln-minor-build sln-minor-build] 
    '("Build project  (solution with C-u)" . msbuild))


  (define-key sln-minor-mode-map [menu-bar sln-minor-build sln-minor-jump] 
    '("Go to project file" . sln-minor-goto-project))

  (if (not (assq 'sln-minor-mode minor-mode-map-alist))
      (setq minor-mode-map-alist
            (cons (cons 'sln-minor-mode sln-minor-mode-map)
                  minor-mode-map-alist)))) 

(defun -sln-minor-find-project-candidates(filename)
  (let ((f (file-expand-wildcards "*.csproj")))
    (if f f (error "No candidates"))))

(defun sln-minor-find-project()
  "Searchs all open buffers for the Project buffer for current
file. If cannot find it, prompt the user. Returns nil if cannot
find it."
  (catch t
    (let ((filename (buffer-file-name)))
      (dolist (b (buffer-list))
        (with-current-buffer b
          (and (eq major-mode 'csproj-mode)
               (csproj-file-exists filename)
               (throw t b))))
      ;; If we get here, did not find the buffer
      (when (y-or-n-p 
           (concat
            "msbuild: Can't find csproj buffer. Try to find it? "))
        (let ((prj
               (ido-completing-read 
                "Project candidates: "
                (-sln-minor-find-project-candidates filename))))
          (and prj (find-file-noselect prj)))))))

(defun sln-minor-goto-project(&optional dont-switch)
  "Searchs open buffers for the csproj file corresponding to this
source file. If cannot find it, try to guess and prompt the user"
  (interactive)
  (let ((prj (sln-minor-find-project)))
    (if prj
        (funcall (if dont-switch 'set-buffer 'switch-to-buffer) prj)
    (error "Can't find project buffer"))))

(defun sln-minor-mode(&optional arg)
  "sln minor mode"
  (interactive "P")
  (setq sln-minor-mode
        (if (null arg)
            (not sln-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if sln-minor-mode
      (progn
        (if (not (assq 'sln-minor-mode minor-mode-alist))
            (setq minor-mode-alist
                  (cons '(sln-minor-mode " sln")
                        minor-mode-alist)))
        (message "SLN minor on")
      )
    (message "SLN minor off")))
