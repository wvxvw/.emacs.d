;;; msbuild.el --- Invoke msbuild to build a .NET project

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

(defconst -msbuild-std-args 
  "/nologo /verbosity:quiet /property:GenerateFullPaths=true"
  "Fixed arguments passed to msbuild")

(defun -msbuild-get-target-verb(target)
  "Gets the word (verb) to print for the action associated to the
symbol passed. 

'build -> \"Building\" 
'rebuild -> \"Rebuilding\"
'clean -> \"Cleaning\""
  (if (symbolp target)
      (case target
        ('build "Building") ('rebuild "Rebuilding") ('clean "Cleaning")
        (t (symbol-name target)))
    "?????????????"))

(defun -msbuild-escape-project-name(n)
  "Escape the passed project name to be uses as a target on a
msbuild invocation. Dots must be replaced with underscores."

  ;; If the project name contains a ., replace it with a _
  ;; If the project name contains a _, do nothing
  ;; If you have projects fo.o and fo_o, run!
  (while (string-match "\\." n)
    (setq n (replace-match "_" nil nil n)))
  n)

(defun msbuild-solution(p &optional target)
  "Runs msbuild for current buffer, which must be a buffer in
sln-mode. Target may be 'build, 'rebuild or 'clean. 'build is the
default. When prefix argument exists, build only project at point" 
  (interactive "P")
  (unless target
    (setq target 'build))
  (let* ((target-str (symbol-name target))
         (verb (-msbuild-get-target-verb target))
         (msg (format "%s solution %s" 
                      verb 
                      (propertize sln-solution-name 'face 
                                  'font-lock-variable-name-face))))
    ;; To build a project, make target = project name : target
    ;; unless target is 'build, in that case use only project name.
    (when p
      (let ((prj (sln-get-name-of-project-at-point)))
        (setq target-str (-msbuild-escape-project-name prj))
        (setq msg (format "%.s project %s from solution %s" 
                          verb 
                          (propertize prj 'face 'font-lock-keyword-face)
                          (propertize sln-solution-name 'face 
                                      'font-lock-variable-name-face)))
        (unless (eq target 'build) 
          (setq target-str (concat target-str ":" (symbol-name target))))))
    (if (and (boundp 'sln-solution-name) sln-solution-name)
        (msbuild-run-msbuild msg target-str (buffer-file-name))
      (error "This is not a solution file"))))

(defun msbuild-run-msbuild(msg target file-name)
  "Runs msbuild. file-name is the build file passed to msbuild"
  (with-current-buffer
      (compile (format "msbuild \"%s\" %s /t:%s" file-name 
                       -msbuild-std-args target))
    (setq header-line-format (format " %s" msg)))
 (message "%s..." msg))

(defun msbuild-csproj(p &optional target)
  "Runs msbuild for current buffer, which must be a buffer in
csproj-mode. Target may be 'build, 'rebuild or 'clean. 'build is
the default. When prefix argument exists, build the entire
solution, not only the current project. The corresponding .sln
file must be open"
  (interactive "P")
  (let ((n csproj-project-name))
    (save-excursion
      (csproj-goto-solution t)
      (sln-move-point-to-project-by-name n)
      (msbuild-solution p target))))

(defun msbuild-source-file(p &optional target)
  "Runs msbuild for current buffer, which must be a source code
buffer. Target may be 'build, 'rebuild or 'clean. 'build is the
default. When prefix argument exists, build the entire solution,
not only the project this file belongs to. The corresponding
.csproj and .sln files must be open"
  (interactive "P")
  (save-excursion
    (sln-minor-goto-project t)
    (msbuild-csproj (not p) target)))

(defun msbuild(p &optional target)
  "Calls msbuils-solution, msbuild-csproj or msbuild-source-file
depending on the major mode of current buffer"
  (interactive "P")
  (case major-mode
    ('csharp-mode (msbuild-source-file p target))
    ('csproj-mode (msbuild-csproj p target))
    ('sln-mode (msbuild-solution p target))
    (t (error "Don't know how to build from here"))))

(defun msbuild-rebuild(p)
  "Calls msbuild with the 'rebuild target. Convenience function
to hook to a key."
  (interactive "P")
  (msbuild p 'rebuild))

(defun msbuild-clean(p)
  "Class msbuils with the 'clean target. Convenience function
to hook to a key."
  (interactive "P")
  (msbuild p 'clean))

;; Setup compile mode to show errors from the C# compiler and the
;; build tool.

(add-to-list                            ; CSC errors
 'compilation-error-regexp-alist 
 '("[ \t]*\\(\\([_a-zA-Z:\]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): error CS[0-9]+:" 
   1 3 4 2))

(add-to-list                            ; CSC warnings
 'compilation-error-regexp-alist 
 '("[ \t]*\\(\\([_a-zA-Z:\]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): warning CS[0-9]+:"
   1 3 4 1))

(add-to-list                            ; MSBuild errors. Will probably link to the XML build file.
 'compilation-error-regexp-alist 
 '("[ \t]*\\(\\([_a-zA-Z:\]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): error MSB[0-9]+:" 
   1 3 4 2))

(add-to-list                            ; MSBuild warnings. Will probably link to the XML build file.
 'compilation-error-regexp-alist 
 '("[ \t]*\\(\\([_a-zA-Z:\]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): warning MSB[0-9]+:" 
   1 3 4 1))
