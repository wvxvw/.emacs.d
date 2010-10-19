;;; sln-mode.el --- Mode to handle .NET Solution files

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

(defconst -sln-guid-csharp    "FAE04EC0-301F-11D3-BF4B-00C04F79EFBC"
  "Guid used to identify C# projects in solution files")
(defconst -sln-guid-slnfolder "2150E333-8FDC-42A3-9474-1A3956D46DE8"
  "Guid used to identify general purpose folders in solution files")

 (defvar -sln-keywords-regexp 
  (regexp-opt  
   '("ProjectSection" "EndProjectSection" "Project" "EndProject" "Global" 
     "EndGlobal" "GlobalSection" "EndGlobalSection" ) 'words)
  "Regular expression to match keywords for font-lock in sln-mode")

(defun -sln-create-font-lock-project-entry(guid-spec guid-replacement 
                                                     project-name-face)
  "Creates an entry for the font-lock-keywords variable. The
entry decorates a project line."
;; List construction is cumbersome, probably because I don't
;; understand very well how lists are supposed to work...
  (let (l x)
    (add-to-list 'l '(5 '(face font-lock-preprocessor-face display "GUID")))
    (add-to-list 'l '(4 '(face font-lock-preprocessor-face display "FILE")))
    (add-to-list 'l (list 3 project-name-face))
    (setq x '(list 'face))
    (add-to-list 'x project-name-face t)
    (add-to-list 'x '(quote display) t)
    (add-to-list 'x guid-replacement t)
    (add-to-list 'l (list 1 x))

    (add-to-list 'l 
     (concat 
      "^Project(\\(\"{?\\(" guid-spec  
      "\\)}?\"\\)) = \"\\([^\"]*\\)\", \\(\"[^\"]*\"\\)"
      ", \\(\"{?[-A-Z0-9]*}?\"\\)$"))
    l))

(defun -sln-init-font-lock()
  "Initialize font lock defaults for sln-mode"
  (set (make-local-variable 'font-lock-extra-managed-props) '(display))
  (setq -sln-mode-keywords
        `((,-sln-keywords-regexp . font-lock-keyword-face)
          ("Microsoft Visual Studio.*$" . font-lock-preprocessor-face)        
          ("^#.*$" . font-lock-preprocessor-face)
          ,(-sln-create-font-lock-project-entry -sln-guid-csharp " C# " 
                                                'font-lock-comment-face)
          ,(-sln-create-font-lock-project-entry -sln-guid-slnfolder " Fd " 
                                                'font-lock-string-face)))
  (setq font-lock-defaults '(-sln-mode-keywords t)))

(defvar sln-mode-hook nil "List of functions to call when entering sln-mode")

(defvar sln-mode-map nil "Keymap for sln-mode")

(setq sln-mode-map nil)

(unless sln-mode-map
  (setq sln-mode-map (make-sparse-keymap))
  (define-key sln-mode-map [down] 'sln-move-point-to-next-project) 
  (define-key sln-mode-map [up] 'sln-move-point-to-previous-project) 
  (define-key sln-mode-map [return] 'sln-open-project-at-point)

  (define-key sln-mode-map [(control c) ?b] 'msbuild)
  (define-key sln-mode-map [(control c) ?r] 'msbuild-rebuild)
  (define-key sln-mode-map [(control c) ?c] 'msbuild-clean)

  (define-key sln-mode-map [(control c) ?p] 
    'sln-ido-go-to-project)


  (define-key sln-mode-map [menu-bar sln-build] 
    (cons "Build" (make-sparse-keymap "Build" )))
 
  (define-key sln-mode-map [menu-bar sln-build sln-clean] 
    '("Clean solution (project at point with C-u)" . msbuild-clean))

  (define-key sln-mode-map [menu-bar sln-build sln-rebuild] 
    '("Rebuild solution (project at point with C-u)" . msbuild-rebuild))

  (define-key sln-mode-map [menu-bar sln-build sln-build] 
    '("Build solution  (project at point with C-u)" . msbuild))
)

(defun -sln-get-matched-project-info-index(component)
  "After matching a project entry with -sln-project-nav-regexp,
use this function to get the index of each piece of info, which
may be 'type, 'name or 'guid"
  (case component ('type 1) ('name 2) ('guid 3)
        (t (error "Wroing project component"))))
  
(defun -sln-get-matched-project-info(component &optional strip-properties)
  "After matching a project entry with -sln-project-nav-regexp,
use this function to get each piece of info, which may be 'type,
'name or 'guid"
  (funcall (if strip-properties 
               'match-string-no-properties
             'match-string)
           (-sln-get-matched-project-info-index component)))

(defun -sln-move-point-to-matched-project()
  "After matching a project entry with -sln-project-nav-regexp,
use this function to move point to this project."
  (message "--> %s -- %s" 
           (-sln-get-matched-project-info 'type)
           (-sln-get-matched-project-info 'name))
  (goto-char 
   (match-beginning 
    (-sln-get-matched-project-info-index 'name))))

(defconst -sln-project-nav-regexp 
  "^Project(\"{?\\([-A-Z0-9]*\\)}?\") = \"\\([^\"]*\\)\", \"\\([^\"]*\\)\", \"{?\\([-A-Z0-9]*\\)}?\"$"
  "Use this regexp to match project entries" )

(defun sln-move-point-to-next-project()
  "Find first project after point and move point there"
  (interactive)
  (if (re-search-forward -sln-project-nav-regexp nil t)
      (-sln-move-point-to-matched-project)
    (message "No more projects")))

(defun sln-move-point-to-previous-project()
  "Find first project before point a move point there"
  (interactive)
  (if (re-search-backward -sln-project-nav-regexp nil t)
      (-sln-move-point-to-matched-project)
    (message "No more projects")))

(defun sln-move-point-to-project-by-name(name)
  "Find project by name, and move point there if found"
  (catch t
    (dolist (l (sln-imenu-create-index t))
      (when (string= (car l) name)
        (goto-char (cdr l))
        (throw t t)))))

(defun -sln-imenu-create-index-entry(raw p ps &optional return-as-child)
  "Create an imenu entry."
  (let ((name (car p)) (location (cadr p)) (prjtype (caddr p)) 
        (prjguid (cadddr p)) (parentguid (nth 4 p)) children)

    ;; If this project has a parent and return-as-child is nil, return nil
    ;; It will be added by its parent.
    
    (if (and (null raw) parentguid (null return-as-child)) nil
      ;; Add some more info to the name if we can
      (if (and (null raw) (string= prjtype -sln-guid-csharp))
          (setq name (concat name " C#")))

      ;; Build a list with this item's children
      (dolist (q ps)
        (if (string= (nth 4 q) prjguid)
            (add-to-list 'children q)))

      (if (and (null raw) children)
          (let (ret)
            (dolist (q children)
              (add-to-list 'ret (-sln-imenu-create-index-entry raw q ps t)))
            (add-to-list 'ret name)
            ret)
        (cons name location)))))

(defun sln-imenu-create-index(&optional raw)
  "Create the imenu list. Is is also used by some other functions
here. If the `raw' parameter is not nil, don't create nested
lists, but put all projects on the same plain list."
  (let (projects ret)
    (goto-char (point-min))
    (while (re-search-forward -sln-project-nav-regexp nil t)
      ;; Prepare a list with: project name, buffer position, project
      ;; type guid, project guid, parent guid (or nil if no parent)
      (let ((project-guid (-sln-get-matched-project-info 'guid t)))
        (add-to-list 'projects (list (-sln-get-matched-project-info 'name t)
                                     (match-beginning 0)
                                     (-sln-get-matched-project-info 'type t)
                                     project-guid
                                     ;;(sln-project-parent project-guid)
                                     ))))

    (dolist (p projects)
      (let ((item (-sln-imenu-create-index-entry raw p projects)))
        (if item
            (add-to-list 'ret item))))
    ret))

(defun sln-get-project-list()
  "Gets a list of project names"
  (save-excursion
    (let (out
          (l (sln-imenu-create-index t)))
      (dolist (el l)
        (add-to-list 'out (car el)))
      out)))

(defun sln-project-exists(prj-name)
  "Returns t if project with name prj-name exists in the solution"
  (catch t
    (dolist (l (sln-get-project-list))
      (if (string= prj-name l)
          (throw t t)))))

(defun -sln-match-project-at-point()
  "Try to match with -sln-project-nav-regexp the project on the
line the point is"
  (save-excursion
    (beginning-of-line)
    (unless (looking-at -sln-project-nav-regexp)
      (error "Not at a project line"))))

(defun sln-open-project-at-point()
  "Open the .csproj file for project at point"
  (interactive)
  (-sln-match-project-at-point)
  (let ((guid (-sln-get-matched-project-info 'type t))
        (file (-sln-get-matched-project-info 'guid t)))
    (if (string= guid -sln-guid-csharp)
        (find-file file)
      (if (string= guid -sln-guid-slnfolder)
          (message "Nothing to with solution folders.")
        (message "Don't know about this kind of solution item")))))

(defun sln-get-name-of-project-at-point()
  "Returns the name of the project where point is"
  (-sln-match-project-at-point)
  (-sln-get-matched-project-info 'name t))

(defun sln-ido-go-to-project()
  "Select project by name with ido and move point there"
  (interactive)
  (sln-move-point-to-project-by-name
   (ido-completing-read "Project:" (sln-get-project-list) nil t)))


(defun sln-forward-element()
  (save-match-data
   (search-forward-regexp (regexp-opt '("EndGlobal" ) 'words))))

(define-derived-mode sln-mode fundamental-mode ".NET Solution"
  "This is a mode for Solution files from the .NET
Framework. Solution files have .sln as extension."

  (kill-all-local-variables)
  (-sln-init-font-lock)
  (setq mode-name ".NET Solution"
        major-mode 'sln-mode)
  (use-local-map sln-mode-map)
  (set (make-variable-buffer-local 'sln-solution-name)
       (if (buffer-file-name)
           (file-name-sans-extension 
            (file-name-nondirectory (buffer-file-name)))
         "?????????"))
  (save-excursion 
    (goto-char (point-min))
    ;; Set the header line
    (setq header-line-format 
          (format "sln %s        format %s"
                  (propertize sln-solution-name 'face 'bold)
                  (if (re-search-forward "Format Version \\(.*\\)$" nil t)
                      (match-string 1)
                    "????"))))
  (setq imenu-create-index-function 'sln-imenu-create-index)
  (imenu-add-to-menubar "Projects")

  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  
  (add-to-list 
   'hs-special-modes-alist
   '(sln-mode
     "\\<\\(Global\\)\\>"
     ""
     "#" ;; won't work on its own; uses syntax table
     (lambda (arg) (sln-forward-element))
     nil))
  (hs-minor-mode 1)

  (save-excursion
    (-csproj-hide-blocks "^Global$"))

  (run-hooks 'sln-mode-hook))




