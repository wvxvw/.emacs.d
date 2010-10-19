;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flymake minor mode - tweaks for csharp
;
; Flymake is built-in to emacs.  It  more-or-less continually compiles an
; active buffer when the minor mode is enabled. It also flags broken lines
; in the compile as you type.
;
; This is a set of tweaks of flymake for C# on Windows.
;
; last saved Time-stamp: <Wednesday, April 23, 2008  13:44:26  (by dinoch)>
;

(require 'flymake) 

(setq flymake-log-level 0)  ;; insure flymake errors get plopped into the *Messages* buffer

;; There are 2 common ways to build C# files: nmake or msbuild.
;; Here are examples for either. 
;;
;; For makefile, use nmake.  Configure this stanza to specify where your
;; nmake is.  Usually it is in the .NET 2.0 SDK directory, or the platform SDK
;; directory.
;;
;; If you use nmake, then you need a make target like this in your makefile:
;;
;; check-syntax:
;;      $(_CSC) /t:module $(CHK_SOURCES)
;;
;; (You could also put this in an alternatively named makefile,
;; like makefile.flymake. In this case you would also need to modify
;; the nmake command line (See below))
;;
;; If you use msbuild, and you are compiling projects that consist of a single
;; source file, you can use a standard (boilerplate) build project
;; file.  Call it msbuild.flymake.xml, and define it like this:
;;
;; <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003"
;;         DefaultTargets="CompileAll"
;;         ToolsVersion="3.5"
;;       >
;;
;;  <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" />
;;
;;  <!-- specify reference assemblies for all builds in this project -->
;;  <ItemGroup>
;;    <Reference Include="mscorlib" />
;;    <Reference Include="System" />
;;    <Reference Include="System.Core" />
;;    <Reference Include="System.Data" />
;;    <Reference Include="System.Data.Linq" />                   <!-- LINQ -->
;;    <!--Reference Include="System.ServiceModel" /-->           <!-- WCF -->
;;    <!--Reference Include="System.ServiceModel.Web" /-->       <!-- WCF -->
;;    <!--Reference Include="System.Runtime.Serialization" /-->  <!-- WCF -->
;;  </ItemGroup>
;;
;;  <Target Name="CheckSyntax"
;;          DependsOnTargets="ResolveAssemblyReferences"
;;        >
;;    <CSC 
;;       Sources="$(SourceFileToCheck)"
;;       References="@(ReferencePath)"
;;       TargetType="module"
;;       Toolpath="$(MSBuildToolsPath)"
;;       Nologo="true"
;;       />
;;  </Target>
;;
;; </Project>
;;
;; -ends-
;;
;; (This msbuild file works only with .NET 3.5.)
;;

;; If your projects consist of multiple source files, then you need to get fancier.
;; You need to compile all files, *except* for the original source file, the one
;; being edited currently. In this case, your msbuild.flymake.xml file should look
;; something like this: 

;; <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003"
;;          DefaultTargets="CompileAll"
;;          ToolsVersion="3.5"
;;          >
;; 
;;   <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" />
;; 
;;    <PropertyGroup>
;;       <Optimize>false</Optimize>
;;       <DebugSymbols>true</DebugSymbols>
;;       <!-- <OutputPath>.\bin\</OutputPath>  -->
;;       <OutputPath>.\</OutputPath>
;;       <OutDir>.\</OutDir>
;;       <IntermediateOutputPath>.\obj\</IntermediateOutputPath>
;;    </PropertyGroup>
;; 
;;   <!-- specify reference assemblies for all builds in this project -->
;;   <ItemGroup>
;;     <Reference Include="mscorlib" />
;;     <Reference Include="System" />
;;     <Reference Include="System.Core" />
;;     <Reference Include="System.Data" />
;;     <Reference Include="System.Data.Linq" />                   <!-- LINQ -->
;;     <!--Reference Include="System.ServiceModel" /-->           <!-- WCF -->
;;     <!--Reference Include="System.ServiceModel.Web" /-->       <!-- WCF -->
;;     <!--Reference Include="System.Runtime.Serialization" /-->  <!-- WCF -->
;;   </ItemGroup>
;; 
;;   <!-- This ItemGroup includes every .cs source file in the directory,           -->
;;   <!-- except for the one indicated by OriginalSourceFile.  In flymake, that     -->
;;   <!-- property indicates the currently edited file. So the result is that the   -->
;;   <!-- ItemGroup CSFile will include all files, including the _flymake.cs clone, -->
;;   <!-- but not including the original file.  Which is what we want.              -->
;;   <ItemGroup>
;;     <CSFile Include="*.cs" Exclude="$(OriginalSourceFile)" />
;;   </ItemGroup>
;; 
;;   <!-- Stuff the OriginalSourceFile property into an ItemGroup.                  -->
;;   <!-- We do this so we can get at the metadata, which I Think is available only -->
;;   <!-- through an item within an ItemGroup.  We want the root filename, which    -->
;;   <!-- we use to name the output netmodule.                                      -->
;;   <ItemGroup>
;;     <ExcludedCSFile Include="$(OriginalSourceFile)" />
;;   </ItemGroup>
;; 
;;   <Target Name="CheckSyntax"
;;           DependsOnTargets="ResolveAssemblyReferences"
;;           >
;;     <!-- Run the Visual C# compilation on the specified set of .cs files. -->
;;     <CSC 
;;        Sources="@(CSFile)"
;;        References="@(ReferencePath)"
;;        TargetType="module"
;;        Toolpath="$(MSBuildToolsPath)"
;;        OutputAssembly="%(ExcludedCSFile.Filename)_flymake.netmodule"
;;        Nologo="true"
;;        />
;;   </Target>
;; 
;; </Project>
;; 


;; These variables are ones I made up for help with C#:
(defvar dino-flymake-netsdk-location "c:\\netsdk2.0"
  "Location of .NET SDK, for finding nmake.exe.  The nmake is found in the bin subdir.  Example value is: c:\\Program Files\\Microsoft Visual Studio 8\\SDK\\v2.0 .")

(defvar dino-flymake-msbuild-location "c:\\.net3.5"
  "Directory containing MSBuild.exe.  Typically, c:\\windows\\Microsoft.NET\\Framework\\v3.5 .")

(defvar dino-flymake-csharp-msbuild-buildfile "msbuild.flymake.xml"
  "Build file if using MSBuild.exe.")

(defvar dino-flymake-csharp-nmake-buildfile "makefile"
  "Build file if using nmake.exe.")

(defvar dino-flymake-csharp-use-msbuild t
  "If t, then flymake uses msbuild.exe and the msbuild.flymake.xml
file.  If nil, then flymake uses nmake and the makefile with a
check-status target. Keep in mind the buildfile for either msbuild or nmake
is customizable.  See the vars dino-flymake-csharp-{nmake,msbuild}-buildfile .")

(defun dino-flymake-csharp-cleanup ()
  "Delete the temporary .netmodule file created in syntax checking,
then call through to flymake-simple-cleanup."
  (if flymake-temp-source-file-name
  (let* ((netmodule-name
          (concat (file-name-sans-extension flymake-temp-source-file-name)
                              ".netmodule"))
         (expanded-netmodule-name (expand-file-name netmodule-name "."))
         )
    (if (file-exists-p expanded-netmodule-name)
        (flymake-safe-delete-file expanded-netmodule-name)
      )
    )
  )
    (flymake-simple-cleanup)
    
  )



(defun dino-flymake-csharp-buildfile ()
  (if dino-flymake-csharp-use-msbuild
      dino-flymake-csharp-msbuild-buildfile
    dino-flymake-csharp-nmake-buildfile
    )
  )


(defun dino-flymake-find-csharp-buildfile (source-file-name)
  (let ((actual-build-file-name (dino-flymake-csharp-buildfile)))
    (if (file-exists-p (expand-file-name actual-build-file-name "."))
        "."
      (flymake-log 1 "no buildfile (%s) for %s" actual-build-file-name source-file-name)
      (flymake-report-fatal-status
       "NOMK" (format "No buildfile (%s) found for %s"
                      actual-build-file-name source-file-name))
      nil
      )

  )
  )


;(debug-on-entry 'flymake-create-temp-inplace)

(defun dino-flymake-csharp-init ()
  (dino-flymake-csharp-init-impl 'flymake-create-temp-inplace t t  'flymake-get-make-cmdline))


(defun dino-flymake-csharp-init-impl (create-temp-f use-relative-base-dir use-relative-source get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
         (source-file-name   buffer-file-name)
         (buildfile-dir      (dino-flymake-find-csharp-buildfile source-file-name)))
    (if buildfile-dir
        (let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
          (setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
                                                            use-relative-base-dir use-relative-source
                                                            get-cmdline-f))))
    args))

;(debug-on-entry 'dino-flymake-csharp-init)



; This fixup sets flymake to use a different cleanup routine for c# compiles
(defun dino-fixup-flymake-for-csharp ()
  (let (elt 
        (csharp-entry nil)
        (masks flymake-allowed-file-name-masks)
        )

    ;; The "flymake-allowed-file-name-masks" variable stores a filename pattern as
    ;; well as the make-init function, and a cleanup function.  In the case of csharp,
    ;; the setting in flymake.el has the cleanup fn as nil, which means it gets the
    ;; standard cleanup : the *_flymake.cs cloned source file gets deleted.  But the
    ;; way I have done the syntax checking, I compile the .cs file into a module,
    ;; which needs to be deleted afterwards.
    ;;
    
    ;; Here, we remove the C# entry in the "flymake-allowed-file-name-masks"
    ;; variable, and replace it with an entry that includes a custom csharp cleanup
    ;; routine.  In that cleanup routine, I delete the .netmodule file.

    ;; I could just setq the "flymake-allowed-file-name-masks" var to the C# thing I
    ;; want, but that would obliterate all the masks for all other languages, which
    ;; would be bad manners.

    ;; You know, come to think of it, I could just delete the generated .netmodule
    ;; file in the msbuild or makefile.  That might be simpler.

    ;; But the main point is this ought to be more easily configurable or customizable
    ;; in flymake.el.  And also, flymake ought to do something reasonable for csharp builds,
    ;; rather than completely punt.
    
    ;; This fixup is really hacky, relying on the string that is used for csharp in
    ;; flymake.el.  But it will do for now...

    ;; Find the entry
    (while (consp masks)
      (setq elt (car masks))
      (if (string= "\\.cs\\'" (car elt))
          (setq csharp-entry elt)
        )
      (setq masks (cdr masks))
      )
    
    ;;  remove the original one ...
    (if csharp-entry
        (setq flymake-allowed-file-name-masks
              (delete csharp-entry flymake-allowed-file-name-masks)))

    ;; Now add a new one, with the custom cleanup method.
    (setq flymake-allowed-file-name-masks
          (cons 
           '("\\.cs\\'" dino-flymake-csharp-init dino-flymake-csharp-cleanup)
           flymake-allowed-file-name-masks))
    )
  )

; need to do this only once, not every time csharp-mode is invoked
(dino-fixup-flymake-for-csharp) 


; This method re-defines the defun shipped in flymake, for csharp.  Re-defining
; this function *will* definitely break flymake for all other languages.  One
; way to fix that problem is to make the "get-make-cmdline" function a
; configurable hook within flymake!  

(defun flymake-get-make-cmdline (source base-dir)
  (if dino-flymake-csharp-use-msbuild
      (list (concat  dino-flymake-msbuild-location "\\msbuild.exe")
            (list (concat base-dir "/" (dino-flymake-csharp-buildfile))
                  "/nologo"
                  "/t:CheckSyntax"
                  "/v:quiet"  ;; normal
                  ;; use file-relative-name to remove the fully-qualified directory name 
                  (concat "/property:SourceFileToCheck=" (file-relative-name source))
                  (concat "/property:OriginalSourceFile=" (file-relative-name buffer-file-name))
                  ))
    
    (list (concat  dino-flymake-netsdk-location "\\bin\\nmake.exe")
        (list "/f"
              (concat base-dir "/" (dino-flymake-csharp-buildfile))
              (concat "CHK_SOURCES=" source)
              "SYNTAX_CHECK_MODE=1"
              "check-syntax"))

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flymake-for-csharp)

;;; end of flymake-for-csharp.el
