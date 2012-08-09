;;; ede/generic.el --- Base Support for generic build systems

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; There are a lot of build systems out there, and EDE can't support
;; them all fully.  The ede/generic.el system is the base for
;; supporting alternate build systems in a simple way, automatically.
;;
;; The structure is for the ede-generic baseclass, which is augmented
;; by simple sub-classes that can be created by users on an as needed
;; basis.  The generic system will have targets for many language
;; types, and create the targets on an as needed basis.  All
;; sub-project types will recycle the same generic target types.
;;
;; The generic target types will only be implemented for languages
;; where having EDE support actually matters, with a single MISC to
;; represent anything else.
;;
;; TOO MANY PROJECTS DETECTED:
;;
;; If enabling ede-generic support starts identifying too many
;; projects, drop a file called `.ede-ignore' into any directory where
;; you do not want a project to be.
;;
;; Customization:
;;
;; Since these projects are all so incredibly generic, a user will
;; need to configure some aspects of the project by hand.  In order to
;; enable this without configuring the project objects directly (which
;; are auto-generated) a special ede-generic-config object is defined to
;; hold the basics.  Generic projects will identify and use these
;; config files.
;;
;; Adding support for new projects:
;;
;; To add support to EDE Generic for new project types is very quick.
;; See the end of this file for examples such as CMake and SCons.
;;
;; Support consists of one class for your project, specifying the file
;; name used by the project system you want to support.  It also
;; should implement th method `ede-generic-setup-configuration' to
;; prepopulate the configurable portion of the generic project with
;; build details.
;;
;; Lastly, call `ede-generic-new-autoloader' to setup your project so
;; EDE can use it.
;;
;; Adding support for new types of source code:
;;
;; Sources of different types are supported with a simple class which
;; subclasses `ede-generic-target'.  The slots `shortname' and
;; `extension' should be given new initial values.
;;
;; Optionally, any target method used by EDE can then be overridden.
;; The ede-generic-target-c-cpp has some example methods setting up
;; the pre-processor map and system include path.
;;
;; NOTE: It is not necessary to modify ede-generic.el to add any of
;; the above described support features.

(require 'eieio-opt)
(require 'ede)
(require 'semantic/db)

;;; Code:
;;
;; Start with the configuration system
(defclass ede-generic-config (eieio-persistent)
  ((extension :initform ".ede")
   (file-header-line :initform ";; EDE Generic Project Configuration")
   (project :initform nil
	    :documentation
	    "The project this config is bound to.")
   ;; Generic customizations
   (build-command :initarg :build-command
		  :initform "make -k"
		  :type string
		  :custom string
		  :group (default build)
		  :documentation
		  "Command used for building this project.")
   (debug-command :initarg :debug-command
		  :initform "gdb "
		  :type string
		  :custom string
		  :group (default build)
		  :documentation
		  "Command used for debugging this project.")
   ;; C target customizations
   (c-include-path :initarg :c-include-path
		   :initform nil
		   :type list
		   :custom (repeat (string :tag "Path"))
		   :group c
		   :documentation
		   "The include path used by C/C++ projects.")
   (c-preprocessor-table :initarg :c-preprocessor-table
			 :initform nil
			 :type list
			 :custom (repeat (cons (string :tag "Macro")
					       (string :tag "Value")))
			 :group c
			 :documentation
			 "Preprocessor Symbols for this project.")
   (c-preprocessor-files :initarg :c-preprocessor-files
			 :initform nil
			 :type list
			 :custom (repeat (string :tag "Include File")))
   )
  "User Configuration object for a generic project.")

(defun ede-generic-load (dir &optional rootproj)
  "Return a Generic Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  ;; Doesn't already exist, so let's make one.
  (let* ((alobj ede-constructing)
	 (this nil))
    (when (not alobj) (error "Cannot load generic project without the autoload instance"))

    (setq this
	  (funcall (oref alobj class-sym)
		   (symbol-name (oref alobj class-sym))
		   :name (file-name-nondirectory
			  (directory-file-name dir))
		   :version "1.0"
		   :directory (file-name-as-directory dir)
		   :file (expand-file-name (oref alobj :proj-file)) ))
    (ede-add-project-to-global-list this)
    ))

;;; Base Classes for the system
(defclass ede-generic-target (ede-target)
  ((shortname :initform ""
	     :type string
	     :allocation :class
	     :documentation
	     "Something prepended to the target name.")
  (extension :initform ""
	      :type string
	      :allocation :class
	      :documentation
	      "Regular expression representing the extension used for this target.
subclasses of this base target will override the default value.")
  )
  "Baseclass for all targets belonging to the generic ede system."
  :abstract t)

(defclass ede-generic-project (ede-project)
  ((buildfile :initform ""
	      :type string
	      :allocation :class
	      :documentation "The file name that identifies a project of this type.
The class allocated value is replace by different sub classes.")
   (config :initform nil
	   :type (or null ede-generic-config)
	   :documentation
	   "The configuration object for this project.")
   )
  "The baseclass for all generic EDE project types."
  :abstract t)

(defmethod initialize-instance ((this ede-generic-project)
				&rest fields)
  "Make sure the targets slot is bound."
  (call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))
  )

(defmethod ede-generic-get-configuration ((proj ede-generic-project))
  "Return the configuration for the project PROJ."
  (let ((config (oref proj config)))
    (when (not config)
      (let ((fname (expand-file-name "EDEConfig.el"
				     (oref proj :directory))))
	(if (file-exists-p fname)
	    ;; Load in the configuration
	    (setq config (eieio-persistent-read fname))
	  ;; Create a new one.
	  (setq config (ede-generic-config
			"Configuration"
			:file fname))
	  ;; Set initial values based on project.
	  (ede-generic-setup-configuration proj config))
	;; Link things together.
	(oset proj config config)
	(oset config project proj)))
    config))

(defmethod ede-generic-setup-configuration ((proj ede-generic-project) config)
  "Default configuration setup method."
  nil)

(defmethod ede-commit-project ((proj ede-generic-project))
  "Commit any change to PROJ to its file."
  (let ((config (ede-generic-get-configuration proj)))
    (ede-commit config)))

;;; A list of different targets
(defclass ede-generic-target-c-cpp (ede-generic-target)
  ((shortname :initform "C/C++")
   (extension :initform "\\([ch]\\(pp\\|xx\\|\\+\\+\\)?\\|cc\\|hh\\|CC?\\)"))
  "EDE Generic Project target for C and C++ code.
All directories need at least one target.")

(defclass ede-generic-target-el (ede-generic-target)
  ((shortname :initform "ELisp")
   (extension :initform "el"))
  "EDE Generic Project target for Emacs Lisp code.
All directories need at least one target.")

(defclass ede-generic-target-fortran (ede-generic-target)
  ((shortname :initform "Fortran")
   (extension :initform "[fF]9[05]\\|[fF]\\|for"))
  "EDE Generic Project target for Fortran code.
All directories need at least one target.")

(defclass ede-generic-target-texi (ede-generic-target)
  ((shortname :initform "Texinfo")
   (extension :initform "texi"))
  "EDE Generic Project target for texinfo code.
All directories need at least one target.")

;; MISC must always be last since it will always match the file.
(defclass ede-generic-target-misc (ede-generic-target)
  ((shortname :initform "Misc")
   (extension :initform ""))
  "EDE Generic Project target for Misc files.
All directories need at least one target.")

;;; Automatic target acquisition.
(defun ede-generic-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
		 (string= (oref T :path) dir))
	(setq match T)
      ))
    match))

(defmethod ede-find-target ((proj ede-generic-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
	 (classes (eieio-build-class-alist 'ede-generic-target t))
	 (cls nil)
	 (targets (oref proj targets))
	 (dir default-directory)
	 (ans nil)
	 )
    ;; Pick a matching class type.
    (when ext
      (dolist (C classes)
	(let* ((classsym (intern (car C)))
	       (extreg (oref classsym extension)))
	  (when (and (not (string= extreg ""))
		     (string-match (concat "^" extreg "$") ext))
	    (setq cls classsym)))))
    (when (not cls) (setq cls 'ede-generic-target-misc))
    ;; find a pre-existing matching target
    (setq ans (ede-generic-find-matching-target cls dir targets))
    ;; Create a new instance if there wasn't one
    (when (not ans)
      (setq ans (make-instance
		 cls
		 :name (oref cls shortname)
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; C/C++ support
(defmethod ede-preprocessor-map ((this ede-generic-target-c-cpp))
  "Get the pre-processor map for some generic C code."
  (let* ((proj (ede-target-parent this))
	 (root (ede-project-root proj))
	 (config (ede-generic-get-configuration proj))
	 filemap
	 )
    ;; Preprocessor files
    (dolist (G (oref config :c-preprocessor-files))
      (let ((table (semanticdb-file-table-object
		    (ede-expand-filename root G))))
	(when table
	  (when (semanticdb-needs-refresh-p table)
	    (semanticdb-refresh-table table))
	  (setq filemap (append filemap (oref table lexical-table)))
	  )))
    ;; The core table
    (setq filemap (append filemap (oref config :c-preprocessor-table)))

    filemap
    ))

(defmethod ede-system-include-path ((this ede-generic-target-c-cpp))
  "Get the system include path used by project THIS."
  (let* ((proj (ede-target-parent this))
	(config (ede-generic-get-configuration proj)))
    (oref config c-include-path)))

;;; Customization
;;
(defmethod ede-customize ((proj ede-generic-project))
  "Customize the EDE project PROJ."
  (let ((config (ede-generic-get-configuration proj)))
    (eieio-customize-object config)))

(defmethod ede-customize ((target ede-generic-target))
  "Customize the EDE TARGET."
  ;; Nothing unique for the targets, use the project.
  (ede-customize-project))

(defmethod eieio-done-customizing ((config ede-generic-config))
  "Called when EIEIO is done customizing the configuration object.
We need to go back through the old buffers, and update them with
the new configuration."
  (ede-commit config)
  ;; Loop over all the open buffers, and re-apply.
  (ede-map-targets
   (oref config project)
   (lambda (target)
     (ede-map-target-buffers
      target
      (lambda (b)
	(with-current-buffer b
	  (ede-apply-target-options)))))))

(defmethod ede-commit ((config ede-generic-config))
  "Commit all changes to the configuration to disk."
  (eieio-persistent-save config))

;;; Creating Derived Projects:
;;
;; Derived projects need an autoloader so that EDE can find the
;; different projects on disk.
(defun ede-generic-new-autoloader (internal-name external-name
						 projectfile class)
  "Add a new EDE Autoload instance for identifying a generic project.
INTERNAL-NAME is a long name that identifies this project type.
EXTERNAL-NAME is a shorter human readable name to describe the project.
PROJECTFILE is a file name that identifies a project of this type to EDE, such as
a Makefile, or SConstruct file.
CLASS is the EIEIO class that is used to track this project.  It should subclass
the class `ede-generic-project' project."
  (add-to-list 'ede-project-class-files
	       (ede-project-autoload internal-name
				     :name external-name
				     :file 'ede/generic
				     :proj-file projectfile
				     :load-type 'ede-generic-load
				     :class-sym class
				     :new-p nil)
	       ;; Generics must go at the end, since more specific types
	       ;; can create Makefiles also.
	       t))

;;;###autoload
(defun ede-enable-generic-projects ()
  "Enable generic project loaders."
  (interactive)
  (ede-generic-new-autoloader "edeproject-makefile" "Make"
			      "Makefile" 'ede-generic-makefile-project)
  (ede-generic-new-autoloader "edeproject-scons" "SCons"
			      "SConstruct" 'ede-generic-scons-project)
  (ede-generic-new-autoloader "edeproject-cmake" "CMake"
			      "CMakeLists" 'ede-generic-cmake-project)
  )


;;; SPECIFIC TYPES OF GENERIC BUILDS
;;

;;; MAKEFILE

(defclass ede-generic-makefile-project (ede-generic-project)
  ((buildfile :initform "Makefile")
   )
  "Generic Project for makefiles.")

(defmethod ede-generic-setup-configuration ((proj ede-generic-makefile-project) config)
  "Setup a configuration for Make."
  (oset config build-command "make -k")
  (oset config debug-command "gdb ")
  )


;;; SCONS
(defclass ede-generic-scons-project (ede-generic-project)
  ((buildfile :initform "SConstruct")
   )
  "Generic Project for scons.")

(defmethod ede-generic-setup-configuration ((proj ede-generic-scons-project) config)
  "Setup a configuration for SCONS."
  (oset config build-command "scons")
  (oset config debug-command "gdb ")
  )


;;; CMAKE
(defclass ede-generic-cmake-project (ede-generic-project)
  ((buildfile :initform "CMakeLists")
   )
  "Generic Project for cmake.")

(defmethod ede-generic-setup-configuration ((proj ede-generic-cmake-project) config)
  "Setup a configuration for CMake."
  (oset config build-command "cmake")
  (oset config debug-command "gdb ")
  )

(provide 'ede/generic)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/generic"
;; End:

;;; ede/generic.el ends here
