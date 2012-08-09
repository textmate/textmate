;;; ede/base.el --- Baseclasses for EDE.

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Baseclasses for EDE.
;;
;; Contains all the base structures needed by EDE.

;;; Code:
(require 'eieio)
(require 'eieio-speedbar)
(require 'ede/auto)

;; Defined in ede.el:
(defvar ede-projects)
(defvar ede-object)
(defvar ede-object-root-project)

(declare-function data-debug-new-buffer "data-debug")
(declare-function data-debug-insert-object-slots "eieio-datadebug")
(declare-function ede-parent-project "ede" (&optional obj))
(declare-function ede-current-project "ede" (&optional dir))

;;; TARGET
;;
;; The TARGET is an entity in a project that knows about files
;; and features of those files.

(defclass ede-target (eieio-speedbar-directory-button)
  ((buttonface :initform speedbar-file-face) ;override for superclass
   (name :initarg :name
	 :type string
	 :custom string
	 :label "Name"
	 :group (default name)
	 :documentation "Name of this target.")
   ;; @todo - I think this should be "dir", and not "path".
   (path :initarg :path
	 :type string
	 ;:custom string
	 ;:label "Path to target"
	 ;:group (default name)
	 :documentation "The path to the sources of this target.
Relative to the path of the project it belongs to.")
   (source :initarg :source
	   :initform nil
	   ;; I'd prefer a list of strings.
	   :type list
	   :custom (repeat (string :tag "File"))
	   :label "Source Files"
	   :group (default source)
	   :documentation "Source files in this target.")
   (versionsource :initarg :versionsource
		  :initform nil
		  :type list
		  :custom (repeat (string :tag "File"))
		  :label "Source Files with Version String"
		  :group (source)
		  :documentation
		  "Source files with a version string in them.
These files are checked for a version string whenever the EDE version
of the master project is changed.  When strings are found, the version
previously there is updated.")
   ;; Class level slots
   ;;
   (sourcetype :allocation :class
	       :type list ;; list of symbols
	       :documentation
	       "A list of `ede-sourcecode' objects this class will handle.
This is used to match target objects with the compilers they can use, and
which files this object is interested in."
	       :accessor ede-object-sourcecode)
   (keybindings :allocation :class
		:initform (("D" . ede-debug-target))
		:documentation
"Keybindings specialized to this type of target."
		:accessor ede-object-keybindings)
   (menu :allocation :class
	 :initform ( [ "Debug target" ede-debug-target
		       (ede-buffer-belongs-to-target-p) ]
		     [ "Run target" ede-run-target
		       (ede-buffer-belongs-to-target-p) ]
		     )
	 :documentation "Menu specialized to this type of target."
	 :accessor ede-object-menu)
   )
  "A target is a structure that describes a file set that produces something.
Targets, as with 'Make', is an entity that will manage a file set
and knows how to compile or otherwise transform those files into some
other desired outcome.")

;;; PROJECT/PLACEHOLDER
;;
;; Project placeholders are minimum parts of a project used
;; by the project cache.  The project cache can refer to these placeholders,
;; and swap them out with the real-deal when that project is loaded.
;;
(defclass ede-project-placeholder (eieio-speedbar-directory-button)
  ((name :initarg :name
	 :initform "Untitled"
	 :type string
	 :custom string
	 :label "Name"
	 :group (default name)
	 :documentation "The name used when generating distribution files.")
   (version :initarg :version
	    :initform "1.0"
	    :type string
	    :custom string
	    :label "Version"
	    :group (default name)
	    :documentation "The version number used when distributing files.")
   (directory :type string
	      :initarg :directory
	      :documentation "Directory this project is associated with.")
   (dirinode :documentation "The inode id for :directory.")
   (file :type string
	 :initarg :file
	 :documentation "File name where this project is stored.")
   (rootproject ; :initarg - no initarg, don't save this slot!
    :initform nil
    :type (or null ede-project-placeholder-child)
    :documentation "Pointer to our root project.")
   )
  "Placeholder object for projects not loaded into memory.
Projects placeholders will be stored in a user specific location
and querying them will cause the actual project to get loaded.")

;;; PROJECT
;;
;; An EDE project controls a set of TARGETS, and can also contain
;; multiple SUBPROJECTS.
;;
;; The project defines a set of features that need to be built from
;; files, in addition as to controlling what to do with the file set,
;; such as creating distributions, compilation, and web sites.
;;
;; Projects can also affect how EDE works, by changing what appears in
;; the EDE menu, or how some keys are bound.
;;
(defclass ede-project (ede-project-placeholder)
  ((subproj :initform nil
	    :type list
	    :documentation "Sub projects controlled by this project.
For Automake based projects, each directory is treated as a project.")
   (targets :initarg :targets
	    :type list
	    :custom (repeat (object :objectcreatefcn ede-new-target-custom))
	    :label "Local Targets"
	    :group (targets)
	    :documentation "List of top level targets in this project.")
   (locate-obj :type (or null ede-locate-base-child)
	       :documentation
	       "A locate object to use as a backup to `ede-expand-filename'.")
   (tool-cache :initarg :tool-cache
	       :type list
	       :custom (repeat object)
	       :label "Tool: "
	       :group tools
	       :documentation "List of tool cache configurations in this project.
This allows any tool to create, manage, and persist project-specific settings.")
   (mailinglist :initarg :mailinglist
		:initform ""
		:type string
		:custom string
		:label "Mailing List Address"
		:group name
		:documentation
		"An email address where users might send email for help.")
   (web-site-url :initarg :web-site-url
		 :initform ""
		 :type string
		 :custom string
		 :label "Web Site URL"
		 :group name
		 :documentation "URL to this projects web site.
This is a URL to be sent to a web site for documentation.")
   (web-site-directory :initarg :web-site-directory
		       :initform ""
		       :custom string
		       :label "Web Page Directory"
		       :group name
		       :documentation
		       "A directory where web pages can be found by Emacs.
For remote locations use a path compatible with ange-ftp or EFS.
You can also use TRAMP for use with rcp & scp.")
   (web-site-file :initarg :web-site-file
		  :initform ""
		  :custom string
		  :label "Web Page File"
		  :group name
		  :documentation
		  "A file which contains the home page for this project.
This file can be relative to slot `web-site-directory'.
This can be a local file, use ange-ftp, EFS, or TRAMP.")
   (ftp-site :initarg :ftp-site
	     :initform ""
	     :type string
	     :custom string
	     :label "FTP site"
	     :group name
	     :documentation
	     "FTP site where this project's distribution can be found.
This FTP site should be in Emacs form, as needed by `ange-ftp', but can
also be of a form used by TRAMP for use with scp, or rcp.")
   (ftp-upload-site :initarg :ftp-upload-site
		    :initform ""
		    :type string
		    :custom string
		    :label "FTP Upload site"
		    :group name
		    :documentation
		    "FTP Site to upload new distributions to.
This FTP site should be in Emacs form as needed by `ange-ftp'.
If this slot is nil, then use `ftp-site' instead.")
   (configurations :initarg :configurations
		   :initform ("debug" "release")
		   :type list
		   :custom (repeat string)
		   :label "Configuration Options"
		   :group (settings)
		   :documentation "List of available configuration types.
Individual target/project types can form associations between a configuration,
and target specific elements such as build variables.")
   (configuration-default :initarg :configuration-default
			  :initform "debug"
			  :custom string
			  :label "Current Configuration"
			  :group (settings)
			  :documentation "The default configuration.")
   (local-variables :initarg :local-variables
		    :initform nil
		    :custom (repeat (cons (sexp :tag "Variable")
					  (sexp :tag "Value")))
		    :label "Project Local Variables"
		    :group (settings)
		    :documentation "Project local variables")
   (keybindings :allocation :class
		:initform (("D" . ede-debug-target)
			   ("R" . ede-run-target))
		:documentation "Keybindings specialized to this type of target."
		:accessor ede-object-keybindings)
   (menu :allocation :class
	 :initform
	 (
	  [ "Update Version" ede-update-version ede-object ]
	  [ "Version Control Status" ede-vc-project-directory ede-object ]
	  [ "Edit Project Homepage" ede-edit-web-page
	    (and ede-object (oref (ede-toplevel) web-site-file)) ]
	  [ "Browse Project URL" ede-web-browse-home
	    (and ede-object
		 (not (string= "" (oref (ede-toplevel) web-site-url)))) ]
	  "--"
	  [ "Rescan Project Files" ede-rescan-toplevel t ]
	  [ "Edit Projectfile" ede-edit-file-target
	    (ede-buffer-belongs-to-project-p) ]
	  )
	 :documentation "Menu specialized to this type of target."
	 :accessor ede-object-menu)
   )
  "Top level EDE project specification.
All specific project types must derive from this project."
  :method-invocation-order :depth-first)

;;; Important macros for doing commands.
;;
(defmacro ede-with-projectfile (obj &rest forms)
  "For the project in which OBJ resides, execute FORMS."
  (list 'save-window-excursion
	(list 'let* (list
		     (list 'pf
			   (list 'if (list 'obj-of-class-p
					   obj 'ede-target)
				 ;; @todo -I think I can change
				 ;; this to not need ede-load-project-file
				 ;; but I'm not sure how to test well.
				 (list 'ede-load-project-file
				       (list 'oref obj 'path))
				 obj))
		     '(dbka (get-file-buffer (oref pf file))))
	      '(if (not dbka) (find-file (oref pf file))
		 (switch-to-buffer dbka))
	      (cons 'progn forms)
	      '(if (not dbka) (kill-buffer (current-buffer))))))
(put 'ede-with-projectfile 'lisp-indent-function 1)

;;; The EDE persistent cache.
;;
;; The cache is a way to mark where all known projects live without
;; loading those projects into memory, or scanning for them each time
;; emacs starts.
;;
(defcustom ede-project-placeholder-cache-file
  (locate-user-emacs-file "ede-projects.el" ".projects.ede")
  "File containing the list of projects EDE has viewed."
  :group 'ede
  :type 'file)

(defvar ede-project-cache-files nil
  "List of project files EDE has seen before.")

(defun ede-save-cache ()
  "Save a cache of EDE objects that Emacs has seen before."
  (interactive)
  (let ((p ede-projects)
	(c ede-project-cache-files)
	(recentf-exclude '( (lambda (f) t) ))
	)
    (condition-case nil
	(progn
	  (set-buffer (find-file-noselect ede-project-placeholder-cache-file t))
	  (erase-buffer)
	  (insert ";; EDE project cache file.
;; This contains a list of projects you have visited.\n(")
	  (while p
	    (when (and (car p) (ede-project-p p))
	      (let ((f (oref (car p) file)))
		(when (file-exists-p f)
		  (insert "\n  \"" f "\""))))
	    (setq p (cdr p)))
	  (while c
	    (insert "\n \"" (car c) "\"")
	    (setq c (cdr c)))
	  (insert "\n)\n")
	  (condition-case nil
	      (save-buffer 0)
	    (error
	     (message "File %s could not be saved."
		      ede-project-placeholder-cache-file)))
	  (kill-buffer (current-buffer))
	  )
      (error
       (message "File %s could not be read."
	       	ede-project-placeholder-cache-file))

      )))

(defun ede-load-cache ()
  "Load the cache of EDE projects."
  (save-excursion
    (let ((cachebuffer nil))
      (condition-case nil
	  (progn
	    (setq cachebuffer
		  (find-file-noselect ede-project-placeholder-cache-file t))
	    (set-buffer cachebuffer)
	    (goto-char (point-min))
	    (let ((c (read (current-buffer)))
		  (new nil)
		  (p ede-projects))
	      ;; Remove loaded projects from the cache.
	      (while p
		(setq c (delete (oref (car p) file) c))
		(setq p (cdr p)))
	      ;; Remove projects that aren't on the filesystem
	      ;; anymore.
	      (while c
		(when (file-exists-p (car c))
		  (setq new (cons (car c) new)))
		(setq c (cdr c)))
	      ;; Save it
	      (setq ede-project-cache-files (nreverse new))))
	(error nil))
      (when cachebuffer (kill-buffer cachebuffer))
      )))

;;; Get the cache usable.

;; @TODO - Remove this cache setup, or use this for something helpful.
;;(add-hook 'kill-emacs-hook 'ede-save-cache)
;;(when (not noninteractive)
;;  ;; No need to load the EDE cache if we aren't interactive.
;;  ;; This occurs during batch byte-compiling of other tools.
;;  (ede-load-cache))


;;; METHODS
;;
;; The methods in ede-base handle project related behavior, and DO NOT
;; related to EDE mode commands directory, such as keybindings.
;;
;; Mode related methods are in ede.el.  These methods are related
;; project specific activities not directly tied to a keybinding.
(defmethod ede-subproject-relative-path ((proj ede-project) &optional parent-in)
  "Get a path name for PROJ which is relative to the parent project.
If PARENT is specified, then be relative to the PARENT project.
Specifying PARENT is useful for sub-sub projects relative to the root project."
  (let* ((parent (or parent-in (ede-parent-project proj)))
	 (dir (file-name-directory (oref proj file))))
    (if (and parent (not (eq parent proj)))
	(file-relative-name dir (file-name-directory (oref parent file)))
      "")))

(defmethod ede-subproject-p ((proj ede-project))
  "Return non-nil if PROJ is a sub project."
  ;; @TODO - Use this in more places, and also pay attention to
  ;; metasubproject in ede/proj.el
  (ede-parent-project proj))


;;; Default descriptive methods for EDE classes
;;
;; These are methods which you might want to override, but there is
;; no need to in most situations because they are either a) simple, or
;; b) cosmetic.

(defmethod ede-name ((this ede-target))
  "Return the name of THIS target."
  (oref this name))

(defmethod ede-target-name ((this ede-target))
  "Return the name of THIS target, suitable for make or debug style commands."
  (oref this name))

(defmethod ede-name ((this ede-project))
  "Return a short-name for THIS project file.
Do this by extracting the lowest directory name."
  (oref this name))

(defmethod ede-description ((this ede-project))
  "Return a description suitable for the minibuffer about THIS."
  (format "Project %s: %d subprojects, %d targets."
	  (ede-name this) (length (oref this subproj))
	  (length (oref this targets))))

(defmethod ede-description ((this ede-target))
  "Return a description suitable for the minibuffer about THIS."
  (format "Target %s: with %d source files."
	  (ede-name this) (length (oref this source))))

;;; HEADERS/DOC
;;
;; Targets and projects are often associated with other files, such as
;; header files, documentation files and the like.  Have strong
;; associations can make useful user commands to quickly navigate
;; between the files based on their associations.
;;
(defun ede-header-file ()
  "Return the header file for the current buffer.
Not all buffers need headers, so return nil if no applicable."
  (if ede-object
      (ede-buffer-header-file ede-object (current-buffer))
    nil))

(defmethod ede-buffer-header-file ((this ede-project) buffer)
  "Return nil, projects don't have header files."
  nil)

(defmethod ede-buffer-header-file ((this ede-target) buffer)
  "There are no default header files in EDE.
Do a quick check to see if there is a Header tag in this buffer."
  (with-current-buffer buffer
    (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1)
					(match-end 1))
      (let ((src (ede-target-sourcecode this))
	    (found nil))
	(while (and src (not found))
	  (setq found (ede-buffer-header-file (car src) (buffer-file-name))
		src (cdr src)))
	found))))

(defun ede-documentation-files ()
  "Return the documentation files for the current buffer.
Not all buffers need documentations, so return nil if no applicable.
Some projects may have multiple documentation files, so return a list."
  (if ede-object
      (ede-buffer-documentation-files ede-object (current-buffer))
    nil))

(defmethod ede-buffer-documentation-files ((this ede-project) buffer)
  "Return all documentation in project THIS based on BUFFER."
  ;; Find the info node.
  (ede-documentation this))

(defmethod ede-buffer-documentation-files ((this ede-target) buffer)
  "Check for some documentation files for THIS.
Also do a quick check to see if there is a Documentation tag in this BUFFER."
  (with-current-buffer buffer
    (if (re-search-forward "::Documentation:: \\([a-zA-Z0-9.]+\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1)
					(match-end 1))
      ;; Check the master project
      (let ((cp (ede-toplevel)))
	(ede-buffer-documentation-files cp (current-buffer))))))

(defmethod ede-documentation ((this ede-project))
  "Return a list of files that provide documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (let ((targ (oref this targets))
	(proj (oref this subproj))
	(found nil))
    (while targ
      (setq found (append (ede-documentation (car targ)) found)
	    targ (cdr targ)))
    (while proj
      (setq found (append (ede-documentation (car proj)) found)
	    proj (cdr proj)))
    found))

(defmethod ede-documentation ((this ede-target))
  "Return a list of files that provide documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  nil)

(defun ede-html-documentation-files ()
  "Return a list of HTML documentation files associated with this project."
  (ede-html-documentation (ede-toplevel))
  )

(defmethod ede-html-documentation ((this ede-project))
  "Return a list of HTML files provided by project THIS."

  )

;;; Default "WANT" methods.
;;
;; These methods are used to determine if a target "wants", or could
;; somehow handle a file, or some source type.
;;
(defmethod ede-want-file-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-p (car src) file)))
      (setq src (cdr src)))
    src))

(defmethod ede-want-file-source-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-source-p (car src) file)))
      (setq src (cdr src)))
    src))

(defmethod ede-target-sourcecode ((this ede-target))
  "Return the sourcecode objects which THIS permits."
  (let ((sc (oref this sourcetype))
	(rs nil))
    (while (and (listp sc) sc)
      (setq rs (cons (symbol-value (car sc)) rs)
	    sc (cdr sc)))
    rs))


;;; Debugging.
;;
(defun ede-adebug-project ()
  "Run adebug against the current EDE project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-current-project)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-current-project) "")
    ))

(defun ede-adebug-project-parent ()
  "Run adebug against the current EDE parent project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-parent-project)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-parent-project) "")
    ))

(defun ede-adebug-project-root ()
  "Run adebug against the current EDE parent project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-toplevel)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-toplevel) "")
    ))



;;; TOPLEVEL PROJECT
;;
;; The toplevel project is a way to identify the EDE structure that belongs
;; to the top of a project.

(defun ede-toplevel (&optional subproj)
  "Return the ede project which is the root of the current project.
Optional argument SUBPROJ indicates a subproject to start from
instead of the current project."
  (or ede-object-root-project
      (let* ((cp (or subproj (ede-current-project))))
	(or (and cp (ede-project-root cp))
	    (progn
	      (while (ede-parent-project cp)
		(setq cp (ede-parent-project cp)))
	      cp)))))


;;; Hooks & Autoloads
;;
;;  These let us watch various activities, and respond appropriately.

;; (add-hook 'edebug-setup-hook
;; 	  (lambda ()
;; 	    (def-edebug-spec ede-with-projectfile
;; 	      (form def-body))))

(provide 'ede/base)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/base"
;; End:

;;; ede/base.el ends here
