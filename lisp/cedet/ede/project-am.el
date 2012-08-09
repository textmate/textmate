;;; project-am.el --- A project management scheme based on automake files.

;; Copyright (C) 1998-2000, 2003, 2005, 2007-2012
;;   Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.0.3
;; Keywords: project, make

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
;; The GNU Automake tool is the first step towards having a really
;; good project management system.  It provides a simple and concise
;; look at what is actually in a project, and records it in a simple
;; fashion.
;;
;; project-am uses the structure defined in all good GNU projects with
;; the Automake file as its base template, and then maintains that
;; information during edits, automatically updating the automake file
;; where appropriate.

(require 'make-mode)
(require 'ede)
(require 'ede/make)
(require 'ede/makefile-edit)
(require 'semantic/find) ;; for semantic-find-tags-by-...
(require 'ede/autoconf-edit)

(declare-function autoconf-parameters-for-macro "ede/autoconf-edit")
(declare-function ede-shell-run-something "ede/shell")
(eval-when-compile (require 'compile))

;;; Code:
(defgroup project-am nil
  "File and tag browser frame."
  :group 'tools
  :group 'ede
  )

(defcustom project-am-compile-project-command nil
  "*Default command used to compile a project."
  :group 'project-am
  :type 'string)

(defcustom project-am-compile-target-command (concat ede-make-command " -k %s")
  "*Default command used to compile a project."
  :group 'project-am
  :type 'string)

(defcustom project-am-debug-target-function 'gdb
  "*Default Emacs command used to debug a target."
  :group 'project-am
  :type 'sexp) ; make this be a list some day

(defconst project-am-type-alist
  '(("bin" project-am-program "bin_PROGRAMS" t)
    ("sbin" project-am-program "sbin_PROGRAMS" t)
    ("noinstbin" project-am-program "noinst_PROGRAMS" t)
    ("checkbin" project-am-program "check_PROGRAMS" t)
    ("lib" project-am-lib "lib_LIBS" t)
    ("libraries" project-am-lib "lib_LIBRARIES" t)
    ("librariesnoinst" project-am-lib "noinst_LIBRARIES" t)
    ("pkglibraries" project-am-lib "pkglib_LIBRARIES" t)
    ("checklibs" project-am-lib "check_LIBRARIES" t)
    ("ltlibraries" project-am-lib "lib_LTLIBRARIES" t)
    ("ltlibrariesnoinst" project-am-lib "noinst_LTLIBRARIES" t)
    ("pkgltlibraries" project-am-lib "pkglib_LTLIBRARIES" t)
    ("checkltlibs" project-am-lib "check_LTLIBRARIES" t)
    ("headernoinst" project-am-header-noinst "noinst_HEADERS")
    ("headerinst" project-am-header-inst "include_HEADERS")
    ("headerpkg" project-am-header-pkg "pkginclude_HEADERS")
    ("headerpkg" project-am-header-chk "check_HEADERS")
    ("texinfo" project-am-texinfo "info_TEXINFOS" t)
    ("man" project-am-man "man_MANS")
    ("lisp" project-am-lisp "lisp_LISP")
    ;; for other global files track EXTRA_
    ("extrabin" project-am-program "EXTRA_PROGRAMS" t)
    ("builtsrcs" project-am-built-src "BUILT_SOURCES")
    ("extradist" project-am-extra-dist "EXTRA_DIST")
    ;; Custom libraries targets?
    ;; ("ltlibcustom" project-am-lib ".*?_LTLIBRARIES" t)
    )
  "Alist of type names and the type of object to create for them.
Each entry is of the form:
  (EMACSNAME CLASS AUTOMAKEVAR INDIRECT)
where EMACSNAME is a name for Emacs to use.
CLASS is the EDE target class to represent the target.
AUTOMAKEVAR is the Automake variable to identify.  This cannot be a
   regular expression.
INDIRECT is optional.  If it is non-nil, then the variable in
question lists other variables that need to be looked up.")


(defconst project-am-meta-type-alist
  '((project-am-program "_PROGRAMS$" t)
    (project-am-lib "_\\(LIBS\\|LIBRARIES\\|LTLIBRARIES\\)$" t)

    ;; direct primary target use a dummy object (man target)
    ;; update to: * 3.3 Uniform  in automake-1.11 info node.
    (project-am-man "_\\(DATA\\|HEADERS\\|PYTHON\\|JAVA\\|SCRIPTS\\|MANS\\|TEXINFOS\\)$" nil)
    )
  "Alist of meta-target type, each entry has form:
     (CLASS REGEXPVAR INDIRECT)
where CLASS is the EDE target class for target.
REGEXPVAR is the regexp used in `semantic-find-tags-by-name-regexp'.
INDIRECT is optional. If it is non-nil, then the variable in it have
other meta-variable based on this name.")


(defclass project-am-target (ede-target)
  nil
  "Base target class for everything in project-am.")

(defclass project-am-objectcode (project-am-target)
  ((source :initarg :source :documentation "List of source files."))
  "A target which creates object code, like a C program or library.")

(defclass project-am-program (project-am-objectcode)
  ((ldadd :initarg :ldadd :documentation "Additional LD args."
	  :initform nil))
  "A top level program to build")

(defclass project-am-header (project-am-target)
  ()
  "A group of misc source files, such as headers.")

(defclass project-am-header-noinst (project-am-header)
  ()
  "A group of header files that are not installed.")

(defclass project-am-header-inst (project-am-header)
  ()
  "A group of header files that are not installed.")

(defclass project-am-header-pkg (project-am-header)
  ()
  "A group of header files that are not installed.")

(defclass project-am-header-chk (project-am-header)
  ()
  "A group of header files that are not installed.")

(defclass project-am-lib (project-am-objectcode)
  nil
  "A top level library to build")

(defclass project-am-lisp (project-am-target)
  ()
  "A group of Emacs Lisp programs to byte compile.")

(defclass project-am-texinfo (project-am-target)
  ((include :initarg :include
	    :initform nil
	    :documentation "Additional texinfo included in this one."))
  "A top level texinfo file to build.")

(defclass project-am-man (project-am-target)
  nil
  "A top level man file to build.")

;; For generic files tracker like EXTRA_DIST
(defclass project-am-built-src (project-am-target)
  ()
  "A group of Emacs Lisp programs to byte compile.")

(defclass project-am-extra-dist (project-am-target)
  ()
  "A group of Emacs Lisp programs to byte compile.")

(defclass project-am-makefile (ede-project)
  ((targets :initarg :targets
	    :initform nil
	    :documentation "Top level targets in this makefile.")
   (configureoutputfiles
    :initform nil
    :documentation
    "List of files output from configure system.")
   )
  "Encode one makefile.")

;;; Code:
(defmethod project-add-file ((ot project-am-target))
  "Add the current buffer into a project.
OT is the object target.  DIR is the directory to start in."
  (let* ((target (if ede-object (error "Already associated w/ a target")
		   (let ((amf (project-am-load default-directory)))
		     (if (not amf) (error "No project file"))
		     (completing-read "Target: "
				      (object-assoc-list 'name
							 (oref amf targets))
				      nil t))))
	 ;; The input target might be new.  See if we can find it.
	 (amf (ede-load-project-file (oref ot path)))
	 (ot (object-assoc target 'name (oref amf targets)))
	 (ofn (file-name-nondirectory (buffer-file-name))))
    (if (not ot)
	(setq ot
	      (project-new-target
	       target (project-am-preferred-target-type (buffer-file-name)))))
    (ede-with-projectfile ot
      (makefile-move-to-macro (project-am-macro ot))
      (makefile-end-of-command)
      (insert " " ofn)
      (makefile-fill-paragraph nil)
      (project-rescan ot)
      (save-buffer))
    (setq ede-object ot)))

(defmethod project-remove-file ((ot project-am-target) fnnd)
  "Remove the current buffer from any project targets."
  (ede-with-projectfile ot
    (makefile-move-to-macro (project-am-macro ot))
    (makefile-navigate-macro (concat " *" (regexp-quote (ede-name fnnd))))
    (replace-match "" t t nil 0)
    (makefile-fill-paragraph nil)
    (project-rescan ot)
    (save-buffer))
  (setq ede-object nil))

(defmethod project-edit-file-target ((obj project-am-target))
  "Edit the target associated w/ this file."
  (find-file (concat (oref obj path) "Makefile.am"))
  (goto-char (point-min))
  (makefile-move-to-macro (project-am-macro obj))
  (if (= (point-min) (point))
      (re-search-forward (ede-target-name obj))))

(defmethod project-new-target ((proj project-am-makefile)
			       &optional name type)
  "Create a new target named NAME.
Argument TYPE is the type of target to insert.  This is a string
matching something in `project-am-type-alist' or type class symbol.
Despite the fact that this is a method, it depends on the current
buffer being in order to provide a smart default target type."
  (let* ((name (or name (read-string "Name: " "")))
	 (type (or type
		   (completing-read "Type: "
				    project-am-type-alist
				    nil t
				    (cond ((eq major-mode 'texinfo-mode)
					   "texinfo")
					  ((eq major-mode 'nroff-mode)
					   "man")
					  ((eq major-mode 'emacs-lisp-mode)
					   "lisp")
					  (t "bin")))))
	 (ntype (assoc type project-am-type-alist))
	 (ot nil))
    (setq ot (apply (car (cdr ntype)) name :name name
		    :path (expand-file-name default-directory) nil))
    (if (not ot) (error "Error creating target object %S" ntype))
    (ede-with-projectfile ot
      (goto-char (point-min))
      (makefile-next-dependency)
      (if (= (point) (point-min))
	  (goto-char (point-max))
	(beginning-of-line)
	(insert "\n")
	(forward-char -1))
      ;; Add the new target sources macro (if needed)
      (if (project-am-macro ot)
	  (makefile-insert-macro (project-am-macro ot)))
      ;; Add to the list of objects.
      (goto-char (point-min))
      (makefile-move-to-macro (car (cdr (cdr ntype))))
      (if (= (point) (point-min))
	  (progn
	    (if (re-search-forward makefile-macroassign-regex nil t)
		(progn (forward-line -1)
		       (end-of-line)
		       (insert "\n"))
	      ;; If the above search fails, that's ok.  We'd just want to be at
	      ;; point-min anyway.
	      )
	    (makefile-insert-macro (car (cdr (cdr ntype))))))
      (makefile-end-of-command)
      (insert " " (ede-target-name ot))
      (save-buffer)
      ;; Rescan the object in this makefile.
      (project-rescan ede-object))))

;;
;; NOTE TO SELF
;;
;;  This should be handled at the EDE level, calling a method of the
;; top most project.
;;
(defmethod project-compile-project ((obj project-am-target) &optional command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  (require 'compile)
  (if (not command)
      (setq
       command
       ;; This interactive statement was taken from compile, and I'll
       ;; use the same command history too.
       (progn
	 (if (not project-am-compile-project-command)
	     (setq project-am-compile-project-command compile-command))
	 (if (or compilation-read-command current-prefix-arg)
	     (read-from-minibuffer "Project compile command: "
				   ;; hardcode make -k
				   ;; This is compile project after all.
				   project-am-compile-project-command
				   nil nil '(compile-history . 1))
	   project-am-compile-project-command))))
  ;; When compile a project, we might be in a subdirectory,
  ;; so we have to make sure we move all the way to the top.
  (let* ((default-directory (project-am-find-topmost-level default-directory)))
    (compile command)))

(defmethod project-compile-project ((obj project-am-makefile)
				    &optional command)
  "Compile the entire current project.
Argument COMMAND is the command to use when compiling."
  (require 'compile)
  (if (not command)
      (setq
       command
       ;; This interactive statement was taken from compile, and I'll
       ;; use the same command history too.
       (progn
	 (if (not project-am-compile-project-command)
	     (setq project-am-compile-project-command compile-command))
	 (if (or compilation-read-command current-prefix-arg)
	     (read-from-minibuffer "Project compile command: "
				   ;; hardcode make -k
				   ;; This is compile project after all.
				   project-am-compile-project-command
				   nil nil '(compile-history . 1))
	   project-am-compile-project-command))))
  ;; When compile a project, we might be in a subdirectory,
  ;; so we have to make sure we move all the way to the top.
  (let* ((default-directory (project-am-find-topmost-level default-directory)))
    (compile command)))

(defmethod project-compile-target ((obj project-am-target) &optional command)
  "Compile the current target.
Argument COMMAND is the command to use for compiling the target."
  (require 'compile)
  (if (not project-am-compile-project-command)
      (setq project-am-compile-project-command compile-command))
  (if (not command)
      (setq
       command
       (if compilation-read-command
	   (read-from-minibuffer "Project compile command: "
				 ;; hardcode make -k
				 ;; This is compile project after all.
				 (if ede-object
				     (format
				      project-am-compile-target-command
				      (project-compile-target-command
				       ede-object))
				   project-am-compile-target-command)
				 nil nil
				 '(compile-history . 1))
	 (if ede-object
	     project-am-compile-project-command
	   (format
	    project-am-compile-target-command
	    (project-compile-target-command ede-object))))))
  ;; We better be in the right place when compiling a specific target.
  (compile command))

(defmethod project-debug-target ((obj project-am-objectcode))
  "Run the current project target in a debugger."
  (let ((tb (get-buffer-create " *padt*"))
	(dd (oref obj path))
	(cmd nil))
    (unwind-protect
	(progn
	  (require 'ede/shell)
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (symbol-name project-am-debug-target-function)
			     " " (ede-target-name obj))))
	  (funcall project-am-debug-target-function cmd))
      (kill-buffer tb))))

(declare-function ede-shell-run-something "ede/shell")

(defmethod project-run-target ((obj project-am-objectcode))
  "Run the current project target in comint buffer."
  (require 'ede/shell)
  (let ((tb (get-buffer-create " *padt*"))
	(dd (oref obj path))
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (ede-target-name obj))))
	  (ede-shell-run-something obj cmd))
      (kill-buffer tb))))

(defmethod project-make-dist ((this project-am-target))
  "Run the current project in the debugger."
  (require 'compile)
  (if (not project-am-compile-project-command)
      (setq project-am-compile-project-command compile-command))
  (project-compile-project this (concat project-am-compile-project-command
					" dist")))

;;; Project loading and saving
;;
(defun project-am-load (directory &optional rootproj)
  "Read an automakefile DIRECTORY into our data structure.
If a given set of projects has already been loaded, then do nothing
but return the project for the directory given.
Optional ROOTPROJ is the root EDE project."
  (let* ((ede-constructing t)
	 (amo (object-assoc (expand-file-name "Makefile.am" directory)
			    'file ede-projects)))
    (when (not amo)
      (setq amo (project-am-load-makefile directory)))
    amo))

(defun project-am-find-topmost-level (dir)
  "Find the topmost automakefile starting with DIR."
  (let ((newdir dir))
    (while (or (file-exists-p (concat newdir "Makefile.am"))
	       (file-exists-p (concat newdir "configure.ac"))
	       (file-exists-p (concat newdir "configure.in"))
	       )
      (setq dir newdir newdir
	    (file-name-directory (directory-file-name newdir))))
    (expand-file-name dir)))

(defmacro project-am-with-makefile-current (dir &rest forms)
  "Set the Makefile.am in DIR to be the current buffer.
Run FORMS while the makefile is current.
Kill the makefile if it was not loaded before the load."
  `(let* ((fn (expand-file-name "Makefile.am" ,dir))
	  (fb nil)
	  (kb (get-file-buffer fn)))
     (if (not (file-exists-p fn))
	 nil
       (save-excursion
	 (if kb (setq fb kb)
	   ;; We need to find-file this thing, but don't use
	   ;; any semantic features.
	   (let ((semantic-init-hook nil)
		 (recentf-exclude '( (lambda (f) t) ))
		 )
	     (setq fb (find-file-noselect fn)))
	   )
	 (set-buffer fb)
	 (prog1 ,@forms
	   (if (not kb) (kill-buffer (current-buffer))))))))
(put 'project-am-with-makefile-current 'lisp-indent-function 1)

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec project-am-with-makefile-current
	      (form def-body))))


(defun project-am-load-makefile (path &optional suggestedname)
  "Convert PATH into a project Makefile, and return its project object.
It does not check for existing project objects.  Use `project-am-load'.
Optional argument SUGGESTEDNAME will be the project name.
This is used when subprojects are made in named subdirectories."
  (project-am-with-makefile-current path
    (if (and ede-object (project-am-makefile-p ede-object))
	ede-object
      (let* ((pi (project-am-package-info path))
	     (sfn (when suggestedname
		    (project-am-last-dir suggestedname)))
	     (pn (or sfn (nth 0 pi) (project-am-last-dir fn)))
	     (ver (or (nth 1 pi) "0.0"))
	     (bug (nth 2 pi))
	     (cof (nth 3 pi))
	     (ampf (project-am-makefile
		    pn :name pn
		    :version ver
		    :mailinglist (or bug "")
		    :file fn)))
	(oset ampf :directory (file-name-directory fn))
	(oset ampf configureoutputfiles cof)
	(make-local-variable 'ede-object)
	(setq ede-object ampf)
	;; Move the rescan after we set ede-object to prevent recursion
	(project-rescan ampf)
	ampf))))

;;; Methods:
(defmethod project-targets-for-file ((proj project-am-makefile))
  "Return a list of targets the project PROJ."
  (oref proj targets))

(defun project-am-scan-for-targets (currproj dir)
  "Scan the current Makefile.am for targets.
CURRPROJ is the current project being scanned.
DIR is the directory to apply to new targets."
  (let* ((otargets (oref currproj targets))
	 ;; `ntargets' results in complete targets list
	 ;; not only the new targets by diffing.
	 (ntargets nil)
	 (tmp nil)
	 )

    (mapc
     ;; Map all the different types
     (lambda (typecar)
       (let ((macro (nth 2 typecar))
	     (class (nth 1 typecar))
	     (indirect (nth 3 typecar))
	     )
	 (if indirect
	     ;; Map all the found objects
	     (mapc (lambda (lstcar)
		     (setq tmp (object-assoc lstcar 'name otargets))
		     (when (not tmp)
		       (setq tmp (apply class lstcar :name lstcar
					:path dir nil)))
		     (project-rescan tmp)
		     (setq ntargets (cons tmp ntargets)))
		   (makefile-macro-file-list macro))
	   ;; Non-indirect will have a target whos sources
	   ;; are actual files, not names of other targets.
	   (let ((files (makefile-macro-file-list macro)))
	     (when files
	       (setq tmp (object-assoc macro 'name otargets))
	       (when (not tmp)
		 (setq tmp (apply class macro :name macro
				  :path dir nil)))
	       (project-rescan tmp)
	       (setq ntargets (cons tmp ntargets))
	       ))
	   )
	 ))
     project-am-type-alist)

    ;; At now check variables for meta-target regexp
    ;; We have to check ntargets to avoid useless rescan.
    ;; Also we have check otargets to prevent duplication.
    (mapc
     (lambda (typecar)
       (let ((class (nth 0 typecar))
	     (metaregex (nth 1 typecar))
	     (indirect (nth 2 typecar)))
	 (if indirect
	     ;; Map all the found objects
	     (mapc
	      (lambda (lstcar)
		(unless (object-assoc lstcar 'name ntargets)
		  (or
		   (setq tmp (object-assoc lstcar 'name otargets))
		   (setq tmp (apply class lstcar :name lstcar
				    :path dir nil)))
		  (project-rescan tmp)
		  (setq ntargets (cons tmp ntargets))))
	      ;; build a target list to map over
	      (let (atargets)
		(dolist (TAG
			 (semantic-find-tags-by-name-regexp
			  metaregex (semantic-find-tags-by-class
				     'variable (semantic-fetch-tags))))
		  ;; default-value have to be a list
		  (when (cadr (assoc ':default-value TAG))
		    (setq atargets
			  (append
			   (nreverse (cadr (assoc ':default-value TAG)))
			   atargets))))
		(nreverse atargets)))

	   ;; else not indirect, TODO: FIX various direct meta type in a sane way.
	   (dolist (T (semantic-find-tags-by-name-regexp
		       metaregex (semantic-find-tags-by-class
				  'variable (semantic-fetch-tags))))
	     (unless (setq tmp (object-assoc (car T) 'name ntargets))
	       (or (setq tmp (object-assoc (car T) 'name otargets))
		   ;; we are really new
		   (setq tmp (apply class (car T) :name (car T)
				    :path dir nil)))
	       (project-rescan tmp)
	       (setq ntargets (cons tmp ntargets))))
	   )))
     project-am-meta-type-alist)
    ntargets))

(defun project-am-expand-subdirlist (place subdirs)
  "Store in PLACE the SUBDIRS expanded from variables.
Strip out duplicates, and recurse on variables."
  (mapc (lambda (sp)
	  (let ((var (makefile-extract-varname-from-text sp)))
	    (if var
		;; If it is a variable, expand that variable, and keep going.
		(project-am-expand-subdirlist
		 place (makefile-macro-file-list var))
	      ;; Else, add SP in if it isn't a dup.
	      (if (member sp (symbol-value place))
		  nil ; don't do it twice.
		(set place (cons sp (symbol-value place))) ;; add
		))))
	subdirs)
  )

(defmethod project-rescan ((this project-am-makefile) &optional suggestedname)
  "Rescan the makefile for all targets and sub targets."
  (project-am-with-makefile-current (file-name-directory (oref this file))
    ;;(message "Scanning %s..." (oref this file))
    (let* ((pi (project-am-package-info (oref this directory)))
	   (pn (nth 0 pi))
	   (pv (nth 1 pi))
	   (bug (nth 2 pi))
	   (cof (nth 3 pi))
	   (osubproj (oref this subproj))
	   ;; 1/30/10 - We need to append these two lists together,
	   ;; then strip out duplicates.  Expanding this list (via
	   ;; references to other variables should also strip out dups
	   (csubproj (append
		      (makefile-macro-file-list "DIST_SUBDIRS")
		      (makefile-macro-file-list "SUBDIRS")))
	   (csubprojexpanded nil)
	   (nsubproj nil)
	   ;; Targets are excluded here because they require
	   ;; special attention.
	   (dir (expand-file-name default-directory))
	   (tmp nil)
	   (ntargets (project-am-scan-for-targets this dir))
	   )
      (if suggestedname
	  (oset this name (project-am-last-dir suggestedname))
	;; Else, setup toplevel project info.
	(and pn (string= (directory-file-name
			  (oref this directory))
			 (directory-file-name
			  (project-am-find-topmost-level
			   (oref this directory))))
	     (oset this name pn)
	     (and pv (oset this version pv))
	     (and bug (oset this mailinglist bug))
	     (oset this configureoutputfiles cof)))
      ;; Now that we have this new list, chuck the old targets
      ;; and replace it with the new list of targets I just created.
      (oset this targets (nreverse ntargets))
      ;; We still have a list of targets.  For all buffers, make sure
      ;; their object still exists!
      ;; FIGURE THIS OUT
      (project-am-expand-subdirlist 'csubprojexpanded csubproj)
      ;; Ok, now let's look at all our sub-projects.
      (mapc (lambda (sp)
	      (let* ((subdir (file-name-as-directory
			      (expand-file-name
			       sp (file-name-directory (oref this :file)))))
		     (submake (expand-file-name
			       "Makefile.am"
			       subdir)))
		(if (string= submake (oref this :file))
		    nil	;; don't recurse.. please!
		  ;; For each project id found, see if we need to recycle,
		  ;; and if we do not, then make a new one.  Check the deep
		  ;; rescan value for behavior patterns.
		  (setq tmp (object-assoc
			     submake
			     'file osubproj))
		  (if (not tmp)
		      (setq tmp
			    (condition-case nil
				;; In case of problem, ignore it.
				(project-am-load-makefile subdir subdir)
			      (error nil)))
		    ;; If we have tmp, then rescan it only if deep mode.
		    (if ede-deep-rescan
			(project-rescan tmp subdir)))
		  ;; Tac tmp onto our list of things to keep, but only
		  ;; if tmp was found.
		  (when tmp
		    ;;(message "Adding %S" (object-print tmp))
		    (setq nsubproj (cons tmp nsubproj)))))
	      )
	    (nreverse csubprojexpanded))
      (oset this subproj nsubproj)
      ;; All elements should be updated now.
      )))


(defmethod project-rescan ((this project-am-program))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this)))
  (unless (oref this :source)
    (oset this :source (list (concat (oref this :name) ".c"))))
  (oset this :ldadd (makefile-macro-file-list
		     (concat (oref this :name) "_LDADD"))))

(defmethod project-rescan ((this project-am-lib))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this)))
  (unless (oref this :source)
    (oset this :source (list (concat (file-name-sans-extension (oref this :name)) ".c")))))

(defmethod project-rescan ((this project-am-texinfo))
  "Rescan object THIS."
  (oset this :include (makefile-macro-file-list (project-am-macro this))))

(defmethod project-rescan ((this project-am-man))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this))))

(defmethod project-rescan ((this project-am-lisp))
  "Rescan the lisp sources."
  (oset this :source (makefile-macro-file-list (project-am-macro this))))

(defmethod project-rescan ((this project-am-header))
  "Rescan the Header sources for object THIS."
  (oset this :source (makefile-macro-file-list (project-am-macro this))))

(defmethod project-rescan ((this project-am-built-src))
  "Rescan built sources for object THIS."
  (oset this :source (makefile-macro-file-list "BUILT_SOURCES")))

(defmethod project-rescan ((this project-am-extra-dist))
  "Rescan object THIS."
  (oset this :source (makefile-macro-file-list "EXTRA_DIST")))

(defmethod project-am-macro ((this project-am-objectcode))
  "Return the default macro to 'edit' for this object type."
  (concat (subst-char-in-string ?- ?_ (oref this :name)) "_SOURCES"))

(defmethod project-am-macro ((this project-am-header-noinst))
  "Return the default macro to 'edit' for this object."
  "noinst_HEADERS")

(defmethod project-am-macro ((this project-am-header-inst))
  "Return the default macro to 'edit' for this object."
  "include_HEADERS")

(defmethod project-am-macro ((this project-am-header-pkg))
  "Return the default macro to 'edit' for this object."
  "pkginclude_HEADERS")

(defmethod project-am-macro ((this project-am-header-chk))
  "Return the default macro to 'edit' for this object."
  "check_HEADERS")

(defmethod project-am-macro ((this project-am-texinfo))
  "Return the default macro to 'edit' for this object type."
  (concat (file-name-sans-extension (oref this :name)) "_TEXINFOS"))

(defmethod project-am-macro ((this project-am-man))
  "Return the default macro to 'edit' for this object type."
  (oref this :name))

(defmethod project-am-macro ((this project-am-lisp))
  "Return the default macro to 'edit' for this object."
  "lisp_LISP")

(defun project-am-buffer-object (amf buffer)
  "Return an object starting with AMF associated with BUFFER.
nil means that this buffer belongs to no-one."
  (if (not amf)
      nil
    (if (ede-buffer-mine amf buffer)
	amf
      (let ((targ (oref amf targets))
	    (sobj (oref amf subproj))
	    (obj nil))
	(while (and targ (not obj))
	  (if (ede-buffer-mine (car targ) buffer)
	      (setq obj (car targ)))
	  (setq targ (cdr targ)))
	(while (and sobj (not obj))
	  (setq obj (project-am-buffer-object (car sobj) buffer)
		sobj (cdr sobj)))
	obj))))

(defmethod ede-buffer-mine ((this project-am-makefile) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((efn  (expand-file-name (buffer-file-name buffer))))
    (or (string= (oref this :file) efn)
	(string-match "/configure\\.ac$" efn)
	(string-match "/configure\\.in$" efn)
	(string-match "/configure$" efn)
	;; Search output files.
	(let ((ans nil))
	  (dolist (f (oref this configureoutputfiles))
	    (when (string-match (concat (regexp-quote f) "$") efn)
	      (setq ans t)))
	  ans)
	)))

(defmethod ede-buffer-mine ((this project-am-objectcode) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (member (file-relative-name (buffer-file-name buffer) (oref this :path))
	  (oref this :source)))

(defmethod ede-buffer-mine ((this project-am-texinfo) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((bfn (file-relative-name (buffer-file-name buffer)
				 (oref this :path))))
    (or (string= (oref this :name)  bfn)
	(member bfn (oref this :include)))))

(defmethod ede-buffer-mine ((this project-am-man) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this :name)
	   (file-relative-name (buffer-file-name buffer) (oref this :path))))

(defmethod ede-buffer-mine ((this project-am-lisp) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (member (file-relative-name (buffer-file-name buffer) (oref this :path))
	  (oref this :source)))

(defmethod project-am-subtree ((ampf project-am-makefile) subdir)
  "Return the sub project in AMPF specified by SUBDIR."
  (object-assoc (expand-file-name subdir) 'file (oref ampf subproj)))

(defmethod project-compile-target-command ((this project-am-target))
  "Default target to use when compiling a given target."
  ;; This is a pretty good default for most.
  "")

(defmethod project-compile-target-command ((this project-am-objectcode))
  "Default target to use when compiling an object code target."
  (oref this :name))

(defmethod project-compile-target-command ((this project-am-texinfo))
  "Default target t- use when compiling a texinfo file."
  (let ((n (oref this :name)))
    (if (string-match "\\.texi?\\(nfo\\)?" n)
	(setq n (replace-match ".info" t t n)))
    n))


;;; Generic useful functions

(defun project-am-last-dir (file)
  "Return the last part of a directory name.
Argument FILE is the file to extract the end directory name from."
  (let* ((s (file-name-directory file))
	 (d (directory-file-name s))
	 )
    (file-name-nondirectory d))
  )

(defun project-am-preferred-target-type (file)
  "For FILE, return the preferred type for that file."
  (cond ((string-match "\\.texi?\\(nfo\\)$" file)
	 project-am-texinfo)
	((string-match "\\.[0-9]$" file)
	 project-am-man)
	((string-match "\\.el$" file)
	 project-am-lisp)
	(t
	 project-am-program)))

(defmethod ede-buffer-header-file((this project-am-objectcode) buffer)
  "There are no default header files."
  (or (call-next-method)
      (let ((s (oref this source))
	    (found nil))
	(while (and s (not found))
	  ;; Add more logic here if applicable.
	  (if (string-match "\\.\\(h\\|H\\|hh\\|hpp\\)" (car s))
	      (setq found (car s)))
	  (setq s (cdr s)))
	found)))

(defmethod ede-documentation ((this project-am-texinfo))
  "Return a list of files that provides documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (let* ((src (append (oref this source)
		      (oref this include)))
	 (proj (ede-target-parent this))
	 (dir (oref proj directory))
	 (out nil))
    ;; Loop over all entries and expand
    (while src
      (setq out (cons
		 (expand-file-name (car src) dir)
		 out))
      (setq src (cdr src)))
    ;; return it
    out))


;;; Configure.in queries.
;;
(defvar project-am-autoconf-file-options
  '("configure.in" "configure.ac")
  "List of possible configure files to look in for project info.")

(defun project-am-autoconf-file (dir)
  "Return the name of the autoconf file to use in DIR."
  (let ((ans nil))
    (dolist (L project-am-autoconf-file-options)
      (when (file-exists-p (expand-file-name L dir))
	(setq ans (expand-file-name L dir))))
    ans))

(defmacro project-am-with-config-current (file &rest forms)
  "Set the Configure FILE in the top most directory above DIR as current.
Run FORMS in the configure file.
Kill the Configure buffer if it was not already in a buffer."
  `(save-excursion
     (let ((fb (generate-new-buffer ,file)))
       (set-buffer fb)
       (erase-buffer)
       (insert-file-contents ,file)
       (prog1 ,@forms
	 (kill-buffer fb)))))

(put 'project-am-with-config-current 'lisp-indent-function 1)

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec project-am-with-config-current
	      (form def-body))))

(defmacro project-am-extract-shell-variable (var)
  "Extract the value of the shell variable VAR from a shell script."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" (regexp-quote var) "\\s-*=\\s-*")
			     nil t)
      (buffer-substring-no-properties (point) (point-at-eol)))))

(defun project-am-extract-package-info (dir)
  "Extract the package information for directory DIR."
  (let ((conf-in (project-am-autoconf-file dir))
	(conf-sh (expand-file-name "configure" dir))
	(name (file-name-nondirectory
	       (directory-file-name dir)))
	(ver "1.0")
	(bugrep nil)
	(configfiles nil)
	)
    (cond
     ;; Try configure.in or configure.ac
     (conf-in
      (project-am-with-config-current conf-in
	(let ((aci (autoconf-parameters-for-macro "AC_INIT"))
	      (aia (autoconf-parameters-for-macro "AM_INIT_AUTOMAKE"))
	      (acf (autoconf-parameters-for-macro "AC_CONFIG_FILES"))
	      (aco (autoconf-parameters-for-macro "AC_OUTPUT"))
	      )
	  (cond
	   ;; AC init has more than 1 parameter
	   ((> (length aci) 1)
	    (setq name (nth 0 aci)
		  ver (nth 1 aci)
		  bugrep (nth 2 aci)))
	   ;; The init automake has more than 1 parameter
	   ((> (length aia) 1)
	    (setq name (nth 0 aia)
		  ver (nth 1 aia)
		  bugrep (nth 2 aia)))
	   )
	  ;; AC_CONFIG_FILES, or AC_OUTPUT lists everything that
	  ;; should be detected as part of this PROJECT, but not in a
	  ;; particular TARGET.
	  (let ((outfiles (cond (aco (list (car aco)))
				(t acf))))
	    (if (> (length outfiles) 1)
		(setq configfiles outfiles)
	      (setq configfiles (split-string (car outfiles) "\\s-" t)))
	    )
	  ))
      )
     ;; Else, try the script
     ((file-exists-p conf-sh)
      (project-am-with-config-current conf-sh
	(setq name (project-am-extract-shell-variable "PACKAGE_NAME")
	      ver (project-am-extract-shell-variable "PACKAGE_VERSION")
	      )
	))
     ;; Don't know what else....
     (t
      nil))
    ;; Return stuff
    (list name ver bugrep configfiles)
    ))

(defun project-am-package-info (dir)
  "Get the package information for directory topmost project dir over DIR.
Calculates the info with `project-am-extract-package-info'."
  (let ((top (ede-toplevel)))
    (when top (setq dir (oref top :directory)))
    (project-am-extract-package-info dir)))

;; for simple per project include path extension
(defmethod ede-system-include-path ((this project-am-makefile))
  "Return `project-am-localvars-include-path', usually local variable
per file or in .dir-locals.el or similar."
  (bound-and-true-p project-am-localvars-include-path))

(defmethod ede-system-include-path ((this project-am-target))
  "Return `project-am-localvars-include-path', usually local variable
per file or in .dir-locals.el or similar."
  (bound-and-true-p project-am-localvars-include-path))


(provide 'ede/project-am)

;;; ede/project-am.el ends here
