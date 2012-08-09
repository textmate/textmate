;;; ede-pmake.el --- EDE Generic Project Makefile code generator.

;; Copyright (C) 1998-2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
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
;; Code generator for Makefiles.
;;
;; Here is how it should work:
;; 1) Collect information about the project and targets
;; 2) Insert header into the Makefile
;; 3) Insert basic variables (target/source)
;; 4) Conditional
;;    a) Makefile
;;       1) Insert support variables (compiler variables, etc)
;;       2) Insert VERSION and DISTDIR
;;       3) Specify top build dir if necessary
;;       4) Specify compile/link commands (c, etc)
;;       5) Specify dependency files
;;       6) Specify all: target
;;       7) Include dependency files
;;       8) Insert commonized target specify rules
;;       9) Insert clean: and dist: rules
;;    b) Automake file
;;       1) Insert distribution source variables for targets
;;       2) Insert user requested rules

(eval-when-compile (require 'cl))
(require 'ede/proj)
(require 'ede/proj-obj)
(require 'ede/proj-comp)

(declare-function ede-srecode-setup "ede/srecode")
(declare-function ede-srecode-insert "ede/srecode")

;;; Code:
(defmethod ede-proj-makefile-create ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS.
MFILENAME is the makefile to generate."
  (require 'ede/srecode)
  (let ((mt nil)
	(isdist (string= mfilename (ede-proj-dist-makefile this)))
	(depth 0)
	(orig-buffer nil)
	(buff-to-kill nil)
	)
    ;; Find out how deep this project is.
    (let ((tmp this))
      (while (setq tmp (ede-parent-project tmp))
	(setq depth (1+ depth))))
    ;; Collect the targets that belong in a makefile.
    (mapc
     (lambda (obj)
       (if (and (obj-of-class-p obj 'ede-proj-target-makefile)
		(string= (oref obj makefile) mfilename))
	   (setq mt (cons obj mt))))
     (oref this targets))
    ;; Fix the order so things compile in the right direction.
    (setq mt (nreverse mt))
    ;; Add in the header part of the Makefile*
    (save-excursion
      (setq orig-buffer (get-file-buffer mfilename))
      (set-buffer (setq buff-to-kill (find-file-noselect mfilename)))
      (goto-char (point-min))
      (if (and
	   (not (eobp))
	   (not (looking-at "# Automatically Generated \\w+ by EDE.")))
	  (if (not (y-or-n-p (format "Really replace %s? " mfilename)))
	      (error "Not replacing Makefile"))
	(message "Replace EDE Makefile"))
      (erase-buffer)
      (ede-srecode-setup)
      ;; Insert a giant pile of stuff that is common between
      ;; one of our Makefiles, and a Makefile.in
      (ede-srecode-insert
       "file:ede-empty"
       "MAKETYPE"
       (with-slots (makefile-type) this
	 (cond ((eq makefile-type 'Makefile) "make")
	       ((eq makefile-type 'Makefile.in) "autoconf")
	       ((eq makefile-type 'Makefile.am) "automake")
	       (t (error ":makefile-type in project invalid")))))

      ;; Just this project's variables
      (ede-proj-makefile-insert-variables this)

      ;; Space
      (insert "\n")

      (cond
       ((eq (oref this makefile-type) 'Makefile)
	;; Make sure the user has the right kind of make
	(ede-make-check-version)

	(let* ((targ (if isdist (oref this targets) mt))
	       (sp (oref this subproj))
	       (df (apply 'append
			  (mapcar (lambda (tg)
				    (ede-proj-makefile-dependency-files tg))
				  targ))))
	  ;; Distribution variables
	  (ede-compiler-begin-unique
	    (mapc 'ede-proj-makefile-insert-variables targ))
	  ;; Only add the distribution stuff in when depth != 0
	  (let ((top  (ede-toplevel this))
		(tmp this)
		(subdir ""))
	    (insert "VERSION=" (oref top version) "\n"
		    "DISTDIR=$(top)" (oref top name) "-$(VERSION)")
	    (while (ede-parent-project tmp)
	      (setq subdir
		    (concat
		     "/"
		     (file-name-nondirectory
		      (directory-file-name
		       (file-name-directory (oref tmp file))))
		     subdir)
		    tmp (ede-parent-project tmp)))
	    (insert subdir "\n"))
	  ;; Some built in variables for C code
	  (if df
	      (let ((tc depth))
		(insert "top_builddir = ")
		(while (/= 0 tc)
		  (setq tc (1- tc))
		  (insert "..")
		  (if (/= tc 0) (insert "/")))
		(insert "\n")))
	  (insert "\n")
	  ;; Create a variable with all the dependency files to include
	  ;; These methods borrowed from automake.
	  (if (and (oref this automatic-dependencies) df)
	      (progn
		(insert "DEP_FILES="
			(mapconcat (lambda (f)
				     (concat ".deps/"
					     (file-name-nondirectory
					      (file-name-sans-extension
					       f)) ".P"))
				   df " "))))
	  ;;
	  ;; Insert ALL Rule
	  ;;
	  (insert "\n\nall:")
	  (mapc (lambda (c)
		  (if (and (slot-exists-p c 'partofall) (oref c partofall))
		      ;; Only insert this rule if it is a part of ALL.
		      (insert " " (ede-proj-makefile-target-name c))))
		targ)
	  (mapc (lambda (c)
		  (insert " " (ede-name c))
		  )
		sp)
	  (insert "\n\n")
	  ;;
	  ;; Add in the include files
	  ;;
	  (mapc (lambda (c)
		  (insert "include " c "\n\n"))
		(oref this include-file))
	  ;; Some C inference rules
	  ;; Dependency rules borrowed from automake.
	  ;;
	  ;; NOTE: This is GNU Make specific.
	  (if (and (oref this automatic-dependencies) df)
	      (insert "DEPS_MAGIC := $(shell mkdir .deps > /dev/null "
		      "2>&1 || :)\n"
		      "-include $(DEP_FILES)\n\n"))
	  ;;
	  ;; General makefile rules stored in the individual targets
	  ;;
	  (ede-compiler-begin-unique
	    (ede-proj-makefile-insert-rules this)
	    (mapc 'ede-proj-makefile-insert-rules targ))
	  ;;
	  ;; phony targets for sub projects
	  ;;
	  (mapc 'ede-proj-makefile-insert-subproj-rules sp)
	  ;;
	  ;; Distribution rules such as CLEAN and DIST
	  ;;
	  (when isdist
	    (ede-proj-makefile-tags this mt)
	    (ede-proj-makefile-insert-dist-rules this)))
	(save-buffer))
       ((eq (oref this makefile-type) 'Makefile.in)
	(error "Makefile.in is not supported"))
       ((eq (oref this makefile-type) 'Makefile.am)
	(require 'ede/pconf)
	;; Basic vars needed:
	(ede-proj-makefile-automake-insert-subdirs this)
	(ede-proj-makefile-automake-insert-extradist this)
	;; Distribution variables
	(let ((targ (if isdist (oref this targets) mt)))
	  (ede-compiler-begin-unique
	    (mapc 'ede-proj-makefile-insert-automake-pre-variables targ))
	  (ede-compiler-begin-unique
	    (mapc 'ede-proj-makefile-insert-source-variables targ))
	  (ede-compiler-begin-unique
	    (mapc 'ede-proj-makefile-insert-automake-post-variables targ))
	  (ede-compiler-begin-unique
	    (ede-proj-makefile-insert-user-rules this))
	  (insert "\n# End of Makefile.am\n")
	  (save-buffer))
	)
       (t (error "Unknown makefile type when generating Makefile")))
      ;; Put the cursor in a nice place
      (goto-char (point-min)))
    ;; If we have an original buffer, then don't kill it.
    (when (not orig-buffer)
      (kill-buffer buff-to-kill))
    ))

;;; VARIABLE insertion
;;
(defun ede-pmake-end-of-variable ()
  "Move to the end of the variable declaration under point."
  (end-of-line)
  (while (= (preceding-char) ?\\)
    (forward-char 1)
    (end-of-line))
  )

(defmacro ede-pmake-insert-variable-shared (varname &rest body)
  "Add VARNAME into the current Makefile.
Execute BODY in a location where a value can be placed."
  `(let ((addcr t) (v ,varname))
     (if (save-excursion
	   (goto-char (point-max))
	   (re-search-backward (concat "^" v "\\s-*=") nil t))
	 (progn
	   (goto-char (match-end 0))
	   (ede-pmake-end-of-variable)
	   (if (< (current-column) 40)
	       (if (and (/= (preceding-char) ?=)
			(/= (preceding-char) ? ))
		   (insert " "))
	     (insert "\\\n   "))
	   (setq addcr nil))
       (insert v "="))
     ,@body
     (if addcr (insert "\n"))
     (goto-char (point-max))))
(put 'ede-pmake-insert-variable-shared 'lisp-indent-function 1)

(defmacro ede-pmake-insert-variable-once (varname &rest body)
  "Add VARNAME into the current Makefile if it doesn't exist.
Execute BODY in a location where a value can be placed."
  `(let ((addcr t) (v ,varname))
     (unless (re-search-backward (concat "^" v "\\s-*=") nil t)
       (insert v "=")
       ,@body
       (if addcr (insert "\n"))
       (goto-char (point-max)))
     ))
(put 'ede-pmake-insert-variable-once 'lisp-indent-function 1)

;;; SOURCE VARIABLE NAME CONSTRUCTION

(defsubst ede-pmake-varname (obj)
  "Convert OBJ into a variable name name.
Change .  to _ in the variable name."
  (let ((name (oref obj name)))
    (while (string-match "\\." name)
      (setq name (replace-match "_" nil t name)))
    name))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_YOU_FOUND_A_BUG"))

;;; DEPENDENCY FILE GENERATOR LISTS
;;
(defmethod ede-proj-makefile-dependency-files ((this ede-proj-target))
  "Return a list of source files to convert to dependencies.
Argument THIS is the target to get sources from."
  nil)

;;; GENERIC VARIABLES
;;
(defmethod ede-proj-makefile-configuration-variables ((this ede-proj-project)
						      configuration)
  "Return a list of configuration variables from THIS.
Use CONFIGURATION as the current configuration to query."
  (cdr (assoc configuration (oref this configuration-variables))))

(defmethod ede-proj-makefile-insert-variables-new ((this ede-proj-project))
  "Insert variables needed by target THIS.

NOTE: Not yet in use!  This is part of an SRecode conversion of
      EDE that is in progress."
;  (let ((conf-table (ede-proj-makefile-configuration-variables
;		     this (oref this configuration-default)))
;	(conf-done nil))
;
;    (ede-srecode-insert-with-dictionary
;     "declaration:ede-vars"
;
;     ;; Insert all variables, and augment them with details from
;     ;; the current configuration.
;     (mapc (lambda (c)
;
;	     (let ((ldict (srecode-dictionary-add-section-dictionary
;			   dict "VARIABLE"))
;		   )
;	       (srecode-dictionary-set-value ldict "NAME" (car c))
;	       (if (assoc (car c) conf-table)
;		   (let ((vdict (srecode-dictionary-add-section-dictionary
;				 ldict "VALUE")))
;		     (srecode-dictionary-set-value
;		      vdict "VAL" (cdr (assoc (car c) conf-table)))
;		     (setq conf-done (cons (car c) conf-done))))
;	       (let ((vdict (srecode-dictionary-add-section-dictionary
;			     ldict "VALUE")))
;		 (srecode-dictionary-set-value vdict "VAL" (cdr c))))
;	     )
;
;	   (oref this variables))
;
;     ;; Add in all variables from the configuration not already covered.
;     (mapc (lambda (c)
;
;	     (if (member (car c) conf-done)
;		 nil
;	       (let* ((ldict (srecode-dictionary-add-section-dictionary
;			      dict "VARIABLE"))
;		      (vdict (srecode-dictionary-add-section-dictionary
;			      ldict "VALUE"))
;		      )
;		 (srecode-dictionary-set-value ldict "NAME" (car c))
;		 (srecode-dictionary-set-value vdict "VAL" (cdr c))))
;	     )
;
;	   conf-table)
;

     ;; @TODO - finish off this function, and replace the below fcn

;     ))
  )

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-project))
  "Insert variables needed by target THIS."
  (let ((conf-table (ede-proj-makefile-configuration-variables
		     this (oref this configuration-default)))
	(conf-done nil))
    ;; Insert all variables, and augment them with details from
    ;; the current configuration.
    (mapc (lambda (c)
	    (insert (car c) "=")
	    (if (assoc (car c) conf-table)
		(progn
		  (insert (cdr (assoc (car c) conf-table)) " ")
		  (setq conf-done (cons (car c) conf-done))))
	    (insert (cdr c) "\n"))
	  (oref this variables))
    ;; Add in all variables from the configuration not already covered.
    (mapc (lambda (c)
	    (if (member (car c) conf-done)
		nil
	      (insert (car c) "=" (cdr c) "\n")))
	  conf-table))
  (let* ((top "")
	 (tmp this))
    ;; Use relative paths for subdirs.
    (while (ede-parent-project tmp)
      (setq tmp (ede-parent-project tmp)
	    top (concat "../" top)))
    ;; If this is the top, then use CURDIR.
    (if (and (not (oref this metasubproject)) (string= top ""))
	(insert "\ntop=\"$(CURDIR)\"/")
      (insert "\ntop=" top)))
  (insert "\nede_FILES=" (file-name-nondirectory (oref this file)) " "
	  (file-name-nondirectory (ede-proj-dist-makefile this)) "\n"))

(defmethod ede-proj-makefile-insert-source-variables ((this ede-proj-target)
						      &optional
						      moresource)
  "Insert the source variables needed by THIS.
Optional argument MORESOURCE is a list of additional sources to add to the
sources variable."
  (let ((sv (ede-proj-makefile-sourcevar this)))
    ;; This variable may be shared between targets
    (ede-pmake-insert-variable-shared (cond ((listp sv) (car sv))
					    (t sv))
      (insert (mapconcat (lambda (a) a) (oref this source) " "))
      (if moresource
	  (insert " \\\n   " (mapconcat (lambda (a) a) moresource " ") "")))))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target) &optional
					       moresource)
  "Insert variables needed by target THIS.
Optional argument MORESOURCE is a list of additional sources to add to the
sources variable."
  (ede-proj-makefile-insert-source-variables this moresource)
  )

(defmethod ede-proj-makefile-configuration-variables ((this ede-proj-target-makefile)
						      configuration)
  "Return a list of configuration variables from THIS.
Use CONFIGURATION as the current configuration to query."
  (cdr (assoc configuration (oref this configuration-variables))))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-makefile)
					       &optional moresource)
  "Insert variables needed by target THIS.
Optional argument MORESOURCE is a list of additional sources to add to the
sources variable."
  (call-next-method)
  (let* ((proj (ede-target-parent this))
	 (conf-table (ede-proj-makefile-configuration-variables
		      this (oref proj configuration-default)))
	 (conf-done nil)
	 )
    ;; Add in all variables from the configuration not already covered.
    (mapc (lambda (c)
	    (if (member (car c) conf-done)
		nil
	      (insert (car c) "=" (cdr c) "\n")))
	  conf-table))
  (let ((comp (ede-proj-compilers this))
	(link (ede-proj-linkers this))
	(name (ede-proj-makefile-target-name this))
	(src (oref this source)))
    (ede-proj-makefile-insert-object-variables (car comp) name src)
    (dolist (obj comp)
      (ede-compiler-only-once obj
			      (ede-proj-makefile-insert-variables obj)))
    (dolist (linker link)
      (ede-linker-only-once linker
			    (ede-proj-makefile-insert-variables linker)))))

(defmethod ede-proj-makefile-insert-automake-pre-variables
  ((this ede-proj-target))
  "Insert variables needed by target THIS in Makefile.am before SOURCES."
  nil)

(defmethod ede-proj-makefile-insert-automake-post-variables
  ((this ede-proj-target))
  "Insert variables needed by target THIS in Makefile.am after SOURCES."
  nil)

;;; GARBAGE PATTERNS
;;
(defmethod ede-proj-makefile-garbage-patterns ((this ede-proj-project))
  "Return a list of patterns that are considered garbage to THIS.
These are removed with make clean."
  (let ((mc (ede-map-targets
	     this (lambda (c) (ede-proj-makefile-garbage-patterns c))))
	(uniq nil))
    (setq mc (sort (apply 'append mc) 'string<))
    ;; Filter out duplicates from the targets.
    (while mc
      (if (and (car uniq) (string= (car uniq) (car mc)))
	  nil
	(setq uniq (cons (car mc) uniq)))
      (setq mc (cdr mc)))
    (nreverse uniq)))

(defmethod ede-proj-makefile-garbage-patterns ((this ede-proj-target))
  "Return a list of patterns that are considered garbage to THIS.
These are removed with make clean."
  ;; Get the source object from THIS, and use the specified garbage.
  (let ((src (ede-target-sourcecode this))
	(garb nil))
    (while src
      (setq garb (append (oref (car src) garbagepattern) garb)
	    src (cdr src)))
    garb))


;;; RULES
;;
(defmethod ede-proj-makefile-insert-subproj-rules ((this ede-proj-project))
  "Insert a rule for the project THIS which should be a subproject."
  (insert ".PHONY:" (ede-name this))
  (newline)
  (insert (ede-name this) ":")
  (newline)
  (insert "\t$(MAKE) -C " (directory-file-name (ede-subproject-relative-path this)))
  (newline)
  (newline)
  )

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-project))
  "Insert rules needed by THIS target."
  (mapc 'ede-proj-makefile-insert-rules (oref this inference-rules))
  )

(defmethod ede-proj-makefile-insert-dist-dependencies ((this ede-proj-project))
  "Insert any symbols that the DIST rule should depend on.
Argument THIS is the project that should insert stuff."
  (mapc 'ede-proj-makefile-insert-dist-dependencies (oref this targets))
  )

(defmethod ede-proj-makefile-insert-dist-dependencies ((this ede-proj-target))
  "Insert any symbols that the DIST rule should depend on.
Argument THIS is the target that should insert stuff."
  nil)

(defmethod ede-proj-makefile-insert-dist-filepatterns ((this ede-proj-target))
  "Insert any symbols that the DIST rule should depend on.
Argument THIS is the target that should insert stuff."
  (ede-proj-makefile-insert-dist-dependencies this)
  )

(defmethod ede-proj-makefile-automake-insert-subdirs ((this ede-proj-project))
  "Insert a SUBDIRS variable for Automake."
  (proj-comp-insert-variable-once "SUBDIRS"
    (ede-map-subprojects
     this (lambda (sproj)
	    (insert " " (ede-subproject-relative-path sproj))
	    ))))

(defmethod ede-proj-makefile-automake-insert-extradist ((this ede-proj-project))
  "Insert the EXTRADIST variable entries needed for Automake and EDE."
  (proj-comp-insert-variable-once "EXTRA_DIST" (insert "Project.ede")))

(defmethod ede-proj-makefile-insert-dist-rules ((this ede-proj-project))
  "Insert distribution rules for THIS in a Makefile, such as CLEAN and DIST."
  (let ((junk (ede-proj-makefile-garbage-patterns this))
	tmp)
    ;; Build CLEAN, DIST, TAG, and other rules here.
    (if junk
	(insert "\nclean:\n"
		"\trm -f "
		(mapconcat (lambda (c) c) junk " ")
		"\n\n"))
    ;; @TODO: ^^^ Clean should also recurse. ^^^

    (insert ".PHONY: dist\n")
    (insert "\ndist:")
    (ede-proj-makefile-insert-dist-dependencies this)
    (insert "\n")
    (unless (or (ede-subproject-p this)
		(oref this metasubproject))
      ;; Only delete if we are the toplevel project.
      (insert "\trm -rf $(DISTDIR)\n"))
    (insert "\tmkdir $(DISTDIR)\n")	;We may need a -p, but I think not.
    (setq tmp (oref this targets))
    (insert "\tcp")
    (while tmp
      (let ((sv (ede-proj-makefile-sourcevar (car tmp))))
	(if (listp sv)
	    ;; Handle special case variables.
	    (cond ((eq (cdr sv) 'share)
		   ;; This variable may be shared between multiple targets.
		   (if (re-search-backward (concat "\\$(" (car sv) ")")
					   (point-at-bol) t)
		       ;; If its already in the dist target, then skip it.
		       nil
		     (setq sv (car sv))))
		  (t (setq sv (car sv)))))
	(if (stringp sv)
	    (insert " $(" sv ")"))
	(ede-proj-makefile-insert-dist-filepatterns (car tmp))
	(setq tmp (cdr tmp))))
    (insert " $(ede_FILES) $(DISTDIR)\n")

    ;; Call our sub projects.
    (ede-map-subprojects
     this (lambda (sproj)
	    (let ((rp (directory-file-name (ede-subproject-relative-path sproj))))
	      (insert "\t$(MAKE) -C " rp " $(MFLAGS) DISTDIR=$(DISTDIR)/" rp
		      " dist"
		      "\n"))))

    ;; Tar up the stuff.
    (unless (or (ede-subproject-p this)
		(oref this metasubproject))
      (insert "\ttar -cvzf $(DISTDIR).tar.gz $(DISTDIR)\n"
	      "\trm -rf $(DISTDIR)\n"))

    ;; Make sure the Makefile is ok.
    (insert "\n"
	    (file-name-nondirectory (buffer-file-name)) ": "
	    (file-name-nondirectory (oref this file)) "\n"
;;	    "$(EMACS) -batch Project.ede -l ede -f ede-proj-regenerate"
	    "\t@echo Makefile is out of date!  "
	    "It needs to be regenerated by EDE.\n"
	    "\t@echo If you have not modified Project.ede, you can"
	    " use 'touch' to update the Makefile time stamp.\n"
	    "\t@false\n\n"
	    "\n\n# End of Makefile\n")))

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target))
  "Insert rules needed by THIS target."
  nil)

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile))
  "Insert rules needed by THIS target."
  (mapc 'ede-proj-makefile-insert-rules (oref this rules))
  (let ((c (ede-proj-compilers this)))
    (when c
      (mapc 'ede-proj-makefile-insert-rules c)
      (if (oref this phony)
	  (insert ".PHONY: " (ede-proj-makefile-target-name this) "\n"))
      (insert (ede-proj-makefile-target-name this) ": "
	      (ede-proj-makefile-dependencies this) "\n")
      (ede-proj-makefile-insert-commands this)
      )))

(defmethod ede-proj-makefile-insert-commands ((this ede-proj-target-makefile))
  "Insert the commands needed by target THIS.
For targets, insert the commands needed by the chosen compiler."
  (mapc 'ede-proj-makefile-insert-commands (ede-proj-compilers this))
  (when (object-assoc t :uselinker (ede-proj-compilers this))
    (mapc 'ede-proj-makefile-insert-commands (ede-proj-linkers this))))


(defmethod ede-proj-makefile-insert-user-rules ((this ede-proj-project))
  "Insert user specified rules needed by THIS target.
This is different from `ede-proj-makefile-insert-rules' in that this
function won't create the building rules which are auto created with
automake."
  (mapc 'ede-proj-makefile-insert-user-rules (oref this inference-rules)))

(defmethod ede-proj-makefile-insert-user-rules ((this ede-proj-target))
  "Insert user specified rules needed by THIS target."
  (mapc 'ede-proj-makefile-insert-rules (oref this rules)))

(defmethod ede-proj-makefile-dependencies ((this ede-proj-target-makefile))
  "Return a string representing the dependencies for THIS.
Some compilers only use the first element in the dependencies, others
have a list of intermediates (object files), and others don't care.
This allows customization of how these elements appear."
  (let* ((c (ede-proj-compilers this))
	 (io (eval (cons 'or (mapcar 'ede-compiler-intermediate-objects-p c))))
	 (out nil))
    (if io
	(progn
	  (while c
	    (setq out
		  (concat out "$(" (ede-compiler-intermediate-object-variable
				    (car c)
				    (ede-proj-makefile-target-name this)) ")")
		  c (cdr c)))
	  out)
      (let ((sv (ede-proj-makefile-sourcevar this))
	    (aux (oref this auxsource)))
	(setq out
	      (if (and (stringp sv) (not (string= sv "")))
		  (concat "$(" sv ")")
		""))
	(while aux
	  (setq out (concat out " " (car aux)))
	  (setq aux (cdr aux)))
	out))))

;; Tags
(defmethod ede-proj-makefile-tags ((this ede-proj-project) targets)
  "Insert into the current location rules to make recursive TAGS files.
Argument THIS is the project to create tags for.
Argument TARGETS are the targets we should depend on for TAGS."
  (insert "tags: ")
  (let ((tg targets))
    ;; Loop over all source variables and insert them
    (while tg
      (insert "$(" (ede-proj-makefile-sourcevar (car tg)) ") ")
      (setq tg (cdr tg)))
    (insert "\n")
    (if targets
	(insert "\tetags $^\n"))
    ;; Now recurse into all subprojects
    (setq tg (oref this subproj))
    (while tg
      (insert "\t$(MAKE) -C " (ede-subproject-relative-path (car tg)) " $(MFLAGS) $@\n")
      (setq tg (cdr tg)))
    (insert "\n")))


(provide 'ede/pmake)

;;; ede/pmake.el ends here
