;;; ede-proj-elisp.el --- EDE Generic Project Emacs Lisp support

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
;; Handle Emacs Lisp in an EDE Project file.

(require 'ede/proj)
(require 'ede/pmake)
(require 'ede/pconf)

(autoload 'semantic-ede-proj-target-grammar "semantic/ede-grammar")

;;; Code:
(defclass ede-proj-target-elisp (ede-proj-target-makefile)
  ((menu :initform nil)
   (keybindings :initform nil)
   (phony :initform t)
   (sourcetype :initform '(ede-source-emacs))
   (availablecompilers :initform '(ede-emacs-compiler ede-xemacs-compiler))
   (aux-packages :initarg :aux-packages
		 :initform nil
		 :type list
		 :custom (repeat string)
		 :documentation "Additional packages needed.
There should only be one toplevel package per auxiliary tool needed.
These packages location is found, and added to the compile time
load path."
   ))
  "This target consists of a group of lisp files.
A lisp target may be one general program with many separate lisp files in it.")

(defvar ede-source-emacs
  (ede-sourcecode "ede-emacs-source"
		  :name "Emacs Lisp"
		  :sourcepattern "\\.el$"
		  :garbagepattern '("*.elc"))
  "Emacs Lisp source code definition.")

(defvar ede-emacs-compiler
  (ede-compiler
   "ede-emacs-compiler"
   :name "emacs"
   :variables '(("EMACS" . "emacs")
		("EMACSFLAGS" . "-batch --no-site-file"))
   :commands
   '("@echo \"(add-to-list 'load-path nil)\" > $@-compile-script"
     "for loadpath in . ${LOADPATH}; do \\"
     "   echo \"(add-to-list 'load-path \\\"$$loadpath\\\")\" >> $@-compile-script; \\"
     "done;"
     "@echo \"(setq debug-on-error t)\" >> $@-compile-script"
     "\"$(EMACS)\" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^"
     )
   :autoconf '("AM_PATH_LISPDIR")
   :sourcetype '(ede-source-emacs)
;   :objectextention ".elc"
   )
  "Compile Emacs Lisp programs.")

(defvar ede-xemacs-compiler
  (clone ede-emacs-compiler "ede-xemacs-compiler"
	 :name "xemacs"
	 :variables '(("EMACS" . "xemacs")))
  "Compile Emacs Lisp programs with XEmacs.")

;;; Claiming files
(defmethod ede-buffer-mine ((this ede-proj-target-elisp) buffer)
  "Return t if object THIS lays claim to the file in BUFFER.
Lays claim to all .elc files that match .el files in this target."
  (if (string-match "\\.elc$" (buffer-file-name buffer))
      (let ((fname
	     (concat
	      (file-name-sans-extension (buffer-file-name buffer))
	      ".el")
	     ))
	;; Is this in our list.
	(member fname (oref this auxsource))
	)
    (call-next-method) ; The usual thing.
    ))

;;; Emacs Lisp Compiler
;;; Emacs Lisp Target
(defun ede-proj-elisp-packages-to-loadpath (packages)
  "Convert a list of PACKAGES, to a list of load path."
  (let ((paths nil)
	(ldir nil))
    (while packages
      (or (setq ldir (locate-library (car packages)))
	  (error "Cannot find package %s" (car packages)))
      (let* ((fnd (file-name-directory ldir))
	     (rel (file-relative-name fnd))
	     (full nil)
	     )
	;; Make sure the relative name isn't to far off
	(when (string-match "^\\.\\./\\.\\./\\.\\./\\.\\." rel)
	  (setq full fnd))
	;; Do the setup.
	(setq paths (cons (or full rel) paths)
	      packages (cdr packages))))
    paths))

(defmethod project-compile-target ((obj ede-proj-target-elisp))
  "Compile all sources in a Lisp target OBJ.
Bonus: Return a cons cell: (COMPILED . UPTODATE)."
  (let* ((proj (ede-target-parent obj))
	 (dir (oref proj directory))
	 (comp 0)
	 (utd 0))
    (mapc (lambda (src)
	    (let* ((fsrc (expand-file-name src dir))
		   (elc (concat (file-name-sans-extension fsrc) ".elc")))
	      (if (eq (byte-recompile-file fsrc nil 0) t)
                  (setq comp (1+ comp))
		(setq utd (1+ utd)))))
	    (oref obj source))
    (message "All Emacs Lisp sources are up to date in %s" (object-name obj))
    (cons comp utd)))

(defmethod ede-update-version-in-source ((this ede-proj-target-elisp) version)
  "In a Lisp file, updated a version string for THIS to VERSION.
There are standards in Elisp files specifying how the version string
is found, such as a `-version' variable, or the standard header."
  (if (and (slot-boundp this 'versionsource)
	   (oref this versionsource))
      (let ((vs (oref this versionsource))
	    (match nil))
	(while vs
	  (with-current-buffer (find-file-noselect
                                (ede-expand-filename this (car vs)))
	    (goto-char (point-min))
	    (let ((case-fold-search t))
	      (if (re-search-forward "-version\\s-+\"\\([^\"]+\\)\"" nil t)
		  (progn
		    (setq match t)
		    (delete-region (match-beginning 1)
				   (match-end 1))
		    (goto-char (match-beginning 1))
		    (insert version)))))
	  (setq vs (cdr vs)))
	(if (not match) (call-next-method)))))


;;; Makefile generation functions
;;
(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-elisp))
  "Return the variable name for THIS's sources."
  (cond ((ede-proj-automake-p) '("lisp_LISP" . share))
	(t (concat (ede-pmake-varname this) "_LISP"))))

(defun ede-proj-makefile-insert-loadpath-items (items)
  "Insert a sequence of ITEMS into the Makefile LOADPATH variable."
    (when items
      (ede-pmake-insert-variable-shared "LOADPATH"
	(let ((begin (save-excursion (re-search-backward "\\s-*="))))
	  (while items
	    (when (not (save-excursion
			 (re-search-backward
			  (concat "\\s-" (regexp-quote (car items)) "[ \n\t\\]")
			  begin t)))
	      (insert " " (car items)))
	    (setq items (cdr items)))))
      ))

(defmethod ede-proj-makefile-insert-variables :AFTER ((this ede-proj-target-elisp))
  "Insert variables needed by target THIS."
  (let ((newitems (if (oref this aux-packages)
		      (ede-proj-elisp-packages-to-loadpath
		       (oref this aux-packages))))
	)
    (ede-proj-makefile-insert-loadpath-items newitems)))

(defun ede-proj-elisp-add-path (path)
  "Add path PATH into the file if it isn't already there."
  (goto-char (point-min))
  (if (re-search-forward (concat "(cons \\\""
				 (regexp-quote path))
			 nil t)
      nil;; We have it already
    (if (re-search-forward "(cons nil" nil t)
	(progn
	  ;; insert stuff here
	  (end-of-line)
	  (insert "\n"
		  "   echo \"(setq load-path (cons \\\""
		  path
		  "\\\" load-path))\" >> script")
	  )
      (error "Don't know how to update load path"))))

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target-elisp))
  "Tweak the configure file (current buffer) to accommodate THIS."
  (call-next-method)
  ;; Ok, now we have to tweak the autoconf provided `elisp-comp' program.
  (let ((ec (ede-expand-filename this "elisp-comp" 'newfile)))
    (if (or (not ec) (not (file-exists-p ec)))
	(message "No elisp-comp file.  There may be compile errors?  Rerun a second time.")
      (save-excursion
	(if (file-symlink-p ec)
	    (progn
	      ;; Desymlinkify
	      (rename-file ec (concat ec ".tmp"))
	      (copy-file (concat ec ".tmp") ec)
	      (delete-file (concat ec ".tmp"))))
	(set-buffer (find-file-noselect ec t))
	(ede-proj-elisp-add-path "..")
	(let ((paths (ede-proj-elisp-packages-to-loadpath
		      (oref this aux-packages))))
	  ;; Add in the current list of paths
	  (while paths
	    (ede-proj-elisp-add-path (car paths))
	    (setq paths (cdr paths))))
	(save-buffer)) )))

(defmethod ede-proj-flush-autoconf ((this ede-proj-target-elisp))
  "Flush the configure file (current buffer) to accommodate THIS."
  ;; Remove crufty old paths from elisp-compile
  (let ((ec (ede-expand-filename this "elisp-comp" 'newfile))
	)
    (if (and ec (file-exists-p ec))
	(with-current-buffer (find-file-noselect ec t)
	  (goto-char (point-min))
	  (while (re-search-forward "(cons \\([^ ]+\\) load-path)"
				    nil t)
	    (let ((path (match-string 1)))
	      (if (string= path "nil")
		  nil
		(delete-region (point-at-bol) (point-at-bol 2)))))))))

;;;
;; Autoload generators
;;
(defclass ede-proj-target-elisp-autoloads (ede-proj-target-elisp)
  ((availablecompilers :initform '(ede-emacs-cedet-autogen-compiler))
   (aux-packages :initform ("cedet-autogen"))
   (phony :initform t)
   (autoload-file :initarg :autoload-file
		  :initform "loaddefs.el"
		  :type string
		  :custom string
		  :documentation "The file that autoload definitions are placed in.
There should be one load defs file for a given package.  The load defs are created
for all Emacs Lisp sources that exist in the directory of the created target.")
   (autoload-dirs :initarg :autoload-dirs
		  :initform nil
		  :type list
		  :custom (repeat string)
		  :documentation "The directories to scan for autoload definitions.
If nil defaults to the current directory.")
   )
  "Target that builds an autoload file.
Files do not need to be added to this target.")


;;; Claiming files
(defmethod ede-buffer-mine ((this ede-proj-target-elisp-autoloads) buffer)
  "Return t if object THIS lays claim to the file in BUFFER.
Lays claim to all .elc files that match .el files in this target."
  (if (string-match
       (concat (regexp-quote (oref this autoload-file)) "$")
       (buffer-file-name buffer))
      t
    (call-next-method) ; The usual thing.
    ))

;; Compilers
(defvar ede-emacs-cedet-autogen-compiler
  (ede-compiler
   "ede-emacs-autogen-compiler"
   :name "emacs"
   :variables '(("EMACS" . "emacs"))
   :commands
   '("@echo \"(add-to-list 'load-path nil)\" > $@-compile-script"
     "for loadpath in . ${LOADPATH}; do \\"
     "   echo \"(add-to-list 'load-path \\\"$$loadpath\\\")\" >> $@-compile-script; \\"
     "done;"
     "@echo \"(require 'cedet-autogen)\" >> $@-compile-script"
     "\"$(EMACS)\" -batch --no-site-file -l $@-compile-script -f cedet-batch-update-autoloads $(LOADDEFS) $(LOADDIRS)"
     )
   :sourcetype '(ede-source-emacs)
   )
  "Build an autoloads file.")

(defmethod ede-proj-compilers ((obj ede-proj-target-elisp-autoloads))
  "List of compilers being used by OBJ.
If the `compiler' slot is empty, get the car of the compilers list."
  (let ((comp (oref obj compiler)))
    (if comp
	(if (listp comp)
	    (setq comp (mapcar 'symbol-value comp))
	  (setq comp (list (symbol-value comp))))
      ;; Get the first element from our list of compilers.
      (let ((avail (mapcar 'symbol-value (oref obj availablecompilers))))
	(setq comp (list (car avail)))))
    comp))

(defmethod ede-proj-makefile-insert-source-variables ((this ede-proj-target-elisp-autoloads)
						      &optional
						      moresource)
  "Insert the source variables needed by THIS.
Optional argument MORESOURCE is a list of additional sources to add to the
sources variable."
  nil)

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-elisp-autoloads))
  "Return the variable name for THIS's sources."
  nil) ; "LOADDEFS")

(defmethod ede-proj-makefile-dependencies ((this ede-proj-target-elisp-autoloads))
  "Return a string representing the dependencies for THIS.
Always return an empty string for an autoloads generator."
  "")

(defmethod ede-proj-makefile-insert-variables :AFTER ((this ede-proj-target-elisp-autoloads))
  "Insert variables needed by target THIS."
  (ede-pmake-insert-variable-shared "LOADDEFS"
    (insert (oref this autoload-file)))
  (ede-pmake-insert-variable-shared "LOADDIRS"
    (insert (mapconcat 'identity
                       (or (oref this autoload-dirs) '("."))
                       " ")))
  )

(defmethod project-compile-target ((obj ede-proj-target-elisp-autoloads))
  "Create or update the autoload target."
  (require 'cedet-autogen)
  (let ((default-directory (ede-expand-filename obj ".")))
    (apply 'cedet-update-autoloads
	   (oref obj autoload-file)
	   (oref obj autoload-dirs))
    ))

(defmethod ede-update-version-in-source ((this ede-proj-target-elisp-autoloads) version)
  "In a Lisp file, updated a version string for THIS to VERSION.
There are standards in Elisp files specifying how the version string
is found, such as a `-version' variable, or the standard header."
  nil)

(defmethod ede-proj-makefile-insert-dist-dependencies ((this ede-proj-target-elisp-autoloads))
  "Insert any symbols that the DIST rule should depend on.
Emacs Lisp autoload files ship the generated .el files.
Argument THIS is the target which needs to insert an info file."
  ;; In some cases, this is ONLY the index file.  That should generally
  ;; be ok.
  (insert " " (ede-proj-makefile-target-name this))
  )

(defmethod ede-proj-makefile-insert-dist-filepatterns ((this ede-proj-target-elisp-autoloads))
  "Insert any symbols that the DIST rule should distribute.
Emacs Lisp autoload files ship the generated .el files.
Argument THIS is the target which needs to insert an info file."
  (insert " " (oref this autoload-file))
  )

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target-elisp-autoloads))
  "Tweak the configure file (current buffer) to accommodate THIS."
  (error "Autoloads not supported in autoconf yet"))

(defmethod ede-proj-flush-autoconf ((this ede-proj-target-elisp-autoloads))
  "Flush the configure file (current buffer) to accommodate THIS."
  nil)

(provide 'ede/proj-elisp)

;;; ede/proj-elisp.el ends here
