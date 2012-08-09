;;; ede/emacs.el --- Special project for Emacs

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Provide a special project type just for Emacs, cause Emacs is special.
;;
;; Identifies an Emacs project automatically.
;; Speedy ede-expand-filename based on extension.
;; Pre-populates the preprocessor map from lisp.h
;;
;; ToDo :
;; * Add "build" options.
;; * Add texinfo lookup options.
;; * Add website

(require 'ede)
(declare-function semanticdb-file-table-object "semantic/db")
(declare-function semanticdb-needs-refresh-p "semantic/db")
(declare-function semanticdb-refresh-table "semantic/db")

;;; Code:
(defvar ede-emacs-project-list nil
  "List of projects created by option `ede-emacs-project'.")

(defun ede-emacs-file-existing (dir)
  "Find a Emacs project in the list of Emacs projects.
DIR is the directory to search from."
  (let ((projs ede-emacs-project-list)
	(ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
	(when (string-match (concat "^" (regexp-quote root))
			    (file-name-as-directory dir))
	  (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

;;;###autoload
(defun ede-emacs-project-root (&optional dir)
  "Get the root directory for DIR."
  (when (not dir) (setq dir default-directory))
  (let ((case-fold-search t)
	(proj (ede-emacs-file-existing dir)))
    (if proj
	(ede-up-directory (file-name-directory
			   (oref proj :file)))
      ;; No pre-existing project.  Let's take a wild-guess if we have
      ;; an Emacs project here.
      (when (string-match "emacs[^/]*" dir)
	(let ((base (substring dir 0 (match-end 0))))
	  (when (file-exists-p (expand-file-name "src/emacs.c" base))
	      base))))))

(defun ede-emacs-version (dir)
  "Find the Emacs version for the Emacs src in DIR.
Return a tuple of ( EMACSNAME . VERSION )."
  (let ((buff (get-buffer-create " *emacs-query*"))
	(emacs "Emacs")
	(ver ""))
    (with-current-buffer buff
      (erase-buffer)
      (setq default-directory (file-name-as-directory dir))
      ;(call-process "egrep" nil buff nil "-n" "-e" "^version=" "Makefile")
      (call-process "egrep" nil buff nil "-n" "-e" "AC_INIT" "configure.in")
      (goto-char (point-min))
      ;(re-search-forward "version=\\([0-9.]+\\)")
      (cond
       ;; Maybe XEmacs?
       ((file-exists-p "version.sh")
	(setq emacs "XEmacs")
	(insert-file-contents "version.sh")
	(goto-char (point-min))
	(re-search-forward "emacs_major_version=\\([0-9]+\\)
emacs_minor_version=\\([0-9]+\\)
emacs_beta_version=\\([0-9]+\\)")
	(setq ver (concat (match-string 1) "."
			  (match-string 2) "."
			  (match-string 3)))
	)
       ;; Insert other Emacs here...

       ;; Vaguely recent version of GNU Emacs?
       (t
	(insert-file-contents "configure.in")
	(goto-char (point-min))
	(re-search-forward "AC_INIT(emacs,\\s-*\\([0-9.]+\\)\\s-*)")
	(setq ver (match-string 1))
	)
       )
      ;; Return a tuple
      (cons emacs ver))))

(defclass ede-emacs-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-emacs-project-list)
   )
  "Project Type for the Emacs source code."
  :method-invocation-order :depth-first)

(defun ede-emacs-load (dir &optional rootproj)
  "Return an Emacs Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-emacs-file-existing dir)
      ;; Doesn't already exist, so let's make one.
      (let* ((vertuple (ede-emacs-version dir)))
	(ede-emacs-project (car vertuple)
			   :name (car vertuple)
			   :version (cdr vertuple)
			   :directory (file-name-as-directory dir)
			   :file (expand-file-name "src/emacs.c"
						   dir)))
      (ede-add-project-to-global-list this)
      )
  )

;;;###autoload
(add-to-list 'ede-project-class-files
	     (ede-project-autoload "emacs"
	      :name "EMACS ROOT"
	      :file 'ede/emacs
	      :proj-file "src/emacs.c"
	      :proj-root 'ede-emacs-project-root
	      :load-type 'ede-emacs-load
	      :class-sym 'ede-emacs-project
	      :new-p nil)
	     t)

(defclass ede-emacs-target-c (ede-target)
  ()
  "EDE Emacs Project target for C code.
All directories need at least one target.")

(defclass ede-emacs-target-el (ede-target)
  ()
  "EDE Emacs Project target for Emacs Lisp code.
All directories need at least one target.")

(defclass ede-emacs-target-misc (ede-target)
  ()
  "EDE Emacs Project target for Misc files.
All directories need at least one target.")

(defmethod initialize-instance ((this ede-emacs-project)
				&rest fields)
  "Make sure the targets slot is bound."
  (call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil)))

;;; File Stuff
;;
(defmethod ede-project-root-directory ((this ede-emacs-project)
				       &optional file)
  "Return the root for THIS Emacs project with file."
  (ede-up-directory (file-name-directory (oref this file))))

(defmethod ede-project-root ((this ede-emacs-project))
  "Return my root."
  this)

(defmethod ede-find-subproject-for-directory ((proj ede-emacs-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;; TARGET MANAGEMENT
;;
(defun ede-emacs-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
		 (string= (oref T :path) dir))
	(setq match T)
      ))
    match))

(defmethod ede-find-target ((proj ede-emacs-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
	 (cls (cond ((not ext)
		     'ede-emacs-target-misc)
		    ((string-match "c\\|h" ext)
		     'ede-emacs-target-c)
		    ((string-match "elc?" ext)
		     'ede-emacs-target-el)
		    (t 'ede-emacs-target-misc)))
	 (targets (oref proj targets))
	 (dir default-directory)
	 (ans (ede-emacs-find-matching-target cls dir targets))
	 )
    (when (not ans)
      (setq ans (make-instance
		 cls
		 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; UTILITIES SUPPORT.
;;
(defmethod ede-preprocessor-map ((this ede-emacs-target-c))
  "Get the pre-processor map for Emacs C code.
All files need the macros from lisp.h!"
  (require 'semantic/db)
  (let* ((proj (ede-target-parent this))
	 (root (ede-project-root proj))
	 (table (semanticdb-file-table-object
		 (ede-expand-filename root "lisp.h")))
	 (config (semanticdb-file-table-object
		  (ede-expand-filename root "config.h")))
	 filemap
	 )
    (when table
      (when (semanticdb-needs-refresh-p table)
	(semanticdb-refresh-table table))
      (setq filemap (append filemap (oref table lexical-table)))
      )
    (when config
      (when (semanticdb-needs-refresh-p config)
	(semanticdb-refresh-table config))
      (setq filemap (append filemap (oref config lexical-table)))
      )
    filemap
    ))

(defun ede-emacs-find-in-directories (name base dirs)
  "Find NAME is BASE directory sublist of DIRS."
  (let ((ans nil))
    (while (and dirs (not ans))
      (let* ((D (car dirs))
	     (ed (expand-file-name D base))
	     (ef (expand-file-name name ed)))
	(if (file-exists-p ef)
	    (setq ans ef)
	  ;; Not in this dir?  How about subdirs?
	  (let ((dirfile (directory-files ed t))
		(moredirs nil)
		)
	    ;; Get all the subdirs.
	    (dolist (DF dirfile)
	      (when (and (file-directory-p DF)
			 (not (string-match "\\.$" DF)))
		(push DF moredirs)))
	    ;; Try again.
	    (setq ans (ede-emacs-find-in-directories name ed moredirs))
	    ))
	(setq dirs (cdr dirs))))
    ans))

(defmethod ede-expand-filename-impl ((proj ede-emacs-project) name)
  "Within this project PROJ, find the file NAME.
Knows about how the Emacs source tree is organized."
  (let* ((ext (file-name-extension name))
	 (root (ede-project-root proj))
	 (dir (ede-project-root-directory root))
	 (dirs (cond
		((not ext) nil)
		((string-match "h\\|c" ext)
		 '("src" "lib-src" "lwlib"))
		((string-match "elc?" ext)
		 '("lisp"))
		((string-match "texi" ext)
		 '("doc"))
		(t nil)))
	 )
    (if (not dirs) (call-next-method)
      (ede-emacs-find-in-directories name dir dirs))
    ))

(provide 'ede/emacs)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/emacs"
;; End:

;;; ede/emacs.el ends here
