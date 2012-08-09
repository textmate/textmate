;;; ede/pconf.el --- configure.ac maintenance for EDE

;;; Copyright (C) 1998-2000, 2005, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project

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
;; Code generator for autoconf configure.ac, and support files.

(require 'ede/proj)
(require 'ede/autoconf-edit)
(defvar compilation-in-progress)

(defvar ede-pconf-create-file-query 'ask
  "Controls if queries are made while creating project files.
A value of 'ask means to always ask the user before creating
a file, such as AUTHORS.  A value of 'never means don't ask, and
don't do it.  A value of nil means to just do it.")

;;; Code:
(defmethod ede-proj-configure-file ((this ede-proj-project))
  "The configure.ac script used by project THIS."
  (ede-expand-filename (ede-toplevel this) "configure.ac" t))

(defmethod ede-proj-configure-test-required-file ((this ede-proj-project) file)
  "For project THIS, test that the file FILE exists, or create it."
  (let ((f (ede-expand-filename (ede-toplevel this) file t)))
    (when (not (file-exists-p f))
      (save-excursion
	(find-file f)
	(cond ((string= file "AUTHORS")
	       (insert (user-full-name) " <" (user-login-name) ">"))
	      ((string= file "NEWS")
	       (insert "NEWS file for " (ede-name this)))
	      (t (insert "\n")))
	(save-buffer)
	(when
	    (and (eq ede-pconf-create-file-query 'ask)
		 (not (eq ede-pconf-create-file-query 'never))
		 (not (y-or-n-p
		       (format "I had to create the %s file for you.  Ok? " file)))
		 (error "Quit")))))))


(defmethod ede-proj-configure-synchronize ((this ede-proj-project))
  "Synchronize what we know about project THIS into configure.ac."
  (let ((b (find-file-noselect (ede-proj-configure-file this)))
	;;(td (file-name-directory (ede-proj-configure-file this)))
	(targs (oref this targets))
	(postcmd "")
	(add-missing nil))
    ;; First, make sure we have a file.
    (if (not (file-exists-p (ede-proj-configure-file this)))
	(autoconf-new-program b (oref this name) "Project.ede"))
    (set-buffer b)
    ;; Next, verify all targets of all subobjects.
    (autoconf-set-version (oref this version))
    (let ((top-level-project-local this))
      (autoconf-set-output
       (ede-map-all-subprojects
	this
	(lambda (sp)
	  ;; NOTE: don't put in ./Makefile - configure complains.
	  (let ((dir (file-name-as-directory
		      (directory-file-name
		       (ede-subproject-relative-path sp top-level-project-local)))))
	    (when (string= dir "./") (setq dir ""))
	    ;; Use concat, because expand-file-name removes the relativity.
	    (concat dir "Makefile") )))))
    ;;
    ;; NOTE TO SELF.  TURN THIS INTO THE OFFICIAL LIST
    ;;
    (ede-proj-dist-makefile this)
    ;; Loop over all targets to clean and then add themselves in.
    (ede-map-all-subprojects
     this
     (lambda (sp)
       (ede-map-targets sp 'ede-proj-flush-autoconf)))
    (ede-map-all-subprojects
     this
     (lambda (sp)
       (ede-map-targets this 'ede-proj-tweak-autoconf)))
    ;; Now save
    (save-buffer)
    (setq postcmd "autoreconf -i;")

    ;; Verify a bunch of files that are required by automake.
    (ede-proj-configure-test-required-file this "AUTHORS")
    (ede-proj-configure-test-required-file this "NEWS")
    (ede-proj-configure-test-required-file this "README")
    (ede-proj-configure-test-required-file this "ChangeLog")
    ;; Let specific targets get missing files.
    (mapc 'ede-proj-configure-create-missing targs)
    ;; Verify that we have a make system.
    (if (or (not (ede-expand-filename (ede-toplevel this) "Makefile"))
	    ;; Now is this one of our old Makefiles?
	    (with-current-buffer
                (find-file-noselect
                 (ede-expand-filename (ede-toplevel this)
                                      "Makefile" t) t)
	      (goto-char (point-min))
	      ;; Here is the unique piece for our makefiles.
	      (re-search-forward "For use with: make" nil t)))
	(setq postcmd (concat postcmd "./configure;")))
    (if (not (string= "" postcmd))
	(progn
	  (compile postcmd)

	  (while compilation-in-progress
	    (accept-process-output)
	    ;; If sit for indicates that input is waiting, then
	    ;; read and discard whatever it is that is going on.
	    (when (not (sit-for 1))
	      (read-event nil nil .1)
	      ))

	  (with-current-buffer "*compilation*"
	    (goto-char (point-max))

	    (when (not (string= mode-line-process ":exit [0]"))
	      (error "Configure failed!"))

	    ;; The Makefile is now recreated by configure?
	    (let ((b (get-file-buffer
		      (ede-expand-filename (ede-toplevel this)
					   "Makefile" 'newfile))))
	      ;; This makes sure that if Makefile was loaded, and old,
	      ;; that it gets flushed so we don't keep rebuilding
	      ;; the autoconf system.
	      (if b (kill-buffer b))))

	  ))))

(defmethod ede-proj-configure-recreate ((this ede-proj-project))
  "Delete project THIS's configure script and start over."
  (if (not (ede-proj-configure-file this))
      (error "Could not determine configure.ac for %S" (object-name this)))
  (let ((b (get-file-buffer (ede-proj-configure-file this))))
    ;; Destroy all evidence of the old configure.ac
    (delete-file (ede-proj-configure-file this))
    (if b (kill-buffer b)))
  (ede-proj-configure-synchronize this))

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target))
  "Tweak the configure file (current buffer) to accommodate THIS."
  ;; Check the compilers belonging to THIS, and call the autoconf
  ;; setup for those compilers.
  (mapc 'ede-proj-tweak-autoconf (ede-proj-compilers this))
  (mapc 'ede-proj-tweak-autoconf (ede-proj-linkers this))
  )

(defmethod ede-proj-flush-autoconf ((this ede-proj-target))
  "Flush the configure file (current buffer) to accommodate THIS.
By flushing, remove any cruft that may be in the file.  Subsequent
calls to `ede-proj-tweak-autoconf' can restore items removed by flush."
  nil)

(defmethod ede-proj-configure-add-missing ((this ede-proj-target))
  "Query if any files needed by THIS provided by automake are missing.
Results in --add-missing being passed to automake."
  nil)

(defmethod ede-proj-configure-create-missing ((this ede-proj-target))
  "Add any missing files for THIS by creating them."
  nil)

(provide 'ede/pconf)

;;; ede/pconf.el ends here
