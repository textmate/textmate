;;; ede-proj-info.el --- EDE Generic Project texinfo support

;;; Copyright (C) 1998-2001, 2004, 2007-2012  Free Software Foundation, Inc.

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
;; Handle texinfo in and EDE Project file.

(require 'ede/pmake)

;;; Code:
(defclass ede-proj-target-makefile-info (ede-proj-target-makefile)
  ((menu :initform nil)
   (keybindings :initform nil)
   (availablecompilers :initform '(ede-makeinfo-compiler
				   ede-texi2html-compiler))
   (sourcetype :initform '(ede-makeinfo-source))
   (mainmenu :initarg :mainmenu
	     :initform ""
	     :type string
	     :custom string
	     :documentation "The main menu resides in this file.
All other sources should be included independently."))
  "Target for a single info file.")

(defvar ede-makeinfo-source
  (ede-sourcecode "ede-makeinfo-source"
		  :name "Texinfo"
		  :sourcepattern "\\.texi?$"
		  :garbagepattern '("*.info*" "*.html"))
  "Texinfo source code definition.")

(defvar ede-makeinfo-compiler
  (ede-compiler
   "ede-makeinfo-compiler"
   :name "makeinfo"
   :variables '(("MAKEINFO" . "makeinfo"))
   :commands '("$(MAKEINFO) $<")
   :autoconf '(("AC_CHECK_PROG" . "MAKEINFO, makeinfo"))
   :sourcetype '(ede-makeinfo-source)
   )
  "Compile texinfo files into info files.")

(defvar ede-texi2html-compiler
  (ede-compiler
   "ede-texi2html-compiler"
   :name "texi2html"
   :variables '(("TEXI2HTML" . "makeinfo -html"))
   :commands '("makeinfo -o $@ $<")
   :sourcetype '(ede-makeinfo-source)
   )
  "Compile texinfo files into html files.")

;;; Makefile generation
;;
(defmethod ede-proj-configure-add-missing
  ((this ede-proj-target-makefile-info))
  "Query if any files needed by THIS provided by automake are missing.
Results in --add-missing being passed to automake."
  (not (ede-expand-filename (ede-toplevel) "texinfo.tex")))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-info))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_TEXINFOS"))

(defmethod ede-proj-makefile-insert-source-variables
  ((this ede-proj-target-makefile-info) &optional moresource)
  "Insert the source variables needed by THIS info target.
Optional argument MORESOURCE is a list of additional sources to add to the
sources variable.
Does the usual for Makefile mode, but splits source into two variables
when working in Automake mode."
  (if (not (ede-proj-automake-p))
      (call-next-method)
    (let* ((sv (ede-proj-makefile-sourcevar this))
	   (src (copy-sequence (oref this source)))
	   (menu (or (oref this menu) (car src))))
      (setq src (delq menu src))
      ;; the info_TEXINFOS variable is probably shared
      (ede-pmake-insert-variable-shared "info_TEXINFOS"
	(insert menu))
      ;; Now insert the rest of the source elsewhere
      (ede-pmake-insert-variable-shared sv
	(insert (mapconcat 'identity src " ")))
      (if moresource
	  (error "Texinfo files should not have moresource")))))

(defun ede-makeinfo-find-info-filename (source)
  "Find the info filename produced by SOURCE texinfo file."
  (let ((opened (get-file-buffer source))
	(buffer (or (get-file-buffer source)
		    (find-file-noselect source nil t)))
	info)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(and (re-search-forward "^@setfilename\\s-+\\([^.]+\\).info$" nil t)
	     (setq info (match-string 1)))))
    (unless (eq buffer opened)
      (kill-buffer buffer))
    info))

(defmethod ede-proj-makefile-target-name ((this ede-proj-target-makefile-info))
  "Return the name of the main target for THIS target."
  ;; The target should be the main-menu file name translated to .info.
  (let* ((source (if (not (string= (oref this mainmenu) ""))
 		     (oref this mainmenu)
 		   (car (oref this source))))
 	 (info (ede-makeinfo-find-info-filename source)))
    (concat (or info (file-name-sans-extension source)) ".info")))

(defmethod ede-proj-makefile-insert-dist-dependencies ((this ede-proj-target-makefile-info))
  "Insert any symbols that the DIST rule should depend on.
Texinfo files want to insert generated `.info' files.
Argument THIS is the target which needs to insert an info file."
  ;; In some cases, this is ONLY the index file.  That should generally
  ;; be ok.
  (insert " " (ede-proj-makefile-target-name this))
  )

(defmethod ede-proj-makefile-insert-dist-filepatterns ((this ede-proj-target-makefile-info))
  "Insert any symbols that the DIST rule should depend on.
Texinfo files want to insert generated `.info' files.
Argument THIS is the target which needs to insert an info file."
  ;; In some cases, this is ONLY the index file.  That should generally
  ;; be ok.
  (insert " " (ede-proj-makefile-target-name this) "*")
  )

;  (let ((n (ede-name this)))
;    (if (string-match "\\.info$" n)
;	n
;      (concat n ".info"))))

(defmethod object-write ((this ede-proj-target-makefile-info))
  "Before committing any change to THIS, make sure the mainmenu is first."
   (let ((mm (oref this mainmenu))
	 (s (oref this source))
	 (nl nil))
     (if (or (string= mm "") (not mm) (string= mm (car s)))
	 nil
       ;; Make sure that MM is first in the list of items.
       (setq nl (cons mm (delq mm s)))
       (oset this source nl)))
   (call-next-method))

(defmethod ede-documentation ((this ede-proj-target-makefile-info))
  "Return a list of files that provides documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (let* ((src (oref this source))
	 (proj (ede-target-parent this))
	 (dir (oref proj directory))
	 (out nil)
	 )
    ;; convert src to full file names.
    (while src
      (setq out (cons
		 (expand-file-name (car src) dir)
		 out))
      (setq src (cdr src)))
    ;; Return it
    out))

(provide 'ede/proj-info)

;;; ede/proj-info.el ends here
