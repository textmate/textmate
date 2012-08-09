;;; cedet-cscope.el --- CScope support for CEDET

;;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Package: cedet

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
;; Support using CScope for symbol lookups.

;;; Code:

(declare-function inversion-check-version "inversion")

(defvar cedet-cscope-min-version "16.0"
  "Minimum version of CScope required.")

(defcustom cedet-cscope-command "cscope"
  "Command name for the CScope executable."
  :type 'string
  :group 'cedet)

(defun cedet-cscope-search (searchtext texttype type scope)
  "Perform a search with CScope, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs."
  ;; CScope is an interactive program.  It uses number flags
  ;; in order to perform command line searches.  Useful for this
  ;; tool are:
  ;;
  ;; -0 = Find C symbol
  ;; -1 = Find global definition
  ;; -3 = Find references
  ;; -6 = Find egrep pattern
  ;; -7 = Find file
  (let ((idx (cond ((eq type 'file)
		    "-7")
		   ;; Non files are symbols and such
		   ((eq texttype 'tagname)
		    "-1")
		   ((eq texttype 'tagregexp)
		    "-0")
		   ((eq texttype 'tagcompletions)
		    (setq searchtext (concat "^" searchtext ".*"))
		    "-1")
		   ((eq texttype 'regexp)
		    "-5")
		   (t
		    "-3")
		   )
	     )
	)
    (cedet-cscope-call (list "-d" "-L" idx searchtext))))

(defun cedet-cscope-create (flags)
  "Create a CScope database at the current directory.
FLAGS are additional flags to pass to cscope beyond the
options -cR."
  (cedet-cscope-call (append (list "-cR") flags)))

(defun cedet-cscope-call (flags)
  "Call CScope with the list of FLAGS."
  (let ((b (get-buffer-create "*CEDET CScope*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-cscope-command
	   nil b nil
	   flags)
    b))

(defun cedet-cscope-expand-filename (filename)
  "Expand the FILENAME with CScope.
Return a fully qualified filename."
  (interactive "sFile: ")
  (let* ((ans1 (with-current-buffer
                   (cedet-cscope-call (list "-d" "-L" "-7" filename))
		 (goto-char (point-min))
		 (if (looking-at "[^ \n]*cscope: ")
		     (error "CScope not available")
		   (split-string (buffer-string) "\n" t))))
	 (ans2 (mapcar (lambda (hit)
			 (expand-file-name (car (split-string hit " "))))
		       ans1)))
    (when (called-interactively-p 'interactive)
      (if ans2
	  (if (= (length ans2) 1)
	      (message "%s" (car ans2))
	    (message "%s + %d others" (car ans2)
		     (length (cdr ans2))))
	(error "No file found")))
    ans2))

(defun cedet-cscope-support-for-directory (&optional dir)
  "Return non-nil if CScope has a support file for DIR.
If DIR is not supplied, use the current default directory.
This works by running cscope on a bogus symbol, and looking for
the error code."
  (interactive "DDirectory: ")
  (save-excursion
    (let ((default-directory (or dir default-directory)))
      (set-buffer (cedet-cscope-call (list "-d" "-L" "-7" "moose")))
      (goto-char (point-min))
      (let ((ans (looking-at "[^ \n]*cscope: ")))
	(if (called-interactively-p 'interactive)
	    (if ans
		(message "No support for CScope in %s" default-directory)
	      (message "CScope is supported in %s" default-directory))
	  (if ans
	      nil
	    t))))))

(defun cedet-cscope-version-check (&optional noerror)
  "Check the version of the installed CScope command.
If optional programmatic argument NOERROR is non-nil,
then instead of throwing an error if CScope isn't available,
return nil."
  (interactive)
  (require 'inversion)
  (let ((b (condition-case nil
	       (cedet-cscope-call (list "-V"))
	     (error nil)))
	(rev nil))
    (if (not b)
	(progn
	  (when (called-interactively-p 'interactive)
	    (message "CScope not found."))
	  nil)
      (with-current-buffer b
	(goto-char (point-min))
	(re-search-forward "cscope: version \\([0-9.]+\\)" nil t)
	(setq rev (match-string 1))
	(if (inversion-check-version rev nil cedet-cscope-min-version)
	    (if noerror
		nil
	      (error "Version of CScope is %s.  Need at least %s"
		     rev cedet-cscope-min-version))
	  ;; Else, return TRUE, as in good enough.
	  (when (called-interactively-p 'interactive)
	    (message "CScope %s  - Good enough for CEDET." rev))
	  t)))))

(defun cedet-cscope-create/update-database (&optional dir)
  "Create a CScope database in DIR.
CScope will automatically choose incremental rebuild if
there is already a database in DIR."
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (cedet-cscope-create nil)))

(provide 'cedet-cscope)

;;; cedet-cscope.el ends here
