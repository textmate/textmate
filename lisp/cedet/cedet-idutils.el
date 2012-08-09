;;; cedet-idutils.el --- ID Utils support for CEDET.

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; Version: 0.2
;; Keywords: OO, lisp
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
;; Basic support calling ID Utils functions, and checking version
;; numbers.

;;; Code:

(declare-function inversion-check-version "inversion")

(defvar cedet-idutils-min-version "4.0"
  "Minimum version of ID Utils required.")

(defcustom cedet-idutils-file-command "fnid"
  "Command name for the ID Utils executable for searching file names."
  :type 'string
  :group 'cedet)

(defcustom cedet-idutils-token-command "lid"
  "Command name for the ID Utils executable for searching for tokens."
  :type 'string
  :group 'cedet)

(defcustom cedet-idutils-make-command "mkid"
  "Command name for the ID Utils executable for creating token databases."
  :type 'string
  :group 'cedet)

(defun cedet-idutils-search (searchtext texttype type scope)
  "Perform a search with ID Utils, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs.
Note: Scope is not yet supported."
  (if (eq type 'file)
      ;; Calls for file stuff is very simple.
      (cedet-idutils-fnid-call (list searchtext))
    ;; Calls for text searches is more complex.
    (let* ((resultflg (if (eq texttype 'tagcompletions)
			  (list "--key=token")
			(list "--result=grep")))
	   (scopeflgs nil) ; (cond ((eq scope 'project) "" ) ((eq scope 'target) "l")))
	   (stflag (cond ((or (eq texttype 'tagname)
			      (eq texttype 'tagregexp))
			  (list "-r" "-w"))
			 ((eq texttype 'tagcompletions)
			  ;; Add regex to search text for beginning of char.
			  (setq searchtext (concat "^" searchtext))
			  (list "-r" "-s" ))
			 ((eq texttype 'regexp)
			  (list "-r"))
			 ;; t means 'symbol
			 (t (list "-l" "-w"))))
	   )
      (cedet-idutils-lid-call (append resultflg scopeflgs stflag
				      (list searchtext))))))

(defun cedet-idutils-fnid-call (flags)
  "Call ID Utils fnid with the list of FLAGS.
Return the created buffer with program output."
  (let ((b (get-buffer-create "*CEDET fnid*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-idutils-file-command
	   nil b nil
	   flags)
    b))

(defun cedet-idutils-lid-call (flags)
  "Call ID Utils lid with the list of FLAGS.
Return the created buffer with with program output."
  (let ((b (get-buffer-create "*CEDET lid*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-idutils-token-command
	   nil b nil
	   flags)
    b))

(defun cedet-idutils-mkid-call (flags)
  "Call ID Utils mkid with the list of FLAGS.
Return the created buffer with program output."
  (let ((b (get-buffer-create "*CEDET mkid*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-idutils-make-command
	   nil b nil
	   flags)
    b))

;;; UTIL CALLS
;;
(defun cedet-idutils-expand-filename (filename)
  "Expand the FILENAME with ID Utils.
Return a filename relative to the default directory."
  (interactive "sFile: ")
  (let ((ans (with-current-buffer (cedet-idutils-fnid-call (list filename))
	       (goto-char (point-min))
	       (if (looking-at "[^ \n]*fnid: ")
		   (error "ID Utils not available")
		 (split-string (buffer-string) "\n" t)))))
    (setq ans (mapcar 'expand-file-name ans))
    (when (called-interactively-p 'interactive)
      (if ans
	  (if (= (length ans) 1)
	      (message "%s" (car ans))
	    (message "%s + %d others" (car ans)
		     (length (cdr ans))))
	(error "No file found")))
    ans))

(defun cedet-idutils-support-for-directory (&optional dir)
  "Return non-nil if ID Utils has a support file for DIR.
If DIR is not supplied, use the current default directory.
This works by running lid on a bogus symbol, and looking for
the error code."
  (save-excursion
    (let ((default-directory (or dir default-directory)))
      (condition-case nil
	  (progn
	    (set-buffer (cedet-idutils-fnid-call '("moose")))
	    (goto-char (point-min))
	    (if (looking-at "[^ \n]*fnid: ")
		nil
	      t))
	(error nil)))))

(defun cedet-idutils-version-check (&optional noerror)
  "Check the version of the installed ID Utils command.
If optional programmatic argument NOERROR is non-nil,
then instead of throwing an error if Global isn't available,
return nil."
  (interactive)
  (require 'inversion)
  (let ((b (condition-case nil
	       (cedet-idutils-fnid-call (list "--version"))
	     (error nil)))
	(rev nil))
    (if (not b)
	(progn
	  (when (called-interactively-p 'interactive)
	    (message "ID Utils not found."))
	  nil)
      (with-current-buffer b
	(goto-char (point-min))
	(re-search-forward "fnid - \\([0-9.]+\\)" nil t)
	(setq rev (match-string 1))
	(if (inversion-check-version rev nil cedet-idutils-min-version)
	    (if noerror
		nil
	      (error "Version of ID Utils is %s.  Need at least %s"
		     rev cedet-idutils-min-version))
	  ;; Else, return TRUE, as in good enough.
	  (when (called-interactively-p 'interactive)
	    (message "ID Utils %s  - Good enough for CEDET." rev))
	  t)))))

(defun cedet-idutils-create/update-database (&optional dir)
  "Create an IDUtils database in DIR.
IDUtils must start from scratch when updating a database."
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (cedet-idutils-mkid-call nil)))

(provide 'cedet-idutils)

;;; cedet-idutils.el ends here
