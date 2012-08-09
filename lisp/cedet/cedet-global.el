;;; cedet-global.el --- GNU Global support for CEDET.

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>
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
;; Basic support for calling GNU Global, and testing version numbers.

(declare-function inversion-check-version "inversion")

(defvar cedet-global-min-version "5.0"
  "Minimum version of GNU Global required.")

(defcustom cedet-global-command "global"
  "Command name for the GNU Global executable."
  :type 'string
  :group 'cedet)

(defcustom cedet-global-gtags-command "gtags"
  "Command name for the GNU Global gtags executable.
GTAGS is used to create the tags table queried by the 'global' command."
  :type 'string
  :group 'cedet)

;;; Code:
(defun cedet-gnu-global-search (searchtext texttype type scope)
  "Perform a search with GNU Global, return the created buffer.
SEARCHTEXT is text to find.
TEXTTYPE is the type of text, such as 'regexp, 'string, 'tagname,
'tagregexp, or 'tagcompletions.
TYPE is the type of search, meaning that SEARCHTEXT is compared to
filename, tagname (tags table), references (uses of a tag) , or
symbol (uses of something not in the tag table.)
SCOPE is the scope of the search, such as 'project or 'subdirs."
  (let ((flgs (cond ((eq type 'file)
		     "-a")
		    (t "-xa")))
	(scopeflgs (cond
		    ((eq scope 'project)
		     ""
		     )
		    ((eq scope 'target)
		     "l")))
	(stflag (cond ((or (eq texttype 'tagname)
			   (eq texttype 'tagregexp))
		       "")
		      ((eq texttype 'tagcompletions)
		       "c")
		      ((eq texttype 'regexp)
		       "g")
		      (t "r"))))
    (cedet-gnu-global-call (list (concat flgs scopeflgs stflag)
				 searchtext))))

(defun cedet-gnu-global-call (flags)
  "Call GNU Global with the list of FLAGS."
  (let ((b (get-buffer-create "*CEDET Global*"))
	(cd default-directory))
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-global-command
	   nil b nil
	   flags)
    b))

(defun cedet-gnu-global-gtags-call (flags)
  "Create GNU Global TAGS using gtags with FLAGS."
  (let ((b (get-buffer-create "*CEDET Global gtags*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-global-gtags-command
	   nil b nil
	   flags)
    b))

(defun cedet-gnu-global-expand-filename (filename)
  "Expand the FILENAME with GNU Global.
Return a fully qualified filename."
  (interactive "sFile: ")
  (let ((ans (with-current-buffer (cedet-gnu-global-call (list "-Pa" filename))
	       (goto-char (point-min))
	       (if (looking-at "global: ")
		   (error "GNU Global not available")
		 (split-string (buffer-string) "\n" t)))))
    (when (called-interactively-p 'interactive)
      (if ans
	  (if (= (length ans) 1)
	      (message "%s" (car ans))
	    (message "%s + %d others" (car ans)
		     (length (cdr ans))))
	(error "No file found")))
    ans))

(defun cedet-gnu-global-show-root ()
  "Show the root of a GNU Global area under the current buffer."
  (interactive)
  (message "%s" (cedet-gnu-global-root)))

(defun cedet-gnu-global-root (&optional dir)
  "Return the root of any GNU Global scanned project.
If a default starting DIR is not specified, the current buffer's
`default-directory' is used."
  (let ((default-directory (or dir default-directory)))
    (with-current-buffer (cedet-gnu-global-call (list "-pq"))
      (goto-char (point-min))
      (when (not (eobp))
	(file-name-as-directory
	 (buffer-substring (point) (point-at-eol)))))))

(defun cedet-gnu-global-version-check (&optional noerror)
  "Check the version of the installed GNU Global command.
If optional programmatic argument NOERROR is non-nil,
then instead of throwing an error if Global isn't available,
return nil."
  (interactive)
  (require 'inversion)
  (let ((b (condition-case nil
	       (cedet-gnu-global-call (list "--version"))
	     (error nil)))
	(rev nil))
    (if (not b)
	(progn
	  (when (called-interactively-p 'interactive)
	    (message "GNU Global not found."))
	  nil)
      (with-current-buffer b
	(goto-char (point-min))
	(re-search-forward "GNU GLOBAL \\([0-9.]+\\)" nil t)
	(setq rev (match-string 1))
	(if (inversion-check-version rev nil cedet-global-min-version)
	    (if noerror
		nil
	      (error "Version of GNU Global is %s.  Need at least %s"
		     rev cedet-global-min-version))
	  ;; Else, return TRUE, as in good enough.
	  (when (called-interactively-p 'interactive)
	    (message "GNU Global %s  - Good enough for CEDET." rev))
	  t)))))

(defun cedet-gnu-global-scan-hits (buffer)
  "Scan all the hits from the GNU Global output BUFFER."
  (let ((hits nil)
	(r1 "^\\([^ ]+\\) +\\([0-9]+\\) \\([^ ]+\\) "))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward r1 nil t)
	(setq hits (cons (cons (string-to-number (match-string 2))
			       (match-string 3))
			 hits)))
      ;; Return the results
      (nreverse hits))))

(defun cedet-gnu-global-create/update-database (&optional dir)
  "Create a GNU Global database in DIR.
If a database already exists, then just update it."
  (interactive "DDirectory: ")
  (let ((root (cedet-gnu-global-root dir)))
    (if root (setq dir root))
    (let ((default-directory dir))
      (cedet-gnu-global-gtags-call
       (when root
	 '("-i");; Incremental update flag.
	 )))))

(provide 'cedet-global)

;;; cedet-global.el ends here
