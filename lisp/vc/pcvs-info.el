;;; pcvs-info.el --- internal representation of a fileinfo entry

;; Copyright (C) 1991-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs
;; Package: pcvs

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

;; The cvs-fileinfo data structure:
;;
;; When the `cvs update' is ready we parse the output.  Every file
;; that is affected in some way is added to the cookie collection as
;; a "fileinfo" (as defined below in cvs-create-fileinfo).

;;; Code:

(eval-when-compile (require 'cl))
(require 'pcvs-util)
;;(require 'pcvs-defs)

;;;;
;;;; config variables
;;;;

(define-obsolete-variable-alias 'cvs-display-full-path
    'cvs-display-full-name "22.1")

(defcustom cvs-display-full-name t
  "Specifies how the filenames should be displayed in the listing.
If non-nil, their full filename name will be displayed, else only the
non-directory part."
  :group 'pcl-cvs
  :type '(boolean))

(defcustom cvs-allow-dir-commit nil
  "Allow `cvs-mode-commit' on directories.
If you commit without any marked file and with the cursor positioned
on a directory entry, cvs would commit the whole directory.  This seems
to confuse some users sometimes."
  :group 'pcl-cvs
  :type '(boolean))

;;;;
;;;; Faces for fontification
;;;;

(defface cvs-header
  '((((class color) (background dark))
     (:foreground "lightyellow" :weight bold))
    (((class color) (background light))
     (:foreground "blue4" :weight bold))
    (t (:weight bold)))
  "PCL-CVS face used to highlight directory changes."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-header-face 'cvs-header "22.1")

(defface cvs-filename
  '((((class color) (background dark))
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "blue4"))
    (t ()))
  "PCL-CVS face used to highlight file names."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-filename-face 'cvs-filename "22.1")

(defface cvs-unknown
  '((((class color) (background dark))
     (:foreground "red1"))
    (((class color) (background light))
     (:foreground "red1"))
    (t (:slant italic)))
  "PCL-CVS face used to highlight unknown file status."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-unknown-face 'cvs-unknown "22.1")

(defface cvs-handled
  '((((class color) (background dark))
     (:foreground "pink"))
    (((class color) (background light))
     (:foreground "pink"))
    (t ()))
  "PCL-CVS face used to highlight handled file status."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-handled-face 'cvs-handled "22.1")

(defface cvs-need-action
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:slant italic)))
  "PCL-CVS face used to highlight status of files needing action."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-need-action-face 'cvs-need-action "22.1")

(defface cvs-marked
  '((((min-colors 88) (class color) (background dark))
     (:foreground "green1" :weight bold))
    (((class color) (background dark))
     (:foreground "green" :weight bold))
    (((class color) (background light))
     (:foreground "green3" :weight bold))
    (t (:weight bold)))
  "PCL-CVS face used to highlight marked file indicator."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-marked-face 'cvs-marked "22.1")

(defface cvs-msg
  '((t (:slant italic)))
  "PCL-CVS face used to highlight CVS messages."
  :group 'pcl-cvs)
(define-obsolete-face-alias 'cvs-msg-face 'cvs-msg "22.1")

(defvar cvs-fi-up-to-date-face 'cvs-handled)
(defvar cvs-fi-unknown-face 'cvs-unknown)
(defvar cvs-fi-conflict-face 'font-lock-warning-face)

;; There is normally no need to alter the following variable, but if
;; your site has installed CVS in a non-standard way you might have
;; to change it.

(defvar cvs-bakprefix ".#"
  "The prefix that CVS prepends to files when rcsmerge'ing.")

(easy-mmode-defmap cvs-status-map
  '(([(mouse-2)] . cvs-mode-toggle-mark))
  "Local keymap for text properties of status")

;; Constructor:

(defstruct (cvs-fileinfo
	    (:constructor nil)
	    (:copier nil)
	    (:constructor -cvs-create-fileinfo (type dir file full-log
						     &key marked subtype
						     merge
						     base-rev
						     head-rev))
	    (:conc-name cvs-fileinfo->))
  marked	;; t/nil.
  type		;; See below
  subtype	;; See below
  dir		;; Relative directory the file resides in.
                ;; (concat dir file) should give a valid path.
  file	     	;; The file name sans the directory.
  base-rev      ;; During status: This is the revision that the
                ;; working file is based on.
  head-rev      ;; During status: This is the highest revision in
                ;; the repository.
  merge		;; A cons cell containing the (ancestor . head) revisions
		;; of the merge that resulted in the current file.
  ;;removed	;; t if the file no longer exists.
  full-log	;; The output from cvs, unparsed.
  ;;mod-time	;; Not used.

  ;; In addition to the above, the following values can be extracted:

  ;; handled    ;; t if this file doesn't require further action.
  ;; full-name  ;; The complete relative filename.
  ;; pp-name    ;; The printed file name
  ;; backup-file;; For MERGED and CONFLICT files after a \"cvs update\",
                ;; this is a full path to the backup file where the
                ;; untouched version resides.

  ;; The meaning of the type field:

  ;; Value	      ---Used by---	Explanation
  ;; 		      update status
  ;; NEED-UPDATE		x	file needs update
  ;; MODIFIED		x	x	modified by you, unchanged in repository
  ;;   MERGED		x	x	successful merge
  ;; ADDED		x	x	added by you, not yet committed
  ;; MISSING			x	rm'd, but not yet `cvs remove'd
  ;; REMOVED		x	x	removed by you, not yet committed
  ;; NEED-MERGE			x	need merge
  ;; CONFLICT		x		conflict when merging
  ;; ;;MOD-CONFLICT	x		removed locally, changed in repository.
  ;; DIRCHANGE		x	x	A change of directory.
  ;; UNKNOWN		x		An unknown file.
  ;; UP-TO-DATE			x	The file is up-to-date.
  ;;   UPDATED		x	x	file copied from repository
  ;;   PATCHED		x	x	diff applied from repository
  ;;   COMMITTED		x	x	cvs commit'd
  ;; DEAD				An entry that should be removed
  ;; MESSAGE		x	x	This is a special fileinfo that is used
  ;;					  to display a text that should be in
  ;;					  full-log."
  ;;   TEMP	A temporary message that should be removed
  )
(defun cvs-create-fileinfo (type dir file msg &rest keys)
  (cvs-check-fileinfo (apply #'-cvs-create-fileinfo type dir file msg keys)))

;; Fake selectors:

(defun cvs-fileinfo->full-name (fileinfo)
  "Return the full path for the file that is described in FILEINFO."
  (let ((dir (cvs-fileinfo->dir fileinfo)))
    (if (eq (cvs-fileinfo->type fileinfo) 'DIRCHANGE)
	(if (string= dir "") "." (directory-file-name dir))
      ;; Here, I use `concat' rather than `expand-file-name' because I want
      ;; the resulting path to stay relative if `dir' is relative.
      (concat dir (cvs-fileinfo->file fileinfo)))))
(define-obsolete-function-alias 'cvs-fileinfo->full-path
    'cvs-fileinfo->full-name "22.1")

(defun cvs-fileinfo->pp-name (fi)
  "Return the filename of FI as it should be displayed."
  (if cvs-display-full-name
      (cvs-fileinfo->full-name fi)
    (cvs-fileinfo->file fi)))

(defun cvs-fileinfo->backup-file (fileinfo)
  "Construct the file name of the backup file for FILEINFO."
  (let* ((dir (cvs-fileinfo->dir fileinfo))
	 (file (cvs-fileinfo->file fileinfo))
	 (default-directory (file-name-as-directory (expand-file-name dir)))
	 (files (directory-files "." nil
				 (concat "\\`" (regexp-quote cvs-bakprefix)
					 (regexp-quote file) "\\(\\.[0-9]+\\.[0-9]+\\)+\\'")))
	 bf)
    (dolist (f files)
      (when (and (file-readable-p f)
		 (or (null bf) (file-newer-than-file-p f bf)))
	(setq bf f)))
    (concat dir bf)))

;; (defun cvs-fileinfo->handled (fileinfo)
;;   "Tell if this requires further action"
;;   (memq (cvs-fileinfo->type fileinfo) '(UP-TO-DATE DEAD)))


;; Predicate:

(defun cvs-check-fileinfo (fi)
  "Check FI's conformance to some conventions."
  (let ((check 'none)
	(type (cvs-fileinfo->type fi))
	(subtype (cvs-fileinfo->subtype fi))
	(marked (cvs-fileinfo->marked fi))
	(dir (cvs-fileinfo->dir fi))
	(file (cvs-fileinfo->file fi))
	(base-rev (cvs-fileinfo->base-rev fi))
	(head-rev (cvs-fileinfo->head-rev fi))
	(full-log (cvs-fileinfo->full-log fi)))
    (if (and (setq check 'marked)	(memq marked '(t nil))
	     (setq check 'base-rev)	(or (null base-rev) (stringp base-rev))
	     (setq check 'head-rev)	(or (null head-rev) (stringp head-rev))
	     (setq check 'full-log)	(stringp full-log)
	     (setq check 'dir)
	     (and (stringp dir)
		  (not (file-name-absolute-p dir))
		  (or (string= dir "")
		      (string= dir (file-name-as-directory dir))))
	     (setq check 'file)
	     (and (stringp file)
		  (string= file (file-name-nondirectory file)))
	     (setq check 'type)		(symbolp type)
	     (setq check 'consistency)
	     (case type
	       (DIRCHANGE (and (null subtype) (string= "." file)))
	       ((NEED-UPDATE ADDED MISSING DEAD MODIFIED MESSAGE UP-TO-DATE
			     REMOVED NEED-MERGE CONFLICT UNKNOWN MESSAGE)
		t)))
	fi
      (error "Invalid :%s in cvs-fileinfo %s" check fi))))


;;;;
;;;; State table to indicate what you can do when.
;;;;

(defconst cvs-states
  `((NEED-UPDATE	update diff ignore)
    (UP-TO-DATE		update nil remove diff safe-rm revert)
    (MODIFIED		update commit undo remove diff merge diff-base)
    (ADDED		update commit remove)
    (MISSING     	remove undo update safe-rm revert)
    (REMOVED     	commit add undo safe-rm)
    (NEED-MERGE     	update undo diff diff-base)
    (CONFLICT		merge remove undo commit diff diff-base)
    (DIRCHANGE		remove update diff ,(if cvs-allow-dir-commit 'commit) tag)
    (UNKNOWN		ignore add remove)
    (DEAD		)
    (MESSAGE))
  "Fileinfo state descriptions for pcl-cvs.
This is an assoc list.  Each element consists of (STATE . FUNS)
- STATE (described in `cvs-create-fileinfo') is the key
- FUNS is the list of applicable operations.
  The first one (if any) should be the \"default\" action.
Most of the actions have the obvious meaning.
`safe-rm' indicates that the file can be removed without losing
  any information.")

;;;;
;;;; Utility functions
;;;;

(defun cvs-applicable-p (fi-or-type func)
  "Check if FUNC is applicable to FI-OR-TYPE.
If FUNC is nil, always return t.
FI-OR-TYPE can either be a symbol (a fileinfo-type) or a fileinfo."
  (let ((type (if (symbolp fi-or-type) fi-or-type
		(cvs-fileinfo->type fi-or-type))))
    (and (not (eq type 'MESSAGE))
	 (eq (car (memq func (cdr (assq type cvs-states)))) func))))

(defun cvs-add-face (str face &optional keymap &rest props)
  (when keymap
    (when (keymapp keymap)
      (setq props (list* 'keymap keymap props)))
    (setq props (list* 'mouse-face 'highlight props)))
  (add-text-properties 0 (length str) (list* 'font-lock-face face props) str)
  str)

(defun cvs-fileinfo-pp (fileinfo)
  "Pretty print FILEINFO.  Insert a printed representation in current buffer.
For use by the cookie package."
  (cvs-check-fileinfo fileinfo)
  (let ((type (cvs-fileinfo->type fileinfo))
	(subtype (cvs-fileinfo->subtype fileinfo)))
    (insert
     (case type
       (DIRCHANGE (concat "In directory "
			  (cvs-add-face (cvs-fileinfo->full-name fileinfo)
					'cvs-header t 'cvs-goal-column t)
			  ":"))
       (MESSAGE
	(cvs-add-face (format "Message: %s" (cvs-fileinfo->full-log fileinfo))
		      'cvs-msg))
       (t
	(let* ((status (if (cvs-fileinfo->marked fileinfo)
			   (cvs-add-face "*" 'cvs-marked)
			 " "))
	       (file (cvs-add-face (cvs-fileinfo->pp-name fileinfo)
				   'cvs-filename t 'cvs-goal-column t))
	       (base (or (cvs-fileinfo->base-rev fileinfo) ""))
	       (head (cvs-fileinfo->head-rev fileinfo))
	       (type
		(let ((str (case type
			     ;;(MOD-CONFLICT "Not Removed")
			     (DEAD	  "")
			     (t (capitalize (symbol-name type)))))
		      (face (let ((sym (intern
					(concat "cvs-fi-"
						(downcase (symbol-name type))
						"-face"))))
			      (or (and (boundp sym) (symbol-value sym))
				  'cvs-need-action))))
		  (cvs-add-face str face cvs-status-map)))
	       (side (or
		      ;; maybe a subtype
		      (when subtype (downcase (symbol-name subtype)))
		      ;; or the head-rev
		      (when (and head (not (string= head base))) head)
		      ;; or nothing
		      "")))
	   (format "%-11s %s %-11s %-11s %s"
		   side status type base file))))
     "\n")))


(defun cvs-fileinfo-update (fi fi-new)
  "Update FI with the information provided in FI-NEW."
  (let ((type (cvs-fileinfo->type fi-new))
	(merge (cvs-fileinfo->merge fi-new)))
    (setf (cvs-fileinfo->type fi) type)
    (setf (cvs-fileinfo->subtype fi) (cvs-fileinfo->subtype fi-new))
    (setf (cvs-fileinfo->full-log fi) (cvs-fileinfo->full-log fi-new))
    (setf (cvs-fileinfo->base-rev fi) (cvs-fileinfo->base-rev fi-new))
    (setf (cvs-fileinfo->head-rev fi) (cvs-fileinfo->head-rev fi-new))
    (cond
     (merge (setf (cvs-fileinfo->merge fi) merge))
     ((memq type '(UP-TO-DATE NEED-UPDATE))
      (setf (cvs-fileinfo->merge fi) nil)))))

(defun cvs-fileinfo< (a b)
  "Compare fileinfo A with fileinfo B and return t if A is `less'.
The ordering defined by this function is such that directories are
sorted alphabetically, and inside every directory the DIRCHANGE
fileinfo will appear first, followed by all files (alphabetically)."
  (let ((subtypea (cvs-fileinfo->subtype a))
	(subtypeb (cvs-fileinfo->subtype b)))
    (cond
     ;; Sort according to directories.
     ((string< (cvs-fileinfo->dir a) (cvs-fileinfo->dir b)) t)
     ((not (string= (cvs-fileinfo->dir a) (cvs-fileinfo->dir b))) nil)

     ;; The DIRCHANGE entry is always first within the directory.
     ((eq (cvs-fileinfo->type b) 'DIRCHANGE) nil)
     ((eq (cvs-fileinfo->type a) 'DIRCHANGE) t)

     ;; All files are sorted by file name.
     ((string< (cvs-fileinfo->file a) (cvs-fileinfo->file b))))))

;;;
;;; Look at CVS/Entries to quickly find a first approximation of the status
;;;

(defun cvs-fileinfo-from-entries (dir &optional all)
  "List of fileinfos for DIR, extracted from CVS/Entries.
Unless ALL is optional, returns only the files that are not up-to-date.
DIR can also be a file."
  (let* ((singlefile
	  (cond
	   ((equal dir "") nil)
	   ((file-directory-p dir) (setq dir (file-name-as-directory dir)) nil)
	   (t (prog1 (file-name-nondirectory dir)
		(setq dir (or (file-name-directory dir) ""))))))
	 (file (expand-file-name "CVS/Entries" dir))
	 (fis nil))
    (if (not (file-readable-p file))
	(push (cvs-create-fileinfo (if singlefile 'UNKNOWN 'DIRCHANGE)
				   dir (or singlefile ".") "") fis)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	;; Select the single file entry in case we're only interested in a file.
	(cond
	 ((not singlefile)
	  (push (cvs-create-fileinfo 'DIRCHANGE dir "." "") fis))
	 ((re-search-forward
	   (concat "^[^/]*/" (regexp-quote singlefile) "/.*") nil t)
	  (setq all t)
	  (goto-char (match-beginning 0))
	  (narrow-to-region (point) (match-end 0)))
	 (t
	  (push (cvs-create-fileinfo 'UNKNOWN dir singlefile "") fis)
	  (narrow-to-region (point-min) (point-min))))
	(while (looking-at "\\([^/]*\\)/\\([^/]*\\)/\\([^/]*\\)/\\([^/]*\\)/")
	  (if (/= (match-beginning 1) (match-end 1))
	      (setq fis (append (cvs-fileinfo-from-entries
				 (concat dir (file-name-as-directory
					      (match-string 2)))
				 all)
				fis))
	    (let ((f (match-string 2))
		  (rev (match-string 3))
		  (date (match-string 4))
		  timestamp
		  (type 'MODIFIED)
		  (subtype nil))
	      (cond
	       ((equal (substring rev 0 1) "-")
		(setq type 'REMOVED rev (substring rev 1)))
	       ((not (file-exists-p (concat dir f))) (setq type 'MISSING))
	       ((equal rev "0") (setq type 'ADDED rev nil))
	       ((equal date "Result of merge") (setq subtype 'MERGED))
	       ((let ((mtime (nth 5 (file-attributes (concat dir f))))
		      (system-time-locale "C"))
		  (setq timestamp (format-time-string "%c" mtime 'utc))
		  ;; Solaris sometimes uses "Wed Sep 05", not "Wed Sep  5".
		  ;; See "grep '[^a-z_]ctime' cvs/src/*.c" for reference.
		  (if (= (aref timestamp 8) ?0)
		      (setq timestamp (concat (substring timestamp 0 8)
					      " " (substring timestamp 9))))
		  (equal timestamp date))
		(setq type (if all 'UP-TO-DATE)))
	       ((equal date (concat "Result of merge+" timestamp))
		(setq type 'CONFLICT)))
	      (when type
		(push (cvs-create-fileinfo type dir f ""
					   :base-rev rev :subtype subtype)
		      fis))))
	  (forward-line 1))))
    fis))

(provide 'pcvs-info)

;;; pcvs-info.el ends here
