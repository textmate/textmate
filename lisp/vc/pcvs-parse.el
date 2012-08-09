;;; pcvs-parse.el --- the CVS output parser

;; Copyright (C) 1991-2012 Free Software Foundation, Inc.

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

;;; Bugs:

;; - when merging a modified file, if the merge says that the file already
;;   contained in the changes, it marks the file as `up-to-date' although
;;   it might still contain further changes.
;;   Example: merging a zero-change commit.

;;; Code:

(eval-when-compile (require 'cl))

(require 'pcvs-util)
(require 'pcvs-info)

;; imported from pcvs.el
(defvar cvs-execute-single-dir)

;; parse vars

(defcustom cvs-update-prog-output-skip-regexp "$"
  "A regexp that matches the end of the output from all cvs update programs.
That is, output from any programs that are run by CVS (by the flag -u
in the `modules' file - see cvs(5)) when `cvs update' is performed should
terminate with a line that this regexp matches.  It is enough that
some part of the line is matched.

The default (a single $) fits programs without output."
  :group 'pcl-cvs
  :type '(regexp :value "$"))

(defcustom cvs-parse-ignored-messages
  '("Executing ssh-askpass to query the password.*$"
    ".*Remote host denied X11 forwarding.*$")
  "A list of regexps matching messages that should be ignored by the parser.
Each regexp should match a whole set of lines and should hence be terminated
by `$'."
  :group 'pcl-cvs
  :type '(repeat regexp))

;; a few more defvars just to shut up the compiler
(defvar cvs-start)
(defvar cvs-current-dir)
(defvar cvs-current-subdir)
(defvar dont-change-disc)

;;;; The parser

(defconst cvs-parse-known-commands
  '("status" "add" "commit" "update" "remove" "checkout" "ci")
  "List of CVS commands whose output is understood by the parser.")

(defun cvs-parse-buffer (parse-spec dont-change-disc &optional subdir)
  "Parse current buffer according to PARSE-SPEC.
PARSE-SPEC is a function of no argument advancing the point and returning
  either a fileinfo or t (if the matched text should be ignored) or
  nil if it didn't match anything.
DONT-CHANGE-DISC just indicates whether the command was changing the disc
  or not (useful to tell the difference between `cvs-examine' and `cvs-update'
  output.
The path names should be interpreted as relative to SUBDIR (defaults
  to the `default-directory').
Return a list of collected entries, or t if an error occurred."
  (goto-char (point-min))
  (let ((fileinfos ())
	(cvs-current-dir "")
	(case-fold-search nil)
	(cvs-current-subdir (or subdir "")))
    (while (not (or (eobp) (eq fileinfos t)))
      (let ((ret (cvs-parse-run-table parse-spec)))
	(cond
	 ;; it matched a known information message
	 ((cvs-fileinfo-p ret) (push ret fileinfos))
	 ;; it didn't match anything at all (impossible)
	 ((and (consp ret) (cvs-fileinfo-p (car ret)))
	  (setq fileinfos (append ret fileinfos)))
	 ((null ret) (setq fileinfos t))
	 ;; it matched something that should be ignored
	 (t nil))))
    (nreverse fileinfos)))


;; All those parsing macros/functions should return a success indicator
(defsubst cvs-parse-msg () (buffer-substring cvs-start (1- (point))))

;;(defsubst COLLECT (exp) (push exp *result*))
;;(defsubst PROG (e) t)
;;(defmacro SEQ (&rest seqs) (cons 'and seqs))

(defmacro cvs-match (re &rest matches)
  "Try to match RE and extract submatches.
If RE matches, advance the point until the line after the match and
then assign the variables as specified in MATCHES (via `setq')."
  (cons 'cvs-do-match
	(cons re (mapcar (lambda (match)
			   `(cons ',(first match) ,(second match)))
			 matches))))

(defun cvs-do-match (re &rest matches)
  "Internal function for the `cvs-match' macro.
Match RE and if successful, execute MATCHES."
  ;; Is it a match?
  (when (looking-at re)
    (goto-char (match-end 0))
    ;; Skip the newline (unless we already are at the end of the buffer).
    (when (and (eolp) (< (point) (point-max))) (forward-char))
    ;; assign the matches
    (dolist (match matches t)
      (let ((val (cdr match)))
	(set (car match) (if (integerp val) (match-string val) val))))))

(defmacro cvs-or (&rest alts)
  "Try each one of the ALTS alternatives until one matches."
  `(let ((-cvs-parse-point (point)))
     ,(cons 'or
	    (mapcar (lambda (es)
		      `(or ,es (ignore (goto-char -cvs-parse-point))))
		    alts))))
(def-edebug-spec cvs-or t)

;; This is how parser tables should be executed
(defun cvs-parse-run-table (parse-spec)
  "Run PARSE-SPEC and provide sensible default behavior."
  (unless (bolp) (forward-line 1))	;this should never be needed
  (let ((cvs-start (point)))
    (cvs-or
     (funcall parse-spec)

     (dolist (re cvs-parse-ignored-messages)
       (when (cvs-match re) (return t)))

     ;; This is a parse error.  Create a message-type fileinfo.
     (and
      (cvs-match ".*$")
      (cvs-create-fileinfo 'MESSAGE cvs-current-dir " "
			   ;; (concat " Unknown msg: '"
			   (cvs-parse-msg) ;; "'")
			   :subtype 'ERROR)))))


(defun cvs-parsed-fileinfo (type path &optional directory &rest keys)
  "Create a fileinfo.
TYPE can either be a type symbol or a cons of the form (TYPE . SUBTYPE).
PATH is the filename.
DIRECTORY influences the way PATH is interpreted:
- if it's a string, it denotes the directory in which PATH (which should then be
  a plain file name with no directory component) resides.
- if it's nil, the PATH should not be trusted: if it has a directory
  component, use it, else, assume it is relative to the current directory.
- else, the PATH should be trusted to be relative to the root
  directory (i.e. if there is no directory component, it means the file
  is inside the main directory).
The remaining KEYS are passed directly to `cvs-create-fileinfo'."
  (let ((dir directory)
	(file path))
    ;; only trust the directory if it's a string
    (unless (stringp directory)
      ;; else, if the directory is true, the path should be trusted
      (setq dir (or (file-name-directory path) (if directory "")))
      (setq file (file-name-nondirectory path)))

    (let ((type (if (consp type) (car type) type))
	  (subtype (if (consp type) (cdr type))))
      (when dir (setq cvs-current-dir dir))
      (apply 'cvs-create-fileinfo type
	     (concat cvs-current-subdir (or dir cvs-current-dir))
	     file (cvs-parse-msg) :subtype subtype keys))))

;;;; CVS Process Parser Tables:
;;;;
;;;; The table for status and update could actually be merged since they
;;;; don't conflict.  But they don't overlap much either.

(defun cvs-parse-table ()
  "Table of message objects for `cvs-parse-process'."
  (let (c file dir path base-rev subtype)
    (cvs-or

     (cvs-parse-status)
     (cvs-parse-merge)
     (cvs-parse-commit)

     ;; this is not necessary because the fileinfo merging will remove
     ;; such duplicate info and luckily the second info is the one we want.
     ;; (and (cvs-match "M \\(.*\\)$" (path 1))
     ;;      (cvs-parse-merge path))

     ;; Normal file state indicator.
     (and
      (cvs-match "\\([MARCUPNJ?]\\) \\(.*\\)$" (c 1) (path 2))
      ;; M: The file is modified by the user, and untouched in the repository.
      ;; A: The file is "cvs add"ed, but not "cvs ci"ed.
      ;; R: The file is "cvs remove"ed, but not "cvs ci"ed.
      ;; C: Conflict
      ;; U: The file is copied from the repository.
      ;; P: The file was patched from the repository.
      ;; ?: Unknown file.
      (let ((code (aref c 0)))
	(cvs-parsed-fileinfo
	 (case code
	   (?M 'MODIFIED)
	   (?A 'ADDED)
	   (?R 'REMOVED)
	   (?? 'UNKNOWN)
	   (?C
	    (if (not dont-change-disc) 'CONFLICT
	      ;; This is ambiguous.  We should look for conflict markers in the
	      ;; file to decide between CONFLICT and NEED-MERGE.  With CVS-1.10
	      ;; servers, this should not be necessary, because they return
	      ;; a complete merge output.
	      (with-temp-buffer
		(ignore-errors (insert-file-contents path))
		(goto-char (point-min))
		(if (re-search-forward "^<<<<<<< " nil t)
		    'CONFLICT 'NEED-MERGE))))
	   (?J 'NEED-MERGE)		;not supported by standard CVS
	   ((?U ?P)
	    (if dont-change-disc 'NEED-UPDATE
	      (cons 'UP-TO-DATE (if (eq code ?U) 'UPDATED 'PATCHED)))))
	 path 'trust)))

     (and
      (cvs-match "pcl-cvs: descending directory \\(.*\\)$" (dir 1))
      (setq cvs-current-subdir dir))

     ;; A special cvs message
     (and
      (let ((case-fold-search t))
	(cvs-match "cvs[.a-z]* [a-z]+: "))
      (cvs-or

       ;; CVS is descending a subdirectory
       ;; (status says `examining' while update says `updating')
       (and
	(cvs-match "\\(Examining\\|Updating\\) \\(.*\\)$" (dir 2))
	(let ((dir (if (string= "." dir) "" (file-name-as-directory dir))))
	  (cvs-parsed-fileinfo 'DIRCHANGE "." dir)))

       ;; [-n update] A new (or pruned) directory appeared but isn't traversed
       (and
	(cvs-match "New directory `\\(.*\\)' -- ignored$" (dir 1))
	;; (cvs-parsed-fileinfo 'MESSAGE " " (file-name-as-directory dir))
	;; These messages either correspond to a true new directory
	;; that an update will bring in, or to a directory that's empty
	;; on the current branch (either because it only exists in other
	;; branches, or because it's been removed).
	(if (ignore-errors
	      (with-temp-buffer
                (ignore-errors
                  (insert-file-contents
                   (expand-file-name ".cvsignore" (file-name-directory dir))))
		(goto-char (point-min))
		(re-search-forward
		 (concat "^" (regexp-quote (file-name-nondirectory dir)) "/$")
		 nil t)))
	    t		       ;The user requested to ignore those messages.
	  (cvs-parsed-fileinfo '(NEED-UPDATE . NEW-DIR) dir t)))

       ;; File removed, since it is removed (by third party) in repository.
       (and
	(cvs-or
         ;; some cvs versions output quotes around these files
	 (cvs-match "warning: `\\(.*\\)' is not (any longer) pertinent$" (file 1))
	 (cvs-match "warning: \\(.*\\) is not (any longer) pertinent$" (file 1))
	 (cvs-match "`\\(.*\\)' is no longer in the repository$" (file 1))
         (cvs-match "\\(.*\\) is no longer in the repository$" (file 1)))
	(cvs-parsed-fileinfo
	 (if dont-change-disc '(NEED-UPDATE . REMOVED) 'DEAD) file))

       ;; [add]
       (and
	(cvs-or
	 (cvs-match "scheduling file `\\(.*\\)' for addition.*$" (path 1))
	 (cvs-match "re-adding file \\(.*\\) (in place of .*)$" (path 1)))
	(cvs-parsed-fileinfo 'ADDED path))

       ;; [add] this will also show up as a `U <file>'
       (and
	(cvs-match "`?\\(.*?\\)'?, version \\(.*\\), resurrected$"
		   (path 1) (base-rev 2))
	;; FIXME: resurrection only brings back the original version,
	;; not the latest on the branch, so `up-to-date' is not always
	;; what we want.
	(cvs-parsed-fileinfo '(UP-TO-DATE . RESURRECTED) path nil
			     :base-rev base-rev))

       ;; [remove]
       (and
	(cvs-match "removed `\\(.*\\)'$" (path 1))
	(cvs-parsed-fileinfo 'DEAD path))

       ;; [remove,merge]
       (and
	(cvs-match "scheduling `\\(.*\\)' for removal$" (file 1))
	(cvs-parsed-fileinfo 'REMOVED file))

       ;; [update] File removed by you, but not cvs rm'd
       (and
	(cvs-match "warning: \\(.*\\) was lost$" (path 1))
	(cvs-match (concat "U " (regexp-quote path) "$"))
	(cvs-parsed-fileinfo (if dont-change-disc
				 'MISSING
			       '(UP-TO-DATE . UPDATED))
			     path))

       ;; Mode conflicts (rather than contents)
       (and
	(cvs-match "conflict: ")
	(cvs-or
	 (cvs-match "removed \\(.*\\) was modified by second party$"
		    (path 1) (subtype 'REMOVED))
	 (cvs-match "\\(.*\\) created independently by second party$"
		    (path 1) (subtype 'ADDED))
	 (cvs-match "\\(.*\\) is modified but no longer in the repository$"
		    (path 1) (subtype 'MODIFIED)))
	(cvs-match (concat "C " (regexp-quote path)))
	(cvs-parsed-fileinfo (cons 'CONFLICT subtype) path))

       ;; Messages that should be shown to the user
       (and
	(cvs-or
	 (cvs-match "move away \\(.*\\); it is in the way$" (file 1))
	 (cvs-match "warning: new-born \\(.*\\) has disappeared$" (file 1))
	 (cvs-match "sticky tag .* for file `\\(.*\\)' is not a branch$"
		    (file 1)))
	(cvs-parsed-fileinfo 'MESSAGE file))

       ;; File unknown.
       (and (cvs-match "use `.+ add' to create an entry for \\(.*\\)$" (path 1))
	    (cvs-parsed-fileinfo 'UNKNOWN path))

       ;; [commit]
       (and (cvs-match "Up-to-date check failed for `\\(.+\\)'$" (file 1))
	    (cvs-parsed-fileinfo 'NEED-MERGE file))

       ;; We use cvs-execute-multi-dir but cvs can't handle it
       ;; Probably because the cvs-client can but the cvs-server can't
       (and (cvs-match ".* files with '?/'? in their name.*$")
	    (not cvs-execute-single-dir)
	    (setq cvs-execute-single-dir t)
	    (cvs-create-fileinfo
	     'MESSAGE "" " "
	     "*** Add (setq cvs-execute-single-dir t) to your .emacs ***
	See the FAQ file or the variable's documentation for more info."))

       ;; Cvs waits for a lock.  Ignored: already handled by the process filter
       (cvs-match "\\[..:..:..\\] \\(waiting for\\|obtained\\) .*lock in .*$")
       ;; File you removed still exists.  Ignore (will be noted as removed).
       (cvs-match ".* should be removed and is still there$")
       ;; just a note
       (cvs-match "use ['`].+ commit' to \\sw+ th\\sw+ files? permanently$")
       ;; [add,status] followed by a more complete status description anyway
       (and (cvs-match "nothing known about \\(.*\\)$" (path 1))
	    (cvs-parsed-fileinfo 'DEAD path 'trust))
       ;; [update] problem with patch
       (cvs-match "checksum failure after patch to .*; will refetch$")
       (cvs-match "refetching unpatchable files$")
       ;; [commit]
       (cvs-match "Rebuilding administrative file database$")
       ;; ???
       (cvs-match "--> Using per-directory sticky tag `.*'")

       ;; CVS is running a *info program.
       (and
	(cvs-match "Executing.*$")
	;; Skip by any output the program may generate to stdout.
	;; Note that pcl-cvs will get seriously confused if the
	;; program prints anything to stderr.
	(re-search-forward cvs-update-prog-output-skip-regexp))))

     (and
      (cvs-match "cvs[.ex]* \\[[a-z]+ aborted\\]:.*$")
      (cvs-parsed-fileinfo 'MESSAGE ""))

     ;; sadly you can't do much with these since the path is in the repository
     (cvs-match "Directory .* added to the repository$")
     )))


(defun cvs-parse-merge ()
  (let (path base-rev head-rev type)
    ;; A merge (maybe with a conflict).
    (and
     (cvs-match "RCS file: .*$")
     ;; Squirrel away info about the files that were retrieved for merging
     (cvs-match "retrieving revision \\([0-9.]+\\)$" (base-rev 1))
     (cvs-match "retrieving revision \\([0-9.]+\\)$" (head-rev 1))
     (cvs-match "Merging differences between [0-9.]+ and [0-9.]+ into \\(.*\\)$"
		(path 1))

     ;; eat up potential conflict warnings
     (cvs-or (cvs-match "\\(rcs\\)?merge:?\\( warning\\)?: \\(overlaps\\|conflicts\\) \\(or other problems \\)?during merge$" (type 'CONFLICT)) t)
     (cvs-or
      (and
       (cvs-match "cvs[.ex]* [a-z]+: ")
       (cvs-or
	(cvs-match "conflicts found in \\(.*\\)$" (path 1) (type 'CONFLICT))
	(cvs-match "could not merge .*$")
	(cvs-match "restoring \\(.*\\) from backup file .*$" (path 1))))
      t)

     ;; Is it a successful merge?
     ;; Figure out result of merging (ie, was there a conflict?)
     (let ((qfile (regexp-quote path)))
       (cvs-or
	;; Conflict
	(and
	 (cvs-match (concat "C \\(.*" qfile "\\)$") (path 1) (type 'CONFLICT))
	 ;; C might be followed by a "spurious" U for non-mergable files
	 (cvs-or (cvs-match (concat "U \\(.*" qfile "\\)$")) t))
	;; Successful merge
	(cvs-match (concat "M \\(.*" qfile "\\)$") (path 1))
	;; The file already contained the modifications
	(cvs-match (concat "^\\(.*" qfile
			   "\\) already contains the differences between .*$")
		   (path 1) (type '(UP-TO-DATE . MERGED)))
	t)
       ;; FIXME: PATH might not be set yet.  Sometimes the only path
       ;; information is in `RCS file: ...' (yuck!!).
       (cvs-parsed-fileinfo (if dont-change-disc 'NEED-MERGE
			      (or type '(MODIFIED . MERGED))) path nil
			    :merge (cons base-rev head-rev))))))

(defun cvs-parse-status ()
  (let (nofile path base-rev head-rev type)
    (and
     (cvs-match
      "===================================================================$")
     (cvs-match "File: \\(no file \\)?\\(.*[^ \t]\\)[ \t]+Status: "
		(nofile 1) (path 2))
     (cvs-or
      (cvs-match "Needs \\(Checkout\\|Patch\\)$"
		 (type (if nofile 'MISSING 'NEED-UPDATE)))
      (cvs-match "Up-to-date$"
		 (type (if nofile '(UP-TO-DATE . REMOVED) 'UP-TO-DATE)))
      (cvs-match "File had conflicts on merge$" (type 'MODIFIED))
      (cvs-match ".*[Cc]onflict.*$"	(type 'CONFLICT))
      (cvs-match "Locally Added$"	(type 'ADDED))
      (cvs-match "Locally Removed$"	(type 'REMOVED))
      (cvs-match "Locally Modified$"	(type 'MODIFIED))
      (cvs-match "Needs Merge$"		(type 'NEED-MERGE))
      (cvs-match "Entry Invalid"	(type '(NEED-MERGE . REMOVED)))
      (cvs-match ".*$"			(type 'UNKNOWN)))
     (cvs-match "$")
     (cvs-or
      (cvs-match " *Version:[ \t]*\\([0-9.]+\\).*$" (base-rev 1))
      ;; NOTE: there's no date on the end of the following for server mode...
      (cvs-match " *Working revision:[ \t]*-?\\([0-9.]+\\).*$" (base-rev 1))
      ;; Let's not get all worked up if the format changes a bit
      (cvs-match " *Working revision:.*$"))
     (cvs-or
      (cvs-match " *RCS Version:[ \t]*\\([0-9.]+\\)[ \t]*.*$" (head-rev 1))
      (cvs-match " *Repository revision:[ \t]*\\([0-9.]+\\)[ \t]*\\(.*\\)$"
		 (head-rev 1))
      (cvs-match " *Repository revision:.*"))
     (cvs-or (cvs-match " *Expansion option:.*") t)  ;Optional CVSNT thingie.
     (cvs-or (cvs-match " *Commit Identifier:.*") t) ;Optional CVSNT thingie.
     (cvs-or
      (and ;; Sometimes those fields are missing.
       (cvs-match " *Sticky Tag:[ \t]*\\(.*\\)$")      ; FIXME: use it.
       (cvs-match " *Sticky Date:[ \t]*\\(.*\\)$")     ; FIXME: use it.
       (cvs-match " *Sticky Options:[ \t]*\\(.*\\)$")) ; FIXME: use it.
      t)
     (cvs-or (cvs-match " *Merge From:.*") t) ;Optional CVSNT thingie.
     (cvs-match "$")
     ;; ignore the tags-listing in the case of `status -v'
     (cvs-or (cvs-match " *Existing Tags:\n\\(\t.*\n\\)*$") t)
     (cvs-parsed-fileinfo type path nil
			  :base-rev base-rev
			  :head-rev head-rev))))

(defun cvs-parse-commit ()
  (let (path file base-rev subtype)
    (cvs-or

     (and
      (cvs-or
       (cvs-match "\\(Checking in\\|Removing\\) \\(.*\\);$" (path 2))
       t)
      (cvs-match ".*,v  <--  \\(.*\\)$" (file 1))
      (cvs-or
       ;; deletion
       (cvs-match "new revision: delete; previous revision: \\([0-9.]*\\)$"
		  (subtype 'REMOVED) (base-rev 1))
       ;; addition
       (cvs-match "initial revision: \\([0-9.]*\\)$"
		  (subtype 'ADDED) (base-rev 1))
       ;; update
       (cvs-match "new revision: \\([0-9.]*\\); previous revision: .*$"
		  (subtype 'COMMITTED) (base-rev 1)))
      (cvs-or (cvs-match "done$") t)
      ;; In cvs-1.12.9 commit messages have been changed and became
      ;; ambiguous.  More specifically, the `path' above is not given.
      ;; We assume here that in future releases the corresponding info will
      ;; be put into `file'.
      (progn
	;; Try to remove the temp files used by VC.
	(vc-delete-automatic-version-backups (expand-file-name (or path file)))
	;; it's important here not to rely on the default directory management
	;; because `cvs commit' might begin by a series of Examining messages
	;; so the processing of the actual checkin messages might begin with
	;; a `current-dir' set to something different from ""
	(cvs-parsed-fileinfo (cons 'UP-TO-DATE subtype)
			     (or path file) 'trust
			     :base-rev base-rev)))

     ;; useless message added before the actual addition: ignored
     (cvs-match "RCS file: .*\ndone$"))))


(provide 'pcvs-parse)

;;; pcvs-parse.el ends here
