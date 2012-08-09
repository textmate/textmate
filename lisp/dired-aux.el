;;; dired-aux.el --- less commonly used parts of dired

;; Copyright (C) 1985-1986, 1992, 1994, 1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Maintainer: FSF
;; Keywords: files
;; Package: emacs

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

;; The parts of dired mode not normally used.  This is a space-saving hack
;; to avoid having to load a large mode when all that's wanted are a few
;; functions.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Finished up by rms in 1992.

;;; Code:

;; We need macros in dired.el to compile properly,
;; and we call subroutines in it too.
(require 'dired)

(defvar dired-create-files-failures nil
  "Variable where `dired-create-files' records failing file names.
Functions that operate recursively can store additional names
into this list; they also should call `dired-log' to log the errors.")

;;; 15K
;;;###begin dired-cmd.el
;; Diffing and compressing

(defconst dired-star-subst-regexp "\\(^\\|[ \t]\\)\\*\\([ \t]\\|$\\)")
(defconst dired-quark-subst-regexp "\\(^\\|[ \t]\\)\\?\\([ \t]\\|$\\)")

;;;###autoload
(defun dired-diff (file &optional switches)
  "Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.  (That's the mark set by
\\[set-mark-command], not by Dired's \\[dired-mark] command.)
The prompted-for FILE is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
which is the string of command switches for `diff'."
  (interactive
   (let* ((current (dired-get-filename t))
	  ;; Get the file at the mark.
	  (file-at-mark (if (mark t)
			    (save-excursion (goto-char (mark t))
					    (dired-get-filename t t))))
	  ;; Use it as default if it's not the same as the current file,
	  ;; and the target dir is the current dir or the mark is active.
	  (default (if (and (not (equal file-at-mark current))
			    (or (equal (dired-dwim-target-directory)
				       (dired-current-directory))
				mark-active))
		       file-at-mark))
	  (target-dir (if default
			  (dired-current-directory)
			(dired-dwim-target-directory)))
	  (defaults (dired-dwim-target-defaults (list current) target-dir)))
     (require 'diff)
     (list
      (minibuffer-with-setup-hook
	  (lambda ()
	    (set (make-local-variable 'minibuffer-default-add-function) nil)
	    (setq minibuffer-default defaults))
	(read-file-name
	 (format "Diff %s with%s: " current
		 (if default (format " (default %s)" default) ""))
	 target-dir default t))
      (if current-prefix-arg
	  (read-string "Options for diff: "
		       (if (stringp diff-switches)
			   diff-switches
			 (mapconcat 'identity diff-switches " ")))))))
  (let ((current (dired-get-filename t)))
    (when (or (equal (expand-file-name file)
		     (expand-file-name current))
	      (and (file-directory-p file)
		   (equal (expand-file-name current file)
			  (expand-file-name current))))
      (error "Attempt to compare the file to itself"))
    (diff file current switches)))

;;;###autoload
(defun dired-backup-diff (&optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for argument SWITCHES which is options for `diff'."
  (interactive
    (if current-prefix-arg
	(list (read-string "Options for diff: "
			   (if (stringp diff-switches)
			       diff-switches
			     (mapconcat 'identity diff-switches " "))))
      nil))
  (diff-backup (dired-get-filename) switches))

;;;###autoload
(defun dired-compare-directories (dir2 predicate)
  "Mark files with different file attributes in two dired buffers.
Compare file attributes of files in the current directory
with file attributes in directory DIR2 using PREDICATE on pairs of files
with the same name.  Mark files for which PREDICATE returns non-nil.
Mark files with different names if PREDICATE is nil (or interactively
with empty input at the predicate prompt).

PREDICATE is a Lisp expression that can refer to the following variables:

    size1, size2   - file size in bytes
    mtime1, mtime2 - last modification time in seconds, as a float
    fa1, fa2       - list of file attributes
                     returned by function `file-attributes'

    where 1 refers to attribute of file in the current dired buffer
    and 2 to attribute of file in second dired buffer.

Examples of PREDICATE:

    (> mtime1 mtime2) - mark newer files
    (not (= size1 size2)) - mark files with different sizes
    (not (string= (nth 8 fa1) (nth 8 fa2))) - mark files with different modes
    (not (and (= (nth 2 fa1) (nth 2 fa2))   - mark files with different UID
              (= (nth 3 fa1) (nth 3 fa2))))   and GID."
  (interactive
   (list
    (let* ((target-dir (dired-dwim-target-directory))
	   (defaults (dired-dwim-target-defaults nil target-dir)))
      (minibuffer-with-setup-hook
	  (lambda ()
	    (set (make-local-variable 'minibuffer-default-add-function) nil)
	    (setq minibuffer-default defaults))
	(read-directory-name (format "Compare %s with: "
				     (dired-current-directory))
			     target-dir target-dir t)))
    (read-from-minibuffer "Mark if (lisp expr or RET): " nil nil t nil "nil")))
  (let* ((dir1 (dired-current-directory))
         (file-alist1 (dired-files-attributes dir1))
         (file-alist2 (dired-files-attributes dir2))
	 file-list1 file-list2)
    (setq file-alist1 (delq (assoc "." file-alist1) file-alist1))
    (setq file-alist1 (delq (assoc ".." file-alist1) file-alist1))
    (setq file-alist2 (delq (assoc "." file-alist2) file-alist2))
    (setq file-alist2 (delq (assoc ".." file-alist2) file-alist2))
    (setq file-list1 (mapcar
		      'cadr
                      (dired-file-set-difference
                       file-alist1 file-alist2
		       predicate))
	  file-list2 (mapcar
		      'cadr
                      (dired-file-set-difference
                       file-alist2 file-alist1
		       predicate)))
    (dired-fun-in-all-buffers
     dir1 nil
     (lambda ()
       (dired-mark-if
        (member (dired-get-filename nil t) file-list1) nil)))
    (dired-fun-in-all-buffers
     dir2 nil
     (lambda ()
       (dired-mark-if
        (member (dired-get-filename nil t) file-list2) nil)))
    (message "Marked in dir1: %s files, in dir2: %s files"
             (length file-list1)
             (length file-list2))))

(defun dired-file-set-difference (list1 list2 predicate)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all file items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
PREDICATE (see `dired-compare-directories') is an additional match
condition.  Two file items are considered to match if they are equal
*and* PREDICATE evaluates to t."
  (if (or (null list1) (null list2))
      list1
    (let (res)
      (dolist (file1 list1)
	(unless (let ((list list2))
		  (while (and list
			      (not (let* ((file2 (car list))
					  (fa1 (car (cddr file1)))
					  (fa2 (car (cddr file2)))
					  (size1 (nth 7 fa1))
					  (size2 (nth 7 fa2))
					  (mtime1 (float-time (nth 5 fa1)))
					  (mtime2 (float-time (nth 5 fa2))))
				     (and
				      (equal (car file1) (car file2))
				      (not (eval predicate))))))
		    (setq list (cdr list)))
		  list)
	  (setq res (cons file1 res))))
      (nreverse res))))

(defun dired-files-attributes (dir)
  "Return a list of all file names and attributes from DIR.
List has a form of (file-name full-file-name (attribute-list))."
  (mapcar
   (lambda (file-name)
     (let ((full-file-name (expand-file-name file-name dir)))
       (list file-name
             full-file-name
             (file-attributes full-file-name))))
   (directory-files dir)))

;;; Change file attributes

(defun dired-do-chxxx (attribute-name program op-symbol arg)
  ;; Change file attributes (group, owner, timestamp) of marked files and
  ;; refresh their file lines.
  ;; ATTRIBUTE-NAME is a string describing the attribute to the user.
  ;; PROGRAM is the program used to change the attribute.
  ;; OP-SYMBOL is the type of operation (for use in `dired-mark-pop-up').
  ;; ARG describes which files to use, as in `dired-get-marked-files'.
  (let* ((files (dired-get-marked-files t arg))
	 (default (and (eq op-symbol 'touch)
		       (stringp (car files))
		       (format-time-string "%Y%m%d%H%M.%S"
					   (nth 5 (file-attributes (car files))))))
	 (prompt (concat "Change " attribute-name " of %s to"
			 (if (eq op-symbol 'touch)
			     " (default now): "
			   ": ")))
	 (new-attribute (dired-mark-read-string prompt nil op-symbol
						arg files default))
	 (operation (concat program " " new-attribute))
	 failures)
    (setq failures
	  (dired-bunch-files 10000
			     (function dired-check-process)
			     (append
			      (list operation program)
			      (unless (string-equal new-attribute "")
				(if (eq op-symbol 'touch)
				    (list "-t" new-attribute)
				  (list new-attribute)))
			      (if (string-match "gnu" system-configuration)
				  '("--") nil))
			     files))
    (dired-do-redisplay arg);; moves point if ARG is an integer
    (if failures
	(dired-log-summary
	 (format "%s: error" operation)
	 nil))))

;;;###autoload
(defun dired-do-chmod (&optional arg)
  "Change the mode of the marked (or next ARG) files.
Symbolic modes like `g+w' are allowed."
  (interactive "P")
  (let* ((files (dired-get-marked-files t arg))
	 (modestr (and (stringp (car files))
		       (nth 8 (file-attributes (car files)))))
	 (default
	   (and (stringp modestr)
		(string-match "^.\\(...\\)\\(...\\)\\(...\\)$" modestr)
		(replace-regexp-in-string
		 "-" ""
		 (format "u=%s,g=%s,o=%s"
			 (match-string 1 modestr)
			 (match-string 2 modestr)
			 (match-string 3 modestr)))))
	 (modes (dired-mark-read-string
		 "Change mode of %s to: "
		 nil 'chmod arg files default))
	 num-modes)
    (cond ((equal modes "")
	   ;; We used to treat empty input as DEFAULT, but that is not
	   ;; such a good idea (Bug#9361).
	   (error "No file mode specified"))
	  ((string-match "^[0-7]+" modes)
	   (setq num-modes (string-to-number modes 8))))

    (dolist (file files)
      (set-file-modes
       file
       (if num-modes num-modes
	 (file-modes-symbolic-to-number modes (file-modes file)))))
    (dired-do-redisplay arg)))

;;;###autoload
(defun dired-do-chgrp (&optional arg)
  "Change the group of the marked (or next ARG) files."
  (interactive "P")
  (if (memq system-type '(ms-dos windows-nt))
      (error "chgrp not supported on this system"))
  (dired-do-chxxx "Group" "chgrp" 'chgrp arg))

;;;###autoload
(defun dired-do-chown (&optional arg)
  "Change the owner of the marked (or next ARG) files."
  (interactive "P")
  (if (memq system-type '(ms-dos windows-nt))
      (error "chown not supported on this system"))
  (dired-do-chxxx "Owner" dired-chown-program 'chown arg))

;;;###autoload
(defun dired-do-touch (&optional arg)
  "Change the timestamp of the marked (or next ARG) files.
This calls touch."
  (interactive "P")
  (dired-do-chxxx "Timestamp" dired-touch-program 'touch arg))

;; Process all the files in FILES in batches of a convenient size,
;; by means of (FUNCALL FUNCTION ARGS... SOME-FILES...).
;; Batches are chosen to need less than MAX chars for the file names,
;; allowing 3 extra characters of separator per file name.
(defun dired-bunch-files (max function args files)
  (let (pending
	past
	(pending-length 0)
	failures)
    ;; Accumulate files as long as they fit in MAX chars,
    ;; then process the ones accumulated so far.
    (while files
      (let* ((thisfile (car files))
	     (thislength (+ (length thisfile) 3))
	     (rest (cdr files)))
	;; If we have at least 1 pending file
	;; and this file won't fit in the length limit, process now.
	(if (and pending (> (+ thislength pending-length) max))
	    (setq pending (nreverse pending)
		  ;; The elements of PENDING are now in forward order.
		  ;; Do the operation and record failures.
		  failures (nconc (apply function (append args pending))
				  failures)
		  ;; Transfer the elements of PENDING onto PAST
		  ;; and clear it out.  Now PAST contains the first N files
		  ;; specified (for some N), and FILES contains the rest.
		  past (nconc past pending)
		  pending nil
		  pending-length 0))
	;; Do (setq pending (cons thisfile pending))
	;; but reuse the cons that was in `files'.
	(setcdr files pending)
	(setq pending files)
	(setq pending-length (+ thislength pending-length))
	(setq files rest)))
    (setq pending (nreverse pending))
    (prog1
	(nconc (apply function (append args pending))
	       failures)
      ;; Now the original list FILES has been put back as it was.
      (nconc past pending))))

;;;###autoload
(defun dired-do-print (&optional arg)
  "Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "P")
  (let* ((file-list (dired-get-marked-files t arg))
	 (command (dired-mark-read-string
		   "Print %s with: "
 		   (mapconcat 'identity
			      (cons lpr-command
				    (if (stringp lpr-switches)
					(list lpr-switches)
				      lpr-switches))
			      " ")
		   'print arg file-list)))
    (dired-run-shell-command (dired-shell-stuff-it command file-list nil))))

(defun dired-mark-read-string (prompt initial op-symbol arg files
			       &optional default-value)
  "Read args for a Dired marked-files command, prompting with PROMPT.
Return the user input (a string).

INITIAL, if non-nil, is the initial minibuffer input.
OP-SYMBOL is an operation symbol (see `dired-no-confirm').
ARG is normally the prefix argument for the calling command.
FILES should be a list of file names.

DEFAULT-VALUE, if non-nil, should be a \"standard\" value or list
of such values, available via history commands.  Note that if the
user enters empty input, this function returns the empty string,
not DEFAULT-VALUE."
  (dired-mark-pop-up nil op-symbol files
		     'read-from-minibuffer
		     (format prompt (dired-mark-prompt arg files))
		     initial nil nil nil default-value))

;;; Cleaning a directory: flagging some backups for deletion.

(defvar dired-file-version-alist)

;;;###autoload
(defun dired-clean-directory (keep)
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
  (interactive "P")
  (setq keep (if keep (prefix-numeric-value keep) dired-kept-versions))
  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	(late-retention (if (<= keep 0) dired-kept-versions keep))
	(dired-file-version-alist ()))
    (message "Cleaning numerical backups (keeping %d late, %d old)..."
	     late-retention early-retention)
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on dired-file-version-alist an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines (function dired-collect-file-versions))
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval dired-file-version-alist))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval))))
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines (function dired-trample-file-versions))
    (message "Cleaning numerical backups...done")))

;;; Subroutines of dired-clean-directory.

(defun dired-map-dired-file-lines (fun)
  ;; Perform FUN with point at the end of each non-directory line.
  ;; FUN takes one argument, the absolute filename.
  (save-excursion
    (let (file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (looking-at dired-re-dir))
	       (not (eolp))
	       (setq file (dired-get-filename nil t)) ; nil on non-file
	       (progn (end-of-line)
		      (funcall fun file))))
	(forward-line 1)))))

(defvar backup-extract-version-start)  ; used in backup-extract-version

(defun dired-collect-file-versions (fn)
  (let ((fn (file-name-sans-versions fn)))
    ;; Only do work if this file is not already in the alist.
    (if (assoc fn dired-file-version-alist)
	nil
      ;; If it looks like file FN has versions, return a list of the versions.
      ;;That is a list of strings which are file names.
      ;;The caller may want to flag some of these files for deletion.
      (let* ((base-versions
	      (concat (file-name-nondirectory fn) ".~"))
	     (backup-extract-version-start (length base-versions))
	     (possibilities (file-name-all-completions
			     base-versions
			     (file-name-directory fn)))
	     (versions (mapcar 'backup-extract-version possibilities)))
	(if versions
	    (setq dired-file-version-alist
		  (cons (cons fn versions)
			dired-file-version-alist)))))))

(defun dired-trample-file-versions (fn)
  (let* ((start-vn (string-match "\\.~[0-9]+~$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which
	       (assoc (substring fn 0 start-vn)	; this looks like a
		      dired-file-version-alist))	; subversion
	 (not (memq (string-to-number (substring fn (+ 2 start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (progn (beginning-of-line)
		(delete-char 1)
		(insert dired-del-marker)))))

;;; Shell commands

(declare-function mailcap-file-default-commands "mailcap" (files))

(defun minibuffer-default-add-dired-shell-commands ()
  "Return a list of all commands associated with current dired files.
This function is used to add all related commands retrieved by `mailcap'
to the end of the list of defaults just after the default value."
  (interactive)
  (let ((commands (and (boundp 'files) (require 'mailcap nil t)
		       (mailcap-file-default-commands files))))
    (if (listp minibuffer-default)
	(append minibuffer-default commands)
      (cons minibuffer-default commands))))

;; This is an extra function so that you can redefine it, e.g., to use gmhist.
(defun dired-read-shell-command (prompt arg files)
  "Read a dired shell command.
PROMPT should be a format string with one \"%s\" format sequence,
which is replaced by the value returned by `dired-mark-prompt',
with ARG and FILES as its arguments.  FILES should be a list of
file names.  The result is used as the prompt.

This normally reads using `read-shell-command', but if the
`dired-x' package is loaded, use `dired-guess-shell-command' to
offer a smarter default choice of shell command."
  (minibuffer-with-setup-hook
      (lambda ()
	(set (make-local-variable 'minibuffer-default-add-function)
	     'minibuffer-default-add-dired-shell-commands))
    (setq prompt (format prompt (dired-mark-prompt arg files)))
    (if (functionp 'dired-guess-shell-command)
	(dired-mark-pop-up nil 'shell files
			   'dired-guess-shell-command prompt files)
      (dired-mark-pop-up nil 'shell files
			 'read-shell-command prompt nil nil))))

;;;###autoload
(defun dired-do-async-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND on the marked files asynchronously.

Like `dired-do-shell-command' but if COMMAND doesn't end in ampersand,
adds `* &' surrounded by whitespace and executes the command asynchronously.
The output appears in the buffer `*Async Shell Command*'."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (dired-do-shell-command command arg file-list))

;; The in-background argument is only needed in Emacs 18 where
;; shell-command doesn't understand an appended ampersand `&'.
;;;###autoload
(defun dired-do-shell-command (command &optional arg file-list)
  "Run a shell command COMMAND on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file.
The prompt mentions the file(s) or the marker, as appropriate.

If there is a `*' in COMMAND, surrounded by whitespace, this runs
COMMAND just once with the entire file list substituted there.

If there is no `*', but there is a `?' in COMMAND, surrounded by
whitespace, this runs COMMAND on each file individually with the
file name substituted for `?'.

Otherwise, this runs COMMAND on each file individually with the
file name added at the end of COMMAND (separated by a space).

`*' and `?' when not surrounded by whitespace have no special
significance for `dired-do-shell-command', and are passed through
normally to the shell, but you must confirm first.

If you want to use `*' as a shell wildcard with whitespace around
it, write `*\"\"' in place of just `*'.  This is equivalent to just
`*' in the shell, but avoids Dired's special handling.

If COMMAND produces output, it goes to a separate buffer.

This feature does not try to redisplay Dired buffers afterward, as
there's no telling what files COMMAND may have changed.
Type \\[dired-do-redisplay] to redisplay the marked files.

When COMMAND runs, its working directory is the top-level directory
of the Dired buffer, so output files usually are created there
instead of in a subdir.

In a noninteractive call (from Lisp code), you must specify
the list of file names explicitly with the FILE-LIST argument, which
can be produced by `dired-get-marked-files', for example."
;;Functions dired-run-shell-command and dired-shell-stuff-it do the
;;actual work and can be redefined for customization.
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command (concat "! on "
					"%s: ")
				current-prefix-arg
				files)
      current-prefix-arg
      files)))
  (let* ((on-each (not (string-match dired-star-subst-regexp command)))
	 (no-subst (not (string-match dired-quark-subst-regexp command)))
	 (star (string-match "\\*" command))
	 (qmark (string-match "\\?" command)))
    ;; Get confirmation for wildcards that may have been meant
    ;; to control substitution of a file name or the file name list.
    (if (cond ((not (or on-each no-subst))
	       (error "You can not combine `*' and `?' substitution marks"))
	      ((and star on-each)
	       (y-or-n-p "Confirm--do you mean to use `*' as a wildcard? "))
	      ((and qmark no-subst)
	       (y-or-n-p "Confirm--do you mean to use `?' as a wildcard? "))
	      (t))
	(if on-each
	    (dired-bunch-files
	     (- 10000 (length command))
	     (function (lambda (&rest files)
			 (dired-run-shell-command
			  (dired-shell-stuff-it command files t arg))))
	     nil
	     file-list)
	  ;; execute the shell command
	  (dired-run-shell-command
	   (dired-shell-stuff-it command file-list nil arg))))))

;; Might use {,} for bash or csh:
(defvar dired-mark-prefix ""
  "Prepended to marked files in dired shell commands.")
(defvar dired-mark-postfix ""
  "Appended to marked files in dired shell commands.")
(defvar dired-mark-separator " "
  "Separates marked files in dired shell commands.")

(defun dired-shell-stuff-it (command file-list on-each &optional _raw-arg)
;; "Make up a shell command line from COMMAND and FILE-LIST.
;; If ON-EACH is t, COMMAND should be applied to each file, else
;; simply concat all files and apply COMMAND to this.
;; FILE-LIST's elements will be quoted for the shell."
;; Might be redefined for smarter things and could then use RAW-ARG
;; (coming from interactive P and currently ignored) to decide what to do.
;; Smart would be a way to access basename or extension of file names.
  (let ((stuff-it
	 (if (or (string-match dired-star-subst-regexp command)
		 (string-match dired-quark-subst-regexp command))
	     (lambda (x)
	       (let ((retval command))
		 (while (string-match
			 "\\(^\\|[ \t]\\)\\([*?]\\)\\([ \t]\\|$\\)" retval)
		   (setq retval (replace-match x t t retval 2)))
		 retval))
	   (lambda (x) (concat command dired-mark-separator x)))))
    (if on-each
	(mapconcat stuff-it (mapcar 'shell-quote-argument file-list) ";")
      (let ((files (mapconcat 'shell-quote-argument
			      file-list dired-mark-separator)))
	(if (> (length file-list) 1)
	    (setq files (concat dired-mark-prefix files dired-mark-postfix)))
	(funcall stuff-it files)))))

;; This is an extra function so that it can be redefined by ange-ftp.
;;;###autoload
(defun dired-run-shell-command (command)
  (let ((handler
	 (find-file-name-handler (directory-file-name default-directory)
				 'shell-command)))
    (if handler (apply handler 'shell-command (list command))
      (shell-command command)))
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)


(defun dired-check-process (msg program &rest arguments)
;  "Display MSG while running PROGRAM, and check for output.
;Remaining arguments are strings passed as command arguments to PROGRAM.
; On error, insert output
; in a log buffer and return the offending ARGUMENTS or PROGRAM.
; Caller can cons up a list of failed args.
;Else returns nil for success."
  (let (err-buffer err (dir default-directory))
    (message "%s..." msg)
    (save-excursion
      ;; Get a clean buffer for error output:
      (setq err-buffer (get-buffer-create " *dired-check-process output*"))
      (set-buffer err-buffer)
      (erase-buffer)
      (setq default-directory dir	; caller's default-directory
	    err (not (eq 0 (apply 'process-file program nil t nil arguments))))
      (if err
	  (progn
	    (dired-log (concat program " " (prin1-to-string arguments) "\n"))
	    (dired-log err-buffer)
	    (or arguments program t))
	(kill-buffer err-buffer)
	(message "%s...done" msg)
	nil))))

;; Commands that delete or redisplay part of the dired buffer.

(defun dired-kill-line (&optional arg)
  "Kill the current line (not the files).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills backward.)"
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let (buffer-read-only file)
    (while (/= 0 arg)
      (setq file (dired-get-filename nil t))
      (if (not file)
	  (error "Can only kill file lines")
	(save-excursion (and file
			     (dired-goto-subdir file)
			     (dired-kill-subdir)))
	(delete-region (line-beginning-position)
		       (progn (forward-line 1) (point)))
	(if (> arg 0)
	    (setq arg (1- arg))
	  (setq arg (1+ arg))
	  (forward-line -1))))
    (dired-move-to-filename)))

;;;###autoload
(defun dired-do-kill-lines (&optional arg fmt)
  "Kill all marked lines (not the files).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills backward.)
If you use this command with a prefix argument to kill the line
for a file that is a directory, which you have inserted in the
Dired buffer as a subdirectory, then it deletes that subdirectory
from the buffer as well.
To kill an entire subdirectory \(without killing its line in the
parent directory), go to its directory header line and use this
command with a prefix argument (the value does not matter)."
  ;; Returns count of killed lines.  FMT="" suppresses message.
  (interactive "P")
  (if arg
      (if (dired-get-subdir)
	  (dired-kill-subdir)
	(dired-kill-line arg))
    (save-excursion
      (goto-char (point-min))
      (let (buffer-read-only
	    (count 0)
	    (regexp (dired-marker-regexp)))
	(while (and (not (eobp))
		    (re-search-forward regexp nil t))
	  (setq count (1+ count))
	  (delete-region (line-beginning-position)
			 (progn (forward-line 1) (point))))
	(or (equal "" fmt)
	    (message (or fmt "Killed %d line%s.") count (dired-plural-s count)))
	count))))

;;;###end dired-cmd.el

;;; 30K
;;;###begin dired-cp.el

(defun dired-compress ()
  ;; Compress or uncompress the current file.
  ;; Return nil for success, offending filename else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (new-file (dired-compress-file from-file)))
    (if new-file
	(let ((start (point)))
	  ;; Remove any preexisting entry for the name NEW-FILE.
	  (condition-case nil
	      (dired-remove-entry new-file)
	    (error nil))
	  (goto-char start)
	  ;; Now replace the current line with an entry for NEW-FILE.
	  (dired-update-file-line new-file) nil)
      (dired-log (concat "Failed to compress" from-file))
      from-file)))

(defvar dired-compress-file-suffixes
  '(("\\.gz\\'" "" "gunzip")
    ("\\.tgz\\'" ".tar" "gunzip")
    ("\\.Z\\'" "" "uncompress")
    ;; For .z, try gunzip.  It might be an old gzip file,
    ;; or it might be from compact? pack? (which?) but gunzip handles both.
    ("\\.z\\'" "" "gunzip")
    ("\\.dz\\'" "" "dictunzip")
    ("\\.tbz\\'" ".tar" "bunzip2")
    ("\\.bz2\\'" "" "bunzip2")
    ("\\.xz\\'" "" "unxz")
    ;; This item controls naming for compression.
    ("\\.tar\\'" ".tgz" nil))
  "Control changes in file name suffixes for compression and uncompression.
Each element specifies one transformation rule, and has the form:
  (REGEXP NEW-SUFFIX PROGRAM)
The rule applies when the old file name matches REGEXP.
The new file name is computed by deleting the part that matches REGEXP
 (as well as anything after that), then adding NEW-SUFFIX in its place.
If PROGRAM is non-nil, the rule is an uncompression rule,
and uncompression is done by running PROGRAM.
Otherwise, the rule is a compression rule, and compression is done with gzip.")

;;;###autoload
(defun dired-compress-file (file)
  ;; Compress or uncompress FILE.
  ;; Return the name of the compressed or uncompressed file.
  ;; Return nil if no change in files.
  (let ((handler (find-file-name-handler file 'dired-compress-file))
	suffix newname
	(suffixes dired-compress-file-suffixes))
    ;; See if any suffix rule matches this file name.
    (while suffixes
      (let (case-fold-search)
	(if (string-match (car (car suffixes)) file)
	    (setq suffix (car suffixes) suffixes nil))
	(setq suffixes (cdr suffixes))))
    ;; If so, compute desired new name.
    (if suffix
	(setq newname (concat (substring file 0 (match-beginning 0))
			      (nth 1 suffix))))
    (cond (handler
	   (funcall handler 'dired-compress-file file))
	  ((file-symlink-p file)
	   nil)
	  ((and suffix (nth 2 suffix))
	   ;; We found an uncompression rule.
	   (if (not (dired-check-process (concat "Uncompressing " file)
					 (nth 2 suffix) file))
	       newname))
	  (t
	   ;;; We don't recognize the file as compressed, so compress it.
	   ;;; Try gzip; if we don't have that, use compress.
	   (condition-case nil
	       (let ((out-name (concat file ".gz")))
		 (and (or (not (file-exists-p out-name))
			  (y-or-n-p
			   (format "File %s already exists.  Really compress? "
				   out-name)))
		      (not (dired-check-process (concat "Compressing " file)
						"gzip" "-f" file))
		      (or (file-exists-p out-name)
			  (setq out-name (concat file ".z")))
		      ;; Rename the compressed file to NEWNAME
		      ;; if it hasn't got that name already.
		      (if (and newname (not (equal newname out-name)))
			  (progn
			    (rename-file out-name newname t)
			    newname)
			out-name)))
	     (file-error
	      (if (not (dired-check-process (concat "Compressing " file)
					    "compress" "-f" file))
		  ;; Don't use NEWNAME with `compress'.
		  (concat file ".Z"))))))))

(defun dired-mark-confirm (op-symbol arg)
  ;; Request confirmation from the user that the operation described
  ;; by OP-SYMBOL is to be performed on the marked files.
  ;; Confirmation consists in a y-or-n question with a file list
  ;; pop-up unless OP-SYMBOL is a member of `dired-no-confirm'.
  ;; The files used are determined by ARG (as in dired-get-marked-files).
  (or (eq dired-no-confirm t)
      (memq op-symbol dired-no-confirm)
      ;; Pass t for DISTINGUISH-ONE-MARKED so that a single file which
      ;; is marked pops up a window.  That will help the user see
      ;; it isn't the current line file.
      (let ((files (dired-get-marked-files t arg nil t))
	    (string (if (eq op-symbol 'compress) "Compress or uncompress"
		      (capitalize (symbol-name op-symbol)))))
	(dired-mark-pop-up nil op-symbol files (function y-or-n-p)
			   (concat string " "
				   (dired-mark-prompt arg files) "? ")))))

(defun dired-map-over-marks-check (fun arg op-symbol &optional show-progress)
;  "Map FUN over marked files (with second ARG like in dired-map-over-marks)
; and display failures.

; FUN takes zero args.  It returns non-nil (the offending object, e.g.
; the short form of the filename) for a failure and probably logs a
; detailed error explanation using function `dired-log'.

; OP-SYMBOL is a symbol describing the operation performed (e.g.
; `compress').  It is used with `dired-mark-pop-up' to prompt the user
; (e.g. with `Compress * [2 files]? ') and to display errors (e.g.
; `Failed to compress 1 of 2 files - type W to see why ("foo")')

; SHOW-PROGRESS if non-nil means redisplay dired after each file."
  (if (dired-mark-confirm op-symbol arg)
      (let* ((total-list;; all of FUN's return values
	      (dired-map-over-marks (funcall fun) arg show-progress))
	     (total (length total-list))
	     (failures (delq nil total-list))
	     (count (length failures))
	     (string (if (eq op-symbol 'compress) "Compress or uncompress"
		       (capitalize (symbol-name op-symbol)))))
	(if (not failures)
	    (message "%s: %d file%s."
		     string total (dired-plural-s total))
	  ;; end this bunch of errors:
	  (dired-log-summary
	   (format "Failed to %s %d of %d file%s"
		   (downcase string) count total (dired-plural-s total))
	   failures)))))

;;;###autoload
(defun dired-query (sym prompt &rest args)
  "Format PROMPT with ARGS, query user, and store the result in SYM.
The return value is either nil or t.

The user may type y or SPC to accept once; n or DEL to skip once;
! to accept this and subsequent queries; or q or ESC to decline
this and subsequent queries.

If SYM is already bound to a non-nil value, this function may
return automatically without querying the user.  If SYM is !,
return t; if SYM is q or ESC, return nil."
  (let* ((char (symbol-value sym))
	 (char-choices '(?y ?\s ?n ?\177 ?! ?q ?\e)))
    (cond ((eq char ?!)
	   t)       ; accept, and don't ask again
	  ((memq char '(?q ?\e))
	   nil)     ; skip, and don't ask again
	  (t        ; no previous answer - ask now
	   (setq prompt
		 (concat (apply 'format prompt args)
			 (if help-form
			     (format " [Type yn!q or %s] "
				     (key-description (vector help-char)))
			   " [Type y, n, q or !] ")))
	   (set sym (setq char (read-char-choice prompt char-choices)))
	   (if (memq char '(?y ?\s ?!)) t)))))


;;;###autoload
(defun dired-do-compress (&optional arg)
  "Compress or uncompress marked (or next ARG) files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-compress) arg 'compress t))

;; Commands for Emacs Lisp files - load and byte compile

(defun dired-byte-compile ()
  ;; Return nil for success, offending file name else.
  (let* ((filename (dired-get-filename))
	 elc-file buffer-read-only failure)
    (condition-case err
	(save-excursion (byte-compile-file filename))
      (error
       (setq failure err)))
    (setq elc-file (byte-compile-dest-file filename))
    (or (file-exists-p elc-file)
	(setq failure t))
    (if failure
	(progn
	  (dired-log "Byte compile error for %s:\n%s\n" filename failure)
	  (dired-make-relative filename))
      (dired-remove-file elc-file)
      (forward-line)			; insert .elc after its .el file
      (dired-add-file elc-file)
      nil)))

;;;###autoload
(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-byte-compile) arg 'byte-compile t))

(defun dired-load ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
      (load file nil nil t)
      (error (setq failure err)))
    (if (not failure)
	nil
      (dired-log "Load error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

;;;###autoload
(defun dired-do-load (&optional arg)
  "Load the marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-load) arg 'load t))

;;;###autoload
(defun dired-do-redisplay (&optional arg test-for-subdir)
  "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details."
  ;; Moves point if the next ARG files are redisplayed.
  (interactive "P\np")
  (if (and test-for-subdir (dired-get-subdir))
      (let* ((dir (dired-get-subdir))
	     (switches (cdr (assoc-string dir dired-switches-alist))))
	(dired-insert-subdir
	 dir
	 (when arg
	   (read-string "Switches for listing: "
			(or switches
			    dired-subdir-switches
			    dired-actual-switches)))))
    (message "Redisplaying...")
    ;; message much faster than making dired-map-over-marks show progress
    (dired-uncache
     (if (consp dired-directory) (car dired-directory) dired-directory))
    (dired-map-over-marks (let ((fname (dired-get-filename))
				;; Postpone readin hook till we map
				;; over all marked files (Bug#6810).
				(dired-after-readin-hook nil))
			    (message "Redisplaying... %s" fname)
			    (dired-update-file-line fname))
			  arg)
    (run-hooks 'dired-after-readin-hook)
    (dired-move-to-filename)
    (message "Redisplaying...done")))

(defun dired-reset-subdir-switches ()
  "Set `dired-switches-alist' to nil and revert dired buffer."
  (interactive)
  (setq dired-switches-alist nil)
  (revert-buffer))

(defun dired-update-file-line (file)
  ;; Delete the current line, and insert an entry for FILE.
  ;; If FILE is nil, then just delete the current line.
  ;; Keeps any marks that may be present in column one (doing this
  ;; here is faster than with dired-add-entry's optional arg).
  ;; Does not update other dired buffers.  Use dired-relist-entry for that.
  (let* ((opoint (line-beginning-position))
	 (char (char-after opoint))
	 (buffer-read-only))
    (delete-region opoint (progn (forward-line 1) (point)))
    (if file
	(progn
	  (dired-add-entry file nil t)
	  ;; Replace space by old marker without moving point.
	  ;; Faster than goto+insdel inside a save-excursion?
	  (subst-char-in-region opoint (1+ opoint) ?\040 char))))
  (dired-move-to-filename))

;;;###autoload
(defun dired-add-file (filename &optional marker-char)
  (dired-fun-in-all-buffers
   (file-name-directory filename) (file-name-nondirectory filename)
   (function dired-add-entry) filename marker-char))

(defvar dired-omit-mode)
(declare-function dired-omit-regexp "dired-x" ())
(defvar dired-omit-localp)

(defun dired-add-entry (filename &optional marker-char relative)
  "Add a new dired entry for FILENAME.
Optionally mark it with MARKER-CHAR (a character, else uses
`dired-marker-char').  Note that this adds the entry `out of order'
if files are sorted by time, etc.
Skips files that match `dired-trivial-filenames'.
Exposes hidden subdirectories if a file is added there.

If `dired-x' is loaded and `dired-omit-mode' is enabled, skips
files matching `dired-omit-regexp'."
  (if (or (not (featurep 'dired-x))
	  (not dired-omit-mode)
	  ;; Avoid calling ls for files that are going to be omitted anyway.
	  (let ((omit-re (dired-omit-regexp)))
	    (or (string= omit-re "")
		(not (string-match omit-re
				   (cond
				    ((eq 'no-dir dired-omit-localp)
				     filename)
				    ((eq t dired-omit-localp)
				     (dired-make-relative filename))
				    (t
				     (dired-make-absolute
				      filename
				      (file-name-directory filename)))))))))
      ;; Do it!
      (progn
	(setq filename (directory-file-name filename))
	;; Entry is always for files, even if they happen to also be directories
	(let* ((opoint (point))
	       (cur-dir (dired-current-directory))
	       (directory (if relative cur-dir (file-name-directory filename)))
	       reason)
	  (setq filename
		(if relative
		    (file-relative-name filename directory)
		  (file-name-nondirectory filename))
		reason
		(catch 'not-found
		  (if (string= directory cur-dir)
		      (progn
			(skip-chars-forward "^\r\n")
			(if (eq (following-char) ?\r)
			    (dired-unhide-subdir))
			;; We are already where we should be, except when
			;; point is before the subdir line or its total line.
			(let ((p (dired-after-subdir-garbage cur-dir)))
			  (if (< (point) p)
			      (goto-char p))))
		    ;; else try to find correct place to insert
		    (if (dired-goto-subdir directory)
			(progn ;; unhide if necessary
			  (if (looking-at "\r")
			      ;; Point is at end of subdir line.
			      (dired-unhide-subdir))
			  ;; found - skip subdir and `total' line
			  ;; and uninteresting files like . and ..
			  ;; This better not move into the next subdir!
			  (dired-goto-next-nontrivial-file))
		      ;; not found
		      (throw 'not-found "Subdir not found")))
		  (let (buffer-read-only opoint)
		    (beginning-of-line)
		    (setq opoint (point))
		    ;; Don't expand `.'.
		    ;; Show just the file name within directory.
		    (let ((default-directory directory))
		      (dired-insert-directory
		       directory
		       (concat dired-actual-switches " -d")
		       (list filename)))
		    (goto-char opoint)
		    ;; Put in desired marker char.
		    (when marker-char
		      (let ((dired-marker-char
			     (if (integerp marker-char) marker-char
			       dired-marker-char)))
			(dired-mark nil)))
		    ;; Compensate for a bug in ange-ftp.
		    ;; It inserts the file's absolute name, rather than
		    ;; the relative one.  That may be hard to fix since it
		    ;; is probably controlled by something in ftp.
		    (goto-char opoint)
		    (let ((inserted-name (dired-get-filename 'verbatim)))
		      (if (file-name-directory inserted-name)
			  (let (props)
			    (end-of-line)
			    (forward-char (- (length inserted-name)))
			    (setq props (text-properties-at (point)))
			    (delete-char (length inserted-name))
			    (let ((pt (point)))
			      (insert filename)
			      (set-text-properties pt (point) props))
			    (forward-char 1))
			(forward-line 1)))
		    (forward-line -1)
		    (if dired-after-readin-hook
			;; The subdir-alist is not affected...
			(save-excursion ; ...so we can run it right now:
			  (save-restriction
			    (beginning-of-line)
			    (narrow-to-region (point)
					      (line-beginning-position 2))
			    (run-hooks 'dired-after-readin-hook))))
		    (dired-move-to-filename))
		  ;; return nil if all went well
		  nil))
	  (if reason	; don't move away on failure
	      (goto-char opoint))
	  (not reason))) ; return t on success, nil else
    ;; Don't do it (dired-omit-mode).
    ;; Return t for success (perhaps we should return file-exists-p).
    t))

(defun dired-after-subdir-garbage (dir)
  ;; Return pos of first file line of DIR, skipping header and total
  ;; or wildcard lines.
  ;; Important: never moves into the next subdir.
  ;; DIR is assumed to be unhidden.
  (save-excursion
    (or (dired-goto-subdir dir) (error "This cannot happen"))
    (forward-line 1)
    (while (and (not (eolp))		; don't cross subdir boundary
		(not (dired-move-to-filename)))
	(forward-line 1))
    (point)))

;;;###autoload
(defun dired-remove-file (file)
  (dired-fun-in-all-buffers
   (file-name-directory file) (file-name-nondirectory file)
   (function dired-remove-entry) file))

(defun dired-remove-entry (file)
  (save-excursion
    (and (dired-goto-file file)
	 (let (buffer-read-only)
	   (delete-region (progn (beginning-of-line) (point))
			  (line-beginning-position 2))))))

;;;###autoload
(defun dired-relist-file (file)
  "Create or update the line for FILE in all Dired buffers it would belong in."
  (dired-fun-in-all-buffers (file-name-directory file)
			    (file-name-nondirectory file)
			    (function dired-relist-entry) file))

(defun dired-relist-entry (file)
  ;; Relist the line for FILE, or just add it if it did not exist.
  ;; FILE must be an absolute file name.
  (let (buffer-read-only marker)
    ;; If cursor is already on FILE's line delete-region will cause
    ;; save-excursion to fail because of floating makers,
    ;; moving point to beginning of line.  Sigh.
    (save-excursion
      (and (dired-goto-file file)
	   (delete-region (progn (beginning-of-line)
				 (setq marker (following-char))
				 (point))
			  (line-beginning-position 2)))
      (setq file (directory-file-name file))
      (dired-add-entry file (if (eq ?\040 marker) nil marker)))))

;;; Copy, move/rename, making hard and symbolic links

(defcustom dired-backup-overwrite nil
  "Non-nil if Dired should ask about making backups before overwriting files.
Special value `always' suppresses confirmation."
  :type '(choice (const :tag "off" nil)
		 (const :tag "suppress" always)
		 (other :tag "ask" t))
  :group 'dired)

;; This is a fluid var used in dired-handle-overwrite.  It should be
;; let-bound whenever dired-copy-file etc are called.  See
;; dired-create-files for an example.
(defvar dired-overwrite-confirmed)

(defun dired-handle-overwrite (to)
  ;; Save old version of file TO that is to be overwritten.
  ;; `dired-overwrite-confirmed' and `overwrite-backup-query' are fluid vars
  ;; from dired-create-files.
  (let (backup)
    (when (and dired-backup-overwrite
	       dired-overwrite-confirmed
	       (setq backup (car (find-backup-file-name to)))
	       (or (eq 'always dired-backup-overwrite)
		   (dired-query 'overwrite-backup-query
				"Make backup for existing file `%s'? "
				to)))
      (rename-file to backup 0)	; confirm overwrite of old backup
      (dired-relist-entry backup))))

;;;###autoload
(defun dired-copy-file (from to ok-flag)
  (dired-handle-overwrite to)
  (dired-copy-file-recursive from to ok-flag dired-copy-preserve-time t
			     dired-recursive-copies))

(declare-function make-symbolic-link "fileio.c")

(defun dired-copy-file-recursive (from to ok-flag &optional
				       preserve-time top recursive)
  (when (and (eq t (car (file-attributes from)))
	     (file-in-directory-p to from))
    (error "Cannot copy `%s' into its subdirectory `%s'" from to))
  (let ((attrs (file-attributes from)))
    (if (and recursive
	     (eq t (car attrs))
	     (or (eq recursive 'always)
		 (yes-or-no-p (format "Recursive copies of %s? " from))))
	;; This is a directory.
	(copy-directory from to preserve-time)
      ;; Not a directory.
      (or top (dired-handle-overwrite to))
      (condition-case err
	  (if (stringp (car attrs))
	      ;; It is a symlink
	      (make-symbolic-link (car attrs) to ok-flag)
	    (copy-file from to ok-flag preserve-time))
	(file-date-error
	 (push (dired-make-relative from)
	       dired-create-files-failures)
	 (dired-log "Can't set date on %s:\n%s\n" from err))))))

;;;###autoload
(defun dired-rename-file (file newname ok-if-already-exists)
  (dired-handle-overwrite newname)
  (rename-file file newname ok-if-already-exists) ; error is caught in -create-files
  ;; Silently rename the visited file of any buffer visiting this file.
  (and (get-file-buffer file)
       (with-current-buffer (get-file-buffer file)
	 (set-visited-file-name newname nil t)))
  (dired-remove-file file)
  ;; See if it's an inserted subdir, and rename that, too.
  (dired-rename-subdir file newname))

(defun dired-rename-subdir (from-dir to-dir)
  (setq from-dir (file-name-as-directory from-dir)
	to-dir (file-name-as-directory to-dir))
  (dired-fun-in-all-buffers from-dir nil
			    (function dired-rename-subdir-1) from-dir to-dir)
  ;; Update visited file name of all affected buffers
  (let ((expanded-from-dir (expand-file-name from-dir))
	(blist (buffer-list)))
    (while blist
      (with-current-buffer (car blist)
	(if (and buffer-file-name
		 (dired-in-this-tree buffer-file-name expanded-from-dir))
	    (let ((modflag (buffer-modified-p))
		  (to-file (dired-replace-in-string
			    (concat "^" (regexp-quote from-dir))
			    to-dir
			    buffer-file-name)))
	      (set-visited-file-name to-file)
	      (set-buffer-modified-p modflag))))
      (setq blist (cdr blist)))))

(defun dired-rename-subdir-1 (dir to)
  ;; Rename DIR to TO in headerlines and dired-subdir-alist, if DIR or
  ;; one of its subdirectories is expanded in this buffer.
  (let ((expanded-dir (expand-file-name dir))
	(alist dired-subdir-alist)
	(elt nil))
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (if (dired-in-this-tree (car elt) expanded-dir)
	  ;; ELT's subdir is affected by the rename
	  (dired-rename-subdir-2 elt dir to)))
    (if (equal dir default-directory)
	;; if top level directory was renamed, lots of things have to be
	;; updated:
	(progn
	  (dired-unadvertise dir)	; we no longer dired DIR...
	  (setq default-directory to
		dired-directory (expand-file-name;; this is correct
				 ;; with and without wildcards
				 (file-name-nondirectory dired-directory)
				 to))
	  (let ((new-name (file-name-nondirectory
			   (directory-file-name dired-directory))))
	    ;; try to rename buffer, but just leave old name if new
	    ;; name would already exist (don't try appending "<%d>")
	    (or (get-buffer new-name)
		(rename-buffer new-name)))
	  ;; ... we dired TO now:
	  (dired-advertise)))))

(defun dired-rename-subdir-2 (elt dir to)
  ;; Update the headerline and dired-subdir-alist element, as well as
  ;; dired-switches-alist element, of directory described by
  ;; alist-element ELT to reflect the moving of DIR to TO.  Thus, ELT
  ;; describes either DIR itself or a subdir of DIR.
  (save-excursion
    (let ((regexp (regexp-quote (directory-file-name dir)))
	  (newtext (directory-file-name to))
	  buffer-read-only)
      (goto-char (dired-get-subdir-min elt))
      ;; Update subdir headerline in buffer
      (if (not (looking-at dired-subdir-regexp))
	  (error "%s not found where expected - dired-subdir-alist broken?"
		 dir)
	(goto-char (match-beginning 1))
	(if (re-search-forward regexp (match-end 1) t)
	    (replace-match newtext t t)
	  (error "Expected to find `%s' in headerline of %s" dir (car elt))))
      ;; Update buffer-local dired-subdir-alist and dired-switches-alist
      (let ((cons (assoc-string (car elt) dired-switches-alist))
	    (cur-dir (dired-normalize-subdir
		      (dired-replace-in-string regexp newtext (car elt)))))
	(setcar elt cur-dir)
	(when cons (setcar cons cur-dir))))))

;; Bound in dired-create-files
(defvar overwrite-query)
(defvar overwrite-backup-query)

;; The basic function for half a dozen variations on cp/mv/ln/ln -s.
(defun dired-create-files (file-creator operation fn-list name-constructor
					&optional marker-char)
  "Create one or more new files from a list of existing files FN-LIST.
This function also handles querying the user, updating Dired
buffers, and displaying a success or failure message.

FILE-CREATOR should be a function.  It is called once for each
file in FN-LIST, and must create a new file, querying the user
and updating Dired buffers as necessary.  It should accept three
arguments: the old file name, the new name, and an argument
OK-IF-ALREADY-EXISTS with the same meaning as in `copy-file'.

OPERATION should be a capitalized string describing the operation
performed (e.g. `Copy').  It is used for error logging.

FN-LIST is the list of files to copy (full absolute file names).

NAME-CONSTRUCTOR should be a function accepting a single
argument, the name of an old file, and returning either the
corresponding new file name or nil to skip.

Optional MARKER-CHAR is a character with which to mark every
newfile's entry, or t to use the current marker character if the
old file was marked."
  (let (dired-create-files-failures failures
	skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
	     overwrite-backup-query)	; for dired-handle-overwrite
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
	      ;; If DESTNAME is a subdirectory of FROM, not a symlink,
	      ;; and the method in use is copying, signal an error.
	      (and (eq t (car (file-attributes destname)))
		   (eq file-creator 'dired-copy-file)
		   (file-in-directory-p destname from)
		   (error "Cannot copy `%s' into its subdirectory `%s'"
			  from to)))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d" operation success-count total)
                  (dired-add-file to actual-marker-char))
              (file-error		; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format "%s failed for %d file%s in %d requests"
		operation (length failures)
		(dired-plural-s (length failures))
		total)
       failures))
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
		operation (length failures)
		total (dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
		operation (length skipped) total
		(dired-plural-s total))
       skipped))
     (t
      (message "%s: %s file%s"
	       operation success-count (dired-plural-s success-count)))))
  (dired-move-to-filename))

(defun dired-do-create-files (op-symbol file-creator operation arg
					&optional marker-char op1
					how-to)
  "Create a new file for each marked file.
Prompts user for target, which is a directory in which to create
  the new files.  Target may also be a plain file if only one marked
  file exists.  The way the default for the target directory is
  computed depends on the value of `dired-dwim-target-directory'.
OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  will determine whether pop-ups are appropriate for this OP-SYMBOL.
FILE-CREATOR and OPERATION as in `dired-create-files'.
ARG as in `dired-get-marked-files'.
Optional arg MARKER-CHAR as in `dired-create-files'.
Optional arg OP1 is an alternate form for OPERATION if there is
  only one file.
Optional arg HOW-TO determines how to treat the target.
  If HOW-TO is nil, use `file-directory-p' to determine if the
   target is a directory.  If so, the marked file(s) are created
   inside that directory.  Otherwise, the target is a plain file;
   an error is raised unless there is exactly one marked file.
  If HOW-TO is t, target is always treated as a plain file.
  Otherwise, HOW-TO should be a function of one argument, TARGET.
   If its return value is nil, TARGET is regarded as a plain file.
   If it return value is a list, TARGET is a generalized
    directory (e.g. some sort of archive).  The first element of
    this list must be a function with at least four arguments:
      operation - as OPERATION above.
      rfn-list  - list of the relative names for the marked files.
      fn-list   - list of the absolute names for the marked files.
      target    - the name of the target itself.
      The rest of into-dir are optional arguments.
   For any other return value, TARGET is treated as a directory."
  (or op1 (setq op1 operation))
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (rfn-list (mapcar (function dired-make-relative) fn-list))
	 (dired-one-file	; fluid variable inside dired-create-files
	  (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	 (target-dir (dired-dwim-target-directory))
	 (default (and dired-one-file
		       (expand-file-name (file-name-nondirectory (car fn-list))
					 target-dir)))
	 (defaults (dired-dwim-target-defaults fn-list target-dir))
	 (target (expand-file-name ; fluid variable inside dired-create-files
		  (minibuffer-with-setup-hook
		      (lambda ()
			(set (make-local-variable 'minibuffer-default-add-function) nil)
			(setq minibuffer-default defaults))
		    (dired-mark-read-file-name
		     (concat (if dired-one-file op1 operation) " %s to: ")
		     target-dir op-symbol arg rfn-list default))))
	 (into-dir (cond ((null how-to)
			  ;; Allow DOS/Windows users to change the letter
			  ;; case of a directory.  If we don't test these
			  ;; conditions up front, file-directory-p below
			  ;; will return t because the filesystem is
			  ;; case-insensitive, and Emacs will try to move
			  ;; foo -> foo/foo, which fails.
			  (if (and (memq system-type '(ms-dos windows-nt cygwin))
				   (eq op-symbol 'move)
				   dired-one-file
				   (string= (downcase
					     (expand-file-name (car fn-list)))
					    (downcase
					     (expand-file-name target)))
				   (not (string=
					 (file-name-nondirectory (car fn-list))
					 (file-name-nondirectory target))))
			      nil
			    (file-directory-p target)))
			 ((eq how-to t) nil)
			 (t (funcall how-to target)))))
    (if (and (consp into-dir) (functionp (car into-dir)))
	(apply (car into-dir) operation rfn-list fn-list target (cdr into-dir))
      (if (not (or dired-one-file into-dir))
	  (error "Marked %s: target must be a directory: %s" operation target))
      ;; rename-file bombs when moving directories unless we do this:
      (or into-dir (setq target (directory-file-name target)))
      (dired-create-files
       file-creator operation fn-list
       (if into-dir			; target is a directory
	   ;; This function uses fluid variable target when called
	   ;; inside dired-create-files:
	   (function
	    (lambda (from)
	      (expand-file-name (file-name-nondirectory from) target)))
	 (function (lambda (_from) target)))
       marker-char))))

;; Read arguments for a marked-files command that wants a file name,
;; perhaps popping up the list of marked files.
;; ARG is the prefix arg and indicates whether the files came from
;; marks (ARG=nil) or a repeat factor (integerp ARG).
;; If the current file was used, the list has but one element and ARG
;; does not matter. (It is non-nil, non-integer in that case, namely '(4)).
;; DEFAULT is the default value to return if the user just hits RET;
;; if it is omitted or nil, then the name of the directory is used.

(defun dired-mark-read-file-name (prompt dir op-symbol arg files
					 &optional default)
  (dired-mark-pop-up
   nil op-symbol files
   (function read-file-name)
   (format prompt (dired-mark-prompt arg files)) dir default))

(defun dired-dwim-target-directory ()
  ;; Try to guess which target directory the user may want.
  ;; If there is a dired buffer displayed in one of the next windows,
  ;; use its current subdir, else use current subdir of this dired buffer.
  (let ((this-dir (and (eq major-mode 'dired-mode)
		       (dired-current-directory))))
    ;; non-dired buffer may want to profit from this function, e.g. vm-uudecode
    (if dired-dwim-target
	(let* ((other-win (get-window-with-predicate
			   (lambda (window)
			     (with-current-buffer (window-buffer window)
			       (eq major-mode 'dired-mode)))))
	       (other-dir (and other-win
			       (with-current-buffer (window-buffer other-win)
				 (and (eq major-mode 'dired-mode)
				      (dired-current-directory))))))
	  (or other-dir this-dir))
      this-dir)))

(defun dired-dwim-target-defaults (fn-list target-dir)
  ;; Return a list of default values for file-reading functions in Dired.
  ;; This list may contain directories from Dired buffers in other windows.
  ;; `fn-list' is a list of file names used to build a list of defaults.
  ;; When nil or more than one element, a list of defaults will
  ;; contain only directory names.  `target-dir' is a directory name
  ;; to exclude from the returned list, for the case when this
  ;; directory name is already presented in initial input.
  ;; For Dired operations that support `dired-dwim-target',
  ;; the argument `target-dir' should have the value returned
  ;; from `dired-dwim-target-directory'.
  (let ((dired-one-file
	 (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	(current-dir (and (eq major-mode 'dired-mode)
			  (dired-current-directory)))
	dired-dirs)
    ;; Get a list of directories of visible buffers in dired-mode.
    (walk-windows (lambda (w)
		    (with-current-buffer (window-buffer w)
		      (and (eq major-mode 'dired-mode)
			   (push (dired-current-directory) dired-dirs)))))
    ;; Force the current dir to be the first in the list.
    (setq dired-dirs
	  (delete-dups (delq nil (cons current-dir (nreverse dired-dirs)))))
    ;; Remove the target dir (if specified) or the current dir from
    ;; default values, because it should be already in initial input.
    (setq dired-dirs (delete (or target-dir current-dir) dired-dirs))
    ;; Return a list of default values.
    (if dired-one-file
	;; For one file operation, provide a list that contains
	;; other directories, other directories with the appended filename
	;; and the current directory with the appended filename, e.g.
	;; 1. /TARGET-DIR/
	;; 2. /TARGET-DIR/FILENAME
	;; 3. /CURRENT-DIR/FILENAME
	(append dired-dirs
		(mapcar (lambda (dir)
			  (expand-file-name
			   (file-name-nondirectory (car fn-list)) dir))
			(reverse dired-dirs))
		(list (expand-file-name
		       (file-name-nondirectory (car fn-list))
		       (or target-dir current-dir))))
      ;; For multi-file operation, return only a list of other directories.
      dired-dirs)))


;;;###autoload
(defun dired-create-directory (directory)
  "Create a directory called DIRECTORY.
If DIRECTORY already exists, signal an error."
  (interactive
   (list (read-file-name "Create directory: " (dired-current-directory))))
  (let* ((expanded (directory-file-name (expand-file-name directory)))
	 (try expanded) new)
    (if (file-exists-p expanded)
	(error "Cannot create directory %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
	    try (directory-file-name (file-name-directory try))))
    (make-directory expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))

(defun dired-into-dir-with-symlinks (target)
  (and (file-directory-p target)
       (not (file-symlink-p target))))
;; This may not always be what you want, especially if target is your
;; home directory and it happens to be a symbolic link, as is often the
;; case with NFS and automounters.  Or if you want to make symlinks
;; into directories that themselves are only symlinks, also quite
;; common.

;; So we don't use this function as value for HOW-TO in
;; dired-do-symlink, which has the minor disadvantage of
;; making links *into* a symlinked-dir, when you really wanted to
;; *overwrite* that symlink.  In that (rare, I guess) case, you'll
;; just have to remove that symlink by hand before making your marked
;; symlinks.

(defvar dired-copy-how-to-fn nil
  "Either nil or a function used by `dired-do-copy' to determine target.
See HOW-TO argument for `dired-do-create-files'.")

;;;###autoload
(defun dired-do-copy (&optional arg)
  "Copy all marked (or next ARG) files, or copy the current file.
This normally preserves the last-modified date when copying.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory,
and new copies of these files are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

This command copies symbolic links by creating new ones,
like `cp -d'."
  (interactive "P")
  (let ((dired-recursive-copies dired-recursive-copies))
    (dired-do-create-files 'copy (function dired-copy-file)
			   "Copy"
			   arg dired-keep-marker-copy
			   nil dired-copy-how-to-fn)))

;;;###autoload
(defun dired-do-symlink (&optional arg)
  "Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

For relative symlinks, use \\[dired-do-relsymlink]."
  (interactive "P")
  (dired-do-create-files 'symlink (function make-symbolic-link)
			   "Symlink" arg dired-keep-marker-symlink))

;;;###autoload
(defun dired-do-hardlink (&optional arg)
  "Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see."
  (interactive "P")
  (dired-do-create-files 'hardlink (function dired-hardlink)
			   "Hardlink" arg dired-keep-marker-hardlink))

(defun dired-hardlink (file newname &optional ok-if-already-exists)
  (dired-handle-overwrite newname)
  ;; error is caught in -create-files
  (add-name-to-file file newname ok-if-already-exists)
  ;; Update the link count
  (dired-relist-file file))

;;;###autoload
(defun dired-do-rename (&optional arg)
  "Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory.
This command also renames any buffers that are visiting the files.
The default suggested for the target directory depends on the value
of `dired-dwim-target', which see."
  (interactive "P")
  (dired-do-create-files 'move (function dired-rename-file)
			 "Move" arg dired-keep-marker-rename "Rename"))
;;;###end dired-cp.el

;;; 5K
;;;###begin dired-re.el
(defvar rename-regexp-query)

(defun dired-do-create-files-regexp
  (file-creator operation arg regexp newname &optional whole-name marker-char)
  ;; Create a new file for each marked file using regexps.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; Matches each marked file against REGEXP and constructs the new
  ;;   filename from NEWNAME (like in function replace-match).
  ;; Optional arg WHOLE-NAME means match/replace the whole file name
  ;;   instead of only the non-directory part of the file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (operation-prompt (concat operation " `%s' to `%s'?"))
	 (rename-regexp-help-form (format "\
Type SPC or `y' to %s one match, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					  (downcase operation)
					  (downcase operation)))
	 (regexp-name-constructor
	  ;; Function to construct new filename using REGEXP and NEWNAME:
	  (if whole-name		; easy (but rare) case
	      (function
	       (lambda (from)
		 (let ((to (dired-string-replace-match regexp from newname))
		       ;; must bind help-form directly around call to
		       ;; dired-query
		       (help-form rename-regexp-help-form))
		   (if to
		       (and (dired-query 'rename-regexp-query
					 operation-prompt
					 from
					 to)
			    to)
		     (dired-log "%s: %s did not match regexp %s\n"
				operation from regexp)))))
	    ;; not whole-name, replace non-directory part only
	    (function
	     (lambda (from)
	       (let* ((new (dired-string-replace-match
			    regexp (file-name-nondirectory from) newname))
		      (to (and new	; nil means there was no match
			       (expand-file-name new
						 (file-name-directory from))))
		      (help-form rename-regexp-help-form))
		 (if to
		     (and (dired-query 'rename-regexp-query
				       operation-prompt
				       (dired-make-relative from)
				       (dired-make-relative to))
			  to)
		   (dired-log "%s: %s did not match regexp %s\n"
			      operation (file-name-nondirectory from) regexp)))))))
	 rename-regexp-query)
    (dired-create-files
     file-creator operation fn-list regexp-name-constructor marker-char)))

(defun dired-mark-read-regexp (operation)
  ;; Prompt user about performing OPERATION.
  ;; Read and return list of: regexp newname arg whole-name.
  (let* ((whole-name
	  (equal 0 (prefix-numeric-value current-prefix-arg)))
	 (arg
	  (if whole-name nil current-prefix-arg))
	 (regexp
	  (dired-read-regexp
	   (concat (if whole-name "Abs. " "") operation " from (regexp): ")))
	 (newname
	  (read-string
	   (concat (if whole-name "Abs. " "") operation " " regexp " to: "))))
    (list regexp newname arg whole-name)))

;;;###autoload
(defun dired-do-rename-regexp (regexp newname &optional arg whole-name)
  "Rename selected files whose names match REGEXP to NEWNAME.

With non-zero prefix argument ARG, the command operates on the next ARG
files.  Otherwise, it operates on all the marked files, or the current
file if none are marked.

As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.

With a zero prefix arg, renaming by regexp affects the absolute file name.
Normally, only the non-directory part of the file name is used and changed."
  (interactive (dired-mark-read-regexp "Rename"))
  (dired-do-create-files-regexp
   (function dired-rename-file)
   "Rename" arg regexp newname whole-name dired-keep-marker-rename))

;;;###autoload
(defun dired-do-copy-regexp (regexp newname &optional arg whole-name)
  "Copy selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "Copy"))
  (let ((dired-recursive-copies nil))	; No recursive copies.
    (dired-do-create-files-regexp
     (function dired-copy-file)
     (if dired-copy-preserve-time "Copy [-p]" "Copy")
     arg regexp newname whole-name dired-keep-marker-copy)))

;;;###autoload
(defun dired-do-hardlink-regexp (regexp newname &optional arg whole-name)
  "Hardlink selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "HardLink"))
  (dired-do-create-files-regexp
   (function add-name-to-file)
   "HardLink" arg regexp newname whole-name dired-keep-marker-hardlink))

;;;###autoload
(defun dired-do-symlink-regexp (regexp newname &optional arg whole-name)
  "Symlink selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "SymLink"))
  (dired-do-create-files-regexp
   (function make-symbolic-link)
   "SymLink" arg regexp newname whole-name dired-keep-marker-symlink))

(defvar rename-non-directory-query)

(defun dired-create-files-non-directory
  (file-creator basename-constructor operation arg)
  ;; Perform FILE-CREATOR on the non-directory part of marked files
  ;; using function BASENAME-CONSTRUCTOR, with query for each file.
  ;; OPERATION like in dired-create-files, ARG as in dired-get-marked-files.
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (dired-get-marked-files nil arg)
     (function
      (lambda (from)
	(let ((to (concat (file-name-directory from)
			  (funcall basename-constructor
				   (file-name-nondirectory from)))))
	  (and (let ((help-form (format "\
Type SPC or `y' to %s one file, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					(downcase operation)
					(downcase operation))))
		 (dired-query 'rename-non-directory-query
			      (concat operation " `%s' to `%s'")
			      (dired-make-relative from)
			      (dired-make-relative to)))
	       to))))
     dired-keep-marker-rename)))

(defun dired-rename-non-directory (basename-constructor operation arg)
  (dired-create-files-non-directory
   (function dired-rename-file)
   basename-constructor operation arg))

;;;###autoload
(defun dired-upcase (&optional arg)
  "Rename all marked (or next ARG) files to upper case."
  (interactive "P")
  (dired-rename-non-directory (function upcase) "Rename upcase" arg))

;;;###autoload
(defun dired-downcase (&optional arg)
  "Rename all marked (or next ARG) files to lower case."
  (interactive "P")
  (dired-rename-non-directory (function downcase) "Rename downcase" arg))

;;;###end dired-re.el

;;; 13K
;;;###begin dired-ins.el

;;;###autoload
(defun dired-maybe-insert-subdir (dirname &optional
					  switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details."
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: "
			  (or dired-subdir-switches dired-actual-switches)))))
  (let ((opoint (point)))
    ;; We don't need a marker for opoint as the subdir is always
    ;; inserted *after* opoint.
    (setq dirname (file-name-as-directory dirname))
    (or (and (not switches)
	     (dired-goto-subdir dirname))
	(dired-insert-subdir dirname switches no-error-if-not-dir-p))
    ;; Push mark so that it's easy to find back.  Do this after the
    ;; insert message so that the user sees the `Mark set' message.
    (push-mark opoint)))

;;;###autoload
(defun dired-insert-subdir (dirname &optional switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the `ls' switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output."
  ;; NO-ERROR-IF-NOT-DIR-P needed for special filesystems like
  ;; Prospero where dired-ls does the right thing, but
  ;; file-directory-p has not been redefined.
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: "
			  (or dired-subdir-switches dired-actual-switches)))))
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (or no-error-if-not-dir-p
      (file-directory-p dirname)
      (error  "Attempt to insert a non-directory: %s" dirname))
  (let ((elt (assoc dirname dired-subdir-alist))
	(cons (assoc-string dirname dired-switches-alist))
	(modflag (buffer-modified-p))
	(old-switches switches)
	switches-have-R mark-alist case-fold-search buffer-read-only)
    (and (not switches) cons (setq switches (cdr cons)))
    (dired-insert-subdir-validate dirname switches)
    ;; case-fold-search is nil now, so we can test for capital `R':
    (if (setq switches-have-R (and switches (string-match "R" switches)))
	;; avoid duplicated subdirs
	(setq mark-alist (dired-kill-tree dirname t)))
    (if elt
	;; If subdir is already present, remove it and remember its marks
	(setq mark-alist (nconc (dired-insert-subdir-del elt) mark-alist))
      (dired-insert-subdir-newpos dirname)) ; else compute new position
    (dired-insert-subdir-doupdate
     dirname elt (dired-insert-subdir-doinsert dirname switches))
    (when old-switches
      (if cons
	  (setcdr cons switches)
	(push (cons dirname switches) dired-switches-alist)))
    (when switches-have-R
      (dired-build-subdir-alist switches)
      (setq switches (dired-replace-in-string "R" "" switches))
      (dolist (cur-ass dired-subdir-alist)
	(let ((cur-dir (car cur-ass)))
	  (and (dired-in-this-tree cur-dir dirname)
	       (let ((cur-cons (assoc-string cur-dir dired-switches-alist)))
		 (if cur-cons
		     (setcdr cur-cons switches)
		   (push (cons cur-dir switches) dired-switches-alist)))))))
    (dired-initial-position dirname)
    (save-excursion (dired-mark-remembered mark-alist))
    (restore-buffer-modified-p modflag)))

(defun dired-insert-subdir-validate (dirname &optional switches)
  ;; Check that it is valid to insert DIRNAME with SWITCHES.
  ;; Signal an error if invalid (e.g. user typed `i' on `..').
  (or (dired-in-this-tree dirname (expand-file-name default-directory))
      (error  "%s: not in this directory tree" dirname))
  (let ((real-switches (or switches dired-subdir-switches)))
    (when real-switches
      (let (case-fold-search)
	(mapcar
	 (function
	  (lambda (x)
	    (or (eq (null (string-match x real-switches))
		    (null (string-match x dired-actual-switches)))
		(error
		 "Can't have dirs with and without -%s switches together" x))))
	 ;; all switches that make a difference to dired-get-filename:
	 '("F" "b"))))))

(defun dired-alist-add (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Sort alist.
  (dired-alist-add-1 dir new-marker)
  (dired-alist-sort))

(defun dired-alist-sort ()
  ;; Keep the alist sorted on buffer position.
  (setq dired-subdir-alist
	(sort dired-subdir-alist
	      (function (lambda (elt1 elt2)
			  (> (dired-get-subdir-min elt1)
			     (dired-get-subdir-min elt2)))))))

(defun dired-kill-tree (dirname &optional remember-marks kill-root)
  "Kill all proper subdirs of DIRNAME, excluding DIRNAME itself.
Interactively, you can kill DIRNAME as well by using a prefix argument.
In interactive use, the command prompts for DIRNAME.

When called from Lisp, if REMEMBER-MARKS is non-nil, return an alist
of marked files.  If KILL-ROOT is non-nil, kill DIRNAME as well."
  (interactive "DKill tree below directory: \ni\nP")
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (let ((s-alist dired-subdir-alist) dir m-alist)
    (while s-alist
      (setq dir (car (car s-alist))
	    s-alist (cdr s-alist))
      (and (or kill-root (not (string-equal dir dirname)))
	   (dired-in-this-tree dir dirname)
	   (dired-goto-subdir dir)
	   (setq m-alist (nconc (dired-kill-subdir remember-marks) m-alist))))
    m-alist))

(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  ;;(goto-char (point-max))
  (let ((alist dired-subdir-alist) elt dir pos new-pos)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt))
      (if (dired-tree-lessp dir new-dir)
	  ;; Insert NEW-DIR after DIR
	  (setq new-pos (dired-get-subdir-max elt)
		alist nil)))
    (goto-char new-pos))
  ;; want a separating newline between subdirs
  (or (eobp)
      (forward-line -1))
  (insert "\n")
  (point))

(defun dired-insert-subdir-del (element)
  ;; Erase an already present subdir (given by ELEMENT) from buffer.
  ;; Move to that buffer position.  Return a mark-alist.
  (let ((begin-marker (dired-get-subdir-min element)))
    (goto-char begin-marker)
    ;; Are at beginning of subdir (and inside it!).  Now determine its end:
    (goto-char (dired-subdir-max))
    (or (eobp);; want a separating newline _between_ subdirs:
	(forward-char -1))
    (prog1
	(dired-remember-marks begin-marker (point))
      (delete-region begin-marker (point)))))

(defun dired-insert-subdir-doinsert (dirname switches)
  ;; Insert ls output after point.
  ;; Return the boundary of the inserted text (as list of BEG and END).
  (save-excursion
    (let ((begin (point)))
      (let ((dired-actual-switches
	     (or switches
		 dired-subdir-switches
		 (dired-replace-in-string "R" "" dired-actual-switches))))
	(if (equal dirname (car (car (last dired-subdir-alist))))
	    ;; If doing the top level directory of the buffer,
	    ;; redo it as specified in dired-directory.
	    (dired-readin-insert)
	  (dired-insert-directory dirname dired-actual-switches nil nil t)))
      (list begin (point)))))

(defun dired-insert-subdir-doupdate (dirname elt beg-end)
  ;; Point is at the correct subdir alist position for ELT,
  ;; BEG-END is the subdir-region (as list of begin and end).
  (if elt				; subdir was already present
      ;; update its position (should actually be unchanged)
      (set-marker (dired-get-subdir-min elt) (point-marker))
    (dired-alist-add dirname (point-marker)))
  ;; The hook may depend on the subdir-alist containing the just
  ;; inserted subdir, so run it after dired-alist-add:
  (if dired-after-readin-hook
      (save-excursion
	(let ((begin (nth 0 beg-end))
	      (end (nth 1 beg-end)))
	  (goto-char begin)
	  (save-restriction
	    (narrow-to-region begin end)
	    ;; hook may add or delete lines, but the subdir boundary
	    ;; marker floats
	    (run-hooks 'dired-after-readin-hook))))))

(defun dired-tree-lessp (dir1 dir2)
  ;; Lexicographic order on file name components, like `ls -lR':
  ;; DIR1 < DIR2 if DIR1 comes *before* DIR2 in an `ls -lR' listing,
  ;;   i.e., if DIR1 is a (grand)parent dir of DIR2,
  ;;   or DIR1 and DIR2 are in the same parentdir and their last
  ;;   components are string-lessp.
  ;; Thus ("/usr/" "/usr/bin") and ("/usr/a/" "/usr/b/") are tree-lessp.
  ;; string-lessp could arguably be replaced by file-newer-than-file-p
  ;;   if dired-actual-switches contained `t'.
  (setq dir1 (file-name-as-directory dir1)
	dir2 (file-name-as-directory dir2))
  (let ((components-1 (dired-split "/" dir1))
	(components-2 (dired-split "/" dir2)))
    (while (and components-1
		components-2
		(equal (car components-1) (car components-2)))
      (setq components-1 (cdr components-1)
	    components-2 (cdr components-2)))
    (let ((c1 (car components-1))
	  (c2 (car components-2)))

      (cond ((and c1 c2)
	     (string-lessp c1 c2))
	    ((and (null c1) (null c2))
	     nil)			; they are equal, not lessp
	    ((null c1)			; c2 is a subdir of c1: c1<c2
	     t)
	    ((null c2)			; c1 is a subdir of c2: c1>c2
	     nil)
	    (t (error "This can't happen"))))))

;; There should be a builtin split function - inverse to mapconcat.
(defun dired-split (pat str &optional limit)
  "Splitting on regexp PAT, turn string STR into a list of substrings.
Optional third arg LIMIT (>= 1) is a limit to the length of the
resulting list.
Thus, if SEP is a regexp that only matches itself,

   (mapconcat 'identity (dired-split SEP STRING) SEP)

is always equal to STRING."
  (let* ((start (string-match pat str))
	 (result (list (substring str 0 start)))
	 (count 1)
	 (end (if start (match-end 0))))
    (if end				; else nothing left
	(while (and (or (not (integerp limit))
			(< count limit))
		    (string-match pat str end))
	  (setq start (match-beginning 0)
		count (1+ count)
		result (cons (substring str end start) result)
		end (match-end 0)
		start end)
	  ))
    (if (and (or (not (integerp limit))
		 (< count limit))
	     end)			; else nothing left
	(setq result
	      (cons (substring str end) result)))
    (nreverse result)))

;;; moving by subdirectories

;;;###autoload
(defun dired-prev-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  ;;(interactive "p")
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   ;; if on subdir start already, don't stay there!
	   (if (dired-get-subdir) 1 0))))
  (dired-next-subdir (- arg) no-error-if-not-found no-skip))

(defun dired-subdir-min ()
  (save-excursion
    (if (not (dired-prev-subdir 0 t t))
	(error "Not in a subdir!")
      (point))))

;;;###autoload
(defun dired-goto-subdir (dir)
  "Go to end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden."
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (completing-read "Goto in situ directory: " ; prompt
			       dired-subdir-alist ; table
			       nil	; predicate
			       t	; require-match
			       (dired-current-directory))))
     (push-mark)))
  (setq dir (file-name-as-directory dir))
  (let ((elt (assoc dir dired-subdir-alist)))
    (and elt
	 (goto-char (dired-get-subdir-min elt))
	 ;; dired-subdir-hidden-p and dired-add-entry depend on point being
	 ;; at either \r or \n after this function succeeds.
	 (progn (skip-chars-forward "^\r\n")
		(point)))))

;;;###autoload
(defun dired-mark-subdir-files ()
  "Mark all files except `.' and `..' in current subdirectory.
If the Dired buffer shows multiple directories, this command
marks the files listed in the subdirectory that point is in."
  (interactive)
  (let ((p-min (dired-subdir-min)))
    (dired-mark-files-in-region p-min (dired-subdir-max))))

;;;###autoload
(defun dired-kill-subdir (&optional remember-marks)
  "Remove all lines of current subdirectory.
Lower levels are unaffected."
  ;; With optional REMEMBER-MARKS, return a mark-alist.
  (interactive)
  (let* ((beg (dired-subdir-min))
	 (end (dired-subdir-max))
	 (modflag (buffer-modified-p))
	 (cur-dir (dired-current-directory))
	 (cons (assoc-string cur-dir dired-switches-alist))
	 buffer-read-only)
    (if (equal cur-dir default-directory)
	(error "Attempt to kill top level directory"))
    (prog1
	(if remember-marks (dired-remember-marks beg end))
      (delete-region beg end)
      (if (eobp)			; don't leave final blank line
	  (delete-char -1))
      (dired-unsubdir cur-dir)
      (when cons
	(setq dired-switches-alist (delete cons dired-switches-alist)))
      (restore-buffer-modified-p modflag))))

(defun dired-unsubdir (dir)
  ;; Remove DIR from the alist
  (setq dired-subdir-alist
	(delq (assoc dir dired-subdir-alist) dired-subdir-alist)))

;;;###autoload
(defun dired-tree-up (arg)
  "Go up ARG levels in the dired tree."
  (interactive "p")
  (let ((dir (dired-current-directory)))
    (while (>= arg 1)
      (setq arg (1- arg)
	    dir (file-name-directory (directory-file-name dir))))
    ;;(setq dir (expand-file-name dir))
    (or (dired-goto-subdir dir)
	(error "Cannot go up to %s - not in this tree" dir))))

;;;###autoload
(defun dired-tree-down ()
  "Go down in the dired tree."
  (interactive)
  (let ((dir (dired-current-directory)) ; has slash
	pos case-fold-search)		; filenames are case sensitive
    (let ((rest (reverse dired-subdir-alist)) elt)
      (while rest
	(setq elt (car rest)
	      rest (cdr rest))
	(if (dired-in-this-tree (directory-file-name (car elt)) dir)
	    (setq rest nil
		  pos (dired-goto-subdir (car elt))))))
    (if pos
	(goto-char pos)
      (error "At the bottom"))))

;;; hiding

(defun dired-unhide-subdir ()
  (let (buffer-read-only)
    (subst-char-in-region (dired-subdir-min) (dired-subdir-max) ?\r ?\n)))

(defun dired-hide-check ()
  (or selective-display
      (error "selective-display must be t for subdir hiding to work!")))

(defun dired-subdir-hidden-p (dir)
  (and selective-display
       (save-excursion
	 (dired-goto-subdir dir)
	 (looking-at "\r"))))

;;;###autoload
(defun dired-hide-subdir (arg)
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories."
  (interactive "p")
  (dired-hide-check)
  (let ((modflag (buffer-modified-p)))
    (while (>=  (setq arg (1- arg)) 0)
      (let* ((cur-dir (dired-current-directory))
	     (hidden-p (dired-subdir-hidden-p cur-dir))
	     (elt (assoc cur-dir dired-subdir-alist))
	     (end-pos (1- (dired-get-subdir-max elt)))
	     buffer-read-only)
	;; keep header line visible, hide rest
	(goto-char (dired-get-subdir-min elt))
	(skip-chars-forward "^\n\r")
	(if hidden-p
	    (subst-char-in-region (point) end-pos ?\r ?\n)
	  (subst-char-in-region (point) end-pos ?\n ?\r)))
      (dired-next-subdir 1 t))
    (restore-buffer-modified-p modflag)))

;;;###autoload
(defun dired-hide-all (&optional ignored)
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
  (interactive "P")
  (dired-hide-check)
  (let ((modflag (buffer-modified-p))
	buffer-read-only)
    (if (save-excursion
	  (goto-char (point-min))
	  (search-forward "\r" nil t))
	;; unhide - bombs on \r in filenames
	(subst-char-in-region (point-min) (point-max) ?\r ?\n)
      ;; hide
      (let ((pos (point-max))		; pos of end of last directory
	    (alist dired-subdir-alist))
	(while alist		        ; while there are dirs before pos
	  (subst-char-in-region (dired-get-subdir-min (car alist)) ; pos of prev dir
				(save-excursion
				  (goto-char pos) ; current dir
				  ;; we're somewhere on current dir's line
				  (forward-line -1)
				  (point))
				?\n ?\r)
	  (setq pos (dired-get-subdir-min (car alist)))	; prev dir gets current dir
	  (setq alist (cdr alist)))))
    (restore-buffer-modified-p modflag)))

;;;###end dired-ins.el


;; Search only in file names in the Dired buffer.

(defcustom dired-isearch-filenames nil
  "Non-nil to Isearch in file names only.
If t, Isearch in Dired always matches only file names.
If `dwim', Isearch matches file names when initial point position is on
a file name.  Otherwise, it searches the whole buffer without restrictions."
  :type '(choice (const :tag "No restrictions" nil)
		 (const :tag "When point is on a file name initially, search file names" dwim)
		 (const :tag "Always search in file names" t))
  :group 'dired
  :version "23.1")

(defvar dired-isearch-filter-predicate-orig nil)

(defun dired-isearch-filenames-toggle ()
  "Toggle file names searching on or off.
When on, Isearch skips matches outside file names using the predicate
`dired-isearch-filter-filenames' that matches only at file names.
When off, it uses the original predicate."
  (interactive)
  (setq isearch-filter-predicate
	(if (eq isearch-filter-predicate 'dired-isearch-filter-filenames)
	    dired-isearch-filter-predicate-orig
	  'dired-isearch-filter-filenames))
  (setq isearch-success t isearch-adjusted t)
  (isearch-update))

;;;###autoload
(defun dired-isearch-filenames-setup ()
  "Set up isearch to search in Dired file names.
Intended to be added to `isearch-mode-hook'."
  (when (or (eq dired-isearch-filenames t)
	    (and (eq dired-isearch-filenames 'dwim)
		 (get-text-property (point) 'dired-filename)))
    (setq isearch-message-prefix-add "filename ")
    (define-key isearch-mode-map "\M-sf" 'dired-isearch-filenames-toggle)
    (setq dired-isearch-filter-predicate-orig
	  (default-value 'isearch-filter-predicate))
    (setq-default isearch-filter-predicate 'dired-isearch-filter-filenames)
    (add-hook 'isearch-mode-end-hook 'dired-isearch-filenames-end nil t)))

(defun dired-isearch-filenames-end ()
  "Clean up the Dired file name search after terminating isearch."
  (setq isearch-message-prefix-add nil)
  (define-key isearch-mode-map "\M-sf" nil)
  (setq-default isearch-filter-predicate dired-isearch-filter-predicate-orig)
  (remove-hook 'isearch-mode-end-hook 'dired-isearch-filenames-end t))

(defun dired-isearch-filter-filenames (beg end)
  "Test whether the current search hit is a visible file name.
Return non-nil if the text from BEG to END is part of a file
name (has the text property `dired-filename') and is visible."
  (and (isearch-filter-visible beg end)
       (if dired-isearch-filenames
	   (text-property-not-all (min beg end) (max beg end)
				  'dired-filename nil)
	 t)))

;;;###autoload
(defun dired-isearch-filenames ()
  "Search for a string using Isearch only in file names in the Dired buffer."
  (interactive)
  (let ((dired-isearch-filenames t))
    (isearch-forward)))

;;;###autoload
(defun dired-isearch-filenames-regexp ()
  "Search for a regexp using Isearch only in file names in the Dired buffer."
  (interactive)
  (let ((dired-isearch-filenames t))
    (isearch-forward-regexp)))


;; Functions for searching in tags style among marked files.

;;;###autoload
(defun dired-do-isearch ()
  "Search for a string through all marked files using Isearch."
  (interactive)
  (multi-isearch-files
   (dired-get-marked-files nil nil 'dired-nondirectory-p)))

;;;###autoload
(defun dired-do-isearch-regexp ()
  "Search for a regexp through all marked files using Isearch."
  (interactive)
  (multi-isearch-files-regexp
   (dired-get-marked-files nil nil 'dired-nondirectory-p)))

;;;###autoload
(defun dired-do-search (regexp)
  "Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive "sSearch marked files (regexp): ")
  (tags-search regexp '(dired-get-marked-files nil nil 'dired-nondirectory-p)))

;;;###autoload
(defun dired-do-query-replace-regexp (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
			buffer-read-only))
	  (error "File `%s' is visited read-only" file))))
  (tags-query-replace from to delimited
		      '(dired-get-marked-files nil nil 'dired-nondirectory-p)))

(defun dired-nondirectory-p (file)
  (not (file-directory-p file)))

;;;###autoload
(defun dired-show-file-type (file &optional deref-symlinks)
  "Print the type of FILE, according to the `file' command.
If you give a prefix to this command, and FILE is a symbolic
link, then the type of the file linked to by FILE is printed
instead."
  (interactive (list (dired-get-filename t) current-prefix-arg))
  (let (process-file-side-effects)
    (with-temp-buffer
      (if deref-symlinks
	  (process-file "file" nil t t "-L" "--" file)
	(process-file "file" nil t t "--" file))
      (when (bolp)
	(backward-delete-char 1))
      (message "%s" (buffer-string)))))

(provide 'dired-aux)

;; Local Variables:
;; byte-compile-dynamic: t
;; generated-autoload-file: "dired.el"
;; End:

;;; dired-aux.el ends here
