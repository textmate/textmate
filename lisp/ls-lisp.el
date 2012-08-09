;;; ls-lisp.el --- emulate insert-directory completely in Emacs Lisp

;; Copyright (C) 1992, 1994, 2000-2012  Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Modified by: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Maintainer: FSF
;; Keywords: unix, dired
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

;; OVERVIEW ==========================================================

;; This file redefines the function `insert-directory' to implement it
;; directly from Emacs lisp, without running ls in a subprocess.  It
;; is useful if you cannot afford to fork Emacs on a real memory UNIX,
;; or other non-UNIX platforms if you don't have the ls
;; program, or if you want a different format from what ls offers.

;; This function can use regexps instead of shell wildcards.  If you
;; enter regexps remember to double each $ sign.  For example, to
;; include files *.el, enter `.*\.el$$', resulting in the regexp
;; `.*\.el$'.

;; RESTRICTIONS ======================================================

;; * A few obscure ls switches are still ignored: see the docstring of
;; `insert-directory'.

;; TO DO =============================================================

;; Complete handling of F switch (if/when possible).

;; FJW: May be able to sort much faster by consing the sort key onto
;; the front of each list element, sorting and then stripping the key
;; off again!

;;; History:

;; Written originally by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Revised by Andrew Innes and Geoff Volker (and maybe others).

;; Modified by Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>, mainly
;; to support many more ls options, "platform emulation" and more
;; robust sorting.

;;; Code:

(defgroup ls-lisp nil
  "Emulate the ls program completely in Emacs Lisp."
  :version "21.1"
  :group 'dired)

(defun ls-lisp-set-options ()
  "Reset the ls-lisp options that depend on `ls-lisp-emulation'."
  (mapc 'custom-reevaluate-setting
	'(ls-lisp-ignore-case ls-lisp-dirs-first ls-lisp-verbosity)))

(defcustom ls-lisp-emulation
  (cond ;; ((eq system-type 'windows-nt) 'MS-Windows)
	((memq system-type '(hpux usg-unix-v irix berkeley-unix))
	 'UNIX))	; very similar to GNU
  ;; Anything else defaults to nil, meaning GNU.
  "Platform to emulate: GNU (default), MacOS, MS-Windows, UNIX.
Corresponding value is one of: nil, `MacOS', `MS-Windows', `UNIX'.
Set this to your preferred value; it need not match the actual platform
you are using.

This variable does not affect the behavior of ls-lisp directly.
Rather, it controls the default values for some variables that do:
`ls-lisp-ignore-case', `ls-lisp-dirs-first', and `ls-lisp-verbosity'.

If you change this variable directly (without using customize)
after loading `ls-lisp', you should use `ls-lisp-set-options' to
update the dependent variables."
  :type '(choice (const :tag "GNU" nil)
		 (const MacOS)
		 (const MS-Windows)
		 (const UNIX))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (unless (equal value (eval symbol))
	   (custom-set-default symbol value)
	   (ls-lisp-set-options)))
  :group 'ls-lisp)

;; Only made an obsolete alias in 23.3.  Before that, the initial
;; value was set according to:
;;  (or (memq ls-lisp-emulation '(MS-Windows MacOS))
;;      (and (boundp 'ls-lisp-dired-ignore-case) ls-lisp-dired-ignore-case))
;; Which isn't the right thing to do.
(define-obsolete-variable-alias 'ls-lisp-dired-ignore-case
  'ls-lisp-ignore-case "21.1")

(defcustom ls-lisp-ignore-case
  (memq ls-lisp-emulation '(MS-Windows MacOS))
  "Non-nil causes ls-lisp alphabetic sorting to ignore case."
  :set-after '(ls-lisp-emulation)
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-dirs-first (eq ls-lisp-emulation 'MS-Windows)
  "Non-nil causes ls-lisp to sort directories first in any ordering.
\(Or last if it is reversed.)  Follows Microsoft Windows Explorer."
  ;; Functionality suggested by Chris McMahan <cmcmahan@one.net>
  :set-after '(ls-lisp-emulation)
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-verbosity
  (cond ((eq ls-lisp-emulation 'MacOS) nil)
	((eq ls-lisp-emulation 'MS-Windows)
	 (if (and (fboundp 'w32-using-nt) (w32-using-nt))
	     '(links)))			; distinguish NT/2K from 9x
	((eq ls-lisp-emulation 'UNIX) '(links uid)) ; UNIX ls
	(t '(links uid gid)))		; GNU ls
  "A list of optional file attributes that ls-lisp should display.
It should contain none or more of the symbols: links, uid, gid.
A value of nil (or an empty list) means display none of them.

Concepts come from UNIX: `links' means count of names associated with
the file; `uid' means user (owner) identifier; `gid' means group
identifier.

If emulation is MacOS then default is nil;
if emulation is MS-Windows then default is `(links)' if platform is
Windows NT/2K, nil otherwise;
if emulation is UNIX then default is `(links uid)';
if emulation is GNU then default is `(links uid gid)'."
  :set-after '(ls-lisp-emulation)
  ;; Functionality suggested by Howard Melman <howard@silverstream.com>
  :type '(set (const :tag "Show Link Count" links)
	      (const :tag "Show User" uid)
	      (const :tag "Show Group" gid))
  :group 'ls-lisp)

(defcustom ls-lisp-use-insert-directory-program
  (not (memq system-type '(ms-dos windows-nt)))
  "Non-nil causes ls-lisp to revert back to using `insert-directory-program'.
This is useful on platforms where ls-lisp is dumped into Emacs, such as
Microsoft Windows, but you would still like to use a program to list
the contents of a directory."
  :type 'boolean
  :group 'ls-lisp)

;;; Autoloaded because it is let-bound in `recover-session', `mail-recover-1'.
;;;###autoload
(defcustom ls-lisp-support-shell-wildcards t
  "Non-nil means ls-lisp treats file patterns as shell wildcards.
Otherwise they are treated as Emacs regexps (for backward compatibility)."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-format-time-list
  '("%b %e %H:%M"
    "%b %e  %Y")
  "List of `format-time-string' specs to display file time stamps.
These specs are used ONLY if a valid locale can not be determined.

If `ls-lisp-use-localized-time-format' is non-nil, these specs are used
regardless of whether the locale can be determined.

Syntax:  (EARLY-TIME-FORMAT OLD-TIME-FORMAT)

The EARLY-TIME-FORMAT is used if file has been modified within the
current year.  The OLD-TIME-FORMAT is used for older files.  To use ISO
8601 dates, you could set:

\(setq ls-lisp-format-time-list
       '(\"%Y-%m-%d %H:%M\"
         \"%Y-%m-%d      \"))"
  :type '(list (string :tag "Early time format")
	       (string :tag "Old time format"))
  :group 'ls-lisp)

(defcustom ls-lisp-use-localized-time-format nil
  "Non-nil means to always use `ls-lisp-format-time-list' for time stamps.
This applies even if a valid locale is specified.

WARNING: Using localized date/time format might cause Dired columns
to fail to line up, e.g. if month names are not all of the same length."
  :type 'boolean
  :group 'ls-lisp)

(defvar original-insert-directory nil
  "This holds the original function definition of `insert-directory'.")

(defvar ls-lisp-uid-d-fmt "-%d"
  "Format to display integer UIDs.")
(defvar ls-lisp-uid-s-fmt "-%s"
  "Format to display user names.")
(defvar ls-lisp-gid-d-fmt "-%d"
  "Format to display integer GIDs.")
(defvar ls-lisp-gid-s-fmt "-%s"
  "Format to display user group names.")
(defvar ls-lisp-filesize-d-fmt "%d"
  "Format to display integer file sizes.")
(defvar ls-lisp-filesize-f-fmt "%.0f"
  "Format to display float file sizes.")

;; Remember the original insert-directory function
(or (featurep 'ls-lisp)  ; FJW: unless this file is being reloaded!
    (setq original-insert-directory (symbol-function 'insert-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
If the value of `ls-lisp-use-insert-directory-program' is non-nil then
it works exactly like the version from `files.el' and runs a directory
listing program whose name is in the variable
`insert-directory-program'; if also WILDCARD is non-nil then it runs
the shell specified by `shell-file-name'.  If the value of
`ls-lisp-use-insert-directory-program' is nil then it runs a Lisp
emulation.

The Lisp emulation does not run any external programs or shells.  It
supports ordinary shell wildcards if `ls-lisp-support-shell-wildcards'
is non-nil; otherwise, it interprets wildcards as regular expressions
to match file names.  It does not support all `ls' switches -- those
that work are: A a B C c F G g h i n R r S s t U u X.  The l switch
is assumed to be always present and cannot be turned off."
  (if ls-lisp-use-insert-directory-program
      (funcall original-insert-directory
	       file switches wildcard full-directory-p)
    ;; We need the directory in order to find the right handler.
    (let ((handler (find-file-name-handler (expand-file-name file)
					   'insert-directory))
	  (orig-file file)
	  wildcard-regexp)
      (if handler
	  (funcall handler 'insert-directory file switches
		   wildcard full-directory-p)
	;; Remove --dired switch
	(if (string-match "--dired " switches)
	    (setq switches (replace-match "" nil nil switches)))
	;; Convert SWITCHES to a list of characters.
	(setq switches (delete ?\  (delete ?- (append switches nil))))
	;; Sometimes we get ".../foo*/" as FILE.  While the shell and
	;; `ls' don't mind, we certainly do, because it makes us think
	;; there is no wildcard, only a directory name.
	(if (and ls-lisp-support-shell-wildcards
		 (string-match "[[?*]" file)
		 ;; Prefer an existing file to wildcards, like
		 ;; dired-noselect does.
		 (not (file-exists-p file)))
	    (progn
	      (or (not (eq (aref file (1- (length file))) ?/))
		  (setq file (substring file 0 (1- (length file)))))
	      (setq wildcard t)))
	(if wildcard
	    (setq wildcard-regexp
		  (if ls-lisp-support-shell-wildcards
		      (wildcard-to-regexp (file-name-nondirectory file))
		    (file-name-nondirectory file))
		  file (file-name-directory file))
	  (if (memq ?B switches) (setq wildcard-regexp "[^~]\\'")))
	(condition-case err
	    (ls-lisp-insert-directory
	     file switches (ls-lisp-time-index switches)
	     wildcard-regexp full-directory-p)
	  (invalid-regexp
	   ;; Maybe they wanted a literal file that just happens to
	   ;; use characters special to shell wildcards.
	   (if (equal (cadr err) "Unmatched [ or [^")
	       (progn
		 (setq wildcard-regexp (if (memq ?B switches) "[^~]\\'")
		       file (file-relative-name orig-file))
		 (ls-lisp-insert-directory
		  file switches (ls-lisp-time-index switches)
		  nil full-directory-p))
	     (signal (car err) (cdr err)))))
	;; Try to insert the amount of free space.
	(save-excursion
	  (goto-char (point-min))
	  ;; First find the line to put it on.
	  (when (re-search-forward "^total" nil t)
	    (let ((available (get-free-disk-space ".")))
	      (when available
		;; Replace "total" with "total used", to avoid confusion.
		(replace-match "total used in directory")
		(end-of-line)
		(insert " available " available)))))))))

(defun ls-lisp-insert-directory
  (file switches time-index wildcard-regexp full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD-REGEXP is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
  (if (or wildcard-regexp full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir)	; so that file-attributes works
	     (file-alist
	      (directory-files-and-attributes dir nil wildcard-regexp t
					      (if (memq ?n switches)
						  'integer
						'string)))
	     (sum 0)
	     (max-uid-len 0)
	     (max-gid-len 0)
	     (max-file-size 0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size attr
	     fuid fgid uid-len gid-len)
	(setq file-alist (ls-lisp-sanitize file-alist))
	(cond ((memq ?A switches)
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\." file-alist))))
	(setq file-alist
	      (ls-lisp-handle-switches file-alist switches))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist)
	  (setq total-line (cons (point) (car-safe file-alist)))
	  ;; Find the appropriate format for displaying uid, gid, and
	  ;; file size, by finding the longest strings among all the
	  ;; files we are about to display.
	  (dolist (elt file-alist)
	    (setq attr (cdr elt)
		  fuid (nth 2 attr)
		  uid-len (if (stringp fuid) (string-width fuid)
			    (length (format "%d" fuid)))
		  fgid (nth 3 attr)
		  gid-len (if (stringp fgid) (string-width fgid)
			    (length (format "%d" fgid)))
		  file-size (nth 7 attr))
	    (if (> uid-len max-uid-len)
		(setq max-uid-len uid-len))
	    (if (> gid-len max-gid-len)
		(setq max-gid-len gid-len))
	    (if (> file-size max-file-size)
		(setq max-file-size file-size)))
	  (setq ls-lisp-uid-d-fmt (format " %%-%dd" max-uid-len))
	  (setq ls-lisp-uid-s-fmt (format " %%-%ds" max-uid-len))
	  (setq ls-lisp-gid-d-fmt (format " %%-%dd" max-gid-len))
	  (setq ls-lisp-gid-s-fmt (format " %%-%ds" max-gid-len))
	  (setq ls-lisp-filesize-d-fmt
		(format " %%%dd"
			(if (memq ?s switches)
			    (length (format "%.0f"
					    (fceiling (/ max-file-size 1024.0))))
			  (length (format "%.0f" max-file-size)))))
	  (setq ls-lisp-filesize-f-fmt
		(format " %%%d.0f"
			(if (memq ?s switches)
			    (length (format "%.0f"
					    (fceiling (/ max-file-size 1024.0))))
			  (length (format "%.0f" max-file-size)))))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 (setq sum (+ file-size
			      ;; Even if neither SUM nor file's size
			      ;; overflow, their sum could.
			      (if (or (< sum (- 134217727 file-size))
				      (floatp sum)
				      (floatp file-size))
				  sum
				(float sum))))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 ;; Under -F, we have already decorated all
			 ;; directories, including "." and "..", with
			 ;; a /, so allow for that as well.
			 (not (string-match "\\`\\.\\.?/?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard-regexp full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (file-name-absolute-p file) (setq file (expand-file-name file)))
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file 'string)))
      (if fattr
	  (insert (ls-lisp-format
		   (if (memq ?F switches)
		       (ls-lisp-classify-file file fattr)
		     file)
		   fattr (nth 7 fattr)
				  switches time-index))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))		; to show user the message!

(defun ls-lisp-sanitize (file-alist)
  "Sanitize the elements in FILE-ALIST.
Fixes any elements in the alist for directory entries whose file
attributes are nil (meaning that `file-attributes' failed for
them).  This is known to happen for some network shares, in
particular for the \"..\" directory entry.

If the \"..\" directory entry has nil attributes, the attributes
are copied from the \".\" entry, if they are non-nil.  Otherwise,
the offending element is removed from the list, as are any
elements for other directory entries with nil attributes."
  (if (and (null (cdr (assoc ".." file-alist)))
	   (cdr (assoc "." file-alist)))
      (setcdr (assoc ".." file-alist) (cdr (assoc "." file-alist))))
  (rassq-delete-all nil file-alist))

(defun ls-lisp-column-format (file-alist)
  "Insert the file names (only) in FILE-ALIST into the current buffer.
Format in columns, sorted vertically, following GNU ls -C.
Responds to the window width as ls should but may not!"
  (let (files fmt ncols collen (nfiles 0) (colwid 0))
    ;; Count number of files as `nfiles', build list of filenames as
    ;; `files', and find maximum filename length as `colwid':
    (let (file len)
      (while file-alist
	(setq nfiles (1+ nfiles)
	      file (caar file-alist)
	      files (cons file files)
	      file-alist (cdr file-alist)
	      len (length file))
	(if (> len colwid) (setq colwid len))))
    (setq files (nreverse files)
	  colwid (+ 2 colwid)		; 2 character column gap
	  fmt (format "%%-%ds" colwid)	; print format
	  ncols (/ (window-width) colwid) ; no of columns
	  collen (/ nfiles ncols))	; floor of column length
    (if (> nfiles (* collen ncols)) (setq collen (1+ collen)))
    ;; Output the file names in columns, sorted vertically:
    (let ((i 0) j)
      (while (< i collen)
	(setq j i)
	(while (< j nfiles)
	  (insert (format fmt (nth j files)))
	  (setq j (+ j collen)))
	;; FJW: This is completely unnecessary, but I don't like
	;; trailing white space...
	(delete-region (point) (progn (skip-chars-backward " \t") (point)))
	(insert ?\n)
	(setq i (1+ i))))))

(defun ls-lisp-delete-matching (regexp list)
  "Delete all elements matching REGEXP from LIST, return new list."
  ;; Should perhaps use setcdr for efficiency.
  (let (result)
    (while list
      (or (string-match regexp (caar list))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    result))

(defsubst ls-lisp-string-lessp (s1 s2)
  "Return t if string S1 is less than string S2 in lexicographic order.
Case is significant if `ls-lisp-ignore-case' is nil.
Unibyte strings are converted to multibyte for comparison."
  (let ((u (compare-strings s1 0 nil s2 0 nil ls-lisp-ignore-case)))
    (and (numberp u) (< u 0))))

(defun ls-lisp-handle-switches (file-alist switches)
  "Return new FILE-ALIST sorted according to SWITCHES.
SWITCHES is a list of characters.  Default sorting is alphabetic."
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  (or (memq ?U switches)		; unsorted
      ;; Catch and ignore unexpected sorting errors
      (condition-case err
	  (setq file-alist
		(let (index)
		  ;; Copy file-alist in case of error
		  (sort (copy-sequence file-alist) ; modifies its argument!
			(cond ((memq ?S switches)
			       (lambda (x y) ; sorted on size
				 ;; 7th file attribute is file size
				 ;; Make largest file come first
				 (< (nth 7 (cdr y))
				    (nth 7 (cdr x)))))
			      ((setq index (ls-lisp-time-index switches))
			       (lambda (x y) ; sorted on time
				 (time-less-p (nth index (cdr y))
					      (nth index (cdr x)))))
			      ((memq ?X switches)
			       (lambda (x y) ; sorted on extension
				 (ls-lisp-string-lessp
				  (ls-lisp-extension (car x))
				  (ls-lisp-extension (car y)))))
			      (t
			       (lambda (x y) ; sorted alphabetically
				 (ls-lisp-string-lessp (car x) (car y))))))))
	(error (message "Unsorted (ls-lisp sorting error) - %s"
			(error-message-string err))
	       (ding) (sit-for 2))))	; to show user the message!
  (if (memq ?F switches)		; classify switch
      (setq file-alist (mapcar 'ls-lisp-classify file-alist)))
  (if ls-lisp-dirs-first
  ;; Re-sort directories first, without otherwise changing the
  ;; ordering, and reverse whole list.  cadr of each element of
  ;; `file-alist' is t for directory, string (name linked to) for
  ;; symbolic link, or nil.
      (let (el dirs files)
	(while file-alist
	  (if (or (eq (cadr (setq el (car file-alist))) t) ; directory
                  (and (stringp (cadr el))
                       (file-directory-p (cadr el)))) ; symlink to a directory
	      (setq dirs (cons el dirs))
	    (setq files (cons el files)))
	  (setq file-alist (cdr file-alist)))
	(setq file-alist
	      (if (memq ?U switches)	; unsorted order is reversed
		  (nconc dirs files)
		(nconc files dirs)
		))))
  ;; Finally reverse file alist if necessary.
  ;; (eq below MUST compare `(not (memq ...))' to force comparison of
  ;; `t' or `nil', rather than list tails!)
  (if (eq (eq (not (memq ?U switches))	; unsorted order is reversed
	      (not (memq ?r switches)))	; reversed sort order requested
	  ls-lisp-dirs-first)		; already reversed
      (nreverse file-alist)
    file-alist))

(defun ls-lisp-classify-file (filename fattr)
  "Append a character to FILENAME indicating the file type.

FATTR is the file attributes returned by `file-attributes' for the file.
The file type indicators are `/' for directories, `@' for symbolic
links, `|' for FIFOs, `=' for sockets, `*' for regular files that
are executable, and nothing for other types of files."
  (let* ((type (car fattr))
	 (modestr (nth 8 fattr))
	 (typestr (substring modestr 0 1)))
    (cond
     (type
      (concat filename (if (eq type t) "/" "@")))
     ((string-match "x" modestr)
      (concat filename "*"))
     ((string= "p" typestr)
      (concat filename "|"))
     ((string= "s" typestr)
      (concat filename "="))
     (t filename))))

(defun ls-lisp-classify (filedata)
  "Append a character to file name in FILEDATA indicating the file type.

FILEDATA has the form (FILENAME . ATTRIBUTES), where ATTRIBUTES is the
structure returned by `file-attributes' for that file.

The file type indicators are `/' for directories, `@' for symbolic
links, `|' for FIFOs, `=' for sockets, `*' for regular files that
are executable, and nothing for other types of files."
  (let ((file-name (car filedata))
        (fattr (cdr filedata)))
    (setq file-name (propertize file-name 'dired-filename t))
    (cons (ls-lisp-classify-file file-name fattr) fattr)))

(defun ls-lisp-extension (filename)
  "Return extension of FILENAME (ignoring any version extension)
FOLLOWED by null and full filename, SOLELY for full alpha sort."
  ;; Force extension sort order: `no ext' then `null ext' then `ext'
  ;; to agree with GNU ls.
  (concat
   (let* ((i (length filename)) end)
     (if (= (aref filename (1- i)) ?.) ; null extension
	 "\0"
       (while (and (>= (setq i (1- i)) 0)
		   (/= (aref filename i) ?.)))
       (if (< i 0) "\0\0"		; no extension
	 (if (/= (aref filename (1+ i)) ?~)
	     (substring filename (1+ i))
	   ;; version extension found -- ignore it
	   (setq end i)
	   (while (and (>= (setq i (1- i)) 0)
		       (/= (aref filename i) ?.)))
	   (if (< i 0) "\0\0"	; no extension
	     (substring filename (1+ i) end))))
       )) "\0" filename))

(defun ls-lisp-format (file-name file-attr file-size switches time-index)
  "Format one line of long ls output for file FILE-NAME.
FILE-ATTR and FILE-SIZE give the file's attributes and size.
SWITCHES and TIME-INDEX give the full switch list and time data."
  (let ((file-type (nth 0 file-attr))
	;; t for directory, string (name linked to)
	;; for symbolic link, or nil.
	(drwxrwxrwx (nth 8 file-attr)))	; attribute string ("drwxrwxrwx")
    (concat (if (memq ?i switches)	; inode number
		(let ((inode (nth 10 file-attr)))
		  (if (consp inode)
		      (if (consp (cdr inode))
			  ;; 2^(24+16) = 1099511627776.0, but
			  ;; multiplying by it and then adding the
			  ;; other members of the cons cell in one go
			  ;; loses precision, since a double does not
			  ;; have enough significant digits to hold a
			  ;; full 64-bit value.  So below we split
			  ;; 1099511627776 into high 13 and low 5
			  ;; digits and compute in two parts.
			  (let ((p1 (* (car inode) 10995116.0))
				(p2 (+ (* (car inode) 27776.0)
				       (* (cadr inode) 65536.0)
				       (cddr inode))))
			    (format " %13.0f%05.0f "
				    ;; Use floor to emulate integer
				    ;; division.
				    (+ p1 (floor p2 100000.0))
				    (mod p2 100000.0)))
			(format " %18.0f "
				(+ (* (car inode) 65536.0)
				   (cdr inode))))
		    (format " %18d " inode))))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format ls-lisp-filesize-f-fmt
			(fceiling (/ file-size 1024.0))))
	    drwxrwxrwx			; attribute string
	    (if (memq 'links ls-lisp-verbosity)
		(format "%3d" (nth 1 file-attr))) ; link count
	    ;; Numeric uid/gid are more confusing than helpful;
	    ;; Emacs should be able to make strings of them.
	    ;; They tend to be bogus on non-UNIX platforms anyway so
	    ;; optionally hide them.
	    (if (memq 'uid ls-lisp-verbosity)
		;; uid can be a string or an integer
		(let ((uid (nth 2 file-attr)))
                  (format (if (stringp uid)
			      ls-lisp-uid-s-fmt
			    ls-lisp-uid-d-fmt)
			  uid)))
	    (if (not (memq ?G switches)) ; GNU ls -- shows group by default
		(if (or (memq ?g switches) ; UNIX ls -- no group by default
			(memq 'gid ls-lisp-verbosity))
                    (let ((gid (nth 3 file-attr)))
                      (format (if (stringp gid)
				  ls-lisp-gid-s-fmt
				ls-lisp-gid-d-fmt)
			      gid))))
	    (ls-lisp-format-file-size file-size (memq ?h switches))
	    " "
	    (ls-lisp-format-time file-attr time-index)
	    " "
	    (if (not (memq ?F switches)) ; ls-lisp-classify already did that
		(propertize file-name 'dired-filename t)
	      file-name)
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type))
	    "\n"
	    )))

(defun ls-lisp-time-index (switches)
  "Return time index into file-attributes according to ls SWITCHES list.
Return nil if no time switch found."
  ;; FJW: Default of nil is IMPORTANT and used in `ls-lisp-handle-switches'!
  (cond ((memq ?c switches) 6)		; last mode change
	((memq ?t switches) 5)		; last modtime
	((memq ?u switches) 4)))	; last access

(defun ls-lisp-format-time (file-attr time-index)
  "Format time for file with attributes FILE-ATTR according to TIME-INDEX.
Use the same method as ls to decide whether to show time-of-day or year,
depending on distance between file date and the current time.
All ls time options, namely c, t and u, are handled."
  (let* ((time (nth (or time-index 5) file-attr)) ; default is last modtime
	 (diff (- (float-time time) (float-time)))
	 ;; Consider a time to be recent if it is within the past six
	 ;; months.  A Gregorian year has 365.2425 * 24 * 60 * 60 ==
	 ;; 31556952 seconds on the average, and half of that is 15778476.
	 ;; Write the constant explicitly to avoid roundoff error.
	 (past-cutoff -15778476)) ; half a Gregorian year
    (condition-case nil
	;; Use traditional time format in the C or POSIX locale,
	;; ISO-style time format otherwise, so columns line up.
	(let ((locale system-time-locale))
	  (if (not locale)
	      (let ((vars '("LC_ALL" "LC_TIME" "LANG")))
		(while (and vars (not (setq locale (getenv (car vars)))))
		  (setq vars (cdr vars)))))
	  (if (member locale '("C" "POSIX"))
	      (setq locale nil))
	  (format-time-string
	   (if (and (<= past-cutoff diff) (<= diff 0))
	       (if (and locale (not ls-lisp-use-localized-time-format))
		   "%m-%d %H:%M"
		 (nth 0 ls-lisp-format-time-list))
	     (if (and locale (not ls-lisp-use-localized-time-format))
		 "%Y-%m-%d "
	       (nth 1 ls-lisp-format-time-list)))
	   time))
      (error "Unk  0  0000"))))

(defun ls-lisp-format-file-size (file-size human-readable)
  (if (not human-readable)
      (format (if (floatp file-size)
		  ls-lisp-filesize-f-fmt
		ls-lisp-filesize-d-fmt)
	      file-size)
    (format " %7s" (file-size-human-readable file-size))))

(provide 'ls-lisp)

;;; ls-lisp.el ends here
