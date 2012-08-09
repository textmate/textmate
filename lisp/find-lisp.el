;;; find-lisp.el --- emulation of find in Emacs Lisp

;; Author: Peter Breton
;; Created: Fri Mar 26 1999
;; Keywords: unix

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

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
;; This is a very generalized form of find; it basically implements a
;; recursive directory descent. The conditions which bound the search
;; are expressed as predicates, and I have not addressed the question
;; of how to wrap up the common chores that find does in a simpler
;; format than writing code for all the various predicates.
;;
;; Some random thoughts are to express simple queries directly with
;; user-level functions, and perhaps use some kind of forms interface
;; for medium-level queries. Really complicated queries can be
;; expressed in Lisp.
;;

;;; Todo
;;
;; It would be nice if we could sort the results without running the find
;; again. Maybe that could work by storing the original file attributes?

;;; Code:

(require 'dired)

(defvar dired-buffers)
(defvar dired-subdir-alist)

;; Internal variables

(defvar find-lisp-regexp nil
  "Internal variable.")

(defconst find-lisp-line-indent "  "
  "Indentation for dired file lines.")

(defvar find-lisp-file-predicate nil
  "Predicate for choosing to include files.")

(defvar find-lisp-directory-predicate nil
  "Predicate for choosing to descend into directories.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar find-lisp-debug-buffer "*Find Lisp Debug*"
  "Buffer for debugging information.")

(defvar find-lisp-debug nil
  "Whether debugging is enabled.")

(defun find-lisp-debug-message (message)
  "Print a debug message MESSAGE in `find-lisp-debug-buffer'."
  (set-buffer (get-buffer-create find-lisp-debug-buffer))
  (goto-char (point-max))
  (insert message "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory and File predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-lisp-default-directory-predicate  (dir parent)
  "True if DIR is not a dot file, and not a symlink.
PARENT is the parent directory of DIR."
  (and find-lisp-debug
       (find-lisp-debug-message
	(format "Processing directory %s in %s" dir parent)))
  ;; Skip current and parent directories
  (not (or (string= dir ".")
	   (string= dir "..")
	   ;; Skip directories which are symlinks
	   ;; Easy way to circumvent recursive loops
	   (file-symlink-p (expand-file-name dir parent)))))

(defun find-lisp-default-file-predicate  (file dir)
  "True if FILE matches `find-lisp-regexp'.
DIR is the directory containing FILE."
  (and find-lisp-debug
       (find-lisp-debug-message
	(format "Processing file %s in %s" file dir)))
  (and (not (file-directory-p (expand-file-name file dir)))
       (string-match find-lisp-regexp file)))

(defun find-lisp-file-predicate-is-directory  (file dir)
  "True if FILE is a directory.
Argument DIR is the directory containing FILE."
  (and find-lisp-debug
       (find-lisp-debug-message
	(format "Processing file %s in %s" file dir)))
  (and (file-directory-p (expand-file-name file dir))
       (not (or (string= file ".")
		(string= file "..")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-lisp-find-files (directory regexp)
  "Find files in DIRECTORY which match REGEXP."
  (let ((file-predicate      'find-lisp-default-file-predicate)
	(directory-predicate 'find-lisp-default-directory-predicate)
	(find-lisp-regexp regexp))
    (find-lisp-find-files-internal
     directory
     file-predicate
     directory-predicate)))

;; Workhorse function
(defun find-lisp-find-files-internal (directory file-predicate
						directory-predicate)
  "Find files under DIRECTORY which satisfy FILE-PREDICATE.
FILE-PREDICATE is a function which takes two arguments: the file and its
directory.

DIRECTORY-PREDICATE is used to decide whether to descend into directories.
It is a function which takes two arguments, the directory and its parent."
  (setq directory (file-name-as-directory directory))
  (let (results sub-results)
    (dolist (file (directory-files directory nil nil t))
      (let ((fullname (expand-file-name file directory)))
	(when (file-readable-p (expand-file-name file directory))
	  ;; If a directory, check it we should descend into it
	  (and (file-directory-p fullname)
	       (funcall directory-predicate file directory)
	       (progn
		 (setq sub-results
		       (find-lisp-find-files-internal
			fullname
			file-predicate
			directory-predicate))
		 (if results
		     (nconc results sub-results)
		   (setq results sub-results))))
	  ;; For all files and directories, call the file predicate
	  (and (funcall file-predicate file directory)
	       (if results
		   (nconc results (list fullname))
		 (setq results (list fullname)))))))
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find-dired all in Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun find-lisp-find-dired (dir regexp)
  "Find files in DIR, matching REGEXP."
  (interactive "DFind files in directory: \nsMatching regexp: ")
  (let ((find-lisp-regexp regexp))
    (find-lisp-find-dired-internal
     dir
     'find-lisp-default-file-predicate
     'find-lisp-default-directory-predicate
     "*Find Lisp Dired*")))

;; Just the subdirectories
;;;###autoload
(defun find-lisp-find-dired-subdirectories (dir)
  "Find all subdirectories of DIR."
  (interactive "DFind subdirectories of directory: ")
  (find-lisp-find-dired-internal
   dir
   'find-lisp-file-predicate-is-directory
   'find-lisp-default-directory-predicate
   "*Find Lisp Dired Subdirectories*"))

;; Most of this is lifted from find-dired.el
;;
(defun find-lisp-find-dired-internal (dir file-predicate
					  directory-predicate buffer-name)
  "Run find (Lisp version) and go into Dired mode on a buffer of the output."
  (let ((dired-buffers dired-buffers)
	(regexp find-lisp-regexp))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
	(error "find-dired needs a directory: %s" dir))
    (or
     (and (buffer-name)
	  (string= buffer-name (buffer-name)))
	(switch-to-buffer (get-buffer-create buffer-name)))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir)
    (dired-mode dir)

    (use-local-map (append (make-sparse-keymap) (current-local-map)))

    (make-local-variable 'find-lisp-file-predicate)
    (setq find-lisp-file-predicate file-predicate)
    (make-local-variable 'find-lisp-directory-predicate)
    (setq find-lisp-directory-predicate directory-predicate)
    (make-local-variable 'find-lisp-regexp)
    (setq find-lisp-regexp regexp)

    (make-local-variable 'revert-buffer-function)
    (setq revert-buffer-function
	  (function
	   (lambda (_ignore1 _ignore2)
	     (find-lisp-insert-directory
	      default-directory
	      find-lisp-file-predicate
	      find-lisp-directory-predicate
	      'ignore)
	     )
	   ))

    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))
    (find-lisp-insert-directory
     dir file-predicate directory-predicate 'ignore)
    (goto-char (point-min))
    (dired-goto-next-file)))

(defun find-lisp-insert-directory (dir
                                   file-predicate
                                   directory-predicate
				   _sort-function)
  "Insert the results of `find-lisp-find-files' in the current buffer."
  (let ((buffer-read-only nil)
	(files (find-lisp-find-files-internal
		dir
		file-predicate
		directory-predicate))
	(len (length dir)))
    (erase-buffer)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert find-lisp-line-indent dir ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    ;;
    ;; No analog for find-lisp?
    (insert find-lisp-line-indent "\n")
    ;; Run the find function
    (mapc
     (function
      (lambda (file)
	(find-lisp-find-dired-insert-file
	 (substring file len)
	 (current-buffer))))
     (sort files 'string-lessp))
    ;; FIXME: Sort function is ignored for now
    ;; (funcall sort-function files))
    (goto-char (point-min))
    (dired-goto-next-file)))

;;;###autoload
(defun find-lisp-find-dired-filter (regexp)
  "Change the filter on a find-lisp-find-dired buffer to REGEXP."
  (interactive "sSet filter to regexp: ")
  (setq find-lisp-regexp regexp)
  (revert-buffer))

(defun find-lisp-find-dired-insert-file (file buffer)
  (set-buffer buffer)
  (insert find-lisp-line-indent
	  (find-lisp-format file (file-attributes file 'string) (list "")
			  (current-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lifted from ls-lisp. We don't want to require it, because that
;; would alter the insert-directory function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-lisp-format (file-name file-attr switches now)
  (let ((file-type (nth 0 file-attr)))
    (concat (if (memq ?i switches)	; inode number
		(format "%6d " (nth 10 file-attr)))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format "%4d " (1+ (/ (nth 7 file-attr) 1024))))
	    (nth 8 file-attr)		; permission bits
	    (format " %3d %-8s %-8s %8d "
		    (nth 1 file-attr)	; no. of links
		    (if (numberp (nth 2 file-attr))
			(int-to-string (nth 2 file-attr))
		      (nth 2 file-attr)) ; uid
		    (if (eq system-type 'ms-dos)
			"root"		; everything is root on MSDOS.
		      (if (numberp (nth 3 file-attr))
			  (int-to-string (nth 3 file-attr))
			(nth 3 file-attr))) ; gid
		    (nth 7 file-attr)	; size in bytes
		    )
	    (find-lisp-format-time file-attr switches now)
	    " "
	    file-name
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type)
	      "")
	    "\n")))

(defun find-lisp-time-index (switches)
  ;; Return index into file-attributes according to ls SWITCHES.
  (cond
   ((memq ?c switches) 6)		; last mode change
   ((memq ?u switches) 4)		; last access
   ;; default is last modtime
   (t 5)))

(defun find-lisp-format-time (file-attr switches now)
  ;; Format time string for file with attributes FILE-ATTR according
  ;; to SWITCHES (a list of ls option letters of which c and u are recognized).
  ;; Use the same method as `ls' to decide whether to show time-of-day or year,
  ;; depending on distance between file date and NOW.
  (let* ((time (nth (find-lisp-time-index switches) file-attr))
	 (diff16 (- (car time) (car now)))
	 (diff (+ (ash diff16 16) (- (car (cdr time)) (car (cdr now)))))
	 (past-cutoff (- (* 6 30 24 60 60)))	; 6 30-day months
	 (future-cutoff (* 60 60)))		; 1 hour
    (format-time-string
     (if (and
	  (<= past-cutoff diff) (<= diff future-cutoff)
	  ;; Sanity check in case `diff' computation overflowed.
	  (<= (1- (ash past-cutoff -16)) diff16)
	  (<= diff16 (1+ (ash future-cutoff -16))))
	 "%b %e %H:%M"
       "%b %e  %Y")
     time)))

(provide 'find-lisp)

;;; find-lisp.el ends here
