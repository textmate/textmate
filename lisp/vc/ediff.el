;;; ediff.el --- a comprehensive visual interface to diff & patch

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Created: February 2, 1994
;; Keywords: comparing, merging, patching, vc, tools, unix
;; Version: 2.81.4

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file on 20/3/2008, and the maintainer agreed that when a bug is
;; filed in the Emacs bug reporting system against this file, a copy
;; of the bug report be sent to the maintainer's email address.

(defconst ediff-version "2.81.4" "The current version of Ediff")
(defconst ediff-date "December 7, 2009" "Date of last update")


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

;; Never read that diff output again!
;; Apply patch interactively!
;; Merge with ease!

;; This package provides a convenient way of simultaneous browsing through
;; the differences between a pair (or a triple) of files or buffers.  The
;; files being compared, file-A, file-B, and file-C (if applicable) are
;; shown in separate windows (side by side, one above the another, or in
;; separate frames), and the differences are highlighted as you step
;; through them.  You can also copy difference regions from one buffer to
;; another (and recover old differences if you change your mind).

;; Ediff also supports merging operations on files and buffers, including
;; merging using ancestor versions.  Both comparison and merging operations can
;; be performed on directories, i.e., by pairwise comparison of files in those
;; directories.

;; In addition, Ediff can apply a patch to a file and then let you step
;; though both files, the patched and the original one, simultaneously,
;; difference-by-difference.  You can even apply a patch right out of a
;; mail buffer, i.e., patches received by mail don't even have to be saved.
;; Since Ediff lets you copy differences between buffers, you can, in
;; effect, apply patches selectively (i.e., you can copy a difference
;; region from file_orig to file, thereby undoing any particular patch that
;; you don't like).

;; Ediff is aware of version control, which lets the user compare
;; files with their older versions.  Ediff can also work with remote and
;; compressed files.  Details are given below.

;; Finally, Ediff supports directory-level comparison, merging and patching.
;; See the on-line manual for details.

;; This package builds upon the ideas borrowed from emerge.el and several
;; Ediff's functions are adaptations from emerge.el.  Much of the functionality
;; Ediff provides is also influenced by emerge.el.

;; The present version of Ediff supersedes Emerge.  It provides a superior user
;; interface and has numerous major features not found in Emerge.  In
;; particular, it can do patching, and 2-way and 3-way file comparison,
;; merging, and directory operations.



;;; Bugs:

;;  1. The undo command doesn't restore deleted regions well.  That is, if
;;  you delete all characters in a difference region and then invoke
;;  `undo', the reinstated text will most likely be inserted outside of
;;  what Ediff thinks is the current difference region. (This problem
;;  doesn't seem to exist with XEmacs.)
;;
;;  If at any point you feel that difference regions are no longer correct,
;;  you can hit '!' to recompute the differences.

;;  2. On a monochrome display, the repertoire of faces with which to
;;  highlight fine differences is limited.  By default, Ediff is using
;;  underlining.  However, if the region is already underlined by some other
;;  overlays, there is no simple way to temporarily remove that residual
;;  underlining.  This problem occurs when a buffer is highlighted with
;;  hilit19.el or font-lock.el packages.  If this residual highlighting gets
;;  in the way, you can do the following.  Both font-lock.el and hilit19.el
;;  provide commands for unhighlighting buffers.  You can either place these
;;  commands in `ediff-prepare-buffer-hook' (which will unhighlight every
;;  buffer used by Ediff) or you can execute them interactively, at any time
;;  and on any buffer.


;;; Acknowledgements:

;; Ediff was inspired by Dale R. Worley's <drw@math.mit.edu> emerge.el.
;; Ediff would not have been possible without the help and encouragement of
;; its many users.  See Ediff on-line Info for the full list of those who
;; helped.  Improved defaults in Ediff file-name reading commands.

;;; Code:

(provide 'ediff)

;; Compiler pacifier
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest  r))))


(eval-when-compile
  (require 'dired)
  (require 'ediff-util)
  (require 'ediff-ptch))
;; end pacifier

(require 'ediff-init)
(require 'ediff-mult)  ; required because of the registry stuff

(defgroup ediff nil
  "A comprehensive visual interface to diff & patch."
  :tag "Ediff"
  :group 'tools)


(defcustom ediff-use-last-dir nil
  "If t, Ediff will use previous directory as default when reading file name."
  :type 'boolean
  :group 'ediff)

;; Last directory used by an Ediff command for file-A.
(defvar ediff-last-dir-A nil)
;; Last directory used by an Ediff command for file-B.
(defvar ediff-last-dir-B nil)
;; Last directory used by an Ediff command for file-C.
(defvar ediff-last-dir-C nil)
;; Last directory used by an Ediff command for the ancestor file.
(defvar ediff-last-dir-ancestor nil)
;; Last directory used by an Ediff command as the output directory for merge.
(defvar ediff-last-merge-autostore-dir nil)


;; Used as a startup hook to set `_orig' patch file read-only.
(defun ediff-set-read-only-in-buf-A ()
  (ediff-with-current-buffer ediff-buffer-A
    (toggle-read-only 1)))

;; Return a plausible default for ediff's first file:
;; In dired, return the file number FILENO (or 0) in the list
;; (all-selected-files, filename under the cursor), where directories are
;; ignored. Otherwise, return DEFAULT file name, if non-nil. Else,
;; if the buffer is visiting a file, return that file name.
(defun ediff-get-default-file-name (&optional default fileno)
  (cond ((eq major-mode 'dired-mode)
	 (let ((current (dired-get-filename nil 'no-error))
	       (marked (condition-case nil
			   (dired-get-marked-files 'no-dir)
			 (error nil)))
	       aux-list choices result)
	   (or (integerp fileno) (setq fileno 0))
	   (if (stringp default)
	       (setq aux-list (cons default aux-list)))
	   (if (and (stringp current) (not (file-directory-p current)))
	       (setq aux-list (cons current aux-list)))
	   (setq choices (nconc  marked aux-list))
	   (setq result (elt choices fileno))
	   (or result
	       default)))
	((stringp default) default)
	((buffer-file-name (current-buffer))
	 (file-name-nondirectory (buffer-file-name (current-buffer))))
	))

;;; Compare files/buffers

;;;###autoload
(defun ediff-files (file-A file-B &optional startup-hooks)
  "Run Ediff on a pair of files, FILE-A and FILE-B."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B f)
     (list (setq f (ediff-read-file-name
		    "File A to compare"
		    dir-A
		    (ediff-get-default-file-name)
		    'no-dirs))
	   (ediff-read-file-name "File B to compare"
				 (setq dir-B
				       (if ediff-use-last-dir
					   ediff-last-dir-B
					 (file-name-directory f)))
				 (progn
				   (ediff-add-to-history
				    'file-name-history
				    (ediff-abbreviate-file-name
				     (expand-file-name
				      (file-name-nondirectory f)
				      dir-B)))
				   (ediff-get-default-file-name f 1)))
	   )))
  (ediff-files-internal file-A
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			nil ; file-C
			startup-hooks
			'ediff-files))

;;;###autoload
(defun ediff-files3 (file-A file-B file-C &optional startup-hooks)
  "Run Ediff on three files, FILE-A, FILE-B, and FILE-C."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B dir-C f ff)
     (list (setq f (ediff-read-file-name
		    "File A to compare"
		    dir-A
		    (ediff-get-default-file-name)
		    'no-dirs))
	   (setq ff (ediff-read-file-name "File B to compare"
					  (setq dir-B
						(if ediff-use-last-dir
						    ediff-last-dir-B
						  (file-name-directory f)))
					  (progn
					    (ediff-add-to-history
					     'file-name-history
					     (ediff-abbreviate-file-name
					      (expand-file-name
					       (file-name-nondirectory f)
					       dir-B)))
					    (ediff-get-default-file-name f 1))))
	   (ediff-read-file-name "File C to compare"
				 (setq dir-C (if ediff-use-last-dir
						 ediff-last-dir-C
					       (file-name-directory ff)))
				 (progn
				   (ediff-add-to-history
				    'file-name-history
				    (ediff-abbreviate-file-name
				     (expand-file-name
				      (file-name-nondirectory ff)
				      dir-C)))
				   (ediff-get-default-file-name ff 2)))
	   )))
  (ediff-files-internal file-A
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			(if (file-directory-p file-C)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-C)
			  file-C)
			startup-hooks
			'ediff-files3))

;;;###autoload
(defalias 'ediff3 'ediff-files3)


(defun ediff-find-file (file-var buffer-name &optional last-dir hooks-var)
  "Visit FILE and arrange its buffer to Ediff's liking.
FILE-VAR is actually a variable symbol whose value must contain a true
file name.
BUFFER-NAME is a variable symbol, which will get the buffer object into
which FILE is read.
LAST-DIR is the directory variable symbol where FILE's
directory name should be returned.  HOOKS-VAR is a variable symbol that will
be assigned the hook to be executed after `ediff-startup' is finished.
`ediff-find-file' arranges that the temp files it might create will be
deleted."
  (let* ((file (symbol-value file-var))
	 (file-magic (ediff-filename-magic-p file))
	 (temp-file-name-prefix (file-name-nondirectory file)))
    (cond ((not (file-readable-p file))
	   (error "File `%s' does not exist or is not readable" file))
	  ((file-directory-p file)
	   (error "File `%s' is a directory" file)))

    ;; some of the commands, below, require full file name
    (setq file (expand-file-name file))

    ;; Record the directory of the file
    (if last-dir
	(set last-dir (expand-file-name (file-name-directory file))))

    ;; Setup the buffer
    (set buffer-name (find-file-noselect file))

    (ediff-with-current-buffer (symbol-value buffer-name)
      (widen) ; Make sure the entire file is seen
      (cond (file-magic  ;   file has a handler, such as jka-compr-handler or
	     		 ;;; ange-ftp-hook-function--arrange for temp file
	     (ediff-verify-file-buffer 'magic)
	     (setq file
		   (ediff-make-temp-file
		    (current-buffer) temp-file-name-prefix))
	     (set hooks-var (cons `(lambda () (delete-file ,file))
				  (symbol-value hooks-var))))
	    ;; file processed via auto-mode-alist, a la uncompress.el
	    ((not (equal (file-truename file)
			 (file-truename (buffer-file-name))))
	     (setq file
		   (ediff-make-temp-file
		    (current-buffer) temp-file-name-prefix))
	     (set hooks-var (cons `(lambda () (delete-file ,file))
				  (symbol-value hooks-var))))
	    (t ;; plain file---just check that the file matches the buffer
	     (ediff-verify-file-buffer))))
    (set file-var file)))

;; MERGE-BUFFER-FILE is the file to be associated with the merge buffer
(defun ediff-files-internal (file-A file-B file-C startup-hooks job-name
				    &optional merge-buffer-file)
  (let (buf-A buf-B buf-C)
    (if (string= file-A file-B)
	(error "Files A and B are the same"))
    (if (stringp file-C)
	(or (and (string= file-A file-C) (error "Files A and C are the same"))
	    (and (string= file-B file-C) (error "Files B and C are the same"))))
    (message "Reading file %s ... " file-A)
    ;;(sit-for 0)
    (ediff-find-file 'file-A 'buf-A 'ediff-last-dir-A 'startup-hooks)
    (message "Reading file %s ... " file-B)
    ;;(sit-for 0)
    (ediff-find-file 'file-B 'buf-B 'ediff-last-dir-B 'startup-hooks)
    (if (stringp file-C)
	(progn
	  (message "Reading file %s ... " file-C)
	  ;;(sit-for 0)
	  (ediff-find-file
	   'file-C 'buf-C
	   (if (eq job-name 'ediff-merge-files-with-ancestor)
	       'ediff-last-dir-ancestor 'ediff-last-dir-C)
	   'startup-hooks)))
    (ediff-setup buf-A file-A
		 buf-B file-B
		 buf-C file-C
		 startup-hooks
		 (list (cons 'ediff-job-name job-name))
		 merge-buffer-file)))

(declare-function diff-latest-backup-file "diff" (fn))

;;;###autoload
(defalias 'ediff 'ediff-files)

;;;###autoload
(defun ediff-current-file ()
  "Start ediff between current buffer and its file on disk.
This command can be used instead of `revert-buffer'.  If there is
nothing to revert then this command fails."
  (interactive)
  (unless (or revert-buffer-function
              revert-buffer-insert-file-contents-function
              (and buffer-file-number
                   (or (buffer-modified-p)
                       (not (verify-visited-file-modtime
                             (current-buffer))))))
    (error "Nothing to revert"))
  (let* ((auto-save-p (and (recent-auto-save-p)
                           buffer-auto-save-file-name
                           (file-readable-p buffer-auto-save-file-name)
                           (y-or-n-p
                            "Buffer has been auto-saved recently.  Compare with auto-save file? ")))
         (file-name (if auto-save-p
                        buffer-auto-save-file-name
                      buffer-file-name))
         (revert-buf-name (concat "FILE=" file-name))
         (revert-buf (get-buffer revert-buf-name))
         (current-major major-mode))
    (unless file-name
      (error "Buffer does not seem to be associated with any file"))
    (when revert-buf
      (kill-buffer revert-buf)
      (setq revert-buf nil))
    (setq revert-buf (get-buffer-create revert-buf-name))
    (with-current-buffer revert-buf
      (insert-file-contents file-name)
      ;; Assume same modes:
      (funcall current-major))
    (ediff-buffers revert-buf (current-buffer))))


;;;###autoload
(defun ediff-backup (file)
  "Run Ediff on FILE and its backup file.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, `ediff' it with its original."
  (interactive (list (read-file-name "Ediff (file with backup): ")))
  ;; The code is taken from `diff-backup'.
  (require 'diff)
  (let (bak ori)
    (if (backup-file-name-p file)
	(setq bak file
	      ori (file-name-sans-versions file))
      (setq bak (or (diff-latest-backup-file file)
		    (error "No backup found for %s" file))
	    ori file))
    (ediff-files bak ori)))

;;;###autoload
(defun ediff-buffers (buffer-A buffer-B &optional startup-hooks job-name)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to compare: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (or job-name (setq job-name 'ediff-buffers))
  (ediff-buffers-internal buffer-A buffer-B nil startup-hooks job-name))

;;;###autoload
(defalias 'ebuffers 'ediff-buffers)


;;;###autoload
(defun ediff-buffers3 (buffer-A buffer-B buffer-C
				 &optional startup-hooks job-name)
  "Run Ediff on three buffers, BUFFER-A, BUFFER-B, and BUFFER-C."
  (interactive
   (let (bf bff)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (setq bff (read-buffer "Buffer B to compare: "
				  (progn
				    ;; realign buffers so that two visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer bf))
				  t))
	   (read-buffer "Buffer C to compare: "
				  (progn
				    ;; realign buffers so that three visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer (list bf bff)))
				  t)
	   )))
  (or job-name (setq job-name 'ediff-buffers3))
  (ediff-buffers-internal buffer-A buffer-B buffer-C startup-hooks job-name))

;;;###autoload
(defalias 'ebuffers3 'ediff-buffers3)



;; MERGE-BUFFER-FILE is the file to be associated with the merge buffer
(defun ediff-buffers-internal (buf-A buf-B buf-C startup-hooks job-name
				     &optional merge-buffer-file)
  (let* ((buf-A-file-name (buffer-file-name (get-buffer buf-A)))
	 (buf-B-file-name (buffer-file-name (get-buffer buf-B)))
	 (buf-C-is-alive (ediff-buffer-live-p buf-C))
	 (buf-C-file-name (if buf-C-is-alive
			      (buffer-file-name (get-buffer buf-B))))
	 file-A file-B file-C)
    (unwind-protect
	(progn
	  (if (not (ediff-buffer-live-p buf-A))
	      (error "Buffer %S doesn't exist" buf-A))
	  (if (not (ediff-buffer-live-p buf-B))
	      (error "Buffer %S doesn't exist" buf-B))
	  (let ((ediff-job-name job-name))
	    (if (and ediff-3way-comparison-job
		     (not buf-C-is-alive))
		(error "Buffer %S doesn't exist" buf-C)))
	  (if (stringp buf-A-file-name)
	      (setq buf-A-file-name (file-name-nondirectory buf-A-file-name)))
	  (if (stringp buf-B-file-name)
	      (setq buf-B-file-name (file-name-nondirectory buf-B-file-name)))
	  (if (stringp buf-C-file-name)
	      (setq buf-C-file-name (file-name-nondirectory buf-C-file-name)))
	  
	  (setq file-A (ediff-make-temp-file buf-A buf-A-file-name)
		file-B (ediff-make-temp-file buf-B buf-B-file-name))
	  (if buf-C-is-alive
	      (setq file-C (ediff-make-temp-file buf-C buf-C-file-name)))
	  
	  (ediff-setup (get-buffer buf-A) file-A
		       (get-buffer buf-B) file-B
		       (if buf-C-is-alive (get-buffer buf-C))
		       file-C
		       (cons `(lambda ()
				(delete-file ,file-A)
				(delete-file ,file-B)
				(if (stringp ,file-C) (delete-file ,file-C)))
			     startup-hooks)
		       (list (cons 'ediff-job-name job-name))
		       merge-buffer-file))
      (if (and (stringp file-A) (file-exists-p file-A))
	  (delete-file file-A))
      (if (and (stringp file-B) (file-exists-p file-B))
	  (delete-file file-B))
      (if (and (stringp file-C) (file-exists-p file-C))
	  (delete-file file-C)))))


;;; Directory and file group operations

;; Get appropriate default name for directory:
;; If ediff-use-last-dir, use ediff-last-dir-A.
;; In dired mode, use the directory that is under the point (if any);
;; otherwise, use default-directory
(defun ediff-get-default-directory-name ()
  (cond (ediff-use-last-dir ediff-last-dir-A)
	((eq major-mode 'dired-mode)
	 (let ((f (dired-get-filename nil 'noerror)))
	   (if (and (stringp f) (file-directory-p f))
	       f
	     default-directory)))
	(t default-directory)))


;;;###autoload
(defun ediff-directories (dir1 dir2 regexp)
  "Run Ediff on a pair of directories, DIR1 and DIR2, comparing files that have
the same name in both.  The third argument, REGEXP, is nil or a regular
expression; only file names that match the regexp are considered."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 f)
     (list (setq f (read-directory-name
		    "Directory A to compare:" dir-A nil 'must-match))
	   (read-directory-name "Directory B to compare:"
			   (if ediff-use-last-dir
			       ediff-last-dir-B
			     (ediff-strip-last-dir f))
			   nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directories-internal
   dir1 dir2 nil regexp 'ediff-files 'ediff-directories
   ))

;;;###autoload
(defalias 'edirs 'ediff-directories)


;;;###autoload
(defun ediff-directory-revisions (dir1 regexp)
  "Run Ediff on a directory, DIR1, comparing its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names.  Only the files that are under revision control are taken into account."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 )
     (list (read-directory-name
	    "Directory to compare with revision:" dir-A nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directory-revisions-internal
   dir1 regexp 'ediff-revision 'ediff-directory-revisions
   ))

;;;###autoload
(defalias 'edir-revisions 'ediff-directory-revisions)


;;;###autoload
(defun ediff-directories3 (dir1 dir2 dir3 regexp)
  "Run Ediff on three directories, DIR1, DIR2, and DIR3, comparing files that
have the same name in all three.  The last argument, REGEXP, is nil or a
regular expression; only file names that match the regexp are considered."

  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 f)
     (list (setq f (read-directory-name "Directory A to compare:" dir-A nil))
	   (setq f (read-directory-name "Directory B to compare:"
				   (if ediff-use-last-dir
				       ediff-last-dir-B
				     (ediff-strip-last-dir f))
				   nil 'must-match))
	   (read-directory-name "Directory C to compare:"
			   (if ediff-use-last-dir
			       ediff-last-dir-C
			     (ediff-strip-last-dir f))
			   nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directories-internal
   dir1 dir2 dir3 regexp 'ediff-files3 'ediff-directories3
   ))

;;;###autoload
(defalias 'edirs3 'ediff-directories3)

;;;###autoload
(defun ediff-merge-directories (dir1 dir2 regexp &optional merge-autostore-dir)
  "Run Ediff on a pair of directories, DIR1 and DIR2, merging files that have
the same name in both.  The third argument, REGEXP, is nil or a regular
expression; only file names that match the regexp are considered."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 f)
     (list (setq f (read-directory-name "Directory A to merge:"
					dir-A nil 'must-match))
	   (read-directory-name "Directory B to merge:"
			   (if ediff-use-last-dir
			       ediff-last-dir-B
			     (ediff-strip-last-dir f))
			   nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directories-internal
   dir1 dir2 nil regexp 'ediff-merge-files 'ediff-merge-directories
   nil merge-autostore-dir
   ))

;;;###autoload
(defalias 'edirs-merge 'ediff-merge-directories)

;;;###autoload
(defun ediff-merge-directories-with-ancestor (dir1 dir2 ancestor-dir regexp
						   &optional
						   merge-autostore-dir)
  "Merge files in directories DIR1 and DIR2 using files in ANCESTOR-DIR as ancestors.
Ediff merges files that have identical names in DIR1, DIR2.  If a pair of files
in DIR1 and DIR2 doesn't have an ancestor in ANCESTOR-DIR, Ediff will merge
without ancestor.  The fourth argument, REGEXP, is nil or a regular expression;
only file names that match the regexp are considered."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 f)
     (list (setq f (read-directory-name "Directory A to merge:" dir-A nil))
	   (setq f (read-directory-name "Directory B to merge:"
				 (if ediff-use-last-dir
				     ediff-last-dir-B
				   (ediff-strip-last-dir f))
				 nil 'must-match))
	   (read-directory-name "Ancestor directory:"
				 (if ediff-use-last-dir
				     ediff-last-dir-C
				   (ediff-strip-last-dir f))
				 nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directories-internal
   dir1 dir2 ancestor-dir regexp
   'ediff-merge-files-with-ancestor 'ediff-merge-directories-with-ancestor
   nil merge-autostore-dir
   ))

;;;###autoload
(defun ediff-merge-directory-revisions (dir1 regexp
					     &optional merge-autostore-dir)
  "Run Ediff on a directory, DIR1, merging its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names.  Only the files that are under revision control are taken into account."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 )
     (list (read-directory-name
	    "Directory to merge with revisions:" dir-A nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directory-revisions-internal
   dir1 regexp 'ediff-merge-revisions 'ediff-merge-directory-revisions
   nil merge-autostore-dir
   ))

;;;###autoload
(defalias 'edir-merge-revisions 'ediff-merge-directory-revisions)

;;;###autoload
(defun ediff-merge-directory-revisions-with-ancestor (dir1 regexp
							   &optional
							   merge-autostore-dir)
  "Run Ediff on a directory, DIR1, merging its files with their revisions and ancestors.
The second argument, REGEXP, is a regular expression that filters the file
names.  Only the files that are under revision control are taken into account."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 (default-regexp (eval ediff-default-filtering-regexp))
	 )
     (list (read-directory-name
	    "Directory to merge with revisions and ancestors:"
	    dir-A nil 'must-match)
	   (read-string
	    (if (stringp default-regexp)
		(format "Filter through regular expression (default %s): "
			 default-regexp)
	      "Filter through regular expression: ")
	    nil
	    'ediff-filtering-regexp-history
	    (eval ediff-default-filtering-regexp))
	   )))
  (ediff-directory-revisions-internal
   dir1 regexp 'ediff-merge-revisions-with-ancestor
   'ediff-merge-directory-revisions-with-ancestor
   nil merge-autostore-dir
   ))

;;;###autoload
(defalias
  'edir-merge-revisions-with-ancestor
  'ediff-merge-directory-revisions-with-ancestor)

;;;###autoload
(defalias 'edirs-merge-with-ancestor 'ediff-merge-directories-with-ancestor)

;; Run ediff-action (ediff-files, ediff-merge, ediff-merge-with-ancestors)
;; on a pair of directories (three directories, in case of ancestor).
;; The third argument, REGEXP, is nil or a regular expression;
;; only file names that match the regexp are considered.
;; JOBNAME is the symbol indicating the meta-job to be performed.
;; MERGE-AUTOSTORE-DIR is the directory in which to store merged files.
(defun ediff-directories-internal (dir1 dir2 dir3 regexp action jobname
					&optional startup-hooks
					merge-autostore-dir)
  (if (stringp dir3)
      (setq dir3 (if (file-directory-p dir3) dir3 (file-name-directory dir3))))

  (cond ((string= dir1 dir2)
	 (error "Directories A and B are the same: %s" dir1))
	((and (eq jobname 'ediff-directories3)
	      (string= dir1 dir3))
	 (error "Directories A and C are the same: %s" dir1))
	((and (eq jobname 'ediff-directories3)
	      (string= dir2 dir3))
	 (error "Directories B and C are the same: %s" dir1)))

  (if merge-autostore-dir
      (or (stringp merge-autostore-dir)
	  (error "%s: Directory for storing merged files must be a string"
		 jobname)))
  (let (;; dir-diff-struct is of the form (common-list diff-list)
	;; It is a structure where ediff-intersect-directories returns
	;; commonalities and differences among directories
	dir-diff-struct
	meta-buf)
    (if (and ediff-autostore-merges
	     (ediff-merge-metajob jobname)
	     (not merge-autostore-dir))
	(setq merge-autostore-dir
	      (read-directory-name "Save merged files in directory: "
			      (if ediff-use-last-dir
					ediff-last-merge-autostore-dir
				      (ediff-strip-last-dir dir1))
			      nil
			      'must-match)))
    ;; verify we are not merging into an orig directory
    (if merge-autostore-dir
	(cond ((and (stringp dir1) (string= merge-autostore-dir dir1))
	       (or (y-or-n-p
		    "Directory for saving merged files = Directory A.  Sure? ")
		   (error "Directory merge aborted")))
	      ((and (stringp dir2) (string= merge-autostore-dir dir2))
	       (or (y-or-n-p
		    "Directory for saving merged files = Directory B.  Sure? ")
		   (error "Directory merge aborted")))
	      ((and (stringp dir3) (string= merge-autostore-dir dir3))
	       (or (y-or-n-p
		    "Directory for saving merged files = Ancestor Directory.  Sure? ")
		   (error "Directory merge aborted")))))

    (setq dir-diff-struct (ediff-intersect-directories
			   jobname
			   regexp dir1 dir2 dir3 merge-autostore-dir))
    (setq startup-hooks
	  ;; this sets various vars in the meta buffer inside
	  ;; ediff-prepare-meta-buffer
	  (cons `(lambda ()
		   ;; tell what to do if the user clicks on a session record
		   (setq ediff-session-action-function (quote ,action))
		   ;; set ediff-dir-difference-list
		   (setq ediff-dir-difference-list
			 (cdr (quote ,dir-diff-struct))))
		startup-hooks))
    (setq meta-buf (ediff-prepare-meta-buffer
		    'ediff-filegroup-action
		    (car dir-diff-struct)
		    "*Ediff Session Group Panel"
		    'ediff-redraw-directory-group-buffer
		    jobname
		    startup-hooks))
    (ediff-show-meta-buffer meta-buf)
    ))

;; MERGE-AUTOSTORE-DIR can be given to tell ediff where to store the merged
;; files
(defun ediff-directory-revisions-internal (dir1 regexp action jobname
						&optional startup-hooks
						merge-autostore-dir)
  (setq dir1 (if (file-directory-p dir1) dir1 (file-name-directory dir1)))

  (if merge-autostore-dir
      (or (stringp merge-autostore-dir)
	  (error "%S: Directory for storing merged files must be a string"
		 jobname)))
  (let (file-list meta-buf)
    (if (and ediff-autostore-merges
	     (ediff-merge-metajob jobname)
	     (not merge-autostore-dir))
	(setq merge-autostore-dir
	      (read-directory-name "Save merged files in directory: "
			      (if ediff-use-last-dir
				  ediff-last-merge-autostore-dir
				(ediff-strip-last-dir dir1))
			      nil
			      'must-match)))
    ;; verify merge-autostore-dir != dir1
    (if (and merge-autostore-dir
	     (stringp dir1)
	     (string= merge-autostore-dir dir1))
	(or (y-or-n-p
	     "Directory for saving merged file = directory A.  Sure? ")
	    (error "Merge of directory revisions aborted")))

    (setq file-list
	  (ediff-get-directory-files-under-revision
	   jobname regexp dir1 merge-autostore-dir))
    (setq startup-hooks
	  ;; this sets various vars in the meta buffer inside
	  ;; ediff-prepare-meta-buffer
	  (cons `(lambda ()
		   ;; tell what to do if the user clicks on a session record
		   (setq ediff-session-action-function (quote ,action)))
		startup-hooks))
    (setq meta-buf (ediff-prepare-meta-buffer
		    'ediff-filegroup-action
		    file-list
		    "*Ediff Session Group Panel"
		    'ediff-redraw-directory-group-buffer
		    jobname
		    startup-hooks))
    (ediff-show-meta-buffer meta-buf)
    ))


;;; Compare regions and windows

;;;###autoload
(defun ediff-windows-wordwise (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking, wordwise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."
  (interactive "P")
  (ediff-windows dumb-mode wind-A wind-B
		 startup-hooks 'ediff-windows-wordwise 'word-mode))

;;;###autoload
(defun ediff-windows-linewise (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking, linewise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."
  (interactive "P")
  (ediff-windows dumb-mode wind-A wind-B
		 startup-hooks 'ediff-windows-linewise nil))

;; Compare WIND-A and WIND-B, which are selected by clicking.
;; With prefix argument, DUMB-MODE, or on a non-windowing display,
;; works as follows:
;; If WIND-A is nil, use selected window.
;; If WIND-B is nil, use window next to WIND-A.
(defun ediff-windows (dumb-mode wind-A wind-B startup-hooks job-name word-mode)
  (if (or dumb-mode (not (ediff-window-display-p)))
      (setq wind-A (ediff-get-next-window wind-A nil)
	    wind-B (ediff-get-next-window wind-B wind-A))
    (setq wind-A (ediff-get-window-by-clicking wind-A nil 1)
	  wind-B (ediff-get-window-by-clicking wind-B wind-A 2)))

  (let ((buffer-A (window-buffer wind-A))
	(buffer-B (window-buffer wind-B))
	beg-A end-A beg-B end-B)

    (save-excursion
      (save-window-excursion
	(sit-for 0) ; sync before using window-start/end -- a precaution
	(select-window wind-A)
	(setq beg-A (window-start)
	      end-A (window-end))
	(select-window wind-B)
	(setq beg-B (window-start)
	      end-B (window-end))))
    (setq buffer-A
	  (ediff-clone-buffer-for-window-comparison
	   buffer-A wind-A "-Window.A-")
	  buffer-B
	  (ediff-clone-buffer-for-window-comparison
	   buffer-B wind-B "-Window.B-"))
    (ediff-regions-internal
     buffer-A beg-A end-A buffer-B beg-B end-B
     startup-hooks job-name word-mode nil)))


;;;###autoload
(defun ediff-regions-wordwise (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in specified buffers.
Regions \(i.e., point and mark\) can be set in advance or marked interactively.
This function is effective only for relatively small regions, up to 200
lines.  For large regions, use `ediff-regions-linewise'."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Region's A buffer: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Region's B buffer: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (if (not (ediff-buffer-live-p buffer-A))
      (error "Buffer %S doesn't exist" buffer-A))
  (if (not (ediff-buffer-live-p buffer-B))
      (error "Buffer %S doesn't exist" buffer-B))


  (let ((buffer-A
         (ediff-clone-buffer-for-region-comparison buffer-A "-Region.A-"))
	(buffer-B
         (ediff-clone-buffer-for-region-comparison buffer-B "-Region.B-"))
        reg-A-beg reg-A-end reg-B-beg reg-B-end)
    (with-current-buffer buffer-A
      (setq reg-A-beg (region-beginning)
	    reg-A-end (region-end))
      (set-buffer buffer-B)
      (setq reg-B-beg (region-beginning)
	    reg-B-end (region-end)))

    (ediff-regions-internal
     (get-buffer buffer-A) reg-A-beg reg-A-end
     (get-buffer buffer-B) reg-B-beg reg-B-end
     startup-hooks 'ediff-regions-wordwise 'word-mode nil)))

;;;###autoload
(defun ediff-regions-linewise (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in specified buffers.
Regions \(i.e., point and mark\) can be set in advance or marked interactively.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines.  For small regions, use `ediff-regions-wordwise'."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Region A's buffer: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Region B's buffer: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (if (not (ediff-buffer-live-p buffer-A))
      (error "Buffer %S doesn't exist" buffer-A))
  (if (not (ediff-buffer-live-p buffer-B))
      (error "Buffer %S doesn't exist" buffer-B))

  (let ((buffer-A
         (ediff-clone-buffer-for-region-comparison buffer-A "-Region.A-"))
	(buffer-B
         (ediff-clone-buffer-for-region-comparison buffer-B "-Region.B-"))
        reg-A-beg reg-A-end reg-B-beg reg-B-end)
    (with-current-buffer buffer-A
      (setq reg-A-beg (region-beginning)
	    reg-A-end (region-end))
      ;; enlarge the region to hold full lines
      (goto-char reg-A-beg)
      (beginning-of-line)
      (setq reg-A-beg (point))
      (goto-char reg-A-end)
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq reg-A-end (point))

      (set-buffer buffer-B)
      (setq reg-B-beg (region-beginning)
	    reg-B-end (region-end))
      ;; enlarge the region to hold full lines
      (goto-char reg-B-beg)
      (beginning-of-line)
      (setq reg-B-beg (point))
      (goto-char reg-B-end)
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq reg-B-end (point))
      ) ; save excursion

    (ediff-regions-internal
     (get-buffer buffer-A) reg-A-beg reg-A-end
     (get-buffer buffer-B) reg-B-beg reg-B-end
     startup-hooks 'ediff-regions-linewise nil nil))) ; no word mode

;; compare region beg-A to end-A of buffer-A
;; to regions beg-B -- end-B in buffer-B.
(defun ediff-regions-internal (buffer-A beg-A end-A buffer-B beg-B end-B
					startup-hooks job-name word-mode
					setup-parameters)
  (let ((tmp-buffer (get-buffer-create ediff-tmp-buffer))
	overl-A overl-B
	file-A file-B)
    (unwind-protect
	(progn
	  ;; in case beg/end-A/B aren't markers--make them into markers
	  (ediff-with-current-buffer buffer-A
	    (setq beg-A (move-marker (make-marker) beg-A)
		  end-A (move-marker (make-marker) end-A)))
	  (ediff-with-current-buffer buffer-B
	    (setq beg-B (move-marker (make-marker) beg-B)
		  end-B (move-marker (make-marker) end-B)))
	  
	  ;; make file-A
	  (if word-mode
	      (ediff-wordify beg-A end-A buffer-A tmp-buffer)
	    (ediff-copy-to-buffer beg-A end-A buffer-A tmp-buffer))
	  (setq file-A (ediff-make-temp-file tmp-buffer "regA"))

	  ;; make file-B
	  (if word-mode
	      (ediff-wordify beg-B end-B buffer-B tmp-buffer)
	    (ediff-copy-to-buffer beg-B end-B buffer-B tmp-buffer))
	  (setq file-B (ediff-make-temp-file tmp-buffer "regB"))
	  
	  (setq overl-A (ediff-make-bullet-proof-overlay beg-A end-A buffer-A))
	  (setq overl-B (ediff-make-bullet-proof-overlay beg-B end-B buffer-B))
	  (ediff-setup buffer-A file-A
		       buffer-B file-B
		       nil nil	    ; buffer & file C
		       (cons `(lambda ()
				(delete-file ,file-A)
				(delete-file ,file-B))
			     startup-hooks)
		       (append
			(list (cons 'ediff-word-mode  word-mode)
			      (cons 'ediff-narrow-bounds (list overl-A overl-B))
			      (cons 'ediff-job-name job-name))
			setup-parameters)))
      (if (and (stringp file-A) (file-exists-p file-A))
	  (delete-file file-A))
      (if (and (stringp file-B) (file-exists-p file-B))
	  (delete-file file-B)))
    ))


;;; Merge files and buffers

;;;###autoload
(defalias 'ediff-merge 'ediff-merge-files)

(defsubst ediff-merge-on-startup ()
  (ediff-do-merge 0)
  ;; Can't remember why this is here, but it may cause the automatically merged
  ;; buffer to be lost. So, keep the buffer modified.
  ;;(ediff-with-current-buffer ediff-buffer-C
  ;;  (set-buffer-modified-p nil))
  )

;;;###autoload
(defun ediff-merge-files (file-A file-B
				 ;; MERGE-BUFFER-FILE is the file to be
				 ;; associated with the merge buffer
				 &optional startup-hooks merge-buffer-file)
  "Merge two files without ancestor."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B f)
     (list (setq f (ediff-read-file-name
		    "File A to merge"
		    dir-A
		    (ediff-get-default-file-name)
		    'no-dirs))
	   (ediff-read-file-name "File B to merge"
				 (setq dir-B
				       (if ediff-use-last-dir
					   ediff-last-dir-B
					 (file-name-directory f)))
				 (progn
				   (ediff-add-to-history
				    'file-name-history
				    (ediff-abbreviate-file-name
				     (expand-file-name
				      (file-name-nondirectory f)
				      dir-B)))
				   (ediff-get-default-file-name f 1)))
	   )))
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (ediff-files-internal file-A
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			  nil ; file-C
			  startup-hooks
			  'ediff-merge-files
			  merge-buffer-file))

;;;###autoload
(defun ediff-merge-files-with-ancestor (file-A file-B file-ancestor
					       &optional
					       startup-hooks
					       ;; MERGE-BUFFER-FILE is the file
					       ;; to be associated with the
					       ;; merge buffer
					       merge-buffer-file)
  "Merge two files with ancestor."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B dir-ancestor f ff)
     (list (setq f (ediff-read-file-name
		    "File A to merge"
		    dir-A
		    (ediff-get-default-file-name)
		    'no-dirs))
	   (setq ff (ediff-read-file-name "File B to merge"
					  (setq dir-B
						(if ediff-use-last-dir
						    ediff-last-dir-B
						  (file-name-directory f)))
					  (progn
					    (ediff-add-to-history
					     'file-name-history
					     (ediff-abbreviate-file-name
					      (expand-file-name
					       (file-name-nondirectory f)
					       dir-B)))
					    (ediff-get-default-file-name f 1))))
	   (ediff-read-file-name "Ancestor file"
				 (setq dir-ancestor
				       (if ediff-use-last-dir
					   ediff-last-dir-ancestor
					 (file-name-directory ff)))
				 (progn
				   (ediff-add-to-history
				    'file-name-history
				    (ediff-abbreviate-file-name
				     (expand-file-name
				      (file-name-nondirectory ff)
				      dir-ancestor)))
				   (ediff-get-default-file-name ff 2)))
	   )))
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (ediff-files-internal file-A
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			  file-ancestor
			  startup-hooks
			  'ediff-merge-files-with-ancestor
			  merge-buffer-file))

;;;###autoload
(defalias 'ediff-merge-with-ancestor 'ediff-merge-files-with-ancestor)

;;;###autoload
(defun ediff-merge-buffers (buffer-A buffer-B
				     &optional
				     ;; MERGE-BUFFER-FILE is the file to be
				     ;; associated with the merge buffer
				     startup-hooks job-name merge-buffer-file)
  "Merge buffers without ancestor."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to merge: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to merge: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))

  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (or job-name (setq job-name 'ediff-merge-buffers))
  (ediff-buffers-internal
   buffer-A buffer-B nil startup-hooks job-name merge-buffer-file))

;;;###autoload
(defun ediff-merge-buffers-with-ancestor (buffer-A buffer-B buffer-ancestor
						   &optional
						   startup-hooks
						   job-name
						   ;; MERGE-BUFFER-FILE is the
						   ;; file to be associated
						   ;; with the merge buffer
						   merge-buffer-file)
  "Merge buffers with ancestor."
  (interactive
   (let (bf bff)
     (list (setq bf (read-buffer "Buffer A to merge: "
				 (ediff-other-buffer "") t))
	   (setq bff (read-buffer "Buffer B to merge: "
				  (progn
				    ;; realign buffers so that two visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer bf))
				  t))
	   (read-buffer "Ancestor buffer: "
				  (progn
				    ;; realign buffers so that three visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer (list bf bff)))
				  t)
	   )))

  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (or job-name (setq job-name 'ediff-merge-buffers-with-ancestor))
  (ediff-buffers-internal
   buffer-A buffer-B buffer-ancestor startup-hooks job-name merge-buffer-file))


;;;###autoload
(defun ediff-merge-revisions (&optional file startup-hooks merge-buffer-file)
  ;; MERGE-BUFFER-FILE is the file to be associated with the merge buffer
  "Run Ediff by merging two revisions of a file.
The file is the optional FILE argument or the file visited by the current
buffer."
  (interactive)
  (if (stringp file) (find-file file))
  (let (rev1 rev2)
    (setq rev1
	  (read-string
	   (format
	    "Version 1 to merge (default %s's working version): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer")))
	  rev2
	  (read-string
	   (format
	    "Version 2 to merge (default %s): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer"))))
    (ediff-load-version-control)
    ;; ancestor-revision=nil
    (funcall
     (intern (format "ediff-%S-merge-internal" ediff-version-control-package))
     rev1 rev2 nil startup-hooks merge-buffer-file)))


;;;###autoload
(defun ediff-merge-revisions-with-ancestor (&optional
					    file startup-hooks
					    ;; MERGE-BUFFER-FILE is the file to
					    ;; be associated with the merge
					    ;; buffer
					    merge-buffer-file)
  "Run Ediff by merging two revisions of a file with a common ancestor.
The file is the optional FILE argument or the file visited by the current
buffer."
  (interactive)
  (if (stringp file) (find-file file))
  (let (rev1 rev2 ancestor-rev)
    (setq rev1
	  (read-string
	   (format
	    "Version 1 to merge (default %s's working version): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer")))
	  rev2
	  (read-string
	   (format
	    "Version 2 to merge (default %s): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer")))
	  ancestor-rev
	  (read-string
	   (format
	    "Ancestor version (default %s's base revision): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer"))))
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-merge-internal" ediff-version-control-package))
     rev1 rev2 ancestor-rev startup-hooks merge-buffer-file)))

;;; Apply patch

;;;###autoload
(defun ediff-patch-file (&optional arg patch-buf)
  "Query for a file name, and then run Ediff by patching that file.
If optional PATCH-BUF is given, use the patch in that buffer
and don't ask the user.
If prefix argument, then: if even argument, assume that the patch is in a
buffer. If odd -- assume it is in a file."
  (interactive "P")
  (let (source-dir source-file)
    (require 'ediff-ptch)
    (setq patch-buf
	  (ediff-get-patch-buffer
	   (if arg (prefix-numeric-value arg)) patch-buf))
    (setq source-dir (cond (ediff-use-last-dir ediff-last-dir-patch)
			   ((and (not ediff-patch-default-directory)
				 (buffer-file-name patch-buf))
			    (file-name-directory
			     (expand-file-name
			      (buffer-file-name patch-buf))))
			   (t default-directory)))
    (setq source-file
	  (read-file-name
	   "File to patch (directory, if multifile patch): "
	   ;; use an explicit initial file
	   source-dir nil nil (ediff-get-default-file-name)))
    (ediff-dispatch-file-patching-job patch-buf source-file)))

;;;###autoload
(defun ediff-patch-buffer (&optional arg patch-buf)
  "Run Ediff by patching the buffer specified at prompt.
Without the optional prefix ARG, asks if the patch is in some buffer and
prompts for the buffer or a file, depending on the answer.
With ARG=1, assumes the patch is in a file and prompts for the file.
With ARG=2, assumes the patch is in a buffer and prompts for the buffer.
PATCH-BUF is an optional argument, which specifies the buffer that contains the
patch. If not given, the user is prompted according to the prefix argument."
  (interactive "P")
  (require 'ediff-ptch)
  (setq patch-buf
	(ediff-get-patch-buffer
	 (if arg (prefix-numeric-value arg)) patch-buf))
  (ediff-patch-buffer-internal
   patch-buf
   (read-buffer
    "Which buffer to patch? "
    (ediff-other-buffer patch-buf))))


;;;###autoload
(defalias 'epatch 'ediff-patch-file)
;;;###autoload
(defalias 'epatch-buffer 'ediff-patch-buffer)




;;; Versions Control functions

;;;###autoload
(defun ediff-revision (&optional file startup-hooks)
  "Run Ediff by comparing versions of a file.
The file is an optional FILE argument or the file entered at the prompt.
Default: the file visited by the current buffer.
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
  ;; if buffer is non-nil, use that buffer instead of the current buffer
  (interactive "P")
  (if (not (stringp file))
    (setq file
	  (ediff-read-file-name "Compare revisions for file"
				(if ediff-use-last-dir
				    ediff-last-dir-A
				  default-directory)
				(ediff-get-default-file-name)
				'no-dirs)))
  (find-file file)
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "Buffer %s is modified. Save buffer? "
                             (buffer-name))))
      (save-buffer (current-buffer)))
  (let (rev1 rev2)
    (setq rev1
	  (read-string
	   (format "Revision 1 to compare (default %s's latest revision): "
		   (file-name-nondirectory file)))
	  rev2
	  (read-string
	   (format "Revision 2 to compare (default %s's current state): "
		   (file-name-nondirectory file))))
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 startup-hooks)
    ))


;;;###autoload
(defalias 'erevision 'ediff-revision)


;; Test if version control package is loaded and load if not
;; Is SILENT is non-nil, don't report error if package is not found.
(defun ediff-load-version-control (&optional silent)
  (require 'ediff-vers)
  (or (featurep ediff-version-control-package)
      (if (locate-library (symbol-name ediff-version-control-package))
	  (progn
	    (message "") ; kill the message from `locate-library'
	    (require ediff-version-control-package))
	(or silent
	    (error "Version control package %S.el not found.  Use vc.el instead"
		   ediff-version-control-package)))))


;;;###autoload
(defun ediff-version ()
  "Return string describing the version of Ediff.
When called interactively, displays the version."
  (interactive)
  ;; called-interactively-p - not in XEmacs
  ;; (if (called-interactively-p 'interactive)
  (if (interactive-p)
      (message "%s" (ediff-version))
    (format "Ediff %s of %s" ediff-version ediff-date)))

;; info is run first, and will autoload info.el.
(declare-function Info-goto-node "info" (nodename &optional fork))

;;;###autoload
(defun ediff-documentation (&optional node)
  "Display Ediff's manual.
With optional NODE, goes to that node."
  (interactive)
  (let ((ctl-window ediff-control-window)
	(ctl-buf ediff-control-buffer))

    (ediff-skip-unsuitable-frames)
    (condition-case nil
	(progn
	  (pop-to-buffer (get-buffer-create "*info*"))
	  (info (if (featurep 'xemacs) "ediff.info" "ediff"))
	  (if node
	      (Info-goto-node node)
	    (message "Type `i' to search for a specific topic"))
	  (raise-frame (selected-frame)))
      (error (beep 1)
	     (with-output-to-temp-buffer ediff-msg-buffer
	       (ediff-with-current-buffer standard-output
		 (fundamental-mode))
	       (princ ediff-BAD-INFO))
	     (if (window-live-p ctl-window)
		 (progn
		   (select-window ctl-window)
		   (set-window-buffer ctl-window ctl-buf)))))))


(dolist (mess '("^Errors in diff output. Diff output is in "
                "^Hmm... I don't see an Ediff command around here...$"
                "^Undocumented command! Type `G' in Ediff Control Panel to drop a note to the Ediff maintainer$"
                ": This command runs in Ediff Control Buffer only!$"
                ": Invalid op in ediff-check-version$"
                "^ediff-shrink-window-C can be used only for merging jobs$"
                "^Lost difference info on these directories$"
                "^This command is inapplicable in the present context$"
                "^This session group has no parent$"
                "^Can't hide active session, $"
                "^Ediff: something wrong--no multiple diffs buffer$"
                "^Can't make context diff for Session $"
                "^The patch buffer wasn't found$"
                "^Aborted$"
                "^This Ediff session is not part of a session group$"
                "^No active Ediff sessions or corrupted session registry$"
                "^No session info in this line$"
                "^`.*' is not an ordinary file$"
                "^Patch appears to have failed$"
                "^Recomputation of differences cancelled$"
                "^No fine differences in this mode$"
                "^Lost connection to ancestor buffer...sorry$"
                "^Not merging with ancestor$"
                "^Don't know how to toggle read-only in buffer "
                "Emacs is not running as a window application$"
                "^This command makes sense only when merging with an ancestor$"
                "^At end of the difference list$"
                "^At beginning of the difference list$"
                "^Nothing saved for diff .* in buffer "
                "^Buffer is out of sync for file "
                "^Buffer out of sync for file "
                "^Output from `diff' not found$"
                "^You forgot to specify a region in buffer "
                "^All right. Make up your mind and come back...$"
                "^Current buffer is not visiting any file$"
                "^Failed to retrieve revision: $"
                "^Can't determine display width.$"
                "^File `.*' does not exist or is not readable$"
                "^File `.*' is a directory$"
                "^Buffer .* doesn't exist$"
                "^Directories . and . are the same: "
                "^Directory merge aborted$"
                "^Merge of directory revisions aborted$"
                "^Buffer .* doesn't exist$"
                "^There is no file to merge$"
                "^Version control package .*.el not found. Use vc.el instead$"))
  (add-to-list 'debug-ignored-errors mess))


(require 'ediff-util)

(run-hooks 'ediff-load-hook)


;; Local Variables:
;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;; End:

;;; ediff.el ends here
