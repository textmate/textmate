;;; em-unix.el --- UNIX command aliases

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; This file contains implementations of several UNIX command in Emacs
;; Lisp, for several reasons:
;;
;;   1) it makes them available on all platforms where the Lisp
;;      functions used are available
;;
;;   2) it makes their functionality accessible and modified by the
;;      Lisp programmer.
;;
;;   3) it allows Eshell to refrain from having to invoke external
;;      processes for common operations.

;;; Code:

(require 'eshell)
(require 'esh-opt)
(require 'pcomplete)

;;;###autoload
(eshell-defgroup eshell-unix nil
  "This module defines many of the more common UNIX utilities as
aliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If
the user passes arguments which are too complex, or are unrecognized
by the Lisp variant, the external version will be called (if
available).  The only reason not to use them would be because they are
usually much slower.  But in several cases their tight integration
with Eshell makes them more versatile than their traditional cousins
\(such as being able to use `kill' to kill Eshell background processes
by name)."
  :tag "UNIX commands in Lisp"
  :group 'eshell-module)

(defcustom eshell-unix-load-hook nil
  "A list of functions to run when `eshell-unix' is loaded."
  :version "24.1"			; removed eshell-unix-initialize
  :type 'hook
  :group 'eshell-unix)

(defcustom eshell-plain-grep-behavior nil
  "If non-nil, standalone \"grep\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-no-grep-available (not (eshell-search-path "grep"))
  "If non-nil, no grep is available on the current machine."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-plain-diff-behavior nil
  "If non-nil, standalone \"diff\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-plain-locate-behavior (featurep 'xemacs)
  "If non-nil, standalone \"locate\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-rm-removes-directories nil
  "If non-nil, `rm' will remove directory entries.
Otherwise, `rmdir' is required."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-rm-interactive-query (= (user-uid) 0)
  "If non-nil, `rm' will query before removing anything."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-mv-interactive-query (= (user-uid) 0)
  "If non-nil, `mv' will query before overwriting anything."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-mv-overwrite-files t
  "If non-nil, `mv' will overwrite files without warning."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-cp-interactive-query (= (user-uid) 0)
  "If non-nil, `cp' will query before overwriting anything."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-cp-overwrite-files t
  "If non-nil, `cp' will overwrite files without warning."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-ln-interactive-query (= (user-uid) 0)
  "If non-nil, `ln' will query before overwriting anything."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-ln-overwrite-files nil
  "If non-nil, `ln' will overwrite files without warning."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-default-target-is-dot nil
  "If non-nil, the default destination for cp, mv or ln is `.'."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-du-prefer-over-ange nil
  "Use Eshell's du in ange-ftp remote directories.
Otherwise, Emacs will attempt to use rsh to invoke du on the remote machine."
  :type 'boolean
  :group 'eshell-unix)

;;; Functions:

(defun eshell-unix-initialize ()
  "Initialize the UNIX support/emulation code."
  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      'eshell-complete-host-reference nil t))
  (make-local-variable 'eshell-complex-commands)
  (setq eshell-complex-commands
	(append '("grep" "egrep" "fgrep" "agrep" "glimpse" "locate"
		  "cat" "time" "cp" "mv" "make" "du" "diff" "su" "sudo")
		eshell-complex-commands)))

(defalias 'eshell/date     'current-time-string)
(defalias 'eshell/basename 'file-name-nondirectory)
(defalias 'eshell/dirname  'file-name-directory)

(defvar em-interactive)
(defvar em-preview)
(defvar em-recursive)
(defvar em-verbose)

(defun eshell/man (&rest args)
  "Invoke man, flattening the arguments appropriately."
  (funcall 'man (apply 'eshell-flatten-and-stringify args)))

(put 'eshell/man 'eshell-no-numeric-conversions t)

(defun eshell/info (&rest args)
  "Run the info command in-frame with the same behavior as command-line `info', ie:
  'info'           => goes to top info window
  'info arg1'      => IF arg1 is a file, then visits arg1
  'info arg1'      => OTHERWISE goes to top info window and then menu item arg1
  'info arg1 arg2' => does action for arg1 (either visit-file or menu-item) and then menu item arg2
  etc."
  (eval-and-compile (require 'info))
  (let ((file (cond
                ((not (stringp (car args)))
                 nil)
                ((file-exists-p (expand-file-name (car args)))
                 (expand-file-name (car args)))
                ((file-exists-p (concat (expand-file-name (car args)) ".info"))
                 (concat (expand-file-name (car args)) ".info")))))

    ;; If the first arg is a file, then go to that file's Top node
    ;; Otherwise, go to the global directory
    (if file
      (progn
        (setq args (cdr args))
        (Info-find-node file "Top"))
      (Info-directory))

    ;; Treat all remaining args as menu references
    (while args
      (Info-menu (car args))
      (setq args (cdr args)))))

(defun eshell-remove-entries (path files &optional top-level)
  "From PATH, remove all of the given FILES, perhaps interactively."
  (while files
    (if (string-match "\\`\\.\\.?\\'"
		      (file-name-nondirectory (car files)))
	(if top-level
	    (eshell-error "rm: cannot remove `.' or `..'\n"))
      (if (and (file-directory-p (car files))
	       (not (file-symlink-p (car files))))
	  (progn
	    (if em-verbose
		(eshell-printn (format "rm: removing directory `%s'"
				       (car files))))
	    (unless
		(or em-preview
		    (and em-interactive
			 (not (y-or-n-p
			       (format "rm: remove directory `%s'? "
				       (car files))))))
	      (eshell-funcalln 'delete-directory (car files) t t)))
	(if em-verbose
	    (eshell-printn (format "rm: removing file `%s'"
				   (car files))))
	(unless (or em-preview
		    (and em-interactive
			 (not (y-or-n-p
			       (format "rm: remove `%s'? "
				       (car files))))))
	  (eshell-funcalln 'delete-file (car files) t))))
    (setq files (cdr files))))

(defun eshell/rm (&rest args)
  "Implementation of rm in Lisp.
This is implemented to call either `delete-file', `kill-buffer',
`kill-process', or `unintern', depending on the nature of the
argument."
  (setq args (eshell-flatten-list args))
  (eshell-eval-using-options
   "rm" args
   '((?h "help" nil nil "show this usage screen")
     (?f "force" nil force-removal "force removal")
     (?i "interactive" nil em-interactive "prompt before any removal")
     (?n "preview" nil em-preview "don't change anything on disk")
     (?r "recursive" nil em-recursive
	 "remove the contents of directories recursively")
     (?R nil nil em-recursive "(same)")
     (?v "verbose" nil em-verbose "explain what is being done")
     :preserve-args
     :external "rm"
     :show-usage
     :usage "[OPTION]... FILE...
Remove (unlink) the FILE(s).")
   (unless em-interactive
     (setq em-interactive eshell-rm-interactive-query))
   (if (and force-removal em-interactive)
       (setq em-interactive nil))
   (while args
     (let ((entry (if (stringp (car args))
		      (directory-file-name (car args))
		    (if (numberp (car args))
			(number-to-string (car args))
		      (car args)))))
       (cond
	((bufferp entry)
	 (if em-verbose
	     (eshell-printn (format "rm: removing buffer `%s'" entry)))
	 (unless (or em-preview
		     (and em-interactive
			  (not (y-or-n-p (format "rm: delete buffer `%s'? "
						 entry)))))
	   (eshell-funcalln 'kill-buffer entry)))
	((eshell-processp entry)
	 (if em-verbose
	     (eshell-printn (format "rm: killing process `%s'" entry)))
	 (unless (or em-preview
		     (and em-interactive
			  (not (y-or-n-p (format "rm: kill process `%s'? "
						 entry)))))
	   (eshell-funcalln 'kill-process entry)))
	((symbolp entry)
	 (if em-verbose
	     (eshell-printn (format "rm: uninterning symbol `%s'" entry)))
	 (unless
	     (or em-preview
		 (and em-interactive
		      (not (y-or-n-p (format "rm: unintern symbol `%s'? "
					     entry)))))
	   (eshell-funcalln 'unintern entry)))
	((stringp entry)
	 (if (and (file-directory-p entry)
		  (not (file-symlink-p entry)))
	     (if (or em-recursive
		     eshell-rm-removes-directories)
		 (if (or em-preview
			 (not em-interactive)
			 (y-or-n-p
			  (format "rm: descend into directory `%s'? "
				  entry)))
		     (eshell-remove-entries nil (list entry) t))
	       (eshell-error (format "rm: %s: is a directory\n" entry)))
	   (eshell-remove-entries nil (list entry) t)))))
     (setq args (cdr args)))
   nil))

(put 'eshell/rm 'eshell-no-numeric-conversions t)

(defun eshell/mkdir (&rest args)
  "Implementation of mkdir in Lisp."
  (eshell-eval-using-options
   "mkdir" args
   '((?h "help" nil nil "show this usage screen")
     :external "mkdir"
     :show-usage
     :usage "[OPTION] DIRECTORY...
Create the DIRECTORY(ies), if they do not already exist.")
   (while args
     (eshell-funcalln 'make-directory (car args))
     (setq args (cdr args)))
   nil))

(put 'eshell/mkdir 'eshell-no-numeric-conversions t)

(defun eshell/rmdir (&rest args)
  "Implementation of rmdir in Lisp."
  (eshell-eval-using-options
   "rmdir" args
   '((?h "help" nil nil "show this usage screen")
     :external "rmdir"
     :show-usage
     :usage "[OPTION] DIRECTORY...
Remove the DIRECTORY(ies), if they are empty.")
   (while args
     (eshell-funcalln 'delete-directory (car args))
     (setq args (cdr args)))
   nil))

(put 'eshell/rmdir 'eshell-no-numeric-conversions t)

(defvar no-dereference)

(defvar eshell-warn-dot-directories t)

(defun eshell-shuffle-files (command action files target func deep &rest args)
  "Shuffle around some filesystem entries, using FUNC to do the work."
  (let ((attr-target (eshell-file-attributes target))
	(is-dir (or (file-directory-p target)
		    (and em-preview (not eshell-warn-dot-directories))))
	attr)
    (if (and (not em-preview) (not is-dir)
	     (> (length files) 1))
	(error "%s: when %s multiple files, last argument must be a directory"
	       command action))
    (while files
      (setcar files (directory-file-name (car files)))
      (cond
       ((string-match "\\`\\.\\.?\\'"
		      (file-name-nondirectory (car files)))
	(if eshell-warn-dot-directories
	    (eshell-error (format "%s: %s: omitting directory\n"
				  command (car files)))))
       ((and attr-target
	     (or (not (eshell-under-windows-p))
		 (eq system-type 'ms-dos))
	     (setq attr (eshell-file-attributes (car files)))
	     (nth 10 attr-target) (nth 10 attr)
	     ;; Use equal, not -, since the inode and the device could
	     ;; cons cells.
	     (equal (nth 10 attr-target) (nth 10 attr))
	     (nth 11 attr-target) (nth 11 attr)
	     (equal (nth 11 attr-target) (nth 11 attr)))
	(eshell-error (format "%s: `%s' and `%s' are the same file\n"
			      command (car files) target)))
       (t
	(let ((source (car files))
	      (target (if is-dir
			  (expand-file-name
			   (file-name-nondirectory (car files)) target)
			target))
	      link)
	  (if (and (file-directory-p source)
		   (or (not no-dereference)
		       (not (file-symlink-p source)))
		   (not (memq func '(make-symbolic-link
				     add-name-to-file))))
	      (if (and (eq func 'copy-file)
		       (not em-recursive))
		  (eshell-error (format "%s: %s: omitting directory\n"
					command (car files)))
		(let (eshell-warn-dot-directories)
		  (if (and (not deep)
			   (eq func 'rename-file)
			   ;; Use equal, since the device might be a
			   ;; cons cell.
			   (equal (nth 11 (eshell-file-attributes
					   (file-name-directory
					    (directory-file-name
					     (expand-file-name source)))))
				  (nth 11 (eshell-file-attributes
					   (file-name-directory
					    (directory-file-name
					     (expand-file-name target)))))))
		      (apply 'eshell-funcalln func source target args)
		  (unless (file-directory-p target)
		    (if em-verbose
			(eshell-printn
			 (format "%s: making directory %s"
				 command target)))
		    (unless em-preview
		      (eshell-funcalln 'make-directory target)))
		  (apply 'eshell-shuffle-files
			 command action
			 (mapcar
			  (function
			   (lambda (file)
			     (concat source "/" file)))
			  (directory-files source))
			 target func t args)
		  (when (eq func 'rename-file)
		    (if em-verbose
			(eshell-printn
			 (format "%s: deleting directory %s"
				 command source)))
		    (unless em-preview
		      (eshell-funcalln 'delete-directory source))))))
	    (if em-verbose
		(eshell-printn (format "%s: %s -> %s" command
				       source target)))
	    (unless em-preview
	      (if (and no-dereference
		       (setq link (file-symlink-p source)))
		  (progn
		    (apply 'eshell-funcalln 'make-symbolic-link
			   link target args)
		    (if (eq func 'rename-file)
			(if (and (file-directory-p source)
				 (not (file-symlink-p source)))
			    (eshell-funcalln 'delete-directory source)
			  (eshell-funcalln 'delete-file source))))
		(apply 'eshell-funcalln func source target args)))))))
      (setq files (cdr files)))))

(defun eshell-shorthand-tar-command (command args)
  "Rewrite `cp -v dir a.tar.gz' to `tar cvzf a.tar.gz dir'."
  (let* ((archive (car (last args)))
	 (tar-args
	  (cond ((string-match "z2" archive) "If")
		((string-match "gz" archive) "zf")
		((string-match "\\(az\\|Z\\)" archive) "Zf")
		(t "f"))))
    (if (file-exists-p archive)
	(setq tar-args (concat "u" tar-args))
      (setq tar-args (concat "c" tar-args)))
    (if em-verbose
	(setq tar-args (concat "v" tar-args)))
    (if (equal command "mv")
	(setq tar-args (concat "--remove-files -" tar-args)))
    ;; truncate the archive name from the arguments
    (setcdr (last args 2) nil)
    (throw 'eshell-replace-command
	   (eshell-parse-command
	    (format "tar %s %s" tar-args archive) args))))

;; this is to avoid duplicating code...
(defmacro eshell-mvcpln-template (command action func query-var
					  force-var &optional preserve)
  `(let ((len (length args)))
     (if (or (= len 0)
	     (and (= len 1) (null eshell-default-target-is-dot)))
	 (error "%s: missing destination file or directory" ,command))
     (if (= len 1)
	 (nconc args '(".")))
     (setq args (eshell-stringify-list (eshell-flatten-list args)))
     (if (and ,(not (equal command "ln"))
	      (string-match eshell-tar-regexp (car (last args)))
	      (or (> (length args) 2)
		  (and (file-directory-p (car args))
		       (or (not no-dereference)
			   (not (file-symlink-p (car args)))))))
	 (eshell-shorthand-tar-command ,command args)
       (let ((target (car (last args)))
	     ange-cache)
	 (setcdr (last args 2) nil)
	 (eshell-shuffle-files
	  ,command ,action args target ,func nil
	  ,@(append
	     `((if (and (or em-interactive
			    ,query-var)
			(not force))
		   1 (or force ,force-var)))
	     (if preserve
		 (list preserve)))))
       nil)))

(defun eshell/mv (&rest args)
  "Implementation of mv in Lisp."
  (eshell-eval-using-options
   "mv" args
   '((?f "force" nil force
	 "remove existing destinations, never prompt")
     (?i "interactive" nil em-interactive
	 "request confirmation if target already exists")
     (?n "preview" nil em-preview
	 "don't change anything on disk")
     (?v "verbose" nil em-verbose
	 "explain what is being done")
     (nil "help" nil nil "show this usage screen")
     :preserve-args
     :external "mv"
     :show-usage
     :usage "[OPTION]... SOURCE DEST
   or: mv [OPTION]... SOURCE... DIRECTORY
Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY.
\[OPTION] DIRECTORY...")
   (let ((no-dereference t))
     (eshell-mvcpln-template "mv" "moving" 'rename-file
			     eshell-mv-interactive-query
			     eshell-mv-overwrite-files))))

(put 'eshell/mv 'eshell-no-numeric-conversions t)

(defun eshell/cp (&rest args)
  "Implementation of cp in Lisp."
  (eshell-eval-using-options
   "cp" args
   '((?a "archive" nil archive
	 "same as -dpR")
     (?d "no-dereference" nil no-dereference
	 "preserve links")
     (?f "force" nil force
	 "remove existing destinations, never prompt")
     (?i "interactive" nil em-interactive
	 "request confirmation if target already exists")
     (?n "preview" nil em-preview
	 "don't change anything on disk")
     (?p "preserve" nil preserve
	 "preserve file attributes if possible")
     (?R "recursive" nil em-recursive
	 "copy directories recursively")
     (?v "verbose" nil em-verbose
	 "explain what is being done")
     (nil "help" nil nil "show this usage screen")
     :preserve-args
     :external "cp"
     :show-usage
     :usage "[OPTION]... SOURCE DEST
   or:  cp [OPTION]... SOURCE... DIRECTORY
Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY.")
   (if archive
       (setq preserve t no-dereference t em-recursive t))
   (eshell-mvcpln-template "cp" "copying" 'copy-file
			   eshell-cp-interactive-query
			   eshell-cp-overwrite-files preserve)))

(put 'eshell/cp 'eshell-no-numeric-conversions t)

(defun eshell/ln (&rest args)
  "Implementation of ln in Lisp."
  (eshell-eval-using-options
   "ln" args
   '((?h "help" nil nil "show this usage screen")
     (?s "symbolic" nil symbolic
	 "make symbolic links instead of hard links")
     (?i "interactive" nil em-interactive
	 "request confirmation if target already exists")
     (?f "force" nil force "remove existing destinations, never prompt")
     (?n "preview" nil em-preview
	 "don't change anything on disk")
     (?v "verbose" nil em-verbose "explain what is being done")
     :preserve-args
     :external "ln"
     :show-usage
     :usage "[OPTION]... TARGET [LINK_NAME]
   or:  ln [OPTION]... TARGET... DIRECTORY
Create a link to the specified TARGET with optional LINK_NAME.  If there is
more than one TARGET, the last argument must be a directory;  create links
in DIRECTORY to each TARGET.  Create hard links by default, symbolic links
with '--symbolic'.  When creating hard links, each TARGET must exist.")
   (let ((no-dereference t))
     (eshell-mvcpln-template "ln" "linking"
			     (if symbolic
				 'make-symbolic-link
			       'add-name-to-file)
			     eshell-ln-interactive-query
			     eshell-ln-overwrite-files))))

(put 'eshell/ln 'eshell-no-numeric-conversions t)

(defun eshell/cat (&rest args)
  "Implementation of cat in Lisp.
If in a pipeline, or the file is not a regular file, directory or
symlink, then revert to the system's definition of cat."
  (setq args (eshell-stringify-list (eshell-flatten-list args)))
  (if (or eshell-in-pipeline-p
	  (catch 'special
	    (dolist (arg args)
	      (unless (or (and (stringp arg)
			       (> (length arg) 0)
			       (eq (aref arg 0) ?-))
			  (let ((attrs (eshell-file-attributes arg)))
			    (and attrs (memq (aref (nth 8 attrs) 0)
					     '(?d ?l ?-)))))
		(throw 'special t)))))
      (let ((ext-cat (eshell-search-path "cat")))
	(if ext-cat
	    (throw 'eshell-replace-command
		   (eshell-parse-command (eshell-quote-argument ext-cat) args))
	  (if eshell-in-pipeline-p
	      (error "Eshell's `cat' does not work in pipelines")
	    (error "Eshell's `cat' cannot display one of the files given"))))
    (eshell-init-print-buffer)
    (eshell-eval-using-options
     "cat" args
     '((?h "help" nil nil "show this usage screen")
       :external "cat"
       :show-usage
       :usage "[OPTION] FILE...
Concatenate FILE(s), or standard input, to standard output.")
     (dolist (file args)
       (if (string= file "-")
	   (throw 'eshell-external
		  (eshell-external-command "cat" args))))
     (let ((curbuf (current-buffer)))
       (dolist (file args)
	 (with-temp-buffer
	   (insert-file-contents file)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (let ((str (buffer-substring
			 (point) (min (1+ (line-end-position))
				      (point-max)))))
	       (with-current-buffer curbuf
		 (eshell-buffered-print str)))
	     (forward-line)))))
     (eshell-flush)
     ;; if the file does not end in a newline, do not emit one
     (setq eshell-ensure-newline-p nil))))

(put 'eshell/cat 'eshell-no-numeric-conversions t)

;; special front-end functions for compilation-mode buffers

(defun eshell/make (&rest args)
  "Use `compile' to do background makes."
  (if (and eshell-current-subjob-p
	   (eshell-interactive-output-p))
      (let ((compilation-process-setup-function
	     (list 'lambda nil
		   (list 'setq 'process-environment
			 (list 'quote (eshell-copy-environment))))))
	(compile (concat "make " (eshell-flatten-and-stringify args))))
    (throw 'eshell-replace-command
	   (eshell-parse-command "*make" (eshell-stringify-list
					  (eshell-flatten-list args))))))

(put 'eshell/make 'eshell-no-numeric-conversions t)

(defun eshell-occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char (marker-position pos))))

(defun eshell-occur-mode-mouse-goto (event)
  "In Occur mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let (pos)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq pos (occur-mode-find-occurrence))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char (marker-position pos))))

(defun eshell-poor-mans-grep (args)
  "A poor version of grep that opens every file and uses `occur'.
This eats up memory, since it leaves the buffers open (to speed future
searches), and it's very slow.  But, if your system has no grep
available..."
  (save-selected-window
    (let ((default-dir default-directory))
      (with-current-buffer (get-buffer-create "*grep*")
	(let ((inhibit-read-only t)
	      (default-directory default-dir))
	  (erase-buffer)
	  (occur-mode)
	  (let ((files (eshell-stringify-list
			(eshell-flatten-list (cdr args))))
		(inhibit-redisplay t)
		string)
	    (when (car args)
	      (if (get-buffer "*Occur*")
		  (kill-buffer (get-buffer "*Occur*")))
	      (setq string nil)
	      (while files
		(with-current-buffer (find-file-noselect (car files))
		  (save-excursion
		    (ignore-errors
		      (occur (car args))))
		  (if (get-buffer "*Occur*")
		      (with-current-buffer (get-buffer "*Occur*")
			(setq string (buffer-string))
			(kill-buffer (current-buffer)))))
		(if string (insert string))
		(setq string nil
		      files (cdr files)))))
	  (local-set-key [mouse-2] 'eshell-occur-mode-mouse-goto)
	  (local-set-key [(control ?c) (control ?c)]
			 'eshell-occur-mode-goto-occurrence)
	  (local-set-key [(control ?m)]
			 'eshell-occur-mode-goto-occurrence)
	  (local-set-key [return] 'eshell-occur-mode-goto-occurrence)
	  (pop-to-buffer (current-buffer) t)
	  (goto-char (point-min))
	  (resize-temp-buffer-window))))))

(defun eshell-grep (command args &optional maybe-use-occur)
  "Generic service function for the various grep aliases.
It calls Emacs's grep utility if the command is not redirecting output,
and if it's not part of a command pipeline.  Otherwise, it calls the
external command."
  (if (and maybe-use-occur eshell-no-grep-available)
      (eshell-poor-mans-grep args)
    (if (or eshell-plain-grep-behavior
	    (not (and (eshell-interactive-output-p)
		      (not eshell-in-pipeline-p)
		      (not eshell-in-subcommand-p))))
	(throw 'eshell-replace-command
	       (eshell-parse-command (concat "*" command)
				     (eshell-stringify-list
				      (eshell-flatten-list args))))
      (let* ((args (mapconcat 'identity
			      (mapcar 'shell-quote-argument
				      (eshell-stringify-list
				       (eshell-flatten-list args)))
			      " "))
	     (cmd (progn
		    (set-text-properties 0 (length args)
					 '(invisible t) args)
		    (format "%s -n %s" command args)))
	     compilation-scroll-output)
	(grep cmd)))))

(defun eshell/grep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (eshell-grep "grep" args t))

(defun eshell/egrep (&rest args)
  "Use Emacs grep facility instead of calling external egrep."
  (eshell-grep "egrep" args t))

(defun eshell/fgrep (&rest args)
  "Use Emacs grep facility instead of calling external fgrep."
  (eshell-grep "fgrep" args t))

(defun eshell/agrep (&rest args)
  "Use Emacs grep facility instead of calling external agrep."
  (eshell-grep "agrep" args))

(defun eshell/glimpse (&rest args)
  "Use Emacs grep facility instead of calling external glimpse."
  (let (null-device)
    (eshell-grep "glimpse" (append '("-z" "-y") args))))

;; completions rules for some common UNIX commands

(defsubst eshell-complete-hostname ()
  "Complete a command that wants a hostname for an argument."
  (pcomplete-here (eshell-read-host-names)))

(defun eshell-complete-host-reference ()
  "If there is a host reference, complete it."
  (let ((arg (pcomplete-actual-arg))
	index)
    (when (setq index (string-match "@[a-z.]*\\'" arg))
      (setq pcomplete-stub (substring arg (1+ index))
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions (eshell-read-host-names)))))

(defalias 'pcomplete/ftp    'eshell-complete-hostname)
(defalias 'pcomplete/ncftp  'eshell-complete-hostname)
(defalias 'pcomplete/ping   'eshell-complete-hostname)
(defalias 'pcomplete/rlogin 'eshell-complete-hostname)

(defun pcomplete/telnet ()
  (require 'pcmpl-unix)
  (pcomplete-opt "xl(pcmpl-unix-user-names)")
  (eshell-complete-hostname))

(defun pcomplete/rsh ()
  "Complete `rsh', which, after the user and hostname, is like xargs."
  (require 'pcmpl-unix)
  (pcomplete-opt "l(pcmpl-unix-user-names)")
  (eshell-complete-hostname)
  (pcomplete-here (funcall pcomplete-command-completion-function))
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

(defvar block-size)
(defvar by-bytes)
(defvar dereference-links)
(defvar grand-total)
(defvar human-readable)
(defvar max-depth)
(defvar only-one-filesystem)
(defvar show-all)

(defsubst eshell-du-size-string (size)
  (let* ((str (eshell-printable-size size human-readable block-size t))
	 (len (length str)))
    (concat str (if (< len 8)
		    (make-string (- 8 len) ? )))))

(defun eshell-du-sum-directory (path depth)
  "Summarize PATH, and its member directories."
  (let ((entries (eshell-directory-files-and-attributes path))
	(size 0.0))
    (while entries
      (unless (string-match "\\`\\.\\.?\\'" (caar entries))
	(let* ((entry (concat path "/"
			      (caar entries)))
	       (symlink (and (stringp (cadr (car entries)))
			     (cadr (car entries)))))
	  (unless (or (and symlink (not dereference-links))
		      (and only-one-filesystem
			   (/= only-one-filesystem
			       (nth 12 (car entries)))))
	    (if symlink
		(setq entry symlink))
	    (setq size
		  (+ size
		     (if (eq t (cadr (car entries)))
			 (eshell-du-sum-directory entry (1+ depth))
		       (let ((file-size (nth 8 (car entries))))
			 (prog1
			     file-size
			   (if show-all
			       (eshell-print
				(concat (eshell-du-size-string file-size)
					entry "\n")))))))))))
      (setq entries (cdr entries)))
    (if (or (not max-depth)
	    (= depth max-depth)
	    (= depth 0))
	(eshell-print (concat (eshell-du-size-string size)
			      (directory-file-name path) "\n")))
    size))

(defun eshell/du (&rest args)
  "Implementation of \"du\" in Lisp, passing ARGS."
  (setq args (if args
		 (eshell-stringify-list (eshell-flatten-list args))
	       '(".")))
  (let ((ext-du (eshell-search-path "du")))
    (if (and ext-du
	     (not (catch 'have-ange-path
		    (dolist (arg args)
		      (if (string-equal
			   (file-remote-p (expand-file-name arg) 'method) "ftp")
			  (throw 'have-ange-path t))))))
	(throw 'eshell-replace-command
	       (eshell-parse-command (eshell-quote-argument ext-du) args))
      (eshell-eval-using-options
       "du" args
       '((?a "all" nil show-all
	     "write counts for all files, not just directories")
	 (nil "block-size" t block-size
	      "use SIZE-byte blocks (i.e., --block-size SIZE)")
	 (?b "bytes" nil by-bytes
	     "print size in bytes")
	 (?c "total" nil grand-total
	     "produce a grand total")
	 (?d "max-depth" t max-depth
	     "display data only this many levels of data")
	 (?h "human-readable" 1024 human-readable
	     "print sizes in human readable format")
	 (?H "is" 1000 human-readable
	     "likewise, but use powers of 1000 not 1024")
	 (?k "kilobytes" 1024 block-size
	     "like --block-size 1024")
	 (?L "dereference" nil dereference-links
	     "dereference all symbolic links")
	 (?m "megabytes" 1048576 block-size
	     "like --block-size 1048576")
	 (?s "summarize" 0 max-depth
	     "display only a total for each argument")
	 (?x "one-file-system" nil only-one-filesystem
	     "skip directories on different filesystems")
	 (nil "help" nil nil
	      "show this usage screen")
	 :external "du"
	 :usage "[OPTION]... FILE...
Summarize disk usage of each FILE, recursively for directories.")
       (unless by-bytes
	 (setq block-size (or block-size 1024)))
       (if (and max-depth (stringp max-depth))
	   (setq max-depth (string-to-number max-depth)))
       ;; filesystem support means nothing under Windows
       (if (eshell-under-windows-p)
	   (setq only-one-filesystem nil))
       (let ((size 0.0) ange-cache)
	 (while args
	   (if only-one-filesystem
	       (setq only-one-filesystem
		     (nth 11 (eshell-file-attributes
			      (file-name-as-directory (car args))))))
	   (setq size (+ size (eshell-du-sum-directory
			       (directory-file-name (car args)) 0)))
	   (setq args (cdr args)))
	 (if grand-total
	     (eshell-print (concat (eshell-du-size-string size)
				   "total\n"))))))))

(defvar eshell-time-start nil)

(defun eshell-show-elapsed-time ()
  (let ((elapsed (format "%.3f secs\n" (- (float-time) eshell-time-start))))
    (set-text-properties 0 (length elapsed) '(face bold) elapsed)
    (eshell-interactive-print elapsed))
  (remove-hook 'eshell-post-command-hook 'eshell-show-elapsed-time t))

(defun eshell/time (&rest args)
  "Implementation of \"time\" in Lisp."
  (let ((time-args (copy-alist args))
	(continue t)
	last-arg)
    (while (and continue args)
      (if (not (string-match "^-" (car args)))
	  (progn
	    (if last-arg
		(setcdr last-arg nil)
	      (setq args '("")))
	    (setq continue nil))
	(setq last-arg args
	      args (cdr args))))
    (eshell-eval-using-options
     "time" args
     '((?h "help" nil nil "show this usage screen")
       :external "time"
       :show-usage
       :usage "COMMAND...
Show wall-clock time elapsed during execution of COMMAND.")
     (setq eshell-time-start (float-time))
     (add-hook 'eshell-post-command-hook 'eshell-show-elapsed-time nil t)
     ;; after setting
     (throw 'eshell-replace-command
	    (eshell-parse-command (car time-args)
;;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2007-08/msg00205.html
				  (eshell-stringify-list
				   (eshell-flatten-list (cdr time-args))))))))

(defun eshell/whoami (&rest args)
  "Make \"whoami\" Tramp aware."
  (or (file-remote-p default-directory 'user) (user-login-name)))

(defvar eshell-diff-window-config nil)

(defun eshell-diff-quit ()
  "Restore the window configuration previous to diff'ing."
  (interactive)
  (if eshell-diff-window-config
      (set-window-configuration eshell-diff-window-config)))

(defun nil-blank-string (string)
  "Return STRING, or nil if STRING contains only non-blank characters."
  (cond
    ((string-match "[^[:blank:]]" string) string)
    (nil)))

(autoload 'diff-no-select "diff")

(defun eshell/diff (&rest args)
  "Alias \"diff\" to call Emacs `diff' function."
  (let ((orig-args (eshell-stringify-list (eshell-flatten-list args))))
    (if (or eshell-plain-diff-behavior
	    (not (and (eshell-interactive-output-p)
		      (not eshell-in-pipeline-p)
		      (not eshell-in-subcommand-p))))
	(throw 'eshell-replace-command
	       (eshell-parse-command "*diff" orig-args))
      (setq args (copy-sequence orig-args))
      (if (< (length args) 2)
	  (throw 'eshell-replace-command
		 (eshell-parse-command "*diff" orig-args)))
      (let ((old (car (last args 2)))
	    (new (car (last args)))
	    (config (current-window-configuration)))
	(if (= (length args) 2)
	    (setq args nil)
	  (setcdr (last args 3) nil))
	(with-current-buffer
	    (condition-case err
		(diff-no-select
		 old new
		 (nil-blank-string (eshell-flatten-and-stringify args)))
	      (error
	       (throw 'eshell-replace-command
		      (eshell-parse-command "*diff" orig-args))))
	  (when (fboundp 'diff-mode)
	    (make-local-variable 'compilation-finish-functions)
	    (add-hook
	     'compilation-finish-functions
	     `(lambda (buff msg)
		(with-current-buffer buff
		  (diff-mode)
		  (set (make-local-variable 'eshell-diff-window-config)
		       ,config)
		  (local-set-key [?q] 'eshell-diff-quit)
		  (if (fboundp 'turn-on-font-lock-if-enabled)
		      (turn-on-font-lock-if-enabled))
		  (goto-char (point-min))))))
	  (pop-to-buffer (current-buffer))))))
  nil)

(put 'eshell/diff 'eshell-no-numeric-conversions t)

(defun eshell/locate (&rest args)
  "Alias \"locate\" to call Emacs `locate' function."
  (if (or eshell-plain-locate-behavior
	  (not (and (eshell-interactive-output-p)
		    (not eshell-in-pipeline-p)
		    (not eshell-in-subcommand-p)))
	  (and (stringp (car args))
	       (string-match "^-" (car args))))
      (throw 'eshell-replace-command
	     (eshell-parse-command "*locate" (eshell-stringify-list
					      (eshell-flatten-list args))))
    (save-selected-window
      (let ((locate-history-list (list (car args))))
	(locate-with-filter (car args) (cadr args))))))

(put 'eshell/locate 'eshell-no-numeric-conversions t)

(defun eshell/occur (&rest args)
  "Alias \"occur\" to call Emacs `occur' function."
  (let ((inhibit-read-only t))
    (if (> (length args) 2)
	(error "usage: occur: (REGEXP &optional NLINES)")
      (apply 'occur args))))

(put 'eshell/occur 'eshell-no-numeric-conversions t)

;; Pacify the byte-compiler.
(defvar tramp-default-proxies-alist)

(defun eshell/su (&rest args)
  "Alias \"su\" to call Tramp."
  (require 'tramp)
  (setq args (eshell-stringify-list (eshell-flatten-list args)))
  (let ((orig-args (copy-tree args)))
    (eshell-eval-using-options
     "su" args
     '((?h "help" nil nil "show this usage screen")
       (?l "login" nil login "provide a login environment")
       (?  nil nil login "provide a login environment")
       :usage "[- | -l | --login] [USER]
Become another USER during a login session.")
     (throw 'eshell-replace-command
	    (let ((user "root")
		  (host (or (file-remote-p default-directory 'host)
			    "localhost"))
		  (dir (or (file-remote-p default-directory 'localname)
			   (expand-file-name default-directory))))
	      (dolist (arg args)
		(if (string-equal arg "-") (setq login t) (setq user arg)))
	      ;; `eshell-eval-using-options' does not handle "-".
	      (if (member "-" orig-args) (setq login t))
	      (if login (setq dir "~/"))
	      (if (and (file-remote-p default-directory)
		       (or
			(not (string-equal
			      "su" (file-remote-p default-directory 'method)))
			(not (string-equal
			      user (file-remote-p default-directory 'user)))))
		  (add-to-list
		   'tramp-default-proxies-alist
		   (list host user (file-remote-p default-directory))))
	      (eshell-parse-command
	       "cd" (list (format "/su:%s@%s:%s" user host dir))))))))

(put 'eshell/su 'eshell-no-numeric-conversions t)

(defun eshell/sudo (&rest args)
  "Alias \"sudo\" to call Tramp."
  (require 'tramp)
  (setq args (eshell-stringify-list (eshell-flatten-list args)))
  (let ((orig-args (copy-tree args)))
    (eshell-eval-using-options
     "sudo" args
     '((?h "help" nil nil "show this usage screen")
       (?u "user" t user "execute a command as another USER")
       :show-usage
       :usage "[(-u | --user) USER] COMMAND
Execute a COMMAND as the superuser or another USER.")
     (throw 'eshell-external
	    (let ((user (or user "root"))
		  (host (or (file-remote-p default-directory 'host)
			    "localhost"))
		  (dir (or (file-remote-p default-directory 'localname)
			   (expand-file-name default-directory))))
	      ;; `eshell-eval-using-options' reads options of COMMAND.
	      (while (and (stringp (car orig-args))
			  (member (car orig-args) '("-u" "--user")))
		(setq orig-args (cddr orig-args)))
	      (if (and (file-remote-p default-directory)
		       (or
			(not (string-equal
			      "sudo" (file-remote-p default-directory 'method)))
			(not (string-equal
			      user (file-remote-p default-directory 'user)))))
		  (add-to-list
		   'tramp-default-proxies-alist
		   (list host user (file-remote-p default-directory))))
	      (let ((default-directory (format "/sudo:%s@%s:%s" user host dir)))
		(eshell-named-command (car orig-args) (cdr orig-args))))))))

(put 'eshell/sudo 'eshell-no-numeric-conversions t)

(provide 'em-unix)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-unix.el ends here
