;;; locate.el --- interface to the locate command

;; Copyright (C) 1996, 1998, 2001-2012  Free Software Foundation, Inc.

;; Author: Peter Breton <pbreton@cs.umb.edu>
;; Keywords: unix files

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

;; Search a database of files and use dired commands on the result.
;;
;; Locate.el provides an interface to a program which searches a
;; database of file names. By default, this program is the GNU locate
;; command, but it could also be the BSD-style find command, or even a
;; user specified command.
;;
;; To use the BSD-style "fast find", or any other shell command of the
;; form
;;
;;   SHELLPROGRAM  Name-to-find
;;
;; set the variable `locate-command' in your .emacs file.
;;
;;   To use a more complicated expression, create a function which
;; takes a string (the name to find) as input and returns a list.
;; The first element should be the command to be executed, the remaining
;; elements should be the arguments (including the name to find). Then put
;;
;; (setq locate-make-command-line 'my-locate-command-line)
;;
;; in your .emacs, using the name of your function in place of
;; my-locate-command-line.
;;
;; You should make sure that whichever command you use works correctly
;; from a shell prompt. GNU locate and BSD find expect the file databases
;; to either be in standard places or located via environment variables.
;; If the latter, make sure these environment variables are set in
;; your emacs process.
;;
;; Locate-mode assumes that each line output from the locate-command
;; consists exactly of a file name, possibly preceded or trailed by
;; whitespace. If your file database has other information on the line (for
;; example, the file size), you will need to redefine the function
;; `locate-get-file-positions' to return a list consisting of the first
;; character in the file name and the last character in the file name.
;;
;; To use locate-mode, simply type M-x locate and then the string
;; you wish to find. You can use almost all of the dired commands in
;; the resulting *Locate* buffer.  It is worth noting that your commands
;; do not, of course, affect the file database. For example, if you
;; compress a file in the locate buffer, the actual file will be
;; compressed, but the entry in the file database will not be
;; affected. Consequently, the database and the filesystem will be out
;; of sync until the next time the database is updated.
;;
;; The command `locate-with-filter' keeps only lines matching a
;; regular expression; this is often useful to constrain a big search.
;;

;;;;; Building a database of files ;;;;;;;;;
;;
;; You can create a simple files database with a port of the Unix find command
;; and one of the various Windows NT various scheduling utilities,
;; for example the AT command from the NT Resource Kit, WinCron which is
;; included with Microsoft FrontPage, or the shareware NTCron program.
;;
;; To set up a function which searches the files database, do something
;; like this:
;;
;; (defvar locate-fcodes-file       "c:/users/peter/fcodes")
;; (defvar locate-make-command-line 'nt-locate-make-command-line)
;;
;; (defun nt-locate-make-command-line (arg)
;;  (list "grep" "-i" arg locate-fcodes-file))
;;
;;;;;;;; ADVICE For dired-make-relative: ;;;;;;;;;
;;
;; For certain dired commands to work right, you should also include the
;; following in your _emacs/.emacs:
;;
;; (defadvice dired-make-relative (before set-no-error activate)
;;   "For locate mode and Windows, don't return errors"
;;   (if (and (eq   major-mode  'locate-mode)
;; 	   (memq system-type '(windows-nt ms-dos)))
;;       (ad-set-arg 2 t)
;;     ))
;;
;; Otherwise, `dired-make-relative' will give error messages like
;; "FILENAME: not in directory tree growing at /"


;;; Code:

(require 'dired)

;; Variables

(defvar locate-current-filter nil)
(defvar locate-local-filter nil)
(defvar locate-local-search nil)
(defvar locate-local-prompt nil)

(defgroup locate nil
  "Interface to the locate command."
  :prefix "locate-"
  :group 'external)

(defcustom locate-command "locate"
  "Executable program for searching a database of files.
The Emacs commands `locate' and `locate-with-filter' use this.
The value should be a program that can be called from a shell
with one argument, SEARCH-STRING.  The program determines which
database it searches.  The output of the program should consist
of those file names in the database that match SEARCH-STRING,
listed one per line, possibly with leading or trailing
whitespace.  If the output is in another form, you may have to
redefine the function `locate-get-file-positions'.

The program may interpret SEARCH-STRING as a literal string, a
shell pattern or a regular expression.  The exact rules of what
constitutes a match may also depend on the program.

The standard value of this variable is \"locate\".
This program normally searches a database of all files on your
system, or of all files that you have access to.  Consult the
documentation of that program for the details about how it determines
which file names match SEARCH-STRING.  (Those details vary highly with
the version.)"
  :type 'string
  :group 'locate)

(defcustom locate-post-command-hook nil
  "List of hook functions run after `locate' (see `run-hooks')."
  :type  'hook
  :group 'locate)

(defvar locate-history-list nil
  "The history list used by the \\[locate] command.")

(defvar locate-grep-history-list nil
  "The history list used by the \\[locate-with-filter] command.")

(defcustom locate-make-command-line 'locate-default-make-command-line
  "Function used to create the locate command line.
The Emacs commands `locate' and `locate-with-filter' use this.
This function should take one argument, a string (the name to find)
and return a list of strings.  The first element of the list should be
the name of a command to be executed by a shell, the remaining elements
should be the arguments to that command (including the name to find)."
  :type 'function
  :group 'locate)

(defcustom locate-buffer-name "*Locate*"
  "Name of the buffer to show results from the \\[locate] command."
  :type 'string
  :group 'locate)

(defcustom locate-fcodes-file nil
  "File name for the database of file names used by `locate'.
If non-nil, `locate' uses this name in the header of the `*Locate*'
buffer.  If nil, it mentions no file name in that header.

Just setting this variable does not actually change the database
that `locate' searches.  The executive program that the Emacs
function `locate' uses, as given by the variables `locate-command'
or `locate-make-command-line', determines the database."
  :type '(choice (const :tag "None" nil) file)
  :group 'locate)

(defcustom locate-header-face nil
  "Face used to highlight the locate header."
  :type '(choice (const :tag "None" nil) face)
  :group 'locate)

;;;###autoload
(defcustom locate-ls-subdir-switches (purecopy "-al")
  "`ls' switches for inserting subdirectories in `*Locate*' buffers.
This should contain the \"-l\" switch, but not the \"-F\" or \"-b\" switches."
  :type 'string
  :group 'locate
  :version "22.1")

(defcustom locate-update-when-revert nil
  "This option affects how the *Locate* buffer gets reverted.
If non-nil, offer to update the locate database when reverting that buffer.
\(Normally, you need to have root privileges for this to work.  See the
option `locate-update-path'.)
If nil, reverting does not update the locate database."
  :type 'boolean
  :group 'locate
  :version "22.1")

(defcustom locate-update-command "updatedb"
  "The executable program used to update the locate database."
  :type 'string
  :group 'locate)

(defcustom locate-update-path "/"
  "The default directory from where `locate-update-command' is called.
Usually, root permissions are required to run that command.  This
can be achieved by setting this option to \"/su::\" or \"/sudo::\"
\(if you have the appropriate authority).  If your current user
permissions are sufficient to run the command, you can set this
option to \"/\"."
  :type 'string
  :group 'locate
  :version "22.1")

(defcustom locate-prompt-for-command nil
  "If non-nil, the `locate' command prompts for a command to run.
Otherwise, that behavior is invoked via a prefix argument.

Setting this option non-nil actually inverts the meaning of a prefix arg;
that is, with a prefix arg, you get the default behavior."
  :group 'locate
  :type 'boolean)

(defcustom locate-mode-hook nil
  "List of hook functions run by `locate-mode' (see `run-mode-hooks')."
  :type  'hook
  :group 'locate)

;; Functions

(defun locate-default-make-command-line (search-string)
  (list locate-command search-string))

(defun locate-word-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "-a-zA-Z0-9.")
       (point))
     (save-excursion
       (skip-chars-forward "-a-zA-Z0-9.")
       (skip-chars-backward "." pt)
       (point)))))

;; Function for use in interactive declarations.
(defun locate-prompt-for-search-string ()
  (if (or (and current-prefix-arg
	       (not locate-prompt-for-command))
	  (and (not current-prefix-arg) locate-prompt-for-command))
      (let ((locate-cmd (funcall locate-make-command-line "")))
	(read-from-minibuffer
	 "Run locate (like this): "
	 (cons
	  (concat (car locate-cmd) "  "
		  (mapconcat 'identity (cdr locate-cmd) " "))
	  (+ 2 (length (car locate-cmd))))
	 nil nil 'locate-history-list))
    (let* ((default (locate-word-at-point))
	   (input
	    (read-from-minibuffer
	     (if  (> (length default) 0)
		 (format "Locate (default %s): " default)
	       (format "Locate: "))
	     nil nil nil 'locate-history-list default t)))
      (and (equal input "") default
	   (setq input default))
      input)))

;;;###autoload
(defun locate (search-string &optional filter arg)
  "Run the program `locate', putting results in `*Locate*' buffer.
Pass it SEARCH-STRING as argument.  Interactively, prompt for SEARCH-STRING.
With prefix arg ARG, prompt for the exact shell command to run instead.

This program searches for those file names in a database that match
SEARCH-STRING and normally outputs all matching absolute file names,
one per line.  The database normally consists of all files on your
system, or of all files that you have access to.  Consult the
documentation of the program for the details about how it determines
which file names match SEARCH-STRING.  (Those details vary highly with
the version.)

You can specify another program for this command to run by customizing
the variables `locate-command' or `locate-make-command-line'.

The main use of FILTER is to implement `locate-with-filter'.  See
the docstring of that function for its meaning.

After preparing the results buffer, this runs `dired-mode-hook' and
then `locate-post-command-hook'."
  (interactive
   (list
    (locate-prompt-for-search-string)
    nil
    current-prefix-arg))

  (if (equal search-string "")
      (error "Please specify a filename to search for"))
  (let* ((locate-cmd-list (funcall locate-make-command-line search-string))
	 (locate-cmd (car locate-cmd-list))
	 (locate-cmd-args (cdr locate-cmd-list))
	 (run-locate-command
	  (or (and arg (not locate-prompt-for-command))
	      (and (not arg) locate-prompt-for-command))))

    ;; Find the Locate buffer
    (save-window-excursion
      (set-buffer (get-buffer-create locate-buffer-name))
      (locate-mode)
      (let ((inhibit-read-only t)
	    (buffer-undo-list t))
	(erase-buffer)

	(setq locate-current-filter filter)
	(set (make-local-variable 'locate-local-search) search-string)
	(set (make-local-variable 'locate-local-filter) filter)
	(set (make-local-variable 'locate-local-prompt) run-locate-command)

	(if run-locate-command
	    (shell-command search-string locate-buffer-name)
	  (apply 'call-process locate-cmd nil t nil locate-cmd-args))

	(and filter
	     (locate-filter-output filter))

	(locate-do-setup search-string)))
    (and (not (string-equal (buffer-name) locate-buffer-name))
	(pop-to-buffer locate-buffer-name))

    (run-hooks 'dired-mode-hook)
    (dired-next-line 3)			;move to first matching file.
    (run-hooks 'locate-post-command-hook)))

;;;###autoload
(defun locate-with-filter (search-string filter &optional arg)
  "Run the executable program `locate' with a filter.
This function is similar to the function `locate', which see.
The difference is that, when invoked interactively, the present function
prompts for both SEARCH-STRING and FILTER.  It passes SEARCH-STRING
to the locate executable program.  It produces a `*Locate*' buffer
that lists only those lines in the output of the locate program that
contain a match for the regular expression FILTER; this is often useful
to constrain a big search.

ARG is the interactive prefix arg, which has the same effect as in `locate'.

When called from Lisp, this function is identical with `locate',
except that FILTER is not optional."
  (interactive
   (list
    (locate-prompt-for-search-string)
    (read-from-minibuffer "Filter: " nil nil
			  nil 'locate-grep-history-list)
    current-prefix-arg))
  (locate search-string filter arg))

(defun locate-filter-output (filter)
  "Filter output from the locate command."
  (goto-char (point-min))
  (keep-lines filter))

(defvar locate-mode-map
  (let ((map (copy-keymap dired-mode-map)))
    ;; Undefine Useless Dired Menu bars
    (define-key map [menu-bar Dired]   'undefined)
    (define-key map [menu-bar subdir]  'undefined)
    (define-key map [menu-bar mark executables] 'undefined)
    (define-key map [menu-bar mark directory]   'undefined)
    (define-key map [menu-bar mark directories] 'undefined)
    (define-key map [menu-bar mark symlinks]    'undefined)
    (define-key map [M-mouse-2] 'locate-mouse-view-file)
    (define-key map "\C-c\C-t"  'locate-tags)
    (define-key map "l"       'locate-do-redisplay)
    (define-key map "U"       'dired-unmark-all-files)
    (define-key map "V"       'locate-find-directory)
    map)
  "Local keymap for Locate mode buffers.")

;; This variable is used to indent the lines and then to search for
;; the file name
(defconst locate-filename-indentation 4
  "The amount of indentation for each file.")

(defun locate-get-file-positions ()
  "Return list of start and end of the file name on the current line.
This is a list of two buffer positions.

You should only call this function on lines that contain a file name
listed by the locate program.  Inside inserted subdirectories, or if
there is no file name on the current line, the return value is
meaningless.  You can check whether the current line contains a file
listed by the locate program, using the function
`locate-main-listing-line-p'."
  (list (+ locate-filename-indentation
           (line-beginning-position))
        ;; Assume names end at the end of the line.
        (line-end-position)))

;; From SQL-mode
(defun locate-current-line-number ()
  "Return the current line number, as an integer."
  (+ (count-lines (point-min) (point))
     (if (eq (current-column) 0)
	 1
       0)))

;; You should only call this function on lines that contain a file name
;; listed by the locate program.  Inside inserted subdirectories, or if
;; there is no file name on the current line, the return value is
;; meaningless.  You can check whether the current line contains a file
;; listed by the locate program, using the function
;; `locate-main-listing-line-p'.
(defun locate-get-filename ()
  (let ((pos    (locate-get-file-positions))
	(lineno (locate-current-line-number)))
    (and (not (eq lineno 1))
	 (not (eq lineno 2))
	 (buffer-substring (elt pos 0) (elt pos 1)))))

(defun locate-main-listing-line-p ()
  "Return t if current line contains a file name listed by locate.
This function returns nil if the current line either contains no
file name or is inside a subdirectory."
  (save-excursion
    (forward-line 0)
    (looking-at (concat "."
			(make-string (1- locate-filename-indentation) ?\s)
			"\\(/\\|[A-Za-z]:\\)"))))

(defun locate-mouse-view-file (event)
  "In Locate mode, view a file, using the mouse."
  (interactive "@e")
  (save-excursion
    (goto-char (posn-point (event-start event)))
    (if (locate-main-listing-line-p)
	(view-file (locate-get-filename))
      (message "This command only works inside main listing."))))

;; Define a mode for locate
;; Default directory is set to "/" so that dired commands, which
;; expect to be in a tree, will work properly
(defun locate-mode ()
  "Major mode for the `*Locate*' buffer made by \\[locate].
\\<locate-mode-map>\
In that buffer, you can use almost all the usual dired bindings.
\\[locate-find-directory] visits the directory of the file on the current line.
This function runs `locate-mode-hook' before returning.

Operating on listed files works, but does not always
automatically update the buffer as in ordinary Dired.
This is true both for the main listing and for subdirectories.
Reverting the buffer using \\[revert-buffer] deletes all subdirectories.
Specific `locate-mode' commands, such as \\[locate-find-directory],
do not work in subdirectories.

\\{locate-mode-map}"
  ;; Not to be called interactively.
  (kill-all-local-variables)
  ;; Avoid clobbering this variable
  (make-local-variable 'dired-subdir-alist)
  (use-local-map             locate-mode-map)
  (setq major-mode          'locate-mode
        mode-name           "Locate"
        default-directory   "/"
	buffer-read-only    t
	selective-display   t)
  (dired-alist-add-1 default-directory (point-min-marker))
  (set (make-local-variable 'dired-directory) "/")
  (set (make-local-variable 'dired-subdir-switches) locate-ls-subdir-switches)
  (setq dired-switches-alist nil)
  (make-local-variable 'directory-listing-before-filename-regexp)
  ;; This should support both Unix and Windoze style names
  (setq directory-listing-before-filename-regexp
	(concat "^.\\("
		(make-string (1- locate-filename-indentation) ?\s)
		 "\\)\\|"
		(default-value 'directory-listing-before-filename-regexp)))
  (make-local-variable 'dired-actual-switches)
  (setq dired-actual-switches "")
  (make-local-variable 'dired-permission-flags-regexp)
  (setq dired-permission-flags-regexp
	(concat "^.\\("
		(make-string (1- locate-filename-indentation) ?\s)
		"\\)\\|"
		(default-value 'dired-permission-flags-regexp)))
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'locate-update)
  (set (make-local-variable 'page-delimiter) "\n\n")
  (run-mode-hooks 'locate-mode-hook))

(defun locate-do-setup (search-string)
  (goto-char (point-min))
  (save-excursion

    ;; Nothing returned from locate command?
    (and (eobp)
	 (progn
	   (kill-buffer locate-buffer-name)
	   (if locate-current-filter
	       (error "Locate: no match for %s in database using filter %s"
		      search-string locate-current-filter)
	     (error "Locate: no match for %s in database" search-string))))

    (locate-insert-header search-string)

    (while (not (eobp))
      (insert-char ?\s locate-filename-indentation t)
      (locate-set-properties)
      (forward-line 1)))
  (goto-char (point-min)))

(defun locate-set-properties ()
  (save-excursion
    (let ((pos (locate-get-file-positions)))
      (dired-insert-set-properties (elt pos 0) (elt pos 1)))))

(defun locate-insert-header (search-string)
  ;; There needs to be a space before `Matches, because otherwise,
  ;; `*!" would erase the `M'.  We can not use two spaces, or the line
  ;; would mistakenly fit `dired-subdir-regexp'.
  (let ((locate-format-string "  /:\n Matches for %s")
	(locate-regexp-match
	 (concat " *Matches for \\(" (regexp-quote search-string) "\\)"))
	(locate-format-args (list search-string))
	)

    (and locate-fcodes-file
	(setq locate-format-string
	      (concat locate-format-string " in %s")
	      locate-regexp-match
	      (concat locate-regexp-match
		      " in \\("
		      (regexp-quote locate-fcodes-file)
		      "\\)")
	      locate-format-args
	      (append (list locate-fcodes-file) locate-format-args)))

    (and locate-current-filter
	(setq locate-format-string
	      (concat locate-format-string " using filter %s")
	      locate-regexp-match
	      (concat locate-regexp-match
		      " using filter "
		      "\\("
		      (regexp-quote locate-current-filter)
		      "\\)")
	      locate-format-args
	      (append (list locate-current-filter) locate-format-args)))

    (setq locate-format-string
	  (concat locate-format-string ":\n\n")
	  locate-regexp-match
	  (concat locate-regexp-match ":\n"))

    (insert (apply 'format locate-format-string (reverse locate-format-args)))

    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (if (not (looking-at locate-regexp-match))
	  nil
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'face locate-header-face))
	(and (match-beginning 2)
	     (add-text-properties (match-beginning 2) (match-end 2)
				  (list 'face locate-header-face)))
	(and (match-beginning 3)
	     (add-text-properties (match-beginning 3) (match-end 3)
				  (list 'face locate-header-face)))
	))))

(defun locate-tags ()
  "Visit a tags table in `*Locate*' mode."
  (interactive)
  (if (locate-main-listing-line-p)
      (let ((tags-table (locate-get-filename)))
	(and (y-or-n-p (format "Visit tags table %s? " tags-table))
	     (visit-tags-table tags-table)))
    (message "This command only works inside main listing.")))

;; From Stephen Eglen <stephen@cns.ed.ac.uk>
(defun locate-update (_ignore1 _ignore2)
  "Revert the *Locate* buffer.
If `locate-update-when-revert' is non-nil, offer to update the
locate database using the shell command in `locate-update-command'."
  (let ((locate-buffer-name (buffer-name))
	(locate-prompt-for-command locate-local-prompt))
    (and locate-update-when-revert
	 (yes-or-no-p "Update locate database (may take a few seconds)? ")
	 ;; `expand-file-name' is used in order to autoload Tramp if
	 ;; necessary.  It cannot be loaded when `default-directory'
	 ;; is remote.
	 (let ((default-directory (expand-file-name locate-update-path)))
	   (shell-command locate-update-command)))
    (locate locate-local-search locate-local-filter)))

;;; Modified three functions from `dired.el':
;;;   dired-find-directory,
;;;   dired-find-directory-other-window
;;;   dired-get-filename

(defun locate-find-directory ()
  "Visit the directory of the file mentioned on this line."
  (interactive)
  (if (locate-main-listing-line-p)
      (let ((directory-name (locate-get-dirname)))
	(if (file-directory-p directory-name)
	    (find-file directory-name)
	  (if (file-symlink-p directory-name)
	      (error "Directory is a symlink to a nonexistent target")
	    (error "Directory no longer exists; run `updatedb' to update database"))))
    (message "This command only works inside main listing.")))

(defun locate-find-directory-other-window ()
  "Visit the directory of the file named on this line in other window."
  (interactive)
  (if (locate-main-listing-line-p)
      (find-file-other-window (locate-get-dirname))
    (message "This command only works inside main listing.")))

;; You should only call this function on lines that contain a file name
;; listed by the locate program.  Inside inserted subdirectories, or if
;; there is no file name on the current line, the return value is
;; meaningless.  You can check whether the current line contains a file
;; listed by the locate program, using the function
;; `locate-main-listing-line-p'.
(defun locate-get-dirname ()
  "Return the directory name of the file mentioned on this line."
  (let (file (filepos (locate-get-file-positions)))
    (if (setq file (buffer-substring (nth 0 filepos) (nth 1 filepos)))
	(progn
	  ;; Get rid of the mouse-face property that file names have.
	  (set-text-properties 0 (length file) nil file)
	  (setq file (file-name-directory file))
	  ;; Unquote names quoted by ls or by dired-insert-directory.
	  ;; Using read to unquote is much faster than substituting
	  ;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
	  (setq file
		(read
		 (concat "\""
			 ;; some ls -b don't escape quotes, argh!
			 ;; This is not needed for GNU ls, though.
			 (or (dired-string-replace-match
			      "\\([^\\]\\|\\`\\)\"" file "\\1\\\\\"" nil t)
			     file)
			 "\"")))))
    (and file buffer-file-coding-system
	 (not file-name-coding-system)
	 (setq file (encode-coding-string file buffer-file-coding-system)))
    file))

;; Only for GNU locate
(defun locate-in-alternate-database  (search-string database)
  "Run the GNU locate program, using an alternate database.

This command only works if you use GNU locate.  It does not work
properly if `locate-prompt-for-command' is set to t.  In that
case, you can just run the regular `locate' command and specify
the database on the command line."
  (interactive
      (list
       (progn
	 ;; (require 'locate)
	 (read-from-minibuffer "Locate: " nil nil
			       nil 'locate-history-list))
       (read-file-name "Locate using Database: " )
       ))
  (or (file-exists-p database)
      (error "Database file %s does not exist" database))
  (let ((locate-make-command-line
	 (function (lambda (string)
		     (cons locate-command
			   (list (concat "--database="
					 (expand-file-name database))
				 string))))))
    (locate search-string)))

(defun locate-do-redisplay (&optional arg test-for-subdir)
  "Like `dired-do-redisplay', but adapted for `*Locate*' buffers."
  (interactive "P\np")
  (if (string= (dired-current-directory) "/")
      (message "This command only works in subdirectories.")
    (let ((dired-actual-switches locate-ls-subdir-switches))
      (dired-do-redisplay arg test-for-subdir))))

(provide 'locate)

;;; locate.el ends here
