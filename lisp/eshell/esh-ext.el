;;; esh-ext.el --- commands external to Eshell

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

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

;; To force a command to invoked external, either provide an explicit
;; pathname for the command argument, or prefix the command name with
;; an asterix character.  Example:
;;
;;   grep        ; make invoke `grep' Lisp function, or `eshell/grep'
;;   /bin/grep   ; will definitely invoke /bin/grep
;;   *grep        ; will also invoke /bin/grep

;;; Code:

(provide 'esh-ext)

(eval-when-compile
  (require 'cl)
  (require 'esh-cmd))
(require 'esh-util)

(defgroup eshell-ext nil
  "External commands are invoked when operating system executables are
loaded into memory, thus beginning a new process."
  :tag "External commands"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-ext-load-hook nil
  "A hook that gets run when `eshell-ext' is loaded."
  :version "24.1"			; removed eshell-ext-initialize
  :type 'hook
  :group 'eshell-ext)

(defcustom eshell-binary-suffixes exec-suffixes
  "A list of suffixes used when searching for executable files."
  :type '(repeat string)
  :group 'eshell-ext)

(defcustom eshell-force-execution nil
  "If non-nil, try to execute binary files regardless of permissions.
This can be useful on systems like Windows, where the operating system
doesn't happen to honor the permission bits in certain cases; or in
cases where you want to associate an interpreter with a particular
kind of script file, but the language won't let you but a '#!'
interpreter line in the file, and you don't want to make it executable
since nothing else but Eshell will be able to understand
`eshell-interpreter-alist'."
  :type 'boolean
  :group 'eshell-ext)

(defun eshell-search-path (name)
  "Search the environment path for NAME."
  (if (file-name-absolute-p name)
      name
    (let ((list (eshell-parse-colon-path eshell-path-env))
	  suffixes n1 n2 file)
      (while list
	(setq n1 (concat (car list) name))
	(setq suffixes eshell-binary-suffixes)
	(while suffixes
	  (setq n2 (concat n1 (car suffixes)))
	  (if (and (or (file-executable-p n2)
		       (and eshell-force-execution
			    (file-readable-p n2)))
		   (not (file-directory-p n2)))
	      (setq file n2 suffixes nil list nil))
	  (setq suffixes (cdr suffixes)))
	(setq list (cdr list)))
      file)))

(defcustom eshell-windows-shell-file
  (if (eshell-under-windows-p)
      (if (string-match "\\(cmdproxy\\|sh\\)\\.\\(com\\|exe\\)"
			shell-file-name)
	  (or (eshell-search-path "cmd.exe")
	      (eshell-search-path "command.com"))
	shell-file-name))
  "The name of the shell command to use for DOS/Windows batch files.
This defaults to nil on non-Windows systems, where this variable is
wholly ignored."
  :type '(choice file (const nil))
  :group 'eshell-ext)

(defsubst eshell-invoke-batch-file (&rest args)
  "Invoke a .BAT or .CMD file on DOS/Windows systems."
  ;; since CMD.EXE can't handle forward slashes in the initial
  ;; argument...
  (setcar args (subst-char-in-string ?/ ?\\ (car args)))
  (throw 'eshell-replace-command
	 (eshell-parse-command
	  (eshell-quote-argument eshell-windows-shell-file)
	  (cons "/c" args))))

(defcustom eshell-interpreter-alist
  (if (eshell-under-windows-p)
      '(("\\.\\(bat\\|cmd\\)\\'" . eshell-invoke-batch-file)))
  "An alist defining interpreter substitutions.
Each member is a cons cell of the form:

  (MATCH . INTERPRETER)

MATCH should be a regexp, which is matched against the command name,
or a function.  If either returns a non-nil value, then INTERPRETER
will be used for that command.

If INTERPRETER is a string, it will be called as the command name,
with the original command name passed as the first argument, with all
subsequent arguments following.  If INTERPRETER is a function, it will
be called with all of those arguments.  Note that interpreter
functions should throw `eshell-replace-command' with the alternate
command form, or they should return a value compatible with the
possible return values of `eshell-external-command', which see."
  :type '(repeat (cons (choice regexp (function :tag "Predicate"))
		       (choice string (function :tag "Interpreter"))))
  :group 'eshell-ext)

(defcustom eshell-alternate-command-hook nil
  "A hook run whenever external command lookup fails.
If a functions wishes to provide an alternate command, they must throw
it using the tag `eshell-replace-command'.  This is done because the
substituted command need not be external at all, and therefore must be
passed up to a higher level for re-evaluation.

Or, if the function returns a filename, that filename will be invoked
with the current command arguments rather than the command specified
by the user on the command line."
  :type 'hook
  :group 'eshell-ext)

(defcustom eshell-command-interpreter-max-length 256
  "The maximum length of any command interpreter string, plus args."
  :type 'integer
  :group 'eshell-ext)

(defcustom eshell-explicit-command-char ?*
  "If this char occurs before a command name, call it externally.
That is, although `vi' may be an alias, `\vi' will always call the
external version."
  :type 'character
  :group 'eshell-ext)

;;; Functions:

(defun eshell-ext-initialize ()
  "Initialize the external command handling code."
  (add-hook 'eshell-named-command-hook 'eshell-explicit-command nil t))

(defun eshell-explicit-command (command args)
  "If a command name begins with `*', call it externally always.
This bypasses all Lisp functions and aliases."
  (when (and (> (length command) 1)
	     (eq (aref command 0) eshell-explicit-command-char))
    (let ((cmd (eshell-search-path (substring command 1))))
      (if cmd
	  (or (eshell-external-command cmd args)
	      (error "%s: external command failed" cmd))
	(error "%s: external command not found"
	       (substring command 1))))))

(defun eshell-remote-command (command args)
  "Insert output from a remote COMMAND, using ARGS.
A remote command is something that executes on a different machine.
An external command simply means external to Emacs.

Note that this function is very crude at the moment.  It gathers up
all the output from the remote command, and sends it all at once,
causing the user to wonder if anything's really going on..."
  (let ((outbuf (generate-new-buffer " *eshell remote output*"))
	(errbuf (generate-new-buffer " *eshell remote error*"))
	(exitcode 1))
    (unwind-protect
	(progn
	  (setq exitcode
		(shell-command
		 (mapconcat 'shell-quote-argument
			    (append (list command) args) " ")
		 outbuf errbuf))
	  (eshell-print (with-current-buffer outbuf (buffer-string)))
	  (eshell-error (with-current-buffer errbuf (buffer-string))))
      (eshell-close-handles exitcode 'nil)
      (kill-buffer outbuf)
      (kill-buffer errbuf))))

(defun eshell-external-command (command args)
  "Insert output from an external COMMAND, using ARGS."
  (setq args (eshell-stringify-list (eshell-flatten-list args)))
  (if (file-remote-p default-directory)
      (eshell-remote-command command args))
  (let ((interp (eshell-find-interpreter command)))
    (assert interp)
    (if (functionp (car interp))
	(apply (car interp) (append (cdr interp) args))
      (eshell-gather-process-output
       (car interp) (append (cdr interp) args)))))

(defun eshell/addpath (&rest args)
  "Add a set of paths to PATH."
  (eshell-eval-using-options
   "addpath" args
   '((?b "begin" nil prepend "add path element at beginning")
     (?h "help" nil nil  "display this usage message")
     :usage "[-b] PATH
Adds the given PATH to $PATH.")
   (if args
       (progn
	 (if prepend
	     (setq args (nreverse args)))
	 (while args
	   (setenv "PATH"
		   (if prepend
		       (concat (car args) path-separator
			       (getenv "PATH"))
		     (concat (getenv "PATH") path-separator
			     (car args))))
	   (setq args (cdr args))))
     (let ((paths (parse-colon-path (getenv "PATH"))))
       (while paths
	 (eshell-printn (car paths))
	 (setq paths (cdr paths)))))))

(put 'eshell/addpath 'eshell-no-numeric-conversions t)

(defun eshell-script-interpreter (file)
  "Extract the script to run from FILE, if it has #!<interp> in it.
Return nil, or a list of the form:

  (INTERPRETER [ARGS] FILE)"
  (let ((maxlen eshell-command-interpreter-max-length))
    (if (and (file-readable-p file)
	     (file-regular-p file))
	(with-temp-buffer
	  (insert-file-contents-literally file nil 0 maxlen)
	  (if (looking-at "#![ \t]*\\([^ \r\t\n]+\\)\\([ \t]+\\(.+\\)\\)?")
	      (if (match-string 3)
		  (list (match-string 1)
			(match-string 3)
			file)
		(list (match-string 1)
		      file)))))))

(defun eshell-find-interpreter (file &optional no-examine-p)
  "Find the command interpreter with which to execute FILE.
If NO-EXAMINE-P is non-nil, FILE will not be inspected for a script
line of the form #!<interp>."
  (let ((finterp
	 (catch 'found
	   (ignore
	    (dolist (possible eshell-interpreter-alist)
	      (cond
	       ((functionp (car possible))
		(and (funcall (car possible) file)
		     (throw 'found (cdr possible))))
	       ((stringp (car possible))
		(and (string-match (car possible) file)
		     (throw 'found (cdr possible))))
	       (t
		(error "Invalid interpreter-alist test"))))))))
    (if finterp                         ; first check
	(list finterp file)
      (let ((fullname (if (file-name-directory file) file
			(eshell-search-path file)))
	    (suffixes eshell-binary-suffixes))
	(if (and fullname (not (or eshell-force-execution
				   (file-executable-p fullname))))
	    (while suffixes
	      (let ((try (concat fullname (car suffixes))))
		(if (or (file-executable-p try)
			(and eshell-force-execution
			     (file-readable-p try)))
		    (setq fullname try suffixes nil)
		  (setq suffixes (cdr suffixes))))))
	(cond ((not (and fullname (file-exists-p fullname)))
	       (let ((name (or fullname file)))
		 (unless (setq fullname
			       (run-hook-with-args-until-success
				'eshell-alternate-command-hook file))
		   (error "%s: command not found" name))))
	      ((not (or eshell-force-execution
			(file-executable-p fullname)))
	       (error "%s: Permission denied" fullname)))
	(let (interp)
	  (unless no-examine-p
	    (setq interp (eshell-script-interpreter fullname))
	    (if interp
		(setq interp
		      (cons (car (eshell-find-interpreter (car interp) t))
			    (cdr interp)))))
	  (or interp (list fullname)))))))

;;; esh-ext.el ends here
