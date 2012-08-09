;;; em-alias.el --- creation and management of command aliases

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

;; Command aliases greatly simplify the definition of new commands.
;; They exist as an alternative to alias functions, which are
;; otherwise quite superior, being more flexible and natural to the
;; Emacs Lisp environment (if somewhat trickier to define; [Alias
;; functions]).
;;
;;;_* Creating aliases
;;
;; The user interface is simple: type 'alias' followed by the command
;; name followed by the definition.  Argument references are made
;; using '$1', '$2', etc., or '$*'.  For example:
;;
;;   alias ll 'ls -l $*'
;;
;; This will cause the command 'll NEWS' to be replaced by 'ls -l
;; NEWS'.  This is then passed back to the command parser for
;; reparsing.{Only the command text specified in the alias definition
;; will be reparsed.  Argument references (such as '$*') are handled
;; using variable values, which means that the expansion will not be
;; reparsed, but used directly.}
;;
;; To delete an alias, specify its name without a definition:
;;
;;   alias ll
;;
;; Aliases are written to disk immediately after being defined or
;; deleted.  The filename in which they are kept is defined by the
;; variable eshell-aliases-file.

;; The format of this file is quite basic.  It specifies the alias
;; definitions in almost exactly the same way that the user entered
;; them, minus any argument quoting (since interpolation is not done
;; when the file is read).  Hence, it is possible to add new aliases
;; to the alias file directly, using a text editor rather than the
;; `alias' command.  Or, this method can be used for editing aliases
;; that have already defined.
;;
;; Here is an example of a few different aliases, and they would
;; appear in the aliases file:
;;
;;   alias clean rm -fr **/.#*~
;;   alias commit cvs commit -m changes $*
;;   alias ll ls -l $*
;;   alias info (info)
;;   alias reindex glimpseindex -o ~/Mail
;;   alias compact for i in ~/Mail/**/*~*.bz2(Lk+50) { bzip2 -9v $i }
;;
;;;_* Auto-correction of bad commands
;;
;; When a user enters the same unknown command many times during a
;; session, it is likely that they are experiencing a spelling
;; difficulty associated with a certain command.  To combat this,
;; Eshell will offer to automatically define an alias for that
;; misspelled command, once a given tolerance threshold has been
;; reached.

;; Whenever the same bad command name is encountered
;; `eshell-bad-command-tolerance' times, the user will be prompted in
;; the minibuffer to provide an alias name.  An alias definition will
;; then be created which will result in an equal call to the correct
;; name.  In this way, Eshell gradually learns about the commands that
;; the user mistypes frequently, and will automatically correct them!
;;
;; Note that a '$*' is automatically appended at the end of the alias
;; definition, so that entering it is unnecessary when specifying the
;; corrected command name.

;;; Code:

(eval-when-compile
  (require 'esh-util))
(require 'eshell)

;;;###autoload
(eshell-defgroup eshell-alias nil
  "Command aliases allow for easy definition of alternate commands."
  :tag "Command aliases"
  ;; :link '(info-link "(eshell)Command aliases")
  :group 'eshell-module)

(defcustom eshell-aliases-file (expand-file-name "alias" eshell-directory-name)
  "The file in which aliases are kept.
Whenever an alias is defined by the user, using the `alias' command,
it will be written to this file.  Thus, alias definitions (and
deletions) are always permanent.  This approach was chosen for the
sake of simplicity, since that's pretty much the only benefit to be
gained by using this module."
  :type 'file
  :group 'eshell-alias)

(defcustom eshell-bad-command-tolerance 3
  "The number of failed commands to ignore before creating an alias."
  :type 'integer
  ;; :link '(custom-manual "(eshell)Auto-correction of bad commands")
  :group 'eshell-alias)

(defcustom eshell-alias-load-hook nil
  "A hook that gets run when `eshell-alias' is loaded."
  :version "24.1"			; removed eshell-alias-initialize
  :type 'hook
  :group 'eshell-alias)

(defvar eshell-command-aliases-list nil
  "A list of command aliases currently defined by the user.
Each element of this alias is a list of the form:

  (NAME DEFINITION)

Where NAME is the textual name of the alias, and DEFINITION is the
command string to replace that command with.

Note: this list should not be modified in your '.emacs' file.  Rather,
any desired alias definitions should be declared using the `alias'
command, which will automatically write them to the file named by
`eshell-aliases-file'.")

(put 'eshell-command-aliases-list 'risky-local-variable t)

(defvar eshell-failed-commands-alist nil
  "An alist of command name failures.")

(defun eshell-alias-initialize ()
  "Initialize the alias handling code."
  (make-local-variable 'eshell-failed-commands-alist)
  (add-hook 'eshell-alternate-command-hook 'eshell-fix-bad-commands t t)
  (eshell-read-aliases-list)
  (add-hook 'eshell-named-command-hook 'eshell-maybe-replace-by-alias t t)
  (make-local-variable 'eshell-complex-commands)
  (add-to-list 'eshell-complex-commands 'eshell-command-aliased-p))

(defun eshell-command-aliased-p (name)
  (assoc name eshell-command-aliases-list))

(defun eshell/alias (&optional alias &rest definition)
  "Define an ALIAS in the user's alias list using DEFINITION."
  (if (not alias)
      (dolist (alias eshell-command-aliases-list)
	(eshell-print (apply 'format "alias %s %s\n" alias)))
    (if (not definition)
	(setq eshell-command-aliases-list
	      (delq (assoc alias eshell-command-aliases-list)
		    eshell-command-aliases-list))
      (and (stringp definition)
	   (set-text-properties 0 (length definition) nil definition))
      (let ((def (assoc alias eshell-command-aliases-list))
	    (alias-def (list alias
			     (eshell-flatten-and-stringify definition))))
	(if def
	    (setq eshell-command-aliases-list
		  (delq def eshell-command-aliases-list)))
	(setq eshell-command-aliases-list
	      (cons alias-def eshell-command-aliases-list))))
    (eshell-write-aliases-list))
  nil)

(defvar pcomplete-stub)
(autoload 'pcomplete-here "pcomplete")

(defun pcomplete/eshell-mode/alias ()
  "Completion function for Eshell's `alias' command."
  (pcomplete-here (eshell-alias-completions pcomplete-stub)))

(defun eshell-read-aliases-list ()
  "Read in an aliases list from `eshell-aliases-file'."
  (let ((file eshell-aliases-file))
    (when (file-readable-p file)
      (setq eshell-command-aliases-list
	    (with-temp-buffer
	      (let (eshell-command-aliases-list)
		(insert-file-contents file)
		(while (not (eobp))
		  (if (re-search-forward
		       "^alias\\s-+\\(\\S-+\\)\\s-+\\(.+\\)")
		      (setq eshell-command-aliases-list
			    (cons (list (match-string 1)
					(match-string 2))
				  eshell-command-aliases-list)))
		  (forward-line 1))
		eshell-command-aliases-list))))))

(defun eshell-write-aliases-list ()
  "Write out the current aliases into `eshell-aliases-file'."
  (if (file-writable-p (file-name-directory eshell-aliases-file))
      (let ((eshell-current-handles
	     (eshell-create-handles eshell-aliases-file 'overwrite)))
	(eshell/alias)
	(eshell-close-handles 0))))

(defsubst eshell-lookup-alias (name)
  "Check whether NAME is aliased.  Return the alias if there is one."
  (assoc name eshell-command-aliases-list))

(defvar eshell-prevent-alias-expansion nil)

(defun eshell-maybe-replace-by-alias (command args)
  "If COMMAND has an alias definition, call that instead using ARGS."
  (unless (and eshell-prevent-alias-expansion
	       (member command eshell-prevent-alias-expansion))
    (let ((alias (eshell-lookup-alias command)))
      (if alias
	  (throw 'eshell-replace-command
		 (list
		  'let
		  (list
		   (list 'eshell-command-name
			 (list 'quote eshell-last-command-name))
		   (list 'eshell-command-arguments
			 (list 'quote eshell-last-arguments))
		   (list 'eshell-prevent-alias-expansion
			 (list 'quote
			       (cons command
				     eshell-prevent-alias-expansion))))
		  (eshell-parse-command (nth 1 alias))))))))

(defun eshell-alias-completions (name)
  "Find all possible completions for NAME.
These are all the command aliases which begin with NAME."
  (let (completions)
    (dolist (alias eshell-command-aliases-list)
      (if (string-match (concat "^" name) (car alias))
	  (setq completions (cons (car alias) completions))))
    completions))

(defun eshell-fix-bad-commands (name)
  "If the user repeatedly a bad command NAME, make an alias for them."
  (ignore
   (unless (file-name-directory name)
     (let ((entry (assoc name eshell-failed-commands-alist)))
       (if (not entry)
	   (setq eshell-failed-commands-alist
		 (cons (cons name 1) eshell-failed-commands-alist))
	 (if (< (cdr entry) eshell-bad-command-tolerance)
	     (setcdr entry (1+ (cdr entry)))
	   (let ((alias (concat
			 (read-string
			  (format "Define alias for \"%s\": " name))
			 " $*")))
	     (eshell/alias name alias)
	     (throw 'eshell-replace-command
		    (list
		     'let
		     (list
		      (list 'eshell-command-name
			    (list 'quote name))
		      (list 'eshell-command-arguments
			    (list 'quote eshell-last-arguments))
		      (list 'eshell-prevent-alias-expansion
			    (list 'quote
				  (cons name
					eshell-prevent-alias-expansion))))
		     (eshell-parse-command alias))))))))))

(provide 'em-alias)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-alias.el ends here
