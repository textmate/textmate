;;; esh-var.el --- handling of variables

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

;; These are the possible variable interpolation syntaxes.  Also keep
;; in mind that if an argument looks like a number, it will be
;; converted to a number.  This is not significant when invoking
;; external commands, but it's important when calling Lisp functions.
;;
;;   $VARIABLE
;;
;; Interval the value of an environment variable, or a Lisp variable
;;
;;   $ALSO-VAR
;;
;; "-" is a valid part of a variable name.
;;
;;   $<MYVAR>-TOO
;;
;; Only "MYVAR" is part of the variable name in this case.
;;
;;   $#VARIABLE
;;
;; Returns the length of the value of VARIABLE.  This could also be
;; done using the `length' Lisp function.
;;
;;   $(lisp)
;;
;; Returns result of lisp evaluation.  Note: Used alone like this, it
;; is identical to just saying (lisp); but with the variable expansion
;; form, the result may be interpolated a larger string, such as
;; '$(lisp)/other'.
;;
;;   ${command}
;;
;; Returns the value of an eshell subcommand.  See the note above
;; regarding Lisp evaluations.
;;
;;   $ANYVAR[10]
;;
;; Return the 10th element of ANYVAR.  If ANYVAR's value is a string,
;; it will be split in order to make it a list.  The splitting will
;; occur at whitespace.
;;
;;   $ANYVAR[: 10]
;;
;; As above, except that splitting occurs at the colon now.
;;
;;   $ANYVAR[: 10 20]
;;
;; As above, but instead of returning just a string, it now returns a
;; list of two strings.  If the result is being interpolated into a
;; larger string, this list will be flattened into one big string,
;; with each element separated by a space.
;;
;;   $ANYVAR["\\\\" 10]
;;
;; Separate on backslash characters.  Actually, the first argument --
;; if it doesn't have the form of a number, or a plain variable name
;; -- can be any regular expression.  So to split on numbers, use
;; '$ANYVAR["[0-9]+" 10 20]'.
;;
;;   $ANYVAR[hello]
;;
;; Calls `assoc' on ANYVAR with 'hello', expecting it to be an alist.
;;
;;   $#ANYVAR[hello]
;;
;; Returns the length of the cdr of the element of ANYVAR who car is
;; equal to "hello".
;;
;; There are also a few special variables defined by Eshell.  '$$' is
;; the value of the last command (t or nil, in the case of an external
;; command).  This makes it possible to chain results:
;;
;;   /tmp $ echo /var/spool/mail/johnw
;;   /var/spool/mail/johnw
;;   /tmp $ dirname $$
;;   /var/spool/mail/
;;   /tmp $ cd $$
;;   /var/spool/mail $
;;
;; '$_' refers to the last argument of the last command.  And $?
;; contains the exit code of the last command (0 or 1 for Lisp
;; functions, based on successful completion).

;;; Code:

(provide 'esh-var)

(eval-when-compile
  (require 'pcomplete)
  (require 'esh-util)
  (require 'esh-opt)
  (require 'esh-mode))
(require 'env)
(require 'ring)

(defgroup eshell-var nil
  "Variable interpolation is introduced whenever the '$' character
appears unquoted in any argument (except when that argument is
surrounded by single quotes).  It may be used to interpolate a
variable value, a subcommand, or even the result of a Lisp form."
  :tag "Variable handling"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-var-load-hook nil
  "A list of functions to call when loading `eshell-var'."
  :version "24.1"			; removed eshell-var-initialize
  :type 'hook
  :group 'eshell-var)

(defcustom eshell-prefer-lisp-variables nil
  "If non-nil, prefer Lisp variables to environment variables."
  :type 'boolean
  :group 'eshell-var)

(defcustom eshell-complete-export-definition t
  "If non-nil, completing names for `export' shows current definition."
  :type 'boolean
  :group 'eshell-var)

(defcustom eshell-modify-global-environment nil
  "If non-nil, using `export' changes Emacs's global environment."
  :type 'boolean
  :group 'eshell-var)

(defcustom eshell-variable-name-regexp "[A-Za-z0-9_-]+"
  "A regexp identifying what constitutes a variable name reference.
Note that this only applies for '$NAME'.  If the syntax '$<NAME>' is
used, then NAME can contain any character, including angle brackets,
if they are quoted with a backslash."
  :type 'regexp
  :group 'eshell-var)

(defcustom eshell-variable-aliases-list
  '(;; for eshell.el
    ("COLUMNS" (lambda (indices) (window-width)) t)
    ("LINES" (lambda (indices) (window-height)) t)

    ;; for eshell-cmd.el
    ("_" (lambda (indices)
	   (if (not indices)
	       (car (last eshell-last-arguments))
	     (eshell-apply-indices eshell-last-arguments
				   indices))))
    ("?" eshell-last-command-status)
    ("$" eshell-last-command-result)
    ("0" eshell-command-name)
    ("1" (lambda (indices) (nth 0 eshell-command-arguments)))
    ("2" (lambda (indices) (nth 1 eshell-command-arguments)))
    ("3" (lambda (indices) (nth 2 eshell-command-arguments)))
    ("4" (lambda (indices) (nth 3 eshell-command-arguments)))
    ("5" (lambda (indices) (nth 4 eshell-command-arguments)))
    ("6" (lambda (indices) (nth 5 eshell-command-arguments)))
    ("7" (lambda (indices) (nth 6 eshell-command-arguments)))
    ("8" (lambda (indices) (nth 7 eshell-command-arguments)))
    ("9" (lambda (indices) (nth 8 eshell-command-arguments)))
    ("*" (lambda (indices)
	   (if (not indices)
	       eshell-command-arguments
	     (eshell-apply-indices eshell-command-arguments
				   indices)))))
  "This list provides aliasing for variable references.
It is very similar in concept to what `eshell-user-aliases-list' does
for commands.  Each member of this defines defines the name of a
command, and the Lisp value to return for that variable if it is
accessed via the syntax '$NAME'.

If the value is a function, that function will be called with two
arguments: the list of the indices that was used in the reference, and
whether the user is requesting the length of the ultimate element.
For example, a reference of '$NAME[10][20]' would result in the
function for alias `NAME' being called (assuming it were aliased to a
function), and the arguments passed to this function would be the list
'(10 20)', and nil."
  :type '(repeat (list string sexp
		       (choice (const :tag "Copy to environment" t)
			       (const :tag "Use only in Eshell" nil))))
  :group 'eshell-var)

(put 'eshell-variable-aliases-list 'risky-local-variable t)

;;; Functions:

(defun eshell-var-initialize ()
  "Initialize the variable handle code."
  ;; Break the association with our parent's environment.  Otherwise,
  ;; changing a variable will affect all of Emacs.
  (unless eshell-modify-global-environment
    (set (make-local-variable 'process-environment)
	 (eshell-copy-environment)))

  (define-key eshell-command-map [(meta ?v)] 'eshell-insert-envvar)

  (set (make-local-variable 'eshell-special-chars-inside-quoting)
       (append eshell-special-chars-inside-quoting '(?$)))
  (set (make-local-variable 'eshell-special-chars-outside-quoting)
       (append eshell-special-chars-outside-quoting '(?$)))

  (add-hook 'eshell-parse-argument-hook 'eshell-interpolate-variable t t)

  (add-hook 'eshell-prepare-command-hook
	    'eshell-handle-local-variables nil t)

  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      'eshell-complete-variable-reference nil t)
    (add-hook 'pcomplete-try-first-hook
	      'eshell-complete-variable-assignment nil t)))

(defun eshell-handle-local-variables ()
  "Allow for the syntax 'VAR=val <command> <args>'."
  ;; strip off any null commands, which can only happen if a variable
  ;; evaluates to nil, such as "$var x", where `var' is nil.  The
  ;; command name in that case becomes `x', for compatibility with
  ;; most regular shells (the difference is that they do an
  ;; interpolation pass before the argument parsing pass, but Eshell
  ;; does both at the same time).
  (while (and (not eshell-last-command-name)
	      eshell-last-arguments)
    (setq eshell-last-command-name (car eshell-last-arguments)
	  eshell-last-arguments (cdr eshell-last-arguments)))
  (let ((setvar "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)\\'")
	(command (eshell-stringify eshell-last-command-name))
	(args eshell-last-arguments))
    ;; local variable settings (such as 'CFLAGS=-O2 make') are handled
    ;; by making the whole command into a subcommand, and calling
    ;; setenv immediately before the command is invoked.  This means
    ;; that 'BLAH=x cd blah' won't work exactly as expected, but that
    ;; is by no means a typical use of local environment variables.
    (if (and command (string-match setvar command))
	(throw
	 'eshell-replace-command
	 (list
	  'eshell-as-subcommand
	  (append
	   (list 'progn)
	   (let ((l (list t)))
	     (while (string-match setvar command)
	       (nconc
		l (list
		   (list 'setenv (match-string 1 command)
			 (match-string 2 command)
			 (= (length (match-string 2 command)) 0))))
	       (setq command (eshell-stringify (car args))
		     args (cdr args)))
	     (cdr l))
	   (list (list 'eshell-named-command
		       command (list 'quote args)))))))))

(defun eshell-interpolate-variable ()
  "Parse a variable interpolation.
This function is explicit for adding to `eshell-parse-argument-hook'."
  (when (and (eq (char-after) ?$)
	     (/= (1+ (point)) (point-max)))
    (forward-char)
    (list 'eshell-escape-arg
	  (eshell-parse-variable))))

(defun eshell/define (var-alias definition)
  "Define a VAR-ALIAS using DEFINITION."
  (if (not definition)
      (setq eshell-variable-aliases-list
	    (delq (assoc var-alias eshell-variable-aliases-list)
		  eshell-variable-aliases-list))
    (let ((def (assoc var-alias eshell-variable-aliases-list))
	  (alias-def
	   (list var-alias
		 (list 'quote (if (= (length definition) 1)
				  (car definition)
				definition)))))
      (if def
	  (setq eshell-variable-aliases-list
		(delq (assoc var-alias eshell-variable-aliases-list)
		      eshell-variable-aliases-list)))
      (setq eshell-variable-aliases-list
	    (cons alias-def
		  eshell-variable-aliases-list))))
  nil)

(defun eshell/export (&rest sets)
  "This alias allows the `export' command to act as bash users expect."
  (while sets
    (if (and (stringp (car sets))
	     (string-match "^\\([^=]+\\)=\\(.*\\)" (car sets)))
	(setenv (match-string 1 (car sets))
		(match-string 2 (car sets))))
    (setq sets (cdr sets))))

(defun pcomplete/eshell-mode/export ()
  "Completion function for Eshell's `export'."
  (while (pcomplete-here
	  (if eshell-complete-export-definition
	      process-environment
	    (eshell-envvar-names)))))

(defun eshell/unset (&rest args)
  "Unset an environment variable."
  (while args
    (if (stringp (car args))
	(setenv (car args) nil t))
    (setq args (cdr args))))

(defun pcomplete/eshell-mode/unset ()
  "Completion function for Eshell's `unset'."
  (while (pcomplete-here (eshell-envvar-names))))

(defun eshell/setq (&rest args)
  "Allow command-ish use of `setq'."
  (let (last-value)
    (while args
      (let ((sym (intern (car args)))
	    (val (cadr args)))
	(setq last-value (set sym val)
	      args (cddr args))))
    last-value))

(defun pcomplete/eshell-mode/setq ()
  "Completion function for Eshell's `setq'."
  (while (and (pcomplete-here (all-completions pcomplete-stub
					       obarray 'boundp))
	      (pcomplete-here))))

(defun eshell/env (&rest args)
  "Implementation of `env' in Lisp."
  (eshell-init-print-buffer)
  (eshell-eval-using-options
   "env" args
   '((?h "help" nil nil "show this usage screen")
     :external "env"
     :usage "<no arguments>")
   (dolist (setting (sort (eshell-environment-variables) 'string-lessp))
     (eshell-buffered-print setting "\n"))
   (eshell-flush)))

(defun eshell-insert-envvar (envvar-name)
  "Insert ENVVAR-NAME into the current buffer at point."
  (interactive
   (list (read-envvar-name "Name of environment variable: " t)))
  (insert-and-inherit "$" envvar-name))

(defun eshell-envvar-names (&optional environment)
  "Return a list of currently visible environment variable names."
  (mapcar (function
	   (lambda (x)
	     (substring x 0 (string-match "=" x))))
	  (or environment process-environment)))

(defun eshell-environment-variables ()
  "Return a `process-environment', fully updated.
This involves setting any variable aliases which affect the
environment, as specified in `eshell-variable-aliases-list'."
  (let ((process-environment (eshell-copy-environment)))
    (dolist (var-alias eshell-variable-aliases-list)
      (if (nth 2 var-alias)
	  (setenv (car var-alias)
		  (eshell-stringify
		   (or (eshell-get-variable (car var-alias)) "")))))
    process-environment))

(defun eshell-parse-variable ()
  "Parse the next variable reference at point.
The variable name could refer to either an environment variable, or a
Lisp variable.  The priority order depends on the setting of
`eshell-prefer-lisp-variables'.

Its purpose is to call `eshell-parse-variable-ref', and then to
process any indices that come after the variable reference."
  (let* ((get-len (when (eq (char-after) ?#)
		    (forward-char) t))
	 value indices)
    (setq value (eshell-parse-variable-ref)
	  indices (and (not (eobp))
		       (eq (char-after) ?\[)
		       (eshell-parse-indices))
	  value (list 'let
		      (list (list 'indices
				  (list 'quote indices)))
		      value))
    (if get-len
	(list 'length value)
      value)))

(defun eshell-parse-variable-ref ()
  "Eval a variable reference.
Returns a Lisp form which, if evaluated, will return the value of the
variable.

Possible options are:

  NAME          an environment or Lisp variable value
  <LONG-NAME>   disambiguates the length of the name
  {COMMAND}     result of command is variable's value
  (LISP-FORM)   result of Lisp form is variable's value"
  (let (end)
    (cond
     ((eq (char-after) ?{)
      (let ((end (eshell-find-delimiter ?\{ ?\})))
	(if (not end)
	    (throw 'eshell-incomplete ?\{)
	  (prog1
	      (list 'eshell-convert
		    (list 'eshell-command-to-value
			  (list 'eshell-as-subcommand
				(eshell-parse-command
				 (cons (1+ (point)) end)))))
	    (goto-char (1+ end))))))
     ((memq (char-after) '(?\' ?\"))
      (let ((name (if (eq (char-after) ?\')
		      (eshell-parse-literal-quote)
		    (eshell-parse-double-quote))))
	(if name
	  (list 'eshell-get-variable (eval name) 'indices))))
     ((eq (char-after) ?\<)
      (let ((end (eshell-find-delimiter ?\< ?\>)))
	(if (not end)
	    (throw 'eshell-incomplete ?\<)
	  (let* ((temp (make-temp-file temporary-file-directory))
		 (cmd (concat (buffer-substring (1+ (point)) end)
			      " > " temp)))
	    (prog1
		(list
		 'let (list (list 'eshell-current-handles
				  (list 'eshell-create-handles temp
					(list 'quote 'overwrite))))
		 (list
		  'progn
		  (list 'eshell-as-subcommand
			(eshell-parse-command cmd))
		  (list 'ignore
			(list 'nconc 'eshell-this-command-hook
			      (list 'list
				    (list 'function
					  (list 'lambda nil
						(list 'delete-file temp))))))
		  (list 'quote temp)))
	      (goto-char (1+ end)))))))
     ((eq (char-after) ?\()
      (condition-case err
	  (list 'eshell-command-to-value
		(list 'eshell-lisp-command
		      (list 'quote (read (current-buffer)))))
	(end-of-file
	 (throw 'eshell-incomplete ?\())))
     ((assoc (char-to-string (char-after))
	     eshell-variable-aliases-list)
      (forward-char)
      (list 'eshell-get-variable
	    (char-to-string (char-before)) 'indices))
     ((looking-at eshell-variable-name-regexp)
      (prog1
	  (list 'eshell-get-variable (match-string 0) 'indices)
	(goto-char (match-end 0))))
     (t
      (error "Invalid variable reference")))))

(defun eshell-parse-indices ()
  "Parse and return a list of list of indices."
  (let (indices)
    (while (eq (char-after) ?\[)
      (let ((end (eshell-find-delimiter ?\[ ?\])))
	(if (not end)
	    (throw 'eshell-incomplete ?\[)
	  (forward-char)
	  (let (eshell-glob-function)
	    (setq indices (cons (eshell-parse-arguments (point) end)
				indices)))
	  (goto-char (1+ end)))))
    (nreverse indices)))

(defun eshell-get-variable (name &optional indices)
  "Get the value for the variable NAME."
  (let* ((alias (assoc name eshell-variable-aliases-list))
	 (var (if alias
		  (cadr alias)
		name)))
    (if (and alias (functionp var))
	(funcall var indices)
      (eshell-apply-indices
       (cond
	((stringp var)
	 (let ((sym (intern-soft var)))
	   (if (and sym (boundp sym)
		    (or eshell-prefer-lisp-variables
			(not (getenv var))))
	       (symbol-value sym)
	     (getenv var))))
	((symbolp var)
	 (symbol-value var))
	(t
	 (error "Unknown variable `%s'" (eshell-stringify var))))
       indices))))

(defun eshell-apply-indices (value indices)
  "Apply to VALUE all of the given INDICES, returning the sub-result.
The format of INDICES is:

  ((INT-OR-NAME-OR-OTHER INT-OR-NAME INT-OR-NAME ...)
   ...)

Each member of INDICES represents a level of nesting.  If the first
member of a sublist is not an integer or name, and the value it's
reference is a string, that will be used as the regexp with which is
to divide the string into sub-parts.  The default is whitespace.
Otherwise, each INT-OR-NAME refers to an element of the list value.
Integers imply a direct index, and names, an associate lookup using
`assoc'.

For example, to retrieve the second element of a user's record in
'/etc/passwd', the variable reference would look like:

  ${egrep johnw /etc/passwd}[: 2]"
  (while indices
    (let ((refs (car indices)))
      (when (stringp value)
	(let (separator)
	  (if (not (or (not (stringp (caar indices)))
		       (string-match
			(concat "^" eshell-variable-name-regexp "$")
			(caar indices))))
	      (setq separator (caar indices)
		    refs (cdr refs)))
	  (setq value
		(mapcar 'eshell-convert
			(split-string value separator)))))
      (cond
       ((< (length refs) 0)
	(error "Invalid array variable index: %s"
	       (eshell-stringify refs)))
       ((= (length refs) 1)
	(setq value (eshell-index-value value (car refs))))
       (t
	(let ((new-value (list t)))
	  (while refs
	    (nconc new-value
		   (list (eshell-index-value value
					     (car refs))))
	    (setq refs (cdr refs)))
	  (setq value (cdr new-value))))))
    (setq indices (cdr indices)))
  value)

(defun eshell-index-value (value index)
  "Reference VALUE using the given INDEX."
  (if (stringp index)
      (cdr (assoc index value))
    (cond
     ((ring-p value)
      (if (> index (ring-length value))
	  (error "Index exceeds length of ring")
	(ring-ref value index)))
     ((listp value)
      (if (> index (length value))
	  (error "Index exceeds length of list")
	(nth index value)))
     ((vectorp value)
      (if (> index (length value))
	  (error "Index exceeds length of vector")
	(aref value index)))
     (t
      (error "Invalid data type for indexing")))))

;;;_* Variable name completion

(defun eshell-complete-variable-reference ()
  "If there is a variable reference, complete it."
  (let ((arg (pcomplete-actual-arg)) index)
    (when (setq index
		(string-match
		 (concat "\\$\\(" eshell-variable-name-regexp
			 "\\)?\\'") arg))
      (setq pcomplete-stub (substring arg (1+ index)))
      (throw 'pcomplete-completions (eshell-variables-list)))))

(defun eshell-variables-list ()
  "Generate list of applicable variables."
  (let ((argname pcomplete-stub)
	completions)
    (dolist (alias eshell-variable-aliases-list)
      (if (string-match (concat "^" argname) (car alias))
	  (setq completions (cons (car alias) completions))))
    (sort
     (append
      (mapcar
       (function
	(lambda (varname)
	  (let ((value (eshell-get-variable varname)))
	    (if (and value
		     (stringp value)
		     (file-directory-p value))
		(concat varname "/")
	      varname))))
       (eshell-envvar-names (eshell-environment-variables)))
      (all-completions argname obarray 'boundp)
      completions)
     'string-lessp)))

(defun eshell-complete-variable-assignment ()
  "If there is a variable assignment, allow completion of entries."
  (let ((arg (pcomplete-actual-arg)) pos)
    (when (string-match (concat "\\`" eshell-variable-name-regexp "=") arg)
      (setq pos (match-end 0))
      (if (string-match "\\(:\\)[^:]*\\'" arg)
	  (setq pos (match-end 1)))
      (setq pcomplete-stub (substring arg pos))
      (throw 'pcomplete-completions (pcomplete-entries)))))

;;; esh-var.el ends here
