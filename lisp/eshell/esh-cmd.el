;;; esh-cmd.el --- command invocation

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

;;;_* Invoking external commands
;;
;; External commands cause processes to be created, by loading
;; external executables into memory.  This is what most normal shells
;; do, most of the time.  For more information, see [External commands].
;;
;;;_* Invoking Lisp functions
;;
;; A Lisp function can be invoked using Lisp syntax, or command shell
;; syntax.  For example, to run `dired' to edit the current directory:
;;
;;   /tmp $ (dired ".")
;;
;; Or:
;;
;;   /tmp $ dired .
;;
;; The latter form is preferable, but the former is more precise,
;; since it involves no translations.  See [Argument parsing], to
;; learn more about how arguments are transformed before passing them
;; to commands.
;;
;; Ordinarily, if 'dired' were also available as an external command,
;; the external version would be called in preference to any Lisp
;; function of the same name.  To change this behavior so that Lisp
;; functions always take precedence, set
;; `eshell-prefer-lisp-functions' to t.

;;;_* Alias functions
;;
;; Whenever a command is specified using a simple name, such as 'ls',
;; Eshell will first look for a Lisp function of the name `eshell/ls'.
;; If it exists, it will be called in preference to any other command
;; which might have matched the name 'ls' (such as command aliases,
;; external commands, Lisp functions of that name, etc).
;;
;; This is the most flexible mechanism for creating new commands,
;; since it does not pollute the global namespace, yet allows you to
;; use all of Lisp's facilities to define that piece of functionality.
;; Most of Eshell's "builtin" commands are defined as alias functions.
;;
;;;_* Lisp arguments
;;
;; It is possible to invoke a Lisp form as an argument.  This can be
;; done either by specifying the form as you might in Lisp, or by
;; using the '$' character to introduce a value-interpolation:
;;
;;   echo (+ 1 2)
;;
;; Or
;;
;;   echo $(+ 1 2)
;;
;; The two forms are equivalent.  The second is required only if the
;; form being interpolated is within a string, or is a subexpression
;; of a larger argument:
;;
;;   echo x$(+ 1 2) "String $(+ 1 2)"
;;
;; To pass a Lisp symbol as a argument, use the alternate quoting
;; syntax, since the single quote character is far too overused in
;; shell syntax:
;;
;;   echo #'lisp-symbol
;;
;; Backquote can also be used:
;;
;;   echo `(list ,lisp-symbol)
;;
;; Lisp arguments are identified using the following regexp:

;;;_* Command hooks
;;
;; There are several hooks involved with command execution, which can
;; be used either to change or augment Eshell's behavior.


;;; Code:

(require 'esh-util)
(unless (featurep 'xemacs)
  (require 'eldoc))
(require 'esh-arg)
(require 'esh-proc)
(require 'esh-ext)

(eval-when-compile
  (require 'cl)
  (require 'pcomplete))


(defgroup eshell-cmd nil
  "Executing an Eshell command is as simple as typing it in and
pressing <RET>.  There are several different kinds of commands,
however."
  :tag "Command invocation"
  ;; :link '(info-link "(eshell)Command invocation")
  :group 'eshell)

(defcustom eshell-prefer-lisp-functions nil
  "If non-nil, prefer Lisp functions to external commands."
  :type 'boolean
  :group 'eshell-cmd)

(defcustom eshell-lisp-regexp "\\([(`]\\|#'\\)"
  "A regexp which, if matched at beginning of an argument, means Lisp.
Such arguments will be passed to `read', and then evaluated."
  :type 'regexp
  :group 'eshell-cmd)

(defcustom eshell-pre-command-hook nil
  "A hook run before each interactive command is invoked."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-post-command-hook nil
  "A hook run after each interactive command is invoked."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-prepare-command-hook nil
  "A set of functions called to prepare a named command.
The command name and its argument are in `eshell-last-command-name'
and `eshell-last-arguments'.  The functions on this hook can change
the value of these symbols if necessary.

To prevent a command from executing at all, set
`eshell-last-command-name' to nil."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-named-command-hook nil
  "A set of functions called before a named command is invoked.
Each function will be passed the command name and arguments that were
passed to `eshell-named-command'.

If any of the functions returns a non-nil value, the named command
will not be invoked, and that value will be returned from
`eshell-named-command'.

In order to substitute an alternate command form for execution, the
hook function should throw it using the tag `eshell-replace-command'.
For example:

  (add-hook 'eshell-named-command-hook 'subst-with-cd)
  (defun subst-with-cd (command args)
    (throw 'eshell-replace-command
	   (eshell-parse-command \"cd\" args)))

Although useless, the above code will cause any non-glob, non-Lisp
command (i.e., 'ls' as opposed to '*ls' or '(ls)') to be replaced by a
call to `cd' using the arguments that were passed to the function."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-pre-rewrite-command-hook
  '(eshell-no-command-conversion
    eshell-subcommand-arg-values)
  "A hook run before command rewriting begins.
The terms of the command to be rewritten is passed as arguments, and
may be modified in place.  Any return value is ignored."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-rewrite-command-hook
  '(eshell-rewrite-for-command
    eshell-rewrite-while-command
    eshell-rewrite-if-command
    eshell-rewrite-sexp-command
    eshell-rewrite-initial-subcommand
    eshell-rewrite-named-command)
  "A set of functions used to rewrite the command argument.
Once parsing of a command line is completed, the next step is to
rewrite the initial argument into something runnable.

A module may wish to associate special behavior with certain argument
syntaxes at the beginning of a command line.  They are welcome to do
so by adding a function to this hook.  The first function to return a
substitute command form is the one used.  Each function is passed the
command's full argument list, which is a list of sexps (typically
forms or strings)."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-post-rewrite-command-hook nil
  "A hook run after command rewriting is finished.
Each function is passed the symbol containing the rewritten command,
which may be modified directly.  Any return value is ignored."
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-complex-commands '("ls")
  "A list of commands names or functions, that determine complexity.
That is, if a command is defined by a function named eshell/NAME,
and NAME is part of this list, it is invoked as a complex command.
Complex commands are always correct, but run much slower.  If a
command works fine without being part of this list, then it doesn't
need to be.

If an entry is a function, it will be called with the name, and should
return non-nil if the command is complex."
  :type '(repeat :tag "Commands"
		 (choice (string :tag "Name")
			 (function :tag "Predicate")))
  :group 'eshell-cmd)

;;; User Variables:

(defcustom eshell-cmd-load-hook nil
  "A hook that gets run when `eshell-cmd' is loaded."
  :version "24.1"		       ; removed eshell-cmd-initialize
  :type 'hook
  :group 'eshell-cmd)

(defcustom eshell-debug-command nil
  "If non-nil, enable debugging code.  SSLLOOWW.
This option is only useful for reporting bugs.  If you enable it, you
will have to visit the file 'eshell-cmd.el' and run the command
\\[eval-buffer]."
  :type 'boolean
  :group 'eshell-cmd)

(defcustom eshell-deferrable-commands
  '(eshell-named-command
    eshell-lisp-command
    eshell-process-identity)
  "A list of functions which might return an asynchronous process.
If they return a process object, execution of the calling Eshell
command will wait for completion (in the background) before finishing
the command."
  :type '(repeat function)
  :group 'eshell-cmd)

(defcustom eshell-subcommand-bindings
  '((eshell-in-subcommand-p t)
    (default-directory default-directory)
    (process-environment (eshell-copy-environment)))
  "A list of `let' bindings for subcommand environments."
  :type 'sexp
  :group 'eshell-cmd)

(put 'risky-local-variable 'eshell-subcommand-bindings t)

(defvar eshell-ensure-newline-p nil
  "If non-nil, ensure that a newline is emitted after a Lisp form.
This can be changed by Lisp forms that are evaluated from the Eshell
command line.")

;;; Internal Variables:

(defvar eshell-current-command nil)
(defvar eshell-command-name nil)
(defvar eshell-command-arguments nil)
(defvar eshell-in-pipeline-p nil
  "Internal Eshell variable, non-nil inside a pipeline.
Has the value 'first, 'last for the first/last commands in the pipeline,
otherwise t.")
(defvar eshell-in-subcommand-p nil)
(defvar eshell-last-arguments nil)
(defvar eshell-last-command-name nil)
(defvar eshell-last-async-proc nil
  "When this foreground process completes, resume command evaluation.")

;;; Functions:

(defsubst eshell-interactive-process ()
  "Return currently running command process, if non-Lisp."
  eshell-last-async-proc)

(defun eshell-cmd-initialize ()
  "Initialize the Eshell command processing module."
  (set (make-local-variable 'eshell-current-command) nil)
  (set (make-local-variable 'eshell-command-name) nil)
  (set (make-local-variable 'eshell-command-arguments) nil)
  (set (make-local-variable 'eshell-last-arguments) nil)
  (set (make-local-variable 'eshell-last-command-name) nil)
  (set (make-local-variable 'eshell-last-async-proc) nil)

  (add-hook 'eshell-kill-hook 'eshell-resume-command nil t)

  ;; make sure that if a command is over, and no process is being
  ;; waited for, that `eshell-current-command' is set to nil.  This
  ;; situation can occur, for example, if a Lisp function results in
  ;; `debug' being called, and the user then types \\[top-level]
  (add-hook 'eshell-post-command-hook
	    (function
	     (lambda ()
	       (setq eshell-current-command nil
		     eshell-last-async-proc nil))) nil t)

  (add-hook 'eshell-parse-argument-hook
	    'eshell-parse-subcommand-argument nil t)
  (add-hook 'eshell-parse-argument-hook
	    'eshell-parse-lisp-argument nil t)

  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      'eshell-complete-lisp-symbols nil t)))

(defun eshell-complete-lisp-symbols ()
  "If there is a user reference, complete it."
  (let ((arg (pcomplete-actual-arg)))
    (when (string-match (concat "\\`" eshell-lisp-regexp) arg)
      (setq pcomplete-stub (substring arg (match-end 0))
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions
	     (all-completions pcomplete-stub obarray 'boundp)))))

;; Command parsing

(defun eshell-parse-command (command &optional args top-level)
  "Parse the COMMAND, adding ARGS if given.
COMMAND can either be a string, or a cons cell demarcating a buffer
region.  TOP-LEVEL, if non-nil, means that the outermost command (the
user's input command) is being parsed, and that pre and post command
hooks should be run before and after the command."
  (let* (sep-terms
	 (terms
	  (append
	   (if (consp command)
	       (eshell-parse-arguments (car command) (cdr command))
	     (let ((here (point))
		   (inhibit-point-motion-hooks t))
               (with-silent-modifications
                 ;; FIXME: Why not use a temporary buffer and avoid this
                 ;; "insert&delete" business?  --Stef
                 (insert command)
                 (prog1
                     (eshell-parse-arguments here (point))
                   (delete-region here (point))))))
	   args))
	 (commands
	  (mapcar
	   (function
	    (lambda (cmd)
              (setq cmd
                    (if (or (not (car sep-terms))
                            (string= (car sep-terms) ";"))
			(eshell-parse-pipeline cmd (not (car sep-terms)))
		      `(eshell-do-subjob
                        (list ,(eshell-parse-pipeline cmd)))))
	      (setq sep-terms (cdr sep-terms))
	      (if eshell-in-pipeline-p
		  cmd
		`(eshell-trap-errors ,cmd))))
	   (eshell-separate-commands terms "[&;]" nil 'sep-terms))))
    (let ((cmd commands))
      (while cmd
	(if (cdr cmd)
	    (setcar cmd `(eshell-commands ,(car cmd))))
	(setq cmd (cdr cmd))))
    (setq commands
	  `(progn
             ,@(if top-level
                   '((run-hooks 'eshell-pre-command-hook)))
             ,@(if (not top-level)
                   commands
                 `((catch 'top-level (progn ,@commands))
                   (run-hooks 'eshell-post-command-hook)))))
    (if top-level
	`(eshell-commands ,commands)
      commands)))

(defun eshell-debug-command (tag subform)
  "Output a debugging message to '*eshell last cmd*'."
  (let ((buf (get-buffer-create "*eshell last cmd*"))
	(text (eshell-stringify eshell-current-command)))
    (with-current-buffer buf
      (if (not tag)
	  (erase-buffer)
	(insert "\n\C-l\n" tag "\n\n" text
		(if subform
		    (concat "\n\n" (eshell-stringify subform)) ""))))))

(defun eshell-debug-show-parsed-args (terms)
  "Display parsed arguments in the debug buffer."
  (ignore
   (if eshell-debug-command
       (eshell-debug-command "parsed arguments" terms))))

(defun eshell-no-command-conversion (terms)
  "Don't convert the command argument."
  (ignore
   (if (and (listp (car terms))
	    (eq (caar terms) 'eshell-convert))
       (setcar terms (cadr (car terms))))))

(defun eshell-subcommand-arg-values (terms)
  "Convert subcommand arguments {x} to ${x}, in order to take their values."
  (setq terms (cdr terms))		; skip command argument
  (while terms
    (if (and (listp (car terms))
	     (eq (caar terms) 'eshell-as-subcommand))
	(setcar terms `(eshell-convert
                        (eshell-command-to-value ,(car terms)))))
    (setq terms (cdr terms))))

(defun eshell-rewrite-sexp-command (terms)
  "Rewrite a sexp in initial position, such as '(+ 1 2)'."
  ;; this occurs when a Lisp expression is in first position
  (if (and (listp (car terms))
	   (eq (caar terms) 'eshell-command-to-value))
      (car (cdar terms))))

(defun eshell-rewrite-initial-subcommand (terms)
  "Rewrite a subcommand in initial position, such as '{+ 1 2}'."
  (if (and (listp (car terms))
	   (eq (caar terms) 'eshell-as-subcommand))
      (car terms)))

(defun eshell-rewrite-named-command (terms)
  "If no other rewriting rule transforms TERMS, assume a named command."
  (let ((sym (if eshell-in-pipeline-p
		 'eshell-named-command*
	       'eshell-named-command))
	(cmd (car terms))
	(args (cdr terms)))
    (if args
	(list sym cmd `(list ,@(cdr terms)))
      (list sym cmd))))

(defvar eshell-command-body)
(defvar eshell-test-body)

(defsubst eshell-invokify-arg (arg &optional share-output silent)
  "Change ARG so it can be invoked from a structured command.

SHARE-OUTPUT, if non-nil, means this invocation should share the
current output stream, which is separately redirectable.  SILENT
means the user and/or any redirections shouldn't see any output
from this command.  If both SHARE-OUTPUT and SILENT are non-nil,
the second is ignored."
  ;; something that begins with `eshell-convert' means that it
  ;; intends to return a Lisp value.  We want to get past this,
  ;; but if it's not _actually_ a value interpolation -- in which
  ;; we leave it alone.  In fact, the only time we muck with it
  ;; is in the case of a {subcommand} that has been turned into
  ;; the interpolation, ${subcommand}, by the parser because it
  ;; didn't know better.
  (if (and (listp arg)
	   (eq (car arg) 'eshell-convert)
	   (eq (car (cadr arg)) 'eshell-command-to-value))
      (if share-output
	  (cadr (cadr arg))
	`(eshell-commands ,(cadr (cadr arg)) ,silent))
    arg))

(defvar eshell-last-command-status)     ;Define in esh-io.el.

(defun eshell-rewrite-for-command (terms)
  "Rewrite a `for' command into its equivalent Eshell command form.
Because the implementation of `for' relies upon conditional evaluation
of its argument (i.e., use of a Lisp special form), it must be
implemented via rewriting, rather than as a function."
  (if (and (equal (car terms) "for")
	   (equal (nth 2 terms) "in"))
      (let ((body (car (last terms))))
	(setcdr (last terms 2) nil)
	`(let ((for-items
                (append
                 ,@(mapcar
                    (lambda (elem)
                      (if (listp elem)
                          elem
                        `(list ,elem)))
                    (cdr (cddr terms)))))
               (eshell-command-body '(nil))
               (eshell-test-body '(nil)))
           (while (consp for-items)
             (let ((,(intern (cadr terms)) (car for-items)))
               (eshell-protect
                ,(eshell-invokify-arg body t)))
             (setq for-items (cdr for-items)))
           (eshell-close-handles
            eshell-last-command-status
            (list 'quote eshell-last-command-result))))))

(defun eshell-structure-basic-command (func names keyword test body
					    &optional else vocal-test)
  "With TERMS, KEYWORD, and two NAMES, structure a basic command.
The first of NAMES should be the positive form, and the second the
negative.  It's not likely that users should ever need to call this
function.

If VOCAL-TEST is non-nil, it means output from the test should be
shown, as well as output from the body."
  ;; If the test form begins with `eshell-convert', it means
  ;; something data-wise will be returned, and we should let
  ;; that determine the truth of the statement.
  (unless (eq (car test) 'eshell-convert)
    (setq test
	  `(progn ,test
                  (eshell-exit-success-p))))

  ;; should we reverse the sense of the test?  This depends
  ;; on the `names' parameter.  If it's the symbol nil, yes.
  ;; Otherwise, it can be a pair of strings; if the keyword
  ;; we're using matches the second member of that pair (a
  ;; list), we should reverse it.
  (if (or (eq names nil)
	  (and (listp names)
	       (string= keyword (cadr names))))
      (setq test `(not ,test)))

  ;; finally, create the form that represents this structured
  ;; command
  `(let ((eshell-command-body '(nil))
         (eshell-test-body '(nil)))
     (,func ,test ,body ,else)
     (eshell-close-handles
        eshell-last-command-status
        (list 'quote eshell-last-command-result))))

(defun eshell-rewrite-while-command (terms)
  "Rewrite a `while' command into its equivalent Eshell command form.
Because the implementation of `while' relies upon conditional
evaluation of its argument (i.e., use of a Lisp special form), it
must be implemented via rewriting, rather than as a function."
  (if (and (stringp (car terms))
	   (member (car terms) '("while" "until")))
      (eshell-structure-basic-command
       'while '("while" "until") (car terms)
       (eshell-invokify-arg (cadr terms) nil t)
       `(eshell-protect
         ,(eshell-invokify-arg (car (last terms)) t)))))

(defun eshell-rewrite-if-command (terms)
  "Rewrite an `if' command into its equivalent Eshell command form.
Because the implementation of `if' relies upon conditional
evaluation of its argument (i.e., use of a Lisp special form), it
must be implemented via rewriting, rather than as a function."
  (if (and (stringp (car terms))
	   (member (car terms) '("if" "unless")))
      (eshell-structure-basic-command
       'if '("if" "unless") (car terms)
       (eshell-invokify-arg (cadr terms) nil t)
       `(eshell-protect
         ,(eshell-invokify-arg (car (last terms (if (= (length terms) 4) 2)))
                               t))
       (if (= (length terms) 4)
	   `(eshell-protect
             ,(eshell-invokify-arg (car (last terms)))) t))))

(defvar eshell-last-command-result)     ;Defined in esh-io.el.

(defun eshell-exit-success-p ()
  "Return non-nil if the last command was \"successful\".
For a bit of Lisp code, this means a return value of non-nil.
For an external command, it means an exit code of 0."
  (if (save-match-data
	(string-match "#<\\(Lisp object\\|function .*\\)>"
		      eshell-last-command-name))
      eshell-last-command-result
    (= eshell-last-command-status 0)))

(defun eshell-parse-pipeline (terms &optional final-p)
  "Parse a pipeline from TERMS, return the appropriate Lisp forms."
  (let* (sep-terms
	 (bigpieces (eshell-separate-commands terms "\\(&&\\|||\\)"
					      nil 'sep-terms))
	 (bp bigpieces)
	 (results (list t))
	 final)
    (while bp
      (let ((subterms (car bp)))
	(let* ((pieces (eshell-separate-commands subterms "|"))
	       (p pieces))
	  (while p
	    (let ((cmd (car p)))
	      (run-hook-with-args 'eshell-pre-rewrite-command-hook cmd)
	      (setq cmd (run-hook-with-args-until-success
			 'eshell-rewrite-command-hook cmd))
	      (run-hook-with-args 'eshell-post-rewrite-command-hook 'cmd)
	      (setcar p cmd))
	    (setq p (cdr p)))
	  (nconc results
		 (list
		  (if (<= (length pieces) 1)
		      (car pieces)
		    (assert (not eshell-in-pipeline-p))
		    `(eshell-execute-pipeline (quote ,pieces))))))
	(setq bp (cdr bp))))
    ;; `results' might be empty; this happens in the case of
    ;; multi-line input
    (setq results (cdr results)
	  results (nreverse results)
	  final (car results)
	  results (cdr results)
	  sep-terms (nreverse sep-terms))
    (while results
      (assert (car sep-terms))
      (setq final (eshell-structure-basic-command
		   'if (string= (car sep-terms) "&&") "if"
		   `(eshell-protect ,(car results))
		   `(eshell-protect ,final)
		   nil t)
	    results (cdr results)
	    sep-terms (cdr sep-terms)))
    final))

(defun eshell-parse-subcommand-argument ()
  "Parse a subcommand argument of the form '{command}'."
  (if (and (not eshell-current-argument)
	   (not eshell-current-quoted)
	   (eq (char-after) ?\{)
	   (or (= (point-max) (1+ (point)))
	       (not (eq (char-after (1+ (point))) ?\}))))
      (let ((end (eshell-find-delimiter ?\{ ?\})))
	(if (not end)
	    (throw 'eshell-incomplete ?\{)
	  (when (eshell-arg-delimiter (1+ end))
	    (prog1
		`(eshell-as-subcommand
                  ,(eshell-parse-command (cons (1+ (point)) end)))
	      (goto-char (1+ end))))))))

(defun eshell-parse-lisp-argument ()
  "Parse a Lisp expression which is specified as an argument."
  (if (and (not eshell-current-argument)
	   (not eshell-current-quoted)
	   (looking-at eshell-lisp-regexp))
      (let* ((here (point))
	     (obj
	      (condition-case err
		  (read (current-buffer))
		(end-of-file
		 (throw 'eshell-incomplete ?\()))))
	(if (eshell-arg-delimiter)
	    `(eshell-command-to-value
              (eshell-lisp-command (quote ,obj)))
	  (ignore (goto-char here))))))

(defun eshell-separate-commands (terms separator &optional
				       reversed last-terms-sym)
  "Separate TERMS using SEPARATOR.
If REVERSED is non-nil, the list of separated term groups will be
returned in reverse order.  If LAST-TERMS-SYM is a symbol, its value
will be set to a list of all the separator operators found (or '(list
nil)' if none)."
  (let ((sub-terms (list t))
	(eshell-sep-terms (list t))
	subchains)
    (while terms
      (if (and (consp (car terms))
	       (eq (caar terms) 'eshell-operator)
	       (string-match (concat "^" separator "$")
			     (nth 1 (car terms))))
	  (progn
	    (nconc eshell-sep-terms (list (nth 1 (car terms))))
	    (setq subchains (cons (cdr sub-terms) subchains)
		  sub-terms (list t)))
	(nconc sub-terms (list (car terms))))
      (setq terms (cdr terms)))
    (if (> (length sub-terms) 1)
	(setq subchains (cons (cdr sub-terms) subchains)))
    (if reversed
	(progn
	  (if last-terms-sym
	      (set last-terms-sym (reverse (cdr eshell-sep-terms))))
	  subchains)                    ; already reversed
      (if last-terms-sym
	  (set last-terms-sym (cdr eshell-sep-terms)))
      (nreverse subchains))))

;;_* Command evaluation macros
;;
;; The structure of the following macros is very important to
;; `eshell-do-eval' [Iterative evaluation]:
;;
;; @ Don't use forms that conditionally evaluate their arguments, such
;;   as `setq', `if', `while', `let*', etc.  The only special forms
;;   that can be used are `let', `condition-case' and
;;   `unwind-protect'.
;;
;; @ The main body of a `let' can contain only one form.  Use `progn'
;;   if necessary.
;;
;; @ The two `special' variables are `eshell-current-handles' and
;;   `eshell-current-subjob-p'.  Bind them locally with a `let' if you
;;   need to change them.  Change them directly only if your intention
;;   is to change the calling environment.

(defmacro eshell-do-subjob (object)
  "Evaluate a command OBJECT as a subjob.
We indicate that the process was run in the background by returning it
ensconced in a list."
  `(let ((eshell-current-subjob-p t))
     ,object))

(defmacro eshell-commands (object &optional silent)
  "Place a valid set of handles, and context, around command OBJECT."
  `(let ((eshell-current-handles
	  (eshell-create-handles ,(not silent) 'append))
	 eshell-current-subjob-p)
     ,object))

(defmacro eshell-trap-errors (object)
  "Trap any errors that occur, so they are not entirely fatal.
Also, the variable `eshell-this-command-hook' is available for the
duration of OBJECT's evaluation.  Note that functions should be added
to this hook using `nconc', and *not* `add-hook'.

Someday, when Scheme will become the dominant Emacs language, all of
this grossness will be made to disappear by using `call/cc'..."
  `(let ((eshell-this-command-hook '(ignore)))
     (eshell-condition-case err
	 (prog1
	     ,object
	   (run-hooks 'eshell-this-command-hook))
       (error
	(run-hooks 'eshell-this-command-hook)
	(eshell-errorn (error-message-string err))
	(eshell-close-handles 1)))))

(defvar eshell-output-handle)           ;Defined in esh-io.el.
(defvar eshell-error-handle)            ;Defined in esh-io.el.

(defmacro eshell-copy-handles (object)
  "Duplicate current I/O handles, so OBJECT works with its own copy."
  `(let ((eshell-current-handles
	  (eshell-create-handles
	   (car (aref eshell-current-handles
		      eshell-output-handle)) nil
	   (car (aref eshell-current-handles
		      eshell-error-handle)) nil)))
     ,object))

(defmacro eshell-protect (object)
  "Protect I/O handles, so they aren't get closed after eval'ing OBJECT."
  `(progn
     (eshell-protect-handles eshell-current-handles)
     ,object))

(defmacro eshell-do-pipelines (pipeline &optional notfirst)
  "Execute the commands in PIPELINE, connecting each to one another.
This macro calls itself recursively, with NOTFIRST non-nil."
  (when (setq pipeline (cadr pipeline))
    `(eshell-copy-handles
      (progn
	,(when (cdr pipeline)
	   `(let ((nextproc
		   (eshell-do-pipelines (quote ,(cdr pipeline)) t)))
              (eshell-set-output-handle ,eshell-output-handle
                                        'append nextproc)
              (eshell-set-output-handle ,eshell-error-handle
                                        'append nextproc)
              (setq tailproc (or tailproc nextproc))))
	,(let ((head (car pipeline)))
	   (if (memq (car head) '(let progn))
	       (setq head (car (last head))))
	   (when (memq (car head) eshell-deferrable-commands)
	     (ignore
	      (setcar head
		      (intern-soft
		       (concat (symbol-name (car head)) "*"))))))
	;; First and last elements in a pipeline may need special treatment.
	;; (Currently only eshell-ls-files uses 'last.)
	;; Affects process-connection-type in eshell-gather-process-output.
	(let ((eshell-in-pipeline-p
	       ,(cond ((not notfirst) (quote 'first))
		      ((cdr pipeline) t)
		      (t (quote 'last)))))
	  ,(car pipeline))))))

(defmacro eshell-do-pipelines-synchronously (pipeline)
  "Execute the commands in PIPELINE in sequence synchronously.
Output of each command is passed as input to the next one in the pipeline.
This is used on systems where `start-process' is not supported."
  (when (setq pipeline (cadr pipeline))
    `(progn
       ,(when (cdr pipeline)
          `(let ((output-marker ,(point-marker)))
             (eshell-set-output-handle ,eshell-output-handle
                                       'append output-marker)
             (eshell-set-output-handle ,eshell-error-handle
                                       'append output-marker)))
       ,(let ((head (car pipeline)))
          (if (memq (car head) '(let progn))
              (setq head (car (last head))))
          ;; FIXME: is deferrable significant here?
          (when (memq (car head) eshell-deferrable-commands)
            (ignore
             (setcar head
                     (intern-soft
                      (concat (symbol-name (car head)) "*"))))))
       ;; The last process in the pipe should get its handles
       ;; redirected as we found them before running the pipe.
       ,(if (null (cdr pipeline))
            `(progn
               (setq eshell-current-handles tail-handles)
               (setq eshell-in-pipeline-p nil)))
       (let ((result ,(car pipeline)))
         ;; tailproc gets the result of the last successful process in
         ;; the pipeline.
         (setq tailproc (or result tailproc))
         ,(if (cdr pipeline)
              `(eshell-do-pipelines-synchronously (quote ,(cdr pipeline))))
         result))))

(defalias 'eshell-process-identity 'identity)

(defmacro eshell-execute-pipeline (pipeline)
  "Execute the commands in PIPELINE, connecting each to one another."
  `(let ((eshell-in-pipeline-p t) tailproc)
     (progn
       ,(if (fboundp 'start-process)
	    `(eshell-do-pipelines ,pipeline)
	  `(let ((tail-handles (eshell-create-handles
				(car (aref eshell-current-handles
					   ,eshell-output-handle)) nil
				(car (aref eshell-current-handles
					   ,eshell-error-handle)) nil)))
	     (eshell-do-pipelines-synchronously ,pipeline)))
       (eshell-process-identity tailproc))))

(defmacro eshell-as-subcommand (command)
  "Execute COMMAND using a temp buffer.
This is used so that certain Lisp commands, such as `cd', when
executed in a subshell, do not disturb the environment of the main
Eshell buffer."
  `(let ,eshell-subcommand-bindings
     ,command))

(defmacro eshell-do-command-to-value (object)
  "Run a subcommand prepared by `eshell-command-to-value'.
This avoids the need to use `let*'."
  `(let ((eshell-current-handles
	  (eshell-create-handles value 'overwrite)))
     (progn
       ,object
       (symbol-value value))))

(defmacro eshell-command-to-value (object)
  "Run OBJECT synchronously, returning its result as a string.
Returns a string comprising the output from the command."
  `(let ((value (make-symbol "eshell-temp")))
     (eshell-do-command-to-value ,object)))

;;;_* Iterative evaluation
;;
;; Eshell runs all of its external commands asynchronously, so that
;; Emacs is not blocked while the operation is being performed.
;; However, this introduces certain synchronization difficulties,
;; since the Lisp code, once it returns, will not "go back" to finish
;; executing the commands which haven't yet been started.
;;
;; What Eshell does to work around this problem (basically, the lack
;; of threads in Lisp), is that it evaluates the command sequence
;; iteratively.  Whenever an asynchronous process is begun, evaluation
;; terminates and control is given back to Emacs.  When that process
;; finishes, it will resume the evaluation using the remainder of the
;; command tree.

(defun eshell/eshell-debug (&rest args)
  "A command for toggling certain debug variables."
  (ignore
   (cond
    ((not args)
     (if eshell-handle-errors
	 (eshell-print "errors\n"))
     (if eshell-debug-command
	 (eshell-print "commands\n")))
    ((member (car args) '("-h" "--help"))
     (eshell-print "usage: eshell-debug [kinds]

This command is used to aid in debugging problems related to Eshell
itself.  It is not useful for anything else.  The recognized `kinds'
at the moment are:

  errors       stops Eshell from trapping errors
  commands     shows command execution progress in `*eshell last cmd*'
"))
    (t
     (while args
       (cond
	((string= (car args) "errors")
	 (setq eshell-handle-errors (not eshell-handle-errors)))
	((string= (car args) "commands")
	 (setq eshell-debug-command (not eshell-debug-command))))
       (setq args (cdr args)))))))

(defun pcomplete/eshell-mode/eshell-debug ()
  "Completion for the `debug' command."
  (while (pcomplete-here '("errors" "commands"))))

(defun eshell-invoke-directly (command input)
  (let ((base (cadr (nth 2 (nth 2 (cadr command))))) name)
    (if (and (eq (car base) 'eshell-trap-errors)
	     (eq (car (cadr base)) 'eshell-named-command))
	(setq name (cadr (cadr base))))
    (and name (stringp name)
	 (not (member name eshell-complex-commands))
	 (catch 'simple
	   (progn
	    (dolist (pred eshell-complex-commands)
	      (if (and (functionp pred)
		       (funcall pred name))
		  (throw 'simple nil)))
	    t))
	 (fboundp (intern-soft (concat "eshell/" name))))))

(defun eshell-eval-command (command &optional input)
  "Evaluate the given COMMAND iteratively."
  (if eshell-current-command
      ;; we can just stick the new command at the end of the current
      ;; one, and everything will happen as it should
      (setcdr (last (cdr eshell-current-command))
	      (list `(let ((here (and (eobp) (point))))
                       ,(and input
                             `(insert-and-inherit ,(concat input "\n")))
                       (if here
                           (eshell-update-markers here))
                       (eshell-do-eval ',command))))
    (and eshell-debug-command
         (with-current-buffer (get-buffer-create "*eshell last cmd*")
           (erase-buffer)
           (insert "command: \"" input "\"\n")))
    (setq eshell-current-command command)
    (let ((delim (catch 'eshell-incomplete
		   (eshell-resume-eval))))
      ;; On systems that don't support async subprocesses, eshell-resume
      ;; can return t.  Don't treat that as an error.
      (if (listp delim)
	  (setq delim (car delim)))
      (if (and delim (not (eq delim t)))
	  (error "Unmatched delimiter: %c" delim)))))

(defun eshell-resume-command (proc status)
  "Resume the current command when a process ends."
  (when proc
    (unless (or (not (stringp status))
		(string= "stopped" status)
		(string-match eshell-reset-signals status))
      (if (eq proc (eshell-interactive-process))
	  (eshell-resume-eval)))))

(defun eshell-resume-eval ()
  "Destructively evaluate a form which may need to be deferred."
  (eshell-condition-case err
      (progn
	(setq eshell-last-async-proc nil)
	(when eshell-current-command
	  (let* (retval
		 (proc (catch 'eshell-defer
			 (ignore
			  (setq retval
				(eshell-do-eval
				 eshell-current-command))))))
	    (if (eshell-processp proc)
		(ignore (setq eshell-last-async-proc proc))
	      (cadr retval)))))
    (error
     (error (error-message-string err)))))

(defmacro eshell-manipulate (tag &rest commands)
  "Manipulate a COMMAND form, with TAG as a debug identifier."
  (declare (indent 1))
  ;; Check `bound'ness since at compile time the code until here has not
  ;; executed yet.
  (if (not (and (boundp 'eshell-debug-command) eshell-debug-command))
      `(progn ,@commands)
    `(progn
       (eshell-debug-command ,(eval tag) form)
       ,@commands
       (eshell-debug-command ,(concat "done " (eval tag)) form))))

(defsubst eshell-macrop (object)
  "Return t if OBJECT is a macro or nil otherwise."
  (and (symbolp object) (fboundp object)
       (setq object (indirect-function object))
       (listp object)
       (eq 'macro (car object))
       (functionp (cdr object))))

(defun eshell-do-eval (form &optional synchronous-p)
  "Evaluate form, simplifying it as we go.
Unless SYNCHRONOUS-P is non-nil, throws `eshell-defer' if it needs to
be finished later after the completion of an asynchronous subprocess."
  (cond
   ((not (listp form))
    (list 'quote (eval form)))
   ((memq (car form) '(quote function))
    form)
   (t
    ;; skip past the call to `eshell-do-eval'
    (when (eq (car form) 'eshell-do-eval)
      (setq form (cadr (cadr form))))
    ;; expand any macros directly into the form.  This is done so that
    ;; we can modify any `let' forms to evaluate only once.
    (if (eshell-macrop (car form))
	(let ((exp (eshell-copy-tree (macroexpand form))))
	  (eshell-manipulate (format "expanding macro `%s'"
				     (symbol-name (car form)))
	    (setcar form (car exp))
	    (setcdr form (cdr exp)))))
    (let ((args (cdr form)))
      (cond
       ((eq (car form) 'while)
	;; `eshell-copy-tree' is needed here so that the test argument
	;; doesn't get modified and thus always yield the same result.
	(when (car eshell-command-body)
	  (assert (not synchronous-p))
	  (eshell-do-eval (car eshell-command-body))
	  (setcar eshell-command-body nil)
	  (setcar eshell-test-body nil))
	(unless (car eshell-test-body)
	  (setcar eshell-test-body (eshell-copy-tree (car args))))
	(while (cadr (eshell-do-eval (car eshell-test-body)))
	  (setcar eshell-command-body
                  (if (cddr args)
                      `(progn ,@(eshell-copy-tree (cdr args)))
                    (eshell-copy-tree (cadr args))))
	  (eshell-do-eval (car eshell-command-body) synchronous-p)
	  (setcar eshell-command-body nil)
	  (setcar eshell-test-body (eshell-copy-tree (car args))))
	(setcar eshell-command-body nil))
       ((eq (car form) 'if)
	;; `eshell-copy-tree' is needed here so that the test argument
	;; doesn't get modified and thus always yield the same result.
	(if (car eshell-command-body)
	    (progn
	      (assert (not synchronous-p))
	      (eshell-do-eval (car eshell-command-body)))
	  (unless (car eshell-test-body)
	    (setcar eshell-test-body (eshell-copy-tree (car args))))
	  (setcar eshell-command-body
                  (eshell-copy-tree
                   (if (cadr (eshell-do-eval (car eshell-test-body)))
                       (cadr args)
                     (car (cddr args)))))
	  (eshell-do-eval (car eshell-command-body) synchronous-p))
	(setcar eshell-command-body nil)
	(setcar eshell-test-body nil))
       ((eq (car form) 'setcar)
	(setcar (cdr args) (eshell-do-eval (cadr args) synchronous-p))
	(eval form))
       ((eq (car form) 'setcdr)
	(setcar (cdr args) (eshell-do-eval (cadr args) synchronous-p))
	(eval form))
       ((memq (car form) '(let catch condition-case unwind-protect))
	;; `let', `condition-case' and `unwind-protect' have to be
	;; handled specially, because we only want to call
	;; `eshell-do-eval' on their first form.
	;;
	;; NOTE: This requires obedience by all forms which this
	;; function might encounter, that they do not contain
	;; other special forms.
	(if (and (eq (car form) 'let)
		 (not (eq (car (cadr args)) 'eshell-do-eval)))
	    (eshell-manipulate "evaluating let args"
	      (dolist (letarg (car args))
		(if (and (listp letarg)
			 (not (eq (cadr letarg) 'quote)))
		    (setcdr letarg
			    (list (eshell-do-eval
				   (cadr letarg) synchronous-p)))))))
	(unless (eq (car form) 'unwind-protect)
	  (setq args (cdr args)))
	(unless (eq (caar args) 'eshell-do-eval)
	  (eshell-manipulate "handling special form"
	    (setcar args `(eshell-do-eval ',(car args) ,synchronous-p))))
	(eval form))
       ((eq (car form) 'setq)
	(if (cddr args) (error "Unsupported form (setq X1 E1 X2 E2..)"))
        (eshell-manipulate "evaluating arguments to setq"
          (setcar (cdr args) (eshell-do-eval (cadr args) synchronous-p)))
	(list 'quote (eval form)))
       (t
	(if (and args (not (memq (car form) '(run-hooks))))
	    (eshell-manipulate
		(format "evaluating arguments to `%s'"
			(symbol-name (car form)))
	      (while args
		(setcar args (eshell-do-eval (car args) synchronous-p))
		(setq args (cdr args)))))
	(cond
	 ((eq (car form) 'progn)
	  (car (last form)))
	 ((eq (car form) 'prog1)
	  (cadr form))
	 (t
	  ;; If a command desire to replace its execution form with
	  ;; another command form, all it needs to do is throw the new
	  ;; form using the exception tag `eshell-replace-command'.
	  ;; For example, let's say that the form currently being
	  ;; eval'd is:
	  ;;
	  ;;   (eshell-named-command "hello")
	  ;;
	  ;; Now, let's assume the 'hello' command is an Eshell alias,
	  ;; the definition of which yields the command:
	  ;;
	  ;;   (eshell-named-command "echo" (list "Hello" "world"))
	  ;;
	  ;; What the alias code would like to do is simply substitute
	  ;; the alias form for the original form.  To accomplish
	  ;; this, all it needs to do is to throw the substitution
	  ;; form with the `eshell-replace-command' tag, and the form
	  ;; will be replaced within the current command, and
	  ;; execution will then resume (iteratively) as before.
	  ;; Thus, aliases can even contain references to asynchronous
	  ;; sub-commands, and things will still work out as they
	  ;; should.
	  (let* (result
                 (new-form
                  (catch 'eshell-replace-command
                    (ignore
                     (setq result (eval form))))))
	    (if new-form
		(progn
		  (eshell-manipulate "substituting replacement form"
		    (setcar form (car new-form))
		    (setcdr form (cdr new-form)))
		  (eshell-do-eval form synchronous-p))
	      (if (and (memq (car form) eshell-deferrable-commands)
		       (not eshell-current-subjob-p)
		       result
		       (eshell-processp result))
		  (if synchronous-p
		      (eshell/wait result)
		    (eshell-manipulate "inserting ignore form"
		      (setcar form 'ignore)
		      (setcdr form nil))
		    (throw 'eshell-defer result))
		(list 'quote result))))))))))))

;; command invocation

(defun eshell/which (command &rest names)
  "Identify the COMMAND, and where it is located."
  (dolist (name (cons command names))
    (let (program alias direct)
      (if (eq (aref name 0) eshell-explicit-command-char)
	  (setq name (substring name 1)
		direct t))
      (if (and (not direct)
	       (eshell-using-module 'eshell-alias)
	       (setq alias
		     (funcall (symbol-function 'eshell-lookup-alias)
			      name)))
	  (setq program
		(concat name " is an alias, defined as \""
			(cadr alias) "\"")))
      (unless program
	(setq program (eshell-search-path name))
	(let* ((esym (eshell-find-alias-function name))
	       (sym (or esym (intern-soft name))))
	  (if (and (or esym (and sym (fboundp sym)))
		   (or eshell-prefer-lisp-functions (not direct)))
	      (let ((desc (let ((inhibit-redisplay t))
			    (save-window-excursion
			      (prog1
				  (describe-function sym)
				(message nil))))))
		(setq desc (if desc (substring desc 0
					       (1- (or (string-match "\n" desc)
						       (length desc))))
			     ;; This should not happen.
			     (format "%s is defined, \
but no documentation was found" name)))
		(if (buffer-live-p (get-buffer "*Help*"))
		    (kill-buffer "*Help*"))
		(setq program (or desc name))))))
      (if (not program)
	  (eshell-error (format "which: no %s in (%s)\n"
				name (getenv "PATH")))
	(eshell-printn program)))))

(put 'eshell/which 'eshell-no-numeric-conversions t)

(defun eshell-named-command (command &optional args)
  "Insert output from a plain COMMAND, using ARGS.
COMMAND may result in an alias being executed, or a plain command."
  (setq eshell-last-arguments args
	eshell-last-command-name (eshell-stringify command))
  (run-hook-with-args 'eshell-prepare-command-hook)
  (assert (stringp eshell-last-command-name))
  (if eshell-last-command-name
      (or (run-hook-with-args-until-success
	   'eshell-named-command-hook eshell-last-command-name
	   eshell-last-arguments)
	  (eshell-plain-command eshell-last-command-name
				eshell-last-arguments))))

(defalias 'eshell-named-command* 'eshell-named-command)

(defun eshell-find-alias-function (name)
  "Check whether a function called `eshell/NAME' exists."
  (let* ((sym (intern-soft (concat "eshell/" name)))
	 (file (symbol-file sym 'defun)))
    ;; If the function exists, but is defined in an eshell module
    ;; that's not currently enabled, don't report it as found
    (if (and file
	     (string-match "\\(em\\|esh\\)-\\(.*\\)\\(\\.el\\)?\\'" file))
	(let ((module-sym
	       (intern (file-name-sans-extension
			(file-name-nondirectory
			 (concat "eshell-" (match-string 2 file)))))))
	  (if (and (functionp sym)
		   (or (null module-sym)
		       (eshell-using-module module-sym)
		       (memq module-sym (eshell-subgroups 'eshell))))
	      sym))
      ;; Otherwise, if it's bound, return it.
      (if (functionp sym)
	  sym))))

(defun eshell-plain-command (command args)
  "Insert output from a plain COMMAND, using ARGS.
COMMAND may result in either a Lisp function being executed by name,
or an external command."
  (let* ((esym (eshell-find-alias-function command))
	 (sym (or esym (intern-soft command))))
    (if (and sym (fboundp sym)
	     (or esym eshell-prefer-lisp-functions
		 (not (eshell-search-path command))))
	(eshell-lisp-command sym args)
      (eshell-external-command command args))))

(defun eshell-exec-lisp (printer errprint func-or-form args form-p)
  "Execute a lisp FUNC-OR-FORM, maybe passing ARGS.
PRINTER and ERRPRINT are functions to use for printing regular
messages, and errors.  FORM-P should be non-nil if FUNC-OR-FORM
represent a lisp form; ARGS will be ignored in that case."
  (eshell-condition-case err
      (let ((result
             (save-current-buffer
               (if form-p
                   (eval func-or-form)
                 (apply func-or-form args)))))
        (and result (funcall printer result))
        result)
    (error
     (let ((msg (error-message-string err)))
       (if (and (not form-p)
                (string-match "^Wrong number of arguments" msg)
                (fboundp 'eldoc-get-fnsym-args-string))
           (let ((func-doc (eldoc-get-fnsym-args-string func-or-form)))
             (setq msg (format "usage: %s" func-doc))))
       (funcall errprint msg))
     nil)))

(defsubst eshell-apply* (printer errprint func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
PRINTER and ERRPRINT are functions to use for printing regular
messages, and errors."
  (eshell-exec-lisp printer errprint func args nil))

(defsubst eshell-funcall* (printer errprint func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output."
  (eshell-apply* printer errprint func args))

(defsubst eshell-eval* (printer errprint form)
  "Evaluate FORM, trapping errors and returning them."
  (eshell-exec-lisp printer errprint form nil t))

(defsubst eshell-apply (func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
PRINTER and ERRPRINT are functions to use for printing regular
messages, and errors."
  (eshell-apply* 'eshell-print 'eshell-error func args))

(defsubst eshell-funcall (func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output."
  (eshell-apply func args))

(defsubst eshell-eval (form)
  "Evaluate FORM, trapping errors and returning them."
  (eshell-eval* 'eshell-print 'eshell-error form))

(defsubst eshell-applyn (func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
PRINTER and ERRPRINT are functions to use for printing regular
messages, and errors."
  (eshell-apply* 'eshell-printn 'eshell-errorn func args))

(defsubst eshell-funcalln (func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output."
  (eshell-applyn func args))

(defsubst eshell-evaln (form)
  "Evaluate FORM, trapping errors and returning them."
  (eshell-eval* 'eshell-printn 'eshell-errorn form))

(defvar eshell-last-output-end)         ;Defined in esh-mode.el.

(defun eshell-lisp-command (object &optional args)
  "Insert Lisp OBJECT, using ARGS if a function."
  (catch 'eshell-external               ; deferred to an external command
    (let* ((eshell-ensure-newline-p (eshell-interactive-output-p))
	   (result
	    (if (functionp object)
		(progn
		  (setq eshell-last-arguments args
			eshell-last-command-name
			(concat "#<function " (symbol-name object) ">"))
		  ;; if any of the arguments are flagged as numbers
		  ;; waiting for conversion, convert them now
		  (unless (get object 'eshell-no-numeric-conversions)
		    (while args
		      (let ((arg (car args)))
			(if (and (stringp arg)
				 (> (length arg) 0)
				 (not (text-property-not-all
				       0 (length arg) 'number t arg)))
			    (setcar args (string-to-number arg))))
		      (setq args (cdr args))))
		  (eshell-apply object eshell-last-arguments))
	      (setq eshell-last-arguments args
		    eshell-last-command-name "#<Lisp object>")
	      (eshell-eval object))))
      (if (and eshell-ensure-newline-p
	       (save-excursion
		 (goto-char eshell-last-output-end)
		 (not (bolp))))
	  (eshell-print "\n"))
      (eshell-close-handles 0 (list 'quote result)))))

(defalias 'eshell-lisp-command* 'eshell-lisp-command)

(provide 'esh-cmd)

;;; esh-cmd.el ends here
