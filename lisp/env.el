;;; env.el --- functions to manipulate environment variables

;; Copyright (C) 1991, 1994, 2000-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: processes, unix
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

;; UNIX processes inherit a list of name-to-string associations from their
;; parents called their `environment'; these are commonly used to control
;; program options.  This package permits you to set environment variables
;; to be passed to any sub-process run under Emacs.

;; Note that the environment string `process-environment' is not
;; decoded, but the args of `setenv' and `getenv' are normally
;; multibyte text and get coding conversion.

;;; Code:

(eval-when-compile (require 'cl))

;; History list for environment variable names.
(defvar read-envvar-name-history nil)

(defun read-envvar-name (prompt &optional mustmatch)
  "Read environment variable name, prompting with PROMPT.
Optional second arg MUSTMATCH, if non-nil, means require existing envvar name.
If it is also not t, RET does not exit if it does non-null completion."
  (completing-read prompt
		   (mapcar (lambda (enventry)
                             (let ((str (substring enventry 0
                                             (string-match "=" enventry))))
                               (if (multibyte-string-p str)
                                   (decode-coding-string
                                    str locale-coding-system t)
                                 str)))
			   (append process-environment
				   ;;(frame-environment)
				   ))
		   nil mustmatch nil 'read-envvar-name-history))

;; History list for VALUE argument to setenv.
(defvar setenv-history nil)


(defun substitute-env-vars (string)
  "Substitute environment variables referred to in STRING.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.  For instance, in `ab$cd-x',
`$cd' is treated as an environment variable.

Use `$$' to insert a single dollar sign."
  (let ((start 0))
    (while (string-match
	    (eval-when-compile
	      (rx (or (and "$" (submatch (1+ (regexp "[[:alnum:]_]"))))
		      (and "${" (submatch (minimal-match (0+ anything))) "}")
		      "$$")))
	    string start)
      (cond ((match-beginning 1)
	     (let ((value (getenv (match-string 1 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    ((match-beginning 2)
	     (let ((value (getenv (match-string 2 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    (t
	     (setq string (replace-match "$" t t string)
		   start (+ (match-beginning 0) 1)))))
    string))


(defun setenv-internal (env variable value keep-empty)
  "Set VARIABLE to VALUE in ENV, adding empty entries if KEEP-EMPTY.
Changes ENV by side-effect, and returns its new value."
  (let ((pattern (concat "\\`" (regexp-quote variable) "\\(=\\|\\'\\)"))
	(case-fold-search nil)
	(scan env)
	prev found)
    ;; Handle deletions from the beginning of the list specially.
    (if (and (null value)
	     (not keep-empty)
	     env
	     (stringp (car env))
	     (string-match pattern (car env)))
	(cdr env)
      ;; Try to find existing entry for VARIABLE in ENV.
      (while (and scan (stringp (car scan)))
	(when (string-match pattern (car scan))
	  (if value
	      (setcar scan (concat variable "=" value))
	    (if keep-empty
		(setcar scan variable)
	      (setcdr prev (cdr scan))))
	  (setq found t
		scan nil))
	(setq prev scan
	      scan (cdr scan)))
      (if (and (not found) (or value keep-empty))
	  (cons (if value
		    (concat variable "=" value)
		  variable)
		env)
	env))))

;; Fixme: Should the environment be recoded if LC_CTYPE &c is set?

(defun setenv (variable &optional value substitute-env-vars)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or
nil, the environment variable VARIABLE will be removed.

Interactively, a prefix argument means to unset the variable, and
otherwise the current value (if any) of the variable appears at
the front of the history list when you type in the new value.
This function always replaces environment variables in the new
value when called interactively.

SUBSTITUTE-ENV-VARS, if non-nil, means to substitute environment
variables in VALUE with `substitute-env-vars', which see.
This is normally used only for interactive calls.

The return value is the new value of VARIABLE, or nil if
it was removed from the environment.

This function works by modifying `process-environment'.

As a special case, setting variable `TZ' calls `set-time-zone-rule' as
a side-effect."
  (interactive
   (if current-prefix-arg
       (list (read-envvar-name "Clear environment variable: " 'exact) nil)
     (let* ((var (read-envvar-name "Set environment variable: " nil))
	    (value (getenv var)))
       (when value
	 (add-to-history 'setenv-history value))
       ;; Here finally we specify the args to give call setenv with.
       (list var
	     (read-from-minibuffer (format "Set %s to value: " var)
				   nil nil nil 'setenv-history
				   value)
	     t))))
  (if (and (multibyte-string-p variable) locale-coding-system)
      (let ((codings (find-coding-systems-string (concat variable value))))
	(unless (or (eq 'undecided (car codings))
		    (memq (coding-system-base locale-coding-system) codings))
	  (error "Can't encode `%s=%s' with `locale-coding-system'"
		 variable (or value "")))))
  (and value
       substitute-env-vars
       (setq value (substitute-env-vars value)))
  (if (multibyte-string-p variable)
      (setq variable (encode-coding-string variable locale-coding-system)))
  (if (and value (multibyte-string-p value))
      (setq value (encode-coding-string value locale-coding-system)))
  (if (string-match "=" variable)
      (error "Environment variable name `%s' contains `='" variable))
  (if (string-equal "TZ" variable)
      (set-time-zone-rule value))
  (setq process-environment (setenv-internal process-environment
                                             variable value t))
  value)

(defun getenv (variable &optional frame)
  "Get the value of environment variable VARIABLE.
VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
the environment.  Otherwise, value is a string.

If optional parameter FRAME is non-nil, then it should be a
frame.  This function will look up VARIABLE in its 'environment
parameter.

Otherwise, this function searches `process-environment' for
VARIABLE.  If it is not found there, then it continues the search
in the environment list of the selected frame."
  (interactive (list (read-envvar-name "Get environment variable: " t)))
  (let ((value (getenv-internal (if (multibyte-string-p variable)
				    (encode-coding-string
				     variable locale-coding-system)
				  variable)
				(and frame
				     (assq 'environment
					   (frame-parameters frame))))))
    (if (and enable-multibyte-characters value)
	(setq value (decode-coding-string value locale-coding-system)))
    (when (called-interactively-p 'interactive)
      (message "%s" (if value value "Not set")))
    value))

(provide 'env)

;;; env.el ends here
