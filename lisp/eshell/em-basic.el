;;; em-basic.el --- basic shell builtin commands

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

;; There are very few basic Eshell commands -- so-called builtins.
;; They are: echo, umask, and version.
;;
;;;_* `echo'
;;
;; The `echo' command repeats its arguments to the screen.  It is
;; optional whether this is done in a Lisp-friendly fashion (so that
;; the value of echo is useful to a Lisp command using the result of
;; echo as an argument), or whether it should try to act like a normal
;; shell echo, and always result in a flat string being returned.

;; An example of the difference is the following:
;;
;;   echo Hello world
;;
;; If `eshell-plain-echo-behavior' is non-nil, this will yield the
;; string "Hello world".  If Lisp behavior is enabled, however, it
;; will yield a list whose two elements are the strings "Hello" and
;; "world".  The way to write an equivalent expression for both would
;; be:
;;
;;   echo "Hello world"
;;
;; This always returns a single string.
;;
;;;_* `umask'
;;
;; The umask command changes the default file permissions for newly
;; created files.  It uses the same syntax as bash.
;;
;;;_* `version'
;;
;; This command reports the version number for Eshell and all its
;; dependent module, including the date when those modules were last
;; modified.

;;; Code:

(eval-when-compile
  (require 'esh-util))

(require 'eshell)
(require 'esh-opt)

;;;###autoload
(eshell-defgroup eshell-basic nil
  "The \"basic\" code provides a set of convenience functions which
are traditionally considered shell builtins.  Since all of the
functionality provided by them is accessible through Lisp, they are
not really builtins at all, but offer a command-oriented way to do the
same thing."
  :tag "Basic shell commands"
  :group 'eshell-module)

(defcustom eshell-plain-echo-behavior nil
  "If non-nil, `echo' tries to behave like an ordinary shell echo.
This comes at some detriment to Lisp functionality.  However, the Lisp
equivalent of `echo' can always be achieved by using `identity'."
  :type 'boolean
  :group 'eshell-basic)

;;; Functions:

(defun eshell-echo (args &optional output-newline)
  "Implementation code for a Lisp version of `echo'.
It returns a formatted value that should be passed to `eshell-print'
or `eshell-printn' for display."
  (if eshell-plain-echo-behavior
      (concat (apply 'eshell-flatten-and-stringify args) "\n")
    (let ((value
	   (cond
	    ((= (length args) 0) "")
	    ((= (length args) 1)
	     (car args))
	    (t
	     (mapcar
	      (function
	       (lambda (arg)
		 (if (stringp arg)
		     (set-text-properties 0 (length arg) nil arg))
		 arg))
	      args)))))
      (if output-newline
	  (cond
	   ((stringp value)
	    (concat value "\n"))
	   ((listp value)
	    (append value (list "\n")))
	   (t
	    (concat (eshell-stringify value) "\n")))
	value))))

(defun eshell/echo (&rest args)
  "Implementation of `echo'.  See `eshell-plain-echo-behavior'."
  (eshell-eval-using-options
   "echo" args
   '((?n nil nil output-newline "terminate with a newline")
     (?h "help" nil nil "output this help screen")
     :preserve-args
     :usage "[-n] [object]")
   (eshell-echo args output-newline)))

(defun eshell/printnl (&rest args)
  "Print out each of the arguments, separated by newlines."
  (let ((elems (eshell-flatten-list args)))
    (while elems
      (eshell-printn (eshell-echo (list (car elems))))
      (setq elems (cdr elems)))))

(defun eshell/listify (&rest args)
  "Return the argument(s) as a single list."
  (if (> (length args) 1)
      args
    (if (listp (car args))
	(car args)
      (list (car args)))))

(defun eshell/umask (&rest args)
  "Shell-like implementation of `umask'."
  (eshell-eval-using-options
   "umask" args
   '((?S "symbolic" nil symbolic-p "display umask symbolically")
     (?h "help" nil nil  "display this usage message")
     :usage "[-S] [mode]")
   (if (or (not args) symbolic-p)
       (let ((modstr
	      (concat "000"
		      (format "%o"
			      (logand (lognot (default-file-modes))
				      511)))))
	 (setq modstr (substring modstr (- (length modstr) 3)))
	 (when symbolic-p
	   (let ((mode (default-file-modes)))
	     (setq modstr
		   (format
		    "u=%s,g=%s,o=%s"
		    (concat (and (= (logand mode 64) 64) "r")
			    (and (= (logand mode 128) 128) "w")
			    (and (= (logand mode 256) 256) "x"))
		    (concat (and (= (logand mode 8) 8) "r")
			    (and (= (logand mode 16) 16) "w")
			    (and (= (logand mode 32) 32) "x"))
		    (concat (and (= (logand mode 1) 1) "r")
			    (and (= (logand mode 2) 2) "w")
			    (and (= (logand mode 4) 4) "x"))))))
	 (eshell-printn modstr))
     (setcar args (eshell-convert (car args)))
     (if (numberp (car args))
	 (set-default-file-modes
	  (- 511 (car (read-from-string
		       (concat "?\\" (number-to-string (car args)))))))
       (error "setting umask symbolically is not yet implemented"))
     (eshell-print
      "Warning: umask changed for all new files created by Emacs.\n"))
   nil))

(provide 'em-basic)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-basic.el ends here
