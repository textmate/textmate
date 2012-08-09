;;; esh-arg.el --- argument processing

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

;; Parsing of arguments can be extended by adding functions to the
;; hook `eshell-parse-argument-hook'.  For a good example of this, see
;; `eshell-parse-drive-letter', defined in eshell-dirs.el.

(provide 'esh-arg)

(eval-when-compile (require 'eshell))

(defgroup eshell-arg nil
  "Argument parsing involves transforming the arguments passed on the
command line into equivalent Lisp forms that, when evaluated, will
yield the values intended."
  :tag "Argument parsing"
  :group 'eshell)

(defcustom eshell-parse-argument-hook
  (list
   ;; a term such as #<buffer NAME>, or #<process NAME> is a buffer
   ;; or process reference
   'eshell-parse-special-reference

   ;; numbers convert to numbers if they stand alone
   (function
    (lambda ()
      (when (and (not eshell-current-argument)
		 (not eshell-current-quoted)
		 (looking-at eshell-number-regexp)
		 (eshell-arg-delimiter (match-end 0)))
	(goto-char (match-end 0))
	(let ((str (match-string 0)))
	  (if (> (length str) 0)
	      (add-text-properties 0 (length str) '(number t) str))
	  str))))

   ;; parse any non-special characters, based on the current context
   (function
    (lambda ()
      (unless eshell-inside-quote-regexp
	(setq eshell-inside-quote-regexp
	      (format "[^%s]+"
		      (apply 'string eshell-special-chars-inside-quoting))))
      (unless eshell-outside-quote-regexp
	(setq eshell-outside-quote-regexp
	      (format "[^%s]+"
		      (apply 'string eshell-special-chars-outside-quoting))))
      (when (looking-at (if eshell-current-quoted
			    eshell-inside-quote-regexp
			  eshell-outside-quote-regexp))
	(goto-char (match-end 0))
	(let ((str (match-string 0)))
	  (if str
	      (set-text-properties 0 (length str) nil str))
	  str))))

   ;; whitespace or a comment is an argument delimiter
   (function
    (lambda ()
      (let (comment-p)
	(when (or (looking-at "[ \t]+")
		  (and (not eshell-current-argument)
		       (looking-at "#\\([^<'].*\\|$\\)")
		       (setq comment-p t)))
	  (if comment-p
	      (add-text-properties (match-beginning 0) (match-end 0)
				   '(comment t)))
	  (goto-char (match-end 0))
	  (eshell-finish-arg)))))

   ;; backslash before a special character means escape it
   'eshell-parse-backslash

   ;; text beginning with ' is a literally quoted
   'eshell-parse-literal-quote

   ;; text beginning with " is interpolably quoted
   'eshell-parse-double-quote

   ;; argument delimiter
   'eshell-parse-delimiter)
  "Define how to process Eshell command line arguments.
When each function on this hook is called, point will be at the
current position within the argument list.  The function should either
return nil, meaning that it did no argument parsing, or it should
return the result of the parse as a sexp.  It is also responsible for
moving the point forward to reflect the amount of input text that was
parsed.

If no function handles the current character at point, it will be
treated as a literal character."
  :type 'hook
  :group 'eshell-arg)

;;; Code:

;;; User Variables:

(defcustom eshell-arg-load-hook nil
  "A hook that gets run when `eshell-arg' is loaded."
  :version "24.1"		       ; removed eshell-arg-initialize
  :type 'hook
  :group 'eshell-arg)

(defcustom eshell-delimiter-argument-list '(?\; ?& ?\| ?\> ?\s ?\t ?\n)
  "List of characters to recognize as argument separators."
  :type '(repeat character)
  :group 'eshell-arg)

(defcustom eshell-special-chars-inside-quoting '(?\\ ?\")
  "Characters which are still special inside double quotes."
  :type '(repeat character)
  :group 'eshell-arg)

(defcustom eshell-special-chars-outside-quoting
  (append eshell-delimiter-argument-list '(?# ?! ?\\ ?\" ?\'))
  "Characters that require escaping outside of double quotes.
Without escaping them, they will introduce a change in the argument."
  :type '(repeat character)
  :group 'eshell-arg)

;;; Internal Variables:

(defvar eshell-current-argument nil)
(defvar eshell-current-modifiers nil)
(defvar eshell-arg-listified nil)
(defvar eshell-nested-argument nil)
(defvar eshell-current-quoted nil)
(defvar eshell-inside-quote-regexp nil)
(defvar eshell-outside-quote-regexp nil)

;;; Functions:

(defun eshell-arg-initialize ()
  "Initialize the argument parsing code."
  (define-key eshell-command-map [(meta ?b)] 'eshell-insert-buffer-name)
  (set (make-local-variable 'eshell-inside-quote-regexp) nil)
  (set (make-local-variable 'eshell-outside-quote-regexp) nil))

(defun eshell-insert-buffer-name (buffer-name)
  "Insert BUFFER-NAME into the current buffer at point."
  (interactive "BName of buffer: ")
  (insert-and-inherit "#<buffer " buffer-name ">"))

(defsubst eshell-escape-arg (string)
  "Return STRING with the `escaped' property on it."
  (if (stringp string)
      (add-text-properties 0 (length string) '(escaped t) string))
  string)

(defun eshell-resolve-current-argument ()
  "If there are pending modifications to be made, make them now."
  (when eshell-current-argument
    (when eshell-arg-listified
      (let ((parts eshell-current-argument))
	(while parts
	  (unless (stringp (car parts))
	    (setcar parts
		    (list 'eshell-to-flat-string (car parts))))
	  (setq parts (cdr parts)))
	(setq eshell-current-argument
	      (list 'eshell-convert
		    (append (list 'concat) eshell-current-argument))))
      (setq eshell-arg-listified nil))
    (while eshell-current-modifiers
      (setq eshell-current-argument
	    (list (car eshell-current-modifiers) eshell-current-argument)
	    eshell-current-modifiers (cdr eshell-current-modifiers))))
  (setq eshell-current-modifiers nil))

(defun eshell-finish-arg (&optional argument)
  "Finish the current argument being processed."
  (if argument
      (setq eshell-current-argument argument))
  (throw 'eshell-arg-done t))

(defsubst eshell-arg-delimiter (&optional pos)
  "Return non-nil if POS is an argument delimiter.
If POS is nil, the location of point is checked."
  (let ((pos (or pos (point))))
    (or (= pos (point-max))
	(memq (char-after pos) eshell-delimiter-argument-list))))

(defun eshell-quote-argument (string)
  "Return STRING with magic characters quoted.
Magic characters are those in `eshell-special-chars-outside-quoting'."
  (let ((index 0))
    (mapconcat (lambda (c)
		 (prog1
		     (or (eshell-quote-backslash string index)
			 (char-to-string c))
		   (setq index (1+ index))))
	       string
	       "")))

;; Argument parsing

(defun eshell-parse-arguments (beg end)
  "Parse all of the arguments at point from BEG to END.
Returns the list of arguments in their raw form.
Point is left at the end of the arguments."
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region beg end)
      (let ((inhibit-point-motion-hooks t)
	    (args (list t))
	    delim)
        (with-silent-modifications
          (remove-text-properties (point-min) (point-max)
                                  '(arg-begin nil arg-end nil))
          (if (setq
               delim
               (catch 'eshell-incomplete
                 (while (not (eobp))
                   (let* ((here (point))
                          (arg (eshell-parse-argument)))
                     (if (= (point) here)
                         (error "Failed to parse argument '%s'"
                                (buffer-substring here (point-max))))
                     (and arg (nconc args (list arg)))))))
              (throw 'eshell-incomplete (if (listp delim)
                                            delim
                                          (list delim (point) (cdr args)))))
          (cdr args))))))

(defun eshell-parse-argument ()
  "Get the next argument.  Leave point after it."
  (let* ((outer (null eshell-nested-argument))
	 (arg-begin (and outer (point)))
	 (eshell-nested-argument t)
	 eshell-current-argument
	 eshell-current-modifiers
	 eshell-arg-listified)
    (catch 'eshell-arg-done
      (while (not (eobp))
	(let ((result
	       (or (run-hook-with-args-until-success
		    'eshell-parse-argument-hook)
		   (prog1
		       (char-to-string (char-after))
		     (forward-char)))))
	  (if (not eshell-current-argument)
	      (setq eshell-current-argument result)
	    (unless eshell-arg-listified
	      (setq eshell-current-argument
		    (list eshell-current-argument)
		    eshell-arg-listified t))
	    (nconc eshell-current-argument (list result))))))
    (when (and outer eshell-current-argument)
      (add-text-properties arg-begin (1+ arg-begin)
			   '(arg-begin t rear-nonsticky
				       (arg-begin arg-end)))
      (add-text-properties (1- (point)) (point)
			   '(arg-end t rear-nonsticky
				     (arg-end arg-begin))))
    (eshell-resolve-current-argument)
    eshell-current-argument))

(defsubst eshell-operator (&rest args)
  "A stub function that generates an error if a floating operator is found."
  (error "Unhandled operator in input text"))

(defsubst eshell-looking-at-backslash-return (pos)
  "Test whether a backslash-return sequence occurs at POS."
  (and (eq (char-after pos) ?\\)
       (or (= (1+ pos) (point-max))
	   (and (eq (char-after (1+ pos)) ?\n)
		(= (+ pos 2) (point-max))))))

(defun eshell-quote-backslash (string &optional index)
  "Intelligently backslash the character occurring in STRING at INDEX.
If the character is itself a backslash, it needs no escaping."
  (let ((char (aref string index)))
    (if (and (eq char ?\\)
	     ;; In Emacs directory-sep-char is always ?/, so this does nothing.
	     (not (and (featurep 'xemacs)
		       (featurep 'mswindows)
		       (eq directory-sep-char ?\\)
		       (eq (1- (string-width string))
			   index))))
	(char-to-string char)
      (if (memq char eshell-special-chars-outside-quoting)
	  (string ?\\ char)))))

(defun eshell-parse-backslash ()
  "Parse a single backslash (\) character, which might mean escape.
It only means escape if the character immediately following is a
special character that is not itself a backslash."
  (when (eq (char-after) ?\\)
    (if (eshell-looking-at-backslash-return (point))
	(throw 'eshell-incomplete ?\\)
      (if (and (not (eq (char-after (1+ (point))) ?\\))
	       (if eshell-current-quoted
		   (memq (char-after (1+ (point)))
			 eshell-special-chars-inside-quoting)
		 (memq (char-after (1+ (point)))
		       eshell-special-chars-outside-quoting)))
	  (progn
	    (forward-char 2)
	    (list 'eshell-escape-arg
		  (char-to-string (char-before))))
	;; allow \\<RET> to mean a literal "\" character followed by a
	;; normal return, rather than a backslash followed by a line
	;; continuation (i.e., "\\ + \n" rather than "\ + \\n").  This
	;; is necessary because backslashes in Eshell are not special
	;; unless they either precede something special, or precede a
	;; backslash that precedes something special.  (Mainly this is
	;; done to make using backslash on Windows systems more
	;; natural-feeling).
	(if (eshell-looking-at-backslash-return (1+ (point)))
	    (forward-char))
	(forward-char)
	"\\"))))

(defun eshell-parse-literal-quote ()
  "Parse a literally quoted string.  Nothing has special meaning!"
  (if (eq (char-after) ?\')
      (let ((end (eshell-find-delimiter ?\' ?\')))
	(if (not end)
	    (throw 'eshell-incomplete ?\')
	  (let ((string (buffer-substring-no-properties (1+ (point)) end)))
	    (goto-char (1+ end))
	    (while (string-match "''" string)
	      (setq string (replace-match "'" t t string)))
	    (list 'eshell-escape-arg string))))))

(defun eshell-parse-double-quote ()
  "Parse a double quoted string, which allows for variable interpolation."
  (when (eq (char-after) ?\")
    (let* ((end (eshell-find-delimiter ?\" ?\" nil nil t))
	   (eshell-current-quoted t))
      (if (not end)
	  (throw 'eshell-incomplete ?\")
	(prog1
	    (save-restriction
	      (forward-char)
	      (narrow-to-region (point) end)
	      (let ((arg (eshell-parse-argument)))
		(if (eq arg nil)
		    ""
		  (list 'eshell-escape-arg arg))))
	  (goto-char (1+ end)))))))

(defun eshell-parse-special-reference ()
  "Parse a special syntax reference, of the form '#<type arg>'."
  (if (and (not eshell-current-argument)
	   (not eshell-current-quoted)
	   (looking-at "#<\\(buffer\\|process\\)\\s-"))
      (let ((here (point)))
	(goto-char (match-end 0))
	(let* ((buffer-p (string= (match-string 1) "buffer"))
	       (end (eshell-find-delimiter ?\< ?\>)))
	  (if (not end)
	      (throw 'eshell-incomplete ?\<)
	    (if (eshell-arg-delimiter (1+ end))
		(prog1
		    (list (if buffer-p 'get-buffer-create 'get-process)
			  (buffer-substring-no-properties (point) end))
		  (goto-char (1+ end)))
	      (ignore (goto-char here))))))))

(defun eshell-parse-delimiter ()
  "Parse an argument delimiter, which is essentially a command operator."
  ;; this `eshell-operator' keyword gets parsed out by
  ;; `eshell-separate-commands'.  Right now the only possibility for
  ;; error is an incorrect output redirection specifier.
  (when (looking-at "[&|;\n]\\s-*")
    (let ((end (match-end 0)))
    (if eshell-current-argument
	(eshell-finish-arg)
      (eshell-finish-arg
       (prog1
	   (list 'eshell-operator
		 (cond
		  ((eq (char-after end) ?\&)
		   (setq end (1+ end)) "&&")
		  ((eq (char-after end) ?\|)
		   (setq end (1+ end)) "||")
		  ((eq (char-after) ?\n) ";")
		  (t
		   (char-to-string (char-after)))))
	 (goto-char end)))))))

;;; esh-arg.el ends here
