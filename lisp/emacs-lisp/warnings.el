;;; warnings.el --- log and display warnings

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; This file implements the entry points `warn', `lwarn'
;; and `display-warning'.

;;; Code:

(defgroup warnings nil
  "Log and display warnings."
  :version "22.1"
  :group 'lisp)

(defvar warning-levels
  '((:emergency "Emergency%s: " ding)
    (:error "Error%s: ")
    (:warning "Warning%s: ")
    (:debug "Debug%s: "))
  "List of severity level definitions for `display-warning'.
Each element looks like (LEVEL STRING FUNCTION) and
defines LEVEL as a severity level.  STRING specifies the
description of this level.  STRING should use `%s' to
specify where to put the warning type information,
or it can omit the `%s' so as not to include that information.

The optional FUNCTION, if non-nil, is a function to call
with no arguments, to get the user's attention.

The standard levels are :emergency, :error, :warning and :debug.
See `display-warning' for documentation of their meanings.
Level :debug is ignored by default (see `warning-minimum-level').")
(put 'warning-levels 'risky-local-variable t)

;; These are for compatibility with XEmacs.
;; I don't think there is any chance of designing meaningful criteria
;; to distinguish so many levels.
(defvar warning-level-aliases
  '((emergency . :emergency)
    (error . :error)
    (warning . :warning)
    (notice . :warning)
    (info . :warning)
    (critical . :emergency)
    (alarm . :emergency))
  "Alist of aliases for severity levels for `display-warning'.
Each element looks like (ALIAS . LEVEL) and defines ALIAS as
equivalent to LEVEL.  LEVEL must be defined in `warning-levels';
it may not itself be an alias.")

(defcustom warning-minimum-level :warning
  "Minimum severity level for displaying the warning buffer.
If a warning's severity level is lower than this,
the warning is logged in the warnings buffer, but the buffer
is not immediately displayed.  See also `warning-minimum-log-level'."
  :group 'warnings
  :type '(choice (const :emergency) (const :error)
                 (const :warning) (const :debug))
  :version "22.1")
(defvaralias 'display-warning-minimum-level 'warning-minimum-level)

(defcustom warning-minimum-log-level :warning
  "Minimum severity level for logging a warning.
If a warning severity level is lower than this,
the warning is completely ignored.
Value must be lower or equal than `warning-minimum-level',
because warnings not logged aren't displayed either."
  :group 'warnings
  :type '(choice (const :emergency) (const :error)
                 (const :warning) (const :debug))
  :version "22.1")
(defvaralias 'log-warning-minimum-level 'warning-minimum-log-level)

(defcustom warning-suppress-log-types nil
  "List of warning types that should not be logged.
If any element of this list matches the TYPE argument to `display-warning',
the warning is completely ignored.
The element must match the first elements of TYPE.
Thus, (foo bar) as an element matches (foo bar)
or (foo bar ANYTHING...) as TYPE.
If TYPE is a symbol FOO, that is equivalent to the list (FOO),
so only the element (FOO) will match it."
  :group 'warnings
  :type '(repeat (repeat symbol))
  :version "22.1")

(defcustom warning-suppress-types nil
  "List of warning types not to display immediately.
If any element of this list matches the TYPE argument to `display-warning',
the warning is logged nonetheless, but the warnings buffer is
not immediately displayed.
The element must match an initial segment of the list TYPE.
Thus, (foo bar) as an element matches (foo bar)
or (foo bar ANYTHING...) as TYPE.
If TYPE is a symbol FOO, that is equivalent to the list (FOO),
so only the element (FOO) will match it.
See also `warning-suppress-log-types'."
  :group 'warnings
  :type '(repeat (repeat symbol))
  :version "22.1")

;; The autoload cookie is so that programs can bind this variable
;; safely, testing the existing value, before they call one of the
;; warnings functions.
;;;###autoload
(defvar warning-prefix-function nil
  "Function to generate warning prefixes.
This function, if non-nil, is called with two arguments,
the severity level and its entry in `warning-levels',
and should return the entry that should actually be used.
The warnings buffer is current when this function is called
and the function can insert text in it.  This text becomes
the beginning of the warning.")

;; The autoload cookie is so that programs can bind this variable
;; safely, testing the existing value, before they call one of the
;; warnings functions.
;;;###autoload
(defvar warning-series nil
  "Non-nil means treat multiple `display-warning' calls as a series.
A marker indicates a position in the warnings buffer
which is the start of the current series; it means that
additional warnings in the same buffer should not move point.
If t, the next warning begins a series (and stores a marker here).
A symbol with a function definition is like t, except
also call that function before the next warning.")
(put 'warning-series 'risky-local-variable t)

;; The autoload cookie is so that programs can bind this variable
;; safely, testing the existing value, before they call one of the
;; warnings functions.
;;;###autoload
(defvar warning-fill-prefix nil
  "Non-nil means fill each warning text using this string as `fill-prefix'.")

;; The autoload cookie is so that programs can bind this variable
;; safely, testing the existing value, before they call one of the
;; warnings functions.
;;;###autoload
(defvar warning-type-format (purecopy " (%s)")
  "Format for displaying the warning type in the warning message.
The result of formatting the type this way gets included in the
message under the control of the string in `warning-levels'.")

(defun warning-numeric-level (level)
  "Return a numeric measure of the warning severity level LEVEL."
  (let* ((elt (assq level warning-levels))
	 (link (memq elt warning-levels)))
    (length link)))

(defun warning-suppress-p (type suppress-list)
  "Non-nil if a warning with type TYPE should be suppressed.
SUPPRESS-LIST is the list of kinds of warnings to suppress."
  (let (some-match)
    (dolist (elt suppress-list)
      (if (symbolp type)
	  ;; If TYPE is a symbol, the ELT must be (TYPE).
	  (if (and (consp elt)
		   (eq (car elt) type)
		   (null (cdr elt)))
	      (setq some-match t))
	;; If TYPE is a list, ELT must match it or some initial segment of it.
	(let ((tem1 type)
	      (tem2 elt)
	      (match t))
	  ;; Check elements of ELT until we run out of them.
	  (while tem2
	    (if (not (equal (car tem1) (car tem2)))
		(setq match nil))
	    (setq tem1 (cdr tem1)
		  tem2 (cdr tem2)))
	  ;; If ELT is an initial segment of TYPE, MATCH is t now.
	  ;; So set SOME-MATCH.
	  (if match
	      (setq some-match t)))))
    ;; If some element of SUPPRESS-LIST matched,
    ;; we return t.
    some-match))

;;;###autoload
(defun display-warning (type message &optional level buffer-name)
  "Display a warning message, MESSAGE.
TYPE is the warning type: either a custom group name (a symbol),
or a list of symbols whose first element is a custom group name.
\(The rest of the symbols represent subcategories, for warning purposes
only, and you can use whatever symbols you like.)

LEVEL should be either :debug, :warning, :error, or :emergency
\(but see `warning-minimum-level' and `warning-minimum-log-level').
Default is :warning.

:emergency -- a problem that will seriously impair Emacs operation soon
	      if you do not attend to it promptly.
:error     -- data or circumstances that are inherently wrong.
:warning   -- data or circumstances that are not inherently wrong,
	      but raise suspicion of a possible problem.
:debug     -- info for debugging only.

BUFFER-NAME, if specified, is the name of the buffer for logging
the warning.  By default, it is `*Warnings*'.  If this function
has to create the buffer, it disables undo in the buffer.

See the `warnings' custom group for user customization features.

See also `warning-series', `warning-prefix-function' and
`warning-fill-prefix' for additional programming features."
  (unless level
    (setq level :warning))
  (unless buffer-name
    (setq buffer-name "*Warnings*"))
  (if (assq level warning-level-aliases)
      (setq level (cdr (assq level warning-level-aliases))))
  (or (< (warning-numeric-level level)
         (warning-numeric-level warning-minimum-log-level))
      (warning-suppress-p type warning-suppress-log-types)
      (let* ((typename (if (consp type) (car type) type))
             (old (get-buffer buffer-name))
	     (buffer (or old (get-buffer-create buffer-name)))
	     (level-info (assq level warning-levels))
	     start end)
	(with-current-buffer buffer
          ;; If we created the buffer, disable undo.
          (unless old
            (special-mode)
            (setq buffer-read-only t)
            (setq buffer-undo-list t))
	  (goto-char (point-max))
	  (when (and warning-series (symbolp warning-series))
	    (setq warning-series
		  (prog1 (point-marker)
		    (unless (eq warning-series t)
		      (funcall warning-series)))))
          (let ((inhibit-read-only t))
	    (unless (bolp)
	      (newline))
	    (setq start (point))
	    (if warning-prefix-function
		(setq level-info (funcall warning-prefix-function
					  level level-info)))
	    (insert (format (nth 1 level-info)
			    (format warning-type-format typename))
		    message)
	    (newline)
	    (when (and warning-fill-prefix (not (string-match "\n" message)))
	      (let ((fill-prefix warning-fill-prefix)
		    (fill-column 78))
		(fill-region start (point))))
	    (setq end (point)))
	  (when (and (markerp warning-series)
		     (eq (marker-buffer warning-series) buffer))
	    (goto-char warning-series)))
	(if (nth 2 level-info)
	    (funcall (nth 2 level-info)))
	(cond (noninteractive
	       ;; Noninteractively, take the text we inserted
	       ;; in the warnings buffer and print it.
	       ;; Do this unconditionally, since there is no way
	       ;; to view logged messages unless we output them.
	       (with-current-buffer buffer
		 (save-excursion
		   ;; Don't include the final newline in the arg
		   ;; to `message', because it adds a newline.
		   (goto-char end)
		   (if (bolp)
		       (forward-char -1))
		   (message "%s" (buffer-substring start (point))))))
	      ((and (daemonp) (null after-init-time))
	       ;; Warnings assigned during daemon initialization go into
	       ;; the messages buffer.
	       (message "%s"
			(with-current-buffer buffer
			  (save-excursion
			    (goto-char end)
			    (if (bolp)
				(forward-char -1))
			    (buffer-substring start (point))))))
	      (t
	       ;; Interactively, decide whether the warning merits
	       ;; immediate display.
	       (or (< (warning-numeric-level level)
		      (warning-numeric-level warning-minimum-level))
		   (warning-suppress-p type warning-suppress-types)
		   (let ((window (display-buffer buffer)))
		     (when (and (markerp warning-series)
				(eq (marker-buffer warning-series) buffer))
		       (set-window-start window warning-series))
		     (sit-for 0))))))))

;;;###autoload
(defun lwarn (type level message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Aside from generating the message with `format',
this is equivalent to `display-warning'.

TYPE is the warning type: either a custom group name (a symbol),
or a list of symbols whose first element is a custom group name.
\(The rest of the symbols represent subcategories and
can be whatever you like.)

LEVEL should be either :debug, :warning, :error, or :emergency
\(but see `warning-minimum-level' and `warning-minimum-log-level').

:emergency -- a problem that will seriously impair Emacs operation soon
	      if you do not attend to it promptly.
:error     -- invalid data or circumstances.
:warning   -- suspicious data or circumstances.
:debug     -- info for debugging only."
  (display-warning type (apply 'format message args) level))

;;;###autoload
(defun warn (message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Aside from generating the message with `format',
this is equivalent to `display-warning', using
`emacs' as the type and `:warning' as the level."
  (display-warning 'emacs (apply 'format message args)))

(provide 'warnings)

;;; warnings.el ends here
