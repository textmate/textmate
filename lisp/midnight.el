;;; midnight.el --- run something every midnight, e.g., kill old buffers

;; Copyright (C) 1998, 2001-2012 Free Software Foundation, Inc.

;; Author: Sam Steingold <sds@gnu.org>
;; Maintainer: Sam Steingold <sds@gnu.org>
;; Created: 1998-05-18
;; Keywords: utilities

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

;; To use the file, put (require 'midnight) into your .emacs.  Then, at
;; midnight, Emacs will run the normal hook `midnight-hook'.  You can
;; put whatever you like there, say, `calendar'; by default there is
;; only one function there - `clean-buffer-list'. It will kill the
;; buffers matching `clean-buffer-list-kill-buffer-names' and
;; `clean-buffer-list-kill-regexps' and the buffers which where last
;; displayed more than `clean-buffer-list-delay-general' days ago,
;; keeping `clean-buffer-list-kill-never-buffer-names' and
;; `clean-buffer-list-kill-never-regexps'.

;;; Code:

(eval-when-compile
 (require 'cl))

(defgroup midnight nil
  "Run something every day at midnight."
  :group 'calendar
  :version "20.3")

(defvar midnight-timer nil
  "Timer running the `midnight-hook' `midnight-delay' seconds after midnight.
Use `cancel-timer' to stop it and `midnight-delay-set' to change
the time when it is run.")

(defcustom midnight-mode nil
  "Non-nil means run `midnight-hook' at midnight.
Setting this variable outside customize has no effect;
call `cancel-timer' or `timer-activate' on `midnight-timer' instead."
  :type 'boolean
  :group 'midnight
  :require 'midnight
  :initialize 'custom-initialize-default
  :set (lambda (symb val)
         (set symb val) (require 'midnight)
         (if val (timer-activate midnight-timer)
             (cancel-timer midnight-timer))))

;;; time conversion

(defun midnight-buffer-display-time (&optional buffer)
  "Return the time-stamp of BUFFER, or current buffer, as float."
  (with-current-buffer (or buffer (current-buffer))
    (when buffer-display-time (float-time buffer-display-time))))

;;; clean-buffer-list stuff

(defcustom clean-buffer-list-delay-general 3
  "The number of days before any buffer becomes eligible for autokilling.
The autokilling is done by `clean-buffer-list' when is it in `midnight-hook'.
Currently displayed and/or modified (unsaved) buffers, as well as buffers
matching `clean-buffer-list-kill-never-buffer-names' and
`clean-buffer-list-kill-never-regexps' are excluded."
  :type 'integer
  :group 'midnight)

(defcustom clean-buffer-list-delay-special 3600
  "The number of seconds before some buffers become eligible for autokilling.
Buffers matched by `clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-buffer-names' are killed if they were last
displayed more than this many seconds ago."
  :type 'integer
  :group 'midnight)

(defcustom clean-buffer-list-kill-regexps nil
  "List of regexps saying which buffers will be killed at midnight.
If buffer name matches a regexp in the list and the buffer was not displayed
in the last `clean-buffer-list-delay-special' seconds, it is killed by
`clean-buffer-list' when is it in `midnight-hook'.
If a member of the list is a cons, its `car' is the regexp and its `cdr' is
the number of seconds to use instead of `clean-buffer-list-delay-special'.
See also `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-regexps' and
`clean-buffer-list-kill-never-buffer-names'."
  :type '(repeat (regexp :tag "Regexp matching Buffer Name"))
  :group 'midnight)

(defcustom clean-buffer-list-kill-buffer-names
    '("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*info*"
      "*vc*" "*vc-diff*" "*diff*")
  "List of strings saying which buffers will be killed at midnight.
Buffers with names in this list, which were not displayed in the last
`clean-buffer-list-delay-special' seconds, are killed by `clean-buffer-list'
when is it in `midnight-hook'.
If a member of the list is a cons, its `car' is the name and its `cdr' is
the number of seconds to use instead of `clean-buffer-list-delay-special'.
See also `clean-buffer-list-kill-regexps',
`clean-buffer-list-kill-never-regexps' and
`clean-buffer-list-kill-never-buffer-names'."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'midnight)

(defcustom clean-buffer-list-kill-never-buffer-names
    '("*scratch*" "*Messages*")
  "List of buffer names which will never be killed by `clean-buffer-list'.
See also `clean-buffer-list-kill-never-regexps'.
Note that this does override `clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-buffer-names' so a buffer matching any of these
two lists will NOT be killed if it is also present in this list."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'midnight)

(defcustom clean-buffer-list-kill-never-regexps '("^ \\*Minibuf-.*\\*$")
  "List of regexp saying which buffers will never be killed at midnight.
See also `clean-buffer-list-kill-never-buffer-names'.
Killing is done by `clean-buffer-list'.
Note that this does override `clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-buffer-names' so a buffer matching any of these
two lists will NOT be killed if it also matches anything in this list."
  :type '(repeat (regexp :tag "Regexp matching Buffer Name"))
  :group 'midnight)

(defun midnight-find (el ls test &optional key)
  "A stopgap solution to the absence of `find' in ELisp."
  (dolist (rr ls)
    (when (funcall test (if key (funcall key rr) rr) el)
      (return rr))))

(defun clean-buffer-list-delay (name)
  "Return the delay, in seconds, before killing a buffer named NAME.
Uses `clean-buffer-list-kill-buffer-names', `clean-buffer-list-kill-regexps'
`clean-buffer-list-delay-general' and `clean-buffer-list-delay-special'.
Autokilling is done by `clean-buffer-list'."
  (or (assoc-default name clean-buffer-list-kill-buffer-names 'string=
                     clean-buffer-list-delay-special)
      (assoc-default name clean-buffer-list-kill-regexps 'string-match
                     clean-buffer-list-delay-special)
      (* clean-buffer-list-delay-general 24 60 60)))

;;;###autoload
(defun clean-buffer-list ()
  "Kill old buffers that have not been displayed recently.
The relevant variables are `clean-buffer-list-delay-general',
`clean-buffer-list-delay-special', `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-buffer-names',
`clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-never-regexps'.
While processing buffers, this procedure displays messages containing
the current date/time, buffer name, how many seconds ago it was
displayed (can be nil if the buffer was never displayed) and its
lifetime, i.e., its \"age\" when it will be purged."
  (interactive)
  (let ((tm (float-time)) bts (ts (format-time-string "%Y-%m-%d %T"))
        delay cbld bn)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
	(setq bts (midnight-buffer-display-time buf) bn (buffer-name buf)
	      delay (if bts (- tm bts) 0) cbld (clean-buffer-list-delay bn))
	(message "[%s] `%s' [%s %d]" ts bn (if bts (round delay)) cbld)
	(unless (or (midnight-find bn clean-buffer-list-kill-never-regexps
				   'string-match)
		    (midnight-find bn clean-buffer-list-kill-never-buffer-names
				   'string-equal)
		    (get-buffer-process buf)
		    (and (buffer-file-name buf) (buffer-modified-p buf))
		    (get-buffer-window buf 'visible) (< delay cbld))
	  (message "[%s] killing `%s'" ts bn)
	  (kill-buffer buf))))))

;;; midnight hook

(defvar midnight-period (* 24 60 60)
  "The number of seconds in a day--the delta for `midnight-timer'.")

(defcustom midnight-hook '(clean-buffer-list)
  "The hook run `midnight-delay' seconds after midnight every day.
The default value is `clean-buffer-list'."
  :type 'hook
  :group 'midnight)

(defun midnight-next ()
  "Return the number of seconds till the next midnight."
  (multiple-value-bind (sec min hrs)
      (values-list (decode-time))
    (- (* 24 60 60) (* 60 60 hrs) (* 60 min) sec)))

;;;###autoload
(defun midnight-delay-set (symb tm)
  "Modify `midnight-timer' according to `midnight-delay'.
Sets the first argument SYMB (which must be symbol `midnight-delay')
to its second argument TM."
  (assert (eq symb 'midnight-delay) t
          "Invalid argument to `midnight-delay-set': `%s'")
  (set symb tm)
  (when (timerp midnight-timer) (cancel-timer midnight-timer))
  (setq midnight-timer
        (run-at-time (if (numberp tm) (+ (midnight-next) tm) tm)
                     midnight-period 'run-hooks 'midnight-hook)))

(defcustom midnight-delay 3600
  "The number of seconds after the midnight when the `midnight-timer' is run.
You should set this variable before loading midnight.el, or
set it by calling `midnight-delay-set', or use `custom'.
If you wish, you can use a string instead, it will be passed as the
first argument to `run-at-time'."
  :type 'sexp
  :set 'midnight-delay-set
  :group 'midnight)

(provide 'midnight)

;;; midnight.el ends here
