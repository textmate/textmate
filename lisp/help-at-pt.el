;;; help-at-pt.el --- local help through the keyboard

;; Copyright (C) 2003-2012 Free Software Foundation, Inc.

;; Author: Luc Teirlinck <teirllm@auburn.edu>
;; Keywords: help

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

;; This file contains functionality to make the help provided by the
;; help-echo text or overlay property available to the keyboard user.
;; It also supports a more keyboard oriented alternative to
;; `help-echo', namely a new text or overlay property `kbd-help'.
;;
;; It provides facilities to access the local help available at point
;; either on demand, using the command `display-local-help', or
;; automatically after a suitable idle time, through the customizable
;; variable `help-at-pt-display-when-idle'.
;;
;; You can get a more global overview of the local help available in
;; the buffer, using the commands `scan-buf-next-region' and
;; `scan-buf-previous-region', which move to the start of the next or
;; previous region with available local help and print the help found
;; there.
;;
;; Suggested key bindings:
;;
;; (global-set-key [C-tab] 'scan-buf-next-region)
;; (global-set-key [C-M-tab] 'scan-buf-previous-region)
;;
;; You do not have to do anything special to use the functionality
;; provided by this file, because all important functions autoload.

;;; Code:

(defgroup help-at-pt nil
  "Features for displaying local help."
  :group 'help
  :version "22.1")

;;;###autoload
(defun help-at-pt-string (&optional kbd)
  "Return the help-echo string at point.
Normally, the string produced by the `help-echo' text or overlay
property, or nil, is returned.
If KBD is non-nil, `kbd-help' is used instead, and any
`help-echo' property is ignored.  In this case, the return value
can also be t, if that is the value of the `kbd-help' property."
  (let* ((prop (if kbd 'kbd-help 'help-echo))
	 (pair (get-char-property-and-overlay (point) prop))
	 (val (car pair))
	 (ov (cdr pair)))
    (if (functionp val)
	(funcall val (selected-window) (if ov ov (current-buffer)) (point))
      (eval val))))

;;;###autoload
(defun help-at-pt-kbd-string ()
  "Return the keyboard help string at point.
If the `kbd-help' text or overlay property at point produces a
string, return it.  Otherwise, use the `help-echo' property.
If this produces no string either, return nil."
  (let ((kbd (help-at-pt-string t))
	(echo (help-at-pt-string)))
    (if (and kbd (not (eq kbd t))) kbd echo)))

;;;###autoload
(defun display-local-help (&optional arg)
  "Display local help in the echo area.
This displays a short help message, namely the string produced by
the `kbd-help' property at point.  If `kbd-help' does not produce
a string, but the `help-echo' property does, then that string is
printed instead.

A numeric argument ARG prevents display of a message in case
there is no help.  While ARG can be used interactively, it is
mainly meant for use from Lisp."
  (interactive "P")
  (let ((help (help-at-pt-kbd-string)))
    (if help
	(message "%s" help)
      (if (not arg) (message "No local help at point")))))

(defvar help-at-pt-timer nil
  "Non-nil means that a timer is set that checks for local help.
If non-nil, this is the value returned by the call of
`run-with-idle-timer' that set that timer.  This variable is used
internally to enable `help-at-pt-display-when-idle'.  Do not set it
yourself.")

(defcustom help-at-pt-timer-delay 1
  "Delay before displaying local help.
This is used if `help-at-pt-display-when-idle' is enabled.
The value may be an integer or floating point number.

If a timer is already active, there are two ways to make the new
value take effect immediately.  After setting the value, you can
first call `help-at-pt-cancel-timer' and then set a new timer
with `help-at-pt-set-timer'.  Alternatively, you can set this
variable through Custom.  This will not set a timer if none is
active, but if one is already active, Custom will make it use the
new value."
  :group 'help-at-pt
  :type 'number
  :initialize 'custom-initialize-default
  :set (lambda (variable value)
	 (set-default variable value)
	 (and (boundp 'help-at-pt-timer)
	      help-at-pt-timer
	      (timer-set-idle-time help-at-pt-timer value t))))

;;;###autoload
(defun help-at-pt-cancel-timer ()
  "Cancel any timer set by `help-at-pt-set-timer'.
This disables `help-at-pt-display-when-idle'."
  (interactive)
  (let ((inhibit-quit t))
    (when help-at-pt-timer
      (cancel-timer help-at-pt-timer)
      (setq help-at-pt-timer nil))))

;;;###autoload
(defun help-at-pt-set-timer ()
  "Enable `help-at-pt-display-when-idle'.
This is done by setting a timer, if none is currently active."
  (interactive)
  (unless help-at-pt-timer
    (setq help-at-pt-timer
	  (run-with-idle-timer
	   help-at-pt-timer-delay t #'help-at-pt-maybe-display))))

;;;###autoload
(defcustom help-at-pt-display-when-idle 'never
  "Automatically show local help on point-over.
If the value is t, the string obtained from any `kbd-help' or
`help-echo' property at point is automatically printed in the
echo area, if nothing else is already displayed there, or after a
quit.  If both `kbd-help' and `help-echo' produce help strings,
`kbd-help' is used.  If the value is a list, the help only gets
printed if there is a text or overlay property at point that is
included in this list.  Suggested properties are `keymap',
`local-map', `button' and `kbd-help'.  Any value other than t or
a non-empty list disables the feature.

This variable only takes effect after a call to
`help-at-pt-set-timer'.  The help gets printed after Emacs has
been idle for `help-at-pt-timer-delay' seconds.  You can call
`help-at-pt-cancel-timer' to cancel the timer set by, and the
effect of, `help-at-pt-set-timer'.

When this variable is set through Custom, `help-at-pt-set-timer'
is called automatically, unless the value is `never', in which
case `help-at-pt-cancel-timer' is called.  Specifying an empty
list of properties through Custom will set the timer, thus
enabling buffer local values.  It sets the actual value to nil.
Thus, Custom distinguishes between a nil value and other values
that disable the feature, which Custom identifies with `never'.
The default is `never'."
  :group 'help-at-pt
  :type '(choice  (const :tag "Always"
			 :format "%t\n%h"
			 :doc
			 "This choice can get noisy.
The text printed from the `help-echo' property is often only
relevant when using the mouse.  If you mind about too many
messages getting printed in the echo area, use \"In certain
situations\".  See the documentation there for more information."
			 t)
		  (repeat :tag "In certain situations"
			  ;; unless we specify 0 offset the doc string
			  ;; for this choice gets indented very
			  ;; differently than for the other two
			  ;; choices, when "More" is selected.
			  :offset 0
			  :format "%{%t%}:\n%v%i\n%h"
			  :doc
			  "This choice lets you specify a list of \
text properties.
Presence of any of these properties will trigger display of
available local help on point-over.
If you use this alternative through Custom without listing any
properties, a timer will be set anyway.  This will enable buffer
local values.  Use \"Never\" if you do not want a timer to be set.

Suggested properties:
The `keymap' and `local-map' properties change keybindings in
parts of the buffer.  Some of these keymaps are mode independent
and are not mentioned in the mode documentation.  Hence, the help
text is likely to be useful.
Specifying `button' is relevant in Custom and similar buffers.
In these buffers, most, but not all, of the text shown this way is
available by default when using tab, but not on regular point-over.
The presence of a `kbd-help' property guarantees that non mouse
specific help is available."
			  :value (keymap local-map button kbd-help)
			  symbol)
		  (other :tag "Never"
			 :format "%t\n%h"
			 :doc
			 "This choice normally disables buffer local values.
If you choose this value through Custom and a timer checking for
local help is currently active, it will be canceled.  No new
timer will be set.  Call `help-at-pt-set-timer' after choosing
this option, or use \"In certain situations\" and specify no text
properties, to enable buffer local values."
			 never))
  :initialize 'custom-initialize-default
  :set #'(lambda (variable value)
	   (set-default variable value)
	   (if (eq value 'never)
	       (help-at-pt-cancel-timer)
	     (help-at-pt-set-timer)))
  :set-after '(help-at-pt-timer-delay)
  :require 'help-at-pt)

;; Function for use in `help-at-pt-set-timer'.
(defun help-at-pt-maybe-display ()
  (and (or (eq help-at-pt-display-when-idle t)
	   (and (consp help-at-pt-display-when-idle)
		(catch 'found
		  (dolist (prop help-at-pt-display-when-idle)
		    (if (get-char-property (point) prop)
			(throw 'found t))))))
       (or (not (current-message))
	   (string= (current-message) "Quit"))
       (display-local-help t)))

;;;###autoload
(defun scan-buf-move-to-region (prop &optional arg hook)
  "Go to the start of the next region with non-nil PROP property.
Then run HOOK, which should be a quoted symbol that is a normal
hook variable, or an expression evaluating to such a symbol.
Adjacent areas with different non-nil PROP properties are
considered different regions.

With numeric argument ARG, move to the start of the ARGth next
such region, then run HOOK.  If ARG is negative, move backward.
If point is already in a region, then that region does not count
toward ARG.  If ARG is 0 and point is inside a region, move to
the start of that region.  If ARG is 0 and point is not in a
region, print a message to that effect, but do not move point and
do not run HOOK.  If there are not enough regions to move over,
an error results and the number of available regions is mentioned
in the error message.  Point is not moved and HOOK is not run."
  (cond ((> arg 0)
	 (if (= (point) (point-max))
	     (error "No further `%s' regions" prop))
	 (let ((pos (point)))
	   (dotimes (x arg)
	     (setq pos (next-single-char-property-change pos prop))
	     (unless (get-char-property pos prop)
	       (setq pos (next-single-char-property-change pos prop))
	       (unless (get-char-property pos prop)
		 (cond ((= x 0)
			(error "No further `%s' regions" prop))
		       ((= x 1)
			(error "There is only one further `%s' region" prop))
		       (t
			(error
			 "There are only %d further `%s' regions"
			 x prop))))))
	   (goto-char pos)
	   (run-hooks hook)))
	((= arg 0)
	 (let ((val (get-char-property (point) prop)))
	   (cond ((not val)
		  (message "Point is not in a `%s' region" prop))
		 ((eq val (get-char-property (1- (point)) prop))
		  (goto-char
		   (previous-single-char-property-change (point) prop))
		  (run-hooks hook))
		 (t (run-hooks hook)))))
	((< arg 0)
	 (let ((pos (point)) (val (get-char-property (point) prop)))
	   (and val
		(eq val (get-char-property (1- pos) prop))
		(setq pos
		      (previous-single-char-property-change pos prop)))
	   (if (= pos (point-min))
	       (error "No prior `%s' regions" prop))
	   (dotimes (x (- arg))
	     (setq pos (previous-single-char-property-change pos prop))
	     (unless (get-char-property pos prop)
	       (setq pos (previous-single-char-property-change pos prop))
	       (unless (get-char-property pos prop)
		 (cond ((= x 0)
			(error "No prior `%s' regions" prop))
		       ((= x 1)
			(error "There is only one prior `%s' region" prop))
		       (t
			(error "There are only %d prior `%s' regions"
			       x prop))))))
	   (goto-char pos)
	   (run-hooks hook)))))

;; To be moved to a different file and replaced by a defcustom in a
;; future version.
(defvar scan-buf-move-hook '(display-local-help)
  "Normal hook run by `scan-buf-next-region'.
Also used by `scan-buf-previous-region'.  The hook is run after
positioning point.")

;;;###autoload
(defun scan-buf-next-region (&optional arg)
  "Go to the start of the next region with non-nil help-echo.
Print the help found there using `display-local-help'.  Adjacent
areas with different non-nil help-echo properties are considered
different regions.

With numeric argument ARG, move to the start of the ARGth next
help-echo region.  If ARG is negative, move backward.  If point
is already in a help-echo region, then that region does not count
toward ARG.  If ARG is 0 and point is inside a help-echo region,
move to the start of that region.  If ARG is 0 and point is not
in such a region, just print a message to that effect.  If there
are not enough regions to move over, an error results and the
number of available regions is mentioned in the error message.

A potentially confusing subtlety is that point can be in a
help-echo region without any local help being available.  This is
because `help-echo' can be a function evaluating to nil.  This
rarely happens in practice."
  (interactive "p")
  (scan-buf-move-to-region 'help-echo arg 'scan-buf-move-hook))

;;;###autoload
(defun scan-buf-previous-region (&optional arg)
  "Go to the start of the previous region with non-nil help-echo.
Print the help found there using `display-local-help'.  Adjacent
areas with different non-nil help-echo properties are considered
different regions.  With numeric argument ARG, behaves like
`scan-buf-next-region' with argument -ARG."
  (interactive "p")
  (scan-buf-move-to-region 'help-echo (- arg) 'scan-buf-move-hook))

(provide 'help-at-pt)

;;; help-at-pt.el ends here
