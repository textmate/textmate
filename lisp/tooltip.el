;;; tooltip.el --- show tooltip windows

;; Copyright (C) 1997, 1999-2012 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@acm.org>
;; Keywords: help c mouse tools
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

;;; Code:

(defvar comint-prompt-regexp)

(defgroup tooltip nil
  "Customization group for the `tooltip' package."
  :group 'help
  :group 'gud
  :group 'mouse
  :group 'tools
  :version "21.1"
  :tag "Tool Tips")

;;; Switching tooltips on/off

(define-minor-mode tooltip-mode
  "Toggle Tooltip mode.
With a prefix argument ARG, enable Tooltip mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When this global minor mode is enabled, Emacs displays help
text (e.g. for buttons and menu items that you put the mouse on)
in a pop-up window.

When Tooltip mode is disabled, Emacs displays help text in the
echo area, instead of making a pop-up window."
  :global t
  ;; Even if we start on a text-only terminal, make this non-nil by
  ;; default because we can open a graphical frame later (multi-tty).
  :init-value t
  :initialize 'custom-initialize-delay
  :group 'tooltip
  (unless (or (null tooltip-mode) (fboundp 'x-show-tip))
    (error "Sorry, tooltips are not yet available on this system"))
  (if tooltip-mode
      (progn
	(add-hook 'pre-command-hook 'tooltip-hide)
	(add-hook 'tooltip-functions 'tooltip-help-tips))
    (unless (and (boundp 'gud-tooltip-mode) gud-tooltip-mode)
      (remove-hook 'pre-command-hook 'tooltip-hide))
    (remove-hook 'tooltip-functions 'tooltip-help-tips))
  (setq show-help-function
	(if tooltip-mode 'tooltip-show-help 'tooltip-show-help-non-mode)))


;;; Customizable settings

(defcustom tooltip-delay 0.7
  "Seconds to wait before displaying a tooltip the first time."
  :type 'number
  :group 'tooltip)

(defcustom tooltip-short-delay 0.1
  "Seconds to wait between subsequent tooltips on different items."
  :type 'number
  :group 'tooltip)

(defcustom tooltip-recent-seconds 1
  "Display tooltips if changing tip items within this many seconds.
Do so after `tooltip-short-delay'."
  :type 'number
  :group 'tooltip)

(defcustom tooltip-hide-delay 10
  "Hide tooltips automatically after this many seconds."
  :type 'number
  :group 'tooltip)

(defcustom tooltip-x-offset 5
  "X offset, in pixels, for the display of tooltips.
The offset is the distance between the X position of the mouse and
the left border of the tooltip window.  It must be chosen so that the
tooltip window doesn't contain the mouse when it pops up, or it may
interfere with clicking where you wish.

If `tooltip-frame-parameters' includes the `left' parameter,
the value of `tooltip-x-offset' is ignored."
  :type 'integer
  :group 'tooltip)

(defcustom tooltip-y-offset +20
  "Y offset, in pixels, for the display of tooltips.
The offset is the distance between the Y position of the mouse and
the top border of the tooltip window.  It must be chosen so that the
tooltip window doesn't contain the mouse when it pops up, or it may
interfere with clicking where you wish.

If `tooltip-frame-parameters' includes the `top' parameter,
the value of `tooltip-y-offset' is ignored."
  :type 'integer
  :group 'tooltip)

(defcustom tooltip-frame-parameters
  '((name . "tooltip")
    (internal-border-width . 2)
    (border-width . 1))
  "Frame parameters used for tooltips.

If `left' or `top' parameters are included, they specify the absolute
position to pop up the tooltip.

Note that font and color parameters are ignored, and the attributes
of the `tooltip' face are used instead."
  :type 'sexp
  :group 'tooltip)

(defface tooltip
  '((((class color))
     :background "lightyellow"
     :foreground "black"
     :inherit variable-pitch)
    (t
     :inherit variable-pitch))
  "Face for tooltips."
  :group 'tooltip
  :group 'basic-faces)

(defcustom tooltip-use-echo-area nil
  "Use the echo area instead of tooltip frames for help and GUD tooltips.
This variable is obsolete; instead of setting it to t, disable
`tooltip-mode' (which has a similar effect)."
  :type 'boolean
  :group 'tooltip)

(make-obsolete-variable 'tooltip-use-echo-area
			"disable Tooltip mode instead" "24.1")


;;; Variables that are not customizable.

(defvar tooltip-functions nil
  "Functions to call to display tooltips.
Each function is called with one argument EVENT which is a copy
of the last mouse movement event that occurred.  If one of these
functions displays the tooltip, it should return non-nil and the
rest are not called.")

(define-obsolete-variable-alias 'tooltip-hook 'tooltip-functions "23.1")

(defvar tooltip-timeout-id nil
  "The id of the timeout started when Emacs becomes idle.")

(defvar tooltip-last-mouse-motion-event nil
  "A copy of the last mouse motion event seen.")

(defvar tooltip-hide-time nil
  "Time when the last tooltip was hidden.")

(defvar gud-tooltip-mode) ;; Prevent warning.

;;; Event accessors

(defun tooltip-event-buffer (event)
  "Return the buffer over which event EVENT occurred.
This might return nil if the event did not occur over a buffer."
  (let ((window (posn-window (event-end event))))
    (and window (window-buffer window))))


;;; Timeout for tooltip display

(defun tooltip-delay ()
  "Return the delay in seconds for the next tooltip."
  (if (and tooltip-hide-time
           (< (- (float-time) tooltip-hide-time) tooltip-recent-seconds))
      tooltip-short-delay
    tooltip-delay))

(defun tooltip-cancel-delayed-tip ()
  "Disable the tooltip timeout."
  (when tooltip-timeout-id
    (disable-timeout tooltip-timeout-id)
    (setq tooltip-timeout-id nil)))

(defun tooltip-start-delayed-tip ()
  "Add a one-shot timeout to call function `tooltip-timeout'."
  (setq tooltip-timeout-id
	(add-timeout (tooltip-delay) 'tooltip-timeout nil)))

(defun tooltip-timeout (_object)
  "Function called when timer with id `tooltip-timeout-id' fires."
  (run-hook-with-args-until-success 'tooltip-functions
				    tooltip-last-mouse-motion-event))


;;; Displaying tips

(defun tooltip-set-param (alist key value)
  "Change the value of KEY in alist ALIST to VALUE.
If there's no association for KEY in ALIST, add one, otherwise
change the existing association.  Value is the resulting alist."
  (let ((param (assq key alist)))
    (if (consp param)
	(setcdr param value)
      (push (cons key value) alist))
    alist))

(declare-function x-show-tip "xfns.c"
		  (string &optional frame parms timeout dx dy))

(defun tooltip-show (text &optional use-echo-area)
  "Show a tooltip window displaying TEXT.

Text larger than `x-max-tooltip-size' is clipped.

If the alist in `tooltip-frame-parameters' includes `left' and `top'
parameters, they determine the x and y position where the tooltip
is displayed.  Otherwise, the tooltip pops at offsets specified by
`tooltip-x-offset' and `tooltip-y-offset' from the current mouse
position.

Optional second arg USE-ECHO-AREA non-nil means to show tooltip
in echo area."
  (if use-echo-area
      (tooltip-show-help-non-mode text)
    (condition-case error
	(let ((params (copy-sequence tooltip-frame-parameters))
	      (fg (face-attribute 'tooltip :foreground))
	      (bg (face-attribute 'tooltip :background)))
	  (when (stringp fg)
	    (setq params (tooltip-set-param params 'foreground-color fg))
	    (setq params (tooltip-set-param params 'border-color fg)))
	  (when (stringp bg)
	    (setq params (tooltip-set-param params 'background-color bg)))
	  (x-show-tip (propertize text 'face 'tooltip)
		      (selected-frame)
		      params
		      tooltip-hide-delay
		      tooltip-x-offset
		      tooltip-y-offset))
      (error
       (message "Error while displaying tooltip: %s" error)
       (sit-for 1)
       (message "%s" text)))))

(declare-function x-hide-tip "xfns.c" ())

(defun tooltip-hide (&optional _ignored-arg)
  "Hide a tooltip, if one is displayed.
Value is non-nil if tooltip was open."
  (tooltip-cancel-delayed-tip)
  (when (x-hide-tip)
    (setq tooltip-hide-time (float-time))))


;;; Debugger-related functions

(defun tooltip-identifier-from-point (point)
  "Extract the identifier at POINT, if any.
Value is nil if no identifier exists at point.  Identifier extraction
is based on the current syntax table."
  (save-excursion
    (goto-char point)
    (let ((start (progn (skip-syntax-backward "w_") (point))))
      (unless (looking-at "[0-9]")
	(skip-syntax-forward "w_")
	(when (> (point) start)
	  (buffer-substring start (point)))))))

(defmacro tooltip-region-active-p ()
  "Value is non-nil if the region should override command actions."
  `(use-region-p))

(defun tooltip-expr-to-print (event)
  "Return an expression that should be printed for EVENT.
If a region is active and the mouse is inside the region, print
the region.  Otherwise, figure out the identifier around the point
where the mouse is."
  (with-current-buffer (tooltip-event-buffer event)
    (let ((point (posn-point (event-end event))))
      (if (tooltip-region-active-p)
	  (when (and (<= (region-beginning) point) (<= point (region-end)))
	    (buffer-substring (region-beginning) (region-end)))
	(tooltip-identifier-from-point point)))))

(defun tooltip-process-prompt-regexp (process)
  "Return regexp matching the prompt of PROCESS at the end of a string.
The prompt is taken from the value of `comint-prompt-regexp' in
the buffer of PROCESS."
  (let ((prompt-regexp (with-current-buffer (process-buffer process)
			 comint-prompt-regexp)))
    (concat "\n*"
            ;; Most start with `^' but the one for `sdb' cannot be easily
            ;; stripped.  Code the prompt for `sdb' fixed here.
            (if (= (aref prompt-regexp 0) ?^)
                (substring prompt-regexp 1)
              "\\*")
            "$")))

(defun tooltip-strip-prompt (process output)
  "Return OUTPUT with any prompt of PROCESS stripped from its end."
  (save-match-data
    (if (string-match (tooltip-process-prompt-regexp process) output)
        (substring output 0 (match-beginning 0))
      output)))


;;; Tooltip help.

(defvar tooltip-help-message nil
  "The last help message received via `show-help-function'.
This is used by `tooltip-show-help' and
`tooltip-show-help-non-mode'.")

(defvar tooltip-previous-message nil
  "The previous content of the echo area.")

(defun tooltip-show-help-non-mode (help)
  "Function installed as `show-help-function' when Tooltip mode is off.
It is also called if Tooltip mode is on, for text-only displays."
  (when (and (not (window-minibuffer-p)) ;Don't overwrite minibuffer contents.
             (not cursor-in-echo-area))  ;Don't overwrite a prompt.
    (cond
     ((stringp help)
      (setq help (replace-regexp-in-string "\n" ", " help))
      (unless (or tooltip-previous-message
		  (string-equal help (current-message))
		  (and (stringp tooltip-help-message)
		       (string-equal tooltip-help-message
				     (current-message))))
        (setq tooltip-previous-message (current-message)))
      (setq tooltip-help-message help)
      (let ((message-truncate-lines t)
            (message-log-max nil))
        (message "%s" help)))
     ((stringp tooltip-previous-message)
      (let ((message-log-max nil))
        (message "%s" tooltip-previous-message)
        (setq tooltip-previous-message nil)))
     (t
      (message nil)))))

(defun tooltip-show-help (msg)
  "Function installed as `show-help-function'.
MSG is either a help string to display, or nil to cancel the display."
  (if (display-graphic-p)
      (let ((previous-help tooltip-help-message))
	(setq tooltip-help-message msg)
	(cond ((null msg)
	       ;; Cancel display.  This also cancels a delayed tip, if
	       ;; there is one.
	       (tooltip-hide))
	      ((equal previous-help msg)
	       ;; Same help as before (but possibly the mouse has moved).
	       ;; Keep what we have.
	       )
	      (t
	       ;; A different help.  Remove a previous tooltip, and
	       ;; display a new one, with some delay.
	       (tooltip-hide)
	       (tooltip-start-delayed-tip))))
    ;; On text-only displays, try `tooltip-show-help-non-mode'.
    (tooltip-show-help-non-mode msg)))

(defun tooltip-help-tips (_event)
  "Hook function to display a help tooltip.
This is installed on the hook `tooltip-functions', which
is run when the timer with id `tooltip-timeout-id' fires.
Value is non-nil if this function handled the tip."
  (when (stringp tooltip-help-message)
    (tooltip-show tooltip-help-message tooltip-use-echo-area)
    t))

(provide 'tooltip)

;;; tooltip.el ends here
