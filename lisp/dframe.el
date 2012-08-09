;;; dframe --- dedicate frame support modes

;; Copyright (C) 1996-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: file, tags, tools

(defvar dframe-version "1.3"
  "The current version of the dedicated frame library.")

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
;;
;; This code was developed and maintained as a part of speedbar since 1996.
;; It became its own support utility in Aug 2000.
;;
;; Dedicated frame mode is an Emacs independent library for supporting
;; a program/buffer combination that resides in a dedicated frame.
;; Support of this nature requires several complex interactions with the
;; user which this library will provide, including:
;;
;; * Creation of a frame.  Positioned relatively.
;;   Includes a frame cache for User position caching.
;; * Switching between frames.
;; * Timed activities using idle-timers
;; * Frame/buffer killing hooks
;; * Mouse-3 position relative menu
;; * Mouse motion, help-echo hacks
;; * Mouse clicking, double clicking, & XEmacs image clicking hack
;; * Mode line hacking
;; * Utilities for use in a program covering:
;;    o keymap massage for some actions
;;    o working with an associated buffer
;;    o shift-click
;;    o detaching a frame
;;    o focus-shifting & optional frame jumping
;;    o currently active frame.
;;    o message/y-or-n-p
;;    o mouse set point
;;
;; To Use:
;; 1) (require 'dframe)
;; 2) Variable Setup:
;;   -frame-parameters -- Frame parameters for Emacs.
;;   -frame-plist -- Frame parameters for XEmacs.
;;   -- Not on parameter lists: They can optionally include width
;;      and height.  If width or height is not included, then it will
;;      be provided to match the originating frame.  In general,
;;      turning off the menu bar, mode line, and minibuffer can
;;      provide a smaller window, or more display area.
;;   -track-mouse-flag -- mouse tracking on/off specific to your tool.
;;   -update-flag -- app toggle for timer use.  Init from
;;     `dframe-have-timer-flag'.  This is nil for terminals, since
;;     updating a frame in a terminal is not useful to the user.
;;   -key-map -- Your keymap.  Call `dframe-update-keymap' on it.
;;   -buffer, -frame, -cached-frame -- Variables used to track your
;;     applications buffer, frame, or frame cache (when hidden).  See
;;     `dframe-frame-mode' for details.
;;   -before-delete-hook, -before-popup-hook, -after-create-hook --
;;     Hooks to have called.  The `-after-create-hook' probably wants
;;     to call a function which calls `dframe-reposition-frame' in an
;;     appropriate manner.
;; 3) Function Setup:
;;   your-frame-mode -- function to toggle your app frame on and off.
;;     its tasks are:
;;       a) create a buffer
;;       b) Call `dframe-frame-mode'.  (See its doc)
;;       c) If successful (your -frame variable has a value), call
;;          timer setup if applicable.
;;   your-frame-reposition- -- Function to call from after-create-hook to
;;     reposition your frame with `dframe-reposition-frame'.
;;   your-mode -- Set up the major mode of the buffer for your app.
;;     Set these variables: dframe-track-mouse-function,
;;                          dframe-help-echo-function,
;;                          dframe-mouse-click-function,
;;                          dframe-mouse-position-function.
;;   See speedbar's implementation of these functions.
;;    `speedbar-current-frame', `speedbar-get-focus', `speedbar-message',
;;    `speedbar-y-or-n-p', `speedbar-set-timer', `speedbar-click',
;;    `speedbar-position-cursor-on-line'
;; 4) Handling mouse clicks, and help text:
;;   dframe-track-mouse, dframe-help-echo-function --
;;    These variables need to be set to functions that display info
;;    based on the mouse's position.
;;   Text property 'help-echo, set to `dframe-help-echo', which will
;;    call `dframe-help-echo-function'.
;;   Have a `-click' function, it can call `dframe-quick-mouse' for
;;    positioning.  If the variable `dframe-power-click' is non-nil,
;;    then `shift' was held down during the click.

;;; Bugs
;;
;;  * The timer managers doesn't handle multiple different timeouts.
;;  * You can't specify continuous timeouts (as opposed to just idle timers.)

(defvar x-pointer-hand2)
(defvar x-pointer-top-left-arrow)

;;; Code:

;;; Compatibility functions
;;
(defalias 'dframe-frame-parameter
  (if (fboundp 'frame-parameter) 'frame-parameter
    (lambda (frame parameter)
      "Return FRAME's PARAMETER value."
      (cdr (assoc parameter (frame-parameters frame))))))


;;; Variables
;;
(defgroup dframe nil
  "Faces used in dframe."
  :prefix "dframe-"
  :group 'dframe)

(defvar dframe-have-timer-flag (if (fboundp 'display-graphic-p)
				   (display-graphic-p)
				 window-system)
  "Non-nil means that timers are available for this Emacs.
This is nil for terminals, since updating a frame in a terminal
is not useful to the user.")

(defcustom dframe-update-speed
  (if (featurep 'xemacs) 2		; 1 is too obtrusive in XEmacs
    1)
  "Idle time in seconds needed before dframe will update itself.
Updates occur to allow dframe to display directory information
relevant to the buffer you are currently editing."
  :group 'dframe
  :type 'integer)

(defcustom dframe-activity-change-focus-flag nil
  "Non-nil means the selected frame will change based on activity.
Thus, if a file is selected for edit, the buffer will appear in the
selected frame and the focus will change to that frame."
  :group 'dframe
  :type 'boolean)

(defcustom dframe-after-select-attached-frame-hook nil
  "Hook run after dframe has selected the attached frame."
  :group 'dframe
  :type 'hook)

(defvar dframe-track-mouse-function nil
  "*A function to call when the mouse is moved in the given frame.
Typically used to display info about the line under the mouse.")
(make-variable-buffer-local 'dframe-track-mouse-function)

(defvar dframe-help-echo-function nil
  "*A function to call when help-echo is used in newer versions of Emacs.
Typically used to display info about the line under the mouse.")
(make-variable-buffer-local 'dframe-help-echo-function)

(defvar dframe-mouse-click-function nil
  "*A function to call when the mouse is clicked.
Valid clicks are mouse 2, our double mouse 1.")
(make-variable-buffer-local 'dframe-mouse-click-function)

(defvar dframe-mouse-position-function nil
  "*A function to call to position the cursor for a mouse click.")
(make-variable-buffer-local 'dframe-mouse-position-function)

(defvar dframe-power-click nil
  "Never set this by hand.  Value is t when S-mouse activity occurs.")

(defvar dframe-timer nil
  "The dframe timer used for updating the buffer.")
(make-variable-buffer-local 'dframe-timer)

(defvar dframe-attached-frame nil
  "The frame which started a frame mode.
This is the frame from which all interesting activities will go
for the mode using dframe.")
(make-variable-buffer-local 'dframe-attached-frame)

(defvar dframe-controlled nil
  "Is this buffer controlled by a dedicated frame.
Local to those buffers, as a function called that created it.")
(make-variable-buffer-local 'dframe-controlled)

(defun dframe-update-keymap (map)
  "Update the keymap MAP for dframe default bindings."
  ;; Frame control
  (define-key map "q" 'dframe-close-frame)
  (define-key map "Q" 'delete-frame)

  ;; Override switch to buffer to never hack our frame.
  (substitute-key-definition 'switch-to-buffer
			     'dframe-switch-buffer-attached-frame
			     map global-map)

  (if (featurep 'xemacs)
      (progn
	;; mouse bindings so we can manipulate the items on each line
	(define-key map 'button2 'dframe-click)
	(define-key map '(shift button2) 'dframe-power-click)
	;; Info doc fix from Bob Weiner
	(if (featurep 'infodoc)
	    nil
	  (define-key map 'button3 'dframe-popup-kludge))
	)

    ;; mouse bindings so we can manipulate the items on each line
    ;; (define-key map [down-mouse-1] 'dframe-double-click)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'dframe-click)
    ;; This is the power click for new frames, or refreshing a cache
    (define-key map [S-mouse-2] 'dframe-power-click)
    ;; This adds a small unnecessary visual effect
    ;;(define-key map [down-mouse-2] 'dframe-quick-mouse)

    (define-key map [down-mouse-3] 'dframe-popup-kludge)

    ;; This lets the user scroll as if we had a scrollbar... well maybe not
    (define-key map [mode-line mouse-2] 'dframe-mouse-hscroll)
    ;; another handy place users might click to get our menu.
    (define-key map [mode-line down-mouse-1]
      'dframe-popup-kludge)

    ;; We can't switch buffers with the buffer mouse menu.  Lets hack it.
    (define-key map [C-down-mouse-1] 'dframe-hack-buffer-menu)

    ;; Lastly, we want to track the mouse.  Play here
    (define-key map [mouse-movement] 'dframe-track-mouse)
    ))

(defun dframe-live-p (frame)
  "Return non-nil if FRAME is currently available."
  (and frame (frame-live-p frame) (frame-visible-p frame)))

(defvar x-sensitive-text-pointer-shape)
(defvar x-pointer-shape)

(defun dframe-frame-mode (arg frame-var cache-var buffer-var frame-name
			      local-mode-fn
			      &optional
			      parameters
			      delete-hook popup-hook create-hook
			      )
  "Manage a frame for an application, enabling it when ARG is positive.
FRAME-VAR is a variable used to cache the frame being used.
This frame is either resurrected, hidden, killed, etc based on
the value.
CACHE-VAR is a variable used to cache a cached frame.
BUFFER-VAR is a variable used to cache the buffer being used in dframe.
This buffer will have `dframe-frame-mode' run on it.
FRAME-NAME is the name of the frame to create.
LOCAL-MODE-FN is the function used to call this one.
PARAMETERS are frame parameters to apply to this dframe.
DELETE-HOOK are hooks to run when deleting a frame.
POPUP-HOOK are hooks to run before showing a frame.
CREATE-HOOK are hooks to run after creating a frame."
  ;; toggle frame on and off.
  (if (not arg) (if (dframe-live-p (symbol-value frame-var))
		    (setq arg -1) (setq arg 1)))
  ;; Make sure the current buffer is set.
  (set-buffer (symbol-value buffer-var))
  ;; turn the frame off on neg number
  (if (and (numberp arg) (< arg 0))
      (progn
	(run-hooks 'delete-hook)
	(if (and (symbol-value frame-var)
		 (frame-live-p (symbol-value frame-var)))
	    (progn
	      (set cache-var (symbol-value frame-var))
	      (make-frame-invisible (symbol-value frame-var))))
	(set frame-var nil))
    ;; Set this as our currently attached frame
    (setq dframe-attached-frame (selected-frame))
    (run-hooks 'popup-hook)
    ;; Updated the buffer passed in to contain all the hacks needed
    ;; to make it work well in a dedicated window.
    (with-current-buffer (symbol-value buffer-var)
      ;; Declare this buffer a dedicated frame
      (setq dframe-controlled local-mode-fn)

      (if (featurep 'xemacs)
	  (progn
	    ;; Hack the XEmacs mouse-motion handler
	    (set (make-local-variable 'mouse-motion-handler)
		 'dframe-track-mouse-xemacs)
	    ;; Hack the double click handler
	    (make-local-variable 'mouse-track-click-hook)
	    (add-hook 'mouse-track-click-hook
		      (lambda (event count)
			(if (/= (event-button event) 1)
			    nil		; Do normal operations.
			  (cond ((eq count 1)
				 (dframe-quick-mouse event))
				((or (eq count 2)
				     (eq count 3))
				 (dframe-click event)
				 (dframe-quick-mouse event)))
			  ;; Don't do normal operations.
			  t))))
	;; Enable mouse tracking in emacs
	(if dframe-track-mouse-function
	    (set (make-local-variable 'track-mouse) t))) ;this could be messy.
;;;;  DISABLED: This causes problems for users with multiple frames.
;;;;       ;; Set this up special just for the passed in buffer
;;;;       ;; Terminal minibuffer stuff does not require this.
;;;;       (if (and (or (assoc 'minibuffer parameters)
;;;; 		   ;; XEmacs plist is not an association list
;;;; 		   (member 'minibuffer parameters))
;;;; 	       window-system (not (eq window-system 'pc))
;;;; 	       (null default-minibuffer-frame))
;;;; 	  (progn
;;;; 	    (make-local-variable 'default-minibuffer-frame)
;;;; 	    (setq default-minibuffer-frame dframe-attached-frame))
;;;; 	)
      ;; Override `temp-buffer-show-hook' so that help and such
      ;; put their stuff into a frame other than our own.
      ;; Correct use of `temp-buffer-show-function': Bob Weiner
      (if (and (boundp 'temp-buffer-show-hook)
	       (boundp 'temp-buffer-show-function))
	  (progn (make-local-variable 'temp-buffer-show-hook)
		 (setq temp-buffer-show-hook temp-buffer-show-function)))
      (make-local-variable 'temp-buffer-show-function)
      (setq temp-buffer-show-function 'dframe-temp-buffer-show-function)
      ;; If this buffer is killed, we must make sure that we destroy
      ;; the frame the dedicated window is in.
      (add-hook 'kill-buffer-hook `(lambda ()
				     (let ((skilling (boundp 'skilling)))
				       (if skilling
					   nil
					 (if dframe-controlled
					     (progn
					       (funcall dframe-controlled -1)
					       (setq ,buffer-var nil)
					       )))))
		t t)
      )
    ;; Get the frame to work in
    (if (frame-live-p (symbol-value cache-var))
	(progn
	  (set frame-var (symbol-value cache-var))
	  (make-frame-visible (symbol-value frame-var))
	  (select-frame (symbol-value frame-var))
	  (set-window-dedicated-p (selected-window) nil)
	  (if (not (eq (current-buffer) (symbol-value buffer-var)))
	      (switch-to-buffer (symbol-value buffer-var)))
	  (set-window-dedicated-p (selected-window) t)
	  (raise-frame (symbol-value frame-var))
	  )
      (if (frame-live-p (symbol-value frame-var))
	  (raise-frame (symbol-value frame-var))
	(set frame-var
	      (if (featurep 'xemacs)
		  ;; Only guess height if it is not specified.
		  (if (member 'height parameters)
		      (make-frame parameters)
		    (make-frame (nconc (list 'height
					     (dframe-needed-height))
				       parameters)))
		(let* ((mh (dframe-frame-parameter dframe-attached-frame
						   'menu-bar-lines))
		       (paramsa
			;; Only add a guessed height if one is not specified
			;; in the input parameters.
			(if (assoc 'height parameters)
			    parameters
			  (append
			   parameters
			   (list (cons 'height (+ (or mh 0) (frame-height)))))))
		       (params
			;; Only add a guessed width if one is not specified
			;; in the input parameters.
			(if (assoc 'width parameters)
			    paramsa
			  (append
			   paramsa
			   (list (cons 'width (frame-width))))))
		       (frame
			(if (not (eq window-system 'x))
			    (make-frame params)
			  (let ((x-pointer-shape x-pointer-top-left-arrow)
				(x-sensitive-text-pointer-shape
				 x-pointer-hand2))
			    (make-frame params)))))
		  frame)))
	;; Put the buffer into the frame
	(save-excursion
	  (select-frame (symbol-value frame-var))
	  (switch-to-buffer (symbol-value buffer-var))
	  (set-window-dedicated-p (selected-window) t))
	;; Run hooks (like reposition)
	(run-hooks 'create-hook)
	;; Frame name
	(if (and (or (null window-system) (eq window-system 'pc))
		 (fboundp 'set-frame-name))
	    (save-window-excursion
	      (select-frame (symbol-value frame-var))
	      (set-frame-name frame-name)))
	;; On a terminal, raise the frame or the user will
	;; be confused.
	(if (not window-system)
	    (select-frame (symbol-value frame-var)))
	))) )

(defun dframe-reposition-frame (new-frame parent-frame location)
  "Move NEW-FRAME to be relative to PARENT-FRAME.
LOCATION can be one of 'random, 'left, 'right, 'left-right, or 'top-bottom."
  (if (featurep 'xemacs)
      (dframe-reposition-frame-xemacs new-frame parent-frame location)
    (dframe-reposition-frame-emacs new-frame parent-frame location)))

;; Not defined in builds without X, but behind window-system test.
(declare-function x-display-pixel-width "xfns.c" (&optional terminal))
(declare-function x-display-pixel-height "xfns.c" (&optional terminal))

(defun dframe-reposition-frame-emacs (new-frame parent-frame location)
  "Move NEW-FRAME to be relative to PARENT-FRAME.
LOCATION can be one of 'random, 'left-right, 'top-bottom, or
a cons cell indicating a position of the form (LEFT . TOP)."
  ;; Position dframe.
  ;; Do no positioning if not on a windowing system,
  (unless (or (not window-system) (eq window-system 'pc))
    (let* ((pfx (dframe-frame-parameter parent-frame 'left))
	   (pfy (dframe-frame-parameter parent-frame 'top))
	   (pfw (+ (tool-bar-pixel-width parent-frame)
		   (frame-pixel-width parent-frame)))
	   (pfh (frame-pixel-height parent-frame))
	   (nfw (frame-pixel-width new-frame))
	   (nfh (frame-pixel-height new-frame))
	   newleft newtop)
      ;; Rebuild pfx,pfy to be absolute positions.
      (setq pfx (if (not (consp pfx))
		    pfx
		  ;; If pfx is a list, that means we grow
		  ;; from a specific edge of the display.
		  ;; Convert that to the distance from the
		  ;; left side of the display.
		  (if (eq (car pfx) '-)
		      ;; A - means distance from the right edge
		      ;; of the display, or DW - pfx - framewidth
		      (- (x-display-pixel-width) (car (cdr pfx)) pfw)
		    (car (cdr pfx))))
	    pfy (if (not (consp pfy))
		    pfy
		  ;; If pfy is a list, that means we grow
		  ;; from a specific edge of the display.
		  ;; Convert that to the distance from the
		  ;; left side of the display.
		  (if (eq (car pfy) '-)
		      ;; A - means distance from the right edge
		      ;; of the display, or DW - pfx - framewidth
		      (- (x-display-pixel-height) (car (cdr pfy)) pfh)
		    (car (cdr pfy)))))
      (cond ((eq location 'right)
	     (setq newleft (+ pfx pfw 10)
		   newtop pfy))
	    ((eq location 'left)
	     (setq newleft (- pfx 10 nfw)
		   newtop pfy))
	    ((eq location 'left-right)
	     (setq newleft
		   ;; Decide which side to put it on.  200 is just a
		   ;; buffer for the left edge of the screen.  The
		   ;; extra 10 is just dressings for window
		   ;; decorations.
		   (let* ((left-guess (- pfx 10 nfw))
			  (right-guess (+ pfx pfw 10))
			  (left-margin left-guess)
			  (right-margin (- (x-display-pixel-width)
					   right-guess 5 nfw)))
		     (cond ((>= left-margin 0) left-guess)
			   ((>= right-margin 0) right-guess)
			   ;; otherwise choose side we overlap less
			   ((> left-margin right-margin) 0)
			   (t (- (x-display-pixel-width) nfw 5))))
		   newtop pfy))
	    ((eq location 'top-bottom)
	     (setq newleft pfx
		   newtop
		   ;; Try and guess if we should be on the top or bottom.
		   (let* ((top-guess (- pfy 15 nfh))
			  (bottom-guess (+ pfy 5 pfh))
			  (top-margin top-guess)
			  (bottom-margin (- (x-display-pixel-height)
					    bottom-guess 5 nfh)))
		     (cond ((>= top-margin 0) top-guess)
			   ((>= bottom-margin 0) bottom-guess)
			   ;; Choose a side to overlap the least.
			   ((> top-margin bottom-margin) 0)
			   (t (- (x-display-pixel-height) nfh 5))))))
	    ((consp location)
	     (setq newleft (or (car location) 0)
		   newtop (or (cdr location) 0)))
	    (t nil))
      (modify-frame-parameters new-frame
			       (list (cons 'left newleft)
				     (cons 'top newtop))))))

(defun dframe-reposition-frame-xemacs (_new-frame _parent-frame _location)
  "Move NEW-FRAME to be relative to PARENT-FRAME.
LOCATION can be one of 'random, 'left-right, or 'top-bottom."
  ;; Not yet implemented
  )

;; XEmacs function only.
(defun dframe-needed-height (&optional frame)
  "The needed height for the tool bar FRAME (in characters)."
  (or frame (setq frame (selected-frame)))
  ;; The 1 is the missing modeline/minibuffer
  (+ 1 (/ (frame-pixel-height frame)
	  ;; This obscure code avoids a byte compiler warning in Emacs.
	  (let ((f 'face-height))
	    (funcall f 'default frame)))))

(defun dframe-detach (frame-var cache-var buffer-var)
  "Detach the frame in symbol FRAME-VAR.
CACHE-VAR and BUFFER-VAR are symbols as in `dframe-frame-mode'"
  (with-current-buffer (symbol-value buffer-var)
    (rename-buffer (buffer-name) t)
    (let ((oldframe (symbol-value frame-var)))
      (set buffer-var nil)
      (set frame-var nil)
      (set cache-var nil)
      ;; FIXME: Looks very suspicious.  Luckily this function is unused.
      (make-variable-buffer-local frame-var)
      (set frame-var oldframe)
      )))

;;; Special frame event proxies
;;
(if (boundp 'special-event-map)
    (progn
      (define-key special-event-map [make-frame-visible]
	'dframe-handle-make-frame-visible)
      (define-key special-event-map [iconify-frame]
	'dframe-handle-iconify-frame)
      (define-key special-event-map [delete-frame]
	'dframe-handle-delete-frame))
  )

(defvar dframe-make-frame-visible-function nil
  "Function used when a dframe controlled frame is de-iconified.
The function must take an EVENT.")
(defvar dframe-iconify-frame-function nil
  "Function used when a dframe controlled frame is iconified.
The function must take an EVENT.")
(defvar dframe-delete-frame-function nil
  "Function used when a frame attached to a dframe frame is deleted.
The function must take an EVENT.")

(defun dframe-handle-make-frame-visible (e)
  "Handle a `make-frame-visible' event.
Should enable auto-updating if the last state was also enabled.
Argument E is the event making the frame visible."
  (interactive "e")
  (let ((f last-event-frame))
    (if (and (dframe-attached-frame f)
	     dframe-make-frame-visible-function)
	(funcall dframe-make-frame-visible-function e)
      )))

(defun dframe-handle-iconify-frame (e)
  "Handle a `iconify-frame' event.
Should disable auto-updating if the last state was also enabled.
Argument E is the event iconifying the frame."
  (interactive "e")
  (let ((f last-event-frame))
    (if (and (dframe-attached-frame f)
	     dframe-iconify-frame-function e)
	(funcall dframe-iconify-frame-function)
      )))

(defun dframe-handle-delete-frame (e)
  "Handle `delete-frame' event.
Argument E is the event deleting the frame."
  (interactive "e")
  (let ((fl (frame-list))
	(sf (selected-frame)))
    ;; Loop over all frames.  If dframe-delete-frame-function is
    ;; non-nil, call it.
    (while fl
      (select-frame (car fl))
      (if dframe-delete-frame-function
	  (funcall dframe-delete-frame-function e))
      (setq fl (cdr fl)))
    (if (frame-live-p sf)
	(select-frame sf))
    (handle-delete-frame e)))


;;; Utilities
;;
(defun dframe-get-focus (frame-var activator &optional hook)
  "Change frame focus to or from a dedicated frame.
If the selected frame is not in the symbol FRAME-VAR, then FRAME-VAR
frame is selected.  If the FRAME-VAR is active, then select the
attached frame.  If FRAME-VAR is nil, ACTIVATOR is called to
created it.  HOOK is an optional argument of hooks to run when
selecting FRAME-VAR."
  (interactive)
  (if (eq (selected-frame) (symbol-value frame-var))
      (if (frame-live-p dframe-attached-frame)
	  (dframe-select-attached-frame))
    ;; make sure we have a frame
    (if (not (frame-live-p (symbol-value frame-var)))
	(funcall activator 1))
    ;; go there
    (select-frame (symbol-value frame-var))
    )
  (other-frame 0)
  ;; If updates are off, then refresh the frame (they want it now...)
  (run-hooks 'hook))


(defun dframe-close-frame ()
  "Close the current frame if it is dedicated."
  (interactive)
  (if dframe-controlled
      (let ((b (current-buffer)))
	(funcall dframe-controlled -1)
	(kill-buffer b))))

(defun dframe-current-frame (frame-var desired-major-mode)
  "Return the existing dedicated frame to use.
FRAME-VAR is the variable storing the currently active dedicated frame.
If the current frame's buffer uses DESIRED-MAJOR-MODE, then use that frame."
  (if (not (eq (selected-frame) (symbol-value frame-var)))
      (if (and (eq major-mode desired-major-mode)
	       (get-buffer-window (current-buffer))
	       (window-frame (get-buffer-window (current-buffer))))
	  (window-frame (get-buffer-window (current-buffer)))
	(symbol-value frame-var))
    (symbol-value frame-var)))

(defun dframe-attached-frame (&optional frame)
  "Return the attached frame belonging to the dframe controlled frame FRAME.
If optional arg FRAME is nil just return `dframe-attached-frame'."
  (save-excursion
    (if frame (select-frame frame))
    dframe-attached-frame))

(defun dframe-select-attached-frame (&optional frame)
  "Switch to the frame the dframe controlled frame FRAME was started from.
If optional arg FRAME is nil assume the attached frame is already selected
and just run the hooks `dframe-after-select-attached-frame-hook'.  Return
the attached frame."
  (let ((frame (dframe-attached-frame frame)))
    (if frame (select-frame frame))
    (prog1 frame
      (run-hooks 'dframe-after-select-attached-frame-hook))))

(defmacro dframe-with-attached-buffer (&rest forms)
  "Execute FORMS in the attached frame's special buffer.
Optionally select that frame if necessary."
  `(save-selected-window
     ;;(speedbar-set-timer speedbar-update-speed)
     (dframe-select-attached-frame)
     ,@forms
     (dframe-maybee-jump-to-attached-frame)))

(defun dframe-maybee-jump-to-attached-frame ()
  "Jump to the attached frame ONLY if this was not a mouse event."
  (when (or (not (dframe-mouse-event-p last-input-event))
            dframe-activity-change-focus-flag)
    (dframe-select-attached-frame)
    ;; KB: For what is this - raising the frame??
    (other-frame 0)))


(defvar dframe-suppress-message-flag nil
  "Non-nil means that `dframe-message' should just return a string.")

(defun dframe-message (fmt &rest args)
  "Like message, but for use in a dedicated frame.
Argument FMT is the format string, and ARGS are the arguments for message."
  (save-selected-window
    (if dframe-suppress-message-flag
	(apply 'format fmt args)
      (if dframe-attached-frame
          ;; KB: Here we do not need calling `dframe-select-attached-frame'
	  (select-frame dframe-attached-frame))
      (apply 'message fmt args))))

(defun dframe-y-or-n-p (prompt)
  "Like `y-or-n-p', but for use in a dedicated frame.
Argument PROMPT is the prompt to use."
  (save-selected-window
    (if (and ;;default-minibuffer-frame
	     dframe-attached-frame
	     ;;(not (eq default-minibuffer-frame dframe-attached-frame))
	     )
        ;; KB: Here we do not need calling `dframe-select-attached-frame'
	(select-frame dframe-attached-frame))
    (y-or-n-p prompt)))

;;; timer management
;;
;; Unlike speedbar with a dedicated set of routines, dframe has one master
;; timer, and all dframe users will use it.  At least until I figure out a way
;; around that problem.
;;
;; Advantage 1: Two apps with timer/frames can munge the master list
;;              to make sure they occur in order.
;; Advantage 2: If a user hits a key between timer functions, we can
;;	        interrupt them safely.
(defvar dframe-client-functions nil
  "List of client functions using the dframe timer.")

(defun dframe-set-timer (timeout fn &optional _null-on-error)
  "Apply a timer with TIMEOUT, to call FN, or remove a timer if TIMEOUT is nil.
TIMEOUT is the number of seconds until the dframe controlled program
timer is called again.  When TIMEOUT is nil, turn off all timeouts.
This function must be called from the buffer belonging to the program
who requested the timer.  NULL-ON-ERROR is ignored."
  ;; First, fix up our list of client functions
  (if timeout
      (add-to-list 'dframe-client-functions fn)
    (setq dframe-client-functions (delete fn dframe-client-functions)))
  ;; Now decided what to do about the timeout.
  (if (or
       ;; We have a timer, restart the timer with the new time.
       timeout
       ;; We have a timer, an off is requested, and no client
       ;; functions are left, shut er down.
       (and dframe-timer (not timeout) dframe-client-functions))
      ;; Only call the low level function if we are changing the state.
      (dframe-set-timer-internal timeout)))

(defun dframe-set-timer-internal (timeout &optional _null-on-error)
  "Apply a timer with TIMEOUT to call the dframe timer manager."
  (when dframe-timer
    (if (featurep 'xemacs)
	(delete-itimer dframe-timer)
      (cancel-timer dframe-timer))
    (setq dframe-timer nil))
  (when timeout
    (setq dframe-timer
	  (if (featurep 'xemacs)
	      (start-itimer "dframe" 'dframe-timer-fn
			    timeout timeout t)
	    (run-with-idle-timer timeout t 'dframe-timer-fn)))))

(defun dframe-timer-fn ()
  "Called due to the dframe timer.
Evaluates all cached timer functions in sequence."
  (let ((l dframe-client-functions))
    (while (and l (sit-for 0))
      (condition-case er
	  (funcall (car l))
	(error (message "DFRAME TIMER ERROR: %S" er)))
      (setq l (cdr l)))))

;;; Menu hacking for mouse-3
;;
(defconst dframe-pass-event-to-popup-mode-menu
  (let (max-args)
    (and (fboundp 'popup-mode-menu)
         (fboundp 'function-max-args)
         (setq max-args (function-max-args 'popup-mode-menu))
         (not (zerop max-args))))
  "The EVENT arg to `popup-mode-menu' was introduced in XEmacs 21.4.0.")

;; In XEmacs, we make popup menus work on the item over mouse (as
;; opposed to where the point happens to be.)  We attain this by
;; temporarily moving the point to that place.
;;    Hrvoje Niksic <hniksic@srce.hr>
(defalias 'dframe-popup-kludge
  (if (featurep 'xemacs)
      (lambda (event)                        ; XEmacs.
        "Pop up a menu related to the clicked on item.
Must be bound to EVENT."
        (interactive "e")
        (save-excursion
          (if dframe-pass-event-to-popup-mode-menu
              (popup-mode-menu event)
            (goto-char (event-closest-point event))
            (beginning-of-line)
            (forward-char (min 5 (- (line-end-position)
                                    (line-beginning-position))))
            (popup-mode-menu))
          ;; Wait for menu to bail out.  `popup-mode-menu' (and other popup
          ;; menu functions) return immediately.
          (let (new)
            (while (not (misc-user-event-p (setq new (next-event))))
              (dispatch-event new))
            (dispatch-event new))))

    (lambda (e)                              ; Emacs.
      "Pop up a menu related to the clicked on item.
Must be bound to event E."
      (interactive "e")
      (save-excursion
        (mouse-set-point e)
        ;; This gets the cursor where the user can see it.
        (if (not (bolp)) (forward-char -1))
        (sit-for 0)
	(if (fboundp 'mouse-menu-major-mode-map)
	    (popup-menu (mouse-menu-major-mode-map) e)
	  (with-no-warnings	  ; don't warn about obsolete fallback
	    (mouse-major-mode-menu e nil)))))))

;;; Interactive user functions for the mouse
;;
(defalias 'dframe-mouse-event-p
  (if (featurep 'xemacs)
      'button-press-event-p
    (lambda (event)
      "Return t if the event is a mouse related event."
      (if (and (listp event)
               (member (event-basic-type event)
                       '(mouse-1 mouse-2 mouse-3)))
          t
        nil))))

(defun dframe-track-mouse (event)
  "For motion EVENT, display info about the current line."
  (interactive "e")
  (when (and dframe-track-mouse-function
	     (or (featurep 'xemacs) ;; XEmacs always safe?
		 (windowp (posn-window (event-end event))) ; Sometimes
					; there is no window to jump into.
		 ))

    (funcall dframe-track-mouse-function event)))

(defun dframe-track-mouse-xemacs (event)
  "For motion EVENT, display info about the current line."
  (if (functionp (default-value 'mouse-motion-handler))
      (funcall (default-value 'mouse-motion-handler) event))
  (if dframe-track-mouse-function
      (funcall dframe-track-mouse-function event)))

(defun dframe-help-echo (_window &optional buffer position)
  "Display help based context.
The context is in WINDOW, viewing BUFFER, at POSITION.
BUFFER and POSITION are optional because XEmacs doesn't use them."
  (when (and (not dframe-track-mouse-function)
	     (bufferp buffer)
	     dframe-help-echo-function)
    (let ((dframe-suppress-message-flag t))
      (with-current-buffer buffer
	(save-excursion
	  (if position (goto-char position))
	  (funcall dframe-help-echo-function))))))

(defun dframe-mouse-set-point (e)
  "Set point based on event E.
Handles clicking on images in XEmacs."
  (if (and (featurep 'xemacs)
           (save-excursion
             (save-window-excursion
               (mouse-set-point e)
               (event-over-glyph-p e))))
      ;; We are in XEmacs, and clicked on a picture
      (let ((ext (event-glyph-extent e)))
	;; This position is back inside the extent where the
	;; junk we pushed into the property list lives.
	(if (extent-end-position ext)
	    (goto-char (1- (extent-end-position ext)))
	  (mouse-set-point e)))
    ;; We are not in XEmacs, OR we didn't click on a picture.
    (mouse-set-point e)))

(defun dframe-quick-mouse (e)
  "Since mouse events are strange, this will keep the mouse nicely positioned.
This should be bound to mouse event E."
  (interactive "e")
  (dframe-mouse-set-point e)
  (if dframe-mouse-position-function
      (funcall dframe-mouse-position-function)))

(defun dframe-power-click (e)
  "Activate any dframe mouse click as a power click.
A power click will dispose of cached data (if available) or bring a buffer
up into a different window.
This should be bound to mouse event E."
  (interactive "e")
  (let ((dframe-power-click t))
    (select-frame last-event-frame)
    (dframe-click e)))

(defun dframe-click (e)
  "Call our clients click function on a user click.
E is the event causing the click."
  (interactive "e")
  (dframe-mouse-set-point e)
  (when dframe-mouse-click-function
    ;; On the off chance of buffer switch, or something incorrectly
    ;; configured.
    (funcall dframe-mouse-click-function e)))

(defun dframe-double-click (e)
  "Activate the registered click function on a double click.
This must be bound to a mouse event.
This should be bound to mouse event E."
  (interactive "e")
  ;; Emacs only.  XEmacs handles this via `mouse-track-click-hook'.
  (cond ((eq (car e) 'down-mouse-1)
	 (dframe-mouse-set-point e))
	((eq (car e) 'mouse-1)
	 (dframe-quick-mouse e))
	((or (eq (car e) 'double-down-mouse-1)
	     (eq (car e) 'triple-down-mouse-1))
	 (dframe-click e))))

;;; Hacks of normal things.
;;
;; Some normal things that happen in one of these dedicated frames
;; must be handled specially, so that our dedicated frame isn't
;; messed up.
(defun dframe-temp-buffer-show-function (buffer)
  "Placed in the variable `temp-buffer-show-function' in dedicated frames.
If a user requests help using \\[help-command] <Key> the temp BUFFER will be
redirected into a window on the attached frame."
  (if dframe-attached-frame (dframe-select-attached-frame))
  (pop-to-buffer buffer nil)
  (other-window -1)
  ;; Fix for using this hook on some platforms: Bob Weiner
  (cond ((not (featurep 'xemacs))
	 (run-hooks 'temp-buffer-show-hook))
	((fboundp 'run-hook-with-args)
	 (run-hook-with-args 'temp-buffer-show-hook buffer))
	((and (boundp 'temp-buffer-show-hook)
	      (listp temp-buffer-show-hook))
	 (mapcar (function (lambda (hook) (funcall hook buffer)))
		 temp-buffer-show-hook))))

(defun dframe-hack-buffer-menu (_e)
  "Control mouse 1 is buffer menu.
This hack overrides it so that the right thing happens in the main
Emacs frame, not in the dedicated frame.
Argument E is the event causing this activity."
  (interactive "e")
  (let ((fn (lookup-key global-map (if (featurep 'xemacs)
                                       '(control button1)
				     [C-down-mouse-1])))
	(oldbuff (current-buffer))
	(newbuff nil))
    (unwind-protect
	(save-excursion
	  (set-window-dedicated-p (selected-window) nil)
	  (call-interactively fn)
	  (setq newbuff (current-buffer)))
      (switch-to-buffer oldbuff)
      (set-window-dedicated-p (selected-window) t))
    (if (not (eq newbuff oldbuff))
	(dframe-with-attached-buffer
	 (switch-to-buffer newbuff)))))

(defun dframe-switch-buffer-attached-frame (&optional buffer)
  "Switch to BUFFER in the attached frame, and raise that frame.
This overrides the default behavior of `switch-to-buffer' which is
broken because of the dedicated frame."
  (interactive)
  ;; Assume we are in the dedicated frame.
  (other-frame 1)
  ;; Now switch buffers
  (if buffer
      (switch-to-buffer buffer)
    (call-interactively 'switch-to-buffer nil nil)))

;; XEmacs: this can be implemented using modeline keymaps, but there
;; is no use, as we have horizontal scrollbar (as the docstring
;; hints.)
(defun dframe-mouse-hscroll (e)
  "Read a mouse event E from the mode line, and horizontally scroll.
If the mouse is being clicked on the far left, or far right of the
mode-line.  This is only useful for non-XEmacs."
  (interactive "e")
  (let* ((x-point (car (nth 2 (car (cdr e)))))
	 (pixels-per-10-col (/ (* 10 (frame-pixel-width))
			       (frame-width)))
	 (click-col (1+ (/ (* 10 x-point) pixels-per-10-col)))
	 )
    (cond ((< click-col 3)
	   (scroll-left 2))
	  ((> click-col (- (window-width) 5))
	   (scroll-right 2))
	  (t (dframe-message
	      "Click on the edge of the modeline to scroll left/right")))
    ))

(provide 'dframe)

;;; dframe.el ends here
