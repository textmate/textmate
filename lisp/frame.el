;;; frame.el --- multi-frame management independent of window systems

;; Copyright (C) 1993-1994, 1996-1997, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
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
(eval-when-compile (require 'cl))

(defvar frame-creation-function-alist
  (list (cons nil
	      (if (fboundp 'tty-create-frame-with-faces)
		  'tty-create-frame-with-faces
                (lambda (_parameters)
                  (error "Can't create multiple frames without a window system")))))
  "Alist of window-system dependent functions to call to create a new frame.
The window system startup file should add its frame creation
function to this list, which should take an alist of parameters
as its argument.")

(defvar window-system-default-frame-alist nil
  "Window-system dependent default frame parameters.
The value should be an alist of elements (WINDOW-SYSTEM . ALIST),
where WINDOW-SYSTEM is a window system symbol (see `window-system')
and ALIST is a frame parameter alist like `default-frame-alist'.
Then, for frames on WINDOW-SYSTEM, any parameters specified in
ALIST supersede the corresponding parameters specified in
`default-frame-alist'.")

;; The initial value given here used to ask for a minibuffer.
;; But that's not necessary, because the default is to have one.
;; By not specifying it here, we let an X resource specify it.
(defcustom initial-frame-alist nil
  "Alist of parameters for the initial X window frame.
You can set this in your init file; for example,

 (setq initial-frame-alist
       '((top . 1) (left . 1) (width . 80) (height . 55)))

Parameters specified here supersede the values given in
`default-frame-alist'.

If the value calls for a frame without a minibuffer, and you have
not created a minibuffer frame on your own, a minibuffer frame is
created according to `minibuffer-frame-alist'.

You can specify geometry-related options for just the initial
frame by setting this variable in your init file; however, they
won't take effect until Emacs reads your init file, which happens
after creating the initial frame.  If you want the initial frame
to have the proper geometry as soon as it appears, you need to
use this three-step process:
* Specify X resources to give the geometry you want.
* Set `default-frame-alist' to override these options so that they
  don't affect subsequent frames.
* Set `initial-frame-alist' in a way that matches the X resources,
  to override what you put in `default-frame-alist'."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'frames)

(defcustom minibuffer-frame-alist '((width . 80) (height . 2))
  "Alist of parameters for the initial minibuffer frame.
This is the minibuffer frame created if `initial-frame-alist'
calls for a frame without a minibuffer.  The parameters specified
here supersede those given in `default-frame-alist', for the
initial minibuffer frame.

You can set this in your init file; for example,

 (setq minibuffer-frame-alist
       '((top . 1) (left . 1) (width . 80) (height . 2)))

It is not necessary to include (minibuffer . only); that is
appended when the minibuffer frame is created."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'frames)

(defun handle-delete-frame (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
	(i 0)
	(tail (frame-list)))
    (while tail
      (and (frame-visible-p (car tail))
	   (not (eq (car tail) frame))
	  (setq i (1+ i)))
      (setq tail (cdr tail)))
    (if (> i 0)
	(delete-frame frame t)
      ;; Gildea@x.org says it is ok to ask questions before terminating.
      (save-buffers-kill-emacs))))

;;;; Arrangement of frames at startup

;; 1) Load the window system startup file from the lisp library and read the
;; high-priority arguments (-q and the like).  The window system startup
;; file should create any frames specified in the window system defaults.
;;
;; 2) If no frames have been opened, we open an initial text frame.
;;
;; 3) Once the init file is done, we apply any newly set parameters
;; in initial-frame-alist to the frame.

;; These are now called explicitly at the proper times,
;; since that is easier to understand.
;; Actually using hooks within Emacs is bad for future maintenance. --rms.
;; (add-hook 'before-init-hook 'frame-initialize)
;; (add-hook 'window-setup-hook 'frame-notice-user-settings)

;; If we create the initial frame, this is it.
(defvar frame-initial-frame nil)

;; Record the parameters used in frame-initialize to make the initial frame.
(defvar frame-initial-frame-alist)

(defvar frame-initial-geometry-arguments nil)

;; startup.el calls this function before loading the user's init
;; file - if there is no frame with a minibuffer open now, create
;; one to display messages while loading the init file.
(defun frame-initialize ()
  "Create an initial frame if necessary."
  ;; Are we actually running under a window system at all?
  (if (and initial-window-system
	   (not noninteractive)
	   (not (eq initial-window-system 'pc)))
      (progn
	;; If there is no frame with a minibuffer besides the terminal
	;; frame, then we need to create the opening frame.  Make sure
	;; it has a minibuffer, but let initial-frame-alist omit the
	;; minibuffer spec.
	(or (delq terminal-frame (minibuffer-frame-list))
	    (progn
	      (setq frame-initial-frame-alist
		    (append initial-frame-alist default-frame-alist nil))
	      (or (assq 'horizontal-scroll-bars frame-initial-frame-alist)
		  (setq frame-initial-frame-alist
			(cons '(horizontal-scroll-bars . t)
			      frame-initial-frame-alist)))
	      (setq frame-initial-frame-alist
		    (cons (cons 'window-system initial-window-system)
			  frame-initial-frame-alist))
	      (setq default-minibuffer-frame
		    (setq frame-initial-frame
			  (make-frame frame-initial-frame-alist)))
	      ;; Delete any specifications for window geometry parameters
	      ;; so that we won't reapply them in frame-notice-user-settings.
	      ;; It would be wrong to reapply them then,
	      ;; because that would override explicit user resizing.
	      (setq initial-frame-alist
		    (frame-remove-geometry-params initial-frame-alist))))
	;; Copy the environment of the Emacs process into the new frame.
	(set-frame-parameter frame-initial-frame 'environment
			     (frame-parameter terminal-frame 'environment))
	;; At this point, we know that we have a frame open, so we
	;; can delete the terminal frame.
	(delete-frame terminal-frame)
	(setq terminal-frame nil))))

(defvar frame-notice-user-settings t
  "Non-nil means function `frame-notice-user-settings' wasn't run yet.")

(declare-function tool-bar-mode "tool-bar" (&optional arg))

;; startup.el calls this function after loading the user's init
;; file.  Now default-frame-alist and initial-frame-alist contain
;; information to which we must react; do what needs to be done.
(defun frame-notice-user-settings ()
  "Act on user's init file settings of frame parameters.
React to settings of `initial-frame-alist',
`window-system-default-frame-alist' and `default-frame-alist'
there (in decreasing order of priority)."
  ;; Creating and deleting frames may shift the selected frame around,
  ;; and thus the current buffer.  Protect against that.  We don't
  ;; want to use save-excursion here, because that may also try to set
  ;; the buffer of the selected window, which fails when the selected
  ;; window is the minibuffer.
  (let ((old-buffer (current-buffer))
	(window-system-frame-alist
         (cdr (assq initial-window-system
                    window-system-default-frame-alist))))

    (when (and frame-notice-user-settings
	       (null frame-initial-frame))
      ;; This case happens when we don't have a window system, and
      ;; also for MS-DOS frames.
      (let ((parms (frame-parameters)))
	;; Don't change the frame names.
	(setq parms (delq (assq 'name parms) parms))
	;; Can't modify the minibuffer parameter, so don't try.
	(setq parms (delq (assq 'minibuffer parms) parms))
	(modify-frame-parameters
	 nil
	 (if initial-window-system
	     parms
	   ;; initial-frame-alist and default-frame-alist were already
	   ;; applied in pc-win.el.
	   (append initial-frame-alist window-system-frame-alist
		   default-frame-alist parms nil)))
	(if (null initial-window-system) ;; MS-DOS does this differently in pc-win.el
	    (let ((newparms (frame-parameters))
		  (frame (selected-frame)))
	      (tty-handle-reverse-video frame newparms)
	      ;; If we changed the background color, we need to update
	      ;; the background-mode parameter, and maybe some faces,
	      ;; too.
	      (when (assq 'background-color newparms)
		(unless (or (assq 'background-mode initial-frame-alist)
			    (assq 'background-mode default-frame-alist))
		  (frame-set-background-mode frame))
		(face-set-after-frame-default frame))))))

    ;; If the initial frame is still around, apply initial-frame-alist
    ;; and default-frame-alist to it.
    (when (frame-live-p frame-initial-frame)

      ;; When tool-bar has been switched off, correct the frame size
      ;; by the lines added in x-create-frame for the tool-bar and
      ;; switch `tool-bar-mode' off.
      (when (display-graphic-p)
	(let ((tool-bar-lines (or (assq 'tool-bar-lines initial-frame-alist)
				  (assq 'tool-bar-lines window-system-frame-alist)
				  (assq 'tool-bar-lines default-frame-alist))))
	  (when (and tool-bar-originally-present
                     (or (null tool-bar-lines)
                         (null (cdr tool-bar-lines))
                         (eq 0 (cdr tool-bar-lines))))
	    (let* ((char-height (frame-char-height frame-initial-frame))
		   (image-height tool-bar-images-pixel-height)
		   (margin (cond ((and (consp tool-bar-button-margin)
				       (integerp (cdr tool-bar-button-margin))
				       (> tool-bar-button-margin 0))
				  (cdr tool-bar-button-margin))
				 ((and (integerp tool-bar-button-margin)
				       (> tool-bar-button-margin 0))
				  tool-bar-button-margin)
				 (t 0)))
		   (relief (if (and (integerp tool-bar-button-relief)
				    (> tool-bar-button-relief 0))
			       tool-bar-button-relief 3))
		   (lines (/ (+ image-height
				(* 2 margin)
				(* 2 relief)
				(1- char-height))
			     char-height))
		   (height (frame-parameter frame-initial-frame 'height))
		   (newparms (list (cons 'height (- height lines))))
		   (initial-top (cdr (assq 'top
					   frame-initial-geometry-arguments)))
		   (top (frame-parameter frame-initial-frame 'top)))
	      (when (and (consp initial-top) (eq '- (car initial-top)))
		(let ((adjusted-top
		       (cond ((and (consp top)
				   (eq '+ (car top)))
			      (list '+
				    (+ (cadr top)
				       (* lines char-height))))
			     ((and (consp top)
				   (eq '- (car top)))
			      (list '-
				    (- (cadr top)
				       (* lines char-height))))
			     (t (+ top (* lines char-height))))))
		  (setq newparms
			(append newparms
				`((top . ,adjusted-top))
				nil))))
	      (modify-frame-parameters frame-initial-frame newparms)
	      (tool-bar-mode -1)))))

      ;; The initial frame we create above always has a minibuffer.
      ;; If the user wants to remove it, or make it a minibuffer-only
      ;; frame, then we'll have to delete the current frame and make a
      ;; new one; you can't remove or add a root window to/from an
      ;; existing frame.
      ;;
      ;; NOTE: default-frame-alist was nil when we created the
      ;; existing frame.  We need to explicitly include
      ;; default-frame-alist in the parameters of the screen we
      ;; create here, so that its new value, gleaned from the user's
      ;; .emacs file, will be applied to the existing screen.
      (if (not (eq (cdr (or (assq 'minibuffer initial-frame-alist)
			    (assq 'minibuffer window-system-frame-alist)
			    (assq 'minibuffer default-frame-alist)
			    '(minibuffer . t)))
		   t))
	  ;; Create the new frame.
	  (let (parms new)
	    ;; If the frame isn't visible yet, wait till it is.
	    ;; If the user has to position the window,
	    ;; Emacs doesn't know its real position until
	    ;; the frame is seen to be visible.
	    (while (not (cdr (assq 'visibility
				   (frame-parameters frame-initial-frame))))
	      (sleep-for 1))
	    (setq parms (frame-parameters frame-initial-frame))

            ;; Get rid of `name' unless it was specified explicitly before.
	    (or (assq 'name frame-initial-frame-alist)
		(setq parms (delq (assq 'name parms) parms)))
	    ;; An explicit parent-id is a request to XEmbed the frame.
	    (or (assq 'parent-id frame-initial-frame-alist)
                (setq parms (delq (assq 'parent-id parms) parms)))

	    (setq parms (append initial-frame-alist
				window-system-frame-alist
				default-frame-alist
				parms
				nil))

	    ;; Get rid of `reverse', because that was handled
	    ;; when we first made the frame.
	    (setq parms (cons '(reverse) (delq (assq 'reverse parms) parms)))

	    (if (assq 'height frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'height parms)))
	    (if (assq 'width frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'width parms)))
	    (if (assq 'left frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'left parms)))
	    (if (assq 'top frame-initial-geometry-arguments)
		(setq parms (assq-delete-all 'top parms)))
	    (setq new
		  (make-frame
		   ;; Use the geometry args that created the existing
		   ;; frame, rather than the parms we get for it.
		   (append frame-initial-geometry-arguments
			   '((user-size . t) (user-position . t))
			   parms)))
	    ;; The initial frame, which we are about to delete, may be
	    ;; the only frame with a minibuffer.  If it is, create a
	    ;; new one.
	    (or (delq frame-initial-frame (minibuffer-frame-list))
		(make-initial-minibuffer-frame nil))

	    ;; If the initial frame is serving as a surrogate
	    ;; minibuffer frame for any frames, we need to wean them
	    ;; onto a new frame.  The default-minibuffer-frame
	    ;; variable must be handled similarly.
	    (let ((users-of-initial
		   (filtered-frame-list
                    (lambda (frame)
                      (and (not (eq frame frame-initial-frame))
                           (eq (window-frame
                                (minibuffer-window frame))
                               frame-initial-frame))))))
              (if (or users-of-initial
		      (eq default-minibuffer-frame frame-initial-frame))

		  ;; Choose an appropriate frame.  Prefer frames which
		  ;; are only minibuffers.
		  (let* ((new-surrogate
			  (car
			   (or (filtered-frame-list
                                (lambda (frame)
                                  (eq (cdr (assq 'minibuffer
                                                 (frame-parameters frame)))
                                      'only)))
			       (minibuffer-frame-list))))
			 (new-minibuffer (minibuffer-window new-surrogate)))

		    (if (eq default-minibuffer-frame frame-initial-frame)
			(setq default-minibuffer-frame new-surrogate))

		    ;; Wean the frames using frame-initial-frame as
		    ;; their minibuffer frame.
		    (dolist (frame users-of-initial)
                      (modify-frame-parameters
                       frame (list (cons 'minibuffer new-minibuffer)))))))

            ;; Redirect events enqueued at this frame to the new frame.
	    ;; Is this a good idea?
	    (redirect-frame-focus frame-initial-frame new)

	    ;; Finally, get rid of the old frame.
	    (delete-frame frame-initial-frame t))

	;; Otherwise, we don't need all that rigmarole; just apply
	;; the new parameters.
	(let (newparms allparms tail)
	  (setq allparms (append initial-frame-alist
				 window-system-frame-alist
				 default-frame-alist nil))
	  (if (assq 'height frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'height allparms)))
	  (if (assq 'width frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'width allparms)))
	  (if (assq 'left frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'left allparms)))
	  (if (assq 'top frame-initial-geometry-arguments)
	      (setq allparms (assq-delete-all 'top allparms)))
	  (setq tail allparms)
	  ;; Find just the parms that have changed since we first
	  ;; made this frame.  Those are the ones actually set by
          ;; the init file.  For those parms whose values we already knew
	  ;; (such as those spec'd by command line options)
	  ;; it is undesirable to specify the parm again
          ;; once the user has seen the frame and been able to alter it
	  ;; manually.
	  (let (newval oldval)
	    (dolist (entry tail)
	      (setq oldval (assq (car entry) frame-initial-frame-alist))
	      (setq newval (cdr (assq (car entry) allparms)))
	      (or (and oldval (eq (cdr oldval) newval))
		  (setq newparms
			(cons (cons (car entry) newval) newparms)))))
	  (setq newparms (nreverse newparms))

	  (let ((new-bg (assq 'background-color newparms)))
	    ;; If the `background-color' parameter is changed, apply
	    ;; it first, then make sure that the `background-mode'
	    ;; parameter and other faces are updated, before applying
	    ;; the other parameters.
	    (when new-bg
	      (modify-frame-parameters frame-initial-frame
				       (list new-bg))
	      (unless (assq 'background-mode newparms)
		(frame-set-background-mode frame-initial-frame))
	      (face-set-after-frame-default frame-initial-frame)
	      (setq newparms (delq new-bg newparms)))
	    (modify-frame-parameters frame-initial-frame newparms)))))

    ;; Restore the original buffer.
    (set-buffer old-buffer)

    ;; Make sure the initial frame can be GC'd if it is ever deleted.
    ;; Make sure frame-notice-user-settings does nothing if called twice.
    (setq frame-notice-user-settings nil)
    (setq frame-initial-frame nil)))

(defun make-initial-minibuffer-frame (display)
  (let ((parms (append minibuffer-frame-alist '((minibuffer . only)))))
    (if display
	(make-frame-on-display display parms)
      (make-frame parms))))

;;;; Creation of additional frames, and other frame miscellanea

(defun modify-all-frames-parameters (alist)
  "Modify all current and future frames' parameters according to ALIST.
This changes `default-frame-alist' and possibly `initial-frame-alist'.
Furthermore, this function removes all parameters in ALIST from
`window-system-default-frame-alist'.
See help of `modify-frame-parameters' for more information."
  (dolist (frame (frame-list))
    (modify-frame-parameters frame alist))

  (dolist (pair alist) ;; conses to add/replace
    ;; initial-frame-alist needs setting only when
    ;; frame-notice-user-settings is true.
    (and frame-notice-user-settings
	 (setq initial-frame-alist
	       (assq-delete-all (car pair) initial-frame-alist)))
    (setq default-frame-alist
	  (assq-delete-all (car pair) default-frame-alist))
    ;; Remove any similar settings from the window-system specific
    ;; parameters---they would override default-frame-alist.
    (dolist (w window-system-default-frame-alist)
      (setcdr w (assq-delete-all (car pair) (cdr w)))))

  (and frame-notice-user-settings
       (setq initial-frame-alist (append initial-frame-alist alist)))
  (setq default-frame-alist (append default-frame-alist alist)))

(defun get-other-frame ()
  "Return some frame other than the current frame.
Create one if necessary.  Note that the minibuffer frame, if separate,
is not considered (see `next-frame')."
  (let ((s (if (equal (next-frame (selected-frame)) (selected-frame))
	       (make-frame)
	     (next-frame (selected-frame)))))
    s))

(defun next-multiframe-window ()
  "Select the next window, regardless of which frame it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      0))
  (select-frame-set-input-focus (selected-frame)))

(defun previous-multiframe-window ()
  "Select the previous window, regardless of which frame it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  0))
  (select-frame-set-input-focus (selected-frame)))

(declare-function x-initialize-window-system "term/x-win" ())
(declare-function ns-initialize-window-system "term/ns-win" ())
(defvar x-display-name)                 ; term/x-win

(defun make-frame-on-display (display &optional parameters)
  "Make a frame on display DISPLAY.
The optional argument PARAMETERS specifies additional frame parameters."
  (interactive "sMake frame on display: ")
  (cond ((featurep 'ns)
	 (when (and (boundp 'ns-initialized) (not ns-initialized))
	   (setq x-display-name display)
	   (ns-initialize-window-system))
	 (make-frame `((window-system . ns)
		       (display . ,display) . ,parameters)))
	((eq system-type 'windows-nt)
	 ;; On Windows, ignore DISPLAY.
	 (make-frame parameters))
	(t
	 (unless (string-match-p "\\`[^:]*:[0-9]+\\(\\.[0-9]+\\)?\\'" display)
	   (error "Invalid display, not HOST:SERVER or HOST:SERVER.SCREEN"))
	 (when (and (boundp 'x-initialized) (not x-initialized))
	   (setq x-display-name display)
	   (x-initialize-window-system))
	 (make-frame `((window-system . x)
		       (display . ,display) . ,parameters)))))

(declare-function x-close-connection "xfns.c" (terminal))

(defun close-display-connection (display)
  "Close the connection to a display, deleting all its associated frames.
For DISPLAY, specify either a frame or a display name (a string).
If DISPLAY is nil, that stands for the selected frame's display."
  (interactive
   (list
    (let* ((default (frame-parameter nil 'display))
           (display (completing-read
                     (format "Close display (default %s): " default)
                     (delete-dups
                      (mapcar (lambda (frame)
                                (frame-parameter frame 'display))
                              (frame-list)))
                     nil t nil nil
                     default)))
      (if (zerop (length display)) default display))))
  (let ((frames (delq nil
                      (mapcar (lambda (frame)
                                (if (equal display
                                           (frame-parameter frame 'display))
                                    frame))
                              (frame-list)))))
    (if (and (consp frames)
             (not (y-or-n-p (if (cdr frames)
                                (format "Delete %s frames? " (length frames))
                              (format "Delete %s ? " (car frames))))))
        (error "Abort!")
      (mapc 'delete-frame frames)
      (x-close-connection display))))

(defun make-frame-command ()
  "Make a new frame, on the same terminal as the selected frame.
If the terminal is a text-only terminal, this also selects the
new frame."
  (interactive)
  (if (display-graphic-p)
      (make-frame)
    (select-frame (make-frame))))

(defvar before-make-frame-hook nil
  "Functions to run before a frame is created.")

(defvar after-make-frame-functions nil
  "Functions to run after a frame is created.
The functions are run with one arg, the newly created frame.")

(defvar after-setting-font-hook nil
  "Functions to run after a frame's font has been changed.")

;; Alias, kept temporarily.
(define-obsolete-function-alias 'new-frame 'make-frame "22.1")

(defvar frame-inherited-parameters '()
  ;; FIXME: Shouldn't we add `font' here as well?
  "Parameters `make-frame' copies from the `selected-frame' to the new frame.")

(defun make-frame (&optional parameters)
  "Return a newly created frame displaying the current buffer.
Optional argument PARAMETERS is an alist of frame parameters for
the new frame.  Each element of PARAMETERS should have the
form (NAME . VALUE), for example:

 (name . STRING)	The frame should be named STRING.

 (width . NUMBER)	The frame should be NUMBER characters in width.
 (height . NUMBER)	The frame should be NUMBER text lines high.

You cannot specify either `width' or `height', you must specify
neither or both.

 (minibuffer . t)	The frame should have a minibuffer.
 (minibuffer . nil)	The frame should have no minibuffer.
 (minibuffer . only)	The frame should contain only a minibuffer.
 (minibuffer . WINDOW)	The frame should use WINDOW as its minibuffer window.

 (window-system . nil)	The frame should be displayed on a terminal device.
 (window-system . x)	The frame should be displayed in an X window.

 (terminal . TERMINAL)  The frame should use the terminal object TERMINAL.

In addition, any parameter specified in `default-frame-alist',
but not present in PARAMETERS, is applied.

Before creating the frame (via `frame-creation-function-alist'),
this function runs the hook `before-make-frame-hook'.  After
creating the frame, it runs the hook `after-make-frame-functions'
with one arg, the newly created frame.

On graphical displays, this function does not itself make the new
frame the selected frame.  However, the window system may select
the new frame according to its own rules."
  (interactive)
  (let* ((w (cond
	     ((assq 'terminal parameters)
	      (let ((type (terminal-live-p (cdr (assq 'terminal parameters)))))
		(cond
		 ((eq type t) nil)
		 ((eq type nil) (error "Terminal %s does not exist"
                                       (cdr (assq 'terminal parameters))))
		 (t type))))
	     ((assq 'window-system parameters)
	      (cdr (assq 'window-system parameters)))
	     (t window-system)))
	 (frame-creation-function (cdr (assq w frame-creation-function-alist)))
	 (oldframe (selected-frame))
	 (params parameters)
	 frame)
    (unless frame-creation-function
      (error "Don't know how to create a frame on window system %s" w))
    ;; Add parameters from `window-system-default-frame-alist'.
    (dolist (p (cdr (assq w window-system-default-frame-alist)))
      (unless (assq (car p) params)
	(push p params)))
    ;; Add parameters from `default-frame-alist'.
    (dolist (p default-frame-alist)
      (unless (assq (car p) params)
	(push p params)))
    ;; Now make the frame.
    (run-hooks 'before-make-frame-hook)
    (setq frame (funcall frame-creation-function params))
    (normal-erase-is-backspace-setup-frame frame)
    ;; Inherit the original frame's parameters.
    (dolist (param frame-inherited-parameters)
      (unless (assq param parameters)   ;Overridden by explicit parameters.
        (let ((val (frame-parameter oldframe param)))
          (when val (set-frame-parameter frame param val)))))
    (run-hook-with-args 'after-make-frame-functions frame)
    frame))

(defun filtered-frame-list (predicate)
  "Return a list of all live frames which satisfy PREDICATE."
  (let* ((frames (frame-list))
	 (list frames))
    (while (consp frames)
      (unless (funcall predicate (car frames))
	(setcar frames nil))
      (setq frames (cdr frames)))
    (delq nil list)))

(defun minibuffer-frame-list ()
  "Return a list of all frames with their own minibuffers."
  (filtered-frame-list
   (lambda (frame)
     (eq frame (window-frame (minibuffer-window frame))))))

;; Used to be called `terminal-id' in termdev.el.
(defun get-device-terminal (device)
  "Return the terminal corresponding to DEVICE.
DEVICE can be a terminal, a frame, nil (meaning the selected frame's terminal),
the name of an X display device (HOST.SERVER.SCREEN) or a tty device file."
  (cond
   ((or (null device) (framep device))
    (frame-terminal device))
   ((stringp device)
    (let ((f (car (filtered-frame-list
                   (lambda (frame)
                     (or (equal (frame-parameter frame 'display) device)
                         (equal (frame-parameter frame 'tty) device)))))))
      (or f (error "Display %s does not exist" device))
      (frame-terminal f)))
   ((terminal-live-p device) device)
   (t
    (error "Invalid argument %s in `get-device-terminal'" device))))

(defun frames-on-display-list (&optional device)
  "Return a list of all frames on DEVICE.

DEVICE should be a terminal, a frame,
or a name of an X display or tty (a string of the form
HOST:SERVER.SCREEN).

If DEVICE is omitted or nil, it defaults to the selected
frame's terminal device."
  (let* ((terminal (get-device-terminal device))
	 (func #'(lambda (frame)
		   (eq (frame-terminal frame) terminal))))
    (filtered-frame-list func)))

(defun framep-on-display (&optional terminal)
  "Return the type of frames on TERMINAL.
TERMINAL may be a terminal id, a display name or a frame.  If it
is a frame, its type is returned.  If TERMINAL is omitted or nil,
it defaults to the selected frame's terminal device.  All frames
on a given display are of the same type."
  (or (terminal-live-p terminal)
      (framep terminal)
      (framep (car (frames-on-display-list terminal)))))

(defun frame-remove-geometry-params (param-list)
  "Return the parameter list PARAM-LIST, but with geometry specs removed.
This deletes all bindings in PARAM-LIST for `top', `left', `width',
`height', `user-size' and `user-position' parameters.
Emacs uses this to avoid overriding explicit moves and resizings from
the user during startup."
  (setq param-list (cons nil param-list))
  (let ((tail param-list))
    (while (consp (cdr tail))
      (if (and (consp (car (cdr tail)))
	       (memq (car (car (cdr tail)))
		     '(height width top left user-position user-size)))
	  (progn
	    (setq frame-initial-geometry-arguments
		  (cons (car (cdr tail)) frame-initial-geometry-arguments))
	    (setcdr tail (cdr (cdr tail))))
	(setq tail (cdr tail)))))
  (setq frame-initial-geometry-arguments
	(nreverse frame-initial-geometry-arguments))
  (cdr param-list))

(declare-function x-focus-frame "xfns.c" (frame))

(defun select-frame-set-input-focus (frame &optional norecord)
  "Select FRAME, raise it, and set input focus, if possible.
If `mouse-autoselect-window' is non-nil, also move mouse pointer
to FRAME's selected window.  Otherwise, if `focus-follows-mouse'
is non-nil, move mouse cursor to FRAME.

Optional argument NORECORD means to neither change the order of
recently selected windows nor the buffer list."
  (select-frame frame norecord)
  (raise-frame frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  (when (memq (window-system frame) '(x w32 ns))
    (x-focus-frame frame))
  ;; Move mouse cursor if necessary.
  (cond
   (mouse-autoselect-window
    (let ((edges (window-inside-edges (frame-selected-window frame))))
      ;; Move mouse cursor into FRAME's selected window to avoid that
      ;; Emacs mouse-autoselects another window.
      (set-mouse-position frame (nth 2 edges) (nth 1 edges))))
   (focus-follows-mouse
    ;; Move mouse cursor into FRAME to avoid that another frame gets
    ;; selected by the window manager.
    (set-mouse-position frame (1- (frame-width frame)) 0))))

(defun other-frame (arg)
  "Select the ARGth different visible frame on current display, and raise it.
All frames are arranged in a cyclic order.
This command selects the frame ARG steps away in that order.
A negative ARG moves in the opposite order.

To make this command work properly, you must tell Emacs
how the system (or the window manager) generally handles
focus-switching between windows.  If moving the mouse onto a window
selects it (gives it focus), set `focus-follows-mouse' to t.
Otherwise, that variable should be nil."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame))
      (while (not (eq (frame-visible-p frame) t))
	(setq frame (next-frame frame)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame))
      (while (not (eq (frame-visible-p frame) t))
	(setq frame (previous-frame frame)))
      (setq arg (1+ arg)))
    (select-frame-set-input-focus frame)))

(defun iconify-or-deiconify-frame ()
  "Iconify the selected frame, or deiconify if it's currently an icon."
  (interactive)
  (if (eq (cdr (assq 'visibility (frame-parameters))) t)
      (iconify-frame)
    (make-frame-visible)))

(defun suspend-frame ()
  "Do whatever is right to suspend the current frame.
Calls `suspend-emacs' if invoked from the controlling tty device,
`suspend-tty' from a secondary tty device, and
`iconify-or-deiconify-frame' from an X frame."
  (interactive)
  (let ((type (framep (selected-frame))))
    (cond
     ((memq type '(x ns w32)) (iconify-or-deiconify-frame))
     ((eq type t)
      (if (controlling-tty-p)
	  (suspend-emacs)
	(suspend-tty)))
     (t (suspend-emacs)))))

(defun make-frame-names-alist ()
  ;; Only consider the frames on the same display.
  (let* ((current-frame (selected-frame))
	 (falist
	  (cons
	   (cons (frame-parameter current-frame 'name) current-frame) nil))
	 (frame (next-frame nil 0)))
    (while (not (eq frame current-frame))
      (progn
	(push (cons (frame-parameter frame 'name) frame) falist)
	(setq frame (next-frame frame 0))))
    falist))

(defvar frame-name-history nil)
(defun select-frame-by-name (name)
  "Select the frame on the current terminal whose name is NAME and raise it.
If there is no frame by that name, signal an error."
  (interactive
   (let* ((frame-names-alist (make-frame-names-alist))
	   (default (car (car frame-names-alist)))
	   (input (completing-read
		   (format "Select Frame (default %s): " default)
		   frame-names-alist nil t nil 'frame-name-history)))
     (if (= (length input) 0)
	 (list default)
       (list input))))
  (let* ((frame-names-alist (make-frame-names-alist))
	 (frame (cdr (assoc name frame-names-alist))))
    (if frame
	(select-frame-set-input-focus frame)
      (error "There is no frame named `%s'" name))))


;;;; Background mode.

(defcustom frame-background-mode nil
  "The brightness of the background.
Set this to the symbol `dark' if your background color is dark,
`light' if your background is light, or nil (automatic by default)
if you want Emacs to examine the brightness for you.  Don't set this
variable with `setq'; this won't have the expected effect."
  :group 'faces
  :set #'(lambda (var value)
	   (set-default var value)
	   (mapc 'frame-set-background-mode (frame-list)))
  :initialize 'custom-initialize-changed
  :type '(choice (const dark)
		 (const light)
		 (const :tag "automatic" nil)))

(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))

(defvar inhibit-frame-set-background-mode nil)

(defun frame-set-background-mode (frame &optional keep-face-specs)
  "Set up display-dependent faces on FRAME.
Display-dependent faces are those which have different definitions
according to the `background-mode' and `display-type' frame parameters.

If optional arg KEEP-FACE-SPECS is non-nil, don't recalculate
face specs for the new background mode."
  (unless inhibit-frame-set-background-mode
    (let* ((frame-default-bg-mode (frame-terminal-default-bg-mode frame))
	   (bg-color (frame-parameter frame 'background-color))
	   (tty-type (tty-type frame))
	   (default-bg-mode
	     (if (or (window-system frame)
		     (and tty-type
			  (string-match "^\\(xterm\\|\\rxvt\\|dtterm\\|eterm\\)"
					tty-type)))
		 'light
	       'dark))
	   (non-default-bg-mode (if (eq default-bg-mode 'light) 'dark 'light))
	   (bg-mode
	    (cond (frame-default-bg-mode)
		  ((equal bg-color "unspecified-fg") ; inverted colors
		   non-default-bg-mode)
		  ((not (color-values bg-color frame))
		   default-bg-mode)
		  ((>= (apply '+ (color-values bg-color frame))
		       ;; Just looking at the screen, colors whose
		       ;; values add up to .6 of the white total
		       ;; still look dark to me.
		       (* (apply '+ (color-values "white" frame)) .6))
		   'light)
		  (t 'dark)))
	   (display-type
	    (cond ((null (window-system frame))
		   (if (tty-display-color-p frame) 'color 'mono))
		  ((display-color-p frame)
		   'color)
		  ((x-display-grayscale-p frame)
		   'grayscale)
		  (t 'mono)))
	   (old-bg-mode
	    (frame-parameter frame 'background-mode))
	   (old-display-type
	    (frame-parameter frame 'display-type)))

      (unless (and (eq bg-mode old-bg-mode) (eq display-type old-display-type))
	(let ((locally-modified-faces nil)
	      ;; Prevent face-spec-recalc from calling this function
	      ;; again, resulting in a loop (bug#911).
	      (inhibit-frame-set-background-mode t)
	      (params (list (cons 'background-mode bg-mode)
			    (cons 'display-type display-type))))
	  (if keep-face-specs
	      (modify-frame-parameters frame params)
	    ;; If we are recomputing face specs, first collect a list
	    ;; of faces that don't match their face-specs.  These are
	    ;; the faces modified on FRAME, and we avoid changing them
	    ;; below.  Use a negative list to avoid consing (we assume
	    ;; most faces are unmodified).
	    (dolist (face (face-list))
	      (and (not (get face 'face-override-spec))
		   (not (face-spec-match-p face
					   (face-user-default-spec face)
					   (selected-frame)))
		   (push face locally-modified-faces)))
	    ;; Now change to the new frame parameters
	    (modify-frame-parameters frame params)
	    ;; For all unmodified named faces, choose face specs
	    ;; matching the new frame parameters.
	    (dolist (face (face-list))
	      (unless (memq face locally-modified-faces)
		(face-spec-recalc face frame)))))))))

(defun frame-terminal-default-bg-mode (frame)
  "Return the default background mode of FRAME.
This checks the `frame-background-mode' variable, the X resource
named \"backgroundMode\" (if FRAME is an X frame), and finally
the `background-mode' terminal parameter."
  (or frame-background-mode
      (let ((bg-resource
	     (and (window-system frame)
		  (x-get-resource "backgroundMode" "BackgroundMode"))))
	(if bg-resource
	    (intern (downcase bg-resource))))
      (terminal-parameter frame 'background-mode)))


;;;; Frame configurations

(defun current-frame-configuration ()
  "Return a list describing the positions and states of all frames.
Its car is `frame-configuration'.
Each element of the cdr is a list of the form (FRAME ALIST WINDOW-CONFIG),
where
  FRAME is a frame object,
  ALIST is an association list specifying some of FRAME's parameters, and
  WINDOW-CONFIG is a window configuration object for FRAME."
  (cons 'frame-configuration
	(mapcar (lambda (frame)
                  (list frame
                        (frame-parameters frame)
                        (current-window-configuration frame)))
		(frame-list))))

(defun set-frame-configuration (configuration &optional nodelete)
  "Restore the frames to the state described by CONFIGURATION.
Each frame listed in CONFIGURATION has its position, size, window
configuration, and other parameters set as specified in CONFIGURATION.
However, this function does not restore deleted frames.

Ordinarily, this function deletes all existing frames not
listed in CONFIGURATION.  But if optional second argument NODELETE
is given and non-nil, the unwanted frames are iconified instead."
  (or (frame-configuration-p configuration)
      (signal 'wrong-type-argument
	      (list 'frame-configuration-p configuration)))
  (let ((config-alist (cdr configuration))
	frames-to-delete)
    (dolist (frame (frame-list))
      (let ((parameters (assq frame config-alist)))
        (if parameters
            (progn
              (modify-frame-parameters
               frame
               ;; Since we can't set a frame's minibuffer status,
               ;; we might as well omit the parameter altogether.
               (let* ((parms (nth 1 parameters))
		      (mini (assq 'minibuffer parms))
		      (name (assq 'name parms))
		      (explicit-name (cdr (assq 'explicit-name parms))))
		 (when mini (setq parms (delq mini parms)))
		 ;; Leave name in iff it was set explicitly.
		 ;; This should fix the behavior reported in
		 ;; http://lists.gnu.org/archive/html/emacs-devel/2007-08/msg01632.html
		 (when (and name (not explicit-name))
		   (setq parms (delq name parms)))
                 parms))
              (set-window-configuration (nth 2 parameters)))
          (setq frames-to-delete (cons frame frames-to-delete)))))
    (mapc (if nodelete
              ;; Note: making frames invisible here was tried
              ;; but led to some strange behavior--each time the frame
              ;; was made visible again, the window manager asked afresh
              ;; for where to put it.
              'iconify-frame
            'delete-frame)
          frames-to-delete)))

;;;; Convenience functions for accessing and interactively changing
;;;; frame parameters.

(defun frame-height (&optional frame)
  "Return number of lines available for display on FRAME.
If FRAME is omitted, describe the currently selected frame.
Exactly what is included in the return value depends on the
window-system and toolkit in use - see `frame-pixel-height' for
more details.  The lines are in units of the default font height.

The result is roughly related to the frame pixel height via
height in pixels = height in lines * `frame-char-height'.
However, this is only approximate, and is complicated e.g. by the
fact that individual window lines and menu bar lines can have
differing font heights."
  (cdr (assq 'height (frame-parameters frame))))

(defun frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (cdr (assq 'width (frame-parameters frame))))

(declare-function x-list-fonts "xfaces.c"
                  (pattern &optional face frame maximum width))

(define-obsolete-function-alias 'set-default-font 'set-frame-font "23.1")

(defun set-frame-font (font-name &optional keep-size frames)
  "Set the default font to FONT-NAME.
When called interactively, prompt for the name of a font, and use
that font on the selected frame.

If KEEP-SIZE is nil, keep the number of frame lines and columns
fixed.  If KEEP-SIZE is non-nil (or with a prefix argument), try
to keep the current frame size fixed (in pixels) by adjusting the
number of lines and columns.

If FRAMES is nil, apply the font to the selected frame only.
If FRAMES is non-nil, it should be a list of frames to act upon,
or t meaning all graphical frames.  Also, if FRAME is non-nil,
alter the user's Customization settings as though the
font-related attributes of the `default' face had been \"set in
this session\", so that the font is applied to future frames."
  (interactive
   (let* ((completion-ignore-case t)
	  (font (completing-read "Font name: "
				 ;; x-list-fonts will fail with an error
				 ;; if this frame doesn't support fonts.
				 (x-list-fonts "*" nil (selected-frame))
                                 nil nil nil nil
                                 (frame-parameter nil 'font))))
     (list font current-prefix-arg nil)))
  (when (stringp font-name)
    (let* ((this-frame (selected-frame))
	   ;; FRAMES nil means affect the selected frame.
	   (frame-list (cond ((null frames)
			      (list this-frame))
			     ((eq frames t)
			      (frame-list))
			     (t frames)))
	   height width)
      (dolist (f frame-list)
	(when (display-multi-font-p f)
	  (if keep-size
	      (setq height (* (frame-parameter f 'height)
			      (frame-char-height f))
		    width  (* (frame-parameter f 'width)
			      (frame-char-width f))))
	  ;; When set-face-attribute is called for :font, Emacs
	  ;; guesses the best font according to other face attributes
	  ;; (:width, :weight, etc.) so reset them too (Bug#2476).
	  (set-face-attribute 'default f
			      :width 'normal :weight 'normal
			      :slant 'normal :font font-name)
	  (if keep-size
	      (modify-frame-parameters
	       f
	       (list (cons 'height (round height (frame-char-height f)))
		     (cons 'width  (round width  (frame-char-width f))))))))
      (when frames
	;; Alter the user's Custom setting of the `default' face, but
	;; only for font-related attributes.
	(let ((specs (cadr (assq 'user (get 'default 'theme-face))))
	      (attrs '(:family :foundry :slant :weight :height :width))
	      (new-specs nil))
	  (if (null specs) (setq specs '((t nil))))
	  (dolist (spec specs)
	    ;; Each SPEC has the form (DISPLAY ATTRIBUTE-PLIST)
	    (let ((display (nth 0 spec))
		  (plist   (copy-tree (nth 1 spec))))
	      ;; Alter only DISPLAY conditions matching this frame.
	      (when (or (memq display '(t default))
			(face-spec-set-match-display display this-frame))
		(dolist (attr attrs)
		  (setq plist (plist-put plist attr
					 (face-attribute 'default attr)))))
	      (push (list display plist) new-specs)))
	  (setq new-specs (nreverse new-specs))
	  (put 'default 'customized-face new-specs)
	  (custom-push-theme 'theme-face 'default 'user 'set new-specs)
	  (put 'default 'face-modified nil))))
    (run-hooks 'after-setting-font-hook 'after-setting-font-hooks)))

(defun set-frame-parameter (frame parameter value)
  "Set frame parameter PARAMETER to VALUE on FRAME.
If FRAME is nil, it defaults to the selected frame.
See `modify-frame-parameters'."
  (modify-frame-parameters frame (list (cons parameter value))))

(defun set-background-color (color-name)
  "Set the background color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current background color, use `frame-parameters'."
  (interactive (list (read-color "Background color: ")))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'background-color color-name)))
  (or window-system
      (face-set-after-frame-default (selected-frame))))

(defun set-foreground-color (color-name)
  "Set the foreground color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current foreground color, use `frame-parameters'."
  (interactive (list (read-color "Foreground color: ")))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'foreground-color color-name)))
  (or window-system
      (face-set-after-frame-default (selected-frame))))

(defun set-cursor-color (color-name)
  "Set the text cursor color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
This works by setting the `cursor-color' frame parameter on the
selected frame.

You can also set the text cursor color, for all frames, by
customizing the `cursor' face."
  (interactive (list (read-color "Cursor color: ")))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'cursor-color color-name))))

(defun set-mouse-color (color-name)
  "Set the color of the mouse pointer of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current mouse color, use `frame-parameters'."
  (interactive (list (read-color "Mouse color: ")))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'mouse-color
				       (or color-name
					   (cdr (assq 'mouse-color
						      (frame-parameters))))))))

(defun set-border-color (color-name)
  "Set the color of the border of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current border color, use `frame-parameters'."
  (interactive (list (read-color "Border color: ")))
  (modify-frame-parameters (selected-frame)
			   (list (cons 'border-color color-name))))

(define-minor-mode auto-raise-mode
  "Toggle whether or not selected frames should auto-raise.
With a prefix argument ARG, enable Auto Raise mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Auto Raise mode does nothing under most window managers, which
switch focus on mouse clicks.  It only has an effect if your
window manager switches focus on mouse movement (in which case
you should also change `focus-follows-mouse' to t).  Then,
enabling Auto Raise mode causes any graphical Emacs frame which
acquires focus to be automatically raised.

Note that this minor mode controls Emacs's own auto-raise
feature.  Window managers that switch focus on mouse movement
often have their own auto-raise feature."
  :variable (frame-parameter nil 'auto-raise)
  (if (frame-parameter nil 'auto-raise)
      (raise-frame)))

(define-minor-mode auto-lower-mode
  "Toggle whether or not the selected frame should auto-lower.
With a prefix argument ARG, enable Auto Lower mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Auto Lower mode does nothing under most window managers, which
switch focus on mouse clicks.  It only has an effect if your
window manager switches focus on mouse movement (in which case
you should also change `focus-follows-mouse' to t).  Then,
enabling Auto Lower Mode causes any graphical Emacs frame which
loses focus to be automatically lowered.

Note that this minor mode controls Emacs's own auto-lower
feature.  Window managers that switch focus on mouse movement
often have their own features for raising or lowering frames."
  :variable (frame-parameter nil 'auto-lower))

(defun set-frame-name (name)
  "Set the name of the selected frame to NAME.
When called interactively, prompt for the name of the frame.
The frame name is displayed on the modeline if the terminal displays only
one frame, otherwise the name is displayed on the frame's caption bar."
  (interactive "sFrame name: ")
  (modify-frame-parameters (selected-frame)
			   (list (cons 'name name))))

(defun frame-current-scroll-bars (&optional frame)
  "Return the current scroll-bar settings in frame FRAME.
Value is a cons (VERTICAL . HORIZ0NTAL) where VERTICAL specifies the
current location of the vertical scroll-bars (left, right, or nil),
and HORIZONTAL specifies the current location of the horizontal scroll
bars (top, bottom, or nil)."
  (let ((vert (frame-parameter frame 'vertical-scroll-bars))
	(hor nil))
    (unless (memq vert '(left right nil))
      (setq vert default-frame-scroll-bars))
    (cons vert hor)))

;;;; Frame/display capabilities.
(defun selected-terminal ()
  "Return the terminal that is now selected."
  (frame-terminal (selected-frame)))

(declare-function msdos-mouse-p "dosfns.c")

(defun display-mouse-p (&optional display)
  "Return non-nil if DISPLAY has a mouse available.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((eq frame-type 'pc)
      (msdos-mouse-p))
     ((eq system-type 'windows-nt)
      (with-no-warnings
       (> w32-num-mouse-buttons 0)))
     ((memq frame-type '(x ns))
      t)    ;; We assume X and NeXTstep *always* have a pointing device
     (t
      (or (and (featurep 'xt-mouse)
	       xterm-mouse-mode)
	  ;; t-mouse is distributed with the GPM package.  It doesn't have
	  ;; a toggle.
	  (featurep 't-mouse))))))

(defun display-popup-menus-p (&optional display)
  "Return non-nil if popup menus are supported on DISPLAY.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display).
Support for popup menus requires that the mouse be available."
  (and
   (let ((frame-type (framep-on-display display)))
     (memq frame-type '(x w32 pc ns)))
   (display-mouse-p display)))

(defun display-graphic-p (&optional display)
  "Return non-nil if DISPLAY is a graphic display.
Graphical displays are those which are capable of displaying several
frames and several different fonts at once.  This is true for displays
that use a window system such as X, and false for text-only terminals.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (not (null (memq (framep-on-display display) '(x w32 ns)))))

(defun display-images-p (&optional display)
  "Return non-nil if DISPLAY can display images.

DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (and (display-graphic-p display)
       (fboundp 'image-mask-p)
       (fboundp 'image-size)))

(defalias 'display-multi-frame-p 'display-graphic-p)
(defalias 'display-multi-font-p 'display-graphic-p)

(defun display-selections-p (&optional display)
  "Return non-nil if DISPLAY supports selections.
A selection is a way to transfer text or other data between programs
via special system buffers called `selection' or `clipboard'.
DISPLAY can be a display name, a frame, or nil (meaning the selected
frame's display)."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((eq frame-type 'pc)
      ;; MS-DOG frames support selections when Emacs runs inside
      ;; the Windows' DOS Box.
      (with-no-warnings
       (not (null dos-windows-version))))
     ((memq frame-type '(x w32 ns))
      t)    ;; FIXME?
     (t
      nil))))

(declare-function x-display-screens "xfns.c" (&optional terminal))

(defun display-screens (&optional display)
  "Return the number of screens associated with DISPLAY."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-screens display))
     (t
      1))))

(declare-function x-display-pixel-height "xfns.c" (&optional terminal))

(defun display-pixel-height (&optional display)
  "Return the height of DISPLAY's screen in pixels.
For character terminals, each character counts as a single pixel."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-pixel-height display))
     (t
      (frame-height (if (framep display) display (selected-frame)))))))

(declare-function x-display-pixel-width "xfns.c" (&optional terminal))

(defun display-pixel-width (&optional display)
  "Return the width of DISPLAY's screen in pixels.
For character terminals, each character counts as a single pixel."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-pixel-width display))
     (t
      (frame-width (if (framep display) display (selected-frame)))))))

(defcustom display-mm-dimensions-alist nil
  "Alist for specifying screen dimensions in millimeters.
The dimensions will be used for `display-mm-height' and
`display-mm-width' if defined for the respective display.

Each element of the alist has the form (display . (width . height)),
e.g. (\":0.0\" . (287 . 215)).

If `display' equals t, it specifies dimensions for all graphical
displays not explicitly specified."
  :version "22.1"
  :type '(alist :key-type (choice (string :tag "Display name")
				  (const :tag "Default" t))
		:value-type (cons :tag "Dimensions"
				  (integer :tag "Width")
				  (integer :tag "Height")))
  :group 'frames)

(declare-function x-display-mm-height "xfns.c" (&optional terminal))

(defun display-mm-height (&optional display)
  "Return the height of DISPLAY's screen in millimeters.
System values can be overridden by `display-mm-dimensions-alist'.
If the information is unavailable, value is nil."
  (and (memq (framep-on-display display) '(x w32 ns))
       (or (cddr (assoc (or display (frame-parameter nil 'display))
			display-mm-dimensions-alist))
	   (cddr (assoc t display-mm-dimensions-alist))
	   (x-display-mm-height display))))

(declare-function x-display-mm-width "xfns.c" (&optional terminal))

(defun display-mm-width (&optional display)
  "Return the width of DISPLAY's screen in millimeters.
System values can be overridden by `display-mm-dimensions-alist'.
If the information is unavailable, value is nil."
  (and (memq (framep-on-display display) '(x w32 ns))
       (or (cadr (assoc (or display (frame-parameter nil 'display))
			display-mm-dimensions-alist))
	   (cadr (assoc t display-mm-dimensions-alist))
	   (x-display-mm-width display))))

(declare-function x-display-backing-store "xfns.c" (&optional terminal))

(defun display-backing-store (&optional display)
  "Return the backing store capability of DISPLAY's screen.
The value may be `always', `when-mapped', `not-useful', or nil if
the question is inapplicable to a certain kind of display."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-backing-store display))
     (t
      'not-useful))))

(declare-function x-display-save-under "xfns.c" (&optional terminal))

(defun display-save-under (&optional display)
  "Return non-nil if DISPLAY's screen supports the SaveUnder feature."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-save-under display))
     (t
      'not-useful))))

(declare-function x-display-planes "xfns.c" (&optional terminal))

(defun display-planes (&optional display)
  "Return the number of planes supported by DISPLAY."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-planes display))
     ((eq frame-type 'pc)
      4)
     (t
      (truncate (log (length (tty-color-alist)) 2))))))

(declare-function x-display-color-cells "xfns.c" (&optional terminal))

(defun display-color-cells (&optional display)
  "Return the number of color cells supported by DISPLAY."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-color-cells display))
     ((eq frame-type 'pc)
      16)
     (t
      (tty-display-color-cells display)))))

(declare-function x-display-visual-class "xfns.c" (&optional terminal))

(defun display-visual-class (&optional display)
  "Return the visual class of DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 ns))
      (x-display-visual-class display))
     ((and (memq frame-type '(pc t))
	   (tty-display-color-p display))
      'static-color)
     (t
      'static-gray))))


;;;; Frame geometry values

(defun frame-geom-value-cons (type value &optional frame)
  "Return equivalent geometry value for FRAME as a cons with car `+'.
A geometry value equivalent to VALUE for FRAME is returned,
where the value is a cons with car `+', not numeric.
TYPE is the car of the original geometry spec (TYPE . VALUE).
   It is `top' or `left', depending on which edge VALUE is related to.
VALUE is the cdr of a frame geometry spec: (left/top . VALUE).
If VALUE is a number, then it is converted to a cons value, perhaps
   relative to the opposite frame edge from that in the original spec.
FRAME defaults to the selected frame.

Examples (measures in pixels) -
 Assuming display height/width=1024, frame height/width=600:
 300 inside display edge:                   300  => (+  300)
                                        (+  300) => (+  300)
 300 inside opposite display edge:      (-  300) => (+  124)
                                           -300  => (+  124)
 300 beyond display edge
  (= 724 inside opposite display edge): (+ -300) => (+ -300)
 300 beyond display edge
  (= 724 inside opposite display edge): (- -300) => (+  724)

In the 3rd, 4th, and 6th examples, the returned value is relative to
the opposite frame edge from the edge indicated in the input spec."
  (cond ((and (consp value) (eq '+ (car value))) ; e.g. (+ 300), (+ -300)
         value)
        ((natnump value) (list '+ value)) ; e.g. 300 => (+ 300)
        (t                              ; e.g. -300, (- 300), (- -300)
         (list '+ (- (if (eq 'left type) ; => (+ 124), (+ 124), (+ 724)
                         (x-display-pixel-width)
                       (x-display-pixel-height))
                     (if (integerp value) (- value) (cadr value))
                     (if (eq 'left type)
                         (frame-pixel-width frame)
                       (frame-pixel-height frame)))))))

(defun frame-geom-spec-cons (spec &optional frame)
  "Return equivalent geometry spec for FRAME as a cons with car `+'.
A geometry specification equivalent to SPEC for FRAME is returned,
where the value is a cons with car `+', not numeric.
SPEC is a frame geometry spec: (left . VALUE) or (top . VALUE).
If VALUE is a number, then it is converted to a cons value, perhaps
   relative to the opposite frame edge from that in the original spec.
FRAME defaults to the selected frame.

Examples (measures in pixels) -
 Assuming display height=1024, frame height=600:
 top 300 below display top:               (top .  300) => (top +  300)
                                          (top +  300) => (top +  300)
 bottom 300 above display bottom:         (top -  300) => (top +  124)
                                          (top . -300) => (top +  124)
 top 300 above display top
  (= bottom 724 above display bottom):    (top + -300) => (top + -300)
 bottom 300 below display bottom
  (= top 724 below display top):          (top - -300) => (top +  724)

In the 3rd, 4th, and 6th examples, the returned value is relative to
the opposite frame edge from the edge indicated in the input spec."
  (cons (car spec) (frame-geom-value-cons (car spec) (cdr spec) frame)))


(defun delete-other-frames (&optional frame)
  "Delete all frames on the current terminal, except FRAME.
If FRAME uses another frame's minibuffer, the minibuffer frame is
left untouched.  FRAME nil or omitted means use the selected frame."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (let* ((mini-frame (window-frame (minibuffer-window frame)))
	 (frames (delq mini-frame (delq frame (frame-list)))))
    ;; Only consider frames on the same terminal.
    (dolist (frame (prog1 frames (setq frames nil)))
      (if (eq (frame-terminal) (frame-terminal frame))
          (push frame frames)))
    ;; Delete mon-minibuffer-only frames first, because `delete-frame'
    ;; signals an error when trying to delete a mini-frame that's
    ;; still in use by another frame.
    (dolist (frame frames)
      (unless (eq (frame-parameter frame 'minibuffer) 'only)
	(delete-frame frame)))
    ;; Delete minibuffer-only frames.
    (dolist (frame frames)
      (when (eq (frame-parameter frame 'minibuffer) 'only)
	(delete-frame frame)))))

;; miscellaneous obsolescence declarations
(define-obsolete-variable-alias 'delete-frame-hook
    'delete-frame-functions "22.1")


;; Highlighting trailing whitespace.

(make-variable-buffer-local 'show-trailing-whitespace)


;; Scrolling

(defgroup scrolling nil
  "Scrolling windows."
  :version "21.1"
  :group 'frames)

(defvaralias 'automatic-hscrolling 'auto-hscroll-mode)


;; Blinking cursor

(defgroup cursor nil
  "Displaying text cursors."
  :version "21.1"
  :group 'frames)

(defcustom blink-cursor-delay 0.5
  "Seconds of idle time after which cursor starts to blink."
  :type 'number
  :group 'cursor)

(defcustom blink-cursor-interval 0.5
  "Length of cursor blink interval in seconds."
  :type 'number
  :group 'cursor)

(defvar blink-cursor-idle-timer nil
  "Timer started after `blink-cursor-delay' seconds of Emacs idle time.
The function `blink-cursor-start' is called when the timer fires.")

(defvar blink-cursor-timer nil
  "Timer started from `blink-cursor-start'.
This timer calls `blink-cursor-timer-function' every
`blink-cursor-interval' seconds.")

(defun blink-cursor-start ()
  "Timer function called from the timer `blink-cursor-idle-timer'.
This starts the timer `blink-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null blink-cursor-timer)
    ;; Set up the timer first, so that if this signals an error,
    ;; blink-cursor-end is not added to pre-command-hook.
    (setq blink-cursor-timer
	  (run-with-timer blink-cursor-interval blink-cursor-interval
			  'blink-cursor-timer-function))
    (add-hook 'pre-command-hook 'blink-cursor-end)
    (internal-show-cursor nil nil)))

(defun blink-cursor-timer-function ()
  "Timer function of timer `blink-cursor-timer'."
  (internal-show-cursor nil (not (internal-show-cursor-p))))

(defun blink-cursor-end ()
  "Stop cursor blinking.
This is installed as a pre-command hook by `blink-cursor-start'.
When run, it cancels the timer `blink-cursor-timer' and removes
itself as a pre-command hook."
  (remove-hook 'pre-command-hook 'blink-cursor-end)
  (internal-show-cursor nil t)
  (when blink-cursor-timer
    (cancel-timer blink-cursor-timer)
    (setq blink-cursor-timer nil)))

(define-minor-mode blink-cursor-mode
  "Toggle cursor blinking (Blink Cursor mode).
With a prefix argument ARG, enable Blink Cursor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This command is effective only on graphical frames.  On text-only
terminals, cursor blinking is controlled by the terminal."
  :init-value (not (or noninteractive
		       no-blinking-cursor
		       (eq system-type 'ms-dos)
		       (not (memq window-system '(x w32 ns)))))
  :initialize 'custom-initialize-delay
  :group 'cursor
  :global t
  (if blink-cursor-idle-timer (cancel-timer blink-cursor-idle-timer))
  (setq blink-cursor-idle-timer nil)
  (blink-cursor-end)
  (when blink-cursor-mode
    ;; Hide the cursor.
    ;;(internal-show-cursor nil nil)
    (setq blink-cursor-idle-timer
          (run-with-idle-timer blink-cursor-delay
                               blink-cursor-delay
                               'blink-cursor-start))))

(define-obsolete-variable-alias 'blink-cursor 'blink-cursor-mode "22.1")


;;;; Key bindings

(define-key ctl-x-5-map "2" 'make-frame-command)
(define-key ctl-x-5-map "1" 'delete-other-frames)
(define-key ctl-x-5-map "0" 'delete-frame)
(define-key ctl-x-5-map "o" 'other-frame)

(provide 'frame)

;;; frame.el ends here
