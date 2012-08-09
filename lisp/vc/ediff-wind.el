;;; ediff-wind.el --- window manipulation utilities

;; Copyright (C) 1994-1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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


;; Compiler pacifier
(defvar icon-title-format)
(defvar top-toolbar-height)
(defvar bottom-toolbar-height)
(defvar left-toolbar-height)
(defvar right-toolbar-height)
(defvar left-toolbar-width)
(defvar right-toolbar-width)
(defvar default-menubar)
(defvar top-gutter)
(defvar frame-icon-title-format)
(defvar ediff-diff-status)

;; declare-function does not exist in XEmacs
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest  r))))

(eval-when-compile
  (require 'ediff-util)
  (require 'ediff-help))
;; end pacifier

(require 'ediff-init)

;; be careful with ediff-tbar
(if (featurep 'xemacs)
    (require 'ediff-tbar)
  (defun ediff-compute-toolbar-width () 0))

(defgroup ediff-window nil
  "Ediff window manipulation."
  :prefix "ediff-"
  :group 'ediff
  :group 'frames)


;; Determine which window setup function to use based on current window system.
(defun ediff-choose-window-setup-function-automatically ()
  (if (ediff-window-display-p)
      'ediff-setup-windows-multiframe
    'ediff-setup-windows-plain))

(defcustom ediff-window-setup-function (ediff-choose-window-setup-function-automatically)
  "Function called to set up windows.
Ediff provides a choice of two functions: `ediff-setup-windows-plain', for
doing everything in one frame and `ediff-setup-windows-multiframe', which sets
the control panel in a separate frame. By default, the appropriate function is
chosen automatically depending on the current window system.
However, `ediff-toggle-multiframe' can be used to toggle between the multiframe
display and the single frame display.
If the multiframe function detects that one of the buffers A/B is seen in some
other frame, it will try to keep that buffer in that frame.

If you don't like any of the two provided functions, write your own one.
The basic guidelines:
    1. It should leave the control buffer current and the control window
       selected.
    2. It should set `ediff-window-A', `ediff-window-B', `ediff-window-C',
       and `ediff-control-window' to contain window objects that display
       the corresponding buffers.
    3. It should accept the following arguments:
       buffer-A, buffer-B, buffer-C, control-buffer
       Buffer C may not be used in jobs that compare only two buffers.
If you plan to do something fancy, take a close look at how the two
provided functions are written."
  :type '(choice (const :tag "Multi Frame" ediff-setup-windows-multiframe)
		 (const :tag "Single Frame" ediff-setup-windows-plain)
		 (function :tag "Other function"))
  :group 'ediff-window)

;; indicates if we are in a multiframe setup
(ediff-defvar-local ediff-multiframe nil "")

;; Share of the frame occupied by the merge window (buffer C)
(ediff-defvar-local ediff-merge-window-share 0.45 "")

;; The control window.
(ediff-defvar-local ediff-control-window nil "")
;; Official window for buffer A
(ediff-defvar-local ediff-window-A nil "")
;; Official window for buffer B
(ediff-defvar-local ediff-window-B nil "")
;; Official window for buffer C
(ediff-defvar-local ediff-window-C nil "")
;; Ediff's window configuration.
;; Used to minimize the need to rearrange windows.
(ediff-defvar-local ediff-window-config-saved "" "")

;; Association between buff-type and ediff-window-*
(defconst ediff-window-alist
  '((A . ediff-window-A)
    (?A . ediff-window-A)
    (B . ediff-window-B)
    (?B . ediff-window-B)
    (C . ediff-window-C)
    (?C . ediff-window-C)))


(defcustom ediff-split-window-function 'split-window-vertically
  "The function used to split the main window between buffer-A and buffer-B.
You can set it to a horizontal split instead of the default vertical split
by setting this variable to `split-window-horizontally'.
You can also have your own function to do fancy splits.
This variable has no effect when buffer-A/B are shown in different frames.
In this case, Ediff will use those frames to display these buffers."
  :type '(choice
	  (const :tag "Split vertically" split-window-vertically)
	  (const :tag "Split horizontally" split-window-horizontally)
	  function)
  :group 'ediff-window)

(defcustom ediff-merge-split-window-function 'split-window-horizontally
  "The function used to split the main window between buffer-A and buffer-B.
You can set it to a vertical split instead of the default horizontal split
by setting this variable to `split-window-vertically'.
You can also have your own function to do fancy splits.
This variable has no effect when buffer-A/B/C are shown in different frames.
In this case, Ediff will use those frames to display these buffers."
  :type '(choice
	  (const :tag "Split vertically" split-window-vertically)
	  (const :tag "Split horizontally" split-window-horizontally)
	  function)
  :group 'ediff-window)

;; Definitions hidden from the compiler by compat wrappers.
(declare-function ediff-display-pixel-width "ediff-init")
(declare-function ediff-display-pixel-height "ediff-init")

(defconst ediff-control-frame-parameters
  (list
   '(name . "Ediff")
   ;;'(unsplittable . t)
   '(minibuffer . nil)
   '(user-position . t)	      ; Emacs only
   '(vertical-scroll-bars . nil)  ; Emacs only
   '(scrollbar-width . 0)         ; XEmacs only
   '(scrollbar-height . 0)        ; XEmacs only
   '(menu-bar-lines . 0)          ; Emacs only
   '(tool-bar-lines . 0)          ; Emacs 21+ only
   '(left-fringe    . 0)
   '(right-fringe   . 0)
   ;; don't lower but auto-raise
   '(auto-lower . nil)
   '(auto-raise . t)
   '(visibility . nil)
   ;; make initial frame small to avoid distraction
   '(width . 1) '(height . 1)
   ;; this blocks queries from  window manager as to where to put
   ;; ediff's control frame. we put the frame outside the display,
   ;; so the initial frame won't jump all over the screen
   (cons 'top  (if (fboundp 'ediff-display-pixel-height)
		   (1+ (ediff-display-pixel-height))
		 3000))
   (cons 'left (if (fboundp 'ediff-display-pixel-width)
		   (1+ (ediff-display-pixel-width))
		 3000))
   )
  "Frame parameters for displaying Ediff Control Panel.
Used internally---not a user option.")

;; position of the mouse; used to decide whether to warp the mouse into ctl
;; frame
(ediff-defvar-local ediff-mouse-pixel-position nil "")

;; not used for now
(defvar ediff-mouse-pixel-threshold 30
  "If the user moves mouse more than this many pixels, Ediff won't warp mouse into control window.")

(defcustom ediff-grab-mouse t
  "If t, Ediff will always grab the mouse and put it in the control frame.
If 'maybe, Ediff will do it sometimes, but not after operations that require
relatively long time.  If nil, the mouse will be entirely user's
responsibility."
  :type 'boolean
  :group 'ediff-window)

(defcustom ediff-control-frame-position-function 'ediff-make-frame-position
  "Function to call to determine the desired location for the control panel.
Expects three parameters: the control buffer, the desired width and height
of the control frame.  It returns an association list
of the form \(\(top . <position>\) \(left . <position>\)\)"
  :type 'function
  :group 'ediff-window)

(defcustom ediff-control-frame-upward-shift 42
  "The upward shift of control frame from the top of buffer A's frame.
Measured in pixels.
This is used by the default control frame positioning function,
`ediff-make-frame-position'.  This variable is provided for easy
customization of the default control frame positioning."
  :type 'integer
  :group 'ediff-window)

(defcustom ediff-narrow-control-frame-leftward-shift (if (featurep 'xemacs) 7 3)
  "The leftward shift of control frame from the right edge of buf A's frame.
Measured in characters.
This is used by the default control frame positioning function,
`ediff-make-frame-position' to adjust the position of the control frame
when it shows the short menu.  This variable is provided for easy
customization of the default."
  :type 'integer
  :group 'ediff-window)

(defcustom ediff-wide-control-frame-rightward-shift 7
  "The rightward shift of control frame from the left edge of buf A's frame.
Measured in characters.
This is used by the default control frame positioning function,
`ediff-make-frame-position' to adjust the position of the control frame
when it shows the full menu.  This variable is provided for easy
customization of the default."
  :type 'integer
  :group 'ediff-window)


;; Wide frame display

;; t means Ediff is using wide display
(ediff-defvar-local ediff-wide-display-p nil "")
;; keeps frame config for toggling wide display
(ediff-defvar-local ediff-wide-display-orig-parameters nil
  "Frame parameters to be restored when the user wants to toggle the wide
display off.")
(ediff-defvar-local ediff-wide-display-frame nil
  "Frame to be used for wide display.")
(ediff-defvar-local ediff-make-wide-display-function 'ediff-make-wide-display
  "The value is a function that is called to create a wide display.
The function is called without arguments.  It should resize the frame in
which buffers A, B, and C are to be displayed, and it should save the old
frame parameters in `ediff-wide-display-orig-parameters'.
The variable `ediff-wide-display-frame' should be set to contain
the frame used for the wide display.")

;; Frame used for the control panel in a windowing system.
(ediff-defvar-local ediff-control-frame nil "")

(defcustom ediff-prefer-iconified-control-frame nil
  "If t, keep control panel iconified when help message is off.
This has effect only on a windowing system.
If t, hitting `?' to toggle control panel off iconifies it.

This is only useful in Emacs and only for certain kinds of window managers,
such as TWM and its derivatives, since the window manager must permit
keyboard input to go into icons.  XEmacs completely ignores keyboard input
into icons, regardless of the window manager."
  :type 'boolean
  :group 'ediff-window)

;;; Functions

(defun ediff-get-window-by-clicking (wind prev-wind wind-number)
  (let (event)
    (message
     "Select windows by clicking.  Please click on Window %d " wind-number)
    (while (not (ediff-mouse-event-p (setq event (ediff-read-event))))
      (if (sit-for 1) ; if sequence of events, wait till the final word
	  (beep 1))
      (message "Please click on Window %d " wind-number))
    (ediff-read-event) ; discard event
    (setq wind (if (featurep 'xemacs)
		   (event-window event)
		 (posn-window (event-start event))))))


;; Select the lowest window on the frame.
(defun ediff-select-lowest-window ()
  (if (featurep 'xemacs)
      (select-window (frame-lowest-window))
    (let* ((lowest-window (selected-window))
	   (bottom-edge (car (cdr (cdr (cdr (window-edges))))))
	   (last-window (save-excursion
			  (other-window -1) (selected-window)))
	   (window-search t))
      (while window-search
	(let* ((this-window (next-window))
	       (next-bottom-edge
		(car (cdr (cdr (cdr (window-edges this-window)))))))
	  (if (< bottom-edge next-bottom-edge)
	      (setq bottom-edge next-bottom-edge
		    lowest-window this-window))
	  (select-window this-window)
	  (when (eq last-window this-window)
	    (select-window lowest-window)
	    (setq window-search nil)))))))


;;; Common window setup routines

;; Set up the window configuration.  If POS is given, set the points to
;; the beginnings of the buffers.
;; When 3way comparison is added, this will have to choose the appropriate
;; setup function based on ediff-job-name
(defun ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  ;; Make sure we are not in the minibuffer window when we try to delete
  ;; all other windows.
  (run-hooks 'ediff-before-setup-windows-hook)
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1))

  ;; in case user did a no-no on a tty
  (or (ediff-window-display-p)
      (setq ediff-window-setup-function 'ediff-setup-windows-plain))

  (or (ediff-keep-window-config control-buffer)
      (funcall
       (ediff-with-current-buffer control-buffer ediff-window-setup-function)
       buffer-A buffer-B buffer-C control-buffer))
  (run-hooks 'ediff-after-setup-windows-hook))

;; Just set up 3 windows.
;; Usually used without windowing systems
;; With windowing, we want to use dedicated frames.
(defun ediff-setup-windows-plain (buffer-A buffer-B buffer-C control-buffer)
  (ediff-with-current-buffer control-buffer
    (setq ediff-multiframe nil))
  (if ediff-merge-job
      (ediff-setup-windows-plain-merge
       buffer-A buffer-B buffer-C control-buffer)
    (ediff-setup-windows-plain-compare
     buffer-A buffer-B buffer-C control-buffer)))

(defun ediff-setup-windows-plain-merge (buf-A buf-B buf-C control-buffer)
  ;; skip dedicated and unsplittable frames
  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 1)
	split-window-function
	merge-window-share merge-window-lines
	wind-A wind-B wind-C)
    (ediff-with-current-buffer control-buffer
      (setq merge-window-share ediff-merge-window-share
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)

    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1)
    (setq merge-window-lines
	  (max 2 (round (* (window-height) merge-window-share))))
    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))

    ;; XEmacs used to have a lot of trouble with display
    ;; It did't set things right unless we tell it to sit still
    ;; 19.12 seems ok.
    ;;(if (featurep 'xemacs) (sit-for 0))

    (split-window-vertically (max 2 (- (window-height) merge-window-lines)))
    (if (eq (selected-window) wind-A)
	(other-window 1))
    (setq wind-C (selected-window))
    (switch-to-buffer buf-C)

    (select-window wind-A)
    (funcall split-window-function)

    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))

    (ediff-with-current-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C))

    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))


;; This function handles all comparison jobs, including 3way jobs
(defun ediff-setup-windows-plain-compare (buf-A buf-B buf-C control-buffer)
  ;; skip dedicated and unsplittable frames
  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 1)
	split-window-function wind-width-or-height
	three-way-comparison
	wind-A-start wind-B-start wind-A wind-B wind-C)
    (ediff-with-current-buffer control-buffer
      (setq wind-A-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'A ediff-narrow-bounds))
	    wind-B-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'B  ediff-narrow-bounds))
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function
	    three-way-comparison ediff-3way-comparison-job))
    ;; if in minibuffer go somewhere else
    (if (save-match-data
	  (string-match "\*Minibuf-" (buffer-name (window-buffer))))
	(select-window (next-window nil 'ignore-minibuf)))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)

    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1)
    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))
    (if three-way-comparison
	(setq wind-width-or-height
	      (/ (if (eq split-window-function 'split-window-vertically)
		     (window-height wind-A)
		   (window-width wind-A))
		 3)))

    ;; XEmacs used to have a lot of trouble with display
    ;; It did't set things right unless we told it to sit still
    ;; 19.12 seems ok.
    ;;(if (featurep 'xemacs) (sit-for 0))

    (funcall split-window-function wind-width-or-height)

    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))

    (if three-way-comparison
	(progn
	  (funcall split-window-function) ; equally
	  (if (eq (selected-window) wind-B)
	      (other-window 1))
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))))

    (ediff-with-current-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C))

    ;; It is unlikely that we will want to implement 3way window comparison.
    ;; So, only buffers A and B are used here.
    (if ediff-windows-job
	(progn
	  (set-window-start wind-A wind-A-start)
	  (set-window-start wind-B wind-B-start)))

    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))


;; dispatch an appropriate window setup function
(defun ediff-setup-windows-multiframe (buf-A buf-B buf-C control-buf)
  (ediff-with-current-buffer control-buf
    (setq ediff-multiframe t))
  (if ediff-merge-job
      (ediff-setup-windows-multiframe-merge buf-A buf-B buf-C control-buf)
    (ediff-setup-windows-multiframe-compare buf-A buf-B buf-C control-buf)))

(defun ediff-setup-windows-multiframe-merge (buf-A buf-B buf-C control-buf)
;;; Algorithm:
;;;   1. Never use frames that have dedicated windows in them---it is bad to
;;;      destroy dedicated windows.
;;;   2. If A and B are in the same frame but C's frame is different--- use one
;;;      frame for A and B and use a separate frame for C.
;;;   3. If C's frame is non-existent, then: if the first suitable
;;;      non-dedicated frame  is different from A&B's, then use it for C.
;;;      Otherwise, put A,B, and C in one frame.
;;;   4. If buffers A, B, C are is separate frames, use them to display these
;;;      buffers.

  ;;   Skip dedicated or iconified frames.
  ;;   Unsplittable frames are taken care of later.
  (ediff-skip-unsuitable-frames 'ok-unsplittable)

  (let* ((window-min-height 1)
	 (wind-A (ediff-get-visible-buffer-window buf-A))
	 (wind-B (ediff-get-visible-buffer-window buf-B))
	 (wind-C (ediff-get-visible-buffer-window buf-C))
	 (frame-A (if wind-A (window-frame wind-A)))
	 (frame-B (if wind-B (window-frame wind-B)))
	 (frame-C (if wind-C (window-frame wind-C)))
	 ;; on wide display, do things in one frame
	 (force-one-frame
	  (ediff-with-current-buffer control-buf ediff-wide-display-p))
	 ;; this lets us have local versions of ediff-split-window-function
	 (split-window-function
	  (ediff-with-current-buffer control-buf ediff-split-window-function))
	 (orig-wind (selected-window))
	 (orig-frame (selected-frame))
	 (use-same-frame (or force-one-frame
			     ;; A and C must be in one frame
			     (eq frame-A (or frame-C orig-frame))
			     ;; B and C must be in one frame
			     (eq frame-B (or frame-C orig-frame))
			     ;; A or B is not visible
			     (not (frame-live-p frame-A))
			     (not (frame-live-p frame-B))
			     ;; A or B is not suitable for display
			     (not (ediff-window-ok-for-display wind-A))
			     (not (ediff-window-ok-for-display wind-B))
			     ;; A and B in the same frame, and no good frame
			     ;; for C
			     (and (eq frame-A frame-B)
				  (not (frame-live-p frame-C)))
			     ))
	 ;; use-same-frame-for-AB implies wind A and B are ok for display
	 (use-same-frame-for-AB (and (not use-same-frame)
				     (eq frame-A frame-B)))
	 (merge-window-share (ediff-with-current-buffer control-buf
			       ediff-merge-window-share))
	 merge-window-lines
	 designated-minibuffer-frame
	 done-A done-B done-C)

    ;; buf-A on its own
    (if (and (window-live-p wind-A)
	     (null use-same-frame) ; implies wind-A is suitable
	     (null use-same-frame-for-AB))
	(progn ; bug A on its own
	  ;; buffer buf-A is seen in live wind-A
	  (select-window wind-A)
	  (delete-other-windows)
	  (setq wind-A (selected-window))
	  (setq done-A t)))

    ;; buf-B on its own
    (if (and (window-live-p wind-B)
	     (null use-same-frame) ; implies wind-B is suitable
	     (null use-same-frame-for-AB))
	(progn ; buf B on its own
	  ;; buffer buf-B is seen in live wind-B
	  (select-window wind-B)
	  (delete-other-windows)
	  (setq wind-B (selected-window))
	  (setq done-B t)))

    ;; buf-C on its own
    (if (and (window-live-p wind-C)
	     (ediff-window-ok-for-display wind-C)
	     (null use-same-frame)) ; buf C on its own
	(progn
	  ;; buffer buf-C is seen in live wind-C
	  (select-window wind-C)
	  (delete-other-windows)
	  (setq wind-C (selected-window))
	  (setq done-C t)))

    (if (and use-same-frame-for-AB  ; implies wind A and B are suitable
	     (window-live-p wind-A))
	(progn
	  ;; wind-A must already be displaying buf-A
	  (select-window wind-A)
	  (delete-other-windows)
	  (setq wind-A (selected-window))

	  (funcall split-window-function)
	  (if (eq (selected-window) wind-A)
	      (other-window 1))
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))

	  (setq done-A t
		done-B t)))

    (if use-same-frame
	(let ((window-min-height 1))
	  (if (and (eq frame-A frame-B)
		   (eq frame-B frame-C)
		   (frame-live-p frame-A))
	      (select-frame frame-A)
	    ;; avoid dedicated and non-splittable windows
	    (ediff-skip-unsuitable-frames))
	  (delete-other-windows)
	  (setq merge-window-lines
		(max 2 (round (* (window-height) merge-window-share))))
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))

	  (split-window-vertically
	   (max 2 (- (window-height) merge-window-lines)))
	  (if (eq (selected-window) wind-A)
	      (other-window 1))
	  (setq wind-C (selected-window))
	  (switch-to-buffer buf-C)

	  (select-window wind-A)

	  (funcall split-window-function)
	  (if (eq (selected-window) wind-A)
	      (other-window 1))
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))

	  (setq done-A t
		done-B t
		done-C t)
	  ))

    (or done-A  ; Buf A to be set in its own frame,
	      ;;; or it was set before because use-same-frame = 1
	(progn
	  ;; Buf-A was not set up yet as it wasn't visible,
	  ;; and use-same-frame = nil, use-same-frame-for-AB = nil
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  ))
    (or done-B  ; Buf B to be set in its own frame,
	      ;;; or it was set before because use-same-frame = 1
	(progn
	  ;; Buf-B was not set up yet as it wasn't visible
	  ;; and use-same-frame = nil, use-same-frame-for-AB = nil
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  ))

    (or done-C  ; Buf C to be set in its own frame,
	      ;;; or it was set before because use-same-frame = 1
	(progn
	  ;; Buf-C was not set up yet as it wasn't visible
	  ;; and use-same-frame = nil
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))
	  ))

    (ediff-with-current-buffer control-buf
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C)
      (setq frame-A (window-frame ediff-window-A)
	    designated-minibuffer-frame
	    (window-frame (minibuffer-window frame-A))))

    (ediff-setup-control-frame control-buf designated-minibuffer-frame)
    ))


;; Window setup for all comparison jobs, including 3way comparisons
(defun ediff-setup-windows-multiframe-compare (buf-A buf-B buf-C control-buf)
;;; Algorithm:
;;;    If a buffer is seen in a frame, use that frame for that buffer.
;;;    If it is not seen, use the current frame.
;;;    If both buffers are not seen, they share the current frame.  If one
;;;    of the buffers is not seen, it is placed in the current frame (where
;;;    ediff started).  If that frame is displaying the other buffer, it is
;;;    shared between the two buffers.
;;;    However, if we decide to put both buffers in one frame
;;;    and the selected frame isn't splittable, we create a new frame and
;;;    put both buffers there, event if one of this buffers is visible in
;;;    another frame.

  ;; Skip dedicated or iconified frames.
  ;; Unsplittable frames are taken care of later.
  (ediff-skip-unsuitable-frames 'ok-unsplittable)

  (let* ((window-min-height 1)
	 (wind-A (ediff-get-visible-buffer-window buf-A))
	 (wind-B (ediff-get-visible-buffer-window buf-B))
	 (wind-C (ediff-get-visible-buffer-window buf-C))
	 (frame-A (if wind-A (window-frame wind-A)))
	 (frame-B (if wind-B (window-frame wind-B)))
	 (frame-C (if wind-C (window-frame wind-C)))
	 (ctl-frame-exists-p (ediff-with-current-buffer control-buf
			       (frame-live-p ediff-control-frame)))
	 ;; on wide display, do things in one frame
	 (force-one-frame
	  (ediff-with-current-buffer control-buf ediff-wide-display-p))
	 ;; this lets us have local versions of ediff-split-window-function
	 (split-window-function
	  (ediff-with-current-buffer control-buf ediff-split-window-function))
	 (three-way-comparison
	  (ediff-with-current-buffer control-buf ediff-3way-comparison-job))
	 (orig-wind (selected-window))
	 (use-same-frame (or force-one-frame
			     (eq frame-A frame-B)
			     (not (ediff-window-ok-for-display wind-A))
			     (not (ediff-window-ok-for-display wind-B))
			     (if three-way-comparison
				 (or (eq frame-A frame-C)
				     (eq frame-B frame-C)
				     (not (ediff-window-ok-for-display wind-C))
				     (not (frame-live-p frame-A))
				     (not (frame-live-p frame-B))
				     (not (frame-live-p frame-C))))
			     (and (not (frame-live-p frame-B))
				  (or ctl-frame-exists-p
				      (eq frame-A (selected-frame))))
			     (and (not (frame-live-p frame-A))
				  (or ctl-frame-exists-p
				      (eq frame-B (selected-frame))))))
	 wind-A-start wind-B-start
	 designated-minibuffer-frame
	 done-A done-B done-C)

    (ediff-with-current-buffer control-buf
      (setq wind-A-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'A ediff-narrow-bounds))
	    wind-B-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'B ediff-narrow-bounds))))

    (if (and (window-live-p wind-A) (null use-same-frame)) ; buf-A on its own
	(progn
	  ;; buffer buf-A is seen in live wind-A
	  (select-window wind-A) ; must be displaying buf-A
	  (delete-other-windows)
	  (setq wind-A (selected-window))
	  (setq done-A t)))

    (if (and (window-live-p wind-B) (null use-same-frame)) ; buf B on its own
	(progn
	  ;; buffer buf-B is seen in live wind-B
	  (select-window wind-B) ; must be displaying buf-B
	  (delete-other-windows)
	  (setq wind-B (selected-window))
	  (setq done-B t)))

    (if (and (window-live-p wind-C) (null use-same-frame)) ; buf C on its own
	(progn
	  ;; buffer buf-C is seen in live wind-C
	  (select-window wind-C) ; must be displaying buf-C
	  (delete-other-windows)
	  (setq wind-C (selected-window))
	  (setq done-C t)))

    (if use-same-frame
	(let (wind-width-or-height) ; this affects 3way setups only
	  (if (and (eq frame-A frame-B) (frame-live-p frame-A))
	      (select-frame frame-A)
	    ;; avoid dedicated and non-splittable windows
	    (ediff-skip-unsuitable-frames))
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))

	  (if three-way-comparison
	      (setq wind-width-or-height
		    (/
		     (if (eq split-window-function 'split-window-vertically)
			 (window-height wind-A)
		       (window-width wind-A))
		     3)))

	  (funcall split-window-function wind-width-or-height)
	  (if (eq (selected-window) wind-A)
	      (other-window 1))
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))

	  (if three-way-comparison
	      (progn
		(funcall split-window-function) ; equally
		(if (memq (selected-window) (list wind-A wind-B))
		    (other-window 1))
		(switch-to-buffer buf-C)
		(setq wind-C (selected-window))))
	  (setq done-A t
		done-B t
		done-C t)
	  ))

    (or done-A  ; Buf A to be set in its own frame
	      ;;; or it was set before because use-same-frame = 1
	(progn
	  ;; Buf-A was not set up yet as it wasn't visible,
	  ;; and use-same-frame = nil
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  ))
    (or done-B  ; Buf B to be set in its own frame
	      ;;; or it was set before because use-same-frame = 1
	(progn
	  ;; Buf-B was not set up yet as it wasn't visible,
	  ;; and use-same-frame = nil
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  ))

    (if three-way-comparison
	(or done-C  ; Buf C to be set in its own frame
		  ;;; or it was set before because use-same-frame = 1
	    (progn
	      ;; Buf-C was not set up yet as it wasn't visible,
	      ;; and use-same-frame = nil
	      (select-window orig-wind)
	      (delete-other-windows)
	      (switch-to-buffer buf-C)
	      (setq wind-C (selected-window))
	      )))

    (ediff-with-current-buffer control-buf
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C)

      (setq frame-A (window-frame ediff-window-A)
	    designated-minibuffer-frame
	    (window-frame (minibuffer-window frame-A))))

    ;; It is unlikely that we'll implement a version of ediff-windows that
    ;; would compare 3 windows at once.  So, we don't use buffer C here.
    (if ediff-windows-job
	(progn
	  (set-window-start wind-A wind-A-start)
	  (set-window-start wind-B wind-B-start)))

    (ediff-setup-control-frame control-buf designated-minibuffer-frame)
    ))

;; skip unsplittable frames and frames that have dedicated windows.
;; create a new splittable frame if none is found
(defun ediff-skip-unsuitable-frames (&optional ok-unsplittable)
  (if (ediff-window-display-p)
      (let ((wind-frame (window-frame (selected-window)))
	     seen-windows)
	(while (and (not (memq (selected-window) seen-windows))
		    (or
		     (ediff-frame-has-dedicated-windows wind-frame)
		     (ediff-frame-iconified-p wind-frame)
		     ;; skip small windows
		     (< (frame-height wind-frame)
			(* 3 window-min-height))
		     (if ok-unsplittable
			 nil
		       (ediff-frame-unsplittable-p wind-frame))))
	  ;; remember history
	  (setq seen-windows (cons (selected-window) seen-windows))
	  ;; try new window
	  (other-window 1 t)
	  (setq wind-frame (window-frame (selected-window)))
	  )
	(if (memq (selected-window) seen-windows)
	    ;; fed up, no appropriate frames
	    (setq wind-frame (make-frame '((unsplittable)))))

	(select-frame wind-frame)
	)))

(defun ediff-frame-has-dedicated-windows (frame)
  (let (ans)
    (walk-windows
     (lambda (wind) (if (window-dedicated-p wind)
			(setq ans t)))
     'ignore-minibuffer
     frame)
    ans))

;; window is ok, if it is only one window on the frame, not counting the
;; minibuffer, or none of the frame's windows is dedicated.
;; The idea is that it is bad to destroy dedicated windows while creating an
;; ediff window setup
(defun ediff-window-ok-for-display (wind)
  (and
   (window-live-p wind)
   (or
    ;; only one window
    (eq wind (next-window wind 'ignore-minibuffer (window-frame wind)))
    ;; none is dedicated (in multiframe setup)
    (not (ediff-frame-has-dedicated-windows (window-frame wind)))
    )))

;; Prepare or refresh control frame
(defun ediff-setup-control-frame (ctl-buffer designated-minibuffer-frame)
  (let ((window-min-height 1)
	ctl-frame-iconified-p dont-iconify-ctl-frame deiconify-ctl-frame
	ctl-frame old-ctl-frame lines
	;; user-grabbed-mouse
	fheight fwidth adjusted-parameters)

    (ediff-with-current-buffer ctl-buffer
      (if (and (featurep 'xemacs) (featurep 'menubar))
	  (set-buffer-menubar nil))
      ;;(setq user-grabbed-mouse (ediff-user-grabbed-mouse))
      (run-hooks 'ediff-before-setup-control-frame-hook))

    (setq old-ctl-frame (ediff-with-current-buffer ctl-buffer ediff-control-frame))
    (ediff-with-current-buffer ctl-buffer
      (setq ctl-frame (if (frame-live-p old-ctl-frame)
			  old-ctl-frame
			(make-frame ediff-control-frame-parameters))
	    ediff-control-frame ctl-frame)
      ;; protect against undefined face-attribute
      (condition-case nil
	  (if (and (featurep 'emacs) (face-attribute 'mode-line :box))
	      (set-face-attribute 'mode-line ctl-frame :box nil))
	(error)))

    (setq ctl-frame-iconified-p (ediff-frame-iconified-p ctl-frame))
    (select-frame ctl-frame)
    (if (window-dedicated-p (selected-window))
	()
      (delete-other-windows)
      (switch-to-buffer ctl-buffer))

    ;; must be before ediff-setup-control-buffer
    ;; just a precaution--we should be in ctl-buffer already
    (ediff-with-current-buffer ctl-buffer
      (make-local-variable 'frame-title-format)
      (make-local-variable 'frame-icon-title-format)	; XEmacs
      (make-local-variable 'icon-title-format))  	; Emacs

    (ediff-setup-control-buffer ctl-buffer)
    (setq dont-iconify-ctl-frame
	  (not (string= ediff-help-message ediff-brief-help-message)))
    (setq deiconify-ctl-frame
	  (and (eq this-command 'ediff-toggle-help)
	       dont-iconify-ctl-frame))

    ;; 1 more line for the modeline
    (setq lines (1+ (count-lines (point-min) (point-max)))
	  fheight lines
	  fwidth (max (+ (ediff-help-message-line-length) 2)
		      (ediff-compute-toolbar-width))
	  adjusted-parameters
	  (list
	   ;; possibly change surrogate minibuffer
	   (cons 'minibuffer
		 (minibuffer-window
		  designated-minibuffer-frame))
	   (cons 'width fwidth)
	   (cons 'height fheight)
	   (cons 'user-position t)
	   ))

    ;; adjust autoraise
    (setq adjusted-parameters
	  (cons (if ediff-use-long-help-message
		    '(auto-raise . nil)
		  '(auto-raise . t))
		adjusted-parameters))

    ;; In XEmacs, buffer menubar needs to be killed before frame parameters
    ;; are changed.
    (if (ediff-has-toolbar-support-p)
	(when (featurep 'xemacs)
	  (if (ediff-has-gutter-support-p)
	      (set-specifier top-gutter (list ctl-frame nil)))
	  (sit-for 0)
	  (set-specifier top-toolbar-height (list ctl-frame 0))
	  ;;(set-specifier bottom-toolbar-height (list ctl-frame 0))
	  (set-specifier left-toolbar-width (list ctl-frame 0))
	  (set-specifier right-toolbar-width (list ctl-frame 0))))

    ;; As a precaution, we call modify frame parameters twice, in
    ;; order to make sure that at least once we do it for
    ;; a non-iconified frame.  (It appears that in the Windows port of
    ;; Emacs, one can't modify frame parameters of iconified frames.)
    (if (eq system-type 'windows-nt)
	(modify-frame-parameters ctl-frame adjusted-parameters))

    ;; make or zap toolbar (if not requested)
    (ediff-make-bottom-toolbar ctl-frame)

    (goto-char (point-min))

    (modify-frame-parameters ctl-frame adjusted-parameters)
    (make-frame-visible ctl-frame)

    ;; This works around a bug in 19.25 and earlier.  There, if frame gets
    ;; iconified, the current buffer changes to that of the frame that
    ;; becomes exposed as a result of this iconification.
    ;; So, we make sure the current buffer doesn't change.
    (select-frame ctl-frame)
    (ediff-refresh-control-frame)

    (cond ((and ediff-prefer-iconified-control-frame
		(not ctl-frame-iconified-p) (not dont-iconify-ctl-frame))
	   (iconify-frame ctl-frame))
	  ((or deiconify-ctl-frame (not ctl-frame-iconified-p))
	   (raise-frame ctl-frame)))

    (set-window-dedicated-p (selected-window) t)

    ;; Now move the frame.  We must do it separately due to an obscure bug in
    ;; XEmacs
    (modify-frame-parameters
     ctl-frame
     (funcall ediff-control-frame-position-function ctl-buffer fwidth fheight))

    ;; synchronize so the cursor will move to control frame
    ;; per RMS suggestion
    (if (ediff-window-display-p)
	(let ((count 7))
	  (sit-for .1)
	  (while (and (not (frame-visible-p ctl-frame)) (> count 0))
	    (setq count (1- count))
	    (sit-for .3))))

    (or (ediff-frame-iconified-p ctl-frame)
	;; don't warp the mouse, unless ediff-grab-mouse = t
	(ediff-reset-mouse ctl-frame
			   (or (eq this-command 'ediff-quit)
			       (not (eq ediff-grab-mouse t)))))

    (when (featurep 'xemacs)
      (ediff-with-current-buffer ctl-buffer
	(make-local-hook 'select-frame-hook)
	(add-hook 'select-frame-hook
		  'ediff-xemacs-select-frame-hook nil 'local)))

    (ediff-with-current-buffer ctl-buffer
      (run-hooks 'ediff-after-setup-control-frame-hook))))


(defun ediff-destroy-control-frame (ctl-buffer)
  (ediff-with-current-buffer ctl-buffer
    (if (and (ediff-window-display-p) (frame-live-p ediff-control-frame))
	(let ((ctl-frame ediff-control-frame))
	  (if (and (featurep 'xemacs) (featurep 'menubar))
	      (set-buffer-menubar default-menubar))
	  (setq ediff-control-frame nil)
	  (delete-frame ctl-frame))))
  (if ediff-multiframe
      (ediff-skip-unsuitable-frames))
  ;;(ediff-reset-mouse nil)
  )


;; finds a good place to clip control frame
(defun ediff-make-frame-position (ctl-buffer ctl-frame-width ctl-frame-height)
  (ediff-with-current-buffer ctl-buffer
    (let* ((frame-A (window-frame ediff-window-A))
	   (frame-A-parameters (frame-parameters frame-A))
	   (frame-A-top (eval (cdr (assoc 'top frame-A-parameters))))
	   (frame-A-left (eval (cdr (assoc 'left frame-A-parameters))))
	   (frame-A-width (frame-width frame-A))
	   (ctl-frame ediff-control-frame)
	   horizontal-adjustment upward-adjustment
	   ctl-frame-top ctl-frame-left)

      ;; Multiple control frames are clipped based on the value of
      ;; ediff-control-buffer-number.  This is done in order not to obscure
      ;; other active control panels.
      (setq horizontal-adjustment (* 2 ediff-control-buffer-number)
	    upward-adjustment (* -14 ediff-control-buffer-number))

      (setq ctl-frame-top
	    (- frame-A-top upward-adjustment ediff-control-frame-upward-shift)
	    ctl-frame-left
	    (+ frame-A-left
	       (if ediff-use-long-help-message
		   (* (ediff-frame-char-width ctl-frame)
		      (+ ediff-wide-control-frame-rightward-shift
			 horizontal-adjustment))
		 (- (* frame-A-width (ediff-frame-char-width frame-A))
		    (* (ediff-frame-char-width ctl-frame)
		       (+ ctl-frame-width
			  ediff-narrow-control-frame-leftward-shift
			  horizontal-adjustment))))))
      (setq ctl-frame-top
	    (min ctl-frame-top
		 (- (ediff-display-pixel-height)
		    (* 2 ctl-frame-height
		       (ediff-frame-char-height ctl-frame))))
	    ctl-frame-left
	    (min ctl-frame-left
		 (- (ediff-display-pixel-width)
		    (* ctl-frame-width (ediff-frame-char-width ctl-frame)))))
      ;; keep ctl frame within the visible bounds
      (setq ctl-frame-top (max ctl-frame-top 1)
	    ctl-frame-left (max ctl-frame-left 1))

      (list (cons 'top ctl-frame-top)
	    (cons 'left ctl-frame-left))
      )))

(defun ediff-xemacs-select-frame-hook ()
  (if (and (equal (selected-frame) ediff-control-frame)
	   (not ediff-use-long-help-message))
      (raise-frame ediff-control-frame)))

(defun ediff-make-wide-display ()
  "Construct an alist of parameters for the wide display.
Saves the old frame parameters in `ediff-wide-display-orig-parameters'.
The frame to be resized is kept in `ediff-wide-display-frame'.
This function modifies only the left margin and the width of the display.
It assumes that it is called from within the control buffer."
  (if (not (fboundp 'ediff-display-pixel-width))
      (error "Can't determine display width"))
  (let* ((frame-A (window-frame ediff-window-A))
	 (frame-A-params (frame-parameters frame-A))
	 (cw (ediff-frame-char-width frame-A))
	 (wd (- (/ (ediff-display-pixel-width) cw) 5)))
    (setq ediff-wide-display-orig-parameters
	  (list (cons 'left (max 0 (eval (cdr (assoc 'left frame-A-params)))))
		(cons 'width (cdr (assoc 'width frame-A-params))))
	  ediff-wide-display-frame frame-A)
    (modify-frame-parameters
     frame-A `((left . ,cw) (width . ,wd) (user-position . t)))))


;; Revise the mode line to display which difference we have selected
;; Also resets modelines of buffers A/B, since they may be clobbered by
;; other invocations of Ediff.
(defun ediff-refresh-mode-lines ()
  (let (buf-A-state-diff buf-B-state-diff buf-C-state-diff buf-C-state-merge)

    (if (ediff-valid-difference-p)
	(setq
	 buf-C-state-diff (ediff-get-state-of-diff ediff-current-difference 'C)
	 buf-C-state-merge (ediff-get-state-of-merge ediff-current-difference)
	 buf-A-state-diff (ediff-get-state-of-diff ediff-current-difference 'A)
	 buf-B-state-diff (ediff-get-state-of-diff ediff-current-difference 'B)
	 buf-A-state-diff (if buf-A-state-diff
			      (format "[%s] " buf-A-state-diff)
			    "")
	 buf-B-state-diff (if buf-B-state-diff
			      (format "[%s] " buf-B-state-diff)
			    "")
	 buf-C-state-diff (if (and (ediff-buffer-live-p ediff-buffer-C)
				   (or buf-C-state-diff buf-C-state-merge))
			      (format "[%s%s%s] "
				      (or buf-C-state-diff "")
				      (if buf-C-state-merge
					  (concat " " buf-C-state-merge)
					"")
				      (if (ediff-get-state-of-ancestor
					   ediff-current-difference)
					  " AncestorEmpty"
					"")
				      )
			    ""))
      (setq buf-A-state-diff ""
	    buf-B-state-diff ""
	    buf-C-state-diff ""))

    ;; control buffer format
    (setq mode-line-format
	  (if (ediff-narrow-control-frame-p)
	      (list "   " mode-line-buffer-identification)
	    (list "-- " mode-line-buffer-identification "        Quick Help")))
    ;; control buffer id
    (setq mode-line-buffer-identification
	  (if (ediff-narrow-control-frame-p)
	      (ediff-make-narrow-control-buffer-id 'skip-name)
	    (ediff-make-wide-control-buffer-id)))
    ;; Force mode-line redisplay
    (force-mode-line-update)

    (if (and (ediff-window-display-p) (frame-live-p ediff-control-frame))
	(ediff-refresh-control-frame))

    (ediff-with-current-buffer ediff-buffer-A
      (setq ediff-diff-status buf-A-state-diff)
      (ediff-strip-mode-line-format)
      (setq mode-line-format
	    (list " A: " 'ediff-diff-status mode-line-format))
      (force-mode-line-update))
    (ediff-with-current-buffer ediff-buffer-B
      (setq ediff-diff-status buf-B-state-diff)
      (ediff-strip-mode-line-format)
      (setq mode-line-format
	    (list " B: " 'ediff-diff-status mode-line-format))
      (force-mode-line-update))
    (if ediff-3way-job
	(ediff-with-current-buffer ediff-buffer-C
	  (setq ediff-diff-status buf-C-state-diff)
	  (ediff-strip-mode-line-format)
	  (setq mode-line-format
		(list " C: " 'ediff-diff-status mode-line-format))
	  (force-mode-line-update)))
    (if (ediff-buffer-live-p ediff-ancestor-buffer)
	(ediff-with-current-buffer ediff-ancestor-buffer
	  (ediff-strip-mode-line-format)
	  ;; we keep the second dummy string in the mode line format of the
	  ;; ancestor, since for other buffers Ediff prepends 2 strings and
	  ;; ediff-strip-mode-line-format expects that.
	  (setq mode-line-format
		(list " Ancestor: "
		      (cond ((not (stringp buf-C-state-merge))
			     "")
			    ((string-match "prefer-A" buf-C-state-merge)
			     "[=diff(B)] ")
			    ((string-match "prefer-B" buf-C-state-merge)
			     "[=diff(A)] ")
			    (t ""))
		      mode-line-format))))
    ))


(defun ediff-refresh-control-frame ()
  (if (featurep 'emacs)
      ;; set frame/icon titles for Emacs
      (modify-frame-parameters
       ediff-control-frame
       (list (cons 'title (ediff-make-base-title))
	     (cons 'icon-name (ediff-make-narrow-control-buffer-id))
	     ))
    ;; set frame/icon titles for XEmacs
    (setq frame-title-format (ediff-make-base-title)
	  frame-icon-title-format (ediff-make-narrow-control-buffer-id))
    ;; force an update of the frame title
    (modify-frame-parameters ediff-control-frame '(()))))


(defun ediff-make-narrow-control-buffer-id (&optional skip-name)
  (concat
   (if skip-name
       " "
     (ediff-make-base-title))
   (cond ((< ediff-current-difference 0)
	  (format " _/%d" ediff-number-of-differences))
	 ((>= ediff-current-difference ediff-number-of-differences)
	  (format " $/%d" ediff-number-of-differences))
	 (t
	  (format " %d/%d"
		  (1+ ediff-current-difference)
		  ediff-number-of-differences)))))

(defun ediff-make-base-title ()
  (concat
   (cdr (assoc 'name ediff-control-frame-parameters))
   ediff-control-buffer-suffix))

(defun ediff-make-wide-control-buffer-id ()
  (cond ((< ediff-current-difference 0)
	 (list (format "%%b   At start of %d diffs"
		       ediff-number-of-differences)))
	((>= ediff-current-difference ediff-number-of-differences)
	 (list (format "%%b   At end of %d diffs"
		       ediff-number-of-differences)))
	(t
	 (list (format "%%b   diff %d of %d"
		       (1+ ediff-current-difference)
		       ediff-number-of-differences)))))



;; If buff is not live, return nil
(defun ediff-get-visible-buffer-window (buff)
  (if (ediff-buffer-live-p buff)
      (if (featurep 'xemacs)
	  (get-buffer-window buff t)
	(get-buffer-window buff 'visible))))


;;; Functions to decide when to redraw windows

(defun ediff-keep-window-config (control-buf)
  (and (eq control-buf (current-buffer))
       (/= (buffer-size) 0)
       (ediff-with-current-buffer control-buf
	 (let ((ctl-wind ediff-control-window)
	       (A-wind ediff-window-A)
	       (B-wind ediff-window-B)
	       (C-wind ediff-window-C))

	   (and
	    (ediff-window-visible-p A-wind)
	    (ediff-window-visible-p B-wind)
	    ;; if buffer C is defined then take it into account
	    (or (not ediff-3way-job)
		(ediff-window-visible-p C-wind))
	    (eq (window-buffer A-wind) ediff-buffer-A)
	    (eq (window-buffer B-wind) ediff-buffer-B)
	    (or (not ediff-3way-job)
		(eq (window-buffer C-wind) ediff-buffer-C))
	    (string= ediff-window-config-saved
		     (format "%S%S%S%S%S%S%S"
			     ctl-wind A-wind B-wind C-wind
			     ediff-split-window-function
			     (ediff-multiframe-setup-p)
			     ediff-wide-display-p)))))))


(provide 'ediff-wind)


;; Local Variables:
;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;; End:

;;; ediff-wind.el ends here
