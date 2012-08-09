;;; avoid.el --- make mouse pointer stay out of the way of editing

;; Copyright (C) 1993-1994, 2000-2012 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Keywords: mouse

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

;; For those who are annoyed by the mouse pointer obscuring text,
;; this mode moves the mouse pointer - either just a little out of
;; the way, or all the way to the corner of the frame.
;; To use, load or evaluate this file and type M-x mouse-avoidance-mode .
;; To set up permanently, put the following in your .emacs:
;;
;; (if (display-mouse-p) (mouse-avoidance-mode 'animate))
;;
;; Other legitimate alternatives include
;; `banish', `exile', `jump', `cat-and-mouse', and `proteus'.
;; They do somewhat different things.
;; See the documentation for function `mouse-avoidance-mode' for
;; details of the different modes.
;;
;; For added silliness, make the animatee animate...
;; put something similar to the following into your .emacs:
;;
;; (if (eq window-system 'x)
;;     (mouse-avoidance-set-pointer-shape
;;	     (eval (nth (random 4)
;;			'(x-pointer-man x-pointer-spider
;;			  x-pointer-gobbler x-pointer-gumby)))))
;;
;; For completely random pointer shape, replace the setq above with:
;; (setq x-pointer-shape (mouse-avoidance-random-shape))
;;
;; Bugs / Warnings / To-Do:
;;
;; - Using this code does slow Emacs down.  "banish" mode shouldn't
;;   be too bad, and on my workstation even "animate" is reasonable.
;;
;; - It ought to find out where any overlapping frames are and avoid them,
;;   rather than always raising the frame.

;; Credits:
;; This code was helped by all those who contributed suggestions,
;;   fixes, and additions
;; Joe Harrington (and his advisor), for the original inspiration.
;; Ken Manheimer, for dreaming up the Protean mode.
;; Richard Stallman, for the awful cat-and-mouse pun, among other things.
;; Mike Williams, Denis Howe, Bill Benedetto, Chris Moore, Don Morris,
;; Simon Marshall, and M.S. Ashton, for their feedback.

;;; Code:

(provide 'avoid)

(defgroup avoid nil
  "Make mouse pointer stay out of the way of editing."
  :prefix "mouse-avoidance-"
  :group 'mouse)

;;;###autoload
(defcustom mouse-avoidance-mode nil
  "Activate Mouse Avoidance mode.
See function `mouse-avoidance-mode' for possible values.
Setting this variable directly does not take effect;
use either \\[customize] or the function `mouse-avoidance-mode'."
  :set (lambda (symbol value)
	 ;; 'none below prevents toggling when value is nil.
	 (mouse-avoidance-mode (or value 'none)))
  :initialize 'custom-initialize-default
  :type '(choice (const :tag "none" nil) (const banish) (const jump)
		 (const animate) (const exile) (const proteus))
  :group 'avoid
  :require 'avoid
  :version "20.3")


(defcustom mouse-avoidance-nudge-dist 15
  "Average distance that mouse will be moved when approached by cursor.
Only applies in Mouse Avoidance mode `jump' and its derivatives.
For best results make this larger than `mouse-avoidance-threshold'."
  :type 'integer
  :group 'avoid)

(defcustom mouse-avoidance-nudge-var 10
  "Variability of `mouse-avoidance-nudge-dist' (which see)."
  :type 'integer
  :group 'avoid)

(defcustom mouse-avoidance-animation-delay .01
  "Delay between animation steps, in seconds."
  :type 'number
  :group 'avoid)

(defcustom mouse-avoidance-threshold 5
  "Mouse-pointer's flight distance.
If the cursor gets closer than this, the mouse pointer will move away.
Only applies in Mouse Avoidance modes `animate' and `jump'."
  :type 'integer
  :group 'avoid)

;; Internal variables
(defvar mouse-avoidance-state nil)
(defvar mouse-avoidance-pointer-shapes nil)
(defvar mouse-avoidance-n-pointer-shapes 0)
(defvar mouse-avoidance-old-pointer-shape nil)
(defvar mouse-avoidance-animating-pointer nil)

;; This timer is used to run something when Emacs is idle.
(defvar mouse-avoidance-timer nil)

;;; Functions:

(defsubst mouse-avoidance-set-pointer-shape (shape)
  "Set the shape of the mouse pointer to SHAPE."
  (when (boundp 'x-pointer-shape)
    (setq x-pointer-shape shape)
    (set-mouse-color nil)))

(defun mouse-avoidance-point-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to `mouse-position'."
  (let ((edges (window-inside-edges))
	(x-y (posn-x-y (posn-at-point))))
    (cons (selected-frame)
	  (cons (+ (car edges)
		   (/ (car x-y) (frame-char-width)))
		(+ (car (cdr edges))
		   (/ (cdr x-y) (frame-char-height)))))))

;(defun mouse-avoidance-point-position-test ()
;  (interactive)
;  (message (format "point=%s mouse=%s"
;		   (cdr (mouse-avoidance-point-position))
;		   (cdr (mouse-position)))))

(defun mouse-avoidance-set-mouse-position (pos)
  ;; Carefully set mouse position to given position (X . Y)
  ;; Ideally, should check if X,Y is in the current frame, and if not,
  ;; leave the mouse where it was.  However, this is currently
  ;; difficult to do, so we just raise the frame to avoid frame switches.
  ;; Returns t if it moved the mouse.
  (let ((f (selected-frame)))
    (raise-frame f)
    (set-mouse-position f (car pos) (cdr pos))
    t))

(defun mouse-avoidance-too-close-p (mouse)
  "Return t if mouse pointer and point cursor are too close.
MOUSE is the current mouse position as returned by `mouse-position'.
Acceptable distance is defined by `mouse-avoidance-threshold'."
  (let* ((frame (car mouse))
	 (mouse-y (cdr (cdr mouse)))
	 (tool-bar-lines (frame-parameter nil 'tool-bar-lines)))
    (or tool-bar-lines
	(setq tool-bar-lines 0))
    (if (and mouse-y (< mouse-y tool-bar-lines))
	nil
      (let ((point (mouse-avoidance-point-position))
	    (mouse-x (car (cdr mouse))))
	(and (eq frame (car point))
	     (not (null mouse-x))
	     (< (abs (- mouse-x (car (cdr point))))
		mouse-avoidance-threshold)
	     (< (abs (- mouse-y (cdr (cdr point))))
		mouse-avoidance-threshold))))))

(defun mouse-avoidance-banish-destination ()
  "The position to which Mouse Avoidance mode `banish' moves the mouse.
You can redefine this if you want the mouse banished to a different corner."
  (let* ((pos (window-edges)))
    (cons (- (nth 2 pos) 2)
	  (nth 1 pos))))

(defun mouse-avoidance-banish-mouse ()
  ;; Put the mouse pointer in the upper-right corner of the current frame.
  (mouse-avoidance-set-mouse-position (mouse-avoidance-banish-destination)))

(defsubst mouse-avoidance-delta (cur delta dist var min max)
  ;; Decide how far to move in either dimension.
  ;; Args are the CURRENT location, the desired DELTA for
  ;; warp-conservation, the DISTANCE we like to move, the VARIABILITY
  ;; in distance allowed, and the MIN and MAX possible window positions.
  ;; Returns something as close to DELTA as possible within the constraints.
  (let ((L1 (max (- min cur) (+ (- dist) (- var))))
	(R1                  (+ (- dist)    var ))
	(L2                  (+    dist  (- var)))
	(R2 (min (- max cur) (+    dist     var))))
    (if (< R1 (- min cur)) (setq L1 nil R1 nil))
    (if (> L2 (- max cur)) (setq L2 nil R2 nil))
    (cond ((and L1 (< delta L1)) L1)
	  ((and R1 (< delta R1)) delta)
	  ((and R1 (< delta 0)) R1)
	  ((and L2 (< delta L2)) L2)
	  ((and R2 (< delta R2)) delta)
	  (R2)
	  ((or R1 L2))
	  (t 0))))

(defun mouse-avoidance-nudge-mouse ()
  ;; Push the mouse a little way away, possibly animating the move.
  ;; For these modes, state keeps track of the total offset that we've
  ;; accumulated, and tries to keep it close to zero.
  (let* ((cur (mouse-position))
	 (cur-frame (car cur))
	 (cur-pos (cdr cur))
 	 (pos (window-edges))
 	 (wleft (pop pos))
 	 (wtop (pop pos))
 	 (wright (pop pos))
 	 (wbot (pop pos))
	 (deltax (mouse-avoidance-delta
		  (car cur-pos) (- (random mouse-avoidance-nudge-var)
				   (car mouse-avoidance-state))
		  mouse-avoidance-nudge-dist mouse-avoidance-nudge-var
		  wleft (1- wright)))
	 (deltay (mouse-avoidance-delta
		  (cdr cur-pos) (- (random mouse-avoidance-nudge-var)
				   (cdr mouse-avoidance-state))
		  mouse-avoidance-nudge-dist mouse-avoidance-nudge-var
		  wtop (1- wbot))))
    (setq mouse-avoidance-state
	  (cons (+ (car mouse-avoidance-state) deltax)
		(+ (cdr mouse-avoidance-state) deltay)))
    (if (or (eq mouse-avoidance-mode 'animate)
	    (eq mouse-avoidance-mode 'proteus))
	(let ((i 0.0)
	      (incr (max .1 (/ 1.0 mouse-avoidance-nudge-dist))))
	  (setq mouse-avoidance-animating-pointer t)
	  (while (<= i 1)
	    (mouse-avoidance-set-mouse-position
	     (cons (+ (car cur-pos) (round (* i deltax)))
		   (+ (cdr cur-pos) (round (* i deltay)))))
    	    (setq i (+ i incr))
	    (if (eq mouse-avoidance-mode 'proteus)
		(mouse-avoidance-set-pointer-shape
		 (mouse-avoidance-random-shape)))
	    (sit-for mouse-avoidance-animation-delay))
	  (setq mouse-avoidance-animating-pointer nil))
      (mouse-avoidance-set-mouse-position (cons (+ (car (cdr cur)) deltax)
						(+ (cdr (cdr cur)) deltay))))))

(defun mouse-avoidance-random-shape ()
  "Return a random cursor shape.
This assumes that any variable whose name begins with x-pointer- and
has an integer value is a valid cursor shape.  You might want to
redefine this function to suit your own tastes."
  (if (null mouse-avoidance-pointer-shapes)
      (progn
	(setq mouse-avoidance-pointer-shapes
	      (mapcar (lambda (x) (symbol-value (intern x)))
		      (all-completions "x-pointer-" obarray
				       (lambda (x)
					  (and (boundp x)
					       (integerp (symbol-value x)))))))
	(setq mouse-avoidance-n-pointer-shapes
	      (length mouse-avoidance-pointer-shapes))))
  (nth (random mouse-avoidance-n-pointer-shapes)
       mouse-avoidance-pointer-shapes))

(defun mouse-avoidance-ignore-p ()
  (let ((mp (mouse-position)))
    (or (not (frame-pointer-visible-p)) ; The pointer is hidden
        (not cursor-type)               ; There's no cursor
        executing-kbd-macro	       ; don't check inside macro
	(null (cadr mp))	       ; don't move unless in an Emacs frame
	(not (eq (car mp) (selected-frame)))
	;; Don't do anything if last event was a mouse event.
	;; FIXME: this code fails in the case where the mouse was moved
	;; since the last key-press but without generating any event.
	(and (consp last-input-event)
	     (symbolp (car last-input-event))
	     (let ((modifiers (event-modifiers (car last-input-event))))
	       (or (memq (car last-input-event)
			 '(mouse-movement scroll-bar-movement
			   select-window switch-frame))
		   (memq 'click modifiers)
		   (memq 'double modifiers)
		   (memq 'triple modifiers)
		   (memq 'drag modifiers)
		   (memq 'down modifiers)))))))

(defun mouse-avoidance-banish ()
  (if (not (mouse-avoidance-ignore-p))
      (mouse-avoidance-banish-mouse)))

(defun mouse-avoidance-exile ()
  ;; For exile mode, the state is nil when the mouse is in its normal
  ;; position, and set to the old mouse-position when the mouse is in exile.
  (if (not (mouse-avoidance-ignore-p))
      (let ((mp (mouse-position)))
	(cond ((and (not mouse-avoidance-state)
		    (mouse-avoidance-too-close-p mp))
	       (setq mouse-avoidance-state mp)
	       (mouse-avoidance-banish-mouse))
	      ((and mouse-avoidance-state
		    (not (mouse-avoidance-too-close-p mouse-avoidance-state)))
	       (if (and (eq (car mp) (selected-frame))
			(equal (cdr mp) (mouse-avoidance-banish-destination)))
		   (mouse-avoidance-set-mouse-position
		    ;; move back only if user has not moved mouse
		    (cdr mouse-avoidance-state)))
	       ;; but clear state anyway, to be ready for another move
	       (setq mouse-avoidance-state nil))))))

(defun mouse-avoidance-fancy ()
  ;; Used for the "fancy" modes, ie jump et al.
  (if (and (not mouse-avoidance-animating-pointer)
	   (not (mouse-avoidance-ignore-p))
	   (mouse-avoidance-too-close-p (mouse-position)))
      (let ((old-pos (mouse-position)))
	(mouse-avoidance-nudge-mouse)
	(if (not (eq (selected-frame) (car old-pos)))
	    ;; This should never happen.
	    (apply 'set-mouse-position old-pos)))))

;;;###autoload
(defun mouse-avoidance-mode (&optional mode)
  "Set Mouse Avoidance mode to MODE.
MODE should be one of the symbols `banish', `exile', `jump', `animate',
`cat-and-mouse', `proteus', or `none'.

If MODE is nil, toggle mouse avoidance between `none' and `banish'
modes.  Positive numbers and symbols other than the above are treated
as equivalent to `banish'; negative numbers and `-' are equivalent to `none'.

Effects of the different modes:
 * banish: Move the mouse to the upper-right corner on any keypress.
 * exile: Move the mouse to the corner only if the cursor gets too close,
     and allow it to return once the cursor is out of the way.
 * jump: If the cursor gets too close to the mouse, displace the mouse
     a random distance & direction.
 * animate: As `jump', but shows steps along the way for illusion of motion.
 * cat-and-mouse: Same as `animate'.
 * proteus: As `animate', but changes the shape of the mouse pointer too.

Whenever the mouse is moved, the frame is also raised.

\(See `mouse-avoidance-threshold' for definition of \"too close\",
and `mouse-avoidance-nudge-dist' and `mouse-avoidance-nudge-var' for
definition of \"random distance\".)"
  (interactive
   (list (intern (completing-read
		  "Select cursor avoidance technique (SPACE for list): "
		  '(("banish") ("exile") ("jump") ("animate")
		    ("cat-and-mouse") ("proteus") ("none"))
		  nil t))))
  (if (eq mode 'cat-and-mouse)
      (setq mode 'animate))
  (if mouse-avoidance-timer
      (cancel-timer mouse-avoidance-timer))
  (setq mouse-avoidance-timer nil)

  ;; Restore pointer shape if necessary
  (if (eq mouse-avoidance-mode 'proteus)
      (mouse-avoidance-set-pointer-shape mouse-avoidance-old-pointer-shape))

  ;; Do additional setup depending on version of mode requested
  (cond	((eq mode 'none)
	 (setq mouse-avoidance-mode nil))
	((or (eq mode 'jump)
	     (eq mode 'animate)
	     (eq mode 'proteus))
	 (setq mouse-avoidance-timer
	       (run-with-idle-timer 0.1 t 'mouse-avoidance-fancy))
	 (setq mouse-avoidance-mode mode
	       mouse-avoidance-state (cons 0 0)
	       mouse-avoidance-old-pointer-shape
	       (and (boundp 'x-pointer-shape) x-pointer-shape)))
	((eq mode 'exile)
	 (setq mouse-avoidance-timer
	       (run-with-idle-timer 0.1 t 'mouse-avoidance-exile))
	 (setq mouse-avoidance-mode mode
	       mouse-avoidance-state nil))
	((or (eq mode 'banish)
	     (eq mode t)
	     (and (null mode) (null mouse-avoidance-mode))
	     (and mode (> (prefix-numeric-value mode) 0)))
	 (setq mouse-avoidance-timer
	       (run-with-idle-timer 0.1 t 'mouse-avoidance-banish))
	 (setq mouse-avoidance-mode 'banish))
	(t (setq mouse-avoidance-mode nil)))
  (force-mode-line-update))

;; Most people who use avoid mode leave it on all the time, so it's not
;; very informative to announce it in the mode line.
;;(or (assq 'mouse-avoidance-mode minor-mode-alist)
;;    (setq minor-mode-alist (cons '(mouse-avoidance-mode " Avoid")
;;				 minor-mode-alist)))

;; Needed for custom.
(if mouse-avoidance-mode
    (mouse-avoidance-mode mouse-avoidance-mode))

;;; avoid.el ends here
