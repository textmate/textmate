;;; mouse-drag.el --- use mouse-2 to do a new style of scrolling

;; Copyright (C) 1996-1997, 2001-2012  Free Software Foundation, Inc.

;; Author: John Heidemann <johnh@ISI.EDU>
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

;; What is ``mouse-drag.el''?
;;
;; Doesn't that scroll bar seem far away when you want to scroll?
;; This module overloads mouse-2 to do ``throw'' scrolling.  You
;; click and drag.  The distance you move from your original click
;; turns into a scroll amount.  The scroll amount is scaled
;; exponentially to make both large moves and short adjustments easy.
;; What this boils down to is that you can easily scroll around the
;; buffer without much mouse movement.  Finally, clicks which aren't
;; drags are passed off to the old mouse-2 binding, so old mouse-2
;; operations (find-file in dired-mode, yanking in most other modes)
;; still work.
;;
;; There is an alternative way to scroll, ``drag'' scrolling.  You
;; can click on a character and then drag it around, scrolling the
;; buffer with you.  The character always stays under the mouse.
;; Compared to throw-scrolling, this approach provides direct
;; manipulation (nice) but requires more mouse movement
;; (unfortunate).  It is offered as an alternative for those who
;; prefer it.
;;
;; If you like mouse-drag, you should also check out mouse-copy
;; for ``one-click text copy and move''.
;;
;; To use mouse-drag, place the following in your .emacs file:
;; -either-
;;     (global-set-key [down-mouse-2] 'mouse-drag-throw)
;; -or-
;;     (global-set-key [down-mouse-2] 'mouse-drag-drag)
;;
;;
;;
;; Options:
;;
;; - reverse the throw-scroll direction with \\[mouse-throw-with-scroll-bar]
;; - work around a bug with \\[mouse-extras-work-around-drag-bug]
;; - auto-enable horizontal scrolling with
;;   \\[mouse-drag-electric-col-scrolling]
;;
;;
;; History and related work:
;;
;; One-click copying and moving was inspired by lemacs-19.8.
;; Throw-scrolling was inspired by MacPaint's ``hand'' and by Tk's
;; mouse-2 scrolling.  The package mouse-scroll.el by Tom Wurgler
;; <twurgler@goodyear.com> is similar to mouse-drag-throw, but
;; doesn't pass clicks through.
;;
;; These functions have been tested in emacs version 19.30,
;; and this package has run in the past on 19.25-19.29.
;;
;; Originally mouse-drag was part of a larger package.
;; As of 11 July 96 the scrolling functions were split out
;; in preparation for incorporation into (the future) emacs-19.32.
;;
;; Thanks:
;;
;; Thanks to Kai Grossjohann
;; <grossjoh@dusty.informatik.uni-dortmund.de> for reporting bugs, to
;; Tom Wurgler <twurgler@goodyear.com> for reporting bugs and
;; suggesting fixes, and to Joel Graber <jgraber@ti.com> for
;; prompting me to do drag-scrolling and for an initial
;; implementation of horizontal drag-scrolling.
;;
;;    -johnh@isi.edu, 11-Jul-96
;;
;;
;; What's new with mouse-drag 2.24?
;;
;; - mouse-drag-electric-col-scrolling (default: on)
;;   auto-enables horizontal scrolling when clicks on wrapped
;;   lines occur

;; TODO:
;; - For mouse-drag-throw, we should try and place some visual indicator
;;   of the original mouse position (like Firefox does).

;;; Code:

;;
;; scrolling code
;;

(defun mouse-drag-safe-scroll (row-delta &optional col-delta)
  "Scroll down ROW-DELTA lines and right COL-DELTA, ignoring buffer edge errors.
Keep the cursor on the screen as needed."
  (let ((scroll-preserve-screen-position nil))
    (if (and row-delta
	     (/= 0 row-delta))
	(condition-case nil ;; catch and ignore movement errors
	    (scroll-down row-delta)
	  (beginning-of-buffer (message "Beginning of buffer"))
	  (end-of-buffer (message "End of buffer"))))
    (if (and col-delta
	     (/= 0 col-delta))
	(progn
	  (scroll-right col-delta)
	  ;; Make sure that the point stays on the visible screen
	  ;; (if truncation-lines in set).
	  ;; This code mimics the behavior we automatically get
	  ;; when doing vertical scrolling.
	  ;; Problem identified and a fix suggested by Tom Wurgler.
	  (cond
	   ((< (current-column) (window-hscroll))
	    (move-to-column (window-hscroll))) ; make on left column
	   ((> (- (current-column) (window-hscroll) (window-width) -2) 0)
	    (move-to-column (+ (window-width) (window-hscroll) -3))))))))

(defun mouse-drag-repeatedly-safe-scroll (row-delta &optional col-delta)
  "Scroll ROW-DELTA rows and COL-DELTA cols until an event happens."
  (while (sit-for mouse-scroll-delay)
    (mouse-drag-safe-scroll row-delta col-delta)))

(defun mouse-drag-events-are-point-events-p (start-posn end-posn)
  "Determine if START-POSN and END-POSN are \"close\"."
  (let*
      ((start-col-row (posn-col-row start-posn))
       (end-col-row (posn-col-row end-posn)))
    (and
     ;; ;; We no longer exclude things by time.
     ;; (< (- (posn-timestamp end-posn) (posn-timestamp start-posn))
     ;;    (if (numberp double-click-time)
     ;;        (* 2 double-click-time) ;; stretch it a little
     ;;      999999)) ;; non-numeric => check by position alone
     (= (car start-col-row) (car end-col-row))
     (= (cdr start-col-row) (cdr end-col-row)))))

(defvar mouse-drag-electric-col-scrolling t
  "If non-nil, mouse-drag on a long line enables truncate-lines.")

(defun mouse-drag-should-do-col-scrolling ()
  "Determine if it's wise to enable col-scrolling for the current window.
Basically, we check for existing horizontal scrolling."
  (or truncate-lines
      (> (window-hscroll (selected-window)) 0)
      (not (window-full-width-p))
      (and
       mouse-drag-electric-col-scrolling
       (save-excursion  ;; on a long line?
	 (let
	     ((beg (line-beginning-position))
	      (end (progn (end-of-line) (point))))
	   (if (> (- end beg) (window-width))
	       (setq truncate-lines t)
	     nil))))))

(defvar mouse-throw-with-scroll-bar nil
  "*Set direction of mouse-throwing.
If nil, the text moves in the direction the mouse moves.
If t, the scroll bar moves in the direction the mouse moves.")
(defconst mouse-throw-magnifier-min -6)
(defconst mouse-throw-magnifier-max 6)
(defconst mouse-throw-magnifier-base 1.5)

(defun mouse-drag-scroll-delta (mouse-delta)
  ;; Limit the exponential explosion.
  (setq mouse-delta
        (max mouse-throw-magnifier-min
             (min mouse-throw-magnifier-max mouse-delta)))
  (* (round (exp (* (log mouse-throw-magnifier-base) (abs mouse-delta))))
     (if (< mouse-delta 0) -1 1)
     (if mouse-throw-with-scroll-bar 1 -1)))

;;;###autoload
(defun mouse-drag-throw (start-event)
  "\"Throw\" the page according to a mouse drag.

A \"throw\" is scrolling the page at a speed relative to the distance
from the original mouse click to the current mouse location.  Try it;
you'll like it.  It's easier to observe than to explain.

If the mouse is clicked and released in the same place of time we
assume that the user didn't want to scroll but wanted to whatever
mouse-2 used to do, so we pass it through.

Throw scrolling was inspired (but is not identical to) the \"hand\"
option in MacPaint, or the middle button in Tk text widgets.

If `mouse-throw-with-scroll-bar' is non-nil, then this command scrolls
in the opposite direction.  (Different people have different ideas
about which direction is natural.  Perhaps it has to do with which
hemisphere you're in.)

To test this function, evaluate:
    (global-set-key [down-mouse-2] 'mouse-drag-throw)"
  (interactive "e")
  ;; we want to do save-selected-window, but that requires 19.29
  (let* ((start-posn (event-start start-event))
	 (start-window (posn-window start-posn))
	 (start-row (cdr (posn-col-row start-posn)))
	 (start-col (car (posn-col-row start-posn)))
	 (old-selected-window (selected-window))
	 event end row scroll-delta
	 have-scrolled
	 col
	 (scroll-col-delta 0)
	 ;; be conservative about allowing horizontal scrolling
	 (col-scrolling-p (mouse-drag-should-do-col-scrolling)))
    (select-window start-window)
    (track-mouse
      (while (progn
	       (setq event (read-event)
		     end (event-end event)
		     row (cdr (posn-col-row end))
		     col (car (posn-col-row end)))
	       (or (mouse-movement-p event)
		   (eq (car-safe event) 'switch-frame)))
	(when (eq start-window (posn-window end))
          (when col-scrolling-p
            (setq scroll-col-delta (mouse-drag-scroll-delta (- start-col col))))
          (setq scroll-delta (mouse-drag-scroll-delta (- start-row row))))

	(if (or (/= 0 scroll-delta)
		(/= 0 scroll-col-delta))
	    (progn
	      (setq have-scrolled t)
	      (mouse-drag-safe-scroll scroll-delta scroll-col-delta)
	      (mouse-drag-repeatedly-safe-scroll scroll-delta scroll-col-delta))))) ;xxx
    ;; If it was a click and not a drag, prepare to pass the event on.
    ;; Is there a more correct way to reconstruct the event?
    (if (and (not have-scrolled)
	     (mouse-drag-events-are-point-events-p start-posn end))
	(push (cons (event-basic-type start-event) (cdr start-event))
	      unread-command-events))
    ;; Now restore the old window.
    (select-window old-selected-window)))

;;;###autoload
(defun mouse-drag-drag (start-event)
  "\"Drag\" the page according to a mouse drag.

Drag scrolling moves the page according to the movement of the mouse.
You \"grab\" the character under the mouse and move it around.

If the mouse is clicked and released in the same place of time we
assume that the user didn't want to scroll but wanted to whatever
mouse-2 used to do, so we pass it through.

Drag scrolling is identical to the \"hand\" option in MacPaint, or the
middle button in Tk text widgets.

To test this function, evaluate:
    (global-set-key [down-mouse-2] 'mouse-drag-drag)"
  (interactive "e")
  ;; we want to do save-selected-window, but that requires 19.29
  (let* ((start-posn (event-start start-event))
	 (start-window (posn-window start-posn))
	 (start-row (cdr (posn-col-row start-posn)))
	 (start-col (car (posn-col-row start-posn)))
	 (old-selected-window (selected-window))
	 event end row scroll-delta
	 have-scrolled
	 window-last-row
	 col window-last-col
	 (scroll-col-delta 0)
	 ;; be conservative about allowing horizontal scrolling
	 (col-scrolling-p (mouse-drag-should-do-col-scrolling)))
    (select-window start-window)
    (setq window-last-row (- (window-height) 2)
	  window-last-col (- (window-width) 2))
    (track-mouse
      (while (progn
	       (setq event (read-event)
		     end (event-end event)
		     row (cdr (posn-col-row end))
		     col (car (posn-col-row end)))
	       (or (mouse-movement-p event)
		   (eq (car-safe event) 'switch-frame)))
	;; Scroll if see if we're on the edge.
	;; NEEDSWORK: should handle mouse-in-other window.
	(cond
	 ((not (eq start-window (posn-window end)))
	  t) ; wait for return to original window
	 ((<= row 0) (mouse-drag-repeatedly-safe-scroll -1 0))
	 ((>= row window-last-row) (mouse-drag-repeatedly-safe-scroll 1 0))
	 ((and col-scrolling-p (<= col 1)) (mouse-drag-repeatedly-safe-scroll 0 -1))
	 ((and col-scrolling-p (>= col window-last-col)) (mouse-drag-repeatedly-safe-scroll 0 1))
	 (t
	  (setq scroll-delta (- row start-row)
		start-row row)
	  (if col-scrolling-p
	      (setq scroll-col-delta (- col start-col)
		    start-col col))
	  (if (or (/= 0 scroll-delta)
		  (/= 0 scroll-col-delta))
	      (progn
		(setq have-scrolled t)
		(mouse-drag-safe-scroll scroll-delta scroll-col-delta)))))))
    ;; If it was a click and not a drag, prepare to pass the event on.
    ;; Is there a more correct way to reconstruct the event?
    (if (and (not have-scrolled)
	     (mouse-drag-events-are-point-events-p start-posn end))
	(push (cons (event-basic-type start-event) (cdr start-event))
	      unread-command-events))
    ;; Now restore the old window.
    (select-window old-selected-window)))


(provide 'mouse-drag)

;;; mouse-drag.el ends here
