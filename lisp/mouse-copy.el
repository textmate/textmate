;;; mouse-copy.el --- one-click text copy and move

;; Copyright (C) 1996, 2001-2012 Free Software Foundation, Inc.

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

;; What is ``mouse-copy.el''?
;;
;; It provides one-click text copy and move.  Rather than the
;; standard stroke-out-a-region (down-mouse-1, up-mouse-1) followed
;; by a yank (down-mouse-2, up-mouse-2 or C-y), you can now stroke
;; out a region and have it automatically pasted at the current
;; point.  You can also move text just as easily.  Although the
;; difference may not sound like much, it does make mousing text
;; around a lot easier, IMHO.
;;
;; If you like mouse-copy, you should also check out mouse-drag
;; for ``one-click scrolling''.
;;
;; To use mouse-copy, place the following in your .emacs file:
;;	(require 'mouse-copy)
;;     (global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
;;     (global-set-key [M-S-down-mouse-1] 'mouse-drag-secondary-moving)
;;
;; (These definitions override the old binding of M-mouse-1 to
;; mouse-drag-secondary.  I find I don't use that command much so its
;; loss is not important, and it can be made up with a M-mouse-1
;; followed by a M-mouse-3.  I personally reserve M-mouse bindings
;; for my window manager and bind everything to C-mouse.)
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
;; Originally mouse-copy was part of a larger package.
;; As of 11 July 96 the scrolling functions were split out
;; in preparation for incorporation into (the future) emacs-19.32.
;;
;;
;; Known Bugs:
;;
;; - Highlighting is sub-optimal under 19.29 and XFree86-3.1.1
;;   (see \\[mouse-copy-work-around-drag-bug] for details).
;; - mouse-drag-secondary-pasting and mouse-drag-secondary-moving
;;   require X11R5 (or better) and so fail under older versions
;;   of Open Windows (like that present in Solaris/x86 2.1).
;;
;;
;; Future plans:
;;
;; I read about the chording features of Plan-9's Acme environment at
;; <http://www.zip.com.au/~cs/app/wily/auug.html>.  I'd like
;; to incorporate some of these ideas into mouse-copy.  The only
;; lose is that this is not the current Emacs Way Of Doing Things, so
;; there would be a learning curve for existing emacs users.
;;
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
;;    -johnh, 11-Jul-96

;;; Code:

;;
;; move/paste code
;;

(defvar mouse-copy-last-paste-start nil
  "Internal to `mouse-drag-secondary-pasting'.")
(defvar mouse-copy-last-paste-end nil
  "Internal to `mouse-drag-secondary-pasting'.")

(defvar mouse-copy-have-drag-bug nil
  "Set to enable mouse-copy-work-around-drag-bug.
See `mouse-copy-work-around-drag-bug' for details.")

(defun mouse-copy-work-around-drag-bug (start-event end-event)
  "Code to work around a bug in post-19.29 Emacs: it drops mouse-drag events.
The problem occurs under XFree86-3.1.1 (X11R6pl11) but not under X11R5,
and under post-19.29 but not early versions of Emacs.

19.29 and 19.30 seems to drop mouse drag events
sometimes. (Reproducible under XFree86-3.1.1 (X11R6pl11) and
XFree86-3.1.2 under Linux 1.2.x.  Doesn't occur under X11R5 and SunOS
4.1.1.)

To see if you have the problem:
Disable this routine (with (setq mouse-copy-have-drag-bug nil)).
Click and drag for a while.
If highlighting stops tracking, you have the bug.
If you have the bug (or the real fix :-), please let me know."

  ;; To work-around, call mouse-set-secondary with a fake
  ;; drag event to set the overlay,
  ;; the load the x-selection.
  (save-excursion
    (let*
	((start-posn (event-start start-event))
	 (end-posn (event-end end-event))
	 (end-buffer (window-buffer (posn-window end-posn)))
	 ;; First, figure out the region (left as point/mark).
	 (range (progn
		  (set-buffer end-buffer)
		  (mouse-start-end (posn-point start-posn)
				 (posn-point end-posn)
				 (1- (event-click-count start-event)))))
	 (beg (car range))
	 (end (car (cdr range))))
      ;; Second, set the overlay.
      (if mouse-secondary-overlay
	  (move-overlay mouse-secondary-overlay beg end)
	(setq mouse-secondary-overlay (make-overlay beg (posn-point end))))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection)
      ;; Third, set the selection.
      ;; (setq me-beg beg me-end end me-range range)  ; for debugging
      (set-buffer end-buffer)
      (x-set-selection 'SECONDARY (buffer-substring beg end)))))


(defun mouse-drag-secondary-pasting (start-event)
  "Drag out a secondary selection, then paste it at the current point.

To test this function, evaluate:
	(global-set-key [M-down-mouse-1] 'mouse-drag-secondary-pasting)
put the point at one place, then click and drag over some other region."
  (interactive "e")
  ;; Work-around: We see and react to each part of a multi-click event
  ;; as it proceeds.  For a triple-event, this means the double-event
  ;; has already copied something that the triple-event will re-copy
  ;; (a Bad Thing).  We therefore undo the prior insertion if we're on
  ;; a multiple event.
  (if (and mouse-copy-last-paste-start
	   (>= (event-click-count start-event) 2))
      (delete-region mouse-copy-last-paste-start
		     mouse-copy-last-paste-end))

  ;; HACK: We assume that mouse-drag-secondary returns nil if
  ;; there's no secondary selection.  This assumption holds as of
  ;; emacs-19.22 but is not documented.  It's not clear that there's
  ;; any other way to get this information.
  (if (mouse-drag-secondary start-event)
      (progn
	(if mouse-copy-have-drag-bug
	    (mouse-copy-work-around-drag-bug start-event last-input-event))
	;; Remember what we do so we can undo it, if necessary.
	(setq mouse-copy-last-paste-start (point))
	(insert (x-get-selection 'SECONDARY))
	(setq mouse-copy-last-paste-end (point)))
    (setq mouse-copy-last-paste-start nil)))


(defun mouse-kill-preserving-secondary ()
  "Kill the text in the secondary selection, but leave the selection set.

This command is like \\[mouse-kill-secondary] (that is, the secondary
selection is deleted and placed in the kill ring), except that it also
leaves the secondary buffer active on exit.

This command was derived from mouse-kill-secondary in emacs-19.28
by johnh@ficus.cs.ucla.edu."
  (interactive)
  (let* ((keys (this-command-keys))
	 (click (elt keys (1- (length keys)))))
    (or (eq (overlay-buffer mouse-secondary-overlay)
	    (if (listp click)
		(window-buffer (posn-window (event-start click)))
	      (current-buffer)))
	(error "Select or click on the buffer where the secondary selection is")))
  (with-current-buffer (overlay-buffer mouse-secondary-overlay)
    (kill-region (overlay-start mouse-secondary-overlay)
		 (overlay-end mouse-secondary-overlay)))
  ;; (delete-overlay mouse-secondary-overlay)
  ;; (x-set-selection 'SECONDARY nil)
  ;; (setq mouse-secondary-overlay nil)
)

(defun mouse-drag-secondary-moving (start-event)
  "Sweep out a secondary selection, then move it to the current point."
  (interactive "e")
  ;; HACK:  We assume that mouse-drag-secondary returns nil if
  ;; there's no secondary selection.  This works as of emacs-19.22.
  ;; It's not clear that there's any other way to get this information.
  (if (mouse-drag-secondary start-event)
      (progn
	(mouse-kill-preserving-secondary)
	(insert (x-get-selection 'SECONDARY))))
)

(provide 'mouse-copy)

;;; mouse-copy.el ends here
