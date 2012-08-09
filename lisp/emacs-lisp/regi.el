;;; regi.el --- REGular expression Interpreting engine

;; Copyright (C) 1993, 2001-2012 Free Software Foundation, Inc.

;; Author: 1993 Barry A. Warsaw, Century Computing, Inc. <bwarsaw@cen.com>
;; Maintainer:    bwarsaw@cen.com
;; Created:       24-Feb-1993
;; Version:       1.8
;; Last Modified: 1993/06/01 21:33:00
;; Keywords:      extensions, matching

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


(defun regi-pos (&optional position col-p)
  "Return the character position at various buffer positions.
Optional POSITION can be one of the following symbols:

`bol'  == beginning of line
`boi'  == beginning of indentation
`eol'  == end of line [default]
`bonl' == beginning of next line
`bopl' == beginning of previous line

Optional COL-P non-nil returns `current-column' instead of character position."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (end-of-line)))
    (if col-p (current-column) (point))))

(defun regi-mapcar (predlist func &optional negate-p case-fold-search-p)
  "Build a regi frame where each element of PREDLIST appears exactly once.
The frame contains elements where each member of PREDLIST is
associated with FUNC, and optionally NEGATE-P and CASE-FOLD-SEARCH-P."
  (let (frame tail)
    (if (or negate-p case-fold-search-p)
	(setq tail (list negate-p)))
    (if case-fold-search-p
	(setq tail (append tail (list case-fold-search-p))))
    (while predlist
      (let ((element (list (car predlist) func)))
	(if tail
	    (setq element (append element tail)))
	(setq frame (append frame (list element))
	      predlist (cdr predlist))
	))
    frame))


(defun regi-interpret (frame &optional start end)
  "Interpret the regi frame FRAME.
If optional START and END are supplied, they indicate the region of
interest, and the buffer is narrowed to the beginning of the line
containing START, and beginning of the line after the line containing
END.  Otherwise, point and mark are not set and processing continues
until your FUNC returns the `abort' symbol (see below).  Beware!  Not
supplying a START or END could put you in an infinite loop.

A regi frame is a list of entries of the form:

     (PRED FUNC [NEGATE-P [CASE-FOLD-SEARCH]])

PRED is a predicate against which each line in the region is tested,
and if a match occurs, FUNC is `eval'd.  Point is then moved to the
beginning of the next line, the frame is reset and checking continues.
If a match doesn't occur, the next entry is checked against the
current line until all entries in the frame are checked.  At this
point, if no match occurred, the frame is reset and point is moved to
the next line.  Checking continues until every line in the region is
checked.  Optional NEGATE-P inverts the result of PRED before FUNC is
called and `case-fold-search' is bound to the optional value of
CASE-FOLD-SEARCH for the PRED check.

PRED can be a string, variable, function or one of the following
symbols: t, nil, `begin', `end', and `every'.  If PRED is a string, or
a variable or list that evaluates to a string, it is interpreted as a
regular expression and is matched against the current line (from the
beginning) using `looking-at'.  If PRED does not evaluate to a string,
it is interpreted as a binary value (nil or non-nil).

PRED can also be one of the following symbols:

t       -- always produces a true outcome
`begin' -- always executes before anything else
`end'   -- always executes after everything else
`every' -- execute after frame is matched on a line

Note that NEGATE-P and CASE-FOLD-SEARCH are meaningless if PRED is one
of these special symbols.  Only the first occurrence of each symbol in
a frame entry is used, the rest are ignored.

Your FUNC can return values which control regi processing.  If a list
is returned from your function, it can contain any combination of the
following elements:

the symbol `continue'
     Tells regi to continue processing frame-entries after a match,
     instead of resetting to the first entry and advancing to the next
     line, as is the default behavior.  When returning this symbol,
     you must take care not to enter an infinite loop.

the symbol `abort'
     Tells regi to terminate processing this frame.  any end
     frame-entry is still processed.

the list `(frame . NEWFRAME)'
     Tells regi to use NEWFRAME as its current frame.  In other words,
     your FUNC can modify the executing regi frame on the fly.

the list `(step . STEP)'
     Tells regi to move STEP number of lines forward during normal
     processing.  By default, regi moves forward 1 line.  STEP can be
     negative, but be careful of infinite loops.

You should usually take care to explicitly return nil from your
function if no action is to take place.  Your FUNC will always be
`eval'ed.  The following variables will be temporarily bound to some
useful information:

`curline'
     the current line in the buffer, as a string

`curframe'
     the full, current frame being executed

`curentry'
     the current frame entry being executed."

  (save-excursion
    (save-restriction
      (let (begin-tag end-tag every-tag current-frame working-frame donep)

	;; set up the narrowed region
	(and start
	     end
	     (let* ((tstart start)
		    (start (min start end))
		    (end   (max start end)))
	       (narrow-to-region
		(progn (goto-char end) (regi-pos 'bonl))
		(progn (goto-char start) (regi-pos 'bol)))))

	;; let's find the special tags and remove them from the working
	;; frame. note that only the last special tag is used.
	(mapc
	 (function
	  (lambda (entry)
	    (let ((pred (car entry))
		  (func (car (cdr entry))))
	      (cond
	       ((eq pred 'begin) (setq begin-tag func))
	       ((eq pred 'end)   (setq end-tag func))
	       ((eq pred 'every) (setq every-tag func))
	       (t
		(setq working-frame (append working-frame (list entry))))
	       ) ; end-cond
	      )))
	 frame) ; end-mapcar

	;; execute the begin entry
	(eval begin-tag)

	;; now process the frame
	(setq current-frame working-frame)
	(while (not (or donep (eobp)))
	  (let* ((entry            (car current-frame))
		 (pred             (nth 0 entry))
		 (func             (nth 1 entry))
		 (negate-p         (nth 2 entry))
		 (case-fold-search (nth 3 entry))
		 match-p)
	    (catch 'regi-throw-top
	      (cond
	       ;; we are finished processing the frame for this line
	       ((not current-frame)
		(setq current-frame working-frame) ;reset frame
		(forward-line 1)
		(throw 'regi-throw-top t))
	       ;; see if predicate evaluates to a string
	       ((stringp (setq match-p (eval pred)))
		(setq match-p (looking-at match-p)))
	       ) ; end-cond

	      ;; now that we've done the initial matching, check for
	      ;; negation of match
	      (and negate-p
		   (setq match-p (not match-p)))

	      ;; if the line matched, package up the argument list and
	      ;; funcall the FUNC
	      (if match-p
		   (let* ((curline (buffer-substring
				    (regi-pos 'bol)
				    (regi-pos 'eol)))
			  (curframe current-frame)
			  (curentry entry)
			  (result (eval func))
			  (step (or (cdr (assq 'step result)) 1))
			  )
		     ;; changing frame on the fly?
		     (if (assq 'frame result)
			 (setq working-frame (cdr (assq 'frame result))))

		     ;; continue processing current frame?
		     (if (memq 'continue result)
			 (setq current-frame (cdr current-frame))
		       (forward-line step)
		       (setq current-frame working-frame))

		     ;; abort current frame?
		     (if (memq 'abort result)
			 (progn
			   (setq donep t)
			   (throw 'regi-throw-top t)))
		     ) ; end-let

		;; else if no match occurred, then process the next
		;; frame-entry on the current line
		(setq current-frame (cdr current-frame))

		) ; end-if match-p
	      ) ; end catch
	    ) ; end let

	  ;; after every cycle, evaluate every-tag
	  (eval every-tag)
	  ) ; end-while

	;; now process the end entry
	(eval end-tag)))))


(provide 'regi)

;;; regi.el ends here
