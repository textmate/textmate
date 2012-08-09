;;; reposition.el --- center a Lisp function or comment on the screen

;; Copyright (C) 1991, 1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Michael D. Ernst <mernst@theory.lcs.mit.edu>
;; Created: Jan 1991
;; Maintainer: FSF

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

;; Reposition-window makes an entire function definition or comment visible,
;; or, if it is already visible, places it at the top of the window;
;; additional invocations toggle the visibility of comments preceding the
;; code.  For the gory details, see the documentation for reposition-window;
;; rather than reading that, you may just want to play with it.

;; This tries pretty hard to do the recentering correctly; the precise
;; action depends on what the buffer looks like.  If you find a situation
;; where it doesn't behave well, let me know.  This function is modeled
;; after one of the same name in ZMACS, but the code is all-new and the
;; behavior in some situations differs.

;;; Code:

;;;###autoload
(defun reposition-window (&optional arg)
  "Make the current definition and/or comment visible.
Further invocations move it to the top of the window or toggle the
visibility of comments that precede it.
  Point is left unchanged unless prefix ARG is supplied.
  If the definition is fully onscreen, it is moved to the top of the
window.  If it is partly offscreen, the window is scrolled to get the
definition (or as much as will fit) onscreen, unless point is in a comment
which is also partly offscreen, in which case the scrolling attempts to get
as much of the comment onscreen as possible.
  Initially `reposition-window' attempts to make both the definition and
preceding comments visible.  Further invocations toggle the visibility of
the comment lines.
  If ARG is non-nil, point may move in order to make the whole defun
visible (if only part could otherwise be made so), to make the defun line
visible (if point is in code and it could not be made so, or if only
comments, including the first comment line, are visible), or to make the
first comment line visible (if point is in a comment)."
  (interactive "P")
  (let* (;; (here (line-beginning-position))
	 (here (point))
	 ;; change this name once I've gotten rid of references to ht.
	 ;; this is actually the number of the last screen line
	 (ht (- (window-height (selected-window)) 2))
	 (line (repos-count-screen-lines (window-start) (point)))
	 (comment-height
	  ;; The call to max deals with the case of cursor between defuns.
	  (max 0
	       (repos-count-screen-lines-signed
		;; the beginning of the preceding comment
		(save-excursion
		  (if (not (eobp)) (forward-char 1))
		  (end-of-defun -1)
		  ;; Skip whitespace, newlines, and form feeds.
		  (if (re-search-forward "[^ \t\n\f]" nil t)
		      (backward-char 1))
		  (point))
		here)))
	 (defun-height
	   (repos-count-screen-lines-signed
	    (save-excursion
	      (end-of-defun 1) ; so comments associate with following defuns
	      (beginning-of-defun 1)
	      (point))
	    here))
	 ;; This must be positive, so don't use the signed version.
	 (defun-depth (repos-count-screen-lines here
						(save-excursion
						  (end-of-defun 1)
						  (point))))
	 (defun-line-onscreen-p
	   (and (<= defun-height line)
		(<= (- line defun-height) ht))))
    (cond ((or (= comment-height line)
	       (and (= line ht)
		    (> comment-height line)
		    ;; if defun line offscreen, we should be in case 4
		    defun-line-onscreen-p))
	   ;; Either first comment line is at top of screen or (point at
	   ;; bottom of screen, defun line onscreen, and first comment line
	   ;; off top of screen).  That is, it looks like we just did
	   ;; recenter-definition, trying to fit as much of the comment
	   ;; onscreen as possible.  Put defun line at top of screen; that
	   ;; is, show as much code, and as few comments, as possible.

	   (if (and arg (> defun-depth (1+ ht)))
	       ;; Can't fit whole defun onscreen without moving point.
	       (progn (end-of-defun) (beginning-of-defun) (recenter 0))
	     (recenter (max defun-height 0)))
	   ;;(repos-debug-macro "1")
	   )

	  ((or (= defun-height line)
	       (= line 0)
	       (and (< line comment-height)
		    (< defun-height 0)))
	   ;; Defun line or cursor at top of screen, OR cursor in comment
	   ;; whose first line is offscreen.
	   ;; Avoid moving definition up even if defun runs offscreen;
	   ;; we care more about getting the comment onscreen.

	   (cond ((= line ht)
		  ;; cursor on last screen line (and so in a comment)
		  (if arg (progn (end-of-defun) (beginning-of-defun)))
		  (recenter 0)
		  ;;(repos-debug-macro "2a")
		  )

		 ;; This condition, copied from case 4, may not be quite right

		 ((and arg (< ht comment-height))
		  ;; Can't get first comment line onscreen.
		  ;; Go there and try again.
		  (forward-line (- comment-height))
		  (beginning-of-line)
		  ;; was (reposition-window)
		  (recenter 0)
		  ;;(repos-debug-macro "2b")
		  )
		 (t
		  (recenter (min ht comment-height))
		  ;;(repos-debug-macro "2c")
		  ))
	   ;; (recenter (min ht comment-height))
	   )

	  ((and (> (+ line defun-depth -1) ht)
		defun-line-onscreen-p)
	   ;; Defun runs off the bottom of the screen and the defun line
	   ;; is onscreen.
	   ;; Move the defun up.
	   (recenter (max 0 (1+ (- ht defun-depth)) defun-height))
	   ;;(repos-debug-macro "3")
	   )

	  (t
	   ;; If on the bottom line and comment start is offscreen
	   ;; then just move all comments offscreen, or at least as
	   ;; far as they'll go.

	   ;; Try to get as much of the comments onscreen as possible.
	   (if (and arg (< ht comment-height))
	       ;; Can't get defun line onscreen; go there and try again.
	       (progn (forward-line (- defun-height))
		      (beginning-of-line)
		      (reposition-window))
	     (recenter (min ht comment-height)))
	   ;;(repos-debug-macro "4")
	   ))))

;;; Auxiliary functions

;; Return number of screen lines between START and END.
(defun repos-count-screen-lines (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (vertical-motion (- (point-max) (point-min))))))

;; Return number of screen lines between START and END; returns a negative
;; number if END precedes START.
(defun repos-count-screen-lines-signed (start end)
  (let ((lines (repos-count-screen-lines start end)))
    (if (< start end)
	lines
      (- lines))))

;; (defmacro repos-debug-macro (case-no)
;;   `(message (concat "Case " ,case-no ": %s %s %s %s %s")
;;             ht line comment-height defun-height defun-depth))

(provide 'reposition)

;;; reposition.el ends here
