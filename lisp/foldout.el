;;; foldout.el --- folding extensions for outline-mode and outline-minor-mode

;; Copyright (C) 1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Kevin Broadey <KevinB@bartley.demon.co.uk>
;; Maintainer: FSF
;; Created: 27 Jan 1994
;; Version: 1.10
;; Keywords: folding, outlines

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

;; This file provides folding editor extensions for outline-mode and
;; outline-minor-mode buffers.  What's a "folding editor"?  Read on...
;;
;; Imagine you're in an outline-mode buffer and you've hidden all the text and
;; subheadings under your level-1 headings.  You now want to look at the stuff
;; hidden under one of these headings.  Normally you'd do C-c C-e (show-entry)
;; to expose the body or C-c C-i to expose the child (level-2) headings.
;;
;; With foldout, you do C-c C-z (foldout-zoom-subtree).  This exposes the body
;; and child subheadings and narrows the buffer so that only the level-1
;; heading, the body and the level-2 headings are visible.  If you now want to
;; look under one of the level-2 headings, position the cursor on it and do C-c
;; C-z again.  This exposes the level-2 body and its level-3 child subheadings
;; and narrows the buffer again.  You can keep on zooming in on successive
;; subheadings as much as you like.  A string in the modeline tells you how
;; deep you've gone.
;;
;; When zooming in on a heading you might only want to see the child
;; subheadings.  You do this by specifying a numeric argument: C-u C-c C-z.
;; You can specify the number of levels of children too (c.f. show-children):
;; e.g. M-2 C-c C-z exposes two levels of child subheadings.  Alternatively,
;; you might only be interested in the body.  You do this by specifying a
;; negative argument: M-- C-c C-z.  You can also cause the whole subtree to be
;; expanded, similar to C-c C-s (show-subtree), by specifying a zero argument:
;; M-0 C-c C-z.
;;
;; While you're zoomed in you can still use outline-mode's exposure and hiding
;; functions.  It won't upset foldout at all.  Also, since the buffer is
;; narrowed, "global" editing actions will only affect the stuff under the
;; zoomed-in heading.  This is useful for restricting changes to a particular
;; chapter or section of your document.
;;
;; You unzoom (exit) a fold by doing C-c C-x (foldout-exit-fold).  This hides
;; all the text and subheadings under the top-level heading and returns you to
;; the previous view of the buffer.  Specifying a numeric argument exits that
;; many folds.  Specifying a zero argument exits *all* folds.
;;
;; You might want to exit a fold *without* hiding the text and subheadings.
;; You do this by specifying a negative argument.  For example, M--2 C-c C-x
;; exits two folds and leaves the text and subheadings exposed.
;;
;; Foldout also provides mouse bindings for entering and exiting folds and for
;; showing and hiding text.  Hold down Meta and Control, then click a mouse
;; button as follows:-
;;
;;   mouse-1 (foldout-mouse-zoom) zooms in on the heading clicked on:-
;;
;;     single click	expose body
;;     double click	expose subheadings
;;     triple click	expose body and subheadings
;;     quad click	expose entire subtree
;;
;;   mouse-2 (foldout-mouse-show) exposes text under the heading clicked on:-
;;
;;     single click	expose body
;;     double click	expose subheadings
;;     triple click	expose body and subheadings
;;     quad click	expose entire subtree
;;
;;   mouse-3 (foldout-mouse-hide-or-exit) hides text under the heading clicked
;;   on or exits the fold:-
;;
;;     single click	hide subtree
;;     double click	exit fold and hide text
;;     triple click	exit fold without hiding text
;;     quad click	exit all folds and hide text
;;
;; You can change the modifier keys used by setting `foldout-mouse-modifiers'.

;;; Installation:

;; To use foldout, put this in your .emacs:-
;;
;;    (require 'foldout)
;;
;; If you don't want it loaded until you need it, try this instead:-
;;
;;    (eval-after-load "outline" '(require 'foldout))

;;; Advertisements:

;; Get out-xtra.el by Per Abrahamsen <abraham@iesd.auc.dk> for more
;; outline-mode goodies.  In particular, `outline-hide-sublevels' makes
;; setup a lot easier.
;;
;; folding.el by Jamie Lokier <u90jl@ecs.ox.ac.uk> supports folding by
;; recognizing special marker text in you file.
;;
;; c-outline.el (by me) provides outline-mode support to recognize `C'
;; statements as outline headings, so with foldout you can have a folding `C'
;; code editor without having to put in start- and end-of-fold markers.  This
;; is a real winner!

;;; ChangeLog:

;; 1.10   21-Mar-94
;; foldout.el is now part of the GNU Emacs distribution!!
;; Put in changes made by RMS to version 1.8 to keep the diffs to a minimum.
;; bugfix: numeric arg to foldout-exit-fold wasn't working - looks like I don't
;; know how to use the Common LISP `loop' macro after all, so use `while'
;; instead.

;; 1.9    15-Mar-94
;; Didn't test that very well, did I?  The change to foldout-zoom-subtree
;; affected foldout-mouse-zoom: if the heading under the `level n' one clicked
;; on was at `level n+2' then it didn't get exposed.  Sorry about that!

;; 1.8	  15-Mar-94
;; Changed meaning of prefix arg to foldout-zoom-subtree.  arg > 0 now means
;; "expose that many children" instead of just "expose children" so it is more
;; like `show-children' (C-c C-i).  Arg of C-u on its own only shows one level
;; of children, though, so you can still zoom by doing C-u C-c C-z.
;;
;; I can't think of a good meaning for the value of a negative prefix.  Any
;; suggestions?
;;
;; Added advertisement for my c-outline.el package.  Now you can have a folding
;; editor for c-mode without any effort!

;; 1.7	  7-Mar-94
;; I got fed up trying to work out how many blank lines there were outside the
;; narrowed region when inside a fold.  Now *all* newlines before the following
;; heading are *in* the narrowed region.  Thus, if the cursor is at point-max,
;; the number of blank lines above it is the number you'll get above the next
;; heading.
;;
;; Since all newlines are now inside the narrowed region, when exiting a fold
;; add a newline at the end of the region if there isn't one so that the
;; following heading doesn't accidentally get joined to the body text.
;;
;; Bugfix: `foldout-mouse-modifiers' should be `defvar', not `defconst'.
;;
;; Use "cond" instead of "case" so that lemacs-19.9 users can use the mouse.
;;
;; Improve "Commentary" entry on using the mouse.
;;
;; Add "Installation" keyword.

;; 1.6	  3-Mar-94
;; Add mouse support functions foldout-mouse-zoom, foldout-mouse-show,
;; foldout-mouse-hide-or-exit.

;; 1.5	  11-Feb-94
;; Rename `foldout-enter-subtree' to `foldout-zoom-subtree' and change
;; keystroke from C-g to C-z.  This is more mnemonic and leaves C-g alone, as
;; users expect this to cancel the current key sequence.
;;
;; Added better commentary at the request of RMS.  Added stuff to comply with
;; the lisp-mnt.el conventions.  Added instructions on how best to load the
;; package.

;; 1.4	  2-Feb-94
;; Bugfix: end-of-fold marking was wrong:-
;;
;;   End of narrowed region should be one character on from
;;   (outline-end-of-subtree) so it includes the end-of-line at the end of the
;;   last line of the subtree.
;;
;;   End-of-fold marker should be outside the narrowed region so text inserted
;;   at the end of the region goes before the marker.  Need to make a special
;;   case for end-of-buffer because it is impossible to set a marker that will
;;   follow eob.  Bummer.

;; 1.3	  28-Jan-94
;; Changed `foldout-zoom-subtree'.  A zero arg now makes it expose the entire
;; subtree on entering the fold.  As before, < 0 shows only the body and > 0
;; shows only the subheadings.

;; 1.2	  28-Jan-94
;; Fixed a dumb bug - didn't make `foldout-modeline-string' buffer-local :-(
;;
;; Changed `foldout-exit-fold' to use prefix arg to say how many folds to exit.
;; Negative arg means exit but don't hide text.  Zero arg means exit all folds.
;;
;; Added `foldout-inhibit-key-bindings' to inhibit key bindings.

;; 1.1	  27-Jan-94
;; Released to the net.  Inspired by a question in gnu.emacs.help from
;; Jason D Lohn <jlohn@eng.umd.edu>.

;;; Code:

(require 'outline)

;; something has gone very wrong if outline-minor-mode isn't bound now.
(if (not (boundp 'outline-minor-mode))
    (error "Can't find outline-minor-mode"))

(defvar foldout-fold-list nil
  "List of start and end markers for the folds currently entered.
An end marker of nil means the fold ends after (point-max).")
(make-variable-buffer-local 'foldout-fold-list)

(defvar foldout-modeline-string nil
  "Modeline string announcing that we are in an outline fold.")
(make-variable-buffer-local 'foldout-modeline-string)

;; put our minor mode string immediately following outline-minor-mode's
(or (assq 'foldout-modeline-string minor-mode-alist)
    (let ((outl-entry (memq (assq 'outline-minor-mode minor-mode-alist)
			    minor-mode-alist))
	  (foldout-entry '((foldout-modeline-string foldout-modeline-string))))

      ;; something's wrong with outline if we can't find it
      (if (null outl-entry)
	  (error "Can't find outline-minor-mode in minor-mode-alist"))

      ;; slip our fold announcement into the list
      (setcdr outl-entry (nconc foldout-entry (cdr outl-entry)))
      ))

;; outline-flag-region has different `flag' values in outline.el and
;; noutline.el for hiding and showing text.

(defconst foldout-hide-flag
  (if (featurep 'noutline) t ?\^M))

(defconst foldout-show-flag
  (if (featurep 'noutline) nil ?\n))


(defun foldout-zoom-subtree (&optional exposure)
  "Open the subtree under the current heading and narrow to it.

Normally the body and the immediate subheadings are exposed, but
optional arg EXPOSURE \(interactively with prefix arg\) changes this:-

	EXPOSURE > 0	exposes n levels of subheadings (c.f. show-children)
	EXPOSURE < 0	exposes only the body
	EXPOSURE = 0	exposes the entire subtree"
  (interactive "P")
  (save-excursion
    (widen)
    (outline-back-to-heading)
    (let* ((exposure-value (prefix-numeric-value exposure))
	   (start (point))
	   (start-marker (point-marker))
	   (end (progn (outline-end-of-subtree)
		       (skip-chars-forward "\n\^M")
		       (point)))
	   ;; I need a marker that will follow the end of the region even when
	   ;; text is inserted right at the end.  Text gets inserted *after*
	   ;; markers, so I need it at end+1.  Unfortunately I can't set a
	   ;; marker at (point-max)+1, so I use nil to mean the region ends at
	   ;; (point-max).
	   (end-marker (if (eobp) nil (set-marker (make-marker) (1+ end))))
	   )

      ;; narrow to this subtree
      (narrow-to-region start end)

      ;; show the body and/or subheadings for this heading
      (goto-char start)
      (cond
       ((null exposure)
	(show-entry)
	(show-children))
       ((< exposure-value 0)
	(show-entry))
       ((consp exposure)
	(show-children))
       ((> exposure-value 0)
	(show-children exposure-value))
       (t
	(show-subtree))
       )

      ;; save the location of the fold we are entering
      (setq foldout-fold-list (cons (cons start-marker end-marker)
				    foldout-fold-list))

      ;; update the modeline
      (foldout-update-modeline)
      )))


(defun foldout-exit-fold (&optional num-folds)
  "Return to the ARG'th enclosing fold view.  With ARG = 0 exit all folds.

Normally causes exited folds to be hidden, but with ARG < 0, -ARG folds are
exited and text is left visible."
  (interactive "p")
  (let ((hide-fold t) start-marker end-marker
	beginning-of-heading end-of-subtree)

    ;; check there are some folds to leave
    (if (null foldout-fold-list)
	(error "Not in a fold!"))

    (cond
     ;; catch a request to leave all folds
     ((zerop num-folds)
      (setq num-folds (length foldout-fold-list)))

     ;; have we been told not to hide the fold?
     ((< num-folds 0)
      (setq hide-fold nil
	    num-folds (- num-folds)))
     )

    ;; limit the number of folds if we've been told to exit too many
    (setq num-folds (min num-folds (length foldout-fold-list)))

    ;; exit the folds
    (widen)
    (while (not (zerop num-folds))
      ;; get the fold at the top of the stack
      (setq start-marker (car (car foldout-fold-list))
	    end-marker (cdr (car foldout-fold-list))
	    foldout-fold-list (cdr foldout-fold-list)
	    num-folds (1- num-folds))

      ;; Make sure there is a newline at the end of this fold,
      ;; otherwise the following heading will get joined to the body
      ;; text.
      (if end-marker
	  (progn
	    (goto-char end-marker)
	    (forward-char -1)
	    (or (memq (preceding-char) '(?\n ?\^M))
		(insert ?\n))))

      ;; If this is the last fold to exit, hide the text unless we've
      ;; been told not to.  Note that at the moment point is at the
      ;; beginning of the following heading if there is one.

      ;; Also, make sure that the newline before the following heading
      ;; is \n otherwise it will be hidden.  If there is a newline
      ;; before this one, make it visible too so we do the same as
      ;; outline.el and leave a blank line before the heading.
      (when (zerop num-folds)
	(if end-marker
	    (setq beginning-of-heading (point)
		  end-of-subtree (progn (forward-char -1)
					(if (memq (preceding-char)
						  '(?\n ?\^M))
					    (forward-char -1))
					(point))))
	;; hide the subtree
	(when hide-fold
	  (goto-char start-marker)
	  (hide-subtree))

	;; make sure the next heading is exposed
	(if end-marker
	    (outline-flag-region end-of-subtree beginning-of-heading
				 foldout-show-flag)))

      ;; zap the markers so they don't slow down editing
      (set-marker start-marker nil)
      (if end-marker (set-marker end-marker nil))
      )

    ;; narrow to the enclosing fold if there is one
    (if foldout-fold-list
	(progn
	  (setq start-marker (car (car foldout-fold-list))
		end-marker (cdr (car foldout-fold-list)))
	  (narrow-to-region start-marker
			    (if end-marker
				(1- (marker-position end-marker))
			      (point-max)))
	  ))
    (recenter)

    ;; update the modeline
    (foldout-update-modeline)
    ))


(defun foldout-update-modeline ()
  "Set the modeline string to indicate our fold depth."
  (let ((depth (length foldout-fold-list)))
    (setq foldout-modeline-string
	  (cond
	   ;; if we're not in a fold, keep quiet
	   ((zerop depth)
	    nil)
	   ;; in outline-minor-mode we're after "Outl:xx" in the modeline
	   (outline-minor-mode
	    (format ":%d" depth))
	   ;; otherwise just announce the depth (I guess we're in outline-mode)
	   ((= depth 1)
	    " Inside 1 fold")
	   (t
	    (format " Inside %d folds" depth))
	   ))))


(defun foldout-mouse-zoom (event)
  "Zoom in on the heading clicked on.

How much is exposed by the zoom depends on the number of mouse clicks:-

	1	expose body
	2	expose subheadings
	3	expose body and subheadings
	4	expose entire subtree"
  (interactive "@e")

  ;; swallow intervening mouse events so we only get the final click-count.
  (setq event (foldout-mouse-swallow-events event))

  ;; go to the heading clicked on
  (foldout-mouse-goto-heading event)

  ;; zoom away
  (foldout-zoom-subtree
   (let ((nclicks (event-click-count event)))
     (cond
      ((= nclicks 1) -1)		; body only
      ((= nclicks 2) '(1))		; subheadings only
      ((= nclicks 3) nil)		; body and subheadings
      (t 0)))))				; entire subtree

(defun foldout-mouse-show (event)
  "Show what is hidden under the heading clicked on.

What gets exposed depends on the number of mouse clicks:-

	1	expose body
	2	expose subheadings
	3	expose body and subheadings
	4	expose entire subtree"
  (interactive "@e")

  ;; swallow intervening mouse events so we only get the final click-count.
  (setq event (foldout-mouse-swallow-events event))

  ;; expose the text
  (foldout-mouse-goto-heading event)
  (let ((nclicks (event-click-count event)))
    (cond
     ((= nclicks 1) (show-entry))
     ((= nclicks 2) (show-children))
     ((= nclicks 3) (show-entry) (show-children))
     (t (show-subtree)))))

(defun foldout-mouse-hide-or-exit (event)
  "Hide the subtree under the heading clicked on, or exit a fold.

What happens depends on the number of mouse clicks:-

	1	hide subtree
	2	exit fold and hide text
	3	exit fold without hiding text
	4	exit all folds and hide text"
  (interactive "@e")

  ;; swallow intervening mouse events so we only get the final click-count.
  (setq event (foldout-mouse-swallow-events event))

  ;; hide or exit
  (let ((nclicks (event-click-count event)))
    (if (= nclicks 1)
	(progn
	  (foldout-mouse-goto-heading event)
	  (hide-subtree))
      (foldout-exit-fold
       (cond
	((= nclicks 2) 1)		; exit and hide
	((= nclicks 3) -1)		; exit don't hide
	(t 0))))))			; exit all


(defun foldout-mouse-swallow-events (event)
  "Swallow intervening mouse events so we only get the final click-count.
Signal an error if the final event isn't the same type as the first one."
  (let ((initial-event-type (event-basic-type event)))
    (while (null (sit-for (/ double-click-time 1000.0) 'nodisplay))
      (setq event (read-event)))
    (or (eq initial-event-type (event-basic-type event))
	(error "")))
  event)

(defun foldout-mouse-goto-heading (event)
  "Go to the heading where the mouse event started.  Signal an error
if the event didn't occur on a heading."
  (goto-char (posn-point (event-start event)))
  (or (outline-on-heading-p)
      ;; outline.el sometimes treats beginning-of-buffer as a heading
      ;; even though outline-on-heading returns nil.
      (save-excursion (beginning-of-line) (bobp))
      (error "Not a heading line")))


;;; Keymaps:

(defvar foldout-inhibit-key-bindings nil
  "Set non-nil before loading foldout to inhibit key bindings.")

(defvar foldout-mouse-modifiers '(meta control)
  "List of modifier keys to apply to foldout's mouse events.

The default (meta control) makes foldout bind its functions to
M-C-down-mouse-{1,2,3}.

Valid modifiers are shift, control, meta, alt, hyper and super.")

(if foldout-inhibit-key-bindings
    ()
  (define-key outline-mode-map "\C-c\C-z" 'foldout-zoom-subtree)
  (define-key outline-mode-map "\C-c\C-x" 'foldout-exit-fold)
  (let ((map (lookup-key outline-minor-mode-map outline-minor-mode-prefix)))
    (unless map
      (setq map (make-sparse-keymap))
      (define-key outline-minor-mode-map outline-minor-mode-prefix map))
    (define-key map "\C-z" 'foldout-zoom-subtree)
    (define-key map "\C-x" 'foldout-exit-fold))
  (let* ((modifiers (apply 'concat
			   (mapcar (function
				    (lambda (modifier)
				      (vector
				       (cond
					 ((eq modifier 'shift) ?S)
					 ((eq modifier 'control) ?C)
					 ((eq modifier 'meta) ?M)
					 ((eq modifier 'alt) ?A)
					 ((eq modifier 'hyper) ?H)
					 ((eq modifier 'super) ?s)
					 (t (error "invalid mouse modifier %s"
						   modifier)))
				       ?-)))
				   foldout-mouse-modifiers)))
	 (mouse-1 (vector (intern (concat modifiers "down-mouse-1"))))
	 (mouse-2 (vector (intern (concat modifiers "down-mouse-2"))))
	 (mouse-3 (vector (intern (concat modifiers "down-mouse-3")))))

    (define-key outline-mode-map mouse-1 'foldout-mouse-zoom)
    (define-key outline-mode-map mouse-2 'foldout-mouse-show)
    (define-key outline-mode-map mouse-3 'foldout-mouse-hide-or-exit)

    (define-key outline-minor-mode-map mouse-1 'foldout-mouse-zoom)
    (define-key outline-minor-mode-map mouse-2 'foldout-mouse-show)
    (define-key outline-minor-mode-map mouse-3 'foldout-mouse-hide-or-exit)
    ))

(provide 'foldout)

;;; foldout.el ends here
