;;; mouse-sel.el --- multi-click selection support

;; Copyright (C) 1993-1995, 2001-2012  Free Software Foundation, Inc.

;; Author: Mike Williams <mdub@bigfoot.com>
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

;; This module provides multi-click mouse support for GNU Emacs versions
;; 19.18 and later.  I've tried to make it behave more like standard X
;; clients (eg. xterm) than the default Emacs 19 mouse selection handlers.
;; Basically:
;;
;;   * Clicking mouse-1 starts (cancels) selection, dragging extends it.
;;
;;   * Clicking or dragging mouse-3 extends the selection as well.
;;
;;   * Double-clicking on word constituents selects words.
;;     Double-clicking on symbol constituents selects symbols.
;;     Double-clicking on quotes or parentheses selects sexps.
;;     Double-clicking on whitespace selects whitespace.
;;     Triple-clicking selects lines.
;;     Quad-clicking selects paragraphs.
;;
;;   * Selecting sets the region & X primary selection, but does NOT affect
;;     the kill-ring.  Because the mouse handlers set the primary selection
;;     directly, mouse-sel sets the variables interprogram-cut-function
;;     and interprogram-paste-function to nil.
;;
;;   * Clicking mouse-2 inserts the contents of the primary selection at
;;     the mouse position (or point, if mouse-yank-at-point is non-nil).
;;
;;   * Pressing mouse-2 while selecting or extending copies selection
;;     to the kill ring.  Pressing mouse-1 or mouse-3 kills it.
;;
;;   * Double-clicking mouse-3 also kills selection.
;;
;;   * M-mouse-1, M-mouse-2 & M-mouse-3 work similarly to mouse-1, mouse-2
;;     & mouse-3, but operate on the X secondary selection rather than the
;;     primary selection and region.
;;
;; This module requires my thingatpt.el module, which it uses to find the
;; bounds of words, lines, sexps, etc.
;;
;; Thanks to KevinB@bartley.demon.co.uk for his useful input.
;;
;;--- Customization -------------------------------------------------------
;;
;; * You may want to use none or more of following:
;;
;;      ;; Enable region highlight
;;      (transient-mark-mode 1)
;;
;;      ;; But only in the selected window
;;      (setq highlight-nonselected-windows nil)
;;
;;      ;; Enable pending-delete
;;      (delete-selection-mode 1)
;;
;; * You can control the way mouse-sel binds its keys by setting the value
;;   of mouse-sel-default-bindings before loading mouse-sel.
;;
;;   (a) If mouse-sel-default-bindings = t (the default)
;;
;;       Mouse sets and insert selection
;;	   mouse-1		mouse-select
;;	   mouse-2		mouse-insert-selection
;; 	   mouse-3		mouse-extend
;;
;;       Selection/kill-ring interaction is disabled
;;         interprogram-cut-function   = nil
;;         interprogram-paste-function = nil
;;
;;   (b) If mouse-sel-default-bindings = 'interprogram-cut-paste
;;
;;       Mouse sets selection, and pastes from kill-ring
;; 	   mouse-1		mouse-select
;; 	   mouse-2		mouse-insert-selection
;; 	   mouse-3		mouse-extend
;; 	 In this mode, mouse-insert-selection just calls mouse-yank-at-click.
;;
;;       Selection/kill-ring interaction is retained
;;         interprogram-cut-function   = x-select-text
;;         interprogram-paste-function = x-selection-value
;;
;;       What you lose is the ability to select some text in
;;       delete-selection-mode and yank over the top of it.
;;
;;   (c) If mouse-sel-default-bindings = nil, no bindings are made.
;;
;; * By default, mouse-insert-selection (mouse-2) inserts the selection at
;;   the mouse position.  You can tell it to insert at point instead with:
;;
;;     (setq mouse-yank-at-point t)
;;
;; * I like to leave point at the end of the region nearest to where the
;;   mouse was, even though this makes region highlighting mis-leading (the
;;   cursor makes it look like one extra character is selected).  You can
;;   disable this behavior with:
;;
;;     (setq mouse-sel-leave-point-near-mouse nil)
;;
;; * By default, mouse-select cycles the click count after 4 clicks.  That
;;   is, clicking mouse-1 five times has the same effect as clicking it
;;   once, clicking six times has the same effect as clicking twice, etc.
;;   Disable this behavior with:
;;
;;     (setq mouse-sel-cycle-clicks nil)
;;
;; * The variables mouse-sel-{set,get}-selection-function control how the
;;   selection is handled.  Under X Windows, these variables default so
;;   that the X primary selection is used.  Under other windowing systems,
;;   alternate functions are used, which simply store the selection value
;;   in a variable.

;;; Code:

(require 'mouse)
(require 'thingatpt)

(eval-when-compile
  (require 'cl))

;;=== User Variables ======================================================

(defgroup mouse-sel nil
  "Mouse selection enhancement."
  :group 'mouse)

(defcustom mouse-sel-leave-point-near-mouse t
  "Leave point near last mouse position.
If non-nil, \\[mouse-select] and \\[mouse-extend] will leave point at the end
of the region nearest to where the mouse last was.
If nil, point will always be placed at the beginning of the region."
  :type 'boolean
  :group 'mouse-sel)

(defcustom mouse-sel-cycle-clicks t
  "If non-nil, \\[mouse-select] cycles the click-counts after 4 clicks."
  :type 'boolean
  :group 'mouse-sel)

(defcustom mouse-sel-default-bindings t
  "Control mouse bindings."
  :type '(choice (const :tag "none" nil)
		 (const :tag "cut and paste" interprogram-cut-paste)
		 (other :tag "default bindings" t))
  :group 'mouse-sel)

;;=== Key bindings ========================================================

(defconst mouse-sel-bound-events
  '(;; Primary selection bindings.
    ;;
    ;; Bind keys to `ignore' instead of unsetting them because modes may
    ;; bind `down-mouse-1', for instance, without binding `mouse-1'.
    ;; If we unset `mouse-1', this leads to a bitch_at_user when the
    ;; mouse goes up because no matching binding is found for that.
    ([mouse-1]		. ignore)
    ([drag-mouse-1]	. ignore)
    ([mouse-3]		. ignore)
    ([down-mouse-1]	. mouse-select)
    ([down-mouse-3]	. mouse-extend)
    ([mouse-2]		. mouse-insert-selection)
    ;; Secondary selection bindings.
    ([M-mouse-1]	. ignore)
    ([M-drag-mouse-1]	. ignore)
    ([M-mouse-3]	. ignore)
    ([M-down-mouse-1]	. mouse-select-secondary)
    ([M-mouse-2]	. mouse-insert-secondary)
    ([M-down-mouse-3]	. mouse-extend-secondary))
  "An alist of events that `mouse-sel-mode' binds.")

;;=== User Command ========================================================

(defvar mouse-sel-has-been-enabled nil
  "Non-nil if Mouse Sel mode has been enabled at least once.")

(defvar mouse-sel-original-bindings nil)
(defvar mouse-sel-original-interprogram-cut-function nil)
(defvar mouse-sel-original-interprogram-paste-function nil)

;;;###autoload
(define-minor-mode mouse-sel-mode
  "Toggle Mouse Sel mode.
With a prefix argument ARG, enable Mouse Sel mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Mouse Sel mode is a global minor mode.  When enabled, mouse
selection is enhanced in various ways:

- Double-clicking on symbol constituents selects symbols.
Double-clicking on quotes or parentheses selects sexps.
Double-clicking on whitespace selects whitespace.
Triple-clicking selects lines.
Quad-clicking selects paragraphs.

- Selecting sets the region & X primary selection, but does NOT affect
the `kill-ring', nor do the kill-ring functions change the X selection.
Because the mouse handlers set the primary selection directly,
mouse-sel sets the variables `interprogram-cut-function' and
`interprogram-paste-function' to nil.

- Clicking mouse-2 inserts the contents of the primary selection at
the mouse position (or point, if `mouse-yank-at-point' is non-nil).

- mouse-2 while selecting or extending copies selection to the
kill ring; mouse-1 or mouse-3 kills it."
  :global t
  :group 'mouse-sel
  (if mouse-sel-mode
      (progn
	;; If mouse-2 has never been done by the user, initialize the
	;; `event-kind' property to ensure that `follow-link' clicks
	;; are interpreted correctly.
	(put 'mouse-2 'event-kind 'mouse-click)
	(add-hook 'x-lost-selection-functions 'mouse-sel-lost-selection-hook)
	(when mouse-sel-default-bindings
	  ;; Save original bindings and replace them with new ones.
	  (setq mouse-sel-original-bindings
		(mapcar (lambda (binding)
			  (let ((event (car binding)))
			    (prog1 (cons event (lookup-key global-map event))
			      (global-set-key event (cdr binding)))))
			mouse-sel-bound-events))
	  ;; Update interprogram functions.
	  (setq mouse-sel-original-interprogram-cut-function
		interprogram-cut-function
		mouse-sel-original-interprogram-paste-function
		interprogram-paste-function
		mouse-sel-has-been-enabled t)
	  (unless (eq mouse-sel-default-bindings 'interprogram-cut-paste)
	    (setq interprogram-cut-function nil
		  interprogram-paste-function nil))))

    ;; Restore original bindings
    (remove-hook 'x-lost-selection-functions 'mouse-sel-lost-selection-hook)
    (dolist (binding mouse-sel-original-bindings)
      (global-set-key (car binding) (cdr binding)))
    ;; Restore the old values of these variables,
    ;; only if they were actually saved previously.
    (if mouse-sel-has-been-enabled
	(setq interprogram-cut-function
	      mouse-sel-original-interprogram-cut-function
	      interprogram-paste-function
	      mouse-sel-original-interprogram-paste-function))))

;;=== Internal Variables/Constants ========================================

(defvar mouse-sel-primary-thing nil
  "Type of PRIMARY selection in current buffer.")
(make-variable-buffer-local 'mouse-sel-primary-thing)

(defvar mouse-sel-secondary-thing nil
  "Type of SECONDARY selection in current buffer.")
(make-variable-buffer-local 'mouse-sel-secondary-thing)

;; Ensure that secondary overlay is defined
(unless (overlayp mouse-secondary-overlay)
  (setq mouse-secondary-overlay (make-overlay 1 1))
  (overlay-put mouse-secondary-overlay 'face 'secondary-selection))

(defconst mouse-sel-primary-overlay
  (let ((ol (make-overlay (point-min) (point-min))))
    (delete-overlay ol)
    (overlay-put ol 'face 'region)
    ol)
  "An overlay which records the current primary selection.
This is used by Mouse Sel mode only.")

(defconst mouse-sel-selection-alist
  '((PRIMARY mouse-sel-primary-overlay mouse-sel-primary-thing)
    (SECONDARY mouse-secondary-overlay mouse-sel-secondary-thing))
  "Alist associating selections with variables.
Each element is of the form:

   (SELECTION-NAME OVERLAY-SYMBOL SELECTION-THING-SYMBOL)

where   SELECTION-NAME          = name of selection
        OVERLAY-SYMBOL          = name of variable containing overlay to use
	SELECTION-THING-SYMBOL 	= name of variable where the current selection
 				  type for this selection should be stored.")

(declare-function x-select-text "term/common-win" (text))

(defvar mouse-sel-set-selection-function
  (if (eq mouse-sel-default-bindings 'interprogram-cut-paste)
      'x-set-selection
    (lambda (selection value)
      (if (eq selection 'PRIMARY)
	  (x-select-text value)
	(x-set-selection selection value))))
  "Function to call to set selection.
Called with two arguments:

  SELECTION, the name of the selection concerned, and
  VALUE, the text to store.

This sets the selection, unless `mouse-sel-default-bindings'
is `interprogram-cut-paste'.")

(declare-function x-selection-value "term/x-win" ())

(defvar mouse-sel-get-selection-function
  (lambda (selection)
    (if (eq selection 'PRIMARY)
	(or (x-selection-value)
	    (bound-and-true-p x-last-selected-text)
	    (bound-and-true-p x-last-selected-text-primary))
      (x-get-selection selection)))
  "Function to call to get the selection.
Called with one argument:

   SELECTION: the name of the selection concerned.")

;;=== Support/access functions ============================================

(defun mouse-sel-determine-selection-thing (nclicks)
  "Determine what `thing' `mouse-sel' should operate on.
The first argument is NCLICKS, is the number of consecutive
mouse clicks at the same position.

Double-clicking on word constituents selects words.
Double-clicking on symbol constituents selects symbols.
Double-clicking on quotes or parentheses selects sexps.
Double-clicking on whitespace selects whitespace.
Triple-clicking selects lines.
Quad-clicking selects paragraphs.

Feel free to re-define this function to support your own desired
multi-click semantics."
  (let* ((next-char (char-after (point)))
	 (char-syntax (if next-char (char-syntax next-char))))
    (if mouse-sel-cycle-clicks
	(setq nclicks (1+ (% (1- nclicks) 4))))
    (cond
     ((= nclicks 1) nil)
     ((= nclicks 3) 'line)
     ((>= nclicks 4) 'paragraph)
     ((memq char-syntax '(?\( ?\) ?\" ?')) 'sexp)
     ((memq next-char '(?\s ?\t ?\n)) 'whitespace)
     ((eq char-syntax ?_) 'symbol)
     ((eq char-syntax ?w) 'word))))

(defun mouse-sel-set-selection (selection value)
  "Set the specified SELECTION to VALUE."
  (if mouse-sel-set-selection-function
      (funcall mouse-sel-set-selection-function selection value)
    (put 'mouse-sel-internal-selection selection value)))

(defun mouse-sel-get-selection (selection)
  "Get the value of the specified SELECTION."
  (if mouse-sel-get-selection-function
      (funcall mouse-sel-get-selection-function selection)
    (get 'mouse-sel-internal-selection selection)))

(defun mouse-sel-selection-overlay (selection)
  "Return overlay corresponding to SELECTION."
  (let ((symbol (nth 1 (assoc selection mouse-sel-selection-alist))))
    (or symbol (error "No overlay corresponding to %s selection" selection))
    (symbol-value symbol)))

(defun mouse-sel-selection-thing (selection)
  "Return overlay corresponding to SELECTION."
  (let ((symbol (nth 2 (assoc selection mouse-sel-selection-alist))))
    (or symbol (error "No symbol corresponding to %s selection" selection))
    symbol))

(defun mouse-sel-region-to-primary (orig-window)
  "Convert region to PRIMARY overlay and deactivate region.
Argument ORIG-WINDOW specifies the window the cursor was in when the
originating command was issued, and is used to determine whether the
region was visible or not."
  (if transient-mark-mode
      (let ((overlay (mouse-sel-selection-overlay 'PRIMARY)))
	(cond
	 ((and mark-active
	       (or highlight-nonselected-windows
		   (eq orig-window (selected-window))))
	  ;; Region was visible, so convert region to overlay
	  (move-overlay overlay (region-beginning) (region-end)
			(current-buffer)))
	 ((eq orig-window (selected-window))
	  ;; Point was visible, so set overlay at point
	  (move-overlay overlay (point) (point) (current-buffer)))
	 (t
	  ;; Nothing was visible, so remove overlay
	  (delete-overlay overlay)))
	(setq mark-active nil))))

(defun mouse-sel-primary-to-region (&optional direction)
  "Convert PRIMARY overlay to region.
Optional argument DIRECTION specifies the mouse drag direction: a value of
1 indicates that the mouse was dragged left-to-right, otherwise it was
dragged right-to-left."
  (let* ((overlay (mouse-sel-selection-overlay 'PRIMARY))
	 (start (overlay-start overlay))
	 (end (overlay-end overlay)))
    (if (eq start end)
	(progn
	  (if start (goto-char start))
	  (deactivate-mark))
      (if (and mouse-sel-leave-point-near-mouse (eq direction 1))
	  (progn
	    (goto-char end)
	    (push-mark start 'nomsg 'active))
	(goto-char start)
	(push-mark end 'nomsg 'active)))
    (if transient-mark-mode (delete-overlay overlay))))

(defmacro mouse-sel-eval-at-event-end (event &rest forms)
  "Evaluate forms at mouse position.
Move to the end position of EVENT, execute FORMS, and restore original
point and window."
  `(let ((posn (event-end ,event)))
    (if posn (mouse-minibuffer-check ,event))
    (if (and posn (not (windowp (posn-window posn))))
        (error "Cursor not in text area of window"))
    (let (orig-window orig-point-marker)
      (setq orig-window (selected-window))
      (if posn (select-window (posn-window posn)))
      (setq orig-point-marker (point-marker))
      (if (and posn (numberp (posn-point posn)))
          (goto-char (posn-point posn)))
      (unwind-protect
           (progn
             ,@forms)
        (goto-char (marker-position orig-point-marker))
        (move-marker orig-point-marker nil)
        (select-window orig-window)))))

(put 'mouse-sel-eval-at-event-end 'lisp-indent-hook 1)

;;=== Select ==============================================================

(defun mouse-select (event)
  "Set region/selection using the mouse.

Click sets point & mark to click position.
Dragging extends region/selection.

Multi-clicking selects word/lines/paragraphs, as determined by
'mouse-sel-determine-selection-thing.

Clicking mouse-2 while selecting copies selected text to the kill-ring.
Clicking mouse-1 or mouse-3 kills the selected text.

This should be bound to a down-mouse event."
  (interactive "@e")
  (let (select)
    (unwind-protect
    	(setq select (mouse-select-internal 'PRIMARY event))
      (if (and select (listp select))
	  (push (cons 'mouse-2 (cdr event)) unread-command-events)
	(mouse-sel-primary-to-region select)))))

(defun mouse-select-secondary (event)
  "Set secondary selection using the mouse.

Click sets the start of the secondary selection to click position.
Dragging extends the secondary selection.

Multi-clicking selects word/lines/paragraphs, as determined by
'mouse-sel-determine-selection-thing.

Clicking mouse-2 while selecting copies selected text to the kill-ring.
Clicking mouse-1 or mouse-3 kills the selected text.

This should be bound to a down-mouse event."
  (interactive "e")
  (mouse-select-internal 'SECONDARY event))

(defun mouse-select-internal (selection event)
  "Set SELECTION using the mouse, with EVENT as the initial down-event.
Normally, this returns the direction in which the selection was
made: a value of 1 indicates that the mouse was dragged
left-to-right, otherwise it was dragged right-to-left.

However, if `mouse-1-click-follows-link' is non-nil and the
subsequent mouse events specify following a link, this returns
the final mouse-event.  In that case, the selection is not set."
  (mouse-sel-eval-at-event-end event
    (let ((thing-symbol (mouse-sel-selection-thing selection))
	  (overlay (mouse-sel-selection-overlay selection)))
      (set thing-symbol
	   (mouse-sel-determine-selection-thing (event-click-count event)))
      (let ((object-bounds (bounds-of-thing-at-point
			    (symbol-value thing-symbol))))
	(if object-bounds
	    (progn
	      (move-overlay overlay
			    (car object-bounds) (cdr object-bounds)
			    (current-buffer)))
	  (move-overlay overlay (point) (point) (current-buffer)))))
    (catch 'follow-link
      (mouse-extend-internal selection event t))))

;;=== Extend ==============================================================

(defun mouse-extend (event)
  "Extend region/selection using the mouse."
  (interactive "e")
  (let ((orig-window (selected-window))
	direction)
    (select-window (posn-window (event-end event)))
    (unwind-protect
	(progn
	  (mouse-sel-region-to-primary orig-window)
	  (setq direction (mouse-extend-internal 'PRIMARY event)))
      (mouse-sel-primary-to-region direction))))

(defun mouse-extend-secondary (event)
  "Extend secondary selection using the mouse."
  (interactive "e")
  (save-window-excursion
    (mouse-extend-internal 'SECONDARY event)))

(defun mouse-extend-internal (selection &optional initial-event no-process)
  "Extend specified SELECTION using the mouse.
Track mouse-motion events, adjusting the SELECTION appropriately.
Optional argument INITIAL-EVENT specifies an initial down-mouse event.
Optional argument NO-PROCESS means not to process the initial
event.

See documentation for mouse-select-internal for more details."
  (mouse-sel-eval-at-event-end initial-event
    (let ((orig-cursor-type
	   (cdr (assoc 'cursor-type (frame-parameters (selected-frame))))))
      (unwind-protect

	  (let* ((thing-symbol (mouse-sel-selection-thing selection))
		 (overlay (mouse-sel-selection-overlay selection))
		 (orig-window (selected-window))
		 (top (nth 1 (window-edges orig-window)))
		 (bottom (nth 3 (window-edges orig-window)))
		 (mark-active nil)	; inhibit normal region highlight
		 (echo-keystrokes 0)	; don't echo mouse events
		 min max
		 direction
		 event)

	    ;; Get current bounds of overlay
	    (if (eq (overlay-buffer overlay) (current-buffer))
		(setq min (overlay-start overlay)
		      max (overlay-end overlay))
	      (setq min (point)
		    max min)
	      (set thing-symbol nil))


	    ;; Bar cursor
	    (if (fboundp 'modify-frame-parameters)
		(modify-frame-parameters (selected-frame)
					 '((cursor-type . bar))))

	    ;; Handle dragging
	    (track-mouse

	      (while (if (and initial-event (not no-process))
			 ;; Use initial event
			 (prog1
			     (setq event initial-event)
			   (setq initial-event nil))
		       (setq event (read-event))
		       (and (consp event)
			    (memq (car event) '(mouse-movement switch-frame))))

		(let ((selection-thing (symbol-value thing-symbol))
		      (end (event-end event)))

		  (cond

		   ;; Ignore any movement outside the frame
		   ((eq (car-safe event) 'switch-frame) nil)
		   ((and (posn-window end)
			 (not (eq (let ((posn-w (posn-window end)))
				    (if (windowp posn-w)
					(window-frame posn-w)
				      posn-w))
				  (window-frame orig-window)))) nil)

		   ;; Different window, same frame
		   ((not (eq (posn-window end) orig-window))
		    (let ((end-row (cdr (cdr (mouse-position)))))
		      (cond
		       ((and end-row (not (bobp)) (< end-row top))
			(mouse-scroll-subr orig-window (- end-row top)
					   overlay max))
		       ((and end-row (not (eobp)) (>= end-row bottom))
			(mouse-scroll-subr orig-window (1+ (- end-row bottom))
					   overlay min))
		       )))

		   ;; On the mode line
		   ((eq (posn-point end) 'mode-line)
		    (mouse-scroll-subr orig-window 1 overlay min))

		   ;; In original window
		   (t (goto-char (posn-point end)))

		   )

		  ;; Determine direction of drag
		  (cond
		   ((and (not direction) (not (eq min max)))
		    (setq direction (if (< (point) (/ (+ min max) 2)) -1 1)))
		   ((and (not (eq direction -1)) (<= (point) min))
		    (setq direction -1))
		   ((and (not (eq direction 1)) (>= (point) max))
		    (setq direction 1)))

		  (if (not selection-thing) nil

		    ;; If dragging forward, goal is next character
		    (if (and (eq direction 1) (not (eobp))) (forward-char 1))

		    ;; Move to start/end of selected thing
		    (let ((goal (point)))
		      (goto-char (if (eq 1 direction) min max))
		      (condition-case nil
			  (progn
			    (while (> (* direction (- goal (point))) 0)
			      (forward-thing selection-thing direction))
			    (let ((end (point)))
			      (forward-thing selection-thing (- direction))
			      (goto-char
			       (if (> (* direction (- goal (point))) 0)
				   end (point)))))
			(error))))

		  ;; Move overlay
		  (move-overlay overlay
				(if (eq 1 direction) min (point))
				(if (eq -1 direction) max (point))
				(current-buffer))

		  )))			; end track-mouse

	    ;; Detect follow-link events
	    (when (mouse-sel-follow-link-p initial-event event)
	      (throw 'follow-link event))

	    ;; Finish up after dragging
	    (let ((overlay-start (overlay-start overlay))
		  (overlay-end (overlay-end overlay)))

	      ;; Set selection
	      (if (not (eq overlay-start overlay-end))
		  (mouse-sel-set-selection
		   selection
		   (buffer-substring overlay-start overlay-end)))

	      ;; Handle copy/kill
	      (let (this-command)
		(cond
		 ((eq (event-basic-type last-input-event) 'mouse-2)
		  (copy-region-as-kill overlay-start overlay-end)
		  (read-event) (read-event))
		 ((and (memq (event-basic-type last-input-event)
			     '(mouse-1 mouse-3))
		       (memq 'down (event-modifiers last-input-event)))
		  (kill-region overlay-start overlay-end)
		  (move-overlay overlay overlay-start overlay-start)
		  (read-event) (read-event))
		 ((and (eq (event-basic-type last-input-event) 'mouse-3)
		       (memq 'double (event-modifiers last-input-event)))
		  (kill-region overlay-start overlay-end)
		  (move-overlay overlay overlay-start overlay-start)))))

	    direction)

	;; Restore cursor
	(if (fboundp 'modify-frame-parameters)
	    (modify-frame-parameters
	     (selected-frame) (list (cons 'cursor-type orig-cursor-type))))

	))))

(defun mouse-sel-follow-link-p (initial final)
  "Return t if we should follow a link, given INITIAL and FINAL mouse events.
See `mouse-1-click-follows-link' for details.  Currently, Mouse
Sel mode does not support using a `double' value to follow links
using double-clicks."
  (and initial final mouse-1-click-follows-link
       (eq (car initial) 'down-mouse-1)
       (mouse-on-link-p (event-start initial))
       (= (posn-point (event-start initial))
	  (posn-point (event-end final)))
       (= (event-click-count initial) 1)
       (or (not (integerp mouse-1-click-follows-link))
	   (let ((t0 (posn-timestamp (event-start initial)))
		 (t1 (posn-timestamp (event-end final))))
	     (and (integerp t0) (integerp t1)
		  (if (> mouse-1-click-follows-link 0)
		      (<= (- t1 t0) mouse-1-click-follows-link)
		    (< (- t0 t1) mouse-1-click-follows-link)))))))

;;=== Paste ===============================================================

(defun mouse-insert-selection (event arg)
  "Insert the contents of the PRIMARY selection at mouse click.
If `mouse-yank-at-point' is non-nil, insert at point instead."
  (interactive "e\nP")
  (if (eq mouse-sel-default-bindings 'interprogram-cut-paste)
      (mouse-yank-at-click event arg)
    (mouse-insert-selection-internal 'PRIMARY event)))

(defun mouse-insert-secondary (event)
  "Insert the contents of the SECONDARY selection at mouse click.
If `mouse-yank-at-point' is non-nil, insert at point instead."
  (interactive "e")
  (mouse-insert-selection-internal 'SECONDARY event))

(defun mouse-insert-selection-internal (selection event)
  "Insert the contents of the named SELECTION at mouse click.
If `mouse-yank-at-point' is non-nil, insert at point instead."
  (unless mouse-yank-at-point
    (mouse-set-point event))
  (when mouse-sel-get-selection-function
    (push-mark (point) 'nomsg)
  (insert-for-yank
   (or (funcall mouse-sel-get-selection-function selection) ""))))

;;=== Handle loss of selections ===========================================

(defun mouse-sel-lost-selection-hook (selection)
  "Remove the overlay for a lost selection."
  (let ((overlay (mouse-sel-selection-overlay selection)))
    (delete-overlay overlay)))

(provide 'mouse-sel)

;;; mouse-sel.el ends here
