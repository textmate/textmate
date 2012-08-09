;;; viper-mous.el --- mouse support for Viper

;; Copyright (C) 1994-1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: viper

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

(provide 'viper-mous)

;; compiler pacifier
(defvar double-click-time)
(defvar mouse-track-multi-click-time)
(defvar viper-search-start-marker)
(defvar viper-local-search-start-marker)
(defvar viper-search-history)
(defvar viper-s-string)
(defvar viper-re-search)

;; loading happens only in non-interactive compilation
;; in order to spare non-viperized emacs from being viperized
(if noninteractive
    (eval-when-compile
      (require 'viper-cmd)
      ))
;; end pacifier

(require 'viper-util)


(defgroup viper-mouse nil
  "Support for Viper special mouse-bound commands."
  :prefix "viper-"
  :group 'viper)


;;; Variables

;; Variable used for catching the switch-frame event.
;; If non-nil, indicates that previous-frame should be the selected
;; one.  Used by viper-mouse-click-get-word.  Not a user option.
(defvar viper-frame-of-focus nil)

;; Frame that was selected before the switch-frame event.
(defvar viper-current-frame-saved (selected-frame))

(defcustom viper-surrounding-word-function 'viper-surrounding-word
  "*Function that determines what constitutes a word for clicking events.
Takes two parameters: a COUNT, indicating how many words to return,
and CLICK-COUNT, telling whether this is the first click, a double-click,
or a triple-click."
  :type 'symbol
  :group 'viper-mouse)

;; time interval in millisecond within which successive clicks are
;; considered related
(defcustom viper-multiclick-timeout (if (viper-window-display-p)
				      (if (featurep 'xemacs)
					  mouse-track-multi-click-time
					double-click-time)
				    500)
  "*Time interval in millisecond within which successive mouse clicks are
considered related."
  :type 'integer
  :group 'viper-mouse)

;; current event click count; XEmacs only
(defvar viper-current-click-count 0)
;; time stamp of the last click event; XEmacs only
(defvar viper-last-click-event-timestamp 0)

;; Local variable used to toggle wraparound search on click.
(viper-deflocalvar  viper-mouse-click-search-noerror t)

;; Local variable used to delimit search after wraparound.
(viper-deflocalvar  viper-mouse-click-search-limit nil)

;; remembers prefix argument to pass along to commands invoked by second
;; click.
;; This is needed because in Emacs (not XEmacs), assigning to prefix-arg
;; causes Emacs to count the second click as if it was a single click
(defvar viper-global-prefix-argument nil)


;; same keys, but parsed
(defvar viper-mouse-up-search-key-parsed nil)
(defvar viper-mouse-down-search-key-parsed nil)
(defvar viper-mouse-up-insert-key-parsed nil)
(defvar viper-mouse-down-insert-key-parsed nil)




;;; Code

(defsubst viper-multiclick-p ()
  (not (viper-sit-for-short viper-multiclick-timeout t)))

;; Returns window where click occurs
(defun viper-mouse-click-window (click)
  (let ((win (if (featurep 'xemacs) (event-window click)
	       (posn-window (event-start click)))))
    (if (window-live-p win)
	win
      (error "Click was not over a live window"))))

;; Returns window where click occurs
(defsubst viper-mouse-click-frame (click)
  (window-frame (viper-mouse-click-window click)))

;; Returns the buffer of the window where click occurs
(defsubst viper-mouse-click-window-buffer (click)
  (window-buffer (viper-mouse-click-window click)))

;; Returns the name of the buffer in the window where click occurs
(defsubst viper-mouse-click-window-buffer-name (click)
  (buffer-name (viper-mouse-click-window-buffer click)))

;; Returns position of a click
(defsubst viper-mouse-click-posn (click)
  (if (featurep 'xemacs) (event-point click)
    (posn-point (event-start click))))


(defun viper-surrounding-word (count click-count)
   "Returns word surrounding point according to a heuristic.
COUNT indicates how many regions to return.
If CLICK-COUNT is 1, `word' is a word in Vi sense.
If CLICK-COUNT is 2,then `word' is a Word in Vi sense.
If the character clicked on is a non-separator and is non-alphanumeric but
is adjacent to an alphanumeric symbol, then it is considered alphanumeric
for the purpose of this command.  If this character has a matching
character, such as `\(' is a match for `\)', then the matching character is
also considered alphanumeric.
For convenience, in Lisp modes, `-' is considered alphanumeric.

If CLICK-COUNT is 3 or more, returns the line clicked on with leading and
trailing space and tabs removed.  In that case, the first argument, COUNT,
is ignored."
   (let ((modifiers "_")
	 beg skip-flag result
	 word-beg)
     (if (> click-count 2)
	 (save-excursion
	   (beginning-of-line)
	   (viper-skip-all-separators-forward 'within-line)
	   (setq beg (point))
	   (end-of-line)
	   (setq result (buffer-substring beg (point))))

       (if (and (not (viper-looking-at-alphasep))
		(or (save-excursion (viper-backward-char-carefully)
				    (viper-looking-at-alpha))
		    (save-excursion (viper-forward-char-carefully)
				    (viper-looking-at-alpha))))
	   (setq modifiers
		 (concat modifiers
			 (cond ((looking-at "\\\\") "\\\\")
			       ((looking-at "-") "C-C-")
			       ((looking-at "[][]") "][")
			       ((looking-at "[()]") ")(")
			       ((looking-at "[{}]") "{}")
			       ((looking-at "[<>]") "<>")
			       ((looking-at "[`']") "`'")
			       ((looking-at "\\^") "\\^")
			       ((viper-looking-at-separator) "")
			       (t (char-to-string (following-char))))
			 )
		 ))

       ;; Add `-' to alphanum, if it wasn't added and if we are in Lisp
       (or (looking-at "-")
	   (not (string-match "lisp" (symbol-name major-mode)))
	   (setq modifiers (concat modifiers "C-C-")))


       (save-excursion
	 (cond ((> click-count 1) (viper-skip-nonseparators 'backward))
	       ((viper-looking-at-alpha modifiers)
		(viper-skip-alpha-backward modifiers))
	       ((not (viper-looking-at-alphasep modifiers))
		(viper-skip-nonalphasep-backward))
	       (t (if (> click-count 1)
		      (viper-skip-nonseparators 'backward)
		    (viper-skip-alpha-backward modifiers))))

	 (setq word-beg (point))

	 (setq skip-flag nil) ; don't move 1 char forw the first time
	 (while (> count 0)
	   (if skip-flag (viper-forward-char-carefully 1))
	   (setq skip-flag t) ; now always move 1 char forward
	   (if (> click-count 1)
	       (viper-skip-nonseparators 'forward)
	     (viper-skip-alpha-forward modifiers))
	   (setq count (1- count)))

	 (setq result (buffer-substring word-beg (point))))
       ) ; if
     ;; XEmacs doesn't have set-text-properties, but there buffer-substring
     ;; doesn't return properties together with the string, so it's not needed.
     (if (featurep 'emacs)
	 (set-text-properties 0 (length result) nil result))
     result
     ))


(defun viper-mouse-click-get-word (click count click-count)
  "Returns word surrounding the position of a mouse click.
Click may be in another window.  Current window and buffer isn't changed.
On single or double click, returns the word as determined by
`viper-surrounding-word-function'."

  (let ((click-word "")
	(click-pos (viper-mouse-click-posn click))
	(click-buf (viper-mouse-click-window-buffer click)))
    (or (natnump count) (setq count 1))
    (or (natnump click-count) (setq click-count 1))

    (save-excursion
      (save-window-excursion
	(if click-pos
	    (progn
	      (set-buffer click-buf)

	      (goto-char click-pos)
	      (setq click-word
		    (funcall viper-surrounding-word-function count click-count)))
	  (error "Click must be over a window"))
	click-word))))


(defun viper-mouse-click-insert-word (click arg)
  "Insert word clicked or double-clicked on.
With prefix argument, N, insert that many words.
This command must be bound to a mouse click.
The double-click action of the same mouse button must not be bound
\(or it must be bound to the same function\).
See `viper-surrounding-word' for the definition of a word in this case."
  (interactive "e\nP")
  (if viper-frame-of-focus	;; to handle clicks in another frame
      (select-frame viper-frame-of-focus))
  (if (save-excursion
	(or (not (eq (key-binding viper-mouse-down-insert-key-parsed)
		     'viper-mouse-catch-frame-switch))
	    (not (eq (key-binding viper-mouse-up-insert-key-parsed)
		     'viper-mouse-click-insert-word))
	    (and (featurep 'xemacs) (not (event-over-text-area-p click)))))
      () ; do nothing, if binding isn't right or not over text
    ;; turn arg into a number
    (cond ((integerp arg) nil)
	  ;; prefix arg is a list when one hits C-u then command
	  ((and (listp arg) (integerp (car arg)))
	   (setq arg (car arg)))
	  (t (setq arg 1)))

    (if (not (eq (key-binding viper-mouse-down-insert-key-parsed)
		 'viper-mouse-catch-frame-switch))
	() ; do nothing
      (let (click-count interrupting-event)
	(if (and
	     (viper-multiclick-p)
	     ;; This trick checks if there is a pending mouse event if so, we
	     ;; use this latter event and discard the current mouse click If
	     ;; the next pending event is not a mouse event, we execute the
	     ;; current mouse event
	     (progn
	       (setq interrupting-event (viper-read-event))
	       (viper-mouse-event-p last-input-event)))
	    (progn ; interrupted wait
	      (setq viper-global-prefix-argument arg)
	      ;; count this click for XEmacs
	      (viper-event-click-count click))
	  ;; uninterrupted wait or the interrupting event wasn't a mouse event
	  (setq click-count (viper-event-click-count click))
	  (if (> click-count 1)
	      (setq arg viper-global-prefix-argument
		    viper-global-prefix-argument nil))
	  (insert (viper-mouse-click-get-word click arg click-count))
	  (if (and interrupting-event
		   (eventp interrupting-event)
		   (not (viper-mouse-event-p interrupting-event)))
	      (viper-set-unread-command-events interrupting-event))
	  )))))

;; Arg is an event.  Accepts symbols and numbers, too
(defun viper-mouse-event-p (event)
  (if (eventp event)
      (string-match "\\(mouse-\\|frame\\|screen\\|track\\)"
		    (prin1-to-string (viper-event-key event)))))

;; XEmacs has no double-click events.  So, we must simulate.
;; So, we have to simulate event-click-count.
(defun viper-event-click-count (click)
  (if (featurep 'xemacs) (viper-event-click-count-xemacs click)
    (event-click-count click)))

(when (featurep 'xemacs)

  ;; kind of semaphore for updating viper-current-click-count
  (defvar viper-counting-clicks-p nil)

  (defun viper-event-click-count-xemacs (click)
    (let ((time-delta (- (event-timestamp click)
			 viper-last-click-event-timestamp))
	  inhibit-quit)
      (while viper-counting-clicks-p
	(ignore))
      (setq viper-counting-clicks-p t)
      (if (> time-delta viper-multiclick-timeout)
	  (setq viper-current-click-count 0))
      (discard-input)
      (setq viper-current-click-count (1+ viper-current-click-count)
	    viper-last-click-event-timestamp (event-timestamp click))
      (setq viper-counting-clicks-p nil)
      (if (viper-sit-for-short viper-multiclick-timeout t)
	  viper-current-click-count
	0))))


(defun viper-mouse-click-search-word (click arg)
   "Find the word clicked or double-clicked on.  Word may be in another window.
With prefix argument, N, search for N-th occurrence.
This command must be bound to a mouse click.  The double-click action of the
same button must not be bound \(or it must be bound to the same function\).
See `viper-surrounding-word' for the details on what constitutes a word for
this command."
  (interactive "e\nP")
  (if viper-frame-of-focus	;; to handle clicks in another frame
      (select-frame viper-frame-of-focus))
  (if (save-excursion
	(or (not (eq (key-binding viper-mouse-down-search-key-parsed)
		     'viper-mouse-catch-frame-switch))
	    (not (eq (key-binding viper-mouse-up-search-key-parsed)
		     'viper-mouse-click-search-word))
	    (and (featurep 'xemacs) (not (event-over-text-area-p click)))))
      () ; do nothing, if binding isn't right or not over text
    (let ((previous-search-string viper-s-string)
	  click-word click-count)

      (if (and
	   (viper-multiclick-p)
	   ;; This trick checks if there is a pending mouse event if so, we use
	   ;; this latter event and discard the current mouse click If the next
	   ;; pending event is not a mouse event, we execute the current mouse
	   ;; event
	   (progn
	     (viper-read-event)
	     (viper-mouse-event-p last-input-event)))
	  (progn ; interrupted wait
	    (setq viper-global-prefix-argument (or viper-global-prefix-argument
						   arg)
		  ;; remember command that was before the multiclick
		  this-command last-command)
	    ;; make sure we counted this event---needed for XEmacs only
	    (viper-event-click-count click))
	;; uninterrupted wait
	(setq click-count (viper-event-click-count click))
	(setq click-word (viper-mouse-click-get-word click nil click-count))

	(if (> click-count 1)
	    (setq arg viper-global-prefix-argument
		  viper-global-prefix-argument nil))
	(setq arg (or arg 1))

	(viper-deactivate-mark)
	(if (or (not (string= click-word viper-s-string))
		(not (markerp viper-search-start-marker))
		(not (equal (marker-buffer viper-search-start-marker)
			    (current-buffer)))
		(not (eq last-command 'viper-mouse-click-search-word)))
	    (progn
	      (setq  viper-search-start-marker (point-marker)
		     viper-local-search-start-marker viper-search-start-marker
		     viper-mouse-click-search-noerror t
		     viper-mouse-click-search-limit nil)

	      ;; make search string known to Viper
	      (setq viper-s-string (if viper-re-search
				       (regexp-quote click-word)
				   click-word))
	      (if (not (string= viper-s-string (car viper-search-history)))
		  (setq viper-search-history
			(cons viper-s-string viper-search-history)))
	      ))

	(push-mark nil t)
	(while (> arg 0)
	  (viper-forward-word 1)
	  (condition-case nil
	      (progn
		(if (not (search-forward
			  click-word viper-mouse-click-search-limit
			  viper-mouse-click-search-noerror))
		    (progn
		      (setq viper-mouse-click-search-noerror nil)
		      (setq viper-mouse-click-search-limit
			    (save-excursion
			      (if (and
				   (markerp viper-local-search-start-marker)
				   (marker-buffer viper-local-search-start-marker))
				  (goto-char viper-local-search-start-marker))
			      (viper-line-pos 'end)))

		      (goto-char (point-min))
		      (search-forward click-word
				      viper-mouse-click-search-limit nil)))
		(goto-char (match-beginning 0))
		(message "Searching for: %s" viper-s-string)
		(if (<= arg 1) ; found the right occurrence of the pattern
		    (progn
		      (viper-adjust-window)
		      (viper-flash-search-pattern)))
		)
	    (error (beep 1)
		   (if (or (not (string= click-word previous-search-string))
			   (not (eq  last-command 'viper-mouse-click-search-word)))
		       (message "`%s': String not found in %s"
				viper-s-string (buffer-name (current-buffer)))
		     (message
		      "`%s': Last occurrence in %s.  Back to beginning of search"
		      click-word (buffer-name (current-buffer)))
		     (setq arg 1) ;; to terminate the loop
		     (sit-for 2))
		   (setq  viper-mouse-click-search-noerror t)
		   (setq  viper-mouse-click-search-limit nil)
		   (if (and (markerp viper-local-search-start-marker)
			    (marker-buffer viper-local-search-start-marker))
		       (goto-char viper-local-search-start-marker))))
	  (setq arg (1- arg)))
	))))

(defun viper-mouse-catch-frame-switch (event arg)
  "Catch the event of switching frame.
Usually is bound to a `down-mouse' event to work properly.  See sample
bindings in the Viper manual."
  (interactive "e\nP")
  (setq viper-frame-of-focus nil)
  ;; pass prefix arg along to viper-mouse-click-search/insert-word
  (setq prefix-arg arg)
  (if (eq last-command 'handle-switch-frame)
      (setq viper-frame-of-focus viper-current-frame-saved))
  ;; make Emacs forget that it executed viper-mouse-catch-frame-switch
  (setq this-command last-command))

;; Called just before switching frames.  Saves the old selected frame.
;; Sets last-command to handle-switch-frame (this is done automatically in
;; Emacs.
;; The semantics of switching frames is different in Emacs and XEmacs.
;; In Emacs, if you select-frame A while mouse is over frame B and then
;; start typing, input goes to frame B, which becomes selected.
;; In XEmacs, input will go to frame A.  This may be a bug in one of the
;; Emacsen, but also may be a design decision.
;; Also, in Emacs sending input to frame B generates handle-switch-frame
;; event, while in XEmacs it doesn't.
;; All this accounts for the difference in the behavior of
;; viper-mouse-click-* commands when you click in a frame other than the one
;; that was the last to receive input.  In Emacs, focus will be in frame A
;; until you do something other than viper-mouse-click-* command.
;; In XEmacs, you have to manually select frame B (with the mouse click) in
;; order to shift focus to frame B.
(defsubst viper-remember-current-frame (frame)
  (setq last-command 'handle-switch-frame
	viper-current-frame-saved (selected-frame)))


;; The key is of the form (MODIFIER ... BUTTON-NUMBER)
;; Converts into a valid mouse button spec for the appropriate version of
;; Emacs.  EVENT-TYPE is either `up' or `down'.  Up returns button-up key; down
;; returns button-down key.
(defun viper-parse-mouse-key (key-var event-type)
  (let ((key (eval key-var))
	button-spec meta-spec shift-spec control-spec key-spec)
    (if (null key)
	;; just return nil
	()
      (setq button-spec
	    (cond ((memq 1 key)
		   (if (featurep 'emacs)
		       (if (eq 'up event-type)
			   "mouse-1" "down-mouse-1")
		     (if (eq 'up event-type)
			 'button1up 'button1)))
		  ((memq 2 key)
		   (if (featurep 'emacs)
		       (if (eq 'up event-type)
			   "mouse-2" "down-mouse-2")
		     (if (eq 'up event-type)
			 'button2up 'button2)))
		  ((memq 3 key)
		   (if (featurep 'emacs)
		       (if (eq 'up event-type)
			   "mouse-3" "down-mouse-3")
		     (if (eq 'up event-type)
			 'button3up 'button3)))
		  (t (error
		      "%S: invalid button number, %S" key-var key)))
	    meta-spec
	    (if (memq 'meta key)
		(if (featurep 'emacs) "M-" 'meta)
	      (if (featurep 'emacs) "" nil))
	    shift-spec
	    (if (memq 'shift key)
		(if (featurep 'emacs) "S-" 'shift)
	      (if (featurep 'emacs) "" nil))
	    control-spec
	    (if (memq 'control key)
		(if (featurep 'emacs) "C-" 'control)
	      (if (featurep 'emacs) "" nil)))

      (setq key-spec (if (featurep 'emacs)
			 (vector
			  (intern
			   (concat
			    control-spec meta-spec shift-spec button-spec)))
		       (vector
			(delq
			 nil
			 (list
			  control-spec meta-spec shift-spec button-spec)))))
      )))

(defun viper-unbind-mouse-search-key ()
  (if viper-mouse-up-search-key-parsed
      (global-unset-key viper-mouse-up-search-key-parsed))
  (if viper-mouse-down-search-key-parsed
      (global-unset-key viper-mouse-down-search-key-parsed))
  (setq viper-mouse-up-search-key-parsed nil
	viper-mouse-down-search-key-parsed nil))

(defun viper-unbind-mouse-insert-key ()
  (if viper-mouse-up-insert-key-parsed
      (global-unset-key viper-mouse-up-insert-key-parsed))
  (if viper-mouse-down-insert-key-parsed
      (global-unset-key viper-mouse-down-insert-key-parsed))
  (setq viper-mouse-up-insert-key-parsed nil
	viper-mouse-down-insert-key-parsed nil))

;; If FORCE, bind even if this mouse action is already bound to something else
(defun viper-bind-mouse-search-key (&optional force)
  (setq viper-mouse-up-search-key-parsed
	(viper-parse-mouse-key 'viper-mouse-search-key 'up)
	viper-mouse-down-search-key-parsed
	(viper-parse-mouse-key 'viper-mouse-search-key 'down))
  (cond ((or (null viper-mouse-up-search-key-parsed)
	     (null viper-mouse-down-search-key-parsed))
	 nil) ; just quit
	((and (null force)
	      (key-binding viper-mouse-up-search-key-parsed)
	      (not (eq (key-binding viper-mouse-up-search-key-parsed)
		       'viper-mouse-click-search-word)))
	 (message
	  "%S already bound to a mouse event.  Viper mouse-search feature disabled"
	  viper-mouse-up-search-key-parsed))
	((and (null force)
	      (key-binding viper-mouse-down-search-key-parsed)
	      (not (eq (key-binding viper-mouse-down-search-key-parsed)
		       'viper-mouse-catch-frame-switch)))
	 (message
	  "%S already bound to a mouse event.  Viper mouse-search feature disabled"
	  viper-mouse-down-search-key-parsed))
	(t
	 (global-set-key viper-mouse-up-search-key-parsed
			 'viper-mouse-click-search-word)
	 (global-set-key viper-mouse-down-search-key-parsed
			 'viper-mouse-catch-frame-switch))))

;; If FORCE, bind even if this mouse action is already bound to something else
(defun viper-bind-mouse-insert-key (&optional force)
  (setq viper-mouse-up-insert-key-parsed
	(viper-parse-mouse-key 'viper-mouse-insert-key 'up)
	viper-mouse-down-insert-key-parsed
	(viper-parse-mouse-key 'viper-mouse-insert-key 'down))
  (cond ((or (null viper-mouse-up-insert-key-parsed)
	     (null viper-mouse-down-insert-key-parsed))
	 nil) ; just quit
	((and (null force)
	      (key-binding viper-mouse-up-insert-key-parsed)
	      (not (eq (key-binding viper-mouse-up-insert-key-parsed)
		       'viper-mouse-click-insert-word)))
	 (message
	  "%S already bound to a mouse event.  Viper mouse-insert feature disabled"
	  viper-mouse-up-insert-key-parsed))
	((and (null force)
	      (key-binding viper-mouse-down-insert-key-parsed)
	      (not (eq (key-binding viper-mouse-down-insert-key-parsed)
		       'viper-mouse-catch-frame-switch)))
	 (message
	  "%S already bound to a mouse event.  Viper mouse-insert feature disabled"
	  viper-mouse-down-insert-key-parsed))
	(t
	 (global-set-key viper-mouse-up-insert-key-parsed
			 'viper-mouse-click-insert-word)
	 (global-set-key viper-mouse-down-insert-key-parsed
			 'viper-mouse-catch-frame-switch))))

(defun viper-reset-mouse-search-key (symb val)
  (viper-unbind-mouse-search-key)
  (set symb val)
  (viper-bind-mouse-search-key 'force))

(defun viper-reset-mouse-insert-key (symb val)
  (viper-unbind-mouse-insert-key)
  (set symb val)
  (viper-bind-mouse-insert-key 'force))


(defcustom viper-mouse-search-key '(meta shift 1)
  "*Key used to click-search in Viper.
This must be a list that specifies the mouse button and modifiers.
The supported modifiers are `meta', `shift', and `control'.
For instance, `(meta shift 1)' means that holding the meta and shift
keys down and clicking on a word with mouse button 1
will search for that word in the buffer that was current before the click.
This buffer may be different from the one where the click occurred."
  :type '(list (set :inline t :tag "Modifiers" :format "%t: %v"
		     (const :format "%v " meta)
		     (const :format "%v " shift)
		     (const control))
	       (integer :tag "Button"))
  :set 'viper-reset-mouse-search-key
  :group 'viper-mouse)

(defcustom viper-mouse-insert-key '(meta shift 2)
  "*Key used to click-insert in Viper.
Must be a list that specifies the mouse button and modifiers.
The supported modifiers are `meta', `shift', and `control'.
For instance, `(meta shift 2)' means that holding the meta and shift keys
down, and clicking on a word with mouse button 2, will insert that word
at the cursor in the buffer that was current just before the click.
This buffer may be different from the one where the click occurred."
  :type '(list (set :inline t :tag "Modifiers" :format "%t: %v"
		     (const :format "%v " meta)
		     (const :format "%v " shift)
		     (const control))
	       (integer :tag "Button"))
  :set 'viper-reset-mouse-insert-key
  :group 'viper-mouse)



;; Local Variables:
;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
;; End:


;;; viper-mous.el ends here
