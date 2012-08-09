;;; rng-valid.el --- real-time validation of XML using RELAX NG

;; Copyright (C) 2003, 2007-2012  Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

;; For usage information, see the documentation for rng-validate-mode.
;;
;; This file provides a minor mode that continually validates a buffer
;; against a RELAX NG schema. The validation state is used to support
;; schema-sensitive editing as well as validation. Validation is
;; performed while Emacs is idle.  XML parsing is done using
;; xmltok.el. This file is responsible for checking that end-tags
;; match their start-tags.  Namespace processing is handled by
;; nxml-ns.el. The RELAX NG Compact Syntax schema is parsed into
;; internal form by rng-cmpct.el.  This internal form is described by
;; rng-pttrn.el.  Validation of the document by matching against this
;; internal form is done by rng-match.el. Handling of W3C XML Schema
;; datatypes is delegated by rng-match.el to rng-xsd.el.  The minor
;; mode is intended to be used in conjunction with the nxml major
;; mode, but does not have to be.
;;
;; The major responsibility of this file is to allow validation to
;; happen incrementally.  If a buffer has been validated and is then
;; changed, we can often revalidate it without having to completely
;; parse and validate it from start to end.  As we parse and validate
;; the buffer, we periodically cache the state.  The state has three
;; components: the stack of open elements, the namespace processing
;; state and the RELAX NG validation state. The state is cached as the
;; value of the rng-state text property on the closing greater-than of
;; tags (but at intervals, not on every tag).  We keep track of the
;; position up to which cached state is known to be correct by adding
;; a function to the buffer's after-change-functions. This is stored
;; in the rng-validate-up-to-date-end variable.  The first way in
;; which we make validation incremental is obvious: we start
;; validation from the first cached state before
;; rng-validate-up-to-date-end.
;;
;; To make this work efficiently, we have to be able to copy the
;; current parsing and validation state efficiently.  We do this by
;; minimizing destructive changes to the objects storing the state.
;; When state is changed, we use the old state to create new objects
;; representing the new state rather than destructively modifying the
;; objects representing the old state. Copying the state is just a
;; matter of making a list of three objects, one for each component of
;; the state; the three objects themselves can be shared and do not
;; need to be copied.
;;
;; There's one other idea that is used to make validation incremental.
;; Suppose we have a buffer that's 4000 bytes long and suppose we
;; validated it, caching state at positions 1000, 2000 and 3000.  Now
;; suppose we make a change at position 1500 inserting 100 characters.
;; rng-validate-up-to-date-end will be changed to 1500.  When Emacs
;; becomes idle and we revalidate, validation will restart using the
;; cached state at position 1000.  However, we take advantage of the
;; cached state beyond rng-validate-up-to-date-end as follows.  When
;; our validation reaches position 2100 (the current position of the
;; character that was at 2000), we compare our current state with the
;; cached state.  If they are the same, then we can stop parsing
;; immediately and set rng-validate-up-to-date-end to the end of the
;; buffer: we already know that the state cached at position 3100 is
;; correct.  If they are not the same, then we have to continue
;; parsing.  After the change, but before revalidation, we call the
;; region from 1600 to the end of the buffer "conditionally
;; up-to-date".
;;
;; As well as the cached parsing and validation state, we also keep
;; track of the errors in the file.  Errors are stored as overlays
;; with a category of rng-error.  The number of such overlays in the
;; buffer must always be equal to rng-error-count.

;;; Code:

(require 'xmltok)
(require 'nxml-enc)
(require 'nxml-util)
(require 'nxml-ns)
(require 'rng-match)
(require 'rng-util)
(require 'rng-loc)

;;; Customizable variables

(defgroup relax-ng nil
  "Validation of XML using RELAX NG."
  :group 'wp
  :group 'nxml
  :group 'languages)

(defface rng-error '((t (:inherit font-lock-warning-face)))
  "Face for highlighting XML errors."
  :group 'relax-ng)

(defcustom rng-state-cache-distance 2000
  "Distance in characters between each parsing and validation state cache."
  :type 'integer
  :group 'relax-ng)

(defcustom rng-validate-chunk-size 8000
  "Number of characters in a RELAX NG validation chunk.
A validation chunk will be the smallest chunk that is at least this
size and ends with a tag.  After validating a chunk, validation will
continue only if Emacs is still idle."
  :type 'integer
  :group 'relax-ng)

(defcustom rng-validate-delay 1.5
  "Time in seconds that Emacs must be idle before starting a full validation.
A full validation continues until either validation is up to date
or Emacs is no longer idle."
  :type 'number
  :group 'relax-ng)

(defcustom rng-validate-quick-delay 0.3
  "Time in seconds that Emacs must be idle before starting a quick validation.
A quick validation validates at most one chunk."
  :type 'number
  :group 'relax-ng)

;; Global variables

(defvar rng-validate-timer nil)
(make-variable-buffer-local 'rng-validate-timer)
;; ensure that we can cancel the timer even after a kill-all-local-variables
(put 'rng-validate-timer 'permanent-local t)

(defvar rng-validate-quick-timer nil)
(make-variable-buffer-local 'rng-validate-quick-timer)
;; ensure that we can cancel the timer even after a kill-all-local-variables
(put 'rng-validate-quick-timer 'permanent-local t)

(defvar rng-error-count nil
  "Number of errors in the current buffer.
Always equal to number of overlays with category `rng-error'.")
(make-variable-buffer-local 'rng-error-count)

(defvar rng-message-overlay nil
  "Overlay in this buffer whose `help-echo' property was last printed.
It is nil if none.")
(make-variable-buffer-local 'rng-message-overlay)

(defvar rng-message-overlay-inhibit-point nil
  "Position at which message from overlay should be inhibited.
If point is equal to this and the error overlay around
point is `rng-message-overlay', then the `help-echo' property
of the error overlay should not be printed with `message'.")
(make-variable-buffer-local 'rng-message-overlay-inhibit-point)

(defvar rng-message-overlay-current nil
  "Non-nil if `rng-message-overlay' is still the current message.")
(make-variable-buffer-local 'rng-message-overlay-current)

(defvar rng-open-elements nil
  "Stack of names of open elements represented as a list.
Each member of the list is either t or a (PREFIX . LOCAL-NAME) pair.
\(PREFIX . LOCAL-NAME) is pushed for a start-tag; t is pushed
for a mismatched end-tag.")

(defvar rng-pending-contents nil
  "Text content of current element that has yet to be processed.
Value is a list of segments (VALUE START END) positions in reverse
order.  VALUE is a string or nil.  If VALUE is nil, then the value is
the string between START and END.  A segment can also be nil
indicating an unresolvable entity or character reference.")

(defvar rng-collecting-text nil)

(defvar rng-validate-up-to-date-end nil
  "Last position where validation is known to be up to date.")
(make-variable-buffer-local 'rng-validate-up-to-date-end)

(defvar rng-conditional-up-to-date-start nil
  "Marker for the start of the conditionally up-to-date region.
It is nil if there is no conditionally up-to-date region.  The
conditionally up-to-date region must be such that for any cached
state S with position P in the conditionally up-to-date region,
if at some point it is determined that S becomes correct for P,
then all states with position >= P in the conditionally up to
date region must also then be correct and all errors between P
and the end of the region must then be correctly marked.")
(make-variable-buffer-local 'rng-conditional-up-to-date-start)

(defvar rng-conditional-up-to-date-end nil
  "Marker for the end of the conditionally up-to-date region.
It is nil if there is no conditionally up-to-date region.
See the variable `rng-conditional-up-to-date-start'.")
(make-variable-buffer-local 'rng-conditional-up-to-date-end)

(defvar rng-parsing-for-state nil
  "Non-nil means we are currently parsing just to compute the state.
Should be dynamically bound.")

(defvar rng-validate-mode nil)
(make-variable-buffer-local 'rng-validate-mode)

(defvar rng-dtd nil)
(make-variable-buffer-local 'rng-dtd)

;;;###autoload
(defun rng-validate-mode (&optional arg no-change-schema)
  "Minor mode performing continual validation against a RELAX NG schema.

Checks whether the buffer is a well-formed XML 1.0 document,
conforming to the XML Namespaces Recommendation and valid against a
RELAX NG schema.  The mode-line indicates whether it is or not.  Any
parts of the buffer that cause it not to be are considered errors and
are highlighted with face `rng-error'.  A description of each error is
available as a tooltip.  \\[rng-next-error] goes to the next error
after point.  Clicking mouse-1 on the word `Invalid' in the mode-line
goes to the first error in the buffer.  If the buffer changes, then it
will be automatically rechecked when Emacs becomes idle; the
rechecking will be paused whenever there is input pending.

By default, uses a vacuous schema that allows any well-formed XML
document.  A schema can be specified explicitly using
\\[rng-set-schema-file-and-validate], or implicitly based on the buffer's
file name or on the root element name.  In each case the schema must
be a RELAX NG schema using the compact schema \(such schemas
conventionally have a suffix of `.rnc').  The variable
`rng-schema-locating-files' specifies files containing rules
to use for finding the schema."
  (interactive "P")
  (setq rng-validate-mode
	(if (null arg)
	    (not rng-validate-mode)
	  (> (prefix-numeric-value arg) 0)))
  (save-restriction
    (widen)
    (nxml-with-unmodifying-text-property-changes
      (rng-clear-cached-state (point-min) (point-max)))
    ;; 1+ to clear empty overlays at (point-max)
    (rng-clear-overlays (point-min) (1+ (point-max)))
    (setq rng-validate-up-to-date-end (point-min)))
  (rng-clear-conditional-region)
  (setq rng-error-count 0)
  ;; do this here to avoid infinite loop if we set the schema
  (remove-hook 'rng-schema-change-hook 'rng-validate-clear t)
  (cond (rng-validate-mode
	 (unwind-protect
	     (save-excursion
	       ;; An error can change the current buffer
	       (when (or (not rng-current-schema)
			 (and (eq rng-current-schema rng-any-element)
			      (not no-change-schema)))
		 (rng-auto-set-schema t)))
	   (unless rng-current-schema (rng-set-schema-file-1 nil))
	   (add-hook 'rng-schema-change-hook 'rng-validate-clear nil t)
	   (add-hook 'after-change-functions 'rng-after-change-function nil t)
	   (add-hook 'kill-buffer-hook 'rng-kill-timers nil t)
	   (add-hook 'echo-area-clear-hook 'rng-echo-area-clear-function nil t)
	   (add-hook 'post-command-hook 'rng-maybe-echo-error-at-point nil t)
	   (rng-match-init-buffer)
	   (rng-activate-timers)
	   ;; Start validating right away if the buffer is visible.
	   ;; If it's not visible, don't do this, because the user
	   ;; won't get any progress indication. When the user finds
	   ;; a new file, then the buffer won't be visible
	   ;; when this is invoked.
	   (when (get-buffer-window (current-buffer) 'visible)
	     (rng-validate-while-idle (current-buffer)))))
	(t
	 (rng-cancel-timers)
	 (force-mode-line-update)
	 (remove-hook 'kill-buffer-hook 'rng-cancel-timers t)
	 (remove-hook 'post-command-hook 'rng-maybe-echo-error-at-point t)
	 (remove-hook 'echo-area-clear-hook 'rng-echo-area-clear-function t)
	 (remove-hook 'after-change-functions 'rng-after-change-function t))))

(defun rng-set-schema-file-and-validate (filename)
  "Sets the schema and turns on `rng-validate-mode' if not already on.
The schema is set like `rng-set-schema'."
  (interactive "fSchema file: ")
  (rng-set-schema-file filename)
  (or rng-validate-mode (rng-validate-mode)))

(defun rng-set-document-type-and-validate (type-id)
  (interactive (list (rng-read-type-id)))
  (and (rng-set-document-type type-id)
       (or rng-validate-mode (rng-validate-mode))))

(defun rng-auto-set-schema-and-validate ()
  "Set the schema for this buffer automatically and turn on `rng-validate-mode'.
The schema is set like `rng-auto-set-schema'."
  (interactive)
  (rng-auto-set-schema)
  (or rng-validate-mode (rng-validate-mode)))

(defun rng-after-change-function (start end pre-change-len)
  (setq rng-message-overlay-inhibit-point nil)
  (nxml-with-unmodifying-text-property-changes
    (rng-clear-cached-state start end))
  ;; rng-validate-up-to-date-end holds the position before the change
  ;; Adjust it to reflect the change.
  (if (< start rng-validate-up-to-date-end)
      (setq rng-validate-up-to-date-end
	    (if (<= (+ start pre-change-len) rng-validate-up-to-date-end)
		(+ rng-validate-up-to-date-end
		   (- end start pre-change-len))
	      start)))
  ;; Adjust the conditional zone
  (cond (rng-conditional-up-to-date-start
	 (when (< rng-conditional-up-to-date-start end)
	   (if (< end rng-conditional-up-to-date-end)
	       (set-marker rng-conditional-up-to-date-start end)
	     (rng-clear-conditional-region))))
	((< end rng-validate-up-to-date-end)
	 (setq rng-conditional-up-to-date-end
	       (copy-marker rng-validate-up-to-date-end nil))
	 (setq rng-conditional-up-to-date-start
	       (copy-marker end t))))
  ;; Adjust rng-validate-up-to-date-end
  (if (< start rng-validate-up-to-date-end)
      (setq rng-validate-up-to-date-end start))
  ;; Must make rng-validate-up-to-date-end < point-max
  ;; (unless the buffer is empty).
  ;; otherwise rng-validate-prepare will say there's nothing to do.
  (when (>= rng-validate-up-to-date-end (point-max))
    (setq rng-validate-up-to-date-end
          (if (< (point-min) (point-max))
              (1- (point-max))
            ;; Only widen if really necessary.
            (save-restriction (widen) (max (point-min) (1- (point-max)))))))
  ;; Arrange to revalidate
  (rng-activate-timers)
  ;; Need to do this after activating the timer
  (force-mode-line-update))

(defun rng-compute-mode-line-string ()
  (cond (rng-validate-timer
	 (concat " Validated:"
		 (number-to-string
		  ;; Use floor rather than round because we want
		  ;; to show 99% rather than 100% for changes near
		  ;; the end.
		  (floor (if (eq (buffer-size) 0)
			     0.0
			   (/ (* (- rng-validate-up-to-date-end (point-min))
                                 100.0)
			      (- (point-max) (point-min))))))
		 "%%"))
	((> rng-error-count 0)
	 (concat " "
		 (propertize "Invalid"
			     'help-echo "mouse-1: go to first error"
			     'local-map (make-mode-line-mouse-map
					 'mouse-1
					 'rng-mouse-first-error))))
	(t " Valid")))

(defun rng-cancel-timers ()
  (let ((inhibit-quit t))
    (when rng-validate-timer
      (cancel-timer rng-validate-timer)
      (setq rng-validate-timer nil))
    (when rng-validate-quick-timer
      (cancel-timer rng-validate-quick-timer)
      (setq rng-validate-quick-timer nil))))

(defun rng-kill-timers ()
  ;; rng-validate-timer and rng-validate-quick-timer have the
  ;; permanent-local property, so that the timers can be
  ;; canceled even after changing mode.
  ;; This function takes care of canceling the timers and
  ;; then killing the local variables.
  (when (local-variable-p 'rng-validate-timer)
    (when rng-validate-timer
      (cancel-timer rng-validate-timer))
    (kill-local-variable 'rng-validate-timer))
  (when (local-variable-p 'rng-validate-quick-timer)
    (when rng-validate-quick-timer
      (cancel-timer rng-validate-quick-timer))
    (kill-local-variable 'rng-validate-quick-timer)))

(defun rng-activate-timers ()
  (unless rng-validate-timer
    (let ((inhibit-quit t))
      (setq rng-validate-timer
	    (run-with-idle-timer rng-validate-delay
				 t
				 'rng-validate-while-idle
				 (current-buffer)))
      (setq rng-validate-quick-timer
	    (run-with-idle-timer rng-validate-quick-delay
				 t
				 'rng-validate-quick-while-idle
				 (current-buffer))))))

(defun rng-validate-clear ()
  (rng-validate-mode 1 t))

;; These two variables are dynamically bound and used
;; to pass information between rng-validate-while-idle
;; and rng-validate-while-idle-continue-p.

(defvar rng-validate-display-point nil)
(defvar rng-validate-display-modified-p nil)

(defun rng-validate-while-idle-continue-p ()
  ;; input-pending-p and sit-for run timers that are
  ;; ripe.  Binding timer-idle-list to nil prevents
  ;; this.  If we don't do this, then any ripe timers
  ;; will get run, and we won't get any chance to
  ;; validate until Emacs becomes idle again or until
  ;; the other lower priority timers finish (which
  ;; can take a very long time in the case of
  ;; jit-lock).
  (let ((timer-idle-list nil))
    (and (not (input-pending-p))
	 ;; Fake rng-validate-up-to-date-end so that the mode line
	 ;; shows progress.  Also use this to save point.
	 (let ((rng-validate-up-to-date-end (point)))
	   (goto-char rng-validate-display-point)
	   (when (not rng-validate-display-modified-p)
	     (restore-buffer-modified-p nil))
	   (force-mode-line-update)
	   (let ((continue (sit-for 0)))
	     (goto-char rng-validate-up-to-date-end)
	     continue)))))

;; Calling rng-do-some-validation once with a continue-p function, as
;; opposed to calling it repeatedly, helps on initial validation of a
;; large buffer with lots of errors.  The overlays for errors will all
;; get added when rng-do-some-validation returns and won't slow the
;; validation process down.

(defun rng-validate-while-idle (buffer)
  (with-current-buffer buffer
    (if rng-validate-mode
	(if (let ((rng-validate-display-point (point))
		  (rng-validate-display-modified-p (buffer-modified-p)))
	      (rng-do-some-validation 'rng-validate-while-idle-continue-p))
	    (force-mode-line-update)
	  (rng-validate-done))
      ;; must have done kill-all-local-variables
      (rng-kill-timers))))

(defun rng-validate-quick-while-idle (buffer)
  (with-current-buffer buffer
    (if rng-validate-mode
	(if (rng-do-some-validation)
	    (force-mode-line-update)
	  (rng-validate-done))
      ;; must have done kill-all-local-variables
      (rng-kill-timers))))

(defun rng-validate-done ()
  (when (or (not (current-message))
	    (rng-current-message-from-error-overlay-p))
    (rng-error-overlay-message (or (rng-error-overlay-after (point))
				   (rng-error-overlay-after (1- (point))))))
  (rng-cancel-timers)
  (force-mode-line-update))

(defun rng-do-some-validation (&optional continue-p-function)
  "Do some validation work.  Return t if more to do, nil otherwise."
  (save-excursion
    (save-restriction
      (widen)
      (nxml-with-invisible-motion
	(condition-case-unless-debug err
	    (and (rng-validate-prepare)
		 (let ((rng-dt-namespace-context-getter '(nxml-ns-get-context)))
		   (nxml-with-unmodifying-text-property-changes
		     (rng-do-some-validation-1 continue-p-function))))
	  ;; errors signaled from a function run by an idle timer
	  ;; are ignored; if we don't catch them, validation
	  ;; will get mysteriously stuck at a single place
	  (rng-compile-error
	   (message "Incorrect schema. %s" (nth 1 err))
	   (rng-validate-mode 0)
	   nil)
	  (error
	   (message "Internal error in rng-validate-mode triggered at buffer position %d. %s"
		    (point)
		    (error-message-string err))
	   (rng-validate-mode 0)
	   nil))))))

(defun rng-validate-prepare ()
  "Prepare to do some validation, initializing point and the state.
Return t if there is work to do, nil otherwise."
  (cond ((= rng-validate-up-to-date-end (point-min))
	 (rng-set-initial-state)
	 t)
	((= rng-validate-up-to-date-end (point-max))
	 nil)
	(t (let ((state (get-text-property (1- rng-validate-up-to-date-end)
					   'rng-state)))
	     (cond (state
		    (rng-restore-state state)
		    (goto-char rng-validate-up-to-date-end))
		   (t
		    (let ((pos (previous-single-property-change
				rng-validate-up-to-date-end
				'rng-state)))
		      (cond (pos
			     (rng-restore-state
			      (or (get-text-property (1- pos) 'rng-state)
				  (error "Internal error: state null")))
			     (goto-char pos))
			    (t (rng-set-initial-state))))))))))

(defun rng-dtd-trivial-p (dtd)
  "Check whether the current dtd is different from the trivial default."
  (or (null dtd) (eq dtd xmltok-predefined-entity-alist)))

(defun rng-do-some-validation-1 (&optional continue-p-function)
  (let ((limit (+ rng-validate-up-to-date-end
		  rng-validate-chunk-size))
	(remove-start rng-validate-up-to-date-end)
	(next-cache-point (+ (point) rng-state-cache-distance))
	(continue t)
	(xmltok-dtd rng-dtd)
	have-remaining-chars
	xmltok-type
	xmltok-start
	xmltok-name-colon
	xmltok-name-end
	xmltok-replacement
	xmltok-attributes
	xmltok-namespace-attributes
	xmltok-dependent-regions
	xmltok-errors)
    (when (= (point) 1)
      (let ((regions (xmltok-forward-prolog)))
	(rng-clear-overlays 1 (point))
	(while regions
	  (when (eq (aref (car regions) 0) 'encoding-name)
	    (rng-process-encoding-name (aref (car regions) 1)
				       (aref (car regions) 2)))
	  (setq regions (cdr regions))))
      (unless (equal rng-dtd xmltok-dtd)
	(rng-clear-conditional-region))
      (setq rng-dtd xmltok-dtd))
    (while continue
      (setq have-remaining-chars (rng-forward))
      (let ((pos (point)))
	(setq continue
	      (and have-remaining-chars
		   (or (< pos limit)
		       (and continue-p-function
			    (funcall continue-p-function)
			    (setq limit (+ limit rng-validate-chunk-size))
			    t))))
	(cond ((and rng-conditional-up-to-date-start
		    ;; > because we are getting the state from (1- pos)
		    (> pos rng-conditional-up-to-date-start)
		    (< pos rng-conditional-up-to-date-end)
		    (rng-state-matches-current (get-text-property (1- pos)
								  'rng-state)))
	       (when (< remove-start (1- pos))
		 (rng-clear-cached-state remove-start (1- pos)))
	       ;; sync up with cached validation state
	       (setq continue nil)
	       ;; do this before setting rng-validate-up-to-date-end
	       ;; in case we get a quit
	       (rng-mark-xmltok-errors)
	       (rng-mark-xmltok-dependent-regions)
	       (setq rng-validate-up-to-date-end
		     (marker-position rng-conditional-up-to-date-end))
	       (rng-clear-conditional-region)
	       (setq have-remaining-chars
		     (< rng-validate-up-to-date-end (point-max))))
	      ((or (>= pos next-cache-point)
		   (not continue))
	       (setq next-cache-point (+ pos rng-state-cache-distance))
	       (rng-clear-cached-state remove-start pos)
	       (when have-remaining-chars
		 (rng-cache-state (1- pos)))
	       (setq remove-start pos)
	       (unless continue
		 ;; if we have just blank chars skip to the end
		 (when have-remaining-chars
		   (skip-chars-forward " \t\r\n")
		   (when (= (point) (point-max))
		     (rng-clear-overlays pos (point))
		     (rng-clear-cached-state pos (point))
		     (setq have-remaining-chars nil)
		     (setq pos (point))))
		 (when (not have-remaining-chars)
		   (rng-process-end-document))
		 (rng-mark-xmltok-errors)
		 (rng-mark-xmltok-dependent-regions)
		 (setq rng-validate-up-to-date-end pos)
		 (when rng-conditional-up-to-date-end
		   (cond ((<= rng-conditional-up-to-date-end pos)
			  (rng-clear-conditional-region))
			 ((< rng-conditional-up-to-date-start pos)
			  (set-marker rng-conditional-up-to-date-start
				      pos)))))))))
    have-remaining-chars))

(defun rng-clear-conditional-region ()
  (when rng-conditional-up-to-date-start
    (set-marker rng-conditional-up-to-date-start nil)
    (setq rng-conditional-up-to-date-start nil))
  (when rng-conditional-up-to-date-end
    (set-marker rng-conditional-up-to-date-end nil)
    (setq rng-conditional-up-to-date-end nil)))

(defun rng-clear-cached-state (start end)
  "Clear cached state between START and END."
  (remove-text-properties start end '(rng-state nil)))

(defun rng-cache-state (pos)
  "Save the current state in a text property on the character at pos."
  (put-text-property pos
		     (1+ pos)
		     'rng-state
		     (rng-get-state)))

(defun rng-state-matches-current (state)
  (and state
       (rng-match-state-equal (car state))
       (nxml-ns-state-equal (nth 1 state))
       (equal (nth 2 state) rng-open-elements)))

(defun rng-get-state ()
  (list (rng-match-state)
	(nxml-ns-state)
	rng-open-elements))

(defun rng-restore-state (state)
  (rng-set-match-state (car state))
  (setq state (cdr state))
  (nxml-ns-set-state (car state))
  (setq rng-open-elements (cadr state))
  (setq rng-pending-contents nil)
  (setq rng-collecting-text (rng-match-text-typed-p)))

(defun rng-set-initial-state ()
  (nxml-ns-init)
  (rng-match-start-document)
  (setq rng-open-elements nil)
  (setq rng-pending-contents nil)
  (goto-char (point-min)))

(defun rng-clear-overlays (beg end)
  (unless rng-parsing-for-state
    (let ((overlays (overlays-in beg end)))
      (while overlays
	(let* ((overlay (car overlays))
	       (category (overlay-get overlay 'category)))
	  (cond ((eq category 'rng-error)
		 (let ((inhibit-quit t))
		   (when (eq overlay rng-message-overlay)
		     (rng-error-overlay-message nil))
		   (delete-overlay overlay)
		   ;; rng-error-count could be nil
		   ;; if overlays left over from a previous use
		   ;; of rng-validate-mode that ended with a change of mode
		   (when rng-error-count
		     (setq rng-error-count (1- rng-error-count)))))
		((and (eq category 'rng-dependent)
		      (<= beg (overlay-start overlay)))
		 (delete-overlay overlay))))
	(setq overlays (cdr overlays))))))

;;; Dependent regions

(defun rng-mark-xmltok-dependent-regions ()
  (while xmltok-dependent-regions
    (apply 'rng-mark-xmltok-dependent-region
	   (car xmltok-dependent-regions))
    (setq xmltok-dependent-regions
	  (cdr xmltok-dependent-regions))))

(defun rng-mark-xmltok-dependent-region (fun start end &rest args)
  (let ((overlay (make-overlay start end nil t t)))
    (overlay-put overlay 'category 'rng-dependent)
    (overlay-put overlay 'rng-funargs (cons fun args))))

(put 'rng-dependent 'evaporate t)
(put 'rng-dependent 'modification-hooks '(rng-dependent-region-changed))
(put 'rng-dependent 'insert-behind-hooks '(rng-dependent-region-changed))

(defun rng-dependent-region-changed (overlay
				     after-p
				     change-start
				     change-end
				     &optional pre-change-length)
  (when (and after-p
	     ;; Emacs sometimes appears to call deleted overlays
	     (overlay-start overlay)
	     (let ((funargs (overlay-get overlay 'rng-funargs)))
	       (save-match-data
		 (save-excursion
		   (save-restriction
		     (widen)
		     (apply (car funargs)
			    (append (list change-start
					  change-end
					  pre-change-length
					  (overlay-start overlay)
					  (overlay-end overlay))
				    (cdr funargs))))))))
    (rng-after-change-function (overlay-start overlay)
			       change-end
			       (+ pre-change-length
				  (- (overlay-start overlay)
				     change-start)))
    (delete-overlay overlay)))

;;; Error state

(defun rng-mark-xmltok-errors ()
  (while xmltok-errors
    (let ((err (car xmltok-errors)))
      (rng-mark-not-well-formed (xmltok-error-message err)
				(xmltok-error-start err)
				(xmltok-error-end err)))
    (setq xmltok-errors (cdr xmltok-errors))))

(defun rng-mark-invalid (message beg end)
  (rng-mark-error message beg end))

(defun rng-mark-not-well-formed (message beg end)
  ;; Don't try to validate further
  ;;(rng-set-match-state rng-not-allowed-ipattern)
  (rng-mark-error message beg end))

(defun rng-mark-error (message beg end)
  (unless rng-parsing-for-state
    (let ((overlays (overlays-in beg end)))
      (while (and overlays message)
	(let ((o (car overlays)))
	  (when (and (eq (overlay-get o 'category) 'rng-error)
		     (= (overlay-start o) beg)
		     (= (overlay-end o) end))
	    (overlay-put o
			 'help-echo
			 (concat (overlay-get o 'help-echo)
				 "\n"
				 message))
	    (setq message nil)))
	(setq overlays (cdr overlays))))
    (when message
      (let ((inhibit-quit t))
	(setq rng-error-count (1+ rng-error-count))
	(let ((overlay
	       (make-overlay beg end nil t
			     ;; Need to make the rear delimiter advance
			     ;; with the front delimiter when the overlay
			     ;; is empty, otherwise the front delimiter
			     ;; will move past the rear delimiter.
			     (= beg end))))
	  ;; Ensure when we have two overlapping messages, the help-echo
	  ;; of the one that starts first is shown
	  (overlay-put overlay 'priority beg)
	  (overlay-put overlay 'category 'rng-error)
	  (overlay-put overlay 'help-echo message))))))

(put 'rng-error 'face 'rng-error)
(put 'rng-error 'modification-hooks '(rng-error-modified))

;; If we don't do this, then the front delimiter can move
;; past the end delimiter.
(defun rng-error-modified (overlay after-p beg end &optional pre-change-len)
  (when (and after-p
	     (overlay-start overlay)	; check not deleted
	     (>= (overlay-start overlay)
		 (overlay-end overlay)))
    (let ((inhibit-quit t))
      (delete-overlay overlay)
      (setq rng-error-count (1- rng-error-count)))))

(defun rng-echo-area-clear-function ()
  (setq rng-message-overlay-current nil))

;;; Error navigation

(defun rng-maybe-echo-error-at-point ()
  (when (or (not (current-message))
	    (rng-current-message-from-error-overlay-p))
    (rng-error-overlay-message (rng-error-overlay-after (point)))))

(defun rng-error-overlay-after (pos)
  (let ((overlays (overlays-in pos (1+ pos)))
	(best nil))
    (while overlays
      (let ((overlay (car overlays)))
	(when (and (eq (overlay-get overlay 'category)
		       'rng-error)
		   (or (not best)
		       (< (overlay-start best)
			  (overlay-start overlay))))
	  (setq best overlay)))
      (setq overlays (cdr overlays)))
    best))

(defun rng-first-error ()
  "Go to the first validation error.
Turn on `rng-validate-mode' if it is not already on."
  (interactive)
  (or rng-validate-mode (rng-validate-mode))
  (rng-do-some-validation)
  (let ((err (rng-find-next-error-overlay (1- (point-min)))))
    (if err
	(rng-goto-error-overlay err)
      (let ((pos (save-excursion
		   (goto-char (point-min))
		   (rng-next-error 1))))
	(when pos
	  (goto-char pos))))))

(defun rng-mouse-first-error (event)
  "Go to the first validation error from a mouse click."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (rng-first-error))

(defun rng-next-error (arg)
  "Go to the next validation error after point.
Turn on `rng-validate-mode' if it is not already on.
A prefix ARG specifies how many errors to move.
A negative ARG moves backwards.  Just \\[universal-argument] as a prefix
means goto the first error."
  (interactive "P")
  (if (consp arg)
      (rng-first-error)
    (or rng-validate-mode (rng-validate-mode))
    (setq arg (prefix-numeric-value arg))
    (if (< arg 0)
	(rng-previous-error-1 (- arg))
      (rng-next-error-1 arg))))

(defun rng-previous-error (arg)
  "Go to the previous validation error before point.
Turn on `rng-validate-mode' if it is not already on.
A prefix ARG specifies how many errors to move.
A negative ARG moves forwards.  Just \\[universal-argument] as a prefix
means goto the first error."
  (interactive "P")
  (if (consp arg)
      (rng-first-error)
    (or rng-validate-mode (rng-validate-mode))
    (setq arg (prefix-numeric-value arg))
    (if (< arg 0)
	(rng-next-error-1 (- arg))
      (rng-previous-error-1 arg))))

(defun rng-next-error-1 (arg)
  (let* ((pos (point))
	 err last-err)
    (while (and (> arg 0)
		(setq err (rng-find-next-error-overlay pos)))
      (setq arg (1- arg))
      (setq last-err err)
      (setq pos (overlay-start err)))
    (when (> arg 0)
      (setq pos (max pos (1- rng-validate-up-to-date-end)))
      (when (< rng-validate-up-to-date-end (point-max))
	(message "Parsing...")
	(while (let ((more-to-do (rng-do-some-validation)))
		 (while (and (> arg 0)
			     (setq err (rng-find-next-error-overlay pos)))
		   (setq arg (1- arg))
		   (setq last-err err)
		   (setq pos (overlay-start err)))
		 (when (and (> arg 0)
			    more-to-do
			    (< rng-validate-up-to-date-end (point-max)))
		   ;; Display percentage validated.
		   (force-mode-line-update)
		   ;; Force redisplay but don't allow idle timers to run.
		   (let ((timer-idle-list nil))
		     (sit-for 0))
		   (setq pos
			 (max pos (1- rng-validate-up-to-date-end)))
		   t)))))
    (if last-err
	(rng-goto-error-overlay last-err)
      (message "No more errors")
      nil)))

(defun rng-previous-error-1 (arg)
  (let* ((pos (point))
	 err last-err)
    (while (and (> arg 0)
		(setq err (rng-find-previous-error-overlay pos)))
      (setq pos (overlay-start err))
      (setq last-err err)
      (setq arg (1- arg)))
    (when (and (> arg 0)
	       (< rng-validate-up-to-date-end (min pos (point-max))))
      (message "Parsing...")
      (while (and (rng-do-some-validation)
		  (< rng-validate-up-to-date-end (min pos (point-max))))
	(force-mode-line-update)
	;; Force redisplay but don't allow idle timers to run.
	(let ((timer-idle-list nil))
	  (sit-for 0)))
      (while (and (> arg 0)
		  (setq err (rng-find-previous-error-overlay pos)))
	(setq pos (overlay-start err))
	(setq last-err err)
	(setq arg (1- arg))))
    (if last-err
	(rng-goto-error-overlay last-err)
      (message "No previous errors")
      nil)))

(defun rng-goto-error-overlay (err)
  "Goto the start of error overlay ERR and print its message."
  (goto-char (overlay-start err))
  (setq rng-message-overlay-inhibit-point nil)
  (rng-error-overlay-message err))

(defun rng-error-overlay-message (err)
  (if err
      (unless (or (and (eq rng-message-overlay-inhibit-point (point))
		       (eq rng-message-overlay err))
		  (= (point-max) 1))
	(message "%s" (overlay-get err 'help-echo))
	(setq rng-message-overlay-current t)
	(setq rng-message-overlay-inhibit-point (point)))
    (when (rng-current-message-from-error-overlay-p)
      (message nil))
    (setq rng-message-overlay-inhibit-point nil))
  (setq rng-message-overlay err))

(defun rng-current-message-from-error-overlay-p ()
  (and rng-message-overlay-current
       rng-message-overlay
       (equal (overlay-get rng-message-overlay 'help-echo)
	      (current-message))))

(defun rng-find-next-error-overlay (pos)
  "Return the overlay for the next error starting after POS.
Return nil if there is no such overlay or it is out of date.
Do not do any additional validation."
  (when rng-error-count
    (let (done found overlays)
      (while (not done)
	(cond (overlays
	       (let ((overlay (car overlays)))
		 (setq overlays (cdr overlays))
		 (when (and (eq (overlay-get overlay 'category) 'rng-error)
			    ;; Is it the first?
			    (= (overlay-start overlay) pos)
			    ;; Is it up to date?
			    (<= (overlay-end overlay)
				rng-validate-up-to-date-end))
		   (setq done t)
		   (setq found overlay))))
	      ((or (= pos (point-max))
		   (> (setq pos (next-overlay-change pos))
		      rng-validate-up-to-date-end))
	       (setq done t))
	      (t (setq overlays (overlays-in pos (1+ pos))))))
      found)))

(defun rng-find-previous-error-overlay (pos)
  "Return the overlay for the last error starting before POS.
Return nil if there is no such overlay or it is out of date.
Do not do any additional validation."
  (when (and rng-error-count
	     (<= pos rng-validate-up-to-date-end))
    (let (done found overlays)
      (while (not done)
	(cond (overlays
	       (let ((overlay (car overlays)))
		 (setq overlays (cdr overlays))
		 (when (and (eq (overlay-get overlay 'category) 'rng-error)
			    ;; Is it the first?
			    (= (overlay-start overlay) pos))
		   (setq done t)
		   (setq found overlay))))
	      ((= pos (point-min))
	       (setq done t))
	      (t
	       (setq pos (previous-overlay-change pos))
	       (setq overlays (overlays-in pos (1+ pos))))))
      found)))

;;; Parsing

(defun rng-forward (&optional limit)
  "Move forward over one or more tokens updating the state.
If LIMIT is nil, stop after tags.
If LIMIT is non-nil, stop when end of last token parsed is >= LIMIT.
Return nil at end of buffer, t otherwise."
  (let (type)
    (while (progn
	     (setq type (xmltok-forward))
	     (rng-clear-overlays xmltok-start (point))
	     (let ((continue
		    (cond ((eq type 'start-tag)
			   (rng-process-start-tag 'start-tag)
			   nil)
			  ((eq type 'end-tag)
			   (rng-process-end-tag)
			   nil)
			  ((eq type 'empty-element)
			   (rng-process-start-tag 'empty-element)
			   nil)
			  ((eq type 'space)
			   (rng-process-text xmltok-start nil t)
			   t)
			  ((eq type 'data)
			   (rng-process-text xmltok-start nil nil)
			   t)
			  ((memq type '(entity-ref char-ref))
			   (cond (xmltok-replacement
				  (rng-process-text xmltok-start
						    nil
						    'maybe
						    xmltok-replacement))
				 ((eq type 'char-ref)
				  (rng-process-unknown-char))
				 (t
				  (rng-process-unknown-entity)))
			   t)
			  ((eq type 'cdata-section)
			   (rng-process-text (+ xmltok-start 9)	; "<![CDATA["
					     (- (point) 3) ; "]]>"
					     'maybe)
			   t)
			  ((eq type 'partial-start-tag)
			   (rng-process-start-tag 'partial-start-tag)
			   t)
			  ((eq type 'partial-empty-element)
			   (rng-process-start-tag 'empty-element)
			   t)
			  ((eq type 'partial-end-tag)
			   (rng-process-end-tag 'partial)
			   t)
			  (t type))))
	       (if limit
		   (< (point) limit)
		 continue))))
    (and type t)))

(defun rng-process-start-tag (tag-type)
  "TAG-TYPE is `start-tag' for a start-tag, `empty-element' for
an empty element.  `partial-empty-element' should be passed
as empty-element."
  (and rng-collecting-text (rng-flush-text))
  (setq rng-collecting-text nil)
  (setq rng-pending-contents nil)
  (rng-process-namespaces)
  (let ((tag (rng-process-tag-name)))
    (rng-process-attributes)
    ;; set the state appropriately
    (cond ((eq tag-type 'empty-element)
	   (rng-process-start-tag-close)
	   ;; deal with missing content with empty element
	   (when (not (rng-match-empty-content))
	     (rng-match-after)
	     (rng-mark-start-tag-close "Empty content not allowed"))
	   (nxml-ns-pop-state))
	  ((eq tag-type 'start-tag)
	   (rng-process-start-tag-close)
	   (setq rng-collecting-text (rng-match-text-typed-p))
	   (rng-push-tag tag))
	  ((eq tag-type 'partial-start-tag)
	   (rng-process-start-tag-close)
	   (rng-match-after)
	   (nxml-ns-pop-state)))))

(defun rng-process-namespaces ()
  (let ((nsatts xmltok-namespace-attributes)
	prefixes)
    (nxml-ns-push-state)
    (while nsatts
      (let* ((att (car nsatts))
	     (value (xmltok-attribute-value att)))
	(when value
	  (let ((ns (nxml-make-namespace value))
		(prefix (and (xmltok-attribute-prefix att)
			     (xmltok-attribute-local-name att))))
	    (cond ((member prefix prefixes)
		   (rng-mark-invalid "Duplicate namespace declaration"
				     (xmltok-attribute-name-start att)
				     (xmltok-attribute-name-end att)))
		  ((not prefix)
		   (nxml-ns-set-default ns))
		  (ns
		   (nxml-ns-set-prefix prefix ns))
		  (t
		   ;; cannot have xmlns:foo=""
		   (rng-mark-invalid "Namespace prefix cannot be undeclared"
				     (1- (xmltok-attribute-value-start att))
				     (1+ (xmltok-attribute-value-end att)))))
	    (setq prefixes (cons prefix prefixes)))))
      (setq nsatts (cdr nsatts)))))

(defun rng-process-tag-name ()
  (let* ((prefix (xmltok-start-tag-prefix))
	 (local-name (xmltok-start-tag-local-name))
	 (name
	  (if prefix
	      (let ((ns (nxml-ns-get-prefix prefix)))
		(cond (ns (cons ns local-name))
		      ((and (setq ns
				  (rng-match-infer-start-tag-namespace
				   local-name))
			    (rng-match-start-tag-open (cons ns local-name)))
		       (nxml-ns-set-prefix prefix ns)
		       (rng-mark-start-tag-close "Missing xmlns:%s=\"%s\""
						 prefix
						 (nxml-namespace-name ns))
		       nil)
		      (t
		       (rng-recover-bad-element-prefix)
		       nil)))
	    (cons (nxml-ns-get-default) local-name))))
    (when (and name
	       (not (rng-match-start-tag-open name)))
      (unless (and (not (car name))
		   (let ((ns (rng-match-infer-start-tag-namespace (cdr name))))
		     (and ns
			  (rng-match-start-tag-open (cons ns local-name))
			  (progn
			    (nxml-ns-set-default ns)
			    ;; XXX need to check we don't have xmlns=""
			    (rng-mark-start-tag-close "Missing xmlns=\"%s\""
						      (nxml-namespace-name ns))
			    t))))
	(rng-recover-start-tag-open name)))
    (cons prefix local-name)))

(defun rng-process-attributes ()
  (let ((atts xmltok-attributes)
	names)
    (while atts
      (let* ((att (car atts))
	     (prefix (xmltok-attribute-prefix att))
	     (local-name (xmltok-attribute-local-name att))
	     (name
	      (if prefix
		  (let ((ns (nxml-ns-get-prefix prefix)))
		    (and ns
			 (cons ns local-name)))
		(cons nil local-name))))
	(cond ((not name)
	       (rng-recover-bad-attribute-prefix att))
	      ((member name names)
	       (rng-recover-duplicate-attribute-name att))
	      ((not (rng-match-attribute-name name))
	       (rng-recover-attribute-name att))
	      ((rng-match-text-typed-p)
	       (let ((value (xmltok-attribute-value att)))
		 (if value
		     (or (rng-match-attribute-value value)
			 (rng-recover-attribute-value att))
		   (rng-match-after))))
	      (t (or (rng-match-end-tag)
		     (error "Internal error:\
 invalid on untyped attribute value"))))
	(setq names (cons name names)))
      (setq atts (cdr atts)))))

(defun rng-process-start-tag-close ()
  ;; deal with missing attributes
  (unless (rng-match-start-tag-close)
    (rng-mark-start-tag-close (rng-missing-attributes-message))
    (rng-match-ignore-attributes)))

(defun rng-mark-start-tag-close (&rest args)
  (when (not (eq xmltok-type 'partial-start-tag))
    (rng-mark-invalid (apply 'format args)
		      (- (point)
			 (if (eq xmltok-type 'empty-element)
			     2
			   1))
		      (point))))

(defun rng-recover-bad-element-prefix ()
  (rng-mark-invalid "Prefix not declared"
		    (1+ xmltok-start)
		    xmltok-name-colon)
  (rng-match-unknown-start-tag-open))

(defun rng-recover-bad-attribute-prefix (att)
  (rng-mark-invalid "Prefix not declared"
		    (xmltok-attribute-name-start att)
		    (xmltok-attribute-name-colon att)))

(defun rng-recover-duplicate-attribute-name (att)
  (rng-mark-invalid "Duplicate attribute"
		    (xmltok-attribute-name-start att)
		    (xmltok-attribute-name-end att)))

(defun rng-recover-start-tag-open (name)
  (let ((required (rng-match-required-element-name)))
    (cond ((and required
		(rng-match-start-tag-open required)
		(rng-match-after)
		(rng-match-start-tag-open name))
	   (rng-mark-invalid (concat "Missing element "
				     (rng-quote-string
				      (rng-name-to-string required)))
			     xmltok-start
			     (1+ xmltok-start)))
	  ((and (rng-match-optionalize-elements)
		(rng-match-start-tag-open name))
	   (rng-mark-invalid "Required elements missing"
			     xmltok-start
			     (1+ xmltok-start)))
	  ((rng-match-out-of-context-start-tag-open name)
	   (rng-mark-invalid "Element not allowed in this context"
			     (1+ xmltok-start)
			     xmltok-name-end))
	  (t
	   (rng-match-unknown-start-tag-open)
	   (rng-mark-invalid "Unknown element"
			     (1+ xmltok-start)
			     xmltok-name-end)))))

(defun rng-recover-attribute-value (att)
  (let ((start (xmltok-attribute-value-start att))
	(end (xmltok-attribute-value-end att)))
    (if (= start end)
	(rng-mark-invalid "Empty attribute value invalid" start (1+ end))
      (rng-mark-invalid "Attribute value invalid" start end)))
  (rng-match-after))

(defun rng-recover-attribute-name (att)
  (rng-mark-invalid "Attribute not allowed"
		    (xmltok-attribute-name-start att)
		    (xmltok-attribute-name-end att)))

(defun rng-missing-attributes-message ()
  (let ((required-attributes
	 (rng-match-required-attribute-names)))
    (cond ((not required-attributes)
	   "Required attributes missing")
	  ((not (cdr required-attributes))
	   (concat "Missing attribute "
		   (rng-quote-string
		    (rng-name-to-string (car required-attributes) t))))
	  (t
	   (concat "Missing attributes "
		   (mapconcat (lambda (nm)
				(rng-quote-string
				 (rng-name-to-string nm t)))
			      required-attributes
			      ", "))))))

(defun rng-process-end-tag (&optional partial)
  (cond ((not rng-open-elements)
	 (rng-mark-not-well-formed "Extra end-tag"
				   xmltok-start
				   (point)))
	((or partial
	     (equal (cons (xmltok-end-tag-prefix)
			  (xmltok-end-tag-local-name))
		    (car rng-open-elements)))
	 (rng-end-element))
	(t (rng-recover-mismatched-end-tag))))

(defun rng-end-element ()
  (if rng-collecting-text
      (let ((contents (rng-contents-string)))
	(cond ((not contents) (rng-match-after))
	      ((not (rng-match-element-value contents))
	       (let* ((region (rng-contents-region)))
		 (if (not region)
		     (rng-mark-invalid "Empty content not allowed"
				       xmltok-start
				       (+ xmltok-start 2))
		   (rng-mark-invalid "Invalid data"
				     (car region)
				     (cdr region))))
	       (rng-match-after)))
	(setq rng-collecting-text nil)
	(setq rng-pending-contents nil))
    (unless (rng-match-end-tag)
       (rng-mark-invalid (rng-missing-element-message)
			 xmltok-start
			 (+ xmltok-start 2))
       (rng-match-after)))
  (nxml-ns-pop-state)
  (when (eq (car rng-open-elements) t)
    (rng-pop-tag))
  (rng-pop-tag))

(defun rng-missing-element-message ()
  (let ((element (rng-match-required-element-name)))
    (if element
	(concat "Missing element "
		(rng-quote-string (rng-name-to-string element)))
      "Required child elements missing")))

(defun rng-recover-mismatched-end-tag ()
  (let* ((name (cons (xmltok-end-tag-prefix)
		     (xmltok-end-tag-local-name))))
    (cond ((member name (cdr rng-open-elements))
	   (let* ((suppress-error (eq (car rng-open-elements) t))
		  missing top)
	     (while (progn
		      (setq top (car rng-open-elements))
		      (rng-pop-tag)
		      (unless (eq top t)
			(setq missing (cons top missing))
			(nxml-ns-pop-state)
			(rng-match-after))
		      (not (equal top name))))
	     (unless suppress-error
	       (rng-mark-missing-end-tags (cdr missing)))))
	  ((rng-match-empty-before-p)
	   (rng-mark-mismatched-end-tag)
	   (rng-end-element))
	  (t (rng-mark-mismatched-end-tag)
	     (setq rng-open-elements
		   (cons t rng-open-elements))))))

(defun rng-mark-missing-end-tags (missing)
  (rng-mark-not-well-formed
   (format "Missing end-tag%s %s"
	   (if (null (cdr missing)) "" "s")
	   (mapconcat (lambda (name)
			(rng-quote-string
			 (if (car name)
			     (concat (car name)
				     ":"
				     (cdr name))
			   (cdr name))))
		      missing
		      ", "))
   xmltok-start
   (+ xmltok-start 2)))

(defun rng-mark-mismatched-end-tag ()
  (rng-mark-not-well-formed "Mismatched end-tag"
			    (+ xmltok-start 2)
			    xmltok-name-end))

(defun rng-push-tag (prefix-local-name)
  (setq rng-open-elements
	(cons prefix-local-name rng-open-elements)))

(defun rng-pop-tag ()
  (setq rng-open-elements (cdr rng-open-elements)))

(defun rng-contents-string ()
  (let ((contents rng-pending-contents))
    (cond ((not contents) "")
	  ((memq nil contents) nil)
	  ((not (cdr contents))
	   (rng-segment-string (car contents)))
	  (t (apply 'concat
		    (nreverse (mapcar 'rng-segment-string
				      contents)))))))

(defun rng-segment-string (segment)
  (or (car segment)
      (apply 'buffer-substring-no-properties
	     (cdr segment))))

(defun rng-segment-blank-p (segment)
  (if (car segment)
      (rng-blank-p (car segment))
    (apply 'rng-region-blank-p
	   (cdr segment))))

(defun rng-contents-region ()
  (if (null rng-pending-contents)
      nil
    (let* ((contents rng-pending-contents)
	   (head (cdar contents))
	   (start (car head))
	   (end (cadr head)))
      (while (setq contents (cdr contents))
	(setq start (car (cdar contents))))
      (cons start end))))

(defun rng-process-text (start end whitespace &optional value)
  "Process characters between position START and END as text.
END nil means point.  WHITESPACE t means known to be whitespace, nil
means known not to be, anything else means unknown whether whitespace
or not.  END must not be nil if WHITESPACE is neither t nor nil.
VALUE is a string or nil; nil means the value is equal to the
string between START and END."
  (cond (rng-collecting-text
	 (setq rng-pending-contents (cons (list value start (or end (point)))
					  rng-pending-contents)))
	((not (or (and whitespace
		       (or (eq whitespace t)
			   (if value
			       (rng-blank-p value)
			     (rng-region-blank-p start end))))
		  (rng-match-mixed-text)))
	 (rng-mark-invalid "Text not allowed" start (or end (point))))))

(defun rng-process-unknown-char ()
  (when rng-collecting-text
    (setq rng-pending-contents
	  (cons nil rng-pending-contents))))

(defun rng-process-unknown-entity ()
  (rng-process-unknown-char)
  (rng-match-optionalize-elements))

(defun rng-region-blank-p (beg end)
  (save-excursion
    (goto-char beg)
    (= (skip-chars-forward " \n\r\t" end)
       (- end beg))))

(defun rng-flush-text ()
  (while rng-pending-contents
    (let ((segment (car rng-pending-contents)))
      (unless (or (rng-segment-blank-p segment)
		  (rng-match-mixed-text))
	(let ((region (cdr segment)))
	  (rng-mark-invalid "In this context text cannot be mixed with elements"
			    (car region)
			    (cadr region)))))
    (setq rng-pending-contents (cdr rng-pending-contents))))

(defun rng-process-end-document ()
  ;; this is necessary to clear empty overlays at (point-max)
  (rng-clear-overlays (point) (point))
  (let ((start (save-excursion
		 (skip-chars-backward " \t\r\n")
		 (point))))
    (cond (rng-open-elements
	   (unless (eq (car rng-open-elements) t)
	     (rng-mark-not-well-formed "Missing end-tag"
				       start
				       (point))))
	  ((not (rng-match-nullable-p))
	   (rng-mark-not-well-formed "No document element"
				     start
				     (point))))))

(defun rng-process-encoding-name (beg end)
  (unless (let ((charset (buffer-substring-no-properties beg end)))
	    (or (nxml-mime-charset-coding-system charset)
		(string= (downcase charset) "utf-16")))
    (rng-mark-not-well-formed "Unsupported encoding" beg end)))

(defun rng-name-to-string (name &optional attributep)
  (let ((ns (car name))
	(local-name (cdr name)))
    (if (or (not ns)
	    (and (not attributep)
		 (eq (nxml-ns-get-default) ns)))
	local-name
      (let ((prefix (nxml-ns-prefix-for ns)))
	(if prefix
	    (concat prefix ":" local-name)
	  (concat "{" (symbol-name ns) "}" local-name))))))

(provide 'rng-valid)

;;; rng-valid.el ends here
