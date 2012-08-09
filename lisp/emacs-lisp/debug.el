;;; debug.el --- debuggers and related commands for Emacs

;; Copyright (C) 1985-1986, 1994, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: lisp, tools, maint

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

;; This is a major mode documented in the Emacs Lisp manual.

;;; Code:

(require 'button)

(defgroup debugger nil
  "Debuggers and related commands for Emacs."
  :prefix "debugger-"
  :group 'debug)

(defcustom debugger-mode-hook nil
  "Hooks run when `debugger-mode' is turned on."
  :type 'hook
  :group 'debugger
  :version "20.3")

(defcustom debugger-batch-max-lines 40
  "Maximum lines to show in debugger buffer in a noninteractive Emacs.
When the debugger is entered and Emacs is running in batch mode,
if the backtrace text has more than this many lines,
the middle is discarded, and just the beginning and end are displayed."
  :type 'integer
  :group 'debugger
  :version "21.1")

(defvar debug-function-list nil
  "List of functions currently set for debug on entry.")

(defvar debugger-step-after-exit nil
  "Non-nil means \"single-step\" after the debugger exits.")

(defvar debugger-value nil
  "This is the value for the debugger to return, when it returns.")

(defvar debugger-old-buffer nil
  "This is the buffer that was current when the debugger was entered.")

(defvar debugger-previous-backtrace nil
  "The contents of the previous backtrace (including text properties).
This is to optimize `debugger-make-xrefs'.")

(defvar debugger-outer-match-data)
(defvar debugger-outer-load-read-function)
(defvar debugger-outer-overriding-local-map)
(defvar debugger-outer-overriding-terminal-local-map)
(defvar debugger-outer-track-mouse)
(defvar debugger-outer-last-command)
(defvar debugger-outer-this-command)
;; unread-command-char is obsolete,
;; but we still save and restore it
;; in case some user program still tries to set it.
(defvar debugger-outer-unread-command-char)
(defvar debugger-outer-unread-command-events)
(defvar debugger-outer-unread-post-input-method-events)
(defvar debugger-outer-last-input-event)
(defvar debugger-outer-last-command-event)
(defvar debugger-outer-last-nonmenu-event)
(defvar debugger-outer-last-event-frame)
(defvar debugger-outer-standard-input)
(defvar debugger-outer-standard-output)
(defvar debugger-outer-inhibit-redisplay)
(defvar debugger-outer-cursor-in-echo-area)
(defvar debugger-will-be-back nil
  "Non-nil if we expect to get back in the debugger soon.")

(defvar inhibit-debug-on-entry nil
  "Non-nil means that debug-on-entry is disabled.")

(defvar debugger-jumping-flag nil
  "Non-nil means that debug-on-entry is disabled.
This variable is used by `debugger-jump', `debugger-step-through',
and `debugger-reenable' to temporarily disable debug-on-entry.")

(defvar inhibit-trace)                  ;Not yet implemented.

(defvar debugger-args nil
  "Arguments with which the debugger was called.
It is a list expected to take the form (CAUSE . REST)
where CAUSE can be:
- debug: called for entry to a flagged function.
- t: called because of debug-on-next-call.
- lambda: same thing but via `funcall'.
- exit: called because of exit of a flagged function.
- error: called because of `debug-on-error'.")

;;;###autoload
(setq debugger 'debug)
;;;###autoload
(defun debug (&rest debugger-args)
  "Enter debugger.  \\<debugger-mode-map>`\\[debugger-continue]' returns from the debugger.
Arguments are mainly for use when this is called from the internals
of the evaluator.

You may call with no args, or you may pass nil as the first arg and
any other args you like.  In that case, the list of args after the
first will be printed into the backtrace buffer."
  (interactive)
  (if inhibit-redisplay
      ;; Don't really try to enter debugger within an eval from redisplay.
      debugger-value
    (unless noninteractive
      (message "Entering debugger..."))
    (let (debugger-value
	  (debug-on-error nil)
	  (debug-on-quit nil)
	  (debugger-previous-state
           (if (get-buffer "*Backtrace*")
               (with-current-buffer (get-buffer "*Backtrace*")
                 (list major-mode (buffer-string)))))
	  (debugger-buffer (get-buffer-create "*Backtrace*"))
	  (debugger-old-buffer (current-buffer))
	  (debugger-step-after-exit nil)
          (debugger-will-be-back nil)
	  ;; Don't keep reading from an executing kbd macro!
	  (executing-kbd-macro nil)
	  ;; Save the outer values of these vars for the `e' command
	  ;; before we replace the values.
	  (debugger-outer-match-data (match-data))
	  (debugger-outer-load-read-function load-read-function)
	  (debugger-outer-overriding-local-map overriding-local-map)
	  (debugger-outer-overriding-terminal-local-map
	   overriding-terminal-local-map)
	  (debugger-outer-track-mouse track-mouse)
	  (debugger-outer-last-command last-command)
	  (debugger-outer-this-command this-command)
	  (debugger-outer-unread-command-char
	   (with-no-warnings unread-command-char))
	  (debugger-outer-unread-command-events unread-command-events)
	  (debugger-outer-unread-post-input-method-events
	   unread-post-input-method-events)
	  (debugger-outer-last-input-event last-input-event)
	  (debugger-outer-last-command-event last-command-event)
	  (debugger-outer-last-nonmenu-event last-nonmenu-event)
	  (debugger-outer-last-event-frame last-event-frame)
	  (debugger-outer-standard-input standard-input)
	  (debugger-outer-standard-output standard-output)
	  (debugger-outer-inhibit-redisplay inhibit-redisplay)
	  (debugger-outer-cursor-in-echo-area cursor-in-echo-area)
	  (debugger-with-timeout-suspend (with-timeout-suspend)))
      ;; Set this instead of binding it, so that `q'
      ;; will not restore it.
      (setq overriding-terminal-local-map nil)
      ;; Don't let these magic variables affect the debugger itself.
      (let ((last-command nil) this-command track-mouse
	    (inhibit-trace t)
	    (inhibit-debug-on-entry t)
	    unread-command-events
	    unread-post-input-method-events
	    last-input-event last-command-event last-nonmenu-event
	    last-event-frame
	    overriding-local-map
	    load-read-function
	    ;; If we are inside a minibuffer, allow nesting
	    ;; so that we don't get an error from the `e' command.
	    (enable-recursive-minibuffers
	     (or enable-recursive-minibuffers (> (minibuffer-depth) 0)))
	    (standard-input t) (standard-output t)
	    inhibit-redisplay
	    (cursor-in-echo-area nil))
	(unwind-protect
	    (save-excursion
	      (save-window-excursion
		(with-no-warnings
		  (setq unread-command-char -1))
		(when (eq (car debugger-args) 'debug)
		  ;; Skip the frames for backtrace-debug, byte-code,
		  ;; and implement-debug-on-entry.
		  (backtrace-debug 4 t)
		  ;; Place an extra debug-on-exit for macro's.
		  (when (eq 'lambda (car-safe (cadr (backtrace-frame 4))))
		    (backtrace-debug 5 t)))
                (pop-to-buffer debugger-buffer)
		(debugger-mode)
		(debugger-setup-buffer debugger-args)
		(when noninteractive
		  ;; If the backtrace is long, save the beginning
		  ;; and the end, but discard the middle.
		  (when (> (count-lines (point-min) (point-max))
			   debugger-batch-max-lines)
		    (goto-char (point-min))
		    (forward-line (/ 2 debugger-batch-max-lines))
		    (let ((middlestart (point)))
		      (goto-char (point-max))
		      (forward-line (- (/ 2 debugger-batch-max-lines)
				       debugger-batch-max-lines))
		      (delete-region middlestart (point)))
		    (insert "...\n"))
		  (goto-char (point-min))
		  (message "%s" (buffer-string))
		  (kill-emacs -1))
		(message "")
		(let ((standard-output nil)
		      (buffer-read-only t))
		  (message "")
		  ;; Make sure we unbind buffer-read-only in the right buffer.
		  (save-excursion
		    (recursive-edit)))))
	  ;; Kill or at least neuter the backtrace buffer, so that users
	  ;; don't try to execute debugger commands in an invalid context.
	  (if (get-buffer-window debugger-buffer 0)
	      ;; Still visible despite the save-window-excursion?  Maybe it
	      ;; it's in a pop-up frame.  It would be annoying to delete and
	      ;; recreate it every time the debugger stops, so instead we'll
	      ;; erase it (and maybe hide it) but keep it alive.
	      (with-current-buffer debugger-buffer
		(with-selected-window (get-buffer-window debugger-buffer 0)
                  (when (and (window-dedicated-p (selected-window))
                             (not debugger-will-be-back))
                    ;; If the window is not dedicated, burying the buffer
                    ;; will mean that the frame created for it is left
                    ;; around showing some random buffer, and next time we
                    ;; pop to the debugger buffer we'll create yet
                    ;; another frame.
                    ;; If debugger-will-be-back is non-nil, the frame
                    ;; would need to be de-iconified anyway immediately
                    ;; after when we re-enter the debugger, so iconifying it
                    ;; here would cause flashing.
                    ;; Drew Adams is not happy with this: he wants to frame
                    ;; to be left at the top-level, still working on how
                    ;; best to do that.
                    (bury-buffer))))
            (unless debugger-previous-state
              (kill-buffer debugger-buffer)))
          ;; Restore the previous state of the debugger-buffer, in case we were
          ;; in a recursive invocation of the debugger.
          (when (buffer-live-p debugger-buffer)
            (with-current-buffer debugger-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (if (null debugger-previous-state)
                    (fundamental-mode)
                  (insert (nth 1 debugger-previous-state))
                  (funcall (nth 0 debugger-previous-state))))))
	  (with-timeout-unsuspend debugger-with-timeout-suspend)
	  (set-match-data debugger-outer-match-data)))
      ;; Put into effect the modified values of these variables
      ;; in case the user set them with the `e' command.
      (setq load-read-function debugger-outer-load-read-function)
      (setq overriding-local-map debugger-outer-overriding-local-map)
      (setq overriding-terminal-local-map
	    debugger-outer-overriding-terminal-local-map)
      (setq track-mouse debugger-outer-track-mouse)
      (setq last-command debugger-outer-last-command)
      (setq this-command debugger-outer-this-command)
      (with-no-warnings
	(setq unread-command-char debugger-outer-unread-command-char))
      (setq unread-command-events debugger-outer-unread-command-events)
      (setq unread-post-input-method-events
	    debugger-outer-unread-post-input-method-events)
      (setq last-input-event debugger-outer-last-input-event)
      (setq last-command-event debugger-outer-last-command-event)
      (setq last-nonmenu-event debugger-outer-last-nonmenu-event)
      (setq last-event-frame debugger-outer-last-event-frame)
      (setq standard-input debugger-outer-standard-input)
      (setq standard-output debugger-outer-standard-output)
      (setq inhibit-redisplay debugger-outer-inhibit-redisplay)
      (setq cursor-in-echo-area debugger-outer-cursor-in-echo-area)
      (setq debug-on-next-call debugger-step-after-exit)
      debugger-value)))

(defun debugger-setup-buffer (debugger-args)
  "Initialize the `*Backtrace*' buffer for entry to the debugger.
That buffer should be current already."
  (setq buffer-read-only nil)
  (erase-buffer)
  (set-buffer-multibyte t)		;Why was it nil ?  -stef
  (setq buffer-undo-list t)
  (let ((standard-output (current-buffer))
	(print-escape-newlines t)
	(print-level 8)
	(print-length 50))
    (backtrace))
  (goto-char (point-min))
  (delete-region (point)
		 (progn
		   (search-forward "\n  debug(")
		   (forward-line (if (eq (car debugger-args) 'debug)
				     2	; Remove implement-debug-on-entry frame.
				   1))
		   (point)))
  (insert "Debugger entered")
  ;; lambda is for debug-on-call when a function call is next.
  ;; debug is for debug-on-entry function called.
  (pcase (car debugger-args)
    ((or `lambda `debug)
     (insert "--entering a function:\n"))
    ;; Exiting a function.
    (`exit
     (insert "--returning value: ")
     (setq debugger-value (nth 1 debugger-args))
     (prin1 debugger-value (current-buffer))
     (insert ?\n)
     (delete-char 1)
     (insert ? )
     (beginning-of-line))
    ;; Debugger entered for an error.
    (`error
     (insert "--Lisp error: ")
     (prin1 (nth 1 debugger-args) (current-buffer))
     (insert ?\n))
    ;; debug-on-call, when the next thing is an eval.
    (`t
     (insert "--beginning evaluation of function call form:\n"))
    ;; User calls debug directly.
    (_
     (insert ": ")
     (prin1 (if (eq (car debugger-args) 'nil)
                (cdr debugger-args) debugger-args)
            (current-buffer))
     (insert ?\n)))
  ;; After any frame that uses eval-buffer,
  ;; insert a line that states the buffer position it's reading at.
  (save-excursion
    (let ((tem eval-buffer-list))
      (while (and tem
		  (re-search-forward "^  eval-\\(buffer\\|region\\)(" nil t))
	(end-of-line)
	(insert (format "  ; Reading at buffer position %d"
			;; This will get the wrong result
			;; if there are two nested eval-region calls
			;; for the same buffer.  That's not a very useful case.
			(with-current-buffer (car tem)
			  (point))))
	(pop tem))))
  (debugger-make-xrefs))

(defun debugger-make-xrefs (&optional buffer)
  "Attach cross-references to function names in the `*Backtrace*' buffer."
  (interactive "b")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (setq buffer (current-buffer))
      (let ((inhibit-read-only t)
	    (old-end (point-min)) (new-end (point-min)))
	;; If we saved an old backtrace, find the common part
	;; between the new and the old.
	;; Compare line by line, starting from the end,
	;; because that's the part that is likely to be unchanged.
	(if debugger-previous-backtrace
	    (let (old-start new-start (all-match t))
	      (goto-char (point-max))
	      (with-temp-buffer
		(insert debugger-previous-backtrace)
		(while (and all-match (not (bobp)))
		  (setq old-end (point))
		  (forward-line -1)
		  (setq old-start (point))
		  (with-current-buffer buffer
		    (setq new-end (point))
		    (forward-line -1)
		    (setq new-start (point)))
		  (if (not (zerop
			    (let ((case-fold-search nil))
			      (compare-buffer-substrings
			       (current-buffer) old-start old-end
			       buffer new-start new-end))))
		      (setq all-match nil))))
	      ;; Now new-end is the position of the start of the
	      ;; unchanged part in the current buffer, and old-end is
	      ;; the position of that same text in the saved old
	      ;; backtrace.  But we must subtract (point-min) since strings are
	      ;; indexed in origin 0.

	      ;; Replace the unchanged part of the backtrace
	      ;; with the text from debugger-previous-backtrace,
	      ;; since that already has the proper xrefs.
	      ;; With this optimization, we only need to scan
	      ;; the changed part of the backtrace.
	      (delete-region new-end (point-max))
	      (goto-char (point-max))
	      (insert (substring debugger-previous-backtrace
				 (- old-end (point-min))))
	      ;; Make the unchanged part of the backtrace inaccessible
	      ;; so it won't be scanned.
	      (narrow-to-region (point-min) new-end)))

	;; Scan the new part of the backtrace, inserting xrefs.
	(goto-char (point-min))
	(while (progn
		 (goto-char (+ (point) 2))
		 (skip-syntax-forward "^w_")
		 (not (eobp)))
	  (let* ((beg (point))
		 (end (progn (skip-syntax-forward "w_") (point)))
		 (sym (intern-soft (buffer-substring-no-properties
				    beg end)))
		 (file (and sym (symbol-file sym 'defun))))
	    (when file
	      (goto-char beg)
	      ;; help-xref-button needs to operate on something matched
	      ;; by a regexp, so set that up for it.
	      (re-search-forward "\\(\\sw\\|\\s_\\)+")
	      (help-xref-button 0 'help-function-def sym file)))
	  (forward-line 1))
	(widen))
      (setq debugger-previous-backtrace (buffer-string)))))

(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq debugger-step-after-exit t)
  (setq debugger-jumping-flag t)
  (setq debugger-will-be-back t)
  (add-hook 'post-command-hook 'debugger-reenable)
  (message "Proceeding, will debug on next eval or call.")
  (exit-recursive-edit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  (unless debugger-may-continue
    (error "Cannot continue"))
  (message "Continuing.")
  (save-excursion
    ;; Check to see if we've flagged some frame for debug-on-exit, in which
    ;; case we'll probably come back to the debugger soon.
    (goto-char (point-min))
    (if (re-search-forward "^\\* " nil t)
        (setq debugger-will-be-back t)))
  (exit-recursive-edit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (when (memq (car debugger-args) '(t lambda error debug))
    (error "Cannot return a value %s"
           (if (eq (car debugger-args) 'error)
               "from an error" "at function entrance")))
  (setq debugger-value val)
  (princ "Returning " t)
  (prin1 debugger-value)
  (save-excursion
    ;; Check to see if we've flagged some frame for debug-on-exit, in which
    ;; case we'll probably come back to the debugger soon.
    (goto-char (point-min))
    (if (re-search-forward "^\\* " nil t)
        (setq debugger-will-be-back t)))
  (exit-recursive-edit))

(defun debugger-jump ()
  "Continue to exit from this frame, with all debug-on-entry suspended."
  (interactive)
  (debugger-frame)
  (setq debugger-jumping-flag t)
  (add-hook 'post-command-hook 'debugger-reenable)
  (message "Continuing through this frame")
  (setq debugger-will-be-back t)
  (exit-recursive-edit))

(defun debugger-reenable ()
  "Turn all debug-on-entry functions back on.
This function is put on `post-command-hook' by `debugger-jump' and
removes itself from that hook."
  (setq debugger-jumping-flag nil)
  (remove-hook 'post-command-hook 'debugger-reenable))

(defun debugger-frame-number ()
  "Return number of frames in backtrace before the one point points at."
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  (count 0))
      (while (not (eq (cadr (backtrace-frame count)) 'debug))
	(setq count (1+ count)))
      ;; Skip implement-debug-on-entry frame.
      (when (eq 'implement-debug-on-entry (cadr (backtrace-frame (1+ count))))
	(setq count (1+ count)))
      (goto-char (point-min))
      (when (looking-at "Debugger entered--\\(Lisp error\\|returning value\\):")
	(goto-char (match-end 0))
	(forward-sexp 1))
      (forward-line 1)
      (while (progn
	       (forward-char 2)
	       (if (= (following-char) ?\()
		   (forward-sexp 1)
		 (forward-sexp 2))
	       (forward-line 1)
	       (<= (point) opoint))
	(if (looking-at " *;;;")
	    (forward-line 1))
	(setq count (1+ count)))
      count)))

(defun debugger-frame ()
  "Request entry to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *;;;\\|[a-z]")
	(error "This line is not a function call")))
  (beginning-of-line)
  (backtrace-debug (debugger-frame-number) t)
  (if (= (following-char) ? )
      (let ((inhibit-read-only t))
	(delete-char 1)
	(insert ?*)))
  (beginning-of-line))

(defun debugger-frame-clear ()
  "Do not enter debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *;;;\\|[a-z]")
	(error "This line is not a function call")))
  (beginning-of-line)
  (backtrace-debug (debugger-frame-number) nil)
  (if (= (following-char) ?*)
      (let ((inhibit-read-only t))
	(delete-char 1)
	(insert ? )))
  (beginning-of-line))

(defmacro debugger-env-macro (&rest body)
  "Run BODY in original environment."
  (declare (indent 0))
  `(save-excursion
    (if (null (buffer-name debugger-old-buffer))
        ;; old buffer deleted
        (setq debugger-old-buffer (current-buffer)))
    (set-buffer debugger-old-buffer)
    (let ((load-read-function debugger-outer-load-read-function)
          (overriding-terminal-local-map
           debugger-outer-overriding-terminal-local-map)
          (overriding-local-map debugger-outer-overriding-local-map)
          (track-mouse debugger-outer-track-mouse)
          (last-command debugger-outer-last-command)
          (this-command debugger-outer-this-command)
          (unread-command-events debugger-outer-unread-command-events)
          (unread-post-input-method-events
           debugger-outer-unread-post-input-method-events)
          (last-input-event debugger-outer-last-input-event)
          (last-command-event debugger-outer-last-command-event)
          (last-nonmenu-event debugger-outer-last-nonmenu-event)
          (last-event-frame debugger-outer-last-event-frame)
          (standard-input debugger-outer-standard-input)
          (standard-output debugger-outer-standard-output)
          (inhibit-redisplay debugger-outer-inhibit-redisplay)
          (cursor-in-echo-area debugger-outer-cursor-in-echo-area))
      (set-match-data debugger-outer-match-data)
      (prog1
	  (let ((save-ucc (with-no-warnings unread-command-char)))
	    (unwind-protect
		(progn
		  (with-no-warnings
		    (setq unread-command-char debugger-outer-unread-command-char))
		  (prog1 (progn ,@body)
		    (with-no-warnings
		      (setq debugger-outer-unread-command-char unread-command-char))))
	      (with-no-warnings
		(setq unread-command-char save-ucc))))
        (setq debugger-outer-match-data (match-data))
        (setq debugger-outer-load-read-function load-read-function)
        (setq debugger-outer-overriding-terminal-local-map
              overriding-terminal-local-map)
        (setq debugger-outer-overriding-local-map overriding-local-map)
        (setq debugger-outer-track-mouse track-mouse)
        (setq debugger-outer-last-command last-command)
        (setq debugger-outer-this-command this-command)
        (setq debugger-outer-unread-command-events unread-command-events)
        (setq debugger-outer-unread-post-input-method-events
              unread-post-input-method-events)
        (setq debugger-outer-last-input-event last-input-event)
        (setq debugger-outer-last-command-event last-command-event)
        (setq debugger-outer-last-nonmenu-event last-nonmenu-event)
        (setq debugger-outer-last-event-frame last-event-frame)
        (setq debugger-outer-standard-input standard-input)
        (setq debugger-outer-standard-output standard-output)
        (setq debugger-outer-inhibit-redisplay inhibit-redisplay)
        (setq debugger-outer-cursor-in-echo-area cursor-in-echo-area)
        ))))

(defun debugger-eval-expression (exp)
  "Eval an expression, in an environment like that outside the debugger."
  (interactive
   (list (read-from-minibuffer "Eval: "
			       nil read-expression-map t
			       'read-expression-history)))
  (debugger-env-macro (eval-expression exp)))

(defvar debugger-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)
    (suppress-keymap map)
    (define-key map "-" 'negative-argument)
    (define-key map "b" 'debugger-frame)
    (define-key map "c" 'debugger-continue)
    (define-key map "j" 'debugger-jump)
    (define-key map "r" 'debugger-return-value)
    (define-key map "u" 'debugger-frame-clear)
    (define-key map "d" 'debugger-step-through)
    (define-key map "l" 'debugger-list-functions)
    (define-key map "h" 'describe-mode)
    (define-key map "q" 'top-level)
    (define-key map "e" 'debugger-eval-expression)
    (define-key map " " 'next-line)
    (define-key map "R" 'debugger-record-expression)
    (define-key map "\C-m" 'debug-help-follow)
    (define-key map [mouse-2] 'push-button)
    (define-key map [menu-bar debugger] (cons "Debugger" menu-map))
    (define-key menu-map [deb-top]
      '(menu-item "Quit" top-level
		  :help "Quit debugging and return to top level"))
    (define-key menu-map [deb-s0] '("--"))
    (define-key menu-map [deb-descr]
      '(menu-item "Describe Debugger Mode" describe-mode
		  :help "Display documentation for debugger-mode"))
    (define-key menu-map [deb-hfol]
      '(menu-item "Help Follow" debug-help-follow
		  :help "Follow cross-reference"))
    (define-key menu-map [deb-nxt]
      '(menu-item "Next Line" next-line
		  :help "Move cursor down"))
    (define-key menu-map [deb-s1] '("--"))
    (define-key menu-map [deb-lfunc]
      '(menu-item "List debug on entry functions" debugger-list-functions
		  :help "Display a list of all the functions now set to debug on entry"))
    (define-key menu-map [deb-fclear]
      '(menu-item "Cancel debug frame" debugger-frame-clear
		  :help "Do not enter debugger when this frame exits"))
    (define-key menu-map [deb-frame]
      '(menu-item "Debug frame" debugger-frame
		  :help "Request entry to debugger when this frame exits"))
    (define-key menu-map [deb-s2] '("--"))
    (define-key menu-map [deb-ret]
      '(menu-item "Return value..." debugger-return-value
		  :help "Continue, specifying value to return."))
    (define-key menu-map [deb-rec]
      '(menu-item "Display and Record Expression" debugger-record-expression
		  :help "Display a variable's value and record it in `*Backtrace-record*' buffer"))
    (define-key menu-map [deb-eval]
      '(menu-item "Eval Expression..." debugger-eval-expression
		  :help "Eval an expression, in an environment like that outside the debugger"))
    (define-key menu-map [deb-jump]
      '(menu-item "Jump" debugger-jump
		  :help "Continue to exit from this frame, with all debug-on-entry suspended"))
    (define-key menu-map [deb-cont]
      '(menu-item "Continue" debugger-continue
		  :help "Continue, evaluating this expression without stopping"))
    (define-key menu-map [deb-step]
      '(menu-item "Step through" debugger-step-through
		  :help "Proceed, stepping through subexpressions of this expression"))
    map))

(put 'debugger-mode 'mode-class 'special)

(defun debugger-mode ()
  "Mode for backtrace buffers, selected in debugger.
\\<debugger-mode-map>
A line starts with `*' if exiting that frame will call the debugger.
Type \\[debugger-frame] or \\[debugger-frame-clear] to set or remove the `*'.

When in debugger due to frame being exited,
use the \\[debugger-return-value] command to override the value
being returned from that frame.

Use \\[debug-on-entry] and \\[cancel-debug-on-entry] to control
which functions will enter the debugger when called.

Complete list of commands:
\\{debugger-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'debugger-mode)
  (setq mode-name "Debugger")
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map debugger-mode-map)
  (run-mode-hooks 'debugger-mode-hook))

(defcustom debugger-record-buffer "*Debugger-record*"
  "Buffer name for expression values, for \\[debugger-record-expression]."
  :type 'string
  :group 'debugger
  :version "20.3")

(defun debugger-record-expression  (exp)
  "Display a variable's value and record it in `*Backtrace-record*' buffer."
  (interactive
   (list (read-from-minibuffer
	  "Record Eval: "
	  nil
	  read-expression-map t
	  'read-expression-history)))
  (let* ((buffer (get-buffer-create debugger-record-buffer))
	 (standard-output buffer))
    (princ (format "Debugger Eval (%s): " exp))
    (princ (debugger-eval-expression exp))
    (terpri))

  (with-current-buffer (get-buffer debugger-record-buffer)
    (message "%s"
	     (buffer-substring (line-beginning-position 0)
			       (line-end-position 0)))))

(declare-function help-xref-interned "help-mode" (symbol))

(defun debug-help-follow (&optional pos)
  "Follow cross-reference at POS, defaulting to point.

For the cross-reference format, see `help-make-xrefs'."
  (interactive "d")
  (require 'help-mode)
  ;; Ideally we'd just do (call-interactively 'help-follow) except that this
  ;; assumes we're already in a *Help* buffer and reuses it, so it ends up
  ;; incorrectly "reusing" the *Backtrace* buffer to show the help info.
  (unless pos
    (setq pos (point)))
  (unless (push-button pos)
    ;; check if the symbol under point is a function or variable
    (let ((sym
	   (intern
	    (save-excursion
	      (goto-char pos) (skip-syntax-backward "w_")
	      (buffer-substring (point)
				(progn (skip-syntax-forward "w_")
				       (point)))))))
      (when (or (boundp sym) (fboundp sym) (facep sym))
        (help-xref-interned sym)))))

;; When you change this, you may also need to change the number of
;; frames that the debugger skips.
(defun implement-debug-on-entry ()
  "Conditionally call the debugger.
A call to this function is inserted by `debug-on-entry' to cause
functions to break on entry."
  (if (or inhibit-debug-on-entry debugger-jumping-flag)
      nil
    (funcall debugger 'debug)))

(defun debugger-special-form-p (symbol)
  "Return whether SYMBOL is a special form."
  (and (fboundp symbol)
       (subrp (symbol-function symbol))
       (eq (cdr (subr-arity (symbol-function symbol))) 'unevalled)))

;;;###autoload
(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.

When called interactively, prompt for FUNCTION in the minibuffer.

This works by modifying the definition of FUNCTION.  If you tell the
debugger to continue, FUNCTION's execution proceeds.  If FUNCTION is a
normal function or a macro written in Lisp, you can also step through
its execution.  FUNCTION can also be a primitive that is not a special
form, in which case stepping is not possible.  Break-on-entry for
primitive functions only works when that function is called from Lisp.

Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it."
  (interactive
   (let ((fn (function-called-at-point)) val)
     (when (debugger-special-form-p fn)
       (setq fn nil))
     (setq val (completing-read
		(if fn
		    (format "Debug on entry to function (default %s): " fn)
		  "Debug on entry to function: ")
		obarray
		#'(lambda (symbol)
		    (and (fboundp symbol)
			 (not (debugger-special-form-p symbol))))
		t nil nil (symbol-name fn)))
     (list (if (equal val "") fn (intern val)))))
  ;; FIXME: Use advice.el.
  (when (debugger-special-form-p function)
    (error "Function %s is a special form" function))
  (if (or (symbolp (symbol-function function))
	  (subrp (symbol-function function)))
      ;; The function is built-in or aliased to another function.
      ;; Create a wrapper in which we can add the debug call.
      (fset function `(lambda (&rest debug-on-entry-args)
			,(interactive-form (symbol-function function))
			(apply ',(symbol-function function)
			       debug-on-entry-args)))
    (when (eq (car-safe (symbol-function function)) 'autoload)
      ;; The function is autoloaded.  Load its real definition.
      (load (cadr (symbol-function function)) nil noninteractive nil t))
    (when (or (not (consp (symbol-function function)))
	      (and (eq (car (symbol-function function)) 'macro)
		   (not (consp (cdr (symbol-function function))))))
      ;; The function is byte-compiled.  Create a wrapper in which
      ;; we can add the debug call.
      (debug-convert-byte-code function)))
  (unless (consp (symbol-function function))
    (error "Definition of %s is not a list" function))
  (fset function (debug-on-entry-1 function t))
  (unless (memq function debug-function-list)
    (push function debug-function-list))
  function)

;;;###autoload
(defun cancel-debug-on-entry (&optional function)
  "Undo effect of \\[debug-on-entry] on FUNCTION.
If FUNCTION is nil, cancel debug-on-entry for all functions.
When called interactively, prompt for FUNCTION in the minibuffer.
To specify a nil argument interactively, exit with an empty minibuffer."
  (interactive
   (list (let ((name
		(completing-read
		 "Cancel debug on entry to function (default all functions): "
		 (mapcar 'symbol-name debug-function-list) nil t)))
	   (when name
	     (unless (string= name "")
	       (intern name))))))
  (if (and function
	   (not (string= function ""))) ; Pre 22.1 compatibility test.
      (progn
	(let ((defn (debug-on-entry-1 function nil)))
	  (condition-case nil
	      (when (and (equal (nth 1 defn) '(&rest debug-on-entry-args))
			 (eq (car (nth 3 defn)) 'apply))
		;; `defn' is a wrapper introduced in debug-on-entry.
		;; Get rid of it since we don't need it any more.
		(setq defn (nth 1 (nth 1 (nth 3 defn)))))
	    (error nil))
	  (fset function defn))
	(setq debug-function-list (delq function debug-function-list))
	function)
    (message "Cancelling debug-on-entry for all functions")
    (mapcar 'cancel-debug-on-entry debug-function-list)))

(defun debug-arglist (definition)
  ;; FIXME: copied from ad-arglist.
  "Return the argument list of DEFINITION."
  (require 'help-fns)
  (help-function-arglist definition 'preserve-names))

(defun debug-convert-byte-code (function)
  (let* ((defn (symbol-function function))
	 (macro (eq (car-safe defn) 'macro)))
    (when macro (setq defn (cdr defn)))
    (when (byte-code-function-p defn)
      (let* ((args (debug-arglist defn))
	     (body
              `((,(if (memq '&rest args) #'apply #'funcall)
                 ,defn
                 ,@(remq '&rest (remq '&optional args))))))
	(if (> (length defn) 5)
            ;; The mere presence of field 5 is sufficient to make
            ;; it interactive.
	    (push `(interactive ,(aref defn 5)) body))
	(if (and (> (length defn) 4) (aref defn 4))
	    ;; Use `documentation' here, to get the actual string,
	    ;; in case the compiled function has a reference
	    ;; to the .elc file.
	    (setq body (cons (documentation function) body)))
	(setq defn `(closure (t) ,args ,@body)))
      (when macro (setq defn (cons 'macro defn)))
      (fset function defn))))

(defun debug-on-entry-1 (function flag)
  (let* ((defn (symbol-function function))
	 (tail defn))
    (when (eq (car-safe tail) 'macro)
      (setq tail (cdr tail)))
    (if (not (memq (car-safe tail) '(closure lambda)))
	;; Only signal an error when we try to set debug-on-entry.
	;; When we try to clear debug-on-entry, we are now done.
	(when flag
	  (error "%s is not a user-defined Lisp function" function))
      (if (eq (car tail) 'closure) (setq tail (cdr tail)))
      (setq tail (cdr tail))
      ;; Skip the docstring.
      (when (and (stringp (cadr tail)) (cddr tail))
	(setq tail (cdr tail)))
      ;; Skip the interactive form.
      (when (eq 'interactive (car-safe (cadr tail)))
	(setq tail (cdr tail)))
      (unless (eq flag (equal (cadr tail) '(implement-debug-on-entry)))
	;; Add/remove debug statement as needed.
	(setcdr tail (if flag
                         (cons '(implement-debug-on-entry) (cdr tail))
                       (cddr tail)))))
    defn))

(defun debugger-list-functions ()
  "Display a list of all the functions now set to debug on entry."
  (interactive)
  (require 'help-mode)
  (help-setup-xref '(debugger-list-functions)
		   (called-interactively-p 'interactive))
  (with-output-to-temp-buffer (help-buffer)
    (with-current-buffer standard-output
      (if (null debug-function-list)
	  (princ "No debug-on-entry functions now\n")
	(princ "Functions set to debug on entry:\n\n")
	(dolist (fun debug-function-list)
	  (make-text-button (point) (progn (prin1 fun) (point))
			    'type 'help-function
			    'help-args (list fun))
	  (terpri))
	(terpri)
	(princ "Note: if you have redefined a function, then it may no longer\n")
	(princ "be set to debug on entry, even if it is in the list.")))))

(provide 'debug)

;;; debug.el ends here
