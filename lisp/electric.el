;;; electric.el --- window maker and Command loop for `electric' modes

;; Copyright (C) 1985-1986, 1995, 2001-2012 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: extensions

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

;; "Electric" has been used in Emacs to refer to different things.
;; Among them:
;;
;; - electric modes and buffers: modes that typically pop-up in a modal kind of
;;   way a transient buffer that automatically disappears as soon as the user
;;   is done with it.
;;
;; - electric keys: self inserting keys which additionally perform some side
;;   operation which happens to be often convenient at that time.  Examples of
;;   such side operations are: reindenting code, inserting a newline,
;;   ... auto-fill-mode and abbrev-mode can be considered as built-in forms of
;;   electric key behavior.

;;; Code:

(eval-when-compile (require 'cl))

;; This loop is the guts for non-standard modes which retain control
;; until some event occurs.  It is a `do-forever', the only way out is
;; to throw.  It assumes that you have set up the keymap, window, and
;; everything else: all it does is read commands and execute them -
;; providing error messages should one occur (if there is no loop
;; function - which see).  The required argument is a tag which should
;; expect a value of nil if the user decides to punt. The second
;; argument is the prompt to be used: if nil, use "->", if 'noprompt,
;; don't use a prompt, if a string, use that string as prompt, and if
;; a function of no variable, it will be evaluated in every iteration
;; of the loop and its return value, which can be nil, 'noprompt or a
;; string, will be used as prompt.  Given third argument non-nil, it
;; INHIBITS quitting unless the user types C-g at toplevel.  This is
;; so user can do things like C-u C-g and not get thrown out.  Fourth
;; argument, if non-nil, should be a function of two arguments which
;; is called after every command is executed.  The fifth argument, if
;; provided, is the state variable for the function.  If the
;; loop-function gets an error, the loop will abort WITHOUT throwing
;; (moral: use unwind-protect around call to this function for any
;; critical stuff).  The second argument for the loop function is the
;; conditions for any error that occurred or nil if none.

(defun Electric-command-loop (return-tag
			      &optional prompt inhibit-quitting
					loop-function loop-state)

  (let (cmd
        (err nil)
        (inhibit-quit inhibit-quitting)
        (prompt-string prompt))
    (while t
      (if (functionp prompt)
          (setq prompt-string (funcall prompt)))
      (if (not (stringp prompt-string))
          (setq prompt-string (unless (eq prompt-string 'noprompt) "->")))
      (setq cmd (read-key-sequence prompt-string))
      (setq last-command-event (aref cmd (1- (length cmd)))
	    this-command (key-binding cmd t)
	    cmd this-command)
      ;; This makes universal-argument-other-key work.
      (setq universal-argument-num-events 0)
      (if (or (prog1 quit-flag (setq quit-flag nil))
	      (eq last-input-event ?\C-g))
	  (progn (setq unread-command-events nil
		       prefix-arg nil)
		 ;; If it wasn't canceling a prefix character, then quit.
		 (if (or (= (length (this-command-keys)) 1)
			 (not inhibit-quit)) ; safety
		     (progn (ding)
			    (message "Quit")
			    (throw return-tag nil))
		   (setq cmd nil))))
      (setq current-prefix-arg prefix-arg)
      (if cmd
	  (condition-case conditions
	      (progn (command-execute cmd)
		     (setq last-command this-command)
		     (if (or (prog1 quit-flag (setq quit-flag nil))
			     (eq last-input-event ?\C-g))
			 (progn (setq unread-command-events nil)
				(if (not inhibit-quit)
				    (progn (ding)
					   (message "Quit")
					   (throw return-tag nil))
				  (ding)))))
	    (buffer-read-only (if loop-function
				  (setq err conditions)
				(ding)
				(message "Buffer is read-only")
				(sit-for 2)))
	    (beginning-of-buffer (if loop-function
				     (setq err conditions)
				   (ding)
				   (message "Beginning of Buffer")
				   (sit-for 2)))
	    (end-of-buffer (if loop-function
			       (setq err conditions)
			     (ding)
			     (message "End of Buffer")
			     (sit-for 2)))
	    (error (if loop-function
		       (setq err conditions)
		     (ding)
		     (message "Error: %s"
			      (if (eq (car conditions) 'error)
				  (car (cdr conditions))
				(prin1-to-string conditions)))
		     (sit-for 2))))
	(ding))
      (if loop-function (funcall loop-function loop-state err))))
  (ding)
  (throw return-tag nil))

;; This function is like pop-to-buffer, sort of.
;; The algorithm is
;; If there is a window displaying buffer
;; 	Select it
;; Else if there is only one window
;; 	Split it, selecting the window on the bottom with height being
;; 	the lesser of max-height (if non-nil) and the number of lines in
;;      the buffer to be displayed subject to window-min-height constraint.
;; Else
;; 	Switch to buffer in the current window.
;;
;; Then if max-height is nil, and not all of the lines in the buffer
;; are displayed, grab the whole frame.
;;
;; Returns selected window on buffer positioned at point-min.

(defun Electric-pop-up-window (buffer &optional max-height)
  (let* ((win (or (get-buffer-window buffer) (selected-window)))
	 (buf (get-buffer buffer))
	 (one-window (one-window-p t))
	 (pop-up-windows t)
	 (pop-up-frames nil))
    (if (not buf)
	(error "Buffer %s does not exist" buffer)
      (cond ((and (eq (window-buffer win) buf))
	     (select-window win))
	    (one-window
	     (pop-to-buffer buffer)
	     (setq win (selected-window)))
	    (t
	     (switch-to-buffer buf)))
      ;; Don't shrink the window, but expand it if necessary.
      (goto-char (point-min))
      (unless (= (point-max) (window-end win t))
	(fit-window-to-buffer win max-height))
      win)))

;;; Electric keys.

(defgroup electricity ()
  "Electric behavior for self inserting keys."
  :group 'editing)

(defun electric--after-char-pos ()
  "Return the position after the char we just inserted.
Returns nil when we can't find this char."
  (let ((pos (point)))
    (when (or (eq (char-before) last-command-event) ;; Sanity check.
              (save-excursion
                (or (progn (skip-chars-backward " \t")
                           (setq pos (point))
                           (eq (char-before) last-command-event))
                    (progn (skip-chars-backward " \n\t")
                           (setq pos (point))
                           (eq (char-before) last-command-event)))))
      pos)))

;; Electric indentation.

;; Autoloading variables is generally undesirable, but major modes
;; should usually set this variable by adding elements to the default
;; value, which only works well if the variable is preloaded.
;;;###autoload
(defvar electric-indent-chars '(?\n)
  "Characters that should cause automatic reindentation.")

(defvar electric-indent-functions nil
  "Special hook run to decide whether to auto-indent.
Each function is called with one argument (the inserted char), with
point right after that char, and it should return t to cause indentation,
`no-indent' to prevent indentation or nil to let other functions decide.")

(defun electric-indent-post-self-insert-function ()
  ;; FIXME: This reindents the current line, but what we really want instead is
  ;; to reindent the whole affected text.  That's the current line for simple
  ;; cases, but not all cases.  We do take care of the newline case in an
  ;; ad-hoc fashion, but there are still missing cases such as the case of
  ;; electric-pair-mode wrapping a region with a pair of parens.
  ;; There might be a way to get it working by analyzing buffer-undo-list, but
  ;; it looks challenging.
  (let (pos)
    (when (and
           ;; Don't reindent while inserting spaces at beginning of line.
           (or (not (memq last-command-event '(?\s ?\t)))
               (save-excursion (skip-chars-backward " \t") (not (bolp))))
           (setq pos (electric--after-char-pos))
           (save-excursion
             (goto-char pos)
             (let ((act (or (run-hook-with-args-until-success
                             'electric-indent-functions
                             last-command-event)
                            (memq last-command-event electric-indent-chars))))
               (not
                (or (memq act '(nil no-indent))
                    ;; In a string or comment.
                    (unless (eq act 'do-indent) (nth 8 (syntax-ppss))))))))
      ;; For newline, we want to reindent both lines and basically behave like
      ;; reindent-then-newline-and-indent (whose code we hence copied).
      (when (< (1- pos) (line-beginning-position))
        (let ((before (copy-marker (1- pos) t)))
          (save-excursion
            (unless (memq indent-line-function
                          '(indent-relative indent-to-left-margin
                                            indent-relative-maybe))
              ;; Don't reindent the previous line if the indentation function
              ;; is not a real one.
              (goto-char before)
              (indent-according-to-mode))
            ;; We are at EOL before the call to indent-according-to-mode, and
            ;; after it we usually are as well, but not always.  We tried to
            ;; address it with `save-excursion' but that uses a normal marker
            ;; whereas we need `move after insertion', so we do the
            ;; save/restore by hand.
            (goto-char before)
            ;; Remove the trailing whitespace after indentation because
            ;; indentation may (re)introduce the whitespace.
            (delete-horizontal-space t))))
      (unless (memq indent-line-function '(indent-to-left-margin))
        (indent-according-to-mode)))))

;;;###autoload
(define-minor-mode electric-indent-mode
  "Toggle on-the-fly reindentation (Electric Indent mode).
With a prefix argument ARG, enable Electric Indent mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This is a global minor mode.  When enabled, it reindents whenever
the hook `electric-indent-functions' returns non-nil, or you
insert a character from `electric-indent-chars'."
  :global t
  :group 'electricity
  (if (not electric-indent-mode)
      (remove-hook 'post-self-insert-hook
                   #'electric-indent-post-self-insert-function)
    ;; post-self-insert-hooks interact in non-trivial ways.
    ;; It turns out that electric-indent-mode generally works better if run
    ;; late, but still before blink-paren.
    (add-hook 'post-self-insert-hook
              #'electric-indent-post-self-insert-function
              'append)
    ;; FIXME: Ugly!
    (let ((bp (memq #'blink-paren-post-self-insert-function
                    (default-value 'post-self-insert-hook))))
      (when (memq #'electric-indent-post-self-insert-function bp)
        (setcar bp #'electric-indent-post-self-insert-function)
        (setcdr bp (cons #'blink-paren-post-self-insert-function
                         (delq #'electric-indent-post-self-insert-function
                               (cdr bp))))))))

;; Electric pairing.

(defcustom electric-pair-pairs
  '((?\" . ?\"))
  "Alist of pairs that should be used regardless of major mode."
  :group 'electricity
  :version "24.1"
  :type '(repeat (cons character character)))

(defcustom electric-pair-skip-self t
  "If non-nil, skip char instead of inserting a second closing paren.
When inserting a closing paren character right before the same character,
just skip that character instead, so that hitting ( followed by ) results
in \"()\" rather than \"())\".
This can be convenient for people who find it easier to hit ) than C-f."
  :group 'electricity
  :version "24.1"
  :type 'boolean)

(defun electric-pair-post-self-insert-function ()
  (let* ((syntax (and (eq (char-before) last-command-event) ; Sanity check.
                      electric-pair-mode
                      (let ((x (assq last-command-event electric-pair-pairs)))
                        (cond
                         (x (if (eq (car x) (cdr x)) ?\" ?\())
                         ((rassq last-command-event electric-pair-pairs) ?\))
                         (t (char-syntax last-command-event))))))
         ;; FIXME: when inserting the closer, we should maybe use
         ;; self-insert-command, although it may prove tricky running
         ;; post-self-insert-hook recursively, and we wouldn't want to trigger
         ;; blink-matching-open.
         (closer (if (eq syntax ?\()
                     (cdr (or (assq last-command-event electric-pair-pairs)
                              (aref (syntax-table) last-command-event)))
                   last-command-event)))
    (cond
     ;; Wrap a pair around the active region.
     ((and (memq syntax '(?\( ?\" ?\$)) (use-region-p))
      (if (> (mark) (point))
          (goto-char (mark))
        ;; We already inserted the open-paren but at the end of the region,
        ;; so we have to remove it and start over.
        (delete-char -1)
        (save-excursion
          (goto-char (mark))
          (insert last-command-event)))
      (insert closer))
     ;; Backslash-escaped: no pairing, no skipping.
     ((save-excursion
        (goto-char (1- (point)))
        (not (zerop (% (skip-syntax-backward "\\") 2))))
      nil)
     ;; Skip self.
     ((and (memq syntax '(?\) ?\" ?\$))
           electric-pair-skip-self
           (eq (char-after) last-command-event))
      ;; This is too late: rather than insert&delete we'd want to only skip (or
      ;; insert in overwrite mode).  The difference is in what goes in the
      ;; undo-log and in the intermediate state which might be visible to other
      ;; post-self-insert-hook.  We'll just have to live with it for now.
      (delete-char 1))
     ;; Insert matching pair.
     ((not (or (not (memq syntax `(?\( ?\" ?\$)))
               overwrite-mode
               ;; I find it more often preferable not to pair when the
               ;; same char is next.
               (eq last-command-event (char-after))
               (eq last-command-event (char-before (1- (point))))
               ;; I also find it often preferable not to pair next to a word.
               (eq (char-syntax (following-char)) ?w)))
      (save-excursion (insert closer))))))

;;;###autoload
(define-minor-mode electric-pair-mode
  "Toggle automatic parens pairing (Electric Pair mode).
With a prefix argument ARG, enable Electric Pair mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Electric Pair mode is a global minor mode.  When enabled, typing
an open parenthesis automatically inserts the corresponding
closing parenthesis.  \(Likewise for brackets, etc.)

See options `electric-pair-pairs' and `electric-pair-skip-self'."
  :global t
  :group 'electricity
  (if electric-pair-mode
      (add-hook 'post-self-insert-hook
                #'electric-pair-post-self-insert-function)
    (remove-hook 'post-self-insert-hook
                 #'electric-pair-post-self-insert-function)))

;; Automatically add newlines after/before/around some chars.

(defvar electric-layout-rules '()
  "List of rules saying where to automatically insert newlines.
Each rule has the form (CHAR . WHERE) where CHAR is the char
that was just inserted and WHERE specifies where to insert newlines
and can be: nil, `before', `after', `around', or a function of no
arguments that returns one of those symbols.")

(defun electric-layout-post-self-insert-function ()
  (let* ((rule (cdr (assq last-command-event electric-layout-rules)))
         pos)
    (when (and rule
               (setq pos (electric--after-char-pos))
               ;; Not in a string or comment.
               (not (nth 8 (save-excursion (syntax-ppss pos)))))
      (let ((end (copy-marker (point) t)))
        (goto-char pos)
        (case (if (functionp rule) (funcall rule) rule)
          ;; FIXME: we used `newline' down here which called
          ;; self-insert-command and ran post-self-insert-hook recursively.
          ;; It happened to make electric-indent-mode work automatically with
          ;; electric-layout-mode (at the cost of re-indenting lines
          ;; multiple times), but I'm not sure it's what we want.
          (before (goto-char (1- pos)) (skip-chars-backward " \t")
                  (unless (bolp) (insert "\n")))
          (after  (insert "\n"))       ; FIXME: check eolp before inserting \n?
          (around (save-excursion
                    (goto-char (1- pos)) (skip-chars-backward " \t")
                    (unless (bolp) (insert "\n")))
                  (insert "\n")))      ; FIXME: check eolp before inserting \n?
        (goto-char end)))))

;;;###autoload
(define-minor-mode electric-layout-mode
  "Automatically insert newlines around some chars.
With a prefix argument ARG, enable Electric Layout mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
The variable `electric-layout-rules' says when and how to insert newlines."
  :global t
  :group 'electricity
  (if electric-layout-mode
      (add-hook 'post-self-insert-hook
                #'electric-layout-post-self-insert-function)
    (remove-hook 'post-self-insert-hook
                 #'electric-layout-post-self-insert-function)))

(provide 'electric)

;;; electric.el ends here
