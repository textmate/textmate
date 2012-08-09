;;; xscheme.el --- run MIT Scheme under Emacs

;; Copyright (C) 1986-1987, 1989-1990, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: languages, lisp

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

;; A major mode for interacting with MIT Scheme.
;;
;; Requires MIT Scheme release 5 or later.
;; Changes to Control-G handler require runtime version 13.85 or later.

;;; Code:

(require 'scheme)

;;;; Internal Variables

(defvar xscheme-previous-mode)
(defvar xscheme-previous-process-state)
(defvar xscheme-last-input-end)

(defvar xscheme-process-command-line nil
  "Command used to start the most recent Scheme process.")

(defvar xscheme-process-name "scheme"
  "Name of xscheme process that we're currently interacting with.")

(defvar xscheme-buffer-name "*scheme*"
  "Name of xscheme buffer that we're currently interacting with.")

(defvar xscheme-expressions-ring-max 30
  "*Maximum length of Scheme expressions ring.")

(defvar xscheme-expressions-ring nil
  "List of expressions recently transmitted to the Scheme process.")

(defvar xscheme-expressions-ring-yank-pointer nil
  "The tail of the Scheme expressions ring whose car is the last thing yanked.")

(defvar xscheme-running-p nil
  "This variable, if nil, indicates that the scheme process is
waiting for input.  Otherwise, it is busy evaluating something.")

(defconst xscheme-control-g-synchronization-p t
  "If non-nil, insert markers in the scheme input stream to indicate when
control-g interrupts were signaled.  Do not allow more control-g's to be
signaled until the scheme process acknowledges receipt.")

(defvar xscheme-control-g-disabled-p nil
  "This variable, if non-nil, indicates that a control-g is being processed
by the scheme process, so additional control-g's are to be ignored.")

(defvar xscheme-string-receiver nil
  "Procedure to send the string argument from the scheme process.")

(defconst default-xscheme-runlight
  '(": " xscheme-runlight-string)
  "Default global (shared) xscheme-runlight modeline format.")

(defvar xscheme-runlight "")
(defvar xscheme-runlight-string nil)

(defvar xscheme-process-filter-state 'idle
  "State of scheme process escape reader state machine:
idle                   waiting for an escape sequence
reading-type           received an altmode but nothing else
reading-string         reading prompt string")

(defvar xscheme-allow-output-p t
  "This variable, if nil, prevents output from the scheme process
from being inserted into the process-buffer.")

(defvar xscheme-prompt ""
  "The current scheme prompt string.")

(defvar xscheme-string-accumulator ""
  "Accumulator for the string being received from the scheme process.")

(defvar xscheme-mode-string nil)
(setq-default scheme-mode-line-process
	      '("" xscheme-runlight))

(mapc 'make-variable-buffer-local
      '(xscheme-expressions-ring
	xscheme-expressions-ring-yank-pointer
	xscheme-process-filter-state
	xscheme-running-p
	xscheme-control-g-disabled-p
	xscheme-allow-output-p
	xscheme-prompt
	xscheme-string-accumulator
	xscheme-mode-string
	scheme-mode-line-process))

(defgroup xscheme nil
  "Major mode for editing Scheme and interacting with MIT's C-Scheme."
  :group 'lisp)

(defcustom scheme-band-name nil
  "*Band loaded by the `run-scheme' command."
  :type '(choice (const nil) string)
  :group 'xscheme)

(defcustom scheme-program-arguments nil
  "*Arguments passed to the Scheme program by the `run-scheme' command."
  :type '(choice (const nil) string)
  :group 'xscheme)

(defcustom xscheme-allow-pipelined-evaluation t
  "If non-nil, an expression may be transmitted while another is evaluating.
Otherwise, attempting to evaluate an expression before the previous expression
has finished evaluating will signal an error."
  :type 'boolean
  :group 'xscheme)

(defcustom xscheme-startup-message
  "This is the Scheme process buffer.
Type \\[xscheme-send-previous-expression] to evaluate the expression before point.
Type \\[xscheme-send-control-g-interrupt] to abort evaluation.
Type \\[describe-mode] for more information.

"
  "String to insert into Scheme process buffer first time it is started.
Is processed with `substitute-command-keys' first."
  :type 'string
  :group 'xscheme)

(defcustom xscheme-signal-death-message nil
  "If non-nil, causes a message to be generated when the Scheme process dies."
  :type 'boolean
  :group 'xscheme)

(defcustom xscheme-start-hook nil
  "If non-nil, a procedure to call when the Scheme process is started.
When called, the current buffer will be the Scheme process-buffer."
  :type 'hook
  :group 'xscheme
  :version "20.3")

(defun xscheme-evaluation-commands (keymap)
  (define-key keymap "\e\C-x" 'xscheme-send-definition)
  (define-key keymap "\C-x\C-e" 'xscheme-send-previous-expression)
  (put 'xscheme-send-previous-expression :advertised-binding "\C-x\C-e")
  (define-key keymap "\eo" 'xscheme-send-buffer)
  (define-key keymap "\ez" 'xscheme-send-definition)
  (define-key keymap "\e\C-m" 'xscheme-send-previous-expression)
  (define-key keymap "\e\C-z" 'xscheme-send-region))

(defun xscheme-interrupt-commands (keymap)
  (define-key keymap "\C-c\C-s" 'xscheme-select-process-buffer)
  (define-key keymap "\C-c\C-b" 'xscheme-send-breakpoint-interrupt)
  (define-key keymap "\C-c\C-c" 'xscheme-send-control-g-interrupt)
  (define-key keymap "\C-c\C-u" 'xscheme-send-control-u-interrupt)
  (define-key keymap "\C-c\C-x" 'xscheme-send-control-x-interrupt))

(xscheme-evaluation-commands scheme-mode-map)
(xscheme-interrupt-commands scheme-mode-map)

(defun run-scheme (command-line)
  "Run MIT Scheme in an inferior process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line."
  (interactive (list (xscheme-read-command-line current-prefix-arg)))
  (xscheme-start command-line xscheme-process-name xscheme-buffer-name))

(defun xscheme-start (command-line process-name buffer-name)
  (setq-default xscheme-process-command-line command-line)
  (switch-to-buffer
   (xscheme-start-process command-line process-name buffer-name))
  (set (make-local-variable 'xscheme-process-command-line) command-line))

(defun xscheme-read-command-line (arg)
  (let ((default
	  (or xscheme-process-command-line
	      (xscheme-default-command-line))))
    (if arg
	(read-string "Run Scheme: " default)
	default)))

(defun xscheme-default-command-line ()
  (concat scheme-program-name " -emacs"
	  (if scheme-program-arguments
	      (concat " " scheme-program-arguments)
	      "")
	  (if scheme-band-name
	      (concat " -band " scheme-band-name)
	      "")))

(defun reset-scheme ()
  "Reset the Scheme process."
  (interactive)
  (let ((process (get-process xscheme-process-name)))
    (cond ((or (not process)
	       (not (eq (process-status process) 'run))
	       (yes-or-no-p
"The Scheme process is running, are you SURE you want to reset it? "))
	   (message "Resetting Scheme process...")
	   (if process
	       (progn
		 (kill-process process t)
		 (delete-process process)))
	   (xscheme-start-process xscheme-process-command-line
				  xscheme-process-name
				  xscheme-buffer-name)
	   (message "Resetting Scheme process...done")))))

;;;; Multiple Scheme buffer management commands

(defun start-scheme (buffer-name &optional globally)
  "Choose a scheme interaction buffer, or create a new one."
  ;; (interactive "BScheme interaction buffer: \nP")
  (interactive
   (list (read-buffer "Scheme interaction buffer: "
		      xscheme-buffer-name
		      nil)
	 current-prefix-arg))
  (let ((buffer (get-buffer-create buffer-name)))
    (let ((process (get-buffer-process buffer)))
      (if process
	  (switch-to-buffer buffer)
	(if (or (not (buffer-file-name buffer))
		(yes-or-no-p (concat "Buffer "
				     (buffer-name buffer)
				     " contains file "
				     (buffer-file-name buffer)
				     "; start scheme in it? ")))
	    (progn
	      (xscheme-start (xscheme-read-command-line t)
			     buffer-name
			     buffer-name)
	      (if globally
		  (global-set-scheme-interaction-buffer buffer-name)))
	  (message "start-scheme aborted"))))))

(fset 'select-scheme 'start-scheme)

(defun global-set-scheme-interaction-buffer (buffer-name)
  "Set the default scheme interaction buffer."
  (interactive
   (list (read-buffer "Scheme interaction buffer: "
		      xscheme-buffer-name
		      t)))
  (let ((process-name (verify-xscheme-buffer buffer-name nil)))
    (setq-default xscheme-buffer-name buffer-name)
    (setq-default xscheme-process-name process-name)
    (setq-default xscheme-runlight-string
		  (with-current-buffer buffer-name
                    xscheme-runlight-string))
    (setq-default xscheme-runlight
		  (if (eq (process-status process-name) 'run)
		      default-xscheme-runlight
		      ""))))

(defun local-set-scheme-interaction-buffer (buffer-name)
  "Set the scheme interaction buffer for the current buffer."
  (interactive
   (list (read-buffer "Scheme interaction buffer: "
		      xscheme-buffer-name
		      t)))
  (let ((process-name (verify-xscheme-buffer buffer-name t)))
    (set (make-local-variable 'xscheme-buffer-name) buffer-name)
    (set (make-local-variable 'xscheme-process-name) process-name)
    (set (make-local-variable 'xscheme-runlight)
         (with-current-buffer buffer-name
           xscheme-runlight))))

(defun local-clear-scheme-interaction-buffer ()
  "Make the current buffer use the default scheme interaction buffer."
  (interactive)
  (if (xscheme-process-buffer-current-p)
      (error "Cannot change the interaction buffer of an interaction buffer"))
  (kill-local-variable 'xscheme-buffer-name)
  (kill-local-variable 'xscheme-process-name)
  (kill-local-variable 'xscheme-runlight))

(defun verify-xscheme-buffer (buffer-name localp)
  (if (and localp (xscheme-process-buffer-current-p))
      (error "Cannot change the interaction buffer of an interaction buffer"))
  (let* ((buffer (get-buffer buffer-name))
	 (process (and buffer (get-buffer-process buffer))))
    (cond ((not buffer)
	   (error "Buffer `%s' does not exist" buffer-name))
	  ((not process)
	   (error "Buffer `%s' is not a scheme interaction buffer" buffer-name))
	  (t
	   (with-current-buffer buffer
	     (if (not (xscheme-process-buffer-current-p))
		 (error "Buffer `%s' is not a scheme interaction buffer"
			buffer-name)))
	   (process-name process)))))

;;;; Interaction Mode

(defun scheme-interaction-mode (&optional preserve)
  "Major mode for interacting with an inferior MIT Scheme process.
Like  scheme-mode  except that:

\\[xscheme-send-previous-expression] sends the expression before point to the Scheme process as input
\\[xscheme-yank-pop] yanks an expression previously sent to Scheme
\\[xscheme-yank-push] yanks an expression more recently sent to Scheme

All output from the Scheme process is written in the Scheme process
buffer, which is initially named \"*scheme*\".  The result of
evaluating a Scheme expression is also printed in the process buffer,
preceded by the string \";Value: \" to highlight it.  If the process
buffer is not visible at that time, the value will also be displayed
in the minibuffer.  If an error occurs, the process buffer will
automatically pop up to show you the error message.

While the Scheme process is running, the modelines of all buffers in
scheme-mode are modified to show the state of the process.  The
possible states and their meanings are:

input		waiting for input
run		evaluating
gc		garbage collecting

The process buffer's modeline contains additional information where
the buffer's name is normally displayed: the command interpreter level
and type.

Scheme maintains a stack of command interpreters.  Every time an error
or breakpoint occurs, the current command interpreter is pushed on the
command interpreter stack, and a new command interpreter is started.
One example of why this is done is so that an error that occurs while
you are debugging another error will not destroy the state of the
initial error, allowing you to return to it after the second error has
been fixed.

The command interpreter level indicates how many interpreters are in
the command interpreter stack.  It is initially set to one, and it is
incremented every time that stack is pushed, and decremented every
time it is popped.  The following commands are useful for manipulating
the command interpreter stack:

\\[xscheme-send-breakpoint-interrupt]	pushes the stack once
\\[xscheme-send-control-u-interrupt]	pops the stack once
\\[xscheme-send-control-g-interrupt]	pops everything off
\\[xscheme-send-control-x-interrupt]	aborts evaluation, doesn't affect stack

Some possible command interpreter types and their meanings are:

\[Evaluator]	read-eval-print loop for evaluating expressions
\[Debugger]	single character commands for debugging errors
\[Where]		single character commands for examining environments

Starting with release 6.2 of Scheme, the latter two types of command
interpreters will change the major mode of the Scheme process buffer
to scheme-debugger-mode , in which the evaluation commands are
disabled, and the keys which normally self insert instead send
themselves to the Scheme process.  The command character ? will list
the available commands.

For older releases of Scheme, the major mode will be be
scheme-interaction-mode , and the command characters must be sent as
if they were expressions.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-interaction-mode-map}

Entry to this mode calls the value of scheme-interaction-mode-hook
with no args, if that value is non-nil.
 Likewise with the value of scheme-mode-hook.
 scheme-interaction-mode-hook is called after scheme-mode-hook."
  ;; FIXME: Use define-derived-mode.
  (interactive "P")
  (if (not preserve)
      (let ((previous-mode major-mode))
        (kill-all-local-variables)
        (make-local-variable 'xscheme-process-name)
        (make-local-variable 'xscheme-previous-process-state)
        (make-local-variable 'xscheme-runlight-string)
        (make-local-variable 'xscheme-runlight)
        (set (make-local-variable 'xscheme-previous-mode) previous-mode)
        (let ((buffer (current-buffer)))
          (set (make-local-variable 'xscheme-buffer-name) (buffer-name buffer))
          (set (make-local-variable 'xscheme-last-input-end) (make-marker))
          (let ((process (get-buffer-process buffer)))
            (if process
                (progn
                  (setq xscheme-process-name (process-name process))
                  (setq xscheme-previous-process-state
                        (cons (process-filter process)
                              (process-sentinel process)))
		  (xscheme-process-filter-initialize t)
		  (xscheme-modeline-initialize xscheme-buffer-name)
		  (set-process-sentinel process 'xscheme-process-sentinel)
		  (set-process-filter process 'xscheme-process-filter))
                (setq xscheme-previous-process-state (cons nil nil)))))))
  (scheme-interaction-mode-initialize)
  (scheme-mode-variables)
  (run-mode-hooks 'scheme-mode-hook 'scheme-interaction-mode-hook))

(defun exit-scheme-interaction-mode ()
  "Take buffer out of scheme interaction mode"
  (interactive)
  (if (not (derived-mode-p 'scheme-interaction-mode))
      (error "Buffer not in scheme interaction mode"))
  (let ((previous-state xscheme-previous-process-state))
    (funcall xscheme-previous-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (if process
	  (progn
	    (if (eq (process-filter process) 'xscheme-process-filter)
		(set-process-filter process (car previous-state)))
	    (if (eq (process-sentinel process) 'xscheme-process-sentinel)
		(set-process-sentinel process (cdr previous-state))))))))

(defvar scheme-interaction-mode-commands-alist nil)
(defvar scheme-interaction-mode-map nil)

(defun scheme-interaction-mode-initialize ()
  (use-local-map scheme-interaction-mode-map)
  (setq major-mode 'scheme-interaction-mode) ;FIXME: Use define-derived-mode.
  (setq mode-name "Scheme Interaction"))

(defun scheme-interaction-mode-commands (keymap)
  (let ((entries scheme-interaction-mode-commands-alist))
    (while entries
      (define-key keymap
	(car (car entries))
	(car (cdr (car entries))))
      (setq entries (cdr entries)))))

;; Initialize the command alist
(setq scheme-interaction-mode-commands-alist
      (append scheme-interaction-mode-commands-alist
	      '(("\C-c\C-m" xscheme-send-current-line)
		("\C-c\C-o" xscheme-delete-output)
		("\C-c\C-p" xscheme-send-proceed)
		("\C-c\C-y" xscheme-yank)
		("\ep" xscheme-yank-pop)
		("\en" xscheme-yank-push))))

;; Initialize the mode map
(if (not scheme-interaction-mode-map)
    (progn
      (setq scheme-interaction-mode-map (make-keymap))
      (scheme-mode-commands scheme-interaction-mode-map)
      (xscheme-interrupt-commands scheme-interaction-mode-map)
      (xscheme-evaluation-commands scheme-interaction-mode-map)
      (scheme-interaction-mode-commands scheme-interaction-mode-map)))

(defun xscheme-enter-interaction-mode ()
  (with-current-buffer (xscheme-process-buffer)
    (if (not (derived-mode-p 'scheme-interaction-mode))
	(if (derived-mode-p 'scheme-debugger-mode)
	    (scheme-interaction-mode-initialize)
	    (scheme-interaction-mode t)))))

(define-obsolete-function-alias 'advertised-xscheme-send-previous-expression
  'xscheme-send-previous-expression "23.2")

;;;; Debugger Mode

(defun scheme-debugger-mode ()
  "Major mode for executing the Scheme debugger.
Like  scheme-mode  except that the evaluation commands
are disabled, and characters that would normally be self inserting are
sent to the Scheme process instead.  Typing ?  will show you which
characters perform useful functions.

Commands:
\\{scheme-debugger-mode-map}"
  (error "Invalid entry to scheme-debugger-mode"))

(defvar scheme-debugger-mode-map nil)

(defun scheme-debugger-mode-initialize ()
  (use-local-map scheme-debugger-mode-map)
  (setq major-mode 'scheme-debugger-mode) ;FIXME: Use define-derived-mode.
  (setq mode-name "Scheme Debugger"))

(defun scheme-debugger-mode-commands (keymap)
  (let ((char ?\s))
    (while (< char 127)
      (define-key keymap (char-to-string char) 'scheme-debugger-self-insert)
      (setq char (1+ char)))))

;; Initialize the debugger mode map
(if (not scheme-debugger-mode-map)
    (progn
      (setq scheme-debugger-mode-map (make-keymap))
      (scheme-mode-commands scheme-debugger-mode-map)
      (xscheme-interrupt-commands scheme-debugger-mode-map)
      (scheme-debugger-mode-commands scheme-debugger-mode-map)))

(defun scheme-debugger-self-insert ()
  "Transmit this character to the Scheme process."
  (interactive)
  (xscheme-send-char last-command-event))

(defun xscheme-enter-debugger-mode (_prompt-string)
  (with-current-buffer (xscheme-process-buffer)
    (if (not (derived-mode-p 'scheme-debugger-mode))
	(progn
	  (if (not (derived-mode-p 'scheme-interaction-mode))
	      (scheme-interaction-mode t))
	  (scheme-debugger-mode-initialize)))))

(defun xscheme-debugger-mode-p ()
  (let ((buffer (xscheme-process-buffer)))
    (and buffer
	 (with-current-buffer buffer
	   (derived-mode-p 'scheme-debugger-mode)))))

;;;; Evaluation Commands

(defun xscheme-send-string (&rest strings)
  "Send the string arguments to the Scheme process.
The strings are concatenated and terminated by a newline."
  (cond ((not (xscheme-process-running-p))
	 (if (yes-or-no-p "The Scheme process has died.  Reset it? ")
	     (progn
	       (reset-scheme)
	       (xscheme-wait-for-process)
	       (xscheme-send-string-1 strings))))
	((xscheme-debugger-mode-p) (error "No sends allowed in debugger mode"))
	((and (not xscheme-allow-pipelined-evaluation)
	      xscheme-running-p)
	 (error "No sends allowed while Scheme running"))
	(t (xscheme-send-string-1 strings))))

(defun xscheme-send-string-1 (strings)
  (let ((string (apply 'concat strings)))
    (xscheme-send-string-2 string)
    (if (derived-mode-p 'scheme-interaction-mode)
	(xscheme-insert-expression string))))

(defun xscheme-send-string-2 (string)
  (let ((process (get-process xscheme-process-name)))
    (process-send-string process (concat string "\n"))
    (if (xscheme-process-buffer-current-p)
	(set-marker (process-mark process) (point)))))

(defun xscheme-select-process-buffer ()
  "Select the Scheme process buffer and move to its output point."
  (interactive)
  (let ((process
	 (or (get-process xscheme-process-name)
	     (error "No scheme process"))))
    (let ((buffer (or (process-buffer process) (error "No process buffer"))))
      (let ((window (get-buffer-window buffer)))
	(if window
	    (select-window window)
	    (switch-to-buffer buffer))
	(goto-char (process-mark process))))))

;;;; Scheme expressions ring

(defun xscheme-insert-expression (string)
  (setq xscheme-expressions-ring-yank-pointer
	(add-to-history 'xscheme-expressions-ring string
			xscheme-expressions-ring-max)))

(defun xscheme-rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring."
  (interactive "p")
  (let ((length (length xscheme-expressions-ring)))
    (if (zerop length)
	(error "Scheme expression ring is empty")
	(setq xscheme-expressions-ring-yank-pointer
	      (let ((index
		     (% (+ arg
			   (- length
			      (length xscheme-expressions-ring-yank-pointer)))
			length)))
		(nthcdr (if (< index 0)
			    (+ index length)
			    index)
			xscheme-expressions-ring))))))

(defun xscheme-yank (&optional arg)
  "Insert the most recent expression at point.
With just C-U as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently sent expression.
See also the commands \\[xscheme-yank-pop] and \\[xscheme-yank-push]."
  (interactive "*P")
  (xscheme-rotate-yank-pointer (if (listp arg) 0
				 (if (eq arg '-) -1
				   (1- arg))))
  (push-mark (point))
  (insert (car xscheme-expressions-ring-yank-pointer))
  (if (consp arg)
      (exchange-point-and-mark)))

;; Old name, to avoid errors in users' init files.
(fset 'xscheme-yank-previous-send
      'xscheme-yank)

(defun xscheme-yank-pop (arg)
  "Insert or replace a just-yanked expression with an older expression.
If the previous command was not a yank, it yanks.
Otherwise, the region contains a stretch of reinserted
expression.  yank-pop deletes that text and inserts in its
place a different expression.

With no argument, the next older expression is inserted.
With argument n, the n'th older expression is inserted.
If n is negative, this is a more recent expression.

The sequence of expressions wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (setq this-command 'xscheme-yank)
  (if (not (eq last-command 'xscheme-yank))
      (progn
	(xscheme-yank)
	(setq arg (- arg 1))))
  (if (not (= arg 0))
      (let ((before (< (point) (mark))))
	(delete-region (point) (mark))
	(xscheme-rotate-yank-pointer arg)
	(set-mark (point))
	(insert (car xscheme-expressions-ring-yank-pointer))
	(if before (exchange-point-and-mark)))))

(defun xscheme-yank-push (arg)
  "Insert or replace a just-yanked expression with a more recent expression.
If the previous command was not a yank, it yanks.
Otherwise, the region contains a stretch of reinserted
expression.  yank-pop deletes that text and inserts in its
place a different expression.

With no argument, the next more recent expression is inserted.
With argument n, the n'th more recent expression is inserted.
If n is negative, a less recent expression is used.

The sequence of expressions wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (xscheme-yank-pop (- 0 arg)))

(defun xscheme-send-region (start end)
  "Send the current region to the Scheme process.
The region is sent terminated by a newline."
  (interactive "r")
  (if (xscheme-process-buffer-current-p)
      (progn
	(goto-char end)
	(if (not (bolp))
	    (insert-before-markers ?\n))
	(set-marker (process-mark (get-process xscheme-process-name))
		    (point))
	(set-marker xscheme-last-input-end (point))))
  (xscheme-send-string (buffer-substring start end)))

(defun xscheme-send-definition ()
  "Send the current definition to the Scheme process.
If the current line begins with a non-whitespace character,
parse an expression from the beginning of the line and send that instead."
  (interactive)
  (let ((start nil) (end nil))
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (if (re-search-backward "^\\s(" nil t)
	  (setq start (point))
	  (error "Can't find definition")))
    (xscheme-send-region start end)))

(defun xscheme-send-next-expression ()
  "Send the expression to the right of `point' to the Scheme process."
  (interactive)
  (let ((start (point)))
    (xscheme-send-region start (save-excursion (forward-sexp) (point)))))

(defun xscheme-send-previous-expression ()
  "Send the expression to the left of `point' to the Scheme process."
  (interactive)
  (let ((end (point)))
    (xscheme-send-region (save-excursion (backward-sexp) (point)) end)))

(defun xscheme-send-current-line ()
  "Send the current line to the Scheme process.
Useful for working with debugging Scheme under adb."
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (insert ?\n)
    (xscheme-send-string-2 line)))

(defun xscheme-send-buffer ()
  "Send the current buffer to the Scheme process."
  (interactive)
  (if (xscheme-process-buffer-current-p)
      (error "Not allowed to send this buffer's contents to Scheme"))
  (xscheme-send-region (point-min) (point-max)))

(defun xscheme-send-char (char)
  "Prompt for a character and send it to the Scheme process."
  (interactive "cCharacter to send: ")
  (process-send-string xscheme-process-name (char-to-string char)))

(defun xscheme-delete-output ()
  "Delete all output from interpreter since last input."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (save-excursion
      (goto-char (process-mark proc))
      (re-search-backward
       "^;\\(Unspecified return value$\\|Value\\( [0-9]+\\)?: \\|\\(Abort\\|Up\\|Quit\\)!$\\)"
       xscheme-last-input-end
       t)
      (forward-line 0)
      (if (< (marker-position xscheme-last-input-end) (point))
	  (progn
	    (delete-region xscheme-last-input-end (point))
	    (insert-before-markers "*** output flushed ***\n"))))))

;;;; Interrupts

(defun xscheme-send-breakpoint-interrupt ()
  "Cause the Scheme process to enter a breakpoint."
  (interactive)
  (xscheme-send-interrupt ?b nil))

(defun xscheme-send-proceed ()
  "Cause the Scheme process to proceed from a breakpoint."
  (interactive)
  (process-send-string xscheme-process-name "(proceed)\n"))

(defconst xscheme-control-g-message-string
  "Sending C-G interrupt to Scheme...")

(defun xscheme-send-control-g-interrupt ()
  "Cause the Scheme processor to halt and flush input.
Control returns to the top level rep loop."
  (interactive)
  (let ((inhibit-quit t))
    (cond ((not xscheme-control-g-synchronization-p)
	   (interrupt-process xscheme-process-name))
	  ((with-current-buffer xscheme-buffer-name
	     xscheme-control-g-disabled-p)
	   (message "Relax..."))
	  (t
	   (with-current-buffer xscheme-buffer-name
	     (setq xscheme-control-g-disabled-p t))
	   (message xscheme-control-g-message-string)
	   (interrupt-process xscheme-process-name)
	   (sleep-for 0.1)
	   (xscheme-send-char 0)))))

(defun xscheme-send-control-u-interrupt ()
  "Cause the Scheme process to halt, returning to previous rep loop."
  (interactive)
  (xscheme-send-interrupt ?u t))

(defun xscheme-send-control-x-interrupt ()
  "Cause the Scheme process to halt, returning to current rep loop."
  (interactive)
  (xscheme-send-interrupt ?x t))

;;; This doesn't really work right -- Scheme just gobbles the first
;;; character in the input.  There is no way for us to guarantee that
;;; the argument to this procedure is the first char unless we put
;;; some kind of marker in the input stream.

(defun xscheme-send-interrupt (char mark-p)
  "Send a ^A type interrupt to the Scheme process."
  (interactive "cInterrupt character to send: ")
  (quit-process xscheme-process-name)
  (sleep-for 0.1)
  (xscheme-send-char char)
  (if (and mark-p xscheme-control-g-synchronization-p)
      (xscheme-send-char 0)))

;;;; Basic Process Control

(defun xscheme-start-process (command-line the-process the-buffer)
  (let ((buffer (get-buffer-create the-buffer)))
    (let ((process (get-buffer-process buffer)))
      (with-current-buffer buffer
	(if (and process (memq (process-status process) '(run stop)))
	    (set-marker (process-mark process) (point-max))
	    (progn (if process (delete-process process))
		   (goto-char (point-max))
		   (scheme-interaction-mode nil)
		   (setq xscheme-process-name the-process)
		   (if (bobp)
		       (insert-before-markers
			(substitute-command-keys xscheme-startup-message)))
		   (setq process
			 (let ((process-connection-type nil))
			   (apply 'start-process
				  (cons the-process
					(cons buffer
					      (xscheme-parse-command-line
					       command-line))))))
		   (if (not (equal (process-name process) the-process))
		       (setq xscheme-process-name (process-name process)))
		   (if (not (equal (buffer-name buffer) the-buffer))
		       (setq xscheme-buffer-name (buffer-name buffer)))
		   (message "Starting process %s in buffer %s"
			    xscheme-process-name
			    xscheme-buffer-name)
		   (set-marker (process-mark process) (point-max))
		   (xscheme-process-filter-initialize t)
		   (xscheme-modeline-initialize xscheme-buffer-name)
		   (set-process-sentinel process 'xscheme-process-sentinel)
		   (set-process-filter process 'xscheme-process-filter)
		   (run-hooks 'xscheme-start-hook)))))
    buffer))

(defun xscheme-parse-command-line (string)
  (setq string (substitute-in-file-name string))
  (let ((start 0)
	(result '()))
    (while start
      (let ((index (string-match "[ \t]" string start)))
	(setq start
	      (cond ((not index)
		     (setq result
			   (cons (substring string start)
				 result))
		     nil)
		    ((= index start)
		     (string-match "[^ \t]" string start))
		    (t
		     (setq result
			   (cons (substring string start index)
				 result))
		     (1+ index))))))
    (nreverse result)))

(defun xscheme-wait-for-process ()
  (sleep-for 2)
  (while xscheme-running-p
    (sleep-for 1)))

(defun xscheme-process-running-p ()
  "True if there is a Scheme process whose status is `run'."
  (let ((process (get-process xscheme-process-name)))
    (and process
	 (eq (process-status process) 'run))))

(defun xscheme-process-buffer ()
  (let ((process (get-process xscheme-process-name)))
    (and process (process-buffer process))))

(defun xscheme-process-buffer-window ()
  (let ((buffer (xscheme-process-buffer)))
    (and buffer (get-buffer-window buffer))))

(defun xscheme-process-buffer-current-p ()
  "True if the current buffer is the Scheme process buffer."
  (eq (xscheme-process-buffer) (current-buffer)))

;;;; Process Filter Operations

(defvar xscheme-process-filter-alist
  '((?A xscheme-eval
	xscheme-process-filter:string-action-noexcursion)
    (?D xscheme-enter-debugger-mode
	xscheme-process-filter:string-action)
    (?E xscheme-eval
	xscheme-process-filter:string-action)
    (?P xscheme-set-prompt-variable
	xscheme-process-filter:string-action)
    (?R xscheme-enter-interaction-mode
	xscheme-process-filter:simple-action)
    (?b xscheme-start-gc
	xscheme-process-filter:simple-action)
    (?c xscheme-unsolicited-read-char
	xscheme-process-filter:simple-action)
    (?e xscheme-finish-gc
	xscheme-process-filter:simple-action)
    (?f xscheme-exit-input-wait
	xscheme-process-filter:simple-action)
    (?g xscheme-enable-control-g
	xscheme-process-filter:simple-action)
    (?i xscheme-prompt-for-expression
	xscheme-process-filter:string-action)
    (?m xscheme-message
	xscheme-process-filter:string-action)
    (?n xscheme-prompt-for-confirmation
	xscheme-process-filter:string-action)
    (?o xscheme-output-goto
	xscheme-process-filter:simple-action)
    (?p xscheme-set-prompt
	xscheme-process-filter:string-action)
    (?s xscheme-enter-input-wait
	xscheme-process-filter:simple-action)
    (?v xscheme-write-value
	xscheme-process-filter:string-action)
    (?w xscheme-cd
	xscheme-process-filter:string-action)
    (?z xscheme-display-process-buffer
	xscheme-process-filter:simple-action))
  "Table used to decide how to handle process filter commands.
Value is a list of entries, each entry is a list of three items.

The first item is the character that the process filter dispatches on.
The second item is the action to be taken, a function.
The third item is the handler for the entry, a function.

When the process filter sees a command whose character matches a
particular entry, it calls the handler with two arguments: the action
and the string containing the rest of the process filter's input
stream.  It is the responsibility of the handler to invoke the action
with the appropriate arguments, and to reenter the process filter with
the remaining input.")

;;;; Process Filter

(defun xscheme-process-sentinel (proc reason)
  (let* ((buffer (process-buffer proc))
	 (name (buffer-name buffer)))
    (with-current-buffer buffer
      (xscheme-process-filter-initialize (eq reason 'run))
      (if (not (eq reason 'run))
	  (progn
	    (setq scheme-mode-line-process "")
	    (setq xscheme-mode-string "no process")
	    (if (equal name (default-value 'xscheme-buffer-name))
		(setq-default xscheme-runlight ""))))
      (if (and (not (memq reason '(run stop)))
	       xscheme-signal-death-message)
	  (progn
	    (beep)
	    (message
"The Scheme process has died!  Do M-x reset-scheme to restart it"))))))

(defun xscheme-process-filter-initialize (running-p)
  (setq xscheme-process-filter-state 'idle)
  (setq xscheme-running-p running-p)
  (setq xscheme-control-g-disabled-p nil)
  (setq xscheme-allow-output-p t)
  (setq xscheme-prompt "")
  (if running-p
      (let ((name (buffer-name (current-buffer))))
	(setq scheme-mode-line-process '(": " xscheme-runlight-string))
	(xscheme-modeline-initialize name)
	(if (equal name (default-value 'xscheme-buffer-name))
	    (setq-default xscheme-runlight default-xscheme-runlight))))
  (if (or (eq xscheme-runlight default-xscheme-runlight)
	  (equal xscheme-runlight ""))
      (setq xscheme-runlight (list ": " 'xscheme-buffer-name ": " "?")))
  (rplaca (nthcdr 3 xscheme-runlight)
	  (if running-p "?" "no process")))

(defun xscheme-process-filter (proc string)
  (let ((xscheme-filter-input string)
	(call-noexcursion nil))
    (while xscheme-filter-input
      (setq call-noexcursion nil)
      (with-current-buffer (process-buffer proc)
	(cond ((eq xscheme-process-filter-state 'idle)
	       (let ((start (string-match "\e" xscheme-filter-input)))
		 (if start
		     (progn
		       (xscheme-process-filter-output
			(substring xscheme-filter-input 0 start))
		       (setq xscheme-filter-input
			     (substring xscheme-filter-input (1+ start)))
		       (setq xscheme-process-filter-state 'reading-type))
		   (let ((string xscheme-filter-input))
		     (setq xscheme-filter-input nil)
		     (xscheme-process-filter-output string)))))
	      ((eq xscheme-process-filter-state 'reading-type)
	       (if (zerop (length xscheme-filter-input))
		   (setq xscheme-filter-input nil)
		 (let ((char (aref xscheme-filter-input 0)))
		   (setq xscheme-filter-input
			 (substring xscheme-filter-input 1))
		   (let ((entry (assoc char xscheme-process-filter-alist)))
		     (if entry
			 (funcall (nth 2 entry) (nth 1 entry))
		       (progn
			 (xscheme-process-filter-output ?\e char)
			 (setq xscheme-process-filter-state 'idle)))))))
	      ((eq xscheme-process-filter-state 'reading-string)
	       (let ((start (string-match "\e" xscheme-filter-input)))
		 (if start
		     (let ((string
			    (concat xscheme-string-accumulator
				    (substring xscheme-filter-input 0 start))))
		       (setq xscheme-filter-input
			     (substring xscheme-filter-input (1+ start)))
		       (setq xscheme-process-filter-state 'idle)
		       (if (listp xscheme-string-receiver)
			   (progn
			     (setq xscheme-string-receiver
				   (car xscheme-string-receiver))
			     (setq call-noexcursion string))
			 (funcall xscheme-string-receiver string)))
		   (progn
		     (setq xscheme-string-accumulator
			   (concat xscheme-string-accumulator
				   xscheme-filter-input))
		     (setq xscheme-filter-input nil)))))
	      (t
	       (error "Scheme process filter -- bad state"))))
      (if call-noexcursion
	  (funcall xscheme-string-receiver call-noexcursion)))))

;;;; Process Filter Output

(defun xscheme-process-filter-output (&rest args)
  (if xscheme-allow-output-p
      (let ((string (apply 'concat args)))
	(save-excursion
	  (xscheme-goto-output-point)
	  (let ((old-point (point)))
	    (while (string-match "\\(\007\\|\f\\)" string)
	      (let ((start (match-beginning 0)))
		(insert-before-markers (substring string 0 start))
		(if (= ?\f (aref string start))
		    (progn
		      (if (not (bolp))
			  (insert-before-markers ?\n))
		      (insert-before-markers ?\f))
		    (beep))
		(setq string (substring string (1+ start)))))
	    (insert-before-markers string)
	    (if (and xscheme-last-input-end
		     (equal (marker-position xscheme-last-input-end) (point)))
		(set-marker xscheme-last-input-end old-point)))))))

(defun xscheme-guarantee-newlines (n)
  (if xscheme-allow-output-p
      (save-excursion
	(xscheme-goto-output-point)
	(let ((stop nil))
	  (while (and (not stop)
		      (bolp))
	    (setq n (1- n))
	    (if (bobp)
		(setq stop t)
		(backward-char))))
	(xscheme-goto-output-point)
	(while (> n 0)
	  (insert-before-markers ?\n)
	  (setq n (1- n))))))

(defun xscheme-goto-output-point ()
  (let ((process (get-process xscheme-process-name)))
    (set-buffer (process-buffer process))
    (goto-char (process-mark process))))

(defun xscheme-modeline-initialize (name)
  (setq xscheme-runlight-string "")
  (if (equal name (default-value 'xscheme-buffer-name))
      (setq-default xscheme-runlight-string ""))
  (setq xscheme-mode-string "")
  (setq mode-line-buffer-identification
	(list (concat name ": ")
	      'xscheme-mode-string)))

(defun xscheme-set-runlight (runlight)
  (setq xscheme-runlight-string runlight)
  (if (equal (buffer-name (current-buffer))
	     (default-value 'xscheme-buffer-name))
      (setq-default xscheme-runlight-string runlight))
  (rplaca (nthcdr 3 xscheme-runlight) runlight)
  (force-mode-line-update t))

(defun xscheme-process-filter:simple-action (action)
  (setq xscheme-process-filter-state 'idle)
  (funcall action))

(defun xscheme-process-filter:string-action (action)
  (setq xscheme-string-receiver action)
  (setq xscheme-string-accumulator "")
  (setq xscheme-process-filter-state 'reading-string))

(defun xscheme-process-filter:string-action-noexcursion (action)
  (xscheme-process-filter:string-action (cons action nil)))

(defconst xscheme-runlight:running "run"
  "The character displayed when the Scheme process is running.")

(defconst xscheme-runlight:input "input"
  "The character displayed when the Scheme process is waiting for input.")

(defconst xscheme-runlight:gc "gc"
  "The character displayed when the Scheme process is garbage collecting.")

(defun xscheme-start-gc ()
  (xscheme-set-runlight xscheme-runlight:gc))

(defun xscheme-finish-gc ()
  (xscheme-set-runlight
   (if xscheme-running-p xscheme-runlight:running xscheme-runlight:input)))

(defun xscheme-enter-input-wait ()
  (xscheme-set-runlight xscheme-runlight:input)
  (setq xscheme-control-g-disabled-p nil)
  (setq xscheme-running-p nil))

(defun xscheme-exit-input-wait ()
  (xscheme-set-runlight xscheme-runlight:running)
  (setq xscheme-running-p t))

(defun xscheme-enable-control-g ()
  (setq xscheme-control-g-disabled-p nil)
  (if (string= (current-message) xscheme-control-g-message-string)
      (message nil)))

(defun xscheme-display-process-buffer ()
  (let ((window (or (xscheme-process-buffer-window)
		    (display-buffer (xscheme-process-buffer)))))
    (save-window-excursion
      (select-window window)
      (xscheme-goto-output-point)
      (if (xscheme-debugger-mode-p)
	  (xscheme-enter-interaction-mode)))))

(defun xscheme-unsolicited-read-char ()
  nil)

(defun xscheme-eval (string)
  (eval (car (read-from-string string))))

(defun xscheme-message (string)
  (if (not (zerop (length string)))
      (xscheme-write-message-1 string (format ";%s" string))))

(defun xscheme-write-value (string)
  (if (zerop (length string))
      (xscheme-write-message-1 "(no value)" ";Unspecified return value")
      (xscheme-write-message-1 string (format ";Value: %s" string))))

(defun xscheme-write-message-1 (message-string output-string)
  (let* ((process (get-process xscheme-process-name))
	 (window (get-buffer-window (process-buffer process))))
    (if (or (not window)
	    (not (pos-visible-in-window-p (process-mark process)
					  window)))
	(message "%s" message-string)))
  (xscheme-guarantee-newlines 1)
  (xscheme-process-filter-output output-string))

(defun xscheme-set-prompt-variable (string)
  (setq xscheme-prompt string))

(defun xscheme-set-prompt (string)
  (setq xscheme-prompt string)
  (xscheme-guarantee-newlines 2)
  (setq xscheme-mode-string (xscheme-coerce-prompt string))
  (force-mode-line-update t))

(defun xscheme-output-goto ()
  (xscheme-goto-output-point)
  (xscheme-guarantee-newlines 2))

(defun xscheme-coerce-prompt (string)
  (if (string-match "^[0-9]+ \\[[^]]+\\] " string)
      (let ((end (match-end 0)))
	(xscheme-process-filter-output (substring string end))
	(substring string 0 (- end 1)))
      string))

(defun xscheme-cd (directory-string)
  (with-current-buffer (xscheme-process-buffer)
    (cd directory-string)))

(defun xscheme-prompt-for-confirmation (prompt-string)
  (xscheme-send-char (if (y-or-n-p prompt-string) ?y ?n)))

(defvar xscheme-prompt-for-expression-map nil)
(if (not xscheme-prompt-for-expression-map)
    (progn
      (setq xscheme-prompt-for-expression-map
	    (copy-keymap minibuffer-local-map))
      (substitute-key-definition 'exit-minibuffer
				 'xscheme-prompt-for-expression-exit
				 xscheme-prompt-for-expression-map)))

(defun xscheme-prompt-for-expression (prompt-string)
  (xscheme-send-string-2
   (read-from-minibuffer prompt-string nil xscheme-prompt-for-expression-map)))

(defun xscheme-prompt-for-expression-exit ()
  (interactive)
  (if (eq (xscheme-region-expression-p (point-min) (point-max)) 'one)
      (exit-minibuffer)
      (error "input must be a single, complete expression")))

(defun xscheme-region-expression-p (start end)
  (save-excursion
    (let ((old-syntax-table (syntax-table)))
      (unwind-protect
	  (progn
	    (set-syntax-table scheme-mode-syntax-table)
	    (let ((state (parse-partial-sexp start end)))
	      (and (zerop (car state))	;depth = 0
		   (nth 2 state)	;last-sexp exists, i.e. >= 1 sexps
		   (let ((state (parse-partial-sexp start (nth 2 state))))
		     (if (nth 2 state) 'many 'one)))))
	(set-syntax-table old-syntax-table)))))

(provide 'xscheme)

;;; xscheme.el ends here
