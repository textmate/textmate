;;; terminal.el --- terminal emulator for GNU Emacs

;; Copyright (C) 1986-1989, 1993-1994, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@eddie.mit.edu>
;; Maintainer: FSF
;; Keywords: comm, terminals

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

;;; This file has been censored by the Communications Decency Act.
;;; That law was passed under the guise of a ban on pornography, but
;;; it bans far more than that.  This file did not contain pornography,
;;; but it was censored nonetheless.

;;; For information on US government censorship of the Internet, and
;;; what you can do to bring back freedom of the press, see the web
;;; site http://www.vtw.org/

;;; Code:

;;>>TODO
;;>> ** Nothing can be done about emacs' meta-lossage **
;;>>  (without redoing keymaps `sanely' -- ask Mly for details)

;;>> One probably wants to do setenv MORE -c when running with
;;>>   more-processing enabled.

(require 'ehelp)

(defgroup terminal nil
  "Terminal emulator for Emacs."
  :group 'terminals)


(defcustom terminal-escape-char ?\C-^
  "All characters except for this are passed verbatim through the
terminal-emulator.  This character acts as a prefix for commands
to the emulator program itself.  Type this character twice to send
it through the emulator.  Type ? after typing it for a list of
possible commands.
This variable is local to each terminal-emulator buffer."
  :type 'character
  :group 'terminal)

(defcustom terminal-scrolling t ;;>> Setting this to t sort-of defeats my whole aim in writing this package...
  "If non-nil, the terminal-emulator will losingly `scroll' when output occurs
past the bottom of the screen.  If nil, output will win and `wrap' to the top
of the screen.
This variable is local to each terminal-emulator buffer."
  :type 'boolean
  :group 'terminal)

(defcustom terminal-more-processing t
  "If non-nil, do more-processing.
This variable is local to each terminal-emulator buffer."
  :type 'boolean
  :group 'terminal)

;; If you are the sort of loser who uses scrolling without more breaks
;; and expects to actually see anything, you should probably set this to
;; around 400
(defcustom terminal-redisplay-interval 5000
  "Maximum number of characters which will be processed by the
terminal-emulator before a screen redisplay is forced.
Set this to a large value for greater throughput,
set it smaller for more frequent updates but overall slower
performance."
  :type 'integer
  :group 'terminal)

(defvar terminal-more-break-insertion
  "*** More break -- Press space to continue ***")

(defvar terminal-meta-map nil)
(if terminal-meta-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'te-pass-through)
    (setq terminal-meta-map map)))

(defvar terminal-map nil)
(if terminal-map
    nil
  (let ((map (make-sparse-keymap)))
    ;; Prevent defining [menu-bar] as te-pass-through
    ;; so we allow the global menu bar to be visible.
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [t] 'te-pass-through)
    (define-key map [switch-frame] 'handle-switch-frame)
    (define-key map "\e" terminal-meta-map)
    ;;(define-key map "\C-l"
    ;;  (lambda () (interactive) (te-pass-through) (redraw-display)))
    (setq terminal-map map)))

(defvar terminal-escape-map nil)
(if terminal-escape-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'undefined)
    (let ((s "0"))
      (while (<= (aref s 0) ?9)
	(define-key map s 'digit-argument)
	(aset s 0 (1+ (aref s 0)))))
    (define-key map "b" 'switch-to-buffer)
    (define-key map "o" 'other-window)
    (define-key map "e" 'te-set-escape-char)
    (define-key map "\C-l" 'redraw-display)
    (define-key map "\C-o" 'te-flush-pending-output)
    (define-key map "m" 'te-toggle-more-processing)
    (define-key map "x" 'te-escape-extended-command)
    ;;>> What use is this?  Why is it in the default terminal-emulator map?
    (define-key map "w" 'te-edit)
    (define-key map "?" 'te-escape-help)
    (define-key map (char-to-string help-char) 'te-escape-help)
    (setq terminal-escape-map map)))

(defvar te-escape-command-alist nil)
(if te-escape-command-alist
    nil
  (setq te-escape-command-alist
	'(("Set Escape Character" . te-set-escape-char)
          ;;>> What use is this?  Why is it in the default terminal-emulator map?
	  ("Edit" . te-edit)
	  ("Refresh" . redraw-display)
	  ("Record Output" . te-set-output-log)
	  ("Photo" . te-set-output-log)
	  ("Tofu" . te-tofu) ;; confuse the uninitiated
	  ("Stuff Input" . te-stuff-string)
	  ("Flush Pending Output" . te-flush-pending-output)
	  ("Enable More Processing" . te-enable-more-processing)
	  ("Disable More Processing" . te-disable-more-processing)
	  ("Scroll at end of page" . te-do-scrolling)
	  ("Wrap at end of page" . te-do-wrapping)
	  ("Switch To Buffer" . switch-to-buffer)
	  ("Other Window" . other-window)
	  ("Kill Buffer" . kill-buffer)
	  ("Help" . te-escape-help)
	  ("Set Redisplay Interval" . te-set-redisplay-interval)
	  )))

(defvar terminal-more-break-map nil)
(if terminal-more-break-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'te-more-break-unread)
    (define-key map (char-to-string help-char) 'te-more-break-help)
    (define-key map " " 'te-more-break-resume)
    (define-key map "\C-l" 'redraw-display)
    (define-key map "\C-o" 'te-more-break-flush-pending-output)
    ;;>>> this isn't right
    ;(define-key map "\^?" 'te-more-break-flush-pending-output) ;DEL
    (define-key map "\r" 'te-more-break-advance-one-line)

    (setq terminal-more-break-map map)))


;;; Pacify the byte compiler
(defvar te-process nil)
(defvar te-log-buffer nil)
(defvar te-height nil)
(defvar te-width nil)
(defvar te-more-count nil)
(defvar te-redisplay-count nil)
(defvar te-pending-output nil)
(defvar te-saved-point)
(defvar te-more-old-point nil)
(defvar te-more-old-local-map nil)
(defvar te-more-old-filter nil)
(defvar te-more-old-mode-line-format nil)
(defvar te-pending-output-info nil)

;; Required to support terminfo systems
(defconst te-terminal-name-prefix "emacs-em"
  "Prefix used for terminal type names for Terminfo.")
(defconst te-terminfo-directory
  (file-name-as-directory
   (expand-file-name "emacs-terminfo" temporary-file-directory))
  "Directory used for run-time terminal definition files for Terminfo.")
(defvar te-terminal-name nil)

;;;;  escape map

(defun te-escape ()
  (interactive)
  (let (s
        (local (current-local-map))
        (global (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map terminal-escape-map)
          (use-local-map terminal-escape-map)
          (setq s (read-key-sequence
                    (if current-prefix-arg
                        (format "Emacs Terminal escape[%s for help]> %d "
				(substitute-command-keys
				 "\\<terminal-escape-map>\\[te-escape-help]")
                                (prefix-numeric-value current-prefix-arg))
                        (format "Emacs Terminal escape[%s for help]> "
				(substitute-command-keys
				 "\\<terminal-escape-map>\\[te-escape-help]"))))))
      (use-global-map global)
      (use-local-map local))

    (message "")

    (cond
     ;;  Certain keys give vector notation, like [escape] when
     ;;  you hit esc key...
     ((and (stringp s)
	   (string= s (make-string 1 terminal-escape-char)))
      (setq last-command-event terminal-escape-char)
      (let ((terminal-escape-char -259))
	(te-pass-through)))

     ((setq s (lookup-key terminal-escape-map s))
      (call-interactively s)))

    ))


(defun te-escape-help ()
  "Provide help on commands available after terminal-escape-char is typed."
  (interactive)
  (message "Terminal emulator escape help...")
  (let ((char (single-key-description terminal-escape-char)))
    (with-electric-help
      (function (lambda ()
	 (princ (format "Terminal-emulator escape, invoked by \"%s\"
Type \"%s\" twice to send a single \"%s\" through.

Other chars following \"%s\" are interpreted as follows:\n"
			char char char char))

	 (princ (substitute-command-keys "\\{terminal-escape-map}\n"))
	 (princ (format "\nSubcommands of \"%s\" (%s)\n"
			(where-is-internal 'te-escape-extended-command
					   terminal-escape-map t)
			'te-escape-extended-command))
	 (let ((l (sort (copy-sequence te-escape-command-alist)
			(function (lambda (a b)
				    (string< (car a) (car b)))))))
	   (while l
	     (let ((doc (or (documentation (cdr (car l)))
			    "Not documented")))
	       (if (string-match "\n" doc)
		   ;; just use first line of documentation
		   (setq doc (substring doc 0 (match-beginning 0))))
	       (princ "  \"")
	       (princ (car (car l)))
	       (princ "\":\n     ")
	       (princ doc)
	       (write-char ?\n))
	     (setq l (cdr l))))
	 nil)))))



(defun te-escape-extended-command ()
  (interactive)
  (let ((c (let ((completion-ignore-case t))
	     (completing-read "terminal command: "
			      te-escape-command-alist
			      nil t))))
    (if c
	(catch 'foo
	  (setq c (downcase c))
	  (let ((l te-escape-command-alist))
	    (while l
	      (if (string= c (downcase (car (car l))))
		  (throw 'foo (call-interactively (cdr (car l))))
		(setq l (cdr l)))))))))

;; not used.
(defun te-escape-extended-command-unread ()
  (interactive)
  (setq unread-command-events (listify-key-sequence (this-command-keys)))
  (te-escape-extended-command))

(defun te-set-escape-char (c)
  "Change the terminal-emulator escape character."
  (interactive "cSet escape character to: ")
  (let ((o terminal-escape-char))
    (message (if (= o c)
		 "\"%s\" is the escape char"
	         "\"%s\" is now the escape; \"%s\" passes through")
	     (single-key-description c)
	     (single-key-description o))
    (setq terminal-escape-char c)))


(defun te-stuff-string (string)
  "Read a string to send to through the terminal emulator
as though that string had been typed on the keyboard.

Very poor man's file transfer protocol."
  (interactive "sStuff string: ")
  (process-send-string te-process string))

(defun te-set-output-log (name)
  "Record output from the terminal emulator in a buffer."
  (interactive (list (if te-log-buffer
			 nil
		       (read-buffer "Record output in buffer: "
				    (format "%s output-log"
					    (buffer-name (current-buffer)))
				    nil))))
  (if (or (null name) (equal name ""))
      (progn (setq te-log-buffer nil)
	     (message "Output logging off."))
    (if (get-buffer name)
	nil
      (with-current-buffer (get-buffer-create name)
	(fundamental-mode)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)))
    (setq te-log-buffer (get-buffer name))
    (message "Recording terminal emulator output into buffer \"%s\""
	     (buffer-name te-log-buffer))))

(defun te-tofu ()
  "Discontinue output log."
  (interactive)
  (te-set-output-log nil))


(defun te-toggle (sym arg)
  (set sym (cond ((not (numberp arg)) arg)
		 ((= arg 1) (not (symbol-value sym)))
		 ((< arg 0) nil)
		 (t t))))

(defun te-toggle-more-processing (arg)
  (interactive "p")
  (message (if (te-toggle 'terminal-more-processing arg)
	       "More processing on" "More processing off"))
  (if terminal-more-processing (setq te-more-count -1)))

(defun te-toggle-scrolling (arg)
  (interactive "p")
  (message (if (te-toggle 'terminal-scrolling arg)
	       "Scroll at end of page" "Wrap at end of page")))

(defun te-enable-more-processing ()
  "Enable ** MORE ** processing"
  (interactive)
  (te-toggle-more-processing t))

(defun te-disable-more-processing ()
  "Disable ** MORE ** processing"
  (interactive)
  (te-toggle-more-processing nil))

(defun te-do-scrolling ()
  "Scroll at end of page (yuck)"
  (interactive)
  (te-toggle-scrolling t))

(defun te-do-wrapping ()
  "Wrap to top of window at end of page"
  (interactive)
  (te-toggle-scrolling nil))


(defun te-set-redisplay-interval (arg)
  "Set the maximum interval (in output characters) between screen updates.
Set this number to large value for greater throughput,
set it smaller for more frequent updates (but overall slower performance."
  (interactive "NMax number of output chars between redisplay updates: ")
  (setq arg (max arg 1))
  (setq terminal-redisplay-interval arg
	te-redisplay-count 0))

;;;; more map

;; every command -must- call te-more-break-unwind
;; or grave lossage will result

(put 'te-more-break-unread 'suppress-keymap t)
(defun te-more-break-unread ()
  (interactive)
  (if (eq last-input-event terminal-escape-char)
      (call-interactively 'te-escape)
    (message "Continuing from more break (\"%s\" typed, %d chars output pending...)"
	     (single-key-description last-input-event)
	     (te-pending-output-length))
    (setq te-more-count 259259)
    (te-more-break-unwind)
    (let ((terminal-more-processing nil))
      (te-pass-through))))

(defun te-more-break-resume ()
  "Proceed past the **MORE** break,
allowing the next page of output to appear"
  (interactive)
  (message "Continuing from more break")
  (te-more-break-unwind))

(defun te-more-break-help ()
  "Provide help on commands available in a terminal-emulator **MORE** break"
  (interactive)
  (message "Terminal-emulator more break help...")
  (sit-for 0)
  (with-electric-help
    (function (lambda ()
      (princ "Terminal-emulator more break.\n\n")
      (princ (format "Type \"%s\" (te-more-break-resume)\n%s\n"
		     (where-is-internal 'te-more-break-resume
					terminal-more-break-map t)
		     (documentation 'te-more-break-resume)))
      (princ (substitute-command-keys "\\{terminal-more-break-map}\n"))
      (princ "Any other key is passed through to the program
running under the terminal emulator and disables more processing until
all pending output has been dealt with.")
      nil))))


(defun te-more-break-advance-one-line ()
  "Allow one more line of text to be output before doing another more break."
  (interactive)
  (setq te-more-count 1)
  (te-more-break-unwind))

(defun te-more-break-flush-pending-output ()
  "Discard any output which has been received by the terminal emulator but
not yet processed and then proceed from the more break."
  (interactive)
  (te-more-break-unwind)
  (te-flush-pending-output))

(defun te-flush-pending-output ()
  "Discard any as-yet-unprocessed output which has been received by
the terminal emulator."
  (interactive)
  ;; this could conceivably be confusing in the presence of
  ;; escape-sequences spanning process-output chunks
  (if (null (cdr te-pending-output))
      (message "(There is no output pending)")
    (let ((length (te-pending-output-length)))
      (message "Flushing %d chars of pending output" length)
      (setq te-pending-output
	    (list 0 (format "\n*** %d chars of pending output flushed ***\n"
			    length)))
      (te-update-pending-output-display)
      (te-process-output nil)
      (sit-for 0))))


(defun te-pass-through ()
  "Character is passed to the program running under the terminal emulator.
One characters is treated specially:
the terminal escape character (normally C-^)
lets you type a terminal emulator command."
  (interactive)
  (cond ((eq last-input-event terminal-escape-char)
	 (call-interactively 'te-escape))
	(t
	 ;; Convert `return' to C-m, etc.
	 (if (and (symbolp last-input-event)
		  (get last-input-event 'ascii-character))
	     (setq last-input-event (get last-input-event 'ascii-character)))
	 ;; Convert meta characters to 8-bit form for transmission.
	 (if (and (integerp last-input-event)
		  (not (zerop (logand last-input-event ?\M-\^@))))
	     (setq last-input-event (+ 128 (logand last-input-event 127))))
	 ;; Now ignore all but actual characters.
	 ;; (It ought to be possible to send through function
	 ;; keys as character sequences if we add a description
	 ;; to our termcap entry of what they should look like.)
	 (if (integerp last-input-event)
	     (progn
	       (and terminal-more-processing (null (cdr te-pending-output))
		    (te-set-more-count nil))
	       (process-send-string te-process (make-string 1 last-input-event))
	       (te-process-output t))
	   (message "Function key `%s' ignored"
		    (single-key-description last-input-event))))))


(defun te-set-window-start ()
  (let* ((w (get-buffer-window (current-buffer)))
	 (h (if w (window-height w))))
    (cond ((not w)) ; buffer not displayed
	  ((>= h (/ (- (point) (point-min)) (1+ te-width)))
	   ;; this is the normal case
	   (set-window-start w (point-min)))
	  ;; this happens if some vandal shrinks our window.
	  ((>= h (/ (- (point-max) (point)) (1+ te-width)))
	   (set-window-start w (- (point-max) (* h (1+ te-width)) -1)))
	  ;; I give up.
	  (t nil))))

(defun te-pending-output-length ()
  (let ((length (car te-pending-output))
	(tem (cdr te-pending-output)))
    (while tem
      (setq length (+ length (length (car tem))) tem (cdr tem)))
    length))

;;>> What use is this terminal-edit stuff anyway?
;;>>  If nothing else, it was written by somebody who didn't
;;>>  competently understand the terminal-emulator...

(defvar terminal-edit-map nil)
(if terminal-edit-map
    nil
  (setq terminal-edit-map (make-sparse-keymap))
  (define-key terminal-edit-map "\C-c\C-c" 'terminal-cease-edit))

;; Terminal Edit mode is suitable only for specially formatted data.
(put 'terminal-edit-mode 'mode-class 'special)

(defun terminal-edit-mode ()
  "Major mode for editing the contents of a terminal-emulator buffer.
The editing commands are the same as in Fundamental mode,
together with a command \\<terminal-edit-map>to return to terminal emulation: \\[terminal-cease-edit]."
  (use-local-map terminal-edit-map)
  (setq major-mode 'terminal-edit-mode)
  (setq mode-name "Terminal Edit")
  (setq mode-line-modified (default-value 'mode-line-modified))
  (setq mode-line-process nil)
  (run-mode-hooks 'terminal-edit-mode-hook))

(defun te-edit ()
  "Start editing the terminal emulator buffer with ordinary Emacs commands."
  (interactive)
  (terminal-edit-mode)
  (force-mode-line-update)
  ;; Make mode line update.
  (if (eq (key-binding "\C-c\C-c") 'terminal-cease-edit)
      (message "Editing: Type C-c C-c to return to Terminal")
    (message "%s"
	     (substitute-command-keys
	       "Editing: Type \\[terminal-cease-edit] to return to Terminal"))))

(defun terminal-cease-edit ()
  "Finish editing message; switch back to Terminal proper."
  (interactive)

  ;;>> emulator will blow out if buffer isn't exactly te-width x te-height
  (let ((buffer-read-only nil))
    (widen)
    (let ((opoint (point-marker))
          (width te-width)
          (h (1- te-height)))
      (goto-char (point-min))
      (while (>= h 0)
        (let ((p (point)))
          (cond ((search-forward "\n" (+ p width) 'move)
                 (forward-char -1)
                 (insert-char ?\s (- width (- (point) p)))
                 (forward-char 1))
                ((eobp)
                 (insert-char ?\s (- width (- (point) p))))
                ((= (following-char) ?\n)
                 (forward-char 1))
                (t
                 (setq p (point))
                 (if (search-forward "\n" nil t)
                     (delete-region p (1- (point)))
                     (delete-region p (point-max))))))
        (if (= h 0)
            (if (not (eobp)) (delete-region (point) (point-max)))
            (if (eobp) (insert ?\n)))
        (setq h (1- h)))
      (goto-char opoint)
      (set-marker opoint nil nil)
      (setq te-saved-point (point))
      (setq te-redisplay-count 0)
      (setq te-more-count -1)))

  (setq mode-line-modified (default-value 'mode-line-modified))
  (use-local-map terminal-map)
  (setq major-mode 'terminal-mode)
  (setq mode-name "terminal")
  (setq mode-line-process '(":%s")))

;;;; more break hair

(defun te-more-break ()
  (te-set-more-count t)
  (make-local-variable 'te-more-old-point)
  (setq te-more-old-point (point))
  (make-local-variable 'te-more-old-local-map)
  (setq te-more-old-local-map (current-local-map))
  (use-local-map terminal-more-break-map)
  (make-local-variable 'te-more-old-filter)
  (setq te-more-old-filter (process-filter te-process))
  (make-local-variable 'te-more-old-mode-line-format)
  (setq te-more-old-mode-line-format mode-line-format
	mode-line-format (list "--   **MORE**  "
			       mode-line-buffer-identification
			       "%-"))
  (set-process-filter te-process
    (function (lambda (process string)
		(with-current-buffer (process-buffer process)
		  (setq te-pending-output (nconc te-pending-output
						 (list string))))
		  (te-update-pending-output-display))))
  (te-update-pending-output-display)
  (if (eq (window-buffer (selected-window)) (current-buffer))
      (message "More break "))
  (or (eobp)
      (null terminal-more-break-insertion)
      (save-excursion
	(forward-char 1)
	(delete-region (point) (+ (point) te-width))
	(insert terminal-more-break-insertion)))
  (run-hooks 'terminal-more-break-hook)
  (sit-for 0) ;get display to update
  (throw 'te-process-output t))

(defun te-more-break-unwind ()
  (use-local-map te-more-old-local-map)
  (set-process-filter te-process te-more-old-filter)
  (goto-char te-more-old-point)
  (setq mode-line-format te-more-old-mode-line-format)
  (force-mode-line-update)
  (let ((buffer-read-only nil))
    (cond ((eobp))
	  (terminal-more-break-insertion
	   (forward-char 1)
	   (delete-region (point)
			  (+ (point) (length terminal-more-break-insertion)))
	   (insert-char ?\s te-width)
	   (goto-char te-more-old-point)))
    (setq te-more-old-point nil)
    (let ((te-more-count 259259))
      (te-newline)))
  ;(sit-for 0)
  (te-process-output t))

(defun te-set-more-count (newline)
  (let ((line (/ (- (point) (point-min)) (1+ te-width))))
    (if newline (setq line (1+ line)))
    (cond ((= line te-height)
	   (setq te-more-count te-height))
	  ;>>>> something is strange.  Investigate this!
	  ((= line (1- te-height))
	   (setq te-more-count te-height))
	  ((or (< line (/ te-height 2))
	       (> (- te-height line) 10))
	   ;; break at end of this page
	   (setq te-more-count (- te-height line)))
	  (t
	   ;; migrate back towards top (ie bottom) of screen.
	   (setq te-more-count (- te-height
				  (if (> te-height 10) 2 1)))))))


;;;; More or less straight-forward terminal escapes

;; ^j, meaning `newline' to non-display programs.
;; (Who would think of ever writing a system which doesn't understand
;;  display terminals natively?  Un*x:  The Operating System of the Future.)
(defun te-newline ()
  "Move down a line, optionally do more processing, perhaps wrap/scroll,
move to start of new line, clear to end of line."
  (end-of-line)
  (cond ((not terminal-more-processing))
	((< (setq te-more-count (1- te-more-count)) 0)
	 (te-set-more-count t))
	((eq te-more-count 0)
	 ;; this doesn't return
	 (te-more-break)))
  (if (eobp)
      (progn
	(delete-region (point-min) (+ (point-min) te-width))
	(goto-char (point-min))
	(if terminal-scrolling
	    (progn (delete-char 1)
		   (goto-char (point-max))
		   (insert ?\n))))
    (forward-char 1)
    (delete-region (point) (+ (point) te-width)))
  (insert-char ?\s te-width)
  (beginning-of-line)
  (te-set-window-start))

; ^p = x+32 y+32
(defun te-move-to-position ()
  ;; must offset by #o40 since cretinous unix won't send a 004 char through
  (let ((y (- (te-get-char) 32))
	(x (- (te-get-char) 32)))
    (if (or (> x te-width)
	    (> y te-height))
	()
      (goto-char (+ (point-min) x (* y (1+ te-width))))
      ;(te-set-window-start?)
      ))
  (setq te-more-count -1))



;; ^p c
(defun te-clear-rest-of-line ()
  (save-excursion
    (let ((n (- (point) (progn (end-of-line) (point)))))
      (delete-region (point) (+ (point) n))
      (insert-char ?\s (- n)))))


;; ^p C
(defun te-clear-rest-of-screen ()
  (save-excursion
    (te-clear-rest-of-line)
    (while (progn (end-of-line) (not (eobp)))
      (forward-char 1) (end-of-line)
      (delete-region (- (point) te-width) (point))
      (insert-char ?\s te-width))))


;; ^p ^l
(defun te-clear-screen ()
  ;; regenerate buffer to compensate for (nonexistent!!) bugs.
  (erase-buffer)
  (let ((i 0))
    (while (< i te-height)
      (setq i (1+ i))
      (insert-char ?\s te-width)
      (insert ?\n)))
  (delete-region (1- (point-max)) (point-max))
  (goto-char (point-min))
  (setq te-more-count -1))


;; ^p ^o count+32
(defun te-insert-lines ()
  (if (not (bolp))
      ();(error "fooI")
    (save-excursion
      (let* ((line (- te-height (/ (- (point) (point-min)) (1+ te-width)) -1))
	     (n (min (- (te-get-char) ?\s) line))
	     (i 0))
	(delete-region (- (point-max) (* n (1+ te-width))) (point-max))
	(if (eq (point) (point-max)) (insert ?\n))
	(while (< i n)
	  (setq i (1+ i))
	  (insert-char ?\s te-width)
	  (or (eq i line) (insert ?\n))))))
  (setq te-more-count -1))


;; ^p ^k count+32
(defun te-delete-lines ()
  (if (not (bolp))
      ();(error "fooD")
    (let* ((line (- te-height (/ (- (point) (point-min)) (1+ te-width)) -1))
	   (n (min (- (te-get-char) ?\s) line))
	   (i 0))
      (delete-region (point)
		     (min (+ (point) (* n (1+ te-width))) (point-max)))
      (save-excursion
	(goto-char (point-max))
	(while (< i n)
	  (setq i (1+ i))
	  (insert-char ?\s te-width)
	  (or (eq i line) (insert ?\n))))))
  (setq te-more-count -1))

;; ^p ^a
(defun te-beginning-of-line ()
  (beginning-of-line))

;; ^p ^b
(defun te-backward-char ()
  (if (not (bolp))
      (backward-char 1)))

;; ^p ^f
(defun te-forward-char ()
  (if (not (eolp))
      (forward-char 1)))


;; 0177
(defun te-delete ()
  (if (bolp)
      ()
    (delete-region (1- (point)) (point))
    (insert ?\s)
    (forward-char -1)))

;; ^p ^g
(defun te-beep ()
  (beep))


;; ^p _ count+32
(defun te-insert-spaces ()
  (let* ((p (point))
	 (n (min (- (te-get-char) 32)
		 (- (progn (end-of-line) (point)) p))))
    (if (<= n 0)
	nil
      (delete-char (- n))
      (goto-char p)
      (insert-char ?\s n))
    (goto-char p)))

;; ^p d count+32  (should be ^p ^d but cretinous un*x won't send ^d chars!!!)
(defun te-delete-char ()
  (let* ((p (point))
	 (n (min (- (te-get-char) 32)
		 (- (progn (end-of-line) (point)) p))))
    (if (<= n 0)
	nil
      (insert-char ?\s n)
      (goto-char p)
      (delete-char n))
    (goto-char p)))



;; disgusting unix-required excrement
;;  Are we living twenty years in the past yet?

(defun te-losing-unix ()
  nil)

;; ^i
(defun te-output-tab ()
  (let* ((p (point))
	 (x (- p (progn (beginning-of-line) (point))))
	 (l (min (- 8 (logand x 7))
		 (progn (end-of-line) (- (point) p)))))
    (goto-char (+ p l))))

;; ^p ^j
;; Handle the `do' or `nl' termcap capability.
;;>> I am not sure why this broken, obsolete, capability is here.
;;>> Perhaps it is for VIle.  No comment was made about why it
;;>> was added (in "Sun Dec  6 01:22:27 1987  Richard Stallman")
(defun te-down-vertically-or-scroll ()
  "Move down a line vertically, or scroll at bottom."
  (let ((column (current-column)))
    (end-of-line)
    (if (eobp)
	(progn
	  (delete-region (point-min) (+ (point-min) te-width))
	  (goto-char (point-min))
	  (delete-char 1)
	  (goto-char (point-max))
	  (insert ?\n)
	  (insert-char ?\s te-width)
	  (beginning-of-line))
      (forward-line 1))
    (move-to-column column))
  (te-set-window-start))

;; Also:
;;  ^m => beginning-of-line (for which it -should- be using ^p ^a, right?!!)
;;  ^g => te-beep (for which it should use ^p ^g)
;;  ^h => te-backward-char (for which it should use ^p ^b)



(defun te-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char te-saved-point)
    (and (bufferp te-log-buffer)
         (if (null (buffer-name te-log-buffer))
             ;; killed
             (setq te-log-buffer nil)
           (set-buffer te-log-buffer)
           (goto-char (point-max))
           (insert-before-markers string)
           (set-buffer (process-buffer process))))
    (setq te-pending-output (nconc te-pending-output (list string)))
    (te-update-pending-output-display)
    (te-process-output (eq (current-buffer)
                           (window-buffer (selected-window))))
    (set-buffer (process-buffer process))
    (setq te-saved-point (point))))

;; (A version of the following comment which might be distractingly offensive
;; to some readers has been moved to term-nasty.el.)
;; unix lacks ITS-style tty control...
(defun te-process-output (preemptible)
  ;;>> There seems no good reason to ever disallow preemption
  (setq preemptible t)
  (catch 'te-process-output
    (let ((buffer-read-only nil)
	  (string nil) ostring start char (matchpos nil))
      (while (cdr te-pending-output)
	(setq ostring string
	      start (car te-pending-output)
	      string (car (cdr te-pending-output))
	      char (aref string start))
	(if (eq (setq start (1+ start)) (length string))
	    (progn (setq te-pending-output
			   (cons 0 (cdr (cdr te-pending-output)))
			 start 0
			 string (car (cdr te-pending-output)))
		   (te-update-pending-output-display))
	    (setcar te-pending-output start))
	(if (and (> char ?\037) (< char ?\377))
	    (cond ((eolp)
		   ;; unread char
		   (if (eq start 0)
		       (setq te-pending-output
			     (cons 0 (cons (make-string 1 char)
					   (cdr te-pending-output))))
		       (setcar te-pending-output (1- start)))
		   (te-newline))
		  ((null string)
		   (delete-char 1) (insert char)
		   (te-redisplay-if-necessary 1))
		  (t
		   (let ((end (or (and (eq ostring string) matchpos)
				  (setq matchpos (string-match
						   "[\000-\037\177-\377]"
						   string start))
				  (length string))))
		     (delete-char 1) (insert char)
		     (setq char (point)) (end-of-line)
		     (setq end (min end (+ start (- (point) char))))
		     (goto-char char)
		     (if (eq end matchpos) (setq matchpos nil))
		     (delete-region (point) (+ (point) (- end start)))
		     (insert (if (and (eq start 0)
				      (eq end (length string)))
				 string
			         (substring string start end)))
		     (if (eq end (length string))
			 (setq te-pending-output
			       (cons 0 (cdr (cdr te-pending-output))))
		         (setcar te-pending-output end))
		     (te-redisplay-if-necessary (1+ (- end start))))))
	  ;; I suppose if I split the guts of this out into a separate
	  ;;  function we could trivially emulate different terminals
	  ;; Who cares in any case?  (Apart from stupid losers using rlogin)
	  (funcall
	    (if (eq char ?\^p)
	        (or (cdr (assq (te-get-char)
			       '((?= . te-move-to-position)
				 (?c . te-clear-rest-of-line)
				 (?C . te-clear-rest-of-screen)
				 (?\C-o . te-insert-lines)
				 (?\C-k . te-delete-lines)
				 ;; not necessary, but help sometimes.
				 (?\C-a . te-beginning-of-line)
				 (?\C-b . te-backward-char)
				 ;; should be C-d, but un*x
				 ;;  pty's won't send \004 through!
                                 ;; Can you believe this?
				 (?d . te-delete-char)
				 (?_ . te-insert-spaces)
				 ;; random
				 (?\C-f . te-forward-char)
				 (?\C-g . te-beep)
				 (?\C-j . te-down-vertically-or-scroll)
				 (?\C-l . te-clear-screen)
				 )))
		    'te-losing-unix)
	        (or (cdr (assq char
			       '((?\C-j . te-newline)
				 (?\177 . te-delete)
				 ;; Did I ask to be sent these characters?
				 ;; I don't remember doing so, either.
				 ;; (Perhaps some operating system or
				 ;; other is completely incompetent...)
				 (?\C-m . te-beginning-of-line)
				 (?\C-g . te-beep)
				 (?\C-h . te-backward-char)
				 (?\C-i . te-output-tab))))
		    'te-losing-unix)))
	  (te-redisplay-if-necessary 1))
	(and preemptible
	     (input-pending-p)
	     ;; preemptible output!  Oh my!!
	     (throw 'te-process-output t)))))
  ;; We must update window-point in every window displaying our buffer
  (walk-windows (lambda (w)
		  (when (and (not (eq w (selected-window)))
			     (eq (window-buffer w) (current-buffer)))
		    (set-window-point w (point))))))

(defun te-get-char ()
  (if (cdr te-pending-output)
      (let ((start (car te-pending-output))
	    (string (car (cdr te-pending-output))))
	(prog1 (aref string start)
	  (if (eq (setq start (1+ start)) (length string))
	      (setq te-pending-output (cons 0 (cdr (cdr te-pending-output))))
	      (setcar te-pending-output start))))
    (catch 'char
      (let ((filter (process-filter te-process)))
	(unwind-protect
	    (progn
	      (set-process-filter te-process
				  (function (lambda (_p s)
                                    (or (eq (length s) 1)
                                        (setq te-pending-output (list 1 s)))
                                    (throw 'char (aref s 0)))))
	      (accept-process-output te-process))
	  (set-process-filter te-process filter))))))


(defun te-redisplay-if-necessary (length)
  (and (<= (setq te-redisplay-count (- te-redisplay-count length)) 0)
       (eq (current-buffer) (window-buffer (selected-window)))
       (waiting-for-user-input-p)
       (progn (te-update-pending-output-display)
	      (sit-for 0)
	      (setq te-redisplay-count terminal-redisplay-interval))))

(defun te-update-pending-output-display ()
  (if (null (cdr te-pending-output))
      (setq te-pending-output-info "")
    (let ((length (te-pending-output-length)))
      (if (< length 1500)
	  (setq te-pending-output-info "")
	(setq te-pending-output-info (format "(%dK chars output pending) "
					     (/ (+ length 512) 1024))))))
  (force-mode-line-update))


(defun te-sentinel (process message)
  (cond ((eq (process-status process) 'run))
	((null (buffer-name (process-buffer process)))) ;deleted
	(t (let ((b (current-buffer)))
	     (with-current-buffer (process-buffer process)
	       (setq buffer-read-only nil)
	       (fundamental-mode)
	       (goto-char (point-max))
	       (delete-blank-lines)
	       (delete-horizontal-space)
	       (insert "\n*******\n" message "*******\n"))
	     (if (and (eq b (process-buffer process))
		      (waiting-for-user-input-p))
		 (progn (goto-char (point-max))
			(recenter -1)))))))

(defvar te-stty-string "stty -nl erase '^?' kill '^u' intr '^c' echo pass8"
  "Shell command to set terminal modes for terminal emulator.")
;; This used to have `new' in it, but that loses outside BSD
;; and it's apparently not needed in BSD.

(defcustom explicit-shell-file-name nil
  "If non-nil, is file name to use for explicitly requested inferior shell."
  :type '(choice (const :tag "None" nil)
		 file)
  :group 'terminal)

;;;###autoload
(defun terminal-emulator (buffer program args &optional width height)
  "Under a display-terminal emulator in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings.  Remaining arguments are WIDTH and HEIGHT.
BUFFER's contents are made an image of the display generated by that program,
and any input typed when BUFFER is the current Emacs buffer is sent to that
program as keyboard input.

Interactively, BUFFER defaults to \"*terminal*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.
WIDTH and HEIGHT are determined from the size of the current window
-- WIDTH will be one less than the window's width, HEIGHT will be its height.

To switch buffers and leave the emulator, or to give commands
to the emulator itself (as opposed to the program running under it),
type Control-^.  The following character is an emulator command.
Type Control-^ twice to send it to the subprogram.
This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

Here is a list of some of the variables which control the behavior
of the emulator -- see their documentation for more information:
terminal-escape-char, terminal-scrolling, terminal-more-processing,
terminal-redisplay-interval.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started."
  (interactive
   (cons (with-current-buffer (get-buffer-create "*terminal*")
           (buffer-name (if (or (not (boundp 'te-process))
                                (null te-process)
                                (not (eq (process-status te-process)
                                         'run)))
                            (current-buffer)
                          (generate-new-buffer "*terminal*"))))
         (append
          (let* ((default-s
                   ;; Default shell is same thing M-x shell uses.
                   (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))
                 (s (read-string
                     (format "Run program in emulator (default %s): "
                             default-s))))
            (if (equal s "")
                (list default-s '())
              (te-parse-program-and-args s))))))
  (switch-to-buffer buffer)
  (if (null width) (setq width (- (window-width (selected-window)) 1)))
  (if (null height) (setq height (- (window-height (selected-window)) 1)))
  (terminal-mode)
  (setq te-width width te-height height)
  (setq te-terminal-name (concat te-terminal-name-prefix
				 (number-to-string te-width)
				 (number-to-string te-height)))
  (setq mode-line-buffer-identification
	(list (format "Emacs terminal %dx%d: %%b  " te-width te-height)
	      'te-pending-output-info))
  (let ((buffer-read-only nil))
    (te-clear-screen))
  (let (process)
    (while (setq process (get-buffer-process (current-buffer)))
      (if (y-or-n-p (format "Kill process %s? " (process-name process)))
	  (delete-process process)
	(error "Process %s not killed" (process-name process)))))
  (condition-case err
      (let ((process-environment
	     (cons (concat "TERM=" te-terminal-name)
		   (cons (concat "TERMCAP=" (te-create-termcap))
			 (cons (concat "TERMINFO=" (te-create-terminfo))
			       process-environment)))))
	(setq te-process
	      (start-process "terminal-emulator" (current-buffer)
			     "/bin/sh" "-c"
			     ;; Yuck!!! Start a shell to set some terminal
			     ;; control characteristics.  Then start the
			     ;; "env" program to setup the terminal type
			     ;; Then finally start the program we wanted.
			     (format "%s; exec %s"
				     te-stty-string
				     (mapconcat 'te-quote-arg-for-sh
						(cons program args) " "))))
	(set-process-filter te-process 'te-filter)
	(set-process-sentinel te-process 'te-sentinel))
    (error (fundamental-mode)
	   (signal (car err) (cdr err))))
  (setq inhibit-quit t)			;sport death
  (use-local-map terminal-map)
  (run-hooks 'terminal-mode-hook)
  (message "Entering Emacs terminal-emulator...  Type %s %s for help"
	   (single-key-description terminal-escape-char)
	   (mapconcat 'single-key-description
		      (where-is-internal 'te-escape-help terminal-escape-map t)
		      " ")))


(defun te-parse-program-and-args (s)
  (cond ((string-match "\\`\\([-a-zA-Z0-9+=_.@/:]+[ \t]*\\)+\\'" s)
	 (let ((l ()) (p 0))
	   (while p
	     (setq l (cons (if (string-match
				"\\([-a-zA-Z0-9+=_.@/:]+\\)\\([ \t]+\\)*"
				s p)
			       (prog1 (substring s p (match-end 1))
				 (setq p (match-end 0))
				 (if (eq p (length s)) (setq p nil)))
			       (prog1 (substring s p)
				 (setq p nil)))
			   l)))
	   (setq l (nreverse l))
	   (list (car l) (cdr l))))
	((and (string-match "[ \t]" s) (not (file-exists-p s)))
	 (list shell-file-name (list "-c" (concat "exec " s))))
	(t (list s ()))))

(put 'terminal-mode 'mode-class 'special)
;; This is only separated out from function terminal-emulator
;; to keep the latter a little more manageable.
(defun terminal-mode ()
  "Set up variables for use with the terminal-emulator.
One should not call this -- it is an internal function
of the terminal-emulator"
  (kill-all-local-variables)
  (buffer-disable-undo (current-buffer))
  (setq major-mode 'terminal-mode)
  (setq mode-name "terminal")
; (make-local-variable 'Helper-return-blurb)
; (setq Helper-return-blurb "return to terminal simulator")
  (setq mode-line-process '(":%s"))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (make-local-variable 'terminal-escape-char)
  (setq terminal-escape-char (default-value 'terminal-escape-char))
  (make-local-variable 'terminal-scrolling)
  (setq terminal-scrolling (default-value 'terminal-scrolling))
  (make-local-variable 'terminal-more-processing)
  (setq terminal-more-processing (default-value 'terminal-more-processing))
  (make-local-variable 'terminal-redisplay-interval)
  (setq terminal-redisplay-interval (default-value 'terminal-redisplay-interval))
  (make-local-variable 'te-width)
  (make-local-variable 'te-height)
  (make-local-variable 'te-process)
  (make-local-variable 'te-pending-output)
  (setq te-pending-output (list 0))
  (make-local-variable 'te-saved-point)
  (setq te-saved-point (point-min))
  (make-local-variable 'te-pending-output-info) ;for the mode line
  (setq te-pending-output-info "")
  (make-local-variable 'inhibit-quit)
  ;(setq inhibit-quit t)
  (make-local-variable 'te-log-buffer)
  (setq te-log-buffer nil)
  (make-local-variable 'te-more-count)
  (setq te-more-count -1)
  (make-local-variable 'te-redisplay-count)
  (setq te-redisplay-count terminal-redisplay-interval)
  ;(use-local-map terminal-mode-map)
  ;; terminal-mode-hook is called above in function terminal-emulator
  )

;;;; what a complete loss

(defun te-quote-arg-for-sh (string)
  (cond ((string-match "\\`[-a-zA-Z0-9+=_.@/:]+\\'"
		       string)
	 string)
	((not (string-match "[$]" string))
	 ;; "[\"\\]" are special to sh and the lisp reader in the same way
	 (prin1-to-string string))
	(t
	 (let ((harder "")
	       (start 0)
	       (end 0))
	   (while (cond ((>= start (length string))
			 nil)
			;; this is the set of chars magic with "..." in `sh'
			((setq end (string-match "[\"\\$]"
						 string start))
			 t)
			(t (setq harder (concat harder
						(substring string start)))
			   nil))
	     (setq harder (concat harder (substring string start end)
                                  ;; Can't use ?\\ since `concat'
                                  ;; unfortunately does prin1-to-string
                                  ;; on fixna.  Amazing.
				  "\\"
				  (substring string
					     end
					     (1+ end)))
		   start (1+ end)))
	   (concat "\"" harder "\"")))))

(defun te-create-terminfo ()
  "Create and compile a terminfo entry for the virtual terminal. This is kept
in the directory specified by `te-terminfo-directory'."
  (when (and system-uses-terminfo
	     (not (file-exists-p (concat te-terminfo-directory
					 (substring te-terminal-name-prefix 0 1)
					 "/" te-terminal-name))))
    (let ( (terminfo
	    (concat
	     ;; The first newline avoids trouble with ncurses.
	     (format "%s,\n\tmir, xon,cols#%d, lines#%d,"
		     te-terminal-name te-width te-height)
	     "bel=^P^G, clear=^P\\f, cr=^P^A, cub1=^P^B, cud1=^P\\n,"
	     "cuf1=^P^F, cup=^P=%p1%'\\s'%+%c%p2%'\\s'%+%c,"
	     "dch=^Pd%p1%'\\s'%+%c, dch1=^Pd!, dl=^P^K%p1%'\\s'%+%c,"
	     "dl1=^P^K!, ed=^PC, el=^Pc, home=^P=\\s\\s,"
	     "ich=^P_%p1%'\\s'%+%c, ich1=^P_!, il=^P^O%p1%'\\s'%+%c,"
	     ;; The last newline avoids trouble with ncurses.
	     "il1=^P^O!, ind=^P\\n, nel=\\n,\n"))
	   ;; This is the desired name for the source file.
	   (file-name (concat te-terminfo-directory te-terminal-name ".tif")) )
      (make-directory te-terminfo-directory t)
      (let ((temp-file
	     (make-temp-file (expand-file-name "tif" te-terminfo-directory))))
	;; Store the source file under a random temp name.
	(with-temp-file temp-file
	  (insert terminfo))
	;; Rename it to the desired name.
	;; We use this roundabout approach
	;; to avoid any risk of writing a name that
	;; was mischievously set up as a symlink.
	(rename-file temp-file file-name))
      ;; Now compile that source to make the binary that the
      ;; programs actually use.
      (let ((process-environment
	     (cons (concat "TERMINFO="
			   (directory-file-name te-terminfo-directory))
		   process-environment)))
	(set-process-sentinel (start-process "tic" nil "tic" file-name)
			      'te-tic-sentinel))))
    (directory-file-name te-terminfo-directory))

(defun te-create-termcap ()
  "Create a termcap entry for the virtual terminal"
  ;; Because of Unix Brain Death(tm), we can't change
  ;;  the terminal type of a running process, and so
  ;;  terminal size and scrollability are wired-down
  ;;  at this point.  ("Detach?  What's that?")
  (concat (format "%s:co#%d:li#%d:%s"
		  ;; Sigh.  These can't be dynamically changed.
		  te-terminal-name te-width te-height (if terminal-scrolling
					 "" "ns:"))
	  ;;-- Basic things
	  ;; cursor-motion, bol, forward/backward char
	  "cm=^p=%+ %+ :cr=^p^a:le=^p^b:nd=^p^f:"
	  ;; newline, clear eof/eof, audible bell
	  "nw=^j:ce=^pc:cd=^pC:cl=^p^l:bl=^p^g:"
	  ;; insert/delete char/line
	  "IC=^p_%+ :DC=^pd%+ :AL=^p^o%+ :DL=^p^k%+ :"
	  ;;-- Not-widely-known (ie nonstandard) flags, which mean
	  ;; o writing in the last column of the last line
	  ;;   doesn't cause idiotic scrolling, and
	  ;; o don't use idiotische c-s/c-q sogenannte
	  ;;   ``flow control'' auf keinen Fall.
	  "LP:NF:"
	  ;;-- For stupid or obsolete programs
	  "ic=^p_!:dc=^pd!:al=^p^o!:dl=^p^k!:ho=^p=  :"
	  ;;-- For disgusting programs.
	  ;; (VI? What losers need these, I wonder?)
	  "im=:ei=:dm=:ed=:mi:do=^p^j:nl=^p^j:bs:")
)

(defun te-tic-sentinel (_proc state-change)
  "If tic has finished, delete the .tif file"
  (if (equal state-change "finished
")
      (delete-file (concat te-terminfo-directory te-terminal-name ".tif"))))

(provide 'terminal)

;;; terminal.el ends here
