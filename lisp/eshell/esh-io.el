;;; esh-io.el --- I/O management

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; At the moment, only output redirection is supported in Eshell.  To
;; use input redirection, the following syntax will work, assuming
;; that the command after the pipe is always an external command:
;;
;;   cat <file> | <command>
;;
;; Otherwise, output redirection and piping are provided in a manner
;; consistent with most shells.  Therefore, only unique features are
;; mentioned here.
;;
;;;_* Insertion
;;
;; To insert at the location of point in a buffer, use '>>>':
;;
;;   echo alpha >>> #<buffer *scratch*>;
;;
;;;_* Pseudo-devices
;;
;; A few pseudo-devices are provided, since Emacs cannot write
;; directly to a UNIX device file:
;;
;;   echo alpha > /dev/null   ; the bit bucket
;;   echo alpha > /dev/kill   ; set the kill ring
;;   echo alpha >> /dev/clip  ; append to the clipboard
;;
;;;_* Multiple output targets
;;
;; Eshell can write to multiple output targets, including pipes.
;; Example:
;;
;;   (+ 1 2) > a > b > c   ; prints number to all three files
;;   (+ 1 2) > a | wc      ; prints to 'a', and pipes to 'wc'

;;; Code:

(provide 'esh-io)

(eval-when-compile
  (require 'cl)
  (require 'eshell))

(defgroup eshell-io nil
  "Eshell's I/O management code provides a scheme for treating many
different kinds of objects -- symbols, files, buffers, etc. -- as
though they were files."
  :tag "I/O management"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-io-load-hook nil
  "A hook that gets run when `eshell-io' is loaded."
  :version "24.1"			; removed eshell-io-initialize
  :type 'hook
  :group 'eshell-io)

(defcustom eshell-number-of-handles 3
  "The number of file handles that eshell supports.
Currently this is standard input, output and error.  But even all of
these Emacs does not currently support with asynchronous processes
\(which is what eshell uses so that you can continue doing work in
other buffers) ."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-output-handle 1
  "The index of the standard output handle."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-error-handle 2
  "The index of the standard error handle."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-buffer-shorthand nil
  "If non-nil, a symbol name can be used for a buffer in redirection.
If nil, redirecting to a buffer requires buffer name syntax.  If this
variable is set, redirection directly to Lisp symbols will be
impossible.

Example:

  echo hello > '*scratch*  ; works if `eshell-buffer-shorthand' is t
  echo hello > #<buffer *scratch*>  ; always works"
  :type 'boolean
  :group 'eshell-io)

(defcustom eshell-print-queue-size 5
  "The size of the print queue, for doing buffered printing.
This is basically a speed enhancement, to avoid blocking the Lisp code
from executing while Emacs is redisplaying."
  :type 'integer
  :group 'eshell-io)

(defcustom eshell-virtual-targets
  '(("/dev/eshell" eshell-interactive-print nil)
    ("/dev/kill" (lambda (mode)
		   (if (eq mode 'overwrite)
		       (kill-new ""))
		   'eshell-kill-append) t)
    ("/dev/clip" (lambda (mode)
		   (if (eq mode 'overwrite)
		       (let ((x-select-enable-clipboard t))
			 (kill-new "")))
		   'eshell-clipboard-append) t))
  "Map virtual devices name to Emacs Lisp functions.
If the user specifies any of the filenames above as a redirection
target, the function in the second element will be called.

If the third element is non-nil, the redirection mode is passed as an
argument (which is the symbol `overwrite', `append' or `insert'), and
the function is expected to return another function -- which is the
output function.  Otherwise, the second element itself is the output
function.

The output function is then called repeatedly with single strings,
which represents successive pieces of the output of the command, until nil
is passed, meaning EOF.

NOTE: /dev/null is handled specially as a virtual target, and should
not be added to this variable."
  :type '(repeat
	  (list (string :tag "Target")
		function
		(choice (const :tag "Func returns output-func" t)
			(const :tag "Func is output-func" nil))))
  :group 'eshell-io)

(put 'eshell-virtual-targets 'risky-local-variable t)

;;; Internal Variables:

(defvar eshell-current-handles nil)

(defvar eshell-last-command-status 0
  "The exit code from the last command.  0 if successful.")

(defvar eshell-last-command-result nil
  "The result of the last command.  Not related to success.")

(defvar eshell-output-file-buffer nil
  "If non-nil, the current buffer is a file output buffer.")

(defvar eshell-print-count)
(defvar eshell-current-redirections)

;;; Functions:

(defun eshell-io-initialize ()
  "Initialize the I/O subsystem code."
  (add-hook 'eshell-parse-argument-hook
	    'eshell-parse-redirection nil t)
  (make-local-variable 'eshell-current-redirections)
  (add-hook 'eshell-pre-rewrite-command-hook
	    'eshell-strip-redirections nil t)
  (add-hook 'eshell-post-rewrite-command-hook
	    'eshell-apply-redirections nil t))

(defun eshell-parse-redirection ()
  "Parse an output redirection, such as '2>'."
  (if (and (not eshell-current-quoted)
	   (looking-at "\\([0-9]\\)?\\(<\\|>+\\)&?\\([0-9]\\)?\\s-*"))
      (if eshell-current-argument
	  (eshell-finish-arg)
	(let ((sh (match-string 1))
	      (oper (match-string 2))
;	      (th (match-string 3))
	      )
	  (if (string= oper "<")
	      (error "Eshell does not support input redirection"))
	  (eshell-finish-arg
	   (prog1
	       (list 'eshell-set-output-handle
		     (or (and sh (string-to-number sh)) 1)
		     (list 'quote
			   (aref [overwrite append insert]
				 (1- (length oper)))))
	     (goto-char (match-end 0))))))))

(defun eshell-strip-redirections (terms)
  "Rewrite any output redirections in TERMS."
  (setq eshell-current-redirections (list t))
  (let ((tl terms)
	(tt (cdr terms)))
    (while tt
      (if (not (and (consp (car tt))
		    (eq (caar tt) 'eshell-set-output-handle)))
	  (setq tt (cdr tt)
		tl (cdr tl))
	(unless (cdr tt)
	  (error "Missing redirection target"))
	(nconc eshell-current-redirections
	       (list (list 'ignore
			   (append (car tt) (list (cadr tt))))))
	(setcdr tl (cddr tt))
	(setq tt (cddr tt))))
    (setq eshell-current-redirections
	  (cdr eshell-current-redirections))))

(defun eshell-apply-redirections (cmdsym)
  "Apply any redirection which were specified for COMMAND."
  (if eshell-current-redirections
      (set cmdsym
	   (append (list 'progn)
		   eshell-current-redirections
		   (list (symbol-value cmdsym))))))

(defun eshell-create-handles
  (standard-output output-mode &optional standard-error error-mode)
  "Create a new set of file handles for a command.
The default location for standard output and standard error will go to
STANDARD-OUTPUT and STANDARD-ERROR, respectively.
OUTPUT-MODE and ERROR-MODE are either `overwrite', `append' or `insert';
a nil value of mode defaults to `insert'."
  (let ((handles (make-vector eshell-number-of-handles nil))
	(output-target (eshell-get-target standard-output output-mode))
	(error-target (eshell-get-target standard-error error-mode)))
    (aset handles eshell-output-handle (cons output-target 1))
    (if standard-error
	(aset handles eshell-error-handle (cons error-target 1))
      (aset handles eshell-error-handle (cons output-target 1)))
    handles))

(defun eshell-protect-handles (handles)
  "Protect the handles in HANDLES from a being closed."
  (let ((idx 0))
    (while (< idx eshell-number-of-handles)
      (if (aref handles idx)
	  (setcdr (aref handles idx)
		  (1+ (cdr (aref handles idx)))))
      (setq idx (1+ idx))))
  handles)

(defun eshell-close-target (target status)
  "Close an output TARGET, passing STATUS as the result.
STATUS should be non-nil on successful termination of the output."
  (cond
   ((symbolp target) nil)

   ;; If we were redirecting to a file, save the file and close the
   ;; buffer.
   ((markerp target)
    (let ((buf (marker-buffer target)))
      (when buf                         ; somebody's already killed it!
	(save-current-buffer
	  (set-buffer buf)
	  (when eshell-output-file-buffer
	    (save-buffer)
	    (when (eq eshell-output-file-buffer t)
	      (or status (set-buffer-modified-p nil))
	      (kill-buffer buf)))))))

   ;; If we're redirecting to a process (via a pipe, or process
   ;; redirection), send it EOF so that it knows we're finished.
   ((eshell-processp target)
    (if (eq (process-status target) 'run)
	(process-send-eof target)))

   ;; A plain function redirection needs no additional arguments
   ;; passed.
   ((functionp target)
    (funcall target status))

   ;; But a more complicated function redirection (which can only
   ;; happen with aliases at the moment) has arguments that need to be
   ;; passed along with it.
   ((consp target)
    (apply (car target) status (cdr target)))))

(defun eshell-close-handles (exit-code &optional result handles)
  "Close all of the current handles, taking refcounts into account.
EXIT-CODE is the process exit code; mainly, it is zero, if the command
completed successfully.  RESULT is the quoted value of the last
command.  If nil, then the meta variables for keeping track of the
last execution result should not be changed."
  (let ((idx 0))
    (assert (or (not result) (eq (car result) 'quote)))
    (setq eshell-last-command-status exit-code
	  eshell-last-command-result (cadr result))
    (while (< idx eshell-number-of-handles)
      (let ((handles (or handles eshell-current-handles)))
	(when (aref handles idx)
	  (setcdr (aref handles idx)
		  (1- (cdr (aref handles idx))))
	  (when (= (cdr (aref handles idx)) 0)
	    (let ((target (car (aref handles idx))))
	      (if (not (listp target))
		  (eshell-close-target target (= exit-code 0))
		(while target
		  (eshell-close-target (car target) (= exit-code 0))
		  (setq target (cdr target)))))
	    (setcar (aref handles idx) nil))))
      (setq idx (1+ idx)))
    nil))

(defun eshell-kill-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (kill-append string nil)))

(defun eshell-clipboard-append (string)
  "Call `kill-append' with STRING, if it is indeed a string."
  (if (stringp string)
      (let ((x-select-enable-clipboard t))
	(kill-append string nil))))

(defun eshell-get-target (target &optional mode)
  "Convert TARGET, which is a raw argument, into a valid output target.
MODE is either `overwrite', `append' or `insert'; if it is omitted or nil,
it defaults to `insert'."
  (setq mode (or mode 'insert))
  (cond
   ((stringp target)
    (let ((redir (assoc target eshell-virtual-targets)))
      (if redir
	  (if (nth 2 redir)
	      (funcall (nth 1 redir) mode)
	    (nth 1 redir))
	(let* ((exists (get-file-buffer target))
	       (buf (find-file-noselect target t)))
	  (with-current-buffer buf
	    (if buffer-file-read-only
		(error "Cannot write to read-only file `%s'" target))
	    (setq buffer-read-only nil)
	    (set (make-local-variable 'eshell-output-file-buffer)
		 (if (eq exists buf) 0 t))
	    (cond ((eq mode 'overwrite)
		   (erase-buffer))
		  ((eq mode 'append)
		   (goto-char (point-max))))
	    (point-marker))))))

   ((or (bufferp target)
	(and (boundp 'eshell-buffer-shorthand)
	     (symbol-value 'eshell-buffer-shorthand)
	     (symbolp target)
	     (not (memq target '(t nil)))))
    (let ((buf (if (bufferp target)
		   target
		 (get-buffer-create
		  (symbol-name target)))))
      (with-current-buffer buf
	(cond ((eq mode 'overwrite)
	       (erase-buffer))
	      ((eq mode 'append)
	       (goto-char (point-max))))
	(point-marker))))

   ((functionp target) nil)

   ((symbolp target)
    (if (eq mode 'overwrite)
	(set target nil))
    target)

   ((or (eshell-processp target)
	(markerp target))
    target)

   (t
    (error "Invalid redirection target: %s"
	   (eshell-stringify target)))))

(defvar grep-null-device)

(defun eshell-set-output-handle (index mode &optional target)
  "Set handle INDEX, using MODE, to point to TARGET."
  (when target
    (if (and (stringp target)
	     (or (cond
		  ((boundp 'null-device)
		   (string= target null-device))
		  ((boundp 'grep-null-device)
		   (string= target grep-null-device))
		  (t nil))
		 (string= target "/dev/null")))
	(aset eshell-current-handles index nil)
      (let ((where (eshell-get-target target mode))
	    (current (car (aref eshell-current-handles index))))
	(if (and (listp current)
		 (not (member where current)))
	    (setq current (append current (list where)))
	  (setq current (list where)))
	(if (not (aref eshell-current-handles index))
	    (aset eshell-current-handles index (cons nil 1)))
	(setcar (aref eshell-current-handles index) current)))))

(defun eshell-interactive-output-p ()
  "Return non-nil if current handles are bound for interactive display."
  (and (eq (car (aref eshell-current-handles
		      eshell-output-handle)) t)
       (eq (car (aref eshell-current-handles
		      eshell-error-handle)) t)))

(defvar eshell-print-queue nil)
(defvar eshell-print-queue-count -1)

(defsubst eshell-print (object)
  "Output OBJECT to the standard output handle."
  (eshell-output-object object eshell-output-handle))

(defun eshell-flush (&optional reset-p)
  "Flush out any lines that have been queued for printing.
Must be called before printing begins with -1 as its argument, and
after all printing is over with no argument."
  (ignore
   (if reset-p
       (setq eshell-print-queue nil
	     eshell-print-queue-count reset-p)
     (if eshell-print-queue
	 (eshell-print eshell-print-queue))
     (eshell-flush 0))))

(defun eshell-init-print-buffer ()
  "Initialize the buffered printing queue."
  (eshell-flush -1))

(defun eshell-buffered-print (&rest strings)
  "A buffered print -- *for strings only*."
  (if (< eshell-print-queue-count 0)
      (progn
	(eshell-print (apply 'concat strings))
	(setq eshell-print-queue-count 0))
    (if (= eshell-print-queue-count eshell-print-queue-size)
	(eshell-flush))
    (setq eshell-print-queue
	  (concat eshell-print-queue (apply 'concat strings))
	  eshell-print-queue-count (1+ eshell-print-queue-count))))

(defsubst eshell-error (object)
  "Output OBJECT to the standard error handle."
  (eshell-output-object object eshell-error-handle))

(defsubst eshell-errorn (object)
  "Output OBJECT followed by a newline to the standard error handle."
  (eshell-error object)
  (eshell-error "\n"))

(defsubst eshell-printn (object)
  "Output OBJECT followed by a newline to the standard output handle."
  (eshell-print object)
  (eshell-print "\n"))

(defun eshell-output-object-to-target (object target)
  "Insert OBJECT into TARGET.
Returns what was actually sent, or nil if nothing was sent."
  (cond
   ((functionp target)
    (funcall target object))

   ((symbolp target)
    (if (eq target t)                   ; means "print to display"
	(eshell-output-filter nil (eshell-stringify object))
      (if (not (symbol-value target))
	  (set target object)
	(setq object (eshell-stringify object))
	(if (not (stringp (symbol-value target)))
	    (set target (eshell-stringify
			 (symbol-value target))))
	(set target (concat (symbol-value target) object)))))

   ((markerp target)
    (if (buffer-live-p (marker-buffer target))
	(with-current-buffer (marker-buffer target)
	  (let ((moving (= (point) target)))
	    (save-excursion
	      (goto-char target)
	      (unless (stringp object)
		(setq object (eshell-stringify object)))
	      (insert-and-inherit object)
	      (set-marker target (point-marker)))
	    (if moving
		(goto-char target))))))

   ((eshell-processp target)
    (when (eq (process-status target) 'run)
      (unless (stringp object)
	(setq object (eshell-stringify object)))
      (process-send-string target object)))

   ((consp target)
    (apply (car target) object (cdr target))))
  object)

(defun eshell-output-object (object &optional handle-index handles)
  "Insert OBJECT, using HANDLE-INDEX specifically)."
  (let ((target (car (aref (or handles eshell-current-handles)
			   (or handle-index eshell-output-handle)))))
    (if (and target (not (listp target)))
	(eshell-output-object-to-target object target)
      (while target
	(eshell-output-object-to-target object (car target))
	(setq target (cdr target))))))

;;; esh-io.el ends here
