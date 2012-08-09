;;; ert-x.el --- Staging area for experimental extensions to ERT

;; Copyright (C) 2008, 2010-2012 Free Software Foundation, Inc.

;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Author: Christian Ohler <ohler@gnu.org>

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file includes some extra helper functions to use while writing
;; automated tests with ERT.  These have been proposed as extensions
;; to ERT but are not mature yet and likely to change.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ert)


;;; Test buffers.

(defun ert--text-button (string &rest properties)
  "Return a string containing STRING as a text button with PROPERTIES.

See `make-text-button'."
  (with-temp-buffer
    (insert string)
    (apply #'make-text-button (point-min) (point-max) properties)
    (buffer-string)))

(defun ert--format-test-buffer-name (base-name)
  "Compute a test buffer name based on BASE-NAME.

Helper function for `ert--test-buffers'."
  (format "*Test buffer (%s)%s*"
	  (or (and (ert-running-test)
		   (ert-test-name (ert-running-test)))
	      "<anonymous test>")
	  (if base-name
	      (format ": %s" base-name)
	    "")))

(defvar ert--test-buffers (make-hash-table :weakness t)
  "Table of all test buffers.  Keys are the buffer objects, values are t.

The main use of this table is for `ert-kill-all-test-buffers'.
Not all buffers in this table are necessarily live, but all live
test buffers are in this table.")

(define-button-type 'ert--test-buffer-button
  'action #'ert--test-buffer-button-action
  'help-echo "mouse-2, RET: Pop to test buffer")

(defun ert--test-buffer-button-action (button)
  "Pop to the test buffer that BUTTON is associated with."
  (pop-to-buffer (button-get button 'ert--test-buffer)))

(defun ert--call-with-test-buffer (ert--base-name ert--thunk)
  "Helper function for `ert-with-test-buffer'.

Create a test buffer with a name based on ERT--BASE-NAME and run
ERT--THUNK with that buffer as current."
  (let* ((ert--buffer (generate-new-buffer
                       (ert--format-test-buffer-name ert--base-name)))
         (ert--button (ert--text-button (buffer-name ert--buffer)
                                        :type 'ert--test-buffer-button
                                        'ert--test-buffer ert--buffer)))
    (puthash ert--buffer 't ert--test-buffers)
    ;; We don't use `unwind-protect' here since we want to kill the
    ;; buffer only on success.
    (prog1 (with-current-buffer ert--buffer
             (ert-info (ert--button :prefix "Buffer: ")
               (funcall ert--thunk)))
      (kill-buffer ert--buffer)
      (remhash ert--buffer ert--test-buffers))))

(defmacro* ert-with-test-buffer ((&key ((:name name-form)))
				 &body body)
  "Create a test buffer and run BODY in that buffer.

To be used in ERT tests.  If BODY finishes successfully, the test
buffer is killed; if there is an error, the test buffer is kept
around on error for further inspection.  Its name is derived from
the name of the test and the result of NAME-FORM."
  (declare (debug ((form) body))
           (indent 1))
  `(ert--call-with-test-buffer ,name-form (lambda () ,@body)))

;; We use these `put' forms in addition to the (declare (indent)) in
;; the defmacro form since the `declare' alone does not lead to
;; correct indentation before the .el/.elc file is loaded.
;; Autoloading these `put' forms solves this.
;;;###autoload
(progn
  ;; TODO(ohler): Figure out what these mean and make sure they are correct.
  (put 'ert-with-test-buffer 'lisp-indent-function 1))

;;;###autoload
(defun ert-kill-all-test-buffers ()
  "Kill all test buffers that are still live."
  (interactive)
  (let ((count 0))
    (maphash (lambda (buffer dummy)
	       (when (or (not (buffer-live-p buffer))
			 (kill-buffer buffer))
		 (incf count)))
	     ert--test-buffers)
    (message "%s out of %s test buffers killed"
	     count (hash-table-count ert--test-buffers)))
  ;; It could be that some test buffers were actually kept alive
  ;; (e.g., due to `kill-buffer-query-functions').  I'm not sure what
  ;; to do about this.  For now, let's just forget them.
  (clrhash ert--test-buffers)
  nil)


;;; Simulate commands.

(defun ert-simulate-command (command)
  ;; FIXME: add unread-events
  "Simulate calling COMMAND the way the Emacs command loop would call it.

This effectively executes

  \(apply (car COMMAND) (cdr COMMAND)\)

and returns the same value, but additionally runs hooks like
`pre-command-hook' and `post-command-hook', and sets variables
like `this-command' and `last-command'.

COMMAND should be a list where the car is the command symbol and
the rest are arguments to the command.

NOTE: Since the command is not called by `call-interactively'
test for `called-interactively' in the command will fail."
  (assert (listp command) t)
  (assert (commandp (car command)) t)
  (assert (not unread-command-events) t)
  (let (return-value)
    ;; For the order of things here see command_loop_1 in keyboard.c.
    ;;
    ;; The command loop will reset the command-related variables so
    ;; there is no reason to let-bind them. They are set here,
    ;; however, to be able to test several commands in a row and how
    ;; they affect each other.
    (setq deactivate-mark nil
          this-original-command (car command)
          ;; remap through active keymaps
          this-command (or (command-remapping this-original-command)
                           this-original-command))
    (run-hooks 'pre-command-hook)
    (setq return-value (apply (car command) (cdr command)))
    (run-hooks 'post-command-hook)
    (and (boundp 'deferred-action-list)
         deferred-action-list
         (run-hooks 'deferred-action-function))
    (setq real-last-command (car command)
          last-command this-command)
    (when (boundp 'last-repeatable-command)
      (setq last-repeatable-command real-last-command))
    (when (and deactivate-mark transient-mark-mode) (deactivate-mark))
    (assert (not unread-command-events) t)
    return-value))

(defun ert-run-idle-timers ()
  "Run all idle timers (from `timer-idle-list')."
  (dolist (timer (copy-sequence timer-idle-list))
    (timer-event-handler timer)))


;;; Miscellaneous utilities.

(defun ert-filter-string (s &rest regexps)
  "Return a copy of S with all matches of REGEXPS removed.

Elements of REGEXPS may also be two-element lists \(REGEXP
SUBEXP\), where SUBEXP is the number of a subexpression in
REGEXP.  In that case, only that subexpression will be removed
rather than the entire match."
  ;; Use a temporary buffer since replace-match copies strings, which
  ;; would lead to N^2 runtime.
  (with-temp-buffer
    (insert s)
    (dolist (x regexps)
      (destructuring-bind (regexp subexp) (if (listp x) x `(,x nil))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match "" t t nil subexp))))
    (buffer-string)))


(defun ert-propertized-string (&rest args)
  "Return a string with properties as specified by ARGS.

ARGS is a list of strings and plists.  The strings in ARGS are
concatenated to produce an output string.  In the output string,
each string from ARGS will be have the preceding plist as its
property list, or no properties if there is no plist before it.

As a simple example,

\(ert-propertized-string \"foo \" '(face italic) \"bar\" \" baz\" nil \
\" quux\"\)

would return the string \"foo bar baz quux\" where the substring
\"bar baz\" has a `face' property with the value `italic'.

None of the ARGS are modified, but the return value may share
structure with the plists in ARGS."
  (with-temp-buffer
    (loop with current-plist = nil
          for x in args do
          (etypecase x
            (string (let ((begin (point)))
                      (insert x)
                      (set-text-properties begin (point) current-plist)))
            (list (unless (zerop (mod (length x) 2))
                    (error "Odd number of args in plist: %S" x))
                  (setq current-plist x))))
    (buffer-string)))


(defun ert-call-with-buffer-renamed (buffer-name thunk)
  "Protect the buffer named BUFFER-NAME from side-effects and run THUNK.

Renames the buffer BUFFER-NAME to a new temporary name, creates a
new buffer named BUFFER-NAME, executes THUNK, kills the new
buffer, and renames the original buffer back to BUFFER-NAME.

This is useful if THUNK has undesirable side-effects on an Emacs
buffer with a fixed name such as *Messages*."
  (lexical-let ((new-buffer-name (generate-new-buffer-name
                                  (format "%s orig buffer" buffer-name))))
    (with-current-buffer (get-buffer-create buffer-name)
      (rename-buffer new-buffer-name))
    (unwind-protect
        (progn
          (get-buffer-create buffer-name)
          (funcall thunk))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (with-current-buffer new-buffer-name
        (rename-buffer buffer-name)))))

(defmacro* ert-with-buffer-renamed ((buffer-name-form) &body body)
  "Protect the buffer named BUFFER-NAME from side-effects and run BODY.

See `ert-call-with-buffer-renamed' for details."
  (declare (indent 1))
  `(ert-call-with-buffer-renamed ,buffer-name-form (lambda () ,@body)))


(defun ert-buffer-string-reindented (&optional buffer)
  "Return the contents of BUFFER after reindentation.

BUFFER defaults to current buffer.  Does not modify BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((clone nil))
      (unwind-protect
          (progn
            ;; `clone-buffer' doesn't work if `buffer-file-name' is non-nil.
            (let ((buffer-file-name nil))
              (setq clone (clone-buffer)))
            (with-current-buffer clone
              (let ((inhibit-read-only t))
                (indent-region (point-min) (point-max)))
              (buffer-string)))
        (when clone
          (let ((kill-buffer-query-functions nil))
            (kill-buffer clone)))))))


(provide 'ert-x)

;;; ert-x.el ends here
