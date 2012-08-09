;;; mh-acros.el --- macros used in MH-E

;; Copyright (C) 2004, 2006-2012  Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;; This file contains all macros that are used in more than one file.
;; If you run "make recompile" in Bazaar Emacs and see the message
;; "Source is newer than compiled," it is a sign that macro probably
;; needs to be moved here.

;; Historically, it was so named with a silent "m" so that it would be
;; compiled first. Otherwise, "make recompile" in Bazaar Emacs would use
;; compiled files with stale macro definitions. Later, no-byte-compile
;; was added to the Local Variables section to avoid this problem and
;; because it's pointless to compile a file full of macros. But we
;; kept the name.

;;; Change Log:

;;; Code:

(require 'cl)



;;; Compatibility

;;;###mh-autoload
(defmacro mh-require-cl ()
  "Macro to load \"cl\" if needed.

Emacs coding conventions require that the \"cl\" package not be
required at runtime. However, the \"cl\" package in Emacs 21.4
and earlier left \"cl\" routines in their macro expansions. In
particular, the expansion of (setf (gethash ...) ...) used
functions in \"cl\" at run time. This macro recognizes that and
loads \"cl\" appropriately."
  (if (eq (car (macroexpand '(setf (gethash foo bar) baz))) 'cl-puthash)
      `(require 'cl)
    `(eval-when-compile (require 'cl))))

;;;###mh-autoload
(defmacro mh-do-in-gnu-emacs (&rest body)
  "Execute BODY if in GNU Emacs."
  (declare (debug t))
  (unless (featurep 'xemacs) `(progn ,@body)))
(put 'mh-do-in-gnu-emacs 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defmacro mh-do-in-xemacs (&rest body)
  "Execute BODY if in XEmacs."
  (declare (debug t))
  (when (featurep 'xemacs) `(progn ,@body)))
(put 'mh-do-in-xemacs 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defmacro mh-funcall-if-exists (function &rest args)
  "Call FUNCTION with ARGS as parameters if it exists."
  (when (fboundp function)
    `(when (fboundp ',function)
       (funcall ',function ,@args))))

;;;###mh-autoload
(defmacro defun-mh (name function arg-list &rest body)
  "Create function NAME.
If FUNCTION exists, then NAME becomes an alias for FUNCTION.
Otherwise, create function NAME with ARG-LIST and BODY."
  (let ((defined-p (fboundp function)))
    (if defined-p
        `(defalias ',name ',function)
      `(defun ,name ,arg-list ,@body))))
(put 'defun-mh 'lisp-indent-function 'defun)
(put 'defun-mh 'doc-string-elt 4)

;;;###mh-autoload
(defmacro defmacro-mh (name macro arg-list &rest body)
  "Create macro NAME.
If MACRO exists, then NAME becomes an alias for MACRO.
Otherwise, create macro NAME with ARG-LIST and BODY."
  (let ((defined-p (fboundp macro)))
    (if defined-p
        `(defalias ',name ',macro)
      `(defmacro ,name ,arg-list ,@body))))
(put 'defmacro-mh 'lisp-indent-function 'defun)
(put 'defmacro-mh 'doc-string-elt 4)



;;; Miscellaneous

;;;###mh-autoload
(defmacro mh-make-local-hook (hook)
  "Make HOOK local if needed.
XEmacs and versions of GNU Emacs before 21.1 require
`make-local-hook' to be called."
  (when (and (fboundp 'make-local-hook)
             (not (get 'make-local-hook 'byte-obsolete-info)))
    `(make-local-hook ,hook)))

;;;###mh-autoload
(defmacro mh-mark-active-p (check-transient-mark-mode-flag)
  "A macro that expands into appropriate code in XEmacs and nil in GNU Emacs.
In GNU Emacs if CHECK-TRANSIENT-MARK-MODE-FLAG is non-nil then
check if variable `transient-mark-mode' is active."
  (cond ((featurep 'xemacs)             ;XEmacs
         `(and (boundp 'zmacs-regions) zmacs-regions (region-active-p)))
        ((not check-transient-mark-mode-flag) ;GNU Emacs
         `(and (boundp 'mark-active) mark-active))
        (t                              ;GNU Emacs
         `(and (boundp 'transient-mark-mode) transient-mark-mode
               (boundp 'mark-active) mark-active))))

;; Shush compiler.
(mh-do-in-xemacs
  (defvar struct)
  (defvar x)
  (defvar y))

;;;###mh-autoload
(defmacro mh-defstruct (name-spec &rest fields)
  "Replacement for `defstruct' from the \"cl\" package.
The `defstruct' in the \"cl\" library produces compiler warnings,
and generates code that uses functions present in \"cl\" at
run-time. This is a partial replacement, that avoids these
issues.

NAME-SPEC declares the name of the structure, while FIELDS
describes the various structure fields. Lookup `defstruct' for
more details."
  (let* ((struct-name (if (atom name-spec) name-spec (car name-spec)))
         (conc-name (or (and (consp name-spec)
                             (cadr (assoc :conc-name (cdr name-spec))))
                        (format "%s-" struct-name)))
         (predicate (intern (format "%s-p" struct-name)))
         (constructor (or (and (consp name-spec)
                               (cadr (assoc :constructor (cdr name-spec))))
                          (intern (format "make-%s" struct-name))))
         (field-names (mapcar #'(lambda (x) (if (atom x) x (car x))) fields))
         (field-init-forms (mapcar #'(lambda (x) (and (consp x) (cadr x)))
                                   fields))
         (struct (gensym "S"))
         (x (gensym "X"))
         (y (gensym "Y")))
    `(progn
       (defun* ,constructor (&key ,@(mapcar* #'(lambda (x y) (list x y))
                                             field-names field-init-forms))
         (list (quote ,struct-name) ,@field-names))
       (defun ,predicate (arg)
         (and (consp arg) (eq (car arg) (quote ,struct-name))))
       ,@(loop for x from 1
               for y in field-names
               collect `(defmacro ,(intern (format "%s%s" conc-name y)) (z)
                          (list 'nth ,x z)))
       (quote ,struct-name))))

;;;###mh-autoload
(defmacro with-mh-folder-updating (save-modification-flag &rest body)
  "Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG) &body BODY).
Execute BODY, which can modify the folder buffer without having to
worry about file locking or the read-only flag, and return its result.
If SAVE-MODIFICATION-FLAG is non-nil, the buffer's modification flag
is unchanged, otherwise it is cleared."
  (declare (debug t))
  (setq save-modification-flag (car save-modification-flag)) ; CL style
  `(prog1
       (let ((mh-folder-updating-mod-flag (buffer-modified-p))
             (buffer-read-only nil)
             (buffer-file-name nil))    ;don't let the buffer get locked
         (prog1
             (progn
               ,@body)
           (mh-set-folder-modified-p mh-folder-updating-mod-flag)))
     ,@(if (not save-modification-flag)
           '((mh-set-folder-modified-p nil)))))
(put 'with-mh-folder-updating 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defmacro mh-in-show-buffer (show-buffer &rest body)
  "Format is (mh-in-show-buffer (SHOW-BUFFER) &body BODY).
Display buffer SHOW-BUFFER in other window and execute BODY in it.
Stronger than `save-excursion', weaker than `save-window-excursion'."
  (declare (debug t))
  (setq show-buffer (car show-buffer))  ; CL style
  `(let ((mh-in-show-buffer-saved-window (selected-window)))
     (switch-to-buffer-other-window ,show-buffer)
     (if mh-bury-show-buffer-flag (bury-buffer (current-buffer)))
     (unwind-protect
         (progn
           ,@body)
       (select-window mh-in-show-buffer-saved-window))))
(put 'mh-in-show-buffer 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defmacro mh-do-at-event-location (event &rest body)
  "Switch to the location of EVENT and execute BODY.
After BODY has been executed return to original window. The
modification flag of the buffer in the event window is
preserved."
  (declare (debug t))
  (let ((event-window (make-symbol "event-window"))
        (event-position (make-symbol "event-position"))
        (original-window (make-symbol "original-window"))
        (original-position (make-symbol "original-position"))
        (modified-flag (make-symbol "modified-flag")))
    `(save-excursion
       (let* ((,event-window
               (or (mh-funcall-if-exists posn-window (event-start ,event))
                   (mh-funcall-if-exists event-window ,event)))
              (,event-position
               (or (mh-funcall-if-exists posn-point (event-start ,event))
                   (mh-funcall-if-exists event-closest-point ,event)))
              (,original-window (selected-window))
              (,original-position (progn
                                   (set-buffer (window-buffer ,event-window))
                                   (set-marker (make-marker) (point))))
              (,modified-flag (buffer-modified-p))
              (buffer-read-only nil))
         (unwind-protect (progn
                           (select-window ,event-window)
                           (goto-char ,event-position)
                           ,@body)
           (set-buffer-modified-p ,modified-flag)
           (goto-char ,original-position)
           (set-marker ,original-position nil)
           (select-window ,original-window))))))
(put 'mh-do-at-event-location 'lisp-indent-hook 'defun)



;;; Sequences and Ranges

;;;###mh-autoload
(defsubst mh-seq-msgs (sequence)
  "Extract messages from the given SEQUENCE."
  (cdr sequence))

;;;###mh-autoload
(defmacro mh-iterate-on-messages-in-region (var begin end &rest body)
  "Iterate over region.

VAR is bound to the message on the current line as we loop
starting from BEGIN till END. In each step BODY is executed.

If VAR is nil then the loop is executed without any binding."
  (declare (debug (symbolp body)))
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var))
    `(save-excursion
       (goto-char ,begin)
       (beginning-of-line)
       (while (and (<= (point) ,end) (not (eobp)))
         (when (looking-at mh-scan-valid-regexp)
           (let ,(if binding-needed-flag `((,var (mh-get-msg-num t))) ())
             ,@body))
         (forward-line 1)))))
(put 'mh-iterate-on-messages-in-region 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defmacro mh-iterate-on-range (var range &rest body)
  "Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over RANGE, which
can be a message number, a list of message numbers, a sequence, a
region in a cons cell, or a MH range (something like last:20) in
a string. In each iteration, BODY is executed.

The parameter RANGE is usually created with
`mh-interactive-range' in order to provide a uniform interface to
MH-E functions."
  (declare (debug (symbolp body)))
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var)
        (msgs (make-symbol "msgs"))
        (seq-hash-table (make-symbol "seq-hash-table")))
    `(cond ((numberp ,range)
            (when (mh-goto-msg ,range t t)
              (let ,(if binding-needed-flag `((,var ,range)) ())
                ,@body)))
           ((and (consp ,range)
                 (numberp (car ,range)) (numberp (cdr ,range)))
            (mh-iterate-on-messages-in-region ,var
              (car ,range) (cdr ,range)
              ,@body))
           (t (let ((,msgs (cond ((and ,range (symbolp ,range))
                                  (mh-seq-to-msgs ,range))
                                 ((stringp ,range)
                                  (mh-translate-range mh-current-folder
                                                      ,range))
                                 (t ,range)))
                    (,seq-hash-table (make-hash-table)))
                (dolist (msg ,msgs)
                  (setf (gethash msg ,seq-hash-table) t))
                (mh-iterate-on-messages-in-region v (point-min) (point-max)
                  (when (gethash v ,seq-hash-table)
                    (let ,(if binding-needed-flag `((,var v)) ())
                      ,@body))))))))
(put 'mh-iterate-on-range 'lisp-indent-hook 'defun)

(provide 'mh-acros)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-acros.el ends here
