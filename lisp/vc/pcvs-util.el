;;; pcvs-util.el --- utility functions for PCL-CVS  -*- byte-compile-dynamic: t -*-

;; Copyright (C) 1991-2012 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs
;; Package: pcvs

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

(eval-when-compile (require 'cl))

;;;;
;;;; list processing
;;;;

(defsubst cvs-car (x) (if (consp x) (car x) x))
(defalias 'cvs-cdr 'cdr-safe)
(defsubst cvs-append (&rest xs)
  (apply 'append (mapcar (lambda (x) (if (listp x) x (list x))) xs)))

(defsubst cvs-every (-cvs-every-f -cvs-every-l)
  (while (consp -cvs-every-l)
    (unless (funcall -cvs-every-f (pop -cvs-every-l))
      (setq -cvs-every-l t)))
  (not -cvs-every-l))

(defun cvs-union (xs ys)
  (let ((zs ys))
    (dolist (x xs zs)
      (unless (member x ys) (push x zs)))))

(defun cvs-map (-cvs-map-f &rest -cvs-map-ls)
  (let ((accum ()))
    (while (not (cvs-every 'null -cvs-map-ls))
      (push (apply -cvs-map-f (mapcar 'car -cvs-map-ls)) accum)
      (setq -cvs-map-ls (mapcar 'cdr -cvs-map-ls)))
    (nreverse accum)))

(defun cvs-first (l &optional n)
  (if (null n) (car l)
    (when l
      (let* ((nl (list (pop l)))
	     (ret nl))
	(while (and l (> n 1))
	  (setcdr nl (list (pop l)))
	  (setq nl (cdr nl))
	  (decf n))
	ret))))

(defun cvs-partition (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))

;;;
;;; frame, window, buffer handling
;;;

(defun cvs-pop-to-buffer-same-frame (buf)
  "Pop to BUF like `pop-to-buffer' but staying on the same frame.
If `pop-to-buffer' would have opened a new frame, this function would
try to split a new window instead."
  (let ((pop-up-windows (or pop-up-windows pop-up-frames))
	(pop-up-frames nil))
    (or (let ((buf (get-buffer-window buf))) (and buf (select-window buf)))
	(and pop-up-windows
	     (ignore-errors (select-window (split-window-below)))
	     (switch-to-buffer buf nil 'force-same-window))
	(pop-to-buffer (current-buffer)))))

(defun cvs-bury-buffer (buf &optional mainbuf)
  "Hide the buffer BUF that was temporarily popped up.
BUF is assumed to be a temporary buffer used from the buffer MAINBUF."
  (interactive (list (current-buffer)))
  (save-current-buffer
    (let ((win (if (eq buf (window-buffer (selected-window))) (selected-window)
		 (get-buffer-window buf t))))
      (when win
	(if (window-dedicated-p win)
	    (condition-case ()
		(delete-window win)
	      (error (iconify-frame (window-frame win))))
;;; 	  (if (and mainbuf (get-buffer-window mainbuf))
;;; 	      ;; FIXME: if the buffer popped into a pre-existing window,
;;; 	      ;; we don't want to delete that window.
;;; 	      t ;;(delete-window win)
;;; 	      )
	  )))
    (with-current-buffer buf
      (bury-buffer (unless (and (eq buf (window-buffer (selected-window)))
				(not (window-dedicated-p (selected-window))))
		     buf)))
    (when mainbuf
      (let ((mainwin (or (get-buffer-window mainbuf)
			 (get-buffer-window mainbuf 'visible))))
	(when mainwin (select-window mainwin))))))

(defun cvs-get-buffer-create (name &optional noreuse)
  "Create a buffer NAME unless such a buffer already exists.
If the NAME looks like an absolute file name, the buffer will be created
with `create-file-buffer' and will probably get another name than NAME.
In such a case, the search for another buffer with the same name doesn't
use the buffer name but the buffer's `list-buffers-directory' variable.
If NOREUSE is non-nil, always return a new buffer."
  (or (and (not (file-name-absolute-p name))
           (if noreuse (generate-new-buffer name)
             (get-buffer-create name)))
      (unless noreuse
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
	    (when (equal name list-buffers-directory)
	      (return buf)))))
      (with-current-buffer (create-file-buffer name)
	(setq list-buffers-directory name)
	(current-buffer))))

;;;;
;;;; string processing
;;;;

(defun cvs-insert-strings (strings)
  "Insert a list of STRINGS into the current buffer.
Uses columns to keep the listing readable but compact."
  (when (consp strings)
    (let* ((length (apply 'max (mapcar 'length strings)))
	   (wwidth (1- (window-width)))
	   (columns (min
		     ;; At least 2 columns; at least 2 spaces between columns.
		     (max 2 (/ wwidth (+ 2 length)))
		     ;; Don't allocate more columns than we can fill.
		     ;; Windows can't show less than 3 lines anyway.
		     (max 1 (/ (length strings) 2))))
	   (colwidth (/ wwidth columns)))
      ;; Use tab-width rather than indent-to.
      (setq tab-width colwidth)
      ;; The insertion should be "sensible" no matter what choices were made.
      (dolist (str strings)
	(unless (bolp)
          (insert " \t")
          (when (< wwidth (+ (max colwidth (length str)) (current-column)))
            (delete-char -2) (insert "\n")))
        (insert str)))))


(defun cvs-file-to-string (file &optional oneline args)
  "Read the content of FILE and return it as a string.
If ONELINE is t, only the first line (no \\n) will be returned.
If ARGS is non-nil, the file will be executed with ARGS as its
arguments.  If ARGS is not a list, no argument will be passed."
  (condition-case nil
      (with-temp-buffer
	(if args
	    (apply 'call-process
		   file nil t nil (when (listp args) args))
	  (insert-file-contents file))
	(goto-char (point-min))
	(buffer-substring (point)
			  (if oneline (line-end-position) (point-max))))
    (file-error nil)))

(defun cvs-string-prefix-p (str1 str2)
  "Tell whether STR1 is a prefix of STR2."
  (eq t (compare-strings str2 nil (length str1) str1 nil nil)))

;;;;
;;;; file names
;;;;

(defsubst cvs-expand-dir-name (d)
  (file-name-as-directory (expand-file-name d)))

;;;;
;;;; (interactive <foo>) support function
;;;;

(defstruct (cvs-qtypedesc
	    (:constructor nil) (:copier nil)
	    (:constructor cvs-qtypedesc-create
			  (str2obj obj2str &optional complete hist-sym require)))
  str2obj
  obj2str
  hist-sym
  complete
  require)


(defconst cvs-qtypedesc-string1 (cvs-qtypedesc-create 'identity 'identity t))
(defconst cvs-qtypedesc-string (cvs-qtypedesc-create 'identity 'identity))
(defconst cvs-qtypedesc-strings
  (cvs-qtypedesc-create 'split-string-and-unquote
			'combine-and-quote-strings nil))

(defun cvs-query-read (default prompt qtypedesc &optional hist-sym)
  (let* ((qtypedesc (or qtypedesc cvs-qtypedesc-strings))
	 (hist-sym (or hist-sym (cvs-qtypedesc-hist-sym qtypedesc)))
	 (complete (cvs-qtypedesc-complete qtypedesc))
	 (completions (and (functionp complete) (funcall complete)))
	 (initval (funcall (cvs-qtypedesc-obj2str qtypedesc) default)))
    (funcall (cvs-qtypedesc-str2obj qtypedesc)
	     (cond
	      ((null complete) (read-string prompt initval hist-sym))
	      ((functionp complete)
	       (completing-read prompt completions
				nil (cvs-qtypedesc-require qtypedesc)
				initval hist-sym))
	      (t initval)))))

;;;;
;;;; Flags handling
;;;;

(defstruct (cvs-flags
	    (:constructor nil)
	    (:constructor -cvs-flags-make
			  (desc defaults &optional qtypedesc hist-sym)))
  defaults persist desc qtypedesc hist-sym)

(defmacro cvs-flags-define (sym defaults
				&optional desc qtypedesc hist-sym docstring)
  `(defconst ,sym
     (let ((bound (boundp ',sym)))
       (if (and bound (cvs-flags-p ,sym)) ,sym
	 (let ((defaults ,defaults))
	   (-cvs-flags-make ,desc
			    (if bound (cons ,sym (cdr defaults)) defaults)
			    ,qtypedesc ,hist-sym))))
     ,docstring))

(defun cvs-flags-query (sym &optional desc arg)
  "Query flags based on SYM.
Optional argument DESC will be used for the prompt.
If ARG (or a prefix argument) is nil, just use the 0th default.
If it is a non-negative integer, use the corresponding default.
If it is a negative integer query for a new value of the corresponding
  default and return that new value.
If it is \\[universal-argument], just query and return a value without
  altering the defaults.
If it is \\[universal-argument] \\[universal-argument], behave just
  as if a negative zero was provided."
  (let* ((flags (symbol-value sym))
	 (desc (or desc (cvs-flags-desc flags)))
	 (qtypedesc (cvs-flags-qtypedesc flags))
	 (hist-sym (cvs-flags-hist-sym flags))
	 (arg (if (eq arg 'noquery) 0 (or arg current-prefix-arg 0)))
	 (numarg (prefix-numeric-value arg))
	 (defaults (cvs-flags-defaults flags))
	 (permstr (if (< numarg 0) (format " (%sth default)" (- numarg)))))
    ;; special case for universal-argument
    (when (consp arg)
      (setq permstr (if (> numarg 4) " (permanent)" ""))
      (setq numarg 0))

    ;; sanity check
    (unless (< (abs numarg) (length defaults))
      (error "There is no %sth default" (abs numarg)))

    (if permstr
	(let* ((prompt (format "%s%s: " desc permstr))
	       (fs (cvs-query-read (nth (- numarg) (cvs-flags-defaults flags))
				   prompt qtypedesc hist-sym)))
	  (when (not (equal permstr ""))
	    (setf (nth (- numarg) (cvs-flags-defaults flags)) fs))
	  fs)
      (nth numarg defaults))))

(defsubst cvs-flags-set (sym index value)
  "Set SYM's INDEX'th setting to VALUE."
  (setf (nth index (cvs-flags-defaults (symbol-value sym))) value))

;;;;
;;;; Prefix keys
;;;;

(defconst cvs-prefix-number 10)

(defsubst cvs-prefix-sym (sym) (intern (concat (symbol-name sym) "-cps")))

(defmacro cvs-prefix-define (sym docstring desc defaults
				 &optional qtypedesc hist-sym)
  (let ((cps (cvs-prefix-sym sym)))
    `(progn
       (defvar ,sym nil ,(concat (or docstring "") "
See `cvs-prefix-set' for further description of the behavior."))
       (defvar ,cps
	 (let ((defaults ,defaults))
	   ;; sanity insurance
	   (unless (>= (length defaults) cvs-prefix-number)
	     (setq defaults (append defaults
				    (make-list (1- cvs-prefix-number)
					       (nth 0 defaults)))))
	   (-cvs-flags-make ,desc defaults ,qtypedesc ,hist-sym))))))

(defun cvs-prefix-make-local (sym)
  (let ((cps (cvs-prefix-sym sym)))
    (make-local-variable sym)
    (set (make-local-variable cps) (copy-cvs-flags (symbol-value cps)))))

(defun cvs-prefix-set (sym arg)
  ;; we could distinguish between numeric and non-numeric prefix args instead of
  ;; relying on that magic `4'.
  "Set the cvs-prefix contained in SYM.
If ARG is between 0 and 9, it selects the corresponding default.
If ARG is negative (or \\[universal-argument] which corresponds to negative 0),
  it queries the user and sets the -ARG'th default.
If ARG is greater than 9 (or \\[universal-argument] \\[universal-argument]),
  the (ARG mod 10)'th prefix is made persistent.
If ARG is nil toggle the PREFIX's value between its 0th default and nil
  and reset the persistence."
  (let* ((prefix (symbol-value (cvs-prefix-sym sym)))
	 (numarg (if (integerp arg) arg 0))
	 ;; (defs (cvs-flags-defaults prefix))
         )

    ;; set persistence if requested
    (when (> (prefix-numeric-value arg) 9)
      (setf (cvs-flags-persist prefix) t)
      (setq numarg (mod numarg 10)))

    ;; set the value
    (set sym
	 (cond
	  ((null arg)
	   (setf (cvs-flags-persist prefix) nil)
	   (unless (symbol-value sym) (nth 0 (cvs-flags-defaults prefix))))

	  ((or (consp arg) (< numarg 0))
	   (setf (nth (- numarg) (cvs-flags-defaults prefix))
		 (cvs-query-read (nth (- numarg) (cvs-flags-defaults prefix))
				 (format "%s: " (cvs-flags-desc prefix))
				 (cvs-flags-qtypedesc prefix)
				 (cvs-flags-hist-sym prefix))))
	  (t (nth numarg (cvs-flags-defaults prefix)))))
    (force-mode-line-update)))

(defun cvs-prefix-get (sym &optional read-only)
  "Return the current value of the prefix SYM.
And reset it unless READ-ONLY is non-nil."
  (prog1 (symbol-value sym)
    (unless (or read-only
		(cvs-flags-persist (symbol-value (cvs-prefix-sym sym))))
      (set sym nil)
      (force-mode-line-update))))

(provide 'pcvs-util)

;;; pcvs-util.el ends here
