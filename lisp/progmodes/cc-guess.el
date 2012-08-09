;;; cc-guess.el --- guess indentation values by scanning existing code

;; Copyright (C) 1985, 1987, 1992-2006, 2011-2012
;;   Free Software Foundation, Inc.

;; Author:     1994-1995 Barry A. Warsaw
;;             2011-     Masatake YAMATO
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    August 1994, split from cc-mode.el
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

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
;;
;; This file contains routines that help guess the cc-mode style in a
;; particular region/buffer.  Here style means `c-offsets-alist' and
;; `c-basic-offset'.
;;
;; The main entry point of this program is `c-guess' command but there
;; are some variants.
;;
;; Suppose the major mode for the current buffer is one of the modes
;; provided by cc-mode. `c-guess' guesses the indentation style by
;; examining the indentation in the region between beginning of buffer
;; and `c-guess-region-max'.

;; and installs the guessed style. The name for installed style is given
;; by `c-guess-style-name'.
;;
;; `c-guess-buffer' does the same but in the whole buffer.
;; `c-guess-region' does the same but in the region between the point
;; and the mark.  `c-guess-no-install', `c-guess-buffer-no-install'
;; and `c-guess-region-no-install' guess the indentation style but
;; don't install it. You can review a guessed style with `c-guess-view'.
;; After reviewing, use `c-guess-install' to install the style
;; if you prefer it.
;;
;; If you want to reuse the guessed style in another buffer,
;; run `c-set-style' command with the name of the guessed style:
;; "*c-guess*:<name-of-file-which-examined-when-guessing>".
;; Once the guessed style is installed explicitly with `c-guess-install'
;; or implicitly with `c-guess', `c-guess-buffer', or `c-guess-region',
;; a style name is given by `c-guess-style-name' with the above form.
;;
;; If you want to reuse the guessed style in future emacs sessions,
;; you may want to put it to your .emacs. `c-guess-view' is for
;; you. It emits emacs lisp code which defines the last guessed
;; style, in a temporary buffer. You can put the emitted code into
;; your .emacs. This command was suggested by Alan Mackenzie.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-engine)
(cc-require 'cc-styles)



(defcustom c-guess-offset-threshold 10
  "Threshold of acceptable offsets when examining indent information.
Discard an examined offset if its absolute value is greater than this.

The offset of a line included in the indent information returned by
`c-guess-basic-syntax'."
  :version "24.1"
  :type 'integer
  :group 'c)

(defcustom c-guess-region-max 50000
  "The maximum region size for examining indent information with `c-guess'.
It takes a long time to examine indent information from a large region;
this option helps you limit that time. `nil' means no limit."
  :version "24.1"
  :type 'integer
  :group 'c)


;;;###autoload
(defvar c-guess-guessed-offsets-alist nil
  "Currently guessed offsets-alist.")
;;;###autoload
(defvar c-guess-guessed-basic-offset nil
  "Currently guessed basic-offset.")

(defvar c-guess-accumulator nil)
;; Accumulated examined indent information.  Information is represented
;; in a list.  Each element in it has following structure:
;;
;; (syntactic-symbol ((indentation-offset1 . number-of-times1)
;;		      (indentation-offset2 . number-of-times2)
;;		      ...))
;;
;; This structure is built by `c-guess-accumulate-offset'.
;;
;; Here we call the pair (indentation-offset1 . number-of-times1) a
;; counter.  `c-guess-sort-accumulator' sorts the order of
;; counters by number-of-times.
;; Use `c-guess-dump-accumulator' to see the value.

(defconst c-guess-conversions
  '((c . c-lineup-C-comments)
    (inher-cont . c-lineup-multi-inher)
    (string . -1000)
    (comment-intro . c-lineup-comment)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-close . c-lineup-close-paren)
    (cpp-macro . -1000)))


;;;###autoload
(defun c-guess (&optional accumulate)
  "Guess the style in the region up to `c-guess-region-max', and install it.

The style is given a name based on the file's absolute file name.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch."
  (interactive "P")
  (c-guess-region (point-min)
		   (min (point-max) (or c-guess-region-max
					(point-max)))
		   accumulate))

;;;###autoload
(defun c-guess-no-install (&optional accumulate)
  "Guess the style in the region up to `c-guess-region-max'; don't install it.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch."
  (interactive "P")
  (c-guess-region-no-install (point-min)
			      (min (point-max) (or c-guess-region-max
						   (point-max)))
			      accumulate))

;;;###autoload
(defun c-guess-buffer (&optional accumulate)
  "Guess the style on the whole current buffer, and install it.

The style is given a name based on the file's absolute file name.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch."
  (interactive "P")
  (c-guess-region (point-min)
		   (point-max)
		   accumulate))

;;;###autoload
(defun c-guess-buffer-no-install (&optional accumulate)
  "Guess the style on the whole current buffer; don't install it.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch."
  (interactive "P")
  (c-guess-region-no-install (point-min)
			      (point-max)
			      accumulate))

;;;###autoload
(defun c-guess-region (start end &optional accumulate)
  "Guess the style on the region and install it.

The style is given a name based on the file's absolute file name.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch."
  (interactive "r\nP")
  (c-guess-region-no-install start end accumulate)
  (c-guess-install))


(defsubst c-guess-empty-line-p ()
  (eq (line-beginning-position)
      (line-end-position)))

;;;###autoload
(defun c-guess-region-no-install (start end &optional accumulate)
  "Guess the style on the region; don't install it.

Every line of code in the region is examined and values for the following two
variables are guessed:

* `c-basic-offset', and
* the indentation values of the various syntactic symbols in
  `c-offsets-alist'.

The guessed values are put into `c-guess-guessed-basic-offset' and
`c-guess-guessed-offsets-alist'.

Frequencies of use are taken into account when guessing, so minor
inconsistencies in the indentation style shouldn't produce wrong guesses.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous examination is extended, otherwise a new
guess is made from scratch.

Note that the larger the region to guess in, the slower the guessing.
So you can limit the region with `c-guess-region-max'."
  (interactive "r\nP")
  (let ((accumulator (when accumulate c-guess-accumulator)))
    (setq c-guess-accumulator (c-guess-examine start end accumulator))
    (let ((pair (c-guess-guess c-guess-accumulator)))
      (setq c-guess-guessed-basic-offset (car pair)
	    c-guess-guessed-offsets-alist (cdr pair)))))


(defun c-guess-examine (start end accumulator)
  (let ((reporter (when (fboundp 'make-progress-reporter)
		    (make-progress-reporter "Examining Indentation "
					    start
					    end))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(unless (c-guess-empty-line-p)
	  (mapc (lambda (s)
		  (setq accumulator (or (c-guess-accumulate accumulator s)
					accumulator)))
		(c-save-buffer-state () (c-guess-basic-syntax))))
	(when reporter (progress-reporter-update reporter (point)))
	(forward-line 1)))
    (when reporter (progress-reporter-done reporter)))
  (c-guess-sort-accumulator accumulator))

(defun c-guess-guess (accumulator)
  ;; Guess basic-offset and offsets-alist from ACCUMULATOR,
  ;; then return them as a cons: (basic-offset . offsets-alist).
  ;; See the comments at `c-guess-accumulator' about the format
  ;; ACCUMULATOR.
  (let* ((basic-offset (c-guess-make-basic-offset accumulator))
	 (typical-offsets-alist (c-guess-make-offsets-alist
				 accumulator))
	 (symbolic-offsets-alist (c-guess-symbolize-offsets-alist
				  typical-offsets-alist
				  basic-offset))
	 (merged-offsets-alist (c-guess-merge-offsets-alists
				(copy-tree c-guess-conversions)
				symbolic-offsets-alist)))
    (cons basic-offset merged-offsets-alist)))

(defun c-guess-current-offset (relpos)
  ;; Calculate relative indentation (point) to RELPOS.
  (- (progn (back-to-indentation)
	    (current-column))
     (save-excursion
       (goto-char relpos)
       (current-column))))

(defun c-guess-accumulate (accumulator syntax-element)
  ;; Add SYNTAX-ELEMENT to ACCUMULATOR.
  (let ((symbol (car syntax-element))
	(relpos (cadr syntax-element)))
    (when (numberp relpos)
      (let ((offset (c-guess-current-offset relpos)))
	(when (< (abs offset) c-guess-offset-threshold)
	  (c-guess-accumulate-offset accumulator
				      symbol
				      offset))))))

(defun c-guess-accumulate-offset (accumulator symbol offset)
  ;; Added SYMBOL and OFFSET to ACCUMULATOR.  See
  ;; `c-guess-accumulator' about the structure of ACCUMULATOR.
  (let* ((entry    (assoc symbol accumulator))
	 (counters (cdr entry))
	 counter)
    (if entry
	(progn
	  (setq counter (assoc offset counters))
	  (if counter
	      (setcdr counter (1+ (cdr counter)))
	    (setq counters (cons (cons offset 1) counters))
	    (setcdr entry counters))
	  accumulator)
      (cons (cons symbol (cons (cons offset 1) nil)) accumulator))))

(defun c-guess-sort-accumulator (accumulator)
  ;; Sort each element of ACCUMULATOR by the number-of-times.  See
  ;; `c-guess-accumulator' for more details.
  (mapcar
   (lambda (entry)
     (let ((symbol (car entry))
	   (counters (cdr entry)))
       (cons symbol (sort counters
			  (lambda (a b)
			    (if (> (cdr a) (cdr b))
				t
			      (and
			       (eq (cdr a) (cdr b))
			       (< (car a) (car b)))))))))
   accumulator))

(defun c-guess-make-offsets-alist (accumulator)
  ;; Throw away the rare cases in accumulator and make an offsets-alist structure.
  (mapcar
   (lambda (entry)
     (cons (car entry)
	   (car (car (cdr entry)))))
   accumulator))

(defun c-guess-merge-offsets-alists (strong weak)
  ;; Merge two offsets-alists into one.
  ;; When two offsets-alists have the same symbol
  ;; entry, give STRONG priority over WEAK.
  (mapc
   (lambda (weak-elt)
     (unless (assoc (car weak-elt) strong)
       (setq strong (cons weak-elt strong))))
   weak)
  strong)

(defun c-guess-make-basic-offset (accumulator)
  ;; As candidate for `c-basic-offset', find the most frequently appearing
  ;; indentation-offset in ACCUMULATOR.
  (let* (;; Drop the value related to `c' syntactic-symbol.
	 ;; (`c': Inside a multiline C style block comment.)
	 ;; The impact for values of `c' is too large for guessing
	 ;; `basic-offset' if the target source file is small and its license
	 ;; notice is at top of the file.
	 (accumulator (assq-delete-all 'c (copy-tree accumulator)))
	 ;; Drop syntactic-symbols from ACCUMULATOR.
	 (alist (apply #'append (mapcar (lambda (elts)
					  (mapcar (lambda (elt)
						    (cons (abs (car elt))
							  (cdr elt)))
						  (cdr elts)))
					accumulator)))
	 ;; Gather all indentation-offsets other than 0.
	 ;; 0 is meaningless as `basic-offset'.
	 (offset-list (delete 0
			      (delete-dups (mapcar
					    (lambda (elt) (car elt))
					    alist))))
	 ;; Sum of number-of-times for offset:
	 ;;  (offset . sum)
	 (summed (mapcar (lambda (offset)
			   (cons offset
				 (apply #'+
					(mapcar (lambda (a)
						  (if (eq (car a) offset)
						      (cdr a)
						    0))
						alist))))
			 offset-list)))
    ;;
    ;; Find the majority.
    ;;
    (let ((majority '(nil . 0)))
      (while summed
	(when (< (cdr majority) (cdr (car summed)))
	  (setq majority (car summed)))
	(setq summed (cdr summed)))
      (car majority))))

(defun c-guess-symbolize-offsets-alist (offsets-alist basic-offset)
  ;; Convert the representation of OFFSETS-ALIST to an alist using
  ;; `+', `-', `++', `--', `*', or `/'. These symbols represent
  ;; a value relative to BASIC-OFFSET.  Their meaning can be found
  ;; in the CC Mode manual.
  (mapcar
   (lambda (elt)
     (let ((s (car elt))
	   (v (cdr elt)))
       (cond
	((integerp v)
	 (cons s (c-guess-symbolize-integer v
					     basic-offset)))
	(t elt))))
   offsets-alist))

(defun c-guess-symbolize-integer (int basic-offset)
  (let ((aint (abs int)))
    (cond
     ((eq int basic-offset) '+)
     ((eq aint basic-offset) '-)
     ((eq int (* 2 basic-offset)) '++)
     ((eq aint (* 2 basic-offset)) '--)
     ((eq (* 2 int) basic-offset) '*)
     ((eq (* 2 aint) basic-offset) '-)
     (t int))))

(defun c-guess-style-name ()
  ;; Make a style name for the guessed style.
  (format "*c-guess*:%s" (buffer-file-name)))

(defun c-guess-make-style (basic-offset offsets-alist)
  (when basic-offset
    ;; Make a style from guessed values.
    (let*	((offsets-alist (c-guess-merge-offsets-alists
				 offsets-alist
				 c-offsets-alist)))
      `((c-basic-offset . ,basic-offset)
	(c-offsets-alist . ,offsets-alist)))))

;;;###autoload
(defun c-guess-install (&optional style-name)
  "Install the latest guessed style into the current buffer.
\(This guessed style is a combination of `c-guess-guessed-basic-offset',
`c-guess-guessed-offsets-alist' and `c-offsets-alist'.)

The style is entered into CC Mode's style system by
`c-add-style'.  Its name is either STYLE-NAME, or a name based on
the absolute file name of the file if STYLE-NAME is nil."
  (interactive "sNew style name (empty for default name): ")
  (let* ((style (c-guess-make-style c-guess-guessed-basic-offset
				    c-guess-guessed-offsets-alist)))
    (if style
	(let ((style-name (or (if (equal style-name "")
				  nil
				style-name)
			      (c-guess-style-name))))
	  (c-add-style style-name style t)
	  (message "Style \"%s\" is installed" style-name))
      (error "Not yet guessed"))))

(defun c-guess-dump-accumulator ()
  "Show `c-guess-accumulator'."
  (interactive)
  (with-output-to-temp-buffer "*Accumulated Examined Indent Information*"
    (pp c-guess-accumulator)))

(defun c-guess-reset-accumulator ()
  "Reset `c-guess-accumulator'."
  (interactive)
  (setq c-guess-accumulator nil))

(defun c-guess-dump-guessed-values ()
  "Show `c-guess-guessed-basic-offset' and `c-guess-guessed-offsets-alist'."
  (interactive)
  (with-output-to-temp-buffer "*Guessed Values*"
    (princ "basic-offset: \n\t")
    (pp c-guess-guessed-basic-offset)
    (princ "\n\n")
    (princ "offsets-alist: \n")
    (pp c-guess-guessed-offsets-alist)
    ))

(defun c-guess-dump-guessed-style (&optional printer)
  "Show the guessed style.
`pp' is used to print the style but if PRINTER is given,
PRINTER is used instead. If PRINTER is not `nil', it
is called with one argument, the guessed style."
  (interactive)
  (let ((style (c-guess-make-style c-guess-guessed-basic-offset
				   c-guess-guessed-offsets-alist)))
    (if style
	(with-output-to-temp-buffer "*Guessed Style*"
	  (funcall (if printer printer 'pp) style))
      (error "Not yet guessed"))))

(defun c-guess-guessed-syntactic-symbols ()
  ;; Return syntactic symbols in c-guess-guessed-offsets-alist
  ;; but not in c-guess-conversions.
  (let ((alist c-guess-guessed-offsets-alist)
	elt
	(symbols nil))
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (unless (assq (car elt) c-guess-conversions)
	(setq symbols (cons (car elt)
			    symbols))))
    symbols))

(defun c-guess-view-reorder-offsets-alist-in-style (style guessed-syntactic-symbols)
  ;; Reorder the `c-offsets-alist' field of STYLE.
  ;; If an entry in `c-offsets-alist' holds a guessed value, move it to
  ;; front in the field. In addition alphabetical sort by entry name is done.
  (setq style (copy-tree style))
  (let ((offsets-alist-cell (assq 'c-offsets-alist style))
	(guessed-syntactic-symbols (c-guess-guessed-syntactic-symbols)))
    (setcdr offsets-alist-cell
	    (sort (cdr offsets-alist-cell)
		  (lambda (a b)
		    (let ((a-guessed? (memq (car a) guessed-syntactic-symbols))
			  (b-guessed? (memq (car b) guessed-syntactic-symbols)))
		      (cond
		       ((or (and a-guessed? b-guessed?)
			    (not (or a-guessed? b-guessed?)))
			(string-lessp (symbol-name (car a))
				      (symbol-name (car b))))
		       (a-guessed? t)
		       (b-guessed? nil)))))))
  style)

(defun c-guess-view-mark-guessed-entries (guessed-syntactic-symbols)
  ;; Put " ; Guess value" markers on all entries which hold
  ;; guessed values.
  ;; `c-basic-offset' is always considered as holding a guessed value.
  (let ((needs-markers (cons 'c-basic-offset
			     guessed-syntactic-symbols)))
    (while needs-markers
      (goto-char (point-min))
      (when (search-forward (concat "("
				    (symbol-name (car needs-markers))
				    " ") nil t)
	(move-end-of-line 1)
	(comment-dwim nil)
	(insert " Guessed value"))
      (setq needs-markers
	    (cdr needs-markers)))))

(defun c-guess-view (&optional with-name)
  "Emit emacs lisp code which defines the last guessed style.
So you can put the code into .emacs if you prefer the
guessed code.
\"STYLE NAME HERE\" is used as the name for the style in the
emitted code. If WITH-NAME is given, it is used instead.
WITH-NAME is expected as a string but if this function
called interactively with prefix argument, the value for
WITH-NAME is asked to the user."
  (interactive "P")
  (let* ((temporary-style-name (cond
				((stringp with-name) with-name)
				(with-name (read-from-minibuffer
					    "New style name: "))
				(t
				 "STYLE NAME HERE")))
	 (guessed-style-name (c-guess-style-name))
	 (current-style-name c-indentation-style)
	 (parent-style-name (if (string-equal guessed-style-name
					      current-style-name)
				;; The guessed style is already installed.
				;; It cannot be used as the parent style.
				;; Use the default style for the current
				;; major mode as the parent style.
				(cc-choose-style-for-mode
				 major-mode
				 c-default-style)
			      ;; The guessed style is not installed yet.
			      current-style-name)))
    (c-guess-dump-guessed-style
     (lambda (style)
       (let ((guessed-syntactic-symbols (c-guess-guessed-syntactic-symbols)))
	 (pp `(c-add-style ,temporary-style-name
			   ',(cons parent-style-name
				   (c-guess-view-reorder-offsets-alist-in-style
				    style
				    guessed-syntactic-symbols))))
	 (with-current-buffer standard-output
	   (lisp-interaction-mode)
	   (c-guess-view-mark-guessed-entries
	    guessed-syntactic-symbols)
	   (buffer-enable-undo)))))))


(cc-provide 'cc-guess)
;;; cc-guess.el ends here
