;;; sort.el --- commands to sort text in an Emacs buffer

;; Copyright (C) 1986-1987, 1994-1995, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Howie Kaye
;; Maintainer: FSF
;; Keywords: unix

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

;; This package provides the sorting facilities documented in the Emacs
;; user's manual.

;;; Code:

(defgroup sort nil
  "Commands to sort text in an Emacs buffer."
  :group 'data)

(defcustom sort-fold-case nil
  "Non-nil if the buffer sort functions should ignore case."
  :group 'sort
  :type 'boolean)
;;;###autoload(put 'sort-fold-case 'safe-local-variable 'booleanp)

;;;###autoload
(defun sort-subr (reverse nextrecfun endrecfun
			  &optional startkeyfun endkeyfun predicate)
  "General text sorting routine to divide buffer into records and sort them.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN.

PREDICATE is the function to use to compare keys.  If keys are numbers,
it defaults to `<', otherwise it defaults to `string<'."
  ;; Heuristically try to avoid messages if sorting a small amt of text.
  (let ((messages (> (- (point-max) (point-min)) 50000)))
    (save-excursion
      (if messages (message "Finding sort keys..."))
      (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
					   startkeyfun endkeyfun))
	     (old (reverse sort-lists))
	     (case-fold-search sort-fold-case))
	(if (null sort-lists)
	    ()
	  (or reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Sorting records..."))
	  (setq sort-lists
		(sort sort-lists
		      (cond (predicate
			     `(lambda (a b) (,predicate (car a) (car b))))
			    ((numberp (car (car sort-lists)))
			     'car-less-than-car)
			    ((consp (car (car sort-lists)))
			     (lambda (a b)
			       (> 0 (compare-buffer-substrings
				     nil (car (car a)) (cdr (car a))
				     nil (car (car b)) (cdr (car b))))))
			    (t
			     (lambda (a b) (string< (car a) (car b)))))))
	  (if reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Reordering buffer..."))
	  (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done"))))
  nil)

;; Parse buffer into records using the arguments as Lisp expressions;
;; return a list of records.  Each record looks like (KEY STARTPOS . ENDPOS)
;; where KEY is the sort key (a number or string),
;; and STARTPOS and ENDPOS are the bounds of this record in the buffer.

;; The records appear in the list lastmost first!

(defun sort-build-lists (nextrecfun endrecfun startkeyfun endkeyfun)
  (let ((sort-lists ())
	(start-rec nil)
	done key)
    ;; Loop over sort records.
    ;(goto-char (point-min)) -- it is the caller's responsibility to
    ;arrange this if necessary
    (while (not (eobp))
      (setq start-rec (point))		;save record start
      (setq done nil)
      ;; Get key value, or move to start of key.
      (setq key (catch 'key
		  (or (and startkeyfun (funcall startkeyfun))
		      ;; If key was not returned as value,
		      ;; move to end of key and get key from the buffer.
		      (let ((start (point)))
			(funcall (or endkeyfun
				     (prog1 endrecfun (setq done t))))
			(cons start (point))))))
      ;; Move to end of this record (start of next one, or end of buffer).
      (cond ((prog1 done (setq done nil)))
	    (endrecfun (funcall endrecfun))
	    (nextrecfun (funcall nextrecfun) (setq done t)))
      (if key (push
	       ;; consing optimization in case in which key is same as record.
	       (if (and (consp key)
			(equal (car key) start-rec)
			(equal (cdr key) (point)))
		   (cons key key)
		 (cons key (cons start-rec (point))))
	       sort-lists))
      (and (not done) nextrecfun (funcall nextrecfun)))
    sort-lists))

(defun sort-reorder-buffer (sort-lists old)
  (let ((last (point-min))
	(min (point-min)) (max (point-max))
	(old-buffer (current-buffer))
        (mb enable-multibyte-characters)
	temp-buffer)
    (with-temp-buffer
      (set-buffer-multibyte mb)
      ;; Record the temporary buffer.
      (setq temp-buffer (current-buffer))

      ;; Copy the sorted text into the temporary buffer.
      (while sort-lists
	(goto-char (point-max))
	(insert-buffer-substring old-buffer
				 last
				 (nth 1 (car old)))
	(goto-char (point-max))
	(insert-buffer-substring old-buffer
				 (nth 1 (car sort-lists))
				 (cdr (cdr (car sort-lists))))
	(setq last (cdr (cdr (car old)))
	      sort-lists (cdr sort-lists)
	      old (cdr old)))
      (goto-char (point-max))
      (insert-buffer-substring old-buffer last max)

      ;; Copy the reordered text from the temporary buffer
      ;; to the buffer we sorted (OLD-BUFFER).
      (set-buffer old-buffer)
      (let ((inhibit-quit t))
	;; Make sure insertions done for reordering
	;; saves any markers at the end of the sorted region,
	;; by leaving the last character of the region.
	(delete-region min (1- max))
  	;; Now replace the one remaining old character with the sorted text.
	(goto-char (point-min))
	(insert-buffer-substring temp-buffer)
	(delete-region max (1+ max))))))

;;;###autoload
(defun sort-lines (reverse beg end)
  "Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
	  ((inhibit-field-text-motion t))
	(sort-subr reverse 'forward-line 'end-of-line)))))

;;;###autoload
(defun sort-paragraphs (reverse beg end)
  "Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
		 (function
		  (lambda ()
		    (while (and (not (eobp)) (looking-at paragraph-separate))
		      (forward-line 1))))
		 'forward-paragraph))))

;;;###autoload
(defun sort-pages (reverse beg end)
  "Sort pages in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
		 (function (lambda () (skip-chars-forward "\n")))
		 'forward-page))))

(defvar sort-fields-syntax-table nil)
(if sort-fields-syntax-table nil
  (let ((table (make-syntax-table))
	(i 0))
    (while (< i 256)
      (modify-syntax-entry i "w" table)
      (setq i (1+ i)))
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\n " " table)
    (modify-syntax-entry ?\. "_" table)	; for floating pt. numbers. -wsr
    (setq sort-fields-syntax-table table)))

(defcustom sort-numeric-base 10
  "The default base used by `sort-numeric-fields'."
  :group 'sort
  :type 'integer)
;;;###autoload(put 'sort-numeric-base 'safe-local-variable 'integerp)

;;;###autoload
(defun sort-numeric-fields (field beg end)
  "Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
Specified field must contain a number in each line of the region,
which may begin with \"0x\" or \"0\" for hexadecimal and octal values.
Otherwise, the number is interpreted according to sort-numeric-base.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  (interactive "p\nr")
  (let ;; To make `end-of-line' and etc. to ignore fields.
      ((inhibit-field-text-motion t))
    (sort-fields-1 field beg end
		   (lambda ()
		     (sort-skip-fields field)
		     (let* ((case-fold-search t)
			    (base
			     (if (looking-at "\\(0x\\)[0-9a-f]\\|\\(0\\)[0-7]")
				 (cond ((match-beginning 1)
					(goto-char (match-end 1))
					16)
				       ((match-beginning 2)
					(goto-char (match-end 2))
					8)
				       (t nil)))))
		       (string-to-number (buffer-substring (point)
							   (save-excursion
							     (forward-sexp 1)
							     (point)))
					 (or base sort-numeric-base))))
		   nil)))

;;;;;###autoload
;;(defun sort-float-fields (field beg end)
;;  "Sort lines in region numerically by the ARGth field of each line.
;;Fields are separated by whitespace and numbered from 1 up.  Specified field
;;must contain a floating point number in each line of the region.  With a
;;negative arg, sorts by the ARGth field counted from the right.  Called from a
;;program, there are three arguments: FIELD, BEG and END.  BEG and END specify
;;region to sort."
;;  (interactive "p\nr")
;;  (sort-fields-1 field beg end
;;		 (function (lambda ()
;;			     (sort-skip-fields field)
;;			     (string-to-number
;;			      (buffer-substring
;;			       (point)
;;			       (save-excursion
;;				 (re-search-forward
;;				  "[+-]?[0-9]*\.?[0-9]*\\([eE][+-]?[0-9]+\\)?")
;;				 (point))))))
;;		 nil))

;;;###autoload
(defun sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "p\nr")
  (let ;; To make `end-of-line' and etc. to ignore fields.
      ((inhibit-field-text-motion t))
    (sort-fields-1 field beg end
		   (function (lambda ()
			       (sort-skip-fields field)
			       nil))
		   (function (lambda () (skip-chars-forward "^ \t\n"))))))

(defun sort-fields-1 (field beg end startkeyfun endkeyfun)
  (let ((tbl (syntax-table)))
    (if (zerop field) (setq field 1))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (set-syntax-table sort-fields-syntax-table)
	    (sort-subr nil
		       'forward-line 'end-of-line
		       startkeyfun endkeyfun)))
      (set-syntax-table tbl))))

;; Position at the beginning of field N on the current line,
;; assuming point is initially at the beginning of the line.
(defun sort-skip-fields (n)
  (if (> n 0)
      ;; Skip across N - 1 fields.
      (let ((i (1- n)))
	(while (> i 0)
	  (skip-chars-forward " \t")
	  (skip-chars-forward "^ \t\n")
	  (setq i (1- i)))
	(skip-chars-forward " \t")
	(if (eolp)
	    (error "Line has too few fields: %s"
		   (buffer-substring
		    (line-beginning-position)
		    (line-end-position)))))
    (end-of-line)
    ;; Skip back across - N - 1 fields.
    (let ((i (1- (- n))))
      (while (> i 0)
	(skip-chars-backward " \t")
	(skip-chars-backward "^ \t\n")
	(setq i (1- i)))
      (skip-chars-backward " \t"))
    (if (bolp)
	(error "Line has too few fields: %s"
	       (buffer-substring
		(line-beginning-position)
		(line-end-position))))
    ;; Position at the front of the field
    ;; even if moving backwards.
    (skip-chars-backward "^ \t\n")))

(defvar sort-regexp-fields-regexp)
(defvar sort-regexp-record-end)

;; Move to the beginning of the next match for record-regexp,
;; and set sort-regexp-record-end to the end of that match.
;; If the next match is empty and does not advance point,
;; skip one character and try again.
(defun sort-regexp-fields-next-record ()
  (let ((oldpos (point)))
    (and (re-search-forward sort-regexp-fields-regexp nil 'move)
	 (setq sort-regexp-record-end (match-end 0))
	 (if (= sort-regexp-record-end oldpos)
	     (progn
	       (forward-char 1)
	       (re-search-forward sort-regexp-fields-regexp nil 'move)
	       (setq sort-regexp-record-end (match-end 0)))
	   t)
	 (goto-char (match-beginning 0)))))

;;;###autoload
(defun sort-regexp-fields (reverse record-regexp key-regexp beg end)
  "Sort the region lexicographically as specified by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\\\digit\" then the digit'th \"\\\\(...\\\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\\\=\\<f\\\\w*\\\\>\""
  ;; using negative prefix arg to mean "reverse" is now inconsistent with
  ;; other sort-.*fields functions but then again this was before, since it
  ;; didn't use the magnitude of the arg to specify anything.
  (interactive "P\nsRegexp specifying records to sort: \n\
sRegexp specifying key within record: \nr")
  (cond ((or (equal key-regexp "") (equal key-regexp "\\&"))
	 (setq key-regexp 0))
	((string-match "\\`\\\\[1-9]\\'" key-regexp)
	 (setq key-regexp (- (aref key-regexp 1) ?0))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (sort-regexp-record-end
	    (sort-regexp-fields-regexp record-regexp))
	(re-search-forward sort-regexp-fields-regexp nil t)
	(setq sort-regexp-record-end (point))
	(goto-char (match-beginning 0))
	(sort-subr reverse
		   'sort-regexp-fields-next-record
		   (function (lambda ()
			       (goto-char sort-regexp-record-end)))
		   (function (lambda ()
			       (let ((n 0))
				 (cond ((numberp key-regexp)
					(setq n key-regexp))
				       ((re-search-forward
					  key-regexp sort-regexp-record-end t)
					(setq n 0))
				       (t (throw 'key nil)))
				 (condition-case ()
				     (cons (match-beginning n)
					   (match-end n))
				   ;; if there was no such register
				   (error (throw 'key nil)))))))))))


(defvar sort-columns-subprocess t)

;;;###autoload
(defun sort-columns (reverse &optional beg end)
  "Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region BEG...END includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into REVERSE order.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

Note that `sort-columns' rejects text that contains tabs,
because tabs could be split across the specified columns
and it doesn't know how to handle that.  Also, when possible,
it uses the `sort' utility program, which doesn't understand tabs.
Use \\[untabify] to convert tabs to spaces before sorting."
  (interactive "P\nr")
  (save-excursion
    (let ;; To make `end-of-line' and etc. to ignore fields.
	((inhibit-field-text-motion t)
	 beg1 end1 col-beg1 col-end1 col-start col-end)
      (goto-char (min beg end))
      (setq col-beg1 (current-column))
      (beginning-of-line)
      (setq beg1 (point))
      (goto-char (max beg end))
      (setq col-end1 (current-column))
      (forward-line)
      (setq end1 (point))
      (setq col-start (min col-beg1 col-end1))
      (setq col-end (max col-beg1 col-end1))
      (if (search-backward "\t" beg1 t)
	  (error "sort-columns does not work with tabs -- use M-x untabify"))
      (if (not (or (memq system-type '(windows-nt))
		   (let ((pos beg1) plist fontified)
		     (catch 'found
		       (while (< pos end1)
			 (setq plist (text-properties-at pos))
			 (setq fontified (plist-get plist 'fontified))
			 (while (consp plist)
			   (unless (or (eq (car plist) 'fontified)
				       (and (eq (car plist) 'face)
					    fontified))
			     (throw 'found t))
			   (setq plist (cddr plist)))
			 (setq pos (next-property-change pos nil end1)))))))
	  ;; Use the sort utility if we can; it is 4 times as fast.
	  ;; Do not use it if there are any non-font-lock properties
	  ;; in the region, since the sort utility would lose the
	  ;; properties.  Tabs are used as field separator; on NetBSD,
	  ;; sort complains if "\n" is used as field separator.
	  (let ((sort-args (list (if reverse "-rt\t" "-t\t")
				 (format "-k1.%d,1.%d"
					 (1+ col-start)
					 (1+ col-end)))))
	    (when sort-fold-case
	      (push "-f" sort-args))
	    (apply #'call-process-region beg1 end1 "sort" t t nil sort-args))
	;; On ms-windows, use Emacs's own facilities.
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg1 end1)
	    (goto-char beg1)
	    (sort-subr reverse 'forward-line 'end-of-line
		       #'(lambda () (move-to-column col-start) nil)
		       #'(lambda () (move-to-column col-end) nil))))))))

;;;###autoload
(defun reverse-region (beg end)
  "Reverse the order of lines in a region.
From a program takes two point or marker arguments, BEG and END."
  (interactive "r")
  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty line;
    ;; it is probably not the case that the line should be included in the
    ;; reversal; it isn't difficult to add it afterward.
    (or (and (eolp) (not (bolp))) (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))
    ;; the real work.  this thing cranks through memory on large regions.
    (let (ll (do t))
      (while do
	(goto-char beg)
	(setq ll (cons (buffer-substring (point) (progn (end-of-line) (point)))
		       ll))
	(setq do (/= (point) end))
	(delete-region beg (if do (1+ (point)) (point))))
      (while (cdr ll)
	(insert (car ll) "\n")
	(setq ll (cdr ll)))
      (insert (car ll)))))

(provide 'sort)

;;; sort.el ends here
