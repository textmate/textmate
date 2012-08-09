;;; rmailsort.el --- Rmail: sort messages

;; Copyright (C) 1990, 1993-1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Maintainer: FSF
;; Keywords: mail
;; Package: rmail

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

;; Functions for sorting messages in an Rmail buffer.

;;; Code:

(require 'rmail)

;;;###autoload
(defun rmail-sort-by-date (reverse)
  "Sort messages of current Rmail buffer by \"Date\" header.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (lambda (msg)
			 (rmail-make-date-sortable
			  (rmail-get-header "Date" msg)))))

;;;###autoload
(defun rmail-sort-by-subject (reverse)
  "Sort messages of current Rmail buffer by \"Subject\" header.
Ignores any \"Re: \" prefix.  If prefix argument REVERSE is
non-nil, sorts in reverse order."
  ;; Note this is a case-sensitive sort.
  (interactive "P")
  (rmail-sort-messages reverse
		       (lambda (msg)
			 (let ((key (or (rmail-get-header "Subject" msg) ""))
			       (case-fold-search t))
			   ;; Remove `Re:'
			   (if (string-match "^\\(re:[ \t]*\\)*" key)
			       (substring key (match-end 0))
			     key)))))

;;;###autoload
(defun rmail-sort-by-author (reverse)
  "Sort messages of current Rmail buffer by author.
This uses either the \"From\" or \"Sender\" header, downcased.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (lambda (msg)
			 (downcase	; canonical name
			  (mail-strip-quoted-names
			   (or (rmail-get-header "From" msg)
			       (rmail-get-header "Sender" msg) ""))))))

;;;###autoload
(defun rmail-sort-by-recipient (reverse)
  "Sort messages of current Rmail buffer by recipient.
This uses either the \"To\" or \"Apparently-To\" header, downcased.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (lambda (msg)
			 (downcase	; canonical name
			  (mail-strip-quoted-names
			   (or (rmail-get-header "To" msg)
			       (rmail-get-header "Apparently-To" msg) ""))))))

;;;###autoload
(defun rmail-sort-by-correspondent (reverse)
  "Sort messages of current Rmail buffer by other correspondent.
This uses either the \"From\", \"Sender\", \"To\", or
\"Apparently-To\" header, downcased.  Uses the first header not
excluded by `mail-dont-reply-to-names'.  If prefix argument
REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (lambda (msg)
			 (downcase
			  (rmail-select-correspondent
			   msg
			   '("From" "Sender" "To" "Apparently-To"))))))

(defun rmail-select-correspondent (msg fields)
  "Find the first header not excluded by `mail-dont-reply-to-names'.
MSG is a message number.  FIELDS is a list of header names."
  (let ((ans ""))
    (while (and fields (string= ans ""))
      (setq ans
	    (mail-dont-reply-to
	     (mail-strip-quoted-names
	      (or (rmail-get-header (car fields) msg) ""))))
      (setq fields (cdr fields)))
    ans))

;;;###autoload
(defun rmail-sort-by-lines (reverse)
  "Sort messages of current Rmail buffer by the number of lines.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (lambda (msg)
			 (count-lines (rmail-msgbeg msg)
				      (rmail-msgend msg)))))

;;;###autoload
(defun rmail-sort-by-labels (reverse labels)
  "Sort messages of current Rmail buffer by labels.
LABELS is a comma-separated list of labels.  The order of these
labels specifies the order of messages: messages with the first
label come first, messages with the second label come second, and
so on.  Messages that have none of these labels come last.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P\nsSort by labels: ")
  (or (string-match "[^ \t]" labels)	; need some non-whitespace
      (error "No labels specified"))
  ;; Remove leading whitespace, add trailing comma.
  (setq labels (concat (substring labels (match-beginning 0)) ","))
  (let (labelvec nmax)
    ;; Convert "l1,..." into "\\(, \\|\\`\\)l1\\(,\\|\\'\\)" "..." ...
    (while (string-match "[ \t]*,[ \t]*" labels)
      (setq labelvec (cons
		      (concat "\\(, \\|\\`\\)"
			      (substring labels 0 (match-beginning 0))
			      "\\(,\\|\\'\\)")
		      labelvec))
      (setq labels (substring labels (match-end 0))))
    (setq labelvec (apply 'vector (nreverse labelvec))
	  nmax (length labelvec))
    (rmail-sort-messages reverse
			 ;; If no labels match, returns nmax; if they
			 ;; match the first specified in LABELS,
			 ;; returns 0; if they match the second, returns 1; etc.
			 ;; Hence sorts as described in the doc-string.
			 (lambda (msg)
			   (let ((n 0)
				 (str (concat (rmail-get-attr-names msg)
					      ", "
					      (rmail-get-keywords msg))))
			     ;; No labels: can't match anything.
			     (if (string-equal ", " str)
				 nmax
			       (while (and (< n nmax)
					   (not (string-match (aref labelvec n)
							      str)))
				 (setq n (1+ n)))
			       n))))))

;; Basic functions
(declare-function rmail-update-summary "rmailsum" (&rest ignore))

(defun rmail-sort-messages (reverse keyfun)
  "Sort messages of current Rmail buffer.
If REVERSE is non-nil, sorts in reverse order.  Calls the
function KEYFUN with a message number (it should return a sort key).
Numeric keys are sorted numerically, all others as strings."
  (with-current-buffer rmail-buffer
    (let ((return-to-point
	   (if (rmail-buffers-swapped-p)
	       (point)))
	  (sort-lists nil))
      (rmail-swap-buffers-maybe)
      (message "Finding sort keys...")
      (widen)
      (let ((msgnum 1))
	(while (>= rmail-total-messages msgnum)
	  (setq sort-lists
		(cons (list (funcall keyfun msgnum) ;Make sorting key
			    (eq rmail-current-message msgnum) ;True if current
			    (aref rmail-message-vector msgnum)
			    (aref rmail-message-vector (1+ msgnum)))
		      sort-lists))
	  (if (zerop (% msgnum 10))
	      (message "Finding sort keys...%d" msgnum))
	  (setq msgnum (1+ msgnum))))
      (or reverse (setq sort-lists (nreverse sort-lists)))
      (setq sort-lists
	    (sort sort-lists
                  ;; Decide predicate: < or string-lessp
                  (if (numberp (car (car sort-lists))) ;Is a key numeric?
                      'car-less-than-car
		    (lambda (a b)
		      (string-lessp (car a) (car b))))))
      (if reverse (setq sort-lists (nreverse sort-lists)))
      ;; Now we enter critical region.  So, keyboard quit is disabled.
      (message "Reordering messages...")
      (let ((inhibit-quit t)		;Inhibit quit
	    (inhibit-read-only t)
	    (current-message nil)
	    (msgnum 1)
	    (msginfo nil)
	    (undo (not (eq buffer-undo-list t))))
	;; There's little hope that we can easily undo after that.
	(buffer-disable-undo (current-buffer))
	(goto-char (rmail-msgbeg 1))
	;; To force update of all markers,
	;; keep the new copies separated from the remaining old messages.
	(insert-before-markers ?Z)
	(backward-char 1)
	;; Now reorder messages.
	(dolist (msginfo sort-lists)
	  ;; Swap two messages.
	  (insert-buffer-substring
	   (current-buffer) (nth 2 msginfo) (nth 3 msginfo))
	  ;; The last message may not have \n\n after it.
	  (rmail-ensure-blank-line)
	  (delete-region (nth 2 msginfo) (nth 3 msginfo))
	  ;; Is current message?
	  (if (nth 1 msginfo)
	      (setq current-message msgnum))
	  (if (zerop (% msgnum 10))
	      (message "Reordering messages...%d" msgnum))
	  (setq msgnum (1+ msgnum)))
	;; Delete the dummy separator Z inserted before.
	(delete-char 1)
	(setq quit-flag nil)
	;; If undo was on before, re-enable it.  But note that it is
	;; disabled in mbox Rmail, so this is kind of pointless.
	(if undo (buffer-enable-undo))
	(rmail-set-message-counters)
	(rmail-show-message-1 current-message)
	(if return-to-point
	    (goto-char return-to-point))
	(if (rmail-summary-exists)
	    (rmail-select-summary (rmail-update-summary)))))))

(autoload 'timezone-make-date-sortable "timezone")

(defun rmail-make-date-sortable (date)
  "Make DATE sortable using the function `string-lessp'."
  ;; Assume the default time zone is GMT.
  (timezone-make-date-sortable date "GMT" "GMT"))

(provide 'rmailsort)

;; Local Variables:
;; generated-autoload-file: "rmail.el"
;; End:

;;; rmailsort.el ends here
