;;; gnus-logic.el --- advanced scoring code for Gnus

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

(require 'gnus)
(require 'gnus-score)
(require 'gnus-util)

;;; Internal variables.

(defvar gnus-advanced-headers nil)

;; To avoid having 8-bit characters in the source file.
(defvar gnus-advanced-not (intern (format "%c" 172)))

(defconst gnus-advanced-index
  ;; Name to index alist.
  '(("number" 0 gnus-advanced-integer)
    ("subject" 1 gnus-advanced-string)
    ("from" 2 gnus-advanced-string)
    ("date" 3 gnus-advanced-date)
    ("message-id" 4 gnus-advanced-string)
    ("references" 5 gnus-advanced-string)
    ("chars" 6 gnus-advanced-integer)
    ("lines" 7 gnus-advanced-integer)
    ("xref" 8 gnus-advanced-string)
    ("head" nil gnus-advanced-body)
    ("body" nil gnus-advanced-body)
    ("all" nil gnus-advanced-body)))

(autoload 'parse-time-string "parse-time")

(defun gnus-score-advanced (rule &optional trace)
  "Apply advanced scoring RULE to all the articles in the current group."
  (let (new-score score multiple)
    (dolist (gnus-advanced-headers gnus-newsgroup-headers)
      (when (setq multiple (gnus-advanced-score-rule (car rule)))
	(setq new-score (or (nth 1 rule)
			    gnus-score-interactive-default-score))
	(when (numberp multiple)
	  (setq new-score (* multiple new-score)))
	;; This rule was successful, so we add the score to this
	;; article.
	(if (setq score (assq (mail-header-number gnus-advanced-headers)
			      gnus-newsgroup-scored))
	    (setcdr score
		    (+ (cdr score) new-score))
	  (push (cons (mail-header-number gnus-advanced-headers)
		      new-score)
		gnus-newsgroup-scored)
	  (when trace
	    (push (cons "A file" rule)
		  ;; Must be synced with `gnus-score-edit-file-at-point'.
		  gnus-score-trace)))))))

(defun gnus-advanced-score-rule (rule)
  "Apply RULE to `gnus-advanced-headers'."
  (let ((type (car rule)))
    (cond
     ;; "And" rule.
     ((or (eq type '&) (eq type 'and))
      (pop rule)
      (if (not rule)
	  t				; Empty rule is true.
	(while (and rule
		    (gnus-advanced-score-rule (car rule)))
	  (pop rule))
	;; If all the rules were true, then `rule' should be nil.
	(not rule)))
     ;; "Or" rule.
     ((or (eq type '|) (eq type 'or))
      (pop rule)
      (if (not rule)
	  nil
	(while (and rule
		    (not (gnus-advanced-score-rule (car rule))))
	  (pop rule))
	;; If one of the rules returned true, then `rule' should be non-nil.
	rule))
     ;; "Not" rule.
     ((or (eq type '!) (eq type 'not) (eq type gnus-advanced-not))
      (not (gnus-advanced-score-rule (nth 1 rule))))
     ;; This is a `1-'-type redirection rule.
     ((and (symbolp type)
	   (string-match "^[0-9]+-$\\|^\\^+$" (symbol-name type)))
      (let ((gnus-advanced-headers
	     (gnus-parent-headers
	      gnus-advanced-headers
	      (if (string-match "^\\([0-9]+\\)-$" (symbol-name type))
		  ;; 1- type redirection.
		  (string-to-number
		   (substring (symbol-name type)
			      (match-beginning 1) (match-end 1)))
		;; ^^^ type redirection.
		(length (symbol-name type))))))
	(when gnus-advanced-headers
	  (gnus-advanced-score-rule (nth 1 rule)))))
     ;; Plain scoring rule.
     ((stringp type)
      (gnus-advanced-score-article rule))
     ;; Bug-out time!
     (t
      (error "Unknown advanced score type: %s" rule)))))

(defun gnus-advanced-score-article (rule)
  ;; `rule' is a semi-normal score rule, so we find out what function
  ;; that's supposed to do the actual processing.
  (let* ((header (car rule))
	 (func (assoc (downcase header) gnus-advanced-index)))
    (if (not func)
	(error "No such header: %s" rule)
      ;; Call the score function.
      (funcall (caddr func) (or (cadr func) header)
	       (cadr rule) (caddr rule)))))

(defun gnus-advanced-string (index match type)
  "See whether string MATCH of TYPE matches `gnus-advanced-headers' in INDEX."
  (let* ((type (or type 's))
	 (case-fold-search (not (eq (downcase (symbol-name type))
				    (symbol-name type))))
	 (header (or (aref gnus-advanced-headers index) "")))
    (cond
     ((memq type '(r R regexp Regexp))
      (string-match match header))
     ((memq type '(s S string String))
      (string-match (regexp-quote match) header))
     ((memq type '(e E exact Exact))
      (string= match header))
     ((memq type '(f F fuzzy Fuzzy))
      (string-match (regexp-quote (gnus-simplify-subject-fuzzy match))
		    header))
     (t
      (error "No such string match type: %s" type)))))

(defun gnus-advanced-integer (index match type)
  (if (not (memq type '(< > <= >= =)))
      (error "No such integer score type: %s" type)
    (funcall type (or (aref gnus-advanced-headers index) 0) match)))

(defun gnus-advanced-date (index match type)
  (let ((date (apply 'encode-time (parse-time-string
				   (aref gnus-advanced-headers index))))
	(match (apply 'encode-time (parse-time-string match))))
    (cond
     ((eq type 'at)
      (equal date match))
     ((eq type 'before)
      (time-less-p match date))
     ((eq type 'after)
      (time-less-p date match))
     (t
      (error "No such date score type: %s" type)))))

(defun gnus-advanced-body (header match type)
  (when (string= header "all")
    (setq header "article"))
  (with-current-buffer nntp-server-buffer
    (let* ((request-func (cond ((string= "head" header)
				'gnus-request-head)
			       ((string= "body" header)
				'gnus-request-body)
			       (t 'gnus-request-article)))
	   ofunc article)
      ;; Not all backends support partial fetching.  In that case, we
      ;; just fetch the entire article.
      (unless (gnus-check-backend-function
	       (intern (concat "request-" header))
	       gnus-newsgroup-name)
	(setq ofunc request-func)
	(setq request-func 'gnus-request-article))
      (setq article (mail-header-number gnus-advanced-headers))
      (gnus-message 7 "Scoring article %s..." article)
      (when (funcall request-func article gnus-newsgroup-name)
	(goto-char (point-min))
	;; If just parts of the article is to be searched and the
	;; backend didn't support partial fetching, we just narrow to
	;; the relevant parts.
	(when ofunc
	  (if (eq ofunc 'gnus-request-head)
	      (narrow-to-region
	       (point)
	       (or (search-forward "\n\n" nil t) (point-max)))
	    (narrow-to-region
	     (or (search-forward "\n\n" nil t) (point))
	     (point-max))))
	(let* ((case-fold-search (not (eq (downcase (symbol-name type))
					  (symbol-name type))))
	       (search-func
		(cond ((memq type '(r R regexp Regexp))
		       're-search-forward)
		      ((memq type '(s S string String))
		       'search-forward)
		      (t
		       (error "Invalid match type: %s" type)))))
	  (goto-char (point-min))
	  (prog1
	      (funcall search-func match nil t)
	    (widen)))))))

(provide 'gnus-logic)

;;; gnus-logic.el ends here
