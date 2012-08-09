;;; ob-ref.el --- org-babel functions for referencing external data

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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

;; Functions for referencing data from the header arguments of a
;; org-babel block.  The syntax of such a reference should be

;;   #+VAR: variable-name=file:resource-id

;; - variable-name :: the name of the variable to which the value
;;                    will be assigned

;; - file :: path to the file containing the resource, or omitted if
;;           resource is in the current file

;; - resource-id :: the id or name of the resource

;; So an example of a simple src block referencing table data in the
;; same file would be

;;  #+TBLNAME: sandbox
;;  | 1 |         2 | 3 |
;;  | 4 | org-babel | 6 |
;;
;;  #+begin_src emacs-lisp :var table=sandbox
;;    (message table)
;;  #+end_src

;;; Code:
(require 'ob)
(eval-when-compile
  (require 'cl))

(declare-function org-remove-if-not "org" (predicate seq))
(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-count "org" (CL-ITEM CL-SEQ))
(declare-function org-at-item-p "org-list" ())
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-id-find-id-in-file "org-id" (id file &optional markerp))
(declare-function org-show-context "org" (&optional key))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

(defvar org-babel-ref-split-regexp
  "[ \f\t\n\r\v]*\\(.+?\\)[ \f\t\n\r\v]*=[ \f\t\n\r\v]*\\(.+\\)[ \f\t\n\r\v]*")

(defvar org-babel-update-intermediate nil
  "Update the in-buffer results of code blocks executed to resolve references.")

(defun org-babel-ref-parse (assignment)
  "Parse a variable ASSIGNMENT in a header argument.
If the right hand side of the assignment has a literal value
return that value, otherwise interpret as a reference to an
external resource and find its value using
`org-babel-ref-resolve'.  Return a list with two elements.  The
first element of the list will be the name of the variable, and
the second will be an emacs-lisp representation of the value of
the variable."
  (when (string-match org-babel-ref-split-regexp assignment)
    (let ((var (match-string 1 assignment))
	  (ref (match-string 2 assignment)))
      (cons (intern var)
	    (let ((out (org-babel-read ref)))
	      (if (equal out ref)
		  (if (string-match "^\".*\"$" ref)
		      (read ref)
		    (org-babel-ref-resolve ref))
		out))))))

(defun org-babel-ref-goto-headline-id (id)
  (goto-char (point-min))
  (let ((rx (regexp-quote id)))
    (or (re-search-forward
	 (concat "^[ \t]*:CUSTOM_ID:[ \t]+" rx "[ \t]*$") nil t)
	(let* ((file (org-id-find-id-file id))
	       (m (when file (org-id-find-id-in-file id file 'marker))))
	  (when (and file m)
	    (message "file:%S" file)
	    (org-pop-to-buffer-same-window (marker-buffer m))
	    (goto-char m)
	    (move-marker m nil)
	    (org-show-context)
	    t)))))

(defun org-babel-ref-headline-body ()
  (save-restriction
    (org-narrow-to-subtree)
    (buffer-substring
     (save-excursion (goto-char (point-min))
		     (forward-line 1)
		     (when (looking-at "[ \t]*:PROPERTIES:")
		       (re-search-forward ":END:" nil)
		       (forward-char))
		     (point))
     (point-max))))

(defvar org-babel-library-of-babel)
(defun org-babel-ref-resolve (ref)
  "Resolve the reference REF and return its value."
  (save-window-excursion
  (save-excursion
    (let ((case-fold-search t)
          type args new-refere new-header-args new-referent result
	  lob-info split-file split-ref index index-row index-col id)
      ;; if ref is indexed grab the indices -- beware nested indices
      (when (and (string-match "\\[\\([^\\[]+\\)\\]$" ref)
		 (let ((str (substring ref 0 (match-beginning 0))))
		   (= (org-count ?( str) (org-count ?) str))))
        (setq index (match-string 1 ref))
        (setq ref (substring ref 0 (match-beginning 0))))
      ;; assign any arguments to pass to source block
      (when (string-match
	     "^\\(.+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)\(\\(.*\\)\)$" ref)
        (setq new-refere      (match-string 1 ref))
	(setq new-header-args (match-string 3 ref))
        (setq new-referent    (match-string 5 ref))
        (when (> (length new-refere) 0)
          (when (> (length new-referent) 0)
	    (setq args (mapcar (lambda (ref) (cons :var ref))
			       (org-babel-ref-split-args new-referent))))
	  (when (> (length new-header-args) 0)
	    (setq args (append (org-babel-parse-header-arguments
				new-header-args) args)))
          (setq ref new-refere)))
      (when (string-match "^\\(.+\\):\\(.+\\)$" ref)
        (setq split-file (match-string 1 ref))
        (setq split-ref (match-string 2 ref))
        (find-file split-file) (setq ref split-ref))
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if (let ((src-rx (org-babel-named-src-block-regexp-for-name ref))
		  (res-rx (org-babel-named-data-regexp-for-name ref)))
	      ;; goto ref in the current buffer
	      (or
	       ;; check for code blocks
	       (re-search-forward src-rx nil t)
	       ;; check for named data
	       (re-search-forward res-rx nil t)
	       ;; check for local or global headlines by id
	       (setq id (org-babel-ref-goto-headline-id ref))
	       ;; check the Library of Babel
	       (setq lob-info (cdr (assoc (intern ref)
					  org-babel-library-of-babel)))))
	    (unless (or lob-info id) (goto-char (match-beginning 0)))
	  ;; ;; TODO: allow searching for names in other buffers
	  ;; (setq id-loc (org-id-find ref 'marker)
	  ;;       buffer (marker-buffer id-loc)
	  ;;       loc (marker-position id-loc))
	  ;; (move-marker id-loc nil)
	  (error "reference '%s' not found in this buffer" ref))
	(cond
	 (lob-info (setq type 'lob))
	 (id (setq type 'id))
	 ((and (looking-at org-babel-src-name-regexp)
	       (save-excursion
		 (forward-line 1)
		 (or (looking-at org-babel-src-block-regexp)
		     (looking-at org-babel-multi-line-header-regexp))))
	  (setq type 'source-block))
	 (t (while (not (setq type (org-babel-ref-at-ref-p)))
	      (forward-line 1)
	      (beginning-of-line)
	      (if (or (= (point) (point-min)) (= (point) (point-max)))
		  (error "reference not found")))))
	(let ((params (append args '((:results . "silent")))))
	  (setq result
		(case type
		  (results-line (org-babel-read-result))
		  (table        (org-babel-read-table))
		  (list         (org-babel-read-list))
		  (file         (org-babel-read-link))
		  (source-block (org-babel-execute-src-block
				 nil nil (if org-babel-update-intermediate
					     nil params)))
		  (lob          (org-babel-execute-src-block
				 nil lob-info params))
		  (id           (org-babel-ref-headline-body)))))
	(if (symbolp result)
	    (format "%S" result)
	  (if (and index (listp result))
	      (org-babel-ref-index-list index result)
	    result)))))))

(defun org-babel-ref-index-list (index lis)
  "Return the subset of LIS indexed by INDEX.

Indices are 0 based and negative indices count from the end of
LIS, so 0 references the first element of LIS and -1 references
the last.  If INDEX is separated by \",\"s then each \"portion\"
is assumed to index into the next deepest nesting or dimension.

A valid \"portion\" can consist of either an integer index, two
integers separated by a \":\" in which case the entire range is
returned, or an empty string or \"*\" both of which are
interpreted to mean the entire range and as such are equivalent
to \"0:-1\"."
  (if (and (> (length index) 0) (string-match "^\\([^,]*\\),?" index))
      (let ((ind-re "\\(\\([-[:digit:]]+\\):\\([-[:digit:]]+\\)\\|\*\\)")
	    (length (length lis))
            (portion (match-string 1 index))
            (remainder (substring index (match-end 0))))
        (flet ((wrap (num) (if (< num 0) (+ length num) num))
               (open (ls) (if (and (listp ls) (= (length ls) 1)) (car ls) ls)))
          (open
           (mapcar
            (lambda (sub-lis)
	      (if (listp sub-lis)
		  (org-babel-ref-index-list remainder sub-lis)
		sub-lis))
            (if (or (= 0 (length portion)) (string-match ind-re portion))
                (mapcar
		 (lambda (n) (nth n lis))
		 (apply 'org-number-sequence
			(if (and (> (length portion) 0) (match-string 2 portion))
			    (list
			     (wrap (string-to-number (match-string 2 portion)))
			     (wrap (string-to-number (match-string 3 portion))))
			  (list (wrap 0) (wrap -1)))))
              (list (nth (wrap (string-to-number portion)) lis)))))))
    lis))

(defun org-babel-ref-split-args (arg-string)
  "Split ARG-STRING into top-level arguments of balanced parenthesis."
  (mapcar #'org-babel-trim (org-babel-balanced-split arg-string 44)))

(defvar org-bracket-link-regexp)
(defun org-babel-ref-at-ref-p ()
  "Return the type of reference located at point.
Return nil if none of the supported reference types are found.
Supported reference types are tables and source blocks."
  (cond ((org-at-table-p) 'table)
	((org-at-item-p) 'list)
        ((looking-at "^[ \t]*#\\+BEGIN_SRC") 'source-block)
        ((looking-at org-bracket-link-regexp) 'file)
        ((looking-at org-babel-result-regexp) 'results-line)))

(provide 'ob-ref)



;;; ob-ref.el ends here
