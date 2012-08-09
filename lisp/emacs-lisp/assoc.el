;;; assoc.el --- insert/delete functions on association lists

;; Copyright (C) 1996, 2001-2012  Free Software Foundation, Inc.

;; Author: Barry A. Warsaw <bwarsaw@cen.com>
;; Keywords: extensions

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

;; Association list utilities providing insertion, deletion, sorting
;; fetching off key-value pairs in association lists.

;;; Code:
(eval-when-compile (require 'cl))

(defun asort (alist-symbol key)
  "Move a specified key-value pair to the head of an alist.
The alist is referenced by ALIST-SYMBOL.  Key-value pair to move to
head is one matching KEY.  Returns the sorted list and doesn't affect
the order of any other key-value pair.  Side effect sets alist to new
sorted list."
  (set alist-symbol
       (sort (copy-alist (symbol-value alist-symbol))
	     (function (lambda (a b) (equal (car a) key))))))


(defun aelement (key value)
  "Make a list of a cons cell containing car of KEY and cdr of VALUE.
The returned list is suitable for concatenating with an existing
alist, via `nconc'."
  (list (cons key value)))


(defun aheadsym (alist)
  "Return the key symbol at the head of ALIST."
  (car (car alist)))


(defun anot-head-p (alist key)
  "Find out if a specified key-value pair is not at the head of an alist.
The alist to check is specified by ALIST and the key-value pair is the
one matching the supplied KEY.  Returns nil if ALIST is nil, or if
key-value pair is at the head of the alist.  Returns t if key-value
pair is not at the head of alist.  ALIST is not altered."
  (not (equal (aheadsym alist) key)))


(defun aput (alist-symbol key &optional value)
  "Insert a key-value pair into an alist.
The alist is referenced by ALIST-SYMBOL.  The key-value pair is made
from KEY and optionally, VALUE.  Returns the altered alist.

If the key-value pair referenced by KEY can be found in the alist, and
VALUE is supplied non-nil, then the value of KEY will be set to VALUE.
If VALUE is not supplied, or is nil, the key-value pair will not be
modified, but will be moved to the head of the alist.  If the key-value
pair cannot be found in the alist, it will be inserted into the head
of the alist (with value nil if VALUE is nil or not supplied)."
  (lexical-let ((elem (aelement key value))
		alist)
    (asort alist-symbol key)
    (setq alist (symbol-value alist-symbol))
    (cond ((null alist) (set alist-symbol elem))
	  ((anot-head-p alist key) (set alist-symbol (nconc elem alist)))
	  (value (setcar alist (car elem)) alist)
	  (t alist))))


(defun adelete (alist-symbol key)
  "Delete a key-value pair from the alist.
Alist is referenced by ALIST-SYMBOL and the key-value pair to remove
is pair matching KEY.  Returns the altered alist."
  (asort alist-symbol key)
  (lexical-let ((alist (symbol-value alist-symbol)))
    (cond ((null alist) nil)
	  ((anot-head-p alist key) alist)
	  (t (set alist-symbol (cdr alist))))))


(defun aget (alist key &optional keynil-p)
  "Return the value in ALIST that is associated with KEY.
Optional KEYNIL-P describes what to do if the value associated with
KEY is nil.  If KEYNIL-P is not supplied or is nil, and the value is
nil, then KEY is returned.  If KEYNIL-P is non-nil, then nil would be
returned.

If no key-value pair matching KEY could be found in ALIST, or ALIST is
nil then nil is returned.  ALIST is not altered."
  (let ((copy (copy-alist alist)))
    (cond ((null alist) nil)
	  ((progn (asort 'copy key)
		  (anot-head-p copy key)) nil)
	  ((cdr (car copy)))
	  (keynil-p nil)
	  ((car (car copy)))
	  (t nil))))


(defun amake (alist-symbol keylist &optional valuelist)
  "Make an association list.
The association list is attached to the alist referenced by
ALIST-SYMBOL.  Each element in the KEYLIST becomes a key and is
associated with the value in VALUELIST with the same index.  If
VALUELIST is not supplied or is nil, then each key in KEYLIST is
associated with nil.

KEYLIST and VALUELIST should have the same number of elements, but
this isn't enforced.  If VALUELIST is smaller than KEYLIST, remaining
keys are associated with nil.  If VALUELIST is larger than KEYLIST,
extra values are ignored.  Returns the created alist."
  (lexical-let ((keycar (car keylist))
		(keycdr (cdr keylist))
		(valcar (car valuelist))
		(valcdr (cdr valuelist)))
    (cond ((null keycdr)
	   (aput alist-symbol keycar valcar))
	  (t
	   (amake alist-symbol keycdr valcdr)
	   (aput alist-symbol keycar valcar))))
  (symbol-value alist-symbol))

(provide 'assoc)

;;; assoc.el ends here
