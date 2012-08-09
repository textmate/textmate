;;; gnus-range.el --- range and sequence functions for Gnus

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

;;; List and range functions

(defsubst gnus-range-normalize (range)
  "Normalize RANGE.
If RANGE is a single range, return (RANGE). Otherwise, return RANGE."
  (if (listp (cdr-safe range)) range (list range)))

(defun gnus-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun gnus-copy-sequence (list)
  "Do a complete, total copy of a list."
  (let (out)
    (while (consp list)
      (if (consp (car list))
	  (push (gnus-copy-sequence (pop list)) out)
	(push (pop list) out)))
    (if list
	(nconc (nreverse out) list)
      (nreverse out))))

(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1))

(defun gnus-range-nconcat (&rest ranges)
  "Return a range comprising all the RANGES, which are pre-sorted.
RANGES will be destructively altered."
  (setq ranges (delete nil ranges))
  (let* ((result (gnus-range-normalize (pop ranges)))
	 (last (last result)))
    (dolist (range ranges)
      (setq range (gnus-range-normalize range))
      ;; Normalize the single-number case, so that we don't need to
      ;; special-case that so much.
      (when (numberp (car last))
	(setcar last (cons (car last) (car last))))
      (when (numberp (car range))
	(setcar range (cons (car range) (car range))))
      (if (= (1+ (cdar last)) (caar range))
	  (progn
	    (setcdr (car last) (cdar range))
	    (setcdr last (cdr range)))
	(setcdr last range)
	;; Denormalize back, since we couldn't join the ranges up.
	(when (= (caar range) (cdar range))
	  (setcar range (caar range)))
	(when (= (caar last) (cdar last))
	  (setcar last (caar last))))
      (setq last (last last)))
    (if (and (consp (car result))
	     (= (length result) 1))
	(car result)
      result)))

(defun gnus-range-difference (range1 range2)
  "Return the range of elements in RANGE1 that do not appear in RANGE2.
Both ranges must be in ascending order."
  (setq range1 (gnus-range-normalize range1))
  (setq range2 (gnus-range-normalize range2))
  (let* ((new-range (cons nil (copy-sequence range1)))
         (r new-range)
         (safe t))
    (while (cdr r)
      (let* ((r1 (cadr r))
             (r2 (car range2))
             (min1 (if (numberp r1) r1 (car r1)))
             (max1 (if (numberp r1) r1 (cdr r1)))
             (min2 (if (numberp r2) r2 (car r2)))
             (max2 (if (numberp r2) r2 (cdr r2))))

        (cond ((> min1 max1)
               ;; Invalid range: may result from overlap condition (below)
               ;; remove Invalid range
               (setcdr r (cddr r)))
              ((and (= min1 max1)
                    (listp r1))
               ;; Inefficient representation: may result from overlap condition (below)
               (setcar (cdr r) min1))
              ((not min2)
               ;; All done with range2
               (setq r nil))
              ((< max1 min2)
               ;; No overlap: range1 precedes range2
               (pop r))
              ((< max2 min1)
               ;; No overlap: range2 precedes range1
               (pop range2))
              ((and (<= min2 min1) (<= max1 max2))
               ;; Complete overlap: range1 removed
               (setcdr r (cddr r)))
              (t
               (setcdr r (nconc (list (cons min1 (1- min2)) (cons (1+ max2) max1)) (cddr r)))))))
    (cdr new-range)))



;;;###autoload
(defun gnus-sorted-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <.
The tail of LIST1 is not copied."
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq out (cons (car list1) out))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (nconc (nreverse out) list1)))

;;;###autoload
(defun gnus-sorted-ndifference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <.
LIST1 is modified."
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setcdr prev (cdr list1))
	     (setq list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (cdr top)))

;;;###autoload
(defun gnus-sorted-complement (list1 list2)
  "Return a list of elements that are in LIST1 or LIST2 but not both.
Both lists have to be sorted over <."
  (let (out)
    (if (or (null list1) (null list2))
	(or list1 list2)
      (while (and list1 list2)
	(cond ((= (car list1) (car list2))
	       (setq list1 (cdr list1)
		     list2 (cdr list2)))
	      ((< (car list1) (car list2))
	       (setq out (cons (car list1) out))
	       (setq list1 (cdr list1)))
	      (t
	       (setq out (cons (car list2) out))
	       (setq list2 (cdr list2)))))
      (nconc (nreverse out) (or list1 list2)))))

;;;###autoload
(defun gnus-intersection (list1 list2)
  (let ((result nil))
    (while list2
      (when (memq (car list2) list1)
	(setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result))

;;;###autoload
(defun gnus-sorted-intersection (list1 list2)
  "Return intersection of LIST1 and LIST2.
LIST1 and LIST2 have to be sorted over <."
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (nreverse out)))

;;;###autoload
(defun gnus-sorted-range-intersection (range1 range2)
  "Return intersection of RANGE1 and RANGE2.
RANGE1 and RANGE2 have to be sorted over <."
  (let* (out
         (min1 (car range1))
         (max1 (if (numberp min1)
                   (if (numberp (cdr range1))
                       (prog1 (cdr range1)
                         (setq range1 nil)) min1)
                 (prog1 (cdr min1)
                   (setq min1 (car min1)))))
         (min2 (car range2))
         (max2 (if (numberp min2)
                   (if (numberp (cdr range2))
                       (prog1 (cdr range2)
                         (setq range2 nil)) min2)
                 (prog1 (cdr min2)
                   (setq min2 (car min2))))))
    (setq range1 (cdr range1)
          range2 (cdr range2))
    (while (and min1 min2)
      (cond ((< max1 min2)              ; range1 precedes range2
             (setq range1 (cdr range1)
                   min1 nil))
            ((< max2 min1)              ; range2 precedes range1
             (setq range2 (cdr range2)
                   min2 nil))
            (t                     ; some sort of overlap is occurring
             (let ((min (max min1 min2))
                   (max (min max1 max2)))
               (setq out (if (= min max)
                             (cons min out)
                           (cons (cons min max) out))))
             (if (< max1 max2)          ; range1 ends before range2
                 (setq min1 nil)        ; incr range1
               (setq min2 nil))))       ; incr range2
      (unless min1
        (setq min1 (car range1)
              max1 (if (numberp min1) min1 (prog1 (cdr min1) (setq min1 (car min1))))
              range1 (cdr range1)))
      (unless min2
        (setq min2 (car range2)
              max2 (if (numberp min2) min2 (prog1 (cdr min2) (setq min2 (car min2))))
              range2 (cdr range2))))
    (cond ((cdr out)
        (nreverse out))
          ((numberp (car out))
           out)
          (t
           (car out)))))

;;;###autoload
(defalias 'gnus-set-sorted-intersection 'gnus-sorted-nintersection)

;;;###autoload
(defun gnus-sorted-nintersection (list1 list2)
  "Return intersection of LIST1 and LIST2 by modifying cdr pointers of LIST1.
LIST1 and LIST2 have to be sorted over <."
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setcdr prev (cdr list1))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (setcdr prev nil)
    (cdr top)))

;;;###autoload
(defun gnus-sorted-union (list1 list2)
  "Return union of LIST1 and LIST2.
LIST1 and LIST2 have to be sorted over <."
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)))
	    (t
	     (setq out (cons (car list2) out)
		   list2 (cdr list2)))))
    (while list1
      (setq out (cons (car list1) out)
	    list1 (cdr list1)))
    (while list2
      (setq out (cons (car list2) out)
	    list2 (cdr list2)))
    (nreverse out)))

;;;###autoload
(defun gnus-sorted-nunion (list1 list2)
  "Return union of LIST1 and LIST2 by modifying cdr pointers of LIST1.
LIST1 and LIST2 have to be sorted over <."
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)))
	    (t
	     (setcdr prev (list (car list2)))
	     (setq prev (cdr prev)
		   list2 (cdr list2))
	     (setcdr prev list1))))
    (while list2
      (setcdr prev (list (car list2)))
      (setq prev (cdr prev)
	    list2 (cdr list2)))
    (cdr top)))

(defun gnus-compress-sequence (numbers &optional always-list)
  "Convert sorted list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges."
  (let* ((first (car numbers))
	 (last (car numbers))
	 result)
    (if (null numbers)
	nil
      (if (not (listp (cdr numbers)))
	  numbers
	(while numbers
	  (cond ((= last (car numbers)) nil) ;Omit duplicated number
		((= (1+ last) (car numbers)) ;Still in sequence
		 (setq last (car numbers)))
		(t			;End of one sequence
		 (setq result
		       (cons (if (= first last) first
			       (cons first last))
			     result))
		 (setq first (car numbers))
		 (setq last  (car numbers))))
	  (setq numbers (cdr numbers)))
	(if (and (not always-list) (null result))
	    (if (= first last) (list first) (cons first last))
	  (nreverse (cons (if (= first last) first (cons first last))
			  result)))))))

(defalias 'gnus-uncompress-sequence 'gnus-uncompress-range)
(defun gnus-uncompress-range (ranges)
  "Expand a list of ranges into a list of numbers.
RANGES is either a single range on the form `(num . num)' or a list of
these ranges."
  (let (first last result)
    (cond
     ((null ranges)
      nil)
     ((not (listp (cdr ranges)))
      (setq first (car ranges))
      (setq last (cdr ranges))
      (while (<= first last)
	(setq result (cons first result))
	(setq first (1+ first)))
      (nreverse result))
     (t
      (while ranges
	(if (atom (car ranges))
	    (when (numberp (car ranges))
	      (setq result (cons (car ranges) result)))
	  (setq first (caar ranges))
	  (setq last  (cdar ranges))
	  (while (<= first last)
	    (setq result (cons first result))
	    (setq first (1+ first))))
	(setq ranges (cdr ranges)))
      (nreverse result)))))

(defun gnus-add-to-range (ranges list)
  "Return a list of ranges that has all articles from both RANGES and LIST.
Note: LIST has to be sorted over `<'."
  (if (not ranges)
      (gnus-compress-sequence list t)
    (setq list (copy-sequence list))
    (unless (listp (cdr ranges))
      (setq ranges (list ranges)))
    (let ((out ranges)
	  ilist lowest highest temp)
      (while (and ranges list)
	(setq ilist list)
	(setq lowest (or (and (atom (car ranges)) (car ranges))
			 (caar ranges)))
	(while (and list (cdr list) (< (cadr list) lowest))
	  (setq list (cdr list)))
	(when (< (car ilist) lowest)
	  (setq temp list)
	  (setq list (cdr list))
	  (setcdr temp nil)
	  (setq out (nconc (gnus-compress-sequence ilist t) out)))
	(setq highest (or (and (atom (car ranges)) (car ranges))
			  (cdar ranges)))
	(while (and list (<= (car list) highest))
	  (setq list (cdr list)))
	(setq ranges (cdr ranges)))
      (when list
	(setq out (nconc (gnus-compress-sequence list t) out)))
      (setq out (sort out (lambda (r1 r2)
			    (< (or (and (atom r1) r1) (car r1))
			       (or (and (atom r2) r2) (car r2))))))
      (setq ranges out)
      (while ranges
	(if (atom (car ranges))
	    (when (cdr ranges)
	      (if (atom (cadr ranges))
		  (when (= (1+ (car ranges)) (cadr ranges))
		    (setcar ranges (cons (car ranges)
					 (cadr ranges)))
		    (setcdr ranges (cddr ranges)))
		(when (= (1+ (car ranges)) (caadr ranges))
		  (setcar (cadr ranges) (car ranges))
		  (setcar ranges (cadr ranges))
		  (setcdr ranges (cddr ranges)))))
	  (when (cdr ranges)
	    (if (atom (cadr ranges))
		(when (= (1+ (cdar ranges)) (cadr ranges))
		  (setcdr (car ranges) (cadr ranges))
		  (setcdr ranges (cddr ranges)))
	      (when (= (1+ (cdar ranges)) (caadr ranges))
		(setcdr (car ranges) (cdadr ranges))
		(setcdr ranges (cddr ranges))))))
	(setq ranges (cdr ranges)))
      out)))

(defun gnus-remove-from-range (range1 range2)
  "Return a range that has all articles from RANGE2 removed from RANGE1.
The returned range is always a list.  RANGE2 can also be a unsorted
list of articles.  RANGE1 is modified by side effects, RANGE2 is not
modified."
  (if (or (null range1) (null range2))
      range1
    (let (out r1 r2 r1_min r1_max r2_min r2_max
	      (range2 (gnus-copy-sequence range2)))
      (setq range1 (if (listp (cdr range1)) range1 (list range1))
	    range2 (sort (if (listp (cdr range2)) range2 (list range2))
			 (lambda (e1 e2)
			   (< (if (consp e1) (car e1) e1)
			      (if (consp e2) (car e2) e2))))
	    r1 (car range1)
	    r2 (car range2)
	    r1_min (if (consp r1) (car r1) r1)
	    r1_max (if (consp r1) (cdr r1) r1)
	    r2_min (if (consp r2) (car r2) r2)
	    r2_max (if (consp r2) (cdr r2) r2))
      (while (and range1 range2)
	(cond ((< r2_max r1_min)	; r2 < r1
	       (pop range2)
	       (setq r2 (car range2)
		     r2_min (if (consp r2) (car r2) r2)
		     r2_max (if (consp r2) (cdr r2) r2)))
	      ((and (<= r2_min r1_min) (<= r1_max r2_max)) ; r2 overlap r1
	       (pop range1)
	       (setq r1 (car range1)
		     r1_min (if (consp r1) (car r1) r1)
		     r1_max (if (consp r1) (cdr r1) r1)))
	      ((and (<= r2_min r1_min) (<= r2_max r1_max)) ; r2 overlap min r1
	       (pop range2)
	       (setq r1_min (1+ r2_max)
		     r2 (car range2)
		     r2_min (if (consp r2) (car r2) r2)
		     r2_max (if (consp r2) (cdr r2) r2)))
	      ((and (<= r1_min r2_min) (<= r2_max r1_max)) ; r2 contained in r1
	       (if (eq r1_min (1- r2_min))
		   (push r1_min out)
		 (push (cons r1_min (1- r2_min)) out))
	       (pop range2)
	       (if (< r2_max r1_max)	; finished with r1?
		   (setq r1_min (1+ r2_max))
		 (pop range1)
		 (setq r1 (car range1)
		       r1_min (if (consp r1) (car r1) r1)
		       r1_max (if (consp r1) (cdr r1) r1)))
	       (setq r2 (car range2)
		     r2_min (if (consp r2) (car r2) r2)
		     r2_max (if (consp r2) (cdr r2) r2)))
	      ((and (<= r2_min r1_max) (<= r1_max r2_max)) ; r2 overlap max r1
	       (if (eq r1_min (1- r2_min))
		   (push r1_min out)
		 (push (cons r1_min (1- r2_min)) out))
	       (pop range1)
	       (setq r1 (car range1)
		     r1_min (if (consp r1) (car r1) r1)
		     r1_max (if (consp r1) (cdr r1) r1)))
	      ((< r1_max r2_min)	; r2 > r1
	       (pop range1)
	       (if (eq r1_min r1_max)
		   (push r1_min out)
		 (push (cons r1_min r1_max) out))
	       (setq r1 (car range1)
		     r1_min (if (consp r1) (car r1) r1)
		     r1_max (if (consp r1) (cdr r1) r1)))))
      (when r1
	(if (eq r1_min r1_max)
	    (push r1_min out)
	  (push (cons r1_min r1_max) out))
	(pop range1))
      (while range1
	(push (pop range1) out))
      (nreverse out))))

(defun gnus-member-of-range (number ranges)
  (if (not (listp (cdr ranges)))
      (and (>= number (car ranges))
	   (<= number (cdr ranges)))
    (let ((not-stop t))
      (while (and ranges
		  (if (numberp (car ranges))
		      (>= number (car ranges))
		    (>= number (caar ranges)))
		  not-stop)
	(when (if (numberp (car ranges))
		  (= number (car ranges))
		(and (>= number (caar ranges))
		     (<= number (cdar ranges))))
	  (setq not-stop nil))
	(setq ranges (cdr ranges)))
      (not not-stop))))

(defun gnus-list-range-intersection (list ranges)
  "Return a list of numbers in LIST that are members of RANGES.
LIST is a sorted list."
  (setq ranges (gnus-range-normalize ranges))
  (let (number result)
    (while (setq number (pop list))
      (while (and ranges
		  (if (numberp (car ranges))
		      (< (car ranges) number)
		    (< (cdar ranges) number)))
	(setq ranges (cdr ranges)))
      (when (and ranges
		 (if (numberp (car ranges))
		      (= (car ranges) number)
		   ;; (caar ranges) <= number <= (cdar ranges)
		   (>= number (caar ranges))))
	(push number result)))
    (nreverse result)))

(defalias 'gnus-inverse-list-range-intersection 'gnus-list-range-difference)

(defun gnus-list-range-difference (list ranges)
  "Return a list of numbers in LIST that are not members of RANGES.
LIST is a sorted list."
  (setq ranges (gnus-range-normalize ranges))
  (let (number result)
    (while (setq number (pop list))
      (while (and ranges
		  (if (numberp (car ranges))
		      (< (car ranges) number)
		    (< (cdar ranges) number)))
	(setq ranges (cdr ranges)))
      (when (or (not ranges)
		(if (numberp (car ranges))
		    (not (= (car ranges) number))
		  ;; not ((caar ranges) <= number <= (cdar ranges))
		  (< number (caar ranges))))
	(push number result)))
    (nreverse result)))

(defun gnus-range-length (range)
  "Return the length RANGE would have if uncompressed."
  (cond
   ((null range)
    0)
   ((not (listp (cdr range)))
    (- (cdr range) (car range) -1))
   (t
    (let ((sum 0))
      (dolist (x range sum)
	(setq sum
	      (+ sum (if (consp x) (- (cdr x) (car x) -1) 1))))))))

(defun gnus-sublist-p (list sublist)
  "Test whether all elements in SUBLIST are members of LIST."
  (let ((sublistp t))
    (while sublist
      (unless (memq (pop sublist) list)
	(setq sublistp nil
	      sublist nil)))
    sublistp))

(defun gnus-range-add (range1 range2)
  "Add RANGE2 to RANGE1 (nondestructively)."
  (unless (listp (cdr range1))
    (setq range1 (list range1)))
  (unless (listp (cdr range2))
    (setq range2 (list range2)))
  (let ((item1 (pop range1))
	(item2 (pop range2))
	range item selector)
    (while (or item1 item2)
      (setq selector
	    (cond
	     ((null item1) nil)
	     ((null item2) t)
	     ((and (numberp item1) (numberp item2)) (< item1 item2))
	     ((numberp item1) (< item1 (car item2)))
	     ((numberp item2) (< (car item1) item2))
	     (t (< (car item1) (car item2)))))
      (setq item
	    (or
	     (let ((tmp1 item) (tmp2 (if selector item1 item2)))
	       (cond
		((null tmp1) tmp2)
		((null tmp2) tmp1)
		((and (numberp tmp1) (numberp tmp2))
		 (cond
		  ((eq tmp1 tmp2) tmp1)
		  ((eq (1+ tmp1) tmp2) (cons tmp1 tmp2))
		  ((eq (1+ tmp2) tmp1) (cons tmp2 tmp1))
		  (t nil)))
		((numberp tmp1)
		 (cond
		  ((and (>= tmp1 (car tmp2)) (<= tmp1 (cdr tmp2))) tmp2)
		  ((eq (1+ tmp1) (car tmp2)) (cons tmp1 (cdr tmp2)))
		  ((eq (1- tmp1) (cdr tmp2)) (cons (car tmp2) tmp1))
		  (t nil)))
		((numberp tmp2)
		 (cond
		  ((and (>= tmp2 (car tmp1)) (<= tmp2 (cdr tmp1))) tmp1)
		  ((eq (1+ tmp2) (car tmp1)) (cons tmp2 (cdr tmp1)))
		  ((eq (1- tmp2) (cdr tmp1)) (cons (car tmp1) tmp2))
		  (t nil)))
		((< (1+ (cdr tmp1)) (car tmp2)) nil)
		((< (1+ (cdr tmp2)) (car tmp1)) nil)
		(t (cons (min (car tmp1) (car tmp2))
			 (max (cdr tmp1) (cdr tmp2))))))
	     (progn
	       (if item (push item range))
	       (if selector item1 item2))))
      (if selector
	  (setq item1 (pop range1))
	(setq item2 (pop range2))))
    (if item (push item range))
    (reverse range)))

;;;###autoload
(defun gnus-add-to-sorted-list (list num)
  "Add NUM into sorted LIST by side effect."
  (let* ((top (cons nil list))
	 (prev top))
    (while (and list (< (car list) num))
      (setq prev list
	    list (cdr list)))
    (unless (eq (car list) num)
      (setcdr prev (cons num list)))
    (cdr top)))

(defun gnus-range-map (func range)
  "Apply FUNC to each value contained by RANGE."
  (setq range (gnus-range-normalize range))
  (while range
    (let ((span (pop range)))
      (if (numberp span)
          (funcall func span)
        (let ((first (car span))
              (last (cdr span)))
          (while (<= first last)
            (funcall func first)
            (setq first (1+ first))))))))

(provide 'gnus-range)

;;; gnus-range.el ends here
