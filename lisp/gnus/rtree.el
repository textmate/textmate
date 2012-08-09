;;; rtree.el --- functions for manipulating range trees

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

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

;; A "range tree" is a binary tree that stores ranges.  They are
;; similar to interval trees, but do not allow overlapping intervals.

;; A range is an ordered list of number intervals, like this:

;; ((10 . 25) 56 78 (98 . 201))

;; Common operations, like lookup, deletion and insertion are O(n) in
;; a range, but an rtree is O(log n) in all these operations.
;; Transformation between a range and an rtree is O(n).

;; The rtrees are quite simple.  The structure of each node is

;; (cons (cons low high) (cons left right))

;; That is, they are three cons cells, where the car of the top cell
;; is the actual range, and the cdr has the left and right child.  The
;; rtrees aren't automatically balanced, but are balanced when
;; created, and can be rebalanced when deemed necessary.

;;; Code:

(eval-when-compile
  (require 'cl))

(defmacro rtree-make-node ()
  `(list (list nil) nil))

(defmacro rtree-set-left (node left)
  `(setcar (cdr ,node) ,left))

(defmacro rtree-set-right (node right)
  `(setcdr (cdr ,node) ,right))

(defmacro rtree-set-range (node range)
  `(setcar ,node ,range))

(defmacro rtree-low (node)
  `(caar ,node))

(defmacro rtree-high (node)
  `(cdar ,node))

(defmacro rtree-set-low (node number)
  `(setcar (car ,node) ,number))

(defmacro rtree-set-high (node number)
  `(setcdr (car ,node) ,number))

(defmacro rtree-left (node)
  `(cadr ,node))

(defmacro rtree-right (node)
  `(cddr ,node))

(defmacro rtree-range (node)
  `(car ,node))

(defsubst rtree-normalise-range (range)
  (when (numberp range)
    (setq range (cons range range)))
  range)

(defun rtree-make (range)
  "Make an rtree from RANGE."
  ;; Normalize the range.
  (unless (listp (cdr-safe range))
    (setq range (list range)))
  (rtree-make-1 (cons nil range) (length range)))

(defun rtree-make-1 (range length)
  (let ((mid (/ length 2))
	(node (rtree-make-node)))
    (when (> mid 0)
      (rtree-set-left node (rtree-make-1 range mid)))
    (rtree-set-range node (rtree-normalise-range (cadr range)))
    (setcdr range (cddr range))
    (when (> (- length mid 1) 0)
      (rtree-set-right node (rtree-make-1 range (- length mid 1))))
    node))

(defun rtree-memq (tree number)
  "Return non-nil if NUMBER is present in TREE."
  (while (and tree
	      (not (and (>= number (rtree-low tree))
			(<= number (rtree-high tree)))))
    (setq tree
	  (if (< number (rtree-low tree))
	      (rtree-left tree)
	    (rtree-right tree))))
  tree)

(defun rtree-add (tree number)
  "Add NUMBER to TREE."
  (while tree
    (cond
     ;; It's already present, so we don't have to do anything.
     ((and (>= number (rtree-low tree))
	   (<= number (rtree-high tree)))
      (setq tree nil))
     ((< number (rtree-low tree))
      (cond
       ;; Extend the low range.
       ((= number (1- (rtree-low tree)))
	(rtree-set-low tree number)
	;; Check whether we need to merge this node with the child.
	(when (and (rtree-left tree)
		   (= (rtree-high (rtree-left tree)) (1- number)))
	  ;; Extend the range to the low from the child.
	  (rtree-set-low tree (rtree-low (rtree-left tree)))
	  ;; The child can't have a right child, so just transplant the
	  ;; child's left tree to our left tree.
	  (rtree-set-left tree (rtree-left (rtree-left tree))))
	(setq tree nil))
       ;; Descend further to the left.
       ((rtree-left tree)
	(setq tree (rtree-left tree)))
       ;; Add a new node.
       (t
	(let ((new-node (rtree-make-node)))
	  (rtree-set-low new-node number)
	  (rtree-set-high new-node number)
	  (rtree-set-left tree new-node)
	  (setq tree nil)))))
     (t
      (cond
       ;; Extend the high range.
       ((= number (1+ (rtree-high tree)))
	(rtree-set-high tree number)
	;; Check whether we need to merge this node with the child.
	(when (and (rtree-right tree)
		   (= (rtree-low (rtree-right tree)) (1+ number)))
	  ;; Extend the range to the high from the child.
	  (rtree-set-high tree (rtree-high (rtree-right tree)))
	  ;; The child can't have a left child, so just transplant the
	  ;; child's left right to our right tree.
	  (rtree-set-right tree (rtree-right (rtree-right tree))))
	(setq tree nil))
       ;; Descend further to the right.
       ((rtree-right tree)
	(setq tree (rtree-right tree)))
       ;; Add a new node.
       (t
	(let ((new-node (rtree-make-node)))
	  (rtree-set-low new-node number)
	  (rtree-set-high new-node number)
	  (rtree-set-right tree new-node)
	  (setq tree nil))))))))

(defun rtree-delq (tree number)
  "Remove NUMBER from TREE destructively.  Returns the new tree."
  (let ((result tree)
	prev)
    (while tree
      (cond
       ((< number (rtree-low tree))
	(setq prev tree
	      tree (rtree-left tree)))
       ((> number (rtree-high tree))
	(setq prev tree
	      tree (rtree-right tree)))
       ;; The number is in this node.
       (t
	(cond
	 ;; The only entry; delete the node.
	 ((= (rtree-low tree) (rtree-high tree))
	  (cond
	   ;; Two children.  Replace with successor value.
	   ((and (rtree-left tree) (rtree-right tree))
	    (let ((parent tree)
		  (successor (rtree-right tree)))
	      (while (rtree-left successor)
		(setq parent successor
		      successor (rtree-left successor)))
	      ;; We now have the leftmost child of our right child.
	      (rtree-set-range tree (rtree-range successor))
	      ;; Transplant the child (if any) to the parent.
	      (rtree-set-left parent (rtree-right successor))))
	   (t
	    (let ((rest (or (rtree-left tree)
			    (rtree-right tree))))
	      ;; One or zero children.  Remove the node.
	      (cond
	       ((null prev)
		(setq result rest))
	       ((eq (rtree-left prev) tree)
		(rtree-set-left prev rest))
	       (t
		(rtree-set-right prev rest)))))))
	 ;; The lowest in the range; just adjust.
	 ((= number (rtree-low tree))
	  (rtree-set-low tree (1+ number)))
	 ;; The highest in the range; just adjust.
	 ((= number (rtree-high tree))
	  (rtree-set-high tree (1- number)))
	 ;; We have to split this range.
	 (t
	  (let ((new-node (rtree-make-node)))
	    (rtree-set-low new-node (rtree-low tree))
	    (rtree-set-high new-node (1- number))
	    (rtree-set-low tree (1+ number))
	    (cond
	     ;; Two children; insert the new node as the predecessor
	     ;; node.
	     ((and (rtree-left tree) (rtree-right tree))
	      (let ((predecessor (rtree-left tree)))
		(while (rtree-right predecessor)
		  (setq predecessor (rtree-right predecessor)))
		(rtree-set-right predecessor new-node)))
	     ((rtree-left tree)
	      (rtree-set-right new-node tree)
	      (rtree-set-left new-node (rtree-left tree))
	      (rtree-set-left tree nil)
	      (cond
	       ((null prev)
		(setq result new-node))
	       ((eq (rtree-left prev) tree)
		(rtree-set-left prev new-node))
	       (t
		(rtree-set-right prev new-node))))
	     (t
	      (rtree-set-left tree new-node))))))
	(setq tree nil))))
    result))

(defun rtree-extract (tree)
  "Convert TREE to range form."
  (let (stack result)
    (while (or stack
	       tree)
      (if tree
	  (progn
	    (push tree stack)
	    (setq tree (rtree-right tree)))
	(setq tree (pop stack))
	(push (if (= (rtree-low tree)
		     (rtree-high tree))
		  (rtree-low tree)
		(rtree-range tree))
	      result)
	(setq tree (rtree-left tree))))
    result))

(defun rtree-length (tree)
  "Return the number of numbers stored in TREE."
  (if (null tree)
      0
    (+ (rtree-length (rtree-left tree))
       (1+ (- (rtree-high tree)
	      (rtree-low tree)))
       (rtree-length (rtree-right tree)))))

(provide 'rtree)

;;; rtree.el ends here
