;;; avl-tree.el --- balanced binary trees, AVL-trees

;; Copyright (C) 1995, 2007-2012  Free Software Foundation, Inc.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;         Inge Wallin <inge@lysator.liu.se>
;;         Thomas Bellman <bellman@lysator.liu.se>
;;         Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: FSF
;; Created: 10 May 1991
;; Keywords: extensions, data structures, AVL, tree

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

;; An AVL tree is a self-balancing binary tree. As such, inserting,
;; deleting, and retrieving data from an AVL tree containing n elements
;; is O(log n). It is somewhat more rigidly balanced than other
;; self-balancing binary trees (such as red-black trees and AA trees),
;; making insertion slightly slower, deletion somewhat slower, and
;; retrieval somewhat faster (the asymptotic scaling is of course the
;; same for all types). Thus it may be a good choice when the tree will
;; be relatively static, i.e. data will be retrieved more often than
;; they are modified.
;;
;; Internally, a tree consists of two elements, the root node and the
;; comparison function. The actual tree has a dummy node as its root
;; with the real root in the left pointer, which allows the root node to
;; be treated on a par with all other nodes.
;;
;; Each node of the tree consists of one data element, one left
;; sub-tree, one right sub-tree, and a balance count. The latter is the
;; difference in depth of the left and right sub-trees.
;;
;; The functions with names of the form "avl-tree--" are intended for
;; internal use only.

;;; Code:

(eval-when-compile (require 'cl))



;; ================================================================
;;; Internal functions and macros for use in the AVL tree package


;; ----------------------------------------------------------------
;; Functions and macros handling an AVL tree.

(defstruct (avl-tree-
            ;; A tagged list is the pre-defstruct representation.
            ;; (:type list)
            :named
            (:constructor nil)
            (:constructor avl-tree--create (cmpfun))
            (:predicate avl-tree-p)
            (:copier nil))
  (dummyroot (avl-tree--node-create nil nil nil 0))
  cmpfun)

(defmacro avl-tree--root (tree)
  ;; Return the root node for an AVL tree.  INTERNAL USE ONLY.
  `(avl-tree--node-left (avl-tree--dummyroot ,tree)))

(defsetf avl-tree--root (tree) (node)
  `(setf (avl-tree--node-left (avl-tree--dummyroot ,tree)) ,node))



;; ----------------------------------------------------------------
;; Functions and macros handling an AVL tree node.

(defstruct (avl-tree--node
            ;; We force a representation without tag so it matches the
            ;; pre-defstruct representation. Also we use the underlying
            ;; representation in the implementation of
            ;; avl-tree--node-branch.
            (:type vector)
            (:constructor nil)
            (:constructor avl-tree--node-create (left right data balance))
            (:copier nil))
  left right data balance)


(defalias 'avl-tree--node-branch 'aref
  ;; This implementation is efficient but breaks the defstruct
  ;; abstraction.  An alternative could be (funcall (aref [avl-tree-left
  ;; avl-tree-right avl-tree-data] branch) node)
  "Get value of a branch of a node.
NODE is the node, and BRANCH is the branch.
0 for left pointer, 1 for right pointer and 2 for the data.")


;; The funcall/aref trick wouldn't work for the setf method, unless we
;; tried to access the underlying setter function, but this wouldn't be
;; portable either.
(defsetf avl-tree--node-branch aset)



;; ----------------------------------------------------------------
;; Convenience macros

(defmacro avl-tree--switch-dir (dir)
  "Return opposite direction to DIR (0 = left, 1 = right)."
  `(- 1 ,dir))

(defmacro avl-tree--dir-to-sign (dir)
  "Convert direction (0,1) to sign factor (-1,+1)."
  `(1- (* 2 ,dir)))

(defmacro avl-tree--sign-to-dir (dir)
  "Convert sign factor (-x,+x) to direction (0,1)."
  `(if (< ,dir 0) 0 1))


;; ----------------------------------------------------------------
;;                          Deleting data

(defun avl-tree--del-balance (node branch dir)
  "Rebalance a tree after deleting a node.
The deletion was done from the left (DIR=0) or right (DIR=1) sub-tree of the
left (BRANCH=0) or right (BRANCH=1) child of NODE.
Return t if the height of the tree has shrunk."
  ;; (or is it vice-versa for BRANCH?)
  (let ((br (avl-tree--node-branch node branch))
	;; opposite direction: 0,1 -> 1,0
	(opp (avl-tree--switch-dir dir))
	;; direction 0,1 -> sign factor -1,+1
	(sgn (avl-tree--dir-to-sign dir))
        p1 b1 p2 b2)
    (cond
     ((> (* sgn (avl-tree--node-balance br)) 0)
      (setf (avl-tree--node-balance br) 0)
      t)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) (- sgn))
      nil)

     (t
      ;; Rebalance.
      (setq p1 (avl-tree--node-branch br opp)
            b1 (avl-tree--node-balance p1))
      (if (<= (* sgn b1) 0)
          ;; Single rotation.
          (progn
            (setf (avl-tree--node-branch br opp)
		    (avl-tree--node-branch p1 dir)
		  (avl-tree--node-branch p1 dir) br
		  (avl-tree--node-branch node branch) p1)
            (if (= 0 b1)
                (progn
                  (setf (avl-tree--node-balance br) (- sgn)
			(avl-tree--node-balance p1) sgn)
                  nil)  ; height hasn't changed
              (setf (avl-tree--node-balance br) 0)
              (setf (avl-tree--node-balance p1) 0)
              t))  ; height has changed

        ;; Double rotation.
        (setf p2 (avl-tree--node-branch p1 dir)
              b2 (avl-tree--node-balance p2)
	      (avl-tree--node-branch p1 dir)
	        (avl-tree--node-branch p2 opp)
	      (avl-tree--node-branch p2 opp) p1
	      (avl-tree--node-branch br opp)
	        (avl-tree--node-branch p2 dir)
	      (avl-tree--node-branch p2 dir) br
	      (avl-tree--node-balance br)
	        (if (< (* sgn b2) 0) sgn 0)
	      (avl-tree--node-balance p1)
	        (if (> (* sgn b2) 0) (- sgn) 0)
	      (avl-tree--node-branch node branch) p2
	      (avl-tree--node-balance p2) 0)
        t)))))

(defun avl-tree--do-del-internal (node branch q)
  (let ((br (avl-tree--node-branch node branch)))
    (if (avl-tree--node-right br)
        (if (avl-tree--do-del-internal br 1 q)
            (avl-tree--del-balance node branch 1))
      (setf (avl-tree--node-data q) (avl-tree--node-data br)
	    (avl-tree--node-branch node branch)
              (avl-tree--node-left br))
      t)))

(defun avl-tree--do-delete (cmpfun root branch data test nilflag)
  "Delete DATA from BRANCH of node ROOT.
\(See `avl-tree-delete' for TEST and NILFLAG).

Return cons cell (SHRUNK . DATA), where SHRUNK is t if the
height of the tree has shrunk and nil otherwise, and DATA is
the related data."
  (let ((br (avl-tree--node-branch root branch)))
    (cond
     ;; DATA not in tree.
     ((null br)
      (cons nil nilflag))

     ((funcall cmpfun data (avl-tree--node-data br))
      (let ((ret (avl-tree--do-delete cmpfun br 0 data test nilflag)))
	(cons (if (car ret) (avl-tree--del-balance root branch 0))
	      (cdr ret))))

     ((funcall cmpfun (avl-tree--node-data br) data)
      (let ((ret (avl-tree--do-delete cmpfun br 1 data test nilflag)))
	(cons (if (car ret) (avl-tree--del-balance root branch 1))
	      (cdr ret))))

     (t  ; Found it.
      ;; if it fails TEST, do nothing
      (if (and test (not (funcall test (avl-tree--node-data br))))
	  (cons nil nilflag)
	(cond
	 ((null (avl-tree--node-right br))
	  (setf (avl-tree--node-branch root branch)
		(avl-tree--node-left br))
	  (cons t (avl-tree--node-data br)))

	 ((null (avl-tree--node-left br))
	  (setf (avl-tree--node-branch root branch)
		(avl-tree--node-right br))
	  (cons t (avl-tree--node-data br)))

	 (t
	  (if (avl-tree--do-del-internal br 0 br)
	      (cons (avl-tree--del-balance root branch 0)
		    (avl-tree--node-data br))
	    (cons nil (avl-tree--node-data br))))
	 ))))))



;; ----------------------------------------------------------------
;;                           Entering data

(defun avl-tree--enter-balance (node branch dir)
  "Rebalance tree after an insertion
into the left (DIR=0) or right (DIR=1) sub-tree of the
left (BRANCH=0) or right (BRANCH=1) child of NODE.
Return t if the height of the tree has grown."
  (let ((br (avl-tree--node-branch node branch))
	;; opposite direction: 0,1 -> 1,0
	(opp (avl-tree--switch-dir dir))
	;; direction 0,1 -> sign factor -1,+1
	(sgn (avl-tree--dir-to-sign dir))
        p1 p2 b2 result)
    (cond
     ((< (* sgn (avl-tree--node-balance br)) 0)
      (setf (avl-tree--node-balance br) 0)
      nil)

     ((= (avl-tree--node-balance br) 0)
      (setf (avl-tree--node-balance br) sgn)
      t)

     (t
      ;; Tree has grown => Rebalance.
      (setq p1 (avl-tree--node-branch br dir))
      (if (> (* sgn (avl-tree--node-balance p1)) 0)
          ;; Single rotation.
          (progn
            (setf (avl-tree--node-branch br dir)
		  (avl-tree--node-branch p1 opp))
            (setf (avl-tree--node-branch p1 opp) br)
            (setf (avl-tree--node-balance br) 0)
            (setf (avl-tree--node-branch node branch) p1))

        ;; Double rotation.
        (setf p2 (avl-tree--node-branch p1 opp)
	      b2 (avl-tree--node-balance p2)
	      (avl-tree--node-branch p1 opp)
	        (avl-tree--node-branch p2 dir)
	      (avl-tree--node-branch p2 dir) p1
	      (avl-tree--node-branch br dir)
	        (avl-tree--node-branch p2 opp)
	      (avl-tree--node-branch p2 opp) br
	      (avl-tree--node-balance br)
	        (if (> (* sgn b2) 0) (- sgn) 0)
	      (avl-tree--node-balance p1)
	        (if (< (* sgn b2) 0) sgn 0)
                (avl-tree--node-branch node branch) p2))
      (setf (avl-tree--node-balance
             (avl-tree--node-branch node branch)) 0)
      nil))))

(defun avl-tree--do-enter (cmpfun root branch data &optional updatefun)
  "Enter DATA in BRANCH of ROOT node.
\(See `avl-tree-enter' for UPDATEFUN).

Return cons cell (GREW . DATA), where GREW is t if height
of tree ROOT has grown and nil otherwise, and DATA is the
inserted data."
  (let ((br (avl-tree--node-branch root branch)))
    (cond
     ((null br)
      ;; Data not in tree, insert it.
      (setf (avl-tree--node-branch root branch)
            (avl-tree--node-create nil nil data 0))
      (cons t data))

     ((funcall cmpfun data (avl-tree--node-data br))
      (let ((ret (avl-tree--do-enter cmpfun br 0 data updatefun)))
	(cons (and (car ret) (avl-tree--enter-balance root branch 0))
	      (cdr ret))))

     ((funcall cmpfun (avl-tree--node-data br) data)
      (let ((ret (avl-tree--do-enter cmpfun br 1 data updatefun)))
	(cons (and (car ret) (avl-tree--enter-balance root branch 1))
	      (cdr ret))))

     ;; Data already in tree, update it.
     (t
      (let ((newdata
	     (if updatefun
		 (funcall updatefun data (avl-tree--node-data br))
	       data)))
	(if (or (funcall cmpfun newdata data)
		(funcall cmpfun data newdata))
	    (error "avl-tree-enter:\
 updated data does not match existing data"))
	(setf (avl-tree--node-data br) newdata)
	(cons nil newdata))  ; return value
      ))))

(defun avl-tree--check (tree)
  "Check the tree's balance."
  (avl-tree--check-node (avl-tree--root tree)))
(defun avl-tree--check-node (node)
  (if (null node) 0
    (let ((dl (avl-tree--check-node (avl-tree--node-left node)))
	  (dr (avl-tree--check-node (avl-tree--node-right node))))
      (assert (= (- dr dl) (avl-tree--node-balance node)))
      (1+ (max dl dr)))))

;; ----------------------------------------------------------------


;;; INTERNAL USE ONLY
(defun avl-tree--mapc (map-function root dir)
  "Apply MAP-FUNCTION to all nodes in the tree starting with ROOT.
The function is applied in-order, either ascending (DIR=0) or
descending (DIR=1).

Note: MAP-FUNCTION is applied to the node and not to the data
itself."
  (let ((node root)
        (stack nil)
        (go-dir t))
    (push nil stack)
    (while node
      (if (and go-dir
               (avl-tree--node-branch node dir))
          ;; Do the DIR subtree first.
          (progn
            (push node stack)
            (setq node (avl-tree--node-branch node dir)))
        ;; Apply the function...
        (funcall map-function node)
        ;; and do the opposite subtree.
        (setq node (if (setq go-dir (avl-tree--node-branch
				     node (avl-tree--switch-dir dir)))
                       (avl-tree--node-branch
			node (avl-tree--switch-dir dir))
                     (pop stack)))))))

;;; INTERNAL USE ONLY
(defun avl-tree--do-copy (root)
  "Copy the AVL tree with ROOT as root.  Highly recursive."
  (if (null root)
      nil
    (avl-tree--node-create
     (avl-tree--do-copy (avl-tree--node-left root))
     (avl-tree--do-copy (avl-tree--node-right root))
     (avl-tree--node-data root)
     (avl-tree--node-balance root))))

(defstruct (avl-tree--stack
	    (:constructor nil)
	    (:constructor avl-tree--stack-create
			  (tree &optional reverse
				&aux
				(store
				 (if (avl-tree-empty tree)
				     nil
				   (list (avl-tree--root tree))))))
	    (:copier nil))
  reverse store)

(defalias 'avl-tree-stack-p 'avl-tree--stack-p
  "Return t if argument is an avl-tree-stack, nil otherwise.")

(defun avl-tree--stack-repopulate (stack)
  ;; Recursively push children of the node at the head of STACK onto the
  ;; front of the STACK, until a leaf is reached.
  (let ((node (car (avl-tree--stack-store stack)))
	(dir (if (avl-tree--stack-reverse stack) 1 0)))
    (when node  ; check for empty stack
      (while (setq node (avl-tree--node-branch node dir))
	(push node (avl-tree--stack-store stack))))))


;; ================================================================
;;; The public functions which operate on AVL trees.

;; define public alias for constructors so that we can set docstring
(defalias 'avl-tree-create 'avl-tree--create
  "Create an empty AVL tree.
COMPARE-FUNCTION is a function which takes two arguments, A and B,
and returns non-nil if A is less than B, and nil otherwise.")

(defalias 'avl-tree-compare-function 'avl-tree--cmpfun
  "Return the comparison function for the AVL tree TREE.

\(fn TREE)")

(defun avl-tree-empty (tree)
  "Return t if AVL tree TREE is empty, otherwise return nil."
  (null (avl-tree--root tree)))

(defun avl-tree-enter (tree data &optional updatefun)
  "Insert DATA into the AVL tree TREE.

If an element that matches DATA (according to the tree's
comparison function, see `avl-tree-create') already exists in
TREE, it will be replaced by DATA by default.

If UPDATEFUN is supplied and an element matching DATA already
exists in TREE, UPDATEFUN is called with two arguments: DATA, and
the matching element.  Its return value replaces the existing
element.  This value *must* itself match DATA (and hence the
pre-existing data), or an error will occur.

Returns the new data."
  (cdr (avl-tree--do-enter (avl-tree--cmpfun tree)
			   (avl-tree--dummyroot tree)
			   0 data updatefun)))

(defun avl-tree-delete (tree data &optional test nilflag)
  "Delete the element matching DATA from the AVL tree TREE.
Matching uses the comparison function previously specified in
`avl-tree-create' when TREE was created.

Returns the deleted element, or nil if no matching element was
found.

Optional argument NILFLAG specifies a value to return instead of
nil if nothing was deleted, so that this case can be
distinguished from the case of a successfully deleted null
element.

If supplied, TEST specifies a test that a matching element must
pass before it is deleted.  If a matching element is found, it is
passed as an argument to TEST, and is deleted only if the return
value is non-nil."
  (cdr (avl-tree--do-delete (avl-tree--cmpfun tree)
			    (avl-tree--dummyroot tree)
			    0 data test nilflag)))


(defun avl-tree-member (tree data &optional nilflag)
  "Return the element in the AVL tree TREE which matches DATA.
Matching uses the comparison function previously specified in
`avl-tree-create' when TREE was created.

If there is no such element in the tree, nil is
returned.  Optional argument NILFLAG specifies a value to return
instead of nil in this case.  This allows non-existent elements to
be distinguished from a null element.  (See also
`avl-tree-member-p', which does this for you.)"
  (let ((node (avl-tree--root tree))
	(compare-function (avl-tree--cmpfun tree)))
    (catch 'found
      (while node
	(cond
	 ((funcall compare-function data (avl-tree--node-data node))
	  (setq node (avl-tree--node-left node)))
	 ((funcall compare-function (avl-tree--node-data node) data)
	  (setq node (avl-tree--node-right node)))
	 (t (throw 'found (avl-tree--node-data node)))))
      nilflag)))


(defun avl-tree-member-p (tree data)
  "Return t if an element matching DATA exists in the AVL tree TREE.
Otherwise return nil.  Matching uses the comparison function
previously specified in `avl-tree-create' when TREE was created."
  (let ((flag '(nil)))
    (not (eq (avl-tree-member tree data flag) flag))))


(defun avl-tree-map (__map-function__ tree &optional reverse)
  "Modify all elements in the AVL tree TREE by applying FUNCTION.

Each element is replaced by the return value of FUNCTION applied
to that element.

FUNCTION is applied to the elements in ascending order, or
descending order if REVERSE is non-nil."
  (avl-tree--mapc
   (lambda (node)
     (setf (avl-tree--node-data node)
           (funcall __map-function__ (avl-tree--node-data node))))
   (avl-tree--root tree)
   (if reverse 1 0)))


(defun avl-tree-mapc (__map-function__ tree &optional reverse)
  "Apply FUNCTION to all elements in AVL tree TREE,
for side-effect only.

FUNCTION is applied to the elements in ascending order, or
descending order if REVERSE is non-nil."
  (avl-tree--mapc
   (lambda (node)
     (funcall __map-function__ (avl-tree--node-data node)))
   (avl-tree--root tree)
   (if reverse 1 0)))


(defun avl-tree-mapf
  (__map-function__ combinator tree &optional reverse)
  "Apply FUNCTION to all elements in AVL tree TREE,
and combine the results using COMBINATOR.

The FUNCTION is applied and the results are combined in ascending
order, or descending order if REVERSE is non-nil."
  (let (avl-tree-mapf--accumulate)
    (avl-tree--mapc
     (lambda (node)
       (setq avl-tree-mapf--accumulate
	     (funcall combinator
		      (funcall __map-function__
			       (avl-tree--node-data node))
		      avl-tree-mapf--accumulate)))
     (avl-tree--root tree)
     (if reverse 0 1))
    (nreverse avl-tree-mapf--accumulate)))


(defun avl-tree-mapcar (__map-function__ tree &optional reverse)
  "Apply FUNCTION to all elements in AVL tree TREE,
and make a list of the results.

The FUNCTION is applied and the list constructed in ascending
order, or descending order if REVERSE is non-nil.

Note that if you don't care about the order in which FUNCTION is
applied, just that the resulting list is in the correct order,
then

  (avl-tree-mapf function 'cons tree (not reverse))

is more efficient."
  (nreverse (avl-tree-mapf __map-function__ 'cons tree reverse)))


(defun avl-tree-first (tree)
  "Return the first element in TREE, or nil if TREE is empty."
  (let ((node (avl-tree--root tree)))
    (when node
      (while (avl-tree--node-left node)
        (setq node (avl-tree--node-left node)))
      (avl-tree--node-data node))))

(defun avl-tree-last (tree)
  "Return the last element in TREE, or nil if TREE is empty."
  (let ((node (avl-tree--root tree)))
    (when node
      (while (avl-tree--node-right node)
        (setq node (avl-tree--node-right node)))
      (avl-tree--node-data node))))

(defun avl-tree-copy (tree)
  "Return a copy of the AVL tree TREE."
  (let ((new-tree (avl-tree-create (avl-tree--cmpfun tree))))
    (setf (avl-tree--root new-tree) (avl-tree--do-copy (avl-tree--root tree)))
    new-tree))

(defun avl-tree-flatten (tree)
  "Return a sorted list containing all elements of TREE."
   (let ((treelist nil))
     (avl-tree--mapc
      (lambda (node) (push (avl-tree--node-data node) treelist))
      (avl-tree--root tree) 1)
     treelist))

(defun avl-tree-size (tree)
  "Return the number of elements in TREE."
  (let ((treesize 0))
    (avl-tree--mapc
     (lambda (data) (setq treesize (1+ treesize)))
     (avl-tree--root tree) 0)
    treesize))

(defun avl-tree-clear (tree)
  "Clear the AVL tree TREE."
  (setf (avl-tree--root tree) nil))


(defun avl-tree-stack (tree &optional reverse)
  "Return an object that behaves like a sorted stack
of all elements of TREE.

If REVERSE is non-nil, the stack is sorted in reverse order.
\(See also `avl-tree-stack-pop'\).

Note that any modification to TREE *immediately* invalidates all
avl-tree-stacks created before the modification (in particular,
calling `avl-tree-stack-pop' will give unpredictable results).

Operations on these objects are significantly more efficient than
constructing a real stack with `avl-tree-flatten' and using
standard stack functions.  As such, they can be useful in
implementing efficient algorithms of AVL trees.  However, in cases
where mapping functions `avl-tree-mapc', `avl-tree-mapcar' or
`avl-tree-mapf' would be sufficient, it is better to use one of
those instead."
  (let ((stack (avl-tree--stack-create tree reverse)))
    (avl-tree--stack-repopulate stack)
    stack))


(defun avl-tree-stack-pop (avl-tree-stack &optional nilflag)
  "Pop the first element from AVL-TREE-STACK.
\(See also `avl-tree-stack').

Returns nil if the stack is empty, or NILFLAG if specified.
\(The latter allows an empty stack to be distinguished from
a null element stored in the AVL tree.)"
  (let (node next)
    (if (not (setq node (pop (avl-tree--stack-store avl-tree-stack))))
	nilflag
      (when (setq next
		  (avl-tree--node-branch
		   node
		   (if (avl-tree--stack-reverse avl-tree-stack) 0 1)))
	(push next (avl-tree--stack-store avl-tree-stack))
	(avl-tree--stack-repopulate avl-tree-stack))
      (avl-tree--node-data node))))


(defun avl-tree-stack-first (avl-tree-stack &optional nilflag)
  "Return the first element of AVL-TREE-STACK, without removing it
from the stack.

Returns nil if the stack is empty, or NILFLAG if specified.
\(The latter allows an empty stack to be distinguished from
a null element stored in the AVL tree.)"
  (or (car (avl-tree--stack-store avl-tree-stack))
      nilflag))


(defun avl-tree-stack-empty-p (avl-tree-stack)
  "Return t if AVL-TREE-STACK is empty, nil otherwise."
  (null (avl-tree--stack-store avl-tree-stack)))


(provide 'avl-tree)

;;; avl-tree.el ends here
