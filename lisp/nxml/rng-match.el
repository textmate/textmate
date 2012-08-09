;;; rng-match.el --- matching of RELAX NG patterns against XML events

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

;; This uses the algorithm described in
;;   http://www.thaiopensource.com/relaxng/derivative.html
;;
;; The schema to be used is contained in the variable
;; rng-current-schema.  It has the form described in the file
;; rng-pttrn.el.
;;
;;; Code:

(require 'rng-pttrn)
(require 'rng-util)
(require 'rng-dt)

(defvar rng-not-allowed-ipattern nil)
(defvar rng-empty-ipattern nil)
(defvar rng-text-ipattern nil)

(defvar rng-compile-table nil)

(defvar rng-being-compiled nil
  "Contains a list of ref patterns currently being compiled.
Used to detect invalid recursive references.")

(defvar rng-ipattern-table nil)

(defvar rng-last-ipattern-index nil)

(defvar rng-match-state nil
  "An ipattern representing the current state of validation.")

;;; Inline functions

(defsubst rng-update-match-state (new-state)
  (if (and (eq new-state rng-not-allowed-ipattern)
	   (not (eq rng-match-state rng-not-allowed-ipattern)))
      nil
    (setq rng-match-state new-state)
    t))

;;; Interned patterns

(eval-when-compile
  (defun rng-ipattern-slot-accessor-name (slot-name)
    (intern (concat "rng-ipattern-get-"
		    (symbol-name slot-name))))

  (defun rng-ipattern-slot-setter-name (slot-name)
    (intern (concat "rng-ipattern-set-"
		    (symbol-name slot-name)))))

(defmacro rng-ipattern-defslot (slot-name index)
  `(progn
     (defsubst ,(rng-ipattern-slot-accessor-name slot-name) (ipattern)
       (aref ipattern ,index))
     (defsubst ,(rng-ipattern-slot-setter-name slot-name) (ipattern value)
       (aset ipattern ,index value))))

(rng-ipattern-defslot type 0)
(rng-ipattern-defslot index 1)
(rng-ipattern-defslot name-class 2)
(rng-ipattern-defslot datatype 2)
(rng-ipattern-defslot after 2)
(rng-ipattern-defslot child 3)
(rng-ipattern-defslot value-object 3)
(rng-ipattern-defslot nullable 4)
(rng-ipattern-defslot memo-text-typed 5)
(rng-ipattern-defslot memo-map-start-tag-open-deriv 6)
(rng-ipattern-defslot memo-map-start-attribute-deriv 7)
(rng-ipattern-defslot memo-start-tag-close-deriv 8)
(rng-ipattern-defslot memo-text-only-deriv 9)
(rng-ipattern-defslot memo-mixed-text-deriv 10)
(rng-ipattern-defslot memo-map-data-deriv 11)
(rng-ipattern-defslot memo-end-tag-deriv 12)

(defconst rng-memo-map-alist-max 10)

(defsubst rng-memo-map-get (key mm)
  "Return the value associated with KEY in memo-map MM."
  (let ((found (assoc key mm)))
    (if found
	(cdr found)
      (and mm
	   (let ((head (car mm)))
	     (and (hash-table-p head)
		  (gethash key head)))))))

(defun rng-memo-map-add (key value mm &optional weakness)
  "Associate KEY with VALUE in memo-map MM and return the new memo-map.
The new memo-map may or may not be a different object from MM.

Alists are better for small maps.  Hash tables are better for large
maps.  A memo-map therefore starts off as an alist and switches to a
hash table for large memo-maps.  A memo-map is always a list.  An empty
memo-map is represented by nil.  A large memo-map is represented by a
list containing just a hash-table.  A small memo map is represented by
a list whose cdr is an alist and whose car is the number of entries in
the alist.  The complete memo-map can be passed to `assoc' without
problems: assoc ignores any members that are not cons cells.  There is
therefore minimal overhead in successful lookups on small lists
\(which is the most common case)."
  (if (null mm)
      (list 1 (cons key value))
    (let ((head (car mm)))
      (cond ((hash-table-p head)
	     (puthash key value head)
	     mm)
	    ((>= head rng-memo-map-alist-max)
	     (let ((ht (make-hash-table :test 'equal
					:weakness weakness
					:size (* 2 rng-memo-map-alist-max))))
	       (setq mm (cdr mm))
	       (while mm
		 (setq head (car mm))
		 (puthash (car head) (cdr head) ht)
		 (setq mm (cdr mm)))
	       (cons ht nil)))
	    (t (cons (1+ head)
		     (cons (cons key value)
			   (cdr mm))))))))

(defsubst rng-make-ipattern (type index name-class child nullable)
  (vector type index name-class child nullable
	  ;; 5 memo-text-typed
	  'unknown
	  ;; 6 memo-map-start-tag-open-deriv
	  nil
	  ;; 7 memo-map-start-attribute-deriv
	  nil
	  ;; 8 memo-start-tag-close-deriv
	  nil
	  ;; 9 memo-text-only-deriv
	  nil
	  ;; 10 memo-mixed-text-deriv
	  nil
	  ;; 11 memo-map-data-deriv
	  nil
	  ;; 12 memo-end-tag-deriv
	  nil))

(defun rng-ipattern-maybe-init ()
  (unless rng-ipattern-table
    (setq rng-ipattern-table (make-hash-table :test 'equal))
    (setq rng-last-ipattern-index -1)))

(defun rng-ipattern-clear ()
  (when rng-ipattern-table
    (clrhash rng-ipattern-table))
  (setq rng-last-ipattern-index -1))

(defsubst rng-gen-ipattern-index ()
  (setq rng-last-ipattern-index (1+ rng-last-ipattern-index)))

(defun rng-put-ipattern (key type name-class child nullable)
  (let ((ipattern
	 (rng-make-ipattern type
			    (rng-gen-ipattern-index)
			    name-class
			    child
			    nullable)))
    (puthash key ipattern rng-ipattern-table)
    ipattern))

(defun rng-get-ipattern (key)
  (gethash key rng-ipattern-table))

(or rng-not-allowed-ipattern
    (setq rng-not-allowed-ipattern
	  (rng-make-ipattern 'not-allowed -3 nil nil nil)))

(or rng-empty-ipattern
    (setq rng-empty-ipattern
	  (rng-make-ipattern 'empty -2 nil nil t)))

(or rng-text-ipattern
    (setq rng-text-ipattern
	  (rng-make-ipattern 'text -1 nil nil t)))

(defconst rng-const-ipatterns
  (list rng-not-allowed-ipattern
	rng-empty-ipattern
	rng-text-ipattern))

(defun rng-intern-after (child after)
  (if (eq child rng-not-allowed-ipattern)
      rng-not-allowed-ipattern
    (let ((key (list 'after
		     (rng-ipattern-get-index child)
		     (rng-ipattern-get-index after))))
      (or (rng-get-ipattern key)
	  (rng-put-ipattern key
			    'after
			    after
			    child
			    nil)))))

(defun rng-intern-attribute (name-class ipattern)
  (if (eq ipattern rng-not-allowed-ipattern)
      rng-not-allowed-ipattern
    (let ((key (list 'attribute
		     name-class
		     (rng-ipattern-get-index ipattern))))
      (or (rng-get-ipattern key)
	  (rng-put-ipattern key
			    'attribute
			    name-class
			    ipattern
			    nil)))))

(defun rng-intern-data (dt matches-anything)
  (let ((key (list 'data dt)))
    (or (rng-get-ipattern key)
	(let ((ipattern (rng-put-ipattern key
					  'data
					  dt
					  nil
					  matches-anything)))
	  (rng-ipattern-set-memo-text-typed ipattern
					    (not matches-anything))
	  ipattern))))

(defun rng-intern-data-except (dt ipattern)
  (let ((key (list 'data-except dt ipattern)))
    (or (rng-get-ipattern key)
	(rng-put-ipattern key
			  'data-except
			  dt
			  ipattern
			  nil))))

(defun rng-intern-value (dt obj)
  (let ((key (list 'value dt obj)))
    (or (rng-get-ipattern key)
	(rng-put-ipattern key
			  'value
			  dt
			  obj
			  nil))))

(defun rng-intern-one-or-more (ipattern)
  (or (rng-intern-one-or-more-shortcut ipattern)
      (let ((key (cons 'one-or-more
		       (list (rng-ipattern-get-index ipattern)))))
	(or (rng-get-ipattern key)
	    (rng-put-ipattern key
			      'one-or-more
			      nil
			      ipattern
			      (rng-ipattern-get-nullable ipattern))))))

(defun rng-intern-one-or-more-shortcut (ipattern)
  (cond ((eq ipattern rng-not-allowed-ipattern)
	 rng-not-allowed-ipattern)
	((eq ipattern rng-empty-ipattern)
	 rng-empty-ipattern)
	((eq (rng-ipattern-get-type ipattern) 'one-or-more)
	 ipattern)
	(t nil)))

(defun rng-intern-list (ipattern)
  (if (eq ipattern rng-not-allowed-ipattern)
      rng-not-allowed-ipattern
    (let ((key (cons 'list
		     (list (rng-ipattern-get-index ipattern)))))
      (or (rng-get-ipattern key)
	  (rng-put-ipattern key
			    'list
			    nil
			    ipattern
			    nil)))))

(defun rng-intern-group (ipatterns)
  "Return an ipattern for the list of group members in IPATTERNS."
  (or (rng-intern-group-shortcut ipatterns)
      (let* ((tem (rng-normalize-group-list ipatterns))
	     (normalized (cdr tem)))
	(or (rng-intern-group-shortcut normalized)
	    (let ((key (cons 'group
			     (mapcar 'rng-ipattern-get-index normalized))))
	      (or (rng-get-ipattern key)
		  (rng-put-ipattern key
				    'group
				    nil
				    normalized
				    (car tem))))))))

(defun rng-intern-group-shortcut (ipatterns)
  "Try to shortcut interning a group list.
If successful, return the interned pattern.  Otherwise return nil."
  (while (and ipatterns
	      (eq (car ipatterns) rng-empty-ipattern))
    (setq ipatterns (cdr ipatterns)))
  (if ipatterns
      (let ((ret (car ipatterns)))
	(if (eq ret rng-not-allowed-ipattern)
	    rng-not-allowed-ipattern
	  (setq ipatterns (cdr ipatterns))
	  (while (and ipatterns ret)
	    (let ((tem (car ipatterns)))
	      (cond ((eq tem rng-not-allowed-ipattern)
		     (setq ret tem)
		     (setq ipatterns nil))
		    ((eq tem rng-empty-ipattern)
		     (setq ipatterns (cdr ipatterns)))
		    (t
		     ;; Stop here rather than continuing
		     ;; looking for not-allowed patterns.
		     ;; We do a complete scan elsewhere.
		     (setq ret nil)))))
	  ret))
    rng-empty-ipattern))

(defun rng-normalize-group-list (ipatterns)
  "Normalize a list containing members of a group.
Expands nested groups, removes empty members, handles notAllowed.
Returns a pair whose car says whether the list is nullable and whose
cdr is the normalized list."
  (let ((nullable t)
	(result nil)
	member)
    (while ipatterns
      (setq member (car ipatterns))
      (setq ipatterns (cdr ipatterns))
      (when nullable
	(setq nullable (rng-ipattern-get-nullable member)))
      (cond ((eq (rng-ipattern-get-type member) 'group)
	     (setq result
		   (nconc (reverse (rng-ipattern-get-child member))
			  result)))
	    ((eq member rng-not-allowed-ipattern)
	     (setq result (list rng-not-allowed-ipattern))
	     (setq ipatterns nil))
	    ((not (eq member rng-empty-ipattern))
	     (setq result (cons member result)))))
    (cons nullable (nreverse result))))

(defun rng-intern-interleave (ipatterns)
  (or (rng-intern-group-shortcut ipatterns)
      (let* ((tem (rng-normalize-interleave-list ipatterns))
	     (normalized (cdr tem)))
	(or (rng-intern-group-shortcut normalized)
	    (let ((key (cons 'interleave
			     (mapcar 'rng-ipattern-get-index normalized))))
	      (or (rng-get-ipattern key)
		  (rng-put-ipattern key
				    'interleave
				    nil
				    normalized
				    (car tem))))))))

(defun rng-normalize-interleave-list (ipatterns)
  "Normalize a list containing members of an interleave.
Expands nested groups, removes empty members, handles notAllowed.
Returns a pair whose car says whether the list is nullable and whose
cdr is the normalized list."
  (let ((nullable t)
	(result nil)
	member)
    (while ipatterns
      (setq member (car ipatterns))
      (setq ipatterns (cdr ipatterns))
      (when nullable
	(setq nullable (rng-ipattern-get-nullable member)))
      (cond ((eq (rng-ipattern-get-type member) 'interleave)
	     (setq result
		   (append (rng-ipattern-get-child member)
			    result)))
	    ((eq member rng-not-allowed-ipattern)
	     (setq result (list rng-not-allowed-ipattern))
	     (setq ipatterns nil))
	    ((not (eq member rng-empty-ipattern))
	     (setq result (cons member result)))))
    (cons nullable (sort result 'rng-compare-ipattern))))

;; Would be cleaner if this didn't modify IPATTERNS.

(defun rng-intern-choice (ipatterns)
  "Return a choice ipattern for the list of choices in IPATTERNS.
May alter IPATTERNS."
  (or (rng-intern-choice-shortcut ipatterns)
      (let* ((tem (rng-normalize-choice-list ipatterns))
	     (normalized (cdr tem)))
	(or (rng-intern-choice-shortcut normalized)
	    (rng-intern-choice1 normalized (car tem))))))

(defun rng-intern-optional (ipattern)
  (cond ((rng-ipattern-get-nullable ipattern) ipattern)
	((eq ipattern rng-not-allowed-ipattern) rng-empty-ipattern)
	(t (rng-intern-choice1
	    ;; This is sorted since the empty pattern
	    ;; is before everything except not allowed.
	    ;; It cannot have a duplicate empty pattern,
	    ;; since it is not nullable.
	    (cons rng-empty-ipattern
		  (if (eq (rng-ipattern-get-type ipattern) 'choice)
		      (rng-ipattern-get-child ipattern)
		    (list ipattern)))
	    t))))


(defun rng-intern-choice1 (normalized nullable)
  (let ((key (cons 'choice
		   (mapcar 'rng-ipattern-get-index normalized))))
    (or (rng-get-ipattern key)
	(rng-put-ipattern key
			  'choice
			  nil
			  normalized
			  nullable))))

(defun rng-intern-choice-shortcut (ipatterns)
  "Try to shortcut interning a choice list.
If successful, return the interned pattern.  Otherwise return nil."
  (while (and ipatterns
	      (eq (car ipatterns)
		  rng-not-allowed-ipattern))
    (setq ipatterns (cdr ipatterns)))
  (if ipatterns
      (let ((ret (car ipatterns)))
	(setq ipatterns (cdr ipatterns))
	(while (and ipatterns ret)
	  (or (eq (car ipatterns) rng-not-allowed-ipattern)
	      (eq (car ipatterns) ret)
	      (setq ret nil))
	  (setq ipatterns (cdr ipatterns)))
	ret)
    rng-not-allowed-ipattern))

(defun rng-normalize-choice-list (ipatterns)
  "Normalize a list of choices.
Expands nested choices, removes not-allowed members, sorts by index
and removes duplicates.  Return a pair whose car says whether the
list is nullable and whose cdr is the normalized list."
  (let ((sorted t)
	(nullable nil)
	(head (cons nil ipatterns)))
    (let ((tail head)
	  (final-tail nil)
	  (prev-index -100)
	  (cur ipatterns)
	  member)
      ;; the cdr of tail is always cur
      (while cur
	(setq member (car cur))
	(or nullable
	    (setq nullable (rng-ipattern-get-nullable member)))
	(cond ((eq (rng-ipattern-get-type member) 'choice)
	       (setq final-tail
		     (append (rng-ipattern-get-child member)
			     final-tail))
	       (setq cur (cdr cur))
	       (setq sorted nil)
	       (setcdr tail cur))
	      ((eq member rng-not-allowed-ipattern)
	       (setq cur (cdr cur))
	       (setcdr tail cur))
	      (t
	       (if (and sorted
			(let ((cur-index (rng-ipattern-get-index member)))
			  (if (>= prev-index cur-index)
			      (or (= prev-index cur-index) ; will remove it
				  (setq sorted nil)) ; won't remove it
			    (setq prev-index cur-index)
			    ;; won't remove it
			    nil)))
		   (progn
		     ;; remove it
		     (setq cur (cdr cur))
		     (setcdr tail cur))
		 ;; don't remove it
		 (setq tail cur)
		 (setq cur (cdr cur))))))
      (setcdr tail final-tail))
    (setq head (cdr head))
    (cons nullable
	  (if sorted
	      head
	    (rng-uniquify-eq (sort head 'rng-compare-ipattern))))))

(defun rng-compare-ipattern (p1 p2)
  (< (rng-ipattern-get-index p1)
     (rng-ipattern-get-index p2)))

;;; Name classes

(defsubst rng-name-class-contains (nc nm)
  (if (consp nc)
      (equal nm nc)
    (rng-name-class-contains1 nc nm)))

(defun rng-name-class-contains1 (nc nm)
  (let ((type (aref nc 0)))
    (cond ((eq type 'any-name) t)
	  ((eq type 'any-name-except)
	   (not (rng-name-class-contains (aref nc 1) nm)))
	  ((eq type 'ns-name)
	   (eq (car nm) (aref nc 1)))
	  ((eq type 'ns-name-except)
	   (and (eq (car nm) (aref nc 1))
		(not (rng-name-class-contains (aref nc 2) nm))))
	  ((eq type 'choice)
	   (let ((choices (aref nc 1))
		 (ret nil))
	     (while choices
	       (if (rng-name-class-contains (car choices) nm)
		   (progn
		     (setq choices nil)
		     (setq ret t))
		 (setq choices (cdr choices))))
	     ret)))))

(defun rng-name-class-possible-names (nc accum)
  "Return a list of possible names that nameclass NC can match.

Each possible name should be returned as a (NAMESPACE . LOCAL-NAME)
pair, where NAMESPACE is a symbol or nil and LOCAL-NAME is a string.
NAMESPACE, if nil, matches the absent namespace.  ACCUM is a list of
names which should be appended to the returned list.  The returned
list may contain duplicates."
  (if (consp nc)
      (cons nc accum)
    (when (eq (aref nc 0) 'choice)
      (let ((members (aref nc 1)) member)
	(while members
	  (setq member (car members))
	  (setq accum
		(if (consp member)
		    (cons member accum)
		  (rng-name-class-possible-names member
						 accum)))
	  (setq members (cdr members)))))
    accum))

;;; Debugging utilities

(defun rng-ipattern-to-string (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (concat (rng-ipattern-to-string
		    (rng-ipattern-get-child ipattern))
		   " </> "
		   (rng-ipattern-to-string
		    (rng-ipattern-get-after ipattern))))
	  ((eq type 'element)
	   (concat "element "
		   (rng-name-class-to-string
		    (rng-ipattern-get-name-class ipattern))
		   ;; we can get cycles with elements so don't print it out
		   " {...}"))
	  ((eq type 'attribute)
	   (concat "attribute "
		   (rng-name-class-to-string
		    (rng-ipattern-get-name-class ipattern))
		   " { "
		   (rng-ipattern-to-string
		    (rng-ipattern-get-child ipattern))
		   " } "))
	  ((eq type 'empty) "empty")
	  ((eq type 'text) "text")
	  ((eq type 'not-allowed) "notAllowed")
	  ((eq type 'one-or-more)
	   (concat (rng-ipattern-to-string
		    (rng-ipattern-get-child ipattern))
		   "+"))
	  ((eq type 'choice)
	   (concat "("
		   (mapconcat 'rng-ipattern-to-string
			      (rng-ipattern-get-child ipattern)
			      " | ")
		   ")"))
	  ((eq type 'group)
	   (concat "("
		   (mapconcat 'rng-ipattern-to-string
			      (rng-ipattern-get-child ipattern)
			      ", ")
		   ")"))
	  ((eq type 'interleave)
	   (concat "("
		   (mapconcat 'rng-ipattern-to-string
			      (rng-ipattern-get-child ipattern)
			      " & ")
		   ")"))
	  (t (symbol-name type)))))

(defun rng-name-class-to-string (nc)
  (if (consp nc)
      (cdr nc)
    (let ((type (aref nc 0)))
      (cond ((eq type 'choice)
	     (mapconcat 'rng-name-class-to-string
			(aref nc 1)
			"|"))
	    (t (concat (symbol-name type) "*"))))))


;;; Compiling

(defun rng-compile-maybe-init ()
  (unless rng-compile-table
    (setq rng-compile-table (make-hash-table :test 'eq))))

(defun rng-compile-clear ()
  (when rng-compile-table
    (clrhash rng-compile-table)))

(defun rng-compile (pattern)
  (or (gethash pattern rng-compile-table)
      (let ((ipattern (apply (get (car pattern) 'rng-compile)
			     (cdr pattern))))
	(puthash pattern ipattern rng-compile-table)
	ipattern)))

(put 'empty 'rng-compile 'rng-compile-empty)
(put 'text 'rng-compile 'rng-compile-text)
(put 'not-allowed 'rng-compile 'rng-compile-not-allowed)
(put 'element 'rng-compile 'rng-compile-element)
(put 'attribute 'rng-compile 'rng-compile-attribute)
(put 'choice 'rng-compile 'rng-compile-choice)
(put 'optional 'rng-compile 'rng-compile-optional)
(put 'group 'rng-compile 'rng-compile-group)
(put 'interleave 'rng-compile 'rng-compile-interleave)
(put 'ref 'rng-compile 'rng-compile-ref)
(put 'one-or-more 'rng-compile 'rng-compile-one-or-more)
(put 'zero-or-more 'rng-compile 'rng-compile-zero-or-more)
(put 'mixed 'rng-compile 'rng-compile-mixed)
(put 'data 'rng-compile 'rng-compile-data)
(put 'data-except 'rng-compile 'rng-compile-data-except)
(put 'value 'rng-compile 'rng-compile-value)
(put 'list 'rng-compile 'rng-compile-list)

(defun rng-compile-not-allowed () rng-not-allowed-ipattern)
(defun rng-compile-empty () rng-empty-ipattern)
(defun rng-compile-text () rng-text-ipattern)

(defun rng-compile-element (name-class pattern)
  ;; don't intern
  (rng-make-ipattern 'element
		     (rng-gen-ipattern-index)
		     (rng-compile-name-class name-class)
		     pattern		; compile lazily
		     nil))

(defun rng-element-get-child (element)
  (let ((tem (rng-ipattern-get-child element)))
    (if (vectorp tem)
	tem
      (rng-ipattern-set-child element (rng-compile tem)))))

(defun rng-compile-attribute (name-class pattern)
  (rng-intern-attribute (rng-compile-name-class name-class)
			(rng-compile pattern)))

(defun rng-compile-ref (pattern name)
  (and (memq pattern rng-being-compiled)
       (rng-compile-error "Reference loop on symbol %s" name))
  (setq rng-being-compiled
	(cons pattern rng-being-compiled))
  (unwind-protect
      (rng-compile pattern)
    (setq rng-being-compiled
	  (cdr rng-being-compiled))))

(defun rng-compile-one-or-more (pattern)
  (rng-intern-one-or-more (rng-compile pattern)))

(defun rng-compile-zero-or-more (pattern)
  (rng-intern-optional
   (rng-intern-one-or-more (rng-compile pattern))))

(defun rng-compile-optional (pattern)
  (rng-intern-optional (rng-compile pattern)))

(defun rng-compile-mixed (pattern)
  (rng-intern-interleave (cons rng-text-ipattern
			       (list (rng-compile pattern)))))

(defun rng-compile-list (pattern)
  (rng-intern-list (rng-compile pattern)))

(defun rng-compile-choice (&rest patterns)
  (rng-intern-choice (mapcar 'rng-compile patterns)))

(defun rng-compile-group (&rest patterns)
  (rng-intern-group (mapcar 'rng-compile patterns)))

(defun rng-compile-interleave (&rest patterns)
  (rng-intern-interleave (mapcar 'rng-compile patterns)))

(defun rng-compile-dt (name params)
  (let ((rng-dt-error-reporter 'rng-compile-error))
    (funcall (let ((uri (car name)))
	       (or (get uri 'rng-dt-compile)
		   (rng-compile-error "Unknown datatype library %s" uri)))
	     (cdr name)
	     params)))

(defun rng-compile-data (name params)
  (let ((dt (rng-compile-dt name params)))
    (rng-intern-data (cdr dt) (car dt))))

(defun rng-compile-data-except (name params pattern)
  (rng-intern-data-except (cdr (rng-compile-dt name params))
			  (rng-compile pattern)))

(defun rng-compile-value (name str context)
  (let* ((dt (cdr (rng-compile-dt name '())))
	 (rng-dt-namespace-context-getter (list 'identity context))
	 (obj (rng-dt-make-value dt str)))
    (if obj
	(rng-intern-value dt obj)
      (rng-compile-error "Value %s is not a valid instance of the datatype %s"
			 str
			 name))))

(defun rng-compile-name-class (nc)
  (let ((type (car nc)))
    (cond ((eq type 'name) (nth 1 nc))
	  ((eq type 'any-name) [any-name])
	  ((eq type 'any-name-except)
	   (vector 'any-name-except
		   (rng-compile-name-class (nth 1 nc))))
	  ((eq type 'ns-name)
	   (vector 'ns-name (nth 1 nc)))
	  ((eq type 'ns-name-except)
	   (vector 'ns-name-except
		   (nth 1 nc)
		   (rng-compile-name-class (nth 2 nc))))
	  ((eq type 'choice)
	   (vector 'choice
		   (mapcar 'rng-compile-name-class (cdr nc))))
	  (t (error "Bad name-class type %s" type)))))

;;; Searching patterns

;; We write this non-recursively to avoid hitting max-lisp-eval-depth
;; on large schemas.

(defun rng-map-element-attribute (function pattern accum &rest args)
  (let ((searched (make-hash-table :test 'eq))
	type todo patterns)
    (while (progn
	     (setq type (car pattern))
	     (cond ((memq type '(element attribute))
		    (setq accum
			  (apply function
				 (cons pattern
				       (cons accum args))))
		    (setq pattern (nth 2 pattern)))
		   ((eq type 'ref)
		    (setq pattern (nth 1 pattern))
		    (if (gethash pattern searched)
			(setq pattern nil)
		      (puthash pattern t searched)))
		   ((memq type '(choice group interleave))
		    (setq todo (cons (cdr pattern) todo))
		    (setq pattern nil))
		   ((memq type '(one-or-more
				 zero-or-more
				 optional
				 mixed))
		    (setq pattern (nth 1 pattern)))
		   (t (setq pattern nil)))
	     (cond (pattern)
		   (patterns
		    (setq pattern (car patterns))
		    (setq patterns (cdr patterns))
		    t)
		   (todo
		    (setq patterns (car todo))
		    (setq todo (cdr todo))
		    (setq pattern (car patterns))
		    (setq patterns (cdr patterns))
		    t))))
    accum))

(defun rng-find-element-content-pattern (pattern accum name)
  (if (and (eq (car pattern) 'element)
	   (rng-search-name name (nth 1 pattern)))
      (cons (rng-compile (nth 2 pattern)) accum)
    accum))

(defun rng-search-name (name nc)
  (let ((type (car nc)))
    (cond ((eq type 'name)
	   (equal (cadr nc) name))
	  ((eq type 'choice)
	   (let ((choices (cdr nc))
		 (found nil))
	     (while (and choices (not found))
	       (if (rng-search-name name (car choices))
		   (setq found t)
		 (setq choices (cdr choices))))
	     found))
	  (t nil))))

(defun rng-find-name-class-uris (nc accum)
  (let ((type (car nc)))
    (cond ((eq type 'name)
	   (rng-accum-namespace-uri (car (nth 1 nc)) accum))
	  ((memq type '(ns-name ns-name-except))
	   (rng-accum-namespace-uri (nth 1 nc) accum))
	  ((eq type 'choice)
	   (let ((choices (cdr nc)))
	     (while choices
	       (setq accum
		     (rng-find-name-class-uris (car choices) accum))
	       (setq choices (cdr choices))))
	   accum)
	  (t accum))))

(defun rng-accum-namespace-uri (ns accum)
  (if (and ns (not (memq ns accum)))
      (cons ns accum)
    accum))

;;; Derivatives

(defun rng-ipattern-text-typed-p (ipattern)
  (let ((memo (rng-ipattern-get-memo-text-typed ipattern)))
    (if (eq memo 'unknown)
	(rng-ipattern-set-memo-text-typed
	 ipattern
	 (rng-ipattern-compute-text-typed-p ipattern))
      memo)))

(defun rng-ipattern-compute-text-typed-p (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'choice)
	   (let ((cur (rng-ipattern-get-child ipattern))
		 (ret nil))
	     (while (and cur (not ret))
	       (if (rng-ipattern-text-typed-p (car cur))
		   (setq ret t)
		 (setq cur (cdr cur))))
	     ret))
	  ((eq type 'group)
	   (let ((cur (rng-ipattern-get-child ipattern))
		 (ret nil)
		 member)
	     (while (and cur (not ret))
	       (setq member (car cur))
	       (if (rng-ipattern-text-typed-p member)
		   (setq ret t))
	       (setq cur
		     (and (rng-ipattern-get-nullable member)
			  (cdr cur))))
	     ret))
	  ((eq type 'after)
	   (rng-ipattern-text-typed-p (rng-ipattern-get-child ipattern)))
	  (t (and (memq type '(value list data data-except)) t)))))

(defun rng-start-tag-open-deriv (ipattern nm)
  (or (rng-memo-map-get
       nm
       (rng-ipattern-get-memo-map-start-tag-open-deriv ipattern))
      (rng-ipattern-memo-start-tag-open-deriv
       ipattern
       nm
       (rng-compute-start-tag-open-deriv ipattern nm))))

(defun rng-ipattern-memo-start-tag-open-deriv (ipattern nm deriv)
  (or (memq ipattern rng-const-ipatterns)
      (rng-ipattern-set-memo-map-start-tag-open-deriv
       ipattern
       (rng-memo-map-add nm
			 deriv
			 (rng-ipattern-get-memo-map-start-tag-open-deriv
			  ipattern))))
  deriv)

(defun rng-compute-start-tag-open-deriv (ipattern nm)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'choice)
	   (rng-transform-choice `(lambda (p)
				    (rng-start-tag-open-deriv p ',nm))
				 ipattern))
	  ((eq type 'element)
	   (if (rng-name-class-contains
		(rng-ipattern-get-name-class ipattern)
		nm)
	       (rng-intern-after (rng-element-get-child ipattern)
				 rng-empty-ipattern)
	     rng-not-allowed-ipattern))
	  ((eq type 'group)
	   (rng-transform-group-nullable
	    `(lambda (p) (rng-start-tag-open-deriv p ',nm))
	    'rng-cons-group-after
	    ipattern))
	  ((eq type 'interleave)
	   (rng-transform-interleave-single
	    `(lambda (p) (rng-start-tag-open-deriv p ',nm))
	    'rng-subst-interleave-after
	    ipattern))
	  ((eq type 'one-or-more)
	   (rng-apply-after
	    `(lambda (p)
	       (rng-intern-group (list p ,(rng-intern-optional ipattern))))
	    (rng-start-tag-open-deriv (rng-ipattern-get-child ipattern)
				      nm)))
	  ((eq type 'after)
	   (rng-apply-after
	    `(lambda (p)
	       (rng-intern-after p
				 ,(rng-ipattern-get-after ipattern)))
	    (rng-start-tag-open-deriv (rng-ipattern-get-child ipattern)
				      nm)))
	  (t rng-not-allowed-ipattern))))

(defun rng-start-attribute-deriv (ipattern nm)
  (or (rng-memo-map-get
       nm
       (rng-ipattern-get-memo-map-start-attribute-deriv ipattern))
      (rng-ipattern-memo-start-attribute-deriv
       ipattern
       nm
       (rng-compute-start-attribute-deriv ipattern nm))))

(defun rng-ipattern-memo-start-attribute-deriv (ipattern nm deriv)
  (or (memq ipattern rng-const-ipatterns)
      (rng-ipattern-set-memo-map-start-attribute-deriv
       ipattern
       (rng-memo-map-add
	nm
	deriv
	(rng-ipattern-get-memo-map-start-attribute-deriv ipattern))))
  deriv)

(defun rng-compute-start-attribute-deriv (ipattern nm)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'choice)
	   (rng-transform-choice `(lambda (p)
				    (rng-start-attribute-deriv p ',nm))
				 ipattern))
	  ((eq type 'attribute)
	   (if (rng-name-class-contains
		(rng-ipattern-get-name-class ipattern)
		nm)
	       (rng-intern-after (rng-ipattern-get-child ipattern)
				 rng-empty-ipattern)
	     rng-not-allowed-ipattern))
	  ((eq type 'group)
	   (rng-transform-interleave-single
	    `(lambda (p) (rng-start-attribute-deriv p ',nm))
	    'rng-subst-group-after
	    ipattern))
	  ((eq type 'interleave)
	   (rng-transform-interleave-single
	    `(lambda (p) (rng-start-attribute-deriv p ',nm))
	    'rng-subst-interleave-after
	    ipattern))
	  ((eq type 'one-or-more)
	   (rng-apply-after
	    `(lambda (p)
	       (rng-intern-group (list p ,(rng-intern-optional ipattern))))
	    (rng-start-attribute-deriv (rng-ipattern-get-child ipattern)
				       nm)))
	  ((eq type 'after)
	   (rng-apply-after
	    `(lambda (p)
	       (rng-intern-after p ,(rng-ipattern-get-after ipattern)))
	    (rng-start-attribute-deriv (rng-ipattern-get-child ipattern)
				       nm)))
	  (t rng-not-allowed-ipattern))))

(defun rng-cons-group-after (x y)
  (rng-apply-after `(lambda (p) (rng-intern-group (cons p ',y)))
		   x))

(defun rng-subst-group-after (new old list)
  (rng-apply-after `(lambda (p)
		      (rng-intern-group (rng-substq p ,old ',list)))
		   new))

(defun rng-subst-interleave-after (new old list)
  (rng-apply-after `(lambda (p)
		      (rng-intern-interleave (rng-substq p ,old ',list)))
		   new))

(defun rng-apply-after (f ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (rng-intern-after
	    (rng-ipattern-get-child ipattern)
	    (funcall f
		     (rng-ipattern-get-after ipattern))))
	  ((eq type 'choice)
	   (rng-transform-choice `(lambda (x) (rng-apply-after ,f x))
				 ipattern))
	  (t rng-not-allowed-ipattern))))

(defun rng-start-tag-close-deriv (ipattern)
  (or (rng-ipattern-get-memo-start-tag-close-deriv ipattern)
      (rng-ipattern-set-memo-start-tag-close-deriv
       ipattern
       (rng-compute-start-tag-close-deriv ipattern))))

(defconst rng-transform-map
  '((choice . rng-transform-choice)
    (group . rng-transform-group)
    (interleave . rng-transform-interleave)
    (one-or-more . rng-transform-one-or-more)
    (after . rng-transform-after-child)))

(defun rng-compute-start-tag-close-deriv (ipattern)
  (let* ((type (rng-ipattern-get-type ipattern)))
    (if (eq type 'attribute)
	rng-not-allowed-ipattern
      (let ((transform (assq type rng-transform-map)))
	(if transform
	    (funcall (cdr transform)
		     'rng-start-tag-close-deriv
		     ipattern)
	  ipattern)))))

(defun rng-ignore-attributes-deriv (ipattern)
  (let* ((type (rng-ipattern-get-type ipattern)))
    (if (eq type 'attribute)
	rng-empty-ipattern
      (let ((transform (assq type rng-transform-map)))
	(if transform
	    (funcall (cdr transform)
		     'rng-ignore-attributes-deriv
		     ipattern)
	  ipattern)))))

(defun rng-text-only-deriv (ipattern)
  (or (rng-ipattern-get-memo-text-only-deriv ipattern)
      (rng-ipattern-set-memo-text-only-deriv
       ipattern
       (rng-compute-text-only-deriv ipattern))))

(defun rng-compute-text-only-deriv (ipattern)
  (let* ((type (rng-ipattern-get-type ipattern)))
    (if (eq type 'element)
	rng-not-allowed-ipattern
      (let ((transform (assq type
			     '((choice . rng-transform-choice)
			       (group . rng-transform-group)
			       (interleave . rng-transform-interleave)
			       (one-or-more . rng-transform-one-or-more)
			       (after . rng-transform-after-child)))))
	(if transform
	    (funcall (cdr transform)
		     'rng-text-only-deriv
		     ipattern)
	  ipattern)))))

(defun rng-mixed-text-deriv (ipattern)
  (or (rng-ipattern-get-memo-mixed-text-deriv ipattern)
      (rng-ipattern-set-memo-mixed-text-deriv
       ipattern
       (rng-compute-mixed-text-deriv ipattern))))

(defun rng-compute-mixed-text-deriv (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'text) ipattern)
	  ((eq type 'after)
	   (rng-transform-after-child 'rng-mixed-text-deriv
				      ipattern))
	  ((eq type 'choice)
	   (rng-transform-choice 'rng-mixed-text-deriv
				 ipattern))
	  ((eq type 'one-or-more)
	   (rng-intern-group
	    (list (rng-mixed-text-deriv
		   (rng-ipattern-get-child ipattern))
		  (rng-intern-optional ipattern))))
	  ((eq type 'group)
	   (rng-transform-group-nullable
	    'rng-mixed-text-deriv
	    (lambda (x y) (rng-intern-group (cons x y)))
	    ipattern))
	  ((eq type 'interleave)
	   (rng-transform-interleave-single
	    'rng-mixed-text-deriv
	    (lambda (new old list) (rng-intern-interleave
				    (rng-substq new old list)))
	    ipattern))
	  ((and (eq type 'data)
		(not (rng-ipattern-get-memo-text-typed ipattern)))
	   ipattern)
	  (t rng-not-allowed-ipattern))))

(defun rng-end-tag-deriv (ipattern)
  (or (rng-ipattern-get-memo-end-tag-deriv ipattern)
      (rng-ipattern-set-memo-end-tag-deriv
       ipattern
       (rng-compute-end-tag-deriv ipattern))))

(defun rng-compute-end-tag-deriv (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'choice)
	   (rng-intern-choice
	    (mapcar 'rng-end-tag-deriv
		    (rng-ipattern-get-child ipattern))))
	  ((eq type 'after)
	   (if (rng-ipattern-get-nullable
		(rng-ipattern-get-child ipattern))
	       (rng-ipattern-get-after ipattern)
	     rng-not-allowed-ipattern))
	  (t rng-not-allowed-ipattern))))

(defun rng-data-deriv (ipattern value)
  (or (rng-memo-map-get value
			(rng-ipattern-get-memo-map-data-deriv ipattern))
      (and (rng-memo-map-get
	    (cons value (rng-namespace-context-get-no-trace))
	    (rng-ipattern-get-memo-map-data-deriv ipattern))
	   (rng-memo-map-get
	    (cons value (apply (car rng-dt-namespace-context-getter)
			       (cdr rng-dt-namespace-context-getter)))
	    (rng-ipattern-get-memo-map-data-deriv ipattern)))
      (let* ((used-context (vector nil))
	     (rng-dt-namespace-context-getter
	      (cons 'rng-namespace-context-tracer
		    (cons used-context
			  rng-dt-namespace-context-getter)))
	     (deriv (rng-compute-data-deriv ipattern value)))
	(rng-ipattern-memo-data-deriv ipattern
				      value
				      (aref used-context 0)
				      deriv))))

(defun rng-namespace-context-tracer (used getter &rest args)
  (let ((context (apply getter args)))
    (aset used 0 context)
    context))

(defun rng-namespace-context-get-no-trace ()
  (let ((tem rng-dt-namespace-context-getter))
    (while (and tem (eq (car tem) 'rng-namespace-context-tracer))
      (setq tem (cddr tem)))
    (apply (car tem) (cdr tem))))

(defconst rng-memo-data-deriv-max-length 80
  "Don't memoize data-derivs for values longer than this.")

(defun rng-ipattern-memo-data-deriv (ipattern value context deriv)
  (or (memq ipattern rng-const-ipatterns)
      (> (length value) rng-memo-data-deriv-max-length)
      (rng-ipattern-set-memo-map-data-deriv
       ipattern
       (rng-memo-map-add (if context (cons value context) value)
			 deriv
			 (rng-ipattern-get-memo-map-data-deriv ipattern)
			 t)))
  deriv)

(defun rng-compute-data-deriv (ipattern value)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'text) ipattern)
	  ((eq type 'choice)
	   (rng-transform-choice `(lambda (p) (rng-data-deriv p ,value))
				 ipattern))
	  ((eq type 'group)
	   (rng-transform-group-nullable
	    `(lambda (p) (rng-data-deriv p ,value))
	    (lambda (x y) (rng-intern-group (cons x y)))
	    ipattern))
	  ((eq type 'one-or-more)
	   (rng-intern-group (list (rng-data-deriv
				    (rng-ipattern-get-child ipattern)
				    value)
				   (rng-intern-optional ipattern))))
	  ((eq type 'after)
	   (let ((child (rng-ipattern-get-child ipattern)))
	     (if (or (rng-ipattern-get-nullable
		      (rng-data-deriv child value))
		     (and (rng-ipattern-get-nullable child)
			  (rng-blank-p value)))
		 (rng-ipattern-get-after ipattern)
	       rng-not-allowed-ipattern)))
	  ((eq type 'data)
	   (if (rng-dt-make-value (rng-ipattern-get-datatype ipattern)
				  value)
	       rng-empty-ipattern
	     rng-not-allowed-ipattern))
	  ((eq type 'data-except)
	   (if (and (rng-dt-make-value (rng-ipattern-get-datatype ipattern)
				       value)
		    (not (rng-ipattern-get-nullable
			  (rng-data-deriv
			   (rng-ipattern-get-child ipattern)
			   value))))
	       rng-empty-ipattern
	     rng-not-allowed-ipattern))
	  ((eq type 'value)
	   (if (equal (rng-dt-make-value (rng-ipattern-get-datatype ipattern)
					 value)
		      (rng-ipattern-get-value-object ipattern))
	       rng-empty-ipattern
	     rng-not-allowed-ipattern))
	  ((eq type 'list)
	   (let ((tokens (split-string value))
		 (state (rng-ipattern-get-child ipattern)))
	     (while (and tokens
			 (not (eq state rng-not-allowed-ipattern)))
	       (setq state (rng-data-deriv state (car tokens)))
	       (setq tokens (cdr tokens)))
	     (if (rng-ipattern-get-nullable state)
		 rng-empty-ipattern
	       rng-not-allowed-ipattern)))
	  ;; don't think interleave can occur
	  ;; since we do text-only-deriv first
	  (t rng-not-allowed-ipattern))))

(defun rng-transform-multi (f ipattern interner)
  (let* ((members (rng-ipattern-get-child ipattern))
	 (transformed (mapcar f members)))
    (if (rng-members-eq members transformed)
	ipattern
      (funcall interner transformed))))

(defun rng-transform-choice (f ipattern)
  (rng-transform-multi f ipattern 'rng-intern-choice))

(defun rng-transform-group (f ipattern)
  (rng-transform-multi f ipattern 'rng-intern-group))

(defun rng-transform-interleave (f ipattern)
  (rng-transform-multi f ipattern 'rng-intern-interleave))

(defun rng-transform-one-or-more (f ipattern)
  (let* ((child (rng-ipattern-get-child ipattern))
	 (transformed (funcall f child)))
    (if (eq child transformed)
	ipattern
      (rng-intern-one-or-more transformed))))

(defun rng-transform-after-child (f ipattern)
  (let* ((child (rng-ipattern-get-child ipattern))
	 (transformed (funcall f child)))
    (if (eq child transformed)
	ipattern
      (rng-intern-after transformed
			(rng-ipattern-get-after ipattern)))))

(defun rng-transform-interleave-single (f subster ipattern)
  (let ((children (rng-ipattern-get-child ipattern))
	found)
    (while (and children (not found))
      (let* ((child (car children))
	     (transformed (funcall f child)))
	(if (eq transformed rng-not-allowed-ipattern)
	    (setq children (cdr children))
	  (setq found
		(funcall subster
			 transformed
			 child
			 (rng-ipattern-get-child ipattern))))))
    (or found
	rng-not-allowed-ipattern)))

(defun rng-transform-group-nullable (f conser ipattern)
  "Given a group x1,...,xn,y1,...,yn where the xs are all
nullable and y1 isn't, return a choice
  (conser f(x1) x2,...,xm,y1,...,yn)
  |(conser f(x2) x3,...,xm,y1,...,yn)
  |...
  |(conser f(xm) y1,...,yn)
  |(conser f(y1) y2,...,yn)"
  (rng-intern-choice
   (rng-transform-group-nullable-gen-choices
    f
    conser
    (rng-ipattern-get-child ipattern))))

(defun rng-transform-group-nullable-gen-choices (f conser members)
  (let ((head (car members))
	(tail (cdr members)))
    (if tail
	(cons (funcall conser (funcall f head) tail)
	      (if (rng-ipattern-get-nullable head)
		  (rng-transform-group-nullable-gen-choices f conser tail)
		nil))
      (list (funcall f head)))))

(defun rng-members-eq (list1 list2)
  (while (and list1
	      list2
	      (eq (car list1) (car list2)))
    (setq list1 (cdr list1))
    (setq list2 (cdr list2)))
  (and (null list1) (null list2)))


(defun rng-ipattern-after (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'choice)
	   (rng-transform-choice 'rng-ipattern-after ipattern))
	  ((eq type 'after)
	   (rng-ipattern-get-after ipattern))
	  ((eq  type 'not-allowed)
	   ipattern)
	  (t (error "Internal error in rng-ipattern-after: unexpected type %s" type)))))

(defun rng-unknown-start-tag-open-deriv (ipattern)
  (rng-intern-after (rng-compile rng-any-content) ipattern))

(defun rng-ipattern-optionalize-elements (ipattern)
  (let* ((type (rng-ipattern-get-type ipattern))
	 (transform (assq type rng-transform-map)))
    (cond (transform
	   (funcall (cdr transform)
		    'rng-ipattern-optionalize-elements
		    ipattern))
	  ((eq type 'element)
	   (rng-intern-optional ipattern))
	  (t ipattern))))

(defun rng-ipattern-empty-before-p (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (eq (rng-ipattern-get-child ipattern) rng-empty-ipattern))
	  ((eq type 'choice)
	   (let ((members (rng-ipattern-get-child ipattern))
		 (ret t))
	     (while (and members ret)
	       (or (rng-ipattern-empty-before-p (car members))
		   (setq ret nil))
	       (setq members (cdr members)))
	     ret))
	  (t nil))))

(defun rng-ipattern-possible-start-tags (ipattern accum)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (rng-ipattern-possible-start-tags
	    (rng-ipattern-get-child ipattern)
	    accum))
	  ((memq type '(choice interleave))
	   (let ((members (rng-ipattern-get-child ipattern)))
	     (while members
	       (setq accum
		     (rng-ipattern-possible-start-tags (car members)
						       accum))
	       (setq members (cdr members))))
	   accum)
	  ((eq type 'group)
	   (let ((members (rng-ipattern-get-child ipattern)))
	     (while members
	       (setq accum
		     (rng-ipattern-possible-start-tags (car members)
						       accum))
	       (setq members
		     (and (rng-ipattern-get-nullable (car members))
			  (cdr members)))))
	   accum)
	  ((eq type 'element)
	   (if (eq (rng-element-get-child ipattern) rng-not-allowed-ipattern)
	       accum
	     (rng-name-class-possible-names
	      (rng-ipattern-get-name-class ipattern)
	      accum)))
	  ((eq type 'one-or-more)
	   (rng-ipattern-possible-start-tags
	    (rng-ipattern-get-child ipattern)
	    accum))
	  (t accum))))

(defun rng-ipattern-start-tag-possible-p (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((memq type '(after one-or-more))
	   (rng-ipattern-start-tag-possible-p
	    (rng-ipattern-get-child ipattern)))
	  ((memq type '(choice interleave))
	   (let ((members (rng-ipattern-get-child ipattern))
		 (possible nil))
	     (while (and members (not possible))
	       (setq possible
		     (rng-ipattern-start-tag-possible-p (car members)))
	       (setq members (cdr members)))
	     possible))
	  ((eq type 'group)
	   (let ((members (rng-ipattern-get-child ipattern))
		 (possible nil))
	     (while (and members (not possible))
	       (setq possible
		     (rng-ipattern-start-tag-possible-p (car members)))
	       (setq members
		     (and (rng-ipattern-get-nullable (car members))
			  (cdr members))))
	     possible))
	  ((eq type 'element)
	   (not (eq (rng-element-get-child ipattern)
		    rng-not-allowed-ipattern)))
	  (t nil))))

(defun rng-ipattern-possible-attributes (ipattern accum)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (rng-ipattern-possible-attributes (rng-ipattern-get-child ipattern)
					     accum))
	  ((memq type '(choice interleave group))
	   (let ((members (rng-ipattern-get-child ipattern)))
	     (while members
	       (setq accum
		     (rng-ipattern-possible-attributes (car members)
						       accum))
	       (setq members (cdr members))))
	   accum)
	  ((eq type 'attribute)
	   (rng-name-class-possible-names
	    (rng-ipattern-get-name-class ipattern)
	    accum))
	  ((eq type 'one-or-more)
	   (rng-ipattern-possible-attributes
	    (rng-ipattern-get-child ipattern)
	    accum))
	  (t accum))))

(defun rng-ipattern-possible-values (ipattern accum)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (rng-ipattern-possible-values (rng-ipattern-get-child ipattern)
					 accum))
	  ((eq type 'choice)
	   (let ((members (rng-ipattern-get-child ipattern)))
	     (while members
	       (setq accum
		     (rng-ipattern-possible-values (car members)
						   accum))
	       (setq members (cdr members))))
	   accum)
	  ((eq type 'value)
	   (let ((value-object (rng-ipattern-get-value-object ipattern)))
	     (if (stringp value-object)
		 (cons value-object accum)
	       accum)))
	  (t accum))))

(defun rng-ipattern-required-element (ipattern)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((memq type '(after one-or-more))
	   (rng-ipattern-required-element (rng-ipattern-get-child ipattern)))
	  ((eq type 'choice)
	   (let* ((members (rng-ipattern-get-child ipattern))
		  (required (rng-ipattern-required-element (car members))))
	     (while (and required
			 (setq members (cdr members)))
	       (unless (equal required
			      (rng-ipattern-required-element (car members)))
		   (setq required nil)))
	     required))
	  ((eq type 'group)
	   (let ((members (rng-ipattern-get-child ipattern))
		 required)
	     (while (and (not (setq required
				    (rng-ipattern-required-element
				     (car members))))
			 (rng-ipattern-get-nullable (car members))
			 (setq members (cdr members))))
	     required))
	  ((eq type 'interleave)
	   (let ((members (rng-ipattern-get-child ipattern))
		 required)
	     (while members
	       (let ((tem (rng-ipattern-required-element (car members))))
		 (cond ((not tem)
			(setq members (cdr members)))
		       ((not required)
			(setq required tem)
			(setq members (cdr members)))
		       ((equal required tem)
			(setq members (cdr members)))
		       (t
			(setq required nil)
			(setq members nil)))))
	     required))
	  ((eq type 'element)
	   (let ((nc (rng-ipattern-get-name-class ipattern)))
	     (and (consp nc)
		  (not (eq (rng-element-get-child ipattern)
			   rng-not-allowed-ipattern))
		  nc))))))

(defun rng-ipattern-required-attributes (ipattern accum)
  (let ((type (rng-ipattern-get-type ipattern)))
    (cond ((eq type 'after)
	   (rng-ipattern-required-attributes (rng-ipattern-get-child ipattern)
					     accum))
	  ((memq type '(interleave group))
	   (let ((members (rng-ipattern-get-child ipattern)))
	     (while members
	       (setq accum
		     (rng-ipattern-required-attributes (car members)
						       accum))
	       (setq members (cdr members))))
	   accum)
	  ((eq type 'choice)
	   (let ((members (rng-ipattern-get-child ipattern))
		 in-all in-this new-in-all)
	     (setq in-all
		   (rng-ipattern-required-attributes (car members)
						     nil))
	     (while (and in-all (setq members (cdr members)))
	       (setq in-this
		     (rng-ipattern-required-attributes (car members) nil))
	       (setq new-in-all nil)
	       (while in-this
		 (when (member (car in-this) in-all)
		   (setq new-in-all
			 (cons (car in-this) new-in-all)))
		 (setq in-this (cdr in-this)))
	       (setq in-all new-in-all))
	     (append in-all accum)))
	  ((eq type 'attribute)
	   (let ((nc (rng-ipattern-get-name-class ipattern)))
	     (if (consp nc)
		 (cons nc accum)
	       accum)))
	  ((eq type 'one-or-more)
	   (rng-ipattern-required-attributes (rng-ipattern-get-child ipattern)
					     accum))
	  (t accum))))

(defun rng-compile-error (&rest args)
  (signal 'rng-compile-error
	  (list (apply 'format args))))

(put 'rng-compile-error
     'error-conditions
     '(error rng-error rng-compile-error))

(put 'rng-compile-error
     'error-message
     "Incorrect schema")


;;; External API

(defsubst rng-match-state () rng-match-state)

(defsubst rng-set-match-state (state)
  (setq rng-match-state state))

(defsubst rng-match-state-equal (state)
  (eq state rng-match-state))

(defun rng-schema-changed ()
  (rng-ipattern-clear)
  (rng-compile-clear))

(defun rng-match-init-buffer ()
  (make-local-variable 'rng-compile-table)
  (make-local-variable 'rng-ipattern-table)
  (make-local-variable 'rng-last-ipattern-index))

(defun rng-match-start-document ()
  (rng-ipattern-maybe-init)
  (rng-compile-maybe-init)
  (add-hook 'rng-schema-change-hook 'rng-schema-changed nil t)
  (setq rng-match-state (rng-compile rng-current-schema)))

(defun rng-match-start-tag-open (name)
  (rng-update-match-state (rng-start-tag-open-deriv rng-match-state
						    name)))

(defun rng-match-attribute-name (name)
  (rng-update-match-state (rng-start-attribute-deriv rng-match-state
						     name)))

(defun rng-match-attribute-value (value)
  (rng-update-match-state (rng-data-deriv rng-match-state
					  value)))

(defun rng-match-element-value (value)
  (and (rng-update-match-state (rng-text-only-deriv rng-match-state))
       (rng-update-match-state (rng-data-deriv rng-match-state
					       value))))

(defun rng-match-start-tag-close ()
  (rng-update-match-state (rng-start-tag-close-deriv rng-match-state)))

(defun rng-match-mixed-text ()
  (rng-update-match-state (rng-mixed-text-deriv rng-match-state)))

(defun rng-match-end-tag ()
  (rng-update-match-state (rng-end-tag-deriv rng-match-state)))

(defun rng-match-after ()
  (rng-update-match-state
   (rng-ipattern-after rng-match-state)))

(defun rng-match-out-of-context-start-tag-open (name)
  (let* ((found (rng-map-element-attribute 'rng-find-element-content-pattern
					   rng-current-schema
					   nil
					   name))
	 (content-pattern (if found
			      (rng-intern-choice found)
			    rng-not-allowed-ipattern)))
    (rng-update-match-state
     (rng-intern-after content-pattern rng-match-state))))

(defun rng-match-possible-namespace-uris ()
  "Return a list of all the namespace URIs used in the current schema.
The absent URI is not included, so the result is always a list of symbols."
  (rng-map-element-attribute (lambda (pattern accum)
			       (rng-find-name-class-uris (nth 1 pattern)
							 accum))
			     rng-current-schema
			     nil))

(defun rng-match-unknown-start-tag-open ()
  (rng-update-match-state
   (rng-unknown-start-tag-open-deriv rng-match-state)))

(defun rng-match-optionalize-elements ()
  (rng-update-match-state
   (rng-ipattern-optionalize-elements rng-match-state)))

(defun rng-match-ignore-attributes ()
  (rng-update-match-state
   (rng-ignore-attributes-deriv rng-match-state)))

(defun rng-match-text-typed-p ()
  (rng-ipattern-text-typed-p rng-match-state))

(defun rng-match-empty-content ()
  (if (rng-match-text-typed-p)
      (rng-match-element-value "")
    (rng-match-end-tag)))

(defun rng-match-empty-before-p ()
  "Return non-nil if what can be matched before an end-tag is empty.
In other words, return non-nil if the pattern for what can be matched
for an end-tag is equivalent to empty."
  (rng-ipattern-empty-before-p rng-match-state))

(defun rng-match-infer-start-tag-namespace (local-name)
  (let ((ncs (rng-ipattern-possible-start-tags rng-match-state nil))
	(nc nil)
	(ns nil))
    (while ncs
      (setq nc (car ncs))
      (if (and (equal (cdr nc) local-name)
	       (symbolp (car nc)))
	  (cond ((not ns)
		 ;; first possible namespace
		 (setq ns (car nc))
		 (setq ncs (cdr ncs)))
		((equal ns (car nc))
		 ;; same as first namespace
		 (setq ncs (cdr ncs)))
		(t
		 ;; more than one possible namespace
		 (setq ns nil)
		 (setq ncs nil)))
	(setq ncs (cdr ncs))))
    ns))

(defun rng-match-nullable-p ()
  (rng-ipattern-get-nullable rng-match-state))

(defun rng-match-possible-start-tag-names ()
  "Return a list of possible names that would be valid for start-tags.

Each possible name is returned as a (NAMESPACE . LOCAL-NAME) pair,
where NAMESPACE is a symbol or nil (meaning the absent namespace) and
LOCAL-NAME is a string.  The returned list may contain duplicates."
  (rng-ipattern-possible-start-tags rng-match-state nil))

;; This is no longer used.  It might be useful so leave it in for now.
(defun rng-match-start-tag-possible-p ()
  "Return non-nil if a start-tag is possible."
  (rng-ipattern-start-tag-possible-p rng-match-state))

(defun rng-match-possible-attribute-names ()
  "Return a list of possible names that would be valid for attributes.

See the function `rng-match-possible-start-tag-names' for
more information."
  (rng-ipattern-possible-attributes rng-match-state nil))

(defun rng-match-possible-value-strings ()
  "Return a list of strings that would be valid as content.
The list may contain duplicates.  Typically, the list will not
be exhaustive."
  (rng-ipattern-possible-values rng-match-state nil))

(defun rng-match-required-element-name ()
  "Return the name of an element which must occur, or nil if none."
  (rng-ipattern-required-element rng-match-state))

(defun rng-match-required-attribute-names ()
  "Return a list of names of attributes which must all occur."
  (rng-ipattern-required-attributes rng-match-state nil))

(defmacro rng-match-save (&rest body)
  (let ((state (make-symbol "state")))
    `(let ((,state rng-match-state))
       (unwind-protect
	   (progn ,@body)
	 (setq rng-match-state ,state)))))

(put 'rng-match-save 'lisp-indent-function 0)
(def-edebug-spec rng-match-save t)

(defmacro rng-match-with-schema (schema &rest body)
  `(let ((rng-current-schema ,schema)
	 rng-match-state
	 rng-compile-table
	 rng-ipattern-table
	 rng-last-ipattern-index)
     (rng-ipattern-maybe-init)
     (rng-compile-maybe-init)
     (setq rng-match-state (rng-compile rng-current-schema))
     ,@body))

(put 'rng-match-with-schema 'lisp-indent-function 1)
(def-edebug-spec rng-match-with-schema t)

(provide 'rng-match)

;;; rng-match.el ends here
