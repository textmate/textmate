;;; semantic/sort.el --- Utilities for sorting and re-arranging tag tables.

;;; Copyright (C) 1999-2005, 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;; Tag tables originate in the order they appear in a buffer, or source file.
;; It is often useful to re-arrange them is some predictable way for browsing
;; purposes.  Re-organization may be alphabetical, or even a complete
;; reorganization of parents and children.
;;
;; Originally written in semantic/util.el
;;

(require 'semantic)
(eval-when-compile
  (require 'semantic/find))

(declare-function semanticdb-find-tags-external-children-of-type
		  "semantic/db-find")

;;; Alphanumeric sorting
;;
;; Takes a list of tags, and sorts them in a case-insensitive way
;; at a single level.

;;; Code:
(defun semantic-string-lessp-ci (s1 s2)
  "Case insensitive version of `string-lessp'.
Argument S1 and S2 are the strings to compare."
  ;; Use downcase instead of upcase because an average name
  ;; has more lower case characters.
  (if (fboundp 'compare-strings)
      (eq (compare-strings s1 0 nil s2 0 nil t) -1)
    (string-lessp (downcase s1) (downcase s2))))

(defun semantic-sort-tag-type (tag)
  "Return a type string for TAG guaranteed to be a string."
  (let ((ty (semantic-tag-type tag)))
    (cond ((stringp ty)
	   ty)
	  ((listp ty)
	   (or (car ty) ""))
	  (t ""))))

(defun semantic-tag-lessp-name-then-type (A B)
  "Return t if tag A is < tag B.
First sorts on name, then sorts on the name of the :type of
each tag."
  (let ((na (semantic-tag-name A))
	(nb (semantic-tag-name B))
	)
    (if (string-lessp na nb)
	t ; a sure thing.
      (if (string= na nb)
	  ;; If equal, test the :type which might be different.
	  (let* ((ta (semantic-tag-type A))
		 (tb (semantic-tag-type B))
		 (tas (cond ((stringp ta)
			     ta)
			    ((semantic-tag-p ta)
			     (semantic-tag-name ta))
			    (t nil)))
		 (tbs (cond ((stringp tb)
			     tb)
			    ((semantic-tag-p tb)
			     (semantic-tag-name tb))
			    (t nil))))
	    (if (and (stringp tas) (stringp tbs))
		(string< tas tbs)
	      ;; This is if A == B, and no types in A or B
	      nil))
	;; This nil is if A > B, but not =
	nil))))

(defun semantic-sort-tags-by-name-increasing (tags)
  "Sort TAGS by name in increasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (string-lessp (semantic-tag-name a)
			     (semantic-tag-name b)))))

(defun semantic-sort-tags-by-name-decreasing (tags)
  "Sort TAGS by name in decreasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (string-lessp (semantic-tag-name b)
			     (semantic-tag-name a)))))

(defun semantic-sort-tags-by-type-increasing (tags)
  "Sort TAGS by type in increasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (string-lessp (semantic-sort-tag-type a)
			     (semantic-sort-tag-type b)))))

(defun semantic-sort-tags-by-type-decreasing (tags)
  "Sort TAGS by type in decreasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (string-lessp (semantic-sort-tag-type b)
			     (semantic-sort-tag-type a)))))

(defun semantic-sort-tags-by-name-increasing-ci (tags)
  "Sort TAGS by name in increasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (semantic-string-lessp-ci (semantic-tag-name a)
					 (semantic-tag-name b)))))

(defun semantic-sort-tags-by-name-decreasing-ci (tags)
  "Sort TAGS by name in decreasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (semantic-string-lessp-ci (semantic-tag-name b)
					 (semantic-tag-name a)))))

(defun semantic-sort-tags-by-type-increasing-ci (tags)
  "Sort TAGS by type in increasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (semantic-string-lessp-ci (semantic-sort-tag-type a)
					 (semantic-sort-tag-type b)))))

(defun semantic-sort-tags-by-type-decreasing-ci (tags)
  "Sort TAGS by type in decreasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b)
	       (semantic-string-lessp-ci (semantic-sort-tag-type b)
					 (semantic-sort-tag-type a)))))

(defun semantic-sort-tags-by-name-then-type-increasing (tags)
  "Sort TAGS by name, then type in increasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b) (semantic-tag-lessp-name-then-type a b))))

(defun semantic-sort-tags-by-name-then-type-decreasing (tags)
  "Sort TAGS by name, then type in increasing order with side effects.
Return the sorted list."
  (sort tags (lambda (a b) (semantic-tag-lessp-name-then-type b a))))

;;; Unique
;;
;; Scan a list of tags, removing duplicates.
;; This must first sort the tags by name alphabetically ascending.
;;
;; Useful for completion lists, or other situations where the
;; other data isn't as useful.

(defun semantic-unique-tag-table-by-name (tags)
  "Scan a list of TAGS, removing duplicate names.
This must first sort the tags by name alphabetically ascending.
For more complex uniqueness testing used by the semanticdb
typecaching system, see `semanticdb-typecache-merge-streams'."
  (let ((sorted (semantic-sort-tags-by-name-increasing
		 (copy-sequence tags)))
	(uniq nil))
    (while sorted
      (if (or (not uniq)
	      (not (string= (semantic-tag-name (car sorted))
			    (semantic-tag-name (car uniq)))))
	  (setq uniq (cons (car sorted) uniq)))
      (setq sorted (cdr sorted))
      )
    (nreverse uniq)))

(defun semantic-unique-tag-table (tags)
  "Scan a list of TAGS, removing duplicates.
This must first sort the tags by position ascending.
TAGS are removed only if they are equivalent, as can happen when
multiple tag sources are scanned.
For more complex uniqueness testing used by the semanticdb
typecaching system, see `semanticdb-typecache-merge-streams'."
  (let ((sorted (sort (copy-sequence tags)
		      (lambda (a b)
			(cond ((not (semantic-tag-with-position-p a))
			       t)
			      ((not (semantic-tag-with-position-p b))
			       nil)
			      (t
			       (< (semantic-tag-start a)
				  (semantic-tag-start b)))))))
	(uniq nil))
    (while sorted
      (if (or (not uniq)
	      (not (semantic-equivalent-tag-p (car sorted) (car uniq))))
	  (setq uniq (cons (car sorted) uniq)))
      (setq sorted (cdr sorted))
      )
    (nreverse uniq)))


;;; Tag Table Flattening
;;
;; In the 1.4 search API, there was a parameter "search-parts" which
;; was used to find tags inside other tags.  This was used
;; infrequently, mostly for completion/jump routines.  These types
;; of commands would be better off with a flattened list, where all
;; tags appear at the top level.

;;;###autoload
(defun semantic-flatten-tags-table (&optional table)
  "Flatten the tags table TABLE.
All tags in TABLE, and all components of top level tags
in TABLE will appear at the top level of list.
Tags promoted to the top of the list will still appear
unmodified as components of their parent tags."
  (let* ((table (semantic-something-to-tag-table table))
	 ;; Initialize the starting list with our table.
	 (lists (list table)))
    (mapc (lambda (tag)
	    (let ((components (semantic-tag-components tag)))
	      (if (and components
		       ;; unpositioned tags can be hazardous to
		       ;; completion.  Do we need any type of tag
		       ;; here?  - EL
		       (semantic-tag-with-position-p (car components)))
		  (setq lists (cons
			       (semantic-flatten-tags-table components)
			       lists)))))
	  table)
    (apply 'append (nreverse lists))
    ))


;;; Buckets:
;;
;; A list of tags can be grouped into buckets based on the tag class.
;; Bucketize means to take a list of tags at a given level in a tag
;; table, and reorganize them into buckets based on class.
;;
(defvar semantic-bucketize-tag-class
  ;; Must use lambda because `semantic-tag-class' is a macro.
  (lambda (tok) (semantic-tag-class tok))
  "Function used to get a symbol describing the class of a tag.
This function must take one argument of a semantic tag.
It should return a symbol found in `semantic-symbol->name-assoc-list'
which `semantic-bucketize' uses to bin up tokens.
To create new bins for an application augment
`semantic-symbol->name-assoc-list', and
`semantic-symbol->name-assoc-list-for-type-parts' in addition
to setting this variable (locally in your function).")

(defun semantic-bucketize (tags &optional parent filter)
  "Sort TAGS into a group of buckets based on tag class.
Unknown classes are placed in a Misc bucket.
Type bucket names are defined by either `semantic-symbol->name-assoc-list'.
If PARENT is specified, then TAGS belong to this PARENT in some way.
This will use `semantic-symbol->name-assoc-list-for-type-parts' to
generate bucket names.
Optional argument FILTER is a filter function to be applied to each bucket.
The filter function will take one argument, which is a list of tokens, and
may re-organize the list with side-effects."
  (let* ((name-list (if parent
			semantic-symbol->name-assoc-list-for-type-parts
		      semantic-symbol->name-assoc-list))
	 (sn name-list)
	 (bins (make-vector (1+ (length sn)) nil))
	 ask tagtype
	 (nsn nil)
	 (num 1)
	 (out nil))
    ;; Build up the bucket vector
    (while sn
      (setq nsn (cons (cons (car (car sn)) num) nsn)
	    sn (cdr sn)
	    num (1+ num)))
    ;; Place into buckets
    (while tags
      (setq tagtype (funcall semantic-bucketize-tag-class (car tags))
	    ask (assq tagtype nsn)
	    num (or (cdr ask) 0))
      (aset bins num (cons (car tags) (aref bins num)))
      (setq tags (cdr tags)))
    ;; Remove from buckets into a list.
    (setq num 1)
    (while (< num (length bins))
      (when (aref bins num)
	(setq out
	      (cons (cons
		     (cdr (nth (1- num) name-list))
		     ;; Filtering, First hacked by David Ponce david@dponce.com
		     (funcall (or filter 'nreverse) (aref bins num)))
		    out)))
      (setq num (1+ num)))
    (if (aref bins 0)
	(setq out (cons (cons "Misc"
			      (funcall (or filter 'nreverse) (aref bins 0)))
			out)))
    (nreverse out)))

;;; Adoption
;;
;; Some languages allow children of a type to be defined outside
;; the syntactic scope of that class.  These routines will find those
;; external members, and bring them together in a cloned copy of the
;; class tag.
;;
(defvar semantic-orphaned-member-metaparent-type "class"
  "In `semantic-adopt-external-members', the type of 'type for metaparents.
A metaparent is a made-up type semantic token used to hold the child list
of orphaned members of a named type.")
(make-variable-buffer-local 'semantic-orphaned-member-metaparent-type)

(defvar semantic-mark-external-member-function nil
  "Function called when an externally defined orphan is found.
By default, the token is always marked with the `adopted' property.
This function should be locally bound by a program that needs
to add additional behaviors into the token list.
This function is called with two arguments.  The first is TOKEN which is
a shallow copy of the token to be modified.  The second is the PARENT
which is adopting TOKEN.  This function should return TOKEN (or a copy of it)
which is then integrated into the revised token list.")

(defun semantic-adopt-external-members (tags)
  "Rebuild TAGS so that externally defined members are regrouped.
Some languages such as C++ and CLOS permit the declaration of member
functions outside the definition of the class.  It is easier to study
the structure of a program when such methods are grouped together
more logically.

This function uses `semantic-tag-external-member-p' to
determine when a potential child is an externally defined member.

Note: Applications which use this function must account for token
types which do not have a position, but have children which *do*
have positions.

Applications should use `semantic-mark-external-member-function'
to modify all tags which are found as externally defined to some
type.  For example, changing the token type for generating extra
buckets with the bucket function."
  (let ((parent-buckets nil)
	(decent-list nil)
	(out nil)
	(tmp nil)
	)
    ;; Rebuild the output list, stripping out all parented
    ;; external entries
    (while tags
      (cond
       ((setq tmp (semantic-tag-external-member-parent (car tags)))
	(let ((tagcopy (semantic-tag-clone (car tags)))
	      (a (assoc tmp parent-buckets)))
	  (semantic--tag-put-property-no-side-effect tagcopy 'adopted t)
	  (if a
	      ;; If this parent is already in the list, append.
	      (setcdr (nthcdr (1- (length a)) a) (list tagcopy))
	    ;; If not, prepend this new parent bucket into our list
	    (setq parent-buckets
		  (cons (cons tmp (list tagcopy)) parent-buckets)))
	  ))
       ((eq (semantic-tag-class (car tags)) 'type)
	;; Types need to be rebuilt from scratch so we can add in new
	;; children to the child list.  Only the top-level cons
	;; cells need to be duplicated so we can hack out the
	;; child list later.
	(setq out (cons (semantic-tag-clone (car tags)) out))
	(setq decent-list (cons (car out) decent-list))
	)
       (t
	;; Otherwise, append this tag to our new output list.
	(setq out (cons (car tags) out)))
       )
      (setq tags (cdr tags)))
    ;; Rescan out, by descending into all types and finding parents
    ;; for all entries moved into the parent-buckets.
    (while decent-list
      (let* ((bucket (assoc (semantic-tag-name (car decent-list))
			    parent-buckets))
	     (bucketkids (cdr bucket)))
	(when bucket
	  ;; Run our secondary marking function on the children
	  (if semantic-mark-external-member-function
	      (setq bucketkids
		    (mapcar (lambda (tok)
			      (funcall semantic-mark-external-member-function
				       tok (car decent-list)))
			    bucketkids)))
	  ;; We have some extra kids.  Merge.
	  (semantic-tag-put-attribute
	   (car decent-list) :members
	   (append (semantic-tag-type-members (car decent-list))
		   bucketkids))
	  ;; Nuke the bucket label so it is not found again.
	  (setcar bucket nil))
	(setq decent-list
	      (append (cdr decent-list)
		      ;; get embedded types to scan and make copies
		      ;; of them.
		      (mapcar
		       (lambda (tok) (semantic-tag-clone tok))
		       (semantic-find-tags-by-class 'type
			(semantic-tag-type-members (car decent-list)))))
	      )))
    ;; Scan over all remaining lost external methods, and tack them
    ;; onto the end.
    (while parent-buckets
      (if (car (car parent-buckets))
	  (let* ((tmp (car parent-buckets))
		 (fauxtag (semantic-tag-new-type
			   (car tmp)
			   semantic-orphaned-member-metaparent-type
			   nil ;; Part list
			   nil ;; parents (unknown)
			   ))
		 (bucketkids (cdr tmp)))
	    (semantic-tag-set-faux fauxtag) ;; properties
	    (if semantic-mark-external-member-function
		(setq bucketkids
		      (mapcar (lambda (tok)
				(funcall semantic-mark-external-member-function
					 tok fauxtag))
			      bucketkids)))
	    (semantic-tag-put-attribute fauxtag :members bucketkids)
	    ;; We have a bunch of methods with no parent in this file.
	    ;; Create a meta-type to hold it.
	    (setq out (cons fauxtag out))
	    ))
      (setq parent-buckets (cdr parent-buckets)))
    ;; Return the new list.
    (nreverse out)))


;;; External children
;;
;; In order to adopt external children, we need a few overload methods
;; to enable the feature.

;;;###autoload
(define-overloadable-function semantic-tag-external-member-parent (tag)
  "Return a parent for TAG when TAG is an external member.
TAG is an external member if it is defined at a toplevel and
has some sort of label defining a parent.  The parent return will
be a string.

The default behavior, if not overridden with
`tag-member-parent' gets the 'parent extra
specifier of TAG.

If this function is overridden, use
`semantic-tag-external-member-parent-default' to also
include the default behavior, and merely extend your own."
  )

(defun semantic-tag-external-member-parent-default (tag)
  "Return the name of TAGs parent only if TAG is not defined in its parent."
  ;; Use only the extra spec because a type has a parent which
  ;; means something completely different.
  (let ((tp (semantic-tag-get-attribute tag :parent)))
    (when (stringp tp)
      tp)))

(define-overloadable-function semantic-tag-external-member-p (parent tag)
  "Return non-nil if PARENT is the parent of TAG.
TAG is an external member of PARENT when it is somehow tagged
as having PARENT as its parent.
PARENT and TAG must both be semantic tags.

The default behavior, if not overridden with
`tag-external-member-p' is to match :parent attribute in
the name of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-p-default' to also
include the default behavior, and merely extend your own."
  )

(defun semantic-tag-external-member-p-default (parent tag)
  "Return non-nil if PARENT is the parent of TAG."
  ;; Use only the extra spec because a type has a parent which
  ;; means something completely different.
  (let ((tp (semantic-tag-external-member-parent tag)))
    (and (stringp tp)
	 (string= (semantic-tag-name parent) tp))))

(define-overloadable-function semantic-tag-external-member-children (tag &optional usedb)
  "Return the list of children which are not *in* TAG.
If optional argument USEDB is non-nil, then also search files in
the Semantic Database.  If USEDB is a list of databases, search those
databases.

Children in this case are functions or types which are members of
TAG, such as the parts of a type, but which are not defined inside
the class.  C++ and CLOS both permit methods of a class to be defined
outside the bounds of the class' definition.

The default behavior, if not overridden with
`tag-external-member-children' is to search using
`semantic-tag-external-member-p' in all top level definitions
with a parent of TAG.

If this function is overridden, use
`semantic-tag-external-member-children-default' to also
include the default behavior, and merely extend your own."
  )

(defun semantic-tag-external-member-children-default (tag &optional usedb)
  "Return list of external children for TAG.
Optional argument USEDB specifies if the semantic database is used.
See `semantic-tag-external-member-children' for details."
  (if (and usedb
	   (require 'semantic/db-mode)
	   (semanticdb-minor-mode-p)
	   (require 'semantic/db-find))
      (let ((m (semanticdb-find-tags-external-children-of-type
		(semantic-tag-name tag))))
	(if m (apply #'append (mapcar #'cdr m))))
    (semantic--find-tags-by-function
     `(lambda (tok)
	;; This bit of annoying backquote forces the contents of
	;; tag into the generated lambda.
       (semantic-tag-external-member-p ',tag tok))
     (current-buffer))
    ))

(define-overloadable-function semantic-tag-external-class (tag)
  "Return a list of real tags that faux TAG might represent.

In some languages, a method can be defined on an object which is
not in the same file.  In this case,
`semantic-adopt-external-members' will create a faux-tag.  If it
is necessary to get the tag from which for faux TAG was most
likely derived, then this function is needed."
  (unless (semantic-tag-faux-p tag)
    (signal 'wrong-type-argument (list tag 'semantic-tag-faux-p)))
  (:override)
  )

(defun semantic-tag-external-class-default (tag)
  "Return a list of real tags that faux TAG might represent.
See `semantic-tag-external-class' for details."
  (if (and (require 'semantic/db-mode)
	   (semanticdb-minor-mode-p))
      (let* ((semanticdb-search-system-databases nil)
	     (m (semanticdb-find-tags-by-class
		 (semantic-tag-class tag)
		 (semanticdb-find-tags-by-name (semantic-tag-name tag)))))
	(semanticdb-strip-find-results m 'name))
    ;; Presumably, if the tag is faux, it is not local.
    nil))

(provide 'semantic/sort)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/sort"
;; End:

;;; semantic/sort.el ends here
