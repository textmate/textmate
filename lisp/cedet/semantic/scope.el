;;; semantic/scope.el --- Analyzer Scope Calculations

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Calculate information about the current scope.
;;
;; Manages the current scope as a structure that can be cached on a
;; per-file basis and recycled between different occurrences of
;; analysis on different parts of a file.
;;
;; Pattern for Scope Calculation
;;
;; Step 1: Calculate DataTypes in Scope:
;;
;; a) What is in scope via using statements or local namespaces
;; b) Lineage of current context.  Some names drawn from step 1.
;;
;; Step 2: Convert type names into lists of concrete tags
;;
;; a) Convert each datatype into the real datatype tag
;; b) Convert namespaces into the list of contents of the namespace.
;; c) Merge all existing scopes together into one search list.
;;
;; Step 3: Local variables
;;
;; a) Local variables are in the master search list.
;;

(require 'semantic/db)
(require 'semantic/analyze/fcn)
(require 'semantic/ctxt)

(eval-when-compile (require 'semantic/find))

(declare-function data-debug-show "eieio-datadebug")
(declare-function semantic-analyze-find-tag "semantic/analyze")
(declare-function semantic-analyze-princ-sequence "semantic/analyze")
(declare-function semanticdb-typecache-merge-streams "semantic/db-typecache")
(declare-function semanticdb-typecache-add-dependant "semantic/db-typecache")

;;; Code:

(defclass semantic-scope-cache (semanticdb-abstract-cache)
  ((tag :initform nil
	:documentation
	"The tag this scope was calculated for.")
   (scopetypes :initform nil
	       :documentation
	       "The list of types currently in scope.
For C++, this would contain anonymous namespaces known, and
anything labeled by a `using' statement.")
   (parents :initform nil
	    :documentation
	    "List of parents in scope w/in the body of this function.
Presumably, the members of these parent classes are available for access
based on private:, or public: style statements.")
   (parentinheritance :initform nil
		      :documentation "Alist of parents by inheritance.
Each entry is ( PARENT . PROTECTION ), where PARENT is a type, and
PROTECTION is a symbol representing the level of inheritance, such as 'private, or 'protected.")
   (scope :initform nil
	  :documentation
	  "Items in scope due to the scopetypes or parents.")
   (fullscope :initform nil
	      :documentation
	      "All the other stuff on one master list you can search.")
   (localargs :initform nil
	      :documentation
	      "The arguments to the function tag.")
   (localvar :initform nil
	     :documentation
	     "The local variables.")
   (typescope :initform nil
	      :documentation
	      "Slot to save intermediate scope while metatypes are dereferenced.")
   )
  "Cache used for storage of the current scope by the Semantic Analyzer.
Saves scoping information between runs of the analyzer.")

;;; METHODS
;;
;; Methods for basic management of the structure in semanticdb.
;;
(defmethod semantic-reset ((obj semantic-scope-cache))
  "Reset OBJ back to it's empty settings."
  (oset obj tag nil)
  (oset obj scopetypes nil)
  (oset obj parents nil)
  (oset obj parentinheritance nil)
  (oset obj scope nil)
  (oset obj fullscope nil)
  (oset obj localargs nil)
  (oset obj localvar nil)
  (oset obj typescope nil)
  )

(defmethod semanticdb-synchronize ((cache semantic-scope-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  (semantic-reset cache))


(defmethod semanticdb-partial-synchronize ((cache semantic-scope-cache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  ;; If there are any includes or datatypes changed, then clear.
  (if (or (semantic-find-tags-by-class 'include new-tags)
	  (semantic-find-tags-by-class 'type new-tags)
	  (semantic-find-tags-by-class 'using new-tags))
      (semantic-reset cache))
  )

(defun semantic-scope-reset-cache ()
  "Get the current cached scope, and reset it."
  (when semanticdb-current-table
    (let ((co (semanticdb-cache-get semanticdb-current-table
				    semantic-scope-cache)))
      (semantic-reset co))))

(defmethod semantic-scope-set-typecache ((cache semantic-scope-cache)
					 types-in-scope)
  "Set the :typescope property on CACHE to some types.
TYPES-IN-SCOPE is a list of type tags whos members are
currently in scope.  For each type in TYPES-IN-SCOPE,
add those members to the types list.
If nil, then the typescope is reset."
  (let ((newts nil)) ;; New Type Scope
    (dolist (onetype types-in-scope)
      (setq newts (append (semantic-tag-type-members onetype)
			  newts))
      )
    (oset cache typescope newts)))

;;; TAG SCOPES
;;
;; These fcns should be used by search routines that return a single
;; tag which, in turn, may have come from a deep scope.  The scope
;; will be attached to the tag.  Thus, in future scope based calls, a
;; tag can be passed in and a scope derived from it.

(defun semantic-scope-tag-clone-with-scope (tag scopetags)
  "Close TAG, and return it.  Add SCOPETAGS as a tag-local scope.
Stores the SCOPETAGS as a set of tag properties on the cloned tag."
  (let ((clone (semantic-tag-clone tag))
	)
    (semantic--tag-put-property clone 'scope scopetags)
    ))

(defun semantic-scope-tag-get-scope (tag)
  "Get from TAG the list of tags comprising the scope from TAG."
  (semantic--tag-get-property tag 'scope))

;;; SCOPE UTILITIES
;;
;; Functions that do the main scope calculations


(define-overloadable-function semantic-analyze-scoped-types (position)
  "Return a list of types currently in scope at POSITION.
This is based on what tags exist at POSITION, and any associated
types available.")

(defun semantic-analyze-scoped-types-default (position)
  "Return a list of types currently in scope at POSITION.
Use `semantic-ctxt-scoped-types' to find types."
  (require 'semantic/db-typecache)
  (save-excursion
    (goto-char position)
    (let ((code-scoped-types nil))
      ;; Let's ask if any types are currently scoped.  Scoped
      ;; classes and types provide their public methods and types
      ;; in source code, but are unrelated hierarchically.
      (let ((sp (semantic-ctxt-scoped-types)))
	(while sp
	  ;; Get this thing as a tag
	  (let ((tmp (cond
		      ((stringp (car sp))
		       (semanticdb-typecache-find (car sp)))
		       ;(semantic-analyze-find-tag (car sp) 'type))
		      ((semantic-tag-p (car sp))
		       (if (semantic-analyze-tag-prototype-p (car sp))
			   (semanticdb-typecache-find (semantic-tag-name (car sp)))
			   ;;(semantic-analyze-find-tag (semantic-tag-name (car sp)) 'type)
			 (car sp)))
		      (t nil))))
	    (when tmp
	      (setq code-scoped-types
		    (cons tmp code-scoped-types))))
	  (setq  sp (cdr sp))))
      (setq code-scoped-types (nreverse code-scoped-types))

      (when code-scoped-types
	(semanticdb-typecache-merge-streams code-scoped-types nil))

      )))

;;------------------------------------------------------------
(define-overloadable-function semantic-analyze-scope-nested-tags (position scopedtypes)
  "Return a list of types in order of nesting for the context of POSITION.
If POSITION is in a method with a named parent, find that parent, and
identify it's scope via overlay instead.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found.")

(defun semantic-analyze-scope-nested-tags-default (position scopetypes)
  "Return a list of types in order of nesting for the context of POSITION.
If POSITION is in a method with a named parent, find that parent, and
identify it's scope via overlay instead.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found.
This only finds ONE immediate parent by name.  All other parents returned
are from nesting data types."
  (require 'semantic/analyze)
  (save-excursion
    (if position (goto-char position))
    (let* ((stack (reverse (semantic-find-tag-by-overlay (point))))
	   (tag (car stack))
	   (pparent (car (cdr stack)))
	   (returnlist nil)
	   )
      ;; In case of arg lists or some-such, throw out non-types.
      (while (and stack (not (semantic-tag-of-class-p pparent 'type)))
	(setq stack (cdr stack) pparent (car (cdr stack))))

      ;; Remove duplicates
      (while (member pparent scopetypes)
	(setq stack (cdr stack) pparent (car (cdr stack))))

      ;; Step 1:
      ;;    Analyze the stack of tags we are nested in as parents.
      ;;

      ;; If we have a pparent tag, let's go there
      ;; an analyze that stack of tags.
      (when (and pparent (semantic-tag-with-position-p pparent))
	(semantic-go-to-tag pparent)
	(setq stack (semantic-find-tag-by-overlay (point)))
	;; Step one, find the merged version of stack in the typecache.
	(let* ((stacknames (reverse (mapcar 'semantic-tag-name stack)))
	       (tc nil)
	       )
	  ;; @todo - can we use the typecache ability to
	  ;;         put a scope into a tag to do this?
	  (while (and stacknames
		      (setq tc (semanticdb-typecache-find
				(reverse stacknames))))
	    (setq returnlist (cons tc returnlist)
		  stacknames (cdr stacknames)))
	  (when (not returnlist)
	    ;; When there was nothing from the typecache, then just
	    ;; use what's right here.
	    (setq stack (reverse stack))
	    ;; Add things to STACK until we cease finding tags of class type.
	    (while (and stack (eq (semantic-tag-class (car stack)) 'type))
	      ;; Otherwise, just add this to the returnlist.
	      (setq returnlist (cons (car stack) returnlist))
	      (setq stack (cdr stack)))

	    (setq returnlist (nreverse returnlist))
	    ))
	)

      ;; Only do this level of analysis for functions.
      (when (eq (semantic-tag-class tag) 'function)
	;; Step 2:
	;;   If the function tag itself has a "parent" by name, then that
	;;   parent will exist in the scope we just calculated, so look it
	;;   up now.
	;;
	(let ((p (semantic-tag-function-parent tag)))
	  (when p
	    ;; We have a parent, search for it.
	    (let* ((searchnameraw (cond ((stringp p) p)
					((semantic-tag-p p)
					 (semantic-tag-name p))
					((and (listp p) (stringp (car p)))
					 (car p))))
		   (searchname (semantic-analyze-split-name searchnameraw))
		   (snlist (if (consp searchname)
			       searchname
			     (list searchname)))
		   (fullsearchname nil)

		   (miniscope (semantic-scope-cache "mini"))
		   ptag)

	      ;; Find the next entry in the referenced type for
	      ;; our function, and append to return list till our
	      ;; returnlist is empty.
	      (while snlist
		(setq fullsearchname
		      (append (mapcar 'semantic-tag-name returnlist)
			      (list (car snlist)))) ;; Next one
		(setq ptag
		      (semanticdb-typecache-find fullsearchname))

		(when (or (not ptag)
			  (not (semantic-tag-of-class-p ptag 'type)))
		  (let ((rawscope
			 (apply 'append
				(mapcar 'semantic-tag-type-members
					(cons (car returnlist) scopetypes)
					)))
			)
		    (oset miniscope parents returnlist) ;; Not really accurate, but close
		    (oset miniscope scope rawscope)
		    (oset miniscope fullscope rawscope)
		    (setq ptag
			  (semantic-analyze-find-tag searchnameraw
						     'type
						     miniscope
						     ))
		    ))

		(when ptag
		  (when (and (not (semantic-tag-p ptag))
			     (semantic-tag-p (car ptag)))
		    (setq ptag (car ptag)))
		  (setq returnlist (append returnlist (list ptag)))
		  )

		(setq snlist (cdr snlist)))
	      (setq returnlist returnlist)
	      )))
	)
      returnlist
      )))

(define-overloadable-function semantic-analyze-scope-lineage-tags (parents scopedtypes)
  "Return the full lineage of tags from PARENTS.
The return list is of the form ( TAG . PROTECTION ), where TAG is a tag,
and PROTECTION is the level of protection offered by the relationship.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found.")

(defun semantic-analyze-scope-lineage-tags-default (parents scopetypes)
  "Return the full lineage of tags from PARENTS.
The return list is of the form ( TAG . PROTECTION ), where TAG is a tag,
and PROTECTION is the level of protection offered by the relationship.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found."
  (let ((lineage nil)
	(miniscope (semantic-scope-cache "mini"))
	)
    (oset miniscope parents parents)
    (oset miniscope scope scopetypes)
    (oset miniscope fullscope scopetypes)

    (dolist (slp parents)
      (semantic-analyze-scoped-inherited-tag-map
       slp (lambda (newparent)
	     (let* ((pname (semantic-tag-name newparent))
		    (prot (semantic-tag-type-superclass-protection slp pname))
		    (effectiveprot (cond ((eq prot 'public)
					  ;; doesn't provide access to private slots?
					  'protected)
					 (t prot))))
	       (push (cons newparent effectiveprot) lineage)
	       ))
       miniscope))

    lineage))


;;------------------------------------------------------------

(define-overloadable-function semantic-analyze-scoped-tags (typelist parentlist)
  "Return accessible tags when TYPELIST and PARENTLIST is in scope.
Tags returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\".")

(defun semantic-analyze-scoped-tags-default (typelist halfscope)
  "Return accessible tags when TYPELIST and HALFSCOPE is in scope.
HALFSCOPE is the current scope partially initialized.
Tags returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\"."
  (let ((typelist2 nil)
	(currentscope nil)
	(parentlist (oref halfscope parents))
	(miniscope halfscope)
	)
    ;; Loop over typelist, and find and merge all namespaces matching
    ;; the names in typelist.
    (while typelist
      (let ((tt (semantic-tag-type (car typelist))))
	(when (and (stringp tt) (string= tt "namespace"))
	  ;; By using the typecache, our namespaces are pre-merged.
	  (setq typelist2 (cons (car typelist) typelist2))
	  ))
      (setq typelist (cdr typelist)))

    ;; Loop over the types (which should be sorted by position)
    ;; adding to the scopelist as we go, and using the scopelist
    ;; for additional searching!
    (while typelist2
      (oset miniscope scope currentscope)
      (oset miniscope fullscope currentscope)
      (setq currentscope (append
			  (semantic-analyze-scoped-type-parts (car typelist2)
							      miniscope)
			  currentscope))
      (setq typelist2 (cdr typelist2)))

    ;; Collect all the types (class, etc) that are in our heritage.
    ;; These are types that we can extract members from, not those
    ;; declared in using statements, or the like.
    ;; Get the PARENTS including nesting scope for this location.
    (while parentlist
      (oset miniscope scope currentscope)
      (oset miniscope fullscope currentscope)
      (setq currentscope (append
			  (semantic-analyze-scoped-type-parts (car parentlist)
							      miniscope)
			  currentscope))
      (setq parentlist (cdr parentlist)))

    ;; Loop over all the items, and collect any type constants.
    (let ((constants nil))
      (dolist (T currentscope)
	(setq constants (append constants
				(semantic-analyze-type-constants T)))
	)

      (setq currentscope (append currentscope constants)))

    currentscope))

;;------------------------------------------------------------
(define-overloadable-function  semantic-analyze-scope-calculate-access (type scope)
  "Calculate the access class for TYPE as defined by the current SCOPE.
Access is related to the :parents in SCOPE.  If type is a member of SCOPE
then access would be 'private.  If TYPE is inherited by a member of SCOPE,
the access would be 'protected.  Otherwise, access is 'public")

(defun semantic-analyze-scope-calculate-access-default (type scope)
  "Calculate the access class for TYPE as defined by the current SCOPE."
  (cond ((semantic-scope-cache-p scope)
	 (let ((parents (oref scope parents))
	       (parentsi (oref scope parentinheritance))
	       )
	   (catch 'moose
	     ;; Investigate the parent, and see how it relates to type.
	     ;; If these tags are basically the same, then we have full access.
	     (dolist (p parents)
	       (when (semantic-tag-similar-p type p)
		 (throw 'moose 'private))
	       )
	     ;; Look to see if type is in our list of inherited parents.
	     (dolist (pi parentsi)
	       ;; pi is a cons cell ( PARENT . protection)
	       (let ((pip (car pi))
		     (piprot (cdr pi)))
		 (when (semantic-tag-similar-p type pip)
		   (throw 'moose
			  ;; protection via inheritance means to pull out different
			  ;; bits based on protection labels in an opposite way.
			  (cdr (assoc piprot
				      '((public . private)
					(protected . protected)
					(private . public))))
			  )))
	       )
	     ;; Not in our parentage.  Is type a FRIEND?
	     (let ((friends (semantic-find-tags-by-class 'friend (semantic-tag-type-members type))))
	       (dolist (F friends)
		 (dolist (pi parents)
		   (if (string= (semantic-tag-name F) (semantic-tag-name pi))
		       (throw 'moose 'private))
		   )))
	     ;; Found nothing, return public
	     'public)
	   ))
	(t 'public)))

(defun semantic-completable-tags-from-type (type)
  "Return a list of slots that are valid completions from the list of SLOTS.
If a tag in SLOTS has a named parent, then that implies that the
tag is not something you can complete from within TYPE."
  (let ((allslots (semantic-tag-components type))
	(leftover nil)
	)
    (dolist (S allslots)
      (when (or (not (semantic-tag-of-class-p S 'function))
		(not (semantic-tag-function-parent S)))
	(setq leftover (cons S leftover)))
      )
    (nreverse leftover)))

(defun semantic-analyze-scoped-type-parts (type &optional scope noinherit protection)
  "Return all parts of TYPE, a tag representing a TYPE declaration.
SCOPE is the scope object.
NOINHERIT turns off searching of inherited tags.
PROTECTION specifies the type of access requested, such as 'public or 'private."
  (if (not type)
      nil
    (let* ((access (semantic-analyze-scope-calculate-access type scope))
	   ;; SLOTS are the slots directly a part of TYPE.
	   (allslots (semantic-completable-tags-from-type type))
	   (slots (semantic-find-tags-by-scope-protection
		   access
		   type allslots))
	   (fname (semantic-tag-file-name type))
	   ;; EXTMETH are externally defined methods that are still
	   ;; a part of this class.

	   ;; @TODO - is this line needed??  Try w/out for a while
	   ;; @note - I think C++ says no.  elisp might, but methods
	   ;;         look like defuns, so it makes no difference.
	   (extmeth nil) ; (semantic-tag-external-member-children type t))

	   ;; INHERITED are tags found in classes that our TYPE tag
	   ;; inherits from.  Do not do this if it was not requested.
	   (inherited (when (not noinherit)
			(semantic-analyze-scoped-inherited-tags type scope
								access)))
	   )
      (when (not (semantic-tag-in-buffer-p type))
	(let ((copyslots nil))
	  (dolist (TAG slots)
	    ;;(semantic--tag-put-property TAG :filename fname)
	    (if (semantic-tag-file-name TAG)
		;; If it has a filename, just go with it...
		(setq copyslots (cons TAG copyslots))
	      ;; Otherwise, copy the tag w/ the guessed filename.
	      (setq copyslots (cons (semantic-tag-copy TAG nil fname)
				    copyslots)))
	    )
	  (setq slots (nreverse copyslots))
	  ))
      ;; Flatten the database output.
      (append slots extmeth inherited)
      )))

(defun semantic-analyze-scoped-inherited-tags (type scope access)
  "Return all tags that TYPE inherits from.
Argument SCOPE specify additional tags that are in scope
whose tags can be searched when needed, OR it may be a scope object.
ACCESS is the level of access we filter on child supplied tags.
For languages with protection on specific methods or slots,
it should strip out those not accessible by methods of TYPE.
An ACCESS of 'public means not in a method of a subclass of type.
A value of 'private means we can access private parts of the originating
type."
  (let ((ret nil))
    (semantic-analyze-scoped-inherited-tag-map
     type (lambda (p)
	    (let* ((pname (semantic-tag-name p))
		   (protection (semantic-tag-type-superclass-protection
				type pname))
		   )
	      (if (and (eq access 'public) (not (eq protection 'public)))
		  nil ;; Don't do it.

		;; We can get some parts of this type.
		(setq ret (nconc ret
				 ;; Do not pull in inherited parts here.  Those
				 ;; will come via the inherited-tag-map fcn
				 (semantic-analyze-scoped-type-parts
				  p scope t protection))
		      ))))
     scope)
    ret))

(defun semantic-analyze-scoped-inherited-tag-map (type fcn scope)
  "Map all parents of TYPE to FCN.  Return tags of all the types.
Argument SCOPE specify additional tags that are in scope
whose tags can be searched when needed, OR it may be a scope object."
  (require 'semantic/analyze)
  (let* (;; PARENTS specifies only the superclasses and not
	 ;; interfaces.  Inheriting from an interfaces implies
	 ;; you have a copy of all methods locally.  I think.
	 (parents (semantic-tag-type-superclasses type))
	 ps pt
	 (tmpscope scope)
	 )
    (save-excursion

      ;; Create a SCOPE just for looking up the parent based on where
      ;; the parent came from.
      ;;
      ;; @TODO - Should we cache these mini-scopes around in Emacs
      ;;         for recycling later?  Should this become a helpful
      ;;         extra routine?
      (when (and parents (semantic-tag-with-position-p type))
	(save-excursion
	  ;; If TYPE has a position, go there and get the scope.
	  (semantic-go-to-tag type)

	  ;; We need to make a mini scope, and only include the misc bits
	  ;; that will help in finding the parent.  We don't really need
	  ;; to do any of the stuff related to variables and what-not.
	  (setq tmpscope (semantic-scope-cache "mini"))
	  (let* ( ;; Step 1:
		 (scopetypes (cons type (semantic-analyze-scoped-types (point))))
		 (parents (semantic-analyze-scope-nested-tags (point) scopetypes))
		 ;;(parentinherited (semantic-analyze-scope-lineage-tags parents scopetypes))
		 (lscope nil)
		 )
	    (oset tmpscope scopetypes scopetypes)
	    (oset tmpscope parents parents)
	    ;;(oset tmpscope parentinheritance parentinherited)

	    (when (or scopetypes parents)
	      (setq lscope (semantic-analyze-scoped-tags scopetypes tmpscope))
	      (oset tmpscope scope lscope))
	    (oset tmpscope fullscope (append scopetypes lscope parents))
	    )))
      ;; END creating tmpscope

      ;; Look up each parent one at a time.
      (dolist (p parents)
	(setq ps (cond ((stringp p) p)
		       ((and (semantic-tag-p p) (semantic-tag-prototype-p p))
			(semantic-tag-name p))
		       ((and (listp p) (stringp (car p)))
			p))
	      pt (condition-case nil
		     (or (semantic-analyze-find-tag ps 'type tmpscope)
			 ;; A backup hack.
			 (semantic-analyze-find-tag ps 'type scope))
		   (error nil)))

	(when pt
	  (funcall fcn pt)
	  ;; Note that we pass the original SCOPE in while recursing.
	  ;; so that the correct inheritance model is passed along.
	  (semantic-analyze-scoped-inherited-tag-map pt fcn scope)
	  )))
    nil))

;;; ANALYZER
;;
;; Create the scope structure for use in the Analyzer.
;;
;;;###autoload
(defun semantic-calculate-scope (&optional point)
  "Calculate the scope at POINT.
If POINT is not provided, then use the current location of point.
The class returned from the scope calculation is variable
`semantic-scope-cache'."
  (interactive)
  (if (not (and (featurep 'semantic/db) semanticdb-current-database))
      nil ;; Don't do anything...
    (require 'semantic/db-typecache)
    (if (not point) (setq point (point)))
    (when (called-interactively-p 'any)
      (semantic-fetch-tags)
      (semantic-scope-reset-cache))
    (save-excursion
      (goto-char point)
      (let* ((TAG  (semantic-current-tag))
	     (scopecache
	      (semanticdb-cache-get semanticdb-current-table
				    semantic-scope-cache))
	     )
	(when (not (semantic-equivalent-tag-p TAG (oref scopecache tag)))
	  (semantic-reset scopecache))
	(if (oref scopecache tag)
	    ;; Even though we can recycle most of the scope, we
	    ;; need to redo the local variables since those change
	    ;; as you move about the tag.
	    (condition-case nil
		(oset scopecache localvar (semantic-get-all-local-variables))
	      (error nil))

	  (let* (;; Step 1:
		 (scopetypes (semantic-analyze-scoped-types point))
		 (parents (semantic-analyze-scope-nested-tags point scopetypes))
		 (parentinherited (semantic-analyze-scope-lineage-tags
				   parents scopetypes))
		 )
	    (oset scopecache tag TAG)
	    (oset scopecache scopetypes scopetypes)
	    (oset scopecache parents parents)
	    (oset scopecache parentinheritance parentinherited)

	    (let* (;; Step 2:
		   (scope (when (or scopetypes parents)
			    (semantic-analyze-scoped-tags scopetypes scopecache))
			  )
		   ;; Step 3:
		   (localargs (semantic-get-local-arguments))
		   (localvar (condition-case nil
				 (semantic-get-all-local-variables)
			       (error nil)))
		   )

	      ;; Try looking for parents again.
	      (when (not parentinherited)
		(setq parentinherited (semantic-analyze-scope-lineage-tags
				       parents (append scopetypes scope)))
		(when parentinherited
		  (oset scopecache parentinheritance parentinherited)
		  ;; Try calculating the scope again with the new inherited parent list.
		  (setq scope (when (or scopetypes parents)
				(semantic-analyze-scoped-tags scopetypes scopecache))
			)))

	      ;; Fill out the scope.
	      (oset scopecache scope scope)
	      (oset scopecache fullscope (append scopetypes scope parents))
	      (oset scopecache localargs localargs)
	      (oset scopecache localvar localvar)
	      )))
	;; Make sure we become dependent on the typecache.
	(semanticdb-typecache-add-dependant scopecache)
	;; Handy debug output.
	(when (called-interactively-p 'any)
	  (require 'eieio-datadebug)
	  (data-debug-show scopecache))
	;; Return ourselves
	scopecache))))

(defun semantic-scope-find (name &optional class scope-in)
  "Find the tag with NAME, and optional CLASS in the current SCOPE-IN.
Searches various elements of the scope for NAME.  Return ALL the
hits in order, with the first tag being in the closest scope."
  (let ((scope (or scope-in (semantic-calculate-scope)))
	(ans nil))
    ;; Is the passed in scope really a scope?  if so, look through
    ;; the options in that scope.
    (if (semantic-scope-cache-p scope)
	(let* ((la
		;; This should be first, but bugs in the
		;; C parser will turn function calls into
		;; assumed int return function prototypes.  Yuck!
		(semantic-find-tags-by-name name (oref scope localargs)))
	       (lv
		(semantic-find-tags-by-name name (oref scope localvar)))
	       (fullscoperaw (oref scope fullscope))
	       (sc (semantic-find-tags-by-name name fullscoperaw))
	       (typescoperaw  (oref scope typescope))
	       (tsc (semantic-find-tags-by-name name typescoperaw))
	       )
	  (setq ans
		(if class
		    ;; Scan out things not of the right class.
		    (semantic-find-tags-by-class class (append la lv sc tsc))
		  (append la lv sc tsc))
		)

	  (when (and (not ans) (or typescoperaw fullscoperaw))
	    (let ((namesplit (semantic-analyze-split-name name)))
	      (when (consp namesplit)
		;; It may be we need to hack our way through type typescope.
		(while namesplit
		  (setq ans (append
			     (semantic-find-tags-by-name (car namesplit)
							 typescoperaw)
			     (semantic-find-tags-by-name (car namesplit)
							 fullscoperaw)
			     ))
		  (if (not ans)
		      (setq typescoperaw nil)
		    (when (cdr namesplit)
		      (setq typescoperaw (semantic-tag-type-members
					  (car ans)))))

		  (setq namesplit (cdr namesplit)))
		;; Once done, store the current typecache lookup
		(oset scope typescope
		      (append typescoperaw (oref scope typescope)))
		)))
	  ;; Return it.
	  ans)
      ;; Not a real scope.  Our scope calculation analyze parts of
      ;; what it finds, and needs to pass lists through to do it's work.
      ;; Tread that list as a singly entry.
      (if class
	  (semantic-find-tags-by-class class scope)
	scope)
      )))

;;; DUMP
;;
(defmethod semantic-analyze-show ((context semantic-scope-cache))
  "Insert CONTEXT into the current buffer in a nice way."
  (require 'semantic/analyze)
  (semantic-analyze-princ-sequence (oref context scopetypes) "-> ScopeTypes: " )
  (semantic-analyze-princ-sequence (oref context parents) "-> Parents: " )
  (semantic-analyze-princ-sequence (oref context scope) "-> Scope: " )
  ;;(semantic-analyze-princ-sequence (oref context fullscope) "Fullscope:  " )
  (semantic-analyze-princ-sequence (oref context localargs) "-> Local Args: " )
  (semantic-analyze-princ-sequence (oref context localvar) "-> Local Vars: " )
  )

(provide 'semantic/scope)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/scope"
;; End:

;;; semantic/scope.el ends here
