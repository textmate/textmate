;;; semantic/analyze/fcn.el --- Analyzer support functions.

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Analyzer support functions.

;;; Code:

(require 'semantic)
(eval-when-compile (require 'semantic/find))

(declare-function semanticdb-typecache-merge-streams "semantic/db-typecache")
(declare-function semantic-scope-find "semantic/scope")
(declare-function semantic-scope-set-typecache "semantic/scope")
(declare-function semantic-scope-tag-get-scope "semantic/scope")

;;; Small Mode Specific Options
;;
;; These queries allow a major mode to help the analyzer make decisions.
;;
(define-overloadable-function semantic-analyze-tag-prototype-p (tag)
  "Non-nil if TAG is a prototype."
  )

(defun semantic-analyze-tag-prototype-p-default (tag)
  "Non-nil if TAG is a prototype."
  (let ((p (semantic-tag-get-attribute tag :prototype-flag)))
    (cond
     ;; Trust the parser author.
     (p p)
     ;; Empty types might be a prototype.
     ((eq (semantic-tag-class tag) 'type)
      (not (semantic-tag-type-members tag)))
     ;; No other heuristics.
     (t nil))
    ))

;;------------------------------------------------------------

(define-overloadable-function semantic-analyze-split-name (name)
  "Split a tag NAME into a sequence.
Sometimes NAMES are gathered from the parser that are compounded,
such as in C++ where foo::bar means:
  \"The class BAR in the namespace FOO.\"
Return the string NAME for no change, or a list if it needs to be split.")

(defun semantic-analyze-split-name-default (name)
  "Don't split up NAME by default."
  name)

(define-overloadable-function semantic-analyze-unsplit-name (namelist)
  "Assemble a NAMELIST into a string representing a compound name.
Return the string representing the compound name.")

(defun semantic-analyze-unsplit-name-default (namelist)
  "Concatenate the names in NAMELIST with a . between."
  (mapconcat 'identity namelist "."))

;;; SELECTING
;;
;; If you narrow things down to a list of tags that all mean
;; the same thing, how to you pick one?  Select or merge.
;;

(defun semantic-analyze-select-best-tag (sequence &optional tagclass)
  "For a SEQUENCE of tags, all with good names, pick the best one.
If SEQUENCE is made up of namespaces, merge the namespaces together.
If SEQUENCE has several prototypes, find the non-prototype.
If SEQUENCE has some items w/ no type information, find the one with a type.
If SEQUENCE is all prototypes, or has no prototypes, get the first one.
Optional TAGCLASS indicates to restrict the return to only
tags of TAGCLASS."

  ;; If there is a srew up and we get just one tag.. massage over it.
  (when (semantic-tag-p sequence)
    (setq sequence (list sequence)))

  ;; Filter out anything not of TAGCLASS
  (when tagclass
    (setq sequence (semantic-find-tags-by-class tagclass sequence)))

  (if (< (length sequence) 2)
      ;; If the remaining sequence is 1 tag or less, just return it
      ;; and skip the rest of this mumbo-jumbo.
      (car sequence)

    ;; 1)
    ;; This step will eliminate a vast majority of the types,
    ;; in addition to merging namespaces together.
    ;;
    ;; 2)
    ;; It will also remove prototypes.
    (require 'semantic/db-typecache)
    (setq sequence (semanticdb-typecache-merge-streams sequence nil))

    (if (< (length sequence) 2)
	;; If the remaining sequence after the merge is 1 tag or less,
	;; just return it and skip the rest of this mumbo-jumbo.
	(car sequence)

      (let ((best nil)
	    (notypeinfo nil)
	    )
	(while (and (not best) sequence)

	  ;; 3) select a non-prototype.
	  (if (not (semantic-tag-type (car sequence)))
	      (setq notypeinfo (car sequence))

	    (setq best (car sequence))
	    )

	  (setq sequence (cdr sequence)))

	;; Select the best, or at least the prototype.
	(or best notypeinfo)))))

;;; Tag Finding
;;
;; Mechanism for lookup up tags by name.
;;
(defun semantic-analyze-find-tags-by-prefix (prefix)
  ;; @todo - only used in semantic-complete.  Find something better?
  "Attempt to find a tag with PREFIX.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
           (semanticdb-minor-mode-p))
      ;; Search the database & concatenate all matches together.
      (semanticdb-strip-find-results
       (semanticdb-find-tags-for-completion prefix)
       'name)
    ;; Search just this file because there is no DB available.
    (semantic-find-tags-for-completion
     prefix (current-buffer))))

;;; Finding Datatypes
;;

(define-overloadable-function semantic-analyze-dereference-metatype (type scope &optional type-declaration)
  ;; todo - move into typecache!!
  "Return a concrete type tag based on input TYPE tag.
A concrete type is an actual declaration of a memory description,
such as a structure, or class.  A meta type is an alias,
or a typedef in C or C++.  If TYPE is concrete, it
is returned.  If it is a meta type, it will return the concrete
type defined by TYPE.
The default behavior always returns TYPE.
Override functions need not return a real semantic tag.
Just a name, or short tag will be ok.  It will be expanded here.
SCOPE is the scope object with additional items in which to search for names."
  (catch 'default-behavior
    (let* ((ans-tuple (:override
                       ;; Nothing fancy, just return type by default.
                       (throw 'default-behavior (list type type-declaration))))
           (ans-type (car ans-tuple))
           (ans-type-declaration (cadr ans-tuple)))
       (list (semantic-analyze-dereference-metatype-1 ans-type scope) ans-type-declaration))))

;; Finding a data type by name within a project.
;;
(defun semantic-analyze-type-to-name (type)
  "Get the name of TAG's type.
The TYPE field in a tag can be nil (return nil)
or a string, or a non-positional tag."
  (cond ((semantic-tag-p type)
	 (semantic-tag-name type))
	((stringp type)
	 type)
	((listp type)
	 (car type))
	(t nil)))

(defun semantic-analyze-tag-type (tag &optional scope nometaderef)
  "Return the semantic tag for a type within the type of TAG.
TAG can be a variable, function or other type of tag.
The behavior of TAG's type is defined by `semantic-analyze-type'.
Optional SCOPE represents a calculated scope in which the
types might be found.  This can be nil.
If NOMETADEREF, then do not dereference metatypes.  This is
used by the analyzer debugger."
  (semantic-analyze-type (semantic-tag-type tag) scope nometaderef))

(defun semantic-analyze-type (type-declaration &optional scope nometaderef)
  "Return the semantic tag for TYPE-DECLARATION.
TAG can be a variable, function or other type of tag.
The type of tag (such as a class or struct) is a name.
Lookup this name in database, and return all slots/fields
within that types field.  Also handles anonymous types.
Optional SCOPE represents a calculated scope in which the
types might be found.  This can be nil.
If NOMETADEREF, then do not dereference metatypes.  This is
used by the analyzer debugger."
  (require 'semantic/scope)
  (let ((name nil)
	(typetag nil)
	)

    ;; Is it an anonymous type?
    (if (and type-declaration
	     (semantic-tag-p type-declaration)
	     (semantic-tag-of-class-p type-declaration 'type)
	     (not (semantic-analyze-tag-prototype-p type-declaration))
	     )
	;; We have an anonymous type for TAG with children.
	;; Use this type directly.
	(if nometaderef
	    type-declaration
	  (semantic-analyze-dereference-metatype-stack
	   type-declaration scope type-declaration))

      ;; Not an anonymous type.  Look up the name of this type
      ;; elsewhere, and report back.
      (setq name (semantic-analyze-type-to-name type-declaration))

      (if (and name (not (string= name "")))
	  (progn
	    ;; Find a type of that name in scope.
	    (setq typetag (and scope (semantic-scope-find name 'type scope)))
	    ;; If no typetag, try the typecache
	    (when (not typetag)
	      (setq typetag (semanticdb-typecache-find name))))

	;; No name to look stuff up with.
	(error "Semantic tag %S has no type information"
	       (semantic-tag-name type-declaration)))

      ;; Handle lists of tags.
      (when (and (consp typetag) (semantic-tag-p (car typetag)))
	(setq typetag (semantic-analyze-select-best-tag typetag 'type))
	)

      ;; We now have a tag associated with the type.  We need to deref it.
      ;;
      ;; If we were asked not to (ie - debugger) push the typecache anyway.
      (if nometaderef
	  typetag
	(unwind-protect
	    (progn
	      (semantic-scope-set-typecache
	       scope (semantic-scope-tag-get-scope typetag))
	      (semantic-analyze-dereference-metatype-stack typetag scope type-declaration)
	      )
	  (semantic-scope-set-typecache scope nil)
	  )))))

(defun semantic-analyze-dereference-metatype-stack (type scope &optional type-declaration)
  "Dereference metatypes repeatedly until we hit a real TYPE.
Uses `semantic-analyze-dereference-metatype'.
Argument SCOPE is the scope object with additional items in which to search.
Optional argument TYPE-DECLARATION is how TYPE was found referenced."
  (let ((lasttype type)
        (lasttypedeclaration type-declaration)
	(nexttype (semantic-analyze-dereference-metatype type scope type-declaration))
	(idx 0))
    (catch 'metatype-recursion
      (while (and nexttype (not (eq (car nexttype) lasttype)))
	(setq lasttype (car nexttype)
	      lasttypedeclaration (cadr nexttype))
	(setq nexttype (semantic-analyze-dereference-metatype lasttype scope lasttypedeclaration))
	(setq idx (1+ idx))
	(when (> idx 20) (message "Possible metatype recursion for %S"
				  (semantic-tag-name lasttype))
	      (throw 'metatype-recursion nil))
	))
    lasttype))

;; @ TODO - the typecache can also return a stack of scope names.

(defun semantic-analyze-dereference-metatype-1 (ans scope)
  "Do extra work after dereferencing a metatype.
ANS is the answer from the language specific query.
SCOPE is the current scope."
  (require 'semantic/scope)
  ;; If ANS is a string, or if ANS is a short tag, we
  ;; need to do some more work to look it up.
  (if (stringp ans)
      ;; The metatype is just a string... look it up.
      (or (and scope (car-safe
		      ;; @todo - should this be `find the best one'?
		      (semantic-scope-find ans 'type scope)))
	  (let ((tcsans nil))
	    (prog1
		(setq tcsans
		      (semanticdb-typecache-find ans))
	      ;; While going through the metatype, if we have
	      ;; a scope, push our new cache in.
	      (when scope
		(semantic-scope-set-typecache
		 scope (semantic-scope-tag-get-scope tcsans))
		))
	    ))
    (when (and (semantic-tag-p ans)
	       (eq (semantic-tag-class ans) 'type))
      ;; We have a tag.
      (if (semantic-analyze-tag-prototype-p ans)
	  ;; It is a prototype.. find the real one.
	  (or (and scope
		   (car-safe
		    (semantic-scope-find (semantic-tag-name ans)
					 'type scope)))
	      (let ((tcsans nil))
		(prog1
		    (setq tcsans
			  (semanticdb-typecache-find (semantic-tag-name ans)))
		  ;; While going through the metatype, if we have
		  ;; a scope, push our new cache in.
		  (when scope
		    (semantic-scope-set-typecache
		     scope (semantic-scope-tag-get-scope tcsans))
		    ))))
	;; We have a tag, and it is not a prototype.
	ans))
    ))

(provide 'semantic/analyze/fcn)

;;; semantic/analyze/fcn.el ends here
