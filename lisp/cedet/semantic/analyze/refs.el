;;; semantic/analyze/refs.el --- Analysis of the references between tags.

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Analyze the references between tags.
;;
;; The original purpose of these analysis is to provide a way to jump
;; between a prototype and implementation.
;;
;; Finding all prototype/impl matches is hard because you have to search
;; through the entire set of allowed databases to capture all possible
;; refs.  The core analysis class stores basic starting point, and then
;; entire raw search data, which is expensive to calculate.
;;
;; Once the raw data is available, queries for impl, prototype, or
;; perhaps other things become cheap.

(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/db-find)
(eval-when-compile (require 'semantic/find))

(declare-function data-debug-new-buffer "data-debug")
(declare-function data-debug-insert-object-slots "eieio-datadebug")
(declare-function semantic-momentary-highlight-tag "semantic/decorate")

;;; Code:
(defclass semantic-analyze-references ()
  ((tag :initarg :tag
	:type semantic-tag
	:documentation
	"The starting TAG we are providing references analysis for.")
   (tagdb :initarg :tagdb
	  :documentation
	  "The database that tag can be found in.")
   (scope :initarg :scope
	  :documentation "A Scope object.")
   (rawsearchdata :initarg :rawsearchdata
		  :documentation
		  "The raw search data for TAG's name across all databases.")
   ;; Note: Should I cache queried data here?  I expect that searching
   ;; through rawsearchdata will be super-fast, so why bother?
   )
  "Class containing data from a semantic analysis.")

(define-overloadable-function semantic-analyze-tag-references (tag &optional db)
  "Analyze the references for TAG.
Returns a class with information about TAG.

Optional argument DB is a database.  It will be used to help
locate TAG.

Use `semantic-analyze-current-tag' to debug this fcn.")

(defun semantic-analyze-tag-references-default (tag &optional db)
  "Analyze the references for TAG.
Returns a class with information about TAG.

Optional argument DB is a database.  It will be used to help
locate TAG.

Use `semantic-analyze-current-tag' to debug this fcn."
  (when (not (semantic-tag-p tag))  (signal 'wrong-type-argument (list 'semantic-tag-p tag)))
  (let ((allhits nil)
	(scope nil)
	)
    (save-excursion
      (semantic-go-to-tag tag db)
      (setq scope (semantic-calculate-scope))

      (setq allhits (semantic--analyze-refs-full-lookup tag scope))

      (semantic-analyze-references (semantic-tag-name tag)
				    :tag tag
				    :tagdb db
				    :scope scope
				    :rawsearchdata allhits)
      )))

;;; METHODS
;;
;; These accessor methods will calculate the useful bits from the context, and cache values
;; into the context.
(defmethod semantic-analyze-refs-impl ((refs semantic-analyze-references) &optional in-buffer)
  "Return the implementations derived in the reference analyzer REFS.
Optional argument IN-BUFFER indicates that the returned tag should be in an active buffer."
  (let ((allhits (oref refs rawsearchdata))
	(tag (oref refs :tag))
	(impl nil)
	)
    (semanticdb-find-result-mapc
     (lambda (T DB)
       "Examine T in the database DB, and sont it."
       (let* ((ans (semanticdb-normalize-one-tag DB T))
	      (aT (cdr ans))
	      (aDB (car ans))
	      )
	 (when (and (not (semantic-tag-prototype-p aT))
		    (semantic-tag-similar-p tag aT :prototype-flag :parent))
	   (when in-buffer (save-excursion (semantic-go-to-tag aT aDB)))
	   (push aT impl))))
     allhits)
    impl))

(defmethod semantic-analyze-refs-proto ((refs semantic-analyze-references) &optional in-buffer)
  "Return the prototypes derived in the reference analyzer REFS.
Optional argument IN-BUFFER indicates that the returned tag should be in an active buffer."
  (let ((allhits (oref refs rawsearchdata))
	(tag (oref refs :tag))
	(proto nil))
    (semanticdb-find-result-mapc
     (lambda (T DB)
       "Examine T in the database DB, and sort it."
       (let* ((ans (semanticdb-normalize-one-tag DB T))
	      (aT (cdr ans))
	      (aDB (car ans))
	      )
	 (when (and (semantic-tag-prototype-p aT)
		    (semantic-tag-similar-p tag aT :prototype-flag :parent))
	   (when in-buffer (save-excursion (semantic-go-to-tag aT aDB)))
	   (push aT proto))))
     allhits)
    proto))

;;; LOOKUP
;;
(defun semantic--analyze-refs-full-lookup (tag scope)
  "Perform a full lookup for all occurrences of TAG in the current project.
TAG should be the tag currently under point.
SCOPE is the scope the cursor is in.  From this a list of parents is
derived.  If SCOPE does not have parents, then only a simple lookup is done."
  (if (not (oref scope parents))
      ;; If this tag has some named parent, but is not
      (semantic--analyze-refs-full-lookup-simple tag)

    ;; We have some sort of lineage we need to consider when we do
    ;; our side lookup of tags.
    (semantic--analyze-refs-full-lookup-with-parents tag scope)
    ))

(defun semantic--analyze-refs-find-child-in-find-results (find-results name class)
  "Find in FIND-RESULT a tag NAME which is a child of a tag in FIND-RESULTS.
CLASS is the class of the tag that ought to be returned."
  (let ((ans nil)
	(subans nil))
    ;; Loop over each segment of the find results.
    (dolist (FDB find-results)
      (setq subans nil)
      ;; Loop over each tag in the find results.
      (dolist (T (cdr FDB))
	;; For each tag, get the children.
	(let* ((chil (semantic-tag-type-members T))
	       (match (semantic-find-tags-by-name name chil)))
	  ;; Go over the matches, looking for matching tag class.
	  (dolist (M match)
	    (when (semantic-tag-of-class-p M class)
	      (push M subans)))))
      ;; Store current matches into a new find results.
      (when subans
	(push (cons (car FDB) subans) ans))
      )
    ans))

(defun semantic--analyze-refs-find-tags-with-parent (find-results parents)
  "Find in FIND-RESULTS all tags with PARENTS.
NAME is the name of the tag needing finding.
PARENTS is a list of names."
  (let ((ans nil) (usingnames nil))
    ;; Loop over the find-results passed in.
    (semanticdb-find-result-mapc
     (lambda (tag db)
       (let* ((p (semantic-tag-named-parent tag))
	      (ps (when (stringp p) (semantic-analyze-split-name p))))
	 (when (stringp ps) (setq ps (list ps)))
	 (when ps
	   ;; If there is a perfect match, then use it.
	   (if (equal ps parents)
	       (push (list db tag) ans))
	   ;; No match, find something from our list of using names.
	   ;; Do we need to split UN?
	   (save-excursion
	     (semantic-go-to-tag tag db)
	     (setq usingnames nil)
	     (let ((imports (semantic-ctxt-imported-packages)))
	       ;; Derive the names from all the using statements.
	       (mapc (lambda (T)
		       (setq usingnames
			     (cons (semantic-format-tag-name-from-anything T) usingnames)))
		     imports))
	     (dolist (UN usingnames)
	       (when (equal (cons UN ps) parents)
		 (push (list db tag) ans)
		 (setq usingnames (cdr usingnames))))
	     ))))
     find-results)
    ans))

(defun semantic--analyze-refs-full-lookup-with-parents (tag scope)
  "Perform a lookup for all occurrences of TAG based on TAG's SCOPE.
TAG should be the tag currently under point."
  (let* ((classmatch (semantic-tag-class tag))
	 (plist (mapcar (lambda (T) (semantic-tag-name T)) (oref scope parents)))
	 ;; The first item in the parent list
	 (name (car plist))
	 ;; Stuff from the simple list.
	 (simple (semantic--analyze-refs-full-lookup-simple tag t))
	 ;; Find all hits for the first parent name.
	 (brute (semanticdb-find-tags-collector
		 (lambda (table tags)
		   (semanticdb-deep-find-tags-by-name-method table name tags)
		   )
		 nil nil t))
	 ;; Prime the answer.
	 (answer (semantic--analyze-refs-find-tags-with-parent simple plist))
	 )
    ;; First parent is already search to initialize "brute".
    (setq plist (cdr plist))

    ;; Go through the list of parents, and try to find matches.
    ;; As we cycle through plist, for each level look for NAME,
    ;; and compare the named-parent, and also dive into the next item of
    ;; plist.
    (while (and plist brute)

      ;; Find direct matches
      (let* ((direct (semantic--analyze-refs-find-child-in-find-results
		      brute (semantic-tag-name tag) classmatch))
	     (pdirect (semantic--analyze-refs-find-tags-with-parent
		       direct plist)))
	(setq answer (append pdirect answer)))

      ;; The next set of search items.
      (setq brute (semantic--analyze-refs-find-child-in-find-results
		   brute (car plist) 'type))

      (setq plist (cdr plist)))

    ;; Brute now has the children from the very last match.
    (let* ((direct (semantic--analyze-refs-find-child-in-find-results
		    brute (semantic-tag-name tag) classmatch))
	   )
      (setq answer (append direct answer)))

    answer))

(defun semantic--analyze-refs-full-lookup-simple (tag &optional noerror)
  "Perform a simple  lookup for occurrences of TAG in the current project.
TAG should be the tag currently under point.
Optional NOERROR means don't throw errors on failure to find something.
This only compares the tag name, and does not infer any matches in namespaces,
or parts of some other data structure.
Only works for tags in the global namespace."
  (let* ((name (semantic-tag-name tag))
	 (brute (semanticdb-find-tags-collector
		 (lambda (table tags)
		   (semanticdb-find-tags-by-name-method table name tags)
		   )
		 nil ;; This may need to be the entire project??
		 nil t))
	 )

	(when (and (not brute) (not noerror))
	  ;; An error, because tag under point ought to be found.
	  (error "Cannot find any references to %s in wide search" name))

	(let* ((classmatch (semantic-tag-class tag))
	       (RES
		(semanticdb-find-tags-collector
		 (lambda (table tags)
		   (semantic-find-tags-by-class classmatch tags)
		   ;; @todo - Add parent check also.
		   )
		 brute nil)))

	  (when (and (not RES) (not noerror))
	    (error "Cannot find any definitions for %s in wide search"
		   (semantic-tag-name tag)))

	  ;; Return the matching tags and databases.
	  RES)))


;;; USER COMMANDS
;;
;;;###autoload
(defun semantic-analyze-current-tag ()
  "Analyze the tag under point."
  (interactive)
  (let* ((tag (semantic-current-tag))
	 (start (current-time))
	 (sac (semantic-analyze-tag-references tag))
	 (end (current-time))
	 )
    (message "Analysis took %.2f seconds." (semantic-elapsed-time start end))
    (if sac
	(progn
	  (require 'eieio-datadebug)
	  (data-debug-new-buffer "*Analyzer Reference ADEBUG*")
	  (data-debug-insert-object-slots sac "]"))
      (message "No Context to analyze here."))))

;;;###autoload
(defun semantic-analyze-proto-impl-toggle ()
  "Toggle between the implementation, and a prototype of tag under point."
  (interactive)
  (require 'semantic/decorate)
  (semantic-fetch-tags)
  (let* ((tag (semantic-current-tag))
	 (sar (if tag
		  (semantic-analyze-tag-references tag)
		(error "Point must be in a declaration")))
	 (target (if (semantic-tag-prototype-p tag)
		     (car (semantic-analyze-refs-impl sar t))
		   (car (semantic-analyze-refs-proto sar t))))
	 )

    (when (not target)
      (error "Could not find suitable %s"
	     (if (semantic-tag-prototype-p tag) "implementation" "prototype")))

    (push-mark)
    (semantic-go-to-tag target)
    (switch-to-buffer (current-buffer))
    (semantic-momentary-highlight-tag target))
  )

(provide 'semantic/analyze/refs)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/analyze/refs"
;; End:

;;; semantic/analyze/refs.el ends here
