;;; semantic/db-typecache.el --- Manage Datatypes

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
;; Manage a datatype cache.
;;
;; For typed languages like C++ collect all known types from various
;; headers, merge namespaces, and expunge duplicates.
;;
;; It is likely this feature will only be needed for C/C++.

(require 'semantic)
(require 'semantic/db)
(require 'semantic/db-find)
(require 'semantic/analyze/fcn)

;; For semantic-find-tags-by-* macros
(eval-when-compile (require 'semantic/find))

(declare-function data-debug-insert-thing "data-debug")
(declare-function data-debug-new-buffer "data-debug")
(declare-function semantic-sort-tags-by-name-then-type-increasing "semantic/sort")
(declare-function semantic-scope-tag-clone-with-scope "semantic/scope")

;;; Code:


;;; TABLE TYPECACHE
;;;###autoload
(defclass semanticdb-typecache ()
  ((filestream :initform nil
	       :documentation
	       "Fully sorted/merged list of tags within this buffer.")
   (includestream :initform nil
		  :documentation
		  "Fully sorted/merged list of tags from this file's includes list.")
   (stream :initform nil
	   :documentation
	   "The searchable tag stream for this cache.
NOTE: Can I get rid of this?  Use a hashtable instead?")
   (dependants :initform nil
	       :documentation
	       "Any other object that is dependent on typecache results.
Said object must support `semantic-reset' methods.")
   ;; @todo - add some sort of fast-hash.
   ;; @note - Rebuilds in large projects already take a while, and the
   ;;     actual searches are pretty fast.  Really needed?
   )
  "Structure for maintaining a typecache.")

(defmethod semantic-reset ((tc semanticdb-typecache))
  "Reset the object IDX."
  (oset tc filestream nil)
  (oset tc includestream nil)

  (oset tc stream nil)

  (mapc 'semantic-reset (oref tc dependants))
  (oset tc dependants nil)
  )

(defmethod semanticdb-typecache-notify-reset ((tc semanticdb-typecache))
  "Do a reset from a notify from a table we depend on."
  (oset tc includestream nil)
  (mapc 'semantic-reset (oref tc dependants))
  (oset tc dependants nil)
  )

(defmethod semanticdb-partial-synchronize ((tc semanticdb-typecache)
					   new-tags)
  "Reset the typecache based on a partial reparse."
  (when (semantic-find-tags-by-class 'include new-tags)
    (oset tc includestream nil)
    (mapc 'semantic-reset (oref tc dependants))
    (oset tc dependants nil)
    )

  (when (semantic-find-tags-by-class 'type new-tags)
    ;; Reset our index
    (oset tc filestream nil)
    t ;; Return true, our core file tags have changed in a relevant way.
    )

  ;; NO CODE HERE
  )

(defun semanticdb-typecache-add-dependant (dep)
  "Add into the local typecache a dependant DEP."
  (let* ((table semanticdb-current-table)
	 ;;(idx (semanticdb-get-table-index table))
	 (cache (semanticdb-get-typecache table))
	 )
    (object-add-to-list cache 'dependants dep)))

(defun semanticdb-typecache-length (thing)
  "How long is THING?
Debugging function."
  (cond ((semanticdb-typecache-child-p thing)
	 (length (oref thing stream)))
	((semantic-tag-p thing)
	 (length (semantic-tag-type-members thing)))
	((and (listp thing) (semantic-tag-p (car thing)))
	 (length thing))
	((null thing)
	 0)
	(t -1)	))


(defmethod semanticdb-get-typecache ((table semanticdb-abstract-table))
  "Retrieve the typecache from the semanticdb TABLE.
If there is no table, create one, and fill it in."
  (semanticdb-refresh-table table)
  (let* ((idx (semanticdb-get-table-index table))
	 (cache (oref idx type-cache))
	 )

    ;; Make sure we have a cache object in the DB index.
    (when (not cache)
      ;; The object won't change as we fill it with stuff.
      (setq cache (semanticdb-typecache (semanticdb-full-filename table)))
      (oset idx type-cache cache))

    cache))

(defmethod semanticdb-have-typecache-p ((table semanticdb-abstract-table))
  "Return non-nil (the typecache) if TABLE has a pre-calculated typecache."
  (let* ((idx (semanticdb-get-table-index table)))
    (oref idx type-cache)))


;;; DATABASE TYPECACHE
;;
;; A full database can cache the types across its files.
;;
;; Unlike file based caches, this one is a bit simpler, and just needs
;; to get reset when a table gets updated.

;;;###autoload
(defclass semanticdb-database-typecache (semanticdb-abstract-db-cache)
  ((stream :initform nil
	   :documentation
	   "The searchable tag stream for this cache.")
   )
  "Structure for maintaining a typecache.")

(defmethod semantic-reset ((tc semanticdb-database-typecache))
  "Reset the object IDX."
  (oset tc stream nil)
  )

(defmethod semanticdb-synchronize ((cache semanticdb-database-typecache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  )

(defmethod semanticdb-partial-synchronize ((cache semanticdb-database-typecache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  )

(defmethod semanticdb-get-typecache ((db semanticdb-project-database))
  "Retrieve the typecache from the semantic database DB.
If there is no table, create one, and fill it in."
  (semanticdb-cache-get db semanticdb-database-typecache)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MERGING
;;
;; Managing long streams of tags representing data types.
;;
(defun semanticdb-typecache-apply-filename (file stream)
  "Apply the filename FILE to all tags in STREAM."
  (let ((new nil))
    (while stream
      (setq new (cons (semantic-tag-copy (car stream) nil file)
		      new))
      ;The below is handled by the tag-copy fcn.
      ;(semantic--tag-put-property (car new) :filename file)
      (setq stream (cdr stream)))
    (nreverse new)))


(defsubst semanticdb-typecache-safe-tag-members (tag)
  "Return a list of members for TAG that are safe to permute."
  (let ((mem (semantic-tag-type-members tag))
	(fname (semantic-tag-file-name tag)))
    (if fname
	(setq mem (semanticdb-typecache-apply-filename fname mem))
      (copy-sequence mem))))

(defsubst semanticdb-typecache-safe-tag-list (tags table)
  "Make the tag list TAGS found in TABLE safe for the typecache.
Adds a filename and copies the tags."
  (semanticdb-typecache-apply-filename
   (semanticdb-full-filename table)
   tags))

(defun semanticdb-typecache-faux-namespace (name members)
  "Create a new namespace tag with NAME and a set of MEMBERS.
The new tag will be a faux tag, used as a placeholder in a typecache."
  (let ((tag (semantic-tag-new-type name "namespace" members nil)))
    ;; Make sure we mark this as a fake tag.
    (semantic-tag-set-faux tag)
    tag))

(defun semanticdb-typecache-merge-streams (cache1 cache2)
  "Merge into CACHE1 and CACHE2 together.  The Caches will be merged in place."
  (if (or (and (not cache1) (not cache2))
	  (and (not (cdr cache1)) (not cache2))
	  (and (not cache1) (not (cdr cache2))))
      ;; If all caches are empty OR
      ;; cache1 is length 1 and no cache2 OR
      ;; no cache1 and length 1 cache2
      ;;
      ;; then just return the cache, and skip all this merging stuff.
      (or cache1 cache2)

    ;; Assume we always have datatypes, as this typecache isn't really
    ;; useful without a typed language.
    (require 'semantic/sort)
    (let ((S (semantic-sort-tags-by-name-then-type-increasing
	      ;; I used to use append, but it copied cache1 but not cache2.
	      ;; Since sort was permuting cache2, I already had to make sure
	      ;; the caches were permute-safe.  Might as well use nconc here.
	      (nconc cache1 cache2)))
	  (ans nil)
	  (next nil)
	  (prev nil)
	  (type nil))
      ;; With all the tags in order, we can loop over them, and when
      ;; two have the same name, we can either throw one away, or construct
      ;; a fresh new tag merging the items together.
      (while S
	(setq prev (car ans))
	(setq next (car S))
	(if (or
	     ;; CASE 1 - First item
	     (null prev)
	     ;; CASE 2 - New name
	     (not (string= (semantic-tag-name next)
			   (semantic-tag-name prev))))
	    (setq ans (cons next ans))
	  ;; ELSE - We have a NAME match.
	  (setq type (semantic-tag-type next))
	  (if (or (semantic-tag-of-type-p prev type) ; Are they the same datatype
		  (semantic-tag-faux-p prev)
		  (semantic-tag-faux-p next) ; or either a faux tag?
		  )
	      ;; Same Class, we can do a merge.
	      (cond
	       ((and (semantic-tag-of-class-p next 'type)
		     (string= type "namespace"))
		;; Namespaces - merge the children together.
		(setcar ans
			(semanticdb-typecache-faux-namespace
			 (semantic-tag-name prev) ; - they are the same
			 (semanticdb-typecache-merge-streams
			  (semanticdb-typecache-safe-tag-members prev)
			  (semanticdb-typecache-safe-tag-members next))
			 ))
		)
	       ((semantic-tag-prototype-p next)
		;; NEXT is a prototype... so keep previous.
		nil			; - keep prev, do nothing
		)
	       ((semantic-tag-prototype-p prev)
		;; PREV is a prototype, but not next.. so keep NEXT.
		;; setcar - set by side-effect on top of prev
		(setcar ans next)
		)
	       (t
		;;(message "Don't know how to merge %s.  Keeping first entry." (semantic-tag-name next))
		))
	    ;; Not same class... but same name
					;(message "Same name, different type: %s, %s!=%s"
					;	   (semantic-tag-name next)
					;	   (semantic-tag-type next)
					;        (semantic-tag-type prev))
	    (setq ans (cons next ans))
	    ))
	(setq S (cdr S)))
      (nreverse ans))))

;;; Refresh / Query API
;;
;; Queries that can be made for the typecache.
(define-overloadable-function semanticdb-expand-nested-tag (tag)
  "Expand TAG from fully qualified names.
If TAG has fully qualified names, expand it to a series of nested
namespaces instead."
  tag)

(defmethod semanticdb-typecache-file-tags ((table semanticdb-abstract-table))
  "No tags available from non-file based tables."
  nil)

(defmethod semanticdb-typecache-file-tags ((table semanticdb-table))
  "Update the typecache for TABLE, and return the file-tags.
File-tags are those that belong to this file only, and excludes
all included files."
  (let* (;(idx (semanticdb-get-table-index table))
	 (cache (semanticdb-get-typecache table))
	 )

    ;; Make sure our file-tags list is up to date.
    (when (not (oref cache filestream))
      (let ((tags  (semantic-find-tags-by-class 'type table))
	    (exptags nil))
	(when tags
	  (setq tags (semanticdb-typecache-safe-tag-list tags table))
	  (dolist (T tags)
	    (push (semanticdb-expand-nested-tag T) exptags))
	  (oset cache filestream (semanticdb-typecache-merge-streams exptags nil)))))

    ;; Return our cache.
    (oref cache filestream)
    ))

(defmethod semanticdb-typecache-include-tags ((table semanticdb-abstract-table))
  "No tags available from non-file based tables."
  nil)

(defmethod semanticdb-typecache-include-tags ((table semanticdb-table))
  "Update the typecache for TABLE, and return the merged types from the include tags.
Include-tags are the tags brought in via includes, all merged together into
a master list."
  (let* ((cache (semanticdb-get-typecache table))
	 )

    ;; Make sure our file-tags list is up to date.
    (when (not (oref cache includestream))
      (let (;; Calc the path first.  This will have a nice side -effect of
	    ;; getting the cache refreshed if a refresh is needed.  Most of the
	    ;; time this value is itself cached, so the query is fast.
	    (incpath (semanticdb-find-translate-path table nil))
	    (incstream nil))
	;; Get the translated path, and extract all the type tags, then merge
	;; them all together.
	(dolist (i incpath)
	  ;; don't include ourselves in this crazy list.
	  (when (and i (not (eq i table))
		     ;; @todo - This eieio fcn can be slow!  Do I need it?
		     ;; (semanticdb-table-child-p i)
		     )
	    (setq incstream
		  (semanticdb-typecache-merge-streams
		   incstream
		   ;; Getting the cache from this table will also cause this
		   ;; file to update its cache from its descendants.
		   ;;
		   ;; In theory, caches are only built for most includes
		   ;; only once (in the loop before this one), so this ends
		   ;; up being super fast as we edit our file.
		   (copy-sequence
		    (semanticdb-typecache-file-tags i))))
	    ))

	;; Save...
	(oset cache includestream incstream)))

    ;; Return our cache.
    (oref cache includestream)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Search Routines
;;
;;;###autoload
(define-overloadable-function semanticdb-typecache-find (type &optional path find-file-match)
  "Search the typecache for TYPE in PATH.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
PATH can be nil for the current buffer, or a semanticdb table.
FIND-FILE-MATCH is non-nil to force all found tags to be loaded into a buffer.")

(defun semanticdb-typecache-find-default (type &optional path find-file-match)
  "Default implementation of `semanticdb-typecache-find'.
TYPE is the datatype to find.
PATH is the search path, which should be one table object.
If FIND-FILE-MATCH is non-nil, then force the file belonging to the
found tag to be loaded."
  (if (not (and (featurep 'semantic/db) semanticdb-current-database))
      nil ;; No DB, no search
    (save-excursion
      (semanticdb-typecache-find-method (or path semanticdb-current-table)
					type find-file-match))))

(defun semanticdb-typecache-find-by-name-helper (name table)
  "Find the tag with NAME in TABLE, which is from a typecache.
If more than one tag has NAME in TABLE, we will prefer the tag that
is of class 'type."
  (let* ((names (semantic-find-tags-by-name name table))
	 (nmerge (semanticdb-typecache-merge-streams names nil))
	 (types (semantic-find-tags-by-class 'type nmerge)))
    (or (car-safe types) (car-safe nmerge))))

(defmethod semanticdb-typecache-find-method ((table semanticdb-abstract-table)
					     type find-file-match)
  "Search the typecache in TABLE for the datatype TYPE.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
If FIND-FILE-MATCH is non-nil, then force the file belonging to the
found tag to be loaded."
  ;; convert string to a list.
  (when (stringp type) (setq type (semantic-analyze-split-name type)))
  (when (stringp type) (setq type (list type)))

  ;; Search for the list in our typecache.
  (let* ((file (semanticdb-typecache-file-tags table))
	 (inc (semanticdb-typecache-include-tags table))
	 (stream nil)
	 (f-ans nil)
	 (i-ans nil)
	 (ans nil)
	 (notdone t)
	 (lastfile nil)
	 (thisfile nil)
	 (lastans nil)
	 (calculated-scope nil)
	 )
    ;; 1) Find first symbol in the two master lists and then merge
    ;;    the found streams.

    ;; We stripped duplicates, so these will be super-fast!
    (setq f-ans (semantic-find-first-tag-by-name (car type) file))
    (setq i-ans (semantic-find-first-tag-by-name (car type) inc))
    (if (and f-ans i-ans)
	(progn
	  ;; This trick merges the two identified tags, making sure our lists are
	  ;; complete.  The second find then gets the new 'master' from the list of 2.
	  (setq ans (semanticdb-typecache-merge-streams (list f-ans) (list i-ans)))
	  (setq ans (semantic-find-first-tag-by-name (car type) ans))
	  )

      ;; The answers are already sorted and merged, so if one misses,
      ;; no need to do any special work.
      (setq ans (or f-ans i-ans)))

    ;; 2) Loop over the remaining parts.
    (while (and type notdone)

      ;; For pass > 1, stream will be non-nil, so do a search, otherwise
      ;; ans is from outside the loop.
      (when stream
	(setq ans (semanticdb-typecache-find-by-name-helper (car type) stream))

	;; NOTE: The below test to make sure we get a type is only relevant
	;;       for the SECOND pass or later.  The first pass can only ever
	;;       find a type/namespace because everything else is excluded.

	;; If this is not the last entry from the list, then it
	;; must be a type or a namespace.  Let's double check.
	(when (cdr type)

	  ;; From above, there is only one tag in ans, and we prefer
	  ;; types.
	  (when (not (semantic-tag-of-class-p ans 'type))

	    (setq ans nil)))
	)

      (push ans calculated-scope)

      ;; Track most recent file.
      (setq thisfile (semantic-tag-file-name ans))
      (when (and thisfile (stringp thisfile))
	(setq lastfile thisfile))

      ;; If we have a miss, exit, otherwise, update the stream to
      ;; the next set of members.
      (if (not ans)
	  (setq notdone nil)
	(setq stream (semantic-tag-type-members ans)))

      (setq lastans ans
	    ans nil
	    type (cdr type)))

    (if (or type (not notdone))
	;; If there is stuff left over, then we failed.  Just return
	;; nothing.
	nil

      ;; We finished, so return everything.

      (if (and find-file-match lastfile)
	  ;; This won't liven up the tag since we have a copy, but
	  ;; we ought to be able to get there and go to the right line.
	  (find-file-noselect lastfile)
	;; We don't want to find-file match, so instead let's
	;; push the filename onto the return tag.
	(when lastans
	  (setq lastans (semantic-tag-copy lastans nil lastfile))
	  ;; We used to do the below, but we would erroneously be putting
	  ;; attributes on tags being shred with other lists.
	  ;;(semantic--tag-put-property lastans :filename lastfile)
	  )
	)

      (if (and lastans calculated-scope)

	  ;; Put our discovered scope into the tag if we have a tag
	  (progn
	    (require 'semantic/scope)
	    (semantic-scope-tag-clone-with-scope
	     lastans (reverse (cdr calculated-scope))))

	;; Else, just return
	lastans
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BRUTISH Typecache
;;
;; Routines for a typecache that crosses all tables in a given database
;; for a matching major-mode.
(defmethod semanticdb-typecache-for-database ((db semanticdb-project-database)
					      &optional mode)
  "Return the typecache for the project database DB.
If there isn't one, create it.
"
  (let ((lmode (or mode major-mode))
	(cache (semanticdb-get-typecache db))
	(stream nil)
	)
    (dolist (table (semanticdb-get-database-tables db))
      (when (eq lmode (oref table :major-mode))
	(setq stream
	      (semanticdb-typecache-merge-streams
	       stream
	       (copy-sequence
		(semanticdb-typecache-file-tags table))))
	))
    (oset cache stream stream)
    cache))

(defun semanticdb-typecache-refresh-for-buffer (buffer)
  "Refresh the typecache for BUFFER."
  (with-current-buffer buffer
    (let* ((tab semanticdb-current-table)
	   ;(idx (semanticdb-get-table-index tab))
	   (tc (semanticdb-get-typecache tab)))
      (semanticdb-typecache-file-tags tab)
      (semanticdb-typecache-include-tags tab)
      tc)))


;;; DEBUG
;;
(defun semanticdb-typecache-complete-flush ()
  "Flush all typecaches referenced by the current buffer."
  (interactive)
  (let* ((path (semanticdb-find-translate-path nil nil)))
    (dolist (P path)
      (oset P pointmax nil)
      (semantic-reset (semanticdb-get-typecache P)))))

(defun semanticdb-typecache-dump ()
  "Dump the typecache for the current buffer."
  (interactive)
  (require 'data-debug)
  (let* ((start (current-time))
	 (tc (semanticdb-typecache-refresh-for-buffer (current-buffer)))
	 (end (current-time))
	 )
    (data-debug-new-buffer "*TypeCache ADEBUG*")
    (message "Calculating Cache took %.2f seconds."
	     (semantic-elapsed-time start end))

    (data-debug-insert-thing tc "]" "")

    ))

(defun semanticdb-db-typecache-dump ()
  "Dump the typecache for the current buffer's database."
  (interactive)
  (require 'data-debug)
  (let* ((tab semanticdb-current-table)
	 (idx (semanticdb-get-table-index tab))
	 (junk (oset idx type-cache nil)) ;; flush!
	 (start (current-time))
	 (tc (semanticdb-typecache-for-database (oref tab parent-db)))
	 (end (current-time))
	 )
    (data-debug-new-buffer "*TypeCache ADEBUG*")
    (message "Calculating Cache took %.2f seconds."
	     (semantic-elapsed-time start end))

    (data-debug-insert-thing tc "]" "")

    ))

(provide 'semantic/db-typecache)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/db-typecache"
;; End:

;;; semantic/db-typecache.el ends here
