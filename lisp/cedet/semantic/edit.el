;;; semantic/edit.el --- Edit Management for Semantic

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

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
;; In Semantic 1.x, changes were handled in a simplistic manner, where
;; tags that changed were reparsed one at a time.  Any other form of
;; edit were managed through a full reparse.
;;
;; This code attempts to minimize the number of times a full reparse
;; needs to occur.  While overlays and tags will continue to be
;; recycled in the simple case, new cases where tags are inserted
;; or old tags removed  from the original list are handled.
;;

;;; NOTES FOR IMPROVEMENT
;;
;; Work done by the incremental parser could be improved by the
;; following:
;;
;; 1. Tags created could have as a property an overlay marking a region
;;    of themselves that can be edited w/out affecting the definition of
;;    that tag.
;;
;; 2. Tags w/ positioned children could have a property of an
;;    overlay marking the region in themselves that contain the
;;    children.  This could be used to better improve splicing near
;;    the beginning and end of the child lists.
;;

;;; BUGS IN INCREMENTAL PARSER
;;
;; 1. Changes in the whitespace between tags could extend a
;;    following tag.  These will be marked as merely unmatched
;;    syntax instead.
;;
;; 2. Incremental parsing while a new function is being typed in
;;    sometimes gets a chance only when lists are incomplete,
;;    preventing correct context identification.

;;
(require 'semantic)

;;; Code:
(defvar semantic-after-partial-cache-change-hook nil
  "Normal hook run after the buffer cache has been updated.

This hook will run when the cache has been partially reparsed.
Partial reparses are incurred when a user edits a buffer, and only the
modified sections are rescanned.

Hook functions must take one argument, which is the list of tags
updated in the current buffer.

For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-change-hooks
  '(semantic-edits-change-function-handle-changes)
  "Abnormal hook run when semantic detects a change in a buffer.
Each hook function must take three arguments, identical to the
common hook `after-change-functions'.")

(defvar semantic-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as needing a reparse.
For language specific hooks, make sure you define this as a local hook.
Not used yet; part of the next generation reparse mechanism.")

(defvar semantic-no-reparse-needed-change-hook nil
  "Hooks run when a user edit is detected as not needing a reparse.
If the hook returns non-nil, then declare that a reparse is needed.
For language specific hooks, make sure you define this as a local hook.
Not used yet; part of the next generation reparse mechanism.")

(defvar semantic-edits-new-change-hooks nil
  "Abnormal hook run when a new change is found.
Functions must take one argument representing an overlay on that change.")

(defvar semantic-edits-delete-change-hooks nil
  "Abnormal hook run before a change overlay is deleted.
Deleted changes occur when multiple changes are merged.
Functions must take one argument representing an overlay being deleted.")

(defvar semantic-edits-move-change-hook nil
  "Abnormal hook run after a change overlay is moved.
Changes move when a new change overlaps an old change.  The old change
will be moved.
Functions must take one argument representing an overlay being moved.")

(defvar semantic-edits-reparse-change-hooks nil
  "Abnormal hook run after a change results in a reparse.
Functions are called before the overlay is deleted, and after the
incremental reparse.")

(defvar semantic-edits-incremental-reparse-failed-hook nil
  "Hook run after the incremental parser fails.
When this happens, the buffer is marked as needing a full reparse.")

(semantic-varalias-obsolete 'semantic-edits-incremental-reparse-failed-hooks
			    'semantic-edits-incremental-reparse-failed-hook "23.2")

(defcustom semantic-edits-verbose-flag nil
  "Non-nil means the incremental parser is verbose.
If nil, errors are still displayed, but informative messages are not."
  :group 'semantic
  :type 'boolean)

;;; Change State management
;;
;; Manage a series of overlays that define changes recently
;; made to the current buffer.
;;;###autoload
(defun semantic-change-function (start end length)
  "Provide a mechanism for semantic tag management.
Argument START, END, and LENGTH specify the bounds of the change."
   (setq semantic-unmatched-syntax-cache-check t)
   (let ((inhibit-point-motion-hooks t)
	 )
     (run-hook-with-args 'semantic-change-hooks start end length)
     ))

(defun semantic-changes-in-region (start end &optional buffer)
  "Find change overlays which exist in whole or in part between START and END.
Optional argument BUFFER is the buffer to search for changes in."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((ol (semantic-overlays-in (max start (point-min))
				    (min end (point-max))))
	  (ret nil))
      (while ol
	(when (semantic-overlay-get (car ol) 'semantic-change)
	  (setq ret (cons (car ol) ret)))
	(setq ol (cdr ol)))
      (sort ret #'(lambda (a b) (< (semantic-overlay-start a)
				   (semantic-overlay-start b)))))))

(defun semantic-edits-change-function-handle-changes  (start end length)
  "Run whenever a buffer controlled by `semantic-mode' change.
Tracks when and how the buffer is re-parsed.
Argument START, END, and LENGTH specify the bounds of the change."
  ;; We move start/end by one so that we can merge changes that occur
  ;; just before, or just after.  This lets simple typing capture everything
  ;; into one overlay.
  (let ((changes-in-change (semantic-changes-in-region (1- start) (1+ end)))
	)
    (semantic-parse-tree-set-needs-update)
    (if (not changes-in-change)
	(let ((o (semantic-make-overlay start end)))
	  (semantic-overlay-put o 'semantic-change t)
	  ;; Run the hooks safely.  When hooks blow it, our dirty
	  ;; function will be removed from the list of active change
	  ;; functions.
	  (condition-case nil
	      (run-hook-with-args 'semantic-edits-new-change-hooks o)
	    (error nil)))
      (let ((tmp changes-in-change))
	;; Find greatest bounds of all changes
	(while tmp
	  (when (< (semantic-overlay-start (car tmp)) start)
	    (setq start (semantic-overlay-start (car tmp))))
	  (when (> (semantic-overlay-end (car tmp)) end)
	    (setq end (semantic-overlay-end (car tmp))))
	  (setq tmp (cdr tmp)))
	;; Move the first found overlay, recycling that overlay.
	(semantic-overlay-move (car changes-in-change) start end)
	(condition-case nil
	    (run-hook-with-args 'semantic-edits-move-change-hooks
				(car changes-in-change))
	  (error nil))
	(setq changes-in-change (cdr changes-in-change))
	;; Delete other changes.  They are now all bound here.
	(while changes-in-change
	  (condition-case nil
	      (run-hook-with-args 'semantic-edits-delete-change-hooks
				  (car changes-in-change))
	    (error nil))
	  (semantic-overlay-delete (car changes-in-change))
	  (setq changes-in-change (cdr changes-in-change))))
      )))

(defsubst semantic-edits-flush-change (change)
  "Flush the CHANGE overlay."
  (condition-case nil
      (run-hook-with-args 'semantic-edits-delete-change-hooks
			  change)
    (error nil))
  (semantic-overlay-delete change))

(defun semantic-edits-flush-changes ()
  "Flush the changes in the current buffer."
  (let ((changes (semantic-changes-in-region (point-min) (point-max))))
    (while changes
      (semantic-edits-flush-change (car changes))
      (setq changes (cdr changes))))
  )

(defun semantic-edits-change-in-one-tag-p (change hits)
  "Return non-nil of the overlay CHANGE exists solely in one leaf tag.
HITS is the list of tags that CHANGE is in.  It can have more than
one tag in it if the leaf tag is within a parent tag."
  (and (< (semantic-tag-start (car hits))
	  (semantic-overlay-start change))
       (> (semantic-tag-end (car hits))
	  (semantic-overlay-end change))
       ;; Recurse on the rest.  If this change is inside all
       ;; of these tags, then they are all leaves or parents
       ;; of the smallest tag.
       (or (not (cdr hits))
	   (semantic-edits-change-in-one-tag-p change (cdr hits))))
  )

;;; Change/Tag Query functions
;;
;; A change (region of space) can effect tags in different ways.
;; These functions perform queries on a buffer to determine different
;; ways that a change effects a buffer.
;;
;; NOTE: After debugging these, replace below to no longer look
;;       at point and mark (via comments I assume.)
(defsubst semantic-edits-os (change)
  "For testing: Start of CHANGE, or smaller of (point) and (mark)."
  (if change (semantic-overlay-start change)
    (if (< (point) (mark)) (point) (mark))))

(defsubst semantic-edits-oe (change)
  "For testing: End of CHANGE, or larger of (point) and (mark)."
  (if change (semantic-overlay-end change)
    (if (> (point) (mark)) (point) (mark))))

(defun semantic-edits-change-leaf-tag (change)
  "A leaf tag which completely encompasses CHANGE.
If change overlaps a tag, but is not encompassed in it, return nil.
Use `semantic-edits-change-overlap-leaf-tag'.
If CHANGE is completely encompassed in a tag, but overlaps sub-tags,
return nil."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tags (nreverse
		  (semantic-find-tag-by-overlay-in-region
		   start end))))
    ;; A leaf is always first in this list
    (if (and tags
	     (<= (semantic-tag-start (car tags)) start)
	     (> (semantic-tag-end (car tags)) end))
	;; Ok, we have a match.  If this tag has children,
	;; we have to do more tests.
	(let ((chil (semantic-tag-components (car tags))))
	  (if (not chil)
	      ;; Simple leaf.
	      (car tags)
	    ;; For this type, we say that we encompass it if the
	    ;; change occurs outside the range of the children.
	    (if (or (not (semantic-tag-with-position-p (car chil)))
		    (> start (semantic-tag-end (nth (1- (length chil)) chil)))
		    (< end (semantic-tag-start (car chil))))
		;; We have modifications to the definition of this parent
		;; so we have to reparse the whole thing.
		(car tags)
	      ;; We actually modified an area between some children.
	      ;; This means we should return nil, as that case is
	      ;; calculated by someone else.
	      nil)))
      nil)))

(defun semantic-edits-change-between-tags (change)
  "Return a cache list of tags surrounding CHANGE.
The returned list is the CONS cell in the master list pointing to
a tag just before CHANGE.  The CDR will have the tag just after CHANGE.
CHANGE cannot encompass or overlap a leaf tag.
If CHANGE is fully encompassed in a tag that has children, and
this change occurs between those children, this returns non-nil.
See `semantic-edits-change-leaf-tag' for details on parents."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tags (nreverse
		  (semantic-find-tag-by-overlay-in-region
		   start end)))
	 (list-to-search nil)
         (found nil))
    (if (not tags)
	(setq list-to-search semantic--buffer-cache)
      ;; A leaf is always first in this list
      (if (and (< (semantic-tag-start (car tags)) start)
	       (> (semantic-tag-end (car tags)) end))
	  ;; We are completely encompassed in a tag.
	  (if (setq list-to-search
		    (semantic-tag-components (car tags)))
	      ;; Ok, we are completely encompassed within the first tag
	      ;; entry, AND that tag has children.  This means that change
	      ;; occurred outside of all children, but inside some tag
	      ;; with children.
	      (if (or (not (semantic-tag-with-position-p (car list-to-search)))
		      (> start (semantic-tag-end
				(nth (1- (length list-to-search))
				     list-to-search)))
		      (< end (semantic-tag-start (car list-to-search))))
		  ;; We have modifications to the definition of this parent
		  ;; and not between it's children.  Clear the search list.
		  (setq list-to-search nil)))
	;; Search list is nil.
	))
    ;; If we have a search list, let's go.  Otherwise nothing.
    (while (and list-to-search (not found))
      (if (cdr list-to-search)
          ;; We end when the start of the CDR is after the end of our
          ;; asked change.
          (if (< (semantic-tag-start (cadr list-to-search)) end)
              (setq list-to-search (cdr list-to-search))
            (setq found t))
        (setq list-to-search nil)))
    ;; Return it.  If it is nil, there is a logic bug, and we need
    ;; to avoid this bit of logic anyway.
    list-to-search
    ))

(defun semantic-edits-change-over-tags (change)
  "Return a cache list of tags surrounding a CHANGE encompassing tags.
CHANGE must not only include all overlapped tags (excepting possible
parent tags) in their entirety.  In this case, the change may be deleting
or moving whole tags.
The return value is a vector.
Cell 0 is a list of all tags completely encompassed in change.
Cell 1 is the cons cell into a master parser cache starting with
the cell which occurs BEFORE the first position of CHANGE.
Cell 2 is the parent of cell 1, or nil for the buffer cache.
This function returns nil if any tag covered by change is not
completely encompassed.
See `semantic-edits-change-leaf-tag' for details on parents."
  (let* ((start (semantic-edits-os change))
	 (end (semantic-edits-oe change))
	 (tags (nreverse
		  (semantic-find-tag-by-overlay-in-region
		   start end)))
	 (parent nil)
	 (overlapped-tags nil)
	 inner-start inner-end
	 (list-to-search nil))
    ;; By the time this is already called, we know that it is
    ;; not a leaf change, nor a between tag change.  That leaves
    ;; an overlap, and this condition.

    ;; A leaf is always first in this list.
    ;; Is the leaf encompassed in this change?
    (if (and tags
	     (>= (semantic-tag-start (car tags)) start)
	     (<= (semantic-tag-end (car tags)) end))
	(progn
	  ;; We encompass one whole change.
	  (setq overlapped-tags (list (car tags))
		inner-start (semantic-tag-start (car tags))
		inner-end (semantic-tag-end (car tags))
		tags (cdr tags))
	  ;; Keep looping while tags are inside the change.
	  (while (and tags
		      (>= (semantic-tag-start (car tags)) start)
		      (<= (semantic-tag-end (car tags)) end))

	    ;; Check if this new all-encompassing tag is a parent
	    ;; of that which went before.  Only check end because
	    ;; we know that start is less than inner-start since
	    ;; tags was sorted on that.
	    (if (> (semantic-tag-end (car tags)) inner-end)
		;; This is a parent.  Drop the children found
		;; so far.
		(setq overlapped-tags (list (car tags))
		      inner-start (semantic-tag-start (car tags))
		      inner-end (semantic-tag-end (car tags))
		      )
	      ;; It is not a parent encompassing tag
	      (setq overlapped-tags (cons (car tags)
					    overlapped-tags)
		    inner-start (semantic-tag-start (car tags))))
	    (setq tags (cdr tags)))
	  (if (not tags)
	      ;; There are no tags left, and all tags originally
	      ;; found are encompassed by the change.  Setup our list
	      ;; from the cache
	      (setq list-to-search semantic--buffer-cache);; We have a tag outside the list.  Check for
	    ;; We know we have a parent because it would
	    ;; completely cover the change.  A tag can only
	    ;; do that if it is a parent after we get here.
	    (when (and tags
		       (< (semantic-tag-start (car tags)) start)
		       (> (semantic-tag-end (car tags)) end))
	      ;; We have a parent.  Stuff in the search list.
	      (setq parent (car tags)
		    list-to-search (semantic-tag-components parent))
	      ;; If the first of TAGS is a parent (see above)
	      ;; then clear out the list.  All other tags in
	      ;; here must therefore be parents of the car.
	      (setq tags nil)
	      ;; One last check,  If start is before the first
	      ;; tag or after the last, we may have overlap into
	      ;; the characters that make up the definition of
	      ;; the tag we are parsing.
	      (when (or (semantic-tag-with-position-p (car list-to-search))
			(< start (semantic-tag-start
				  (car list-to-search)))
			(> end (semantic-tag-end
				(nth (1- (length list-to-search))
				     list-to-search))))
		;; We have a problem
		(setq list-to-search nil
		      parent nil))))

	  (when list-to-search

	    ;; Ok, return the vector only if all TAGS are
	    ;; confirmed as the lineage of `overlapped-tags'
	    ;; which must have a value by now.

	    ;; Loop over the search list to find the preceding CDR.
	    ;; Fortunately, (car overlapped-tags) happens to be
	    ;; the first tag positionally.
	    (let ((tokstart (semantic-tag-start (car overlapped-tags))))
	      (while (and list-to-search
			  ;; Assume always (car (cdr list-to-search)).
			  ;; A thrown error will be captured nicely, but
			  ;; that case shouldn't happen.

			  ;; We end when the start of the CDR is after the
			  ;; end of our asked change.
			  (cdr list-to-search)
			  (< (semantic-tag-start (car (cdr list-to-search)))
			     tokstart)
			  (setq list-to-search (cdr list-to-search)))))
	    ;; Create the return vector
	    (vector overlapped-tags
		    list-to-search
		    parent)
	    ))
      nil)))

;;; Default Incremental Parser
;;
;; Logic about how to group changes for effective reparsing and splicing.

(defun semantic-parse-changes-failed (&rest args)
  "Signal that Semantic failed to parse changes.
That is, display a message by passing all ARGS to `format', then throw
a 'semantic-parse-changes-failed exception with value t."
  (when semantic-edits-verbose-flag
    (message "Semantic parse changes failed: %S"
	     (apply 'format args)))
  (throw 'semantic-parse-changes-failed t))

(defsubst semantic-edits-incremental-fail ()
  "When the incremental parser fails, we mark that we need a full reparse."
  ;;(debug)
  (semantic-parse-tree-set-needs-rebuild)
  (when semantic-edits-verbose-flag
    (message "Force full reparse (%s)"
	     (buffer-name (current-buffer))))
  (run-hooks 'semantic-edits-incremental-reparse-failed-hook))

;;;###autoload
(defun semantic-edits-incremental-parser ()
  "Incrementally reparse the current buffer.
Incremental parser allows semantic to only reparse those sections of
the buffer that have changed.  This function depends on
`semantic-edits-change-function-handle-changes' setting up change
overlays in the current buffer.  Those overlays are analyzed against
the semantic cache to see what needs to be changed."
  (let ((changed-tags
         ;; Don't use `semantic-safe' here to explicitly catch errors
         ;; and reset the parse tree.
         (catch 'semantic-parse-changes-failed
           (if debug-on-error
               (semantic-edits-incremental-parser-1)
             (condition-case err
                 (semantic-edits-incremental-parser-1)
               (error
                (message "incremental parser error: %S"
			 (error-message-string err))
                t))))))
    (when (eq changed-tags t)
      ;; Force a full reparse.
      (semantic-edits-incremental-fail)
      (setq changed-tags nil))
    changed-tags))

(defmacro semantic-edits-assert-valid-region ()
  "Assert that parse-start and parse-end are sorted correctly."
;;;  (if (> parse-start parse-end)
;;;      (error "Bug is %s !> %d!  Buff min/max = [ %d %d ]"
;;;	     parse-start parse-end
;;;	     (point-min) (point-max)))
  )

(defun semantic-edits-incremental-parser-1 ()
  "Incrementally reparse the current buffer.
Return the list of tags that changed.
If the incremental parse fails, throw a 'semantic-parse-changes-failed
exception with value t, that can be caught to schedule a full reparse.
This function is for internal use by `semantic-edits-incremental-parser'."
  (let* ((changed-tags nil)
         (debug-on-quit t)            ; try to find this annoying bug!
         (changes (semantic-changes-in-region
                   (point-min) (point-max)))
         (tags nil)                   ;tags found at changes
         (newf-tags nil)              ;newfound tags in change
         (parse-start nil)              ;location to start parsing
         (parse-end nil)                ;location to end parsing
         (parent-tag nil)             ;parent of the cache list.
         (cache-list nil)               ;list of children within which
					;we incrementally reparse.
         (reparse-symbol nil)           ;The ruled we start at for reparse.
         (change-group nil)             ;changes grouped in this reparse
	 (last-cond nil)		;track the last case used.
					;query this when debugging to find
					;source of bugs.
         )
    (or changes
        ;; If we were called, and there are no changes, then we
        ;; don't know what to do.  Force a full reparse.
        (semantic-parse-changes-failed "Don't know what to do"))
    ;; Else, we have some changes.  Loop over them attempting to
    ;; patch things up.
    (while changes
      ;; Calculate the reparse boundary.
      ;; We want to take some set of changes, and group them
      ;; together into a small change group. One change forces
      ;; a reparse of a larger region (the size of some set of
      ;; tags it encompasses.)  It may contain several tags.
      ;; That region may have other changes in it (several small
      ;; changes in one function, for example.)
      ;; Optimize for the simple cases here, but try to handle
      ;; complex ones too.

      (while (and changes               ; we still have changes
                  (or (not parse-start)
                      ;; Below, if the change we are looking at
                      ;; is not the first change for this
                      ;; iteration, and it starts before the end
                      ;; of current parse region, then it is
                      ;; encompassed within the bounds of tags
                      ;; modified by the previous iteration's
                      ;; change.
                      (< (semantic-overlay-start (car changes))
                         parse-end)))

        ;; REMOVE LATER
        (if (eq (car changes) (car change-group))
            (semantic-parse-changes-failed
             "Possible infinite loop detected"))

        ;; Store this change in this change group.
        (setq change-group (cons (car changes) change-group))

        (cond
         ;; Is this is a new parse group?
         ((not parse-start)
	  (setq last-cond "new group")
          (let (tmp)
            (cond

;;;; Are we encompassed all in one tag?
             ((setq tmp (semantic-edits-change-leaf-tag (car changes)))
	      (setq last-cond "Encompassed in tag")
              (setq tags (list tmp)
                    parse-start (semantic-tag-start tmp)
                    parse-end (semantic-tag-end tmp)
                    )
	      (semantic-edits-assert-valid-region))

;;;; Did the change occur between some tags?
             ((setq cache-list (semantic-edits-change-between-tags
                                (car changes)))
	      (setq last-cond "Between and not overlapping tags")
              ;; The CAR of cache-list is the tag just before
              ;; our change, but wasn't modified.  Hmmm.
              ;; Bound our reparse between these two tags
              (setq tags nil
                    parent-tag
                    (car (semantic-find-tag-by-overlay
                          parse-start)))
              (cond
               ;; A change at the beginning of the buffer.
	       ;; Feb 06 -
	       ;; IDed when the first cache-list tag is after
	       ;; our change, meaning there is nothing before
	       ;; the change.
               ((> (semantic-tag-start (car cache-list))
                   (semantic-overlay-end (car changes)))
		(setq last-cond "Beginning of buffer")
                (setq parse-start
                      ;; Don't worry about parents since
                      ;; there there would be an exact
                      ;; match in the tag list otherwise
                      ;; and the routine would fail.
                      (point-min)
                      parse-end
                      (semantic-tag-start (car cache-list)))
		(semantic-edits-assert-valid-region)
                )
               ;; A change stuck on the first surrounding tag.
               ((= (semantic-tag-end (car cache-list))
                   (semantic-overlay-start (car changes)))
		(setq last-cond "Beginning of Tag")
                ;; Reparse that first tag.
                (setq parse-start
                      (semantic-tag-start (car cache-list))
                      parse-end
                      (semantic-overlay-end (car changes))
                      tags
                      (list (car cache-list)))
		(semantic-edits-assert-valid-region)
                )
               ;; A change at the end of the buffer.
               ((not (car (cdr cache-list)))
		(setq last-cond "End of buffer")
                (setq parse-start (semantic-tag-end
                                   (car cache-list))
                      parse-end (point-max))
		(semantic-edits-assert-valid-region)
                )
               (t
		(setq last-cond "Default")
                (setq parse-start
                      (semantic-tag-end (car cache-list))
                      parse-end
                      (semantic-tag-start (car (cdr cache-list)))
                      )
		(semantic-edits-assert-valid-region))))

;;;; Did the change completely overlap some number of tags?
             ((setq tmp (semantic-edits-change-over-tags
                         (car changes)))
	      (setq last-cond "Overlap multiple tags")
              ;; Extract the information
              (setq tags (aref tmp 0)
                    cache-list (aref tmp 1)
                    parent-tag (aref tmp 2))
              ;; We can calculate parse begin/end by checking
              ;; out what is in TAGS.  The one near start is
              ;; always first.  Make sure the reparse includes
              ;; the `whitespace' around the snarfed tags.
              ;; Since cache-list is positioned properly, use it
              ;; to find that boundary.
              (if (eq (car tags) (car cache-list))
                  ;; Beginning of the buffer!
                  (let ((end-marker (nth (length tags)
                                         cache-list)))
                    (setq parse-start (point-min))
                    (if end-marker
                        (setq parse-end
                              (semantic-tag-start end-marker))
                      (setq parse-end (semantic-overlay-end
                                       (car changes))))
		    (semantic-edits-assert-valid-region)
		    )
                ;; Middle of the buffer.
                (setq parse-start
                      (semantic-tag-end (car cache-list)))
                ;; For the end, we need to scoot down some
                ;; number of tags.  We 1+ the length of tags
                ;; because we want to skip the first tag
                ;; (remove 1-) then want the tag after the end
                ;; of the list (1+)
                (let ((end-marker (nth (1+ (length tags)) cache-list)))
                  (if end-marker
                      (setq parse-end (semantic-tag-start end-marker))
                    ;; No marker.  It is the last tag in our
                    ;; list of tags.  Only possible if END
                    ;; already matches the end of that tag.
                    (setq parse-end
                          (semantic-overlay-end (car changes)))))
		(semantic-edits-assert-valid-region)
                ))

;;;; Unhandled case.
             ;; Throw error, and force full reparse.
             ((semantic-parse-changes-failed "Unhandled change group")))
            ))
         ;; Is this change inside the previous parse group?
         ;; We already checked start.
         ((< (semantic-overlay-end (car changes)) parse-end)
	  (setq last-cond "in bounds")
          nil)
         ;; This change extends the current parse group.
         ;; Find any new tags, and see how to append them.
         ((semantic-parse-changes-failed
	   (setq last-cond "overlap boundary")
           "Unhandled secondary change overlapping boundary"))
         )
        ;; Prepare for the next iteration.
        (setq changes (cdr changes)))

      ;; By the time we get here, all TAGS are children of
      ;; some parent.  They should all have the same start symbol
      ;; since that is how the multi-tag parser works.  Grab
      ;; the reparse symbol from the first of the returned tags.
      ;;
      ;; Feb '06 - If reparse-symbol is nil, then they are top level
      ;;     tags.  (I'm guessing.)  Is this right?
      (setq reparse-symbol
            (semantic--tag-get-property (car (or tags cache-list))
                                        'reparse-symbol))
      ;; Find a parent if not provided.
      (and (not parent-tag) tags
           (setq parent-tag
                 (semantic-find-tag-parent-by-overlay
                  (car tags))))
      ;; We can do the same trick for our parent and resulting
      ;; cache list.
      (unless cache-list
	(if parent-tag
	    (setq cache-list
		  ;; We need to get all children in case we happen
		  ;; to have a mix of positioned and non-positioned
		  ;; children.
		  (semantic-tag-components parent-tag))
	  ;; Else, all the tags since there is no parent.
	  ;; It sucks to have to use the full buffer cache in
	  ;; this case because it can be big.  Failure to provide
	  ;; however results in a crash.
	  (setq cache-list semantic--buffer-cache)
	  ))
      ;; Use the boundary to calculate the new tags found.
      (setq newf-tags (semantic-parse-region
			 parse-start parse-end reparse-symbol))
      ;; Make sure all these tags are given overlays.
      ;; They have already been cooked by the parser and just
      ;; need the overlays.
      (let ((tmp newf-tags))
        (while tmp
          (semantic--tag-link-to-buffer (car tmp))
          (setq tmp (cdr tmp))))

      ;; See how this change lays out.
      (cond

;;;; Whitespace change
       ((and (not tags) (not newf-tags))
        ;; A change that occurred outside of any existing tags
        ;; and there are no new tags to replace it.
	(when semantic-edits-verbose-flag
	  (message "White space changes"))
        nil
        )

;;;; New tags in old whitespace area.
       ((and (not tags) newf-tags)
        ;; A change occurred outside existing tags which added
        ;; a new tag.  We need to splice these tags back
        ;; into the cache at the right place.
        (semantic-edits-splice-insert newf-tags parent-tag cache-list)

        (setq changed-tags
              (append newf-tags changed-tags))

	(when semantic-edits-verbose-flag
	  (message "Inserted tags: (%s)"
		   (semantic-format-tag-name (car newf-tags))))
        )

;;;; Old tags removed
       ((and tags (not newf-tags))
        ;; A change occurred where pre-existing tags were
        ;; deleted!  Remove the tag from the cache.
        (semantic-edits-splice-remove tags parent-tag cache-list)

        (setq changed-tags
              (append tags changed-tags))

        (when semantic-edits-verbose-flag
	  (message "Deleted tags: (%s)"
		   (semantic-format-tag-name (car tags))))
        )

;;;; One tag was updated.
       ((and (= (length tags) 1) (= (length newf-tags) 1))
        ;; One old tag was modified, and it is replaced by
        ;; One newfound tag.  Splice the new tag into the
        ;; position of the old tag.
        ;; Do the splice.
        (semantic-edits-splice-replace (car tags) (car newf-tags))
        ;; Add this tag to our list of changed toksns
        (setq changed-tags (cons (car tags) changed-tags))
        ;; Debug
        (when semantic-edits-verbose-flag
	  (message "Update Tag Table: %s"
		   (semantic-format-tag-name (car tags) nil t)))
        ;; Flush change regardless of above if statement.
        )

;;;; Some unhandled case.
       ((semantic-parse-changes-failed "Don't know what to do")))

      ;; We got this far, and we didn't flag a full reparse.
      ;; Clear out this change group.
      (while change-group
        (semantic-edits-flush-change (car change-group))
        (setq change-group (cdr change-group)))

      ;; Don't increment change here because an earlier loop
      ;; created change-groups.
      (setq parse-start nil)
      )
    ;; Mark that we are done with this glop
    (semantic-parse-tree-set-up-to-date)
    ;; Return the list of tags that changed.  The caller will
    ;; use this information to call hooks which can fix themselves.
    changed-tags))

;; Make it the default changes parser
;;;###autoload
(defalias 'semantic-parse-changes-default
  'semantic-edits-incremental-parser)

;;; Cache Splicing
;;
;; The incremental parser depends on the ability to parse up sections
;; of the file, and splice the results back into the cache.  There are
;; three types of splices.  A REPLACE, an ADD, and a REMOVE.  REPLACE
;; is one of the simpler cases, as the starting cons cell representing
;; the old tag can be used to auto-splice in.  ADD and REMOVE
;; require scanning the cache to find the correct location so that the
;; list can be fiddled.
(defun semantic-edits-splice-remove (oldtags parent cachelist)
  "Remove OLDTAGS from PARENT's CACHELIST.
OLDTAGS are tags in the current buffer, preferably linked
together also in CACHELIST.
PARENT is the parent tag containing OLDTAGS.
CACHELIST should be the children from PARENT, but may be
pre-positioned to a convenient location."
  (let* ((first (car oldtags))
	 (last (nth (1- (length oldtags)) oldtags))
	 (chil (if parent
		   (semantic-tag-components parent)
		 semantic--buffer-cache))
	 (cachestart cachelist)
	 (cacheend nil)
	 )
    ;; First in child list?
    (if (eq first (car chil))
	;; First tags in the cache are being deleted.
	(progn
	  (when semantic-edits-verbose-flag
	    (message "To Remove First Tag: (%s)"
		     (semantic-format-tag-name first)))
	  ;; Find the last tag
	  (setq cacheend chil)
	  (while (and cacheend (not (eq last (car cacheend))))
	    (setq cacheend (cdr cacheend)))
	  ;; The spliceable part is after cacheend.. so move cacheend
	  ;; one more tag.
	  (setq cacheend (cdr cacheend))
	  ;; Splice the found end tag into the cons cell
	  ;; owned by the current top child.
	  (setcar chil (car cacheend))
	  (setcdr chil (cdr cacheend))
	  (when (not cacheend)
	    ;; No cacheend.. then the whole system is empty.
	    ;; The best way to deal with that is to do a full
	    ;; reparse
	    (semantic-parse-changes-failed "Splice-remove failed.  Empty buffer?")
	    ))
      (message "To Remove Middle Tag: (%s)"
	       (semantic-format-tag-name first)))
    ;; Find in the cache the preceding tag
    (while (and cachestart (not (eq first (car (cdr cachestart)))))
      (setq cachestart (cdr cachestart)))
    ;; Find the last tag
    (setq cacheend cachestart)
    (while (and cacheend (not (eq last (car cacheend))))
      (setq cacheend (cdr cacheend)))
    ;; Splice the end position into the start position.
    ;; If there is no start, then this whole section is probably
    ;; gone.
    (if cachestart
	(setcdr cachestart (cdr cacheend))
      (semantic-parse-changes-failed "Splice-remove failed."))

    ;; Remove old overlays of these deleted tags
    (while oldtags
      (semantic--tag-unlink-from-buffer (car oldtags))
      (setq oldtags (cdr oldtags)))
    ))

(defun semantic-edits-splice-insert (newtags parent cachelist)
  "Insert NEWTAGS into PARENT using CACHELIST.
PARENT could be nil, in which case CACHLIST is the buffer cache
which must be updated.
CACHELIST must be searched to find where NEWTAGS are to be inserted.
The positions of NEWTAGS must be synchronized with those in
CACHELIST for this to work.  Some routines pre-position CACHLIST at a
convenient location, so use that."
  (let* ((start (semantic-tag-start (car newtags)))
	 (newtagendcell (nthcdr (1- (length newtags)) newtags))
	 (end (semantic-tag-end (car newtagendcell)))
	 )
    (if (> (semantic-tag-start (car cachelist)) start)
	;; We are at the beginning.
	(let* ((pc (if parent
		       (semantic-tag-components parent)
		     semantic--buffer-cache))
	       (nc (cons (car pc) (cdr pc)))  ; new cons cell.
	       )
	  ;; Splice the new cache cons cell onto the end of our list.
	  (setcdr newtagendcell nc)
	  ;; Set our list into parent.
	  (setcar pc (car newtags))
	  (setcdr pc (cdr newtags)))
      ;; We are at the end, or in the middle.  Find our match first.
      (while (and (cdr cachelist)
		  (> end (semantic-tag-start (car (cdr cachelist)))))
	(setq cachelist (cdr cachelist)))
      ;; Now splice into the list!
      (setcdr newtagendcell (cdr cachelist))
      (setcdr cachelist newtags))))

(defun semantic-edits-splice-replace (oldtag newtag)
  "Replace OLDTAG with NEWTAG in the current cache.
Do this by recycling OLDTAG's first CONS cell.  This effectively
causes the new tag to completely replace the old one.
Make sure that all information in the overlay is transferred.
It is presumed that OLDTAG and NEWTAG are both cooked.
When this routine returns, OLDTAG is raw, and the data will be
lost if not transferred into NEWTAG."
  (let* ((oo (semantic-tag-overlay oldtag))
	 (o (semantic-tag-overlay newtag))
	 (oo-props (semantic-overlay-properties oo)))
    (while oo-props
      (semantic-overlay-put o (car oo-props) (car (cdr oo-props)))
      (setq oo-props (cdr (cdr oo-props)))
      )
    ;; Free the old overlay(s)
    (semantic--tag-unlink-from-buffer oldtag)
    ;; Recover properties
    (semantic--tag-copy-properties oldtag newtag)
    ;; Splice into the main list.
    (setcdr oldtag (cdr newtag))
    (setcar oldtag (car newtag))
    ;; This important bit is because the CONS cell representing
    ;; OLDTAG is now pointing to NEWTAG, but the NEWTAG
    ;; cell is about to be abandoned.  Here we update our overlay
    ;; to point at the updated state of the world.
    (semantic-overlay-put o 'semantic oldtag)
    ))

(add-hook 'semantic-before-toplevel-cache-flush-hook
          #'semantic-edits-flush-changes)

(provide 'semantic/edit)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/edit"
;; End:

;;; semantic/edit.el ends here
