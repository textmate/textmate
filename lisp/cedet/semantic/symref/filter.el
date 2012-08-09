;;; semantic/symref/filter.el --- Filter symbol reference hits for accuracy.

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

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
;; Filter symbol reference hits for accuracy.
;;
;; Most symbol referencing tools, such as find/grep only find matching
;; strings, but cannot determine the difference between an actual use,
;; and something else with a similar name, or even a string in a comment.
;;
;; This file provides utilities for filtering down to accurate matches
;; starting at a basic filter level that doesn't use symref, up to filters
;; across symref results.

;;; Code:

(require 'semantic)
(require 'semantic/analyze)
(declare-function srecode-active-template-region "srecode/fields")
(declare-function srecode-delete "srecode/fields")
(declare-function srecode-field "srecode/fields")
(declare-function srecode-template-inserted-region "srecode/fields")
(declare-function srecode-overlaid-activate "srecode/fields")
(declare-function semantic-idle-summary-useful-context-p "semantic/idle")

;;; FILTERS
;;
(defun semantic-symref-filter-hit (target &optional position)
  "Determine if the tag TARGET is used at POSITION in the current buffer.
Return non-nil for a match."
  (semantic-analyze-current-symbol
   (lambda (start end prefix)
     (let ((tag (car (nreverse prefix))))
       (and (semantic-tag-p tag)
	    (semantic-equivalent-tag-p target tag))))
   position))

;;; IN-BUFFER FILTERING

;; The following does filtering in-buffer only, and not against
;; a symref results object.

(defun semantic-symref-hits-in-region (target hookfcn start end)
  "Find all occurrences of the symbol TARGET that match TARGET the tag.
For each match, call HOOKFCN.
HOOKFCN takes three arguments that match
`semantic-analyze-current-symbol's use of HOOKFCN.
  ( START END PREFIX )

Search occurs in the current buffer between START and END."
  (require 'semantic/idle)
  (save-excursion
    (goto-char start)
    (let* ((str (semantic-tag-name target))
	   (case-fold-search semantic-case-fold)
	   (regexp (concat "\\<" (regexp-quote str) "\\>")))
      (while (re-search-forward regexp end t)
	(when (semantic-idle-summary-useful-context-p)
	  (semantic-analyze-current-symbol
	   (lambda (start end prefix)
	     (let ((tag (car (nreverse prefix))))
	       ;; check for semantic match on the text match.
	       (when (and (semantic-tag-p tag)
			  (semantic-equivalent-tag-p target tag))
		 (save-excursion
		   (funcall hookfcn start end prefix)))))
	   (point)))))))

(defun semantic-symref-rename-local-variable ()
  "Fancy way to rename the local variable under point.
Depends on the SRecode Field editing API."
  (interactive)
  ;; Do the replacement as needed.
  (let* ((ctxt (semantic-analyze-current-context))
	 (target (car (reverse (oref ctxt prefix))))
	 (tag (semantic-current-tag))
	 )

    (when (or (not target)
	      (not (semantic-tag-with-position-p target)))
      (error "Cannot identify symbol under point"))

    (when (not (semantic-tag-of-class-p target 'variable))
      (error "Can only rename variables"))

    (when (or (< (semantic-tag-start target) (semantic-tag-start tag))
	      (> (semantic-tag-end target) (semantic-tag-end tag)))
      (error "Can only rename variables declared in %s"
	     (semantic-tag-name tag)))

    ;; I think we're good for this example.  Give it a go through
    ;; our fancy interface from SRecode.
    (require 'srecode/fields)

    ;; Make sure there is nothing active.
    (let ((ar (srecode-active-template-region)))
      (when ar (srecode-delete ar)))

    (let ((srecode-field-archive nil)
	  (region nil)
	  )
      (semantic-symref-hits-in-region
       target (lambda (start end prefix)
		;; For every valid hit, create one field.
		(srecode-field "LOCAL" :name "LOCAL" :start start :end end))
       (semantic-tag-start tag) (semantic-tag-end tag))

      ;; Now that the fields are setup, create the region.
      (setq region (srecode-template-inserted-region
		    "REGION" :start (semantic-tag-start tag)
		    :end (semantic-tag-end tag)))

      ;; Activate the region.
      (srecode-overlaid-activate region)

      )
    ))

(provide 'semantic/symref/filter)

;;; semantic/symref/filter.el ends here
