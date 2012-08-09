;;; semantic/analyze/debug.el --- Debug the analyzer

;;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Provide a top-order debugging tool for figuring out what's going on with
;; smart completion and analyzer mode.

(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/analyze/complete)
(require 'semantic/db-typecache)

;; For semantic-find-tags-by-class:
(eval-when-compile (require 'semantic/find))

(declare-function ede-get-locator-object "ede/files")

;;; Code:

(defun semantic-analyze-debug-assist ()
  "Debug semantic analysis at the current point."
  (interactive)
  (let ((actualfcn (fetch-overload 'semantic-analyze-current-context))
	(ctxt (semantic-analyze-current-context))
	)
    ;; What to show.
    (if actualfcn
	(message "Mode %s does not use the default analyzer."
		 major-mode)
      ;; Debug our context.
      )
    (or (semantic-analyzer-debug-test-local-context)
	(and ctxt (semantic-analyzer-debug-found-prefix ctxt))
	)

    ))

;; @TODO - If this happens, but the last found type is
;; a datatype, then the below is wrong
(defun semantic-analyzer-debug-found-prefix (ctxt)
  "Debug the prefix found by the analyzer output CTXT."
  (let* ((pf (oref ctxt prefix))
	 (pft (oref ctxt prefixtypes))
	 (idx 0)
	 (stop nil)
	 (comp (condition-case nil
		   (semantic-analyze-possible-completions ctxt)
		 (error nil)))
	 )
    (while (and (nth idx pf) (not stop))
      (let ((pentry (nth idx pf))
	    (ptentry (nth idx pft)))
	(if (or (stringp pentry) (not ptentry))
	    ;; Found something ok.  Stop.
	    (setq stop t)
	  (setq idx (1+ idx)))))
    ;; We found the first non-tag entry.  What is the situation?
    (cond
     ((and (eq idx 0) (stringp (car pf)))
      ;; First part, we couldn't find it.
      (semantic-analyzer-debug-global-symbol ctxt (car pf) comp))
     ((not (nth (1- idx) pft)) ;; idx can't be 0 here.
      ;; The previous entry failed to have an identifiable data
      ;; type, which is a global search.
      (semantic-analyzer-debug-missing-datatype ctxt idx comp))
     ((and (nth (1- idx) pft) (stringp (nth idx pf)))
      ;; Non-first search, didn't find string in known data type.
      (semantic-analyzer-debug-missing-innertype ctxt idx comp))
     (t
      ;; Things are ok?
      (message "Things look ok."))
    )))

(defun semantic-analyzer-debug-global-symbol (ctxt prefix comp)
  "Debug why we can't find the first entry in the CTXT PREFIX.
Argument COMP are possible completions here."
  (let ((tab semanticdb-current-table)
	(finderr nil)
	(origbuf (current-buffer))
	)
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	(princ "Unable to find symbol ")
	(princ prefix)
	(princ ".\n\n")

	;; NOTE: This line is copied from semantic-analyze-current-context.
	;;       You will need to update both places.
	(condition-case err
	    (with-current-buffer origbuf
	      (let* ((position (or (cdr-safe (oref ctxt bounds)) (point)))
		     (prefixtypes nil) ; Used as type return
		     (scope (semantic-calculate-scope position))
		     )
		(semantic-analyze-find-tag-sequence
		 (list prefix "") scope 'prefixtypes)
		)
	      )
	  (error (setq finderr err)))

	(if finderr
	    (progn
	      (princ "The prefix lookup code threw the following error:\n  ")
	      (prin1 finderr)
	      (princ "\n\nTo debug this error you can do this:
  M-x toggle-debug-on-error RET
and then re-run the debug analyzer.\n")
	      )
	  ;; No find error, just not found
	  (princ "The prefix ")
	  (princ prefix)
	  (princ " could not be found in the local scope,
nor in any search tables.\n")
	  )
	(princ "\n")

	;; Describe local scope, and why we might not be able to
	;; find it.
	(semantic-analyzer-debug-describe-scope ctxt)

	(semantic-analyzer-debug-show-completions comp)

	(princ "When Semantic cannot find a symbol, it could be because the include
path was setup incorrectly.\n")

	(semantic-analyzer-debug-insert-include-summary tab)

	))
    (semantic-analyzer-debug-add-buttons)
    ))

(defun semantic-analyzer-debug-missing-datatype (ctxt idx comp)
  "Debug why we can't find a datatype entry for CTXT prefix at IDX.
Argument COMP are possible completions here."
  (let* ((prefixitem (nth idx (oref ctxt prefix)))
	 (dt (nth (1- idx) (oref ctxt prefixtypes)))
	 (tt (semantic-tag-type prefixitem))
	 (tab semanticdb-current-table)
	 )
    (when dt (error "Missing Datatype debugger is confused"))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	(princ "Unable to find datatype for: \"")
	(princ (semantic-format-tag-prototype prefixitem))
	(princ "\".
Declared type is: ")
	(when (semantic-tag-p tt)
	  (semantic-analyzer-debug-insert-tag tt)
	  (princ "\nRaw data type is: "))
	(princ (format "%S" tt))
	(princ "

Semantic could not find this data type in any of its global tables.

Semantic locates datatypes through either the local scope, or the global
typecache.
")

	;; Describe local scope, and why we might not be able to
	;; find it.
	(semantic-analyzer-debug-describe-scope ctxt '(type))

	;; Describe the typecache.
	(princ "\nSemantic creates and maintains a type cache for each buffer.
If the type is a global type, then it should appear in they typecache.
To examine the typecache, type:

  M-x semanticdb-typecache-dump RET

Current typecache Statistics:\n")
	(princ (format "   %4d types global in this file\n   %4d types from includes.\n"
		       (length (semanticdb-typecache-file-tags tab))
		       (length (semanticdb-typecache-include-tags tab))))

	(princ "\nIf the datatype is not in the typecache, then your include
path may be incorrect.  ")

	(semantic-analyzer-debug-insert-include-summary tab)

	;; End with-buffer
	))
    (semantic-analyzer-debug-add-buttons)
    ))

(defun semantic-analyzer-debug-missing-innertype (ctxt idx comp)
  "Debug why we can't find an entry for CTXT prefix at IDX for known type.
We need to see if we have possible completions against the entry before
being too vocal about it.
Argument COMP are possible completions here."
  (let* ((prefixitem (nth idx (oref ctxt prefix)))
	 (prevprefix (nth (1- idx) (oref ctxt prefix)))
	 (dt (nth (1- idx) (oref ctxt prefixtypes)))
	 (desired-type (semantic-analyze-type-constraint ctxt))
	 (orig-buffer (current-buffer))
	 (ots (semantic-analyze-tag-type prevprefix
					 (oref ctxt scope)
					 t ; Don't deref
					 ))
	 )
    (when (not dt) (error "Missing Innertype debugger is confused"))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer standard-output
	(princ "Cannot find symbol \"")
	(princ prefixitem)
	(princ "\" in datatype:
  ")
	(semantic-analyzer-debug-insert-tag dt)
	(princ "\n")

	(cond
	 ;; Any language with a namespace.
	 ((string= (semantic-tag-type dt) "namespace")
	  (princ "Semantic may not have found all possible namespaces with
the name ")
	  (princ (semantic-tag-name dt))
	  (princ ".  You can debug the entire typecache, including merged namespaces
with the command:

  M-x semanticdb-typecache-dump RET")
	  )

	 ;; @todo - external declarations??
	 (nil
	  nil)

	 ;; A generic explanation
	 (t
	  (princ "\nSemantic has found the datatype ")
	  (semantic-analyzer-debug-insert-tag dt)
	  (if (or (not (semantic-equivalent-tag-p ots dt))
		  (not (with-current-buffer orig-buffer
			 (car (semantic-analyze-dereference-metatype
			  ots (oref ctxt scope))))))
	      (let ((lasttype ots)
		    (nexttype (with-current-buffer orig-buffer
				(car (semantic-analyze-dereference-metatype
				 ots (oref ctxt scope))))))
		(if (eq nexttype lasttype)
		    (princ "\n  [ Debugger error trying to help with metatypes ]")

		  (if (eq ots dt)
		      (princ "\nwhich is a metatype")
		    (princ "\nwhich is derived from metatype ")
		    (semantic-analyzer-debug-insert-tag lasttype)))

		(princ ".\nThe Metatype stack is:\n")
		(princ "   ")
		(semantic-analyzer-debug-insert-tag lasttype)
		(princ "\n")
		(while (and nexttype
			    (not (eq nexttype lasttype)))
		  (princ "   ")
		  (semantic-analyzer-debug-insert-tag nexttype)
		  (princ "\n")
		  (setq lasttype nexttype
			nexttype
			(with-current-buffer orig-buffer
			  (car (semantic-analyze-dereference-metatype
			   nexttype (oref ctxt scope)))))
		  )
		(when (not nexttype)
		  (princ "   nil\n\n")
		  (princ
		   "Last metatype is nil.  This means that semantic cannot derive
the list of members because the type referred to cannot be found.\n")
		  )
		)
	    (princ "\nand its list of members.")

	    (if (not comp)
		(progn
		  (princ "  Semantic does not know what
possible completions there are for \"")
		  (princ prefixitem)
		  (princ "\".  Examine the known
members below for more."))
	      (princ "  Semantic knows of some
possible completions for \"")
	      (princ prefixitem)
	      (princ "\".")))
	  )
	 ;; end cond
	 )

	(princ "\n")
	(semantic-analyzer-debug-show-completions comp)

	(princ "\nKnown members of ")
	(princ (semantic-tag-name dt))
	(princ ":\n")
	(dolist (M (semantic-tag-type-members dt))
	  (princ "  ")
	  ;;(princ (semantic-format-tag-prototype M))
	  (semantic-analyzer-debug-insert-tag M)
	  (princ "\n"))

	;; This doesn't refer to in-type completions.
	;;(semantic-analyzer-debug-global-miss-text prefixitem)

	;; More explanation
	(when desired-type
	  (princ "\nWhen there are known members that would make good completion
candidates that are not in the completion list, then the most likely
cause is a type constraint.  Semantic has determined that there is a
type constraint looking for the type ")
	  (if (semantic-tag-p desired-type)
	      (semantic-analyzer-debug-insert-tag desired-type)
	    (princ (format "%S" desired-type)))
	  (princ "."))
	))
    (semantic-analyzer-debug-add-buttons)

    ))


(defun semantic-analyzer-debug-test-local-context ()
  "Test the local context parsed from the file."
  (let* ((prefixandbounds (semantic-ctxt-current-symbol-and-bounds (point)))
	 (prefix (car prefixandbounds))
	 (bounds (nth 2 prefixandbounds))
	 )
    (when (and (or (not prefixandbounds)
		   (not prefix)
		   (not bounds))
	       )
      (with-output-to-temp-buffer (help-buffer)
	(with-current-buffer standard-output
	  (princ "Local Context Parser Failed.

If this is unexpected, then there is likely a bug in the Semantic
local context parser.

Consider debugging the function ")
	  (let ((lcf (fetch-overload 'semantic-ctxt-current-symbol-and-bounds)))
	    (if lcf
		(princ (symbol-name lcf))
	      (princ "semantic-ctxt-current-symbol-and-bounds,
or implementing a version specific to ")
	      (princ (symbol-name major-mode))
	      )
	    (princ ".\n"))
	  (semantic-analyzer-debug-add-buttons)
	t)))
    ))

;;; General Inserters with help
;;
(defun semantic-analyzer-debug-show-completions (comp)
  "Show the completion list COMP."
  (if (not comp)
      (princ "\nNo known possible completions.\n")

    (princ "\nPossible completions are:\n")
    (dolist (C comp)
      (princ "  ")
      (cond ((stringp C)
	     (princ C)
	     )
	    ((semantic-tag-p C)
	     (semantic-analyzer-debug-insert-tag C)))
      (princ "\n"))
    (princ "\n")))

(defvar semantic-dependency-system-include-path)

(defun semantic-analyzer-debug-insert-include-summary (table)
  "Display a summary of includes for the semanticdb TABLE."
  (require 'semantic/dep)
  (semantic-fetch-tags)
  (let ((inc (semantic-find-tags-by-class 'include table))
	;;(path (semanticdb-find-test-translate-path-no-loading))
	(unk
	 (with-current-buffer (semanticdb-get-buffer table)
	   semanticdb-find-lost-includes))
	(ip
	 (with-current-buffer (semanticdb-get-buffer table)
	   semantic-dependency-system-include-path))
	(edeobj
	 (with-current-buffer (semanticdb-get-buffer table)
	   (and (boundp 'ede-object)
		ede-object)))
	(edeproj
	 (with-current-buffer (semanticdb-get-buffer table)
	   (and (boundp 'ede-object-project)
		ede-object-project))))

    (princ "\n\nInclude Path Summary:")
    (when edeobj
	(princ "\n\nThis file's project include search is handled by the EDE object:\n")
	(princ "  Buffer Target:  ")
	(princ (object-print edeobj))
	(princ "\n")
	(when (not (eq edeobj edeproj))
	  (princ "  Buffer Project: ")
	  (princ (object-print edeproj))
	  (princ "\n"))
	(when edeproj
	  (let ((loc (ede-get-locator-object edeproj)))
	    (princ "  Backup Locator: ")
	    (princ (object-print loc))
	    (princ "\n")))
	)

    (princ "\n\nThe system include path is:\n")
    (dolist (dir ip)
      (princ "  ")
      (princ dir)
      (princ "\n"))

    (princ "\n\nInclude Summary: ")
    (princ (semanticdb-full-filename table))
    (princ "\n\n")
    (princ (format "%s contains %d includes.\n"
		   (file-name-nondirectory
		    (semanticdb-full-filename table))
		   (length inc)))
    (let ((ok 0)
	  (unknown 0)
	  (unparsed 0)
	  (all 0))
      (dolist (i inc)
	(let* ((fileinner (semantic-dependency-tag-file i))
	       (tableinner (when fileinner
			     (semanticdb-file-table-object fileinner t))))
	  (cond ((not fileinner)
		 (setq unknown (1+ unknown)))
		((number-or-marker-p (oref tableinner pointmax))
		 (setq ok (1+ ok)))
		(t
		 (setq unparsed (1+ unparsed))))))
      (setq all (+ ok unknown unparsed))
      (when (not (= 0 all))
	(princ (format "   Unknown Includes:  %d\n" unknown))
	(princ (format "   Unparsed Includes: %d\n" unparsed))
	(princ (format "   Parsed Includes:   %d\n" ok)))
      )

    ;; Unknowns...
    (if unk
	(progn
	  (princ "\nA likely cause of an unfound tag is missing include files.")
	  (semantic-analyzer-debug-insert-tag-list
	   "The following includes were not found" unk)

	  (princ "\nYou can fix the include path for ")
	  (princ (symbol-name (oref table major-mode)))
	  (princ " by using this function:

M-x semantic-customize-system-include-path RET

which customizes the mode specific variable for the mode-local
variable `semantic-dependency-system-include-path'.")
	  )

      (princ "\n No unknown includes.\n"))
    ))

(defun semantic-analyzer-debug-describe-scope (ctxt &optional classconstraint)
  "Describe the scope in CTXT for finding a global symbol.
Optional argument CLASSCONSTRAINT says to output to tags of that class."
  (let* ((scope (oref ctxt :scope))
	 (parents (oref scope parents))
	 (cc (or classconstraint (oref ctxt prefixclass)))
	 )
    (princ "\nLocal Scope Information:")
    (princ "\n * Tag Class Constraint against SCOPE: ")
    (princ (format "%S" classconstraint))

    (if parents
	(semantic-analyzer-debug-insert-tag-list
	 " >> Known parent types with possible in scope symbols"
	 parents)
      (princ "\n * No known parents in current scope."))

    (let ((si (semantic-analyze-tags-of-class-list
	       (oref scope scope) cc))
	  (lv (semantic-analyze-tags-of-class-list
	       (oref scope localvar) cc))
	  )
      (if si
	  (semantic-analyzer-debug-insert-tag-list
	   " >> Known symbols within the current scope"
	   si)
	(princ "\n * No known symbols currently in scope."))

      (if lv
	  (semantic-analyzer-debug-insert-tag-list
	   " >> Known symbols that are declared locally"
	   lv)
	(princ "\n * No known symbols declared locally."))
      )
    )
  )

(defun semantic-analyzer-debug-global-miss-text (name-in)
  "Use 'princ' to show text describing not finding symbol NAME-IN.
NAME is the name of the unfound symbol."
  (let ((name (cond ((stringp name-in)
		     name-in)
		    ((semantic-tag-p name-in)
		     (semantic-format-tag-name name-in))
		    (t (format "%S" name-in)))))
    (when (not (string= name ""))
      (princ "\nIf ")
      (princ name)
      (princ " is a local variable, argument, or symbol in some
namespace or class exposed via scoping statements, then it should
appear in the scope.

Debugging the scope can be done with:
  M-x semantic-calculate-scope RET

If the prefix is a global symbol, in an included file, then
your search path may be incomplete.
"))))

;;; Utils
;;
(defun semantic-analyzer-debug-insert-tag-list (text taglist)
  "Prefixing with TEXT, dump TAGLIST in a help buffer."
  (princ "\n") (princ text) (princ ":\n")

  (dolist (M taglist)
    (princ "  ")
    ;;(princ (semantic-format-tag-prototype M))
    (semantic-analyzer-debug-insert-tag M)
    (princ "\n"))
  )

(defun semantic-analyzer-debug-insert-tag (tag &optional parent)
  "Display a TAG by name, with possible jumpitude.
PARENT is a possible parent (by nesting) tag."
  (let ((str (semantic-format-tag-prototype tag parent)))
    (if (and (semantic-tag-with-position-p tag)
	     (semantic-tag-file-name tag))
	(with-current-buffer standard-output
	  (insert-button str
			 'mouse-face 'custom-button-pressed-face
			 'tag tag
			 'action
			 `(lambda (button)
			    (let ((buff nil)
				  (pnt nil))
			      (save-excursion
				(semantic-go-to-tag
				 (button-get button 'tag))
				(setq buff (current-buffer))
				(setq pnt (point)))
			      (if (get-buffer-window buff)
				  (select-window (get-buffer-window buff))
				(pop-to-buffer buff t))
			      (goto-char pnt)
			      (pulse-line-hook-function)))
			 ))
      (princ "\"")
      (princ str)
      (princ "\""))
    ))

(defvar semantic-analyzer-debug-orig nil
  "The originating buffer for a help button.")

(defun semantic-analyzer-debug-add-buttons ()
  "Add push-buttons to the *Help* buffer.
Look for key expressions, and add push-buttons near them."
  (let ((orig-buffer (make-marker)))
    (set-marker orig-buffer (point) (current-buffer))
    ;; Get a buffer ready.
    (with-current-buffer "*Help*"
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(set (make-local-variable 'semantic-analyzer-debug-orig) orig-buffer)
	;; First, add do-in buttons to recommendations.
	(while (re-search-forward "^\\s-*M-x \\(\\(\\w\\|\\s_\\)+\\) " nil t)
	  (let ((fcn (match-string 1)))
	    (when (not (fboundp (intern-soft fcn)))
	      (error "Help Err: Can't find %s" fcn))
	    (end-of-line)
	    (insert "   ")
	    (insert-button "[ Do It ]"
			   'mouse-face 'custom-button-pressed-face
			   'do-fcn fcn
			   'action `(lambda (arg)
				      (let ((M semantic-analyzer-debug-orig))
					(set-buffer (marker-buffer M))
					(goto-char M))
				      (call-interactively (quote ,(intern-soft fcn))))))))
      ;; Do something else?
      ;; Clean up the mess
      (set-buffer-modified-p nil))))

(provide 'semantic/analyze/debug)

;;; semantic/analyze/debug.el ends here
