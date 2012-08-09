;;; semantic/tag-file.el --- Routines that find files based on tags.

;; Copyright (C) 1999-2005, 2007-2012  Free Software Foundation, Inc.

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
;; A tag, by itself, can have representations in several files.
;; These routines will find those files.

(require 'semantic/tag)

(defvar ede-minor-mode)
(declare-function semanticdb-table-child-p "semantic/db" t t)
(declare-function semanticdb-get-buffer "semantic/db")
(declare-function semantic-dependency-find-file-on-path "semantic/dep")
(declare-function ede-toplevel "ede/base")

;;; Code:

;;; Location a TAG came from.
;;
;;;###autoload
(define-overloadable-function semantic-go-to-tag (tag &optional parent)
  "Go to the location of TAG.
TAG may be a stripped element, in which case PARENT specifies a
parent tag that has position information.
PARENT can also be a `semanticdb-table' object."
  (:override
   (save-match-data
     (cond ((semantic-tag-in-buffer-p tag)
	    ;; We have a linked tag, go to that buffer.
	    (set-buffer (semantic-tag-buffer tag)))
	   ((semantic-tag-file-name tag)
	    ;; If it didn't have a buffer, but does have a file
	    ;; name, then we need to get to that file so the tag
	    ;; location is made accurate.
	    (set-buffer (find-file-noselect (semantic-tag-file-name tag))))
	   ((and parent (semantic-tag-p parent) (semantic-tag-in-buffer-p parent))
	    ;; The tag had nothing useful, but we have a parent with
	    ;; a buffer, then go there.
	    (set-buffer (semantic-tag-buffer parent)))
	   ((and parent (semantic-tag-p parent) (semantic-tag-file-name parent))
	    ;; Tag had nothing, and the parent only has a file-name, then
	    ;; find that file, and switch to that buffer.
	    (set-buffer (find-file-noselect (semantic-tag-file-name parent))))
	   ((and parent (featurep 'semantic/db)
		 (semanticdb-table-child-p parent))
	    (set-buffer (semanticdb-get-buffer parent)))
	   (t
	    ;; Well, just assume things are in the current buffer.
	    nil
	    )))
   ;; We should be in the correct buffer now, try and figure out
   ;; where the tag is.
   (cond ((semantic-tag-with-position-p tag)
	  ;; If it's a number, go there
	  (goto-char (semantic-tag-start tag)))
	 ((semantic-tag-with-position-p parent)
	  ;; Otherwise, it's a trimmed vector, such as a parameter,
	  ;; or a structure part.  If there is a parent, we can use it
	  ;; as a bounds for searching.
	  (goto-char (semantic-tag-start parent))
	  ;; Here we make an assumption that the text returned by
	  ;; the parser and concocted by us actually exists
	  ;; in the buffer.
	  (re-search-forward (semantic-tag-name tag)
			     (semantic-tag-end parent)
			     t))
	 ((semantic-tag-get-attribute tag :line)
	  ;; The tag has a line number in it.  Go there.
	  (goto-char (point-min))
	  (forward-line (1- (semantic-tag-get-attribute tag :line))))
	 ((and (semantic-tag-p parent) (semantic-tag-get-attribute parent :line))
	  ;; The tag has a line number in it.  Go there.
	  (goto-char (point-min))
	  (forward-line (1- (semantic-tag-get-attribute parent :line)))
	  (re-search-forward (semantic-tag-name tag) nil t))
	 (t
	  ;; Take a guess that the tag has a unique name, and just
	  ;; search for it from the beginning of the buffer.
	  (goto-char (point-min))
	  (re-search-forward (semantic-tag-name tag) nil t)))
   )
  )

(make-obsolete-overload 'semantic-find-nonterminal
                        'semantic-go-to-tag "23.2")

;;; Dependencies
;;
;; A tag which is of type 'include specifies a dependency.
;; Dependencies usually represent a file of some sort.
;; Find the file described by a dependency.

;;;###autoload
(define-overloadable-function semantic-dependency-tag-file (&optional tag)
  "Find the filename represented from TAG.
Depends on `semantic-dependency-include-path' for searching.  Always searches
`.' first, then searches additional paths."
  (or tag (setq tag (car (semantic-find-tag-by-overlay nil))))
  (unless (semantic-tag-of-class-p tag 'include)
    (signal 'wrong-type-argument (list tag 'include)))
  (save-excursion
    (let ((result nil)
	  (default-directory default-directory)
	  (edefind nil)
	  (tag-fname nil))
      (cond ((semantic-tag-in-buffer-p tag)
	     ;; If the tag has an overlay and buffer associated with it,
	     ;; switch to that buffer so that we get the right override methods.
	     (set-buffer (semantic-tag-buffer tag)))
	    ((semantic-tag-file-name tag)
	     ;; If it didn't have a buffer, but does have a file
	     ;; name, then we need to get to that file so the tag
	     ;; location is made accurate.
	     ;;(set-buffer (find-file-noselect (semantic-tag-file-name tag)))
	     ;;
	     ;; 2/3/08
	     ;; The above causes unnecessary buffer loads all over the place. Ick!
	     ;; All we really need is for 'default-directory' to be set correctly.
	     (setq default-directory (file-name-directory (semantic-tag-file-name tag)))
	     ))
      ;; Setup the filename represented by this include
      (setq tag-fname (semantic-tag-include-filename tag))

      ;; First, see if this file exists in the current EDE project
      (if (and (fboundp 'ede-expand-filename) ede-minor-mode
	       (setq edefind
		     (condition-case nil
			 (let ((proj  (ede-toplevel)))
			   (when proj
			     (ede-expand-filename proj tag-fname)))
		       (error nil))))
	  (setq result edefind))
      (if (not result)
	  (setq result
		;; I don't have a plan for refreshing tags with a dependency
		;; stuck on them somehow.  I'm thinking that putting a cache
		;; onto the dependency finding with a hash table might be best.
		;;(if (semantic--tag-get-property tag 'dependency-file)
		;;  (semantic--tag-get-property tag 'dependency-file)
		(:override
		 (save-excursion
		   (require 'semantic/dep)
		   (semantic-dependency-find-file-on-path
		    tag-fname (semantic-tag-include-system-p tag))))
		;; )
		))
      (if (stringp result)
	  (progn
	    (semantic--tag-put-property tag 'dependency-file result)
	    result)
	;; @todo: Do something to make this get flushed w/
	;;        when the path is changed.
	;; @undo: Just eliminate
	;; (semantic--tag-put-property tag 'dependency-file 'none)
	nil)
      )))

(make-obsolete-overload 'semantic-find-dependency
                        'semantic-dependency-tag-file "23.2")

;;; PROTOTYPE FILE
;;
;; In C, a function in the .c file often has a representation in a
;; corresponding .h file.  This routine attempts to find the
;; prototype file a given source file would be associated with.
;; This can be used by prototype manager programs.
(define-overloadable-function semantic-prototype-file (buffer)
  "Return a file in which prototypes belonging to BUFFER should be placed.
Default behavior (if not overridden) looks for a token specifying the
prototype file, or the existence of an EDE variable indicating which
file prototypes belong in."
  (:override
   ;; Perform some default behaviors
   (if (and (fboundp 'ede-header-file) ede-minor-mode)
       (with-current-buffer buffer
         (ede-header-file))
     ;; No EDE options for a quick answer.  Search.
     (with-current-buffer buffer
       (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
           (match-string 1))))))

(semantic-alias-obsolete 'semantic-find-nonterminal
                         'semantic-go-to-tag "23.2")

(semantic-alias-obsolete 'semantic-find-dependency
                         'semantic-dependency-tag-file "23.2")


(provide 'semantic/tag-file)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/tag-file"
;; End:

;;; semantic/tag-file.el ends here
