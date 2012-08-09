;;; semantic/texi.el --- Semantic details for Texinfo files

;; Copyright (C) 2001-2005, 2007-2012  Free Software Foundation, Inc.

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
;; Parse Texinfo buffers using regular expressions.  The core parser
;; engine is the function `semantic-texi-parse-headings'.  The
;; parser plug-in is the function `semantic-texi-parse-region' that
;; overrides `semantic-parse-region'.

(require 'semantic)
(require 'semantic/format)
(require 'texinfo)

(eval-when-compile
  (require 'semantic/db)
  (require 'semantic/db-find)
  (require 'semantic/ctxt)
  (require 'semantic/find)
  (require 'semantic/doc))

(defvar ede-minor-mode)
(declare-function lookup-words "ispell")
(declare-function ede-current-project "ede")

(defvar semantic-texi-super-regex
  "^@\\(top\\|chapter\\|\\(sub\\)*section\\|unnumbered\\(\\(sub\\)*sec\\)?\\|\
\\(chap\\|\\(sub\\)+\\|major\\)?heading\\|appendix\\(\\(sub\\)*sec\\)?\\|\
centerchap\\|def\\(var\\|un\\|fn\\|opt\\)x?\\)"
  "Regular expression used to find special sections in a Texinfo file.")

(defvar semantic-texi-name-field-list
  '( ("defvar" . 1)
     ("defvarx" . 1)
     ("defun" . 1)
     ("defunx" . 1)
     ("defopt" . 1)
     ("deffn" . 2)
     ("deffnx" . 2)
     )
  "List of definition commands, and the field position.
The field position is the field number (based at 1) where the
name of this section is.")

;;; Code:
(defun semantic-texi-parse-region (&rest ignore)
  "Parse the current texinfo buffer for semantic tags.
IGNORE any arguments, always parse the whole buffer.
Each tag returned is of the form:
 (\"NAME\" section (:members CHILDREN))
or
 (\"NAME\" def)

It is an override of 'parse-region and must be installed by the
function `semantic-install-function-overrides'."
  (mapcar 'semantic-texi-expand-tag
          (semantic-texi-parse-headings)))

(defun semantic-texi-parse-changes ()
  "Parse changes in the current texinfo buffer."
  ;; NOTE: For now, just schedule a full reparse.
  ;;       To be implemented later.
  (semantic-parse-tree-set-needs-rebuild))

(defun semantic-texi-expand-tag (tag)
  "Expand the texinfo tag TAG."
  (let ((chil (semantic-tag-components tag)))
    (if chil
        (semantic-tag-put-attribute
         tag :members (mapcar 'semantic-texi-expand-tag chil)))
    (car (semantic--tag-expand tag))))

(defun semantic-texi-parse-headings ()
  "Parse the current texinfo buffer for all semantic tags now."
  (let ((pass1 nil))
    ;; First search and snarf.
    (save-excursion
      (goto-char (point-min))
      (let ((semantic--progress-reporter
	     (make-progress-reporter
	      (format "Parsing %s..."
		      (file-name-nondirectory buffer-file-name))
	      (point-min) (point-max))))
	(while (re-search-forward semantic-texi-super-regex nil t)
	  (setq pass1 (cons (match-beginning 0) pass1))
	  (progress-reporter-update semantic--progress-reporter (point)))
	(progress-reporter-done semantic--progress-reporter)))
    (setq pass1 (nreverse pass1))
    ;; Now, make some tags while creating a set of children.
    (car (semantic-texi-recursive-combobulate-list pass1 0))
    ))

(defsubst semantic-texi-new-section-tag (name members start end)
  "Create a semantic tag of class section.
NAME is the name of this section.
MEMBERS is a list of semantic tags representing the elements that make
up this section.
START and END define the location of data described by the tag."
  (append (semantic-tag name 'section :members members)
          (list start end)))

(defsubst semantic-texi-new-def-tag (name start end)
  "Create a semantic tag of class def.
NAME is the name of this definition.
START and END define the location of data described by the tag."
  (append (semantic-tag name 'def)
          (list start end)))

(defun semantic-texi-set-endpoint (metataglist pnt)
  "Set the end point of the first section tag in METATAGLIST to PNT.
METATAGLIST is a list of tags in the intermediate tag format used by the
texinfo parser.  PNT is the new point to set."
  (let ((metatag nil))
    (while (and metataglist
		(not (eq (semantic-tag-class (car metataglist)) 'section)))
      (setq metataglist (cdr metataglist)))
    (setq metatag (car metataglist))
    (when metatag
      (setcar (nthcdr (1- (length metatag)) metatag) pnt)
      metatag)))

(defun semantic-texi-recursive-combobulate-list (sectionlist level)
  "Rearrange SECTIONLIST to be a hierarchical tag list starting at LEVEL.
Return the rearranged new list, with all remaining tags from
SECTIONLIST starting at ELT 2.  Sections not are not dealt with as soon as a
tag with greater section value than LEVEL is found."
  (let ((newl nil)
	(oldl sectionlist)
        tag
	)
    (save-excursion
      (catch 'level-jump
	(while oldl
	  (goto-char (car oldl))
	  (if (looking-at "@\\(\\w+\\)")
	      (let* ((word (match-string 1))
		     (levelmatch (assoc word texinfo-section-list))
		     text begin tmp
		     )
		;; Set begin to the right location
		(setq begin (point))
		;; Get out of here if there if we made it that far.
		(if (and levelmatch (<= (car (cdr levelmatch)) level))
		    (progn
		      (when newl
			(semantic-texi-set-endpoint newl begin))
		      (throw 'level-jump t)))
		;; Recombobulate
		(if levelmatch
		    (let ((end (match-end 1)))
		      ;; Levels sometimes have a @node just in front.
		      ;; That node statement should be included in the space
		      ;; for this entry.
		      (save-excursion
			(skip-chars-backward "\n \t")
			(beginning-of-line)
			(when (looking-at "@node\\>")
			  (setq begin (point))))
		      ;; When there is a match, the descriptive text
		      ;; consists of the rest of the line.
		      (goto-char end)
		      (skip-chars-forward " \t")
		      (setq text (buffer-substring-no-properties
				  (point)
				  (progn (end-of-line) (point))))
		      ;; Next, recurse into the body to find the end.
		      (setq tmp (semantic-texi-recursive-combobulate-list
				 (cdr oldl) (car (cdr levelmatch))))
		      ;; Build a tag
                      (setq tag (semantic-texi-new-section-tag
                                 text (car tmp) begin (point)))
		      ;; Before appending the newtag, update the previous tag
		      ;; if it is a section tag.
		      (when newl
			(semantic-texi-set-endpoint newl begin))
		      ;; Append new tag to our master list.
		      (setq newl (cons tag newl))
		      ;; continue
		      (setq oldl (cdr tmp))
		      )
		  ;; No match means we have a def*, so get the name from
		  ;; it based on the type of thingy we found.
		  (setq levelmatch (assoc word semantic-texi-name-field-list)
			tmp (or (cdr levelmatch) 1))
		  (forward-sexp tmp)
		  (skip-chars-forward " \t")
		  (setq text (buffer-substring-no-properties
			      (point)
			      (progn (forward-sexp 1) (point))))
		  ;; Seek the end of this definition
		  (goto-char begin)
		  (semantic-texi-forward-deffn)
                  (setq tag (semantic-texi-new-def-tag text begin (point))
                        newl (cons tag newl))
		  ;; continue
		  (setq oldl (cdr oldl)))
		)
	    (error "Problem finding section in semantic/texi parser"))
	  ;; (setq oldl (cdr oldl))
	  )
	;; When oldl runs out, force a new endpoint as point-max
	(when (not oldl)
	  (semantic-texi-set-endpoint newl (point-max)))
	))
    (cons (nreverse newl) oldl)))

(defun semantic-texi-forward-deffn ()
  "Move forward over one deffn type definition.
The cursor should be on the @ sign."
  (when (looking-at "@\\(\\w+\\)")
    (let* ((type (match-string 1))
	   (seek (concat "^@end\\s-+" (regexp-quote type))))
      (re-search-forward seek nil t))))

(define-mode-local-override semantic-tag-components
  texinfo-mode (tag)
  "Return components belonging to TAG."
  (semantic-tag-get-attribute tag :members))


;;; Overrides: Context Parsing
;;
;; How to treat texi as a language?
;;
(defvar semantic-texi-environment-regexp
  (if (string-match texinfo-environment-regexp "@menu")
      ;; Make sure our Emacs has menus in it.
      texinfo-environment-regexp
    ;; If no menus, then merge in the menu concept.
    (when (string-match "cartouche" texinfo-environment-regexp)
      (concat (substring texinfo-environment-regexp
			 0 (match-beginning 0))
	      "menu\\|"
	       (substring texinfo-environment-regexp
			 (match-beginning 0)))))
  "Regular expression for matching texinfo environments.
uses `texinfo-environment-regexp', but makes sure that it
can handle the @menu environment.")

(define-mode-local-override semantic-up-context texinfo-mode ()
  "Handle texinfo constructs which do not use parenthetical nesting."
  (let ((done nil))
    (save-excursion
      (let ((parenthetical (semantic-up-context-default))
	    )
	(when (not parenthetical)
	  ;; We are in parentheses.  Are they the types of parens
	  ;; belonging to a texinfo construct?
	  (forward-word -1)
	  (when (looking-at "@\\w+{")
	    (setq done (point))))))
    ;; If we are not in a parenthetical node, then find a block instead.
    ;; Use the texinfo support to find block start/end constructs.
    (save-excursion
      (while (and (not done)
		  (re-search-backward  semantic-texi-environment-regexp nil t))
	;; For any hit, if we find an @end foo, then jump to the
	;; matching @foo.  If it is not an end, then we win!
	(if (not (looking-at "@end\\s-+\\(\\w+\\)"))
	    (setq done (point))
	  ;; Skip over this block
	  (let ((env (match-string 1)))
	    (re-search-backward (concat "@" env))))
	))
    ;; All over, post what we find.
    (if done
	;; We found something, so use it.
	(progn (goto-char done)
	       nil)
      t)))

(define-mode-local-override semantic-beginning-of-context texinfo-mode (&optional point)
  "Move to the beginning of the context surrounding POINT."
  (if (semantic-up-context point)
      ;; If we can't go up, we can't do this either.
      t
    ;; We moved, so now we need to skip into whatever this thing is.
    (forward-word 1) ;; skip the command
    (if (looking-at "\\s-*{")
	;; In a short command.  Go in.
	(down-list 1)
      ;; An environment.  Go to the next line.
      (end-of-line)
      (forward-char 1))
    nil))

(define-mode-local-override semantic-ctxt-current-class-list
  texinfo-mode (&optional point)
  "Determine the class of tags that can be used at POINT.
For texinfo, there two possibilities returned.
1) 'function - for a call to a texinfo function
2) 'word     - indicates an english word.
It would be nice to know function arguments too, but not today."
  (let ((sym (semantic-ctxt-current-symbol)))
    (if (and sym (= (aref (car sym) 0) ?@))
	'(function)
      '(word))))


;;; Overrides : Formatting
;;
;; Various override to better format texi tags.
;;

(define-mode-local-override semantic-format-tag-abbreviate
  texinfo-mode  (tag &optional parent color)
  "Texinfo tags abbreviation."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	)
    (cond ((eq class 'function)
	   (concat name "{ }"))
	  (t (semantic-format-tag-abbreviate-default tag parent color)))
    ))

(define-mode-local-override semantic-format-tag-prototype
  texinfo-mode  (tag &optional parent color)
  "Texinfo tags abbreviation."
  (semantic-format-tag-abbreviate tag parent color))


;;; Texi Unique Features
;;
(defun semantic-tag-texi-section-text-bounds (tag)
  "Get the bounds to the text of TAG.
The text bounds is the text belonging to this node excluding
the text of any child nodes, but including any defuns."
  (let ((memb (semantic-tag-components tag)))
    ;; Members.. if one is a section, check it out.
    (while (and memb (not (semantic-tag-of-class-p (car memb) 'section)))
      (setq memb (cdr memb)))
    ;; No members? ... then a simple problem!
    (if (not memb)
	(semantic-tag-bounds tag)
      ;; Our end is their beginning...
      (list (semantic-tag-start tag) (semantic-tag-start (car memb))))))

(defun semantic-texi-current-environment (&optional point)
  "Return as a string the type of the current environment.
Optional argument POINT is where to look for the environment."
  (save-excursion
    (when point (goto-char (point)))
    (while (and (or (not (looking-at  semantic-texi-environment-regexp))
		    (looking-at "@end"))
		(not (semantic-up-context)))
      )
    (when (looking-at  semantic-texi-environment-regexp)
      (match-string 1))))


;;; Analyzer
;;
(eval-when-compile
  (require 'semantic/analyze))

(define-mode-local-override semantic-analyze-current-context
  texinfo-mode (point)
  "Analysis context makes no sense for texinfo.  Return nil."
  (let* ((prefixandbounds (semantic-ctxt-current-symbol-and-bounds (point)))
	 (prefix (car prefixandbounds))
	 (bounds (nth 2 prefixandbounds))
	 (prefixclass (semantic-ctxt-current-class-list))
	 )
    (when prefix
      (require 'semantic/analyze)
      (semantic-analyze-context
       "Context-for-texinfo"
       :buffer (current-buffer)
       :scope nil
       :bounds bounds
       :prefix prefix
       :prefixtypes nil
       :prefixclass prefixclass)
      )
    ))

(defvar semantic-texi-command-completion-list
  (append (mapcar (lambda (a) (car a)) texinfo-section-list)
	  (condition-case nil
	      texinfo-environments
	    (error
	     ;; XEmacs doesn't use the above.  Split up its regexp
	     (split-string texinfo-environment-regexp "\\\\|\\|\\^@\\\\(\\|\\\\)")
	     ))
	  ;; Is there a better list somewhere?  Here are few
	  ;; of the top of my head.
	  "anchor" "asis"
	  "bullet"
	  "code" "copyright"
	  "defun" "deffn" "defoption" "defvar" "dfn"
	  "emph" "end"
	  "ifinfo" "iftex" "inforef" "item" "itemx"
	  "kdb"
	  "node"
	  "ref"
	  "set" "setfilename" "settitle"
	  "value" "var"
	  "xref"
	  )
  "List of commands that we might bother completing.")

(define-mode-local-override semantic-analyze-possible-completions
  texinfo-mode (context)
  "List smart completions at point.
Since texinfo is not a programming language the default version is not
useful.  Instead, look at the current symbol.  If it is a command
do primitive texinfo built ins.  If not, use ispell to lookup words
that start with that symbol."
  (let ((prefix (car (oref context :prefix)))
	)
    (cond ((member 'function (oref context :prefixclass))
	   ;; Do completion for texinfo commands
	   (let* ((cmd (substring prefix 1))
		  (lst (all-completions
			cmd semantic-texi-command-completion-list)))
	     (mapcar (lambda (f) (semantic-tag (concat "@" f) 'function))
		     lst))
	   )
	  ((member 'word (oref context :prefixclass))
	   ;; Do completion for words via ispell.
	   (require 'ispell)
	   (let ((word-list (lookup-words prefix)))
	     (mapcar (lambda (f) (semantic-tag f 'word)) word-list))
	   )
	  (t nil))
    ))


;;; Parser Setup
;;
;; In semantic/imenu.el, not part of Emacs.
(defvar semantic-imenu-expandable-tag-classes)
(defvar semantic-imenu-bucketize-file)
(defvar semantic-imenu-bucketize-type-members)

(defun semantic-default-texi-setup ()
  "Set up a buffer for parsing of Texinfo files."
  ;; This will use our parser.
  (semantic-install-function-overrides
   '((parse-region . semantic-texi-parse-region)
     (parse-changes . semantic-texi-parse-changes)))
  (setq semantic-parser-name "TEXI"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
        imenu-create-index-function 'semantic-create-imenu-index
	semantic-command-separation-character "@"
	semantic-type-relation-separator-character '(":")
	semantic-symbol->name-assoc-list '((section . "Section")
					   (def . "Definition")
					   )
	semantic-imenu-expandable-tag-classes '(section)
	semantic-imenu-bucketize-file nil
	semantic-imenu-bucketize-type-members nil
	senator-step-at-start-end-tag-classes '(section)
	semantic-stickyfunc-sticky-classes '(section)
	)
  ;; (local-set-key [(f9)] 'semantic-texi-update-doc-from-texi)
  )

(add-hook 'texinfo-mode-hook 'semantic-default-texi-setup)


;;; Special features of Texinfo tag streams
;;
;; This section provides specialized access into texinfo files.
;; Because texinfo files often directly refer to functions and programs
;; it is useful to access the texinfo file from the C code for document
;; maintenance.
(defun semantic-texi-associated-files (&optional buffer)
  "Find texinfo files associated with BUFFER."
  (save-excursion
    (if buffer (set-buffer buffer))
    (cond ((and (fboundp 'ede-documentation-files)
                ede-minor-mode (ede-current-project))
	   ;; When EDE is active, ask it.
	   (ede-documentation-files)
	   )
	  ((and (featurep 'semantic/db) (semanticdb-minor-mode-p))
	   ;; See what texinfo files we have loaded in the database
	   (let ((tabs (semanticdb-get-database-tables
			semanticdb-current-database))
		 (r nil))
	     (while tabs
	       (if (eq (oref (car tabs) major-mode) 'texinfo-mode)
		   (setq r (cons (oref (car tabs) file) r)))
	       (setq tabs (cdr tabs)))
	     r))
	  (t
	   (directory-files default-directory nil "\\.texi$"))
	  )))

;; Turns out this might not be useful.
;; Delete later if that is true.
(defun semantic-texi-find-documentation (name &optional type)
  "Find the function or variable NAME of TYPE in the texinfo source.
NAME is a string representing some functional symbol.
TYPE is a string, such as \"variable\" or \"Command\" used to find
the correct definition in case NAME qualifies as several things.
When this function exists, POINT is at the definition.
If the doc was not found, an error is thrown.
Note: TYPE not yet implemented."
  (let ((f (semantic-texi-associated-files))
	stream match)
    (while (and f (not match))
      (unless stream
	(with-current-buffer (find-file-noselect (car f))
	  (setq stream (semantic-fetch-tags))))
      (setq match (semantic-find-first-tag-by-name name stream))
      (when match
	(set-buffer (semantic-tag-buffer match))
	(goto-char (semantic-tag-start match)))
      (setq f (cdr f)))))

;; (defun semantic-texi-update-doc-from-texi (&optional tag)
;;   "Update the documentation in the texinfo deffn class tag TAG.
;; The current buffer must be a texinfo file containing TAG.
;; If TAG is nil, determine a tag based on the current position."
;;   (interactive)
;;   (unless (or (featurep 'semantic/db)
;; 	      (require 'semantic/db-mode)
;; 	      (semanticdb-minor-mode-p))
;;     (error "Texinfo updating only works when `semanticdb' is being used"))
;;   (semantic-fetch-tags)
;;   (unless tag
;;     (beginning-of-line)
;;     (setq tag (semantic-current-tag)))
;;   (unless (semantic-tag-of-class-p tag 'def)
;;     (error "Only deffns (or defun or defvar) can be updated"))
;;   (let* ((name (semantic-tag-name tag))
;; 	 (tags (semanticdb-strip-find-results
;; 		(semanticdb-with-match-any-mode
;; 		  (semanticdb-brute-deep-find-tags-by-name name))
;; 		'name))
;; 	 (docstring nil)
;; 	 (docstringproto nil)
;; 	 (docstringvar nil)
;; 	 (doctag nil)
;; 	 (doctagproto nil)
;; 	 (doctagvar nil)
;; 	 )
;;     (save-excursion
;;       (while (and tags (not docstring))
;; 	(let ((sourcetag (car tags)))
;; 	  ;; There could be more than one!  Come up with a better
;; 	  ;; solution someday.
;; 	  (when (semantic-tag-buffer sourcetag)
;; 	    (set-buffer (semantic-tag-buffer sourcetag))
;; 	    (unless (eq major-mode 'texinfo-mode)
;; 	    (cond ((semantic-tag-get-attribute sourcetag :prototype-flag)
;; 		   ;; If we found a match with doc that is a prototype, then store
;; 		   ;; that, but don't exit till we find the real deal.
;; 		   (setq docstringproto (semantic-documentation-for-tag sourcetag)
;; 			 doctagproto sourcetag))
;; 		  ((eq (semantic-tag-class sourcetag) 'variable)
;; 		   (setq docstringvar (semantic-documentation-for-tag sourcetag)
;; 			 doctagvar sourcetag))
;; 		  ((semantic-tag-get-attribute sourcetag :override-function-flag)
;; 		   nil)
;; 		  (t
;; 		   (setq docstring (semantic-documentation-for-tag sourcetag))))
;; 	    (setq doctag (if docstring sourcetag nil))))
;; 	  (setq tags (cdr tags)))))
;;     ;; If we found a prototype of the function that has some doc, but not the
;;     ;; actual function, let's make due with that.
;;     (if (not docstring)
;; 	(cond ((stringp docstringvar)
;; 	       (setq docstring docstringvar
;; 		     doctag doctagvar))
;; 	      ((stringp docstringproto)
;; 	       (setq docstring docstringproto
;; 		     doctag doctagproto))))
;;     ;; Test for doc string
;;     (unless docstring
;;       (error "Could not find documentation for %s" (semantic-tag-name tag)))
;;
;;     (require 'srecode)
;;     (require 'srecode/texi)
;;
;;     ;; If we have a string, do the replacement.
;;     (delete-region (semantic-tag-start tag)
;; 		   (semantic-tag-end tag))
;;     ;; Use useful functions from the document library.
;;    (srecode-texi-insert-tag-as-doc doctag)
;;    ;(semantic-insert-foreign-tag doctag)
;;     ))

;; (defun semantic-texi-update-doc-from-source (&optional tag)
;;   "Update the documentation for the source TAG.
;; The current buffer must be a non-texinfo source file containing TAG.
;; If TAG is nil, determine the tag based on the current position.
;; The current buffer must include TAG."
;;   (interactive)
;;   (when (eq major-mode 'texinfo-mode)
;;     (error "Not a source file"))
;;   (semantic-fetch-tags)
;;   (unless tag
;;     (setq tag (semantic-current-tag)))
;;   (unless (semantic-documentation-for-tag tag)
;;     (error "Cannot find interesting documentation to use for %s"
;; 	   (semantic-tag-name tag)))
;;   (let* ((name (semantic-tag-name tag))
;; 	 (texi (semantic-texi-associated-files))
;; 	 (doctag nil)
;; 	 (docbuff nil))
;;     (while (and texi (not doctag))
;;       (set-buffer (find-file-noselect (car texi)))
;;       (setq doctag (car (semantic-deep-find-tags-by-name
;; 			 name (semantic-fetch-tags)))
;; 	    docbuff (if doctag (current-buffer) nil))
;;       (setq texi (cdr texi)))
;;     (unless doctag
;;       (error "Tag %s is not yet documented.  Use the `document' command"
;;              name))
;;     ;; Ok, we should have everything we need.  Do the deed.
;;     (if (get-buffer-window docbuff)
;; 	(set-buffer docbuff)
;;       (switch-to-buffer docbuff))
;;     (goto-char (semantic-tag-start doctag))
;;     (delete-region (semantic-tag-start doctag)
;; 		   (semantic-tag-end doctag))
;;     ;; Use useful functions from the document library.
;;     (require 'document)
;;     (document-insert-texinfo tag (semantic-tag-buffer tag))
;;     ))

;; (defun semantic-texi-update-doc (&optional tag)
;;   "Update the documentation for TAG.
;; If the current buffer is a texinfo file, then find the source doc, and
;; update it.  If the current buffer is a source file, then get the
;; documentation for this item, find the existing doc in the associated
;; manual, and update that."
;;   (interactive)
;;   (cond ;;((eq major-mode 'texinfo-mode)
;; 	;; (semantic-texi-update-doc-from-texi tag))
;; 	(t
;; 	 (semantic-texi-update-doc-from-source tag))))

(defun semantic-texi-goto-source (&optional tag)
  "Jump to the source for the definition in the texinfo file TAG.
If TAG is nil, it is derived from the deffn under POINT."
  (interactive)
  (unless (or (featurep 'semantic/db) (semanticdb-minor-mode-p))
    (error "Texinfo updating only works when `semanticdb' is being used"))
  (semantic-fetch-tags)
  (unless tag
    (beginning-of-line)
    (setq tag (semantic-current-tag)))
  (unless (semantic-tag-of-class-p tag 'def)
    (error "Only deffns (or defun or defvar) can be updated"))
  (let* ((name (semantic-tag-name tag))
	 (tags (semanticdb-fast-strip-find-results
		(semanticdb-with-match-any-mode
		  (semanticdb-brute-deep-find-tags-by-name name nil 'name))
		))

	 (done nil)
	 )
    (save-excursion
      (while (and tags (not done))
	(set-buffer (semantic-tag-buffer (car tags)))
	(unless (eq major-mode 'texinfo-mode)
	  (switch-to-buffer (semantic-tag-buffer (car tags)))
	  (goto-char (semantic-tag-start (car tags)))
	  (setq done t))
	(setq tags (cdr tags)))
      (if (not done)
	  (error "Could not find tag for %s" (semantic-tag-name tag)))
      )))

(provide 'semantic/texi)

;;; semantic/texi.el ends here
