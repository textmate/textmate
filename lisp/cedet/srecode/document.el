;;; srecode/document.el --- Documentation (comment) generation

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Routines for fabricating human readable text from function and
;; variable names as base-text for function comments.  Document is not
;; meant to generate end-text for any function.  It is merely meant to
;; provide some useful base words and text, and as a framework for
;; managing comments.
;;
;;; Origins:
;;
;; Document was first written w/ cparse, a custom regexp based c parser.
;;
;; Document was then ported to cedet/semantic using sformat (super
;; format) as the templating engine.
;;
;; Document has now been ported to srecode, using the semantic recoder
;; as the templating engine.

;; This file combines srecode/document.el and srecode/document-vars.el
;; from the CEDET repository.

(require 'srecode/args)
(require 'srecode/dictionary)
(require 'srecode/extract)
(require 'srecode/insert)
(require 'srecode/semantic)

(require 'semantic)
(require 'semantic/tag)
(require 'semantic/doc)
(require 'pulse)

;;; Code:

(defgroup document nil
  "File and tag browser frame."
  :group 'texinfo
  :group 'srecode)

(defcustom srecode-document-autocomment-common-nouns-abbrevs
  '(
    ("sock\\(et\\)?" . "socket")
    ("addr\\(ess\\)?" . "address")
    ("buf\\(f\\(er\\)?\\)?" . "buffer")
    ("cur\\(r\\(ent\\)?\\)?" . "current")
    ("dev\\(ice\\)?" . "device")
    ("doc" . "document")
    ("i18n" . "internationalization")
    ("file" . "file")
    ("line" . "line")
    ("l10n" . "localization")
    ("msg\\|message" . "message")
    ("name" . "name")
    ("next\\|nxt" . "next")
    ("num\\(ber\\)?" . "number")
    ("port" . "port")
    ("host" . "host")
    ("obj\\|object" . "object")
    ("previous\\|prev" . "previous")
    ("str\\(ing\\)?" . "string")
    ("use?r" . "user")
    ("\\(^\\|\\s-\\)id\\($\\|\\s-\\)" . "Identifier") ;complex cause ;common syllable
    )
  "List of common English abbreviations or full words.
These are nouns (as opposed to verbs) for use in creating expanded
versions of names.  This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom srecode-document-autocomment-function-alist
  '(
    ("abort" . "Aborts the")
    ;; trick to get re-alloc and alloc to pair into one sentence.
    ("realloc" . "moves or ")
    ("alloc\\(ate\\)?" . "Allocates and initializes a new ")
    ("clean" . "Cleans up the")
    ("clobber" . "Removes")
    ("close" . "Cleanly closes")
    ("check" . "Checks the")
    ("comp\\(are\\)?" . "Compares the")
    ("create" . "Creates a new ")
    ("find" . "Finds ")
    ("free" . "Frees up space")
    ("gen\\(erate\\)?" . "Generates a new ")
    ("get\\|find" . "Looks for the given ")
    ("gobble" . "Removes")
    ("he?lp" . "Provides help for")
    ("li?ste?n" . "Listens for ")
    ("connect" . "Connects to ")
    ("acc?e?pt" . "Accepts a ")
    ("load" . "Loads in ")
    ("match" . "Check that parameters match")
    ("name" . "Provides a name which ")
    ("new" . "Allocates a ")
    ("parse" . "Parses the parameters and returns ")
    ("print\\|display" . "Prints out")
    ("read" . "Reads from")
    ("reset" . "Resets the parameters and returns")
    ("scan" . "Scans the ")
    ("setup\\|init\\(ialize\\)?" . "Initializes the ")
    ("select" . "Chooses the ")
    ("send" . "Sends a")
    ("re?c\\(v\\|ieves?\\)" . "Receives a ")
    ("to" . "Converts ")
    ("update" . "Updates the ")
    ("wait" . "Waits for ")
    ("write" . "Writes to")
    )
  "List of names to string match against the function name.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string.

Certain prefixes may always mean the same thing, and the same comment
can be used as a beginning for the description.  Regexp should be
lower case since the string they are compared to is downcased.
A string may end in a space, in which case, last-alist is searched to
see how best to describe what can be returned.
Doesn't always work correctly, but that is just because English
doesn't always work correctly."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom srecode-document-autocomment-common-nouns-abbrevs
  '(
    ("sock\\(et\\)?" . "socket")
    ("addr\\(ess\\)?" . "address")
    ("buf\\(f\\(er\\)?\\)?" . "buffer")
    ("cur\\(r\\(ent\\)?\\)?" . "current")
    ("dev\\(ice\\)?" . "device")
    ("file" . "file")
    ("line" . "line")
    ("msg\\|message" . "message")
    ("name" . "name")
    ("next\\|nxt" . "next")
    ("port" . "port")
    ("host" . "host")
    ("obj\\|object" . "object")
    ("previous\\|prev" . "previous")
    ("str\\(ing\\)?" . "string")
    ("use?r" . "user")
    ("num\\(ber\\)?" . "number")
    ("\\(^\\|\\s-\\)id\\($\\|\\s-\\)" . "Identifier") ;complex cause ;common syllable
    )
  "List of common English abbreviations or full words.
These are nouns (as opposed to verbs) for use in creating expanded
versions of names.  This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom srecode-document-autocomment-return-first-alist
  '(
    ;; Static must be first in the list to provide the intro to the sentence
    ("static" . "Locally defined function which ")
    ("Bool\\|BOOL" . "Status of ")
    )
  "List of regexp matches for types.
They provide a little bit of text when typing information is
described.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom srecode-document-autocomment-return-last-alist
  '(
    ("static[ \t\n]+struct \\([a-zA-Z0-9_]+\\)" . "%s")
    ("struct \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+union \\([a-zA-Z0-9_]+\\)" . "%s")
    ("union \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+enum \\([a-zA-Z0-9_]+\\)" . "%s")
    ("enum \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+\\([a-zA-Z0-9_]+\\)" . "%s")
    ("\\([a-zA-Z0-9_]+\\)" . "of type %s")
    )
  "List of regexps which provide the type of the return value.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string, which can contain %s, which is replaced with
`match-string' 1."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom srecode-document-autocomment-param-alist
  '( ("[Cc]txt" . "Context")
     ("[Ii]d" . "Identifier of")
     ("[Tt]ype" . "Type of")
     ("[Nn]ame" . "Name of")
     ("argc" . "Number of arguments")
     ("argv" . "Argument vector")
     ("envp" . "Environment variable vector")
     )
  "Alist of common variable names appearing as function parameters.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string of text to use to describe MATCH.
When one is encountered, document-insert-parameters will automatically
place this comment after the parameter name."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

(defcustom srecode-document-autocomment-param-type-alist
  '(("const" . "Constant")
    ("void" . "Empty")
    ("char[ ]*\\*" . "String ")
    ("\\*\\*" . "Pointer to ")
    ("\\*" . "Pointer ")
    ("char[ ]*\\([^ \t*]\\|$\\)" . "Character")
    ("int\\|long" . "Number of")
    ("FILE" . "File of")
    ("float\\|double" . "Value of")
    ;; How about some X things?
    ("Bool\\|BOOL" . "Flag")
    ("Window" . "Window")
    ("GC" . "Graphic Context")
    ("Widget" . "Widget")
    )
  "Alist of input parameter types and strings describing them.
This is an alist with each element of the form:
 (MATCH . RESULT)
MATCH is a regexp to match in the type field.
RESULT is a string."
  :group 'document
  :type '(repeat (cons (string :tag "Regexp")
		       (string :tag "Doc Text"))))

;;;###autoload
(defun srecode-document-insert-comment ()
  "Insert some comments.
Whack any comments that may be in the way and replace them.
If the region is active, then insert group function comments.
If the cursor is in a comment, figure out what kind of comment it is
  and replace it.
If the cursor is in a function, insert a function comment.
If the cursor is on a one line prototype, then insert post-fcn comments."
  (interactive)
  (semantic-fetch-tags)
  (let ((ctxt (srecode-calculate-context)))
    (if ;; Active region stuff.
	(or srecode-handle-region-when-non-active-flag
	    (eq last-command 'mouse-drag-region)
	    (and transient-mark-mode mark-active))
	(if (> (point) (mark))
	    (srecode-document-insert-group-comments (mark) (point))
	  (srecode-document-insert-group-comments (point) (mark)))
      ;; ELSE

      ;; A declaration comment.  Find what it documents.
      (when (equal ctxt '("declaration" "comment"))

	;; If we are on a one line tag/comment, go to that fcn.
	(if (save-excursion (back-to-indentation)
			    (semantic-current-tag))
	    (back-to-indentation)

	  ;; Else, do we have a fcn following us?
	  (let ((tag (semantic-find-tag-by-overlay-next)))
	    (when tag (semantic-go-to-tag tag))))
	)

      ;; Now analyze the tag we may be on.

      (if (semantic-current-tag)
	  (cond
	   ;; A one-line variable
	   ((and (semantic-tag-of-class-p (semantic-current-tag) 'variable)
		 (srecode-document-one-line-tag-p (semantic-current-tag)))
	    (srecode-document-insert-variable-one-line-comment))
	   ;; A plain function
	   ((semantic-tag-of-class-p (semantic-current-tag) 'function)
	    (srecode-document-insert-function-comment))
	   ;; Don't know.
	   (t
	    (error "Not sure what to comment"))
	   )

	;; ELSE, no tag.  Perhaps we should just insert a nice section
	;; header??

	(let ((title (read-string "Section Title (RET to skip): ")))

	  (when (and (stringp title) (not (= (length title) 0)))
	    (srecode-document-insert-section-comment title)))

	))))

(defun srecode-document-insert-section-comment (&optional title)
  "Insert a section comment with TITLE."
  (interactive "sSection Title: ")

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   "section-comment"
					   "declaration"
					   'document)))
    (if (not temp)
	(error "No templates for inserting section comments"))

    (when title
      (srecode-dictionary-set-value
       dict "TITLE" title))

    (srecode-insert-fcn temp dict)
    ))


(defun srecode-document-trim-whitespace (str)
  "Strip stray whitespace from around STR."
  (when (string-match "^\\(\\s-\\|\n\\)+" str)
    (setq str (replace-match "" t t str)))
  (when (string-match "\\(\\s-\\|\n\\)+$" str)
    (setq str (replace-match "" t t str)))
  str)

;;;###autoload
(defun srecode-document-insert-function-comment (&optional fcn-in)
  "Insert or replace a function comment.
FCN-IN is the Semantic tag of the function to add a comment too.
If FCN-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just in front of FCN-IN."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   "function-comment"
					   "declaration"
					   'document)))
    (if (not temp)
	(error "No templates for inserting function comments"))

    ;; Try to figure out the tag we want to use.
    (when (not fcn-in)
      (semantic-fetch-tags)
      (setq fcn-in (semantic-current-tag)))

    (when (or (not fcn-in)
	      (not (semantic-tag-of-class-p fcn-in 'function)))
      (error "No tag of class 'function to insert comment for"))

    (if (not (eq (current-buffer) (semantic-tag-buffer fcn-in)))
	(error "Only insert comments for tags in the current buffer"))

    ;; Find any existing doc strings.
    (semantic-go-to-tag fcn-in)
    (beginning-of-line)
    (forward-char -1)

    (let ((lextok (semantic-documentation-comment-preceeding-tag fcn-in 'lex))
	  (doctext
	   (srecode-document-function-name-comment fcn-in))
	  )

      (when lextok
	(let* ((s (semantic-lex-token-start lextok))
	       (e (semantic-lex-token-end lextok))
	       (plaintext
		(srecode-document-trim-whitespace
		 (save-excursion
		   (goto-char s)
		   (semantic-doc-snarf-comment-for-tag nil))))
	       (extract (condition-case nil
			    (srecode-extract temp s e)
			  (error nil))
			)
	       (distance (count-lines e (semantic-tag-start fcn-in)))
	       (belongelsewhere (save-excursion
				  (goto-char s)
				  (back-to-indentation)
				  (semantic-current-tag)))
	       )

	  (when (not belongelsewhere)

	    (pulse-momentary-highlight-region s e)

	    ;; There are many possible states that comment could be in.
	    ;; Take a guess about what the user would like to do, and ask
	    ;; the right kind of question.
	    (when (or (not (> distance 2))
		      (y-or-n-p "Replace this comment? "))

	      (when (> distance 2)
		(goto-char e)
		(delete-horizontal-space)
		(delete-blank-lines))

	      (cond
	       ((and plaintext (not extract))
		(if (y-or-n-p "Convert old-style comment to Template with old text? ")
		    (setq doctext plaintext))
		(delete-region s e)
		(goto-char s))
	       (extract
		(when (y-or-n-p "Refresh pre-existing comment (recycle old doc)? ")
		  (delete-region s e)
		  (goto-char s)
		  (setq doctext
			(srecode-document-trim-whitespace
			 (srecode-dictionary-lookup-name extract "DOC")))))
	       ))
	    )))

      (beginning-of-line)

      ;; Perform the insertion
      (let ((srecode-semantic-selected-tag fcn-in)
	    (srecode-semantic-apply-tag-augment-hook
	     (lambda (tag dict)
	       (srecode-dictionary-set-value
		dict "DOC"
		(if (eq tag fcn-in)
		    doctext
		  (srecode-document-parameter-comment tag))
		)))
	    )
	(srecode-insert-fcn temp dict)
	))
    ))

;;;###autoload
(defun srecode-document-insert-variable-one-line-comment (&optional var-in)
  "Insert or replace a variable comment.
VAR-IN is the Semantic tag of the function to add a comment too.
If VAR-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just after VAR-IN."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let* ((dict (srecode-create-dictionary))
	 (temp (srecode-template-get-table (srecode-table)
					   "variable-same-line-comment"
					   "declaration"
					   'document)))
    (if (not temp)
	(error "No templates for inserting variable comments"))

    ;; Try to figure out the tag we want to use.
    (when (not var-in)
      (semantic-fetch-tags)
      (setq var-in (semantic-current-tag)))

    (when (or (not var-in)
	      (not (semantic-tag-of-class-p var-in 'variable)))
      (error "No tag of class 'variable to insert comment for"))

    (if (not (eq (current-buffer) (semantic-tag-buffer var-in)))
	(error "Only insert comments for tags in the current buffer"))

    ;; Find any existing doc strings.
    (goto-char (semantic-tag-end var-in))
    (skip-syntax-forward "-" (point-at-eol))
    (let ((lextok (semantic-doc-snarf-comment-for-tag 'lex))
	  )

      (when lextok
	(let ((s (semantic-lex-token-start lextok))
	      (e (semantic-lex-token-end lextok)))

	  (pulse-momentary-highlight-region s e)

	  (when (not (y-or-n-p "A comment already exists.  Replace? "))
	    (error "Quit"))

	  ;; Extract text from the existing comment.
	  (srecode-extract temp s e)

	  (delete-region s e)
	  (goto-char s) ;; To avoid adding a CR.
	  ))
      )

    ;; Clean up the end of the line and use handy comment-column.
    (end-of-line)
    (delete-horizontal-space)
    (move-to-column comment-column t)
    (when (< (point) (point-at-eol)) (end-of-line))

    ;; Perform the insertion
    (let ((srecode-semantic-selected-tag var-in)
	  (srecode-semantic-apply-tag-augment-hook
	   (lambda (tag dict)
	     (srecode-dictionary-set-value
	      dict "DOC" (srecode-document-parameter-comment
			  tag))))
	  )
      (srecode-insert-fcn temp dict)
      ))
  )

;;;###autoload
(defun srecode-document-insert-group-comments (beg end)
  "Insert group comments around the active between BEG and END.
If the region includes only parts of some tags, expand out
to the beginning and end of the tags on the region.
If there is only one tag in the region, complain."
  (interactive "r")
  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let* ((dict (srecode-create-dictionary))
	 (context "declaration")
	 (temp-start nil)
	 (temp-end nil)
	 (tag-start (save-excursion
		      (goto-char beg)
		      (or (semantic-current-tag)
			  (semantic-find-tag-by-overlay-next))))
	 (tag-end (save-excursion
		    (goto-char end)
		    (or (semantic-current-tag)
			(semantic-find-tag-by-overlay-prev))))
	 (parent-tag nil)
	 (first-pos beg)
	 (second-pos end)
	 )

    ;; If beg/end wrapped nothing, then tag-start,end would actually
    ;; point at some odd stuff that is out of order.
    (when (or (not tag-start) (not tag-end)
	      (> (semantic-tag-end tag-start)
		 (semantic-tag-start tag-end)))
      (setq tag-start nil
	    tag-end nil))

    (when tag-start
      ;; If tag-start and -end are the same, and it is a class or
      ;; struct, try to find child tags inside the classdecl.
      (cond
       ((and (eq tag-start tag-end)
	     tag-start
	     (semantic-tag-of-class-p tag-start 'type))
	(setq parent-tag tag-start)
	(setq tag-start (semantic-find-tag-by-overlay-next beg)
	      tag-end (semantic-find-tag-by-overlay-prev end))
	)
       ((eq (semantic-find-tag-parent-by-overlay tag-start) tag-end)
	(setq parent-tag tag-end)
	(setq tag-end (semantic-find-tag-by-overlay-prev end))
	)
       ((eq tag-start (semantic-find-tag-parent-by-overlay tag-end))
	(setq parent-tag tag-start)
	(setq tag-start (semantic-find-tag-by-overlay-next beg))
	)
       )

      (when parent-tag
	;; We are probably in a classdecl
	;; @todo -could I really use (srecode-calculate-context) ?

	(setq context "classdecl")
	)

      ;; Derive start and end locations based on the tags.
      (setq first-pos (semantic-tag-start tag-start)
	    second-pos (semantic-tag-end tag-end))
      )
    ;; Now load the templates
    (setq temp-start (srecode-template-get-table (srecode-table)
						 "group-comment-start"
						 context
						 'document)
	  temp-end (srecode-template-get-table (srecode-table)
					       "group-comment-end"
					       context
					       'document))

    (when (or (not temp-start) (not temp-end))
      (error "No templates for inserting group comments"))

    ;; Setup the name of this group ahead of time.

    ;; @todo - guess at a name based on common strings
    ;;         of the tags in the group.
    (srecode-dictionary-set-value
     dict "GROUPNAME"
     (read-string "Name of group: "))

    ;; Perform the insertion
    ;; Do the end first so we don't need to recalculate anything.
    ;;
    (goto-char second-pos)
    (end-of-line)
    (srecode-insert-fcn temp-end dict)

    (goto-char first-pos)
    (beginning-of-line)
    (srecode-insert-fcn temp-start dict)

    ))


;;; Document Generation Functions
;;
;; Routines for making up English style comments.

(defun srecode-document-function-name-comment (tag)
  "Create documentation for the function defined in TAG.
If we can identify a verb in the list followed by some
name part then check the return value to see if we can use that to
finish off the sentence.  That is, any function with 'alloc' in it will be
allocating something based on its type."
  (let ((al srecode-document-autocomment-return-first-alist)
	(dropit nil)
	(tailit nil)
	(news "")
	(fname (semantic-tag-name tag))
	(retval (or (semantic-tag-type tag) "")))
    (if (listp retval)
	;; convert a type list into a long string to analyze.
	(setq retval (car retval)))
    ;; check for modifiers like static
    (while al
      (if (string-match (car (car al)) (downcase retval))
	  (progn
	    (setq news (concat news (cdr (car al))))
	    (setq dropit t)
	    (setq al nil)))
      (setq al (cdr al)))
    ;; check for verb parts!
    (setq al srecode-document-autocomment-function-alist)
    (while al
      (if (string-match (car (car al)) (downcase fname))
	  (progn
	    (setq news
		  (concat news (if dropit (downcase (cdr (car al)))
				 (cdr (car al)))))
	    ;; if we end in a space, then we are expecting a potential
	    ;; return value.
	    (if (= ?  (aref news (1- (length news))))
		(setq tailit t))
	    (setq al nil)))
      (setq al (cdr al)))
    ;; check for noun parts!
    (setq al srecode-document-autocomment-common-nouns-abbrevs)
    (while al
      (if (string-match (car (car al)) (downcase fname))
	  (progn
	    (setq news
		  (concat news (if dropit (downcase (cdr (car al)))
				 (cdr (car al)))))
	    (setq al nil)))
      (setq al (cdr al)))
    ;; add trailers to names which are obviously returning something.
    (if tailit
	(progn
	  (setq al srecode-document-autocomment-return-last-alist)
	  (while al
	    (if (string-match (car (car al)) (downcase retval))
		(progn
		  (setq news
			(concat news " "
				;; this one may use parts of the return value.
				(format (cdr (car al))
					(srecode-document-programmer->english
					 (substring retval (match-beginning 1)
						    (match-end 1))))))
		  (setq al nil)))
	    (setq al (cdr al)))))
    news))

(defun srecode-document-parameter-comment (param &optional commentlist)
  "Convert tag or string PARAM into a name,comment pair.
Optional COMMENTLIST is list of previously existing comments to
use instead in alist form.  If the name doesn't appear in the list of
standard names, then english it instead."
  (let ((cmt "")
	(aso srecode-document-autocomment-param-alist)
	(fnd nil)
	(name (if (stringp param) param (semantic-tag-name param)))
	(tt (if (stringp param) nil (semantic-tag-type param))))
    ;; Make sure the type is a string.
    (if (listp tt)
	(setq tt (semantic-tag-name tt)))
    ;; Find name description parts.
    (while aso
      (if (string-match (car (car aso)) name)
	  (progn
	    (setq fnd t)
	    (setq cmt (concat cmt (cdr (car aso))))))
      (setq aso (cdr aso)))
    (if (/= (length cmt) 0)
	nil
      ;; finally check for array parts
      (if (and (not (stringp param)) (semantic-tag-modifiers param))
	  (setq cmt (concat cmt "array of ")))
      (setq aso srecode-document-autocomment-param-type-alist)
      (while (and aso tt)
	(if (string-match (car (car aso)) tt)
	    (setq cmt (concat cmt (cdr (car aso)))))
	(setq aso (cdr aso))))
    ;; Convert from programmer to english.
    (if (not fnd)
	(setq cmt (concat cmt " "
			  (srecode-document-programmer->english name))))
    cmt))

(defun srecode-document-programmer->english (programmer)
  "Take PROGRAMMER and convert it into English.
Works with the following rules:
  1) convert all _ into spaces.
  2) inserts spaces between CamelCasing word breaks.
  3) expands noun names based on common programmer nouns.

  This function is designed for variables, not functions.  This does
not account for verb parts."
  (if (string= "" programmer)
      ""
    (let ((ind 0) 			;index in string
	  (llow nil)			;lower/upper case flag
	  (newstr nil)			;new string being generated
	  (al nil))			;autocomment list
      ;;
      ;; 1) Convert underscores
      ;;
      (while (< ind (length programmer))
	(setq newstr (concat newstr
			     (if (= (aref programmer ind) ?_)
				 " " (char-to-string (aref programmer ind)))))
	(setq ind (1+ ind)))
      (setq programmer newstr
	    newstr nil
	    ind 0)
      ;;
      ;; 2) Find word breaks between case changes
      ;;
      (while (< ind (length programmer))
	(setq newstr
	      (concat newstr
		      (let ((tc (aref programmer ind)))
			(if (and (>= tc ?a) (<= tc ?z))
			    (progn
			      (setq llow t)
			      (char-to-string tc))
			  (if llow
			      (progn
				(setq llow nil)
				(concat " " (char-to-string tc)))
			    (char-to-string tc))))))
	(setq ind (1+ ind)))
      ;;
      ;; 3) Expand the words if possible
      ;;
      (setq llow nil
	    ind 0
	    programmer newstr
	    newstr nil)
      (while (string-match (concat "^\\s-*\\([^ \t\n]+\\)") programmer)
	(let ((ts (substring programmer (match-beginning 1) (match-end 1)))
	      (end (match-end 1)))
	  (setq al srecode-document-autocomment-common-nouns-abbrevs)
	  (setq llow nil)
	  (while al
	    (if (string-match (car (car al)) (downcase ts))
		(progn
		  (setq newstr (concat newstr (cdr (car al))))
		  ;; don't terminate because we may actually have 2 words
		  ;; next to each other we didn't identify before
		  (setq llow t)))
	    (setq al (cdr al)))
	  (if (not llow) (setq newstr (concat newstr ts)))
	  (setq newstr (concat newstr " "))
	  (setq programmer (substring programmer end))))
      newstr)))

;;; UTILS
;;
(defun srecode-document-one-line-tag-p (tag)
  "Does TAG fit on one line with space on the end?"
  (save-excursion
    (semantic-go-to-tag tag)
    (and (<= (semantic-tag-end tag) (point-at-eol))
	 (goto-char (semantic-tag-end tag))
	 (< (current-column) 70))))

(provide 'srecode/document)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/document"
;; End:

;;; srecode/document.el ends here
