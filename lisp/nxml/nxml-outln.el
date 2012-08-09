;;; nxml-outln.el --- outline support for nXML mode

;; Copyright (C) 2004, 2007-2012  Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML

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

;; A section can be in one of three states
;; 1. display normally; this displays each child section
;; according to its state; anything not part of child sections is also
;; displayed normally
;; 2. display just the title specially; child sections are not displayed
;; regardless of their state; anything not part of child sections is
;; not displayed
;; 3. display the title specially and display child sections
;; according to their state; anything not part of the child section is
;; not displayed
;; The state of a section is determined by the value of the
;; nxml-outline-state text property of the < character that starts
;; the section.
;; For state 1 the value is nil or absent.
;; For state 2 it is the symbol hide-children.
;; For state 3 it is t.
;; The special display is achieved by using overlays.  The overlays
;; are computed from the nxml-outline-state property by
;; `nxml-refresh-outline'. There overlays all have a category property
;; with an nxml-outline-display property with value t.
;;
;; For a section to be recognized as such, the following conditions must
;; be satisfied:
;; - its start-tag must occur at the start of a line (possibly indented)
;; - its local name must match `nxml-section-element-name-regexp'
;; - it must have a heading element; a heading element is an
;; element whose name matches `nxml-heading-element-name-regexp',
;; and that occurs as, or as a descendant of, the first child element
;; of the section
;;
;; XXX What happens if an nxml-outline-state property is attached to a
;; character that doesn't start a section element?
;;
;; An outlined section (an section with a non-nil nxml-outline-state
;; property) can be displayed in either single-line or multi-line
;; form.  Single-line form is used when the outline state is hide-children
;; or there are no child sections; multi-line form is used otherwise.
;; There are two flavors of single-line form: with children and without.
;; The with-children flavor is used when there are child sections.
;; Single line with children looks like
;;    <+section>A section title...</>
;; Single line without children looks like
;;    <-section>A section title...</>
;; Multi line looks likes
;;    <-section>A section title...
;;    [child sections displayed here]
;;    </-section>
;; The indent of an outlined section is computed relative to the
;; outermost containing outlined element.  The indent of the
;; outermost containing element comes from the non-outlined
;; indent of the section start-tag.

;;; Code:

(require 'xmltok)
(require 'nxml-util)
(require 'nxml-rap)

(defcustom nxml-section-element-name-regexp
  "article\\|\\(sub\\)*section\\|chapter\\|div\\|appendix\\|part\\|preface\\|reference\\|simplesect\\|bibliography\\|bibliodiv\\|glossary\\|glossdiv"
  "Regular expression matching the name of elements used as sections.
An XML element is treated as a section if:

- its local name (that is, the name without the prefix) matches
this regexp;

- either its first child element or a descendant of that first child
element has a local name matching the variable
`nxml-heading-element-name-regexp'; and

- its start-tag occurs at the beginning of a line (possibly indented)."
  :group 'nxml
  :type 'regexp)

(defcustom nxml-heading-element-name-regexp "title\\|head"
  "Regular expression matching the name of elements used as headings.
An XML element is only recognized as a heading if it occurs as or
within the first child of an element that is recognized as a section.
See the variable `nxml-section-element-name-regexp' for more details."
  :group 'nxml
  :type 'regexp)

(defcustom nxml-outline-child-indent 2
  "Indentation in an outline for child element relative to parent element."
  :group 'nxml
  :type 'integer)

(defface nxml-heading
  '((t (:weight bold)))
  "Face used for the contents of abbreviated heading elements."
  :group 'nxml-faces)

(defface nxml-outline-indicator
  '((t (:inherit default)))
  "Face used for `+' or `-' before element names in outlines."
  :group 'nxml-faces)

(defface nxml-outline-active-indicator
  '((t (:box t :inherit nxml-outline-indicator)))
  "Face used for clickable `+' or `-' before element names in outlines."
  :group 'nxml-faces)

(defface nxml-outline-ellipsis
  '((t (:bold t :inherit default)))
  "Face used for `...' in outlines."
  :group 'nxml-faces)

(defvar nxml-heading-scan-distance 1000
  "Maximum distance from section to scan for heading.")

(defvar nxml-outline-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-a" 'nxml-show-all)
    (define-key map "\C-t" 'nxml-hide-all-text-content)
    (define-key map "\C-r" 'nxml-refresh-outline)
    (define-key map "\C-c" 'nxml-hide-direct-text-content)
    (define-key map "\C-e" 'nxml-show-direct-text-content)
    (define-key map "\C-d" 'nxml-hide-subheadings)
    (define-key map "\C-s" 'nxml-show)
    (define-key map "\C-k" 'nxml-show-subheadings)
    (define-key map "\C-l" 'nxml-hide-text-content)
    (define-key map "\C-i" 'nxml-show-direct-subheadings)
    (define-key map "\C-o" 'nxml-hide-other)
    map))

;;; Commands for changing visibility

(defun nxml-show-all ()
  "Show all elements in the buffer normally."
  (interactive)
  (nxml-with-unmodifying-text-property-changes
    (remove-text-properties (point-min)
			    (point-max)
			    '(nxml-outline-state nil)))
  (nxml-outline-set-overlay nil (point-min) (point-max)))

(defun nxml-hide-all-text-content ()
  "Hide all text content in the buffer.
Anything that is in a section but is not a heading will be hidden.
The visibility of headings at any level will not be changed.  See the
variable `nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (nxml-transform-buffer-outline '((nil . t))))

(defun nxml-show-direct-text-content ()
  "Show the text content that is directly part of the section containing point.
Each subsection will be shown according to its individual state, which
will not be changed.  The section containing point is the innermost
section that contains the character following point.  See the variable
`nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (nxml-outline-pre-adjust-point)
  (nxml-set-outline-state (nxml-section-start-position) nil)
  (nxml-refresh-outline)
  (nxml-outline-adjust-point))

(defun nxml-show-direct-subheadings ()
  "Show the immediate subheadings of the section containing point.
The section containing point is the innermost section that contains
the character following point.  See the variable
`nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (let ((pos (nxml-section-start-position)))
    (when (eq (nxml-get-outline-state pos) 'hide-children)
      (nxml-set-outline-state pos t)))
  (nxml-refresh-outline)
  (nxml-outline-adjust-point))

(defun nxml-hide-direct-text-content ()
  "Hide the text content that is directly part of the section containing point.
The heading of the section will remain visible.  The state of
subsections will not be changed.  The section containing point is the
innermost section that contains the character following point.  See the
variable `nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (let ((pos (nxml-section-start-position)))
    (when (null (nxml-get-outline-state pos))
      (nxml-set-outline-state pos t)))
  (nxml-refresh-outline)
  (nxml-outline-adjust-point))

(defun nxml-hide-subheadings ()
  "Hide the subheadings that are part of the section containing point.
The text content will also be hidden, leaving only the heading of the
section itself visible.  The state of the subsections will also be
changed to hide their headings, so that \\[nxml-show-direct-text-content]
would show only the heading of the subsections.  The section containing
point is the innermost section that contains the character following
point.  See the variable `nxml-section-element-name-regexp' for more
details on how to customize which elements are recognized as sections
and headings."
  (interactive)
  (nxml-transform-subtree-outline '((nil . hide-children)
				    (t . hide-children))))

(defun nxml-show ()
  "Show the section containing point normally, without hiding anything.
This includes everything in the section at any level.  The section
containing point is the innermost section that contains the character
following point.  See the variable `nxml-section-element-name-regexp'
for more details on how to customize which elements are recognized as
sections and headings."
  (interactive)
  (nxml-transform-subtree-outline '((hide-children . nil)
				    (t . nil))))

(defun nxml-hide-text-content ()
  "Hide text content at all levels in the section containing point.
The section containing point is the innermost section that contains
the character following point.  See the variable
`nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (nxml-transform-subtree-outline '((nil . t))))

(defun nxml-show-subheadings ()
  "Show the subheadings at all levels of the section containing point.
The visibility of the text content at all levels in the section is not
changed.  The section containing point is the innermost section that
contains the character following point.  See the variable
`nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (nxml-transform-subtree-outline '((hide-children . t))))

(defun nxml-hide-other ()
  "Hide text content other than that directly in the section containing point.
Hide headings other than those of ancestors of that section and their
immediate subheadings.  The section containing point is the innermost
section that contains the character following point.  See the variable
`nxml-section-element-name-regexp' for more details on how to
customize which elements are recognized as sections and headings."
  (interactive)
  (let ((nxml-outline-state-transform-exceptions nil))
    (save-excursion
      (while (and (condition-case err
		      (nxml-back-to-section-start)
		    (nxml-outline-error (nxml-report-outline-error
					 "Couldn't find containing section: %s"
					 err)))
		  (progn
		    (when (and nxml-outline-state-transform-exceptions
			       (null (nxml-get-outline-state (point))))
		      (nxml-set-outline-state (point) t))
		    (setq nxml-outline-state-transform-exceptions
			  (cons (point)
				nxml-outline-state-transform-exceptions))
		    (< nxml-prolog-end (point))))
	(goto-char (1- (point)))))
    (nxml-transform-buffer-outline '((nil . hide-children)
				     (t . hide-children)))))

;; These variables are dynamically bound.  They are use to pass information to
;; nxml-section-tag-transform-outline-state.

(defvar nxml-outline-state-transform-exceptions nil)
(defvar nxml-target-section-pos nil)
(defvar nxml-depth-in-target-section nil)
(defvar nxml-outline-state-transform-alist nil)

(defun nxml-transform-buffer-outline (alist)
  (let ((nxml-target-section-pos nil)
	(nxml-depth-in-target-section 0)
	(nxml-outline-state-transform-alist alist)
	(nxml-outline-display-section-tag-function
	 'nxml-section-tag-transform-outline-state))
    (nxml-refresh-outline))
  (nxml-outline-adjust-point))

(defun nxml-transform-subtree-outline (alist)
  (let ((nxml-target-section-pos (nxml-section-start-position))
	(nxml-depth-in-target-section nil)
	(nxml-outline-state-transform-alist alist)
	(nxml-outline-display-section-tag-function
	 'nxml-section-tag-transform-outline-state))
    (nxml-refresh-outline))
  (nxml-outline-adjust-point))

(defun nxml-outline-pre-adjust-point ()
  (cond ((and (< (point-min) (point))
	      (get-char-property (1- (point)) 'invisible)
	      (not (get-char-property (point) 'invisible))
	      (let ((str (or (get-char-property (point) 'before-string)
			     (get-char-property (point) 'display))))
		(and (stringp str)
		     (>= (length str) 3)
		     (string= (substring str 0 3) "..."))))
	 ;; The ellipsis is a display property on a visible character
	 ;; following an invisible region. The position of the event
	 ;; will be the position before that character. We want to
	 ;; move point to the other side of the invisible region, i.e.
	 ;; following the last visible character before that invisible
	 ;; region.
	 (goto-char (previous-single-char-property-change (1- (point))
							  'invisible)))
	((and (< (point) (point-max))
	      (get-char-property (point) 'display)
	      (get-char-property (1+ (point)) 'invisible))
	 (goto-char (next-single-char-property-change (1+ (point))
						      'invisible)))
	((and (< (point) (point-max))
	      (get-char-property (point) 'invisible))
	 (goto-char (next-single-char-property-change (point)
						      'invisible)))))

(defun nxml-outline-adjust-point ()
  "Adjust point after showing or hiding elements."
  (when (and (get-char-property (point) 'invisible)
	     (< (point-min) (point))
	     (get-char-property (1- (point)) 'invisible))
    (goto-char (previous-single-char-property-change (point)
						     'invisible
						     nil
						     nxml-prolog-end))))

(defun nxml-transform-outline-state (section-start-pos)
  (let* ((old-state
	  (nxml-get-outline-state section-start-pos))
	 (change (assq old-state
		       nxml-outline-state-transform-alist)))
    (when change
      (nxml-set-outline-state section-start-pos
			      (cdr change)))))

(defun nxml-section-tag-transform-outline-state (startp
						 section-start-pos
						 &optional
						 heading-start-pos)
  (if (not startp)
      (setq nxml-depth-in-target-section
	    (and nxml-depth-in-target-section
		 (> nxml-depth-in-target-section 0)
		 (1- nxml-depth-in-target-section)))
    (cond (nxml-depth-in-target-section
	   (setq nxml-depth-in-target-section
		 (1+ nxml-depth-in-target-section)))
	  ((= section-start-pos nxml-target-section-pos)
	   (setq nxml-depth-in-target-section 0)))
    (when (and nxml-depth-in-target-section
	       (not (member section-start-pos
			    nxml-outline-state-transform-exceptions)))
      (nxml-transform-outline-state section-start-pos))))

(defun nxml-get-outline-state (pos)
  (get-text-property pos 'nxml-outline-state))

(defun nxml-set-outline-state (pos state)
  (nxml-with-unmodifying-text-property-changes
    (if state
	(put-text-property pos (1+ pos) 'nxml-outline-state state)
      (remove-text-properties pos (1+ pos) '(nxml-outline-state nil)))))

;;; Mouse interface

(defun nxml-mouse-show-direct-text-content (event)
  "Do the same as \\[nxml-show-direct-text-content] from a mouse click."
  (interactive "e")
  (and (nxml-mouse-set-point event)
       (nxml-show-direct-text-content)))

(defun nxml-mouse-hide-direct-text-content (event)
  "Do the same as \\[nxml-hide-direct-text-content] from a mouse click."
  (interactive "e")
  (and (nxml-mouse-set-point event)
       (nxml-hide-direct-text-content)))

(defun nxml-mouse-hide-subheadings (event)
  "Do the same as \\[nxml-hide-subheadings] from a mouse click."
  (interactive "e")
  (and (nxml-mouse-set-point event)
       (nxml-hide-subheadings)))

(defun nxml-mouse-show-direct-subheadings (event)
  "Do the same as \\[nxml-show-direct-subheadings] from a mouse click."
  (interactive "e")
  (and (nxml-mouse-set-point event)
       (nxml-show-direct-subheadings)))

(defun nxml-mouse-set-point (event)
  (mouse-set-point event)
  (and nxml-prolog-end t))

;; Display

(defsubst nxml-token-start-tag-p ()
  (or (eq xmltok-type 'start-tag)
      (eq xmltok-type 'partial-start-tag)))

(defsubst nxml-token-end-tag-p ()
  (or (eq xmltok-type 'end-tag)
      (eq xmltok-type 'partial-end-tag)))

(defun nxml-refresh-outline ()
  "Refresh the outline to correspond to the current XML element structure."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (kill-local-variable 'line-move-ignore-invisible)
    (make-local-variable 'line-move-ignore-invisible)
    (condition-case err
	(nxml-outline-display-rest nil nil nil)
      (nxml-outline-error
       (nxml-report-outline-error "Cannot display outline: %s" err)))))

(defvar nxml-outline-display-section-tag-function nil)

(defun nxml-outline-display-rest (outline-state start-tag-indent tag-qnames)
  "Display up to and including the end of the current element.
OUTLINE-STATE can be nil, t, hide-children.  START-TAG-INDENT is the
indent of the start-tag of the current element, or nil if no
containing element has a non-nil OUTLINE-STATE.  TAG-QNAMES is a list
of the qnames of the open elements.  Point is after the title content.
Leave point after the closing end-tag.  Return t if we had a
non-transparent child section."
  (let ((last-pos (point))
	(transparent-depth 0)
	;; don't want ellipsis before root element
	(had-children (not tag-qnames)))
    (while
	(cond ((not (nxml-section-tag-forward))
	       (if (null tag-qnames)
		   nil
		 (nxml-outline-error "missing end-tag %s"
				     (car tag-qnames))))
	      ;; section end-tag
	      ((nxml-token-end-tag-p)
	       (when nxml-outline-display-section-tag-function
		 (funcall nxml-outline-display-section-tag-function
			  nil
			  xmltok-start))
	       (let ((qname (xmltok-end-tag-qname)))
		 (unless tag-qnames
		   (nxml-outline-error "extra end-tag %s" qname))
		 (unless (string= (car tag-qnames) qname)
		   (nxml-outline-error "mismatched end-tag; expected %s, got %s"
				       (car tag-qnames)
				       qname)))
	       (cond ((> transparent-depth 0)
		      (setq transparent-depth (1- transparent-depth))
		      (setq tag-qnames (cdr tag-qnames))
		      t)
		     ((not outline-state)
		      (nxml-outline-set-overlay nil last-pos (point))
		      nil)
		     ((or (not had-children)
			  (eq outline-state 'hide-children))
		      (nxml-outline-display-single-line-end-tag last-pos)
		      nil)
		     (t
		      (nxml-outline-display-multi-line-end-tag last-pos
							       start-tag-indent)
		      nil)))
	      ;; section start-tag
	      (t
	       (let* ((qname (xmltok-start-tag-qname))
		      (section-start-pos xmltok-start)
		      (heading-start-pos
		       (and (or nxml-outline-display-section-tag-function
				(not (eq outline-state 'had-children))
				(not had-children))
			    (nxml-token-starts-line-p)
			    (nxml-heading-start-position))))
		 (when nxml-outline-display-section-tag-function
		   (funcall nxml-outline-display-section-tag-function
			    t
			    section-start-pos
			    heading-start-pos))
		 (setq tag-qnames (cons qname tag-qnames))
		 (if (or (not heading-start-pos)
			 (and (eq outline-state 'hide-children)
			      (setq had-children t)))
		     (setq transparent-depth (1+ transparent-depth))
		   (nxml-display-section last-pos
					 section-start-pos
					 heading-start-pos
					 start-tag-indent
					 outline-state
					 had-children
					 tag-qnames)
		   (setq had-children t)
		   (setq tag-qnames (cdr tag-qnames))
		   (setq last-pos (point))))
	       t)))
    had-children))

(defconst nxml-highlighted-less-than
  (propertize "<" 'face 'nxml-tag-delimiter))

(defconst nxml-highlighted-greater-than
  (propertize ">" 'face 'nxml-tag-delimiter))

(defconst nxml-highlighted-colon
  (propertize ":" 'face 'nxml-element-colon))

(defconst nxml-highlighted-slash
  (propertize "/" 'face 'nxml-tag-slash))

(defconst nxml-highlighted-ellipsis
  (propertize "..." 'face 'nxml-outline-ellipsis))

(defconst nxml-highlighted-empty-end-tag
  (concat nxml-highlighted-ellipsis
	  nxml-highlighted-less-than
	  nxml-highlighted-slash
	  nxml-highlighted-greater-than))

(defconst nxml-highlighted-inactive-minus
  (propertize "-" 'face 'nxml-outline-indicator))

(defconst nxml-highlighted-active-minus
  (propertize "-" 'face 'nxml-outline-active-indicator))

(defconst nxml-highlighted-active-plus
  (propertize "+" 'face 'nxml-outline-active-indicator))

(defun nxml-display-section (last-pos
			     section-start-pos
			     heading-start-pos
			     parent-indent
			     parent-outline-state
			     had-children
			     tag-qnames)
  (let* ((section-start-pos-bol
	  (save-excursion
	    (goto-char section-start-pos)
	    (skip-chars-backward " \t")
	    (point)))
	 (outline-state (nxml-get-outline-state section-start-pos))
	 (newline-before-section-start-category
	  (cond ((and (not had-children) parent-outline-state)
		 'nxml-outline-display-ellipsis)
		 (outline-state 'nxml-outline-display-show)
		 (t nil))))
    (nxml-outline-set-overlay (and parent-outline-state
				   'nxml-outline-display-hide)
			      last-pos
			      (1- section-start-pos-bol)
			      nil
			      t)
    (if outline-state
      (let* ((indent (if parent-indent
			 (+ parent-indent nxml-outline-child-indent)
		       (save-excursion
			 (goto-char section-start-pos)
			 (current-column))))
	     start-tag-overlay)
	(nxml-outline-set-overlay newline-before-section-start-category
				  (1- section-start-pos-bol)
				  section-start-pos-bol
				  t)
	(nxml-outline-set-overlay 'nxml-outline-display-hide
				  section-start-pos-bol
				  section-start-pos)
	(setq start-tag-overlay
	    (nxml-outline-set-overlay 'nxml-outline-display-show
				      section-start-pos
				      (1+ section-start-pos)
				      t))
	;; line motion commands don't work right if start-tag-overlay
	;; covers multiple lines
	(nxml-outline-set-overlay 'nxml-outline-display-hide
				  (1+ section-start-pos)
				  heading-start-pos)
	(goto-char heading-start-pos)
	(nxml-end-of-heading)
	(nxml-outline-set-overlay 'nxml-outline-display-heading
				  heading-start-pos
				  (point))
	(let* ((had-children
		(nxml-outline-display-rest outline-state
					   indent
					   tag-qnames)))
	  (overlay-put start-tag-overlay
		       'display
		       (concat
			;; indent
			(make-string indent ?\ )
			;; <
			nxml-highlighted-less-than
			;; + or - indicator
			(cond ((not had-children)
			       nxml-highlighted-inactive-minus)
			      ((eq outline-state 'hide-children)
			       (overlay-put start-tag-overlay
					    'category
					    'nxml-outline-display-hiding-tag)
			       nxml-highlighted-active-plus)
			      (t
			       (overlay-put start-tag-overlay
					    'category
					    'nxml-outline-display-showing-tag)
			       nxml-highlighted-active-minus))
			;; qname
			(nxml-highlighted-qname (car tag-qnames))
			;; >
			nxml-highlighted-greater-than))))
      ;; outline-state nil
      (goto-char heading-start-pos)
      (nxml-end-of-heading)
      (nxml-outline-set-overlay newline-before-section-start-category
				(1- section-start-pos-bol)
				(point)
				t)
      (nxml-outline-display-rest outline-state
				 (and parent-indent
				      (+ parent-indent
					 nxml-outline-child-indent))
				 tag-qnames))))

(defun nxml-highlighted-qname (qname)
  (let ((colon (string-match ":" qname)))
    (if colon
	(concat (propertize (substring qname 0 colon)
			    'face
			    'nxml-element-prefix)
		nxml-highlighted-colon
		(propertize (substring qname (1+ colon))
			    'face
			    'nxml-element-local-name))
      (propertize qname
		  'face
		  'nxml-element-local-name))))

(defun nxml-outline-display-single-line-end-tag (last-pos)
  (nxml-outline-set-overlay 'nxml-outline-display-hide
			    last-pos
			    xmltok-start
			    nil
			    t)
  (overlay-put (nxml-outline-set-overlay 'nxml-outline-display-show
					 xmltok-start
					 (point)
					 t)
	       'display
	       nxml-highlighted-empty-end-tag))

(defun nxml-outline-display-multi-line-end-tag (last-pos start-tag-indent)
  (let ((indentp (save-excursion
		   (goto-char last-pos)
		   (skip-chars-forward " \t")
		   (and (eq (char-after) ?\n)
			(progn
			  (goto-char (1+ (point)))
			  (nxml-outline-set-overlay nil last-pos (point))
			  (setq last-pos (point))
			  (goto-char xmltok-start)
			  (beginning-of-line)
			  t))))
	end-tag-overlay)
    (nxml-outline-set-overlay 'nxml-outline-display-hide
			      last-pos
			      xmltok-start
			      nil
			      t)
    (setq end-tag-overlay
	  (nxml-outline-set-overlay 'nxml-outline-display-showing-tag
				    xmltok-start
				    (point)
				    t))
    (overlay-put end-tag-overlay
		 'display
		 (concat (if indentp
			     (make-string start-tag-indent ?\ )
			   "")
			 nxml-highlighted-less-than
			 nxml-highlighted-slash
			 nxml-highlighted-active-minus
			 (nxml-highlighted-qname (xmltok-end-tag-qname))
			 nxml-highlighted-greater-than))))

(defvar nxml-outline-show-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'nxml-show-direct-text-content)
    (define-key map [mouse-2] 'nxml-mouse-show-direct-text-content)
    map))

(defvar nxml-outline-show-help "mouse-2: show")

(put 'nxml-outline-display-show 'nxml-outline-display t)
(put 'nxml-outline-display-show 'evaporate t)
(put 'nxml-outline-display-show 'keymap nxml-outline-show-map)
(put 'nxml-outline-display-show 'help-echo nxml-outline-show-help)

(put 'nxml-outline-display-hide 'nxml-outline-display t)
(put 'nxml-outline-display-hide 'evaporate t)
(put 'nxml-outline-display-hide 'invisible t)
(put 'nxml-outline-display-hide 'keymap nxml-outline-show-map)
(put 'nxml-outline-display-hide 'help-echo nxml-outline-show-help)

(put 'nxml-outline-display-ellipsis 'nxml-outline-display t)
(put 'nxml-outline-display-ellipsis 'evaporate t)
(put 'nxml-outline-display-ellipsis 'keymap nxml-outline-show-map)
(put 'nxml-outline-display-ellipsis 'help-echo nxml-outline-show-help)
(put 'nxml-outline-display-ellipsis 'before-string nxml-highlighted-ellipsis)

(put 'nxml-outline-display-heading 'keymap nxml-outline-show-map)
(put 'nxml-outline-display-heading 'help-echo nxml-outline-show-help)
(put 'nxml-outline-display-heading 'nxml-outline-display t)
(put 'nxml-outline-display-heading 'evaporate t)
(put 'nxml-outline-display-heading 'face 'nxml-heading)

(defvar nxml-outline-hiding-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'nxml-mouse-show-direct-subheadings)
    (define-key map [mouse-2] 'nxml-mouse-show-direct-text-content)
    (define-key map "\C-m" 'nxml-show-direct-text-content)
    map))

(defvar nxml-outline-hiding-tag-help
  "mouse-1: show subheadings, mouse-2: show text content")

(put 'nxml-outline-display-hiding-tag 'nxml-outline-display t)
(put 'nxml-outline-display-hiding-tag 'evaporate t)
(put 'nxml-outline-display-hiding-tag 'keymap nxml-outline-hiding-tag-map)
(put 'nxml-outline-display-hiding-tag 'help-echo nxml-outline-hiding-tag-help)

(defvar nxml-outline-showing-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'nxml-mouse-hide-subheadings)
    (define-key map [mouse-2] 'nxml-mouse-show-direct-text-content)
    (define-key map "\C-m" 'nxml-show-direct-text-content)
    map))

(defvar nxml-outline-showing-tag-help
  "mouse-1: hide subheadings, mouse-2: show text content")

(put 'nxml-outline-display-showing-tag 'nxml-outline-display t)
(put 'nxml-outline-display-showing-tag 'evaporate t)
(put 'nxml-outline-display-showing-tag 'keymap nxml-outline-showing-tag-map)
(put 'nxml-outline-display-showing-tag
     'help-echo
     nxml-outline-showing-tag-help)

(defun nxml-outline-set-overlay (category
				 start
				 end
				 &optional
				 front-advance
				 rear-advance)
  "Replace any `nxml-outline-display' overlays between START and END.
Overlays are removed if they overlay the region between START and END,
and have a non-nil `nxml-outline-display' property (typically via their
category).  If CATEGORY is non-nil, they will be replaced with a new
overlay with that category from START to END.  If CATEGORY is nil,
no new overlay will be created."
  (when (< start end)
    (let ((overlays (overlays-in start end))
	  overlay)
      (while overlays
	(setq overlay (car overlays))
	(setq overlays (cdr overlays))
	(when (overlay-get overlay 'nxml-outline-display)
	  (delete-overlay overlay))))
    (and category
	 (let ((overlay (make-overlay start
				      end
				      nil
				      front-advance
				      rear-advance)))
	   (overlay-put overlay 'category category)
	   (setq line-move-ignore-invisible t)
	   overlay))))

(defun nxml-end-of-heading ()
  "Move from the start of the content of the heading to the end.
Do not move past the end of the line."
  (let ((pos (condition-case err
		 (and (nxml-scan-element-forward (point) t)
		      xmltok-start)
	       (nxml-scan-error nil))))
    (end-of-line)
    (skip-chars-backward " \t")
    (cond ((not pos)
	   (setq pos (nxml-token-before))
	   (when (eq xmltok-type 'end-tag)
	     (goto-char pos)))
	  ((< pos (point))
	   (goto-char pos)))
    (skip-chars-backward " \t")
    (point)))

;;; Navigating section structure

(defun nxml-token-starts-line-p ()
  (save-excursion
    (goto-char xmltok-start)
    (skip-chars-backward " \t")
    (bolp)))

(defvar nxml-cached-section-tag-regexp nil)
(defvar nxml-cached-section-element-name-regexp nil)

(defsubst nxml-make-section-tag-regexp ()
  (if (eq nxml-cached-section-element-name-regexp
	  nxml-section-element-name-regexp)
      nxml-cached-section-tag-regexp
    (nxml-make-section-tag-regexp-1)))

(defun nxml-make-section-tag-regexp-1 ()
  (setq nxml-cached-section-element-name-regexp nil)
  (setq nxml-cached-section-tag-regexp
	(concat "</?\\("
		"\\(" xmltok-ncname-regexp ":\\)?"
		nxml-section-element-name-regexp
		"\\)[ \t\r\n>]"))
  (setq nxml-cached-section-element-name-regexp
	nxml-section-element-name-regexp)
  nxml-cached-section-tag-regexp)

(defun nxml-section-tag-forward ()
  "Move forward past the first tag that is a section start- or end-tag.
Return `xmltok-type' for tag.
If no tag found, return nil and move to the end of the buffer."
  (let ((case-fold-search nil)
	(tag-regexp (nxml-make-section-tag-regexp))
	match-end)
    (when (< (point) nxml-prolog-end)
      (goto-char nxml-prolog-end))
    (while (cond ((not (re-search-forward tag-regexp nil 'move))
		  (setq xmltok-type nil)
		  nil)
		 ((progn
		    (goto-char (match-beginning 0))
		    (setq match-end (match-end 0))
		    (nxml-ensure-scan-up-to-date)
		    (let ((end (nxml-inside-end (point))))
		      (when end
			(goto-char end)
			t))))
		 ((progn
		    (xmltok-forward)
		    (and (memq xmltok-type '(start-tag
					     partial-start-tag
					     end-tag
					     partial-end-tag))
			 ;; just in case wildcard matched non-name chars
			 (= xmltok-name-end (1- match-end))))
		  nil)
		 (t))))
    xmltok-type)

(defun nxml-section-tag-backward ()
  "Move backward to the end of a tag that is a section start- or end-tag.
The position of the end of the tag must be <= point.
Point is at the end of the tag.  `xmltok-start' is the start."
  (let ((case-fold-search nil)
	(start (point))
	(tag-regexp (nxml-make-section-tag-regexp))
	match-end)
    (if (< (point) nxml-prolog-end)
	(progn
	  (goto-char (point-min))
	  nil)
      (while (cond ((not (re-search-backward tag-regexp
					     nxml-prolog-end
					     'move))
		    (setq xmltok-type nil)
		    (goto-char (point-min))
		    nil)
		   ((progn
		      (goto-char (match-beginning 0))
		      (setq match-end (match-end 0))
		      (nxml-ensure-scan-up-to-date)
		      (let ((pos (nxml-inside-start (point))))
			(when pos
			  (goto-char (1- pos))
			  t))))
		   ((progn
		      (xmltok-forward)
		      (and (<= (point) start)
			   (memq xmltok-type '(start-tag
					       partial-start-tag
					       end-tag
					       partial-end-tag))
			   ;; just in case wildcard matched non-name chars
			   (= xmltok-name-end (1- match-end))))
		    nil)
		   (t (goto-char xmltok-start)
		      t)))
      xmltok-type)))

(defun nxml-section-start-position ()
  "Return the position of the start of the section containing point.
Signal an error on failure."
  (condition-case err
      (save-excursion (if (nxml-back-to-section-start)
			  (point)
			(error "Not in section")))
    (nxml-outline-error
     (nxml-report-outline-error "Couldn't determine containing section: %s"
				err))))

(defun nxml-back-to-section-start (&optional invisible-ok)
  "Try to move back to the start of the section containing point.
The start of the section must be <= point.
Only visible sections are included unless INVISIBLE-OK is non-nil.
If found, return t.  Otherwise move to `point-min' and return nil.
If unbalanced section tags are found, signal an `nxml-outline-error'."
  (when (or (nxml-after-section-start-tag)
	    (nxml-section-tag-backward))
    (let (open-tags found)
      (while (let (section-start-pos)
	       (setq section-start-pos xmltok-start)
	       (if (nxml-token-end-tag-p)
		   (setq open-tags (cons (xmltok-end-tag-qname)
					 open-tags))
		 (if (not open-tags)
		     (when (and (nxml-token-starts-line-p)
				(or invisible-ok
				    (not (get-char-property section-start-pos
							    'invisible)))
				(nxml-heading-start-position))
		       (setq found t))
		   (let ((qname (xmltok-start-tag-qname)))
		     (unless (string= (car open-tags) qname)
		       (nxml-outline-error "mismatched end-tag"))
		     (setq open-tags (cdr open-tags)))))
	       (goto-char section-start-pos)
	       (and (not found)
		    (nxml-section-tag-backward))))
      found)))

(defun nxml-after-section-start-tag ()
  "If the character after point is in a section start-tag, move after it.
Return the token type.  Otherwise return nil.
Set up variables like `xmltok-forward'."
  (let ((pos (nxml-token-after))
	(case-fold-search nil))
   (when (and (memq xmltok-type '(start-tag partial-start-tag))
	      (save-excursion
		(goto-char xmltok-start)
		(looking-at (nxml-make-section-tag-regexp))))
     (goto-char pos)
     xmltok-type)))

(defun nxml-heading-start-position ()
  "Return the position of the start of the content of a heading element.
Adjust the position to be after initial leading whitespace.
Return nil if no heading element is found.  Requires point to be
immediately after the section's start-tag."
  (let ((depth 0)
	(heading-regexp (concat "\\`\\("
				nxml-heading-element-name-regexp
				"\\)\\'"))

	(section-regexp (concat "\\`\\("
				nxml-section-element-name-regexp
				"\\)\\'"))
	(start (point))
	found)
    (save-excursion
      (while (and (xmltok-forward)
		  (cond ((memq xmltok-type '(end-tag partial-end-tag))
			 (and (not (string-match section-regexp
						 (xmltok-end-tag-local-name)))
			      (> depth 0)
			      (setq depth (1- depth))))
			;; XXX Not sure whether this is a good idea
			;;((eq xmltok-type 'empty-element)
			;; nil)
			((not (memq xmltok-type
				    '(start-tag partial-start-tag)))
			 t)
			((string-match section-regexp
				       (xmltok-start-tag-local-name))
			 nil)
			((string-match heading-regexp
				       (xmltok-start-tag-local-name))
			 (skip-chars-forward " \t\r\n")
			 (setq found (point))
			 nil)
			(t
			 (setq depth (1+ depth))
			 t))
		  (<= (- (point) start) nxml-heading-scan-distance))))
    found))

;;; Error handling

(defun nxml-report-outline-error (msg err)
  (error msg (apply 'format (cdr err))))

(defun nxml-outline-error (&rest args)
  (signal 'nxml-outline-error args))

(put 'nxml-outline-error
     'error-conditions
     '(error nxml-error nxml-outline-error))

(put 'nxml-outline-error
     'error-message
     "Cannot create outline of buffer that is not well-formed")

;;; Debugging

(defun nxml-debug-overlays ()
  (interactive)
  (let ((overlays (nreverse (overlays-in (point-min) (point-max))))
	overlay)
    (while overlays
      (setq overlay (car overlays))
      (setq overlays (cdr overlays))
      (when (overlay-get overlay 'nxml-outline-display)
	(message "overlay %s: %s...%s (%s)"
		 (overlay-get overlay 'category)
		 (overlay-start overlay)
		 (overlay-end overlay)
		 (overlay-get overlay 'display))))))

(provide 'nxml-outln)

;;; nxml-outln.el ends here
