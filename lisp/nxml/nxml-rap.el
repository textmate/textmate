;;; nxml-rap.el --- low-level support for random access parsing for nXML mode

;; Copyright (C) 2003-2004, 2007-2012 Free Software Foundation, Inc.

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

;; This uses xmltok.el to do XML parsing. The fundamental problem is
;; how to handle changes. We don't want to maintain a complete parse
;; tree.  We also don't want to reparse from the start of the document
;; on every keystroke.  However, it is not possible in general to
;; parse an XML document correctly starting at a random point in the
;; middle.  The main problems are comments, CDATA sections and
;; processing instructions: these can all contain things that are
;; indistinguishable from elements. Literals in the prolog are also a
;; problem.  Attribute value literals are not a problem because
;; attribute value literals cannot contain less-than signs.
;;
;; Our strategy is to keep track of just the problematic things.
;; Specifically, we keep track of all comments, CDATA sections and
;; processing instructions in the instance.  We do this by marking all
;; except the first character of these with a non-nil nxml-inside text
;; property. The value of the nxml-inside property is comment,
;; cdata-section or processing-instruction.  The first character does
;; not have the nxml-inside property so we can find the beginning of
;; the construct by looking for a change in a text property value
;; (Emacs provides primitives for this).  We use text properties
;; rather than overlays, since the implementation of overlays doesn't
;; look like it scales to large numbers of overlays in a buffer.
;;
;; We don't in fact track all these constructs, but only track them in
;; some initial part of the instance. The variable `nxml-scan-end'
;; contains the limit of where we have scanned up to for them.
;;
;; Thus to parse some random point in the file we first ensure that we
;; have scanned up to that point.  Then we search backwards for a
;; <. Then we check whether the < has an nxml-inside property. If it
;; does we go backwards to first character that does not have an
;; nxml-inside property (this character must be a <).  Then we start
;; parsing forward from the < we have found.
;;
;; The prolog has to be parsed specially, so we also keep track of the
;; end of the prolog in `nxml-prolog-end'. The prolog is reparsed on
;; every change to the prolog.  This won't work well if people try to
;; edit huge internal subsets. Hopefully that will be rare.
;;
;; We keep track of the changes by adding to the buffer's
;; after-change-functions hook.  Scanning is also done as a
;; prerequisite to fontification by adding to fontification-functions
;; (in the same way as jit-lock).  This means that scanning for these
;; constructs had better be quick.  Fortunately it is. Firstly, the
;; typical proportion of comments, CDATA sections and processing
;; instructions is small relative to other things.  Secondly, to scan
;; we just search for the regexp <[!?].
;;
;; One problem is unclosed comments, processing instructions and CDATA
;; sections.  Suppose, for example, we encounter a <!-- but there's no
;; matching -->.  This is not an unexpected situation if the user is
;; creating a comment. It is not helpful to treat the whole of the
;; file starting from the <!-- onwards as a single unclosed comment
;; token. Instead we treat just the <!-- as a piece of not well-formed
;; markup and continue.  The problem is that if at some later stage a
;; --> gets added to the buffer after the unclosed <!--, we will need
;; to reparse the buffer starting from the <!--.  We need to keep
;; track of these reparse dependencies; they are called dependent
;; regions in the code.

;;; Code:

(require 'xmltok)
(require 'nxml-util)

(defvar nxml-prolog-end nil
  "Integer giving position following end of the prolog.")
(make-variable-buffer-local 'nxml-prolog-end)

(defvar nxml-scan-end nil
  "Marker giving position up to which we have scanned.
nxml-scan-end must be >= nxml-prolog-end.  Furthermore, nxml-scan-end
must not be an inside position in the following sense.  A position is
inside if the following character is a part of, but not the first
character of, a CDATA section, comment or processing instruction.
Furthermore all positions >= nxml-prolog-end and < nxml-scan-end that
are inside positions must have a non-nil `nxml-inside' property whose
value is a symbol specifying what it is inside.  Any characters with a
non-nil `fontified' property must have position < nxml-scan-end and
the correct face.  Dependent regions must also be established for any
unclosed constructs starting before nxml-scan-end.
There must be no `nxml-inside' properties after nxml-scan-end.")
(make-variable-buffer-local 'nxml-scan-end)

(defsubst nxml-get-inside (pos)
  (get-text-property pos 'nxml-inside))

(defsubst nxml-clear-inside (start end)
  (nxml-debug-clear-inside start end)
  (remove-text-properties start end '(nxml-inside nil)))

(defsubst nxml-set-inside (start end type)
  (nxml-debug-set-inside start end)
  (put-text-property start end 'nxml-inside type))

(defun nxml-inside-end (pos)
  "Return the end of the inside region containing POS.
Return nil if the character at POS is not inside."
  (if (nxml-get-inside pos)
      (or (next-single-property-change pos 'nxml-inside)
	  (point-max))
    nil))

(defun nxml-inside-start (pos)
  "Return the start of the inside region containing POS.
Return nil if the character at POS is not inside."
  (if (nxml-get-inside pos)
      (or (previous-single-property-change (1+ pos) 'nxml-inside)
	  (point-min))
    nil))

;;; Change management

(defun nxml-scan-after-change (start end)
  "Restore `nxml-scan-end' invariants after a change.
The change happened between START and END.
Return position after which lexical state is unchanged.
END must be > `nxml-prolog-end'.  START must be outside
any 'inside' regions and at the beginning of a token."
  (if (>= start nxml-scan-end)
      nxml-scan-end
    (let ((inside-remove-start start)
	  xmltok-errors
	  xmltok-dependent-regions)
      (while (or (when (xmltok-forward-special (min end nxml-scan-end))
		   (when (memq xmltok-type
			       '(comment
				 cdata-section
				 processing-instruction))
		     (nxml-clear-inside inside-remove-start
					(1+ xmltok-start))
		     (nxml-set-inside (1+ xmltok-start)
				      (point)
				      xmltok-type)
		     (setq inside-remove-start (point)))
		   (if (< (point) (min end nxml-scan-end))
		       t
		     (setq end (point))
		     nil))
		 ;; The end of the change was inside but is now outside.
		 ;; Imagine something really weird like
		 ;; <![CDATA[foo <!-- bar ]]> <![CDATA[ stuff --> <!-- ]]> -->
		 ;; and suppose we deleted "<![CDATA[f"
		 (let ((inside-end (nxml-inside-end end)))
		   (when inside-end
		     (setq end inside-end)
		     t))))
      (nxml-clear-inside inside-remove-start end)
      (nxml-clear-dependent-regions start end)
      (nxml-mark-parse-dependent-regions))
    (when (> end nxml-scan-end)
      (set-marker nxml-scan-end end))
    end))

;; n-s-p only called from nxml-mode.el, where this variable is defined.
(defvar nxml-prolog-regions)

(defun nxml-scan-prolog ()
  (goto-char (point-min))
  (let (xmltok-dtd
	xmltok-errors
	xmltok-dependent-regions)
    (setq nxml-prolog-regions (xmltok-forward-prolog))
    (setq nxml-prolog-end (point))
    (nxml-clear-inside (point-min) nxml-prolog-end)
    (nxml-clear-dependent-regions (point-min) nxml-prolog-end)
    (nxml-mark-parse-dependent-regions))
  (when (< nxml-scan-end nxml-prolog-end)
    (set-marker nxml-scan-end nxml-prolog-end)))


;;; Dependent regions

(defun nxml-adjust-start-for-dependent-regions (start end pre-change-length)
  (let ((overlays (overlays-in (1- start) start))
	(adjusted-start start))
    (while overlays
      (let* ((overlay (car overlays))
	     (ostart (overlay-start overlay)))
	(when (and (eq (overlay-get overlay 'category) 'nxml-dependent)
		   (< ostart adjusted-start))
	  (let ((funargs (overlay-get overlay 'nxml-funargs)))
	    (when (apply (car funargs)
			 (append (list start
				       end
				       pre-change-length
				       ostart
				       (overlay-end overlay))
				 (cdr funargs)))
	      (setq adjusted-start ostart)))))
      (setq overlays (cdr overlays)))
    adjusted-start))

(defun nxml-mark-parse-dependent-regions ()
  (while xmltok-dependent-regions
    (apply 'nxml-mark-parse-dependent-region
	   (car xmltok-dependent-regions))
    (setq xmltok-dependent-regions
	  (cdr xmltok-dependent-regions))))

(defun nxml-mark-parse-dependent-region (fun start end &rest args)
  (let ((overlay (make-overlay start end nil t t)))
    (overlay-put overlay 'category 'nxml-dependent)
    (overlay-put overlay 'nxml-funargs (cons fun args))))

(put 'nxml-dependent 'evaporate t)

(defun nxml-clear-dependent-regions (start end)
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let* ((overlay (car overlays))
	     (category (overlay-get overlay 'category)))
	(when (and (eq category 'nxml-dependent)
		   (<= start (overlay-start overlay)))
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

;;; Random access parsing

(defun nxml-token-after ()
  "Return the position after the token containing the char after point.
Sets up the variables `xmltok-type', `xmltok-start',
`xmltok-name-end', `xmltok-name-colon', `xmltok-attributes',
`xmltok-namespace-attributes' in the same was as does
`xmltok-forward'.  The prolog will be treated as a single token with
type `prolog'."
  (let ((pos (point)))
    (if (< pos nxml-prolog-end)
	(progn
	  (setq xmltok-type 'prolog
		xmltok-start (point-min))
	  (min nxml-prolog-end (point-max)))
      (nxml-ensure-scan-up-to-date)
      (if (nxml-get-inside pos)
	  (save-excursion
	    (nxml-move-outside-backwards)
	    (xmltok-forward)
	    (point))
	(save-excursion
	  (if (or (eq (char-after) ?<)
		      (search-backward "<"
				       (max (point-min) nxml-prolog-end)
				       t))
	      (nxml-move-outside-backwards)
	    (goto-char (if (<= (point-min) nxml-prolog-end)
			   nxml-prolog-end
			 (or (nxml-inside-end (point-min))
			     (point-min)))))
	  (while (and (nxml-tokenize-forward)
		      (<= (point) pos)))
	  (point))))))

(defun nxml-token-before ()
  "Return the position after the token containing the char before point.
Sets variables like `nxml-token-after'."
  (if (/= (point-min) (point))
      (save-excursion
	(goto-char (1- (point)))
	(nxml-token-after))
    (setq xmltok-start (point))
    (setq xmltok-type nil)
    (point)))

(defun nxml-tokenize-forward ()
  (let (xmltok-dependent-regions
	xmltok-errors)
    (when (and (xmltok-forward)
	       (> (point) nxml-scan-end))
      (cond ((memq xmltok-type '(comment
				 cdata-section
				 processing-instruction))
	     (nxml-with-unmodifying-text-property-changes
	       (nxml-set-inside (1+ xmltok-start) (point) xmltok-type)))
	    (xmltok-dependent-regions
	     (nxml-mark-parse-dependent-regions)))
      (set-marker nxml-scan-end (point)))
    xmltok-type))

(defun nxml-move-tag-backwards (bound)
  "Move point backwards outside any 'inside' regions or tags.
Point will not move past `nxml-prolog-end'.
Point will either be at BOUND or a '<' character starting a tag
outside any 'inside' regions.  Ignores dependent regions.
As a precondition, point must be >= BOUND."
  (nxml-move-outside-backwards)
  (when (not (equal (char-after) ?<))
    (if (search-backward "<" bound t)
        (progn
          (nxml-move-outside-backwards)
          (when (not (equal (char-after) ?<))
            (search-backward "<" bound t)))
      (goto-char bound))))

(defun nxml-move-outside-backwards ()
  "Move point to first character of the containing special thing.
Leave point unmoved if it is not inside anything special."
  (let ((start (nxml-inside-start (point))))
    (when start
      (goto-char (1- start))
      (when (nxml-get-inside (point))
	(error "Char before inside-start at %s had nxml-inside property %s"
	       (point)
	       (nxml-get-inside (point)))))))

(defun nxml-ensure-scan-up-to-date ()
  (let ((pos (point)))
    (when (< nxml-scan-end pos)
      (save-excursion
	(goto-char nxml-scan-end)
	(let (xmltok-errors
	      xmltok-dependent-regions)
	  (while (when (xmltok-forward-special pos)
		   (when (memq xmltok-type
			       '(comment
				 processing-instruction
				 cdata-section))
		     (nxml-with-unmodifying-text-property-changes
		       (nxml-set-inside (1+ xmltok-start)
					(point)
					xmltok-type)))
		   (if (< (point) pos)
		       t
		     (setq pos (point))
		     nil)))
	  (nxml-clear-dependent-regions nxml-scan-end pos)
	  (nxml-mark-parse-dependent-regions)
	  (set-marker nxml-scan-end pos))))))

;;; Element scanning

(defun nxml-scan-element-forward (from &optional up)
  "Scan forward from FROM over a single balanced element.
Point must be between tokens.  Return the position of the end of
the tag that ends the element. `xmltok-start' will contain the
position of the start of the tag.  If UP is non-nil, then scan
past end-tag of element containing point.  If no element is
found, return nil.  If a well-formedness error prevents scanning,
signal an `nxml-scan-error'.  Point is not moved."
  (let ((open-tags (and up t))
	found)
    (save-excursion
      (goto-char from)
      (while (cond ((not (nxml-tokenize-forward))
		    (when (consp open-tags)
		      (nxml-scan-error (cadr open-tags)
				       "Start-tag has no end-tag"))
		    nil)
		   ((eq xmltok-type 'start-tag)
		    (setq open-tags
			  (cons (xmltok-start-tag-qname)
				(cons xmltok-start
				      open-tags)))
		    t)
		   ((eq xmltok-type 'end-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags)) (setq found (point)) nil)
			  ((not (string= (car open-tags)
					 (xmltok-end-tag-qname)))
			   (nxml-scan-error (+ 2 xmltok-start)
					    "Mismatched end-tag; \
expected `%s'"
					    (car open-tags)))
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found (point)) nil)))
		   ((memq xmltok-type '(empty-element
					partial-empty-element))
		    (if open-tags
			t
		      (setq found (point))
		      nil))
		   ((eq xmltok-type 'partial-end-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags)) (setq found (point)) nil)
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found (point)) nil)))
		   ((eq xmltok-type 'partial-start-tag)
		    (nxml-scan-error xmltok-start
				     "Missing `>'"))
		   (t t))))
    found))

(defun nxml-scan-element-backward (from &optional up bound)
  "Scan backward from FROM over a single balanced element.
Point must be between tokens.  Return the position of the end of
the tag that starts the element. `xmltok-start' will contain the
position of the start of the tag.  If UP is non-nil, then scan
past start-tag of element containing point.  If BOUND is non-nil,
then don't scan back past BOUND.  If no element is found, return
nil.  If a well-formedness error prevents scanning, signal an
`nxml-scan-error'.  Point is not moved."
  (let ((open-tags (and up t))
	token-end found)
    (save-excursion
      (goto-char from)
      (while (cond ((or (< (point) nxml-prolog-end)
			(not (search-backward "<"
					      (max (or bound 0)
						   nxml-prolog-end)
					      t)))
		    (when (and (consp open-tags) (not bound))
		      (nxml-scan-error (cadr open-tags)
				       "End-tag has no start-tag"))
		    nil)
		   ((progn
		      (nxml-move-outside-backwards)
		      (save-excursion
			(nxml-tokenize-forward)
			(setq token-end (point)))
		      (eq xmltok-type 'end-tag))
		    (setq open-tags
			  (cons (xmltok-end-tag-qname)
				(cons xmltok-start open-tags)))
		    t)
		   ((eq xmltok-type 'start-tag)
		    (cond ((not open-tags) nil)
			  ((not (consp open-tags))
			   (setq found token-end)
			   nil)
			  ((and (car open-tags)
				(not (string= (car open-tags)
					      (xmltok-start-tag-qname))))
			   (nxml-scan-error (1+ xmltok-start)
					    "Mismatched start-tag; \
expected `%s'"
					    (car open-tags)))
			  ((setq open-tags (cddr open-tags)) t)
			  (t (setq found token-end) nil)))
		   ((memq xmltok-type '(empty-element
					partial-empty-element))
		    (if open-tags
			t
		      (setq found token-end)
		      nil))
		   ((eq xmltok-type 'partial-end-tag)
		    (setq open-tags
			  (cons nil (cons xmltok-start open-tags)))
		    t)
		   ((eq xmltok-type 'partial-start-tag)
		    ;; if we have only a partial-start-tag
		    ;; then it's unlikely that there's a matching
		    ;; end-tag, so it's probably not helpful
		    ;; to treat it as a complete start-tag
		    (nxml-scan-error xmltok-start
				     "Missing `>'"))
		   (t t))))
    found))

(defun nxml-scan-error (&rest args)
  (signal 'nxml-scan-error args))

(put 'nxml-scan-error
     'error-conditions
     '(error nxml-error nxml-scan-error))

(put 'nxml-scan-error
     'error-message
     "Scan over element that is not well-formed")

(provide 'nxml-rap)

;;; nxml-rap.el ends here
