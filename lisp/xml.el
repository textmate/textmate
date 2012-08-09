;;; xml.el --- XML parser

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Emmanuel Briot  <briot@gnat.com>
;; Maintainer: Mark A. Hershberger <mah@everybody.org>
;; Keywords: xml, data

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

;; This file contains a somewhat incomplete non-validating XML parser.  It
;; parses a file, and returns a list that can be used internally by
;; any other Lisp libraries.

;;; FILE FORMAT

;; The document type declaration may either be ignored or (optionally)
;; parsed, but currently the parsing will only accept element
;; declarations.  The XML file is assumed to be well-formed.  In case
;; of error, the parsing stops and the XML file is shown where the
;; parsing stopped.
;;
;; It also knows how to ignore comments and processing instructions.
;;
;; The XML file should have the following format:
;;    <node1 attr1="name1" attr2="name2" ...>value
;;       <node2 attr3="name3" attr4="name4">value2</node2>
;;       <node3 attr5="name5" attr6="name6">value3</node3>
;;    </node1>
;; Of course, the name of the nodes and attributes can be anything.  There can
;; be any number of attributes (or none), as well as any number of children
;; below the nodes.
;;
;; There can be only top level node, but with any number of children below.

;;; LIST FORMAT

;; The functions `xml-parse-file', `xml-parse-region' and
;; `xml-parse-tag' return a list with the following format:
;;
;;    xml-list   ::= (node node ...)
;;    node       ::= (qname attribute-list . child_node_list)
;;    child_node_list ::= child_node child_node ...
;;    child_node ::= node | string
;;    qname      ::= (:namespace-uri . "name") | "name"
;;    attribute_list ::= ((qname . "value") (qname . "value") ...)
;;                       | nil
;;    string     ::= "..."
;;
;; Some macros are provided to ease the parsing of this list.
;; Whitespace is preserved.  Fixme: There should be a tree-walker that
;; can remove it.

;; TODO:
;;  * xml:base, xml:space support
;;  * more complete DOCTYPE parsing
;;  * pi support

;;; Code:

;; Note that buffer-substring and match-string were formerly used in
;; several places, because the -no-properties variants remove
;; composition info.  However, after some discussion on emacs-devel,
;; the consensus was that the speed of the -no-properties variants was
;; a worthwhile tradeoff especially since we're usually parsing files
;; instead of hand-crafted XML.

;;*******************************************************************
;;**
;;**  Macros to parse the list
;;**
;;*******************************************************************

(defconst xml-undefined-entity "?"
  "What to substitute for undefined entities")

(defvar xml-entity-alist
  '(("lt"   . "<")
    ("gt"   . ">")
    ("apos" . "'")
    ("quot" . "\"")
    ("amp"  . "&"))
  "The defined entities.  Entities are added to this when the DTD is parsed.")

(defvar xml-sub-parser nil
  "Dynamically set this to a non-nil value if you want to parse an XML fragment.")

(defvar xml-validating-parser nil
  "Set to non-nil to get validity checking.")

(defsubst xml-node-name (node)
  "Return the tag associated with NODE.
Without namespace-aware parsing, the tag is a symbol.

With namespace-aware parsing, the tag is a cons of a string
representing the uri of the namespace with the local name of the
tag.  For example,

    <foo>

would be represented by

    '(\"\" . \"foo\")."

  (car node))

(defsubst xml-node-attributes (node)
  "Return the list of attributes of NODE.
The list can be nil."
  (nth 1 node))

(defsubst xml-node-children (node)
  "Return the list of children of NODE.
This is a list of nodes, and it can be nil."
  (cddr node))

(defun xml-get-children (node child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should match the value returned by `xml-node-name'."
  (let ((match ()))
    (dolist (child (xml-node-children node))
      (if (and (listp child)
               (equal (xml-node-name child) child-name))
          (push child match)))
    (nreverse match)))

(defun xml-get-attribute-or-nil (node attribute)
  "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found.

See also `xml-get-attribute'."
  (cdr (assoc attribute (xml-node-attributes node))))

(defsubst xml-get-attribute (node attribute)
  "Get from NODE the value of ATTRIBUTE.
An empty string is returned if the attribute was not found.

See also `xml-get-attribute-or-nil'."
  (or (xml-get-attribute-or-nil node attribute) ""))

;;*******************************************************************
;;**
;;**  Creating the list
;;**
;;*******************************************************************

;;;###autoload
(defun xml-parse-file (file &optional parse-dtd parse-ns)
  "Parse the well-formed XML file FILE.
If FILE is already visited, use its buffer and don't kill it.
Returns the top node with all its children.
If PARSE-DTD is non-nil, the DTD is parsed rather than skipped.
If PARSE-NS is non-nil, then QNAMES are expanded."
  (if (get-file-buffer file)
      (with-current-buffer (get-file-buffer file)
	(save-excursion
	  (xml-parse-region (point-min)
			    (point-max)
			    (current-buffer)
			    parse-dtd parse-ns)))
    (with-temp-buffer
      (insert-file-contents file)
      (xml-parse-region (point-min)
			(point-max)
			(current-buffer)
			parse-dtd parse-ns))))


(defvar xml-name-re)
(defvar xml-entity-value-re)
(defvar xml-att-def-re)
(let* ((start-chars (concat "[:alpha:]:_"))
       (name-chars  (concat "-[:digit:]." start-chars))
       ;;[3]   	S	   ::=   	(#x20 | #x9 | #xD | #xA)+
       (whitespace  "[ \t\n\r]"))
  ;;[4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6]
  ;;                      | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF]
  ;;                      | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
  ;;                      | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
  (defvar xml-name-start-char-re (concat "[" start-chars "]"))
  ;;[4a] NameChar	::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
  (defvar xml-name-char-re       (concat "[" name-chars  "]"))
  ;;[5] Name     ::= NameStartChar (NameChar)*
  (defvar xml-name-re            (concat xml-name-start-char-re xml-name-char-re "*"))
  ;;[6] Names    ::= Name (#x20 Name)*
  (defvar xml-names-re           (concat xml-name-re "\\(?: " xml-name-re "\\)*"))
  ;;[7] Nmtoken ::= (NameChar)+
  (defvar xml-nmtoken-re         (concat xml-name-char-re "+"))
  ;;[8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
  (defvar xml-nmtokens-re        (concat xml-nmtoken-re "\\(?: " xml-name-re "\\)*"))
  ;;[66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
  (defvar xml-char-ref-re        "\\(?:&#[0-9]+;\\|&#x[0-9a-fA-F]+;\\)")
  ;;[68] EntityRef   ::= '&' Name ';'
  (defvar xml-entity-ref         (concat "&" xml-name-re ";"))
  ;;[69] PEReference ::= '%' Name ';'
  (defvar xml-pe-reference-re    (concat "%" xml-name-re ";"))
  ;;[67] Reference   ::= EntityRef | CharRef
  (defvar xml-reference-re       (concat "\\(?:" xml-entity-ref "\\|" xml-char-ref-re "\\)"))
  ;;[10]   	AttValue	   ::=   	'"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
  (defvar xml-att-value-re    (concat "\\(?:\"\\(?:[^&\"]\\|" xml-reference-re "\\)*\"\\|"
				      "'\\(?:[^&']\\|" xml-reference-re "\\)*'\\)"))
  ;;[56]   	TokenizedType	   ::=   	'ID'	   [VC: ID] [VC: One ID per Element Type] [VC: ID Attribute Default]
  ;;                                            | 'IDREF'    [VC: IDREF]
  ;;                             	              | 'IDREFS'   [VC: IDREF]
  ;;                                            | 'ENTITY'   [VC: Entity Name]
  ;;                                            | 'ENTITIES' [VC: Entity Name]
  ;;                                            | 'NMTOKEN'  [VC: Name Token]
  ;;                                            | 'NMTOKENS' [VC: Name Token]
  (defvar xml-tokenized-type-re "\\(?:ID\\|IDREF\\|IDREFS\\|ENTITY\\|ENTITIES\\|NMTOKEN\\|NMTOKENS\\)")
  ;;[58]   	NotationType	   ::=   	'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
  (defvar xml-notation-type-re (concat "\\(?:NOTATION" whitespace "(" whitespace "*" xml-name-re
				       "\\(?:" whitespace "*|" whitespace "*" xml-name-re "\\)*" whitespace "*)\\)"))
  ;;[59]   	Enumeration	   ::=   	'(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'	[VC: Enumeration] [VC: No Duplicate Tokens]
  (defvar xml-enumeration-re (concat "\\(?:(" whitespace "*" xml-nmtoken-re
				     "\\(?:" whitespace "*|" whitespace "*" xml-nmtoken-re "\\)*"
				     whitespace ")\\)"))
  ;;[57]   	EnumeratedType	   ::=   	NotationType | Enumeration
  (defvar xml-enumerated-type-re (concat "\\(?:" xml-notation-type-re "\\|" xml-enumeration-re "\\)"))
  ;;[54]   	AttType	   ::=   	StringType | TokenizedType | EnumeratedType
  ;;[55]   	StringType	   ::=   	'CDATA'
  (defvar xml-att-type-re (concat "\\(?:CDATA\\|" xml-tokenized-type-re "\\|" xml-notation-type-re"\\|" xml-enumerated-type-re "\\)"))
  ;;[60]   	DefaultDecl	   ::=   	'#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
  (defvar xml-default-decl-re (concat "\\(?:#REQUIRED\\|#IMPLIED\\|\\(?:#FIXED" whitespace "\\)*" xml-att-value-re "\\)"))
  ;;[53]   	AttDef	   ::=   	S Name S AttType S DefaultDecl
  (defvar xml-att-def-re         (concat "\\(?:" whitespace "*" xml-name-re
					 whitespace "*" xml-att-type-re
					 whitespace "*" xml-default-decl-re "\\)"))
  ;;[9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
  ;;		   |  "'" ([^%&'] | PEReference | Reference)* "'"
  (defvar xml-entity-value-re    (concat "\\(?:\"\\(?:[^%&\"]\\|" xml-pe-reference-re
					 "\\|" xml-reference-re "\\)*\"\\|'\\(?:[^%&']\\|"
					 xml-pe-reference-re "\\|" xml-reference-re "\\)*'\\)")))
;;[75] ExternalID ::= 'SYSTEM' S SystemLiteral
;;                 | 'PUBLIC' S PubidLiteral S SystemLiteral
;;[76] NDataDecl ::=   	S 'NDATA' S
;;[73] EntityDef  ::= EntityValue| (ExternalID NDataDecl?)
;;[71] GEDecl     ::= '<!ENTITY' S Name S EntityDef S? '>'
;;[74] PEDef      ::= EntityValue | ExternalID
;;[72] PEDecl     ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
;;[70] EntityDecl ::= GEDecl | PEDecl

;; Note that this is setup so that we can do whitespace-skipping with
;; `(skip-syntax-forward " ")', inter alia.  Previously this was slow
;; compared with `re-search-forward', but that has been fixed.  Also
;; note that the standard syntax table contains other characters with
;; whitespace syntax, like NBSP, but they are invalid in contexts in
;; which we might skip whitespace -- specifically, they're not
;; NameChars [XML 4].

(defvar xml-syntax-table
  (let ((table (make-syntax-table)))
    ;; Get space syntax correct per XML [3].
    (dotimes (c 31)
      (modify-syntax-entry c "." table)) ; all are space in standard table
    (dolist (c '(?\t ?\n ?\r))		 ; these should be space
      (modify-syntax-entry c " " table))
    ;; For skipping attributes.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Non-alnum name chars should be symbol constituents (`-' and `_'
    ;; are OK by default).
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?: "_" table)
    ;; XML [89]
    (unless (featurep 'xemacs)
      (dolist (c '(#x00B7 #x02D0 #x02D1 #x0387 #x0640 #x0E46 #x0EC6 #x3005
			  #x3031 #x3032 #x3033 #x3034 #x3035 #x309D #x309E #x30FC
			  #x30FD #x30FE))
	(modify-syntax-entry (decode-char 'ucs c) "w" table)))
    ;; Fixme: rest of [4]
    table)
  "Syntax table used by `xml-parse-region'.")

;; XML [5]
;; Note that [:alpha:] matches all multibyte chars with word syntax.
(eval-and-compile
  (defconst xml-name-regexp "[[:alpha:]_:][[:alnum:]._:-]*"))

;; Fixme:  This needs re-writing to deal with the XML grammar properly, i.e.
;;   document    ::=    prolog element Misc*
;;   prolog    ::=    XMLDecl? Misc* (doctypedecl Misc*)?

;;;###autoload
(defun xml-parse-region (beg end &optional buffer parse-dtd parse-ns)
  "Parse the region from BEG to END in BUFFER.
If BUFFER is nil, it defaults to the current buffer.
Returns the XML list for the region, or raises an error if the region
is not well-formed XML.
If PARSE-DTD is non-nil, the DTD is parsed rather than skipped,
and returned as the first element of the list.
If PARSE-NS is non-nil, then QNAMES are expanded."
  ;; Use fixed syntax table to ensure regexp char classes and syntax
  ;; specs DTRT.
  (with-syntax-table (standard-syntax-table)
    (let ((case-fold-search nil)	; XML is case-sensitive.
 	  xml result dtd)
      (save-excursion
 	(if buffer
 	    (set-buffer buffer))
 	(save-restriction
 	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (search-forward "<" nil t)
		(progn
		  (forward-char -1)
		  (setq result (xml-parse-tag parse-dtd parse-ns))
		  (cond
		   ((null result)
		    ;; Not looking at an xml start tag.
		    (unless (eobp)
		      (forward-char 1)))
		   ((and xml (not xml-sub-parser))
		    ;; Translation of rule [1] of XML specifications
		    (error "XML: (Not Well-Formed) Only one root tag allowed"))
		   ((and (listp (car result))
			 parse-dtd)
		    (setq dtd (car result))
		    (if (cdr result)	; possible leading comment
			(add-to-list 'xml (cdr result))))
		   (t
		    (add-to-list 'xml result))))
	      (goto-char (point-max))))
	  (if parse-dtd
	      (cons dtd (nreverse xml))
	    (nreverse xml)))))))

(defun xml-maybe-do-ns (name default xml-ns)
  "Perform any namespace expansion.
NAME is the name to perform the expansion on.
DEFAULT is the default namespace.  XML-NS is a cons of namespace
names to uris.  When namespace-aware parsing is off, then XML-NS
is nil.

During namespace-aware parsing, any name without a namespace is
put into the namespace identified by DEFAULT.  nil is used to
specify that the name shouldn't be given a namespace."
  (if (consp xml-ns)
      (let* ((nsp (string-match ":" name))
	     (lname (if nsp (substring name (match-end 0)) name))
	     (prefix (if nsp (substring name 0 (match-beginning 0)) default))
	     (special (and (string-equal lname "xmlns") (not prefix)))
             ;; Setting default to nil will insure that there is not
             ;; matching cons in xml-ns.  In which case we
	     (ns (or (cdr (assoc (if special "xmlns" prefix)
                                 xml-ns))
                     "")))
        (cons ns (if special "" lname)))
    (intern name)))

(defun xml-parse-fragment (&optional parse-dtd parse-ns)
  "Parse xml-like fragments."
  (let ((xml-sub-parser t)
	children)
    (while (not (eobp))
      (let ((bit (xml-parse-tag
		  parse-dtd parse-ns)))
	(if children
	    (setq children (append (list bit) children))
	  (if (stringp bit)
	      (setq children (list bit))
	    (setq children bit)))))
    (reverse children)))

(defun xml-parse-tag (&optional parse-dtd parse-ns)
  "Parse the tag at point.
If PARSE-DTD is non-nil, the DTD of the document, if any, is parsed and
returned as the first element in the list.
If PARSE-NS is non-nil, then QNAMES are expanded.
Returns one of:
 - a list : the matching node
 - nil    : the point is not looking at a tag.
 - a pair : the first element is the DTD, the second is the node."
  (let ((xml-validating-parser (or parse-dtd xml-validating-parser))
	(xml-ns (if (consp parse-ns)
		    parse-ns
		  (if parse-ns
		      (list
		       ;; Default for empty prefix is no namespace
		       (cons ""      "")
		       ;; "xml" namespace
		       (cons "xml"   "http://www.w3.org/XML/1998/namespace")
		       ;; We need to seed the xmlns namespace
		       (cons "xmlns" "http://www.w3.org/2000/xmlns/"))))))
    (cond
     ;; Processing instructions (like the <?xml version="1.0"?> tag at the
     ;; beginning of a document).
     ((looking-at "<\\?")
      (search-forward "?>")
      (skip-syntax-forward " ")
      (xml-parse-tag parse-dtd xml-ns))
     ;;  Character data (CDATA) sections, in which no tag should be interpreted
     ((looking-at "<!\\[CDATA\\[")
      (let ((pos (match-end 0)))
	(unless (search-forward "]]>" nil t)
	  (error "XML: (Not Well Formed) CDATA section does not end anywhere in the document"))
	(concat
	 (buffer-substring-no-properties pos (match-beginning 0))
	 (xml-parse-string))))
     ;;  DTD for the document
     ((looking-at "<!DOCTYPE")
      (let ((dtd (xml-parse-dtd parse-ns)))
	(skip-syntax-forward " ")
	(if xml-validating-parser
	    (cons dtd (xml-parse-tag nil xml-ns))
	  (xml-parse-tag nil xml-ns))))
     ;;  skip comments
     ((looking-at "<!--")
      (search-forward "-->")
      (skip-syntax-forward " ")
      (unless (eobp)
	(xml-parse-tag parse-dtd xml-ns)))
     ;;  end tag
     ((looking-at "</")
      '())
     ;;  opening tag
     ((looking-at "<\\([^/>[:space:]]+\\)")
      (goto-char (match-end 1))

      ;; Parse this node
      (let* ((node-name (match-string-no-properties 1))
	     ;; Parse the attribute list.
	     (attrs (xml-parse-attlist xml-ns))
	     children)

	;; add the xmlns:* attrs to our cache
	(when (consp xml-ns)
	  (dolist (attr attrs)
	    (when (and (consp (car attr))
		       (equal "http://www.w3.org/2000/xmlns/"
			      (caar attr)))
	      (push (cons (cdar attr) (cdr attr))
		    xml-ns))))

	(setq children (list attrs (xml-maybe-do-ns node-name "" xml-ns)))

	;; is this an empty element ?
	(if (looking-at "/>")
	    (progn
	      (forward-char 2)
	      (nreverse children))

	  ;; is this a valid start tag ?
	  (if (eq (char-after) ?>)
	      (progn
		(forward-char 1)
		;;  Now check that we have the right end-tag. Note that this
		;;  one might contain spaces after the tag name
		(let ((end (concat "</" node-name "\\s-*>")))
		  (while (not (looking-at end))
		    (cond
		     ((looking-at "</")
		      (error "XML: (Not Well-Formed) Invalid end tag (expecting %s) at pos %d"
			     node-name (point)))
		     ((= (char-after) ?<)
		      (let ((tag (xml-parse-tag nil xml-ns)))
			(when tag
			  (push tag children))))
		     (t
		      (let ((expansion (xml-parse-string)))
			(setq children
			      (if (stringp expansion)
				  (if (stringp (car children))
				      ;; The two strings were separated by a comment.
				      (setq children (append (list (concat (car children) expansion))
							     (cdr children)))
				    (setq children (append (list expansion) children)))
				(setq children (append expansion children))))))))

		  (goto-char (match-end 0))
		  (nreverse children)))
	    ;;  This was an invalid start tag (Expected ">", but didn't see it.)
	    (error "XML: (Well-Formed) Couldn't parse tag: %s"
		   (buffer-substring-no-properties (- (point) 10) (+ (point) 1)))))))
     (t	;; (Not one of PI, CDATA, Comment, End tag, or Start tag)
      (unless xml-sub-parser		; Usually, we error out.
	(error "XML: (Well-Formed) Invalid character"))

      ;; However, if we're parsing incrementally, then we need to deal
      ;; with stray CDATA.
      (xml-parse-string)))))

(defun xml-parse-string ()
  "Parse the next whatever.  Could be a string, or an element."
  (let* ((pos (point))
	 (string (progn (skip-chars-forward "^<")
			(buffer-substring-no-properties pos (point)))))
    ;; Clean up the string.  As per XML specifications, the XML
    ;; processor should always pass the whole string to the
    ;; application.  But \r's should be replaced:
    ;; http://www.w3.org/TR/2000/REC-xml-20001006#sec-line-ends
    (setq pos 0)
    (while (string-match "\r\n?" string pos)
      (setq string (replace-match "\n" t t string))
      (setq pos (1+ (match-beginning 0))))

    (xml-substitute-special string)))

(defun xml-parse-attlist (&optional xml-ns)
  "Return the attribute-list after point.
Leave point at the first non-blank character after the tag."
  (let ((attlist ())
	end-pos name)
    (skip-syntax-forward " ")
    (while (looking-at (eval-when-compile
			 (concat "\\(" xml-name-regexp "\\)\\s-*=\\s-*")))
      (setq end-pos (match-end 0))
      (setq name (xml-maybe-do-ns (match-string-no-properties 1) nil xml-ns))
      (goto-char end-pos)

      ;; See also: http://www.w3.org/TR/2000/REC-xml-20001006#AVNormalize

      ;; Do we have a string between quotes (or double-quotes),
      ;;  or a simple word ?
      (if (looking-at "\"\\([^\"]*\\)\"")
	  (setq end-pos (match-end 0))
	(if (looking-at "'\\([^']*\\)'")
	    (setq end-pos (match-end 0))
	  (error "XML: (Not Well-Formed) Attribute values must be given between quotes")))

      ;; Each attribute must be unique within a given element
      (if (assoc name attlist)
	  (error "XML: (Not Well-Formed) Each attribute must be unique within an element"))

      ;; Multiple whitespace characters should be replaced with a single one
      ;; in the attributes
      (let ((string (match-string-no-properties 1)))
	(replace-regexp-in-string "\\s-\\{2,\\}" " " string)
	(let ((expansion (xml-substitute-special string)))
	  (unless (stringp expansion)
					; We say this is the constraint.  It is actually that neither
					; external entities nor "<" can be in an attribute value.
	    (error "XML: (Not Well-Formed) Entities in attributes cannot expand into elements"))
	  (push (cons name expansion) attlist)))

      (goto-char end-pos)
      (skip-syntax-forward " "))
    (nreverse attlist)))

;;*******************************************************************
;;**
;;**  The DTD (document type declaration)
;;**  The following functions know how to skip or parse the DTD of
;;**  a document
;;**
;;*******************************************************************

;; Fixme: This fails at least if the DTD contains conditional sections.

(defun xml-skip-dtd ()
  "Skip the DTD at point.
This follows the rule [28] in the XML specifications."
  (let ((xml-validating-parser nil))
    (xml-parse-dtd)))

(defun xml-parse-dtd (&optional parse-ns)
  "Parse the DTD at point."
  (forward-char (eval-when-compile (length "<!DOCTYPE")))
  (skip-syntax-forward " ")
  (if (and (looking-at ">")
	   xml-validating-parser)
      (error "XML: (Validity) Invalid DTD (expecting name of the document)"))

  ;;  Get the name of the document
  (looking-at xml-name-regexp)
  (let ((dtd (list (match-string-no-properties 0) 'dtd))
	type element end-pos)
    (goto-char (match-end 0))

    (skip-syntax-forward " ")
    ;; XML [75]
    (cond ((looking-at "PUBLIC\\s-+")
	   (goto-char (match-end 0))
	   (unless (or (re-search-forward
			"\\=\"\\([[:space:][:alnum:]-'()+,./:=?;!*#@$_%]*\\)\""
			nil t)
		       (re-search-forward
			"\\='\\([[:space:][:alnum:]-()+,./:=?;!*#@$_%]*\\)'"
			nil t))
	     (error "XML: Missing Public ID"))
	   (let ((pubid (match-string-no-properties 1)))
	     (skip-syntax-forward " ")
	     (unless (or (re-search-forward "\\='\\([^']*\\)'" nil t)
			 (re-search-forward "\\=\"\\([^\"]*\\)\"" nil t))
	       (error "XML: Missing System ID"))
	     (push (list pubid (match-string-no-properties 1) 'public) dtd)))
	  ((looking-at "SYSTEM\\s-+")
	   (goto-char (match-end 0))
	   (unless (or (re-search-forward "\\='\\([^']*\\)'" nil t)
		       (re-search-forward "\\=\"\\([^\"]*\\)\"" nil t))
	     (error "XML: Missing System ID"))
	   (push (list (match-string-no-properties 1) 'system) dtd)))
    (skip-syntax-forward " ")
    (if (eq ?> (char-after))
	(forward-char)
      (if (not (eq (char-after) ?\[))
	  (error "XML: Bad DTD")
	(forward-char)
	;;  Parse the rest of the DTD
	;;  Fixme: Deal with NOTATION, PIs.
	(while (not (looking-at "\\s-*\\]"))
	  (skip-syntax-forward " ")
	  (cond

	   ;;  Translation of rule [45] of XML specifications
	   ((looking-at
	     "<!ELEMENT\\s-+\\([[:alnum:].%;]+\\)\\s-+\\([^>]+\\)>")

	    (setq element (match-string-no-properties 1)
		  type    (match-string-no-properties 2))
	    (setq end-pos (match-end 0))

	    ;;  Translation of rule [46] of XML specifications
	    (cond
	     ((string-match "^EMPTY[ \t\n\r]*$" type) ;; empty declaration
	      (setq type 'empty))
	     ((string-match "^ANY[ \t\n\r]*$" type) ;; any type of contents
	      (setq type 'any))
	     ((string-match "^(\\(.*\\))[ \t\n\r]*$" type) ;; children ([47])
	      (setq type (xml-parse-elem-type (match-string-no-properties 1 type))))
	     ((string-match "^%[^;]+;[ \t\n\r]*$" type)	;; substitution
	      nil)
	     (t
	      (if xml-validating-parser
		  (error "XML: (Validity) Invalid element type in the DTD"))))

	    ;;  rule [45]: the element declaration must be unique
	    (if (and (assoc element dtd)
		     xml-validating-parser)
		(error "XML: (Validity) Element declarations must be unique in a DTD (<%s>)"
		       element))

	    ;;  Store the element in the DTD
	    (push (list element type) dtd)
	    (goto-char end-pos))

	   ;; Translation of rule [52] of XML specifications
	   ((looking-at (concat "<!ATTLIST[ \t\n\r]*\\(" xml-name-re
				"\\)[ \t\n\r]*\\(" xml-att-def-re
				"\\)*[ \t\n\r]*>"))

	    ;; We don't do anything with ATTLIST currently
	    (goto-char (match-end 0)))

	   ((looking-at "<!--")
	    (search-forward "-->"))
	   ((looking-at (concat "<!ENTITY[ \t\n\r]*\\(" xml-name-re
				"\\)[ \t\n\r]*\\(" xml-entity-value-re
				"\\)[ \t\n\r]*>"))
	    (let ((name  (match-string-no-properties 1))
		  (value (substring (match-string-no-properties 2) 1
				    (- (length (match-string-no-properties 2)) 1))))
	      (goto-char (match-end 0))
	      (setq xml-entity-alist
		    (append xml-entity-alist
			    (list (cons name
					(with-temp-buffer
					  (insert value)
					  (goto-char (point-min))
					  (xml-parse-fragment
					   xml-validating-parser
					   parse-ns))))))))
	   ((or (looking-at (concat "<!ENTITY[ \t\n\r]+\\(" xml-name-re
				    "\\)[ \t\n\r]+SYSTEM[ \t\n\r]+"
				    "\\(\"[^\"]*\"\\|'[^']*'\\)[ \t\n\r]*>"))
		(looking-at (concat "<!ENTITY[ \t\n\r]+\\(" xml-name-re
				    "\\)[ \t\n\r]+PUBLIC[ \t\n\r]+"
				    "\"[- \r\na-zA-Z0-9'()+,./:=?;!*#@$_%]*\""
				    "\\|'[- \r\na-zA-Z0-9()+,./:=?;!*#@$_%]*'"
				    "[ \t\n\r]+\\(\"[^\"]*\"\\|'[^']*'\\)"
				    "[ \t\n\r]*>")))
	    (let ((name  (match-string-no-properties 1))
		  (file  (substring (match-string-no-properties 2) 1
				    (- (length (match-string-no-properties 2)) 1))))
	      (goto-char (match-end 0))
	      (setq xml-entity-alist
		    (append xml-entity-alist
			    (list (cons name (with-temp-buffer
					       (insert-file-contents file)
					       (goto-char (point-min))
					       (xml-parse-fragment
						xml-validating-parser
						parse-ns))))))))
	   ;; skip parameter entity declarations
	   ((or (looking-at (concat "<!ENTITY[ \t\n\r]+%[ \t\n\r]+\\(" xml-name-re
				    "\\)[ \t\n\r]+SYSTEM[ \t\n\r]+"
				    "\\(\"[^\"]*\"\\|'[^']*'\\)[ \t\n\r]*>"))
		(looking-at (concat "<!ENTITY[ \t\n\r]+"
				    "%[ \t\n\r]+"
				    "\\(" xml-name-re "\\)[ \t\n\r]+"
				    "PUBLIC[ \t\n\r]+"
				    "\\(\"[- \r\na-zA-Z0-9'()+,./:=?;!*#@$_%]*\""
				    "\\|'[- \r\na-zA-Z0-9()+,./:=?;!*#@$_%]*'\\)[ \t\n\r]+"
				    "\\(\"[^\"]+\"\\|'[^']+'\\)"
				    "[ \t\n\r]*>")))
	    (goto-char (match-end 0)))
	   ;; skip parameter entities
	   ((looking-at (concat "%" xml-name-re ";"))
	    (goto-char (match-end 0)))
	   (t
	    (when xml-validating-parser
	      (error "XML: (Validity) Invalid DTD item"))))))
      (if (looking-at "\\s-*]>")
	  (goto-char (match-end 0))))
    (nreverse dtd)))

(defun xml-parse-elem-type (string)
  "Convert element type STRING into a Lisp structure."

  (let (elem modifier)
    (if (string-match "(\\([^)]+\\))\\([+*?]?\\)" string)
	(progn
	  (setq elem     (match-string-no-properties 1 string)
		modifier (match-string-no-properties 2 string))
	  (if (string-match "|" elem)
	      (setq elem (cons 'choice
			       (mapcar 'xml-parse-elem-type
				       (split-string elem "|"))))
	    (if (string-match "," elem)
		(setq elem (cons 'seq
				 (mapcar 'xml-parse-elem-type
					 (split-string elem ",")))))))
      (if (string-match "[ \t\n\r]*\\([^+*?]+\\)\\([+*?]?\\)" string)
	  (setq elem	 (match-string-no-properties 1 string)
		modifier (match-string-no-properties 2 string))))

    (if (and (stringp elem) (string= elem "#PCDATA"))
	(setq elem 'pcdata))

    (cond
     ((string= modifier "+")
      (list '+ elem))
     ((string= modifier "*")
      (list '* elem))
     ((string= modifier "?")
      (list '\? elem))
     (t
      elem))))

;;*******************************************************************
;;**
;;**  Substituting special XML sequences
;;**
;;*******************************************************************

(defun xml-substitute-special (string)
  "Return STRING, after substituting entity references."
  ;; This originally made repeated passes through the string from the
  ;; beginning, which isn't correct, since then either "&amp;amp;" or
  ;; "&#38;amp;" won't DTRT.

  (let ((point 0)
	children end-point)
    (while (string-match "&\\([^;]*\\);" string point)
      (setq end-point (match-end 0))
      (let* ((this-part (match-string-no-properties 1 string))
	     (prev-part (substring string point (match-beginning 0)))
	     (entity (assoc this-part xml-entity-alist))
	     (expansion
	      (cond ((string-match "#\\([0-9]+\\)" this-part)
		     (let ((c (decode-char
			       'ucs
			       (string-to-number (match-string-no-properties 1 this-part)))))
		       (if c (string c))))
		    ((string-match "#x\\([[:xdigit:]]+\\)" this-part)
		     (let ((c (decode-char
			       'ucs
			       (string-to-number (match-string-no-properties 1 this-part) 16))))
		       (if c (string c))))
		    (entity
		     (cdr entity))
		    ((eq (length this-part) 0)
		     (error "XML: (Not Well-Formed) No entity given"))
		    (t
		     (if xml-validating-parser
			 (error "XML: (Validity) Undefined entity `%s'"
				this-part)
		       xml-undefined-entity)))))

	(cond ((null children)
	       ;; FIXME: If we have an entity that expands into XML, this won't work.
	       (setq children
		     (concat prev-part expansion)))
	      ((stringp children)
	       (if (stringp expansion)
		   (setq children (concat children prev-part expansion))
		 (setq children (list expansion (concat prev-part children)))))
	      ((and (stringp expansion)
		    (stringp (car children)))
	       (setcar children (concat prev-part expansion (car children))))
	      ((stringp expansion)
	       (setq children (append (concat prev-part expansion)
				      children)))
	      ((stringp (car children))
	       (setcar children (concat (car children) prev-part))
	       (setq children (append expansion children)))
	      (t
	       (setq children (list expansion
				    prev-part
				    children))))
	(setq point end-point)))
    (cond ((stringp children)
	   (concat children (substring string point)))
	  ((stringp (car (last children)))
	   (concat (car (last children)) (substring string point)))
	  ((null children)
	   string)
	  (t
	   (concat (mapconcat 'identity
			      (nreverse children)
			      "")
		   (substring string point))))))

(defun xml-substitute-numeric-entities (string)
  "Substitute SGML numeric entities by their respective utf characters.
This function replaces numeric entities in the input STRING and
returns the modified string.  For example \"&#42;\" gets replaced
by \"*\"."
  (if (and string (stringp string))
      (let ((start 0))
        (while (string-match "&#\\([0-9]+\\);" string start)
          (condition-case nil
              (setq string (replace-match
                            (string (read (substring string
                                                     (match-beginning 1)
                                                     (match-end 1))))
                            nil nil string))
            (error nil))
          (setq start (1+ (match-beginning 0))))
        string)
    nil))

;;*******************************************************************
;;**
;;**  Printing a tree.
;;**  This function is intended mainly for debugging purposes.
;;**
;;*******************************************************************

(defun xml-debug-print (xml &optional indent-string)
  "Outputs the XML in the current buffer.
XML can be a tree or a list of nodes.
The first line is indented with the optional INDENT-STRING."
  (setq indent-string (or indent-string ""))
  (dolist (node xml)
    (xml-debug-print-internal node indent-string)))

(defalias 'xml-print 'xml-debug-print)

(defun xml-escape-string (string)
  "Return the string with entity substitutions made from
xml-entity-alist."
  (mapconcat (lambda (byte)
               (let ((char (char-to-string byte)))
                 (if (rassoc char xml-entity-alist)
                     (concat "&" (car (rassoc char xml-entity-alist)) ";")
                   char)))
             ;; This differs from the non-unicode branch.  Just
             ;; grabbing the string works here.
             string ""))

(defun xml-debug-print-internal (xml indent-string)
  "Outputs the XML tree in the current buffer.
The first line is indented with INDENT-STRING."
  (let ((tree xml)
	attlist)
    (insert indent-string ?< (symbol-name (xml-node-name tree)))

    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert ?\  (symbol-name (caar attlist)) "=\""
              (xml-escape-string (cdar attlist)) ?\")
      (setq attlist (cdr attlist)))

    (setq tree (xml-node-children tree))

    (if (null tree)
	(insert ?/ ?>)
      (insert ?>)

      ;;  output the children
      (dolist (node tree)
	(cond
	 ((listp node)
	  (insert ?\n)
	  (xml-debug-print-internal node (concat indent-string "  ")))
	 ((stringp node)
          (insert (xml-escape-string node)))
	 (t
	  (error "Invalid XML tree"))))

      (when (not (and (null (cdr tree))
		      (stringp (car tree))))
	(insert ?\n indent-string))
      (insert ?< ?/ (symbol-name (xml-node-name xml)) ?>))))

(provide 'xml)

;;; xml.el ends here
