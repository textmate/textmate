;;; xmltok.el --- XML tokenization

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

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

;; This implements an XML 1.0 parser. It also implements the XML
;; Namespaces Recommendation.  It is designed to be conforming, but it
;; works a bit differently from a normal XML parser. An XML document
;; consists of the prolog and an instance.  The prolog is parsed as a
;; single unit using `xmltok-forward-prolog'.  The instance is
;; considered as a sequence of tokens, where a token is something like
;; a start-tag, a comment, a chunk of data or a CDATA section. The
;; tokenization of the instance is stateless: the tokenization of one
;; part of the instance does not depend on tokenization of the
;; preceding part of the instance.  This allows the instance to be
;; parsed incrementally.  The main entry point is `xmltok-forward':
;; this can be called at any point in the instance provided it is
;; between tokens.  The other entry point is `xmltok-forward-special'
;; which skips over tokens other comments, processing instructions or
;; CDATA sections (i.e. the constructs in an instance that can contain
;; less than signs that don't start a token).
;;
;; This is a non-validating XML 1.0 processor.  It does not resolve
;; parameter entities (including the external DTD subset) and it does
;; not resolve external general entities.
;;
;; It is non-conformant by design in the following respects.
;;
;; 1. It expects the client to detect aspects of well-formedness that
;; are not internal to a single token, specifically checking that
;; end-tags match start-tags and that the instance contains exactly
;; one element.
;;
;; 2. It expects the client to detect duplicate attributes.  Detection
;; of duplicate attributes after expansion of namespace prefixes
;; requires the namespace processing state.  Detection of duplicate
;; attributes before expansion of namespace prefixes does not, but is
;; redundant given that the client will do detection of duplicate
;; attributes after expansion of namespace prefixes.
;;
;; 3. It allows the client to recover from well-formedness errors.
;; This is essential for use in applications where the document is
;; being parsed during the editing process.
;;
;; 4. It does not support documents that do not conform to the lexical
;; requirements of the XML Namespaces Recommendation (e.g. a document
;; with a colon in an entity name).
;;
;; There are also a number of things that have not yet been
;; implemented that make it non-conformant.
;;
;; 1. It does not implement default attributes.  ATTLIST declarations
;; are parsed, but no checking is done on the content of attribute
;; value literals specifying default attribute values, and default
;; attribute values are not reported to the client.
;;
;; 2. It does not implement internal entities containing elements. If
;; an internal entity is referenced and parsing its replacement text
;; yields one or more tags, then it will skip the reference and
;; report this to the client.
;;
;; 3. It does not check the syntax of public identifiers in the DTD.
;;
;; 4. It allows some non-ASCII characters in certain situations where
;; it should not.  For example, it only enforces XML 1.0's
;; restrictions on name characters strictly for ASCII characters.  The
;; problem here is XML's character model is based squarely on Unicode,
;; whereas Emacs's is not (as of version 21).  It is not clear what
;; the right thing to do is.

;;; Code:

(defvar xmltok-type nil)
(defvar xmltok-start nil)
(defvar xmltok-name-colon nil)
(defvar xmltok-name-end nil)
(defvar xmltok-replacement nil
  "String containing replacement for a character or entity reference.")

(defvar xmltok-attributes nil
  "List containing attributes of last scanned element.
Each member of the list is a vector representing an attribute, which
can be accessed using the functions `xmltok-attribute-name-start',
`xmltok-attribute-name-colon', `xmltok-attribute-name-end',
`xmltok-attribute-value-start', `xmltok-attribute-value-end',
`xmltok-attribute-raw-normalized-value', `xmltok-attribute-refs'.")

(defvar xmltok-namespace-attributes nil
  "List containing namespace declarations of last scanned element.
List has same format as `xmltok-attributes'.")

(defvar xmltok-dtd nil
  "Information about the DTD used by `xmltok-forward'.
`xmltok-forward-prolog' sets this up.

It consists of an alist of general entity names vs definitions.  The
first member of the alist is t if references to entities not in the
alist are well-formed \(e.g. because there's an external subset that
wasn't parsed).

Each general entity name is a string.  The definition is either nil,
a symbol, a string, a cons cell.  If the definition is nil, then it
means that it's an internal entity but the result of parsing it is
unknown.  If it is a symbol, then the symbol is either `unparsed',
meaning the entity is an unparsed entity, `external', meaning the
entity is or references an external entity, `element', meaning the
entity includes one or more elements, or `not-well-formed', meaning
the replacement text is not well-formed.  If the definition is a
string, then the replacement text of the entity is that string; this
happens only during the parsing of the prolog.  If the definition is
a cons cell \(ER . AR), then ER specifies the string that results
from referencing the entity in element content and AR is either nil,
meaning the replacement text included a <, or a string which is the
normalized attribute value.")

(defvar xmltok-dependent-regions nil
  "List of descriptors of regions that a parsed token depends on.

A token depends on a region if the region occurs after the token and a
change in the region may require the token to be reparsed.  This only
happens with markup that is not well-formed.  For example, if a <?
occurs without a matching ?>, then the <? is returned as a
not-well-formed token.  However, this token is dependent on region
from the end of the token to the end of the buffer: if this ever
contains ?> then the buffer must be reparsed from the <?.

A region descriptor is a list (FUN START END ARG ...), where FUN is a
function to be called when the region changes, START and END are
integers giving the start and end of the region, and ARG... are
additional arguments to be passed to FUN.  FUN will be called with 5
arguments followed by the additional arguments if any: the position of
the start of the changed area in the region, the position of the end
of the changed area in the region, the length of the changed area
before the change, the position of the start of the region, the
position of the end of the region.  FUN must return non-nil if the
region needs reparsing.  FUN will be called in a `save-excursion'
with match-data saved.

`xmltok-forward', `xmltok-forward-special' and `xmltok-forward-prolog'
may add entries to the beginning of this list, but will not clear it.
`xmltok-forward' and `xmltok-forward-special' will only add entries
when returning tokens of type not-well-formed.")

(defvar xmltok-errors nil
  "List of errors detected by `xmltok-forward' and `xmltok-forward-prolog'.
When `xmltok-forward' and `xmltok-forward-prolog' detect a
well-formedness error, they will add an entry to the beginning of this
list.  Each entry is a vector [MESSAGE START END], where MESSAGE is a
string giving the error message and START and END are integers
indicating the position of the error.")

(defmacro xmltok-save (&rest body)
  `(let (xmltok-type
	 xmltok-start
	 xmltok-name-colon
	 xmltok-name-end
	 xmltok-replacement
	 xmltok-attributes
	 xmltok-namespace-attributes
	 xmltok-dependent-regions
	 xmltok-errors)
     ,@body))

(put 'xmltok-save 'lisp-indent-function 0)
(def-edebug-spec xmltok-save t)

(defsubst xmltok-attribute-name-start (att)
  (aref att 0))

(defsubst xmltok-attribute-name-colon (att)
  (aref att 1))

(defsubst xmltok-attribute-name-end (att)
  (aref att 2))

(defsubst xmltok-attribute-value-start (att)
  (aref att 3))

(defsubst xmltok-attribute-value-end (att)
  (aref att 4))

(defsubst xmltok-attribute-raw-normalized-value (att)
  "Return an object representing the normalized value of ATT.
This can be t indicating that the normalized value is the same as
the buffer substring from the start to the end of the value, or nil
indicating that the value is not well-formed or a string."
  (aref att 5))

(defsubst xmltok-attribute-refs (att)
  "Return a list of the entity and character references in ATT.
Each member is a vector [TYPE START END] where TYPE is either char-ref
or entity-ref and START and END are integers giving the start and end of
the reference.  Nested entity references are not included in the list."
  (aref att 6))

(defun xmltok-attribute-prefix (att)
  (let ((colon (xmltok-attribute-name-colon att)))
    (and colon
	 (buffer-substring-no-properties (xmltok-attribute-name-start att)
					 colon))))

(defun xmltok-attribute-local-name (att)
  (let ((colon (xmltok-attribute-name-colon att)))
    (buffer-substring-no-properties (if colon
					(1+ colon)
				      (xmltok-attribute-name-start att))
				    (xmltok-attribute-name-end att))))

(defun xmltok-attribute-value (att)
  (let ((rnv (xmltok-attribute-raw-normalized-value att)))
    (and rnv
	 (if (stringp rnv)
	     rnv
	   (buffer-substring-no-properties (xmltok-attribute-value-start att)
					   (xmltok-attribute-value-end att))))))

(defun xmltok-start-tag-prefix ()
  (and xmltok-name-colon
       (buffer-substring-no-properties (1+ xmltok-start)
				       xmltok-name-colon)))

(defun xmltok-start-tag-local-name ()
  (buffer-substring-no-properties (1+ (or xmltok-name-colon
					  xmltok-start))
				  xmltok-name-end))

(defun xmltok-end-tag-prefix ()
  (and xmltok-name-colon
       (buffer-substring-no-properties (+ 2 xmltok-start)
				       xmltok-name-colon)))

(defun xmltok-end-tag-local-name ()
  (buffer-substring-no-properties (if xmltok-name-colon
				      (1+ xmltok-name-colon)
				    (+ 2 xmltok-start))
				  xmltok-name-end))

(defun xmltok-start-tag-qname ()
  (buffer-substring-no-properties (+ xmltok-start 1) xmltok-name-end))

(defun xmltok-end-tag-qname ()
  (buffer-substring-no-properties (+ xmltok-start 2) xmltok-name-end))

(defsubst xmltok-make-attribute (name-begin
				 name-colon
				 name-end
				 &optional
				 value-begin
				 value-end
				 raw-normalized-value)
  "Make an attribute.
RAW-NORMALIZED-VALUE is nil if the value is not well-formed,
t if the normalized value is the string between VALUE-BEGIN
and VALUE-END, otherwise a STRING giving the value."
  (vector name-begin
	  name-colon
	  name-end
	  value-begin
	  value-end
	  raw-normalized-value
	  nil))

(defsubst xmltok-error-message (err)
  (aref err 0))

(defsubst xmltok-error-start (err)
  (aref err 1))

(defsubst xmltok-error-end (err)
  (aref err 2))

(defsubst xmltok-make-error (message start end)
  (vector message start end))

(defun xmltok-add-error (message &optional start end)
  (setq xmltok-errors
	(cons (xmltok-make-error message
				 (or start xmltok-start)
				 (or end (point)))
	      xmltok-errors)))

(defun xmltok-add-dependent (fun &optional start end &rest args)
  (setq xmltok-dependent-regions
	(cons (cons fun
		    (cons (or start xmltok-start)
			  (cons (or end (point-max))
				args)))
	      xmltok-dependent-regions)))

(defun xmltok-forward ()
  (setq xmltok-start (point))
  (let* ((case-fold-search nil)
	 (space-count (skip-chars-forward " \t\r\n"))
	 (ch (char-after)))
    (cond ((eq ch ?\<)
	   (cond ((> space-count 0)
		  (setq xmltok-type 'space))
		 (t
		  (forward-char 1)
		  (xmltok-scan-after-lt))))
	  ((eq ch ?\&)
	   (cond ((> space-count 0)
		  (setq xmltok-type 'space))
		 (t
		  (forward-char 1)
		  (xmltok-scan-after-amp 'xmltok-handle-entity))))
	  ((re-search-forward "[<&]\\|\\(]]>\\)" nil t)
	   (cond ((not (match-beginning 1))
		  (goto-char (match-beginning 0))
		  ;; must have got a non-space char
		  (setq xmltok-type 'data))
		 ((= (match-beginning 1) xmltok-start)
		  (xmltok-add-error "Found `]]>' not closing a CDATA section")
		  (setq xmltok-type 'not-well-formed))
		 (t
		  (goto-char (match-beginning 0))
		  (setq xmltok-type
			(if (= (point) (+ xmltok-start space-count))
			    'space
			  'data)))))
	  ((eq ch nil)
	   (setq xmltok-type
		 (if (> space-count 0)
		     'space
		   nil)))
	  (t
	   (goto-char (point-max))
	   (setq xmltok-type 'data)))))

(defun xmltok-forward-special (bound)
  "Scan forward past the first special token starting at or after point.
Return nil if there is no special token that starts before BOUND.
CDATA sections, processing instructions and comments (and indeed
anything starting with < following by ? or !) count as special.
Return the type of the token."
  (when (re-search-forward "<[?!]" (1+ bound) t)
    (setq xmltok-start (match-beginning 0))
    (goto-char (1+ xmltok-start))
    (let ((case-fold-search nil))
      (xmltok-scan-after-lt))))

(eval-when-compile

  ;; A symbolic regexp is represented by a list whose CAR is the string
  ;; containing the regexp and whose cdr is a list of symbolic names
  ;; for the groups in the string.

  ;; Construct a symbolic regexp from a regexp.
  (defun xmltok-r (str)
    (cons str nil))

  ;; Concatenate zero of more regexps and symbolic regexps.
  (defun xmltok+ (&rest args)
    (let (strs names)
      (while args
	(let ((arg (car args)))
	  (if (stringp arg)
	      (setq strs (cons arg strs))
	    (setq strs (cons (car arg) strs))
	    (setq names (cons (cdr arg) names)))
	  (setq args (cdr args))))
      (cons (apply 'concat (nreverse strs))
	    (apply 'append (nreverse names))))))

(eval-when-compile
  ;; Make a symbolic group named NAME from the regexp R.
  ;; R may be a symbolic regexp or an ordinary regexp.
  (defmacro xmltok-g (name &rest r)
    (let ((sym (make-symbol "r")))
      `(let ((,sym (xmltok+ ,@r)))
	 (if (stringp ,sym)
	     (cons (concat "\\(" ,sym "\\)") (cons ',name nil))
	   (cons (concat "\\(" (car ,sym) "\\)") (cons ',name (cdr ,sym)))))))

  (defun xmltok-p (&rest r) (xmltok+ "\\(?:"
				     (apply 'xmltok+ r)
				     "\\)"))

  ;; Get the group index of ELEM in a LIST of symbols.
  (defun xmltok-get-index (elem list)
    (or elem
	(error "Missing group name"))
    (let ((found nil)
	  (i 1))
      (while list
	(cond ((eq elem (car list))
	       (setq found i)
	       (setq list nil))
	      (t
	       (setq i (1+ i))
	       (setq list (cdr list)))))
      (or found
	  (error "Bad group name %s" elem))))

  ;; Define a macro SYM using a symbolic regexp R.
  ;; SYM can be called in three ways:
  ;; (SYM regexp)
  ;;   expands to the regexp in R
  ;; (SYM start G)
  ;;   expands to
  ;;   (match-beginning N)
  ;;   where N is the group index of G in R.
  ;; (SYM end G)
  ;;   expands to
  ;;   (match-end N)
  ;;   where N is the group index of G in R.
  (defmacro xmltok-defregexp (sym r)
    `(defalias ',sym
       (let ((r ,r))
	 `(macro lambda (action &optional group-name)
		 (cond ((eq action 'regexp)
			,(car r))
		       ((or (eq action 'start) (eq action 'beginning))
			(list 'match-beginning (xmltok-get-index group-name
								 ',(cdr r))))
		       ((eq action 'end)
			(list 'match-end (xmltok-get-index group-name
							   ',(cdr r))))
		       ((eq action 'string)
			(list 'match-string
			      (xmltok-get-index group-name ',(cdr r))))
		       ((eq action 'string-no-properties)
			(list 'match-string-no-properties
			      (xmltok-get-index group-name ',(cdr r))))
		       (t (error "Invalid action: %s" action))))))))


(eval-when-compile
  (let* ((or "\\|")
	 (open "\\(?:")
	 (gopen "\\(")
	 (close "\\)")
	 (name-start-char "[_[:alpha:]]")
	 (name-continue-not-start-char "[-.[:digit:]]")
	 (name-continue-char "[-._[:alnum:]]")
	 (* "*")
	 (+ "+")
	 (opt "?")
	 (question "\\?")
	 (s "[ \r\t\n]")
	 (s+ (concat s +))
	 (s* (concat s *))
	 (ncname (concat name-start-char name-continue-char *))
	 (entity-ref
	  (xmltok+ (xmltok-g entity-name ncname)
		   (xmltok-g entity-ref-close ";") opt))
	 (decimal-ref
	  (xmltok+ (xmltok-g decimal "[0-9]" +)
		   (xmltok-g decimal-ref-close ";") opt))
	 (hex-ref
	  (xmltok+ "x" open
		   (xmltok-g hex "[0-9a-fA-F]" +)
		   (xmltok-g hex-ref-close ";") opt
		   close opt))
	 (char-ref
	  (xmltok+ (xmltok-g number-sign "#")
		   open decimal-ref or hex-ref close opt))
	 (start-tag-close
	  (xmltok+ open (xmltok-g start-tag-close s* ">")
		   or open (xmltok-g empty-tag-slash s* "/")
		   (xmltok-g empty-tag-close ">") opt close
		   or (xmltok-g start-tag-s s+)
		   close))
	 (start-tag
	  (xmltok+ (xmltok-g start-tag-name
			     ncname (xmltok-g start-tag-colon ":" ncname) opt)
		   start-tag-close opt))
	 (end-tag
	  (xmltok+ (xmltok-g end-tag-slash "/")
		   open (xmltok-g end-tag-name
				  ncname
				  (xmltok-g end-tag-colon ":" ncname) opt)
		   (xmltok-g end-tag-close s* ">") opt
		   close opt))
	 (comment
	  (xmltok+ (xmltok-g markup-declaration "!")
		   (xmltok-g comment-first-dash "-"
			     (xmltok-g comment-open "-") opt) opt))
	 (cdata-section
	  (xmltok+ "!"
		  (xmltok-g marked-section-open "\\[")
		  open "C"
		  open "D"
		  open "A"
		  open "T"
		  open "A"
		  (xmltok-g cdata-section-open "\\[" ) opt
		  close opt		; A
		  close opt		; T
		  close opt		; A
		  close opt		; D
		  close opt))		; C
	 (processing-instruction
	  (xmltok-g processing-instruction-question question)))

    (xmltok-defregexp xmltok-ncname  (xmltok+ open ncname close))

    (xmltok-defregexp xmltok-after-amp
		      (xmltok+ entity-ref or char-ref))
    (xmltok-defregexp xmltok-after-lt
		      (xmltok+ start-tag
			       or end-tag
			       ;; cdata-section must come before comment
			       ;; because we treat <! as a comment
			       ;; and Emacs doesn't do fully greedy matching
			       ;; by default
			       or cdata-section
			       or comment
			       or processing-instruction))
    (xmltok-defregexp
     xmltok-attribute
     (let* ((lit1
	     (xmltok+ "'"
		      "[^<'&\r\n\t]*"
		      (xmltok-g complex1 "[&\r\n\t][^<']*") opt
		      "'"))
	    (lit2 (cons (replace-regexp-in-string "'" "\"" (car lit1))
			'(complex2)))
	    (literal (xmltok-g literal lit1 or lit2))
	    (name (xmltok+ open (xmltok-g xmlns "xmlns") or ncname close
			   (xmltok-g colon ":" ncname) opt)))
	(xmltok+ (xmltok-g name name)
		 s* "="
		 ;; If the literal isn't followed by what it should be,
		 ;; then the closing delimiter is probably really the
		 ;; opening delimiter of another literal, so don't
		 ;; absorb the literal in this case.
		 open s* literal start-tag-close close opt)))
    (xmltok-defregexp
     xmltok-xml-declaration
     (let* ((literal-content "[-._:a-zA-Z0-9]+")
	    (literal
	     (concat open "\"" literal-content "\""
		     or "'" literal-content "'" close))
	    (version-att
	     (xmltok+ open
		      s+ (xmltok-g version-name "version")
		      s* "="
		      s* (xmltok-g version-value literal)
		      close opt))
	    (encoding-att
	     (xmltok+ open
		      s+ (xmltok-g encoding-name "encoding")
		      s* "="
		      s* (xmltok-g encoding-value literal)
		      close opt))
	   (yes-no
	    (concat open "yes" or "no" close))
	   (standalone-att
	    (xmltok+ open
		     s+ (xmltok-g standalone-name "standalone")
		     s* "="
		     s* (xmltok-g standalone-value
				  "\"" yes-no "\"" or "'" yes-no "'")
		     close opt)))
       (xmltok+ "<" question "xml"
		version-att
		encoding-att
		standalone-att
		s* question ">")))
    (xmltok-defregexp
     xmltok-prolog
     (let* ((single-char (xmltok-g single-char "[[|,(\"'>]"))
	    (internal-subset-close (xmltok-g internal-subset-close
					     "][ \t\r\n]*>"))
	    (starts-with-close-paren
	     (xmltok-g close-paren
		       ")"
		       (xmltok-p
			(xmltok-g close-paren-occur "[+?]")
			or
			(xmltok-g close-paren-star "\\*"))
		       opt))
	    (starts-with-percent
	     (xmltok-g percent
		       "%" (xmltok-g param-entity-ref
				     ncname
				     (xmltok-g param-entity-ref-close
					       ";") opt) opt))
	    (starts-with-nmtoken-not-name
	     (xmltok-g nmtoken
		       (xmltok-p name-continue-not-start-char or ":")
		       (xmltok-p name-continue-char or ":") *))
	    (nmtoken-after-colon
	     (xmltok+
	      (xmltok-p name-continue-not-start-char or ":")
	      (xmltok-p name-continue-char or ":") *
	      or
	      name-start-char
	      name-continue-char *
	      ":"
	      (xmltok-p name-continue-char or ":") *))
	    (after-ncname
	     (xmltok+ (xmltok-g ncname-nmtoken
				":" (xmltok-p nmtoken-after-colon))
		      or (xmltok-p (xmltok-g colon ":" ncname)
				   (xmltok-g colon-name-occur "[?+*]") opt)
		      or (xmltok-g ncname-occur "[?+*]")
		      or (xmltok-g ncname-colon ":")))
	    (starts-with-name
	     (xmltok-g name ncname (xmltok-p after-ncname) opt))
	    (starts-with-hash
	     (xmltok-g pound
		       "#" (xmltok-g hash-name ncname)))
	    (markup-declaration
	     (xmltok-g markup-declaration
		       "!" (xmltok-p (xmltok-g comment-first-dash "-"
					       (xmltok-g comment-open "-") opt)
				     or (xmltok-g named-markup-declaration
						 ncname)) opt))
	    (after-lt
	     (xmltok+ markup-declaration
		      or (xmltok-g processing-instruction-question
				   question)
		      or (xmltok-g instance-start
				   ncname)))
	    (starts-with-lt (xmltok-g less-than "<" (xmltok-p after-lt) opt)))
       (xmltok+ starts-with-lt
		or single-char
		or starts-with-close-paren
		or starts-with-percent
		or starts-with-name
		or starts-with-nmtoken-not-name
		or starts-with-hash
		or internal-subset-close)))))

(defconst xmltok-ncname-regexp (xmltok-ncname regexp))

(defun xmltok-scan-after-lt ()
  (cond ((not (looking-at (xmltok-after-lt regexp)))
	 (xmltok-add-error "`<' that is not markup must be entered as `&lt;'")
	 (setq xmltok-type 'not-well-formed))
	(t
	 (goto-char (match-end 0))
	 (cond ((xmltok-after-lt start start-tag-close)
		(setq xmltok-name-end
		      (xmltok-after-lt end start-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start start-tag-colon))
		(setq xmltok-attributes nil)
		(setq xmltok-namespace-attributes nil)
		(setq xmltok-type 'start-tag))
	       ((xmltok-after-lt start end-tag-close)
		(setq xmltok-name-end
		      (xmltok-after-lt end end-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start end-tag-colon))
		(setq xmltok-type 'end-tag))
	       ((xmltok-after-lt start start-tag-s)
		(setq xmltok-name-end
		      (xmltok-after-lt end start-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start start-tag-colon))
		(setq xmltok-namespace-attributes nil)
		(setq xmltok-attributes nil)
		(xmltok-scan-attributes)
		xmltok-type)
	       ((xmltok-after-lt start empty-tag-close)
		(setq xmltok-name-end
		      (xmltok-after-lt end start-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start start-tag-colon))
		(setq xmltok-attributes nil)
		(setq xmltok-namespace-attributes nil)
		(setq xmltok-type 'empty-element))
	       ((xmltok-after-lt start cdata-section-open)
		(setq xmltok-type
		      (if (search-forward "]]>" nil t)
			  'cdata-section
			(xmltok-add-error "No closing ]]>")
			(xmltok-add-dependent 'xmltok-unclosed-reparse-p
					      nil
					      nil
					      "]]>")
			'not-well-formed)))
	       ((xmltok-after-lt start processing-instruction-question)
		(xmltok-scan-after-processing-instruction-open))
	       ((xmltok-after-lt start comment-open)
		(xmltok-scan-after-comment-open))
	       ((xmltok-after-lt start empty-tag-slash)
		(setq xmltok-name-end
		      (xmltok-after-lt end start-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start start-tag-colon))
		(setq xmltok-attributes nil)
		(setq xmltok-namespace-attributes nil)
		(xmltok-add-error "Expected `/>'" (1- (point)))
		(setq xmltok-type 'partial-empty-element))
	       ((xmltok-after-lt start start-tag-name)
		(xmltok-add-error "Missing `>'"
				  nil
				  (1+ xmltok-start))
		(setq xmltok-name-end
		      (xmltok-after-lt end start-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start start-tag-colon))
		(setq xmltok-namespace-attributes nil)
		(setq xmltok-attributes nil)
		(setq xmltok-type 'partial-start-tag))
	       ((xmltok-after-lt start end-tag-name)
		(setq xmltok-name-end (xmltok-after-lt end end-tag-name))
		(setq xmltok-name-colon
		      (xmltok-after-lt start end-tag-colon))
		(cond ((and (not xmltok-name-colon)
			    (eq (char-after) ?:))
		       (goto-char (1+ (point)))
		       (xmltok-add-error "Expected name following `:'"
					 (1- (point))))
		      (t
		       (xmltok-add-error "Missing `>'"
					 nil
					 (1+ xmltok-start))))
		(setq xmltok-type 'partial-end-tag))
	       ((xmltok-after-lt start end-tag-slash)
		(xmltok-add-error "Expected name following `</'")
		(setq xmltok-name-end nil)
		(setq xmltok-name-colon nil)
		(setq xmltok-type 'partial-end-tag))
	       ((xmltok-after-lt start marked-section-open)
		(xmltok-add-error "Expected `CDATA[' after `<!['"
				  xmltok-start
				  (+ 3 xmltok-start))
		(setq xmltok-type 'not-well-formed))
	       ((xmltok-after-lt start comment-first-dash)
		(xmltok-add-error "Expected `-' after `<!-'"
				  xmltok-start
				  (+ 3 xmltok-start))
		(setq xmltok-type 'not-well-formed))
	       ((xmltok-after-lt start markup-declaration)
		(xmltok-add-error "Expected `[CDATA[' or `--' after `<!'"
				  xmltok-start
				  (+ 2 xmltok-start))
		(setq xmltok-type 'not-well-formed))
	       (t
		(xmltok-add-error "Not well-formed")
		(setq xmltok-type 'not-well-formed))))))

;; XXX This should be unified with
;; xmltok-scan-prolog-after-processing-instruction-open
;; XXX maybe should include rest of line (up to any <,>) in unclosed PI
(defun xmltok-scan-after-processing-instruction-open ()
  (cond ((not (search-forward "?>" nil t))
	 (xmltok-add-error "No closing ?>"
			   xmltok-start
			   (+ xmltok-start 2))
	 (xmltok-add-dependent 'xmltok-unclosed-reparse-p
			       nil
			       nil
			       "?>")
	 (setq xmltok-type 'not-well-formed))
	(t
	 (cond ((not (save-excursion
		       (goto-char (+ 2 xmltok-start))
		       (and (looking-at (xmltok-ncname regexp))
			    (setq xmltok-name-end (match-end 0)))))
		(setq xmltok-name-end (+ xmltok-start 2))
		(xmltok-add-error "<? not followed by name"
				  (+ xmltok-start 2)
				  (+ xmltok-start 3)))
	       ((not (or (memq (char-after xmltok-name-end)
			       '(?\n ?\t ?\r ? ))
			 (= xmltok-name-end (- (point) 2))))
		(xmltok-add-error "Target not followed by whitespace"
				  xmltok-name-end
				  (1+ xmltok-name-end)))
	       ((and (= xmltok-name-end (+ xmltok-start 5))
		     (save-excursion
		       (goto-char (+ xmltok-start 2))
		       (let ((case-fold-search t))
			 (looking-at "xml"))))
		(xmltok-add-error "Processing instruction target is xml"
				  (+ xmltok-start 2)
				  (+ xmltok-start 5))))
	 (setq xmltok-type 'processing-instruction))))

(defun xmltok-scan-after-comment-open ()
  (setq xmltok-type
	(cond ((not (search-forward "--" nil t))
	       (xmltok-add-error "No closing -->")
	       (xmltok-add-dependent 'xmltok-unclosed-reparse-p
				     nil
				     nil
				     ;; not --> because
				     ;; -- is not allowed
				     ;; in comments in XML
				     "--")
	       'not-well-formed)
	      ((eq (char-after) ?>)
	       (goto-char (1+ (point)))
	       'comment)
	      (t
	       (xmltok-add-dependent
		'xmltok-semi-closed-reparse-p
		nil
		(point)
		"--"
		2)
	       ;; just include the <!-- in the token
	       (goto-char (+ xmltok-start 4))
	       ;; Need do this after the goto-char because
	       ;; marked error should just apply to <!--
	       (xmltok-add-error "First following `--' not followed by `>'")
	       'not-well-formed))))

(defun xmltok-scan-attributes ()
  (let ((recovering nil)
	(atts-needing-normalization nil))
    (while (cond ((or (looking-at (xmltok-attribute regexp))
		      ;; use non-greedy group
		      (when (looking-at (concat "[^<>\n]+?"
						(xmltok-attribute regexp)))
			(unless recovering
			  (xmltok-add-error "Malformed attribute"
					    (point)
					    (save-excursion
					      (goto-char (xmltok-attribute start
									   name))
					      (skip-chars-backward "\r\n\t ")
					      (point))))
			t))
		  (setq recovering nil)
		  (goto-char (match-end 0))
		  (let ((att (xmltok-add-attribute)))
		    (when att
		      (setq atts-needing-normalization
			    (cons att atts-needing-normalization))))
		  (cond ((xmltok-attribute start start-tag-s) t)
			((xmltok-attribute start start-tag-close)
			 (setq xmltok-type 'start-tag)
			 nil)
			((xmltok-attribute start empty-tag-close)
			 (setq xmltok-type 'empty-element)
			 nil)
			((xmltok-attribute start empty-tag-slash)
			 (setq xmltok-type 'partial-empty-element)
			 (xmltok-add-error "Expected `/>'"
					   (1- (point)))
			 nil)
			((looking-at "[ \t\r\n]*[\"']")
			 (goto-char (match-end 0))
			 (xmltok-add-error "Missing closing delimiter"
					   (1- (point)))
			 (setq recovering t)
			 t)
			((looking-at "[ \t]*\\([^ \t\r\n\"'=<>/]+\\)[ \t\r\n/>]")
			 (goto-char (match-end 1))
			 (xmltok-add-error "Attribute value not quoted"
					   (match-beginning 1))
			 (setq recovering t)
			 t)
			(t
			 (xmltok-add-error "Missing attribute value"
					   (1- (point)))
			 (setq recovering t)
			 t)))
		 ((looking-at "[^<>\n]*/>")
		  (let ((start (point)))
		    (goto-char (match-end 0))
		    (unless recovering
		      (xmltok-add-error "Malformed empty-element"
					start
					(- (point) 2))))
		  (setq xmltok-type 'empty-element)
		  nil)
		 ((looking-at "[^<>\n]*>")
		  (let ((start (point)))
		    (goto-char (match-end 0))
		    (unless recovering
		      (xmltok-add-error "Malformed start-tag"
					start
					(1- (point)))))
		  (setq xmltok-type 'start-tag)
		  nil)
		 (t
		  (when recovering
		    (skip-chars-forward "^<>\n"))
		  (xmltok-add-error "Missing `>'"
				    xmltok-start
				    (1+ xmltok-start))
		  (setq xmltok-type 'partial-start-tag)
		  nil)))
    (while atts-needing-normalization
      (xmltok-normalize-attribute (car atts-needing-normalization))
      (setq atts-needing-normalization (cdr atts-needing-normalization))))
  (setq xmltok-attributes
	(nreverse xmltok-attributes))
  (setq xmltok-namespace-attributes
	(nreverse xmltok-namespace-attributes)))

(defun xmltok-add-attribute ()
  "Return the attribute if it needs normalizing, otherwise nil."
  (let* ((needs-normalizing nil)
	 (att
	  (if (xmltok-attribute start literal)
	      (progn
		(setq needs-normalizing
		      (or (xmltok-attribute start complex1)
			  (xmltok-attribute start complex2)))
		(xmltok-make-attribute (xmltok-attribute start name)
				       (xmltok-attribute start colon)
				       (xmltok-attribute end name)
				       (1+ (xmltok-attribute start literal))
				       (1- (xmltok-attribute end literal))
				       (not needs-normalizing)))
	   (xmltok-make-attribute (xmltok-attribute start name)
				  (xmltok-attribute start colon)
				  (xmltok-attribute end name)))))
    (if (xmltok-attribute start xmlns)
	(setq xmltok-namespace-attributes
	      (cons att xmltok-namespace-attributes))
      (setq xmltok-attributes
	    (cons att xmltok-attributes)))
    (and needs-normalizing
	 att)))

(defun xmltok-normalize-attribute (att)
  (let ((end (xmltok-attribute-value-end att))
	(well-formed t)
	(value-parts nil)
	(refs nil))
    (save-excursion
      (goto-char (xmltok-attribute-value-start att))
      (while (progn
	       (let ((n (skip-chars-forward "^\r\t\n&" end)))
		 (when (> n 0)
		   (setq value-parts
			 (cons (buffer-substring-no-properties (- (point) n)
							       (point))
			       value-parts))))
	       (when (< (point) end)
		 (goto-char (1+ (point)))
		 (cond ((eq (char-before) ?\&)
			(let ((xmltok-start (1- (point)))
			       xmltok-type xmltok-replacement)
			  (xmltok-scan-after-amp
			   (lambda (start end)
			     (xmltok-handle-entity start end t)))
			  (cond ((or (eq xmltok-type 'char-ref)
				     (eq xmltok-type 'entity-ref))
				 (setq refs
				       (cons (vector xmltok-type
						     xmltok-start
						     (point))
					     refs))
				 (if xmltok-replacement
				     (setq value-parts
					   (cons xmltok-replacement
						 value-parts))
				   (setq well-formed nil)))
				(t (setq well-formed nil)))))
		       (t (setq value-parts
				(cons " " value-parts)))))
	       (< (point) end))))
    (when well-formed
      (aset att 5 (apply 'concat (nreverse value-parts))))
    (aset att 6 (nreverse refs))))

(defun xmltok-scan-after-amp (entity-handler)
  (cond ((not (looking-at (xmltok-after-amp regexp)))
	 (xmltok-add-error "`&' that is not markup must be entered as `&amp;'")
	 (setq xmltok-type 'not-well-formed))
	(t
	 (goto-char (match-end 0))
	 (cond ((xmltok-after-amp start entity-ref-close)
		(funcall entity-handler
			 (xmltok-after-amp start entity-name)
			 (xmltok-after-amp end entity-name))
		(setq xmltok-type 'entity-ref))
	       ((xmltok-after-amp start decimal-ref-close)
		(xmltok-scan-char-ref (xmltok-after-amp start decimal)
				      (xmltok-after-amp end decimal)
				      10))
	       ((xmltok-after-amp start hex-ref-close)
		(xmltok-scan-char-ref (xmltok-after-amp start hex)
				      (xmltok-after-amp end hex)
				      16))
	       ((xmltok-after-amp start number-sign)
		(xmltok-add-error "Missing character number")
		(setq xmltok-type 'not-well-formed))
	       (t
		(xmltok-add-error "Missing closing `;'")
		(setq xmltok-type 'not-well-formed))))))

(defconst xmltok-entity-error-messages
  '((unparsed . "Referenced entity is unparsed")
    (not-well-formed . "Referenced entity is not well-formed")
    (external nil . "Referenced entity is external")
    (element nil . "Referenced entity contains <")))

(defun xmltok-handle-entity (start end &optional attributep)
  (let* ((name (buffer-substring-no-properties start end))
	 (name-def (assoc name xmltok-dtd))
	 (def (cdr name-def)))
    (cond ((setq xmltok-replacement (and (consp def)
					 (if attributep
					     (cdr def)
					   (car def)))))
	  ((null name-def)
	   (unless (eq (car xmltok-dtd) t)
	     (xmltok-add-error "Referenced entity has not been defined"
			       start
			       end)))
	  ((and attributep (consp def))
	   (xmltok-add-error "Referenced entity contains <"
			     start
			     end))
	  (t
	   (let ((err (cdr (assq def xmltok-entity-error-messages))))
	     (when (consp err)
	       (setq err (if attributep (cdr err) (car err))))
	     (when err
	       (xmltok-add-error err start end)))))))

(defun xmltok-scan-char-ref (start end base)
  (setq xmltok-replacement
	(let ((n (string-to-number (buffer-substring-no-properties start end)
				base)))
	  (cond ((and (integerp n) (xmltok-valid-char-p n))
		 (setq n (xmltok-unicode-to-char n))
		 (and n (string n)))
		(t
		 (xmltok-add-error "Invalid character code" start end)
		 nil))))
  (setq xmltok-type 'char-ref))

(defun xmltok-char-number (start end)
  (let* ((base (if (eq (char-after (+ start 2)) ?x)
		   16
		 10))
	 (n (string-to-number
	     (buffer-substring-no-properties (+ start (if (= base 16) 3 2))
					     (1- end))
	     base)))
    (and (integerp n)
	 (xmltok-valid-char-p n)
	 n)))

(defun xmltok-unclosed-reparse-p (change-start
				  change-end
				  pre-change-length
				  start
				  end
				  delimiter)
  (let ((len-1 (1- (length delimiter))))
    (goto-char (max start (- change-start len-1)))
    (search-forward delimiter (min end (+ change-end len-1)) t)))

;; Handles a <!-- with the next -- not followed by >

(defun xmltok-semi-closed-reparse-p (change-start
				     change-end
				     pre-change-length
				     start
				     end
				     delimiter
				     delimiter-length)
  (or (<= (- end delimiter-length) change-end)
      (xmltok-unclosed-reparse-p change-start
				 change-end
				 pre-change-length
				 start
				 end
				 delimiter)))

(defun xmltok-valid-char-p (n)
  "Return non-nil if N is the Unicode code of a valid XML character."
  (cond ((< n #x20) (memq n '(#xA #xD #x9)))
	((< n #xD800) t)
	((< n #xE000) nil)
	((< n #xFFFE) t)
	(t (and (> n #xFFFF)
		(< n #x110000)))))

(defun xmltok-unicode-to-char (n)
  "Return the character corresponding to Unicode scalar value N.
Return nil if unsupported in Emacs."
  (decode-char 'ucs n))

;;; Prolog parsing

(defvar xmltok-contains-doctype nil)
(defvar xmltok-doctype-external-subset-flag nil)
(defvar xmltok-internal-subset-start nil)
(defvar xmltok-had-param-entity-ref nil)
(defvar xmltok-prolog-regions nil)
(defvar xmltok-standalone nil
  "Non-nil if there was an XML declaration specifying standalone=\"yes\".")
(defvar xmltok-markup-declaration-doctype-flag nil)

(defconst xmltok-predefined-entity-alist
  '(("lt" "<" . "<")
    ("gt" ">" . ">")
    ("amp" "&" . "&")
    ("apos" "'" . "'")
    ("quot" "\"" . "\"")))

(defun xmltok-forward-prolog ()
  "Move forward to the end of the XML prolog.

Returns a list of vectors [TYPE START END] where TYPE is a symbol and
START and END are integers giving the start and end of the region of
that type.  TYPE can be one of xml-declaration,
xml-declaration-attribute-name, xml-declaration-attribute-value,
comment, processing-instruction-left, processing-instruction-right,
markup-declaration-open, markup-declaration-close,
internal-subset-open, internal-subset-close, hash-name, keyword,
literal, encoding-name.
Adds to `xmltok-errors' and `xmltok-dependent-regions' as appropriate."
  (let ((case-fold-search nil)
	xmltok-start
	xmltok-type
	xmltok-prolog-regions
	xmltok-contains-doctype
	xmltok-internal-subset-start
	xmltok-had-param-entity-ref
	xmltok-standalone
	xmltok-doctype-external-subset-flag
	xmltok-markup-declaration-doctype-flag)
    (setq xmltok-dtd xmltok-predefined-entity-alist)
    (xmltok-scan-xml-declaration)
    (xmltok-next-prolog-token)
    (while (condition-case err
	       (when (xmltok-parse-prolog-item)
		 (xmltok-next-prolog-token))
	     (xmltok-markup-declaration-parse-error
	      (xmltok-skip-markup-declaration))))
    (when xmltok-internal-subset-start
      (xmltok-add-error "No closing ]"
			(1- xmltok-internal-subset-start)
			xmltok-internal-subset-start))
    (xmltok-parse-entities)
    ;; XXX prune dependent-regions for those entirely in prolog
    (nreverse xmltok-prolog-regions)))

(defconst xmltok-bad-xml-decl-regexp
  "[ \t\r\n]*<\\?xml\\(?:[ \t\r\n]\\|\\?>\\)")

;;;###autoload
(defun xmltok-get-declared-encoding-position (&optional limit)
  "Return the position of the encoding in the XML declaration at point.
If there is a well-formed XML declaration starting at point and it
contains an encoding declaration, then return (START . END)
where START and END are the positions of the start and the end
of the encoding name; if there is no encoding declaration return
the position where and encoding declaration could be inserted.
If there is XML that is not well-formed that looks like an XML
declaration, return nil.  Otherwise, return t.
If LIMIT is non-nil, then do not consider characters beyond LIMIT."
  (cond ((let ((case-fold-search nil))
	   (and (looking-at (xmltok-xml-declaration regexp))
		(or (not limit) (<= (match-end 0) limit))))
	 (let ((end (xmltok-xml-declaration end encoding-value)))
	   (if end
	       (cons (1+ (xmltok-xml-declaration start encoding-value))
		     (1- end))
	     (or (xmltok-xml-declaration end version-value)
		 (+ (point) 5)))))
	((not (let ((case-fold-search t))
		(looking-at xmltok-bad-xml-decl-regexp))))))

(defun xmltok-scan-xml-declaration ()
  (when (looking-at (xmltok-xml-declaration regexp))
    (xmltok-add-prolog-region 'xml-declaration (point) (match-end 0))
    (goto-char (match-end 0))
    (when (xmltok-xml-declaration start version-name)
      (xmltok-add-prolog-region 'xml-declaration-attribute-name
				(xmltok-xml-declaration start version-name)
				(xmltok-xml-declaration end version-name))
      (let ((start (xmltok-xml-declaration start version-value))
	    (end (xmltok-xml-declaration end version-value)))
	(xmltok-add-prolog-region 'xml-declaration-attribute-value
				  start
				  end)))
    ;; XXX need to check encoding name
    ;; Should start with letter, not contain colon
    (when (xmltok-xml-declaration start encoding-name)
      (xmltok-add-prolog-region 'xml-declaration-attribute-name
				(xmltok-xml-declaration start encoding-name)
				(xmltok-xml-declaration end encoding-name))
      (let ((start (xmltok-xml-declaration start encoding-value))
	    (end (xmltok-xml-declaration end encoding-value)))
	(xmltok-add-prolog-region 'encoding-name
				  (1+ start)
				  (1- end))
	(xmltok-add-prolog-region 'xml-declaration-attribute-value
				  start
				  end)))
    (when (xmltok-xml-declaration start standalone-name)
      (xmltok-add-prolog-region 'xml-declaration-attribute-name
				(xmltok-xml-declaration start standalone-name)
				(xmltok-xml-declaration end standalone-name))
      (let ((start (xmltok-xml-declaration start standalone-value))
	    (end (xmltok-xml-declaration end standalone-value)))
	(xmltok-add-prolog-region 'xml-declaration-attribute-value
				  start
				  end)
	(setq xmltok-standalone
	      (string= (buffer-substring-no-properties (1+ start) (1- end))
		       "yes"))))
    t))

(defconst xmltok-markup-declaration-alist
  '(("ELEMENT" . xmltok-parse-element-declaration)
    ("ATTLIST" . xmltok-parse-attlist-declaration)
    ("ENTITY" . xmltok-parse-entity-declaration)
    ("NOTATION" . xmltok-parse-notation-declaration)))

(defun xmltok-parse-prolog-item ()
  (cond ((eq xmltok-type 'comment)
	 (xmltok-add-prolog-region 'comment
				   xmltok-start
				   (point))
	 t)
	((eq xmltok-type 'processing-instruction))
	((eq xmltok-type 'named-markup-declaration)
	 (setq xmltok-markup-declaration-doctype-flag nil)
	 (xmltok-add-prolog-region 'markup-declaration-open
				   xmltok-start
				   (point))
	 (let* ((name (buffer-substring-no-properties
		       (+ xmltok-start 2)
		       (point)))
		(fun (cdr (assoc name xmltok-markup-declaration-alist))))
	   (cond (fun
		  (unless xmltok-internal-subset-start
		    (xmltok-add-error
		     "Declaration allowed only in internal subset"))
		  (funcall fun))
		 ((string= name "DOCTYPE")
		  (xmltok-parse-doctype))
		 (t
		  (xmltok-add-error "Unknown markup declaration"
				    (+ xmltok-start 2))
		  (xmltok-next-prolog-token)
		  (xmltok-markup-declaration-parse-error))))
	 t)
	((or (eq xmltok-type 'end-prolog)
	     (not xmltok-type))
	 nil)
	((eq xmltok-type 'internal-subset-close)
	 (xmltok-add-prolog-region 'internal-subset-close
				   xmltok-start
				   (1+ xmltok-start))
	 (xmltok-add-prolog-region 'markup-declaration-close
				   (1- (point))
				   (point))
	 (if xmltok-internal-subset-start
	     (setq xmltok-internal-subset-start nil)
	   (xmltok-add-error "]> outside internal subset"))
	 t)
	((eq xmltok-type 'param-entity-ref)
	 (if xmltok-internal-subset-start
	     (setq xmltok-had-param-entity-ref t)
	   (xmltok-add-error "Parameter entity reference outside document type declaration"))
	 t)
	;; If we don't do this, we can get thousands of errors when
	;; a plain text file is parsed.
	((not xmltok-internal-subset-start)
	 (when (let ((err (car xmltok-errors)))
		 (or (not err)
		     (<= (xmltok-error-end err) xmltok-start)))
	   (goto-char xmltok-start))
	 nil)
	((eq xmltok-type 'not-well-formed) t)
	(t
	 (xmltok-add-error "Token allowed only inside markup declaration")
	 t)))

(defun xmltok-parse-doctype ()
  (setq xmltok-markup-declaration-doctype-flag t)
  (xmltok-next-prolog-token)
  (when xmltok-internal-subset-start
    (xmltok-add-error "DOCTYPE declaration not allowed in internal subset")
    (xmltok-markup-declaration-parse-error))
  (when xmltok-contains-doctype
    (xmltok-add-error "Duplicate DOCTYPE declaration")
    (xmltok-markup-declaration-parse-error))
  (setq xmltok-contains-doctype t)
  (xmltok-require-token 'name 'prefixed-name)
  (xmltok-require-next-token "SYSTEM" "PUBLIC" ?\[ ?>)
  (cond ((eq xmltok-type ?\[)
	 (setq xmltok-internal-subset-start (point)))
	((eq xmltok-type ?>))
	(t
	 (setq xmltok-doctype-external-subset-flag t)
	 (xmltok-parse-external-id)
	 (xmltok-require-token ?\[ ?>)
	 (when (eq xmltok-type ?\[)
	   (setq xmltok-internal-subset-start (point))))))

(defun xmltok-parse-attlist-declaration ()
  (xmltok-require-next-token 'prefixed-name 'name)
  (while (progn
	   (xmltok-require-next-token ?> 'name 'prefixed-name)
	   (if (eq xmltok-type ?>)
	       nil
	     (xmltok-require-next-token ?\(
					"CDATA"
					"ID"
					"IDREF"
					"IDREFS"
					"ENTITY"
					"ENTITIES"
					"NMTOKEN"
					"NMTOKENS"
					"NOTATION")
	     (cond ((eq xmltok-type ?\()
		    (xmltok-parse-nmtoken-group))
		   ((string= (xmltok-current-token-string)
			     "NOTATION")
		    (xmltok-require-next-token ?\()
		    (xmltok-parse-nmtoken-group)))
	     (xmltok-require-next-token "#IMPLIED"
					"#REQUIRED"
					"#FIXED"
					'literal)
	     (when (string= (xmltok-current-token-string) "#FIXED")
	       (xmltok-require-next-token 'literal))
	     t))))

(defun xmltok-parse-nmtoken-group ()
  (while (progn
	   (xmltok-require-next-token 'nmtoken 'prefixed-name 'name)
	   (xmltok-require-next-token ?| ?\))
	   (eq xmltok-type ?|))))

(defun xmltok-parse-element-declaration ()
  (xmltok-require-next-token 'name 'prefixed-name)
  (xmltok-require-next-token "EMPTY" "ANY" ?\()
  (when (eq xmltok-type ?\()
    (xmltok-require-next-token "#PCDATA"
			       'name
			       'prefixed-name
			       'name-occur
			       ?\()
    (cond ((eq xmltok-type 'hash-name)
	   (xmltok-require-next-token ?| ?\) 'close-paren-star)
	   (while (eq xmltok-type ?|)
	     (xmltok-require-next-token 'name 'prefixed-name)
	     (xmltok-require-next-token 'close-paren-star ?|)))
	  (t (xmltok-parse-model-group))))
  (xmltok-require-next-token ?>))

(defun xmltok-parse-model-group ()
  (xmltok-parse-model-group-member)
  (xmltok-require-next-token ?|
			     ?,
			     ?\)
			     'close-paren-star
			     'close-paren-occur)
  (when (memq xmltok-type '(?, ?|))
    (let ((connector xmltok-type))
      (while (progn
	       (xmltok-next-prolog-token)
	       (xmltok-parse-model-group-member)
	       (xmltok-require-next-token connector
					  ?\)
					  'close-paren-star
					  'close-paren-occur)
	       (eq xmltok-type connector))))))

(defun xmltok-parse-model-group-member ()
  (xmltok-require-token 'name
			'prefixed-name
			'name-occur
			?\()
  (when (eq xmltok-type ?\()
    (xmltok-next-prolog-token)
    (xmltok-parse-model-group)))

(defun xmltok-parse-entity-declaration ()
  (let (paramp name)
    (xmltok-require-next-token 'name ?%)
    (when (eq xmltok-type ?%)
      (setq paramp t)
      (xmltok-require-next-token 'name))
    (setq name (xmltok-current-token-string))
    (xmltok-require-next-token 'literal "SYSTEM" "PUBLIC")
    (cond ((eq xmltok-type 'literal)
	   (let ((replacement (xmltok-parse-entity-value)))
	     (unless paramp
	       (xmltok-define-entity name replacement)))
	   (xmltok-require-next-token ?>))
	  (t
	   (xmltok-parse-external-id)
	   (if paramp
	       (xmltok-require-token ?>)
	     (xmltok-require-token ?> "NDATA")
	     (if (eq xmltok-type ?>)
		 (xmltok-define-entity name 'external)
	       (xmltok-require-next-token 'name)
	       (xmltok-require-next-token ?>)
	       (xmltok-define-entity name 'unparsed)))))))

(defun xmltok-define-entity (name value)
  (when (and (or (not xmltok-had-param-entity-ref)
		 xmltok-standalone)
	     (not (assoc name xmltok-dtd)))
    (setq xmltok-dtd
	  (cons (cons name value) xmltok-dtd))))

(defun xmltok-parse-entity-value ()
  (let ((lim (1- (point)))
	(well-formed t)
	value-parts
	start)
    (save-excursion
      (goto-char (1+ xmltok-start))
      (setq start (point))
      (while (progn
	       (skip-chars-forward "^%&" lim)
	       (when (< (point) lim)
		 (goto-char (1+ (point)))
		 (cond ((eq (char-before) ?%)
			(xmltok-add-error "Parameter entity references are not allowed in the internal subset"
					  (1- (point))
					  (point))
			(setq well-formed  nil))
		       (t
			(let ((xmltok-start (1- (point)))
			       xmltok-type xmltok-replacement)
			  (xmltok-scan-after-amp (lambda (start end)))
			  (cond ((eq xmltok-type 'char-ref)
				 (setq value-parts
				       (cons (buffer-substring-no-properties
					      start
					      xmltok-start)
					     value-parts))
				 (setq value-parts
				       (cons xmltok-replacement
					     value-parts))
				 (setq start (point)))
				((eq xmltok-type 'not-well-formed)
				 (setq well-formed nil))))))
		 t))))
    (if (not well-formed)
	nil
      (apply 'concat
	     (nreverse (cons (buffer-substring-no-properties start lim)
			     value-parts))))))

(defun xmltok-parse-notation-declaration ()
  (xmltok-require-next-token 'name)
  (xmltok-require-next-token "SYSTEM" "PUBLIC")
  (let ((publicp (string= (xmltok-current-token-string) "PUBLIC")))
    (xmltok-require-next-token 'literal)
    (cond (publicp
	   (xmltok-require-next-token 'literal ?>)
	   (unless (eq xmltok-type ?>)
	     (xmltok-require-next-token ?>)))
	  (t (xmltok-require-next-token ?>)))))

(defun xmltok-parse-external-id ()
  (xmltok-require-token "SYSTEM" "PUBLIC")
  (let ((publicp (string= (xmltok-current-token-string) "PUBLIC")))
    (xmltok-require-next-token 'literal)
    (when publicp
      (xmltok-require-next-token 'literal)))
  (xmltok-next-prolog-token))

(defun xmltok-require-next-token (&rest types)
  (xmltok-next-prolog-token)
  (apply 'xmltok-require-token types))

(defun xmltok-require-token (&rest types)
  ;; XXX Generate a more helpful error message
  (while (and (not (let ((type (car types)))
		     (if (stringp (car types))
			 (string= (xmltok-current-token-string) type)
		       (eq type xmltok-type))))
	      (setq types (cdr types))))
  (unless types
    (when (and xmltok-type
	       (not (eq xmltok-type 'not-well-formed)))
      (xmltok-add-error "Unexpected token"))
    (xmltok-markup-declaration-parse-error))
  (let ((region-type (xmltok-prolog-region-type (car types))))
    (when region-type
      (xmltok-add-prolog-region region-type
				xmltok-start
				(point)))))

(defun xmltok-current-token-string ()
  (buffer-substring-no-properties xmltok-start (point)))

(put 'xmltok-markup-declaration-parse-error
     'error-conditions
     '(error xmltok-markup-declaration-parse-error))

(put 'xmltok-markup-declaration-parse-error
     'error-message
     "Syntax error in markup declaration")

(defun xmltok-markup-declaration-parse-error ()
  (signal 'xmltok-markup-declaration-parse-error nil))

(defun xmltok-skip-markup-declaration ()
  (while (cond ((eq xmltok-type ?>)
		(xmltok-next-prolog-token)
		nil)
	       ((and xmltok-markup-declaration-doctype-flag
		     (eq xmltok-type ?\[))
		(setq xmltok-internal-subset-start (point))
		(xmltok-next-prolog-token)
		nil)
	       ((memq xmltok-type '(nil
				    end-prolog
				    named-markup-declaration
				    comment
				    processing-instruction))
		nil)
	       ((and xmltok-internal-subset-start
		     (eq xmltok-type 'internal-subset-close))
		nil)
	       (t (xmltok-next-prolog-token) t)))
  xmltok-type)

(defun xmltok-prolog-region-type (required)
  (cond ((cdr (assq xmltok-type
		    '((literal . literal)
		      (?> . markup-declaration-close)
		      (?\[ . internal-subset-open)
		      (hash-name . hash-name)))))
	((and (stringp required) (eq xmltok-type 'name))
	 'keyword)))

;; Return new token type.

(defun xmltok-next-prolog-token ()
  (skip-chars-forward " \t\r\n")
  (setq xmltok-start (point))
  (cond ((not (and (looking-at (xmltok-prolog regexp))
		   (goto-char (match-end 0))))
	 (let ((ch (char-after)))
	   (cond (ch
		  (goto-char (1+ (point)))
		  (xmltok-add-error "Illegal char in prolog")
		  (setq xmltok-type 'not-well-formed))
		 (t (setq xmltok-type nil)))))
	((or (xmltok-prolog start ncname-occur)
	     (xmltok-prolog start colon-name-occur))
	 (setq xmltok-name-end (1- (point)))
	 (setq xmltok-name-colon (xmltok-prolog start colon))
	 (setq xmltok-type 'name-occur))
	((xmltok-prolog start colon)
	 (setq xmltok-name-end (point))
	 (setq xmltok-name-colon (xmltok-prolog start colon))
	 (unless (looking-at "[ \t\r\n>),|[%]")
	   (xmltok-add-error "Missing space after name"))
	 (setq xmltok-type 'prefixed-name))
	((or (xmltok-prolog start ncname-nmtoken)
	     (xmltok-prolog start ncname-colon))
	 (unless (looking-at "[ \t\r\n>),|[%]")
	   (xmltok-add-error "Missing space after name token"))
	 (setq xmltok-type 'nmtoken))
	((xmltok-prolog start name)
	 (setq xmltok-name-end (point))
	 (setq xmltok-name-colon nil)
	 (unless (looking-at "[ \t\r\n>),|[%]")
	   (xmltok-add-error "Missing space after name"))
	 (setq xmltok-type 'name))
	((xmltok-prolog start hash-name)
	 (setq xmltok-name-end (point))
	 (unless (looking-at "[ \t\r\n>)|%]")
	   (xmltok-add-error "Missing space after name"))
	 (setq xmltok-type 'hash-name))
	((xmltok-prolog start processing-instruction-question)
	 (xmltok-scan-prolog-after-processing-instruction-open))
	((xmltok-prolog start comment-open)
	 ;; XXX if not-well-formed, ignore some stuff
	 (xmltok-scan-after-comment-open))
	((xmltok-prolog start named-markup-declaration)
	 (setq xmltok-type 'named-markup-declaration))
	((xmltok-prolog start instance-start)
	 (goto-char xmltok-start)
	 (setq xmltok-type 'end-prolog))
	((xmltok-prolog start close-paren-star)
	 (setq xmltok-type 'close-paren-star))
	((xmltok-prolog start close-paren-occur)
	 (setq xmltok-type 'close-paren-occur))
	((xmltok-prolog start close-paren)
	 (unless (looking-at "[ \t\r\n>,|)]")
	   (xmltok-add-error "Missing space after )"))
	 (setq xmltok-type ?\)))
	((xmltok-prolog start single-char)
	 (let ((ch (char-before)))
	   (cond ((memq ch '(?\" ?\'))
		  (xmltok-scan-prolog-literal))
		 (t (setq xmltok-type ch)))))
	((xmltok-prolog start percent)
	 (cond ((xmltok-prolog start param-entity-ref-close)
		(setq xmltok-name-end (1- (point)))
		(setq xmltok-type 'param-entity-ref))
	       ((xmltok-prolog start param-entity-ref)
		(xmltok-add-error "Missing ;")
		(setq xmltok-name-end (point))
		(setq xmltok-type 'param-entity-ref))
	       ((looking-at "[ \t\r\n%]")
		(setq xmltok-type ?%))
	       (t
		(xmltok-add-error "Expected name after %")
		(setq xmltok-type 'not-well-formed))))
	((xmltok-prolog start nmtoken)
	 (unless (looking-at "[ \t\r\n>),|[%]")
	   (xmltok-add-error "Missing space after name token"))
	 (setq xmltok-type 'nmtoken))
	((xmltok-prolog start internal-subset-close)
	 (setq xmltok-type 'internal-subset-close))
	((xmltok-prolog start pound)
	 (xmltok-add-error "Expected name after #")
	 (setq xmltok-type 'not-well-formed))
	((xmltok-prolog start markup-declaration)
	 (xmltok-add-error "Expected name or -- after <!")
	 (setq xmltok-type 'not-well-formed))
	((xmltok-prolog start comment-first-dash)
	 (xmltok-add-error "Expected <!--")
	 (setq xmltok-type 'not-well-formed))
	((xmltok-prolog start less-than)
	 (xmltok-add-error "Incomplete markup")
	 (setq xmltok-type 'not-well-formed))
	(t (error "Unhandled token in prolog %s"
		  (match-string-no-properties 0)))))

(defun xmltok-scan-prolog-literal ()
  (let* ((delim (string (char-before)))
	 (safe-end (save-excursion
		     (skip-chars-forward (concat "^<>[]" delim))
		     (point)))
	 (end (save-excursion
		(goto-char safe-end)
		(search-forward delim nil t))))
    (or (cond ((not end)
	       (xmltok-add-dependent 'xmltok-unclosed-reparse-p
				     nil
				     nil
				     delim)
	       nil)
	      ((save-excursion
		 (goto-char end)
		 (looking-at "[ \t\r\n>%[]"))
	       (goto-char end)
	       (setq xmltok-type 'literal))
	      ((eq (1+ safe-end) end)
	       (goto-char end)
	       (xmltok-add-error (format "Missing space after %s" delim)
				 safe-end)
	       (setq xmltok-type 'literal))
	      (t
	       (xmltok-add-dependent 'xmltok-semi-closed-reparse-p
				     xmltok-start
				     (1+ end)
				     delim
				     1)
	       nil))
	(progn
	  (xmltok-add-error (format "Missing closing %s" delim))
	  (goto-char safe-end)
	  (skip-chars-backward " \t\r\n")
	  (setq xmltok-type 'not-well-formed)))))

(defun xmltok-scan-prolog-after-processing-instruction-open ()
  (cond ((not (search-forward "?>" nil t))
	 (xmltok-add-error "No closing ?>"
			   xmltok-start
			   (+ xmltok-start 2))
	 (xmltok-add-dependent 'xmltok-unclosed-reparse-p
			       nil
			       nil
			       "?>")
	 (setq xmltok-type 'not-well-formed))
	(t
	 (let* ((end (point))
		(target
		 (save-excursion
		   (goto-char (+ xmltok-start 2))
		   (and (looking-at (xmltok-ncname regexp))
			(or (memq (char-after (match-end 0))
				  '(?\n ?\t ?\r ? ))
			    (= (match-end 0) (- end 2)))
			(match-string-no-properties 0)))))
	   (cond ((not target)
		  (xmltok-add-error "\
Processing instruction does not start with a name"
				    (+ xmltok-start 2)
				    (+ xmltok-start 3)))
		 ((not (and (= (length target) 3)
			    (let ((case-fold-search t))
			      (string-match "xml" target)))))
		 ((= xmltok-start 1)
		  (xmltok-add-error "Invalid XML declaration"
				    xmltok-start
				    (point)))
		 ((save-excursion
		    (goto-char xmltok-start)
		    (looking-at (xmltok-xml-declaration regexp)))
		  (xmltok-add-error "XML declaration not at beginning of file"
				    xmltok-start
				    (point)))
		 (t
		  (xmltok-add-error "Processing instruction has target of xml"
				    (+ xmltok-start 2)
				    (+ xmltok-start 5))))
	   (xmltok-add-prolog-region 'processing-instruction-left
				     xmltok-start
				     (+ xmltok-start
					2
					(if target
					    (length target)
					  0)))
	   (xmltok-add-prolog-region 'processing-instruction-right
				     (if target
					 (save-excursion
					   (goto-char (+ xmltok-start
							 (length target)
							 2))
					   (skip-chars-forward " \t\r\n")
					   (point))
				       (+ xmltok-start 2))
				     (point)))
	 (setq xmltok-type 'processing-instruction))))

(defun xmltok-parse-entities ()
  (let ((todo xmltok-dtd))
    (when (and (or xmltok-had-param-entity-ref
		   xmltok-doctype-external-subset-flag)
	       (not xmltok-standalone))
      (setq xmltok-dtd (cons t xmltok-dtd)))
    (while todo
      (xmltok-parse-entity (car todo))
      (setq todo (cdr todo)))))

(defun xmltok-parse-entity (name-def)
  (let ((def (cdr name-def))
	;; in case its value is buffer local
	(xmltok-dtd xmltok-dtd)
	buf)
    (when (stringp def)
      (if (string-match "\\`[^&<\t\r\n]*\\'" def)
	  (setcdr name-def (cons def def))
	(setcdr name-def 'not-well-formed) ; avoid infinite expansion loops
	(setq buf (get-buffer-create
		   (format " *Entity %s*" (car name-def))))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert def)
	  (goto-char (point-min))
	  (setcdr name-def
		  (xmltok-parse-entity-replacement)))
	(kill-buffer buf)))))

(defun xmltok-parse-entity-replacement ()
  (let ((def (cons "" "")))
    (while (let* ((start (point))
		  (found (re-search-forward "[<&\t\r\n]\\|]]>" nil t))
		  (ch (and found (char-before)))
		  (str (buffer-substring-no-properties
			start
			(if found
			    (match-beginning 0)
			  (point-max)))))
	     (setq def
		   (xmltok-append-entity-def def
					     (cons str str)))
	     (cond ((not found) nil)
		   ((eq ch ?>)
		    (setq def 'not-well-formed)
		    nil)
		   ((eq ch ?<)
		    (xmltok-save
		      (setq xmltok-start (1- (point)))
		      (xmltok-scan-after-lt)
		      (setq def
			    (xmltok-append-entity-def
			     def
			     (cond ((memq xmltok-type
					  '(start-tag
					    end-tag
					    empty-element))
				    'element)
				   ((memq xmltok-type
					  '(comment
					    processing-instruction))
				    (cons "" nil))
				   ((eq xmltok-type
					'cdata-section)
				    (cons (buffer-substring-no-properties
					   (+ xmltok-start 9)
					   (- (point) 3))
					  nil))
				   (t 'not-well-formed)))))
		    t)
		   ((eq ch ?&)
		    (let ((xmltok-start (1- (point)))
			  xmltok-type
			  xmltok-replacement
			  xmltok-errors)
		      (xmltok-scan-after-amp 'xmltok-handle-nested-entity)
		      (cond ((eq xmltok-type 'entity-ref)
			     (setq def
				   (xmltok-append-entity-def
				    def
				    xmltok-replacement)))
			    ((eq xmltok-type 'char-ref)
			     (setq def
				   (xmltok-append-entity-def
				    def
				    (if xmltok-replacement
					 (cons xmltok-replacement
					       xmltok-replacement)
				      (and xmltok-errors 'not-well-formed)))))
			    (t
			     (setq def 'not-well-formed))))
		    t)
		   (t
		    (setq def
			  (xmltok-append-entity-def
			   def
			   (cons (match-string-no-properties 0)
				 " ")))
		    t))))
    def))

(defun xmltok-handle-nested-entity (start end)
  (let* ((name-def (assoc (buffer-substring-no-properties start end)
			  xmltok-dtd))
	 (def (cdr name-def)))
    (when (stringp def)
      (xmltok-parse-entity name-def)
      (setq def (cdr name-def)))
    (setq xmltok-replacement
	  (cond ((null name-def)
		 (if (eq (car xmltok-dtd) t)
		     nil
		   'not-well-formed))
		((eq def 'unparsed) 'not-well-formed)
		(t def)))))

(defun xmltok-append-entity-def (d1 d2)
  (cond ((consp d1)
	 (if (consp d2)
	     (cons (concat (car d1) (car d2))
		   (and (cdr d1)
			(cdr d2)
			(concat (cdr d1) (cdr d2))))
	   d2))
	((consp d2) d1)
	(t
	 (let ((defs '(not-well-formed external element)))
	   (while (not (or (eq (car defs) d1)
			   (eq (car defs) d2)))
	     (setq defs (cdr defs)))
	   (car defs)))))

(defun xmltok-add-prolog-region (type start end)
  (setq xmltok-prolog-regions
	(cons (vector type start end)
	      xmltok-prolog-regions)))

(defun xmltok-merge-attributes ()
  "Return a list merging `xmltok-attributes' and `xmltok-namespace-attributes'.
The members of the merged list are in order of occurrence in the
document.  The list may share list structure with `xmltok-attributes'
and `xmltok-namespace-attributes'."
  (cond ((not xmltok-namespace-attributes)
	 xmltok-attributes)
	((not xmltok-attributes)
	 xmltok-namespace-attributes)
	(t
	 (let ((atts1 xmltok-attributes)
	       (atts2 xmltok-namespace-attributes)
	       merged)
	   (while (and atts1 atts2)
	     (cond ((< (xmltok-attribute-name-start (car atts1))
		       (xmltok-attribute-name-start (car atts2)))
		    (setq merged (cons (car atts1) merged))
		    (setq atts1 (cdr atts1)))
		   (t
		    (setq merged (cons (car atts2) merged))
		    (setq atts2 (cdr atts2)))))
	   (setq merged (nreverse merged))
	   (cond (atts1 (setq merged (nconc merged atts1)))
		 (atts2 (setq merged (nconc merged atts2))))
	   merged))))

;;; Testing

(defun xmltok-forward-test ()
  (interactive)
  (if (xmltok-forward)
      (message "Scanned %s" xmltok-type)
    (message "Scanned nothing")))

(defun xmltok-next-prolog-token-test ()
  (interactive)
  (if (xmltok-next-prolog-token)
      (message "Scanned %s"
	       (if (integerp xmltok-type)
		   (string xmltok-type)
		 xmltok-type))
    (message "Scanned end of file")))

(provide 'xmltok)

;;; xmltok.el ends here
