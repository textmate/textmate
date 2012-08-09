;;; nxml-parse.el --- XML parser, sharing infrastructure with nxml-mode

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

;; Entry point is `nxml-parse-file'.

;;; Code:

(require 'nxml-util)
(require 'xmltok)
(require 'nxml-enc)
(require 'nxml-ns)

(defvar nxml-parse-file-name nil)

(defvar nxml-validate-function nil
  "Either nil or a function called by `nxml-parse-file' to perform validation.
The function will be called once for each start-tag or end-tag.  The
function is passed two arguments TEXT and START-TAG.  For a start-tag,
START-TAG is a list (NAME ATTRIBUTES) where NAME and ATTRIBUTES are in
the same form as returned by `nxml-parse-file'.  For an end-tag,
START-TAG is nil.  TEXT is a string containing the text immediately
preceding the tag, or nil if there was no such text.  An empty element
is treated as a start-tag followed by an end-tag.

For a start-tag, the namespace state will be the state after
processing the namespace declarations in the start-tag.  For an
end-tag, the namespace state will be the state before popping the
namespace declarations for the corresponding start-tag.

The function must return nil if no error is detected or a
cons (MESSAGE . LOCATION) where MESSAGE is a string containing
an error message and LOCATION indicates what caused the error
as follows:

- nil indicates the tag as whole caused it; this is always allowed;

- text indicates the text caused it; this is allowed only if
TEXT is non-nil;

- tag-close indicates the close of the tag caused it; this is
allowed only if START-TAG is non-nil;

- (attribute-name . N) indicates that the name of the Nth attribute
caused it; N counts from 0; this is allowed only if START-TAG is non-nil
and N must be less than the number of attributes;

- (attribute-value . N) indicates that the value of the Nth attribute
caused it; N counts from 0; this is allowed only if START-TAG is non-nil
and N must be less than the number of attributes.")

(defun nxml-parse-file (file)
  "Parse the XML document in FILE and return it as a list.
An XML element is represented as a list (NAME ATTRIBUTES . CHILDREN).
NAME is either a string, in the case where the name does not have a
namespace, or a cons (NAMESPACE . LOCAL-NAME), where NAMESPACE is a
symbol and LOCAL-NAME is a string, in the case where the name does
have a namespace.  NAMESPACE is a keyword whose name is `:URI', where
URI is the namespace name.  ATTRIBUTES is an alist of attributes where
each attribute has the form (NAME . VALUE), where NAME has the same
form as an element name, and VALUE is a string.  A namespace
declaration is represented as an attribute whose name is
\(:http://www.w3.org/2000/xmlns/ . LOCAL-NAME).  CHILDREN is a list
containing strings and child elements; CHILDREN never contains two
consecutive strings and never contains an empty string.  Processing
instructions and comments are not represented.  The return value is a
list representing the document element.

If the XML document is not well-formed, an error having the condition
`nxml-file-parse-error' will be signaled; the error data will be a
list of the form \(FILE POSITION MESSAGE), where POSITION is an
integer specifying the position where the error was detected, and
MESSAGE is a string describing the error.

The current contents of FILE will be parsed even if there is a
modified buffer currently visiting FILE.

If the variable `nxml-validate-function' is non-nil, it will be called
twice for each element, and any reported error will be signaled in the
same way as well-formedness error."
  (with-current-buffer (nxml-parse-find-file file)
    (unwind-protect
	(let ((nxml-parse-file-name file))
	  (nxml-parse-instance))
      (kill-buffer nil))))

(defun nxml-parse-find-file (file)
  (with-current-buffer (get-buffer-create " *nXML Parse*")
    (erase-buffer)
    (let ((set-auto-coding-function 'nxml-set-xml-coding))
      (insert-file-contents file))
    (current-buffer)))

(defun nxml-parse-instance ()
  (let (xmltok-dtd)
    (xmltok-save
      (xmltok-forward-prolog)
      (nxml-check-xmltok-errors)
      (nxml-ns-save
	(nxml-parse-instance-1)))))

(defun nxml-parse-instance-1 ()
  (let* ((top (cons nil nil))
	 ;; tail is a cons cell, whose cdr is nil
	 ;; additional elements will destructively appended to tail
	 (tail top)
	 ;; stack of tails one for each open element
	 tail-stack
	 ;; list of QNames of open elements
	 open-element-tags
	 ;; list of strings buffering a text node, in reverse order
	 text
	 ;; position of beginning of first (in buffer) string in text
	 text-pos)
    (while (xmltok-forward)
      (nxml-check-xmltok-errors)
      (cond ((memq xmltok-type '(start-tag end-tag empty-element))
	     (when text
	       (setq text (apply 'concat (nreverse text)))
	       (setcdr tail (cons text nil))
	       (setq tail (cdr tail)))
	     (when (not (eq xmltok-type 'end-tag))
	       (when (and (not open-element-tags)
			  (not (eq tail top)))
		 (nxml-parse-error nil "Multiple top-level elements"))
	       (setq open-element-tags
		     (cons (xmltok-start-tag-qname)
			   open-element-tags))
	       (nxml-ns-push-state)
	       (let ((tag (nxml-parse-start-tag)))
		 (nxml-validate-tag text text-pos tag)
		 (setq text nil)
		 (setcdr tail (cons tag nil))
		 (setq tail (cdr tail))
		 (setq tail-stack (cons tail tail-stack))
		 (setq tail (last tag))))
	     (when (not (eq xmltok-type 'start-tag))
	       (or (eq xmltok-type 'empty-element)
		   (equal (car open-element-tags)
			  (xmltok-end-tag-qname))
		   (if open-element-tags
		       (nxml-parse-error nil
					 "Unbalanced end-tag; expected </%s>"
					 (car open-element-tags))
		     (nxml-parse-error nil "Extra end-tag")))
	       (nxml-validate-tag text text-pos nil)
	       (setq text nil)
	       (nxml-ns-pop-state)
	       (setq open-element-tags (cdr open-element-tags))
	       (setq tail (car tail-stack))
	       (setq tail-stack (cdr tail-stack)))
	     (setq text-pos nil))
	    ((memq xmltok-type '(space data entity-ref char-ref cdata-section))
	     (cond (open-element-tags
		    (unless text-pos
		      (setq text-pos xmltok-start))
		    (setq text
			  (cons (nxml-current-text-string) text)))
		   ((not (eq xmltok-type 'space))
		    (nxml-parse-error
		     nil
		     "%s at top-level"
		     (cdr (assq xmltok-type
				'((data . "Text characters")
				  (entity-ref . "Entity reference")
				  (char-ref . "Character reference")
				  (cdata-section . "CDATA section"))))))))))
    (unless (cdr top)
      (nxml-parse-error (point-max) "Missing document element"))
    (cadr top)))

(defun nxml-parse-start-tag ()
  (let (parsed-attributes
	parsed-namespace-attributes
	atts att prefixes prefix ns value name)
    (setq atts xmltok-namespace-attributes)
    (while atts
      (setq att (car atts))
      (setq value (or (xmltok-attribute-value att)
		      (nxml-parse-error nil "Invalid attribute value")))
      (setq ns (nxml-make-namespace value))
      (setq prefix (and (xmltok-attribute-prefix att)
			(xmltok-attribute-local-name att)))
      (cond ((member prefix prefixes)
	     (nxml-parse-error nil "Duplicate namespace declaration"))
	    ((not prefix)
	     (nxml-ns-set-default ns))
	    (ns
	     (nxml-ns-set-prefix prefix ns))
	    (t (nxml-parse-error nil "Cannot undeclare namespace prefix")))
      (setq prefixes (cons prefix prefixes))
      (setq parsed-namespace-attributes
	    (cons (cons (nxml-make-name nxml-xmlns-namespace-uri
					(xmltok-attribute-local-name att))
			value)
		  parsed-namespace-attributes))
      (setq atts (cdr atts)))
    (setq name
	  (nxml-make-name
	   (let ((prefix (xmltok-start-tag-prefix)))
	     (if prefix
		 (or (nxml-ns-get-prefix prefix)
		     (nxml-parse-error (1+ xmltok-start)
				       "Prefix `%s' undeclared"
				       prefix))
	       (nxml-ns-get-default)))
	   (xmltok-start-tag-local-name)))
    (setq atts xmltok-attributes)
    (while atts
      (setq att (car atts))
      (setq ns
	    (let ((prefix (xmltok-attribute-prefix att)))
	      (and prefix
		   (or (nxml-ns-get-prefix prefix)
		       (nxml-parse-error (xmltok-attribute-name-start att)
					 "Prefix `%s' undeclared"
					 prefix)))))
      (setq parsed-attributes
	    (let ((nm (nxml-make-name ns
				      (xmltok-attribute-local-name att))))
	      (when (assoc nm parsed-attributes)
		(nxml-parse-error (xmltok-attribute-name-start att)
				  "Duplicate attribute"))
	      (cons (cons nm (or (xmltok-attribute-value att)
				 (nxml-parse-error nil "Invalid attribute value")))
		    parsed-attributes)))
      (setq atts (cdr atts)))
    ;; We want to end up with the attributes followed by the
    ;; the namespace attributes in the same order as
    ;; xmltok-attributes and xmltok-namespace-attributes respectively.
    (when parsed-namespace-attributes
      (setq parsed-attributes
	    (nconc parsed-namespace-attributes parsed-attributes)))
    (list name (nreverse parsed-attributes))))

(defun nxml-validate-tag (text text-pos tag)
  (when nxml-validate-function
    (let ((err (funcall nxml-validate-function text tag))
	  pos)
      (when err
	(setq pos (nxml-validate-error-position (cdr err)
						(and text text-pos)
						tag))
	(or pos (error "Incorrect return value from %s"
		       nxml-validate-function))
	(nxml-parse-error pos (car err))))))

(defun nxml-validate-error-position (location text-pos tag)
  (cond ((null location) xmltok-start)
	((eq location 'text)  text-pos)
	((eq location 'tag-close)
	 (and tag (- (point) (if (eq xmltok-type 'empty-element ) 2 1))))
	((consp location)
	 (let ((att (nth (cdr location) xmltok-attributes)))
	   (when (not att)
	     (setq att (nth (- (cdr location) (length xmltok-attributes))
			    xmltok-namespace-attributes)))
	   (cond ((not att))
		 ((eq (car location) 'attribute-name)
		  (xmltok-attribute-name-start att))
		 ((eq (car location) 'attribute-value)
		  (xmltok-attribute-value-start att)))))))

(defun nxml-make-name (ns local-name)
  (if ns
      (cons ns local-name)
    local-name))

(defun nxml-current-text-string ()
  (cond ((memq xmltok-type '(space data))
	 (buffer-substring-no-properties xmltok-start
					 (point)))
	((eq xmltok-type 'cdata-section)
	 (buffer-substring-no-properties (+ xmltok-start 9)
					 (- (point) 3)))
	((memq xmltok-type '(char-ref entity-ref))
	 (unless xmltok-replacement
	   (nxml-parse-error nil
			     (if (eq xmltok-type 'char-ref)
				 "Reference to unsupported Unicode character"
			       "Unresolvable entity reference")))
	 xmltok-replacement)))

(defun nxml-parse-error (position &rest args)
  (nxml-signal-file-parse-error nxml-parse-file-name
				(or position xmltok-start)
				(apply 'format args)))

(defun nxml-check-xmltok-errors ()
  (when xmltok-errors
    (let ((err (car (last xmltok-errors))))
      (nxml-signal-file-parse-error nxml-parse-file-name
				    (xmltok-error-start err)
				    (xmltok-error-message err)))))

(provide 'nxml-parse)

;;; nxml-parse.el ends here
