;;;; soap-client.el -- Access SOAP web services from Emacs

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Alexandru Harsanyi <AlexHarsanyi@gmail.com>
;; Created: December, 2009
;; Keywords: soap, web-services, comm, hypermedia
;; Package: soap-client
;; Homepage: http://code.google.com/p/emacs-soap-client

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
;; To use the SOAP client, you first need to load the WSDL document for the
;; service you want to access, using `soap-load-wsdl-from-url'.  A WSDL
;; document describes the available operations of the SOAP service, how their
;; parameters and responses are encoded.  To invoke operations, you use the
;; `soap-invoke' method passing it the WSDL, the service name, the operation
;; you wish to invoke and any required parameters.
;;
;; Ideally, the service you want to access will have some documentation about
;; the operations it supports.  If it does not, you can try using
;; `soap-inspect' to browse the WSDL document and see the available operations
;; and their parameters.
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'xml)
(require 'warnings)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'mm-decode)

(defsubst soap-warning (message &rest args)
  "Display a warning MESSAGE with ARGS, using the 'soap-client warning type."
  (display-warning 'soap-client (apply 'format message args) :warning))

(defgroup soap-client nil
  "Access SOAP web services from Emacs."
  :version "24.1"
  :group 'tools)

;;;; Support for parsing XML documents with namespaces

;; XML documents with namespaces are difficult to parse because the names of
;; the nodes depend on what "xmlns" aliases have been defined in the document.
;; To work with such documents, we introduce a translation layer between a
;; "well known" namespace tag and the local namespace tag in the document
;; being parsed.

(defconst soap-well-known-xmlns
  '(("apachesoap" . "http://xml.apache.org/xml-soap")
    ("soapenc" . "http://schemas.xmlsoap.org/soap/encoding/")
    ("wsdl" . "http://schemas.xmlsoap.org/wsdl/")
    ("wsdlsoap" . "http://schemas.xmlsoap.org/wsdl/soap/")
    ("xsd" . "http://www.w3.org/2001/XMLSchema")
    ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")
    ("soap" . "http://schemas.xmlsoap.org/soap/envelope/")
    ("soap12" . "http://schemas.xmlsoap.org/wsdl/soap12/")
    ("http" . "http://schemas.xmlsoap.org/wsdl/http/")
    ("mime" . "http://schemas.xmlsoap.org/wsdl/mime/"))
  "A list of well known xml namespaces and their aliases.")

(defvar soap-local-xmlns nil
  "A list of local namespace aliases.
This is a dynamically bound variable, controlled by
`soap-with-local-xmlns'.")

(defvar soap-default-xmlns nil
  "The default XML namespaces.
Names in this namespace will be unqualified.  This is a
dynamically bound variable, controlled by
`soap-with-local-xmlns'")

(defvar soap-target-xmlns nil
  "The target XML namespace.
New XSD elements will be defined in this namespace, unless they
are fully qualified for a different namespace.  This is a
dynamically bound variable, controlled by
`soap-with-local-xmlns'")

(defun soap-wk2l (well-known-name)
  "Return local variant of WELL-KNOWN-NAME.
This is done by looking up the namespace in the
`soap-well-known-xmlns' table and resolving the namespace to
the local name based on the current local translation table
`soap-local-xmlns'.  See also `soap-with-local-xmlns'."
  (let ((wk-name-1 (if (symbolp well-known-name)
                       (symbol-name well-known-name)
                       well-known-name)))
    (cond
      ((string-match "^\\(.*\\):\\(.*\\)$" wk-name-1)
       (let ((ns (match-string 1 wk-name-1))
             (name (match-string 2 wk-name-1)))
         (let ((namespace (cdr (assoc ns soap-well-known-xmlns))))
           (cond ((equal namespace soap-default-xmlns)
                  ;; Name is unqualified in the default namespace
                  (if (symbolp well-known-name)
                      (intern name)
                      name))
                 (t
                  (let* ((local-ns (car (rassoc namespace soap-local-xmlns)))
                         (local-name (concat local-ns ":" name)))
                    (if (symbolp well-known-name)
                        (intern local-name)
                        local-name)))))))
          (t well-known-name))))

(defun soap-l2wk (local-name)
  "Convert LOCAL-NAME into a well known name.
The namespace of LOCAL-NAME is looked up in the
`soap-well-known-xmlns' table and a well known namespace tag is
used in the name.

nil is returned if there is no well-known namespace for the
namespace of LOCAL-NAME."
  (let ((l-name-1 (if (symbolp local-name)
                       (symbol-name local-name)
                       local-name))
        namespace name)
    (cond
      ((string-match "^\\(.*\\):\\(.*\\)$" l-name-1)
       (setq name (match-string 2 l-name-1))
       (let ((ns (match-string 1 l-name-1)))
         (setq namespace (cdr (assoc ns soap-local-xmlns)))
         (unless namespace
           (error "Soap-l2wk(%s): no namespace for alias %s" local-name ns))))
      (t
       (setq name l-name-1)
       (setq namespace soap-default-xmlns)))

    (if namespace
        (let ((well-known-ns (car (rassoc namespace soap-well-known-xmlns))))
          (if well-known-ns
              (let ((well-known-name (concat well-known-ns ":" name)))
                (if (symbol-name local-name)
                    (intern well-known-name)
                    well-known-name))
              (progn
                ;; (soap-warning "soap-l2wk(%s): namespace %s has no well-known tag"
                ;;               local-name namespace)
                nil)))
        ;; if no namespace is defined, just return the unqualified name
        name)))


(defun soap-l2fq (local-name &optional use-tns)
  "Convert LOCAL-NAME into a fully qualified name.
A fully qualified name is a cons of the namespace name and the
name of the element itself.  For example \"xsd:string\" is
converted to \(\"http://www.w3.org/2001/XMLSchema\" . \"string\"\).

The USE-TNS argument specifies what to do when LOCAL-NAME has no
namespace tag.  If USE-TNS is non-nil, the `soap-target-xmlns'
will be used as the element's namespace, otherwise
`soap-default-xmlns' will be used.

This is needed because different parts of a WSDL document can use
different namespace aliases for the same element."
  (let ((local-name-1 (if (symbolp local-name)
                          (symbol-name local-name)
                          local-name)))
    (cond ((string-match "^\\(.*\\):\\(.*\\)$" local-name-1)
           (let ((ns (match-string 1 local-name-1))
                 (name (match-string 2 local-name-1)))
             (let ((namespace (cdr (assoc ns soap-local-xmlns))))
               (if namespace
                   (cons namespace name)
                   (error "Soap-l2fq(%s): unknown alias %s" local-name ns)))))
          (t
           (cons (if use-tns
                     soap-target-xmlns
                     soap-default-xmlns)
                 local-name)))))

(defun soap-extract-xmlns (node &optional xmlns-table)
  "Return a namespace alias table for NODE by extending XMLNS-TABLE."
  (let (xmlns default-ns target-ns)
    (dolist (a (xml-node-attributes node))
      (let ((name (symbol-name (car a)))
            (value (cdr a)))
        (cond ((string= name "targetNamespace")
               (setq target-ns value))
              ((string= name "xmlns")
               (setq default-ns value))
              ((string-match "^xmlns:\\(.*\\)$" name)
               (push (cons (match-string 1 name) value) xmlns)))))

    (let ((tns (assoc "tns" xmlns)))
      (cond ((and tns target-ns)
             ;; If a tns alias is defined for this node, it must match
             ;; the target namespace.
             (unless (equal target-ns (cdr tns))
               (soap-warning
		"soap-extract-xmlns(%s): tns alias and targetNamespace mismatch"
		(xml-node-name node))))
            ((and tns (not target-ns))
             (setq target-ns (cdr tns)))
            ((and (not tns) target-ns)
             ;; a tns alias was not defined in this node.  See if the node has
             ;; a "targetNamespace" attribute and add an alias to this.  Note
             ;; that we might override an existing tns alias in XMLNS-TABLE,
             ;; but that is intended.
             (push (cons "tns" target-ns) xmlns))))

    (list default-ns target-ns (append xmlns xmlns-table))))

(defmacro soap-with-local-xmlns (node &rest body)
  "Install a local alias table from NODE and execute BODY."
  (declare (debug (form &rest form)) (indent 1))
  (let ((xmlns (make-symbol "xmlns")))
    `(let ((,xmlns (soap-extract-xmlns ,node soap-local-xmlns)))
       (let ((soap-default-xmlns (or (nth 0 ,xmlns) soap-default-xmlns))
             (soap-target-xmlns (or (nth 1 ,xmlns) soap-target-xmlns))
             (soap-local-xmlns (nth 2 ,xmlns)))
         ,@body))))

(defun soap-get-target-namespace (node)
  "Return the target namespace of NODE.
This is the namespace in which new elements will be defined."
  (or (xml-get-attribute-or-nil node 'targetNamespace)
      (cdr (assoc "tns"  soap-local-xmlns))
      soap-target-xmlns))

(defun soap-xml-get-children1 (node child-name)
  "Return the children of NODE named CHILD-NAME.
This is the same as `xml-get-children', but CHILD-NAME can have
namespace tag."
  (let (result)
    (dolist (c (xml-node-children node))
      (when (and (consp c)
                 (soap-with-local-xmlns c
                   ;; We use `ignore-errors' here because we want to silently
                   ;; skip nodes for which we cannot convert them to a
                   ;; well-known name.
                   (eq (ignore-errors (soap-l2wk (xml-node-name c)))
		       child-name)))
        (push c result)))
    (nreverse result)))

(defun soap-xml-get-attribute-or-nil1 (node attribute)
  "Return the NODE's ATTRIBUTE, or nil if it does not exist.
This is the same as `xml-get-attribute-or-nil', but ATTRIBUTE can
be tagged with a namespace tag."
  (catch 'found
    (soap-with-local-xmlns node
      (dolist (a (xml-node-attributes node))
        ;; We use `ignore-errors' here because we want to silently skip
        ;; attributes for which we cannot convert them to a well-known name.
        (when (eq (ignore-errors (soap-l2wk (car a))) attribute)
          (throw 'found (cdr a)))))))


;;;; XML namespaces

;; An element in an XML namespace, "things" stored in soap-xml-namespaces will
;; be derived from this object.

(defstruct soap-element
  name
  ;; The "well-known" namespace tag for the element.  For example, while
  ;; parsing XML documents, we can have different tags for the XMLSchema
  ;; namespace, but internally all our XMLSchema elements will have the "xsd"
  ;; tag.
  namespace-tag)

(defun soap-element-fq-name (element)
  "Return a fully qualified name for ELEMENT.
A fq name is the concatenation of the namespace tag and the
element name."
  (concat (soap-element-namespace-tag element)
          ":" (soap-element-name element)))

;; a namespace link stores an alias for an object in once namespace to a
;; "target" object possibly in a different namespace

(defstruct (soap-namespace-link (:include soap-element))
  target)

;; A namespace is a collection of soap-element objects under a name (the name
;; of the namespace).

(defstruct soap-namespace
  (name nil :read-only t)               ; e.g "http://xml.apache.org/xml-soap"
  (elements (make-hash-table :test 'equal) :read-only t))

(defun soap-namespace-put (element ns)
  "Store ELEMENT in NS.
Multiple elements with the same name can be stored in a
namespace.  When retrieving the element you can specify a
discriminant predicate to `soap-namespace-get'"
  (let ((name (soap-element-name element)))
    (push element (gethash name (soap-namespace-elements ns)))))

(defun soap-namespace-put-link (name target ns &optional replace)
  "Store a link from NAME to TARGET in NS.
An error will be signaled if an element by the same name is
already present in NS, unless REPLACE is non nil.

TARGET can be either a SOAP-ELEMENT or a string denoting an
element name into another namespace.

If NAME is nil, an element with the same name as TARGET will be
added to the namespace."

  (unless (and name (not (equal name "")))
    ;; if name is nil, use TARGET as a name...
    (cond ((soap-element-p target)
           (setq name (soap-element-name target)))
          ((consp target)               ; a fq name: (namespace . name)
           (setq name (cdr target)))
          ((stringp target)
           (cond ((string-match "^\\(.*\\):\\(.*\\)$" target)
                  (setq name (match-string 2 target)))
                 (t
                  (setq name target))))))

  ;; by now, name should be valid
  (assert (and name (not (equal name "")))
          nil
          "Cannot determine name for namespace link")
  (push (make-soap-namespace-link :name name :target target)
        (gethash name (soap-namespace-elements ns))))

(defun soap-namespace-get (name ns &optional discriminant-predicate)
  "Retrieve an element with NAME from the namespace NS.
If multiple elements with the same name exist,
DISCRIMINANT-PREDICATE is used to pick one of them.  This allows
storing elements of different types (like a message type and a
binding) but the same name."
  (assert (stringp name))
  (let ((elements (gethash name (soap-namespace-elements ns))))
    (cond (discriminant-predicate
           (catch 'found
             (dolist (e elements)
               (when (funcall discriminant-predicate e)
                 (throw 'found e)))))
          ((= (length elements) 1) (car elements))
          ((> (length elements) 1)
           (error
	    "Soap-namespace-get(%s): multiple elements, discriminant needed"
	    name))
          (t
           nil))))


;;;; WSDL documents
;;;;; WSDL document elements

(defstruct (soap-basic-type (:include soap-element))
  kind                              ; a symbol of: string, dateTime, long, int
  )

(defstruct soap-sequence-element
  name type nillable? multiple?)

(defstruct (soap-sequence-type (:include soap-element))
  parent                                ; OPTIONAL WSDL-TYPE name
  elements                              ; LIST of SOAP-SEQUENCE-ELEMENT
  )

(defstruct (soap-array-type (:include soap-element))
  element-type                          ; WSDL-TYPE of the array elements
  )

(defstruct (soap-message (:include soap-element))
  parts                                 ; ALIST of NAME => WSDL-TYPE name
  )

(defstruct (soap-operation (:include soap-element))
  parameter-order
  input                                 ; (NAME . MESSAGE)
  output                                ; (NAME . MESSAGE)
  faults)                               ; a list of (NAME . MESSAGE)

(defstruct (soap-port-type (:include soap-element))
  operations)                           ; a namespace of operations

;; A bound operation is an operation which has a soap action and a use
;; method attached -- these are attached as part of a binding and we
;; can have different bindings for the same operations.
(defstruct soap-bound-operation
  operation                             ; SOAP-OPERATION
  soap-action                           ; value for SOAPAction HTTP header
  use                                   ; 'literal or 'encoded, see
					; http://www.w3.org/TR/wsdl#_soap:body
  )

(defstruct (soap-binding (:include soap-element))
  port-type
  (operations (make-hash-table :test 'equal) :readonly t))

(defstruct (soap-port (:include soap-element))
  service-url
  binding)

(defun soap-default-xsd-types ()
  "Return a namespace containing some of the XMLSchema types."
  (let ((ns (make-soap-namespace :name "http://www.w3.org/2001/XMLSchema")))
    (dolist (type '("string" "dateTime" "boolean" "long" "int" "float"
                    "base64Binary" "anyType" "Array" "byte[]"))
      (soap-namespace-put
       (make-soap-basic-type :name type :kind (intern type))
       ns))
    ns))

(defun soap-default-soapenc-types ()
  "Return a namespace containing some of the SOAPEnc types."
  (let ((ns (make-soap-namespace
	     :name "http://schemas.xmlsoap.org/soap/encoding/")))
    (dolist (type '("string" "dateTime" "boolean" "long" "int" "float"
                    "base64Binary" "anyType" "Array" "byte[]"))
      (soap-namespace-put
       (make-soap-basic-type :name type :kind (intern type))
       ns))
    ns))

(defun soap-type-p (element)
  "Return t if ELEMENT is a SOAP data type (basic or complex)."
  (or (soap-basic-type-p element)
      (soap-sequence-type-p element)
      (soap-array-type-p element)))


;;;;; The WSDL document

;; The WSDL data structure used for encoding/decoding SOAP messages
(defstruct soap-wsdl
  origin                         ; file or URL from which this wsdl was loaded
  ports                          ; a list of SOAP-PORT instances
  alias-table                    ; a list of namespace aliases
  namespaces                     ; a list of namespaces
  )

(defun soap-wsdl-add-alias (alias name wsdl)
  "Add a namespace ALIAS for NAME to the WSDL document."
  (push (cons alias name) (soap-wsdl-alias-table wsdl)))

(defun soap-wsdl-find-namespace (name wsdl)
  "Find a namespace by NAME in the WSDL document."
  (catch 'found
    (dolist (ns (soap-wsdl-namespaces wsdl))
      (when (equal name (soap-namespace-name ns))
        (throw 'found ns)))))

(defun soap-wsdl-add-namespace (ns wsdl)
  "Add the namespace NS to the WSDL document.
If a namespace by this name already exists in WSDL, individual
elements will be added to it."
  (let ((existing (soap-wsdl-find-namespace (soap-namespace-name ns) wsdl)))
    (if existing
        ;; Add elements from NS to EXISTING, replacing existing values.
        (maphash (lambda (key value)
                   (dolist (v value)
                     (soap-namespace-put v existing)))
                 (soap-namespace-elements ns))
        (push ns (soap-wsdl-namespaces wsdl)))))

(defun soap-wsdl-get (name wsdl &optional predicate use-local-alias-table)
  "Retrieve element NAME from the WSDL document.

PREDICATE is used to differentiate between elements when NAME
refers to multiple elements.  A typical value for this would be a
structure predicate for the type of element you want to retrieve.
For example, to retrieve a message named \"foo\" when other
elements named \"foo\" exist in the WSDL you could use:

  (soap-wsdl-get \"foo\" WSDL 'soap-message-p)

If USE-LOCAL-ALIAS-TABLE is not nil, `soap-local-xmlns` will be
used to resolve the namespace alias."
  (let ((alias-table (soap-wsdl-alias-table wsdl))
        namespace element-name element)

    (when (symbolp name)
      (setq name (symbol-name name)))

    (when use-local-alias-table
      (setq alias-table (append soap-local-xmlns alias-table)))

    (cond ((consp name) ; a fully qualified name, as returned by `soap-l2fq'
           (setq element-name (cdr name))
           (when (symbolp element-name)
             (setq element-name (symbol-name element-name)))
           (setq namespace (soap-wsdl-find-namespace (car name) wsdl))
           (unless namespace
             (error "Soap-wsdl-get(%s): unknown namespace: %s" name namespace)))

          ((string-match "^\\(.*\\):\\(.*\\)$" name)
           (setq element-name (match-string 2 name))

           (let* ((ns-alias (match-string 1 name))
                  (ns-name (cdr (assoc ns-alias alias-table))))
             (unless ns-name
               (error "Soap-wsdl-get(%s): cannot find namespace alias %s"
		      name ns-alias))

             (setq namespace (soap-wsdl-find-namespace ns-name wsdl))
             (unless namespace
               (error
		"Soap-wsdl-get(%s): unknown namespace %s, referenced by alias %s"
		name ns-name ns-alias))))
          (t
           (error "Soap-wsdl-get(%s): bad name" name)))

    (setq element (soap-namespace-get
                   element-name namespace
                   (if predicate
                       (lambda (e)
                         (or (funcall 'soap-namespace-link-p e)
                             (funcall predicate e)))
                       nil)))

    (unless element
      (error "Soap-wsdl-get(%s): cannot find element" name))

    (if (soap-namespace-link-p element)
        ;; NOTE: don't use the local alias table here
        (soap-wsdl-get (soap-namespace-link-target element) wsdl predicate)
        element)))

;;;;; Resolving references for wsdl types

;; See `soap-wsdl-resolve-references', which is the main entry point for
;; resolving references

(defun soap-resolve-references-for-element (element wsdl)
  "Resolve references in ELEMENT using the WSDL document.
This is a generic function which invokes a specific function
depending on the element type.

If ELEMENT has no resolver function, it is silently ignored.

All references are resolved in-place, that is the ELEMENT is
updated."
  (let ((resolver (get (aref element 0) 'soap-resolve-references)))
    (when resolver
      (funcall resolver element wsdl))))

(defun soap-resolve-references-for-sequence-type (type wsdl)
  "Resolve references for a sequence TYPE using WSDL document.
See also `soap-resolve-references-for-element' and
`soap-wsdl-resolve-references'"
  (let ((parent (soap-sequence-type-parent type)))
    (when (or (consp parent) (stringp parent))
      (setf (soap-sequence-type-parent type)
            (soap-wsdl-get parent wsdl 'soap-type-p))))
  (dolist (element (soap-sequence-type-elements type))
    (let ((element-type (soap-sequence-element-type element)))
      (cond ((or (consp element-type) (stringp element-type))
             (setf (soap-sequence-element-type element)
                   (soap-wsdl-get element-type wsdl 'soap-type-p)))
            ((soap-element-p element-type)
             ;; since the element already has a child element, it
             ;; could be an inline structure.  we must resolve
             ;; references in it, because it might not be reached by
             ;; scanning the wsdl names.
             (soap-resolve-references-for-element element-type wsdl))))))

(defun soap-resolve-references-for-array-type (type wsdl)
  "Resolve references for an array TYPE using WSDL.
See also `soap-resolve-references-for-element' and
`soap-wsdl-resolve-references'"
  (let ((element-type (soap-array-type-element-type type)))
    (when (or (consp element-type) (stringp element-type))
      (setf (soap-array-type-element-type type)
            (soap-wsdl-get element-type wsdl 'soap-type-p)))))

(defun soap-resolve-references-for-message (message wsdl)
  "Resolve references for a MESSAGE type using the WSDL document.
See also `soap-resolve-references-for-element' and
`soap-wsdl-resolve-references'"
  (let (resolved-parts)
    (dolist (part (soap-message-parts message))
      (let ((name (car part))
            (type (cdr part)))
        (when (stringp name)
          (setq name (intern name)))
        (when (or (consp type) (stringp type))
          (setq type (soap-wsdl-get type wsdl 'soap-type-p)))
        (push (cons name type) resolved-parts)))
     (setf (soap-message-parts message) (nreverse resolved-parts))))

(defun soap-resolve-references-for-operation (operation wsdl)
  "Resolve references for an OPERATION type using the WSDL document.
See also `soap-resolve-references-for-element' and
`soap-wsdl-resolve-references'"
  (let ((input (soap-operation-input operation))
        (counter 0))
    (let ((name (car input))
          (message (cdr input)))
      ;; Name this part if it was not named
      (when (or (null name) (equal name ""))
        (setq name (format "in%d" (incf counter))))
      (when (or (consp message) (stringp message))
        (setf (soap-operation-input operation)
              (cons (intern name)
		    (soap-wsdl-get message wsdl 'soap-message-p))))))

  (let ((output (soap-operation-output operation))
        (counter 0))
    (let ((name (car output))
          (message (cdr output)))
      (when (or (null name) (equal name ""))
        (setq name (format "out%d" (incf counter))))
      (when (or (consp message) (stringp message))
        (setf (soap-operation-output operation)
              (cons (intern name)
		    (soap-wsdl-get message wsdl 'soap-message-p))))))

  (let ((resolved-faults nil)
        (counter 0))
    (dolist (fault (soap-operation-faults operation))
      (let ((name (car fault))
            (message (cdr fault)))
        (when (or (null name) (equal name ""))
          (setq name (format "fault%d" (incf counter))))
        (if (or (consp message) (stringp message))
            (push (cons (intern name)
			(soap-wsdl-get message wsdl 'soap-message-p))
                  resolved-faults)
            (push fault resolved-faults))))
    (setf (soap-operation-faults operation) resolved-faults))

  (when (= (length (soap-operation-parameter-order operation)) 0)
    (setf (soap-operation-parameter-order operation)
          (mapcar 'car (soap-message-parts
                        (cdr (soap-operation-input operation))))))

  (setf (soap-operation-parameter-order operation)
        (mapcar (lambda (p)
                  (if (stringp p)
                      (intern p)
                      p))
                (soap-operation-parameter-order operation))))

(defun soap-resolve-references-for-binding (binding wsdl)
 "Resolve references for a BINDING type using the WSDL document.
See also `soap-resolve-references-for-element' and
`soap-wsdl-resolve-references'"
  (when (or (consp (soap-binding-port-type binding))
            (stringp (soap-binding-port-type binding)))
    (setf (soap-binding-port-type binding)
          (soap-wsdl-get (soap-binding-port-type binding)
			 wsdl 'soap-port-type-p)))

  (let ((port-ops (soap-port-type-operations (soap-binding-port-type binding))))
    (maphash (lambda (k v)
               (setf (soap-bound-operation-operation v)
                     (soap-namespace-get k port-ops 'soap-operation-p)))
             (soap-binding-operations binding))))

(defun soap-resolve-references-for-port (port wsdl)
  "Resolve references for a PORT type using the WSDL document.
See also `soap-resolve-references-for-element' and
`soap-wsdl-resolve-references'"
  (when (or (consp (soap-port-binding port))
            (stringp (soap-port-binding port)))
    (setf (soap-port-binding port)
          (soap-wsdl-get (soap-port-binding port) wsdl 'soap-binding-p))))

;; Install resolvers for our types
(progn
  (put (aref (make-soap-sequence-type) 0) 'soap-resolve-references
       'soap-resolve-references-for-sequence-type)
  (put (aref (make-soap-array-type) 0) 'soap-resolve-references
       'soap-resolve-references-for-array-type)
  (put (aref (make-soap-message) 0) 'soap-resolve-references
       'soap-resolve-references-for-message)
  (put (aref (make-soap-operation) 0) 'soap-resolve-references
       'soap-resolve-references-for-operation)
  (put (aref (make-soap-binding) 0) 'soap-resolve-references
       'soap-resolve-references-for-binding)
  (put (aref (make-soap-port) 0) 'soap-resolve-references
       'soap-resolve-references-for-port))

(defun soap-wsdl-resolve-references (wsdl)
  "Resolve all references inside the WSDL structure.

When the WSDL elements are created from the XML document, they
refer to each other by name.  For example, the ELEMENT-TYPE slot
of an SOAP-ARRAY-TYPE will contain the name of the element and
the user would have to call `soap-wsdl-get' to obtain the actual
element.

After the entire document is loaded, we resolve all these
references to the actual elements they refer to so that at
runtime, we don't have to call `soap-wsdl-get' each time we
traverse an element tree."
  (let ((nprocessed 0)
        (nstag-id 0)
        (alias-table (soap-wsdl-alias-table wsdl)))
    (dolist (ns (soap-wsdl-namespaces wsdl))
      (let ((nstag (car-safe (rassoc (soap-namespace-name ns) alias-table))))
        (unless nstag
          ;; If this namespace does not have an alias, create one for it.
          (catch 'done
            (while t
              (setq nstag (format "ns%d" (incf nstag-id)))
              (unless (assoc nstag alias-table)
                (soap-wsdl-add-alias nstag (soap-namespace-name ns) wsdl)
                (throw 'done t)))))

        (maphash (lambda (name element)
                   (cond ((soap-element-p element) ; skip links
                          (incf nprocessed)
                          (soap-resolve-references-for-element element wsdl)
                          (setf (soap-element-namespace-tag element) nstag))
                         ((listp element)
                          (dolist (e element)
                            (when (soap-element-p e)
                              (incf nprocessed)
                              (soap-resolve-references-for-element e wsdl)
                              (setf (soap-element-namespace-tag e) nstag))))))
                 (soap-namespace-elements ns)))))
    wsdl)

;;;;; Loading WSDL from XML documents

(defun soap-load-wsdl-from-url (url)
  "Load a WSDL document from URL and return it.
The returned WSDL document needs to be used for `soap-invoke'
calls."
  (let ((url-request-method "GET")
        (url-package-name "soap-client.el")
        (url-package-version "1.0")
        (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
        (url-request-coding-system 'utf-8)
        (url-http-attempt-keepalives nil))
    (let ((buffer (url-retrieve-synchronously url)))
      (with-current-buffer buffer
        (declare (special url-http-response-status))
        (if (> url-http-response-status 299)
            (error "Error retrieving WSDL: %s" url-http-response-status))
        (let ((mime-part (mm-dissect-buffer t t)))
          (unless mime-part
            (error "Failed to decode response from server"))
          (unless (equal (car (mm-handle-type mime-part)) "text/xml")
            (error "Server response is not an XML document"))
          (with-temp-buffer
            (mm-insert-part mime-part)
            (let ((wsdl-xml (car (xml-parse-region (point-min) (point-max)))))
              (prog1
                  (let ((wsdl (soap-parse-wsdl wsdl-xml)))
                    (setf (soap-wsdl-origin wsdl) url)
                    wsdl)
                (kill-buffer buffer)))))))))

(defun soap-load-wsdl (file)
  "Load a WSDL document from FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((xml (car (xml-parse-region (point-min) (point-max)))))
      (let ((wsdl (soap-parse-wsdl xml)))
        (setf (soap-wsdl-origin wsdl) file)
        wsdl))))

(defun soap-parse-wsdl (node)
  "Construct a WSDL structure from NODE, which is an XML document."
  (soap-with-local-xmlns node

    (assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:definitions)
            nil
            "soap-parse-wsdl: expecting wsdl:definitions node, got %s"
            (soap-l2wk (xml-node-name node)))

    (let ((wsdl (make-soap-wsdl)))

      ;; Add the local alias table to the wsdl document -- it will be used for
      ;; all types in this document even after we finish parsing it.
      (setf (soap-wsdl-alias-table wsdl) soap-local-xmlns)

      ;; Add the XSD types to the wsdl document
      (let ((ns (soap-default-xsd-types)))
        (soap-wsdl-add-namespace ns wsdl)
        (soap-wsdl-add-alias "xsd" (soap-namespace-name ns) wsdl))

      ;; Add the soapenc types to the wsdl document
      (let ((ns (soap-default-soapenc-types)))
        (soap-wsdl-add-namespace ns wsdl)
        (soap-wsdl-add-alias "soapenc" (soap-namespace-name ns) wsdl))

      ;; Find all the 'xsd:schema nodes which are children of wsdl:types nodes
      ;; and build our type-library

      (let ((types (car (soap-xml-get-children1 node 'wsdl:types))))
        (dolist (node (xml-node-children types))
          ;; We cannot use (xml-get-children node (soap-wk2l 'xsd:schema))
          ;; because each node can install its own alias type so the schema
          ;; nodes might have a different prefix.
          (when (consp node)
            (soap-with-local-xmlns node
              (when (eq (soap-l2wk (xml-node-name node)) 'xsd:schema)
                (soap-wsdl-add-namespace (soap-parse-schema node) wsdl))))))

      (let ((ns (make-soap-namespace :name (soap-get-target-namespace node))))
        (dolist (node (soap-xml-get-children1 node 'wsdl:message))
          (soap-namespace-put (soap-parse-message node) ns))

        (dolist (node (soap-xml-get-children1 node 'wsdl:portType))
          (let ((port-type (soap-parse-port-type node)))
            (soap-namespace-put port-type ns)
            (soap-wsdl-add-namespace
	     (soap-port-type-operations port-type) wsdl)))

        (dolist (node (soap-xml-get-children1 node 'wsdl:binding))
          (soap-namespace-put (soap-parse-binding node) ns))

        (dolist (node (soap-xml-get-children1 node 'wsdl:service))
          (dolist (node (soap-xml-get-children1 node 'wsdl:port))
            (let ((name (xml-get-attribute node 'name))
                  (binding (xml-get-attribute node 'binding))
                  (url (let ((n (car (soap-xml-get-children1
				      node 'wsdlsoap:address))))
                         (xml-get-attribute n 'location))))
              (let ((port (make-soap-port
                           :name name :binding (soap-l2fq binding 'tns)
			   :service-url url)))
                (soap-namespace-put port ns)
                (push port (soap-wsdl-ports wsdl))))))

        (soap-wsdl-add-namespace ns wsdl))

      (soap-wsdl-resolve-references wsdl)

      wsdl)))

(defun soap-parse-schema (node)
  "Parse a schema NODE.
Return a SOAP-NAMESPACE containing the elements."
  (soap-with-local-xmlns node
    (assert (eq (soap-l2wk (xml-node-name node)) 'xsd:schema)
            nil
            "soap-parse-schema: expecting an xsd:schema node, got %s"
            (soap-l2wk (xml-node-name node)))
    (let ((ns (make-soap-namespace :name (soap-get-target-namespace node))))
      ;; NOTE: we only extract the complexTypes from the schema, we wouldn't
      ;; know how to handle basic types beyond the built in ones anyway.
      (dolist (node (soap-xml-get-children1 node 'xsd:complexType))
        (soap-namespace-put (soap-parse-complex-type node) ns))

      (dolist (node (soap-xml-get-children1 node 'xsd:element))
        (soap-namespace-put (soap-parse-schema-element node) ns))

      ns)))

(defun soap-parse-schema-element (node)
  "Parse NODE and construct a schema element from it."
  (assert (eq (soap-l2wk (xml-node-name node)) 'xsd:element)
          nil
          "soap-parse-schema-element: expecting xsd:element node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        type)
    ;; A schema element that contains an inline complex type --
    ;; construct the actual complex type for it.
    (let ((type-node (soap-xml-get-children1 node 'xsd:complexType)))
      (when (> (length type-node) 0)
        (assert (= (length type-node) 1)) ; only one complex type
					  ; definition per element
        (setq type (soap-parse-complex-type (car type-node)))))
    (setf (soap-element-name type) name)
    type))

(defun soap-parse-complex-type (node)
  "Parse NODE and construct a complex type from it."
  (assert (eq (soap-l2wk (xml-node-name node)) 'xsd:complexType)
          nil
          "soap-parse-complex-type: expecting xsd:complexType node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        ;; Use a dummy type for the complex type, it will be replaced
        ;; with the real type below, except when the complex type node
        ;; is empty...
        (type (make-soap-sequence-type :elements nil)))
    (dolist (c (xml-node-children node))
      (when (consp c)               ; skip string nodes, which are whitespace
        (let ((node-name (soap-l2wk (xml-node-name c))))
          (cond
            ;; The difference between xsd:all and xsd:sequence is that fields
            ;; in xsd:all are not ordered and they can occur only once.  We
            ;; don't care about that difference in soap-client.el
            ((or (eq node-name 'xsd:sequence)
                 (eq node-name 'xsd:all))
             (setq type (soap-parse-complex-type-sequence c)))
            ((eq node-name 'xsd:complexContent)
             (setq type (soap-parse-complex-type-complex-content c)))
            ((eq node-name 'xsd:attribute)
             ;; The name of this node comes from an attribute tag
             (let ((n (xml-get-attribute-or-nil c 'name)))
               (setq name n)))
            (t
             (error "Unknown node type %s" node-name))))))
    (setf (soap-element-name type) name)
    type))

(defun soap-parse-sequence (node)
  "Parse NODE and a list of sequence elements that it defines.
NODE is assumed to be an xsd:sequence node.  In that case, each
of its children is assumed to be a sequence element.  Each
sequence element is parsed constructing the corresponding type.
A list of these types is returned."
  (assert (let ((n (soap-l2wk (xml-node-name node))))
            (memq n '(xsd:sequence xsd:all)))
          nil
          "soap-parse-sequence: expecting xsd:sequence or xsd:all node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let (elements)
    (dolist (e (soap-xml-get-children1 node 'xsd:element))
      (let ((name (xml-get-attribute-or-nil e 'name))
            (type (xml-get-attribute-or-nil e 'type))
            (nillable? (or (equal (xml-get-attribute-or-nil e 'nillable) "true")
                           (let ((e (xml-get-attribute-or-nil e 'minOccurs)))
                             (and e (equal e "0")))))
            (multiple? (let ((e (xml-get-attribute-or-nil e 'maxOccurs)))
                         (and e (not (equal e "1"))))))
        (if type
            (setq type (soap-l2fq type 'tns))

            ;; The node does not have a type, maybe it has a complexType
            ;; defined inline...
            (let ((type-node (soap-xml-get-children1 e 'xsd:complexType)))
              (when (> (length type-node) 0)
                (assert (= (length type-node) 1)
                        nil
                        "only one complex type definition per element supported")
                (setq type (soap-parse-complex-type (car type-node))))))

        (push (make-soap-sequence-element
               :name (intern name) :type type :nillable? nillable?
	       :multiple? multiple?)
              elements)))
    (nreverse elements)))

(defun soap-parse-complex-type-sequence (node)
  "Parse NODE as a sequence type."
  (let ((elements (soap-parse-sequence node)))
    (make-soap-sequence-type :elements elements)))

(defun soap-parse-complex-type-complex-content (node)
  "Parse NODE as a xsd:complexContent node.
A sequence or an array type is returned depending on the actual
contents."
  (assert (eq (soap-l2wk (xml-node-name node)) 'xsd:complexContent)
          nil
          "soap-parse-complex-type-complex-content: expecting xsd:complexContent node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let (array? parent elements)
    (let ((extension (car-safe (soap-xml-get-children1 node 'xsd:extension)))
          (restriction (car-safe
			(soap-xml-get-children1 node 'xsd:restriction))))
      ;; a complex content node is either an extension or a restriction
      (cond (extension
             (setq parent (xml-get-attribute-or-nil extension 'base))
             (setq elements (soap-parse-sequence
                             (car (soap-xml-get-children1
				   extension 'xsd:sequence)))))
            (restriction
             (let ((base (xml-get-attribute-or-nil restriction 'base)))
               (assert (equal base "soapenc:Array")
                       nil
                       "restrictions supported only for soapenc:Array types, this is a %s"
                       base))
             (setq array? t)
             (let ((attribute (car (soap-xml-get-children1
				    restriction 'xsd:attribute))))
               (let ((array-type (soap-xml-get-attribute-or-nil1
				  attribute 'wsdl:arrayType)))
                 (when (string-match "^\\(.*\\)\\[\\]$" array-type)
                   (setq parent (match-string 1 array-type))))))

            (t
             (error "Unknown complex type"))))

    (if parent
        (setq parent (soap-l2fq parent 'tns)))

    (if array?
        (make-soap-array-type :element-type parent)
        (make-soap-sequence-type :parent parent :elements elements))))

(defun soap-parse-message (node)
  "Parse NODE as a wsdl:message and return the corresponding type."
  (assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:message)
          nil
          "soap-parse-message: expecting wsdl:message node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        parts)
    (dolist (p (soap-xml-get-children1 node 'wsdl:part))
      (let ((name (xml-get-attribute-or-nil p 'name))
            (type (xml-get-attribute-or-nil p 'type))
            (element (xml-get-attribute-or-nil p 'element)))

        (when type
          (setq type (soap-l2fq type 'tns)))

        (when element
          (setq element (soap-l2fq element 'tns)))

        (push (cons name (or type element)) parts)))
    (make-soap-message :name name :parts (nreverse parts))))

(defun soap-parse-port-type (node)
  "Parse NODE as a wsdl:portType and return the corresponding port."
  (assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:portType)
          nil
          "soap-parse-port-type: expecting wsdl:portType node got %s"
          (soap-l2wk (xml-node-name node)))
  (let ((ns (make-soap-namespace
             :name (concat "urn:" (xml-get-attribute node 'name)))))
    (dolist (node (soap-xml-get-children1 node 'wsdl:operation))
      (let ((o (soap-parse-operation node)))

        (let ((other-operation (soap-namespace-get
				(soap-element-name o) ns 'soap-operation-p)))
          (if other-operation
              ;; Unfortunately, the Confluence WSDL defines two operations
              ;; named "search" which differ only in parameter names...
              (soap-warning "Discarding duplicate operation: %s"
			    (soap-element-name o))

              (progn
                (soap-namespace-put o ns)

                ;; link all messages from this namespace, as this namespace
                ;; will be used for decoding the response.
                (destructuring-bind (name . message) (soap-operation-input o)
                  (soap-namespace-put-link name message ns))

                (destructuring-bind (name . message) (soap-operation-output o)
                  (soap-namespace-put-link name message ns))

                (dolist (fault (soap-operation-faults o))
                  (destructuring-bind (name . message) fault
                    (soap-namespace-put-link name message ns 'replace)))

                )))))

    (make-soap-port-type :name (xml-get-attribute node 'name)
                        :operations ns)))

(defun soap-parse-operation (node)
  "Parse NODE as a wsdl:operation and return the corresponding type."
  (assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:operation)
          nil
          "soap-parse-operation: expecting wsdl:operation node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute node 'name))
        (parameter-order (split-string
			  (xml-get-attribute node 'parameterOrder)))
        input output faults)
    (dolist (n (xml-node-children node))
      (when (consp n)                 ; skip string nodes which are whitespace
        (let ((node-name (soap-l2wk (xml-node-name n))))
          (cond
            ((eq node-name 'wsdl:input)
             (let ((message (xml-get-attribute n 'message))
                   (name (xml-get-attribute n 'name)))
               (setq input (cons name (soap-l2fq message 'tns)))))
            ((eq node-name 'wsdl:output)
             (let ((message (xml-get-attribute n 'message))
                   (name (xml-get-attribute n 'name)))
               (setq output (cons name (soap-l2fq message 'tns)))))
            ((eq node-name 'wsdl:fault)
             (let ((message (xml-get-attribute n 'message))
                   (name (xml-get-attribute n 'name)))
               (push (cons name (soap-l2fq message 'tns)) faults)))))))
    (make-soap-operation
     :name name
     :parameter-order parameter-order
     :input input
     :output output
     :faults (nreverse faults))))

(defun soap-parse-binding (node)
  "Parse NODE as a wsdl:binding and return the corresponding type."
  (assert (eq (soap-l2wk (xml-node-name node)) 'wsdl:binding)
          nil
          "soap-parse-binding: expecting wsdl:binding node, got %s"
          (soap-l2wk (xml-node-name node)))
  (let ((name (xml-get-attribute node 'name))
        (type (xml-get-attribute node 'type)))
    (let ((binding (make-soap-binding :name name
				      :port-type (soap-l2fq type 'tns))))
      (dolist (wo (soap-xml-get-children1 node 'wsdl:operation))
        (let ((name (xml-get-attribute wo 'name))
              soap-action
              use)
          (dolist (so (soap-xml-get-children1 wo 'wsdlsoap:operation))
            (setq soap-action (xml-get-attribute-or-nil so 'soapAction)))

          ;; Search a wsdlsoap:body node and find a "use" tag.  The
          ;; same use tag is assumed to be present for both input and
          ;; output types (although the WDSL spec allows separate
          ;; "use"-s for each of them...

          (dolist (i (soap-xml-get-children1 wo 'wsdl:input))
            (dolist (b (soap-xml-get-children1 i 'wsdlsoap:body))
              (setq use (or use
                            (xml-get-attribute-or-nil b 'use)))))

          (unless use
            (dolist (i (soap-xml-get-children1 wo 'wsdl:output))
              (dolist (b (soap-xml-get-children1 i 'wsdlsoap:body))
                (setq use (or use
                              (xml-get-attribute-or-nil b 'use))))))

          (puthash name (make-soap-bound-operation :operation name
                                                   :soap-action soap-action
                                                   :use (and use (intern use)))
                   (soap-binding-operations binding))))
      binding)))

;;;; SOAP type decoding

(defvar soap-multi-refs nil
  "The list of multi-ref nodes in the current SOAP response.
This is a dynamically bound variable used during decoding the
SOAP response.")

(defvar soap-decoded-multi-refs nil
  "List of decoded multi-ref nodes in the current SOAP response.
This is a dynamically bound variable used during decoding the
SOAP response.")

(defvar soap-current-wsdl nil
  "The current WSDL document used when decoding the SOAP response.
This is a dynamically bound variable.")

(defun soap-decode-type (type node)
  "Use TYPE (an xsd type) to decode the contents of NODE.

NODE is an XML node, representing some SOAP encoded value or a
reference to another XML node (a multiRef).  This function will
resolve the multiRef reference, if any, than call a TYPE specific
decode function to perform the actual decoding."
  (let ((href (xml-get-attribute-or-nil node 'href)))
    (cond (href
           (catch 'done
             ;; NODE is actually a HREF, find the target and decode that.
             ;; Check first if we already decoded this multiref.

             (let ((decoded (cdr (assoc href soap-decoded-multi-refs))))
               (when decoded
                 (throw 'done decoded)))

             (string-match "^#\\(.*\\)$" href) ; TODO: check that it matched

             (let ((id (match-string 1 href)))
               (dolist (mr soap-multi-refs)
                 (let ((mrid (xml-get-attribute mr 'id)))
                   (when (equal id mrid)
                     ;; recurse here, in case there are multiple HREF's
                     (let ((decoded (soap-decode-type type mr)))
                       (push (cons href decoded) soap-decoded-multi-refs)
                       (throw 'done decoded)))))
               (error "Cannot find href %s" href))))
          (t
           (soap-with-local-xmlns node
             (if (equal (soap-xml-get-attribute-or-nil1 node 'xsi:nil) "true")
                 nil
                 (let ((decoder (get (aref type 0) 'soap-decoder)))
                   (assert decoder nil "no soap-decoder for %s type"
			   (aref type 0))
                   (funcall decoder type node))))))))

(defun soap-decode-any-type (node)
  "Decode NODE using type information inside it."
  ;; If the NODE has type information, we use that...
  (let ((type (soap-xml-get-attribute-or-nil1 node 'xsi:type)))
    (if type
        (let ((wtype (soap-wsdl-get type soap-current-wsdl 'soap-type-p)))
          (if wtype
              (soap-decode-type wtype node)
              ;; The node has type info encoded in it, but we don't know how
              ;; to decode it...
              (error "Soap-decode-any-type: node has unknown type: %s" type)))

        ;; No type info in the node...

        (let ((contents (xml-node-children node)))
          (if (and (= (length contents) 1) (stringp (car contents)))
              ;; contents is just a string
              (car contents)

              ;; we assume the NODE is a sequence with every element a
              ;; structure name
              (let (result)
                (dolist (element contents)
                  (let ((key (xml-node-name element))
                        (value (soap-decode-any-type element)))
                    (push (cons key value) result)))
                (nreverse result)))))))

(defun soap-decode-array (node)
  "Decode NODE as an Array using type information inside it."
  (let ((type (soap-xml-get-attribute-or-nil1 node 'soapenc:arrayType))
        (wtype nil)
        (contents (xml-node-children node))
        result)
    (when type
        ;; Type is in the format "someType[NUM]" where NUM is the number of
        ;; elements in the array.  We discard the [NUM] part.
        (setq type (replace-regexp-in-string "\\[[0-9]+\\]\\'" "" type))
        (setq wtype (soap-wsdl-get type soap-current-wsdl 'soap-type-p))
        (unless wtype
          ;; The node has type info encoded in it, but we don't know how to
          ;; decode it...
          (error "Soap-decode-array: node has unknown type: %s" type)))
    (dolist (e contents)
      (when (consp e)
        (push (if wtype
                  (soap-decode-type wtype e)
                  (soap-decode-any-type e))
              result)))
    (nreverse result)))

(defun soap-decode-basic-type (type node)
  "Use TYPE to decode the contents of NODE.
TYPE is a `soap-basic-type' struct, and NODE is an XML document.
A LISP value is returned based on the contents of NODE and the
type-info stored in TYPE."
  (let ((contents (xml-node-children node))
        (type-kind (soap-basic-type-kind type)))

    (if (null contents)
        nil
        (ecase type-kind
          (string (car contents))
          (dateTime (car contents))     ; TODO: convert to a date time
          ((long int float) (string-to-number (car contents)))
          (boolean (string= (downcase (car contents)) "true"))
          (base64Binary (base64-decode-string (car contents)))
          (anyType (soap-decode-any-type node))
          (Array (soap-decode-array node))))))

(defun soap-decode-sequence-type (type node)
  "Use TYPE to decode the contents of NODE.
TYPE is assumed to be a sequence type and an ALIST with the
contents of the NODE is returned."
  (let ((result nil)
        (parent (soap-sequence-type-parent type)))
    (when parent
      (setq result (nreverse (soap-decode-type parent node))))
    (dolist (element (soap-sequence-type-elements type))
      (let ((instance-count 0)
            (e-name (soap-sequence-element-name element))
            (e-type (soap-sequence-element-type element)))
        (dolist (node (xml-get-children node e-name))
          (incf instance-count)
          (push (cons e-name (soap-decode-type e-type node)) result))
        ;; Do some sanity checking
        (cond ((and (= instance-count 0)
                    (not (soap-sequence-element-nillable? element)))
               (soap-warning "While decoding %s: missing non-nillable slot %s"
                             (soap-element-name type) e-name))
              ((and (> instance-count 1)
                    (not (soap-sequence-element-multiple? element)))
               (soap-warning "While decoding %s: multiple slots named %s"
                             (soap-element-name type) e-name)))))
    (nreverse result)))

(defun soap-decode-array-type (type node)
  "Use TYPE to decode the contents of NODE.
TYPE is assumed to be an array type.  Arrays are decoded as lists.
This is because it is easier to work with list results in LISP."
  (let ((result nil)
        (element-type (soap-array-type-element-type type)))
    (dolist (node (xml-node-children node))
      (when (consp node)
        (push (soap-decode-type element-type node) result)))
    (nreverse result)))

(progn
  (put (aref (make-soap-basic-type) 0)
       'soap-decoder 'soap-decode-basic-type)
  (put (aref (make-soap-sequence-type) 0)
       'soap-decoder 'soap-decode-sequence-type)
  (put (aref (make-soap-array-type) 0)
       'soap-decoder 'soap-decode-array-type))

;;;; Soap Envelope parsing

(put 'soap-error
     'error-conditions
     '(error soap-error))
(put 'soap-error 'error-message "SOAP error")

(defun soap-parse-envelope (node operation wsdl)
  "Parse the SOAP envelope in NODE and return the response.
OPERATION is the WSDL operation for which we expect the response,
WSDL is used to decode the NODE"
  (soap-with-local-xmlns node
    (assert (eq (soap-l2wk (xml-node-name node)) 'soap:Envelope)
            nil
            "soap-parse-envelope: expecting soap:Envelope node, got %s"
            (soap-l2wk (xml-node-name node)))
    (let ((body (car (soap-xml-get-children1 node 'soap:Body))))

      (let ((fault (car (soap-xml-get-children1 body 'soap:Fault))))
        (when fault
          (let ((fault-code (let ((n (car (xml-get-children
					   fault 'faultcode))))
                              (car-safe (xml-node-children n))))
                (fault-string (let ((n (car (xml-get-children
					     fault 'faultstring))))
                                (car-safe (xml-node-children n)))))
          (while t
            (signal 'soap-error (list fault-code fault-string))))))

      ;; First (non string) element of the body is the root node of he
      ;; response
      (let ((response (if (eq (soap-bound-operation-use operation) 'literal)
                          ;; For 'literal uses, the response is the actual body
                          body
                          ;; ...otherwise the first non string element
                          ;; of the body is the response
                          (catch 'found
                            (dolist (n (xml-node-children body))
                              (when (consp n)
                                (throw 'found n)))))))
        (soap-parse-response response operation wsdl body)))))

(defun soap-parse-response (response-node operation wsdl soap-body)
  "Parse RESPONSE-NODE and return the result as a LISP value.
OPERATION is the WSDL operation for which we expect the response,
WSDL is used to decode the NODE.

SOAP-BODY is the body of the SOAP envelope (of which
RESPONSE-NODE is a sub-node).  It is used in case RESPONSE-NODE
reference multiRef parts which are external to RESPONSE-NODE."
  (let* ((soap-current-wsdl wsdl)
         (op (soap-bound-operation-operation operation))
         (use (soap-bound-operation-use operation))
         (message (cdr (soap-operation-output op))))

    (soap-with-local-xmlns response-node

      (when (eq use 'encoded)
        (let* ((received-message-name (soap-l2fq (xml-node-name response-node)))
               (received-message (soap-wsdl-get
				  received-message-name wsdl 'soap-message-p)))
          (unless (eq received-message message)
            (error "Unexpected message: got %s, expecting %s"
                   received-message-name
                   (soap-element-name message)))))

      (let ((decoded-parts nil)
            (soap-multi-refs (xml-get-children soap-body 'multiRef))
            (soap-decoded-multi-refs nil))

        (dolist (part (soap-message-parts message))
          (let ((tag (car part))
                (type (cdr part))
                node)

            (setq node
                  (cond
                    ((eq use 'encoded)
                     (car (xml-get-children response-node tag)))

                    ((eq use 'literal)
                     (catch 'found
                       (let* ((ns-aliases (soap-wsdl-alias-table wsdl))
                              (ns-name (cdr (assoc
					     (soap-element-namespace-tag type)
					     ns-aliases)))
                              (fqname (cons ns-name (soap-element-name type))))
                         (dolist (c (xml-node-children response-node))
                           (when (consp c)
                             (soap-with-local-xmlns c
                               (when (equal (soap-l2fq (xml-node-name c))
					    fqname)
                                 (throw 'found c))))))))))

            (unless node
              (error "Soap-parse-response(%s): cannot find message part %s"
                     (soap-element-name op) tag))
            (push (soap-decode-type type node) decoded-parts)))

        decoded-parts))))

;;;; SOAP type encoding

(defvar soap-encoded-namespaces nil
  "A list of namespace tags used during encoding a message.
This list is populated by `soap-encode-value' and used by
`soap-create-envelope' to add aliases for these namespace to the
XML request.

This variable is dynamically bound in `soap-create-envelope'.")

(defun soap-encode-value (xml-tag value type)
  "Encode inside an XML-TAG the VALUE using TYPE.
The resulting XML data is inserted in the current buffer
at (point)/

TYPE is one of the soap-*-type structures which defines how VALUE
is to be encoded.  This is a generic function which finds an
encoder function based on TYPE and calls that encoder to do the
work."
  (let ((encoder (get (aref type 0) 'soap-encoder)))
    (assert encoder nil "no soap-encoder for %s type" (aref type 0))
    ;; XML-TAG can be a string or a symbol, but we pass only string's to the
    ;; encoders
    (when (symbolp xml-tag)
      (setq xml-tag (symbol-name xml-tag)))
    (funcall encoder xml-tag value type))
  (add-to-list 'soap-encoded-namespaces (soap-element-namespace-tag type)))

(defun soap-encode-basic-type (xml-tag value type)
  "Encode inside XML-TAG the LISP VALUE according to TYPE.
Do not call this function directly, use `soap-encode-value'
instead."
  (let ((xsi-type (soap-element-fq-name type))
        (basic-type (soap-basic-type-kind type)))

    ;; try to classify the type based on the value type and use that type when
    ;; encoding
    (when (eq basic-type 'anyType)
      (cond ((stringp value)
             (setq xsi-type "xsd:string" basic-type 'string))
            ((integerp value)
             (setq xsi-type "xsd:int" basic-type 'int))
            ((memq value '(t nil))
             (setq xsi-type "xsd:boolean" basic-type 'boolean))
            (t
             (error
	      "Soap-encode-basic-type(%s, %s, %s): cannot classify anyType value"
	      xml-tag value xsi-type))))

    (insert "<" xml-tag " xsi:type=\"" xsi-type "\"")

    ;; We have some ambiguity here, as a nil value represents "false" when the
    ;; type is boolean, we will never have a "nil" boolean type...

    (if (or value (eq basic-type 'boolean))
        (progn
          (insert ">")
          (case basic-type
            (string
             (unless (stringp value)
               (error "Soap-encode-basic-type(%s, %s, %s): not a string value"
                      xml-tag value xsi-type))
             (insert (url-insert-entities-in-string value)))

            (dateTime
             (cond ((and (consp value) ; is there a time-value-p ?
                         (>= (length value) 2)
                         (numberp (nth 0 value))
                         (numberp (nth 1 value)))
                    ;; Value is a (current-time) style value, convert
                    ;; to a string
                    (insert (format-time-string "%Y-%m-%dT%H:%M:%S" value)))
                   ((stringp value)
                    (insert (url-insert-entities-in-string value)))
                   (t
                    (error
		     "Soap-encode-basic-type(%s, %s, %s): not a dateTime value"
		     xml-tag value xsi-type))))

            (boolean
             (unless (memq value '(t nil))
               (error "Soap-encode-basic-type(%s, %s, %s): not a boolean value"
                      xml-tag value xsi-type))
             (insert (if value "true" "false")))

            ((long int)
             (unless (integerp value)
               (error "Soap-encode-basic-type(%s, %s, %s): not an integer value"
                      xml-tag value xsi-type))
             (insert (number-to-string value)))

            (base64Binary
             (unless (stringp value)
               (error "Soap-encode-basic-type(%s, %s, %s): not a string value"
                      xml-tag value xsi-type))
             (insert (base64-encode-string value)))

            (otherwise
             (error
	      "Soap-encode-basic-type(%s, %s, %s): don't know how to encode"
	      xml-tag value xsi-type))))

        (insert " xsi:nil=\"true\">"))
    (insert "</" xml-tag ">\n")))

(defun soap-encode-sequence-type (xml-tag value type)
  "Encode inside XML-TAG the LISP VALUE according to TYPE.
Do not call this function directly, use `soap-encode-value'
instead."
  (let ((xsi-type (soap-element-fq-name type)))
    (insert "<" xml-tag " xsi:type=\"" xsi-type "\"")
    (if value
        (progn
          (insert ">\n")
          (let ((parents (list type))
                (parent (soap-sequence-type-parent type)))

            (while parent
              (push parent parents)
              (setq parent (soap-sequence-type-parent parent)))

            (dolist (type parents)
              (dolist (element (soap-sequence-type-elements type))
                (let ((instance-count 0)
                      (e-name (soap-sequence-element-name element))
                      (e-type (soap-sequence-element-type element)))
                  (dolist (v value)
                    (when (equal (car v) e-name)
                      (incf instance-count)
                      (soap-encode-value e-name (cdr v) e-type)))

                  ;; Do some sanity checking
                  (cond ((and (= instance-count 0)
                              (not (soap-sequence-element-nillable? element)))
                         (soap-warning
			  "While encoding %s: missing non-nillable slot %s"
			  (soap-element-name type) e-name))
                        ((and (> instance-count 1)
                              (not (soap-sequence-element-multiple? element)))
                         (soap-warning
			  "While encoding %s: multiple slots named %s"
			  (soap-element-name type) e-name))))))))
        (insert " xsi:nil=\"true\">"))
    (insert "</" xml-tag ">\n")))

(defun soap-encode-array-type (xml-tag value type)
  "Encode inside XML-TAG the LISP VALUE according to TYPE.
Do not call this function directly, use `soap-encode-value'
instead."
  (unless (vectorp value)
    (error "Soap-encode: %s(%s) expects a vector, got: %s"
           xml-tag (soap-element-fq-name type) value))
  (let* ((element-type (soap-array-type-element-type type))
         (array-type (concat (soap-element-fq-name element-type)
                             "[" (format "%s" (length value)) "]")))
    (insert "<" xml-tag
            " soapenc:arrayType=\"" array-type "\" "
            " xsi:type=\"soapenc:Array\">\n")
    (loop for i below (length value)
         do (soap-encode-value xml-tag (aref value i) element-type))
    (insert "</" xml-tag ">\n")))

(progn
  (put (aref (make-soap-basic-type) 0)
       'soap-encoder 'soap-encode-basic-type)
  (put (aref (make-soap-sequence-type) 0)
       'soap-encoder 'soap-encode-sequence-type)
  (put (aref (make-soap-array-type) 0)
       'soap-encoder 'soap-encode-array-type))

(defun soap-encode-body (operation parameters wsdl)
  "Create the body of a SOAP request for OPERATION in the current buffer.
PARAMETERS is a list of parameters supplied to the OPERATION.

The OPERATION and PARAMETERS are encoded according to the WSDL
document."
  (let* ((op (soap-bound-operation-operation operation))
         (use (soap-bound-operation-use operation))
         (message (cdr (soap-operation-input op)))
         (parameter-order (soap-operation-parameter-order op)))

    (unless (= (length parameter-order) (length parameters))
      (error "Wrong number of parameters for %s: expected %d, got %s"
             (soap-element-name op)
             (length parameter-order)
             (length parameters)))

    (insert "<soap:Body>\n")
    (when (eq use 'encoded)
      (add-to-list 'soap-encoded-namespaces (soap-element-namespace-tag op))
      (insert "<" (soap-element-fq-name op) ">\n"))

    (let ((param-table (loop for formal in parameter-order
                          for value in parameters
                          collect (cons formal value))))
      (dolist (part (soap-message-parts message))
        (let* ((param-name (car part))
               (type (cdr part))
               (tag-name (if (eq use 'encoded)
                             param-name
                             (soap-element-name type)))
               (value (cdr (assoc param-name param-table)))
               (start-pos (point)))
          (soap-encode-value tag-name value type)
          (when (eq use 'literal)
            ;; hack: add the xmlns attribute to the tag, the only way
            ;; ASP.NET web services recognize the namespace of the
            ;; element itself...
            (save-excursion
              (goto-char start-pos)
              (when (re-search-forward " ")
                (let* ((ns (soap-element-namespace-tag type))
                       (namespace (cdr (assoc ns
					      (soap-wsdl-alias-table wsdl)))))
                  (when namespace
                    (insert "xmlns=\"" namespace "\" ")))))))))

    (when (eq use 'encoded)
      (insert "</" (soap-element-fq-name op) ">\n"))
    (insert "</soap:Body>\n")))

(defun soap-create-envelope (operation parameters wsdl)
  "Create a SOAP request envelope for OPERATION using PARAMETERS.
WSDL is the wsdl document used to encode the PARAMETERS."
  (with-temp-buffer
    (let ((soap-encoded-namespaces '("xsi" "soap" "soapenc"))
          (use (soap-bound-operation-use operation)))

      ;; Create the request body
      (soap-encode-body operation parameters wsdl)

      ;; Put the envelope around the body
      (goto-char (point-min))
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<soap:Envelope\n")
      (when (eq use 'encoded)
        (insert "    soapenc:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"\n"))
      (dolist (nstag soap-encoded-namespaces)
        (insert "    xmlns:" nstag "=\"")
        (let ((nsname (cdr (assoc nstag soap-well-known-xmlns))))
          (unless nsname
            (setq nsname (cdr (assoc nstag (soap-wsdl-alias-table wsdl)))))
          (insert nsname)
        (insert "\"\n")))
      (insert ">\n")
      (goto-char (point-max))
      (insert "</soap:Envelope>\n"))

    (buffer-string)))

;;;; invoking soap methods

(defcustom soap-debug nil
  "When t, enable some debugging facilities."
  :type 'boolean
  :group 'soap-client)

(defun soap-invoke (wsdl service operation-name &rest parameters)
  "Invoke a SOAP operation and return the result.

WSDL is used for encoding the request and decoding the response.
It also contains information about the WEB server address that
will service the request.

SERVICE is the SOAP service to invoke.

OPERATION-NAME is the operation to invoke.

PARAMETERS -- the remaining parameters are used as parameters for
the SOAP request.

NOTE: The SOAP service provider should document the available
operations and their parameters for the service.  You can also
use the `soap-inspect' function to browse the available
operations in a WSDL document."
  (let ((port (catch 'found
                (dolist (p (soap-wsdl-ports wsdl))
                  (when (equal service (soap-element-name p))
                    (throw 'found p))))))
    (unless port
      (error "Unknown SOAP service: %s" service))

    (let* ((binding (soap-port-binding port))
           (operation (gethash operation-name
			       (soap-binding-operations binding))))
      (unless operation
        (error "No operation %s for SOAP service %s" operation-name service))

      (let ((url-request-method "POST")
            (url-package-name "soap-client.el")
            (url-package-version "1.0")
            (url-http-version "1.0")
            (url-request-data (soap-create-envelope operation parameters wsdl))
            (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
            (url-request-coding-system 'utf-8)
            (url-http-attempt-keepalives t)
            (url-request-extra-headers (list
                                        (cons "SOAPAction"
					      (soap-bound-operation-soap-action
					       operation))
                                        (cons "Content-Type"
					      "text/xml; charset=utf-8"))))
        (let ((buffer (url-retrieve-synchronously
		       (soap-port-service-url port))))
          (condition-case err
              (with-current-buffer buffer
                (declare (special url-http-response-status))
                (if (null url-http-response-status)
                    (error "No HTTP response from server"))
                (if (and soap-debug (> url-http-response-status 299))
                    ;; This is a warning because some SOAP errors come
                    ;; back with a HTTP response 500 (internal server
                    ;; error)
                    (warn "Error in SOAP response: HTTP code %s"
			  url-http-response-status))
                (let ((mime-part (mm-dissect-buffer t t)))
                  (unless mime-part
                    (error "Failed to decode response from server"))
                  (unless (equal (car (mm-handle-type mime-part)) "text/xml")
                    (error "Server response is not an XML document"))
                  (with-temp-buffer
                    (mm-insert-part mime-part)
                    (let ((response (car (xml-parse-region
					  (point-min) (point-max)))))
                      (prog1
                          (soap-parse-envelope response operation wsdl)
                        (kill-buffer buffer)
                        (mm-destroy-part mime-part))))))
            (soap-error
             ;; Propagate soap-errors -- they are error replies of the
             ;; SOAP protocol and don't indicate a communication
             ;; problem or a bug in this code.
             (signal (car err) (cdr err)))
            (error
             (when soap-debug
               (pop-to-buffer buffer))
             (error (error-message-string err)))))))))

(provide 'soap-client)


;;; Local Variables:
;;; eval: (outline-minor-mode 1)
;;; outline-regexp: ";;;;+"
;;; End:

;;; soap-client.el ends here
