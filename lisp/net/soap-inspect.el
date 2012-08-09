;;;; soap-inspect.el -- Interactive inspector for soap WSDL structures

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Alexandru Harsanyi <AlexHarsanyi@gmail.com>
;; Created: October 2010
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
;; This package provides an inspector for a WSDL document loaded with
;; `soap-load-wsdl' or `soap-load-wsdl-from-url'.  To use it, evaluate:
;;
;; (soap-inspect *wsdl*)
;;
;; This will pop-up the inspector buffer.  You can click on ports, operations
;; and types to explore the structure of the wsdl document.
;;


;;; Code:

(eval-when-compile (require 'cl))

(require 'soap-client)

;;; sample-value

(defun soap-sample-value (type)
  "Provide a sample value for TYPE, a WSDL type.
A sample value is a LISP value which soap-client.el will accept
for encoding it using TYPE when making SOAP requests.

This is a generic function, depending on TYPE a specific function
will be called."
  (let ((sample-value (get (aref type 0) 'soap-sample-value)))
    (if sample-value
        (funcall sample-value type)
        (error "Cannot provide sample value for type %s" (aref type 0)))))

(defun soap-sample-value-for-basic-type (type)
  "Provide a sample value for TYPE which is a basic type.
This is a specific function which should not be called directly,
use `soap-sample-value' instead."
  (case (soap-basic-type-kind type)
    (string "a string value")
    (boolean t)                         ; could be nil as well
    ((long int) (random 4200))
    ;; TODO: we need better sample values for more types.
    (t (format "%s" (soap-basic-type-kind type)))))

(defun soap-sample-value-for-seqence-type (type)
  "Provide a sample value for TYPE which is a sequence type.
Values for sequence types are ALISTS of (slot-name . VALUE) for
each sequence element.

This is a specific function which should not be called directly,
use `soap-sample-value' instead."
  (let ((sample-value nil))
    (dolist (element (soap-sequence-type-elements type))
      (push (cons (soap-sequence-element-name element)
                  (soap-sample-value (soap-sequence-element-type element)))
            sample-value))
    (when (soap-sequence-type-parent type)
      (setq sample-value
            (append (soap-sample-value (soap-sequence-type-parent type))
                    sample-value)))
    sample-value))

(defun soap-sample-value-for-array-type (type)
  "Provide a sample value for TYPE which is an array type.
Values for array types are LISP vectors of values which are
array's element type.

This is a specific function which should not be called directly,
use `soap-sample-value' instead."
  (let* ((element-type (soap-array-type-element-type type))
         (sample1 (soap-sample-value element-type))
         (sample2 (soap-sample-value element-type)))
    ;; Our sample value is a vector of two elements, but any number of
    ;; elements are permissible
    (vector sample1 sample2 '&etc)))

(defun soap-sample-value-for-message (message)
  "Provide a sample value for a WSDL MESSAGE.
This is a specific function which should not be called directly,
use `soap-sample-value' instead."
  ;; NOTE: parameter order is not considered.
  (let (sample-value)
    (dolist (part (soap-message-parts message))
      (push (cons (car part)
                  (soap-sample-value (cdr part)))
            sample-value))
    (nreverse sample-value)))

(progn
  ;; Install soap-sample-value methods for our types
  (put (aref (make-soap-basic-type) 0) 'soap-sample-value
       'soap-sample-value-for-basic-type)

  (put (aref (make-soap-sequence-type) 0) 'soap-sample-value
       'soap-sample-value-for-seqence-type)

  (put (aref (make-soap-array-type) 0) 'soap-sample-value
       'soap-sample-value-for-array-type)

  (put (aref (make-soap-message) 0) 'soap-sample-value
       'soap-sample-value-for-message) )



;;; soap-inspect

(defvar soap-inspect-previous-items nil
  "A stack of previously inspected items in the *soap-inspect* buffer.
Used to implement the BACK button.")

(defvar soap-inspect-current-item nil
  "The current item being inspected in the *soap-inspect* buffer.")

(progn
  (make-variable-buffer-local 'soap-inspect-previous-items)
  (make-variable-buffer-local 'soap-inspect-current-item))

(defun soap-inspect (element)
  "Inspect a SOAP ELEMENT in the *soap-inspect* buffer.
The buffer is populated with information about ELEMENT with links
to its sub elements.  If ELEMENT is the WSDL document itself, the
entire WSDL can be inspected."
  (let ((inspect (get (aref element 0) 'soap-inspect)))
    (unless inspect
      (error "Soap-inspect: no inspector for element"))

    (with-current-buffer (get-buffer-create "*soap-inspect*")
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)

        (when soap-inspect-current-item
          (push soap-inspect-current-item
                soap-inspect-previous-items))
        (setq soap-inspect-current-item element)

        (funcall inspect element)

        (unless (null soap-inspect-previous-items)
          (insert "\n\n")
          (insert-text-button
           "[back]"
           'type 'soap-client-describe-back-link
           'item element)
          (insert "\n"))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))


(define-button-type 'soap-client-describe-link
    'face 'italic
    'help-echo "mouse-2, RET: describe item"
    'follow-link t
    'action (lambda (button)
              (let ((item (button-get button 'item)))
                (soap-inspect item)))
    'skip t)

(define-button-type 'soap-client-describe-back-link
    'face 'italic
    'help-echo "mouse-2, RET: browse the previous item"
    'follow-link t
    'action (lambda (button)
              (let ((item (pop soap-inspect-previous-items)))
                (when item
                  (setq soap-inspect-current-item nil)
                  (soap-inspect item))))
    'skip t)

(defun soap-insert-describe-button (element)
  "Insert a button to inspect ELEMENT when pressed."
  (insert-text-button
   (soap-element-fq-name element)
   'type 'soap-client-describe-link
   'item element))

(defun soap-inspect-basic-type (basic-type)
  "Insert information about BASIC-TYPE into the current buffer."
  (insert "Basic type: " (soap-element-fq-name basic-type))
  (insert "\nSample value\n")
  (pp (soap-sample-value basic-type) (current-buffer)))

(defun soap-inspect-sequence-type (sequence)
  "Insert information about SEQUENCE into the current buffer."
  (insert "Sequence type: " (soap-element-fq-name sequence) "\n")
  (when (soap-sequence-type-parent sequence)
    (insert "Parent: ")
    (soap-insert-describe-button
     (soap-sequence-type-parent sequence))
    (insert "\n"))
  (insert "Elements: \n")
  (dolist (element (soap-sequence-type-elements sequence))
    (insert "\t" (symbol-name (soap-sequence-element-name element))
            "\t")
    (soap-insert-describe-button
     (soap-sequence-element-type element))
    (when (soap-sequence-element-multiple? element)
      (insert " multiple"))
    (when (soap-sequence-element-nillable? element)
      (insert " optional"))
    (insert "\n"))
  (insert "Sample value:\n")
  (pp (soap-sample-value sequence) (current-buffer)))

(defun soap-inspect-array-type (array)
  "Insert information about the ARRAY into the current buffer."
  (insert "Array name: " (soap-element-fq-name array) "\n")
  (insert "Element type: ")
  (soap-insert-describe-button
   (soap-array-type-element-type array))
  (insert "\nSample value:\n")
  (pp (soap-sample-value array) (current-buffer)))

(defun soap-inspect-message (message)
  "Insert information about MESSAGE into the current buffer."
  (insert "Message name: " (soap-element-fq-name message) "\n")
  (insert "Parts:\n")
  (dolist (part (soap-message-parts message))
    (insert "\t" (symbol-name (car part))
            " type: ")
    (soap-insert-describe-button (cdr part))
    (insert "\n")))

(defun soap-inspect-operation (operation)
  "Insert information about OPERATION into the current buffer."
  (insert "Operation name: " (soap-element-fq-name operation) "\n")
  (let ((input (soap-operation-input operation)))
    (insert "\tInput: " (symbol-name (car input)) " (" )
    (soap-insert-describe-button (cdr input))
    (insert ")\n"))
  (let ((output (soap-operation-output operation)))
    (insert "\tOutput: " (symbol-name (car output)) " (")
    (soap-insert-describe-button (cdr output))
    (insert ")\n"))

  (insert "\n\nSample invocation:\n")
  (let ((sample-message-value
	 (soap-sample-value (cdr (soap-operation-input operation))))
        (funcall (list 'soap-invoke '*WSDL* "SomeService" (soap-element-name operation))))
    (let ((sample-invocation
	   (append funcall (mapcar 'cdr sample-message-value))))
      (pp sample-invocation (current-buffer)))))

(defun soap-inspect-port-type (port-type)
  "Insert information about PORT-TYPE into the current buffer."
  (insert "Port-type name: " (soap-element-fq-name port-type) "\n")
  (insert "Operations:\n")
  (loop for o being the hash-values of
       (soap-namespace-elements (soap-port-type-operations port-type))
       do (progn
            (insert "\t")
            (soap-insert-describe-button (car o)))))

(defun soap-inspect-binding (binding)
  "Insert information about BINDING into the current buffer."
  (insert "Binding: " (soap-element-fq-name binding) "\n")
  (insert "\n")
  (insert "Bound operations:\n")
  (let* ((ophash (soap-binding-operations binding))
         (operations (loop for o being the hash-keys of ophash
                        collect o))
         op-name-width)

    (setq operations (sort operations 'string<))

    (setq op-name-width (loop for o in operations maximizing (length o)))

    (dolist (op operations)
      (let* ((bound-op (gethash op ophash))
             (soap-action (soap-bound-operation-soap-action bound-op))
             (use (soap-bound-operation-use bound-op)))
        (unless soap-action
          (setq soap-action ""))
        (insert "\t")
        (soap-insert-describe-button (soap-bound-operation-operation bound-op))
        (when (or use (not (equal soap-action "")))
          (insert (make-string (- op-name-width (length op)) ?\s))
          (insert " (")
          (insert soap-action)
          (when use
            (insert " " (symbol-name use)))
          (insert ")"))
        (insert "\n")))))

(defun soap-inspect-port (port)
  "Insert information about PORT into the current buffer."
  (insert "Port name:   " (soap-element-name port) "\n"
          "Service URL: " (soap-port-service-url port) "\n"
          "Binding:     ")
  (soap-insert-describe-button (soap-port-binding port)))

(defun soap-inspect-wsdl (wsdl)
  "Insert information about WSDL into the current buffer."
  (insert "WSDL Origin: " (soap-wsdl-origin wsdl) "\n")
  (insert "Ports:")
  (dolist (p (soap-wsdl-ports wsdl))
    (insert "\n--------------------\n")
    ;; (soap-insert-describe-button p)
    (soap-inspect-port p))
  (insert "\n--------------------\nNamespace alias table:\n")
  (dolist (a (soap-wsdl-alias-table wsdl))
    (insert "\t" (car a) " => " (cdr a) "\n")))

(progn
  ;; Install the soap-inspect methods for our types

  (put (aref (make-soap-basic-type) 0) 'soap-inspect
       'soap-inspect-basic-type)

  (put (aref (make-soap-sequence-type) 0) 'soap-inspect
       'soap-inspect-sequence-type)

  (put (aref (make-soap-array-type) 0) 'soap-inspect
       'soap-inspect-array-type)

  (put (aref (make-soap-message) 0) 'soap-inspect
       'soap-inspect-message)
  (put (aref (make-soap-operation) 0) 'soap-inspect
       'soap-inspect-operation)

  (put (aref (make-soap-port-type) 0) 'soap-inspect
       'soap-inspect-port-type)

  (put (aref (make-soap-binding) 0) 'soap-inspect
       'soap-inspect-binding)

  (put (aref (make-soap-port) 0) 'soap-inspect
       'soap-inspect-port)

  (put (aref (make-soap-wsdl) 0) 'soap-inspect
       'soap-inspect-wsdl))

(provide 'soap-inspect)
;;; soap-inspect.el ends here
