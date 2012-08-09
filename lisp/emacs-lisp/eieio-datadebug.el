;;; eieio-datadebug.el --- EIEIO extensions to the data debugger.

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: OO, lisp
;; Package: eieio

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
;; Extensions to data-debug for EIEIO objects.
;;

(require 'eieio)
(require 'data-debug)

;;; Code:

(defun data-debug-insert-object-slots (object prefix)
  "Insert all the slots of OBJECT.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "] ")))
    (data-debug/eieio-insert-slots object attrprefix)))

(defun data-debug-insert-object-slots-from-point (point)
  "Insert the object slots found at the object button at POINT."
  (let ((object (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-object-slots object
				    (concat (make-string indent ? )
					    "~ "))
    (goto-char start)))

(defun data-debug-insert-object-button (object prefix prebuttontext)
  "Insert a button representing OBJECT.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between PREFIX and the object button."
  (let ((start (point))
	(end nil)
	(str (object-print object))
	(tip (format "Object %s\nClass: %S\nParent(s): %S\n%d slots"
		     (object-name-string object)
		     (object-class object)
		     (class-parents (object-class object))
		     (length (object-slots object))
		     ))
	)
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-keyword-face)
    (put-text-property start end 'ddebug object)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-object-slots-from-point)
    (insert "\n")))

;;; METHODS
;;
;; Each object should have an opportunity to show stuff about itself.

(defmethod data-debug/eieio-insert-slots ((obj eieio-default-superclass)
						prefix)
  "Insert the slots of OBJ into the current DDEBUG buffer."
  (data-debug-insert-thing (object-name-string obj)
				prefix
				"Name: ")
  (let* ((cl (object-class obj))
	 (cv (class-v cl)))
    (data-debug-insert-thing (class-constructor cl)
				  prefix
				  "Class: ")
    ;; Loop over all the public slots
    (let ((publa (aref cv class-public-a))
	  (publd (aref cv class-public-d))
	  )
      (while publa
	(if (slot-boundp obj (car publa))
	    (let ((i (class-slot-initarg cl (car publa)))
		  (v (eieio-oref obj (car publa))))
	      (data-debug-insert-thing
	       v prefix (concat
			 (if i (symbol-name i)
			   (symbol-name (car publa)))
			 " ")))
	  ;; Unbound case
	  (let ((i (class-slot-initarg cl (car publa))))
	    (data-debug-insert-custom
	     "#unbound" prefix
	     (concat (if i (symbol-name i)
		       (symbol-name (car publa)))
		     " ")
	     'font-lock-keyword-face))
	  )
	(setq publa (cdr publa) publd (cdr publd))))))

;;; Augment the Data debug thing display list.
(data-debug-add-specialized-thing (lambda (thing) (object-p thing))
				  #'data-debug-insert-object-button)

;;; DEBUG METHODS
;;
;; A generic function to run DDEBUG on an object and popup a new buffer.
;;
(defmethod data-debug-show ((obj eieio-default-superclass))
  "Run ddebug against any EIEIO object OBJ."
  (data-debug-new-buffer (format "*%s DDEBUG*" (object-name obj)))
  (data-debug-insert-object-slots obj "]"))

;;; DEBUG FUNCTIONS
;;
(defun eieio-debug-methodinvoke (method class)
  "Show the method invocation order for METHOD with CLASS object."
  (interactive "aMethod: \nXClass Expression: ")
  (let* ((eieio-pre-method-execution-hooks
	  (lambda (l) (throw 'moose l) ))
	 (data
	  (catch 'moose (eieio-generic-call
			 method (list class))))
	 (buf (data-debug-new-buffer "*Method Invocation*"))
	 (data2 (mapcar (lambda (sym)
			  (symbol-function (car sym)))
			  data)))
    (data-debug-insert-thing data2 ">" "")))

(provide 'eieio-datadebug)

;;; eieio-datadebug.el ends here
