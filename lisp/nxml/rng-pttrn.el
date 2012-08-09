;;; rng-pttrn.el --- RELAX NG patterns

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

;; pattern ::=
;;   (ref <pattern> <local-name>)
;;   | (choice <pattern> <pattern> ...)
;;   | (group <pattern> <pattern> ...)
;;   | (interleave <pattern> <pattern> ...)
;;   | (zero-or-more <pattern>)
;;   | (one-or-more <pattern>)
;;   | (optional <pattern>)
;;   | (mixed <pattern>)
;;   | (value <datatype> <string> <context>)
;;   | (data <datatype> <params>)
;;   | (data-except <datatype> <params> <pattern>)
;;   | (list <pattern>)
;;   | (element <name-class> <pattern>)
;;   | (attribute <name-class> <pattern>)
;;   | (text)
;;   | (empty)
;;   | (not-allowed)
;;
;; params ::=
;;   ((<param-name> . <param-value> ) ...)
;; param-name ::= <symbol>
;; param-value ::= <string>
;;
;; name-class ::=
;;   (name <name>)
;;   | (any-name)
;;   | (any-name-except <name-class>)
;;   | (ns-name <ns>)
;;   | (ns-name-except <ns> <name-class>)
;;   | (choice <name-class> <name-class> ...)
;;
;; name ::= (<ns> . <local-name>)
;; ns ::= nil | <symbol>
;; local-name ::= <string>
;; datatype ::= (<datatype-uri> . <datatype-local-name>)
;; datatype-uri ::= nil | <symbol>
;; datatype-local-name ::= <symbol>

;;; Code:

(defvar rng-schema-change-hook nil
  "Hook to be run after `rng-current-schema' changes.")

(defvar rng-current-schema nil
  "Pattern to be used as schema for the current buffer.")
(make-variable-buffer-local 'rng-current-schema)

(defun rng-make-ref (name)
  (list 'ref nil name))

(defun rng-ref-set (ref pattern)
  (setcar (cdr ref) pattern))

(defun rng-ref-get (ref) (cadr ref))

(defun rng-make-choice (patterns)
  (cons 'choice patterns))

(defun rng-make-group (patterns)
  (cons 'group patterns))

(defun rng-make-interleave (patterns)
  (cons 'interleave patterns))

(defun rng-make-zero-or-more (pattern)
  (list 'zero-or-more pattern))

(defun rng-make-one-or-more (pattern)
  (list 'one-or-more pattern))

(defun rng-make-optional (pattern)
  (list 'optional pattern))

(defun rng-make-mixed (pattern)
  (list 'mixed pattern))

(defun rng-make-value (datatype str context)
  (list 'value datatype str context))

(defun rng-make-data (name params)
  (list 'data name params))

(defun rng-make-data-except (name params pattern)
  (list 'data-except name params pattern))

(defun rng-make-list (pattern)
  (list 'list pattern))

(defun rng-make-element (name-class pattern)
  (list 'element name-class pattern))

(defun rng-make-attribute (name-class pattern)
  (list 'attribute name-class pattern))

(defun rng-make-text ()
  '(text))

(defun rng-make-empty ()
  '(empty))

(defun rng-make-not-allowed ()
  '(not-allowed))

(defun rng-make-any-name-name-class ()
  '(any-name))

(defun rng-make-any-name-except-name-class (name-class)
  (list 'any-name-except name-class))

(defun rng-make-ns-name-name-class (ns)
  (list 'ns-name ns))

(defun rng-make-ns-name-except-name-class (ns name-class)
  (list 'ns-name-except ns name-class))

(defun rng-make-name-name-class (name)
  (list 'name name))

(defun rng-make-choice-name-class (name-classes)
  (cons 'choice name-classes))

(defconst rng-any-content
  (let* ((ref (rng-make-ref "any-content"))
	 (pattern (rng-make-zero-or-more
		    (rng-make-choice
		     (list
		      (rng-make-text)
		      (rng-make-attribute (rng-make-any-name-name-class)
					  (rng-make-text))
		      (rng-make-element (rng-make-any-name-name-class)
					ref))))))
    (rng-ref-set ref pattern)
    pattern)
  "A pattern that matches the attributes and content of any element.")

(defconst rng-any-element
  (let* ((ref (rng-make-ref "any-element"))
	 (pattern
	  (rng-make-element
	   (rng-make-any-name-name-class)
	   (rng-make-zero-or-more
	    (rng-make-choice
	     (list
	      (rng-make-text)
	      (rng-make-attribute (rng-make-any-name-name-class)
				  (rng-make-text))
	      ref))))))
    (rng-ref-set ref pattern)
    pattern)
  "A pattern that matches any element.")

;;; Names

(defun rng-make-name (ns local-name)
  (cons ns local-name))

;;; Datatypes

(defun rng-make-datatype (uri local-name)
  (cons uri (intern local-name)))

(provide 'rng-pttrn)

;;; rng-pttrn.el ends here
