;;; nxml-ns.el --- XML namespace processing

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

;; This file uses a prefix of `nxml-ns'.

;;; Code:

(require 'nxml-util)

(defvar nxml-ns-state nil
  "Contains the state of namespace processing.
The state is never modified destructively and so can be saved and
restored without copying.

The value is a stack represented by a list.  The list has length
N + 1 where N is the number of open elements.  Each member of the
list represents the bindings in effect for a particular element.
Each member is itself a list whose car is the default namespace
\(a symbol or nil) and whose cdr is an alist of (PREFIX . NS) pairs
where PREFIX is a string (never nil) and NS is the namespace URI
symbol.")

(defconst nxml-ns-initial-state
  (list (list nil (cons "xml" nxml-xml-namespace-uri)))
  "A list to be used as the initial value of `nxml-ns-state'.
This represents the state with no open elements and with the default
namespace bindings (no default namespace and only the xml prefix bound).")

(defsubst nxml-ns-state () nxml-ns-state)

(defsubst nxml-ns-set-state (state)
  (setq nxml-ns-state state))

(defsubst nxml-ns-state-equal (state)
  (equal nxml-ns-state state))

(defmacro nxml-ns-save (&rest body)
  `(let ((nxml-ns-state nxml-ns-initial-state))
     ,@body))

(put 'nxml-ns-save 'lisp-indent-function 0)
(def-edebug-spec nxml-ns-save t)

(defun nxml-ns-init ()
  (setq nxml-ns-state nxml-ns-initial-state))

(defun nxml-ns-push-state ()
  "Change the state by starting a new element.
Namespace declarations are inherited from the parent state."
  (setq nxml-ns-state (cons (car nxml-ns-state) nxml-ns-state)))

(defun nxml-ns-pop-state ()
  "Change the state by ending an element.
The behavior is undefined if there is no open element."
  (setq nxml-ns-state (cdr nxml-ns-state)))

(defun nxml-ns-get-prefix (prefix)
  "Return the symbol for namespace bound to PREFIX.
Return nil if PREFIX is unbound.  PREFIX is a string, never nil."
  (let ((binding (assoc prefix (cdar nxml-ns-state))))
    (and binding (cdr binding))))

(defun nxml-ns-set-prefix (prefix ns)
  "Change the binding of PREFIX.
PREFIX is a string (never nil).  NS is a symbol (never nil).
The change will be in effect until the end of the current element."
  (setq nxml-ns-state
	(let ((bindings (car nxml-ns-state)))
	  (cons (cons (car bindings)
		      (cons (cons prefix ns) (cdr bindings)))
		(cdr nxml-ns-state)))))

(defun nxml-ns-get-default ()
  "Return the current default namespace as a symbol.
Return nil if there is no default namespace."
  (caar nxml-ns-state))

(defun nxml-ns-set-default (ns)
  "Changes the current default namespace.
The change will be in effect until the end of the current element.
NS is a symbol or nil."
  (setq nxml-ns-state
	(cons (cons ns (cdar nxml-ns-state))
	      (cdr nxml-ns-state))))

(defun nxml-ns-get-context ()
  (car nxml-ns-state))

(defun nxml-ns-prefixes-for (ns &optional attributep)
  (let ((current (car nxml-ns-state))
	prefixes)
    (when (if attributep
	      (not ns)
	    (eq (car current) ns))
      (setq prefixes '(nil)))
    (setq current (cdr current))
    (while (let ((binding (rassq ns current)))
	     (when binding
	       (when (eq (nxml-ns-get-prefix (car binding)) ns)
		 (add-to-list 'prefixes
			      (car binding)))
	       (setq current
		     (cdr (member binding current))))))
    prefixes))

(defun nxml-ns-prefix-for (ns)
  (car (rassq ns (cdar nxml-ns-state))))

(defun nxml-ns-changed-prefixes ()
  (let ((old (cadr nxml-ns-state))
	(new (car nxml-ns-state))
	changed)
    (if (eq old new)
	nil
      (unless (eq (car new) (car old))
	(setq changed '(nil)))
      (setq new (cdr new))
      (setq old (cdr old))
      (while (not (eq new old))
	(setq changed
	      (cons (caar new) changed))
	(setq new (cdr new))))
    changed))

(provide 'nxml-ns)

;;; nxml-ns.el ends here
