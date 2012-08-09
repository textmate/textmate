;;; rng-dt.el --- datatype library interface for RELAX NG

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

;;; Code:

(require 'rng-util)

(defvar rng-dt-error-reporter nil)

(defun rng-dt-error (string &rest objs)
  (if rng-dt-error-reporter
      (apply rng-dt-error-reporter (cons string objs))
    nil))

(defvar rng-dt-namespace-context-getter nil
  "A list used by datatype libraries to expand names.
The car of the list is a symbol which is the name of a function.
This function is applied to the cdr of the list.  The function must
return a list whose car is the default namespace and whose cdr is an
alist of (PREFIX . NAMESPACE) pairs, where PREFIX is a string and
NAMESPACE is a symbol.  This must be dynamically bound before calling
a datatype library.")

(defsubst rng-dt-make-value (dt str)
  (apply (car dt) (cons str (cdr dt))))

(defun rng-dt-builtin-compile (name params)
  (cond ((eq name 'string)
	 (if (null params)
	     '(t identity)
	   (rng-dt-error "The string datatype does not take any parameters")))
	((eq name 'token)
	 (if (null params)
	     '(t rng-collapse-space)
	   (rng-dt-error "The token datatype does not take any parameters")))
	(t
	 (rng-dt-error "There is no built-in datatype %s" name))))

(put (rng-make-datatypes-uri "") 'rng-dt-compile 'rng-dt-builtin-compile)

(provide 'rng-dt)

;;; rng-dt.el ends here
