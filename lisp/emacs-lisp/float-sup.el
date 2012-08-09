;;; float-sup.el --- define some constants useful for floating point numbers.

;; Copyright (C) 1985-1987, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

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

;; Provide an easy hook to tell if we are running with floats or not.
;; Define pi and e via math-lib calls (much less prone to killer typos).
(defconst float-pi (* 4 (atan 1)) "The value of Pi (3.1415926...).")
(progn
  ;; Simulate a defconst that doesn't declare the variable dynamically bound.
  (setq-default pi float-pi)
  (put 'pi 'variable-documentation
       "Obsolete since Emacs-23.3.  Use `float-pi' instead.")
  (put 'pi 'risky-local-variable t)
  (push 'pi current-load-list))

(defconst float-e (exp 1) "The value of e (2.7182818...).")

(defconst degrees-to-radians (/ float-pi 180.0)
  "Degrees to radian conversion constant.")
(defconst radians-to-degrees (/ 180.0 float-pi)
  "Radian to degree conversion constant.")

;; These expand to a single multiply by a float when byte compiled.

(defmacro degrees-to-radians (x)
  "Convert X from degrees to radians."
  (list '* degrees-to-radians x))
(defmacro radians-to-degrees (x)
  "Convert X from radians to degrees."
  (list '* radians-to-degrees x))

(provide 'lisp-float-type)

;;; float-sup.el ends here
