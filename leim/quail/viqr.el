;;; viqr.el --- Quail packages for inputting Vietnamese with VIQR system  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, latin

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

(require 'quail)
(require 'viet-util)

;; `viet-viqr-alist' is an alist of Vietnamese characters vs
;; corresponding VIQR strings.  We create Quail map which maps VIQR
;; strings to corresponding Vietnamese characters.

(defmacro viet-quail-define-rules ()
  (cons 'quail-define-rules
	(let ((l viet-viqr-alist)
	      rules)
	  (while l
	    (setq rules (cons (list (cdr (car l)) (car (car l))) rules))
	    (setq l (cdr l)))
	  rules)))

(quail-define-package
 "vietnamese-viqr" "Vietnamese" "VQ" t
 "Vietnamese input method with VIQR mnemonic system

    effect   | postfix | examples
 ------------+---------+----------
    breve    |    (    | a( -> ,1e(B
  circumflex |    ^    | a^ -> ,1b(B
    horn     |    +    | o+ -> ,1=(B
 ------------+---------+----------
    acute    |    '    | a' -> ,1a(B
    grave    |    `    | a` -> ,1`(B
  hook above |    ?    | a? -> ,1d(B
    tilde    |    ~    | a~ -> ,1c(B
   dot below |    .    | a. -> ,1U(B
 ------------+---------+----------
    d bar    |   dd    | dd -> ,1p(B
 ------------+---------+----------
  no compose |    \\    | a\\. -> a.
 ------------+---------+----------
  combination|   (~    | a(~ -> ,1G(B
" nil t t nil nil t nil nil nil nil t)


(viet-quail-define-rules)

;;; viqr.el ends here
