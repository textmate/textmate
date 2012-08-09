;;; romanian.el --- support for Romanian -*- coding: iso-latin-2; no-byte-compile: t -*-

;; Copyright (C) 1998, 2001-2012  Free Software Foundation, Inc.

;; Author:    Dan Nicolaescu <done@ece.arizona.edu>
;; Keywords: multilingual, Romanian, i18n

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

;; Romanian ISO 8859-2 environment plus 8859-16 coding system.

;;; Code:

(set-language-info-alist
 "Romanian" '((charset iso-8859-2)
	      (coding-system iso-8859-2 iso-latin-10)
	      (coding-priority iso-8859-2)
	      (nonascii-translation . iso-8859-2)
	      (input-method . "latin-2-postfix")
	      (unibyte-display . iso-8859-2)
	      (tutorial . "TUTORIAL.ro")
	      (sample-text . "Bunã ziua, bine aþi venit!")
	      (documentation . "Romanian environment using Latin-2 encoding.
An environment for generic Latin-10 encoding is also available."))
 '("European"))

(define-coding-system 'iso-latin-10
  "ISO 2022 based 8-bit encoding for Latin-10."
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(iso-8859-16)
  :mime-charset 'iso-8859-16)

(define-coding-system-alias 'iso-8859-16 'iso-latin-10)
(define-coding-system-alias 'latin-10 'iso-latin-10)

(provide 'romanian)

;;; romanian.el ends here
