;;; greek.el --- support for Greek -*- no-byte-compile: t -*-

;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Greek

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

;; For Greek, the character set ISO8859-7 is supported.

;;; Code:

(define-coding-system 'greek-iso-8bit
  "ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7)."
  :coding-type 'charset
  :mnemonic ?7
  :charset-list '(iso-8859-7)
  :mime-charset 'iso-8859-7)

(define-coding-system-alias 'iso-8859-7 'greek-iso-8bit)

(define-coding-system 'windows-1253
  "windows-1253 encoding for Greek"
  :coding-type 'charset
  :mnemonic ?g
  :charset-list '(windows-1253)
  :mime-charset 'windows-1253)
(define-coding-system-alias 'cp1253 'windows-1253)

(define-coding-system 'cp737
  "Codepage 737 (PC Greek)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp737)
  :mime-charset 'cp737)

(define-coding-system 'cp851
  "DOS codepage 851 (Greek)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp851)
  :mime-charset 'cp851)
(define-coding-system-alias 'ibm851 'cp851)

(define-coding-system 'cp869
  "DOS codepage 869 (Greek)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp869)
  :mime-charset 'cp869)
(define-coding-system-alias 'ibm869 'cp869)

(set-language-info-alist
 "Greek" '((charset iso-8859-7)
	   (coding-system greek-iso-8bit windows-1253 cp851 cp869)
	   (coding-priority greek-iso-8bit)
	   (nonascii-translation . iso-8859-7)
	   (input-method . "greek")
	   (documentation . t)))

(provide 'greek)

;;; greek.el ends here
