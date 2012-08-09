;;; english.el --- support for English -*- no-byte-compile: t -*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multibyte character, character set, syntax, category

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

;; We need nothing special to support English on Emacs.  Selecting
;; English as a language environment is one of the ways to reset
;; various multilingual environment to the original setting.

;;; Code:

(set-language-info-alist
 "English" '((tutorial . "TUTORIAL")
	     (charset ascii)
	     (sample-text . "Hello!, Hi!, How are you?")
	     (documentation . "\
Nothing special is needed to handle English.")
	     ))

;; Mostly because we can now...
(define-coding-system 'ebcdic-us
  "US version of EBCDIC"
  :coding-type 'charset
  :charset-list '(ebcdic-us)
  :mnemonic ?*)

(define-coding-system 'ebcdic-uk
  "UK version of EBCDIC"
  :coding-type 'charset
  :charset-list '(ebcdic-uk)
  :mnemonic ?*)

(define-coding-system 'ibm1047
  "A version of EBCDIC used in OS/390 Unix"  ; says Groff
  :coding-type 'charset
  :charset-list '(ibm1047)
  :mnemonic ?*)
(define-coding-system-alias 'cp1047 'ibm1047)

;; Make "ASCII" an alias of "English" language environment.
(set-language-info-alist
 "ASCII" (cdr (assoc "English" language-info-alist)))

;;; english.el ends here
