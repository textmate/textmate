;;; korean.el --- support for Korean -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Korean

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

;; For Korean, the character set KSC5601 is supported.

;;; Code:

(define-coding-system 'korean-iso-8bit
  "ISO 2022 based EUC encoding for Korean KSC5601 (MIME:EUC-KR)."
  :coding-type 'iso-2022
  :mnemonic ?K
  :designation [ascii korean-ksc5601 nil nil]
  :charset-list '(ascii korean-ksc5601)
  :mime-charset 'euc-kr)

(define-coding-system-alias 'euc-kr 'korean-iso-8bit)
(define-coding-system-alias 'euc-korea 'korean-iso-8bit)

(define-coding-system 'iso-2022-kr
  "ISO 2022 based 7-bit encoding for Korean KSC5601 (MIME:ISO-2022-KR)."
  :coding-type 'iso-2022
  :mnemonic ?k
  :designation [ascii (nil korean-ksc5601) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl 7-bit designation locking-shift
			designation-bol)
  :charset-list '(ascii korean-ksc5601)
  :mime-charset 'iso-2022-kr
  :suitable-for-keyboard t)

(define-coding-system-alias 'korean-iso-7bit-lock 'iso-2022-kr)

(define-coding-system 'korean-cp949
  "CP949 (Microsoft Unified Hangul Code)"
  :coding-type 'charset
  :mnemonic ?K
  :charset-list '(ascii cp949))

(define-coding-system-alias 'cp949 'korean-cp949)

(set-language-info-alist
 "Korean" '((setup-function . setup-korean-environment-internal)
	    (exit-function . exit-korean-environment)
	    (iso639-language . ko)
	    (tutorial . "TUTORIAL.ko")
	    (charset korean-ksc5601 cp949)
	    (coding-system iso-2022-kr korean-iso-8bit korean-cp949)
	    (input-method . "korean-hangul")
	    (features korea-util)
	    (coding-priority korean-iso-8bit korean-cp949 iso-2022-kr)
	    (sample-text . "Hangul (한글)	안녕하세요, 안녕하십니까")
	    (documentation . "\
The following key bindings are available for controlling Korean input methods:
  Shift-SPC, Hangul:	toggle-korean-input-method
  Control-F9:		quail-hangul-switch-symbol-ksc
  F9:			quail-hangul-switch-hanja
and the following key bindings are available within Korean input methods:
  F9, Hangul_Hanja:	hangul-to-hanja-conversion")
	    ))

(provide 'korean)

;;; korean.el ends here
