;;; japanese.el --- support for Japanese -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Japanese

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

;; For Japanese, character sets JISX0201, JISX0208, JISX0212 are
;; supported.

;;; Code:

;;; Load translation tables for CP932.
(load "international/cp51932")
(load "international/eucjp-ms")

(define-coding-system 'iso-2022-jp
  "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)."
  :coding-type 'iso-2022
  :mnemonic ?J
  :designation [(ascii japanese-jisx0208-1978 japanese-jisx0208
		       latin-jisx0201)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation)
  :charset-list '(ascii japanese-jisx0208
			japanese-jisx0208-1978 latin-jisx0201)
  :mime-charset 'iso-2022-jp
  :suitable-for-keyboard t)

(define-coding-system-alias 'junet 'iso-2022-jp)

(define-coding-system 'iso-2022-jp-2
  "ISO 2022 based 7bit encoding for CJK, Latin-1, Greek (MIME:ISO-2022-JP-2)."
  :coding-type 'iso-2022
  :mnemonic ?J
  :designation [(ascii japanese-jisx0208-1978 japanese-jisx0208
		       latin-jisx0201 japanese-jisx0212
		       chinese-gb2312 korean-ksc5601)
		nil
		(nil latin-iso8859-1 greek-iso8859-7)
		nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation single-shift
		 init-at-bol)
  :charset-list '(ascii japanese-jisx0208 japanese-jisx0212
			latin-jisx0201 japanese-jisx0208-1978
			chinese-gb2312 korean-ksc5601
			latin-iso8859-1 greek-iso8859-7)
  :mime-charset 'iso-2022-jp-2
  :suitable-for-keyboard t)

(let ((map			; JIS		vs	CP932
       '((#x301C . #xFF5E)	; WAVE DASH		FULLWIDTH TILDE
	 (#x2014 . #x2015)	; EM DASH		HORIZONTAL BAR
	 (#x2016 . #x2225)	; DOUBLE VERTICAL LINE	PARALLEL TO
	 (#x2212 . #xFF0D)	; MINUS SIGN		FULLWIDTH HYPHEN-MINUS
	 (#x00A2 . #xFFE0)	; CENT SIGN		FULLWIDTH CENT SIGN
	 (#x00A3 . #xFFE1)	; POUND SIGN		FULLWIDTH POUND SIGN
	 (#x00AC . #xFFE2)	; NOT SIGN		FULLWIDTH NOT SIGN
	 (#x00A6 . #xFFE4)	; BROKEN LINE		FULLWIDTH BROKEN LINE
	 )))
  (define-translation-table 'japanese-ucs-jis-to-cp932-map map)
  (mapc #'(lambda (x) (let ((tmp (car x)))
			(setcar x (cdr x)) (setcdr x tmp)))
	map)
  (define-translation-table 'japanese-ucs-cp932-to-jis-map map))

;; U+2014 (EM DASH) vs U+2015 (HORIZONTAL BAR)
(define-translation-table 'japanese-ucs-glibc-to-jis-map '((#x2015 . #x2014)))
(define-translation-table 'japanese-ucs-jis-to-glibc-map '((#x2014 . #x2015)))

(define-coding-system 'japanese-shift-jis
  "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)"
  :coding-type 'shift-jis
  :mnemonic ?S
  :charset-list '(ascii katakana-jisx0201 japanese-jisx0208)
  :mime-charset 'shift_jis)

(define-coding-system-alias 'shift_jis 'japanese-shift-jis)
(define-coding-system-alias 'sjis 'japanese-shift-jis)

(define-coding-system 'japanese-cp932
  "CP932 (Microsoft shift-jis)"
  :coding-type 'charset
  :mnemonic ?S
  :charset-list '(ascii katakana-sjis cp932-2-byte))

(define-coding-system-alias 'cp932 'japanese-cp932)

(define-coding-system 'japanese-iso-7bit-1978-irv
  "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman."
  :coding-type 'iso-2022
  :mnemonic ?j
  :designation [(latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
				japanese-jisx0212 katakana-jisx0201)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation
		 use-roman use-oldjis)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0208-1978 japanese-jisx0208
			japanese-jisx0212))

(define-coding-system-alias 'iso-2022-jp-1978-irv 'japanese-iso-7bit-1978-irv)
(define-coding-system-alias 'old-jis 'japanese-iso-7bit-1978-irv)

(define-coding-system 'japanese-iso-8bit
  "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)."
  :coding-type 'iso-2022
  :mnemonic ?E
  :designation [ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212]
  :flags '(short ascii-at-eol ascii-at-cntl single-shift)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0208
			katakana-jisx0201 japanese-jisx0212
			japanese-jisx0208-1978)
  :mime-charset 'euc-jp)

(define-coding-system-alias 'euc-japan-1990 'japanese-iso-8bit)
(define-coding-system-alias 'euc-japan 'japanese-iso-8bit)
(define-coding-system-alias 'euc-jp 'japanese-iso-8bit)

(define-coding-system 'eucjp-ms
  "eucJP-ms (like EUC-JP but with CP932 extension).
eucJP-ms is defined in <http://www.opengroup.or.jp/jvc/cde/appendix.html>."
  :coding-type 'iso-2022
  :mnemonic ?E
  :designation [ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212]
  :flags '(short ascii-at-eol ascii-at-cntl single-shift)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0208
			katakana-jisx0201 japanese-jisx0212)
  :decode-translation-table 'eucjp-ms-decode
  :encode-translation-table 'eucjp-ms-encode)

(define-coding-system 'iso-2022-jp-2004
  "ISO 2022 based 7bit encoding for JIS X 0213:2004 (MIME:ISO-2022-JP-2004)."
  :coding-type 'iso-2022
  :mnemonic ?J
  :designation [(ascii japanese-jisx0208 japanese-jisx0213.2004-1
		       japanese-jisx0213-1 japanese-jisx0213-2)
		nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation)
		 ;; init-at-bol)
  :charset-list '(ascii japanese-jisx0208 japanese-jisx0213.2004-1
			japanese-jisx0213-1 japanese-jisx0213-2)
  :mime-charset 'iso-2022-jp-2004
  :suitable-for-keyboard t)

(define-coding-system-alias 'iso-2022-jp-3 'iso-2022-jp-2004)

(define-coding-system 'euc-jis-2004
  "ISO 2022 based EUC encoding for JIS X 0213 (MIME:EUC-JIS-2004)."
  :coding-type 'iso-2022
  :mnemonic ?E
  :designation [ascii japanese-jisx0213.2004-1 katakana-jisx0201
                      japanese-jisx0213-2]
  :flags '(short ascii-at-eol ascii-at-cntl single-shift)
  :charset-list '(ascii latin-jisx0201 japanese-jisx0213.2004-1
                        japanese-jisx0213-1 katakana-jisx0201
                        japanese-jisx0213-2)
  :mime-charset 'euc-jis-2004)

(define-coding-system-alias 'euc-jisx0213 'euc-jis-2004)

(define-coding-system 'japanese-shift-jis-2004
  "Shift_JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS-2004)"
  :coding-type 'shift-jis
  :mnemonic ?S
  :charset-list '(ascii katakana-jisx0201 
                        japanese-jisx0213.2004-1 japanese-jisx0213-2))

(define-coding-system-alias 'shift_jis-2004 'japanese-shift-jis-2004)

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment-internal)
	      (exit-function . use-default-char-width-table)
	      (iso639-language . ja)
	      (tutorial . "TUTORIAL.ja")
	      (charset japanese-jisx0208
		       japanese-jisx0212 latin-jisx0201 katakana-jisx0201
		       japanese-jisx0213.2004-1 japanese-jisx0213-1 
		       japanese-jisx0213-2 japanese-jisx0208-1978)
	      (coding-system iso-2022-jp japanese-iso-8bit
			     japanese-shift-jis japanese-iso-7bit-1978-irv
                             iso-2022-jp-2004 japanese-shift-jis-2004
                             euc-jis-2004)
	      (coding-priority iso-2022-jp japanese-iso-8bit
			       japanese-shift-jis 
                               iso-2022-jp-2004 euc-jis-2004 
                               japanese-shift-jis-2004
                               iso-2022-jp-2)
	      (input-method . "japanese")
	      (features japan-util)
	      (sample-text . "Japanese (日本語)	こんにちは, ｺﾝﾆﾁﾊ")
	      (documentation . t)))

(let ((map
       ;; JISX0213-1 vs Unicode
       '((#x2477 . [#x304B #x309A])
	 (#x2478 . [#x304D #x309A])
	 (#x2479 . [#x304F #x309A])
	 (#x247a . [#x3051 #x309A])
	 (#x247b . [#x3053 #x309A])
	 (#x2577 . [#x30AB #x309A])
	 (#x2578 . [#x30AD #x309A])
	 (#x2579 . [#x30AF #x309A])
	 (#x257a . [#x30B1 #x309A])
	 (#x257b . [#x30B3 #x309A])
	 (#x257c . [#x30BB #x309A])
	 (#x257d . [#x30C4 #x309A])
	 (#x257e . [#x30C8 #x309A])
	 (#x2678 . [#x31F7 #x309A])
	 (#x2b44 . [#x00E6 #x0300])
	 (#x2b48 . [#x0254 #x0300])
	 (#x2b49 . [#x0254 #x0301])
	 (#x2b4a . [#x028C #x0300])
	 (#x2b4b . [#x028C #x0301])
	 (#x2b4c . [#x0259 #x0300])
	 (#x2b4d . [#x0259 #x0301])
	 (#x2b4e . [#x025A #x0300])
	 (#x2b4f . [#x025A #x0301])
	 (#x2b65 . [#x02E9 #x02E5])
	 (#x2b66 . [#x02E5 #x02E9])))
      table)
  (dolist (elt map)
    (setcar elt (decode-char 'japanese-jisx0213-1 (car elt))))
  (setq table (make-translation-table-from-alist map))
  (define-translation-table 'jisx0213-to-unicode table)
  (define-translation-table 'unicode-to-jisx0213
    (char-table-extra-slot table 0)))

(defun compose-gstring-for-variation-glyph (gstring)
  "Compose glyph-string GSTRING for graphic display.
GSTRING must have two glyphs; the first is a glyph for a han character,
and the second is a glyph for a variation selector."
  (let* ((font (lgstring-font gstring))
	 (han (lgstring-char gstring 0))
	 (vs (lgstring-char gstring 1))
	 (glyphs (font-variation-glyphs font han))
	 (g0 (lgstring-glyph gstring 0))
	 (g1 (lgstring-glyph gstring 1)))
    (catch 'tag
      (dolist (elt glyphs)
	(if (= (car elt) vs)
	    (progn
	      (lglyph-set-code g0 (cdr elt))
	      (lglyph-set-from-to g0 (lglyph-from g0) (lglyph-to g1))
	      (lgstring-set-glyph gstring 1 nil)
	      (throw 'tag gstring)))))))

(let ((elt '([".." 1 compose-gstring-for-variation-glyph])))
  (set-char-table-range composition-function-table '(#xFE00 . #xFE0F) elt)
  (set-char-table-range composition-function-table '(#xE0100 . #xE01EF) elt))

(provide 'japanese)

;;; japanese.el ends here
