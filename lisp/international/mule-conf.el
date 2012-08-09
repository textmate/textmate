;;; mule-conf.el --- configure multilingual environment

;; Copyright (C) 1997-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: i18n, mule, multilingual, character set, coding system

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

;; This file defines the Emacs charsets and some basic coding systems.
;; Other coding systems are defined in the files in directory
;; lisp/language.

;;; Code:

;;; Remarks

;; The ISO-IR registry is at http://www.itscj.ipsj.or.jp/ISO-IR/.
;; Standards docs equivalent to iso-2022 and iso-8859 are at
;; http://www.ecma.ch/.

;; FWIW, http://www.microsoft.com/globaldev/ lists the following for
;; MS Windows, which are presumably the only charsets we really need
;; to worry about on such systems:
;; `OEM codepages': 437, 720, 737, 775, 850, 852, 855, 857, 858, 862, 866
;; `Windows codepages': 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257,
;;                      1258, 874, 932, 936, 949, 950

;;; Definitions of character sets.

;; The charsets `ascii', `unicode' and `eight-bit' are already defined
;; in charset.c as below:
;;
;; (define-charset 'ascii
;;   ""
;;   :dimension 1
;;   :code-space [0 127]
;;   :iso-final-char ?B
;;   :ascii-compatible-p t
;;   :emacs-mule-id 0
;;   :code-offset 0)
;;
;; (define-charset 'unicode
;;   ""
;;   :dimension 3
;;   :code-space [0 255 0 255 0 16]
;;   :ascii-compatible-p t
;;   :code-offset 0)
;;
;; (define-charset 'emacs
;;   ""
;;   :dimension 3
;;   :code-space [0 255 0 255 0 63]
;;   :ascii-compatible-p t
;;   :supplementary-p t
;;   :code-offset 0)
;;
;; (define-charset 'eight-bit
;;   ""
;;   :dimension 1
;;   :code-space [128 255]
;;   :code-offset #x3FFF80)
;;
;; We now set :docstring, :short-name, and :long-name properties.

(put-charset-property
 'ascii :docstring "ASCII (ISO646 IRV)")
(put-charset-property
 'ascii :short-name "ASCII")
(put-charset-property
 'ascii :long-name "ASCII (ISO646 IRV)")
(put-charset-property
 'iso-8859-1 :docstring "Latin-1 (ISO/IEC 8859-1)")
(put-charset-property
 'iso-8859-1 :short-name "Latin-1")
(put-charset-property
 'iso-8859-1 :long-name "Latin-1")
(put-charset-property
 'unicode :docstring "Unicode (ISO10646)")
(put-charset-property
 'unicode :short-name "Unicode")
(put-charset-property
 'unicode :long-name "Unicode (ISO10646)")
(put-charset-property
 'emacs :docstring "Full Emacs charset (excluding eight bit chars)")
(put-charset-property
 'emacs :short-name "Emacs")
(put-charset-property
 'emacs :long-name "Emacs")

(put-charset-property 'eight-bit :docstring "Raw bytes 128-255")
(put-charset-property 'eight-bit :short-name "Raw bytes")

(define-charset-alias 'ucs 'unicode)

(define-charset 'latin-iso8859-1
  "Right-Hand Part of ISO/IEC 8859/1 (Latin-1): ISO-IR-100"
  :short-name "RHP of Latin-1"
  :long-name "RHP of ISO/IEC 8859/1 (Latin-1): ISO-IR-100"
  :iso-final-char ?A
  :emacs-mule-id 129
  :code-space [32 127]
  :code-offset 160)

;; Name perhaps not ideal, but is XEmacs-compatible.
(define-charset 'control-1
  "8-bit control code (0x80..0x9F)"
  :short-name "8-bit control code"
  :code-space [128 159]
  :code-offset 128)

(define-charset 'eight-bit-control
  "Raw bytes in the range 0x80..0x9F (usually produced from invalid encodings)"
  :short-name "Raw bytes 0x80..0x9F"
  :supplementary-p t
  :code-space [128 159]
  :code-offset #x3FFF80)		; see character.h

(define-charset 'eight-bit-graphic
  "Raw bytes in the range 0xA0..0xFF (usually produced from invalid encodings)"
  :short-name "Raw bytes 0xA0..0xFF"
  :supplementary-p t
  :code-space [160 255]
  :code-offset #x3FFFA0)		; see character.h

(defmacro define-iso-single-byte-charset (symbol iso-symbol name nickname
						 iso-ir iso-final
						 emacs-mule-id map)
  `(progn
     (define-charset ,symbol
       ,name
       :short-name ,nickname
       :long-name ,name
       :ascii-compatible-p t
       :code-space [0 255]
       :map ,map)
     (if ,iso-symbol
	 (define-charset ,iso-symbol
	   (if ,iso-ir
	       (format "Right-Hand Part of %s (%s): ISO-IR-%d"
		       ,name ,nickname ,iso-ir)
	     (format "Right-Hand Part of %s (%s)" ,name ,nickname))
	   :short-name (format "RHP of %s" ,name)
	   :long-name (format "RHP of %s (%s)" ,name ,nickname)
	   :iso-final-char ,iso-final
	   :emacs-mule-id ,emacs-mule-id
	   :code-space [32 127]
	   :subset (list ,symbol 160 255 -128)))))

(define-iso-single-byte-charset 'iso-8859-2 'latin-iso8859-2
  "ISO/IEC 8859/2" "Latin-2" 101 ?B 130 "8859-2")

(define-iso-single-byte-charset 'iso-8859-3 'latin-iso8859-3
  "ISO/IEC 8859/3" "Latin-3" 109 ?C 131 "8859-3")

(define-iso-single-byte-charset 'iso-8859-4 'latin-iso8859-4
  "ISO/IEC 8859/4" "Latin-4" 110 ?D 132 "8859-4")

(define-iso-single-byte-charset 'iso-8859-5 'cyrillic-iso8859-5
  "ISO/IEC 8859/5" "Latin/Cyrillic" 144 ?L 140 "8859-5")

(define-iso-single-byte-charset 'iso-8859-6 'arabic-iso8859-6
  "ISO/IEC 8859/6" "Latin/Arabic" 127 ?G 135 "8859-6")

(define-iso-single-byte-charset 'iso-8859-7 'greek-iso8859-7
  "ISO/IEC 8859/7" "Latin/Greek" 126 ?F 134 "8859-7")

(define-iso-single-byte-charset 'iso-8859-8 'hebrew-iso8859-8
  "ISO/IEC 8859/8" "Latin/Hebrew" 138 ?H 136 "8859-8")

(define-iso-single-byte-charset 'iso-8859-9 'latin-iso8859-9
  "ISO/IEC 8859/9" "Latin-5" 148 ?M 141 "8859-9")

(define-iso-single-byte-charset 'iso-8859-10 'latin-iso8859-10
  "ISO/IEC 8859/10" "Latin-6" 157 ?V nil "8859-10")

;; http://www.nectec.or.th/it-standards/iso8859-11/
;; http://www.cwi.nl/~dik/english/codes/8859.html says this is tis-620
;; plus nbsp
(define-iso-single-byte-charset 'iso-8859-11 'thai-iso8859-11
  "ISO/IEC 8859/11" "Latin/Thai" 166 ?T nil "8859-11")

;; 8859-12 doesn't (yet?) exist.

(define-iso-single-byte-charset 'iso-8859-13 'latin-iso8859-13
  "ISO/IEC 8859/13" "Latin-7" 179 ?Y nil "8859-13")

(define-iso-single-byte-charset 'iso-8859-14 'latin-iso8859-14
  "ISO/IEC 8859/14" "Latin-8" 199 ?_ 143 "8859-14")

(define-iso-single-byte-charset 'iso-8859-15 'latin-iso8859-15
  "ISO/IEC 8859/15" "Latin-9" 203 ?b 142 "8859-15")

(define-iso-single-byte-charset 'iso-8859-16 'latin-iso8859-16
  "ISO/IEC 8859/16" "Latin-10" 226 ?f nil "8859-16")

;; No point in keeping it around.
(fmakunbound 'define-iso-single-byte-charset)

;; Can this be shared with 8859-11?
;; N.b. not all of these are defined in Unicode.
(define-charset 'thai-tis620
  "TIS620.2533"
  :short-name "TIS620.2533"
  :iso-final-char ?T
  :emacs-mule-id 133
  :code-space [32 127]
  :code-offset #x0E00)

;; Fixme: doc for this, c.f. above
(define-charset 'tis620-2533
  "TIS620.2533"
  :short-name "TIS620.2533"
  :ascii-compatible-p t
  :code-space [0 255]
  :superset '(ascii eight-bit-control (thai-tis620 . 128)))

(define-charset 'jisx0201
  "JISX0201"
  :short-name "JISX0201"
  :code-space [0 #xDF]
  :map "JISX0201")

(define-charset 'latin-jisx0201
  "Roman Part of JISX0201.1976"
  :short-name "JISX0201 Roman"
  :long-name "Japanese Roman (JISX0201.1976)"
  :iso-final-char ?J
  :emacs-mule-id  138
  :supplementary-p t
  :code-space [33 126]
  :subset '(jisx0201 33 126 0))

(define-charset 'katakana-jisx0201
  "Katakana Part of JISX0201.1976"
  :short-name "JISX0201 Katakana"
  :long-name "Japanese Katakana (JISX0201.1976)"
  :iso-final-char ?I
  :emacs-mule-id  137
  :supplementary-p t
  :code-space [33 126]
  :subset '(jisx0201 161 254 -128))

(define-charset 'chinese-gb2312
  "GB2312 Chinese simplified: ISO-IR-58"
  :short-name "GB2312"
  :long-name "GB2312: ISO-IR-58"
  :iso-final-char ?A
  :emacs-mule-id 145
  :code-space [33 126 33 126]
  :code-offset #x110000
  :unify-map "GB2312")

(define-charset 'chinese-gbk
  "GBK Chinese simplified."
  :short-name "GBK"
  :code-space [#x40 #xFE #x81 #xFE]
  :code-offset #x160000
  :unify-map "GBK")
(define-charset-alias 'cp936 'chinese-gbk)
(define-charset-alias 'windows-936 'chinese-gbk)

(define-charset 'chinese-cns11643-1
  "CNS11643 Plane 1 Chinese traditional: ISO-IR-171"
  :short-name "CNS11643-1"
  :long-name "CNS11643-1 (Chinese traditional): ISO-IR-171"
  :iso-final-char ?G
  :emacs-mule-id  149
  :code-space [33 126 33 126]
  :code-offset #x114000
  :unify-map "CNS-1")

(define-charset 'chinese-cns11643-2
  "CNS11643 Plane 2 Chinese traditional: ISO-IR-172"
  :short-name "CNS11643-2"
  :long-name "CNS11643-2 (Chinese traditional): ISO-IR-172"
  :iso-final-char ?H
  :emacs-mule-id  150
  :code-space [33 126 33 126]
  :code-offset #x118000
  :unify-map "CNS-2")

(define-charset 'chinese-cns11643-3
  "CNS11643 Plane 3 Chinese Traditional: ISO-IR-183"
  :short-name  "CNS11643-3"
  :long-name "CNS11643-3 (Chinese traditional): ISO-IR-183"
  :iso-final-char ?I
  :code-space [33 126 33 126]
  :emacs-mule-id  246
  :code-offset #x11C000
  :unify-map "CNS-3")

(define-charset 'chinese-cns11643-4
  "CNS11643 Plane 4 Chinese Traditional: ISO-IR-184"
  :short-name  "CNS11643-4"
  :long-name "CNS11643-4 (Chinese traditional): ISO-IR-184"
  :iso-final-char ?J
  :emacs-mule-id  247
  :code-space [33 126 33 126]
  :code-offset #x120000
  :unify-map "CNS-4")

(define-charset 'chinese-cns11643-5
  "CNS11643 Plane 5 Chinese Traditional: ISO-IR-185"
  :short-name  "CNS11643-5"
  :long-name "CNS11643-5 (Chinese traditional): ISO-IR-185"
  :iso-final-char ?K
  :emacs-mule-id  248
  :code-space [33 126 33 126]
  :code-offset #x124000
  :unify-map "CNS-5")

(define-charset 'chinese-cns11643-6
  "CNS11643 Plane 6 Chinese Traditional: ISO-IR-186"
  :short-name  "CNS11643-6"
  :long-name "CNS11643-6 (Chinese traditional): ISO-IR-186"
  :iso-final-char ?L
  :emacs-mule-id 249
  :code-space [33 126 33 126]
  :code-offset #x128000
  :unify-map "CNS-6")

(define-charset 'chinese-cns11643-7
  "CNS11643 Plane 7 Chinese Traditional: ISO-IR-187"
  :short-name  "CNS11643-7"
  :long-name "CNS11643-7 (Chinese traditional): ISO-IR-187"
  :iso-final-char ?M
  :emacs-mule-id 250
  :code-space [33 126 33 126]
  :code-offset #x12C000
  :unify-map "CNS-7")

(define-charset 'big5
  "Big5 (Chinese traditional)"
  :short-name "Big5"
  :code-space [#x40 #xFE #xA1 #xFE]
  :code-offset #x130000
  :unify-map "BIG5")
;; Fixme: AKA cp950 according to
;; <URL:http://www.microsoft.com/globaldev/reference/WinCP.asp>.  Is
;; that correct?

(define-charset 'chinese-big5-1
  "Frequently used part (A141-C67E) of Big5 (Chinese traditional)"
  :short-name "Big5 (Level-1)"
  :long-name "Big5 (Level-1) A141-C67F"
  :iso-final-char ?0
  :emacs-mule-id 152
  :supplementary-p t
  :code-space [#x21 #x7E #x21 #x7E]
  :code-offset #x135000
  :unify-map "BIG5-1")

(define-charset 'chinese-big5-2
  "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"
  :short-name "Big5 (Level-2)"
  :long-name "Big5 (Level-2) C940-FEFE"
  :iso-final-char ?1
  :emacs-mule-id  153
  :supplementary-p t
  :code-space [#x21 #x7E #x21 #x7E]
  :code-offset #x137800
  :unify-map "BIG5-2")

(define-charset 'japanese-jisx0208
  "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"
  :short-name "JISX0208"
  :long-name "JISX0208.1983/1990 (Japanese): ISO-IR-87"
  :iso-final-char ?B
  :emacs-mule-id 146
  :code-space [33 126 33 126]
  :code-offset #x140000
  :unify-map "JISX0208")

(define-charset 'japanese-jisx0208-1978
  "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"
  :short-name "JISX0208.1978"
  :long-name  "JISX0208.1978 (JISC6226.1978): ISO-IR-42"
  :iso-final-char ?@
  :emacs-mule-id  144
  :code-space [33 126 33 126]
  :code-offset #x144000
  :unify-map "JISC6226")

(define-charset 'japanese-jisx0212
  "JISX0212 Japanese supplement: ISO-IR-159"
  :short-name "JISX0212"
  :long-name "JISX0212 (Japanese): ISO-IR-159"
  :iso-final-char ?D
  :emacs-mule-id 148
  :code-space [33 126 33 126]
  :code-offset #x148000
  :unify-map "JISX0212")

;; Note that jisx0213 contains characters not in Unicode (3.2?).  It's
;; arguable whether it should have a unify-map.
(define-charset 'japanese-jisx0213-1
  "JISX0213.2000 Plane 1 (Japanese)"
  :short-name "JISX0213-1"
  :iso-final-char ?O
  :emacs-mule-id  151
  :unify-map "JISX2131"
  :code-space [33 126 33 126]
  :code-offset #x14C000)

(define-charset 'japanese-jisx0213-2
  "JISX0213.2000 Plane 2 (Japanese)"
  :short-name "JISX0213-2"
  :iso-final-char ?P
  :emacs-mule-id 254
  :unify-map "JISX2132"
  :code-space [33 126 33 126]
  :code-offset #x150000)

(define-charset 'japanese-jisx0213-a
  "JISX0213.2004 adds these characters to JISX0213.2000."
  :short-name "JISX0213A"
  :dimension 2
  :code-space [33 126 33 126]
  :supplementary-p t
  :map "JISX213A")

(define-charset 'japanese-jisx0213.2004-1
  "JISX0213.2004 Plane1 (Japanese)"
  :short-name "JISX0213.2004-1"
  :dimension 2
  :code-space [33 126 33 126]
  :iso-final-char ?Q
  :superset '(japanese-jisx0213-a japanese-jisx0213-1))

(define-charset 'katakana-sjis
  "Katakana part of Shift-JIS"
  :dimension 1
  :code-space [#xA1 #xDF]
  :subset '(jisx0201 #xA1 #xDF 0)
  :supplementary-p t)

(define-charset 'cp932-2-byte
  "2-byte part of CP932"
  :dimension 2
  :map "CP932-2BYTE"
  :code-space [#x40 #xFC #x81 #xFC]
  :supplementary-p t)

(define-charset 'cp932
  "CP932 (Microsoft shift-jis)"
  :code-space [#x00 #xFF #x00 #xFE]
  :short-name "CP932"
  :superset '(ascii katakana-sjis cp932-2-byte))

(define-charset 'korean-ksc5601
  "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
  :short-name "KSC5601"
  :long-name "KSC5601 (Korean): ISO-IR-149"
  :iso-final-char ?C
  :emacs-mule-id 147
  :code-space [33 126 33 126]
  :code-offset #x279f94			; ... #x27c217
  :unify-map "KSC5601")

(define-charset 'big5-hkscs
  "Big5-HKSCS (Chinese traditional, Hong Kong supplement)"
  :short-name "Big5"
  :code-space [#x40 #xFE #xA1 #xFE]
  :code-offset #x27c218			; ... #x280839
  :unify-map "BIG5-HKSCS")

(define-charset 'cp949-2-byte
  "2-byte part of CP949"
  :dimension 2
  :map "CP949-2BYTE"
  :code-space [#x41 #xFE #x81 #xFD]
  :supplementary-p t)

(define-charset 'cp949
  "CP949 (Korean)"
  :short-name "CP949"
  :long-name  "CP949 (Korean)"
  :code-space [#x00 #xFE #x00 #xFD]
  :superset '(ascii cp949-2-byte))

(define-charset 'chinese-sisheng
  "SiSheng characters for PinYin/ZhuYin"
  :short-name "SiSheng"
  :long-name "SiSheng (PinYin/ZhuYin)"
  :iso-final-char ?0
  :emacs-mule-id 160
  :code-space [33 126]
  :unify-map "MULE-sisheng"
  :supplementary-p t
  :code-offset #x200000)

;; A subset of the 1989 version of IPA.  It consists of the consonant
;; signs used in English, French, German and Italian, and all vowels
;; signs in the table.  [says old MULE doc]
(define-charset 'ipa
  "IPA (International Phonetic Association)"
  :short-name "IPA"
  :iso-final-char ?0
  :emacs-mule-id  161
  :unify-map "MULE-ipa"
  :code-space [32 127]
  :supplementary-p t
  :code-offset #x200080)

(define-charset 'viscii
  "VISCII1.1"
  :short-name "VISCII"
  :long-name "VISCII 1.1"
  :code-space [0 255]
  :map "VISCII")

(define-charset 'vietnamese-viscii-lower
  "VISCII1.1 lower-case"
  :short-name "VISCII lower"
  :long-name "VISCII lower-case"
  :iso-final-char ?1
  :emacs-mule-id  162
  :code-space [32 127]
  :code-offset #x200200
  :supplementary-p t
  :unify-map "MULE-lviscii")

(define-charset 'vietnamese-viscii-upper
  "VISCII1.1 upper-case"
  :short-name "VISCII upper"
  :long-name "VISCII upper-case"
  :iso-final-char ?2
  :emacs-mule-id  163
  :code-space [32 127]
  :code-offset #x200280
  :supplementary-p t
  :unify-map "MULE-uviscii")

(define-charset 'vscii
  "VSCII1.1 (TCVN-5712 VN1)"
  :short-name "VSCII"
  :code-space [0 255]
  :map "VSCII")

(define-charset-alias 'tcvn-5712 'vscii)

;; Fixme: see note in tcvn.map about combining characters
(define-charset 'vscii-2
  "VSCII-2 (TCVN-5712 VN2)"
  :code-space [0 255]
  :map "VSCII-2")

(define-charset 'koi8-r
  "KOI8-R"
  :short-name "KOI8-R"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "KOI8-R")

(define-charset-alias 'koi8 'koi8-r)

(define-charset 'alternativnyj
  "ALTERNATIVNYJ"
  :short-name "alternativnyj"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "ALTERNATIVNYJ")

(define-charset 'cp866
  "CP866"
  :short-name "cp866"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "IBM866")
(define-charset-alias 'ibm866 'cp866)

(define-charset 'koi8-u
  "KOI8-U"
  :short-name "KOI8-U"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "KOI8-U")

(define-charset 'koi8-t
  "KOI8-T"
  :short-name "KOI8-T"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "KOI8-T")

(define-charset 'georgian-ps
  "GEORGIAN-PS"
  :short-name "GEORGIAN-PS"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "KA-PS")

(define-charset 'georgian-academy
  "GEORGIAN-ACADEMY"
  :short-name "GEORGIAN-ACADEMY"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "KA-ACADEMY")

(define-charset 'windows-1250
  "WINDOWS-1250 (Central Europe)"
  :short-name "WINDOWS-1250"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1250")
(define-charset-alias 'cp1250 'windows-1250)

(define-charset 'windows-1251
  "WINDOWS-1251 (Cyrillic)"
  :short-name "WINDOWS-1251"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1251")
(define-charset-alias 'cp1251 'windows-1251)

(define-charset 'windows-1252
  "WINDOWS-1252 (Latin I)"
  :short-name "WINDOWS-1252"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1252")
(define-charset-alias 'cp1252 'windows-1252)

(define-charset 'windows-1253
  "WINDOWS-1253 (Greek)"
  :short-name "WINDOWS-1253"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1253")
(define-charset-alias 'cp1253 'windows-1253)

(define-charset 'windows-1254
  "WINDOWS-1254 (Turkish)"
  :short-name "WINDOWS-1254"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1254")
(define-charset-alias 'cp1254 'windows-1254)

(define-charset 'windows-1255
  "WINDOWS-1255 (Hebrew)"
  :short-name "WINDOWS-1255"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1255")
(define-charset-alias 'cp1255 'windows-1255)

(define-charset 'windows-1256
  "WINDOWS-1256 (Arabic)"
  :short-name "WINDOWS-1256"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1256")
(define-charset-alias 'cp1256 'windows-1256)

(define-charset 'windows-1257
  "WINDOWS-1257 (Baltic)"
  :short-name "WINDOWS-1257"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1257")
(define-charset-alias 'cp1257 'windows-1257)

(define-charset 'windows-1258
  "WINDOWS-1258 (Viet Nam)"
  :short-name "WINDOWS-1258"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "CP1258")
(define-charset-alias 'cp1258 'windows-1258)

(define-charset 'next
  "NEXT"
  :short-name "NEXT"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "NEXTSTEP")

(define-charset 'cp1125
  "CP1125"
  :short-name "CP1125"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "CP1125")
(define-charset-alias 'ruscii 'cp1125)
;; Original name for cp1125, says Serhii Hlodin <hlodin@lutsk.bank.gov.ua>
(define-charset-alias 'cp866u 'cp1125)

;; Fixme: C.f. iconv, http://czyborra.com/charsets/codepages.html
;; shows this as not ASCII compatible, with various graphics in
;; 0x01-0x1F.
(define-charset 'cp437
  "CP437 (MS-DOS United States, Australia, New Zealand, South Africa)"
  :short-name "CP437"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM437")

(define-charset 'cp720
  "CP720 (Arabic)"
  :short-name "CP720"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "CP720")

(define-charset 'cp737
  "CP737 (PC Greek)"
  :short-name "CP737"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "CP737")

(define-charset 'cp775
  "CP775 (PC Baltic)"
  :short-name "CP775"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "CP775")

(define-charset 'cp851
  "CP851 (Greek)"
  :short-name "CP851"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM851")

(define-charset 'cp852
  "CP852 (MS-DOS Latin-2)"
  :short-name "CP852"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM852")

(define-charset 'cp855
  "CP855 (IBM Cyrillic)"
  :short-name "CP855"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM855")

(define-charset 'cp857
  "CP857 (IBM Turkish)"
  :short-name "CP857"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM857")

(define-charset 'cp858
  "CP858 (Multilingual Latin I + Euro)"
  :short-name "CP858"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "CP858")
(define-charset-alias 'cp00858 'cp858)	; IANA has IBM00858/CP00858

(define-charset 'cp860
  "CP860 (MS-DOS Portuguese)"
  :short-name "CP860"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM860")

(define-charset 'cp861
  "CP861 (MS-DOS Icelandic)"
  :short-name "CP861"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM861")

(define-charset 'cp862
  "CP862 (PC Hebrew)"
  :short-name "CP862"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM862")

(define-charset 'cp863
  "CP863 (MS-DOS Canadian French)"
  :short-name "CP863"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM863")

(define-charset 'cp864
  "CP864 (PC Arabic)"
  :short-name "CP864"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM864")

(define-charset 'cp865
  "CP865 (MS-DOS Nordic)"
  :short-name "CP865"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM865")

(define-charset 'cp869
  "CP869 (IBM Modern Greek)"
  :short-name "CP869"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM869")

(define-charset 'cp874
  "CP874 (IBM Thai)"
  :short-name "CP874"
  :code-space [0 255]
  :ascii-compatible-p t
  :map "IBM874")

;; For Arabic, we need three different types of character sets.
;; Digits are of direction left-to-right and of width 1-column.
;; Others are of direction right-to-left and of width 1-column or
;; 2-column.
(define-charset 'arabic-digit
  "Arabic digit"
  :short-name "Arabic digit"
  :iso-final-char ?2
  :emacs-mule-id 164
  :supplementary-p t
  :code-space [34 42]
  :code-offset #x0600)

(define-charset 'arabic-1-column
  "Arabic 1-column"
  :short-name "Arabic 1-col"
  :long-name "Arabic 1-column"
  :iso-final-char ?3
  :emacs-mule-id 165
  :supplementary-p t
  :code-space [33 126]
  :code-offset #x200100)

(define-charset 'arabic-2-column
  "Arabic 2-column"
  :short-name "Arabic 2-col"
  :long-name "Arabic 2-column"
  :iso-final-char ?4
  :emacs-mule-id 224
  :supplementary-p t
  :code-space [33 126]
  :code-offset #x200180)

;; Lao script.
;; Codes 0x21..0x7E are mapped to Unicode U+0E81..U+0EDF.
;; Not all of them are defined in Unicode.
(define-charset 'lao
  "Lao characters (ISO10646 0E81..0EDF)"
  :short-name "Lao"
  :iso-final-char ?1
  :emacs-mule-id 167
  :supplementary-p t
  :code-space [33 126]
  :code-offset #x0E81)

(define-charset 'mule-lao
  "Lao characters (ISO10646 0E81..0EDF)"
  :short-name "Lao"
  :code-space [0 255]
  :supplementary-p t
  :superset '(ascii eight-bit-control (lao . 128)))


;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
;; not assigned.  They are automatically converted to each Indian
;; script which IS-13194 supports.

(define-charset 'indian-is13194
  "Generic Indian charset for data exchange with IS 13194"
  :short-name "IS 13194"
  :long-name "Indian IS 13194"
  :iso-final-char ?5
  :emacs-mule-id 225
  :supplementary-p t
  :code-space [33 126]
  :code-offset #x180000)

(let ((code-offset #x180100))
  (dolist (script '(devanagari sanskrit bengali tamil telugu assamese
			       oriya kannada malayalam gujarati punjabi))
    (define-charset (intern (format "%s-cdac" script))
      (format "Glyphs of %s script for CDAC font.  Subset of `indian-glyph'."
	      (capitalize (symbol-name script)))
      :short-name (format "CDAC %s glyphs" (capitalize (symbol-name script)))
      :supplementary-p t
      :code-space [0 255]
      :code-offset code-offset)
    (setq code-offset (+ code-offset #x100)))

  (dolist (script '(devanagari bengali punjabi gujarati
			       oriya tamil telugu kannada malayalam))
    (define-charset (intern (format "%s-akruti" script))
      (format "Glyphs of %s script for AKRUTI font.  Subset of `indian-glyph'."
	      (capitalize (symbol-name script)))
      :short-name (format "AKRUTI %s glyphs" (capitalize (symbol-name script)))
      :supplementary-p t
      :code-space [0 255]
      :code-offset code-offset)
    (setq code-offset (+ code-offset #x100))))

(define-charset 'indian-glyph
  "Glyphs for Indian characters."
  :short-name "Indian glyph"
  :iso-final-char ?4
  :emacs-mule-id 240
  :supplementary-p t
  :code-space [32 127 32 127]
  :code-offset #x180100)

;; Actual Glyph for 1-column width.
(define-charset 'indian-1-column
  "Indian charset for 1-column width glyphs."
  :short-name "Indian 1-col"
  :long-name "Indian 1 Column"
  :iso-final-char ?6
  :emacs-mule-id  251
  :supplementary-p t
  :code-space [33 126 33 126]
  :code-offset #x184000)

;; Actual Glyph for 2-column width.
(define-charset 'indian-2-column
  "Indian charset for 2-column width glyphs."
  :short-name "Indian 2-col"
  :long-name "Indian 2 Column"
  :iso-final-char ?5
  :emacs-mule-id  251
  :supplementary-p t
  :code-space [33 126 33 126]
  :code-offset #x184000)

(define-charset 'tibetan
  "Tibetan characters"
  :iso-final-char ?7
  :short-name "Tibetan 2-col"
  :long-name "Tibetan 2 column"
  :iso-final-char ?7
  :emacs-mule-id 252
  :unify-map "MULE-tibetan"
  :supplementary-p t
  :code-space [33 126 33 37]
  :code-offset #x190000)

(define-charset 'tibetan-1-column
  "Tibetan 1 column glyph"
  :short-name "Tibetan 1-col"
  :long-name "Tibetan 1 column"
  :iso-final-char ?8
  :emacs-mule-id 241
  :supplementary-p t
  :code-space [33 126 33 37]
  :code-offset #x190000)

;; Subsets of Unicode.
(define-charset 'mule-unicode-2500-33ff
  "Unicode characters of the range U+2500..U+33FF."
  :short-name "Unicode subset 2"
  :long-name "Unicode subset (U+2500..U+33FF)"
  :iso-final-char ?2
  :emacs-mule-id 242
  :supplementary-p t
  :code-space [#x20 #x7f #x20 #x47]
  :code-offset #x2500)

(define-charset 'mule-unicode-e000-ffff
  "Unicode characters of the range U+E000..U+FFFF."
  :short-name "Unicode subset 3"
  :long-name "Unicode subset (U+E000+FFFF)"
  :iso-final-char ?3
  :emacs-mule-id 243
  :supplementary-p t
  :code-space [#x20 #x7F #x20 #x75]
  :code-offset #xE000
  :max-code 30015)			; U+FFFF

(define-charset 'mule-unicode-0100-24ff
  "Unicode characters of the range U+0100..U+24FF."
  :short-name "Unicode subset"
  :long-name "Unicode subset (U+0100..U+24FF)"
  :iso-final-char ?1
  :emacs-mule-id 244
  :supplementary-p t
  :code-space [#x20 #x7F #x20 #x7F]
  :code-offset #x100)

(define-charset 'unicode-bmp
  "Unicode Basic Multilingual Plane (U+0000..U+FFFF)"
  :short-name "Unicode BMP"
  :code-space [0 255 0 255]
  :code-offset 0)

(define-charset 'unicode-smp
  "Unicode Supplementary Multilingual Plane (U+10000..U+1FFFF)"
  :short-name "Unicode SMP "
  :code-space [0 255 0 255]
  :code-offset #x10000)

(define-charset 'unicode-sip
  "Unicode Supplementary Ideographic Plane (U+20000..U+2FFFF)"
  :short-name "Unicode SIP"
  :code-space [0 255 0 255]
  :code-offset #x20000)

(define-charset 'unicode-ssp
  "Unicode Supplementary Special-purpose Plane (U+E0000..U+EFFFF)"
  :short-name "Unicode SSP"
  :code-space [0 255 0 255]
  :code-offset #xE0000)

(define-charset 'ethiopic
  "Ethiopic characters for Amharic and Tigrigna."
  :short-name "Ethiopic"
  :long-name "Ethiopic characters"
  :iso-final-char ?3
  :emacs-mule-id  245
  :supplementary-p t
  :unify-map "MULE-ethiopic"
  :code-space [33 126 33 126]
  :code-offset #x1A0000)

(define-charset 'mac-roman
  "Mac Roman charset"
  :short-name "Mac Roman"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "MACINTOSH")

;; Fixme: modern EBCDIC variants, e.g. IBM00924?
(define-charset 'ebcdic-us
  "US version of EBCDIC"
  :short-name "EBCDIC-US"
  :code-space [0 255]
  :mime-charset 'ebcdic-us
  :map "EBCDICUS")

(define-charset 'ebcdic-uk
  "UK version of EBCDIC"
  :short-name "EBCDIC-UK"
  :code-space [0 255]
  :mime-charset 'ebcdic-uk
  :map "EBCDICUK")

(define-charset 'ibm1047
  ;; Says groff:
  "IBM1047, `EBCDIC Latin 1/Open Systems' used by OS/390 Unix."
  :short-name "IBM1047"
  :code-space [0 255]
  :mime-charset 'ibm1047
  :map "IBM1047")
(define-charset-alias 'cp1047 'ibm1047)

(define-charset 'hp-roman8
  "Encoding used by Hewlet-Packard printer software"
  :short-name "HP-ROMAN8"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "HP-ROMAN8")

;; To make a coding system with this, a pre-write-conversion should
;; account for the commented-out multi-valued code points in
;; stdenc.map.
(define-charset 'adobe-standard-encoding
  "Adobe `standard encoding' used in PostScript"
  :short-name "ADOBE-STANDARD-ENCODING"
  :code-space [#x20 255]
  :map "stdenc")

(define-charset 'symbol
  "Adobe symbol encoding used in PostScript"
  :short-name "ADOBE-SYMBOL"
  :code-space [#x20 255]
  :map "symbol")

(define-charset 'ibm850
  "DOS codepage 850 (Latin-1)"
  :short-name "IBM850"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "IBM850")
(define-charset-alias 'cp850 'ibm850)

(define-charset 'mik
  "Bulgarian DOS codepage"
  :short-name "MIK"
  :ascii-compatible-p t
  :code-space [0 255]
  :map "MIK")

(define-charset 'ptcp154
  "`Paratype' codepage (Asian Cyrillic)"
  :short-name "PT154"
  :ascii-compatible-p t
  :code-space [0 255]
  :mime-charset 'pt154
  :map "PTCP154")
(define-charset-alias 'pt154 'ptcp154)
(define-charset-alias 'cp154 'ptcp154)

(define-charset 'gb18030-2-byte
  "GB18030 2-byte (0x814E..0xFEFE)"
  :code-space [#x40 #xFE #x81 #xFE]
  :supplementary-p t
  :map "GB180302")

(define-charset 'gb18030-4-byte-bmp
  "GB18030 4-byte for BMP (0x81308130-0x8431A439)"
  :code-space [#x30 #x39 #x81 #xFE #x30 #x39 #x81 #x84]
  :supplementary-p t
  :map "GB180304")

(define-charset 'gb18030-4-byte-smp
  "GB18030 4-byte for SMP (0x90308130-0xE3329A35)"
  :code-space [#x30 #x39 #x81 #xFE #x30 #x39 #x90 #xE3]
  :min-code '(#x9030 . #x8130)
  :max-code '(#xE332 . #x9A35)
  :supplementary-p t
  :code-offset #x10000)

(define-charset 'gb18030-4-byte-ext-1
  "GB18030 4-byte (0x8431A530-0x8F39FE39)"
  :code-space [#x30 #x39 #x81 #xFE #x30 #x39 #x84 #x8F]
  :min-code '(#x8431 . #xA530)
  :max-code '(#x8F39 . #xFE39)
  :supplementary-p t
  :code-offset #x200000			; ... #x22484B
  )

(define-charset 'gb18030-4-byte-ext-2
  "GB18030 4-byte (0xE3329A36-0xFE39FE39)"
  :code-space [#x30 #x39 #x81 #xFE #x30 #x39 #xE3 #xFE]
  :min-code '(#xE332 . #x9A36)
  :max-code '(#xFE39 . #xFE39)
  :supplementary-p t
  :code-offset #x22484C			; ... #x279f93
  )

(define-charset 'gb18030
  "GB18030"
  :code-space [#x00 #xFF #x00 #xFE #x00 #xFE #x00 #xFE]
  :min-code 0
  :max-code '(#xFE39 . #xFE39)
  :superset '(ascii gb18030-2-byte
		    gb18030-4-byte-bmp gb18030-4-byte-smp
		    gb18030-4-byte-ext-1 gb18030-4-byte-ext-2))

(define-charset 'chinese-cns11643-15
  "CNS11643 Plane 15 Chinese Traditional"
  :short-name  "CNS11643-15"
  :long-name "CNS11643-15 (Chinese traditional)"
  :code-space [33 126 33 126]
  :code-offset #x27A000)

(unify-charset 'chinese-gb2312)
(unify-charset 'chinese-gbk)
(unify-charset 'chinese-cns11643-1)
(unify-charset 'chinese-cns11643-2)
(unify-charset 'chinese-cns11643-3)
(unify-charset 'chinese-cns11643-4)
(unify-charset 'chinese-cns11643-5)
(unify-charset 'chinese-cns11643-6)
(unify-charset 'chinese-cns11643-7)
(unify-charset 'big5)
(unify-charset 'chinese-big5-1)
(unify-charset 'chinese-big5-2)
(unify-charset 'big5-hkscs)
(unify-charset 'korean-ksc5601)
(unify-charset 'vietnamese-viscii-lower)
(unify-charset 'vietnamese-viscii-upper)
(unify-charset 'chinese-sisheng)
(unify-charset 'ipa)
(unify-charset 'tibetan)
(unify-charset 'ethiopic)
(unify-charset 'japanese-jisx0208-1978)
(unify-charset 'japanese-jisx0208)
(unify-charset 'japanese-jisx0212)
(unify-charset 'japanese-jisx0213-1)
(unify-charset 'japanese-jisx0213-2)


;; These are tables for translating characters on decoding and
;; encoding.
;; Fixme: these aren't used now -- should they be?
(setq standard-translation-table-for-decode nil)

(setq standard-translation-table-for-encode nil)

;;; Make fundamental coding systems.

;; The coding system `no-conversion' and `undecided' are already
;; defined in coding.c as below:
;;
;; (define-coding-system 'no-conversion
;;   "..."
;;   :coding-type 'raw-text
;;   ...)
;; (define-coding-system 'undecided
;;   "..."
;;   :coding-type 'undecided
;;   ...)

(define-coding-system-alias 'binary 'no-conversion)
(define-coding-system-alias 'unix 'undecided-unix)
(define-coding-system-alias 'dos 'undecided-dos)
(define-coding-system-alias 'mac 'undecided-mac)

(define-coding-system 'raw-text
  "Raw text, which means text contains random 8-bit codes.
Encoding text with this coding system produces the actual byte
sequence of the text in buffers and strings.  An exception is made for
characters from the `eight-bit' character set.  Each of them is encoded
into a single byte.

When you visit a file with this coding, the file is read into a
unibyte buffer as is (except for EOL format), thus each byte of a file
is treated as a character."
  :coding-type 'raw-text
  :for-unibyte t
  :mnemonic ?t)

(define-coding-system 'no-conversion-multibyte
  "Like `no-conversion' but don't read a file into a unibyte buffer."
  :coding-type 'raw-text
  :eol-type 'unix
  :mnemonic ?=)

(define-coding-system 'iso-latin-1
  "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)."
  :coding-type 'charset
  :mnemonic ?1
  :charset-list '(iso-8859-1)
  :mime-charset 'iso-8859-1)

(define-coding-system-alias 'iso-8859-1 'iso-latin-1)
(define-coding-system-alias 'latin-1 'iso-latin-1)

;; Coding systems not specific to each language environment.

(define-coding-system 'emacs-mule
 "Emacs 21 internal format used in buffer and string."
 :coding-type 'emacs-mule
 :charset-list 'emacs-mule
 :mnemonic ?M)

(define-coding-system 'utf-8
  "UTF-8 (no signature (BOM))"
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(unicode)
  :mime-charset 'utf-8)

(define-coding-system 'utf-8-with-signature
  "UTF-8 (with signature (BOM))"
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(unicode)
  :bom t)

(define-coding-system 'utf-8-auto
  "UTF-8 (auto-detect signature (BOM))"
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(unicode)
  :bom '(utf-8-with-signature . utf-8))

(define-coding-system-alias 'mule-utf-8 'utf-8)

(define-coding-system 'utf-8-emacs
  "Support for all Emacs characters (including non-Unicode characters)."
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(emacs))

;; The encoding used internally.  This encoding is meant to be able to save
;; any multibyte buffer without losing information.  It can change between
;; Emacs releases, tho, so should only be used for internal files.
(define-coding-system-alias 'emacs-internal 'utf-8-emacs-unix)

(define-coding-system 'utf-16le
  "UTF-16LE (little endian, no signature (BOM))."
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :endian 'little
  :mime-text-unsuitable t
  :mime-charset 'utf-16le)

(define-coding-system 'utf-16be
  "UTF-16BE (big endian, no signature (BOM))."
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :endian 'big
  :mime-text-unsuitable t
  :mime-charset 'utf-16be)

(define-coding-system 'utf-16le-with-signature
  "UTF-16 (little endian, with signature (BOM))."
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :bom t
  :endian 'little
  :mime-text-unsuitable t
  :mime-charset 'utf-16)

(define-coding-system 'utf-16be-with-signature
  "UTF-16 (big endian, with signature (BOM))."
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :bom t
  :endian 'big
  :mime-text-unsuitable t
  :mime-charset 'utf-16)

(define-coding-system 'utf-16
  "UTF-16 (detect endian on decoding, use big endian on encoding with BOM)."
  :coding-type 'utf-16
  :mnemonic ?U
  :charset-list '(unicode)
  :bom '(utf-16le-with-signature . utf-16be-with-signature)
  :endian 'big
  :mime-text-unsuitable t
  :mime-charset 'utf-16)

;; Backwards compatibility (old names, also used by Mule-UCS).  We
;; prefer the MIME names.
(define-coding-system-alias 'utf-16-le 'utf-16le-with-signature)
(define-coding-system-alias 'utf-16-be 'utf-16be-with-signature)


(define-coding-system 'iso-2022-7bit
  "ISO 2022 based 7-bit encoding using only G0."
  :coding-type 'iso-2022
  :mnemonic ?J
  :charset-list 'iso-2022
  :designation [(ascii t) nil nil nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit designation composition))

(define-coding-system 'iso-2022-7bit-ss2
  "ISO 2022 based 7-bit encoding using SS2 for 96-charset."
  :coding-type 'iso-2022
  :mnemonic ?$
  :charset-list 'iso-2022
  :designation [(ascii 94) nil (nil 96) nil]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit
		 designation single-shift composition))

(define-coding-system 'iso-2022-7bit-lock
  "ISO-2022 coding system using Locking-Shift for 96-charset."
  :coding-type 'iso-2022
  :mnemonic ?&
  :charset-list 'iso-2022
  :designation [(ascii 94) (nil 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl 7-bit
			designation locking-shift composition))

(define-coding-system-alias 'iso-2022-int-1 'iso-2022-7bit-lock)

(define-coding-system 'iso-2022-7bit-lock-ss2
  "Mixture of ISO-2022-JP, ISO-2022-KR, and ISO-2022-CN."
  :coding-type 'iso-2022
  :mnemonic ?i
  :charset-list '(ascii
		  japanese-jisx0208 japanese-jisx0208-1978 latin-jisx0201
		  korean-ksc5601
		  chinese-gb2312
		  chinese-cns11643-1 chinese-cns11643-2 chinese-cns11643-3
		  chinese-cns11643-4 chinese-cns11643-5 chinese-cns11643-6
		  chinese-cns11643-7)
  :designation [(ascii 94)
		(nil korean-ksc5601 chinese-gb2312 chinese-cns11643-1 96)
		(nil chinese-cns11643-2)
		(nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
		     chinese-cns11643-6 chinese-cns11643-7)]
  :flags '(short ascii-at-eol ascii-at-cntl 7-bit locking-shift
		 single-shift init-bol))

(define-coding-system-alias 'iso-2022-cjk 'iso-2022-7bit-lock-ss2)

(define-coding-system 'iso-2022-8bit-ss2
  "ISO 2022 based 8-bit encoding using SS2 for 96-charset."
  :coding-type 'iso-2022
  :mnemonic ?@
  :charset-list 'iso-2022
  :designation [(ascii 94) nil (nil 96) nil]
  :flags '(ascii-at-eol ascii-at-cntl designation single-shift composition))

(define-coding-system 'compound-text
  "Compound text based generic encoding.
This coding system is an extension of X's \"Compound Text Encoding\".
It encodes many characters using the normal ISO-2022 designation sequences,
but it doesn't support extended segments of CTEXT."
  :coding-type 'iso-2022
  :mnemonic ?x
  :charset-list 'iso-2022
  :designation [(ascii 94) (latin-iso8859-1 katakana-jisx0201 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl long-form
			designation locking-shift single-shift composition)
  ;; Fixme: this isn't a valid MIME charset and has to be
  ;; special-cased elsewhere  -- fx
  :mime-charset 'x-ctext)

(define-coding-system-alias  'x-ctext 'compound-text)
(define-coding-system-alias  'ctext 'compound-text)

;; Same as compound-text, but doesn't produce composition escape
;; sequences.  Used in post-read and pre-write conversions of
;; compound-text-with-extensions, see mule.el.  Note that this should
;; not have a mime-charset property, to prevent it from showing up
;; close to the beginning of coding systems ordered by priority.
(define-coding-system 'ctext-no-compositions
 "Compound text based generic encoding.

Like `compound-text', but does not produce escape sequences for compositions."
  :coding-type 'iso-2022
  :mnemonic ?x
  :charset-list 'iso-2022
  :designation [(ascii 94) (latin-iso8859-1 katakana-jisx0201 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl
			designation locking-shift single-shift))

(define-coding-system 'compound-text-with-extensions
 "Compound text encoding with ICCCM Extended Segment extensions.

See the variables `ctext-standard-encodings' and
`ctext-non-standard-encodings-alist' for the detail about how
extended segments are handled.

This coding system should be used only for X selections.  It is inappropriate
for decoding and encoding files, process I/O, etc."
  :coding-type 'iso-2022
  :mnemonic ?x
  :charset-list 'iso-2022
  :designation [(ascii 94) (latin-iso8859-1 katakana-jisx0201 96) nil nil]
  :flags '(ascii-at-eol ascii-at-cntl long-form
			designation locking-shift single-shift)
  :post-read-conversion 'ctext-post-read-conversion
  :pre-write-conversion 'ctext-pre-write-conversion)

(define-coding-system-alias
  'x-ctext-with-extensions 'compound-text-with-extensions)
(define-coding-system-alias
  'ctext-with-extensions 'compound-text-with-extensions)

(define-coding-system 'us-ascii
  "Encode ASCII as-is and encode non-ASCII characters to `?'."
  :coding-type 'charset
  :mnemonic ?-
  :charset-list '(ascii)
  :default-char ??
  :mime-charset 'us-ascii)

(define-coding-system-alias 'iso-safe 'us-ascii)

(define-coding-system 'utf-7
  "UTF-7 encoding of Unicode (RFC 2152)."
  :coding-type 'utf-8
  :mnemonic ?U
  :mime-charset 'utf-7
  :charset-list '(unicode)
  :pre-write-conversion 'utf-7-pre-write-conversion
  :post-read-conversion 'utf-7-post-read-conversion)

(define-coding-system 'utf-7-imap
  "UTF-7 encoding of Unicode, IMAP version (RFC 2060)"
  :coding-type 'utf-8
  :mnemonic ?u
  :charset-list '(unicode)
  :pre-write-conversion 'utf-7-imap-pre-write-conversion
  :post-read-conversion 'utf-7-imap-post-read-conversion)

;; Use us-ascii for terminal output if some other coding system is not
;; specified explicitly.
(set-safe-terminal-coding-system-internal 'us-ascii)

;; The other coding-systems are defined in each language specific
;; files under lisp/language.

;; Normally, set coding system to `undecided' before reading a file.
;; Compiled Emacs Lisp files (*.elc) are not decoded at all,
;; but we regard them as containing multibyte characters.
;; Tar files are not decoded at all, but we treat them as raw bytes.

(setq file-coding-system-alist
      (mapcar (lambda (arg) (cons (purecopy (car arg)) (cdr arg)))
      '(("\\.elc\\'" . utf-8-emacs)
	("\\.utf\\(-8\\)?\\'" . utf-8)
	("\\.xml\\'" . xml-find-file-coding-system)
	;; We use raw-text for reading loaddefs.el so that if it
	;; happens to have DOS or Mac EOLs, they are converted to
	;; newlines.  This is required to make the special treatment
	;; of the "\ newline" combination in loaddefs.el, which marks
	;; the beginning of a doc string, work.
	("\\(\\`\\|/\\)loaddefs.el\\'" . (raw-text . raw-text-unix))
	("\\.tar\\'" . (no-conversion . no-conversion))
	( "\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
	("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system)
	("" . (undecided . nil)))))


;;; Setting coding categories and their priorities.

;; This setting is just to read an Emacs Lisp source files which
;; contain multilingual text while dumping Emacs.  More appropriate
;; values are set by the command `set-language-environment' for each
;; language environment.

(set-coding-system-priority
 'iso-latin-1
 'utf-8
 'iso-2022-7bit
 )


;;; Miscellaneous settings.

;; Make all multibyte characters self-insert.
(set-char-table-range (nth 1 global-map)
		      (cons 128 (max-char))
		      'self-insert-command)

(aset latin-extra-code-table ?\221 t)
(aset latin-extra-code-table ?\222 t)
(aset latin-extra-code-table ?\223 t)
(aset latin-extra-code-table ?\224 t)
(aset latin-extra-code-table ?\225 t)
(aset latin-extra-code-table ?\226 t)

;; The old code-pages library is obsoleted by coding systems based on
;; the charsets defined in this file but might be required by user
;; code.
(provide 'code-pages)

;;; mule-conf.el ends here
