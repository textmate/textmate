;;; characters.el --- set syntax and category for multibyte characters

;; Copyright (C) 1997, 2000-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
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

;;; Code:

;;; Predefined categories.

;; For each character set.

(define-category ?a "ASCII
ASCII graphic characters 32-126 (ISO646 IRV:1983[4/0])")
(define-category ?l "Latin")
(define-category ?t "Thai")
(define-category ?g "Greek")
(define-category ?b "Arabic")
(define-category ?w "Hebrew")
(define-category ?y "Cyrillic")
(define-category ?k "Katakana
Japanese katakana")
(define-category ?r "Roman
Japanese roman")
(define-category ?c "Chinese")
(define-category ?j "Japanese")
(define-category ?h "Korean")
(define-category ?e "Ethiopic
Ethiopic (Ge'ez)")
(define-category ?v "Viet
Vietnamese")
(define-category ?i "Indian")
(define-category ?o "Lao")
(define-category ?q "Tibetan")

;; For each group (row) of 2-byte character sets.

(define-category ?A "2-byte alnum
Alpha-numeric characters of 2-byte character sets")
(define-category ?C "2-byte han
Chinese (Han) characters of 2-byte character sets")
(define-category ?G "2-byte Greek
Greek characters of 2-byte character sets")
(define-category ?H "2-byte Hiragana
Japanese Hiragana characters of 2-byte character sets")
(define-category ?K "2-byte Katakana
Japanese Katakana characters of 2-byte character sets")
(define-category ?N "2-byte Korean
Korean Hangul characters of 2-byte character sets")
(define-category ?Y "2-byte Cyrillic
Cyrillic characters of 2-byte character sets")
(define-category ?I "Indian Glyphs")

;; For phonetic classifications.

(define-category ?0 "consonant")
(define-category ?1 "base vowel
Base (independent) vowel")
(define-category ?2 "upper diacritic
Upper diacritical mark (including upper vowel)")
(define-category ?3 "lower diacritic
Lower diacritical mark (including lower vowel)")
(define-category ?4 "combining tone
Combining tone mark")
(define-category ?5 "symbol")
(define-category ?6 "digit")
(define-category ?7 "vowel diacritic
Vowel-modifying diacritical mark")
(define-category ?8 "vowel-signs")
(define-category ?9 "semivowel lower")

;; For filling.
(define-category ?| "line breakable
While filling, we can break a line at this character.")

;; For indentation calculation.
(define-category ?\s
  "space for indent
This character counts as a space for indentation purposes.")

;; Keep the following for `kinsoku' processing.  See comments in
;; kinsoku.el.
(define-category ?> "Not at bol
A character which can't be placed at beginning of line.")
(define-category ?< "Not at eol
A character which can't be placed at end of line.")

;; Base and Combining
(define-category ?. "Base
Base characters (Unicode General Category L,N,P,S,Zs)")
(define-category ?^ "Combining
Combining diacritic or mark (Unicode General Category M)")

;; bidi types
(define-category ?R "Right-to-left (strong)
Characters with \"strong\" right-to-left directionality, i.e.
with R, AL, RLE, or RLO Unicode bidi character type.")

(define-category ?L "Left-to-right (strong)
Characters with \"strong\" left-to-right directionality, i.e.
with L, LRE, or LRO Unicode bidi character type.")


;;; Setting syntax and category.

;; ASCII

;; All ASCII characters have the category `a' (ASCII) and `l' (Latin).
(modify-category-entry '(32 . 127) ?a)
(modify-category-entry '(32 . 127) ?l)

;; Deal with the CJK charsets first.  Since the syntax of blocks is
;; defined per charset, and the charsets may contain e.g. Latin
;; characters, we end up with the wrong syntax definitions if we're
;; not careful.

;; Chinese characters (Unicode)
(modify-category-entry '(#x2E80 . #x312F) ?|)
(modify-category-entry '(#x3190 . #x33FF) ?|)
(modify-category-entry '(#x3400 . #x4DBF) ?C)
(modify-category-entry '(#x4E00 . #x9FAF) ?C)
(modify-category-entry '(#x3400 . #x9FAF) ?c)
(modify-category-entry '(#x3400 . #x9FAF) ?|)
(modify-category-entry '(#xF900 . #xFAFF) ?C)
(modify-category-entry '(#xF900 . #xFAFF) ?c)
(modify-category-entry '(#xF900 . #xFAFF) ?|)
(modify-category-entry '(#x20000 . #x2FFFF) ?|)
(modify-category-entry '(#x20000 . #x2FFFF) ?C)
(modify-category-entry '(#x20000 . #x2FFFF) ?c)


;; Chinese character set (GB2312)

(map-charset-chars #'modify-syntax-entry 'chinese-gb2312 "_" #x2121 #x217E)
(map-charset-chars #'modify-syntax-entry 'chinese-gb2312 "_" #x2221 #x227E)
(map-charset-chars #'modify-syntax-entry 'chinese-gb2312 "_" #x2921 #x297E)

(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?c)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?A #x2330 #x2339)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?A #x2341 #x235A)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?A #x2361 #x237A)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?H #x2421 #x247E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?K #x2521 #x257E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?G #x2621 #x267E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?Y #x2721 #x277E)
(map-charset-chars #'modify-category-entry 'chinese-gb2312 ?C #x3021 #x7E7E)

;; Chinese character set (BIG5)

(map-charset-chars #'modify-category-entry 'big5 ?c)
(map-charset-chars #'modify-category-entry 'big5 ?C #xA259 #xA261)
(map-charset-chars #'modify-category-entry 'big5 ?C #xA440 #xC67E)
(map-charset-chars #'modify-category-entry 'big5 ?C #xC940 #xF9DC)

;; Chinese character set (CNS11643)

(dolist (c '(chinese-cns11643-1 chinese-cns11643-2 chinese-cns11643-3
	     chinese-cns11643-4 chinese-cns11643-5 chinese-cns11643-6
	     chinese-cns11643-7))
  (map-charset-chars #'modify-category-entry c ?c)
  (if (eq c 'chinese-cns11643-1)
      (map-charset-chars #'modify-category-entry c ?C #x4421 #x7E7E)
    (map-charset-chars #'modify-category-entry c ?C)))

;; Japanese character set (JISX0201, JISX0208, JISX0212, JISX0213)

(map-charset-chars #'modify-category-entry 'katakana-jisx0201 ?k)

(map-charset-chars #'modify-category-entry 'latin-jisx0201 ?r)

(dolist (l '(katakana-jisx0201 japanese-jisx0208 japanese-jisx0212
			       japanese-jisx0213-1 japanese-jisx0213-2
			       cp932-2-byte))
  (map-charset-chars #'modify-category-entry l ?j))

;; Fullwidth characters
(modify-category-entry '(#xff01 . #xff60) ?\|)

;; Unicode equivalents of JISX0201-kana
(let ((range '(#xff61 . #xff9f)))
  (modify-category-entry range  ?k)
  (modify-category-entry range ?j)
  (modify-category-entry range ?\|))

;; Katakana block
(modify-category-entry '(#x3099 . #x309C) ?K)
(modify-category-entry '(#x30A0 . #x30FF) ?K)
(modify-category-entry '(#x31F0 . #x31FF) ?K)
(modify-category-entry '(#x30A0 . #x30FA) ?\|)
(modify-category-entry #x30FF ?\|)

;; Hiragana block
(modify-category-entry '(#x3040 . #x309F) ?H)
(modify-category-entry '(#x3040 . #x3096) ?\|)
(modify-category-entry #x309F ?\|)
(modify-category-entry #x30A0 ?H)
(modify-category-entry #x30FC ?H)


;; JISX0208
(map-charset-chars #'modify-syntax-entry 'japanese-jisx0208 "_" #x2121 #x227E)
(map-charset-chars #'modify-syntax-entry 'japanese-jisx0208 "_" #x2821 #x287E)
(let ((chars '(?ー ?゛ ?゜ ?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)))
  (dolist (elt chars)
    (modify-syntax-entry (car chars) "w")))

(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?A #x2321 #x237E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?H #x2421 #x247E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?K #x2521 #x257E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?G #x2621 #x267E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?Y #x2721 #x277E)
(map-charset-chars #'modify-category-entry 'japanese-jisx0208 ?C #x3021 #x7E7E)
(modify-category-entry ?ー ?K)
(let ((chars '(?゛ ?゜)))
  (while chars
    (modify-category-entry (car chars) ?K)
    (modify-category-entry (car chars) ?H)
    (setq chars (cdr chars))))
(let ((chars '(?仝 ?々 ?〆 ?〇)))
  (while chars
    (modify-category-entry (car chars) ?C)
    (setq chars (cdr chars))))

;; JISX0212

(map-charset-chars #'modify-syntax-entry 'japanese-jisx0212 "_" #x2121 #x237E)

;; JISX0201-Kana

(let ((chars '(?｡ ?､ ?･)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))

(modify-syntax-entry ?\｢ "(｣")
(modify-syntax-entry ?\｣ "(｢")

;; Korean character set (KSC5601)

(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?h)

(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2121 #x227E)
(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2621 #x277E)
(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2830 #x287E)
(map-charset-chars #'modify-syntax-entry 'korean-ksc5601 "_" #x2930 #x297E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?A #x2330 #x2339)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?A #x2341 #x235A)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?A #x2361 #x237A)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?G #x2521 #x257E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?H #x2A21 #x2A7E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?K #x2B21 #x2B7E)
(map-charset-chars #'modify-category-entry 'korean-ksc5601 ?Y #x2C21 #x2C7E)

;; These are in more than one charset.
(let ((parens (concat "〈〉《》「」『』【】〔〕〖〗〘〙〚〛"
		      "︵︶︷︸︹︺︻︼︽︾︿﹀﹁﹂﹃﹄"
		      "（）［］｛｝"))
      open close)
  (dotimes (i (/ (length parens) 2))
    (setq open (aref parens (* i 2))
	  close (aref parens (1+ (* i 2))))
    (modify-syntax-entry open (format "(%c" close))
    (modify-syntax-entry close (format ")%c" open))))

;; Arabic character set

(let ((charsets '(arabic-iso8859-6
		  arabic-digit
		  arabic-1-column
		  arabic-2-column)))
  (while charsets
    (map-charset-chars #'modify-category-entry (car charsets) ?b)
    (setq charsets (cdr charsets))))
(modify-category-entry '(#x600 . #x6ff) ?b)
(modify-category-entry '(#xfb50 . #xfdff) ?b)
(modify-category-entry '(#xfe70 . #xfefe) ?b)

;; Cyrillic character set (ISO-8859-5)

(modify-syntax-entry ?№ ".")

;; Ethiopic character set

(modify-category-entry '(#x1200 . #x1399) ?e)
(modify-category-entry '(#x2d80 . #x2dde) ?e)
(let ((chars '(?፡ ?። ?፣ ?፤ ?፥ ?፦ ?፧ ?፨)))
  (while chars
    (modify-syntax-entry (car chars) ".")
    (setq chars (cdr chars))))
(map-charset-chars #'modify-category-entry 'ethiopic ?e)

;; Hebrew character set (ISO-8859-8)

(modify-syntax-entry #x5be ".") ; MAQAF
(modify-syntax-entry #x5c0 ".") ; PASEQ
(modify-syntax-entry #x5c3 ".") ; SOF PASUQ
(modify-syntax-entry #x5f3 ".") ; GERESH
(modify-syntax-entry #x5f4 ".") ; GERSHAYIM

;; Indian character set (IS 13194 and other Emacs original Indian charsets)

(modify-category-entry '(#x901 . #x970) ?i)
(map-charset-chars #'modify-category-entry 'indian-is13194 ?i)
(map-charset-chars #'modify-category-entry 'indian-2-column ?i)

;; Lao character set

(modify-category-entry '(#xe80 . #xeff) ?o)
(map-charset-chars #'modify-category-entry 'lao ?o)

(let ((deflist	'(("ກ-ຮ"	"w"	?0) ; consonant
		  ("ະາຳຽເ-ໄ"	"w"	?1) ; vowel base
		  ("ັິ-ືົໍ"	"w"	?2) ; vowel upper
		  ("ຸູ"	"w"	?3) ; vowel lower
		  ("່-໋"	"w"	?4) ; tone mark
		  ("ຼຽ"	"w"	?9) ; semivowel lower
		  ("໐-໙"	"w"	?6) ; digit
		  ("ຯໆ"	"_"	?5) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Thai character set (TIS620)

(modify-category-entry '(#xe00 . #xe7f) ?t)
(map-charset-chars #'modify-category-entry 'thai-tis620 ?t)

(let ((deflist	'(;; chars	syntax	category
		  ("ก-รลว-ฮ"	"w"	?0) ; consonant
		  ("ฤฦะาำเ-ๅ"	"w"	?1) ; vowel base
		  ("ัิ-ื็๎"	"w"	?2) ; vowel upper
		  ("ุ-ฺ"	"w"	?3) ; vowel lower
		  ("่-ํ"	"w"	?4) ; tone mark
		  ("๐-๙"	"w"	?6) ; digit
		  ("ฯๆ฿๏๚๛"	"_"	?5) ; symbol
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Tibetan character set

(modify-category-entry '(#xf00 . #xfff) ?q)
(map-charset-chars #'modify-category-entry 'tibetan ?q)
(map-charset-chars #'modify-category-entry 'tibetan-1-column ?q)

(let ((deflist	'(;; chars             syntax category
		  ("ཀ-ཀྵཪ"        	"w"	?0) ; consonant
		  ("ྐ-ྐྵྺྻྼ"       "w"     ?0) ;
		  ("ིེཻོཽྀ"       "w"	?2) ; upper vowel
		  ("ཾྂྃ྆྇ྈྉྊྋ" "w"	?2) ; upper modifier
		  ("྄ཱུ༙༵༷"       "w"	?3) ; lower vowel/modifier
		  ("཰"		"w" ?3)		    ; invisible vowel a
		  ("༠-༩༪-༳"	        "w"	?6) ; digit
		  ("་།-༒༔ཿ"        "."     ?|) ; line-break char
		  ("་།༏༐༑༔ཿ"            "."     ?|) ;
		  ("༈་།-༒༔ཿ༽༴"  "."     ?>) ; prohibition
		  ("་།༏༐༑༔ཿ"            "."     ?>) ;
		  ("ༀ-༊༼࿁࿂྅"      "."     ?<) ; prohibition
		  ("༓༕-༘༚-༟༶༸-༻༾༿྾྿-࿏" "." ?q) ; others
		  ))
      elm chars len syntax category to ch i)
  (while deflist
    (setq elm (car deflist))
    (setq chars (car elm)
	  len (length chars)
	  syntax (nth 1 elm)
	  category (nth 2 elm)
	  i 0)
    (while (< i len)
      (if (= (aref chars i) ?-)
	  (setq i (1+ i)
		to (aref chars i))
	(setq ch (aref chars i)
	      to ch))
      (while (<= ch to)
	(unless (string-equal syntax "w")
	  (modify-syntax-entry ch syntax))
	(modify-category-entry ch category)
	(setq ch (1+ ch)))
      (setq i (1+ i)))
    (setq deflist (cdr deflist))))

;; Vietnamese character set

;; To make a word with Latin characters
(map-charset-chars #'modify-category-entry 'vietnamese-viscii-lower ?l)
(map-charset-chars #'modify-category-entry 'vietnamese-viscii-lower ?v)

(map-charset-chars #'modify-category-entry 'vietnamese-viscii-upper ?l)
(map-charset-chars #'modify-category-entry 'vietnamese-viscii-upper ?v)

(let ((tbl (standard-case-table))
      (i 32))
  (while (< i 128)
    (let* ((char (decode-char 'vietnamese-viscii-upper i))
	   (charl (decode-char 'vietnamese-viscii-lower i))
	   (uc (encode-char char 'ucs))
	   (lc (encode-char charl 'ucs)))
      (set-case-syntax-pair char (decode-char 'vietnamese-viscii-lower i)
			    tbl)
      (if uc (modify-category-entry uc ?v))
      (if lc (modify-category-entry lc ?v)))
    (setq i (1+ i))))

;; Tai Viet
(let ((deflist '(;; chars	syntax	category
		 ((?ꪀ.  ?ꪯ)	"w"	?0) ; consonant
		 ("ꪱꪵꪶ"		"w"	?1) ; vowel base
		 ((?ꪹ . ?ꪽ)	"w"	?1) ; vowel base
		 ("ꪰꪲꪳꪷꪸꪾ"	"w"	?2) ; vowel upper
		 ("ꪴ"		"w"	?3) ; vowel lower
		 ("ꫀꫂ"		"w"	?1) ; non-combining tone-mark
		 ("꪿꫁"		"w"	?4) ; combining tone-mark
		 ((?ꫛ . ?꫟)	"_"	?5) ; symbol
		 )))
  (dolist (elm deflist)
    (let ((chars (car elm))
	  (syntax (nth 1 elm))
	  (category (nth 2 elm)))
      (if (consp chars)
	  (progn
	    (modify-syntax-entry chars syntax)
	    (modify-category-entry chars category))
	(mapc #'(lambda (x)
		  (modify-syntax-entry x syntax)
		  (modify-category-entry x category))
	      chars)))))

;; Bidi categories

(map-char-table (lambda (key val)
		  (cond
		   ((memq val '(R AL RLO RLE))
		    (modify-category-entry key ?R))
		   ((memq val '(L LRE LRO))
		    (modify-category-entry key ?L))))
		(unicode-property-table-internal 'bidi-class))

;; Latin

(modify-category-entry '(#x80 . #x024F) ?l)

(let ((tbl (standard-case-table)) c)

  ;; Latin-1

  ;; Fixme: Some of the non-word syntaxes here perhaps should be
  ;; reviewed.  (Note that the following all implicitly have word
  ;; syntax: ¢£¤¥¨ª¯²³´¶¸¹º.)  There should be a well-defined way of
  ;; relating Unicode categories to Emacs syntax codes.

  ;; NBSP isn't semantically interchangeable with other whitespace chars,
  ;; so it's more like punctuation.
  (set-case-syntax ?  "." tbl)
  (set-case-syntax ?¡ "." tbl)
  (set-case-syntax ?¦ "_" tbl)
  (set-case-syntax ?§ "." tbl)
  (set-case-syntax ?© "_" tbl)
  (set-case-syntax-delims 171 187 tbl)	; « »
  (set-case-syntax ?¬ "_" tbl)
  (set-case-syntax ?­ "_" tbl)
  (set-case-syntax ?® "_" tbl)
  (set-case-syntax ?° "_" tbl)
  (set-case-syntax ?± "_" tbl)
  (set-case-syntax ?µ "_" tbl)
  (set-case-syntax ?· "_" tbl)
  (set-case-syntax ?¼ "_" tbl)
  (set-case-syntax ?½ "_" tbl)
  (set-case-syntax ?¾ "_" tbl)
  (set-case-syntax ?¿ "." tbl)
  (let ((c 192))
    (while (<= c 222)
      (set-case-syntax-pair c (+ c 32) tbl)
      (setq c (1+ c))))
  (set-case-syntax ?× "_" tbl)
  (set-case-syntax ?ß "w" tbl)
  (set-case-syntax ?÷ "_" tbl)
  ;; See below for ÿ.

  ;; Latin Extended-A, Latin Extended-B
  (setq c #x0100)
  (while (<= c #x02B8)
    (modify-category-entry c ?l)
    (setq c (1+ c)))

  (let ((pair-ranges '((#x0100 . #x012F)
		       (#x0132 . #x0137)
		       (#x0139 . #x0148)
		       (#x014a . #x0177)
		       (#x0179 . #x017E)
		       (#x0182 . #x0185)
		       (#x0187 . #x0188)
		       (#x018B . #x018C)
		       (#x0191 . #x0192)
		       (#x0198 . #x0199)
		       (#x01A0 . #x01A5)
		       (#x01A7 . #x01A8)
		       (#x01AC . #x01AD)
		       (#x01AF . #x01B0)
		       (#x01B3 . #x01B6)
		       (#x01B8 . #x01B9)
		       (#x01BC . #x01BD)
		       (#x01CD . #x01DC)
		       (#x01DE . #x01EF)
		       (#x01F4 . #x01F5)
		       (#x01F8 . #x021F)
		       (#x0222 . #x0233)
		       (#x023B . #x023C)
		       (#x0241 . #x0242)
		       (#x0246 . #x024F))))
    (dolist (elt pair-ranges)
      (let ((from (car elt)) (to (cdr elt)))
	(while (< from to)
	  (set-case-syntax-pair from (1+ from) tbl)
	  (setq from (+ from 2))))))

  (set-case-syntax-pair ?Ÿ ?ÿ tbl)

  ;; In some languages, such as Turkish, U+0049 LATIN CAPITAL LETTER I
  ;; and U+0131 LATIN SMALL LETTER DOTLESS I make a case pair, and so
  ;; do U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE and U+0069 LATIN
  ;; SMALL LETTER I.

  ;; We used to set up half of those correspondence unconditionally,
  ;; but that makes searches slow.  So now we don't set up either half
  ;; of these correspondences by default.

  ;; (set-downcase-syntax  ?İ ?i tbl)
  ;; (set-upcase-syntax    ?I ?ı tbl)

  (set-case-syntax-pair ?Ɓ ?ɓ tbl)
  (set-case-syntax-pair ?Ɔ ?ɔ tbl)
  (set-case-syntax-pair ?Ɖ ?ɖ tbl)
  (set-case-syntax-pair ?Ɗ ?ɗ tbl)
  (set-case-syntax-pair ?Ǝ ?ǝ tbl)
  (set-case-syntax-pair ?Ə ?ə tbl)
  (set-case-syntax-pair ?Ɛ ?ɛ tbl)
  (set-case-syntax-pair ?Ɠ ?ɠ tbl)
  (set-case-syntax-pair ?Ɣ ?ɣ tbl)
  (set-case-syntax-pair ?Ɩ ?ɩ tbl)
  (set-case-syntax-pair ?Ɨ ?ɨ tbl)
  (set-case-syntax-pair ?Ɯ ?ɯ tbl)
  (set-case-syntax-pair ?Ɲ ?ɲ tbl)
  (set-case-syntax-pair ?Ɵ ?ɵ tbl)
  (set-case-syntax-pair ?Ʀ ?ʀ tbl)
  (set-case-syntax-pair ?Ʃ ?ʃ tbl)
  (set-case-syntax-pair ?Ʈ ?ʈ tbl)
  (set-case-syntax-pair ?Ʊ ?ʊ tbl)
  (set-case-syntax-pair ?Ʋ ?ʋ tbl)
  (set-case-syntax-pair ?Ʒ ?ʒ tbl)
  (set-case-syntax-pair ?Ǆ ?ǆ tbl)
  (set-case-syntax-pair ?ǅ ?ǆ tbl)
  (set-case-syntax-pair ?Ǉ ?ǉ tbl)
  (set-case-syntax-pair ?ǈ ?ǉ tbl)
  (set-case-syntax-pair ?Ǌ ?ǌ tbl)
  (set-case-syntax-pair ?ǋ ?ǌ tbl)

  ;; 01F0; F; 006A 030C; # LATIN SMALL LETTER J WITH CARON
  (set-case-syntax-pair ?Ǳ ?ǳ tbl)
  (set-case-syntax-pair ?ǲ ?ǳ tbl)
  (set-case-syntax-pair ?Ƕ ?ƕ tbl)
  (set-case-syntax-pair ?Ƿ ?ƿ tbl)
  (set-case-syntax-pair ?Ⱥ ?ⱥ tbl)
  (set-case-syntax-pair ?Ƚ ?ƚ tbl)
  (set-case-syntax-pair ?Ⱦ ?ⱦ tbl)
  (set-case-syntax-pair ?Ƀ ?ƀ tbl)
  (set-case-syntax-pair ?Ʉ ?ʉ tbl)
  (set-case-syntax-pair ?Ʌ ?ʌ tbl)

  ;; Latin Extended Additional
  (modify-category-entry '(#x1e00 . #x1ef9) ?l)
  (setq c #x1e00)
  (while (<= c #x1ef9)
    (and (zerop (% c 2))
	 (or (<= c #x1e94) (>= c #x1ea0))
	 (set-case-syntax-pair c (1+ c) tbl))
    (setq c (1+ c)))

  ;; Greek
  (modify-category-entry '(#x0370 . #x03ff) ?g)
  (setq c #x0370)
  (while (<= c #x03ff)
    (if (or (and (>= c #x0391) (<= c #x03a1))
	    (and (>= c #x03a3) (<= c #x03ab)))
	(set-case-syntax-pair c (+ c 32) tbl))
    (and (>= c #x03da)
	 (<= c #x03ee)
	 (zerop (% c 2))
	 (set-case-syntax-pair c (1+ c) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ά ?ά tbl)
  (set-case-syntax-pair ?Έ ?έ tbl)
  (set-case-syntax-pair ?Ή ?ή tbl)
  (set-case-syntax-pair ?Ί ?ί tbl)
  (set-case-syntax-pair ?Ό ?ό tbl)
  (set-case-syntax-pair ?Ύ ?ύ tbl)
  (set-case-syntax-pair ?Ώ ?ώ tbl)

  ;; Armenian
  (setq c #x531)
  (while (<= c #x556)
    (set-case-syntax-pair c (+ c #x30) tbl)
    (setq c (1+ c)))

  ;; Greek Extended
  (modify-category-entry '(#x1f00 . #x1fff) ?g)
  (setq c #x1f00)
  (while (<= c #x1fff)
    (and (<= (logand c #x000f) 7)
	 (<= c #x1fa7)
	 (not (memq c '(#x1f16 #x1f17 #x1f56 #x1f57
			       #x1f50 #x1f52 #x1f54 #x1f56)))
	 (/= (logand c #x00f0) #x70)
	 (set-case-syntax-pair (+ c 8) c tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ᾰ ?ᾰ tbl)
  (set-case-syntax-pair ?Ᾱ ?ᾱ tbl)
  (set-case-syntax-pair ?Ὰ ?ὰ tbl)
  (set-case-syntax-pair ?Ά ?ά tbl)
  (set-case-syntax-pair ?ᾼ ?ᾳ tbl)
  (set-case-syntax-pair ?Ὲ ?ὲ tbl)
  (set-case-syntax-pair ?Έ ?έ tbl)
  (set-case-syntax-pair ?Ὴ ?ὴ tbl)
  (set-case-syntax-pair ?Ή ?ή tbl)
  (set-case-syntax-pair ?ῌ ?ῃ tbl)
  (set-case-syntax-pair ?Ῐ ?ῐ tbl)
  (set-case-syntax-pair ?Ῑ ?ῑ tbl)
  (set-case-syntax-pair ?Ὶ ?ὶ tbl)
  (set-case-syntax-pair ?Ί ?ί tbl)
  (set-case-syntax-pair ?Ῠ ?ῠ tbl)
  (set-case-syntax-pair ?Ῡ ?ῡ tbl)
  (set-case-syntax-pair ?Ὺ ?ὺ tbl)
  (set-case-syntax-pair ?Ύ ?ύ tbl)
  (set-case-syntax-pair ?Ῥ ?ῥ tbl)
  (set-case-syntax-pair ?Ὸ ?ὸ tbl)
  (set-case-syntax-pair ?Ό ?ό tbl)
  (set-case-syntax-pair ?Ὼ ?ὼ tbl)
  (set-case-syntax-pair ?Ώ ?ώ tbl)
  (set-case-syntax-pair ?ῼ ?ῳ tbl)

  ;; cyrillic
  (modify-category-entry '(#x0400 . #x04FF) ?y)
  (setq c #x0400)
  (while (<= c #x04ff)
    (and (>= c #x0400)
	 (<= c #x040f)
	 (set-case-syntax-pair c (+ c 80) tbl))
    (and (>= c #x0410)
	 (<= c #x042f)
	 (set-case-syntax-pair c (+ c 32) tbl))
    (and (zerop (% c 2))
	 (or (and (>= c #x0460) (<= c #x0480))
	     (and (>= c #x048c) (<= c #x04be))
	     (and (>= c #x04d0) (<= c #x04f4)))
	 (set-case-syntax-pair c (1+ c) tbl))
    (setq c (1+ c)))
  (set-case-syntax-pair ?Ӂ ?ӂ tbl)
  (set-case-syntax-pair ?Ӄ ?ӄ tbl)
  (set-case-syntax-pair ?Ӈ ?ӈ tbl)
  (set-case-syntax-pair ?Ӌ ?ӌ tbl)
  (set-case-syntax-pair ?Ӹ ?ӹ tbl)

  ;; general punctuation
  (setq c #x2000)
  (while (<= c #x200b)
    (set-case-syntax c " " tbl)
    (setq c (1+ c)))
  (while (<= c #x200F)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Fixme: These aren't all right:
  (setq c #x2010)
  (while (<= c #x2016)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))
  ;; Punctuation syntax for quotation marks (like `)
  (while (<= c #x201f)
    (set-case-syntax  c "." tbl)
    (setq c (1+ c)))
  ;; Fixme: These aren't all right:
  (while (<= c #x2027)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))
  (while (<= c #x206F)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))

  ;; Roman numerals
  (setq c #x2160)
  (while (<= c #x216f)
    (set-case-syntax-pair c (+ c #x10) tbl)
    (setq c (1+ c)))

  ;; Fixme: The following blocks might be better as symbol rather than
  ;; punctuation.
  ;; Arrows
  (setq c #x2190)
  (while (<= c #x21FF)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Mathematical Operators
  (while (<= c #x22FF)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Miscellaneous Technical
  (while (<= c #x23FF)
    (set-case-syntax c "." tbl)
    (setq c (1+ c)))
  ;; Control Pictures
  (while (<= c #x243F)
    (set-case-syntax c "_" tbl)
    (setq c (1+ c)))

  ;; Circled Latin
  (setq c #x24b6)
  (while (<= c #x24cf)
    (set-case-syntax-pair c (+ c 26) tbl)
    (modify-category-entry c ?l)
    (modify-category-entry (+ c 26) ?l)
    (setq c (1+ c)))

  ;; Fullwidth Latin
  (setq c #xff21)
  (while (<= c #xff3a)
    (set-case-syntax-pair c (+ c #x20) tbl)
    (modify-category-entry c ?l)
    (modify-category-entry (+ c #x20) ?l)
    (setq c (1+ c)))

  ;; Combining diacritics
  (modify-category-entry '(#x300 . #x362) ?^)
  ;; Combining marks
  (modify-category-entry '(#x20d0 . #x20e3) ?^)

  ;; Fixme: syntax for symbols &c
  )

(let ((pairs
       '("⁅⁆"				; U+2045 U+2046
	 "⁽⁾"				; U+207D U+207E
	 "₍₎"				; U+208D U+208E
	 "〈〉"				; U+2329 U+232A
	 "⎴⎵"				; U+23B4 U+23B5
	 "❨❩"				; U+2768 U+2769
	 "❪❫"				; U+276A U+276B
	 "❬❭"				; U+276C U+276D
	 "❰❱"				; U+2770 U+2771
	 "❲❳"				; U+2772 U+2773
	 "❴❵"				; U+2774 U+2775
	 "⟦⟧"				; U+27E6 U+27E7
	 "⟨⟩"				; U+27E8 U+27E9
	 "⟪⟫"				; U+27EA U+27EB
	 "⦃⦄"				; U+2983 U+2984
	 "⦅⦆"				; U+2985 U+2986
	 "⦇⦈"				; U+2987 U+2988
	 "⦉⦊"				; U+2989 U+298A
	 "⦋⦌"				; U+298B U+298C
	 "⦍⦎"				; U+298D U+298E
	 "⦏⦐"				; U+298F U+2990
	 "⦑⦒"				; U+2991 U+2992
	 "⦓⦔"				; U+2993 U+2994
	 "⦕⦖"				; U+2995 U+2996
	 "⦗⦘"				; U+2997 U+2998
	 "⧼⧽"				; U+29FC U+29FD
	 "〈〉"				; U+3008 U+3009
	 "《》"				; U+300A U+300B
	 "「」"				; U+300C U+300D
	 "『』"				; U+300E U+300F
	 "【】"				; U+3010 U+3011
	 "〔〕"				; U+3014 U+3015
	 "〖〗"				; U+3016 U+3017
	 "〘〙"				; U+3018 U+3019
	 "〚〛"				; U+301A U+301B
	 "﴾﴿"				; U+FD3E U+FD3F
	 "︵︶"				; U+FE35 U+FE36
	 "︷︸"				; U+FE37 U+FE38
	 "︹︺"				; U+FE39 U+FE3A
	 "︻︼"				; U+FE3B U+FE3C
	 "︽︾"				; U+FE3D U+FE3E
	 "︿﹀"				; U+FE3F U+FE40
	 "﹁﹂"				; U+FE41 U+FE42
	 "﹃﹄"				; U+FE43 U+FE44
	 "﹙﹚"				; U+FE59 U+FE5A
	 "﹛﹜"				; U+FE5B U+FE5C
	 "﹝﹞"				; U+FE5D U+FE5E
	 "（）"				; U+FF08 U+FF09
	 "［］"				; U+FF3B U+FF3D
	 "｛｝"				; U+FF5B U+FF5D
	 "｟｠"				; U+FF5F U+FF60
	 "｢｣"				; U+FF62 U+FF63
	 )))
  (dolist (elt pairs)
    (modify-syntax-entry (aref elt 0) (string ?\( (aref elt 1)))
    (modify-syntax-entry (aref elt 1) (string ?\) (aref elt 0)))))


;; For each character set, put the information of the most proper
;; coding system to encode it by `preferred-coding-system' property.

;; Fixme: should this be junked?
(let ((l '((latin-iso8859-1	. iso-latin-1)
	   (latin-iso8859-2	. iso-latin-2)
	   (latin-iso8859-3	. iso-latin-3)
	   (latin-iso8859-4	. iso-latin-4)
	   (thai-tis620		. thai-tis620)
	   (greek-iso8859-7	. greek-iso-8bit)
	   (arabic-iso8859-6	. iso-2022-7bit)
	   (hebrew-iso8859-8	. hebrew-iso-8bit)
	   (katakana-jisx0201	. japanese-shift-jis)
	   (latin-jisx0201	. japanese-shift-jis)
	   (cyrillic-iso8859-5	. cyrillic-iso-8bit)
	   (latin-iso8859-9	. iso-latin-5)
	   (japanese-jisx0208-1978 . iso-2022-jp)
	   (chinese-gb2312	. chinese-iso-8bit)
	   (chinese-gbk		. chinese-gbk)
	   (gb18030-2-byte	. chinese-gb18030)
	   (gb18030-4-byte-bmp	. chinese-gb18030)
	   (gb18030-4-byte-smp	. chinese-gb18030)
	   (gb18030-4-byte-ext-1 . chinese-gb18030)
	   (gb18030-4-byte-ext-2 . chinese-gb18030)
	   (japanese-jisx0208	. iso-2022-jp)
	   (korean-ksc5601	. iso-2022-kr)
	   (japanese-jisx0212	. iso-2022-jp)
	   (chinese-big5-1	. chinese-big5)
	   (chinese-big5-2	. chinese-big5)
	   (chinese-sisheng	. iso-2022-7bit)
	   (ipa			. iso-2022-7bit)
	   (vietnamese-viscii-lower . vietnamese-viscii)
	   (vietnamese-viscii-upper . vietnamese-viscii)
	   (arabic-digit	. iso-2022-7bit)
	   (arabic-1-column	. iso-2022-7bit)
	   (lao			. lao)
	   (arabic-2-column	. iso-2022-7bit)
	   (indian-is13194	. devanagari)
	   (indian-glyph	. devanagari)
	   (tibetan-1-column	. tibetan)
	   (ethiopic		. iso-2022-7bit)
	   (chinese-cns11643-1	. iso-2022-cn)
	   (chinese-cns11643-2	. iso-2022-cn)
	   (chinese-cns11643-3	. iso-2022-cn)
	   (chinese-cns11643-4	. iso-2022-cn)
	   (chinese-cns11643-5	. iso-2022-cn)
	   (chinese-cns11643-6	. iso-2022-cn)
	   (chinese-cns11643-7	. iso-2022-cn)
	   (indian-2-column	. devanagari)
	   (tibetan		. tibetan)
	   (latin-iso8859-14	. iso-latin-8)
	   (latin-iso8859-15	. iso-latin-9))))
  (while l
    (put-charset-property (car (car l)) 'preferred-coding-system (cdr (car l)))
    (setq l (cdr l))))


;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLINE are already set.

(set-char-table-range auto-fill-chars '(#x3041 . #x30FF) t)
(set-char-table-range auto-fill-chars '(#x3400 . #x4DB5) t)
(set-char-table-range auto-fill-chars '(#x4e00 . #x9fbb) t)
(set-char-table-range auto-fill-chars '(#xF900 . #xFAFF) t)
(set-char-table-range auto-fill-chars '(#xFF00 . #xFF9F) t)
(set-char-table-range auto-fill-chars '(#x20000 . #x2FFFF) t)


;;; Setting char-width-table.  The default is 1.

;; 0: non-spacing, enclosing combining, formatting, Hangul Jamo medial
;;    and final characters.
(let ((l '((#x0300 . #x036F)
	   (#x0483 . #x0489)
	   (#x0591 . #x05BD)
	   (#x05BF . #x05BF)
	   (#x05C1 . #x05C2)
	   (#x05C4 . #x05C5)
	   (#x05C7 . #x05C7)
	   (#x0600 . #x0603)
	   (#x0610 . #x0615)
	   (#x064B . #x065E)
	   (#x0670 . #x0670)
	   (#x06D6 . #x06E4)
	   (#x06E7 . #x06E8)
	   (#x06EA . #x06ED)
	   (#x070F . #x070F)
	   (#x0711 . #x0711)
	   (#x0730 . #x074A)
	   (#x07A6 . #x07B0)
	   (#x07EB . #x07F3)
	   (#x0901 . #x0902)
	   (#x093C . #x093C)
	   (#x0941 . #x0948)
	   (#x094D . #x094D)
	   (#x0951 . #x0954)
	   (#x0962 . #x0963)
	   (#x0981 . #x0981)
	   (#x09BC . #x09BC)
	   (#x09C1 . #x09C4)
	   (#x09CD . #x09CD)
	   (#x09E2 . #x09E3)
	   (#x0A01 . #x0A02)
	   (#x0A3C . #x0A3C)
	   (#x0A41 . #x0A4D)
	   (#x0A70 . #x0A71)
	   (#x0A81 . #x0A82)
	   (#x0ABC . #x0ABC)
	   (#x0AC1 . #x0AC8)
	   (#x0ACD . #x0ACD)
	   (#x0AE2 . #x0AE3)
	   (#x0B01 . #x0B01)
	   (#x0B3C . #x0B3C)
	   (#x0B3F . #x0B3F)
	   (#x0B41 . #x0B43)
	   (#x0B4D . #x0B56)
	   (#x0B82 . #x0B82)
	   (#x0BC0 . #x0BC0)
	   (#x0BCD . #x0BCD)
	   (#x0C3E . #x0C40)
	   (#x0C46 . #x0C56)
	   (#x0CBC . #x0CBC)
	   (#x0CBF . #x0CBF)
	   (#x0CC6 . #x0CC6)
	   (#x0CCC . #x0CCD)
	   (#x0CE2 . #x0CE3)
	   (#x0D41 . #x0D43)
	   (#x0D4D . #x0D4D)
	   (#x0DCA . #x0DCA)
	   (#x0DD2 . #x0DD6)
	   (#x0E31 . #x0E31)
	   (#x0E34 . #x0E3A)
	   (#x0E47 . #x0E4E)
	   (#x0EB1 . #x0EB1)
	   (#x0EB4 . #x0EBC)
	   (#x0EC8 . #x0ECD)
	   (#x0F18 . #x0F19)
	   (#x0F35 . #x0F35)
	   (#x0F37 . #x0F37)
	   (#x0F39 . #x0F39)
	   (#x0F71 . #x0F7E)
	   (#x0F80 . #x0F84)
	   (#x0F86 . #x0F87)
	   (#x0F90 . #x0FBC)
	   (#x0FC6 . #x0FC6)
	   (#x102D . #x1030)
	   (#x1032 . #x1037)
	   (#x1039 . #x1039)
	   (#x1058 . #x1059)
	   (#x1160 . #x11FF)
	   (#x135F . #x135F)
	   (#x1712 . #x1714)
	   (#x1732 . #x1734)
	   (#x1752 . #x1753)
	   (#x1772 . #x1773)
	   (#x17B4 . #x17B5)
	   (#x17B7 . #x17BD)
	   (#x17C6 . #x17C6)
	   (#x17C9 . #x17D3)
	   (#x17DD . #x17DD)
	   (#x180B . #x180D)
	   (#x18A9 . #x18A9)
	   (#x1920 . #x1922)
	   (#x1927 . #x1928)
	   (#x1932 . #x1932)
	   (#x1939 . #x193B)
	   (#x1A17 . #x1A18)
	   (#x1B00 . #x1B03)
	   (#x1B34 . #x1B34)
	   (#x1B36 . #x1B3A)
	   (#x1B3C . #x1B3C)
	   (#x1B42 . #x1B42)
	   (#x1B6B . #x1B73)
	   (#x1DC0 . #x1DFF)
	   (#x200B . #x200F)
	   (#x202A . #x202E)
	   (#x2060 . #x206F)
	   (#x20D0 . #x20EF)
	   (#x302A . #x302F)
	   (#x3099 . #x309A)
	   (#xA806 . #xA806)
	   (#xA80B . #xA80B)
	   (#xA825 . #xA826)
	   (#xFB1E . #xFB1E)
	   (#xFE00 . #xFE0F)
	   (#xFE20 . #xFE23)
	   (#xFEFF . #xFEFF)
	   (#xFFF9 . #xFFFB)
	   (#x10A01 . #x10A0F)
	   (#x10A38 . #x10A3F)
	   (#x1D167 . #x1D169)
	   (#x1D173 . #x1D182)
	   (#x1D185 . #x1D18B)
	   (#x1D1AA . #x1D1AD)
	   (#x1D242 . #x1D244)
	   (#xE0001 . #xE01EF))))
  (dolist (elt l)
    (set-char-table-range char-width-table elt 0)))

;; 2: East Asian Wide and Full-width characters.
(let ((l '((#x1100 . #x115F)
	   (#x2329 . #x232A)
	   (#x2E80 . #x303E)
	   (#x3040 . #xA4CF)
	   (#xAC00 . #xD7A3)
	   (#xF900 . #xFAFF)
	   (#xFE30 . #xFE6F)
	   (#xFF01 . #xFF60)
	   (#xFFE0 . #xFFE6)
	   (#x20000 . #x2FFFF)
	   (#x30000 . #x3FFFF))))
  (dolist (elt l)
    (set-char-table-range char-width-table elt 2)))

;; Other double width
;;(map-charset-chars
;; (lambda (range ignore) (set-char-table-range char-width-table range 2))
;; 'ethiopic)
;; (map-charset-chars
;;  (lambda (range ignore) (set-char-table-range char-width-table range 2))
;; 'tibetan)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'indian-2-column)
(map-charset-chars
 (lambda (range ignore) (set-char-table-range char-width-table range 2))
 'arabic-2-column)

;; Internal use only.
;; Alist of locale symbol vs charsets.  In a language environment
;; corresponding to the locale, width of characters in the charsets is
;; set to 2.  Each element has the form:
;;   (LOCALE TABLE (CHARSET (FROM-CODE . TO-CODE) ...) ...)
;; LOCALE: locale symbol
;; TABLE: char-table used for char-width-table, initially nil.
;; CAHRSET: character set
;; FROM-CODE, TO-CODE: range of code-points in CHARSET

(defvar cjk-char-width-table-list
  '((ja_JP nil (japanese-jisx0208 (#x2121 . #x287E))
	       (cp932-2-byte (#x8140 . #x879F)))
    (zh_CN nil (chinese-gb2312 (#x2121 . #x297E)))
    (zh_HK nil (big5-hkscs (#xA140 . #xA3FE) (#xC6A0 . #xC8FE)))
    (zh_TW nil (big5 (#xA140 . #xA3FE))
	       (chinese-cns11643-1 (#x2121 . #x427E)))
    (ko_KR nil (korean-ksc5601 (#x2121 . #x2C7E)))))

;; Internal use only.
;; Setup char-width-table appropriate for a language environment
;; corresponding to LOCALE-NAME (symbol).

(defun use-cjk-char-width-table (locale-name)
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((slot (assq locale-name cjk-char-width-table-list))
	table)
    (or slot (error "Unknown locale for CJK language environment: %s"
		    locale-name))
    (unless (nth 1 slot)
      (let ((table (make-char-table nil)))
	(dolist (charset-info (nthcdr 2 slot))
	  (let ((charset (car charset-info)))
	    (dolist (code-range (cdr charset-info))
	      (map-charset-chars #'(lambda (range arg)
				     (set-char-table-range table range 2))
				 charset nil
				 (car code-range) (cdr code-range)))))
	(optimize-char-table table)
	(set-char-table-parent table char-width-table)
	(setcar (cdr slot) table)))
    (setq char-width-table (nth 1 slot))))

(defun use-default-char-width-table ()
  "Internal use only.
Setup char-width-table appropriate for non-CJK language environment."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table))))

(optimize-char-table (standard-case-table))
(optimize-char-table (standard-syntax-table))


;; Setting char-script-table.

;; The data is compiled from Blocks.txt and Scripts.txt in the
;; "Unicode Character Database", simplified to lump together all the
;; blocks belonging to the same language.  E.g., "Basic Latin",
;; "Latin-1 Supplement", "Latin Extended-A", etc. are all lumped
;; together under "latin".
;;
;; The Unicode blocks actually extend past some of these ranges with
;; undefined codepoints.
(let ((script-list nil))
  (dolist
      (elt
       '((#x0000 #x007F latin)
	 (#x00A0 #x024F latin)
	 (#x0250 #x02AF phonetic)
	 (#x02B0 #x036F latin)
	 (#x0370 #x03E1 greek)
	 (#x03E2 #x03EF coptic)
	 (#x03F0 #x03F3 greek)
	 (#x0400 #x052F cyrillic)
	 (#x0530 #x058F armenian)
	 (#x0590 #x05FF hebrew)
	 (#x0600 #x06FF arabic)
	 (#x0700 #x074F syriac)
	 (#x0750 #x077F arabic)
	 (#x0780 #x07BF thaana)
	 (#x07C0 #x07FF nko)
	 (#x0800 #x083F samaritan)
	 (#x0840 #x085F mandaic)
	 (#x08A0 #x08FF arabic)
	 (#x0900 #x097F devanagari)
	 (#x0980 #x09FF bengali)
	 (#x0A00 #x0A7F gurmukhi)
	 (#x0A80 #x0AFF gujarati)
	 (#x0B00 #x0B7F oriya)
	 (#x0B80 #x0BFF tamil)
	 (#x0C00 #x0C7F telugu)
	 (#x0C80 #x0CFF kannada)
	 (#x0D00 #x0D7F malayalam)
	 (#x0D80 #x0DFF sinhala)
	 (#x0E00 #x0E7F thai)
	 (#x0E80 #x0EFF lao)
	 (#x0F00 #x0FFF tibetan)
	 (#x1000 #x109F burmese) ; according to Unicode 6.1, should be "myanmar"
	 (#x10A0 #x10FF georgian)
	 (#x1100 #x11FF hangul)
	 (#x1200 #x139F ethiopic)
	 (#x13A0 #x13FF cherokee)
	 (#x1400 #x167F canadian-aboriginal)
	 (#x1680 #x169F ogham)
	 (#x16A0 #x16FF runic)
	 (#x1700 #x171F tagalog)
	 (#x1720 #x173F hanunoo)
	 (#x1740 #x175F buhid)
	 (#x1760 #x177F tagbanwa)
	 (#x1780 #x17FF khmer)
	 (#x1800 #x18AF mongolian)
	 (#x18B0 #x18FF canadian-aboriginal)
	 (#x1900 #x194F limbu)
	 (#x1950 #x197F tai-le)
	 (#x1980 #x19DF tai-lue)
	 (#x19E0 #x19FF khmer)
	 (#x1A00 #x1A00 buginese)
	 (#x1A20 #x1AAF tai-tham)
	 (#x1B00 #x1B7F balinese)
	 (#x1B80 #x1BBF sundanese)
	 (#x1BC0 #x1BFF batak)
	 (#x1C00 #x1C4F lepcha)
	 (#x1C50 #x1C7F ol-chiki)
	 (#x1CC0 #x1CCF sundanese)
	 (#x1CD0 #x1CFF vedic)
	 (#x1D00 #x1DBF phonetic)
	 (#x1DC0 #x1EFF latin)
	 (#x1F00 #x1FFF greek)
	 (#x2000 #x27FF symbol)
	 (#x2800 #x28FF braille)
	 (#x2900 #x2BFF symbol)
	 (#x2C00 #x2C5F glagolitic)
	 (#x2C60 #x2C7F latin)
	 (#x2C80 #x2CFF coptic)
	 (#x2D00 #x2D2F georgian)
	 (#x2D30 #x2D7F tifinagh)
	 (#x2D80 #x2DDF ethiopic)
	 (#x2DE0 #x2DFF cyrillic)
	 (#x2E00 #x2E7F symbol)
	 (#x2E80 #x2FDF han)
	 (#x2FF0 #x2FFF ideographic-description)
	 (#x3000 #x303F cjk-misc)
	 (#x3040 #x30FF kana)
	 (#x3100 #x312F bopomofo)
	 (#x3130 #x318F hangul)
	 (#x3190 #x319F kanbun)
	 (#x31A0 #x31BF bopomofo)
	 (#x31C0 #x31EF cjk-misc)
	 (#x31F0 #x31FF kana)
	 (#x3200 #x9FAF han)
	 (#xA000 #xA4CF yi)
	 (#xA4D0 #xA4FF lisu)
	 (#xA500 #xA63F vai)
	 (#xA640 #xA69F cyrillic)
	 (#xA6A0 #xA6FF bamum)
	 (#xA700 #xA7FF latin)
	 (#xA800 #xA82F syloti-nagri)
	 (#xA830 #xA83F north-indic-number)
	 (#xA840 #xA87F phags-pa)
	 (#xA880 #xA8DF saurashtra)
	 (#xA8E0 #xA8FF devanagari)
	 (#xA900 #xA92F kayah-li)
	 (#xA930 #xA95F rejang)
	 (#xA960 #xA97F hangul)
	 (#xA980 #xA9DF javanese)
	 (#xAA00 #xAA5F cham)
	 (#xAA60 #xAA7B burmese)	; Unicode 6.1: "myanmar"
	 (#xAA80 #xAADF tai-viet)
	 (#xAAE0 #xAAFF meetei-mayek)
	 (#xAB00 #xAB2F ethiopic)
	 (#xABC0 #xABFF meetei-mayek)
	 (#xAC00 #xD7FF hangul)
	 (#xF900 #xFAFF han)
	 (#xFB1D #xFB4F hebrew)
	 (#xFB50 #xFDFF arabic)
	 (#xFE30 #xFE4F han)
	 (#xFE70 #xFEFF arabic)
	 (#xFF00 #xFF5F cjk-misc)
	 (#xFF61 #xFF9F kana)
	 (#xFFE0 #xFFE6 cjk-misc)
	 (#x10000 #x100FF linear-b)
	 (#x10100 #x1013F aegean-number)
	 (#x10140 #x1018F ancient-greek-number)
	 (#x10190 #x101CF ancient-symbol)
	 (#x101D0 #x101FF phaistos-disc)
	 (#x10280 #x1029F lycian)
	 (#x102A0 #x102DF carian)
	 (#x10300 #x1032F olt-italic)
	 (#x10330 #x1034F gothic)
	 (#x10380 #x1039F ugaritic)
	 (#x103A0 #x103DF old-persian)
	 (#x10400 #x1044F deseret)
	 (#x10450 #x1047F shavian)
	 (#x10480 #x104AF osmanya)
	 (#x10800 #x1083F cypriot-syllabary)
	 (#x10840 #x1085F aramaic)
	 (#x10900 #x1091F phoenician)
	 (#x10920 #x1093F lydian)
	 (#x10980 #x109FF meroitic)
	 (#x10A00 #x10A5F kharoshthi)
	 (#x10A60 #x10A7F old-south-arabian)
	 (#x10B00 #x10B3F avestan)
	 (#x10B40 #x10B5F inscriptional-parthian)
	 (#x10B60 #x10B7F inscriptional-pahlavi)
	 (#x10C00 #x10C4F old-turkic)
	 (#x10E60 #x10E7F rumi-number)
	 (#x11000 #x1107F brahmi)
	 (#x11080 #x110CF kaithi)
	 (#x110D0 #x110FF sora-sompeng)
	 (#x11100 #x1114F chakma)
	 (#x11180 #x111DF sharada)
	 (#x11680 #x116CF takri)
	 (#x12000 #x123FF cuneiform)
	 (#x12400 #x1247F cuneiform-numbers-and-punctuation)
	 (#x13000 #x1342F egyptian)
	 (#x16800 #x16A3F bamum)
	 (#x16F00 #x16F9F miao)
	 (#x1B000 #x1B0FF kana)
	 (#x1D000 #x1D0FF byzantine-musical-symbol)
	 (#x1D100 #x1D1FF musical-symbol)
	 (#x1D200 #x1D24F ancient-greek-musical-notation)
	 (#x1D300 #x1D35F tai-xuan-jing-symbol)
	 (#x1D360 #x1D37F counting-rod-numeral)
	 (#x1D400 #x1D7FF mathematical)
	 (#x1EE00 #x1EEFF arabic)
	 (#x1F000 #x1F02F mahjong-tile)
	 (#x1F030 #x1F09F domino-tile)
	 (#x1F0A0 #x1F0FF playing-cards)
	 (#x1F100 #x1F1FF symbol)
	 (#x1F200 #x1F2FF han)
	 (#x1F300 #x1F64F symbol)
	 (#x1F680 #x1F77F symbol)
	 (#x20000 #x2B81F han)
	 (#x2F800 #x2FFFF han)))
    (set-char-table-range char-script-table
			  (cons (car elt) (nth 1 elt)) (nth 2 elt))
    (or (memq (nth 2 elt) script-list)
	(setq script-list (cons (nth 2 elt) script-list))))
  (set-char-table-extra-slot char-script-table 0 (nreverse script-list)))

(map-charset-chars
 #'(lambda (range ignore)
     (set-char-table-range char-script-table range 'tibetan))
 'tibetan)


;;; Setting unicode-category-table.

(setq unicode-category-table
      (unicode-property-table-internal 'general-category))
(map-char-table #'(lambda (key val)
		    (if (and val
			     (or (and (/= (aref (symbol-name val) 0) ?M)
				      (/= (aref (symbol-name val) 0) ?C))
				 (eq val 'Zs)))
			(modify-category-entry key ?.)))
		unicode-category-table)

(optimize-char-table (standard-category-table))


;; Display of glyphless characters.

(defvar char-acronym-table
  (make-char-table 'char-acronym-table nil)
  "Char table of acronyms for non-graphic characters.")

(let ((c0-acronyms '("NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL"
		     "BS"   nil   nil  "VT"  "FF"  "CR"  "SO"  "SI"
		     "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
		     "CAN" "EM"  "SUB" "ESC" "FC"  "GS"  "RS"  "US")))
  (dotimes (i 32)
    (aset char-acronym-table i (car c0-acronyms))
    (setq c0-acronyms (cdr c0-acronyms))))

(let ((c1-acronyms '("XXX" "XXX" "BPH" "NBH" "IND" "NEL" "SSA" "ESA"
		     "HTS" "HTJ" "VTS" "PLD" "PLU" "R1"  "SS2" "SS1"
		     "DCS" "PU1" "PU2" "STS" "CCH" "MW"  "SPA" "EPA"
		     "SOS" "XXX" "SC1" "CSI" "ST"  "OSC" "PM"  "APC")))
  (dotimes (i 32)
    (aset char-acronym-table (+ #x0080 i) (car c1-acronyms))
    (setq c1-acronyms (cdr c1-acronyms))))

(aset char-acronym-table #x17B4 "KIVAQ")   ; KHMER VOWEL INHERENT AQ
(aset char-acronym-table #x17B5 "KIVAA")   ; KHMER VOWEL INHERENT AA
(aset char-acronym-table #x200B "ZWSP")    ; ZERO WIDTH SPACE
(aset char-acronym-table #x200C "ZWNJ")    ; ZERO WIDTH NON-JOINER
(aset char-acronym-table #x200D "ZWJ")	   ; ZERO WIDTH JOINER
(aset char-acronym-table #x200E "LRM")	   ; LEFT-TO-RIGHT MARK
(aset char-acronym-table #x200F "RLM")	   ; RIGHT-TO-LEFT MARK
(aset char-acronym-table #x202A "LRE")	   ; LEFT-TO-RIGHT EMBEDDING
(aset char-acronym-table #x202B "RLE")	   ; RIGHT-TO-LEFT EMBEDDING
(aset char-acronym-table #x202C "PDF")	   ; POP DIRECTIONAL FORMATTING
(aset char-acronym-table #x202D "LRO")	   ; LEFT-TO-RIGHT OVERRIDE
(aset char-acronym-table #x202E "RLO")	   ; RIGHT-TO-LEFT OVERRIDE
(aset char-acronym-table #x2060 "WJ")	   ; WORD JOINER
(aset char-acronym-table #x206A "ISS")	   ; INHIBIT SYMMETRIC SWAPPING
(aset char-acronym-table #x206B "ASS")	   ; ACTIVATE SYMMETRIC SWAPPING
(aset char-acronym-table #x206C "IAFS")    ; INHIBIT ARABIC FORM SHAPING
(aset char-acronym-table #x206D "AAFS")    ; ACTIVATE ARABIC FORM SHAPING
(aset char-acronym-table #x206E "NADS")    ; NATIONAL DIGIT SHAPES
(aset char-acronym-table #x206F "NODS")    ; NOMINAL DIGIT SHAPES
(aset char-acronym-table #xFEFF "ZWNBSP")  ; ZERO WIDTH NO-BREAK SPACE
(aset char-acronym-table #xFFF9 "IAA")	   ; INTERLINEAR ANNOTATION ANCHOR
(aset char-acronym-table #xFFFA "IAS")     ; INTERLINEAR ANNOTATION SEPARATOR
(aset char-acronym-table #xFFFB "IAT")     ; INTERLINEAR ANNOTATION TERMINATOR
(aset char-acronym-table #x1D173 "BEGBM")  ; MUSICAL SYMBOL BEGIN BEAM
(aset char-acronym-table #x1D174 "ENDBM")  ; MUSICAL SYMBOL END BEAM
(aset char-acronym-table #x1D175 "BEGTIE") ; MUSICAL SYMBOL BEGIN TIE
(aset char-acronym-table #x1D176 "END")	   ; MUSICAL SYMBOL END TIE
(aset char-acronym-table #x1D177 "BEGSLR") ; MUSICAL SYMBOL BEGIN SLUR
(aset char-acronym-table #x1D178 "ENDSLR") ; MUSICAL SYMBOL END SLUR
(aset char-acronym-table #x1D179 "BEGPHR") ; MUSICAL SYMBOL BEGIN PHRASE
(aset char-acronym-table #x1D17A "ENDPHR") ; MUSICAL SYMBOL END PHRASE
(aset char-acronym-table #xE0001 "|->TAG") ; LANGUAGE TAG
(aset char-acronym-table #xE0020 "SP TAG") ; TAG SPACE
(dotimes (i 94)
  (aset char-acronym-table (+ #xE0021 i) (format " %c TAG" (+ 33 i))))
(aset char-acronym-table #xE007F "->|TAG") ; CANCEL TAG

(defun update-glyphless-char-display (&optional variable value)
  "Make the setting of `glyphless-char-display-control' take effect.
This function updates the char-table `glyphless-char-display'."
  (when value
    (set-default variable value))
  (dolist (elt value)
    (let ((target (car elt))
	  (method (cdr elt)))
      (or (memq method '(zero-width thin-space empty-box acronym hex-code))
	  (error "Invalid glyphless character display method: %s" method))
      (cond ((eq target 'c0-control)
	     (set-char-table-range glyphless-char-display '(#x00 . #x1F)
				   method)
	     ;; Users will not expect their newlines and TABs be
	     ;; displayed as anything but themselves, so exempt those
	     ;; two characters from c0-control.
	     (set-char-table-range glyphless-char-display #x9 nil)
	     (set-char-table-range glyphless-char-display #xa nil))
	    ((eq target 'c1-control)
	     (set-char-table-range glyphless-char-display '(#x80 . #x9F)
				   method))
	    ((eq target 'format-control)
	     (map-char-table
	      #'(lambda (char category)
		  (if (eq category 'Cf)
		      (let ((this-method method)
			    from to)
			(if (consp char)
			    (setq from (car char) to (cdr char))
			  (setq from char to char))
			(while (<= from to)
			  (when (/= from #xAD)
			    (if (eq method 'acronym)
				(setq this-method
				      (aref char-acronym-table from)))
			    (set-char-table-range glyphless-char-display
						  from this-method))
			  (setq from (1+ from))))))
	      unicode-category-table))
	    ((eq target 'no-font)
	     (set-char-table-extra-slot glyphless-char-display 0 method))
	    (t
	     (error "Invalid glyphless character group: %s" target))))))

;;; Control of displaying glyphless characters.
(defcustom glyphless-char-display-control
  '((format-control . thin-space)
    (no-font . hex-code))
  "List of directives to control display of glyphless characters.

Each element has the form (GROUP . METHOD), where GROUP is a
symbol specifying the character group, and METHOD is a symbol
specifying the method of displaying characters belonging to that
group.

GROUP must be one of these symbols:
  `c0-control':     U+0000..U+001F, but excluding newline and TAB.
  `c1-control':     U+0080..U+009F.
  `format-control': Characters of Unicode General Category `Cf',
                    such as U+200C (ZWNJ), U+200E (LRM), but
                    excluding characters that have graphic images,
                    such as U+00AD (SHY).
  `no-font':        characters for which no suitable font is found.
                    For character terminals, characters that cannot
                    be encoded by `terminal-coding-system'.

METHOD must be one of these symbols:
  `zero-width': don't display.
  `thin-space': display a thin (1-pixel width) space.  On character
                terminals, display as 1-character space.
  `empty-box':  display an empty box.
  `acronym':    display an acronym of the character in a box.  The
                acronym is taken from `char-acronym-table', which see.
  `hex-code':   display the hexadecimal character code in a box."
  :version "24.1"
  :type '(alist :key-type (symbol :tag "Character Group")
		:value-type (symbol :tag "Display Method"))
  :options '((c0-control
	      (choice (const :tag "Don't display" zero-width)
		      (const :tag "Display as thin space" thin-space)
		      (const :tag "Display as empty box" empty-box)
		      (const :tag "Display acronym" acronym)
		      (const :tag "Display hex code in a box" hex-code)))
	     (c1-control
	      (choice (const :tag "Don't display" zero-width)
		      (const :tag "Display as thin space" thin-space)
		      (const :tag "Display as empty box" empty-box)
		      (const :tag "Display acronym" acronym)
		      (const :tag "Display hex code in a box" hex-code)))
	     (format-control
	      (choice (const :tag "Don't display" zero-width)
		      (const :tag "Display as thin space" thin-space)
		      (const :tag "Display as empty box" empty-box)
		      (const :tag "Display acronym" acronym)
		      (const :tag "Display hex code in a box" hex-code)))
	     (no-font
	      (choice (const :tag "Don't display" zero-width)
		      (const :tag "Display as thin space" thin-space)
		      (const :tag "Display as empty box" empty-box)
		      (const :tag "Display acronym" acronym)
		      (const :tag "Display hex code in a box" hex-code))))
  :set 'update-glyphless-char-display
  :group 'display)


;;; Setting word boundary.

(setq word-combining-categories
      '((nil . ?^)
	(?^ . nil)
	(?C . ?H)
	(?C . ?K)))

(setq word-separating-categories	;  (2-byte character sets)
      '((?H . ?K)			; Hiragana - Katakana
	))

;; Local Variables:
;; coding: utf-8
;; End:

;;; characters.el ends here
