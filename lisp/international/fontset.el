;;; fontset.el --- commands for handling fontset

;; Copyright (C) 1997-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003, 2006
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, i18n, fontset

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

;; Setup font-encoding-alist for all known encodings.

(setq font-encoding-alist
      (mapcar (lambda (arg)
		(cons (purecopy (car arg)) (cdr arg)))
      '(("iso8859-1$" . iso-8859-1)
	("iso8859-2$" . iso-8859-2)
	("iso8859-3$" . iso-8859-3)
	("iso8859-4$" . iso-8859-4)
	("iso8859-5$" . iso-8859-5)
	("iso8859-6$" . iso-8859-6)
	("iso8859-7$" . iso-8859-7)
	("iso8859-8$" . iso-8859-8)
	("iso8859-9$" . iso-8859-9)
	("iso8859-10$" . iso-8859-10)
	("iso8859-11$" . iso-8859-11)
	("iso8859-13$" . iso-8859-13)
	("iso8859-14$" . iso-8859-14)
	("iso8859-15$" . iso-8859-15)
	("iso8859-16$" . iso-8859-16)
	("ascii-0$" . ascii)
	("gb2312.1980" . chinese-gb2312)
	("gbk" . chinese-gbk)
	("gb18030" . (unicode . nil))
	("jisx0208.1978" . japanese-jisx0208-1978)
	("jisx0208" . japanese-jisx0208)
	("jisx0201" . jisx0201)
	("jisx0212" . japanese-jisx0212)
	("ksx1001" . korean-ksc5601)
	("ksc5601.1987" . korean-ksc5601)
	("cns11643.1992.*1" . chinese-cns11643-1)
	("cns11643.1992.*2" . chinese-cns11643-2)
	("cns11643.1992.*3" . chinese-cns11643-3)
	("cns11643.1992.*4" . chinese-cns11643-4)
	("cns11643.1992.*5" . chinese-cns11643-5)
	("cns11643.1992.*6" . chinese-cns11643-6)
	("cns11643.1992.*7" . chinese-cns11643-7)
	("cns11643.92p1-0" . chinese-cns11643-1)
	("cns11643.92p2-0" . chinese-cns11643-2)
	("cns11643.92p3-0" . chinese-cns11643-3)
	("cns11643.92p4-0" . chinese-cns11643-4)
	("cns11643.92p5-0" . chinese-cns11643-5)
	("cns11643.92p6-0" . chinese-cns11643-6)
	("cns11643.92p7-0" . chinese-cns11643-7)
	("big5" . big5)
	("viscii" . viscii)
	("tis620" . tis620-2533)
	("microsoft-cp1251" . windows-1251)
	("koi8-r" . koi8-r)
	("jisx0213.2000-1" . japanese-jisx0213-1)
	("jisx0213.2000-2" . japanese-jisx0213-2)
	("jisx0213.2004-1" . japanese-jisx0213.2004-1)
	("iso10646-1$" . (unicode-bmp . nil))
	("iso10646.indian-1" . (unicode-bmp . nil))
	("unicode-bmp" . (unicode-bmp . nil))
	("abobe-symbol" . symbol)
	("sisheng_cwnn" . chinese-sisheng)
	("mulearabic-0" . arabic-digit)
	("mulearabic-1" . arabic-1-column)
	("mulearabic-2" . arabic-2-column)
	("muleipa" . ipa)
	("ethiopic-unicode" . (unicode-bmp . ethiopic))
	("is13194-devanagari" . indian-is13194)
	("Devanagari-CDAC" . devanagari-cdac)
 	("Sanskrit-CDAC" . sanskrit-cdac)
 	("Bengali-CDAC" . bengali-cdac)
 	("Assamese-CDAC" . assamese-cdac)
 	("Punjabi-CDAC" . punjabi-cdac)
 	("Gujarati-CDAC" . gujarati-cdac)
 	("Oriya-CDAC" . oriya-cdac)
 	("Tamil-CDAC" . tamil-cdac)
 	("Telugu-CDAC" . telugu-cdac)
 	("Kannada-CDAC" . kannada-cdac)
 	("Malayalam-CDAC" . malayalam-cdac)
	("Devanagari-Akruti" . devanagari-akruti)
	("Bengali-Akruti" . bengali-akruti)
	("Punjabi-Akruti" . punjabi-akruti)
	("Gujarati-Akruti" . gujarati-akruti)
	("Oriya-Akruti" . oriya-akruti)
	("Tamil-Akruti" . tamil-akruti)
	("Telugu-Akruti" . telugu-akruti)
	("Kannada-Akruti" . kannada-akruti)
	("Malayalam-Akruti" . malayalam-akruti)
	("muleindian-2" . indian-2-column)
	("muleindian-1" . indian-1-column)
	("mulelao-1" . mule-lao)
	("muletibetan-2" . tibetan)
	("muletibetan-0" . tibetan)
	("muletibetan-1" . tibetan-1-column))))

(defvar font-encoding-charset-alist)

(setq font-encoding-charset-alist
      '((latin-iso8859-1 . iso-8859-1)
	(latin-iso8859-2 . iso-8859-2)
	(latin-iso8859-3 . iso-8859-3)
	(latin-iso8859-4 . iso-8859-4)
	(latin-iso8859-9 . iso-8859-9)
	(latin-iso8859-10 . iso-8859-10)
	(latin-iso8859-13 . iso-8859-13)
	(latin-iso8859-14 . iso-8859-14)
	(latin-iso8859-15 . iso-8859-15)
	(latin-iso8859-16 . iso-8859-16)
	(cyrillic-iso8859-5 . iso-8859-5)
	(greek-iso8859-7 . iso-8859-7)
	(arabic-iso8859-6 . iso-8859-6)
	(thai-tis620 . tis620-2533)
	(latin-jisx0201 . jisx0201)
	(katakana-jisx0201 . jisx0201)
	(chinese-big5-1 . big5)
	(chinese-big5-2 . big5)
	(vietnamese-viscii-lower . viscii)
	(vietnamese-viscii-upper . viscii)
	(tibetan . unicode-bmp)))

(setq script-representative-chars
      '((latin ?A ?Z ?a ?z #x00C0 #x0100 #x0180 #x1e00)
	(phonetic #x250 #x283)
	(greek #x3A9)
	(coptic #x3E2)
	(cyrillic #x42F)
	(armenian #x531)
	(hebrew #x5D0)
	(arabic #x628)
	(syriac #x710)
	(thaana #x78C)
	(devanagari #x915)
	(bengali #x995)
	(gurmukhi #xA15)
	(gujarati #xA95)
	(oriya #xB15)
	(tamil #xB95)
	(telugu #xC15)
	(kannada #xC95)
	(malayalam #xD15)
	(sinhala #xD95)
	(thai #xE17)
	(lao #xEA5)
	(tibetan #xF40)
	(burmese #x1000)
	(georgian #x10D3)
	(ethiopic #x1208)
	(cherokee #x13B6)
	(canadian-aboriginal #x14C0)
	(ogham #x168F)
	(runic #x16A0)
	(khmer #x1780)
	(mongolian #x1826)
	(symbol . [#x201C #x2200 #x2500])
	(braille #x2800)
	(ideographic-description #x2FF0)
	(cjk-misc #x300E)
	(kana #x304B)
	(bopomofo #x3105)
	(kanbun #x319D)
	(han #x5B57)
	(yi #xA288)
	(cham #xAA00)
	(tai-viet #xAA80)
	(hangul #xAC00)
	(linear-b #x10000)
	(aegean-number #x10100)
	(ancient-greek-number #x10140)
	(ancient-symbol #x10190)
	(phaistos-disc #x101D0)
	(lycian #x10280)
	(carian #x102A0)
	(olt-italic #x10300)
	(ugaritic #x10380)
	(old-persian #x103A0)
	(deseret #x10400)
	(shavian #x10450)
	(osmanya #x10480)
	(cypriot-syllabary #x10800)
	(phoenician #x10900)
	(lydian #x10920)
	(kharoshthi #x10A00)
	(cuneiform #x12000)
	(cuneiform-numbers-and-punctuation #x12400)
	(byzantine-musical-symbol #x1D000)
	(musical-symbol #x1D100)
	(ancient-greek-musical-notation #x1D200)
	(tai-xuan-jing-symbol #x1D300)
	(counting-rod-numeral #x1D360)
	(mahjong-tile #x1F000)
	(domino-tile #x1F030)))

(defvar otf-script-alist)

(setq otf-script-alist
      '((arab . arabic)
	(armn . armenian)
	(bali . balinese)
	(beng . bengali)
	(bopo . bopomofo)
	(brai . braille)
	(bugi . buginese)
	(buhd . buhid)
	(byzm . byzantine-musical-symbol)
	(cans . canadian-aboriginal)
	(cher . cherokee)
	(copt . coptic)
	(xsux . cuneiform)
	(cyrl . cyrillic)
	(cprt . cypriot)
	(dsrt . deseret)
	(deva . devanagari)
	(ethi . ethiopic)
	(geor . georgian)
	(glag . glagolitic)
	(goth . gothic)
	(grek . greek)
	(gujr . gujarati)
	(guru . gurmukhi)
	(hani . han)
	(hang . hangul)
	(hano . hanunoo)
	(hebr . hebrew)
	(kana . kana)
	(knda . kannada)
	(khar . kharoshthi)
	(khmr . khmer)
	(lao\  . lao)
	(latn . latin)
	(limb . limbu)
	(linb . linear_b)
	(mlym . malayalam)
	(math . mathematical)
	(mong . mongolian)
	(musc . musical-symbol)
	(mymr . burmese)
	(nko\  . nko)
	(ogam . ogham)
	(ital . old_italic)
	(xpeo . old_persian)
	(orya . oriya)
	(osma . osmanya)
	(phag . phags-pa)
	(phnx . phoenician)
	(runr . runic)
	(shaw . shavian)
	(sinh . sinhala)
	(sylo . syloti_nagri)
	(syrc . syriac)
	(tglg . tagalog)
	(tagb . tagbanwa)
	(taml . tamil)
	(tale . tai_le)
	(telu . telugu)
	(thaa . thaana)
	(thai . thai)
	(tibt . tibetan)
	(tfng . tifinagh)
	(ugar . ugaritic)
	(yi\ \   . yi)))

;; Set standard fontname specification of characters in the default
;; fontset to find an appropriate font for each script/charset.  The
;; specification has the form ((SCRIPT FONT-SPEC ...) ...), where
;; FONT-SPEC is:
;;	a cons (FAMILY . REGISTRY),
;;	or a string FONT-NAME,
;;	or an object created by `font-spec'.
;;
;; FAMILY may be nil, in which case, the corresponding name of
;; default face is used.  If REGISTRY contains a character `-', the
;; string before that is embedded in `CHARSET_REGISTRY' field, and the
;; string after that is embedded in `CHARSET_ENCODING' field.  If it
;; does not contain `-', the whole string is embedded in
;; `CHARSET_REGISTRY' field, and a wild card character `*' is embedded
;; in `CHARSET_ENCODING' field.
;;
;; SCRIPT is a symbol that appears as an element of the char table
;; `char-script-table'.  SCRIPT may be a charset specifying the range
;; of characters.

(declare-function new-fontset "fontset.c" (name fontlist))
(declare-function set-fontset-font "fontset.c"
		  (name target font-spec &optional frame add))

(eval-when-compile

;; Build a data to initialize the default fontset at compile time to
;; avoid loading charsets that won't be necessary at runtime.

;; The value is (CJK-REGISTRY-VECTOR TARGET-SPEC ...), where
;; CJK-REGISTRY-VECTOR is ["JISX0208.1983-0" "GB2312.1980-0" ...],
;; TARGET-SPEC is (TARGET . BITMASK) or (TARGET SPEC ...),
;; TARGET is CHAR or (FROM-CHAR . TO-CHAR),
;; BITMASK is a bitmask of indices to CJK-REGISTRY-VECTOR,
;; SPEC is a list of arguments to font-spec.

(defmacro build-default-fontset-data ()
  (let* (;;       CHARSET-REGISTRY  CHARSET            FROM-CODE TO-CODE
	 (cjk '(("JISX0208.1983-0" japanese-jisx0208  #x2121    #x287E)
		("GB2312.1980-0"   chinese-gb2312     #x2121    #x297E)
		("BIG5-0"          big5               #xA140    #xA3FE)
		("CNS11643.1992-1" chinese-cns11643-1 #x2121    #x427E)
		("KSC5601.1987-0"  korean-ksc5601     #x2121    #x2C7E)))
	 (scripts '((tibetan
		     (:registry "iso10646-1" :otf (tibt nil (ccmp blws abvs)))
		     (:family "mtib" :registry "iso10646-1")
		     (:registry "muletibetan-2"))
		    (ethiopic
		     (:registry "iso10646-1" :script ethiopic)
		     (:registry "ethiopic-unicode"))
		    (phonetic
		     (:registry "iso10646-1" :script phonetic)
		     (:registry "MuleIPA-1")
		     (:registry "iso10646-1"))))
	 (cjk-table (make-char-table nil))
	 (script-coverage
	  #'(lambda (script)
	      (let ((coverage))
		(map-char-table
		 #'(lambda (range val)
		     (when (eq val script)
		       (if (consp range)
			   (setq range (cons (car range) (cdr range))))
		       (push range coverage)))
		 char-script-table)
		coverage)))
	 (data (list (vconcat (mapcar 'car cjk))))
	 (i 0))
    (dolist (elt cjk)
      (let ((mask (lsh 1 i)))
	(map-charset-chars
	 #'(lambda (range arg)
	     (let ((from (car range)) (to (cdr range)))
	       (if (< to #x110000)
		   (while (<= from to)
		     (or (memq (aref char-script-table from)
			       '(kana hangul han cjk-misc))
			 (aset cjk-table from
			       (logior (or (aref cjk-table from) 0) mask)))
		     (setq from (1+ from))))))
	 (nth 1 elt) nil (nth 2 elt) (nth 3 elt)))
      (setq i (1+ i)))
    (map-char-table
     #'(lambda (range val)
	 (if (consp range)
	     (setq range (cons (car range) (cdr range))))
	 (push (cons range val) data))
     cjk-table)
    (dolist (script scripts)
      (dolist (range (funcall script-coverage (car script)))
	(push (cons range (cdr script)) data)))
    `(quote ,(nreverse data))))
)

(defun setup-default-fontset ()
  "Setup the default fontset."
  (new-fontset
   "fontset-default"
   `(;; for each script
     (latin (nil . "ISO8859-1")
	    (nil . "ISO8859-2")
	    (nil . "ISO8859-3")
	    (nil . "ISO8859-4")
	    (nil . "ISO8859-9")
	    (nil . "ISO8859-10")
	    (nil . "ISO8859-13")
	    (nil . "ISO8859-14")
	    (nil . "ISO8859-15")
	    (nil . "ISO8859-16")
	    (nil . "VISCII1.1-1")
	    ,(font-spec :registry "iso10646-1" :script 'latin))

     (thai  ,(font-spec :registry "iso10646-1" :otf '(thai nil nil (mark)))
	    ,(font-spec :registry "iso10646-1" :script 'thai)
	    (nil . "TIS620*")
	    (nil . "ISO8859-11"))

     (devanagari ,(font-spec :registry "iso10646-1" :otf '(deva nil (rphf)))
		 (nil . "iso10646.indian-1"))
     (bengali ,(font-spec :registry "iso10646-1" :otf '(beng nil (rphf))))
     (gurmukhi ,(font-spec :registry "iso10646-1" :otf '(guru nil (blwf))))
     (gujarati ,(font-spec :registry "iso10646-1" :otf '(gujr nil (rphf))))
     (oriya ,(font-spec :registry "iso10646-1" :otf '(orya nil (rphf))))
     (tamil ,(font-spec :registry "iso10646-1" :otf '(taml nil (akhn))))
     (telugu ,(font-spec :registry "iso10646-1" :otf '(telu nil (blwf))))
     (kannada ,(font-spec :registry "iso10646-1" :otf '(knda nil (rphf))))
     (sinhala ,(font-spec :registry "iso10646-1" :otf '(sinh nil (akhn))))
     (malayalam ,(font-spec :registry "iso10646-1" :otf '(mlym nil (akhn))))

     (burmese ,(font-spec :registry "iso10646-1" :otf '(mymr nil nil))
	      ,(font-spec :registry "iso10646-1" :script 'burmese))

     (lao ,(font-spec :registry "iso10646-1" :otf '(lao\  nil nil (mark)))
	  ,(font-spec :registry "iso10646-1" :script 'lao)
	  (nil . "MuleLao-1"))

     (tai-viet ("TaiViet" . "iso10646-1"))

     (greek ,(font-spec :registry "iso10646-1" :script 'greek)
	    (nil . "ISO8859-7"))

     (cyrillic ,(font-spec :registry "iso10646-1" :script 'cyrillic)
	       (nil . "ISO8859-5")
	       (nil . "microsoft-cp1251")
	       (nil . "koi8-r"))

     (arabic ,(font-spec :registry "iso10646-1"
			 :otf '(arab nil (init medi fina liga)))
	     (nil . "MuleArabic-0")
	     (nil . "MuleArabic-1")
	     (nil . "MuleArabic-2")
	     (nil . "ISO8859-6"))

     (hebrew ,(font-spec :registry "iso10646-1" :script 'hebrew)
	     (nil . "ISO8859-8"))

     (khmer ,(font-spec :registry "iso10646-1" :otf '(khmr nil (pres))))

     (kana (nil . "JISX0208*")
	   (nil . "GB2312.1980-0")
	   (nil . "KSC5601.1987*")
	   (nil . "JISX0201*")
	   (nil . "JISX0213.2000-1")
	   (nil . "JISX0213.2004-1")
	   ,(font-spec :registry "iso10646-1" :script 'kana))

     (bopomofo ,(font-spec :registry "iso10646-1" :script 'bopomofo)
	       (nil . "sisheng_cwnn-0"))

     (han (nil . "GB2312.1980-0")
	  (nil . "JISX0208*")
	  (nil . "JISX0212*")
	  (nil . "big5*")
	  (nil . "KSC5601.1987*")
	  (nil . "CNS11643.1992-1")
	  (nil . "CNS11643.1992-2")
	  (nil . "CNS11643.1992-3")
	  (nil . "CNS11643.1992-4")
	  (nil . "CNS11643.1992-5")
	  (nil . "CNS11643.1992-6")
	  (nil . "CNS11643.1992-7")
	  (nil . "gbk-0")
	  (nil . "gb18030")
	  (nil . "JISX0213.2000-1")
	  (nil . "JISX0213.2000-2")
	  (nil . "JISX0213.2004-1")
	  ,(font-spec :registry "iso10646-1" :lang 'ja)
	  ,(font-spec :registry "iso10646-1" :lang 'zh))

     (cjk-misc (nil . "GB2312.1980-0")
	       (nil . "JISX0208*")
	       (nil . "JISX0212*")
	       (nil . "big5*")
	       (nil . "KSC5601.1987*")
	       (nil . "CNS11643.1992-1")
	       (nil . "CNS11643.1992-2")
	       (nil . "CNS11643.1992-3")
	       (nil . "CNS11643.1992-4")
	       (nil . "CNS11643.1992-5")
	       (nil . "CNS11643.1992-6")
	       (nil . "CNS11643.1992-7")
	       (nil . "gbk-0")
	       (nil . "gb18030")
	       (nil . "JISX0213.2000-1")
	       (nil . "JISX0213.2000-2")
	       ,(font-spec :registry "iso10646-1" :lang 'ja)
	       ,(font-spec :registry "iso10646-1" :lang 'zh))

     (hangul (nil . "KSC5601.1987-0")
	     ,(font-spec :registry "iso10646-1" :lang 'ko))

     ;; for each charset
     (ascii (nil . "ISO8859-1"))
     (arabic-digit ("*" . "MuleArabic-0"))
     (arabic-1-column ("*" . "MuleArabic-1"))
     (arabic-2-column ("*" . "MuleArabic-2"))
     (indian-is13194 (nil . "is13194-devanagari"))
     (indian-1-column ("*" . "muleindian-2"))
     ;; Indian CDAC
     (devanagari-cdac (nil . "Devanagari-CDAC"))
     (sanskrit-cdac (nil . "Sanskrit-CDAC"))
     (bengali-cdac (nil . "Bengali-CDAC"))
     (assamese-cdac (nil . "Assamese-CDAC"))
     (punjabi-cdac (nil . "Punjabi-CDAC"))
     (gujarati-cdac (nil . "Gujarati-CDAC"))
     (oriya-cdac (nil . "Oriya-CDAC"))
     (tamil-cdac (nil . "Tamil-CDAC"))
     (telugu-cdac (nil . "Telugu-CDAC"))
     (kannada-cdac (nil . "Kannada-CDAC"))
     (malayalam-cdac (nil . "Malayalam-CDAC"))
     ;; Indian AKRUTI
     (devanagari-akruti (nil . "Devanagari-Akruti"))
     (bengali-akruti (nil . "Bengali-Akruti"))
     (punjabi-akruti (nil . "Punjabi-Akruti"))
     (gujarati-akruti (nil . "Gujarati-Akruti"))
     (oriya-akruti (nil . "Oriya-Akruti"))
     (tamil-akruti (nil . "Tamil-Akruti"))
     (telugu-akruti (nil . "Telugu-Akruti"))
     (kannada-akruti (nil . "Kannada-Akruti"))
     (malayalam-akruti (nil . "Malayalam-Akruti"))

     ;; Fallback fonts
     (nil (nil . "gb2312.1980")
	  (nil . "gbk-0")
	  (nil . "gb18030")
	  (nil . "jisx0208")
	  (nil . "ksc5601.1987")
	  (nil . "CNS11643.1992-1")
	  (nil . "CNS11643.1992-2")
	  (nil . "CNS11643.1992-3")
	  (nil . "CNS11643.1992-4")
	  (nil . "CNS11643.1992-5")
	  (nil . "CNS11643.1992-6")
	  (nil . "CNS11643.1992-7")
	  (nil . "big5")
	  (nil . "jisx0213.2000-1")
	  (nil . "jisx0213.2004-1")
	  (nil . "jisx0212"))
     ))

  ;; For simple scripts
  (dolist (script '(phonetic
		    armenian
		    syriac
		    thaana
		    georgian
		    cherokee
		    canadian-aboriginal
		    ogham
		    runic
		    symbol
		    braille
		    yi
		    aegean-number
		    ancient-greek-number
		    ancient-symbol
		    phaistos-disc
		    lycian
		    carian
		    olt-italic
		    ugaritic
		    old-persian
		    deseret
		    shavian
		    osmanya
		    cypriot-syllabary
		    phoenician
		    lydian
		    kharoshthi
		    cuneiform
		    cuneiform-numbers-and-punctuation
		    byzantine-musical-symbol
		    musical-symbol
		    ancient-greek-musical-notation
		    tai-xuan-jing-symbol
		    counting-rod-numeral
		    mahjong-tile
		    domino-tile))
    (set-fontset-font "fontset-default"
		      script (font-spec :registry "iso10646-1" :script script)
		      nil 'append))

  ;; Special settings for `MATHEMATICAL (U+1D400..U+1D7FF)'.
  (dolist (math-subgroup '((#x1D400 #x1D433 mathematical-bold)
			   (#x1D434 #x1D467 mathematical-italic)
			   (#x1D468 #x1D49B mathematical-bold-italic)
			   (#x1D49C #x1D4CF mathematical-script)
			   (#x1D4D0 #x1D503 mathematical-bold-script)
			   (#x1D504 #x1D537 mathematical-fraktur)
			   (#x1D538 #x1D56B mathematical-double-struck)
			   (#x1D56C #x1D59F mathematical-bold-fraktur)
			   (#x1D5A0 #x1D5D3 mathematical-sans-serif)
			   (#x1D5D4 #x1D607 mathematical-sans-serif-bold)
			   (#x1D608 #x1D63B mathematical-sans-serif-italic)
			   (#x1D63C #x1D66F mathematical-sans-serif-bold-italic)
			   (#x1D670 #x1D6A3 mathematical-monospace)
			   (#x1D6A4 #x1D6A5 mathematical-italic)
			   (#x1D6A8 #x1D6E1 mathematical-bold)
			   (#x1D6E2 #x1D71B mathematical-italic)
			   (#x1D71C #x1D755 mathematical-bold-italic)
			   (#x1D756 #x1D78F mathematical-sans-serif-bold)
			   (#x1D790 #x1D7C9 mathematical-sans-serif-bold-italic)
			   (#x1D7CA #x1D7D7 mathematical-bold)
			   (#x1D7D8 #x1D7E1 mathematical-double-struck)
			   (#x1D7E2 #x1D7EB mathematical-sans-serif)
			   (#x1D7EC #x1D7F5 mathematical-sans-serif-bold)
			   (#x1D7F6 #x1D7FF mathematical-monospace)))
    (let ((slot (assq (nth 2 math-subgroup) script-representative-chars)))
      (if slot
	  (if (vectorp (cdr slot))
	      (setcdr slot (vconcat (cdr slot) (vector (car math-subgroup))))
	    (setcdr slot (vector (cadr slot) (car math-subgroup))))
	(setq slot (list (nth 2 math-subgroup) (car math-subgroup)))
	(nconc script-representative-chars (list slot))))
    (set-fontset-font
     "fontset-default"
     (cons (car math-subgroup) (nth 1 math-subgroup))
     (font-spec :registry "iso10646-1" :script (nth 2 math-subgroup))))

  ;; Append CJK fonts for characters other than han, kana, cjk-misc.
  ;; Append fonts for scripts whose name is also a charset name.
  (let* ((data (build-default-fontset-data))
	 (registries (car data)))
    (dolist (target-spec (cdr data))
      (let ((target (car target-spec))
	    (spec (cdr target-spec)))
	(if (integerp spec)
	    (dotimes (i (length registries))
	      (if (> (logand spec (lsh 1 i)) 0)
		  (set-fontset-font "fontset-default" target
				    (cons nil (aref registries i))
				    nil 'append)))
	(dolist (args spec)
	  (set-fontset-font "fontset-default" target
			    (apply 'font-spec args) nil 'append))))))

  ;; Append Unicode fonts.
  ;; This may find fonts with more variants (bold, italic) but which
  ;; don't cover many characters.
  (set-fontset-font "fontset-default" nil
		    '(nil . "iso10646-1") nil 'prepend)
  ;; These may find fonts that cover many characters but with fewer
  ;; variants.
  (set-fontset-font "fontset-default" nil
		    '("gnu-unifont" . "iso10646-1") nil 'prepend)
  (set-fontset-font "fontset-default" nil
		    '("mutt-clearlyu" . "iso10646-1") nil 'prepend)
  (set-fontset-font "fontset-default" '(#x20000 . #x2FFFF)
		    '(nil . "unicode-sip"))

  (set-fontset-font "fontset-default" '(#xE000 . #xF8FF)
		    '(nil . "iso10646-1"))
  ;; Don't try the fallback fonts even if no suitable font was found
  ;; by the above font-spec.
  (set-fontset-font "fontset-default" '(#xE000 . #xF8FF) nil nil 'append))

(defun create-default-fontset ()
  "Create the default fontset.
Internal use only.  Should be called at startup time."
  (condition-case err
      (setup-default-fontset)
    (error (display-warning
	    'initialization
	    (format "Creation of the default fontsets failed: %s" err)
	    :error))))

;; These are the registered registries/encodings from
;; ftp://ftp.x.org/pub/DOCS/registry 2001/06/01

;; Name                                            Reference
;; ----                                            ---------
;; "DEC"                                           [27]
;;         registry prefix
;; "DEC.CNS11643.1986-2"                           [53]
;;         CNS11643 2-plane using the encoding
;;         suggested in that standard
;; "DEC.DTSCS.1990-2"                              [54]
;;         DEC Taiwan Supplemental Character Set
;; "fujitsu.u90x01.1991-0"                         [87]
;; "fujitsu.u90x03.1991-0"                         [87]
;; "GB2312.1980-0"                                 [39],[12]
;;         China (PRC) Hanzi, GL encoding
;; "GB2312.1980-1"                                 [39]
;;         (deprecated)
;;         China (PRC) Hanzi, GR encoding
;; "HP-Arabic8"                                    [36]
;;         HPARABIC8 8-bit character set
;; "HP-East8"                                      [36]
;;         HPEAST8 8-bit character set
;; "HP-Greek8"                                     [36]
;;         HPGREEK8 8-bit character set
;; "HP-Hebrew8"                                    [36]
;;         HPHEBREW8 8-bit character set
;; "HP-Japanese15"                                 [36]
;;         HPJAPAN15 15-bit character set,
;;         modified from industry de facto
;;         standard Shift-JIS
;; "HP-Kana8"                                      [36]
;;         HPKANA8 8-bit character set
;; "HP-Korean15"                                   [36]
;;         HPKOREAN15 15-bit character set
;; "HP-Roman8"                                     [36]
;;         HPROMAN8 8-bit character set
;; "HP-SChinese15"                                 [36]
;;         HPSCHINA15 15-bit character set for
;;         support of Simplified Chinese
;; "HP-TChinese15"                                 [36]
;;         HPTCHINA15 15-bit character set for
;;         support of Traditional Chinese
;; "HP-Turkish8"                                   [36]
;;         HPTURKISH8 8-bit character set
;; "IPSYS"                                         [59]
;;         registry prefix
;; "IPSYS.IE-1"                                    [59]
;; "ISO2022"<REG>"-"<ENC>                          [44]
;; "ISO646.1991-IRV"                               [107]
;;         ISO 646 International Reference Version
;; "ISO8859-1"                                     [15],[12]
;;         ISO Latin alphabet No. 1
;; "ISO8859-2"                                     [15],[12]
;;         ISO Latin alphabet No. 2
;; "ISO8859-3"                                     [15],[12]
;;         ISO Latin alphabet No. 3
;; "ISO8859-4"                                     [15],[12]
;;         ISO Latin alphabet No. 4
;; "ISO8859-5"                                     [15],[12]
;;         ISO Latin/Cyrillic alphabet
;; "ISO8859-6"                                     [15],[12]
;;         ISO Latin/Arabic alphabet
;; "ISO8859-7"                                     [15],[12]
;;         ISO Latin/Greek alphabet
;; "ISO8859-8"                                     [15],[12]
;;         ISO Latin/Hebrew alphabet
;; "ISO8859-9"                                     [15],[12]
;;         ISO Latin alphabet No. 5
;; "ISO8859-10"                                    [15],[12]
;;         ISO Latin alphabet No. 6
;; "ISO8859-13"                                    [15],[12]
;;         ISO Latin alphabet No. 7
;; "ISO8859-14"                                    [15],[12]
;;         ISO Latin alphabet No. 8
;; "ISO8859-15"                                    [15],[12]
;;         ISO Latin alphabet No. 9
;; "FCD8859-15"                                    [7]
;;         (deprecated)
;;         ISO Latin alphabet No. 9, Final Committee Draft
;; "ISO10646-1"                                    [133]
;;         Unicode Universal Multiple-Octet Coded Character Set
;; "ISO10646-MES"                                  [133]
;;         (deprecated)
;;         Unicode Minimum European Subset
;; "JISX0201.1976-0"                               [38],[12]
;;         8-Bit Alphanumeric-Katakana Code
;; "JISX0208.1983-0"                               [40],[12]
;;         Japanese Graphic Character Set,
;;         GL encoding
;; "JISX0208.1990-0"                               [71]
;;         Japanese Graphic Character Set,
;;         GL encoding
;; "JISX0208.1983-1"                               [40]
;;         (deprecated)
;;         Japanese Graphic Character Set,
;;         GR encoding
;; "JISX0212.1990-0"                               [72]
;;         Supplementary Japanese Graphic Character Set,
;;         GL encoding
;; "KOI8-R"                                        [119]
;;         Cyrillic alphabet
;; "KSC5601.1987-0"                                [41],[12]
;;         Korean Graphic Character Set,
;;         GL encoding
;; "KSC5601.1987-1"                                [41]
;;         (deprecated)
;;         Korean Graphic Character Set,
;;         GR encoding
;; "omron_CNS11643-0"                              [45]
;; "omron_CNS11643-1"                              [45]
;; "omron_BIG5-0"                                  [45]
;; "omron_BIG5-1"                                  [45]
;; "wn.tamil.1993"                                 [103]

(defun set-font-encoding (pattern charset)
  "Set arguments in `font-encoding-alist' (which see)."
  (let ((slot (assoc pattern font-encoding-alist)))
    (if slot
	(setcdr slot charset)
      (setq font-encoding-alist
	    (cons (cons pattern charset) font-encoding-alist)))))

(defvar x-pixel-size-width-font-regexp)
(defvar vertical-centering-font-regexp)

;; Setting for suppressing XLoadQueryFont on big fonts.
(setq x-pixel-size-width-font-regexp
      (purecopy "gb2312\\|gbk\\|gb18030\\|jisx0208\\|ksc5601\\|cns11643\\|big5"))

;; These fonts require vertical centering.
(setq vertical-centering-font-regexp
      (purecopy "gb2312\\|gbk\\|gb18030\\|jisx0208\\|jisx0212\\|ksc5601\\|cns11643\\|big5"))

;; CDAC fonts are actually smaller than their design sizes.
(setq face-font-rescale-alist
      (list (cons (purecopy "-cdac$")  1.3)))

(defvar x-font-name-charset-alist nil
  "This variable has no meaning now.  Just kept for backward compatibility.")

;;; XLFD (X Logical Font Description) format handler.

;; Define XLFD's field index numbers.		; field name
(defconst xlfd-regexp-family-subnum 0)		; FOUNDRY and FAMILY
(defconst xlfd-regexp-weight-subnum 1)		; WEIGHT_NAME
(defconst xlfd-regexp-slant-subnum 2)		; SLANT
(defconst xlfd-regexp-swidth-subnum 3)		; SETWIDTH_NAME
(defconst xlfd-regexp-adstyle-subnum 4)		; ADD_STYLE_NAME
(defconst xlfd-regexp-pixelsize-subnum 5)	; PIXEL_SIZE
(defconst xlfd-regexp-pointsize-subnum 6)	; POINT_SIZE
(defconst xlfd-regexp-resx-subnum 7)		; RESOLUTION_X
(defconst xlfd-regexp-resy-subnum 8)		; RESOLUTION_Y
(defconst xlfd-regexp-spacing-subnum 8)		; SPACING
(defconst xlfd-regexp-avgwidth-subnum 10)	; AVERAGE_WIDTH
(defconst xlfd-regexp-registry-subnum 11)	; REGISTRY and ENCODING

;; Regular expression matching against a fontname which conforms to
;; XLFD (X Logical Font Description).  All fields in XLFD should be
;; not be omitted (but can be a wild card) to be matched.
(defconst xlfd-tight-regexp
  "^\
-\\([^-]*-[^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)\
-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*-[^-]*\\)$")

;; Regular expression matching against a fontname which conforms to
;; XLFD (X Logical Font Description).  All fields in XLFD from FOUNDRY
;; to ADSTYLE, REGISTRY, and ENCODING should be not be omitted (but
;; can be a wild card) to be matched.
(defconst xlfd-style-regexp
  "^\
-\\([^-]*-[^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-\\([^-]*\\)-.*\
-\\([^-]*-[^-]*\\)$")

;; List of field numbers of XLFD whose values are numeric.
(defconst xlfd-regexp-numeric-subnums
  (list xlfd-regexp-pixelsize-subnum	;5
	xlfd-regexp-pointsize-subnum	;6
	xlfd-regexp-resx-subnum		;7
	xlfd-regexp-resy-subnum		;8
	xlfd-regexp-avgwidth-subnum	;10
	))

(defun x-decompose-font-name (pattern)
  "Decompose PATTERN into XLFD fields and return a vector of the fields.
The length of the vector is 12.
The FOUNDRY and FAMILY fields are concatenated and stored in the first
element of the vector.
The REGISTRY and ENCODING fields are concatenated and stored in the last
element of the vector.

Return nil if PATTERN doesn't conform to XLFD."
  (if (string-match xlfd-tight-regexp pattern)
      (let ((xlfd-fields (make-vector 12 nil)))
	(dotimes (i 12)
	  (aset xlfd-fields i (match-string (1+ i) pattern)))
	(dotimes (i 12)
	  (if (string-match "^[*-]+$" (aref xlfd-fields i))
	      (aset xlfd-fields i nil)))
	xlfd-fields)))

(defun x-compose-font-name (fields &optional reduce)
  "Compose X fontname from FIELDS.
FIELDS is a vector of XLFD fields, of length 12.
If a field is nil, wild-card letter `*' is embedded.
Optional argument REDUCE exists just for backward compatibility,
and is always ignored."
  (concat "-" (mapconcat (lambda (x) (or x "*")) fields "-")))


(defun x-must-resolve-font-name (xlfd-fields)
  "Like `x-resolve-font-name', but always return a font name.
XLFD-FIELDS is a vector of XLFD (X Logical Font Description) fields.
If no font matching XLFD-FIELDS is available, successively replace
parts of the font name pattern with \"*\" until some font is found.
Value is name of that font."
  (let ((ascii-font nil) (index 0))
    (while (and (null ascii-font) (<= index xlfd-regexp-registry-subnum))
      (let ((pattern (x-compose-font-name xlfd-fields)))
	(condition-case nil
	    (setq ascii-font (x-resolve-font-name pattern))
	  (error
	   (message "Warning: no fonts matching `%s' available" pattern)
	   (aset xlfd-fields index "*")
	   (setq index (1+ index))))))
    (unless ascii-font
      (error "No fonts found"))
    ascii-font))


(defun x-complement-fontset-spec (default-spec fontlist)
  "Complement elements of FONTLIST based on DEFAULT-SPEC.
DEFAULT-SPEC is a font-spec object providing default font properties.
FONTLIST is an alist of script names vs the corresponding font names.

The font names are parsed and unspecified font properties are
given from DEFAULT-SPEC."
  (let ((prop-list '(:foundry :family :weight :slant :width :adstyle :size)))
    (dolist (elt fontlist)
      (let ((spec (font-spec :name (cadr elt))))
	(dolist (prop prop-list)
	  (let ((val (font-get spec prop)))
	    (or val
		(font-put spec prop (font-get default-spec prop)))))
	(setcar (cdr elt) spec)))
    fontlist))

(defun fontset-name-p (fontset)
  "Return non-nil if FONTSET is valid as fontset name.
A valid fontset name should conform to XLFD (X Logical Font Description)
with \"fontset\" in `<CHARSET_REGISTRY>' field."
  (and (string-match xlfd-tight-regexp fontset)
       (string= (match-string (1+ xlfd-regexp-registry-subnum) fontset)
		"fontset")))

(declare-function fontset-list "fontset.c" ())

(defun generate-fontset-menu ()
  "Return list to be appended to `x-fixed-font-alist'.
Done when `mouse-set-font' is called."
  (let (l)
    (dolist (fontset (fontset-list))
      (or (string-match "fontset-default$" fontset)
	  (string-match "fontset-auto[0-9]+$" fontset)
	  (push (list (fontset-plain-name fontset) fontset) l)))
    (cons "Fontset"
	  (sort l #'(lambda (x y) (string< (car x) (car y)))))))

(declare-function query-fontset "fontset.c" (pattern &optional regexpp))

(defun fontset-plain-name (fontset)
  "Return a plain and descriptive name of FONTSET."
  (if (not (setq fontset (query-fontset fontset)))
      (error "Invalid fontset: %s" fontset))
  (let ((xlfd-fields (x-decompose-font-name fontset)))
    (if xlfd-fields
	(let ((family (aref xlfd-fields xlfd-regexp-family-subnum))
	      (weight (aref xlfd-fields xlfd-regexp-weight-subnum))
	      (slant  (aref xlfd-fields xlfd-regexp-slant-subnum))
	      (swidth (aref xlfd-fields xlfd-regexp-swidth-subnum))
	      (size   (aref xlfd-fields xlfd-regexp-pixelsize-subnum))
	      (nickname (aref xlfd-fields xlfd-regexp-registry-subnum))
	      name)
	  (if (not (string-match "^fontset-\\(.*\\)$" nickname))
	      (setq nickname family)
	    (setq nickname (match-string 1 nickname)))
	  (if (and size (> (string-to-number size) 0))
	      (setq name (format "%s: %s-dot" nickname size))
	    (setq name nickname))
	  (and weight
	       (cond ((string-match "^medium$" weight)
		      (setq name (concat name " " "medium")))
		     ((string-match "^bold$\\|^demibold$" weight)
		      (setq name (concat name " " weight)))))
	  (and slant
	       (cond ((string-match "^i$" slant)
		      (setq name (concat name " " "italic")))
		     ((string-match "^o$" slant)
		      (setq name (concat name " " "slant")))
		     ((string-match "^ri$" slant)
		      (setq name (concat name " " "reverse italic")))
		     ((string-match "^ro$" slant)
		      (setq name (concat name " " "reverse slant")))))
	  name)
      fontset)))

(defvar charset-script-alist
  '((ascii . latin)
    (latin-iso8859-1 . latin)
    (latin-iso8859-2 . latin)
    (latin-iso8859-3 . latin)
    (latin-iso8859-4 . latin)
    (latin-iso8859-9 . latin)
    (latin-iso8859-10 . latin)
    (latin-iso8859-13 . latin)
    (latin-iso8859-14 . latin)
    (latin-iso8859-15 . latin)
    (latin-iso8859-16 . latin)
    (latin-jisx0201 . latin)
    (thai-tis620 . thai)
    (cyrillic-iso8859-5 . cyrillic)
    (arabic-iso8859-6 . arabic)
    (greek-iso8859-7 . latin)
    (hebrew-iso8859-8 . latin)
    (katakana-jisx0201 . kana)
    (chinese-gb2312 . han)
    (chinese-gbk . han)
    (gb18030-2-byte . han)
    (gb18030-4-byte-bmp . han)
    (gb18030-4-byte-ext-1 . han)
    (gb18030-4-byte-ext-2 . han)
    (gb18030-4-byte-smp . han)
    (chinese-big5-1 . han)
    (chinese-big5-2 . han)
    (chinese-cns11643-1 . han)
    (chinese-cns11643-2 . han)
    (chinese-cns11643-3 . han)
    (chinese-cns11643-4 . han)
    (chinese-cns11643-5 . han)
    (chinese-cns11643-6 . han)
    (chinese-cns11643-7 . han)
    (japanese-jisx0208 . han)
    (japanese-jisx0208-1978 . han)
    (japanese-jisx0212 . han)
    (japanese-jisx0213-1 . han)
    (japanese-jisx0213-2 . han)
    (korean-ksc5601 . hangul)
    (chinese-sisheng . bopomofo)
    (vietnamese-viscii-lower . latin)
    (vietnamese-viscii-upper . latin)
    (arabic-digit . arabic)
    (arabic-1-column . arabic)
    (arabic-2-column . arabic)
    (indian-is13194 . devanagari)
    (indian-glyph . devanagari)
    (indian-1-column . devanagari)
    (indian-2-column . devanagari)
    (tibetan-1-column . tibetan))
  "Alist of charsets vs the corresponding most appropriate scripts.

This alist is used by the function `create-fontset-from-fontset-spec'
to map charsets to scripts.")

(defun create-fontset-from-fontset-spec (fontset-spec
					 &optional style-variant noerror)
  "Create a fontset from fontset specification string FONTSET-SPEC.
FONTSET-SPEC is a string of the format:
	FONTSET-NAME,SCRIPT-NAME0:FONT-NAME0,SCRIPT-NAME1:FONT-NAME1, ...
Any number of SPACE, TAB, and NEWLINE can be put before and after commas.

When a frame uses the fontset as the `font' parameter, the frame's
default font name is derived from FONTSET-NAME by substituting
\"iso8859-1\" for the tail part \"fontset-XXX\".  But, if SCRIPT-NAMEn
is \"ascii\", use the corresponding FONT-NAMEn as the default font
name.

Optional 2nd and 3rd arguments exist just for backward compatibility,
and are ignored.

It returns a name of the created fontset.

For backward compatibility, SCRIPT-NAME may be a charset name, in
which case, the corresponding script is decided by the variable
`charset-script-alist' (which see)."
  (or (string-match "^[^,]+" fontset-spec)
      (error "Invalid fontset spec: %s" fontset-spec))
  (let ((idx (match-end 0))
	(name (match-string 0 fontset-spec))
	default-spec target script fontlist)
    (or (string-match xlfd-tight-regexp name)
	(error "Fontset name \"%s\" not conforming to XLFD" name))
    (setq default-spec (font-spec :name name))
    ;; At first, extract pairs of charset and fontname from FONTSET-SPEC.
    (while (string-match "[, \t\n]*\\([^:]+\\):[ \t]*\\([^,]+\\)"
			 fontset-spec idx)
      (setq idx (match-end 0))
      (setq target (intern (match-string 1 fontset-spec)))
      (cond ((or (eq target 'ascii)
		 (memq target (char-table-extra-slot char-script-table 0)))
	     (push (list target (match-string 2 fontset-spec)) fontlist))
	    ((setq script (cdr (assq target charset-script-alist)))
	     (push (list script (match-string 2 fontset-spec)) fontlist))
	    ((charsetp target)
	     (push (list target (match-string 2 fontset-spec)) fontlist))))

    ;; Complement FONTLIST.
    (setq fontlist (x-complement-fontset-spec default-spec fontlist))

    ;; Create a fontset.
    (new-fontset name (nreverse fontlist))))

(defun create-fontset-from-ascii-font (font &optional resolved-font
					    fontset-name)
  "Create a fontset from an ASCII font FONT.

Optional 2nd arg RESOLVED-FONT is a resolved name of FONT.
If omitted, `x-resolve-font-name' is called to get the resolved name.
At this time, if FONT is not available, an error is signaled.

Optional 3rd arg FONTSET-NAME is a string to be used in
`<CHARSET_ENCODING>' fields of a new fontset name.  If it is omitted,
an appropriate name is generated automatically.

It returns a name of the created fontset."
  (setq font (downcase font))
  (setq resolved-font
	(downcase (or resolved-font (x-resolve-font-name font))))
  (let ((xlfd (x-decompose-font-name resolved-font))
	fontset)
    (if fontset-name
	(setq fontset-name (downcase fontset-name))
      (if (query-fontset "fontset-startup")
	  (setq fontset-name
		(subst-char-in-string
		 ?- ?_ (aref xlfd xlfd-regexp-registry-subnum) t))
	(setq fontset-name "startup")))
    (aset xlfd xlfd-regexp-registry-subnum
	  (format "fontset-%s" fontset-name))
    (setq fontset (x-compose-font-name xlfd))
    (or (query-fontset fontset)
	(create-fontset-from-fontset-spec (concat fontset ", ascii:" font)))))


;; Create standard fontset from 16 dots fonts which are the most widely
;; installed fonts.  Fonts for Chinese-GB, Korean, and Chinese-CNS are
;; specified here because FAMILY of those fonts are not "fixed" in
;; many cases.
(defvar standard-fontset-spec
  (purecopy "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard")
  "String of fontset spec of the standard fontset.
You have the biggest chance to display international characters
with correct glyphs by using the standard fontset.
See the documentation of `create-fontset-from-fontset-spec' for the format.")


;; Create fontsets from X resources of the name `fontset-N (class
;; Fontset-N)' where N is integer 0, 1, ...
;; The values of the resources the string of the same format as
;; `standard-fontset-spec'.

(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))

(defun create-fontset-from-x-resource ()
  (let ((idx 0)
	fontset-spec)
    (while (setq fontset-spec (x-get-resource (format "fontset-%d" idx)
					      (format "Fontset-%d" idx)))
      (condition-case nil
	  (create-fontset-from-fontset-spec fontset-spec t)
	(error (display-warning
		'initialization
		(format "Fontset-%d: invalid specification in X resource" idx)
		:warning)))
      (setq idx (1+ idx)))))

;;
(provide 'fontset)

;;; fontset.el ends here
