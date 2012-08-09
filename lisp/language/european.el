;;; european.el --- support for European languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997-1998, 2000-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, European

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

;; For European scripts, all the ISO Latin character sets are
;; supported, along with various others.

;;; Code:

;; Latin-1 (ISO-8859-1)

(set-language-info-alist
 "Latin-1" '((charset iso-8859-1)
	     (coding-system iso-latin-1 iso-latin-9 windows-1252)
	     (coding-priority iso-latin-1)
	     (nonascii-translation . iso-8859-1)
	     (unibyte-display . iso-latin-1)
	     (input-method . "latin-1-prefix")
	     (sample-text
	      . "Hello, Hej, Tere, Hei, Bonjour, Gr$(D+d)N(B Gott, Ciao, $(D"B(BHola!")
	     (documentation . "\
This language environment is a generic one for the Latin-1 (ISO-8859-1)
character set which supports the following European languages:
 Albanian, Basque, Breton, Catalan, Danish, Dutch, English, Faeroese,
 Finnish, French (with restrictions -- see Latin-9), Frisian, Galician,
 German, Greenlandic, Icelandic, Irish Gaelic (new orthography),
 Italian, Latin, Luxemburgish, Norwegian, Portuguese, Rhaeto-Romanic,
 Scottish Gaelic, Spanish, and Swedish.
We also have specific language environments for the following languages:
  For Dutch, \"Dutch\".
  For German, \"German\".
  For French, \"French\".
  For Italian, \"Italian\".
  For Slovenian, \"Slovenian\".
  For Spanish, \"Spanish\".

Latin-1 also covers several written languages outside Europe, including
Indonesian/Malay, Tagalog (Philippines), Swahili and Afrikaans."))
 '("European"))


;; Latin-2 (ISO-8859-2)

(define-coding-system 'iso-latin-2
   "ISO 2022 based 8-bit encoding for Latin-2 (MIME:ISO-8859-2)."
  :coding-type 'charset
  :mnemonic ?2
  :charset-list '(iso-8859-2)
  :mime-charset 'iso-8859-2)

(define-coding-system-alias 'iso-8859-2 'iso-latin-2)
(define-coding-system-alias 'latin-2 'iso-latin-2)

(set-language-info-alist
 "Latin-2" '((charset iso-8859-2)
	     (coding-system iso-latin-2 windows-1250)
	     (coding-priority iso-latin-2)
	     (nonascii-translation . iso-8859-2)
	     (unibyte-display . iso-latin-2)
	     (input-method . "latin-2-prefix")
	     (documentation . "\
This language environment is a generic one for the Latin-2 (ISO-8859-2)
character set which supports the following languages:
 Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbo-Croatian or Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish.
We also have specific language environments for the following languages:
  For Czech, \"Czech\".
  For Croatian, \"Croatian\".
  For Polish, \"Polish\".
  For Romanian, \"Romanian\".
  For Slovak, \"Slovak\"."))
 '("European"))


;; Latin-3 (ISO-8859-3)

(define-coding-system 'iso-latin-3
  "ISO 2022 based 8-bit encoding for Latin-3 (MIME:ISO-8859-3)."
  :coding-type 'charset
  :mnemonic ?3
  :charset-list '(iso-8859-3)
  :mime-charset 'iso-8859-3)

(define-coding-system-alias 'iso-8859-3 'iso-latin-3)
(define-coding-system-alias 'latin-3 'iso-latin-3)

(set-language-info-alist
 "Latin-3" '((charset iso-8859-3)
	     (coding-system iso-latin-3)
	     (coding-priority iso-latin-3)
	     (nonascii-translation . iso-8859-3)
	     (unibyte-display . iso-latin-3)
	     (input-method . "latin-3-prefix")
	     (documentation . "\
These languages are supported with the Latin-3 (ISO-8859-3) character set:
 Afrikaans, Catalan, Dutch, English, Esperanto, French, Galician,
 German, Italian, Maltese, Spanish, and Turkish."))
 '("European"))


;; Latin-4 (ISO-8859-4)

(define-coding-system 'iso-latin-4
  "ISO 2022 based 8-bit encoding for Latin-4 (MIME:ISO-8859-4)."
  :coding-type 'charset
  :mnemonic ?4
  :charset-list '(iso-8859-4)
  :mime-charset 'iso-8859-4)

(define-coding-system-alias 'iso-8859-4 'iso-latin-4)
(define-coding-system-alias 'latin-4 'iso-latin-4)

(set-language-info-alist
 "Latin-4" '((charset iso-8859-4)
	     (coding-system iso-8859-4)
	     (coding-priority iso-8859-4)
	     (nonascii-translation . iso-8859-4)
	     (unibyte-display . iso-8859-4)
	     (input-method . "latin-4-postfix")
	     (documentation . "\
These languages are supported with the Latin-4 (ISO-8859-4) character set:
 Danish, English, Estonian, Finnish, German, Greenlandic, Latvian,
 Lithuanian, Norwegian, and Sami."))
 '("European"))


;; Latin-5 (ISO-8859-9)

(define-coding-system 'iso-latin-5
  "ISO 2022 based 8-bit encoding for Latin-5 (MIME:ISO-8859-9)."
  :coding-type 'charset
  :mnemonic ?9
  :charset-list '(iso-8859-9)
  :mime-charset 'iso-8859-9)

(define-coding-system-alias 'iso-8859-9 'iso-latin-5)
(define-coding-system-alias 'latin-5 'iso-latin-5)

(set-language-info-alist
 "Latin-5" '((charset iso-8859-9)
	     (coding-system iso-latin-5)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . iso-8859-9)
	     (unibyte-display . iso-latin-5)
	     (input-method . "latin-5-postfix")
	     (documentation . "Support for Latin-5.\
See also the Turkish environment."))
 '("European"))


;; Latin-6 (ISO-8859-10)

(define-coding-system 'iso-latin-6
  "ISO 2022 based 8-bit encoding for Latin-6 (MIME:ISO-8859-10)."
  :coding-type 'charset
  :mnemonic ?9
  :charset-list '(iso-8859-10)
  :mime-charset 'iso-8859-10)

(define-coding-system-alias 'iso-8859-10 'iso-latin-6)
(define-coding-system-alias 'latin-6 'iso-latin-6)

(set-language-info-alist
 "Latin-6" '((charset iso-8859-10)
	     (coding-system iso-latin-6)
	     (coding-priority iso-latin-6)
	     (nonascii-translation . iso-8859-10)
	     (unibyte-display . iso-latin-6)
	     ;; Fixme: input method.
	     (documentation . "Support for generic Latin-6 (Northern European)."))
 '("European"))


;; Latin-7 (ISO-8859-13)

(define-coding-system 'iso-latin-7
  "ISO 2022 based 8-bit encoding for Latin-7 (MIME:ISO-8859-13)."
  :coding-type 'charset
  :mnemonic ?9
  :charset-list '(iso-8859-13)
  :mime-charset 'iso-8859-13)

(define-coding-system-alias 'iso-8859-13 'iso-latin-7)
(define-coding-system-alias 'latin-7 'iso-latin-7)

(set-language-info-alist
 "Latin-7" '((charset iso-8859-13)
	     (coding-system iso-latin-7)
	     (coding-priority iso-latin-7)
	     (nonascii-translation . iso-8859-13)
	     (unibyte-display . iso-latin-7)
	     ;; Fixme: input method.
	     (documentation . "Support for generic Latin-7 (Baltic Rim)."))
 '("European"))

;; Latin-8 (ISO-8859-14)

(define-coding-system 'iso-latin-8
  "ISO 2022 based 8-bit encoding for Latin-8 (MIME:ISO-8859-14)."
  :coding-type 'charset
  ;; `W' for `Welsh', since `C' for `Celtic' is taken.
  :mnemonic ?W
  :charset-list '(iso-8859-14)
  :mime-charset 'iso-8859-14)

(define-coding-system-alias 'iso-8859-14 'iso-latin-8)
(define-coding-system-alias 'latin-8 'iso-latin-8)

(set-language-info-alist
 "Latin-8" '((charset iso-8859-14)
	     (coding-system iso-latin-8)
	     (coding-priority iso-latin-8)
	     (nonascii-translation . iso-8859-14)
	     (unibyte-display . iso-latin-8)
	     (input-method . "latin-8-prefix")
	     ;; Fixme: Welsh/Ga{e}lic greetings
	     (sample-text . ",_"(B $(D+q(B $(D*t(B")
	     (documentation . "\
This language environment is a generic one for the Latin-8 (ISO-8859-14)
character set which supports the Celtic languages, including those not
covered by other ISO-8859 character sets:
 Welsh, Manx Gaelic and Irish Gaelic (old orthography)."))
 '("European"))

;; Latin-9 (ISO-8859-15)

(define-coding-system 'iso-latin-9
  "ISO 2022 based 8-bit encoding for Latin-9 (MIME:ISO-8859-15)."
  :coding-type 'charset
  ;; `0' for `Latin-0'
  :mnemonic ?0
  :charset-list '(iso-8859-15)
  :mime-charset 'iso-8859-15)

(define-coding-system-alias 'iso-8859-15 'iso-latin-9)
(define-coding-system-alias 'latin-9 'iso-latin-9)
(define-coding-system-alias 'latin-0 'iso-latin-9)

(set-language-info-alist
 "Latin-9" '((charset iso-8859-15)
	     (coding-system iso-latin-9)
	     (coding-priority iso-latin-9)
	     (nonascii-translation . iso-8859-15)
	     (unibyte-display . iso-latin-9)
	     (input-method . "latin-9-prefix")
	     (sample-text
	      . "AVE. $(D*^+^*v+v)-)M*s(B $(Q)!(B")
	     (documentation . "\
This language environment is a generic one for the Latin-9 (ISO-8859-15)
character set which supports the same languages as Latin-1 with the
addition of the Euro sign and some additional French and Finnish letters.
Latin-9 is sometimes nicknamed `Latin-0'."))
 '("European"))

(set-language-info-alist
 "Esperanto" '((tutorial . "TUTORIAL.eo")
	       (charset iso-8859-3)
	       (coding-system iso-latin-3)
	       (coding-priority iso-latin-3)
	       (nonascii-translation . latin-iso8859-3)
	       (unibyte-syntax . "latin-3")
	       (unibyte-display . iso-latin-3)
	       (input-method . "latin-3-prefix")
	       (documentation . "Support for Esperanto with ISO-8859-3 character set."))
 '("European"))


(define-coding-system 'windows-1250
  "windows-1250 (Central European) encoding (MIME: WINDOWS-1250)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(windows-1250)
  :mime-charset 'windows-1250)
(define-coding-system-alias 'cp1250 'windows-1250)

(define-coding-system 'windows-1252
  "windows-1252 (Western European) encoding (MIME: WINDOWS-1252)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(windows-1252)
  :mime-charset 'windows-1252)
(define-coding-system-alias 'cp1252 'windows-1252)

(define-coding-system 'windows-1254
  "windows-1254 (Turkish) encoding (MIME: WINDOWS-1254)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(windows-1254)
  :mime-charset 'windows-1254)
(define-coding-system-alias 'cp1254 'windows-1254)

(define-coding-system 'windows-1257
  "windows-1257 (Baltic) encoding (MIME: WINDOWS-1257)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(windows-1257)
  :mime-charset 'windows-1257)
(define-coding-system-alias 'cp1257 'windows-1257)

(define-coding-system 'cp850
  "DOS codepage 850 (Western European)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp850)
  :mime-charset 'cp850)
(define-coding-system-alias 'ibm850 'cp850)

(define-coding-system 'cp852
  "DOS codepage 852 (Slavic)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp852)
  :mime-charset 'cp852)
(define-coding-system-alias 'ibm852 'cp852)

(define-coding-system 'cp857
  "DOS codepage 857 (Turkish)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp857)
  :mime-charset 'cp857)
(define-coding-system-alias 'ibm857 'cp857)

(define-coding-system 'cp858
  "Codepage 858 (Multilingual Latin I + Euro)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp858)
  :mime-charset 'cp858)

(define-coding-system 'cp860
  "DOS codepage 860 (Portuguese)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp860)
  :mime-charset 'cp860)
(define-coding-system-alias 'ibm860 'cp860)

(define-coding-system 'cp861
  "DOS codepage 861 (Icelandic)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp861)
  :mime-charset 'cp861)
(define-coding-system-alias 'ibm861 'cp861)

(define-coding-system 'cp863
  "DOS codepage 863 (French Canadian)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp863)
  :mime-charset 'cp863)
(define-coding-system-alias 'ibm863 'cp863)

(define-coding-system 'cp865
  "DOS codepage 865 (Norwegian/Danish)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp865)
  :mime-charset 'cp865)
(define-coding-system-alias 'ibm865 'cp865)

(define-coding-system 'cp437
  "DOS codepage 437"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp437)
  :mime-charset 'cp437)
(define-coding-system-alias 'ibm437 'cp437)

(set-language-info-alist
 "Dutch" '((tutorial . "TUTORIAL.nl")
	   (charset iso-8859-1)
	   (coding-system iso-latin-1 iso-latin-9)
	   (coding-priority iso-latin-1)
	   (nonascii-translation . iso-8859-1)
	   (unibyte-display . iso-latin-1)
	   (input-method . "dutch")
	   (sample-text . "Er is een aantal manieren waarop je dit kan doen")
	   (documentation . "\
This language environment is almost the same as Latin-1,
but it selects the Dutch tutorial and input method."))
 '("European"))

(set-language-info-alist
 "German" '((tutorial . "TUTORIAL.de")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1 iso-latin-9)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (input-method . "german-postfix")
	    (unibyte-display . iso-latin-1)
	    (sample-text . "\
German (Deutsch Nord)	Guten Tag
German (Deutsch S$(D+d(Bd)	Gr$(D+d)N(B Gott")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but sets the default input method to \"german-postfix\".
Additionally, it selects the German tutorial."))
 '("European"))

(set-language-info-alist
 "French" '((tutorial . "TUTORIAL.fr")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1 iso-latin-9)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (unibyte-display . iso-latin-1)
	    (input-method . "latin-1-prefix")
	    (sample-text . "French (Fran$(D+.(Bais)	Bonjour, Salut")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but it selects the French tutorial and input method."))
 '("European"))

(set-language-info-alist
 "Italian" '((tutorial . "TUTORIAL.it")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1 iso-latin-9)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (unibyte-display . iso-latin-1)
	    (input-method . "italian-postfix")
	    (sample-text . "Salve, ciao!")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but sets the default input method to \"italian-postfix\".
Additionally, it selects the Italian tutorial."))
 '("European"))

(set-language-info-alist
 "Slovenian" '((charset iso-8859-2)
	      (coding-system . (iso-8859-2 windows-1250))
	      (coding-priority . (iso-8859-2))
	      (nonascii-translation . iso-8859-2)
	      (input-method . "slovenian")
	      (unibyte-display . iso-8859-2)
	      (tutorial . "TUTORIAL.sl")
	      (sample-text . "$(D*v(Belimo vam uspe$(D+^(Ben dan!")
	      (documentation . "\
This language environment is almost the same as Latin-2,
but it selects the Slovenian tutorial and input method."))
 '("European"))

(set-language-info-alist
 "Spanish" '((tutorial . "TUTORIAL.es")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1 iso-latin-9)
	    (coding-priority iso-latin-1)
	    (input-method . "spanish-postfix")
	    (nonascii-translation . iso-8859-1)
	    (unibyte-display . iso-latin-1)
	    (sample-text . "Spanish (Espa$(D+P(Bol)	$(D"B(BHola!")
	    (documentation . "\
This language environment is almost the same as Latin-1,
but it sets the default input method to \"spanish-postfix\",
and it selects the Spanish tutorial."))
 '("European"))

;; For Turkish, the character set ISO-8859-9 (Latin-5) is used.  But,
;; before the introduction of ISO-8859-9 in 1988, ISO-8859-3 (Latin-3)
;; was used for Turkish.  Those who use Latin-3 for Turkish should use
;; "Latin-3" language environment.

(set-language-info-alist
 "Turkish" '((charset iso-8859-9)
	     (coding-system iso-latin-5 windows-1254 iso-latin-3)
	     (coding-priority iso-latin-5)
	     (nonascii-translation . iso-8859-9)
	     (unibyte-display . iso-latin-5)
	     (input-method . "turkish-postfix")
	     (sample-text . "Turkish (T$(D+d(Brk$(D+.(Be)	Merhaba")
	     (setup-function . turkish-case-conversion-enable)
	     (setup-function . turkish-case-conversion-disable)
	     (documentation . "Support for Turkish.
Differs from the Latin-5 environment in using the `turkish-postfix' input
method and applying Turkish case rules for the characters i, I, $(D)E(B, $(D*D(B.")))

(defun turkish-case-conversion-enable ()
  "Set up Turkish case conversion of `i' and `I' into `$(D*D(B' and `$(D)E(B'."
  (let ((table (standard-case-table)))
    (set-case-syntax-pair ?$(D*D(B ?i table)
    (set-case-syntax-pair ?I ?$(D)E(B table)))

(defun turkish-case-conversion-disable ()
  "Set up normal (non-Turkish) case conversion of `i' into `I'."
  (let ((table (standard-case-table)))
    (set-case-syntax-pair ?I ?i table)
    (set-case-syntax ?$(D*D(B "w" table)
    (set-case-syntax ?$(D)E(B "w" table)))

;; Polish ISO 8859-2 environment.
;; Maintainer: Wlodek Bzyl <matwb@univ.gda.pl>
;; Keywords: multilingual, Polish

(set-language-info-alist
 "Polish" '((charset iso-8859-2)
	   (coding-system iso-8859-2 windows-1250)
	   (coding-priority iso-8859-2)
	   (input-method . "polish-slash")
	   (nonascii-translation . iso-8859-2)
	   (unibyte-display . iso-8859-2)
	   (tutorial . "TUTORIAL.pl")
	   (sample-text . "P$(D+Q(Bjd$(D+u(B, ki$(D+M(B-$(D+w(Be t$(D+8(B chmurno$(D+\++(B w g$(D)H+((Bb flaszy")
	   (documentation . t))
 '("European"))

(set-language-info-alist
 "Welsh" `((coding-system utf-8 latin-8) ; the input method is Unicode-based
	   (coding-priority utf-8 latin-8)
	   (nonascii-translation . iso-8859-14)
	   (input-method . "welsh")
	   (documentation . "Support for Welsh, using Unicode."))
 '("European"))

(set-language-info-alist
 "Latin-6" `((coding-system latin-6)
	     (coding-priority latin-6)
	     (nonascii-translation . ,(get 'decode-iso-latin-6 'translation-table))
	     (input-method . "latin-prefix")
	     (features code-pages)
	     (documentation . "Support for Latin-6."))
 '("European"))

(set-language-info-alist
 "Latin-7" `((coding-system latin-7)
	     (coding-priority latin-7)
	     (nonascii-translation . iso-8859-13)
	     (input-method . "latin-prefix")
	     (documentation . "Support for Latin-7, e.g. Latvian, Lithuanian."))
 '("European"))

(set-language-info-alist
 "Lithuanian" `((coding-system latin-7 windows-1257)
		(coding-priority latin-7)
		(nonascii-translation . iso-8859-13)
		(input-method . "lithuanian-keyboard")
		(documentation . "Support for Lithuanian."))
 '("European"))

(set-language-info-alist
 "Latvian" `((coding-system latin-7 windows-1257)
	     (coding-priority latin-7)
	     (nonascii-translation . iso-8859-13)
	     (input-method . "latvian-keyboard")
	     (documentation . "Support for Latvian."))
 '("European"))

(set-language-info-alist
 "Swedish" '((tutorial . "TUTORIAL.sv")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (unibyte-display . iso-latin-1)
	    (sample-text . "Goddag Hej")
	    (documentation . "Support for Swedish"))
 '("European"))

(set-language-info-alist
 "Croatian" '((charset iso-8859-2)
	      (coding-system iso-8859-2)
	      (coding-priority iso-8859-2)
	      (input-method . "croatian")
	      (nonascii-translation . iso-8859-2)
	      (unibyte-display . iso-8859-2)
	      (documentation . "Support for Croatian with Latin-2 encoding."))
 '("European"))

(set-language-info-alist
 "Brazilian Portuguese" '((tutorial . "TUTORIAL.pt_BR")
	    (charset iso-8859-1)
	    (coding-system iso-latin-1 iso-latin-9)
	    (coding-priority iso-latin-1)
	    (nonascii-translation . iso-8859-1)
	    (unibyte-display . iso-8859-1)
	    (input-method . "latin-1-prefix")
	    (sample-text . "Oi")
	    (documentation . "Support for Brazilian Portuguese."))
 '("European"))


(define-coding-system 'mac-roman
  "Mac Roman Encoding (MIME:MACINTOSH)."
  :coding-type 'charset
  :mnemonic ?M
  :charset-list '(mac-roman)
  :mime-charset 'macintosh)
(define-coding-system-alias 'macintosh 'mac-roman)

(define-coding-system 'next
  "NeXTstep encoding"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(next)
  :mime-charset 'next)

(define-coding-system 'hp-roman8
  "Hewlet-Packard roman-8 encoding (MIME:ROMAN-8)"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(hp-roman8)
  :mime-charset 'hp-roman8)
(define-coding-system-alias 'roman8 'hp-roman8)

(define-coding-system 'adobe-standard-encoding
  "Adobe `standard' encoding for PostScript"
  :coding-type 'charset
  :mnemonic ?*
  :charset-list '(adobe-standard-encoding)
  :mime-charset 'adobe-standard-encoding)

(provide 'european)

;;; european.el ends here
