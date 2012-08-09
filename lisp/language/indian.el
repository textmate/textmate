;;; indian.el --- Indian languages support -*- coding: utf-8; -*-

;; Copyright (C) 1997, 1999, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Maintainer:  Kenichi Handa <handa@m17n.org>
;;		KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: 	multilingual, i18n, Indian

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

;; This file contains definitions of Indian language environments, and
;; setups for displaying the scrtipts used there.

;;; Code:

(define-coding-system 'in-is13194-devanagari
  "8-bit encoding for ASCII (MSB=0) and IS13194-Devanagari (MSB=1)."
  :coding-type 'iso-2022
  :mnemonic ?D
  :designation [ascii indian-is13194 nil nil]
  :charset-list '(ascii indian-is13194)
  :post-read-conversion 'in-is13194-post-read-conversion
  :pre-write-conversion 'in-is13194-pre-write-conversion)

(define-coding-system-alias 'devanagari 'in-is13194-devanagari)

(set-language-info-alist
 "Devanagari" '((charset unicode)
		(coding-system utf-8)
		(coding-priority utf-8)
		(input-method . "devanagari-aiba")
		(documentation . "\
Such languages using Devanagari script as Hindi and Marathi
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Bengali" '((charset unicode)
	     (coding-system utf-8)
	     (coding-priority utf-8)
	     (input-method . "bengali-itrans")
	     (documentation . "\
Such languages using Bengali script as Bengali and Assamese
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Punjabi" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "punjabi-itrans")
	      (documentation . "\
North Indian language Punjabi is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Gujarati" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "gujarati-itrans")
	      (documentation . "\
North Indian language Gujarati is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Oriya" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "oriya-itrans")
	      (documentation . "\
Such languages using Oriya script as Oriya, Khonti, and Santali
are supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Tamil" '((charset unicode)
	   (coding-system utf-8)
	   (coding-priority utf-8)
	   (input-method . "tamil-itrans")
	   (documentation . "\
South Indian Language Tamil is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Telugu" '((charset unicode)
	    (coding-system utf-8)
	    (coding-priority utf-8)
	    (input-method . "telugu-itrans")
	    (documentation . "\
South Indian Language Telugu is supported in this language environment."))
 '("Indian"))

(set-language-info-alist
 "Kannada" '((charset unicode)
	     (coding-system mule-utf-8)
	     (coding-priority mule-utf-8)
	     (input-method . "kannada-itrans")
	     (sample-text . "Kannada (ಕನ್ನಡ)	ನಮಸ್ಕಾರ")
	     (documentation . "\
Kannada language and script is supported in this language
environment.")) 
 '("Indian"))

(set-language-info-alist
 "Malayalam" '((charset unicode)
	       (coding-system utf-8)
	       (coding-priority utf-8)
	       (input-method . "malayalam-itrans")
	       (documentation . "\
South Indian language Malayalam is supported in this language environment."))
 '("Indian"))

;; Replace mnemonic characters in REGEXP according to TABLE.  TABLE is
;; an alist of (MNEMONIC-STRING . REPLACEMENT-STRING).

(defun indian-compose-regexp (regexp table)
  (let ((case-fold-search nil))
    (dolist (elt table)
      (setq regexp (replace-regexp-in-string (car elt) (cdr elt) regexp t t)))
    regexp))

(defconst devanagari-composable-pattern
  (let ((table
	 '(("a" . "[\u0900-\u0902]")	; vowel modifier (above)
	   ("A" . "\u0903")		; vowel modifier (post) 
	   ("V" . "[\u0904-\u0914\u0960-\u0961\u0972]") ; independent vowel
	   ("C" . "[\u0915-\u0939\u0958-\u095F\u0979-\u097F]") ; consonant
	   ("R" . "\u0930")		; RA
	   ("n" . "\u093C")		; NUKTA
	   ("v" . "[\u093E-\u094C\u094E\u0955\u0962-\u0963]") ; vowel sign
	   ("H" . "\u094D")		; HALANT
	   ("s" . "[\u0951-\u0952]")	; stress sign
	   ("t" . "[\u0953-\u0954]")	; accent
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0900-\u097F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HR\\)?v*n?a?s?t?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?s?t?A?\\)\\|"
      ;; special consonant form, or
      "JHR\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Devanagari characters.")

(defconst bengali-composable-pattern
  (let ((table
	 '(("a" . "\u0981")		; SIGN CANDRABINDU
	   ("A" . "[\u0982-\u0983]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0985-\u0994\u09E0-\u09E1]") ; independent vowel
	   ("C" . "[\u0995-\u09B9\u09DC-\u09DF\u09F1]") ; consonant
	   ("B" . "[\u09AC\u09AF-\u09B0\u09F0]")		; BA, YA, RA
	   ("R" . "[\u09B0\u09F0]")		; RA
	   ("n" . "\u09BC")		; NUKTA
	   ("v" . "[\u09BE-\u09CC\u09D7\u09E2-\u09E3]") ; vowel sign
	   ("H" . "\u09CD")		; HALANT
	   ("T" . "\u09CE")		; KHANDA TA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0980-\u09FF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HB\\)?v*n?a?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*[NJ]?v?a?A?\\)\\|"
      ;; another syllables with an independent vowel, or
      "\\(?:RH\\)?T\\|"
      ;; special consonant form, or
      "JHB\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Bengali characters.")

(defconst gurmukhi-composable-pattern
  (let ((table
	 '(("a" . "[\u0A01-\u0A02\u0A70]") ; SIGN ADAK BINDI .. BINDI, TIPPI
	   ("A" . "\u0A03")		; SIGN VISARGA
	   ("V" . "[\u0A05-\u0A14]")	; independent vowel
	   ("C" . "[\u0A15-\u0A39\u0A59-\u0A5E]")	; consonant
	   ("Y" . "[\u0A2F-u0A30\u0A35\u0A39]") ; YA, RA, VA, HA
	   ("n" . "\u0A3C")		; NUKTA
	   ("v" . "[\u0A3E-\u0A4C]")	; vowel sign
	   ("H" . "\u0A4D")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0A00-\u0A7F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?A?\\)\\|"
      ;; syllables with an independent vowel, or
      "Vn?\\(?:J?HY\\)?v*n?a?A?\\|"
      ;; special consonant form, or
      "JHY\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Gurmukhi characters.")

(defconst gujarati-composable-pattern
  (let ((table
	 '(("a" . "[\u0A81-\u0A82]")	; SIGN CANDRABINDU .. ANUSVARA
	   ("A" . "\u0A83")		; SIGN VISARGA
	   ("V" . "[\u0A85-\u0A94\u0AE0-\u0AE1]") ; independent vowel
	   ("C" . "[\u0A95-\u0AB9]")	; consonant
	   ("R" . "\u0AB0")		; RA
	   ("n" . "\u0ABC")		; NUKTA
	   ("v" . "[\u0ABE-\u0ACC\u0AE2-\u0AE3]") ; vowel sign
	   ("H" . "\u0ACD")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0A80-\u0AFF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HR\\)?v*n?a?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?A?\\)\\|"
      ;; special consonant form, or
      "JHR\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Gujarati characters.")

(defconst oriya-composable-pattern
  (let ((table
	 '(("a" . "\u0B01")		; SIGN CANDRABINDU
	   ("A" . "[\u0B02-\u0B03]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0B05-\u0B14\u0B60-\u0B61]") ; independent vowel
	   ("C" . "[\u0B15-\u0B39\u0B5C-\u0B5D\u0B71]")	; consonant
	   ("B" . "[\u0B15-\u0B17\u0B1B-\u0B1D\u0B1F-\u0B21\u0B23-\u0B24\u0B27-\u0B30\u0B32-\u0B35\u0B38-\u0B39]") ; consonant with below form
	   ("R" . "\u0B30")		; RA
	   ("n" . "\u0B3C")		; NUKTA
	   ("v" . "[\u0B3E-\u0B4C\u0B56-\u0B57\u0B62-\u0B63]") ; vowel sign
	   ("H" . "\u0B4D")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0B00-\u0B7F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HB\\)?v*n?a?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?a?A?\\)\\|"
      ;; special consonant form, or
      "JHB\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Oriya characters.")

(defconst tamil-composable-pattern
  (let ((table
	 '(("a" . "\u0B82")		; SIGN ANUSVARA
	   ("V" . "[\u0B85-\u0B94]")	; independent vowel
	   ("C" . "[\u0B95-\u0BB9]")	; consonant
	   ("v" . "[\u0BBE-\u0BCC\u0BD7]") ; vowel sign
	   ("H" . "\u0BCD")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0B80-\u0BFF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "C\\(?:J?HJ?C\\)*\\(?:H[NJ]?\\|v*a?\\)\\|"
      ;; syllables with an independent vowel, or
      "Vv*a?\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Tamil characters.")

(defconst telugu-composable-pattern
  (let ((table
	 '(("a" . "[\u0C01-\u0C03]")	; SIGN CANDRABINDU .. VISARGA
	   ("V" . "[\u0C05-\u0C14\u0C60-\u0C61]") ; independent vowel
	   ("C" . "[\u0C15-\u0C39\u0C58-\u0C59]") ; consonant
	   ("v" . "[\u0C3E-\u0C4C\u0C55-\u0C56\u0C62-\u0C63]")	; vowel sign
	   ("H" . "\u0C4D")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0C00-\u0C7F]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "C\\(?:J?HJ?C\\)*\\(?:H[NJ]?\\|v*a?\\)\\|"
      ;; syllables with an independent vowel, or
      "V\\(?:J?HC\\)?v*a?\\|"
      ;; special consonant form, or
      "JHC\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Telugu characters.")

(defconst kannada-composable-pattern
  (let ((table
	 '(("A" . "[\u0C82-\u0C83]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0C85-\u0C94\u0CE0-\u0CE1]") ; independent vowel
	   ("C" . "[\u0C95-\u0CB9\u0CDE]")	  ; consonant
	   ("R" . "\u0CB0")		; RA
	   ("n" . "\u0CBC")		; NUKTA
	   ("v" . "[\u0CBE-\u0CCC\u0CD5-\u0CD6\u0CE2-\u0CE3]") ; vowel sign
	   ("H" . "\u0CCD")		; VIRAMA
	   ("N" . "\u200C")		; ZWNJ
	   ("J" . "\u200D")		; ZWJ
	   ("X" . "[\u0C80-\u0CFF]"))))	; all coverage
    (indian-compose-regexp
     (concat
      ;; syllables with an independent vowel, or
      "\\(?:RH\\)?Vn?\\(?:J?HC\\)?v?A?\\|"
      ;; consonant-based syllables, or
      "Cn?\\(?:J?HJ?Cn?\\)*\\(?:H[NJ]?\\|v*n?A?\\)\\|"
      ;; special consonant form, or
      "JHC\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Kannada characters.")

(defconst malayalam-composable-pattern
  (let ((table
	 '(("A" . "[\u0D02-\u0D03]")	; SIGN ANUSVARA .. VISARGA
	   ("V" . "[\u0D05-\u0D14\u0D60-\u0D61]") ; independent vowel
	   ("C" . "[\u0D15-\u0D39]")		  ; consonant 
	   ("Y" . "[\u0D2F-\u0D30\u0D32\u0D35]")  ; YA, RA, LA, VA
	   ("v" . "[\u0D3E-\u0D4C\u0D57\u0D62-\u0D63]")	; postbase matra
	   ("H" . "\u0D4D")			  ; SIGN VIRAMA
	   ("N" . "\u200C")			  ; ZWNJ
	   ("J" . "\u200D")			  ; ZWJ
	   ("X" . "[\u0D00-\u0D7F]"))))		  ; all coverage
    (indian-compose-regexp
     (concat
      ;; consonant-based syllables, or
      "C\\(?:J?HJ?C\\)*\\(?:H[NJ]?\\|v?A?\\)\\|"
      ;; syllables with an independent vowel, or
      "V\\(?:J?HY\\)?v*?A?\\|"
      ;; special consonant form, or
      "JHY\\|"
      ;; any other singleton characters
      "X")
     table))
  "Regexp matching a composable sequence of Malayalam characters.")

(let ((script-regexp-alist
       `((devanagari . ,devanagari-composable-pattern)
	 (bengali . ,bengali-composable-pattern)
	 (gurmukhi . ,gurmukhi-composable-pattern)
	 (gujarati . ,gujarati-composable-pattern)
	 (oriya . ,oriya-composable-pattern)
	 (tamil . ,tamil-composable-pattern)
	 (telugu . ,telugu-composable-pattern)
	 (kannada . ,kannada-composable-pattern)
	 (malayalam . ,malayalam-composable-pattern))))
  (map-char-table
   #'(lambda (key val)
       (let ((slot (assq val script-regexp-alist)))
	 (if slot
	     (set-char-table-range
	      composition-function-table key
	      (list (vector (cdr slot) 0 'font-shape-gstring))))))
   char-script-table))

(provide 'indian)

;;; indian.el ends here
