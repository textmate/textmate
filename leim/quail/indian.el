;;; indian.el --- Quail packages for inputting Indian

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: KAWABATA, Taichi <kawabata@m17n.org>

;; Keywords: multilingual, input method, Indian, Devanagari

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

;; History:

;; 2000.12.12
;; Totally re-written from devanagari.el to handle multiple Indian Scripts.

;;; Code:

(require 'quail)
(require 'ind-util)

;;;
;;; Input by transliteration
;;;

(defun quail-define-indian-trans-package (hashtbls pkgname
						   lang title doc)
  (funcall 'quail-define-package pkgname lang title t doc
	   nil nil nil nil nil nil t nil)
  (maphash
   (lambda (key val)
     (quail-defrule key (if (= (length val) 1)
			    (string-to-char val)
			  (vector val))))
   (cdr hashtbls)))

;; This needs to be seen by quail-update-leim-list-file, but cannot be
;; commented out because quail-update-leim-list-file ignores
;; commented-out lines.
(if nil
    (quail-define-package "devanagari-itrans" "Devanagari" "DevIT" t "Devanagari ITRANS"))
(quail-define-indian-trans-package
 indian-dev-itrans-v5-hash "devanagari-itrans" "Devanagari" "DevIT"
 "Devanagari transliteration by ITRANS method.")
(quail-defrule "..." ?॥)
(quail-defrule "\\'" ?॑)
(quail-defrule "\\_" ?॒)
(quail-defrule "\\__" ?_)
(quail-defrule "\\''" ?')

(if nil
    (quail-define-package "devanagari-kyoto-harvard" "Devanagari" "DevKH" t "Devanagari Kyoto-Harvard"))
(quail-define-indian-trans-package
 indian-dev-kyoto-harvard-hash
 "devanagari-kyoto-harvard" "Devanagari" "DevKH"
 "Devanagari transliteration by Kyoto-Harvard method.")

(if nil
    (quail-define-package "devanagari-aiba" "Devanagari" "DevAB" t "Devanagari Aiba"))
(quail-define-indian-trans-package
 indian-dev-aiba-hash "devanagari-aiba" "Devanagari" "DevAB"
 "Devanagari transliteration by Aiba-method.")

(if nil
    (quail-define-package "punjabi-itrans" "Punjabi" "PnjIT" t "Punjabi ITRANS"))
(quail-define-indian-trans-package
 indian-pnj-itrans-v5-hash "punjabi-itrans" "Punjabi" "PnjIT"
 "Punjabi transliteration by ITRANS method.")

(if nil
    (quail-define-package "gujarati-itrans" "Gujarati" "GjrIT" t "Gujarati ITRANS"))
(quail-define-indian-trans-package
 indian-gjr-itrans-v5-hash "gujarati-itrans" "Gujarati" "GjrIT"
 "Gujarati transliteration by ITRANS method.")

(if nil
    (quail-define-package "oriya-itrans" "Oriya" "OriIT" t "Oriya ITRANS"))
(quail-define-indian-trans-package
 indian-ori-itrans-v5-hash "oriya-itrans" "Oriya" "OriIT"
 "Oriya transliteration by ITRANS method.")

(if nil
    (quail-define-package "bengali-itrans" "Bengali" "BngIT" t "Bengali ITRANS"))
(quail-define-indian-trans-package
 indian-bng-itrans-v5-hash "bengali-itrans" "Bengali" "BngIT"
 "Bengali transliteration by ITRANS method.")

(if nil
    (quail-define-package "assamese-itrans" "Assamese" "AsmIT" t "Assamese ITRANS"))
(quail-define-indian-trans-package
 indian-asm-itrans-v5-hash "assamese-itrans" "Assamese" "AsmIT"
 "Assamese transliteration by ITRANS method.")

(if nil
    (quail-define-package "telugu-itrans" "Telugu" "TlgIT" t "Telugu ITRANS"))
(quail-define-indian-trans-package
 indian-tlg-itrans-v5-hash "telugu-itrans" "Telugu" "TlgIT"
 "Telugu transliteration by ITRANS method.")

(if nil
    (quail-define-package "kannada-itrans" "Kannada" "KndIT" t "Kannada ITRANS"))
(quail-define-indian-trans-package
 indian-knd-itrans-v5-hash "kannada-itrans" "Kannada" "KndIT"
 "Kannada transliteration by ITRANS method.")

(if nil
    (quail-define-package "malayalam-itrans" "Malayalam" "MlmIT" t "Malayalam ITRANS"))
(quail-define-indian-trans-package
 indian-mlm-itrans-v5-hash "malayalam-itrans" "Malayalam" "MlmIT"
 "Malayalam transliteration by ITRANS method.")

(defvar quail-tamil-itrans-syllable-table
  (let ((vowels
	 '(("அ" nil "a")
	   ("ஆ" "ா" "A")
	   ("இ" "ி" "i")
	   ("ஈ" "ீ" "I")
	   ("உ" "ு" "u")
	   ("ஊ" "ூ" "U")
	   ("எ" "ெ" "e")
	   ("ஏ" "ே" "E")
	   ("ஐ" "ை" "ai")
	   ("ஒ" "ொ" "o")
	   ("ஓ" "ோ" "O")
	   ("ஔ" "ௌ" "au")))
	(consonants
	 '(("க" "k")			; U+0B95
	   ("ங" "N^")			; U+0B99
	   ("ச" "ch")			; U+0B9A
	   ("ஞ" "JN")			; U+0B9E
	   ("ட" "T")			; U+0B9F
	   ("ண" "N")			; U+0BA3
	   ("த" "t")			; U+0BA4
	   ("ந" "n")			; U+0BA8
	   ("ப" "p")			; U+0BAA
	   ("ம" "m")			; U+0BAE
	   ("ய" "y")			; U+0BAF
	   ("ர" "r")			; U+0BB0
	   ("ல" "l")			; U+0BB2
	   ("வ" "v")			; U+0BB5
	   ("ழ" "z")			; U+0BB4
	   ("ள" "L")			; U+0BB3
	   ("ற" "rh")			; U+0BB1
	   ("ன" "nh")			; U+0BA9
	   ("ஜ" "j")			; U+0B9C
	   ("ஶ" nil)			; U+0BB6
	   ("ஷ" "Sh")			; U+0BB7
	   ("ஸ" "s")			; U+0BB8
	   ("ஹ" "h")			; U+0BB9
	   ("க்ஷ" "x" )			; U+0B95
	   ))
	(virama #x0BCD)
	clm)
    (with-temp-buffer
      (insert "\n")
      (insert "    +")
      (insert-char ?- 74)
      (insert "\n    |")
      (setq clm 6)
      (dolist (v vowels)
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(car v))
	(setq clm (+ clm 6)))
      (insert "\n    |")
      (setq clm 6)
      (dolist (v vowels)
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(nth 2 v))
	(setq clm (+ clm 6)))
      (dolist (c consonants)
	(insert "\n----+")
	(insert-char ?- 74)
	(insert "\n")
	(insert (car c) virama
		(propertize "\t" 'display '(space :align-to 4))
		"|")
	(setq clm 6)
	(dolist (v vowels)
	  (insert (propertize "\t" 'display (list 'space :align-to clm))
		  (car c) (or (nth 1 v) ""))
	  (setq clm (+ clm 6)))
	(insert "\n" (or (nth 1 c) "")
		(propertize "\t" 'display '(space :align-to 4))
		"|")
	(setq clm 6)

	(dolist (v vowels)
	  (apply 'insert (propertize "\t" 'display (list 'space :align-to clm))
		 (if (nth 1 c) (list (nth 1 c) (nth 2 v)) (list "")))
	  (setq clm (+ clm 6))))
      (insert "\n")
      (insert "----+")
      (insert-char ?- 74)
      (insert "\n")
      (buffer-string))))

(defvar quail-tamil-itrans-numerics-and-symbols-table
  (let ((numerics '((?௰ "பத்து") (?௱ "நூறு") (?௲ "ஆயிரம்")))
	(symbols '((?௳ "நாள்") (?௴ "மாதம்") (?௵ "வருடம்")
		   (?௶ "பற்று") (?௷ "வரவு") (?௸ "மேற்படி")
		   (?௹ "ரூபாய்") (?௺ "எண்")))
	clm)
    (with-temp-buffer
      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (insert
       (propertize "\t" 'display '(space :align-to 5)) "numerics"
       (propertize "\t" 'display '(space :align-to 18)) "|"
       (propertize "\t" 'display '(space :align-to 45)) "symbols")
      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (dotimes (i 2)
	(setq clm 0)
	(dolist (elm numerics)
	  (if (> clm 0)
	      (insert (propertize "\t" 'display (list 'space :align-to clm))))
	  (insert (nth i elm))
	  (setq clm (+ clm 5)))
	(insert (propertize "\t" 'display '(space :align-to 18)) "|")
	(setq clm 19)
	(dolist (elm symbols)
	  (if (> clm 19)
	      (insert (propertize "\t" 'display (list 'space :align-to clm))))
	  (insert (nth i elm))
	  (setq clm (+ clm 8)))
	(insert "\n"))
      (insert (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (insert "\n")
      (buffer-string))))

(defvar quail-tamil-itrans-various-signs-and-digits-table
  (let ((various '((?ஃ . "H") ("ஸ்ரீ" . "srii") (?ௐ)))
	(digits "௦௧௨௩௪௫௬௭௮௯")
	(width 6) clm)
    (with-temp-buffer
      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (insert
       (propertize "\t" 'display '(space :align-to 5)) "various"
       (propertize "\t" 'display '(space :align-to 18)) "|"
       (propertize "\t" 'display '(space :align-to 45)) "digits")

      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (setq clm 0 )

      (dotimes (i (length various))
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(car (nth i various)))
	(setq clm (+ clm width)))
      (insert (propertize "\t" 'display '(space :align-to 18)) "|")
      (setq clm 20)
      (dotimes (i 10)
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(aref digits i))
	(setq clm (+ clm width)))
      (insert "\n")
      (setq clm 0)
      (dotimes (i (length various))
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(or (cdr (nth i various)) ""))
	(setq clm (+ clm width)))
      (insert (propertize "\t" 'display '(space :align-to 18)) "|")
      (setq clm 20)
      (dotimes (i 10)
	(insert (propertize "\t" 'display (list 'space :align-to clm))
		(format "%d" i))
	(setq clm (+ clm width)))
      (insert "\n" (make-string 18 ?-) "+" (make-string 60 ?-) "\n")
      (buffer-string))))

(if nil
    (quail-define-package "tamil-itrans" "Tamil" "TmlIT" t "Tamil ITRANS"))
(quail-define-indian-trans-package
 indian-tml-itrans-v5-hash "tamil-itrans" "Tamil" "TmlIT"
 "Tamil transliteration by ITRANS method.

You can input characters using the following mapping tables.
    Example: To enter வணக்கம், type vaNakkam.

### Basic syllables (consonants + vowels) ###
\\<quail-tamil-itrans-syllable-table>

### Miscellaneous (various signs + digits) ###
\\<quail-tamil-itrans-various-signs-and-digits-table>

### Others (numerics + symbols) ###

Characters below have no ITRANS method associated with them.
Their descriptions are included for easy reference.
\\<quail-tamil-itrans-numerics-and-symbols-table>

Full key sequences are listed below:")

;;;
;;; Input by Inscript
;;;

(defun quail-define-inscript-package (char-tables key-tables pkgname lang
                                                  title docstring)
  (funcall 'quail-define-package pkgname lang title nil docstring
	   nil nil nil nil nil nil nil nil)
  (let (char-table key-table char key)
    (while (and char-tables key-tables)
      (setq char-table  (car char-tables)
            char-tables (cdr char-tables)
            key-table   (car key-tables)
            key-tables  (cdr key-tables))
      (while (and char-table key-table)
        (setq char       (car char-table)
              char-table (cdr char-table)
              key        (car key-table)
              key-table  (cdr key-table))
        (if (and (consp char) (consp key))
            (setq char-table (append char char-table)
                  key-table  (append key  key-table))
          (if (and key char)
              (quail-defrule
               (if (characterp key) (char-to-string key) key)
               (if (stringp char)   (vector char) char))))))))

;;

(defvar inscript-dev-keytable
  '(
    (;; VOWELS  (18)
     (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
     (?+ ?=) ("F]" "f]") (?! ?@) (?Z ?z) (?S ?s) (?W ?w)
     (?| ?\\) (?~ ?`) (?A ?a) (?Q ?q) ("+]" "=]") ("R]" "r]"))
    (;; CONSONANTS (42)
     ?k ?K ?i ?I ?U                ;; GRUTTALS
     ?\; ?: ?p ?P ?}               ;; PALATALS
     ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
     ?l ?L ?o ?O ?v ?V             ;; DENTALS
     ?h ?H ?y ?Y ?c                ;; LABIALS
     ?/ ?j ?J ?n ?N "N]" ?b        ;; SEMIVOWELS
     ?M ?< ?m ?u                   ;; SIBILANTS
     "k]" "K]" "i]" "p]" "[]" "{]" "H]" "/]" ;; NUKTAS
     ?% ?&)
    (;; Misc Symbols (7)
     ?X ?x ?_ ">]" ?d "X]" ?>)
    (;; Digits
     ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
    (;; Inscripts
     ?# ?$ ?^ ?* ?\])))

(defvar inscript-mlm-keytable
  '(
    (;; VOWELS  (18)
     (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
     (?+ ?=) ("F]" "f]") (?! ?@) (?S ?s) (?Z ?z) (?W ?w)
     (?| ?\\) (?~ ?`) (?A ?a) (?Q ?q) ("+]" "=]") ("R]" "r]"))
    (;; CONSONANTS (42)
     ?k ?K ?i ?I ?U                ;; GRUTTALS
     ?\; ?: ?p ?P ?}               ;; PALATALS
     ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
     ?l ?L ?o ?O ?v ?V             ;; DENTALS
     ?h ?H ?y ?Y ?c                ;; LABIALS
     ?/ ?j ?J ?n ?N "N]" ?b        ;; SEMIVOWELS
     ?M ?< ?m ?u                   ;; SIBILANTS
     "k]" "K]" "i]" "p]" "[]" "{]" "H]" "/]" ;; NUKTAS
     ?% ?&)
    (;; Misc Symbols (7)
     ?X ?x ?_ ">]" ?d "X]" ?>)
    (;; Digits
     ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
    (;; Inscripts
     ?# ?$ ?^ ?* ?\])))

(defvar inscript-tml-keytable
  '(
    (;; VOWELS  (18)
     (?D nil) (?E ?e) (?F ?f) (?R ?r) (?G ?g) (?T ?t)
     nil nil nil (?S ?s) (?Z ?z) (?W ?w)
     nil (?A ?a) (?~ ?`) (?Q ?q) nil nil)
    (;; CONSONANTS (42)
     ?k ?K ?i ?I ?U                ;; GRUTTALS
     ?\; ?: ?p ?P ?}               ;; PALATALS
     ?' ?\" ?\[ ?{ ?C              ;; CEREBRALS
     ?l ?L ?o ?O ?v ?V             ;; DENTALS
     ?h ?H ?y ?Y ?c                ;; LABIALS
     ?/ ?j ?J ?n ?N "N]" ?b        ;; SEMIVOWELS
     ?M ?< ?m ?u                   ;; SIBILANTS
     "k]" "K]" "i]" "p]" "[]" "{]" "H]" "/]" ;; NUKTAS
     ?% ?&)
    (;; Misc Symbols (7)
     ?X ?x ?_ ">]" ?d "X]" ?>)
    (;; Digits
     ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
    (;; Inscripts
     ?# ?$ ?^ ?* ?\])))

(if nil
    (quail-define-package "devanagari-inscript" "Devanagari" "DevIS" t "Devanagari keyboard Inscript"))
(quail-define-inscript-package
 indian-dev-base-table inscript-dev-keytable
 "devanagari-inscript" "Devanagari" "DevIS"
 "Devanagari keyboard Inscript.")

(if nil
    (quail-define-package "punjabi-inscript" "Punjabi" "PnjIS" t "Punjabi keyboard Inscript"))
(quail-define-inscript-package
 indian-pnj-base-table inscript-dev-keytable
 "punjabi-inscript" "Punjabi" "PnjIS"
 "Punjabi keyboard Inscript.")

(if nil
    (quail-define-package "gujarati-inscript" "Gujarati" "GjrIS" t "Gujarati keyboard Inscript"))
(quail-define-inscript-package
 indian-gjr-base-table inscript-dev-keytable
 "gujarati-inscript" "Gujarati" "GjrIS"
 "Gujarati keyboard Inscript.")

(if nil
    (quail-define-package "oriya-inscript" "Oriya" "OriIS" t "Oriya keyboard Inscript"))
(quail-define-inscript-package
 indian-ori-base-table inscript-dev-keytable
 "oriya-inscript" "Oriya" "OriIS"
 "Oriya keyboard Inscript.")

(if nil
    (quail-define-package "bengali-inscript" "Bengali" "BngIS" t "Bengali keyboard Inscript"))
(quail-define-inscript-package
 indian-bng-base-table inscript-dev-keytable
 "bengali-inscript" "Bengali" "BngIS"
 "Bengali keyboard Inscript.")

(if nil
    (quail-define-package "assamese-inscript" "Assamese" "AsmIS" t "Assamese keyboard Inscript"))
(quail-define-inscript-package
 indian-asm-base-table inscript-dev-keytable
 "assamese-inscript" "Assamese" "AsmIS"
 "Assamese keyboard Inscript.")

(if nil
    (quail-define-package "telugu-inscript" "Telugu" "TlgIS" t "Telugu keyboard Inscript"))
(quail-define-inscript-package
 indian-tlg-base-table inscript-dev-keytable
 "telugu-inscript" "Telugu" "TlgIS"
 "Telugu keyboard Inscript.")

(if nil
    (quail-define-package "kannada-inscript" "Kannada" "KndIS" t "Kannada keyboard Inscript"))
(quail-define-inscript-package
 indian-knd-base-table inscript-dev-keytable
 "kannada-inscript" "Kannada" "KndIS"
 "Kannada keyboard Inscript.")

(if nil
    (quail-define-package "malayalam-inscript" "Malayalam" "MlmIS" t "Malayalam keyboard Inscript"))
(quail-define-inscript-package
 indian-mlm-base-table inscript-mlm-keytable
 "malayalam-inscript" "Malayalam" "MlmIS"
 "Malayalam keyboard Inscript.")

(if nil
    (quail-define-package "tamil-inscript" "Tamil" "TmlIS" t "Tamil keyboard Inscript"))
(quail-define-inscript-package
 indian-tml-base-table inscript-tml-keytable
 "tamil-inscript" "Tamil" "TmlIS"
 "Tamil keyboard Inscript.")

;;; indian.el ends here
