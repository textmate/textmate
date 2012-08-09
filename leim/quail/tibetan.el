;;; tibetan.el --- Quail package for inputting Tibetan characters -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Tibetan

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

;; Author: Toru TOMABECHI, <Toru.Tomabechi@orient.unil.ch>

;; Created: Feb. 17. 1997

;; History:
;; 1997.03.13 Support for inputting special signs and punctuation added.
;;            (Only Ext. Wylie input)

;;; Commentary:

;;; Code:

(require 'quail)
(require 'tibet-util)

;; Now quail-current-key is set to Tibetan-Roman transcription.  We
;; set quail-current-str to the corresponding Tibetan string (composed
;; if necessary).  Both Wylie and TibKey input methods use this
;; function.

(defun quail-tibetan-update-translation (control-flag)
  (if (numberp control-flag)
      ;; Non-composable-character typed.
      (setq quail-current-str
	    (buffer-substring (overlay-start quail-overlay)
			      (overlay-end quail-overlay))
	    unread-command-events
	    (string-to-list
	     (substring quail-current-key control-flag)))
    ;; Special treatment of "-d..." and "-y...".
    (if (string-match "^-[dy]" quail-current-key)
	(setq quail-current-key (substring quail-current-key 1)))
    (let ((str (tibetan-transcription-to-tibetan quail-current-key)))
      ;; If quail-current-key is for punctuation, it can't be
      ;; transcribed by tibetan-transcription-to-tibetan, thus STR
      ;; contains ASCII string now.  In that case, use the current
      ;; characters set in quail-current-str.
      (if (> (aref str 0) 255)
	  (setq quail-current-str (tibetan-compose-string str))
	(or quail-current-str
	    (setq quail-current-str quail-current-key)))))
  control-flag)

;;; Wylie transcription based input methods.

;; Special alist for `$(7"A(B'.  It must be treated as a subjoined
;; consonant if it follows a consonant.
;; * Removed by Tomabechi 2000/06/10 *
;; 'a chung must be explicitly typed as a vowel ("fa")
;; $(7"A(B is now treated as normal base consonants
;; (defconst tibetan-wylie-quote-alist '(("'" . ?$(7"A(B)))

;; Special alist to avoid default stacking.
(defconst tibetan-wylie-non-stacking-alist
  '(("-d" . "$(7"2(B")
    ("-y" . "$(7"B(B")))

;; Punctuation characters are not transcribed.

(defconst tibetan-wylie-punctuation-alist
   '(("."  . " ")
     (":"  . "$(7"`(B")
     (" "  . "$(7!;(B")
     ("/"  . "$(7!=(B")
     ("//" . "$(7!>(B")
     ("////" . ["$(7!>(B $(7!>(B"])
     ("$"  . "$(7!?(B")
     ("/\"" . "$(7!@(B")			; Not defined in Ext. Wylie.
     ("&"  . "$(7!@(B")
     (";"  . "$(7!A(B")
     ("%"  . "$(7!D(B")
     ("!"  . "$(7!8(B")
     ("<"  . "$(7!l(B")
     (">"  . "$(7!m(B")
     ("@"  . "$(7"f(B")
     ("*"  . ["$(7!4!5(B"])
     ("#"  . ["$(7!4!5!5(B"])
     ("^"  . "$(7!6(B")
     ("0" . "$(7!P(B")
     ("1" . "$(7!Q(B")
     ("2" . "$(7!R(B")
     ("3" . "$(7!S(B")
     ("4" . "$(7!T(B")
     ("5" . "$(7!U(B")
     ("6" . "$(7!V(B")
     ("7" . "$(7!W(B")
     ("8" . "$(7!X(B")
     ("9" . "$(7!Y(B")
     ("-0" . "$(7!c(B")
     ("-1" . "$(7!Z(B")
     ("-2" . "$(7![(B")
     ("-3" . "$(7!\(B")
     ("-4" . "$(7!](B")
     ("-5" . "$(7!^(B")
     ("-6" . "$(7!_(B")
     ("-7" . "$(7!`(B")
     ("-8" . "$(7!a(B")
     ("-9" . "$(7!b(B")
     ("|"  . "$(7!0!1!2!3!7!9!:!B!C!E!F!G!H!I!J!K!L!M!N!O!d!f!h!j!k!n!o#O#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#`(B")))

(quail-define-package "tibetan-wylie" "Tibetan" "TIBw" t
"Tibetan character input by Extended Wylie key assignment.

    +-------------------------------------+
    |$(7"!!;(B k |$(7""!;(B kh |$(7"#!;(B g  |$(7"$!;(B gh |$(7"%!;(B ng|   $(7"S(B i          $(7!=(B        /
    |$(7"&!;(B c |$(7"'!;(B ch |$(7"(!;(B j  |       |$(7"*!;(B ny|   $(7"U(B u          $(7!>(B       //
    |$(7"+!;(B T |$(7",!;(B TH |$(7"-!;(B D  |$(7".!;(B DH |$(7"/!;(B N |   $(7"[(B e          $(7!>(B $(7!>(B    ////
    |$(7"0!;(B t |$(7"1!;(B th |$(7"2!;(B d  |$(7"3!;(B dh |$(7"4!;(B n |   $(7"](B o          $(7!A(B       ;
    |$(7"5!;(B p |$(7"6!;(B ph |$(7"7!;(B b  |$(7"8!;(B bh |$(7"9!;(B m |   $(7"\(B ai (ee, E) $(7!?(B        $
    |$(7":!;(B ts|$(7";!;(B tsh|$(7"<!;(B dz |$(7"=!;(B dzh|$(7">!;(B w |   $(7"^(B au (oo, O) $(7!@(B        &
    |$(7"?!;(B zh|$(7"@!;(B z  |$(7"A!;(B '  |       |$(7"B!;(B y |   $(7"a(B I          $(7!4!5(B   *
    |$(7"C!;(B r |$(7"D!;(B l  |$(7"E!;(B sh |$(7"F!;(B SH |$(7"G!;(B s |   $(7"`(B :         $(7!4!5!5(B  #
    |$(7"H!;(B h |$(7"I!;(B A  |$(7"J!;(B kSH|       |      |   $(7"_(B M           $(7!l(B $(7!m(B   < >
    +-------------------------------------+   $(7!D(B  %
    (The consonant $(7"I!;(B must be typed explicitly.)

  NOT SPECIFIED IN EXT. WYLIE:
    +--------------------------------------------------------+
    |$(7"c(B = ~ |$(7"d(B = ` |$(7"e(B = , |$(7"f(B = @ |$(7!g(B = _o|$(7!e(B = _O|$(7!6(B = ^|
    +--------------------------------------------------------+
    |$(7"i(B = x |$(7"j(B = X |$(7"g(B = v |$(7"h(B = V |$(7"k(B = q |$(7"l(B = Q |
    +-----------------------------------------------+

  SPECIAL KEYS
  +     :  Consonant Stacking
          \(Consonant stacking for ordinary Tibetan is done automatically)
  -     : No Consonant Stacking
          \(To suppress automatic stacking for \"g-y\",
            and to get da-drag in  -r-d, -l-d .)
  |     : Special signs.

  Tsheg is assigned to SPC. Space is assigned to period '.'.
"
 nil nil nil nil nil nil nil nil
 'quail-tibetan-update-translation)

;; Here we build up a Quail map for a Tibetan sequence the whole of
;; which can be one composition.
;;
;; A Tibetan syllable is typically structured as follows:
;;      [P] C [c+] V [M] [S [s]]
;;          ^^^^^^^^^^^^
;; where P:prefix, C:base consonant, c:subjoined consonant,
;; V:vowel, M:vowel modifier, S:suffix, s:post suffix.
;; In this pattern, the part indicated by "^^^" can be one composition.

;;; modified by Tomabechi 1999/12/10
;;; modified by Tomabechi 2000/06/08
;;;             Allows infinite addition of vowels/modifiers
;;;             as specified in Unicode v.3
(quail-install-map
 (quail-map-from-table
  '((base-state (tibetan-consonant-transcription-alist . svm-state)
		(tibetan-precomposed-transcription-alist . svm-state)
		(tibetan-wylie-non-stacking-alist . svm-state)
		tibetan-subjoined-transcription-alist
		tibetan-vowel-transcription-alist
		tibetan-modifier-transcription-alist
		tibetan-wylie-punctuation-alist)
    (svm-state ;;(tibetan-wylie-quote-alist . vm-state)
		(tibetan-vowel-transcription-alist . vm-state)
		(tibetan-subjoined-transcription-alist . svm-state)
		(tibetan-modifier-transcription-alist . m-state))
    (vm-state (tibetan-vowel-transcription-alist . vm-state)
	      (tibetan-modifier-transcription-alist . m-state))
    (m-state (tibetan-modifier-transcription-alist . m-state)))))

;;;
;;; TibKey key alignment based input method
;;;

(defconst tibetan-tibkey-to-transcription-alist
  '(;; consonant
    ("`" . "`")				; sna ldan
    ("~" . "~")				; sna ldan + nada
    ("q" . "k")				; ka
    ("Q" ."kSH")			; kSHa
    ("w" . "kh")			; kha
    ("e" . "g")				; ga
    ("r" . "ng")			; nga
    ("t" . "c")				; ca
    ("T" . "I")				; gi gu log
    ("y" . "ch")			; cha
    ("u" . "j")				; ja
    ("i" . "ny")			; nya
    ("o" . "t")				; ta
    ("O" . "T")				; Ta
    ("p" . "th")			; tha
    ("P" . "TH")			; THa
    ("[" . "d")				; da
    ("{" . "D")				; Da
    ("]" . "n")				; na
    ("}" . "N")				; Na
    ("a" . "p")				; pa
    ("A" . "a")				; Vowel a (not used in original TibKey)
    ("s" . "ph")			; pha
    ("d" . "b")				; ba
    ("f" . "m")				; ma
    ("F" . "M")				; anusvara
    ("g" . "u")				; zhabs kyu
    ("G" . "i")				; gi gu
    ("H" . ",")				; virama
    ("j" . "o")				; naro
    ("J" . "e")				; 'greng bu
    ("k" . "ts")			; tsa
    ("l" . "tsh")			; tsha
    (";" . "dz")                        ; dza
    ("'" . "w")				; wa
    ("\"" . "+w")			; wa zur
    ("z" . "zh")			; zha
    ("x" . "z")				; za
    ("c" . "'")				; 'a
    ("C" . "+'")			; 'a chung
    ("v" . "y")				; ya
    ("V" . "+y")			; ya btags
    ("b" . "r")				; ra
    ("B" . "+r")			; ra btags
    ("n" . "l")				; la
    ("N" . "+l")			; la btags
    ("m" . "sh")			; sha
    ("M" . "SH")			; SHa
    ("," . "s")				; sa
    ("." . "h")				; ha
    ("/" . "A")				; Aa
    ;; subjoined
    ("hq" . "+k")			; ka
    ("hQ" ."+kSH")			; kSHa
    ("hw" . "+kh")			; kha
    ("he" . "+g")			; ga
    ("hr" . "+ng")			; nga
    ("ht" . "+c")			; ca
    ("hy" . "+ch")			; cha
    ("hu" . "+j")			; ja
    ("hi" . "+ny")			; nya
    ("ho" . "+t")			; ta
    ("hO" . "+T")			; Ta
    ("hp" . "+th")			; tha
    ("hP" . "+TH")			; THa
    ("h[" . "+d")			; da
    ("h{" . "+D")			; Da
    ("h]" . "+n")			; na
    ("h}" . "+N")			; Na
    ("ha" . "+p")			; pa
    ("hs" . "+ph")			; pha
    ("hd" . "+b")			; ba
    ("hf" . "+m")			; ma
    ("hk" . "+ts")			; tsa
    ("hl" . "+tsh")			; tsha
    ("h;" . "+dz")                      ; dza
    ("h'" . "+w")			; wa
    ("hz" . "+zh")			; zha
    ("hx" . "+z")			; za
    ("hc" . "+'")			; 'a
    ("hv" . "+y")			; ya
    ("hb" . "+r")			; ra
    ("hn" . "+l")			; la
    ("hm" . "+sh")			; sha
    ("hM" . "+SH")			; SHa
    ("h," . "+s")			; sa
    ("h." . "+h")			; ha
    ("h/" . "+A")			; Aa
    ;; Special rule for `$(7"B(B' to avoid stacking.
    ("E" . "-y")
    ))

(defconst tibetan-consonant-tibkey-alist nil)
(defconst tibetan-subjoined-tibkey-alist nil)
(defconst tibetan-vowel-tibkey-alist nil)
(defconst tibetan-modifier-tibkey-alist nil)
(defconst tibetan-non-stacking-tibkey-alist nil)

(let ((type-list '("consonant" "subjoined" "vowel" "modifier" "non-stacking"))
      (tail tibetan-tibkey-to-transcription-alist)
      elt)
  (while tail
    (setq elt (car tail) tail (cdr tail))
    (let ((types type-list)
	  type transcription trans-alist tibkey-alist)
      (while types
	(setq type (car types) types (cdr types))
	(setq trans-alist
	      (if (string= type "non-stacking")
		  'tibetan-wylie-non-stacking-alist
		(intern (format "tibetan-%s-transcription-alist" type)))
	      transcription
	      (cdr (assoc (cdr elt) (symbol-value trans-alist))))
	(when transcription
	  (setq tibkey-alist (intern (format "tibetan-%s-tibkey-alist" type)))
	  (set tibkey-alist
	       (cons (cons (car elt) transcription)
		     (symbol-value tibkey-alist)))))
      (or tibkey-alist
	  (error "No Tibetan transcription for %s" (cdr elt))))))

(defconst tibetan-punctuation-tibkey-alist
  '(("1" . "$(7!Q(B")
    ("!" . "$(7!4(B")		; nyi zla long
    ("2" . "$(7!R(B")
    ("@" . "$(7!5(B")			; nyi zla simple
    ("3" . "$(7!S(B")
;;; ("#" )
    ("4" . "$(7!T(B")
;;; ("$" )
    ("5" . "$(7!U(B")
    ("%" . "$(7!D(B")
    ("6" . "$(7!V(B")
    ("^" . "$(7!1(B")
    ("7" . "$(7!W(B")
    ("8" . "$(7!X(B")
;;; ("*" ) ; avagraha, not supported yet
    ("9" . "$(7!Y(B")
    ("(" . "$(7!l(B")
    ("0" . "$(7!P(B")
    (")" . "$(7!m(B")
;;; ("-" ) ; emphatic, not yet supported
;;; ("_" ) ; id.
;;; ("=" ) ; special sign, not yet supported
    ("+" . "$(7!A(B")
    ("\\" . "$(7!?(B")
    ("|" . "$(7!8(B")
    ("I" . "$(7"f(B")				; avagraha
    (":" . "$(7"`(B")
;;; (">" ?$(7!;(B) ; to be assigned to SPC
    (">" . " ")
    ("?" . "$(7!=(B")
    ("??" . "$(7!>(B")
    ("????" . ["$(7!>(B $(7!>(B"])
    (" " . "$(7!;(B")
    ))

;; Convert TibKey string to Tibetan-Roman transcription string.
;; If there's no proper conversion, return nil.
(defun quail-tibkey-to-transcription (tibkey)
  (let ((len (length tibkey))
	(i 0)
	(trans-list nil))
    (while (< i len)
      (let ((last len)
	    trans)
	(while (and (not trans) (> last i))
	  (or (setq trans (cdr (assoc (substring tibkey i last)
				      tibetan-tibkey-to-transcription-alist)))
	      (setq last (1- last))))
	(if trans
	    (setq trans-list (cons trans trans-list)
		  i last)
	  (setq trans-list nil i len))))
    (apply 'concat (nreverse trans-list))))

(defvar quail-tibkey-characters nil)

(defun quail-tibkey-update-translation (control-flag)
  (if (integerp control-flag)
      ;; Non-composable-character typed.
      (setq quail-current-str
	    (buffer-substring (overlay-start quail-overlay)
			      (overlay-end quail-overlay))
	    unread-command-events
	    (string-to-list
	     (substring quail-current-key control-flag)))
    (let ((transcription (quail-tibkey-to-transcription quail-current-key)))
      (if (> (length transcription) 0)
	  (let ((quail-current-key transcription))
	    (setq control-flag
		  (quail-tibetan-update-translation control-flag)))
	(or quail-current-str
	    (setq quail-current-str quail-current-key)))))
  control-flag)

(quail-define-package "tibetan-tibkey" "Tibetan" "TIBt" t
"Tibetan character input by TibKey key assignment.

\(This implementation is still incomplete.
 Therefore, the following key assignment is a provisional one.)

  [NOT SHIFTED]

  +-------------------------------------------------------+
  |`$(7"d(B|1$(7!Q(B|2$(7!R(B|3$(7!S(B|4$(7!T(B|5$(7!U(B|6$(7!V(B|7$(7!W(B|8$(7!X(B|9$(7!Y(B|0$(7!P(B|-  |=  |\\$(7!8(B|
  +-------------------------------------------------------+
     |q$(7"!(B|w$(7""(B|e$(7"#(B|r$(7"%(B|t$(7"&(B|y$(7"'(B|u$(7"((B|i$(7"*(B|o$(7"0(B|p$(7"1(B|[$(7"2(B|]$(7"4(B|
     +-----------------------------------------------+
      |a$(7"5(B| s$(7"6(B| d$(7"7(B|f$(7"9(B|g$(7"U(B|h  |j$(7"](B|k$(7":(B|l$(7";(B|;$(7"<(B|'$(7">(B|
      +---------------------------------------------+
         |z$(7"?(B|x$(7"@(B|c$(7"A(B|v$(7"B(B|b$(7"C(B|n$(7"D(B|m$(7"E(B|,$(7"G(B|.$(7"H(B|/$(7"I(B|
         +---------------------------------------+
  The key 'h' is used for consonant stacking.

  [SHIFTED]

  +----------------------------------------------------------+
  |~$(7"c(B|!$(7!4(B|@$(7!5(B|#  |$  |%$(7!D(B |^$(7!1(B|&  |*  |($(7!l(B|)$(7!m(B|_  |+$(7!A(B| |$(7!8(B|
  +----------------------------------------------------------+
     |Q$(7"J(B|W  |E  |R  |T$(7"a(B|Y  |U  |I$(7"f(B|O$(7"+(B|P$(7",(B|{$(7"-(B|}$(7"/(B|
     +-----------------------------------------------+
      |A  |S  |D  |F$(7"_(B|G$(7"S(B|H$(7"e(B|J$(7"[(B|K  |L  |:$(7"`(B|\"$(7#>(B|
      +-------------------------------------------+
         |Z  |X  |C$(7"R(B|V$(7#B(B|B$(7#C(B|N$(7#D(B|M$(7"F(B|<  |>  |?$(7!=(B |
         +---------------------------------------+

  DIFFERENCE FROM THE ORIGINAL TIBKEY:

    1. Vowel 'a' should be typed explicitly by the key 'A'.
       This is really inconvenient. But to make the coding
       scheme clear, it is desirable to have an explicit
       vowel sign for 'a'.
    2. Tsheg is assigned to SPC key. You can input a space
       by typing '>'.
    4. To avoid the default stacking $(7$B(B and to obtain $(7"#"B(B,
       type 'E' instead of 'v' (=$(7"B(B).
    3. There are many characters that are not supported in the
       current implementation (especially special signs). I hope
       I'll complete in a future revision.
"
 nil nil nil nil nil nil nil nil
 'quail-tibkey-update-translation)

(quail-install-map
 (quail-map-from-table
  '((base-state (tibetan-consonant-tibkey-alist . s-state)
		(tibetan-non-stacking-tibkey-alist . s-state)
		tibetan-subjoined-tibkey-alist
		tibetan-vowel-tibkey-alist
		tibetan-modifier-tibkey-alist
		tibetan-punctuation-tibkey-alist)
    (s-state (tibetan-subjoined-tibkey-alist . s-state)
	     (tibetan-vowel-tibkey-alist . m-state))
    (m-state tibetan-modifier-tibkey-alist))))

;;; tibetan.el ends here
