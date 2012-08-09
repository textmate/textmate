;;; ipa.el --- Quail package for inputting IPA characters  -*-coding: utf-8;-*-

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, IPA

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

(require 'quail)
(eval-when-compile (require 'cl))

(quail-define-package
 "ipa" "IPA" "IPA" t
 "International Phonetic Alphabet for English, French, German and Italian

Upside-down characters are obtained by a preceding slash (/)."
 nil nil nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("i" ?i)
 ("I" ?ɪ)
 ("e" ?e)
 ("/3" ?ɛ)
 ("E" ?ɛ)
 ("ae" ?æ)
 ("a" ?a)
 ("i-" ?ɨ)
 ("/e" ?ə)
 ("/a" ?ɐ)
 ("/m" ?ɯ)
 ("&" ?ɤ)
 ("/v" ?ʌ)
 ("A" ?ɑ)
 ("o|" ?ɑ)
 ("y" ?y)
 ("Y" ?ʏ)
 ("o/" ?ø)
 ("oe" ?œ)
 ("OE" ?ɶ)
 ("u-" ?ʉ)
 ("o-" ?ɵ)
 ("u" ?u)
 ("U" ?ʊ)
 ("o" ?o)
 ("/c" ?ɔ)
 ("/A" ?ɒ)
 ("|o" ?ɒ)
 ("e-" ?ɚ)
 ("e|" ?ɚ)
 ("/3~" ["ɛ̃"])
 ("E~" ["ɛ̃"])
 ("A~" ["ɑ̃"])
 ("oe~" ["œ̃"])
 ("/c~" ["ɔ̃"])
 ("p" ?p)
 ("b" ?b)
 ("t" ?t)
 ("d" ?d)
 ("k" ?k)
 ("g" ?ɡ)
 ("f" ?f)
 ("v" ?v)
 ("th" ?θ)
 ("dh" ?ð)
 ("s" ?s)
 ("z" ?z)
 ("sh" ?ʃ)
 ("tsh" ["ʧ" "tʃ" "t⁀ʃ"])
 ("zh" ?ʒ)
 ("3" ?ʒ)
 ("c," ?ç)
 ("x" ?x)
 ("/R" ?ʁ)
 ("h" ?h)
 ("m" ?m)
 ("n" ?n)
 ("gn" ?ɲ)
 ("ng" ?ŋ)
 ("r" ?r)
 ("R" ?ʀ)
 ("/r" ?ɹ)
 ("j" ?j)
 ("l" ?l)
 ("/y" ?ʎ)
 ("L" ?ʟ)
 ("/h" ?ɥ)
 ("w" ?w)
 ("M" ?ʍ)
 ("'" ?ˈ)
 ("`" ?ˌ)
 (":" ?ː))

(quail-define-package
 "ipa-kirshenbaum" "IPA" "IPA-K" t
 "The International Phonetic Alphabet, using Kirshenbaum ASCII translit.

Kirshenbaum IPA is an ASCII transliteration of the phonetic alphabet, common
in the Usenet groups `sci.lang' and `alt.usage.english'.  This input method
allows you to type Kirshenbaum on your ASCII-capable keyboard, producing the
corresponding actual IPA characters in your editor.

See http://www.kirshenbaum.net/IPA/ascii-ipa.pdf for full details of the
transliteration.

A caveat with regard to that document; while XEmacs currently preserves
Unicode diacritics on reading and emitting them, it displays them,
incorrectly, as separate from the modified glyphs.")

(quail-define-rules
 ("g" "ɡ")	;; Voiced velar plosive			U+0261
 ("r" "ɹ")	;; Alveolar approximant			U+0279
 ("A" "ɑ")	;; Low back unrounded vowel		U+0251
 ("B" "β")	;; Voiced bilabial fricative		U+03B2
 ("C" "ç")	;; Voiced palatal fricative		U+00E7
 ("D" "ð")	;; Voiced dental fricative		U+00F0
 ("E" "ɛ")	;; Lower-mid front unrounded vowel	U+025B
 ("G" "ɢ")	;; Voiced uvular stop			U+0262
 ("H" "ħ")	;; Voiced pharyngeal fricative		U+0127
 ("I" "ɪ")	;; Semi-high front unrounded vowel	U+026A
 ("J" "ɟ")	;; Voiced palatal stop			U+025F
 ("L" ["ɫ"	;; Voiced velar lateral			U+026B
       "ʟ"	;; Voiced velar lateral			U+029F
       "ɬ"])	;; Voiced alveolar lateral fricative	U+026C
 ("M" "ɱ")	;; Labio-dental nasal			U+0271
 ("N" "ŋ")	;; Velar nasal				U+014B
 ("O" "ɔ")	;; Low-mid back rounded vowel		U+0254
 ("P" "ɸ")	;; Voiceless bilabial fricative		U+0278
 ("Q" "ɣ")	;; Voiced velar fricative		U+0263
 ("R" ["ʀ"	;; Alveolar trill			U+0280
       "ɚ"])    ;; Rhotacized schwa			U+025A
 ("@<r>" "ɚ")	;; Mid central rhotacized vowel		U+025A
 ("S" "ʃ")	;; Voiceless postalveolar fricative	U+0283
 ("tS" ["ʧ"	;; Voiceless postalveolar affricate	U+02A7
	"tʃ"	;;                               U+0074 U+0283
	"t⁀ʃ"]) ;;                        U+0074 U+2040 U+0283
 ("T" "θ")	;; Voiceless dental fricative		U+03B8
 ("U" "ʊ")	;; Semi-high back rounded vowel		U+028A
 ("V" "ʌ")	;; Low-mid back unrounded vowel		U+028C
 ("W" "œ")	;; Low-mid front rounded vowel		U+0153
 ("X" "χ")	;; Voiceless uvular fricative		U+03C7
 ("Y" "ø")	;; Upper-mid front rounded vowel	U+00F8
 ("Z" "ʒ")	;; Voiced postalveolar fricative	U+0292
 ("?" "ʔ")	;; Glottal stop				U+0294
 ("@" "ə")	;; Mid central unrounded vowel (schwa)	U+0259
 ("&" "æ")	;; Low front unrounded vowel		U+00E6
 ("*" "ɾ")	;; Voiced alveolar flap			U+027E

 ("a~" "ã")	;; Low central unrounded vowel, nasal	U+00E3
 ("o~" "õ")	;; Upper-mid back rounded vowel, nasal	U+00F5
 ("u~" "ũ")	;; High back rounded vowel, nasal	U+0169
 ("~" "̃")	;; +Nasalized modifier			U+0303
 (":" "ː")	;; +Long modifier			U+02D0
 ("-" "̩")	;; +Syllabic modifier			U+0329
 ("." "̣")	;; +Retroflex modifier			U+0323
 ("`" "ʼ")	;; +Ejective modifier			U+02BC
 ("[" "̪")	;; +Dental modifier			U+032A
 (";" "ʲ")	;; +Palatalized modifier		U+02B2
 ("<H>" "̴")	;; +Pharyngealized modifier		U+0334
 ("<h>" "ʰ")	;; +Aspirated modifier			U+02B0
 ("<o>" ["̥"	;; +Voiceless modifier			U+0325
	 "˚"])	;; +Unexploded modifier			U+02DA
 ("<r>" "ʳ")	;; +Rhotacized modifier			U+02B3
 ("<w>" "ʷ")	;; +Labialized modifier			U+02B7
 ("<?>" "ʱ")	;; +Murmured modifier			U+02B1

 ("b<trl>" "ʙ")	;; Bilabial trill			U+0299
 ("b`" "ɓ")	;; Bilabial implosive			U+0253
 ("p!" "ʘ")	;; Bilabial click			U+0298
 ("r<lbd>" "ʋ")	;; Labio-dental approximant		U+028B
 ("d`" "ɗ")	;; Dental implosive			U+0257
 ("t!" "ʇ")	;; Dental click				U+0287
 ("s<lat>" "ɬ")	;; Voiceless alveolar lateral fricative	U+026C
 ("z<lat>" "ɮ")	;; Voiced alveolar lateral fricative	U+026E
 ("r<trl>" "ʀ")	;; Alveolar trill			U+0280

 ("*<lat>" "ɺ")	;; Voiced alveolar lateral flap		U+027A
 ("c!" "ʗ")	;; Alveolar click			U+0297
 ("l!" "ʖ")	;; Alveolar lateral click		U+0296
 ("n." "ɳ")	;; Retroflex nasal			U+0273
 ("t." "ʈ")	;; Voiceless retroflex stop		U+0288
 ("d." "ɖ")	;; Voiced retroflex stop		U+0256
 ("s." "ʂ")	;; Voiceless retroflex fricative	U+0282
 ("z." "ʐ")	;; Voiceless retroflex fricative	U+0290
 ("r." "ɻ")	;; Retroflex approximant		U+027B
 ("l." "ɭ")	;; Retroflex lateral			U+026D
 ("*." "ɽ")	;; Retroflex flap			U+027D

 ("C<vcd>" "ʝ")	;; Voiced palatal fricative		U+029D
 ("j<rnd>" "ɥ")	;; Rounded palatal approximant		U+0265
 ("l^" "ʎ")	;; Palatal lateral			U+028E
 ("J`" "ʄ")	;; Palatal implosive			U+0284
 ("j<vel>" "ɰ")	;; Velar approximant			U+0270
 ("g`" "ɠ")	;; Velar implosive			U+0260
 ("k!" "ʞ")	;; Velar click				U+029E

 ("n<lbv>" ["n⁀g"]) ;; Labio-velar nasal
 ("t<lbv>" ["k⁀p"]) ;; Voiceless labio-velar stop

 ;; "n<lbv> for "gb" WITH U+2030 CHARACTER TIE was ambiguous and
 ;; misleading. I _believe_ this is what was meant instead.
 ("d<lbv>" ["g⁀b"]) ;; Voiced labio-velar stop.

 ("w<vls>" "ʍ")	;; Voiceless labio-velar stop		U+028D
 ("n\"" "ɴ")	;; Uvular nasal				U+0274
 ("g\"" "ʁ")	;; Voiced uvular fricative		U+0281
 ("r\"" "ʀ")	;; Uvular trill				U+0280
 ("G`" "ʛ")	;; Voiced uvular implosive		U+029B
 ("H<vcd>" "ʕ")	;; Voiced pharyngeal fricative		U+0295

 ("h<?>" "ɦ")	;; Murmured glottal fricative		U+0266
 ("I." "ʏ")	;; Semi-high front rounded vowel	U+028F
 ("&." "ɶ")	;; Low front unrounded vowel		U+0276

 ("i\"" "ɨ")	;; High central unrounded vowel		U+0268
 ("u\"" "ʉ")	;; High central rounded vowel		U+0289
 ("@<umd>" "ɘ")	;; Upper-mid central unrounded vowel	U+0258

 ("R<umd>" "ɝ")	;; Upper-mid central rhotacized vowel	U+025D

 ("@." "ɵ")	;; Mid central rounded vowel		U+0275
 ("V\"" "ɜ")	;; Lower-mid central unrounded vowel	U+025C
 ("O\"" "ɞ")	;; Lower-mid central rounded vowel	U+025E
 ("u-" "ɯ")	;; High back unrounded vowel		U+026F
 ("o-" "ɤ")	;; Upper-mid back unrounded vowel	U+0264
 ("A." "ɒ"))	;; Lower back rounded vowel		U+0252


(defconst ipa-x-sampa-implosive-submap
  '(("b_<"   ?ɓ)   ;; Voiced bilabial implosive U+0253
    ("d_<"   ?ɗ)   ;; Voiced alveolar implosive U+0257
    ("g_<"   ?ɠ)   ;; Voiced velar implosive    U+0260
    ("G\\_<" ?ʛ)   ;; Voiced uvular implosive   U+029B
    ("J\\_<" ?ʄ))  ;; Voiced palatal implosive  U+0284
  "A map from the X-SAMPA for some implosive consonants to characters.
This is used because their X-SAMPA syntax is quasi-diacritic, but the
corresponding Unicode characters themselves don't have diacritics, they are
separate code points.  So we need to implement some extra logic that isn't
normally provided by Quail.")

;; On XEmacs, with the supplied X-SAMPA data, this function is capably
;; implemented with:
;;
;;   (list (vector (concat to-prepend quail-keymap)))
;;
;; Supporting GNU Emacs too makes it a good deal more complicated.

(defun ipa-x-sampa-prepend-to-keymap-entry (to-prepend quail-keymap)
  "Return QUAIL-KEYMAP with TO-PREPEND at the beginning of each result.

QUAIL-KEYMAP is a cons that satisfies `quail-map-p'; TO-PREPEND is a
string."
  (when (consp quail-keymap) (setq quail-keymap (cdr quail-keymap)))
  (if (or (integerp quail-keymap)
	  (and (fboundp 'characterp) (characterp quail-keymap)))
      (setq quail-keymap (list (string quail-keymap)))
    (if (stringp quail-keymap)
	(setq quail-keymap (list quail-keymap))
      (assert (vectorp quail-keymap) t)
      (setq quail-keymap (append quail-keymap nil))))
  (list
   (apply 'vector
	  (mapcar
	   #'(lambda (entry)
               (assert (char-or-string-p entry) t)
               (format "%s%s" to-prepend
                       (if (integerp entry) (string entry) entry)))
	   quail-keymap))))

(defun ipa-x-sampa-underscore-implosive (input-string length)
  "Return keymap with IPA implosives, for INPUT-STRING, length LENGTH.

The implosive consonants in X-SAMPA are represented with more or less a
diacritic syntax, but the property +implosive in the IPA is expressed using
separate characters, and not using a diacritic.  This function works around
the confusion that implies when generating IPA from X-SAMPA; it returns a
Quail map that is a copy of the map for `_', but with all the DIACRITIC
entries changed to return the diacritic together with the base character,
and with the map to the implosive added to its end.

Like all `quail-defrule'-assigned functions, this will be called once for
each particular sequence of keys, the first time the user types that
particular sequence of keys, and the result will be cached by Quail."
  (let* ((input-string (substring input-string 0 (or length)))
	 (underscore-map (copy-tree (quail-lookup-key "_")))
	 (split-input (split-string input-string "_"))
	 (pre-underscore (car split-input))
	 (pre-underscore-map (quail-lookup-key pre-underscore))
	 (x-sampa-submap-entry
	  (assoc (format "%s<" input-string) ipa-x-sampa-implosive-submap))
	 underscore-map-entry)
    (if (and (consp pre-underscore-map) (car pre-underscore-map))
	(setq pre-underscore-map (car pre-underscore-map))
      (setq pre-underscore-map pre-underscore))
    (unless (stringp pre-underscore-map)
      (setq pre-underscore-map (string pre-underscore-map)))
    (dolist (underscoring underscore-map)
      (cond ((null underscoring))
	    ((eq (length underscoring) 2)
	     (setq underscore-map-entry (second underscoring))
	     (setcdr underscoring (ipa-x-sampa-prepend-to-keymap-entry
				   pre-underscore-map underscore-map-entry)))
	    ((eq (length underscoring) 3)
	     (setq underscore-map-entry (second (third underscoring)))
	     (setcdr (third underscoring)
		     (ipa-x-sampa-prepend-to-keymap-entry
		      pre-underscore-map underscore-map-entry)))
	    (t
	     (assert (null t) t
		     "Can't handle subtrees of this level right now."))))
    (append underscore-map (list (list ?< (second x-sampa-submap-entry))))))

(quail-define-package
 "ipa-x-sampa" "IPA" "IPA-X" t
 "The International Phonetic Alphabet, using J.C. Wells' X-SAMPA.

X-SAMPA is an ASCII transliteration of the IPA, normally used for data
exchange in environments where Unicode is not available.  This input method
uses this transliteration to allow you to produce the IPA in your editor
with a keyboard that's limited to ASCII.

See http://www.phon.ucl.ac.uk/home/sampa/ipasam-x.pdf for a full definition
of the mapping. A caveat with regard to that document; while XEmacs
currently preserves Unicode diacritics on reading and emitting them, it
displays them, incorrectly, as separate from the modified glyphs.")

(quail-define-rules
 ;; Table taken from http://en.wikipedia.org/wiki/X-SAMPA, checked with
 ;; http://www.phon.ucl.ac.uk/home/sampa/ipasam-x.pdf

 ("d`" "ɖ")	;; Voiced retroflex plosive		U+0256
 ("g" "ɡ")	;; Voiced velar plosive			U+0261
 ("h\\" "ɦ")	;; Voiced glottal fricative		U+0266
 ("j\\" "ʝ")	;; Voiced palatal fricative		U+029D
 ("l`" "ɭ")	;; Retroflex lateral approximant	U+026D
 ("l\\" "ɺ")	;; Alveolar lateral flap		U+027A
 ("n`" "ɳ")	;; Retroflex nasal			U+0273
 ("p\\" "ɸ")	;; Voiceless bilabial fricative		U+0278
 ("r`" "ɽ")	;; Retroflex flap			U+027D
 ("r\\" "ɹ")	;; Alveolar approximant			U+0279
 ("r\\`" "ɻ")	;; Retroflex approximant		U+027B
 ("s`" "ʂ")	;; Voiceless retroflex fricative	U+0282
 ("s\\" "ɕ")	;; Voiceless alveolo-palatal fricative	U+0255
 ("t`" "ʈ")	;; Voiceless retroflex plosive		U+0288
 ("v\\" "ʋ")	;; Labiodental approximant		U+028B
 ("x\\" "ɧ")	;; Voiceless palatal-velar fricative	U+0267
 ("z`" "ʐ")	;; Voiced retroflex fricative		U+0290
 ("z\\" "ʑ")	;; Voiced alveolo-palatal fricative	U+0291
 ("A" "ɑ")	;; Open back unrounded vowel		U+0251
 ("B" "β")	;; Voiced bilabial fricative		U+03B2
 ("B\\" "ʙ")	;; Bilabial trill			U+0299
 ("C" "ç")	;; Voiceless palatal fricative		U+00E7
 ("D" "ð")	;; Voiced dental fricative		U+00F0
 ("E" "ɛ")	;; Open-mid front unrounded vowel	U+025B
 ("F" "ɱ")	;; Labiodental nasal			U+0271
 ("G" "ɣ")	;; Voiced velar fricative		U+0263
 ("G\\" "ɢ")	;; Voiced uvular plosive		U+0262
 ("H" "ɥ")	;; Labial-palatal approximant		U+0265
 ("H\\" "ʜ")	;; Voiceless epiglottal fricative	U+029C
 ("I" "ɪ")	;; Near-close near-front unrounded vowel	U+026A
 ("I\\" "Ɨ")	;; Central lax close unrounded vowel	U+0197
 ("J" "ɲ")	;; Palatal nasal			U+0272
 ("J\\" "ɟ")	;; Voiceless palatal plosive		U+025F
 ("K" "ɬ")	;; Voiceless alveolar lateral fricative	U+026C
 ("K\\" "ɮ")	;; Voiced alveolar lateral fricative	U+026E
 ("L" "ʎ")	;; Palatal lateral approximant		U+028E
 ("L\\" "ʟ")	;; Velar lateral approximant		U+029F
 ("M" "ɯ")	;; Close back unrounded vowel		U+026F
 ("M\\" "ɰ")	;; Velar approximant			U+0270
 ("N" "ŋ")	;; Velar nasal				U+014B
 ("N\\" "ɴ")	;; Uvular nasal				U+0274
 ("O" "ɔ")	;; Open-mid back rounded vowel		U+0254
 ("O\\" "ʘ")	;; Bilabial click			U+0298
 ("P" "ʋ")	;; Labiodental approximant		U+028B
 ("Q" "ɒ")	;; Open back rounded vowel		U+0252
 ("R" "ʁ")	;; Voiced uvular fricative		U+0281
 ("R\\" "ʀ")	;; Uvular trill				U+0280
 ("S" "ʃ")	;; Voiceless postalveolar fricative	U+0283
 ("tS" ["ʧ"	;; Voiceless postalveolar affricate	U+02A7
	"tʃ"	;;                               U+0074 U+0283
	"t⁀ʃ"]) ;;                        U+0074 U+2040 U+0283
 ("T" "θ")	;; Voiceless dental fricative		U+03B8
 ("U" "ʊ")	;; Near-close near-back rounded vowel	U+028A
 ("U\\" ["ʊ̵"])	;; Central lax close rounded vowel, U+028A U+0335
 ("V" "ʌ")	;; Open-mid back unrounded vowel	U+028C
 ("W" "ʍ")	;; Voiceless labial-velar fricative	U+028D
 ("X" "χ")	;; Voiceless uvular fricative		U+03C7
 ("X\\" "ħ")	;; Voiceless pharyngeal fricative	U+0127
 ("Y" "ʏ")	;; Near-close near-front rounded vowel	U+028F
 ("Z" "ʒ")	;; Voiced postalveolar fricative	U+0292

 ("\"" "ˈ")	;; Primary stress			U+02C8
 ("%" "ˌ")	;; Secondary stress	 		U+02CC
 (":" "ː")	;; Long					U+02D0
 (":\\" "ˑ")	;; Half-long				U+02D1
 ("@" "ə")	;; Schwa				U+0259
 ("@\\" "ɘ")	;; Close-mid central unrounded vowel	U+0258
 ("@`" "ɚ")	;; Rhotacized schwa			U+025A
 ("{" "æ")	;; Near-open front unrounded vowel	U+00E6
 ("}" "ʉ")	;; Close central rounded vowel		U+0289
 ("1" "ɨ")	;; Close central unrounded vowel	U+0268
 ("2" "ø")	;; Close-mid front rounded vowel	U+00F8
 ("3" "ɜ")	;; Open-mid central unrounded vowel	U+025C
 ("3\\" "ɞ")	;; Open-mid central rounded vowel	U+025E
 ("4" "ɾ")	;; Alveolar flap			U+027E
 ("5" "ɫ")	;; Velarized alveolar lateral approximant	U+026B
 ("6" "ɐ")	;; Near-open central vowel 		U+0250
 ("7" "ɤ")	;; Close-mid back unrounded vowel	U+0264
 ("8" "ɵ")	;; Close-mid central rounded vowel	U+0275
 ("9" "œ")	;; Open-mid front rounded vowel		U+0153
 ("&" "ɶ")	;; Open front rounded vowel		U+0276
 ("?" "ʔ")	;; Glottal stop				U+0294
 ("?\\" "ʕ")	;; Voiced pharyngeal fricative		U+0295
 ;; The undefined escape character, ignored.
 ;; Indeterminacy in French vowels, ignored.
 ;; Begin nonsegmental notation, ignored.
 ("<\\" "ʢ")	;; Voiced epiglottal fricative		U+02A2
 ;; End nonsegmental notation, ignored.
 (">\\" "ʡ")	;; Epiglottal plosive			U+02A1
 ("^" "↑")	;; Upstep				U+2191
 ("!" "↓")	;; Downstep				U+2193
 ("!\\" "ǃ")	;; Postalveolal click			U+01C3
 ("\\" "ǀ")	;; Dental click				U+01C0
 ("\\|\\" "ǁ")	;; Lateral alveolar click		U+01C1
 ("=\\" "ǂ")	;; Palatal click			U+01C2
 ("-\\" "̮")	;; Linking mark				U+032E

 ;; Diacritics. Note that XEmacs doesn't yet have composed characters, so we
 ;; can input them, but they won't display properly. If you send email using
 ;; them, and the recipient's client is capable, they will get through,
 ;; though.

 ("_\"" "̈")	;; Centralized		 		U+0308
 ("_+" "̟")	;; Advanced				U+031F
 ("_-" "̠")	;; Retracted				U+0320
 ("_/" "ˇ")	;; Rising tone				U+02C7
 ("_0" "̥")	;; Voiceless				U+0325
 ("_=" "̩")	;; Syllabic				U+0329
 ("=" "̩")	;; Syllabic				U+0329
 ("_>" "ʼ")	;; Ejective				U+02BC
 ("_?\\" "ˤ")	;; Pharyngealized			U+02E4
 ("_\\" "ˆ")	;; Falling Tone				U+02C6
 ("_^" "̯")	;; Non-syllabic				U+032F
 ("_}" "̚")	;; No audible release			U+031A
 ;; ` is alternatively; retroflexion in consonants
 ("`" "˞")	;; Rhotacization in vowels		U+02DE
 ("_~" "̃")	;; Nasalization				U+0303
 ("~" "̃")	;; Nasalization				U+0303
 ("_A" "̘")	;; Advanced tongue root			U+0318
 ("_a" "̺")	;; Apical				U+033A
 ("_B" "̏")	;; Extra low tone			U+030F
 ;; _B_L omitted, no Unicode code point for "low rising tone."
 ("_c" "̜")	;; Less rounded				U+031C
 ("_d" "̪")	;; Dental				U+032A
 ("_e" "̴")	;; Velarized or pharyngeal		U+0334

 ("<F>" "↙")	;; Global fall; SOUTH EAST ARROW; may be a bit smaller than
		;; intended.
 ("_F" "̂")	;; Falling tone				U+0302
 ("_G" "ˠ")	;; Velarized				U+02E0
 ("_H" "́")	;; High tone				U+0301
 ;; "_H_T omitted, no Unicode code point for "high rising tone"
 ("_h" "ʰ")	;; Aspirated				U+02B0
 ("_j" "ʲ")	;; Palatalized				U+02B2
 ("'" "ʲ")	;; Palatalized				U+02B2
 ("_k" "̰")	;; Creaky voice				U+0330
 ("_L" "̀")	;; Low tone				U+0300
 ("_l" "ˡ")	;; Lateral release			U+02E1
 ("_M" "̄")	;; Mid tone				U+0304
 ("_m" "̻")	;; Laminal				U+033B
 ("_N" "̼")	;; Linguolabial				U+033C
 ("_n" "ⁿ")	;; Nasal release			U+207F
 ("_O" "̹")	;; More rounded				U+0339
 ("_o" "̞")	;; Lowered				U+031E
 ("_q" "̙")	;; Retracted tongue root		U+0319
 ("<R>" "↗")	;; NORTH EAST ARROW; may be a bit smaller than intended.
 ("_R" "̌")     ;; Haček, caron, rising tone.		U+030C
 ;; _R_F omitted, apparently there's no corresponding Unicode entry.
 ("_r" "̝")	;; Raised				U+031D
 ("_T" "̋")	;; Extra high tone			U+030B
 ("_t" "̤")	;; Breathy voice			U+0324
 ("_v" "̬")	;; Voiced				U+032C
 ("_w" "ʷ")	;; Labialized				U+02B7
 ("_X" "̆")	;; Extra-short				U+0306
 ("_x" "̽"))	;; Mid-centralized			U+033D

;; Putting in place rules for the implosives like for the others above
;; breaks the "_<diacritic>" rules for b, d, g, G and J a little--you need
;; to interrupt Quail before typing the underscore if you want the
;; diacritic. To avoid this, handle the input specially with the function
;; ipa-x-sampa-underscore-implosive.

(dolist (implosive-x-sampa (mapcar 'car ipa-x-sampa-implosive-submap))
  (setq implosive-x-sampa (car (split-string implosive-x-sampa "_")))
  (quail-defrule (format "%s_" implosive-x-sampa)
		 'ipa-x-sampa-underscore-implosive))

;;; ipa.el ends here
