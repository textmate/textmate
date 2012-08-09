;;; ipa-praat.el --- Inputting IPA characters with the conventions of Praat

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Oliver Scholz <epameinondas@gmx.de>
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

;; This is a new input method for IPA characters and diacritics, which follows
;; the conventions of Praat, a GPLed program for phonetical analysis.
;; 
;; This input method is much more complete than the current ipa.el.

;;; Code:

(require 'quail)

(quail-define-package
 "ipa-praat" "IPA" "IPAP" t
 "International Phonetic Alphabet input method.
This follows the input method of the phonetical analysis program
Praat (http://www.fon.hum.uva.nl/praat/).


* Vowels

- Unrounded
             | front | centr. | back
-------------+-------+--------+------
close        | i i   | ɨ \\i-  | ɯ \\mt
close centr. | ɪ \\ic |        |
close-mid    | e e   | ɘ \\e-  | ɤ \\rh
             |       | ə \\sw  |
open-mid     | ɛ \\ef | ɜ \\er  | ʌ \\vt
             | æ \\ae | ɐ \\at  |
open         | a a   |        | ɑ \\as



- Rounded
             | front | centr. | back
-------------+-------+--------+-------
close        | y y   | ʉ \\u-  | u u
close centr. | ʏ \\yc |        | ʊ \\hs
close-mid    | ø \\o/ | ɵ \\o-  | o o
open-mid     | œ \\oe | ɞ \\kb  | ɔ \\ct
open         | ɶ \\Oe |        | ɒ  \\ab



For most of the codes, the first letter tells you the most
similar letter of the English alphabet. The second letter can be
t (turned), c (capital), s (script), r (reversed), - (barred or
retracted), or / (slashed). One symbol (ɛ) is a phonetic version
of a Greek letter. The codes for ə, ɤ, ʊ and ɞ are abbreviations
for schwa, ram's horn, horseshoe, and kidney bean.


* Consonants

- Pulmonic

           | plos. | nasal | fric. | approx. | trill | tap/flap | l. appr.
-----------+-------+-------+-------+---------+-------+----------+---------
bilabial   | p p   | m m   | ɸ \\ff |         |       |          |
           | b b   |       | β \\bf | ʋ \\vs   | ʙ \\bc |          |
labiodent. |       | ɱ \\mj | f f   |         |       |          |
           |       |       | v v   |         |       |          |
dental     |       |       | θ \\tf |         |       |          |
           |       |       | ð \\dh |         |       |          |
alveolar   | t t   | n n   | s s   |         |       | ɾ \\fh    |
           | d d   |       | z z   | ɹ \\rt   | r r   |          | l l
alv. lat.  |       |       | ɬ \\l- |         |       | ɺ \\rl    |
           |       |       | ɮ \\lz | l l     |       |          | l l
postalv.   |       |       | ʃ \\sh |         |       |          |
           |       |       | ʒ \\zh |         |       |          |
retroflex  | ʈ \\t. | ɳ \\n. | ʂ \\s. |         |       | ɽ \\f.    |
           | ɖ \\d. |       | ʐ \\z. | ɻ \\r.   |       |          | ɭ \\l.
alv.-pala. |       |       | ɕ \\cc |         |       |          |
           |       |       | ʑ \\zc |         |       |          |
palatal    | c c   | ɲ \\nj | ç \\c, |         |       |          |
           | ɟ \\j. |       | ʝ \\jc | j j     |       |          | ʎ \\yt
lab-pal.   |       |       |       |         |       |          |
           |       |       |       | ɥ \\ht   |       |          |
lab.-vela. |       |       | ʍ \\wt |         |       |          |
           |       |       |       | w w     |       |          |
velar      | k k   | ŋ \\ng | x x   |         |       |          | ʟ \\lc
           | ɡ \\gs |       | ɣ \\gf | ɰ \\ml   |       |          |
uvular     | q q   | ɴ \\nc | χ \\cf |         |       |          |
           | ɢ \\gc |       | ʁ \\ri |         | ʀ \\rc |          |
pharyngeal |       |       | ħ \\h- |         |       |          |
           |       |       | ʕ \\9e |         |       |          |
epiglottal | ʡ \\?- |       | ʜ \\hc |         |       |          |
           |       |       | ʢ \\9- |         |       |          |
glottal    | ʔ     |       | h h   |         |       |          |
           |       |       | ɦ \\h^ |         |       |          |

- Nonpulmonic

          | implosive | click
----------+-----------+------
bilabial  | ɓ \\b^     | ʘ \\O.
dental    |           | ǀ \\|1
alveolar  | ɗ \\d^     |
alv.-lat. |           | ǁ \\|2
postalv.  |           | ǂ \\|-
retrofl.  |           | ! !
palatal   | ʄ \\j^     |
velar     | ɠ \\g^     |
uvular    | ʛ \\G^     |

For most of the codes, the first letter tells you the most
similar letter of the English alphabet. The second letter can be
t (turned), c (capital or curled), s (script), - (barred),
l (with leg), i (inverted), or j (left tail). Some phonetic
symbols are similar to Greek letters but have special
phonetic (f) versions with serifs (ɸ, β, ɣ) or are otherwise
slightly different (θ, χ). The codes for ŋ (engma), ð (eth),
ʃ (esh), and ʒ (yogh) are traditional alternative spellings. The
retroflexes have a period in the second place, because an
alternative traditional spelling is to write a dot under
them. The code for ɾ is an abbreviation for fishhook.


* Diacritics

- In line

input | example | description
------+---------+---------------------
\\:f   | ː       | phonetic length sign
\\'1   | ˈ       | primary stress
\\'2   | ˌ       | secondary stress
\\cn   | t̚       | unreleased plosive
\\rh   | ɜ˞      | rhotacized vowel

- Understrikes

input | example | description
------+---------+--------------------------------
\\|v   | n̩       | syllabic consonant
\\0v   | b̥       | voiceless
\\Tv   | o̞       | lowered
\\T^   | o̝       | raised
\\T(   | o̘       | advanced tongue root
\\T)   | o̙       | retracted tongue root
\\-v   | e̱       | backed
\\+v   | o̟       | fronted
\\:v   | o̤       | breathy voice
\\~v   | o̰       | creaky voice
\\Nv   | d̪       | dental (as opposed to alveolar)
\\Uv   | d̺       | apical
\\Dv   | d̻       | laminal
\\nv   | u̯       | nonsyllabic
\\e3v  | e̹       | slightly rounded
\\cv   | u̜       | slightly unrounded

- Overstrikes

input | example | description
------+---------+--------------------------------------------
\\0^   | ɣ̊       | voiceless
\\'^   |         | high tone
\\`^   |         | low tone
\\-^   |         | mid tone
\\~^   |         | nasalized
\\v^   |         | rising tone
\\^^   |         | falling tone
\\:^   |         | centralized
\\N^   |         | short
\\li   | k͡p      | simultaneous articulation or single segment
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ;; plosives
 ("\\t." ?ʈ) ; retroflex
 ("\\d." ?ɖ) ; voiced retroflex
 ("\\j-" ?ɟ) ; voiced palatal
 ("\\gs" ?ɡ) ; voiced velar
 ("\\gc" ?ɢ) ; voiced uvular
 ("\\?-" ?ʡ) ; epiglottal
 ("\\?g" ?ʔ) ; glottal
 
 ;; nasals
 ("\\mj" ?ɱ) ; labiodental
 ("\\n." ?ɳ) ; retroflex
 ("\\nj" ?ɲ) ; palatal
 ("\\ng" ?ŋ) ; velar
 ("\\nc" ?ɴ) ; uvular

 ;; fricatives
 ("\\ff" ?ɸ) ; bilabial
 ("\\bf" ?β) ; voiced bilabial
 ("\\tf" ?θ) ; labiodental
 ("\\dh" ?ð) ; voiced labiodental
 ("\\sh" ?ʃ) ; postalveolar
 ("\\l-" ?ɬ) ; alv. lateral
 ("\\lz" ?ɮ) ; voiced alv. lateral
 ("\\zh" ?ʒ) ; voiced postalveolar
 ("\\s." ?ʂ) ; retroflex
 ("\\z." ?ʐ) ; voiced retroflex
 ("\\cc" ?ɕ) ; alveolo-palatal
 ("\\zc" ?ʑ) ; voiced alveolo-palatal
 ("\\c," ?ç) ; palatal
 ("\\jc" ?ʝ) ; voiced palatal
 ("\\wt" ?ʍ) ; labial-velar
 ("\\gf" ?ɣ) ; voiced velar
 ("\\cf" ?χ) ; uvular
 ("\\ri" ?ʁ) ; voiced uvular
 ("\\h-" ?ħ) ; pharyngeal
 ("\\9e" ?ʕ) ; voiced pharyngeal
 ("\\hc" ?ʜ) ; epiglottal
 ("\\9-" ?ʢ) ; voiced epiglottal
 ("\\h^" ?ɦ) ; voiced glottal

 ;; approximants
 ("\\vs" ?ʋ) ; labiodental
 ("\\rt" ?ɹ) ; alveolar
 ("\\r." ?ɻ) ; retroflex
 ("\\ht" ?ɥ) ; labial-palatal
 ("\\ml" ?ɰ) ; velar
 
 ;; trills
 ("\\bc" ?ʙ) ; bilabial
 ("\\rc" ?ʀ) ; uvular

 ;; taps or flaps
 ; ⱱ -- labiodental
 ("\\fh" ?ɾ) ; alveolar
 ("\\rl" ?ɺ) ; alv.-lateral
 ("\\f." ?ɽ) ; retroflex
 
 ;; lateral approx.
 ("\\l." ?ɭ) ; retroflex
 ("\\yt" ?ʎ) ; palatal
 ("\\lc" ?ʟ) ; velar

 ;; implosives
 ("\\b^" ?ɓ) ; bilabial
 ("\\d^" ?ɗ) ; alveolar
 ("\\j^" ?ʄ) ; palatal
 ("\\g^" ?ɠ) ; velar
 ("\\G^" ?ʛ) ; uvular

 ;; clicks
 ("\\O." ?ʘ) ; bilabial
 ("\\|1" ?ǀ) ; dental
 ("\\|2" ?ǁ) ; alv. lateral
 ("\\|-" ?ǂ) ; postalveolar

 ;; other
 ("\\l~" ?ɫ) ; velarized l
 ("\\hj" ?ɧ) ; post-alveolar & velar fricative

 ;; vowels
 ("\\i-" ?ɨ)
 ("\\u-" ?ʉ)
 
 ("\\mt" ?ɯ)

 ("\\ic" ?ɪ)
 ("\\yc" ?ʏ)
 
 ("\\hs" ?ʊ)

 ("\\o/" ?ø)
 ("\\e-" ?ɘ)
 ("\\o-" ?ɵ)
 ("\\rh" ?ɤ)

 ("\\sw" ?ə)

 ("\\ef" ?ɛ)
 ("\\oe" ?œ)
 ("\\er" ?ɜ)
 ("\\kb" ?ɞ)
 ("\\vt" ?ʌ)
 ("\\ct" ?ɔ)

 ("\\ae" ?æ)
 ("\\at" ?ɐ)

 ("\\Oe" ?ɶ)
 ("\\as" ?ɑ)
 ("\\ab" ?ɒ)

 ("\\sr" ?ɚ)

 ;; diacritics
 ("\\:f" ?ː) ; phonetic length sign
 ("\\'1" ?ˈ) ; primary stress
 ("\\'2" ?ˌ) ; secondary stress
 ("\\cn" #x031A) ; t̚ unreleased plosive
 ("\\rh" #x02DE) ; ɜ˞ rhotacized vowel

 ("\\|v" #x0329) ; n̩ syllabic consonant
 ("\\0v" #x0325) ; b̥ voiceless
 ("\\Tv" #x031E) ; o̞ lowered
 ("\\T^" #x031D ) ; o̝ raised
 ("\\T(" #x0318) ;  o̘ advanced tongue root
 ("\\T)" #x0319) ; o̙ retracted tongue root
 ("\\-v" #x0331) ; e̱ backed
 ("\\+v" #x031F) ; o̟ fronted
 ("\\:v" #x0324) ; o̤ breathy voice
 ("\\~v" #x0330) ; o̰ creaky voice
 ("\\Nv" #x032A) ; d̪ dental (as opposed to alveolar)
 ("\\Uv" #x033A) ; d̺ apical
 ("\\Dv" #x033B) ; d̻ laminal
 ("\\nv" #x032F) ; u̯ nonsyllabic
 ("\\e3v" #x0339) ; e̹ slightly rounded
 ("\\cv" #x031C) ; u̜ slightly unrounded

 ("\\0^" #x030A) ; ɣ̊ voiceless
 ("\\'^" #x0301) ; high tone
 ("\\`^" #x0300) ; low tone
 ("\\-^" #x0304) ; mid tone
 ("\\~^" #x0303) ; nasalized
 ("\\v^" #x030C) ; rising tone
 ("\\^^" #x0302) ; falling tone
 ("\\:^" #x0308) ; centralized
 ("\\N^" #x0306) ; short
 ("\\li" #x0361) ; k͡p simultaneous articulation or single segment
 )

;; Local Variables:
;; coding: utf-8
;; End:

;;; ipa-praat.el ends here
