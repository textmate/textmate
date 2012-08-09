;;; latin-post.el --- Quail packages for inputting various European characters  -*-coding: utf-8;-*-

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, input method, latin, i18n

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

;; Author: TAKAHASHI Naoto <ntakahas@etl.go.jp>

;;; Commentary:

;;; Code:

(require 'quail)

(quail-define-package
 "latin-1-postfix" "Latin-1" "1<" t
 "Latin-1 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  tilde      |    ~    | a~ -> ã
  cedilla    |    ,    | c, -> ç
  nordic     |    /    | d/ -> ð   t/ -> þ   a/ -> å   e/ -> æ   o/ -> ø
  others     |    /    | s/ -> ß   ?/ -> ¿   !/ -> ¡   // -> °
             | various | << -> «   >> -> »   o_ -> º   a_ -> ª

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?À)
 ("A'" ?Á)
 ("A^" ?Â)
 ("A~" ?Ã)
 ("A\"" ?Ä)
 ("A/" ?Å)
 ("a`" ?à)
 ("a'" ?á)
 ("a^" ?â)
 ("a~" ?ã)
 ("a\"" ?ä)
 ("a/" ?å)
 ("E`" ?È)
 ("E'" ?É)
 ("E^" ?Ê)
 ("E\"" ?Ë)
 ("E/" ?Æ)
 ("e`" ?è)
 ("e'" ?é)
 ("e^" ?ê)
 ("e\"" ?ë)
 ("e/" ?æ)
 ("I`" ?Ì)
 ("i`" ?ì)
 ("I'" ?Í)
 ("i'" ?í)
 ("I^" ?Î)
 ("i^" ?î)
 ("I\"" ?Ï)
 ("i\"" ?ï)
 ("O`" ?Ò)
 ("o`" ?ò)
 ("O'" ?Ó)
 ("o'" ?ó)
 ("O^" ?Ô)
 ("o^" ?ô)
 ("O~" ?Õ)
 ("o~" ?õ)
 ("O\"" ?Ö)
 ("o\"" ?ö)
 ("O/" ?Ø)
 ("o/" ?ø)
 ("U`" ?Ù)
 ("u`" ?ù)
 ("U'" ?Ú)
 ("u'" ?ú)
 ("U^" ?Û)
 ("u^" ?û)
 ("U\"" ?Ü)
 ("u\"" ?ü)
 ("Y'" ?Ý)
 ("y'" ?ý)
 ("y\"" ?ÿ)
 ("D/" ?Ð)
 ("d/" ?ð)
 ("T/" ?Þ)
 ("t/" ?þ)
 ("s/" ?ß)
 ("C," ?Ç)
 ("c," ?ç)
 ("N~" ?Ñ)
 ("n~" ?ñ)
 ("?/" ?¿)
 ("!/" ?¡)
 ("<<" ?«)
 (">>" ?»)
 ("o_" ?º)
 ("a_" ?ª)
 ("//" ?°)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("A\"\"" ["A\""])
 ("A//" ["A/"])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("a\"\"" ["a\""])
 ("a//" ["a/"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("E//" ["E/"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("e//" ["e/"])
 ("I``" ["I`"])
 ("i``" ["i`"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("I^^" ["I^"])
 ("i^^" ["i^"])
 ("I\"\"" ["I\""])
 ("i\"\"" ["i\""])
 ("O``" ["O`"])
 ("o``" ["o`"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("O^^" ["O^"])
 ("o^^" ["o^"])
 ("O~~" ["O~"])
 ("o~~" ["o~"])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("O//" ["O/"])
 ("o//" ["o/"])
 ("U``" ["U`"])
 ("u``" ["u`"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("Y''" ["Y'"])
 ("y''" ["y'"])
 ("y\"\"" ["y\""])
 ("D//" ["D/"])
 ("d//" ["d/"])
 ("T//" ["T/"])
 ("t//" ["t/"])
 ("s//" ["s/"])
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 ("///" ["//"])
 )

(quail-define-package
 "latin-2-postfix" "Latin-2" "2<" t
 "Latin-2 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  ogonek     |    ,    | a, -> ą
  diaeresis  |    \"    | a\" -> ä
  circumflex |    ^    | a^ -> â
  breve      |    ~    | a~ -> ă
  cedilla    |    ,    | c, -> ç
  caron      |    ~    | c~ -> č
  dbl. acute |    :    | o: -> ő
  ring       |    .    | u. -> ů
  dot        |    .    | z. -> ż
  stroke     |    /    | d/ -> đ
  others     |    /    | s/ -> ß

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?Á)
 ("A," ?Ą)
 ("A\"" ?Ä)
 ("A^" ?Â)
 ("A~" ?Ă)
 ("C'" ?Ć)
 ("C," ?Ç)
 ("C~" ?Č)
 ("D/" ?Đ)
 ("D~" ?Ď)
 ("E'" ?É)
 ("E," ?Ę)
 ("E\"" ?Ë)
 ("E~" ?Ě)
 ("I'" ?Í)
 ("I^" ?Î)
 ("L'" ?Ĺ)
 ("L/" ?Ł)
 ("L~" ?Ľ)
 ("N'" ?Ń)
 ("N~" ?Ň)
 ("O'" ?Ó)
 ("O:" ?Ő)
 ("O\"" ?Ö)
 ("O^" ?Ô)
 ("R'" ?Ŕ)
 ("R~" ?Ř)
 ("S'" ?Ś)
 ("S," ?Ş)
 ("S~" ?Š)
 ("T," ?Ţ)
 ("T~" ?Ť)
 ("U'" ?Ú)
 ("U:" ?Ű)
 ("U\"" ?Ü)
 ("U." ?Ů)
 ("Y'" ?Ý)
 ("Z'" ?Ź)
 ("Z." ?Ż)
 ("Z~" ?Ž)
 ("a'" ?á)
 ("a," ?ą)
 ("a\"" ?ä)
 ("a^" ?â)
 ("a~" ?ă)
 ("c'" ?ć)
 ("c," ?ç)
 ("c~" ?č)
 ("d/" ?đ)
 ("d~" ?ď)
 ("e'" ?é)
 ("e," ?ę)
 ("e\"" ?ë)
 ("e~" ?ě)
 ("i'" ?í)
 ("i^" ?î)
 ("l'" ?ĺ)
 ("l/" ?ł)
 ("l~" ?ľ)
 ("n'" ?ń)
 ("n~" ?ň)
 ("o'" ?ó)
 ("o:" ?ő)
 ("o\"" ?ö)
 ("o^" ?ô)
 ("r'" ?ŕ)
 ("r~" ?ř)
 ("s'" ?ś)
 ("s," ?ş)
 ("s/" ?ß)
 ("s~" ?š)
 ("t," ?ţ)
 ("t~" ?ť)
 ("u'" ?ú)
 ("u:" ?ű)
 ("u\"" ?ü)
 ("u." ?ů)
 ("y'" ?ý)
 ("z'" ?ź)
 ("z." ?ż)
 ("z~" ?ž)

 ("A''" ["A'"])
 ("A,," ["A,"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C,," ["C,"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E,," ["E,"])
 ("E\"\"" ["E\""])
 ("E~~" ["E~"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("L''" ["L'"])
 ("L//" ["L/"])
 ("L~~" ["L~"])
 ("N''" ["N'"])
 ("N~~" ["N~"])
 ("O''" ["O'"])
 ("O::" ["O:"])
 ("O\"\"" ["O\""])
 ("O^^" ["O^"])
 ("R''" ["R'"])
 ("R~~" ["R~"])
 ("S''" ["S'"])
 ("S,," ["S,"])
 ("S~~" ["S~"])
 ("T,," ["T,"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U.." ["U."])
 ("Y''" ["Y'"])
 ("Z''" ["Z'"])
 ("Z.." ["Z."])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a,," ["a,"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c,," ["c,"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e,," ["e,"])
 ("e\"\"" ["e\""])
 ("e~~" ["e~"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("l''" ["l'"])
 ("l//" ["l/"])
 ("l~~" ["l~"])
 ("n''" ["n'"])
 ("n~~" ["n~"])
 ("o''" ["o'"])
 ("o::" ["o:"])
 ("o\"\"" ["o\""])
 ("o^^" ["o^"])
 ("r''" ["r'"])
 ("r~~" ["r~"])
 ("s''" ["s'"])
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t,," ["t,"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u.." ["u."])
 ("y''" ["y'"])
 ("z''" ["z'"])
 ("z.." ["z."])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-3-postfix" "Latin-3" "3<" t
 "Latin-3 character input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  dot        |    .    | c. -> ċ   i. -> ı   I. -> İ
  cedilla    |    ,    | c, -> ç
  breve      |    ~    | g~ -> ğ
  tilde      |    ~    | n~ -> ñ
  stroke     |    /    | h/ -> ħ
  others     |    /    | s/ -> ß

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?À)
 ("A'" ?Á)
 ("A^" ?Â)
 ("A\"" ?Ä)
 ("C." ?Ċ)
 ("C^" ?Ĉ)
 ("C," ?Ç)
 ("E`" ?È)
 ("E'" ?É)
 ("E^" ?Ê)
 ("E\"" ?Ë)
 ("G~" ?Ğ)
 ("G." ?Ġ)
 ("G^" ?Ĝ)
 ("H/" ?Ħ)
 ("H^" ?Ĥ)
 ("I." ?İ)
 ("I`" ?Ì)
 ("I'" ?Í)
 ("I^" ?Î)
 ("I\"" ?Ï)
 ("J^" ?Ĵ)
 ("N~" ?Ñ)
 ("O`" ?Ò)
 ("O'" ?Ó)
 ("O^" ?Ô)
 ("O\"" ?Ö)
 ("S," ?Ş)
 ("S^" ?Ŝ)
 ("U`" ?Ù)
 ("U'" ?Ú)
 ("U^" ?Û)
 ("U\"" ?Ü)
 ("U~" ?Ŭ)
 ("Z." ?Ż)
 ("a`" ?à)
 ("a'" ?á)
 ("a^" ?â)
 ("a\"" ?ä)
 ("c." ?ċ)
 ("c^" ?ĉ)
 ("c," ?ç)
 ("e`" ?è)
 ("e'" ?é)
 ("e^" ?ê)
 ("e\"" ?ë)
 ("g~" ?ğ)
 ("g." ?ġ)
 ("g^" ?ĝ)
 ("h/" ?ħ)
 ("h^" ?ĥ)
 ("i." ?ı)
 ("i`" ?ì)
 ("i'" ?í)
 ("i^" ?î)
 ("i\"" ?ï)
 ("j^" ?ĵ)
 ("n~" ?ñ)
 ("o`" ?ò)
 ("o'" ?ó)
 ("o^" ?ô)
 ("o\"" ?ö)
 ("s," ?ş)
 ("s/" ?ß)
 ("s^" ?ŝ)
 ("u`" ?ù)
 ("u'" ?ú)
 ("u^" ?û)
 ("u\"" ?ü)
 ("u~" ?ŭ)
 ("z." ?ż)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A\"\"" ["A\""])
 ("C.." ["C."])
 ("C^^" ["C^"])
 ("C,," ["C,"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("G~~" ["G~"])
 ("G.." ["G."])
 ("G^^" ["G^"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I.." ["I."])
 ("I``" ["I`"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("I\"\"" ["I\""])
 ("J^^" ["J^"])
 ("N~~" ["N~"])
 ("O``" ["O`"])
 ("O''" ["O'"])
 ("O^^" ["O^"])
 ("O\"\"" ["O\""])
 ("S,," ["S,"])
 ("S^^" ["S^"])
 ("U``" ["U`"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("Z.." ["Z."])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a\"\"" ["a\""])
 ("c.." ["c."])
 ("c^^" ["c^"])
 ("c,," ["c,"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("g~~" ["g~"])
 ("g.." ["g."])
 ("g^^" ["g^"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i.." ["i."])
 ("i``" ["i`"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("i\"\"" ["i\""])
 ("j^^" ["j^"])
 ("n~~" ["n~"])
 ("o``" ["o`"])
 ("o''" ["o'"])
 ("o^^" ["o^"])
 ("o\"\"" ["o\""])
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("u``" ["u`"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("z.." ["z."])
 )

(quail-define-package
 "latin-4-postfix" "Latin-4" "4<" t
 "Latin-4 characters input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  ogonek     |    ,    | a, -> ą
  macron     |    -    | a- -> ā
  tilde      |    ~    | a~ -> ã
  caron      |    ~    | c~ -> č
  dot        |    .    | e. -> ė
  cedilla    |    ,    | k, -> ķ   g, -> ģ
  stroke     |    /    | d/ -> đ
  nordic     |    /    | a/ -> å   e/ -> æ   o/ -> ø
  others     |    /    | s/ -> ß   n/ -> ŋ   k/ -> ĸ

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A," ?Ą)
 ("A-" ?Ā)
 ("A'" ?Á)
 ("A^" ?Â)
 ("A~" ?Ã)
 ("A\"" ?Ä)
 ("A/" ?Å)
 ("C~" ?Č)
 ("D/" ?Đ)
 ("E/" ?Æ)
 ("E-" ?Ē)
 ("E'" ?É)
 ("E," ?Ę)
 ("E\"" ?Ë)
 ("E." ?Ė)
 ("G," ?Ģ)
 ("I~" ?Ĩ)
 ("I," ?Į)
 ("I'" ?Í)
 ("I^" ?Î)
 ("I-" ?Ī)
 ("K," ?Ķ)
 ("L," ?Ļ)
 ("N/" ?Ŋ)
 ("N," ?Ņ)
 ("O-" ?Ō)
 ("O^" ?Ô)
 ("O~" ?Õ)
 ("O\"" ?Ö)
 ("O/" ?Ø)
 ("R," ?Ŗ)
 ("S~" ?Š)
 ("T/" ?Ŧ)
 ("U," ?Ų)
 ("U'" ?Ú)
 ("U^" ?Û)
 ("U\"" ?Ü)
 ("U~" ?Ũ)
 ("U-" ?Ū)
 ("Z~" ?Ž)
 ("a," ?ą)
 ("a-" ?ā)
 ("a'" ?á)
 ("a^" ?â)
 ("a~" ?ã)
 ("a\"" ?ä)
 ("a/" ?å)
 ("c~" ?č)
 ("d/" ?đ)
 ("e/" ?æ)
 ("e-" ?ē)
 ("e'" ?é)
 ("e," ?ę)
 ("e\"" ?ë)
 ("e." ?ė)
 ("g," ?ģ)
 ("i~" ?ĩ)
 ("i," ?į)
 ("i'" ?í)
 ("i^" ?î)
 ("i-" ?ī)
 ("k/" ?ĸ)
 ("k," ?ķ)
 ("l," ?ļ)
 ("n/" ?ŋ)
 ("n," ?ņ)
 ("o-" ?ō)
 ("o^" ?ô)
 ("o~" ?õ)
 ("o\"" ?ö)
 ("o/" ?ø)
 ("r," ?ŗ)
 ("s/" ?ß)
 ("s~" ?š)
 ("t/" ?ŧ)
 ("u," ?ų)
 ("u'" ?ú)
 ("u^" ?û)
 ("u\"" ?ü)
 ("u~" ?ũ)
 ("u-" ?ū)
 ("z~" ?ž)

 ("A,," ["A,"])
 ("A--" ["A-"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("A\"\"" ["A\""])
 ("A//" ["A/"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("E//" ["E/"])
 ("E--" ["E-"])
 ("E''" ["E'"])
 ("E,," ["E,"])
 ("E\"\"" ["E\""])
 ("E.." ["E."])
 ("G,," ["G,"])
 ("I~~" ["I~"])
 ("I,," ["I,"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("I--" ["I-"])
 ("K,," ["K,"])
 ("L,," ["L,"])
 ("N//" ["N/"])
 ("N,," ["N,"])
 ("O--" ["O-"])
 ("O^^" ["O^"])
 ("O~~" ["O~"])
 ("O\"\"" ["O\""])
 ("O//" ["O/"])
 ("R,," ["R,"])
 ("S~~" ["S~"])
 ("T//" ["T/"])
 ("U,," ["U,"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("U--" ["U-"])
 ("Z~~" ["Z~"])
 ("a,," ["a,"])
 ("a--" ["a-"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("a\"\"" ["a\""])
 ("a//" ["a/"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("e//" ["e/"])
 ("e--" ["e-"])
 ("e''" ["e'"])
 ("e,," ["e,"])
 ("e\"\"" ["e\""])
 ("e.." ["e."])
 ("g,," ["g,"])
 ("i~~" ["i~"])
 ("i,," ["i,"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("i--" ["i-"])
 ("k//" ["k/"])
 ("k,," ["k,"])
 ("l,," ["l,"])
 ("n//" ["n/"])
 ("n,," ["n,"])
 ("o--" ["o-"])
 ("o^^" ["o^"])
 ("o~~" ["o~"])
 ("o\"\"" ["o\""])
 ("o//" ["o/"])
 ("r,," ["r,"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t//" ["t/"])
 ("u,," ["u,"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("u--" ["u-"])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-5-postfix" "Latin-5" "5<" t
 "Latin-5 characters input method with postfix modifiers

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  tilde      |    ~    | a~ -> ã
  breve      |    ~    | g~ -> ğ
  cedilla    |    ,    | c, -> ç
  dot        |    .    | i. -> ı   I. -> İ
  nordic     |    /    | a/ -> å   e/ -> æ   o/ -> ø
  others     |    /    | s/ -> ß

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?Á)
 ("A/" ?Å)
 ("A\"" ?Ä)
 ("A^" ?Â)
 ("A`" ?À)
 ("A~" ?Ã)
 ("C," ?Ç)
 ("E'" ?É)
 ("E/" ?Æ)
 ("E\"" ?Ë)
 ("E^" ?Ê)
 ("E`" ?È)
 ("G~" ?Ğ)
 ("I'" ?Í)
 ("I." ?İ)
 ("I\"" ?Ï)
 ("I^" ?Î)
 ("I`" ?Ì)
 ("N~" ?Ñ)
 ("O'" ?Ó)
 ("O/" ?Ø)
 ("O\"" ?Ö)
 ("O^" ?Ô)
 ("O`" ?Ò)
 ("O~" ?Õ)
 ("S," ?Ş)
 ("U'" ?Ú)
 ("U\"" ?Ü)
 ("U^" ?Û)
 ("U`" ?Ù)
 ("a'" ?á)
 ("a/" ?å)
 ("a\"" ?ä)
 ("a^" ?â)
 ("a`" ?à)
 ("a~" ?ã)
 ("c," ?ç)
 ("e'" ?é)
 ("e/" ?æ)
 ("e\"" ?ë)
 ("e^" ?ê)
 ("e`" ?è)
 ("g~" ?ğ)
 ("i'" ?í)
 ("i." ?ı)
 ("i\"" ?ï)
 ("i^" ?î)
 ("i`" ?ì)
 ("n~" ?ñ)
 ("o'" ?ó)
 ("o/" ?ø)
 ("o\"" ?ö)
 ("o^" ?ô)
 ("o`" ?ò)
 ("o~" ?õ)
 ("s," ?ş)
 ("s/" ?ß)
 ("u'" ?ú)
 ("u\"" ?ü)
 ("u^" ?û)
 ("u`" ?ù)
 ("y\"" ?ÿ)

 ("A''" ["A'"])
 ("A//" ["A/"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A``" ["A`"])
 ("A~~" ["A~"])
 ("C,," ["C,"])
 ("E''" ["E'"])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("G~~" ["G~"])
 ("I''" ["I'"])
 ("I.." ["I."])
 ("I\"\"" ["I\""])
 ("I^^" ["I^"])
 ("I``" ["I`"])
 ("N~~" ["N~"])
 ("O''" ["O'"])
 ("O//" ["O/"])
 ("O\"\"" ["O\""])
 ("O^^" ["O^"])
 ("O``" ["O`"])
 ("O~~" ["O~"])
 ("S,," ["S,"])
 ("U''" ["U'"])
 ("U\"\"" ["U\""])
 ("U^^" ["U^"])
 ("U``" ["U`"])
 ("a''" ["a'"])
 ("a//" ["a/"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a``" ["a`"])
 ("a~~" ["a~"])
 ("c,," ["c,"])
 ("e''" ["e'"])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("g~~" ["g~"])
 ("i''" ["i'"])
 ("i.." ["i."])
 ("i\"\"" ["i\""])
 ("i^^" ["i^"])
 ("i``" ["i`"])
 ("n~~" ["n~"])
 ("o''" ["o'"])
 ("o//" ["o/"])
 ("o\"\"" ["o\""])
 ("o^^" ["o^"])
 ("o``" ["o`"])
 ("o~~" ["o~"])
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("u''" ["u'"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("y\"\"" ["y\""])
 )

(quail-define-package
 "danish-postfix" "Latin-1" "DA<" t
 "Danish input method (rule: AE -> Æ, OE -> Ø, AA -> Å, E' -> É)

Doubling the postfix separates the letter and postfix: e.g. aee -> ae
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?Æ)
 ("ae" ?æ)
 ("OE" ?Ø)
 ("oe" ?ø)
 ("AA" ?Å)
 ("aa" ?å)
 ("E'" ?É)
 ("e'" ?é)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "esperanto-postfix" "Latin-3" "EO<" t
 "Esperanto input method with postfix modifiers

A following ^ or x will produce an accented character,
e.g. c^ -> ĉ   gx -> ĝ   u^ -> ŭ.

Doubling the postfix separates the letter and postfix,
e.g. a'' -> a'.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("Cx" ?Ĉ)
 ("C^" ?Ĉ)
 ("cx" ?ĉ)
 ("c^" ?ĉ)
 ("Gx" ?Ĝ)
 ("G^" ?Ĝ)
 ("gx" ?ĝ)
 ("g^" ?ĝ)
 ("Hx" ?Ĥ)
 ("H^" ?Ĥ)
 ("hx" ?ĥ)
 ("h^" ?ĥ)
 ("Jx" ?Ĵ)
 ("J^" ?Ĵ)
 ("jx" ?ĵ)
 ("j^" ?ĵ)
 ("Sx" ?Ŝ)
 ("S^" ?Ŝ)
 ("sx" ?ŝ)
 ("s^" ?ŝ)
 ("Ux" ?Ŭ)
 ("U^" ?Ŭ)
 ("ux" ?ŭ)
 ("u^" ?ŭ)

 ("Cxx" ["Cx"])
 ("C^^" ["C^"])
 ("cxx" ["cx"])
 ("c^^" ["c^"])
 ("Gxx" ["Gx"])
 ("G^^" ["G^"])
 ("gxx" ["gx"])
 ("g^^" ["g^"])
 ("Hxx" ["Hx"])
 ("H^^" ["H^"])
 ("hxx" ["hx"])
 ("h^^" ["h^"])
 ("Jxx" ["Jx"])
 ("J^^" ["J^"])
 ("jxx" ["jx"])
 ("j^^" ["j^"])
 ("Sxx" ["Sx"])
 ("S^^" ["S^"])
 ("sxx" ["sx"])
 ("s^^" ["s^"])
 ("Uxx" ["Ux"])
 ("U^^" ["U^"])
 ("uxx" ["ux"])
 ("u^^" ["u^"])
 )

(quail-define-package
 "finnish-postfix" "Latin-1" "FI<" t
 "Finnish (Suomi) input method

AE  -> Ä
AEE -> AE
OE  -> Ö
OEE -> OE
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?Ä)
 ("ae" ?ä)
 ("OE" ?Ö)
 ("oe" ?ö)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 )

(quail-define-package
 "french-postfix" "French" "FR<" t
 "French (Français) input method with postfix modifiers

` pour grave, ' pour aigu, ^ pour circonflexe, et \" pour tréma.
Par exemple: a` -> à   e' -> é.

Ç, «, et » sont produits par C,, <<, et >>.

En doublant la frappe des diacritiques, ils s'isoleront de la lettre.
Par exemple: e'' -> e'

<e dans l'o> n'est pas disponible."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?À)
 ("A^" ?Â)
 ("a`" ?à)
 ("a^" ?â)
 ("E`" ?È)
 ("E'" ?É)
 ("E^" ?Ê)
 ("E\"" ?Ë)
 ("e`" ?è)
 ("e'" ?é)
 ("e^" ?ê)
 ("e\"" ?ë)
 ("I^" ?Î)
 ("I\"" ?Ï)
 ("i^" ?î)
 ("i\"" ?ï)
 ("O^" ?Ô)
 ("o^" ?ô)
 ("U`" ?Ù)
 ("U^" ?Û)
 ("U\"" ?Ü)
 ("u`" ?ù)
 ("u^" ?û)
 ("u\"" ?ü)
 ("C," ?Ç)
 ("c," ?ç)
 ("<<" ?«)
 (">>" ?»)

 ("A``" ["A`"])
 ("A^^" ["A^"])
 ("a``" ["a`"])
 ("a^^" ["a^"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("I^^" ["I^"])
 ("I\"\"" ["I\""])
 ("i^^" ["i^"])
 ("i\"\"" ["i\""])
 ("O^^" ["O^"])
 ("o^^" ["o^"])
 ("U``" ["U`"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("u``" ["u`"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 )

(quail-define-package
 "german-postfix" "German" "DE<" t
 "German (Deutsch) input method

ae  -> ä
aee -> ae
oe  -> ö
oee -> oe
ue  -> ü (not after a/e/q)
uee -> ue
sz  -> ß
szz -> sz
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?Ä)
 ("ae" ?ä)
 ("OE" ?Ö)
 ("oe" ?ö)
 ("UE" ?Ü)
 ("ue" ?ü)
 ("sz" ?ß)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("UEE" ["UE"])
 ("uee" ["ue"])
 ("szz" ["sz"])
 ("ge" ["ge"])
 ("eue" ["eue"])
 ("Eue" ["Eue"])
 ("aue" ["aue"])
 ("Aue" ["Aue"])
 ("que" ["que"])
 ("Que" ["Que"]) 
)

(quail-define-package
 "icelandic-postfix" "Latin-1" "IS<" t
 "Icelandic (Íslenska) input method with postfix modifiers

A' -> Á
E' -> É
I' -> Í
O' -> Ó
U' -> Ú
Y' -> Ý
AE -> Æ
OE -> Ö
D/ -> Ð (eth)
T/ -> Þ (thorn)

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?Á)
 ("a'" ?á)
 ("E'" ?É)
 ("e'" ?é)
 ("I'" ?Í)
 ("i'" ?í)
 ("O'" ?Ó)
 ("o'" ?ó)
 ("U'" ?Ú)
 ("u'" ?ú)
 ("Y'" ?Ý)
 ("y'" ?ý)
 ("AE" ?Æ)
 ("ae" ?æ)
 ("OE" ?Ö)
 ("oe" ?ö)
 ("D/" ?Ð)
 ("d/" ?ð)
 ("T/" ?Þ)
 ("t/" ?þ)

 ("A''" ["A'"])
 ("a''" ["a'"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("Y''" ["Y'"])
 ("y''" ["y'"])
 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("D//" ["D/"])
 ("d//" ["d/"])
 ("T//" ["T/"])
 ("t//" ["t/"])
 )

(quail-define-package
 "italian-postfix" "Latin-1" "IT<" t
 "Italian (Italiano) input method with postfix modifiers

a` -> à    A` -> À    e' -> é    << -> «
e` -> è    E` -> È    E' -> É    >> -> »
i` -> ì    I` -> Ì               o_ -> º
o` -> ò    O` -> Ò               a_ -> ª
u` -> ù    U` -> Ù

Typewriter-style italian characters.

Doubling the postfix separates the letter and postfix: e.g. a`` -> a`
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?À)
 ("a`" ?à)
 ("E`" ?È)
 ("E'" ?É)
 ("e`" ?è)
 ("e'" ?é)
 ("I`" ?Ì)
 ("i`" ?ì)
 ("O`" ?Ò)
 ("o`" ?ò)
 ("U`" ?Ù)
 ("u`" ?ù)
 ("<<" ?«)
 (">>" ?»)
 ("o_" ?º)
 ("a_" ?ª)

 ("A``" ["A`"])
 ("a``" ["a`"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("I``" ["I`"])
 ("i``" ["i`"])
 ("O``" ["O`"])
 ("o``" ["o`"])
 ("U``" ["U`"])
 ("u``" ["u`"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 )

(quail-define-package
 "norwegian-postfix" "Latin-1" "NO<" t
 "Norwegian (Norsk) input method (rule: AE->Æ   OE->Ø   AA->Å   E'->É)

Doubling the postfix separates the letter and postfix: e.g. aee -> ae
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?Æ)
 ("ae" ?æ)
 ("OE" ?Ø)
 ("oe" ?ø)
 ("AA" ?Å)
 ("aa" ?å)
 ("E'" ?É)
 ("e'" ?é)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "scandinavian-postfix" "Latin-1" "SC<" t
 "Scandinavian input method with postfix modifiers
Supported languages are Swedish, Norwegian, Danish, and Finnish.

ae -> æ
oe -> ø
aa -> å
a\" -> ä
o\" -> ö
e' -> é

Doubling the postfix separates the letter and postfix:
aee -> ae   o\"\" -> o\"   etc.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?Æ)
 ("ae" ?æ)
 ("OE" ?Ø)
 ("oe" ?ø)
 ("AA" ?Å)
 ("aa" ?å)
 ("A\"" ?Ä)
 ("a\"" ?ä)
 ("O\"" ?Ö)
 ("o\"" ?ö)
 ("E'" ?É)
 ("e'" ?é)

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("A\"\"" ["A\""])
 ("a\"\"" ["a\""])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "spanish-postfix" "Spanish" "ES<" t
 "Spanish (Español) input method with postfix modifiers

A' -> Á
E' -> É
I' -> Í
O' -> Ó
U' -> Ú
N~ -> Ñ
!/ -> ¡
?/ -> ¿

Doubling the postfix separates the letter and postfix:
a'' -> a'   n~~ -> n~, etc.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?Á)
 ("a'" ?á)
 ("E'" ?É)
 ("e'" ?é)
 ("I'" ?Í)
 ("i'" ?í)
 ("O'" ?Ó)
 ("o'" ?ó)
 ("U'" ?Ú)
 ("u'" ?ú)
 ("U\"" ?Ü)
 ("u\"" ?ü)
 ("N~" ?Ñ)
 ("n~" ?ñ)
 ("?/" ?¿)
 ("!/" ?¡)

 ("A''" ["A'"])
 ("a''" ["a'"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("U\"" ["U\""])
 ("u\"" ["U\""])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 )

(quail-define-package
 "swedish-postfix" "Latin-1" "SV<" t
 "Swedish (Svenska) input method (rule: AA -> Å   AE -> Ä   OE -> Ö   E' -> É)

Doubling the postfix separates the letter and postfix: e.g. aee -> ae
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AA" ?Å)
 ("aa" ?å)
 ("AE" ?Ä)
 ("ae" ?ä)
 ("OE" ?Ö)
 ("oe" ?ö)
 ("E'" ?É)
 ("e'" ?é)

 ("AAA" ["AA"])
 ("aaa" ["aa"])
 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("E''" ["E'"])
 ("e''" ["e'"])
 )

(quail-define-package
 "turkish-postfix" "Turkish" "TR<" t
 "Turkish (Türkçe) input method with postfix modifiers.
turkish-latin-3-postfix is an obsolete alias for turkish-postfix.

Note for I, ı, İ, i.

A^ -> Â
C, -> Ç
G^ -> Ğ
I  -> I
i  -> ı
I. -> İ
i. -> i
O\" -> Ö
S, -> Ş
U\" -> Ü
U^ -> Û

Doubling the postfix separates the letter and postfix: e.g. a^^ -> a^
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A^" ?Â)
 ("a^" ?â)
 ("C," ?Ç)
 ("c," ?ç)
 ("G^" ?Ğ)
 ("g^" ?ğ)
 ("I." ?İ)
 ("i" ?ı)
 ("i." ?i)
 ("O\"" ?Ö)
 ("o\"" ?ö)
 ("S," ?Ş)
 ("s," ?ş)
 ("U\"" ?Ü)
 ("u\"" ?ü)
 ("U^" ?Û)
 ("u^" ?û)

 ("A^^" ["A^"])
 ("a^^" ["a^"])
 ("C,," ["C,"])
 ("c,," ["c,"])
 ("G^^" ["G^"])
 ("g^^" ["g^"])
 ("I.." ["I."])
 ("i" ["i"])
 ("i.." ["i."])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("S,," ["S,"])
 ("s,," ["s,"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 )

;; Backwards compatibility.
(push (cons "turkish-latin-3-postfix"
	    (cdr (assoc "turkish-postfix" quail-package-alist)))
      quail-package-alist)

(quail-define-package
 "british" "Latin-1" "£@" t
 "British English input method with Latin-1 character £ (# -> £)"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("#" [?£ ?#])
 )

;; The following are various quail packages for those who think
;; the above are too awkward.  Supported languages and their
;; package name are:
;;
;; French	(frnch, azerty)
;; Icelandic	(iclndc)
;; Denish	(dnsh)
;; Norwegian	(nrwgn)
;; Swedish	(swdsh)
;; Finnish	(fnnsh)
;; German	(grmn)
;; Italian	(itln)
;; Spanish	(spnsh)
;; Dvorak	(dvorak)
;;
;;; 92.12.15  created for Mule Ver.0.9.6 by Takahashi N. <ntakahas@etl.go.jp>
;;; 92.12.29  modified by Takahashi N. <ntakahas@etl.go.jp>

;;
(quail-define-package
 "french-keyboard" "French" "FR@" t
 "French (Français) input method simulating some French keyboard
<e dans l'o> n'est pas disponible." nil t t t t nil nil nil nil nil t)

;; ê1  é2  è3  ô4  î5  ï6  â7  û8  ù9  à0  -_  ë+  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  çÇ  ü&
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ;:  '"  \|
;;    zZ  xX  cC  vV  bB  nN  mM  ,(  .)  !?

(quail-define-rules
 ("1" ?ê)
 ("2" ?é)
 ("3" ?è)
 ("4" ?ô)
 ("5" ?î)
 ("6" ?ï)
 ("7" ?â)
 ("8" ?û)
 ("9" ?ù)
 ("0" ?à)
 ("=" ?ë)
 ("[" ?ç)
 ("]" ?ü)

 ("!" ?1)
 ("@" ?2)
 ("#" ?3)
 ("$" ?4)
 ("%" ?5)
 ("^" ?6)
 ("&" ?7)
 ("*" ?8)
 ("(" ?9)
 (")" ?0)
 ("{" ?Ç)
 ("}" ?&)
 ("<" ?\()
 (">" ?\))
 )

;;
(quail-define-package
 "french-azerty" "French" "AZ@" t
 "French (Français) input method simulating Azerty keyboard

Similaire au clavier français de SUN.
préfixes:  ^ pour circonflexe,  ¨ pour tréma.
<e dans l'o> n'est pas disponible." nil t t t t nil nil nil nil nil t)

;; &1  é2  "3  '4  (5  §6  è7  !8  ç9  à0  )° -_  @~
;;  aA  zZ  eE  rR  tT  yY  uU  iI  oO  pP  ^¨  `$
;;   qQ  sS  dD  fF  gG  hH  jJ  kK  lL  mM  ù%  *|
;;    wW  xX  cC  vV  bB  nN  ,?  ;.  :/  =+

(quail-define-rules
 ("1" ?&)
 ("2" ?é)
 ("3" ?\")
 ("4" ?')
 ("5" ?\()
 ("6" ?§)
 ("7" ?è)
 ("8" ?!)
 ("9" ?ç)
 ("0" ?à)
 ("-" ?\))
 ("=" ?-)
 ("`" ?@)
 ("q" ?a)
 ("w" ?z)
 ("e" ?e)
 ("r" ?r)
 ("t" ?t)
 ("y" ?y)
 ("u" ?u)
 ("i" ?i)
 ("o" ?o)
 ("p" ?p)
 ("[" ?^)
 ("]" ?`)
 ("a" ?q)
 ("s" ?s)
 ("d" ?d)
 ("f" ?f)
 ("g" ?g)
 ("h" ?h)
 ("j" ?j)
 ("k" ?k)
 ("l" ?l)
 (";" ?m)
 ("'" ?ù)
 ("\\" ?*)
 ("z" ?w)
 ("x" ?x)
 ("c" ?c)
 ("v" ?v)
 ("b" ?b)
 ("n" ?n)
 ("m" ?,)
 ("," ?\;)
 ("." ?:)
 ("/" ?=)

 ("!" ?1)
 ("@" ?2)
 ("#" ?3)
 ("$" ?4)
 ("%" ?5)
 ("^" ?6)
 ("&" ?7)
 ("*" ?8)
 ("(" ?9)
 (")" ?0)
 ("_" ?°)
 ("+" ?_)
 ("~" ?~)
 ("Q" ?A)
 ("W" ?Z)
 ("E" ?E)
 ("R" ?R)
 ("T" ?T)
 ("Y" ?Y)
 ("U" ?U)
 ("I" ?I)
 ("O" ?O)
 ("P" ?P)
 ("{" ?¨)
 ("}" ?$)
 ("A" ?Q)
 ("S" ?S)
 ("D" ?D)
 ("F" ?F)
 ("G" ?G)
 ("H" ?H)
 ("J" ?J)
 ("K" ?K)
 ("L" ?L)
 (":" ?M)
 ("\"" ?%)
 ("|" ?|)
 ("Z" ?W)
 ("X" ?X)
 ("C" ?C)
 ("V" ?V)
 ("B" ?B)
 ("N" ?N)
 ("M" ??)
 ("<" ?.)
 (">" ?/)
 ("?" ?+)

 ("[q" ?â)
 ("[e" ?ê)
 ("[i" ?î)
 ("[o" ?ô)
 ("[u" ?û)

 ("{e" ?ë)
 ("{i" ?ï)
 ("{u" ?ü)

 ("[[" ?^)
 ("{{" ?¨)
 )

;;
(quail-define-package
 "icelandic-keyboard" "Latin-1" "IS@" t
 "Icelandic (Íslenska) input method simulating some Icelandic keyboard

Dead accent is right to æ." nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4$  5%  6^  7&  8*  9(  0)  öÖ  -_  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ðÐ  '?
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  æÆ  ´´  +*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  þÞ

(quail-define-rules
 ("-" ?ö)
 ("=" ?-)
 ("[" ?ð)
 ("]" ?')
 (";" ?æ)
 ("'" ?´)
 ("\\" ?+)
 ("/" ?þ)

 ("@" ?\")
 ("_" ?Ö)
 ("+" ?_)
 ("{" ?Ð)
 ("}" ??)
 (":" ?Æ)
 ("\"" ?´)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?Þ)

 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("'y" ?ý)
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'Y" ?Ý)

 ("''" ?´)
 )

;;
(quail-define-package
 "danish-keyboard" "Latin-1" "DA@" t
 "Danish input method simulating SUN Danish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4¤  5%  6&  7/  8(  9)  0=  +?  ½§  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  åÅ  éÉ
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  æÆ  øØ  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?+)
 ("=" ?½)
 ("`" ?~)
 ("[" ?å)
 ("]" ?é)
 (";" ?æ)
 ("'" ?ø)
 ("\\" ?')
 ("/" ?-)

 ("@" ?\")
 ("$" ?¤)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?§)
 ("~" ?^)
 ("{" ?Å)
 ("}" ?É)
 (":" ?Æ)
 ("\"" ?Ø)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "norwegian-keyboard" "Latin-1" "NO@" t
 "Norwegian (Norsk) input method simulating SUN Norwegian keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4¤  5%  6&  7/  8(  9)  0=  +?  |§  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  åÅ  éÉ
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  øØ  æÆ  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  '?

(quail-define-rules
 ("-" ?+)
 ("=" ?|)
 ("`" ?~)
 ("[" ?å)
 ("]" ?é)
 (";" ?ø)
 ("'" ?æ)
 ("\\" ?')
 ("/" ?-)

 ("!" ?!)
 ("@" ?\")
 ("$" ?¤)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?§)
 ("~" ?^)
 ("{" ?Å)
 ("}" ?É)
 (":" ?Ø)
 ("\"" ?Æ)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "swedish-keyboard" "Latin-1" "SV@" t
 "Swedish (Svenska) input method simulating SUN Swedish/Finnish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4¤  5%  6&  7/  8(  9)  0=  +?  §½  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  åÅ  éÉ
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  öÖ  äÄ  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?+)
 ("=" ?§)
 ("`" ?~)
 ("[" ?å)
 ("]" ?é)
 (";" ?ö)
 ("'" ?ä)
 ("\\" ?')
 ("/" ?-)

 ("@" ?\")
 ("$" ?¤)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?½)
 ("~" ?^)
 ("{" ?Å)
 ("}" ?É)
 (":" ?Ö)
 ("\"" ?Ä)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "finnish-keyboard" "Latin-1" "FI@" t
 "Finnish input method simulating SUN Finnish/Swedish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3#  4¤  5%  6&  7/  8(  9)  0=  +?  §½  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  åÅ  éÉ
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  öÖ  äÄ  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?+)
 ("=" ?§)
 ("`" ?~)
 ("[" ?å)
 ("]" ?é)
 (";" ?ö)
 ("'" ?ä)
 ("\\" ?')
 ("/" ?-)

 ("@" ?\")
 ("$" ?¤)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?½)
 ("~" ?^)
 ("{" ?Å)
 ("}" ?É)
 (":" ?Ö)
 ("\"" ?Ä)
 ("|" ?*)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "german" "German" "DE@" t
 "German (Deutsch) input method simulating SUN German keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3§  4$  5%  6&  7/  8(  9)  0=  ß?  [{  ]}
;;  qQ  wW  eE  rR  tT  zZ  uU  iI  oO  pP  üÜ  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  öÖ  äÄ  #^
;;    yY  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?ß)
 ("=" ?\[)
 ("`" ?\])
 ("y" ?z)
 ("[" ?ü)
 ("]" ?+)
 (";" ?ö)
 ("'" ?ä)
 ("\\" ?#)
 ("z" ?y)
 ("/" ?-)

 ("@" ?\")
 ("#" ?§)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("Y" ?Z)
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?{)
 ("~" ?})
 ("{" ?Ü)
 ("}" ?*)
 (":" ?Ö)
 ("\"" ?Ä)
 ("|" ?^)
 ("Z" ?Y)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "italian-keyboard" "Latin-1" "IT@" t
 "Italian (Italiano) input method simulating SUN Italian keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3£  4$  5%  6&  7/  8(  9)  0=  '?  ì^  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  èé  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  òç  à°  ù§
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?')
 ("=" ?ì)
 ("[" ?è)
 ("]" ?+)
 (";" ?ò)
 ("'" ?à)
 ("\\" ?ù)
 ("/" ?-)

 ("@" ?\")
 ("#" ?£)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?^)
 ("~" ?~)
 ("{" ?é)
 ("}" ?*)
 (":" ?ç)
 ("\"" ?°)
 ("|" ?§)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "spanish-keyboard" "Spanish" "ES@" t
 "Spanish (Español) input method simulating SUN Spanish keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3·  4$  5%  6&  7/  8(  9)  0=  '?  ¡¿  íÍ
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  éÉ  óÓ
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ñÑ  áÁ  úÚ
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?')
 ("=" ?¡)
 ("`" ?í)
 ("[" ?é)
 ("]" ?ó)
 (";" ?ñ)
 ("'" ?á)
 ("\\" ?ú)
 ("/" ?-)

 ("@" ?\")
 ("#" ?·)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?¿)
 ("~" ?Í)
 ("{" ?É)
 ("}" ?Ó)
 (":" ?Ñ)
 ("\"" ?Á)
 ("|" ?Ú)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_)
 )

;;
(quail-define-package
 "english-dvorak" "English" "DV@" t
 "English (ASCII) input method simulating Dvorak keyboard"
 nil t t t t nil nil nil nil nil t)

;; 1!  2@  3#  4$  5%  6^  7&  8*  9(  0)  [{  ]}  `~
;;  '"  ,<  .>  pP  yY  fF  gG  cC  rR  lL  /?  =+
;;   aA  oO  eE  uU  iI  dD  hH  tT  nN  sS  -_  \|
;;    ;:  qQ  jJ  kK  xX  bB  mM  wW  vV  zZ

(quail-define-rules
 ("-" ?\[)
 ("=" ?\])
 ("`" ?`)
 ("q" ?')
 ("w" ?,)
 ("e" ?.)
 ("r" ?p)
 ("t" ?y)
 ("y" ?f)
 ("u" ?g)
 ("i" ?c)
 ("o" ?r)
 ("p" ?l)
 ("[" ?/)
 ("]" ?=)
 ("a" ?a)
 ("s" ?o)
 ("d" ?e)
 ("f" ?u)
 ("g" ?i)
 ("h" ?d)
 ("j" ?h)
 ("k" ?t)
 ("l" ?n)
 (";" ?s)
 ("'" ?-)
 ("\\" ?\\)
 ("z" ?\;)
 ("x" ?q)
 ("c" ?j)
 ("v" ?k)
 ("b" ?x)
 ("n" ?b)
 ("m" ?m)
 ("," ?w)
 ("." ?v)
 ("/" ?z)

 ("_" ?{)
 ("+" ?})
 ("~" ?~)
 ("Q" ?\")
 ("W" ?<)
 ("E" ?>)
 ("R" ?P)
 ("T" ?Y)
 ("Y" ?F)
 ("U" ?G)
 ("I" ?C)
 ("O" ?R)
 ("P" ?L)
 ("{" ??)
 ("}" ?+)
 ("A" ?A)
 ("S" ?O)
 ("D" ?E)
 ("F" ?U)
 ("G" ?I)
 ("H" ?D)
 ("J" ?H)
 ("K" ?T)
 ("L" ?N)
 (":" ?S)
 ("\"" ?_)
 ("|" ?|)
 ("Z" ?:)
 ("X" ?Q)
 ("C" ?J)
 ("V" ?K)
 ("B" ?X)
 ("N" ?B)
 ("M" ?M)
 ("<" ?W)
 (">" ?V)
 ("?" ?Z)
 )

(quail-define-package
 "latin-postfix" "Latin" "L<" t
 "Latin character input method with postfix modifiers.
This is the union of various input methods originally made for input
of characters from a single Latin-N charset.

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  tilde      |    ~    | a~ -> ã
  cedilla    |    ,    | c, -> ç
  ogonek     |    ,    | a, -> ą
  breve      |    ~    | a~ -> ă
  caron      |    ~    | c~ -> č
  dbl. acute |    :    | o: -> ő
  ring       |    .    | u. -> ů
  dot        |    .    | z. -> ż
  stroke     |    /    | d/ -> đ
  nordic     |    /    | d/ -> ð   t/ -> þ   a/ -> å   e/ -> æ   o/ -> ø
  others     |    /    | s/ -> ß   ?/ -> ¿   !/ -> ¡   // -> °
             | various | << -> «   >> -> »   o_ -> º   a_ -> ª

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

;; Fixme: ¦ § ¨ © ¬ ­ ® ¯ ± ² ³ ´ µ ¶ · ¸ ¹ ¼ ½ ¾ × ÷
(quail-define-rules
 (" _" ? )
 ("!/" ?¡)
 ("//" ?°)
 ("<<" ?\«)
 (">>" ?\»)
 ("?/" ?¿)
 ("$/" ?£)
 ("$/" ?¤)
 ("A'" ?Á)
 ("A," ?Ą)
 ("A-" ?Ā)
 ("A/" ?Å)
 ("A\"" ?Ä)
 ("A^" ?Â)
 ("A`" ?À)
 ("A~" ?Ã)
 ("A~" ?Ă)
 ("C'" ?Ć)
 ("C," ?Ç)
 ("C." ?Ċ)
 ("C^" ?Ĉ)
 ("C~" ?Č)
 ("D/" ?Ð)
 ("D/" ?Đ)
 ("D~" ?Ď)
 ("E'" ?É)
 ("E," ?Ę)
 ("E-" ?Ē)
 ("E." ?Ė)
 ("E/" ?Æ)
 ("E\"" ?Ë)
 ("E^" ?Ê)
 ("E`" ?È)
 ("E~" ?Ě)
 ("G," ?Ģ)
 ("G." ?Ġ)
 ("G^" ?Ĝ)
 ("G~" ?Ğ)
 ("H/" ?Ħ)
 ("H^" ?Ĥ)
 ("I'" ?Í)
 ("I," ?Į)
 ("I-" ?Ī)
 ("I." ?İ)
 ("I\"" ?Ï)
 ("I^" ?Î)
 ("I`" ?Ì)
 ("I~" ?Ĩ)
 ("J^" ?Ĵ)
 ("K," ?Ķ)
 ("L'" ?Ĺ)
 ("L," ?Ļ)
 ("L/" ?Ł)
 ("L~" ?Ľ)
 ("N'" ?Ń)
 ("N," ?Ņ)
 ("N/" ?Ŋ)
 ("N~" ?Ñ)
 ("N~" ?Ň)
 ("O'" ?Ó)
 ("O-" ?Ō)
 ("O/" ?Ø)
 ("O/" ?Œ)
 ("O:" ?Ő)
 ("O\"" ?Ö)
 ("O^" ?Ô)
 ("O`" ?Ò)
 ("O~" ?Õ)
 ("R'" ?Ŕ)
 ("R," ?Ŗ)
 ("R~" ?Ř)
 ("S'" ?Ś)
 ("S," ?Ş)
 ("S^" ?Ŝ)
 ("S~" ?Š)
 ("T," ?Ţ)
 ("T/" ?Þ)
 ("T/" ?Ŧ)
 ("T~" ?Ť)
 ("U'" ?Ú)
 ("U," ?Ų)
 ("U-" ?Ū)
 ("U." ?Ů)
 ("U:" ?Ű)
 ("U\"" ?Ü)
 ("U^" ?Û)
 ("U`" ?Ù)
 ("U~" ?Ũ)
 ("U~" ?Ŭ)
 ("Y'" ?Ý)
 ("Y\"" ?Ÿ)
 ("Y=" ?¥)
 ("Z'" ?Ź)
 ("Z." ?Ż)
 ("Z~" ?Ž)
 ("a'" ?á)
 ("a," ?ą)
 ("a-" ?ā)
 ("a/" ?å)
 ("a\"" ?ä)
 ("a^" ?â)
 ("a_" ?ª)
 ("a`" ?à)
 ("a~" ?ã)
 ("a~" ?ă)
 ("c'" ?ć)
 ("c," ?ç)
 ("c." ?ċ)
 ("c^" ?ĉ)
 ("c~" ?č)
 ("c/" ?¢)
 ("d/" ?ð)
 ("d/" ?đ)
 ("d~" ?ď)
 ("e'" ?é)
 ("e," ?ę)
 ("e-" ?ē)
 ("e." ?ė)
 ("e/" ?æ)
 ("e\"" ?ë)
 ("e^" ?ê)
 ("e`" ?è)
 ("e~" ?ě)
 ("e=" ?€)
 ("g," ?ģ)
 ("g." ?ġ)
 ("g^" ?ĝ)
 ("g~" ?ğ)
 ("h/" ?ħ)
 ("h^" ?ĥ)
 ("i'" ?í)
 ("i," ?į)
 ("i-" ?ī)
 ("i." ?ı)
 ("i\"" ?ï)
 ("i^" ?î)
 ("i`" ?ì)
 ("i~" ?ĩ)
 ("j^" ?ĵ)
 ("k," ?ķ)
 ("k/" ?ĸ)
 ("l'" ?ĺ)
 ("l," ?ļ)
 ("l/" ?ł)
 ("l~" ?ľ)
 ("n'" ?ń)
 ("n," ?ņ)
 ("n/" ?ŋ)
 ("n~" ?ñ)
 ("n~" ?ň)
 ("o'" ?ó)
 ("o-" ?ō)
 ("o/" ?ø)
 ("o/" ?œ)
 ("o:" ?ő)
 ("o\"" ?ö)
 ("o^" ?ô)
 ("o_" ?º)
 ("o`" ?ò)
 ("o~" ?õ)
 ("r'" ?ŕ)
 ("r," ?ŗ)
 ("r~" ?ř)
 ("s'" ?ś)
 ("s," ?ş)
 ("s/" ?ß)
 ("s^" ?ŝ)
 ("s~" ?š)
 ("t," ?ţ)
 ("t/" ?þ)
 ("t/" ?ŧ)
 ("t~" ?ť)
 ("u'" ?ú)
 ("u," ?ų)
 ("u-" ?ū)
 ("u." ?ů)
 ("u:" ?ű)
 ("u\"" ?ü)
 ("u^" ?û)
 ("u`" ?ù)
 ("u~" ?ũ)
 ("u~" ?ŭ)
 ("y'" ?ý)
 ("y\"" ?ÿ)
 ("z'" ?ź)
 ("z." ?ż)
 ("z~" ?ž)

 ("!//" ["!/"])
 ("///" ["//"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("?//" ["?/"])
 ("$//" ["$/"])
 ("A''" ["A'"])
 ("A,," ["A,"])
 ("A--" ["A-"])
 ("A//" ["A/"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A``" ["A`"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C,," ["C,"])
 ("C.." ["C."])
 ("C^^" ["C^"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E,," ["E,"])
 ("E--" ["E-"])
 ("E.." ["E."])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("E~~" ["E~"])
 ("G,," ["G,"])
 ("G.." ["G."])
 ("G^^" ["G^"])
 ("G~~" ["G~"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I''" ["I'"])
 ("I,," ["I,"])
 ("I--" ["I-"])
 ("I.." ["I."])
 ("I\"\"" ["I\""])
 ("I^^" ["I^"])
 ("I``" ["I`"])
 ("I~~" ["I~"])
 ("J^^" ["J^"])
 ("K,," ["K,"])
 ("L''" ["L'"])
 ("L,," ["L,"])
 ("L//" ["L/"])
 ("L~~" ["L~"])
 ("N''" ["N'"])
 ("N,," ["N,"])
 ("N//" ["N/"])
 ("N~~" ["N~"])
 ("O''" ["O'"])
 ("O--" ["O-"])
 ("O//" ["O/"])
 ("O::" ["O:"])
 ("O\"\"" ["O\""])
 ("O^^" ["O^"])
 ("O``" ["O`"])
 ("O~~" ["O~"])
 ("R''" ["R'"])
 ("R,," ["R,"])
 ("R~~" ["R~"])
 ("S''" ["S'"])
 ("S,," ["S,"])
 ("S^^" ["S^"])
 ("S~~" ["S~"])
 ("T,," ["T,"])
 ("T//" ["T/"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U,," ["U,"])
 ("U--" ["U-"])
 ("U.." ["U."])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U^^" ["U^"])
 ("U``" ["U`"])
 ("U~~" ["U~"])
 ("Y''" ["Y'"])
 ("Y\"\"" ["Y\""])
 ("Y==" ["Y="])
 ("Z''" ["Z'"])
 ("Z.." ["Z."])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a,," ["a,"])
 ("a--" ["a-"])
 ("a//" ["a/"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a__" ["a_"])
 ("a``" ["a`"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c,," ["c,"])
 ("c.." ["c."])
 ("c^^" ["c^"])
 ("c~~" ["c~"])
 ("c//" ["c/"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e,," ["e,"])
 ("e--" ["e-"])
 ("e.." ["e."])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("e==" ["e="])
 ("e~~" ["e~"])
 ("g,," ["g,"])
 ("g.." ["g."])
 ("g^^" ["g^"])
 ("g~~" ["g~"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i''" ["i'"])
 ("i,," ["i,"])
 ("i--" ["i-"])
 ("i.." ["i."])
 ("i\"\"" ["i\""])
 ("i^^" ["i^"])
 ("i``" ["i`"])
 ("i~~" ["i~"])
 ("j^^" ["j^"])
 ("k,," ["k,"])
 ("k//" ["k/"])
 ("l''" ["l'"])
 ("l,," ["l,"])
 ("l//" ["l/"])
 ("l~~" ["l~"])
 ("n''" ["n'"])
 ("n,," ["n,"])
 ("n//" ["n/"])
 ("n~~" ["n~"])
 ("o''" ["o'"])
 ("o--" ["o-"])
 ("o//" ["o/"])
 ("o::" ["o:"])
 ("o\"\"" ["o\""])
 ("o^^" ["o^"])
 ("o__" ["o_"])
 ("o``" ["o`"])
 ("o~~" ["o~"])
 ("r''" ["r'"])
 ("r,," ["r,"])
 ("r~~" ["r~"])
 ("s''" ["s'"])
 ("s,," ["s,"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("s~~" ["s~"])
 ("t,," ["t,"])
 ("t//" ["t/"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u,," ["u,"])
 ("u--" ["u-"])
 ("u.." ["u."])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("u~~" ["u~"])
 ("y''" ["y'"])
 ("y\"\"" ["y\""])
 ("z''" ["z'"])
 ("z.." ["z."])
 ("z~~" ["z~"])
 )

;; Derived from Slovenian.kmap from Yudit
;; attributed as: 2001-11-11 Roman Maurer <roman.maurer@amis.net>
(quail-define-package
 "slovenian" "Slovenian" "Sl" t
 "Slovenian postfix input."
 nil t t t nil nil nil nil nil nil t)

(quail-define-rules
 ("C<" ?Č)
 ("C'" ?Ć)
 ("D;" ?Đ)
 ("S<" ?Š)
 ("Z<" ?Ž)
 ("c<" ?č)
 ("c'" ?ć)
 ("d;" ?đ)
 ("s<" ?š)
 ("z<" ?ž))

;;; latin-post.el ends here
