;;; latin-alt.el --- Quail package for inputting various European characters -*-coding: utf-8;-*-

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, latin

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

;; Author (of latin-post.el): TAKAHASHI Naoto <ntakahas@etl.go.jp>

;;; Commentary:

;; These input methods differ from those in latin-post.el
;; in that comma is not special (use / instead),
;; and // is not special either (so you can enter a slash
;; by typing //).

;; At least, that's what I could see by comparing the first few
;; of these with latin-post.el.

;;; Code:

(require 'quail)

(quail-define-package
 "latin-1-alt-postfix" "Latin-1" "1<" t
 "Latin-1 character input method with postfix modifiers
This input method differs from `latin-1-postfix' in that
comma is not special (use slash instead), and `//' is not
special (so you can use that to enter a slash).

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  tilde      |    ~    | a~ -> ã
  cedilla    |    /    | c/ -> ç
  nordic     |    /    | d/ -> ð   t/ -> þ   a/ -> å   e/ -> æ   o/ -> ø
  others     |   /<>   | s/ -> ß   ?/ -> ¿   !/ -> ¡
             | various | << -> «   >> -> »   o_ -> º   a_ -> ª

It seems natural to use comma for cedillas, but that is
inconvenient in practice because commas are needed very
often after a letter.

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
 ("C/" ?Ç)
 ("c/" ?ç)
 ("N~" ?Ñ)
 ("n~" ?ñ)
 ("?/" ?¿)
 ("!/" ?¡)
 ("<<" ?«)
 (">>" ?»)
 ("o_" ?º)
 ("a_" ?ª)

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
 ("C//" ["C/"])
 ("c//" ["c/"])
 ("N~~" ["N~"])
 ("n~~" ["n~"])
 ("?//" ["?/"])
 ("!//" ["!/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 )

(quail-define-package
 "latin-2-alt-postfix" "Latin-2" "2<" t
 "Latin-2 character input method with postfix modifiers
This input method differs from `latin-2-postfix' in that
comma and period are not special (use ` instead).

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  ogonek     |    `    | a` -> ą
  diaeresis  |    \"    | a\" -> ä
  circumflex |    ^    | a^ -> â
  breve      |    ~    | a~ -> ă
  cedilla    |    `    | c` -> ç
  caron      |    ~    | c~ -> č
  dbl. acute |    :    | o: -> ő
  ring       |    `    | u` -> ů
  dot        |    `    | z` -> ż
  stroke     |    /    | d/ -> đ
  others     |    /    | s/ -> ß

It seems natural to use period and comma for dots/rings and
cedillas/ogoneks, but that is inconvenient in practice, because
periods and commas are needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?Á)
 ("A`" ?Ą)
 ("A\"" ?Ä)
 ("A^" ?Â)
 ("A~" ?Ă)
 ("C'" ?Ć)
 ("C`" ?Ç)
 ("C~" ?Č)
 ("D/" ?Đ)
 ("D~" ?Ď)
 ("E'" ?É)
 ("E`" ?Ę)
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
 ("S`" ?Ş)
 ("S~" ?Š)
 ("T`" ?Ţ)
 ("T~" ?Ť)
 ("U'" ?Ú)
 ("U:" ?Ű)
 ("U\"" ?Ü)
 ("U`" ?Ů)
 ("Y'" ?Ý)
 ("Z'" ?Ź)
 ("Z`" ?Ż)
 ("Z~" ?Ž)
 ("a'" ?á)
 ("a`" ?ą)
 ("a\"" ?ä)
 ("a^" ?â)
 ("a~" ?ă)
 ("c'" ?ć)
 ("c`" ?ç)
 ("c~" ?č)
 ("d/" ?đ)
 ("d~" ?ď)
 ("e'" ?é)
 ("e`" ?ę)
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
 ("s`" ?ş)
 ("s/" ?ß)
 ("s~" ?š)
 ("t`" ?ţ)
 ("t~" ?ť)
 ("u'" ?ú)
 ("u:" ?ű)
 ("u\"" ?ü)
 ("u`" ?ů)
 ("y'" ?ý)
 ("z'" ?ź)
 ("z`" ?ż)
 ("z~" ?ž)

 ("A''" ["A'"])
 ("A``" ["A`"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C``" ["C`"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E``" ["E`"])
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
 ("S``" ["S`"])
 ("S~~" ["S~"])
 ("T``" ["T`"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U``" ["U`"])
 ("Y''" ["Y'"])
 ("Z''" ["Z'"])
 ("Z``" ["Z`"])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a``" ["a`"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c``" ["c`"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e``" ["e`"])
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
 ("s``" ["s`"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t``" ["t`"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u``" ["u`"])
 ("y''" ["y'"])
 ("z''" ["z'"])
 ("z``" ["z`"])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-3-alt-postfix" "Latin-3" "3<" t
 "Latin-3 character input method with postfix modifiers
This input method differs from `latin-3-postfix' in that
comma is not special (use ` instead), and period is not
special (use slash instead).

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  dot        |    /    | c/ -> ċ   i/ -> ı   I/ -> İ
  cedilla    |    `    | c` -> ç
  breve      |    ~    | g~ -> ğ
  tilde      |    ~    | n~ -> ñ
  stroke     |    /    | h/ -> ħ
  others     |    /    | s/ -> ß

It would be natural to use period and comma for dots and cedillas, but
that would inconvenient in practice, because periods and commas are
needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?À)
 ("A'" ?Á)
 ("A^" ?Â)
 ("A\"" ?Ä)
 ("C/" ?Ċ)
 ("C^" ?Ĉ)
 ("C`" ?Ç)
 ("E`" ?È)
 ("E'" ?É)
 ("E^" ?Ê)
 ("E\"" ?Ë)
 ("G~" ?Ğ)
 ("G/" ?Ġ)
 ("G^" ?Ĝ)
 ("H/" ?Ħ)
 ("H^" ?Ĥ)
 ("I/" ?İ)
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
 ("S`" ?Ş)
 ("S^" ?Ŝ)
 ("U`" ?Ù)
 ("U'" ?Ú)
 ("U^" ?Û)
 ("U\"" ?Ü)
 ("U~" ?Ŭ)
 ("Z/" ?Ż)
 ("a`" ?à)
 ("a'" ?á)
 ("a^" ?â)
 ("a\"" ?ä)
 ("c/" ?ċ)
 ("c^" ?ĉ)
 ("c`" ?ç)
 ("e`" ?è)
 ("e'" ?é)
 ("e^" ?ê)
 ("e\"" ?ë)
 ("g~" ?ğ)
 ("g/" ?ġ)
 ("g^" ?ĝ)
 ("h/" ?ħ)
 ("h^" ?ĥ)
 ("i/" ?ı)
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
 ("s`" ?ş)
 ("s/" ?ß)
 ("s^" ?ŝ)
 ("u`" ?ù)
 ("u'" ?ú)
 ("u^" ?û)
 ("u\"" ?ü)
 ("u~" ?ŭ)
 ("z/" ?ż)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("A^^" ["A^"])
 ("A\"\"" ["A\""])
 ("C//" ["C/"])
 ("C^^" ["C^"])
 ("C``" ["C`"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("E^^" ["E^"])
 ("E\"\"" ["E\""])
 ("G~~" ["G~"])
 ("G//" ["G/"])
 ("G^^" ["G^"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I//" ["I/"])
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
 ("S``" ["S`"])
 ("S^^" ["S^"])
 ("U``" ["U`"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("Z//" ["Z/"])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("a^^" ["a^"])
 ("a\"\"" ["a\""])
 ("c//" ["c/"])
 ("c^^" ["c^"])
 ("c``" ["c`"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("e^^" ["e^"])
 ("e\"\"" ["e\""])
 ("g~~" ["g~"])
 ("g//" ["g/"])
 ("g^^" ["g^"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i//" ["i/"])
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
 ("s``" ["s`"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("u``" ["u`"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("z//" ["z/"])
 )

(quail-define-package
 "latin-4-alt-postfix" "Latin-4" "4<" t
 "Latin-4 characters input method with postfix modifiers
This input method differs from `latin-4-postfix' in that
comma is not special (use ` instead), and period is not
special (use ~ instead).

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  ogonek     |    `    | a` -> ą
  macron     |    -    | a- -> ā
  tilde      |    ~    | a~ -> ã
  caron      |    ~    | c~ -> č
  dot        |    ~    | e~ -> ė
  cedilla    |    `    | k` -> ķ   g` -> ģ
  stroke     |    /    | d/ -> đ
  nordic     |    /    | a/ -> å   e/ -> æ   o/ -> ø
  others     |    /    | s/ -> ß   n/ -> ŋ   k/ -> ĸ

It seems natural to use period and comma for dots and
cedillas/ogoneks, but that is inconvenient in practice, because
periods and commas are needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?Ą)
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
 ("E`" ?Ę)
 ("E\"" ?Ë)
 ("E~" ?Ė)
 ("G`" ?Ģ)
 ("I~" ?Ĩ)
 ("I`" ?Į)
 ("I'" ?Í)
 ("I^" ?Î)
 ("I-" ?Ī)
 ("K`" ?Ķ)
 ("L`" ?Ļ)
 ("N/" ?Ŋ)
 ("N`" ?Ņ)
 ("O-" ?Ō)
 ("O^" ?Ô)
 ("O~" ?Õ)
 ("O\"" ?Ö)
 ("O/" ?Ø)
 ("R`" ?Ŗ)
 ("S~" ?Š)
 ("T/" ?Ŧ)
 ("U`" ?Ų)
 ("U'" ?Ú)
 ("U^" ?Û)
 ("U\"" ?Ü)
 ("U~" ?Ũ)
 ("U-" ?Ū)
 ("Z~" ?Ž)
 ("a`" ?ą)
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
 ("e`" ?ę)
 ("e\"" ?ë)
 ("e~" ?ė)
 ("g`" ?ģ)
 ("i~" ?ĩ)
 ("i`" ?į)
 ("i'" ?í)
 ("i^" ?î)
 ("i-" ?ī)
 ("k/" ?ĸ)
 ("k`" ?ķ)
 ("l`" ?ļ)
 ("n/" ?ŋ)
 ("n`" ?ņ)
 ("o-" ?ō)
 ("o^" ?ô)
 ("o~" ?õ)
 ("o\"" ?ö)
 ("o/" ?ø)
 ("r`" ?ŗ)
 ("s/" ?ß)
 ("s~" ?š)
 ("t/" ?ŧ)
 ("u`" ?ų)
 ("u'" ?ú)
 ("u^" ?û)
 ("u\"" ?ü)
 ("u~" ?ũ)
 ("u-" ?ū)
 ("z~" ?ž)

 ("A``" ["A`"])
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
 ("E``" ["E`"])
 ("E\"\"" ["E\""])
 ("E~~" ["E~"])
 ("G``" ["G`"])
 ("I~~" ["I~"])
 ("I``" ["I`"])
 ("I''" ["I'"])
 ("I^^" ["I^"])
 ("I--" ["I-"])
 ("K``" ["K`"])
 ("L``" ["L`"])
 ("N//" ["N/"])
 ("N``" ["N`"])
 ("O--" ["O-"])
 ("O^^" ["O^"])
 ("O~~" ["O~"])
 ("O\"\"" ["O\""])
 ("O//" ["O/"])
 ("R``" ["R`"])
 ("S~~" ["S~"])
 ("T//" ["T/"])
 ("U``" ["U`"])
 ("U''" ["U'"])
 ("U^^" ["U^"])
 ("U\"\"" ["U\""])
 ("U~~" ["U~"])
 ("U--" ["U-"])
 ("Z~~" ["Z~"])
 ("a``" ["a`"])
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
 ("e``" ["e`"])
 ("e\"\"" ["e\""])
 ("e~~" ["e~"])
 ("g``" ["g`"])
 ("i~~" ["i~"])
 ("i``" ["i`"])
 ("i''" ["i'"])
 ("i^^" ["i^"])
 ("i--" ["i-"])
 ("k//" ["k/"])
 ("k``" ["k`"])
 ("l``" ["l`"])
 ("n//" ["n/"])
 ("n``" ["n`"])
 ("o--" ["o-"])
 ("o^^" ["o^"])
 ("o~~" ["o~"])
 ("o\"\"" ["o\""])
 ("o//" ["o/"])
 ("r``" ["r`"])
 ("s//" ["s/"])
 ("s~~" ["s~"])
 ("t//" ["t/"])
 ("u``" ["u`"])
 ("u''" ["u'"])
 ("u^^" ["u^"])
 ("u\"\"" ["u\""])
 ("u~~" ["u~"])
 ("u--" ["u-"])
 ("z~~" ["z~"])
 )

(quail-define-package
 "latin-5-alt-postfix" "Latin-5" "5<" t
 "Latin-5 characters input method with postfix modifiers
This input method differs from `latin-5-postfix' in that
comma is not special (use ` instead), and period is not
special (use / instead).

             | postfix | examples
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  diaeresis  |    \"    | a\" -> ä
  tilde      |    ~    | a~ -> ã
  breve      |    ~    | g~ -> ğ
  cedilla    |    `    | c` -> ç
  dot        |    /    | i/ -> ı   I/ -> İ
  nordic     |    /    | a/ -> å   e/ -> æ   o/ -> ø
  others     |    /    | s/ -> ß

It seems natural to use period and comma for dots and cedillas, but
that is inconvenient in practice, because periods and commas are
needed very often after a letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A'" ?Á)
 ("A/" ?Å)
 ("A\"" ?Ä)
 ("A^" ?Â)
 ("A`" ?À)
 ("A~" ?Ã)
 ("C`" ?Ç)
 ("E'" ?É)
 ("E/" ?Æ)
 ("E\"" ?Ë)
 ("E^" ?Ê)
 ("E`" ?È)
 ("G~" ?Ğ)
 ("I'" ?Í)
 ("I/" ?İ)
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
 ("S`" ?Ş)
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
 ("c`" ?ç)
 ("e'" ?é)
 ("e/" ?æ)
 ("e\"" ?ë)
 ("e^" ?ê)
 ("e`" ?è)
 ("g~" ?ğ)
 ("i'" ?í)
 ("i/" ?ı)
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
 ("s`" ?ş)
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
 ("C``" ["C`"])
 ("E''" ["E'"])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("G~~" ["G~"])
 ("I''" ["I'"])
 ("I//" ["I/"])
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
 ("S``" ["S`"])
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
 ("c``" ["c`"])
 ("e''" ["e'"])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("g~~" ["g~"])
 ("i''" ["i'"])
 ("i//" ["i/"])
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
 ("s``" ["s`"])
 ("s//" ["s/"])
 ("u''" ["u'"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("y\"\"" ["y\""])
 )



(quail-define-package
 "french-alt-postfix" "French" "FR<" t
 "French (Français) input method with postfix modifiers

` pour grave, ' pour aigu, ^ pour circonflexe, et \" pour tréma.
Par exemple: a` -> à   e' -> é.

Ç, «, et » sont produits par C/, <<, et >>.

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
 ("C/" ?Ç)
 ("c/" ?ç)
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
 ("C//" ["C/"])
 ("c//" ["c/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 )



(quail-define-package
 "italian-alt-postfix" "Latin-1" "IT<" t
 "Italian (Italiano) input method with postfix modifiers

a' -> á    A' -> Á    a` -> à    A` -> À    i^ -> î    << -> «
e' -> é    E' -> É    e` -> è    E` -> È    I^ -> Î    >> -> »
i' -> í    I' -> Í    i` -> ì    I` -> Ì               o_ -> º
o' -> ó    O' -> Ó    o` -> ò    O` -> Ò               a_ -> ª
u' -> ú    U' -> Ú    u` -> ù    U` -> Ù

This method is for purists who like accents the old way.

Doubling the postfix separates the letter and postfix: e.g. a`` -> a`
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A`" ?À)
 ("A'" ?Á)
 ("a`" ?à)
 ("a'" ?á)
 ("E`" ?È)
 ("E'" ?É)
 ("e`" ?è)
 ("e'" ?é)
 ("I`" ?Ì)
 ("i`" ?ì)
 ("I'" ?Í)
 ("i'" ?í)
 ("I^" ?Î)
 ("i^" ?î)
 ("O`" ?Ò)
 ("o`" ?ò)
 ("O'" ?Ó)
 ("o'" ?ó)
 ("U`" ?Ù)
 ("u`" ?ù)
 ("U'" ?Ú)
 ("u'" ?ú)
 ("<<" ?«)
 (">>" ?»)
 ("o_" ?º)
 ("a_" ?ª)

 ("A``" ["A`"])
 ("A''" ["A'"])
 ("a``" ["a`"])
 ("a''" ["a'"])
 ("E``" ["E`"])
 ("E''" ["E'"])
 ("e``" ["e`"])
 ("e''" ["e'"])
 ("I``" ["I`"])
 ("i``" ["i`"])
 ("I''" ["I'"])
 ("i''" ["i'"])
 ("I^^" ["I^"])
 ("i^^" ["i^"])
 ("O``" ["O`"])
 ("o``" ["o`"])
 ("O''" ["O'"])
 ("o''" ["o'"])
 ("U``" ["U`"])
 ("u``" ["u`"])
 ("U''" ["U'"])
 ("u''" ["u'"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("o__" ["o_"])
 ("a__" ["a_"])
 )


(quail-define-package
 "turkish-alt-postfix" "Turkish" "TR«" t
 "Turkish (Türkçe) input method with postfix modifiers.
This input method differs from `turkish-postfix' in that
comma is not special (use ` instead).

turkish-latin-3-alt-postfix is an obsolete alias for turkish-alt-postfix.

Note for I, ı, İ, i.

A^ -> Â
C` -> Ç
G^ -> Ğ
I  -> I
i  -> ı
I/ -> İ
i/ -> i
O\" -> Ö
S` -> Ş
U\" -> Ü
U^ -> Û

Doubling the postfix separates the letter and postfix: e.g. a^^ -> a^
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A^" ?Â)
 ("a^" ?â)
 ("C`" ?Ç)
 ("c`" ?ç)
 ("G^" ?Ğ)
 ("g^" ?ğ)
 ("I/" ?İ)
 ("i" ?ı)
 ("i/" ?i)
 ("O\"" ?Ö)
 ("o\"" ?ö)
 ("S`" ?Ş)
 ("s`" ?ş)
 ("U\"" ?Ü)
 ("u\"" ?ü)
 ("U^" ?Û)
 ("u^" ?û)

 ("A^^" ["A^"])
 ("a^^" ["a^"])
 ("C``" ["C`"])
 ("c``" ["c`"])
 ("G^^" ["G^"])
 ("g^^" ["g^"])
 ("I//" ["I/"])
 ("i" ["i"])
 ("i//" ["i/"])
 ("O\"\"" ["O\""])
 ("o\"\"" ["o\""])
 ("S``" ["S`"])
 ("s``" ["s`"])
 ("U\"\"" ["U\""])
 ("u\"\"" ["u\""])
 ("U^^" ["U^"])
 ("u^^" ["u^"])
 )

;; Backwards compatibility.
(push (cons "turkish-latin-3-alt-postfix"
	    (cdr (assoc "turkish-alt-postfix" quail-package-alist)))
      quail-package-alist)

;; Dutch Quail input method derived from the one in Yudit by Roman
;; Czyborra.
(quail-define-package
 "dutch" "Dutch" "NL" t
 "Dutch character mixfix input method.
Caters for French and Turkish as well as Dutch.

             |         | examples
 ------------+---------+----------
  others     |         | fl. -> ƒ  eur. -> €  ij -> ĳ  IJ -> Ĳ
 ------------+---------+----------
             | postfix |
 ------------+---------+----------
  acute      |    '    | a' -> á
  grave      |    `    | a` -> à
  circumflex |    ^    | a^ -> â
  Turkish    | various | i/ -> ı  s, -> ş  g^ -> ğ   I/ -> İ
             |         |  S, -> Ş  G^ -> Ğ
 ------------+---------+----------
             | prefix  |
 ------------+---------+----------
  diaeresis  |    \"    | \"a -> ä

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("fl." ?ƒ) ;; LATIN SMALL LETTER F WITH HOOK (florin currency symbol)
 ("eur." ?€) ;; EURO SIGN
 ;; “The 25th letter of the Dutch alphabet.”
 ("ij" ?ĳ) ;; LATIN SMALL LIGATURE IJ
 ("IJ" ?Ĳ) ;; LATIN CAPITAL LIGATURE IJ
 ;; “Trema on the second letter of vowel pair.”  Yudit uses `:', not `"'.
 ("\"a" ?ä) ;; LATIN SMALL LETTER A WITH DIAERESIS 
 ("\"e" ?ë) ;; LATIN SMALL LETTER E WITH DIAERESIS 
 ("\"i" ?ï) ;; LATIN SMALL LETTER I WITH DIAERESIS 
 ("\"o" ?ö) ;; LATIN SMALL LETTER O WITH DIAERESIS 
 ("\"u" ?ü) ;; LATIN SMALL LETTER U WITH DIAERESIS 
 ("\"A" ?Ä) ;; LATIN CAPITAL LETTER A WITH DIAERESIS 
 ("\"E" ?Ë) ;; LATIN CAPITAL LETTER E WITH DIAERESIS 
 ("\"I" ?Ï) ;; LATIN CAPITAL LETTER I WITH DIAERESIS 
 ("\"O" ?Ö) ;; LATIN CAPITAL LETTER O WITH DIAERESIS 
 ("\"U" ?Ü) ;; LATIN CAPITAL LETTER U WITH DIAERESIS 
 ;; “Acute, marking emphasis on long vowels”:
 ("a'" ?á) ;; LATIN SMALL LETTER A WITH ACUTE 
 ("e'" ?é) ;; LATIN SMALL LETTER E WITH ACUTE 
 ("i'" ?í) ;; LATIN SMALL LETTER I WITH ACUTE 
 ("o'" ?ó) ;; LATIN SMALL LETTER O WITH ACUTE 
 ("u'" ?ú) ;; LATIN SMALL LETTER U WITH ACUTE 
 ("A'" ?Á) ;; LATIN CAPITAL LETTER A WITH ACUTE 
 ("E'" ?É) ;; LATIN CAPITAL LETTER E WITH ACUTE 
 ("I'" ?Í) ;; LATIN CAPITAL LETTER I WITH ACUTE 
 ("O'" ?Ó) ;; LATIN CAPITAL LETTER O WITH ACUTE 
 ("U'" ?Ú) ;; LATIN CAPITAL LETTER U WITH ACUTE 
 ;; “Grave, marking emphasis on short vowels”:
 ("a`" ?à) ;; LATIN SMALL LETTER A WITH GRAVE
 ("e`" ?è) ;; LATIN SMALL LETTER E WITH GRAVE 
 ("i`" ?ì) ;; LATIN SMALL LETTER I WITH GRAVE 
 ("o`" ?ò) ;; LATIN SMALL LETTER O WITH GRAVE 
 ("u`" ?ù) ;; LATIN SMALL LETTER U WITH GRAVE 
 ("A`" ?À) ;; LATIN CAPITAL LETTER A WITH GRAVE 
 ("E`" ?È) ;; LATIN CAPITAL LETTER E WITH GRAVE 
 ("I`" ?Ì) ;; LATIN CAPITAL LETTER I WITH GRAVE 
 ("O`" ?Ò) ;; LATIN CAPITAL LETTER O WITH GRAVE 
 ("U`" ?Ù) ;; LATIN CAPITAL LETTER U WITH GRAVE
 ;; “Cater for the use of many French words and use of the circumflex
 ;; in Frisian.”  Yudit used `;' for cedilla.
 ("c," ?ç) ;; LATIN SMALL LETTER C WITH CEDILLA 
 ("C," ?Ç) ;; LATIN CAPITAL LETTER C WITH CEDILLA 
 ("a^" ?â) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX 
 ("e^" ?ê) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX 
 ("i^" ?î) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX 
 ("o^" ?ô) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX 
 ("u^" ?û) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX 
 ("A^" ?Â) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX 
 ("E^" ?Ê) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX 
 ("I^" ?Î) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX 
 ("O^" ?Ô) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX 
 ("U^" ?Û) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
 ;; “Follow the example of the Dutch POSIX locale, using ISO-8859-9 to
 ;; cater to the many Turks in Dutch society.”  Perhaps German methods
 ;; should do so too.  Follow turkish-alt-postfix here.
 ("i/" ?ı) ;; LATIN SMALL LETTER I WITH NO DOT
 ("s," ?ş) ;; LATIN SMALL LETTER S WITH CEDILLA 
 ("g^" ?ğ) ;; LATIN SMALL LETTER G WITH BREVE 
 ("I/" ?İ) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
 ("S," ?Ş) ;; LATIN CAPITAL LETTER S WITH CEDILLA 
 ("G^" ?Ğ) ;; LATIN CAPITAL LETTER G WITH BREVE 
 )

;; Originally from Yudit, discussed with Albertas Agejevas
;; <alga@uosis.mif.vu.lt>
(quail-define-package
 "lithuanian-numeric" "Lithuanian" "LtN" t
 "Lithuanian numeric input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?ą)
 ("2" ?č)
 ("3" ?ę)
 ("4" ?ė)
 ("5" ?į)
 ("6" ?š)
 ("7" ?ų)
 ("8" ?ū)
 ("9" ?„)
 ("0" ?“)
 ("=" ?ž)
 ("!" ?Ą)
 ("@" ?Č)
 ("#" ?Ę)
 ("$" ?Ė)
 ("%" ?Į)
 ("^" ?Š)
 ("&" ?Ų)
 ("*" ?Ū)
 ("+" ?Ž))

;; From XFree 4.1 /usr/X11R6/lib/X11/xkb/symbols/lt, suggested by
;; Albertas Agejevas <alga@uosis.mif.vu.lt>
(quail-define-package
 "lithuanian-keyboard" "Lithuanian" "Lt" t
 "Lithuanian standard keyboard input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?ą)
 ("!" ?Ą)
 ("2" ?č)
 ("@" ?Č)
 ("#" ?Ę)
 ("4" ?ė)
 ("$" ?Ė)
 ("5" ?į)
 ("%" ?Į)
 ("6" ?š)
 ("^" ?Š)
 ("7" ?ų)
 ("&" ?Ų)
 ("9" ?„)
 ("0" ?“)
 ("=" ?ž)
 ("+" ?Ž))

;; From XFree 4.1 /usr/X11R6/lib/X11/xkb/symbols/lv
(quail-define-package
 "latvian-keyboard" "Latvian" "Lv" t
 "Latvian standard keyboard input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("4" ?€)
 ("$" ?¢)
 ("e" ?ē)
 ("E" ?Ē)
 ("r" ?ŗ)
 ("R" ?Ŗ)
 ("u" ?ū)
 ("U" ?Ū)
 ("i" ?ī)
 ("I" ?Ī)
 ("o" ?ō)
 ("O" ?Ō)
 ("a" ?ā)
 ("A" ?Ā)
 ("s" ?š)
 ("S" ?Š)
 ("g" ?ģ)
 ("G" ?Ģ)
 ("k" ?ķ)
 ("K" ?Ķ)
 ("l" ?ļ)
 ("L" ?Ļ)
 ("\'" ?“)
 ("\"" ?„)
 ("z" ?ž)
 ("Z" ?Ž)
 ("c" ?č)
 ("C" ?Č)
 ("n" ?ņ)
 ("N" ?Ņ))

(quail-define-package
 "latin-alt-postfix" "Latin" "L<" t
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
  cedilla    |    /`   | c/ -> ç   c` -> ç
  ogonek     |    `    | a` -> ą
  breve      |    ~    | a~ -> ă
  caron      |    ~    | c~ -> č
  dbl. acute |    :    | o: -> ő
  ring       |    `    | u` -> ů
  dot        |    `    | z` -> ż
  stroke     |    /    | d/ -> đ
  nordic     |    /    | d/ -> ð   t/ -> þ   a/ -> å   e/ -> æ   o/ -> ø
  others     |   /<>   | s/ -> ß   ?/ -> ¿   !/ -> ¡
             | various | << -> «   >> -> »   o_ -> º   a_ -> ª

It would be natural to use comma for cedillas, but that would be
inconvenient in practice because commas are needed very often after a
letter.

Doubling the postfix separates the letter and postfix: e.g. a'' -> a'
" nil t nil nil nil nil nil nil nil nil t)

;; Fixme: ¦ § ¨ © ¬ ­ ® ¯ ° ± ² ³ ´ µ ¶ · ¸ ¹ ¼ ½ ¾ × ÷
(quail-define-rules
 (" _" ? )
 ("!/" ?¡)
 ("//" ?°)
 ("<<" ?«)
 (">>" ?»)
 ("?/" ?¿)
 ("$/" ?£)
 ("$/" ?¤)
 ("A'" ?Á)
 ("A-" ?Ā)
 ("A/" ?Å)
 ("A\"" ?Ä)
 ("A^" ?Â)
 ("A`" ?À)
 ("A`" ?Ą)
 ("A~" ?Ã)
 ("A~" ?Ă)
 ("C'" ?Ć)
 ("C/" ?Ç)
 ("C/" ?Ċ)
 ("C^" ?Ĉ)
 ("C`" ?Ç)
 ("C~" ?Č)
 ("D/" ?Ð)
 ("D/" ?Đ)
 ("D~" ?Ď)
 ("E'" ?É)
 ("E-" ?Ē)
 ("E/" ?Æ)
 ("E\"" ?Ë)
 ("E^" ?Ê)
 ("E`" ?È)
 ("E`" ?Ę)
 ("E~" ?Ė)
 ("E~" ?Ě)
 ("G/" ?Ġ)
 ("G^" ?Ĝ)
 ("G`" ?Ģ)
 ("G~" ?Ğ)
 ("H/" ?Ħ)
 ("H^" ?Ĥ)
 ("I'" ?Í)
 ("I-" ?Ī)
 ("I/" ?İ)
 ("I\"" ?Ï)
 ("I^" ?Î)
 ("I`" ?Ì)
 ("I`" ?Į)
 ("I~" ?Ĩ)
 ("J^" ?Ĵ)
 ("K`" ?Ķ)
 ("L'" ?Ĺ)
 ("L/" ?Ł)
 ("L`" ?Ļ)
 ("L~" ?Ľ)
 ("N'" ?Ń)
 ("N/" ?Ŋ)
 ("N`" ?Ņ)
 ("N~" ?Ñ)
 ("N~" ?Ň)
 ("O'" ?Ó)
 ("O-" ?Ō)
 ("O/" ?Ø)
 ("O:" ?Ő)
 ("O\"" ?Ö)
 ("O^" ?Ô)
 ("O`" ?Ò)
 ("O~" ?Õ)
 ("R'" ?Ŕ)
 ("R`" ?Ŗ)
 ("R~" ?Ř)
 ("S'" ?Ś)
 ("S^" ?Ŝ)
 ("S`" ?Ş)
 ("S~" ?Š)
 ("T/" ?Þ)
 ("T/" ?Ŧ)
 ("T`" ?Ţ)
 ("T~" ?Ť)
 ("U'" ?Ú)
 ("U-" ?Ū)
 ("U:" ?Ű)
 ("U\"" ?Ü)
 ("U^" ?Û)
 ("U`" ?Ù)
 ("U`" ?Ů)
 ("U`" ?Ų)
 ("U~" ?Ũ)
 ("U~" ?Ŭ)
 ("Y'" ?Ý)
 ("Y\"" ?Ÿ)
 ("Y=" ?¥)
 ("Z'" ?Ź)
 ("Z/" ?Ż)
 ("Z`" ?Ż)
 ("Z~" ?Ž)
 ("a'" ?á)
 ("a-" ?ā)
 ("a/" ?å)
 ("a\"" ?ä)
 ("a^" ?â)
 ("a_" ?ª)
 ("a`" ?à)
 ("a`" ?ą)
 ("a~" ?ã)
 ("a~" ?ă)
 ("c'" ?ć)
 ("c/" ?ç)
 ("c/" ?ċ)
 ("c/" ?¢)
 ("c^" ?ĉ)
 ("c`" ?ç)
 ("c~" ?č)
 ("d/" ?ð)
 ("d/" ?đ)
 ("d~" ?ď)
 ("e'" ?é)
 ("e-" ?ē)
 ("e/" ?æ)
 ("e\"" ?ë)
 ("e^" ?ê)
 ("e`" ?è)
 ("e`" ?ę)
 ("e~" ?ė)
 ("e~" ?ě)
 ("e=" ?€)
 ("g/" ?ġ)
 ("g^" ?ĝ)
 ("g`" ?ģ)
 ("g~" ?ğ)
 ("h/" ?ħ)
 ("h^" ?ĥ)
 ("i'" ?í)
 ("i-" ?ī)
 ("i/" ?ı)
 ("i\"" ?ï)
 ("i^" ?î)
 ("i`" ?ì)
 ("i`" ?į)
 ("i~" ?ĩ)
 ("j^" ?ĵ)
 ("k/" ?ĸ)
 ("k`" ?ķ)
 ("l'" ?ĺ)
 ("l/" ?ł)
 ("l`" ?ļ)
 ("l~" ?ľ)
 ("n'" ?ń)
 ("n/" ?ŋ)
 ("n`" ?ņ)
 ("n~" ?ñ)
 ("n~" ?ň)
 ("o'" ?ó)
 ("o-" ?ō)
 ("o/" ?ø)
 ("o:" ?ő)
 ("o\"" ?ö)
 ("o^" ?ô)
 ("o_" ?º)
 ("o`" ?ò)
 ("o~" ?õ)
 ("r'" ?ŕ)
 ("r`" ?ŗ)
 ("r~" ?ř)
 ("s'" ?ś)
 ("s/" ?ß)
 ("s^" ?ŝ)
 ("s`" ?ş)
 ("s~" ?š)
 ("t/" ?þ)
 ("t/" ?ŧ)
 ("t`" ?ţ)
 ("t~" ?ť)
 ("u'" ?ú)
 ("u-" ?ū)
 ("u:" ?ű)
 ("u\"" ?ü)
 ("u^" ?û)
 ("u`" ?ù)
 ("u`" ?ů)
 ("u`" ?ų)
 ("u~" ?ũ)
 ("u~" ?ŭ)
 ("y'" ?ý)
 ("y\"" ?ÿ)
 ("z'" ?ź)
 ("z/" ?ż)
 ("z`" ?ż)
 ("z~" ?ž)

 (" __" [" _"])
 ("!//" ["!/"])
 ("<<<" ["<<"])
 (">>>" [">>"])
 ("?//" ["?/"])
 ("///" ["//"])
 ("$//" ["$/"])
 ("A''" ["A'"])
 ("A--" ["A-"])
 ("A//" ["A/"])
 ("A\"\"" ["A\""])
 ("A^^" ["A^"])
 ("A``" ["A`"])
 ("A~~" ["A~"])
 ("C''" ["C'"])
 ("C//" ["C/"])
 ("C^^" ["C^"])
 ("C``" ["C`"])
 ("C~~" ["C~"])
 ("D//" ["D/"])
 ("D~~" ["D~"])
 ("E''" ["E'"])
 ("E--" ["E-"])
 ("E//" ["E/"])
 ("E\"\"" ["E\""])
 ("E^^" ["E^"])
 ("E``" ["E`"])
 ("E~~" ["E~"])
 ("G//" ["G/"])
 ("G^^" ["G^"])
 ("G``" ["G`"])
 ("G~~" ["G~"])
 ("H//" ["H/"])
 ("H^^" ["H^"])
 ("I''" ["I'"])
 ("I--" ["I-"])
 ("I//" ["I/"])
 ("I\"\"" ["I\""])
 ("I^^" ["I^"])
 ("I``" ["I`"])
 ("I~~" ["I~"])
 ("J^^" ["J^"])
 ("K``" ["K`"])
 ("L''" ["L'"])
 ("L//" ["L/"])
 ("L``" ["L`"])
 ("L~~" ["L~"])
 ("N''" ["N'"])
 ("N//" ["N/"])
 ("N``" ["N`"])
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
 ("R``" ["R`"])
 ("R~~" ["R~"])
 ("S''" ["S'"])
 ("S^^" ["S^"])
 ("S``" ["S`"])
 ("S~~" ["S~"])
 ("T//" ["T/"])
 ("T``" ["T`"])
 ("T~~" ["T~"])
 ("U''" ["U'"])
 ("U--" ["U-"])
 ("U::" ["U:"])
 ("U\"\"" ["U\""])
 ("U^^" ["U^"])
 ("U``" ["U`"])
 ("U~~" ["U~"])
 ("Y''" ["Y'"])
 ("Z''" ["Z'"])
 ("Z//" ["Z/"])
 ("Z``" ["Z`"])
 ("Z~~" ["Z~"])
 ("a''" ["a'"])
 ("a--" ["a-"])
 ("a//" ["a/"])
 ("a\"\"" ["a\""])
 ("a^^" ["a^"])
 ("a__" ["a_"])
 ("a``" ["a`"])
 ("a~~" ["a~"])
 ("c''" ["c'"])
 ("c//" ["c/"])
 ("c^^" ["c^"])
 ("c``" ["c`"])
 ("c~~" ["c~"])
 ("d//" ["d/"])
 ("d~~" ["d~"])
 ("e''" ["e'"])
 ("e--" ["e-"])
 ("e//" ["e/"])
 ("e\"\"" ["e\""])
 ("e^^" ["e^"])
 ("e``" ["e`"])
 ("e~~" ["e~"])
 ("e==" ["e="])
 ("g//" ["g/"])
 ("g^^" ["g^"])
 ("g``" ["g`"])
 ("g~~" ["g~"])
 ("h//" ["h/"])
 ("h^^" ["h^"])
 ("i''" ["i'"])
 ("i--" ["i-"])
 ("i//" ["i/"])
 ("i\"\"" ["i\""])
 ("i^^" ["i^"])
 ("i``" ["i`"])
 ("i~~" ["i~"])
 ("j^^" ["j^"])
 ("k//" ["k/"])
 ("k``" ["k`"])
 ("l''" ["l'"])
 ("l//" ["l/"])
 ("l``" ["l`"])
 ("l~~" ["l~"])
 ("n''" ["n'"])
 ("n//" ["n/"])
 ("n``" ["n`"])
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
 ("r``" ["r`"])
 ("r~~" ["r~"])
 ("s''" ["s'"])
 ("s//" ["s/"])
 ("s^^" ["s^"])
 ("s``" ["s`"])
 ("s~~" ["s~"])
 ("t//" ["t/"])
 ("t``" ["t`"])
 ("t~~" ["t~"])
 ("u''" ["u'"])
 ("u--" ["u-"])
 ("u::" ["u:"])
 ("u\"\"" ["u\""])
 ("u^^" ["u^"])
 ("u``" ["u`"])
 ("u~~" ["u~"])
 ("y''" ["y'"])
 ("y\"\"" ["y\""])
 ("z''" ["z'"])
 ("z//" ["z/"])
 ("z``" ["z`"])
 ("z~~" ["z~"])
 )

;;; latin-alt.el ends here
