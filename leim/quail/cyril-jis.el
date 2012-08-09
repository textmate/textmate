;;; cyril-jis.el --- Quail package for inputting JISX0208 Cyrillic letters

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Cyrillic

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

;; This Quail package is for inputting Cyrillic letters of Japanese
;; character set JISX0208, not for inputting Cyrillic letters of
;; ISO-8859-5.  For the latter, use packages in quail/cyrillic.el.

;;; Code:

(quail-define-package
 "cyrillic-jis-russian" "Cyrillic" "ЖЙ" nil
 "ЙЦУКЕН keyboard layout same as JCUKEN (JIS X0208.1983 encoding)"
 nil t t t t nil nil nil nil nil t)

;;  1! 2@ 3# 4" 5: 6, 7. 8* 9( 0) -_ =+ ,L!
;;   ,L9  ,LF  ,LC  ,L:  ,L5  ,L=  ,L3  ,LH  ,LI  ,L7  ,LE  ,Lj
;;    ,LD  ,LK  ,L2  ,L0  ,L?  ,L@  ,L>  ,L;  ,L4  ,L6  ,LM
;;     ,LO  ,LG  ,LA  ,L<  ,L8  ,LB  ,LL  ,L1  ,LN  /?

(quail-define-rules
 ("1" ?１)
 ("2" ?２)
 ("3" ?３)
 ("4" ?４)
 ("5" ?５)
 ("6" ?６)
 ("7" ?７)
 ("8" ?８)
 ("9" ?９)
 ("0" ?０)
 ("-" ?−)
 ("=" ?＝)
 ("`" ?ё)
 ("q" ?й)
 ("w" ?ц)
 ("e" ?у)
 ("r" ?к)
 ("t" ?е)
 ("y" ?н)
 ("u" ?г)
 ("i" ?ш)
 ("o" ?щ)
 ("p" ?з)
 ("[" ?х)
 ("]" ?ъ)
 ("a" ?ф)
 ("s" ?ы)
 ("d" ?в)
 ("f" ?а)
 ("g" ?п)
 ("h" ?р)
 ("j" ?о)
 ("k" ?л)
 ("l" ?д)
 (";" ?ж)
 ("'" ?э)
 ("\\" ?＼)
 ("z" ?я)
 ("x" ?ч)
 ("c" ?с)
 ("v" ?м)
 ("b" ?и)
 ("n" ?т)
 ("m" ?ь)
 ("," ?б)
 ("." ?ю)
 ("/" ?／)

 ("!" ?！)
 ("@" ?＠)
 ("#" ?＃)
 ("$" ?”)
 ("%" ?：)
 ("^" ?，)
 ("&" ?．)
 ("*" ?＊)
 ("(" ?（)
  (")" ?）)
 ("_" ?＿)
 ("+" ?＋)
 ("~" ?Ё)
 ("Q" ?Й)
 ("W" ?Ц)
 ("E" ?У)
 ("R" ?К)
 ("T" ?Е)
 ("Y" ?Н)
 ("U" ?Г)
 ("I" ?Ш)
 ("O" ?Щ)
 ("P" ?З)
 ("{" ?Х)
 ("}" ?Ъ)
 ("A" ?Ф)
 ("S" ?Ы)
 ("D" ?В)
 ("F" ?А)
 ("G" ?П)
 ("H" ?Р)
 ("J" ?О)
 ("K" ?Л)
 ("L" ?Д)
 (":" ?Ж)
 ("\"" ?Э)
 ("|" ?｜)
 ("Z" ?Я)
 ("X" ?Ч)
 ("C" ?С)
 ("V" ?М)
 ("B" ?И)
 ("N" ?Т)
 ("M" ?Ь)
 ("<" ?Б)
 (">" ?Ю)
 ("?" ?？))

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyril-jis.el ends here
