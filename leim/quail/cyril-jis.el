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
 "cyrillic-jis-russian" "Cyrillic" "$B'('+(B" nil
 "$B'+'8'5','&'/(B keyboard layout same as JCUKEN (JIS X0208.1983 encoding)"
 nil t t t t nil nil nil nil nil t)

;;  1! 2@ 3# 4" 5: 6, 7. 8* 9( 0) -_ =+ ,L!(B
;;   ,L9(B  ,LF(B  ,LC(B  ,L:(B  ,L5(B  ,L=(B  ,L3(B  ,LH(B  ,LI(B  ,L7(B  ,LE(B  ,Lj(B
;;    ,LD(B  ,LK(B  ,L2(B  ,L0(B  ,L?(B  ,L@(B  ,L>(B  ,L;(B  ,L4(B  ,L6(B  ,LM(B
;;     ,LO(B  ,LG(B  ,LA(B  ,L<(B  ,L8(B  ,LB(B  ,LL(B  ,L1(B  ,LN(B  /?

(quail-define-rules
 ("1" ?$B#1(B)
 ("2" ?$B#2(B)
 ("3" ?$B#3(B)
 ("4" ?$B#4(B)
 ("5" ?$B#5(B)
 ("6" ?$B#6(B)
 ("7" ?$B#7(B)
 ("8" ?$B#8(B)
 ("9" ?$B#9(B)
 ("0" ?$B#0(B)
 ("-" ?$B!](B)
 ("=" ?$B!a(B)
 ("`" ?$B'W(B)
 ("q" ?$B'[(B)
 ("w" ?$B'h(B)
 ("e" ?$B'e(B)
 ("r" ?$B'\(B)
 ("t" ?$B'V(B)
 ("y" ?$B'_(B)
 ("u" ?$B'T(B)
 ("i" ?$B'j(B)
 ("o" ?$B'k(B)
 ("p" ?$B'Y(B)
 ("[" ?$B'g(B)
 ("]" ?$B'l(B)
 ("a" ?$B'f(B)
 ("s" ?$B'm(B)
 ("d" ?$B'S(B)
 ("f" ?$B'Q(B)
 ("g" ?$B'a(B)
 ("h" ?$B'b(B)
 ("j" ?$B'`(B)
 ("k" ?$B'](B)
 ("l" ?$B'U(B)
 (";" ?$B'X(B)
 ("'" ?$B'o(B)
 ("\\" ?$B!@(B)
 ("z" ?$B'q(B)
 ("x" ?$B'i(B)
 ("c" ?$B'c(B)
 ("v" ?$B'^(B)
 ("b" ?$B'Z(B)
 ("n" ?$B'd(B)
 ("m" ?$B'n(B)
 ("," ?$B'R(B)
 ("." ?$B'p(B)
 ("/" ?$B!?(B)

 ("!" ?$B!*(B)
 ("@" ?$B!w(B)
 ("#" ?$B!t(B)
 ("$" ?$B!I(B)
 ("%" ?$B!'(B)
 ("^" ?$B!$(B)
 ("&" ?$B!%(B)
 ("*" ?$B!v(B)
 ("(" ?$B!J(B)
  (")" ?$B!K(B)
 ("_" ?$B!2(B)
 ("+" ?$B!\(B)
 ("~" ?$B''(B)
 ("Q" ?$B'+(B)
 ("W" ?$B'8(B)
 ("E" ?$B'5(B)
 ("R" ?$B',(B)
 ("T" ?$B'&(B)
 ("Y" ?$B'/(B)
 ("U" ?$B'$(B)
 ("I" ?$B':(B)
 ("O" ?$B';(B)
 ("P" ?$B')(B)
 ("{" ?$B'7(B)
 ("}" ?$B'<(B)
 ("A" ?$B'6(B)
 ("S" ?$B'=(B)
 ("D" ?$B'#(B)
 ("F" ?$B'!(B)
 ("G" ?$B'1(B)
 ("H" ?$B'2(B)
 ("J" ?$B'0(B)
 ("K" ?$B'-(B)
 ("L" ?$B'%(B)
 (":" ?$B'((B)
 ("\"" ?$B'?(B)
 ("|" ?$B!C(B)
 ("Z" ?$B'A(B)
 ("X" ?$B'9(B)
 ("C" ?$B'3(B)
 ("V" ?$B'.(B)
 ("B" ?$B'*(B)
 ("N" ?$B'4(B)
 ("M" ?$B'>(B)
 ("<" ?$B'"(B)
 (">" ?$B'@(B)
 ("?" ?$B!)(B))

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyril-jis.el ends here
