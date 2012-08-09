;;; cyrillic.el --- Quail package for inputting Cyrillic characters

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>
;; Keywords: multilingual, input method, Cyrillic, i18n

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

;; These methods use a mixture of 8859-5 and Unicode.  Quail, used
;; with ucs-tables provides support for translating on the fly to
;; what's appropriate for aa buffer's file coding system, so the
;; encoding shouldn't matter too much provided it supports the
;; necessary characters.

;;; Code:

(require 'quail)

;; This was `cyrillic-jcuken'.  Alexander Mikhailian
;; <mikhailian@altern.org> says:  "cyrillic-jcuken" is actually
;; russian.  It is ok but a bit outdated.  This layout has been used
;; in typewriters for ages but it has been superseded on desktops by
;; a variation of this layout, implemented in M$ Windows software.
;; The Windows layout is greatly preferred because of the comma and
;; period being placed more conveniently and, of course, because of
;; the popularity of Windows software. This layout is a common option
;; in X Windows and console layouts for GNU/Linux.  [See
;; `russian-computer' below.]
(quail-define-package
 "russian-typewriter" "Russian" ",L69(B" nil
 ",L9FC:5=(B Russian typewriter layout (ISO 8859-5 encoding)."
 nil t t t t nil nil nil nil nil t)

;;  ,Lp(B1 -2 /3 "4 :5 ,6 .7 _8 ?9 %0 != ;\ |+
;;   ,L9(B  ,LF(B  ,LC(B  ,L:(B  ,L5(B  ,L=(B  ,L3(B  ,LH(B  ,LI(B  ,L7(B  ,LE(B  ,LJ(B )(
;;    ,LD(B  ,LK(B  ,L2(B  ,L0(B  ,L?(B  ,L@(B  ,L>(B  ,L;(B  ,L4(B  ,L6(B  ,LM(B
;;     ,LO(B  ,LG(B  ,LA(B  ,L<(B  ,L8(B  ,LB(B  ,LL(B  ,L1(B  ,LN(B  ,L!(B

(quail-define-rules
 ("1" ?,Lp(B)
 ("2" ?-)
 ("3" ?/)
 ("4" ?\")
 ("5" ?:)
 ("6" ?,)
 ("7" ?.)
 ("8" ?_)
 ("9" ??)
 ("0" ?%)
 ("-" ?!)
 ("=" ?\;)
 ("`" ?|)
 ("q" ?,LY(B)
 ("w" ?,Lf(B)
 ("e" ?,Lc(B)
 ("r" ?,LZ(B)
 ("t" ?,LU(B)
 ("y" ?,L](B)
 ("u" ?,LS(B)
 ("i" ?,Lh(B)
 ("o" ?,Li(B)
 ("p" ?,LW(B)
 ("[" ?,Le(B)
 ("]" ?,Lj(B)
 ("a" ?,Ld(B)
 ("s" ?,Lk(B)
 ("d" ?,LR(B)
 ("f" ?,LP(B)
 ("g" ?,L_(B)
 ("h" ?,L`(B)
 ("j" ?,L^(B)
 ("k" ?,L[(B)
 ("l" ?,LT(B)
 (";" ?,LV(B)
 ("'" ?,Lm(B)
 ("\\" ?\))
 ("z" ?,Lo(B)
 ("x" ?,Lg(B)
 ("c" ?,La(B)
 ("v" ?,L\(B)
 ("b" ?,LX(B)
 ("n" ?,Lb(B)
 ("m" ?,Ll(B)
 ("," ?,LQ(B)
 ("." ?,Ln(B)
 ("/" ?,Lq(B)

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
 ("_" ?=)
 ("+" ?\\)
 ("~" ?+)
 ("Q" ?,L9(B)
 ("W" ?,LF(B)
 ("E" ?,LC(B)
 ("R" ?,L:(B)
 ("T" ?,L5(B)
 ("Y" ?,L=(B)
 ("U" ?,L3(B)
 ("I" ?,LH(B)
 ("O" ?,LI(B)
 ("P" ?,L7(B)
 ("{" ?,LE(B)
 ("}" ?,LJ(B)
 ("A" ?,LD(B)
 ("S" ?,LK(B)
 ("D" ?,L2(B)
 ("F" ?,L0(B)
 ("G" ?,L?(B)
 ("H" ?,L@(B)
 ("J" ?,L>(B)
 ("K" ?,L;(B)
 ("L" ?,L4(B)
 (":" ?,L6(B)
 ("\"" ?,LM(B)
 ("|" ?\()
 ("Z" ?,LO(B)
 ("X" ?,LG(B)
 ("C" ?,LA(B)
 ("V" ?,L<(B)
 ("B" ?,L8(B)
 ("N" ?,LB(B)
 ("M" ?,LL(B)
 ("<" ?,L1(B)
 (">" ?,LN(B)
 ("?" ?,L!(B)
 )

;; Maintain the obsolete name for now.
(push (cons "cyrillic-jcuken"
	    (cdr (assoc "russian-typewriter" quail-package-alist)))
      quail-package-alist)

;; This needs to be seen by quail-update-leim-list-file, but cannot be
;; commented out because quail-update-leim-list-file ignores
;; commented-out lines.
(if nil
    (quail-define-package
     "cyrillic-jcuken" "Russian" ",L69(B" nil
     ",L9FC:5=(B Russian typewriter layout (ISO 8859-5 encoding)."))

;; See comment above.  This is the variant `winkeys' from `ru' in XKB.
(quail-define-package
 "russian-computer" "Russian" "RU" nil
 ",L9FC:5=(B Russian computer layout"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3,Lp(B 4; 5% 6: 7? 8* 9( 0) -_ =+ \/ ,Lq!(B
;;   ,L9(B  ,LF(B  ,LC(B  ,L:(B  ,L5(B  ,L=(B  ,L3(B  ,LH(B  ,LI(B  ,L7(B  ,LE(B  ,LJ(B
;;    ,LD(B  ,LK(B  ,L2(B  ,L0(B  ,L?(B  ,L@(B  ,L>(B  ,L;(B  ,L4(B  ,L6(B  ,LM(B
;;     ,LO(B  ,LG(B  ,LA(B  ,L<(B  ,L8(B  ,LB(B  ,LL(B  ,L1(B  ,LN(B  .,

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?=)
 ("|" ?/)
 ("`" ?,Lq(B)
 ("q" ?,LY(B)
 ("w" ?,Lf(B)
 ("e" ?,Lc(B)
 ("r" ?,LZ(B)
 ("t" ?,LU(B)
 ("y" ?,L](B)
 ("u" ?,LS(B)
 ("i" ?,Lh(B)
 ("o" ?,Li(B)
 ("p" ?,LW(B)
 ("[" ?,Le(B)
 ("]" ?,Lj(B)
 ("a" ?,Ld(B)
 ("s" ?,Lk(B)
 ("d" ?,LR(B)
 ("f" ?,LP(B)
 ("g" ?,L_(B)
 ("h" ?,L`(B)
 ("j" ?,L^(B)
 ("k" ?,L[(B)
 ("l" ?,LT(B)
 (";" ?,LV(B)
 ("'" ?,Lm(B)
 ("\\" ?\\)
 ("z" ?,Lo(B)
 ("x" ?,Lg(B)
 ("c" ?,La(B)
 ("v" ?,L\(B)
 ("b" ?,LX(B)
 ("n" ?,Lb(B)
 ("m" ?,Ll(B)
 ("," ?,LQ(B)
 ("." ?,Ln(B)
 ("/" ?.)
 ("!" ?!)
 ("@" ?\")
 ("#" ?,Lp(B)
 ("$" ?\;)
 ("%" ?%)
 ("^" ?:)
 ("&" ??)
 ("*" ?*)
 ("(" ?()
 (")" ?))
 ("_" ?_)
 ("+" ?+)
 ("~" ?,L!(B)
 ("Q" ?,L9(B)
 ("W" ?,LF(B)
 ("E" ?,LC(B)
 ("R" ?,L:(B)
 ("T" ?,L5(B)
 ("Y" ?,L=(B)
 ("U" ?,L3(B)
 ("I" ?,LH(B)
 ("O" ?,LI(B)
 ("P" ?,L7(B)
 ("{" ?,LE(B)
 ("}" ?,LJ(B)
 ("A" ?,LD(B)
 ("S" ?,LK(B)
 ("D" ?,L2(B)
 ("F" ?,L0(B)
 ("G" ?,L?(B)
 ("H" ?,L@(B)
 ("J" ?,L>(B)
 ("K" ?,L;(B)
 ("L" ?,L4(B)
 (":" ?,L6(B)
 ("\"" ?,LM(B)
 ("|" ?|)
 ("Z" ?,LO(B)
 ("X" ?,LG(B)
 ("C" ?,LA(B)
 ("V" ?,L<(B)
 ("B" ?,L8(B)
 ("N" ?,LB(B)
 ("M" ?,LL(B)
 ("<" ?,L1(B)
 (">" ?,LN(B)
 ("?" ?,))

;; Mikhailian couldn't check the next two.

;; This seems to have the same layout for letters as mk in XKB, but at
;; least the top row is different.
(quail-define-package
 "cyrillic-macedonian" "Cyrillic" ",L6(BM" nil
 ",L)*5@B7(B-,L#,(B keyboard layout based on JUS.I.K1.004"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;;   ,L)(B  ,L*(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L#(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L,(B  ,L6(B
;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?/)
 ("=" ?+)
 ("`" ?<)
 ("q" ?,Ly(B)
 ("w" ?,Lz(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,LW(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Ls(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,Lx(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?,Lg(B)
 ("'" ?,L|(B)
 ("\\" ?,LV(B)
 ("z" ?,Lu(B)
 ("x" ?,L(B)
 ("c" ?,Lf(B)
 ("v" ?,LR(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?-)

 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?&)
 ("&" ?')
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?*)
 ("~" ?>)
 ("Q" ?,L)(B)
 ("W" ?,L*(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,L7(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,L#(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L((B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?,LG(B)
 ("\"" ?,L,(B)
 ("|" ?,L6(B)
 ("Z" ?,L%(B)
 ("X" ?,L/(B)
 ("C" ?,LF(B)
 ("V" ?,L2(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))

;;

(quail-define-package
 "cyrillic-serbian" "Cyrillic" ",L6(BS" nil
 ",L)*5@B7(B-,L"+(B keyboard layout based on JUS.I.K1.005"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;;   ,L)(B  ,L*(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L"(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L+(B  ,L6(B
;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?/)
 ("=" ?+)
 ("`" ?<)
 ("q" ?,Ly(B)
 ("w" ?,Lz(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,LW(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Lr(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,Lx(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?,Lg(B)
 ("'" ?,L{(B)
 ("\\" ?,LV(B)
 ("z" ?,Lu(B)
 ("x" ?,L(B)
 ("c" ?,Lf(B)
 ("v" ?,LR(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?-)

 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?&)
 ("&" ?')
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?*)
 ("~" ?>)
 ("Q" ?,L)(B)
 ("W" ?,L*(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,L7(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,L"(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L((B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?,LG(B)
 ("\"" ?,L+(B)
 ("|" ?,L6(B)
 ("Z" ?,L%(B)
 ("X" ?,L/(B)
 ("C" ?,LF(B)
 ("V" ?,L2(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))

;;

;; Alexander Mikhailian comments:
;; Having worked for several years as a Belarusian linguist, I still
;; can not find the origin of this layout which BTW does include
;; several characters that are not present in Belarusian and does not
;; include a few ones that do exist in Belarusian.  Besides, the typo
;; in the name of this layout speaks for itself since Belarusian has
;; an outdated version of spelling which is "Byelorussian" and not
;; "beylorussian".  I suggest that you just remove this layout.

;; [`derived from JUS.I.K1' according to an old Mule note -- fx]

;; (quail-define-package
;;  "cyrillic-beylorussian" "Belarussian" ",L6(BB" nil
;;  ",L)*5@B7(B-,L&.(B BEYLORUSSIAN (ISO 8859-5 encoding)"
;;  nil t t t t nil nil nil nil nil t)

;; ;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;; ;;   ,L)(B  ,L*(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L&(B
;; ;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L.(B  ,L6(B
;; ;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

;; (quail-define-rules
;;  ("-" ?/)
;;  ("=" ?+)
;;  ("`" ?<)
;;  ("q" ?,Ly(B)
;;  ("w" ?,Lz(B)
;;  ("e" ?,LU(B)
;;  ("r" ?,L`(B)
;;  ("t" ?,Lb(B)
;;  ("y" ?,LW(B)
;;  ("u" ?,Lc(B)
;;  ("i" ?,LX(B)
;;  ("o" ?,L^(B)
;;  ("p" ?,L_(B)
;;  ("[" ?,Lh(B)
;;  ("]" ?,Lv(B)
;;  ("a" ?,LP(B)
;;  ("s" ?,La(B)
;;  ("d" ?,LT(B)
;;  ("f" ?,Ld(B)
;;  ("g" ?,LS(B)
;;  ("h" ?,Le(B)
;;  ("j" ?,Lx(B)
;;  ("k" ?,LZ(B)
;;  ("l" ?,L[(B)
;;  (";" ?,Lg(B)
;;  ("'" ?,L~(B)
;;  ("\\" ?,LV(B)
;;  ("z" ?,Lu(B)
;;  ("x" ?,L(B)
;;  ("c" ?,Lf(B)
;;  ("v" ?,LR(B)
;;  ("b" ?,LQ(B)
;;  ("n" ?,L](B)
;;  ("m" ?,L\(B)
;;  ("/" ?-)

;;  ("@" ?\")
;;  ("^" ?&)
;;  ("&" ?')
;;  ("*" ?\()
;;  ("(" ?\))
;;  (")" ?=)
;;  ("_" ??)
;;  ("+" ?*)
;;  ("~" ?>)
;;  ("Q" ?,L)(B)
;;  ("W" ?,L*(B)
;;  ("E" ?,L5(B)
;;  ("R" ?,L@(B)
;;  ("T" ?,LB(B)
;;  ("Y" ?,L7(B)
;;  ("U" ?,LC(B)
;;  ("I" ?,L8(B)
;;  ("O" ?,L>(B)
;;  ("P" ?,L?(B)
;;  ("{" ?,LH(B)
;;  ("}" ?,L&(B)
;;  ("A" ?,L0(B)
;;  ("S" ?,LA(B)
;;  ("D" ?,L4(B)
;;  ("F" ?,LD(B)
;;  ("G" ?,L3(B)
;;  ("H" ?,LE(B)
;;  ("J" ?,L((B)
;;  ("K" ?,L:(B)
;;  ("L" ?,L;(B)
;;  (":" ?,LG(B)
;;  ("\"" ?,L.(B)
;;  ("|" ?,L6(B)
;;  ("Z" ?,L%(B)
;;  ("X" ?,L/(B)
;;  ("C" ?,LF(B)
;;  ("V" ?,L2(B)
;;  ("B" ?,L1(B)
;;  ("N" ?,L=(B)
;;  ("M" ?,L<(B)
;;  ("<" ?\;)
;;  (">" ?:)
;;  ("?" ?_))

;;

;; Alexander Mikhailian reports the opinion of fellow Ukrainian
;; linguist Bogdan Babych <babych@altern.org>:
;; He had seen this layout on some oldish systems but that the vast
;; majority of the population uses a modified version of the M$ Windows
;; layout.  In fact, Microsoft shipped for a while a layout that was lacking
;; two characters, precisely the "GHE_WITH_UPTURN" and the apostrophe.  The
;; latest versions of Windows software do have the "GHE_WITH_UPTURN" in the
;; ukrainian keyboard layout but the apostrophe is still not there, whereas
;; there is one letter, "Cyrillic_YO", not used in ukrainian.  Ukrainians
;; normally replace the "Cyrillic_YO" by the apostrophe sign and live
;; happily with this little change.  [See "ukrainian-computer" below.]

;; Fixme: add GHE_WITH_UPTURN.
(quail-define-package
 "cyrillic-ukrainian" "Ukrainian" ",L6(BU" nil
 ",L$'5@B7(B-,L&.(B UKRAINIAN

Sorry, but 'ghe with upturn' is not included in ISO 8859-5."
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3# 4$ 5% 6& 7' 8( 9) 0= /? +* <>
;;   ,L$(B  ,L'(B  ,L5(B  ,L@(B  ,LB(B  ,L7(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,L&(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L((B  ,L:(B  ,L;(B  ,LG(B  ,L.(B  ,L6(B
;;     ,L%(B  ,L/(B  ,LF(B  ,L2(B  ,L1(B  ,L=(B  ,L<(B  ,; .: -_

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?/)
 ("=" ?+)
 ("`" ?<)
 ("q" ?,Lt(B)
 ("w" ?,Lw(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,LW(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Lv(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,Lx(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?,Lg(B)
 ("'" ?,L~(B)
 ("\\" ?,LV(B)
 ("z" ?,Lu(B)
 ("x" ?,L(B)
 ("c" ?,Lf(B)
 ("v" ?,LR(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?-)

 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?&)
 ("&" ?')
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?*)
 ("~" ?>)
 ("Q" ?,L$(B)
 ("W" ?,L'(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,L7(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,L&(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L((B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?,LG(B)
 ("\"" ?,L.(B)
 ("|" ?,L6(B)
 ("Z" ?,L%(B)
 ("X" ?,L/(B)
 ("C" ?,LF(B)
 ("V" ?,L2(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))


(quail-define-package
 "ukrainian-computer" "Ukrainian" "UK" nil
 "$,1(9(F(C(:(5(=(B Ukrainian (Unicode-based for use with KOI8-U encoding)."
 nil t t t t nil nil nil nil nil t)

;;  ' 1! 2" 3$,1uV(B 4; 5% 6: 7? 8* 9( 0) -_ =+
;;   $,1(9(B  $,1(F(B  $,1(C(B  $,1(:(B  $,1(5(B  $,1(=(B  $,1(3(B  $,1(H(B  $,1(I(B  $,1(7(B  $,1(E(B  $,1('(B
;;    $,1(D(B  $,1(&(B  $,1(2(B  $,1(0(B  $,1(?(B  $,1(@(B  $,1(>(B  $,1(;(B  $,1(4(B  $,1(6(B  $,1($(B  $,1)P(B
;;      $,1(O(B  $,1(G(B  $,1(A(B  $,1(<(B  $,1(8(B  $,1(B(B  $,1(L(B  $,1(1(B  $,1(N(B  .,

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?=)
 ("`" ?')
 ("q" ?$,1(Y(B)
 ("w" ?$,1(f(B)
 ("e" ?$,1(c(B)
 ("r" ?$,1(Z(B)
 ("t" ?$,1(U(B)
 ("y" ?$,1(](B)
 ("u" ?$,1(S(B)
 ("i" ?$,1(h(B)
 ("o" ?$,1(i(B)
 ("p" ?$,1(W(B)
 ("[" ?$,1(e(B)
 ("]" ?$,1(w(B)
 ("a" ?$,1(d(B)
 ("s" ?$,1(v(B)
 ("d" ?$,1(R(B)
 ("f" ?$,1(P(B)
 ("g" ?$,1(_(B)
 ("h" ?$,1(`(B)
 ("j" ?$,1(^(B)
 ("k" ?$,1([(B)
 ("l" ?$,1(T(B)
 (";" ?$,1(V(B)
 ("'" ?$,1(t(B)
 ("z" ?$,1(o(B)
 ("x" ?$,1(g(B)
 ("c" ?$,1(a(B)
 ("v" ?$,1(\(B)
 ("b" ?$,1(X(B)
 ("n" ?$,1(b(B)
 ("m" ?$,1(l(B)
 ("," ?$,1(Q(B)
 ("." ?$,1(n(B)
 ("/" ?.)
 ("!" ?!)
 ("@" ?\")
 ("#" ?$,1uV(B)
 ("$" ?\;)
 ("%" ?%)
 ("^" ?:)
 ("&" ??)
 ("*" ?*)
 ("(" ?()
 (")" ?))
 ("_" ?_)
 ("+" ?+)
 ("~" ?')
 ("Q" ?$,1(9(B)
 ("W" ?$,1(F(B)
 ("E" ?$,1(C(B)
 ("R" ?$,1(:(B)
 ("T" ?$,1(5(B)
 ("Y" ?$,1(=(B)
 ("U" ?$,1(3(B)
 ("I" ?$,1(H(B)
 ("O" ?$,1(I(B)
 ("P" ?$,1(7(B)
 ("{" ?$,1(E(B)
 ("}" ?$,1('(B)
 ("A" ?$,1(D(B)
 ("S" ?$,1(&(B)
 ("D" ?$,1(2(B)
 ("F" ?$,1(0(B)
 ("G" ?$,1(?(B)
 ("H" ?$,1(@(B)
 ("J" ?$,1(>(B)
 ("K" ?$,1(;(B)
 ("L" ?$,1(4(B)
 (":" ?$,1(6(B)
 ("\"" ?$,1($(B)
 ("Z" ?$,1(O(B)
 ("X" ?$,1(G(B)
 ("C" ?$,1(A(B)
 ("V" ?$,1(<(B)
 ("B" ?$,1(8(B)
 ("N" ?$,1(B(B)
 ("M" ?$,1(L(B)
 ("<" ?$,1(1(B)
 (">" ?$,1(N(B)
 ("?" ?,)
 ("\\" ?$,1)Q(B)
 ("|" ?$,1)P(B))
;;

;; Alexander Mikhailian says this is of limited use.  It has been
;; popular among emigrants or foreigners who have to type in Cyrillic
;; (mostly Russian) from time to time.
(quail-define-package
 "cyrillic-yawerty" "Cyrillic" ",L6O(B" nil
 ",LO25@BK(B Roman transcription

This layout is based on Roman transcription by phonemic resemblance.
When preceded by a '/', the second and the third rows (number key row) change
as follows.

  keytop | Q  W  E  R  T  Y  U  I  O  P  A  S  D
 --------+---------------------------------------
  input  | ,L"(B  ,L#(B  ,L$(B  ,L%(B  ,L&(B  ,L'(B  ,L((B  ,L)(B  ,L*(B  ,L+(B  ,L,(B  ,L.(B  ,L/(B"
 nil t t t t nil nil nil nil nil t)

;;  1! 2,Lq(B 3,Lj(B 4,L!(B 5% 6^ 7& 8* 9( 0) -_ ,LG(B  ,LN(B
;;   ,LO(B  ,L2(B  ,L5(B  ,L@(B  ,LB(B  ,LK(B  ,LC(B  ,L8(B  ,L>(B  ,L?(B  ,LH(B  ,LI(B
;;    ,L0(B  ,LA(B  ,L4(B  ,LD(B  ,L3(B  ,LE(B  ,L9(B  ,L:(B  ,L;(B  ;: '" ,LM(B
;;     ,L7(B  ,LL(B  ,LF(B  ,L6(B  ,L1(B  ,L=(B  ,L<(B  ,< .> /?

;;  1! 2,Lq(B 3,Lj(B 4,L!(B 5% 6^ 7& 8* 9( 0) -_ ,LG(B  ,LN(B
;;   ,L"(B  ,L#(B  ,L$(B  ,L%(B  ,L&(B  ,L'(B  ,L((B  ,L)(B  ,L*(B  ,L+(B  ,LH(B  ,LI(B
;;    ,L,(B  ,L.(B  ,L/(B  ,LD(B  ,L3(B  ,LE(B  ,L9(B  ,L:(B  ,L;(B  ;: '" ,LM(B
;;     ,L7(B  ,LL(B  ,LF(B  ,L6(B  ,L1(B  ,L=(B  ,L<(B  ,< .> /?

(quail-define-rules
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?,Lg(B)
 ("`" ?,Ln(B)
 ("q" ?,Lo(B)
 ("w" ?,LR(B)
 ("e" ?,LU(B)
 ("r" ?,L`(B)
 ("t" ?,Lb(B)
 ("y" ?,Lk(B)
 ("u" ?,Lc(B)
 ("i" ?,LX(B)
 ("o" ?,L^(B)
 ("p" ?,L_(B)
 ("[" ?,Lh(B)
 ("]" ?,Li(B)
 ("a" ?,LP(B)
 ("s" ?,La(B)
 ("d" ?,LT(B)
 ("f" ?,Ld(B)
 ("g" ?,LS(B)
 ("h" ?,Le(B)
 ("j" ?,LY(B)
 ("k" ?,LZ(B)
 ("l" ?,L[(B)
 (";" ?\;)
 ("'" ?')
 ("\\" ?,Lm(B)
 ("z" ?,LW(B)
 ("x" ?,Ll(B)
 ("c" ?,Lf(B)
 ("v" ?,LV(B)
 ("b" ?,LQ(B)
 ("n" ?,L](B)
 ("m" ?,L\(B)
 ("," ?,)
 ("." ?.)
 ("/" ?/)

 ("!" ?!)
 ("@" ?,Lq(B)
 ("#" ?,Lj(B)
 ("$" ?,L!(B)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?,LG(B)
 ("~" ?,LN(B)
 ("Q" ?,LO(B)
 ("W" ?,L2(B)
 ("E" ?,L5(B)
 ("R" ?,L@(B)
 ("T" ?,LB(B)
 ("Y" ?,LK(B)
 ("U" ?,LC(B)
 ("I" ?,L8(B)
 ("O" ?,L>(B)
 ("P" ?,L?(B)
 ("{" ?,LH(B)
 ("}" ?,LI(B)
 ("A" ?,L0(B)
 ("S" ?,LA(B)
 ("D" ?,L4(B)
 ("F" ?,LD(B)
 ("G" ?,L3(B)
 ("H" ?,LE(B)
 ("J" ?,L9(B)
 ("K" ?,L:(B)
 ("L" ?,L;(B)
 (":" ?:)
 ("\"" ?\")
 ("|" ?,LM(B)
 ("Z" ?,L7(B)
 ("X" ?,LL(B)
 ("C" ?,LF(B)
 ("V" ?,L6(B)
 ("B" ?,L1(B)
 ("N" ?,L=(B)
 ("M" ?,L<(B)
 ("<" ?<)
 (">" ?>)
 ("?" ??)

 ("/q" ?,Lr(B)
 ("/w" ?,Ls(B)
 ("/e" ?,Lt(B)
 ("/r" ?,Lu(B)
 ("/t" ?,Lv(B)
 ("/y" ?,Lw(B)
 ("/u" ?,Lx(B)
 ("/i" ?,Ly(B)
 ("/o" ?,Lz(B)
 ("/p" ?,L{(B)
 ("/a" ?,L|(B)
 ("/s" ?,L~(B)
 ("/d" ?,L(B)

 ("/Q" ?,L"(B)
 ("/W" ?,L#(B)
 ("/E" ?,L$(B)
 ("/R" ?,L%(B)
 ("/T" ?,L&(B)
 ("/Y" ?,L'(B)
 ("/U" ?,L((B)
 ("/I" ?,L)(B)
 ("/O" ?,L*(B)
 ("/P" ?,L+(B)
 ("/A" ?,L,(B)
 ("/S" ?,L.(B)
 ("/D" ?,L/(B))

;; This was provided by Valery Alexeev <valery@domovoy.math.uga.edu>.

;; Ognyan Kulev <ogi@fmi.uni-sofia.bg> wrote:

;; I would suggest future `cyrillic-translit' to be with the
;; modification of `cyrillic-translit-bulgarian' applied and the
;; latter to disappear.  It could be used by people who write
;; bulgarian e-mails with latin letters for kick start (phonetic input
;; method is not so obvious as translit input method but each letter
;; is one keypress and a *lot* of people know it).

;; Anton Zinoviev <anton@lml.bas.bg> wrote:
;; I would say that the main idea for cyrillic-translit is to be
;; language-independent and universal.  It should be able to generate all
;; Cyrillic symbols.
(quail-define-package
 "cyrillic-translit" "Cyrillic" ",L6(Bt" t
 "Intuitively transliterated keyboard layout.
Most convenient for entering Russian, but all Cyrillic characters
are included.  Should handle most cases.  However:
  for ,Lf(B (TSE) use \"c\", never \"ts\"
  ,Li(B (SHCHA = Bulgarian SHT) = \"shch\", \"sj\", \"/sht\" or \"/t\",
  ,Lm(B (REVERSE ROUNDED E) = \"e\\\"
  ,Le(B (KHA) when after ,La(B (S) = \"x\" or \"kh\"
  ,Lj(B (HARD SIGN) = \"~\", ,LJ(B (CAPITAL HARD SIGN) = \"~~\",
  ,Ll(B (SOFT SIGN) = \"'\", ,LL(B (CAPITAL SOFT SIGN) = \"''\",
  ,Lo(B (YA) = \"ya\", \"ja\" or \"q\".

Russian alphabet: a b v=w g d e yo=jo zh z i j=j' k l m n o p r s t
u f h=kh=x c ch sh shch=sj=/s=/sht ~ y ' e\\ yu=ju ya=ja=q

Also included are Ukrainian ,Lt(B (YE) = \"/e\", ,Lw(B (YI) = \"yi\",
$,1)Q(B (GHE WITH UPTURN) = \"g'\",
Belarusian ,L~(B (SHORT U) = \"u~\",
Serbo-Croatian ,Lr(B (DJE) = \"/d\", ,L{(B (CHJE)= \"/ch\",
Macedonian ,Ls(B (GJE) = \"/g\", ,Lu(B (DZE) = \"/s\", ,L|(B (KJE) = \"/k\",
cyrillic ,Lv(B (I DECIMAL) = \"/i\", ,Lx(B (JE) = \"/j\",
,Ly(B (LJE) = \"/l\", ,Lz(B (NJE) = \"/n\" and ,L(B (DZE) =\"/z\"."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?,LP(B) ("b" ?,LQ(B) ("v" ?,LR(B) ("w" ?,LR(B) ("g" ?,LS(B) ("d" ?,LT(B)
 ("e" ?,LU(B) ("je" ?,LU(B)
 ("yo" ?,Lq(B) ("jo" ?,Lq(B)
 ("zh" ?,LV(B) ("z" ?,LW(B) ("i" ?,LX(B)
 ("j" ?,LY(B) ("j'" ?,LY(B) ("j`" ?,LY(B) ("k" ?,LZ(B) ("l" ?,L[(B)
 ("m" ?,L\(B) ("n" ?,L](B) ("o" ?,L^(B) ("p" ?,L_(B) ("r" ?,L`(B) ("s" ?,La(B) ("t" ?,Lb(B) ("u" ?,Lc(B)
 ("f" ?,Ld(B) ("x" ?,Le(B) ("h" ?,Le(B) ("kh" ?,Le(B)
 ("c" ?,Lf(B) ("ch" ?,Lg(B)
 ("sh" ?,Lh(B)
 ("shch" ?,Li(B) ("sj" ?,Li(B)
 ("/sht" ?,Li(B) ("/t" ?,Li(B)
 ("~" ?,Lj(B) ("y" ?,Lk(B) ("'" ?,Ll(B) ("`" ?,Ll(B)
 ("e\\" ?,Lm(B) ("e'" ?,Lm(B) ("e`" ?,Lm(B) ("@" ?,Lm(B)
 ("yu" ?,Ln(B) ("ju" ?,Ln(B)
 ("ya" ?,Lo(B) ("ja" ?,Lo(B) ("q" ?,Lo(B)

 ("A" ?,L0(B) ("B" ?,L1(B) ("V" ?,L2(B) ("W" ?,L2(B) ("G" ?,L3(B) ("D" ?,L4(B)
 ("E" ?,L5(B) ("Je" ?,L5(B) ("JE" ?,L5(B)
 ("Yo" ?,L!(B) ("YO" ?,L!(B) ("Jo" ?,L!(B) ("JO" ?,L!(B)
 ("Zh" ?,L6(B) ("ZH" ?,L6(B) ("Z" ?,L7(B) ("I" ?,L8(B)
 ("J" ?,L9(B) ("J'" ?,L9(B) ("J`" ?,L9(B) ("K" ?,L:(B) ("L" ?,L;(B)
 ("M" ?,L<(B) ("N" ?,L=(B) ("O" ?,L>(B) ("P" ?,L?(B) ("R" ?,L@(B) ("S" ?,LA(B) ("T" ?,LB(B) ("U" ?,LC(B)
 ("F" ?,LD(B) ("X" ?,LE(B) ("H" ?,LE(B) ("Kh" ?,LE(B) ("KH" ?,LE(B)
 ("C" ?,LF(B) ("Ch" ?,LG(B) ("CH" ?,LG(B)
 ("Sh" ?,LH(B) ("SH" ?,LH(B)
 ("Shch" ?,LI(B) ("SHCH" ?,LI(B) ("Sj" ?,LI(B) ("SJ" ?,LI(B)
 ("/Sht" ?,LI(B) ("/SHT" ?,LI(B) ("/T" ?,LI(B)
 ("~~" ?,LJ(B) ("Y" ?,LK(B) ("''" ?,LL(B)
 ("E\\" ?,LM(B) ("E'" ?,LM(B) ("E`" ?,LM(B) ("@@" ?,LM(B)
 ("Yu" ?,LN(B) ("YU" ?,LN(B) ("Ju" ?,LN(B) ("JU" ?,LN(B)
 ("Ya" ?,LO(B) ("YA" ?,LO(B) ("Ja" ?,LO(B) ("JA" ?,LO(B) ("Q" ?,LO(B)

 ("/e" ?,Lt(B) ("yi" ?,Lw(B) ("u'" ?,L~(B) ("u~" ?,L~(B)
 ("g'" ?$,1)Q(B)
 ("/d" ?,Lr(B) ("/ch" ?,L{(B)
 ("/g" ?,Ls(B) ("/s" ?,Lu(B) ("/k" ?,L|(B)
 ("/i" ?,Lv(B) ("/j" ?,Lx(B) ("/l" ?,Ly(B) ("/n" ?,Lz(B) ("/z" ?,L(B)
 ("/E" ?,L$(B) ("YE" ?,L$(B) ("Yi" ?,L'(B) ("YI" ?,L'(B) ("U'" ?,L.(B) ("U~" ?,L.(B)
 ("G'" ?$,1)P(B)
 ("/D" ?,L"(B) ("/Ch" ?,L+(B) ("/CH" ?,L+(B)
 ("/G" ?,L#(B) ("/S" ?,L%(B) ("/K" ?,L,(B)
 ("/I" ?,L&(B) ("/J" ?,L((B) ("/L" ?,L)(B) ("/N" ?,L*(B) ("/Z" ?,L/(B)

 ;; Combining accents as a separate character
 ("//'" ?$(O+Z(B) ("//`" ?$(O+\(B)

 ;; In the following two rules the accent is not a separate character
 ("i`" ?$,1(}(B) ("I`" ?$,1(-(B)

 ("/-"  ?$(G!9(B)  ;; EN DASH
 ("/--" ?$(G!7(B)  ;; EM DASH
 ("/*" ?$(O#@(B)   ;; BULLET
 ("/." ?$,1s$(B)   ;; ONE DOT LEADER
 ("/.." ?$(G!-(B)  ;; TWO DOT LEADER
 ("/..." ?$A!-(B) ;; HORIZONTAL ELLIPSIS
 ("/,," ?,Y%(B)  ;; DOUBLE LOW-9 QUOTATION MARK
 ("/," ?$,1rz(B)   ;; SINGLE LOW-9 QUOTATION MARK
 ("/''" ?,Y!(B)  ;; RIGHT DOUBLE QUOTATION MARK
 ("/'" ?,F"(B)   ;; RIGHT SINGLE QUOTATION MARK
 ("/``" ?,Y4(B)  ;; LEFT DOUBLE QUOTATION MARK
 ("/`" ?,F!(B)   ;; LEFT SINGLE QUOTATION MARK
 ("/<<" ?,A+(B)  ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
 ("/>>" ?,A;(B)  ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK

 ("/&" ?,A'(B)
 ("/ab" ?,A'(B)                             ; _,LPQ(B_,LWPf(B
 ("/pa" ?,A'(B)                             ; _pa_ragraph
 ("/#" ?,Lp(B)
 ("/no" ?,Lp(B)                             ; _,L]^(B_,L\U`(B

 ("/c" ?,A)(B)
 ("/tm" ?$(D"o(B)
 ("/reg" ?,A.(B)
 ("/eu"  ?,b$(B)
 ("/ce"  ?,A"(B)

 ;; fractions
 ("/78" ?$(C(~(B)
 ("/58" ?$(C(}(B)
 ("/38" ?$(C(|(B)
 ("/18" ?$(C({(B)
 ("/56" ?$,1v:(B)
 ("/16" ?$,1v9(B)
 ("/45" ?$,1v8(B)
 ("/35" ?$,1v7(B)
 ("/25" ?$,1v6(B)
 ("/15" ?$(O'z(B)
 ("/23" ?$(O'y(B)
 ("/13" ?$(O'x(B)
 ("/34" ?,A>(B)
 ("/12" ?,A=(B)
 ("/14" ?,A<(B)

 ;; Roman numerals, commonly used for months and section/subsection numbers
 ("/RI" ?$A"q(B)
 ("/RII" ?$A"r(B)
 ("/RIII" ?$A"s(B)
 ("/RIV" ?$A"t(B)
 ("/RV" ?$A"u(B)
 ("/RVI" ?$A"v(B)
 ("/RVII" ?$A"w(B)
 ("/RVIII" ?$A"x(B)
 ("/RIX" ?$A"y(B)
 ("/RX" ?$A"z(B)
 ("/RXI" ?$A"{(B)
 ("/RXII" ?$A"|(B)

 ("/ri" ?$(G&5(B)
 ("/rii" ?$(G&6(B)
 ("/riii" ?$(G&7(B)
 ("/riv" ?$(G&8(B)
 ("/rv" ?$(G&9(B)
 ("/rvi" ?$(G&:(B)
 ("/rvii" ?$(G&;(B)
 ("/rviii" ?$(G&<(B)
 ("/rix" ?$(G&=(B)
 ("/rx" ?$(G&>(B)
 ("/rxi" ?$(O,?(B)
 ("/rxii" ?$(O,@(B)
)

;; Originally from Yudit's `Belarusian input table according to
;; STB955-94 belarusian standard' (not all) by Alexander Mikhailian
;; <mikhailian@altern.org>, subsequently amended by AM.
(quail-define-package
 "belarusian" "Belarusian" "BE" nil
 "$,1(9(F(C(:(5(=(B keyboard layout registered as STB955-94 Belarusian standard.
Unicode based."
 nil t t t t nil nil nil nil nil t)

;; $,1(q(!(B 1! 2" 3N 4; 5% 6: 7? 8* 9( 0) -_ =+
;;     $,1(9(B  $,1(F(B  $,1(C(B  $,1(:(B  $,1(5(B  $,1(=(B  $,1(3(B  $,1(H(B  $,1(.(B  $,1(7(B  $,1(E(B  '
;;      $,1(D(B  $,1(K(B  $,1(2(B  $,1(0(B  $,1(?(B  $,1(@(B  $,1(>(B  $,1(;(B  $,1(4(B  $,1(6(B  $,1(M(B
;;       $,1(O(B  $,1(G(B  $,1(A(B  $,1(<(B  $,1(&(B  $,1(B(B  $,1(L(B  $,1(1(B  $,1(N(B  .,

(quail-define-rules
 ("~" ?$,1(!(B)
 ("@" ?\")
 ("#" ?$,1uV(B)
 ("$" ?\;)
 ("%" ?%)
 ("^" ?:)
 ("&" ??)
 ("Q" ?$,1(9(B)
 ("W" ?$,1(F(B)
 ("E" ?$,1(C(B)
 ("R" ?$,1(:(B)
 ("T" ?$,1(5(B)
 ("Y" ?$,1(=(B)
 ("U" ?$,1(3(B)
 ("I" ?$,1(H(B)
 ("O" ?$,1(.(B)
 ("P" ?$,1(7(B)
 ("{" ?$,1(E(B)
 ("}" ?')
 ("A" ?$,1(D(B)
 ("S" ?$,1(K(B)
 ("D" ?$,1(2(B)
 ("F" ?$,1(0(B)
 ("G" ?$,1(?(B)
 ("H" ?$,1(@(B)
 ("J" ?$,1(>(B)
 ("K" ?$,1(;(B)
 ("L" ?$,1(4(B)
 (":" ?$,1(6(B)
 ("\"" ?$,1(M(B)
 ("|" ?|)
 ("Z" ?$,1(O(B)
 ("X" ?$,1(G(B)
 ("C" ?$,1(A(B)
 ("V" ?$,1(<(B)
 ("B" ?$,1(&(B)
 ("N" ?$,1(B(B)
 ("M" ?$,1(L(B)
 ("<" ?$,1(1(B)
 (">" ?$,1(N(B)
 ("?" ?,)

 ("`" ?$,1(q(B)
 ("q" ?$,1(Y(B)
 ("w" ?$,1(f(B)
 ("e" ?$,1(c(B)
 ("r" ?$,1(Z(B)
 ("t" ?$,1(U(B)
 ("y" ?$,1(](B)
 ("u" ?$,1(S(B)
 ("i" ?$,1(h(B)
 ("o" ?$,1(~(B)
 ("p" ?$,1(W(B)
 ("[" ?$,1(e(B)
 ("]" ?')
 ("a" ?$,1(d(B)
 ("s" ?$,1(k(B)
 ("d" ?$,1(R(B)
 ("f" ?$,1(P(B)
 ("g" ?$,1(_(B)
 ("h" ?$,1(`(B)
 ("j" ?$,1(^(B)
 ("k" ?$,1([(B)
 ("l" ?$,1(T(B)
 (";" ?$,1(V(B)
 ("'" ?$,1(m(B)
 ("z" ?$,1(o(B)
 ("x" ?$,1(g(B)
 ("c" ?$,1(a(B)
 ("v" ?$,1(\(B)
 ("b" ?$,1(v(B)
 ("n" ?$,1(b(B)
 ("m" ?$,1(l(B)
 ("," ?$,1(Q(B)
 ("." ?$,1(n(B)
 ("/" ?.))

(quail-define-package
 "bulgarian-alt-phonetic" "Bulgarian" "$,1(1(=(D(B"
 nil
 "Bulgarian alternative Phonetic keyboard layout, producing Unicode.

This phonetic layout replaces all the Latin letters with Bulgarian
\(Cyrillic\) letters based on similarities in their pronunciation or look.

Note that, since the letters ',Li(B', ',Ll(B', ',Ln(B' and ',Lo(B' are attached to the
']', '\', '`' and '[' keys respectively, Caps Lock does not affect them."
nil t t t t nil nil nil nil nil t)

;;  $,1(N(B  1! 2@ 3$,1uV(B 4$ 5% 6$,1tL(B 7,A'(B 8* 9( 0) -$,1rs(B =+ $,1(l(}(B
;;      $,1(G(B  $,1(H(B  $,1(5(B  $,1(@(B  $,1(B(B  $,1(J(B  $,1(C(B  $,1(8(B  $,1(>(B  $,1(?(B  $,1(O(B  $,1(I(B
;;       ,L0(B  $,1(A(B  $,1(4(B  $,1(D(B  $,1(3(B  $,1(E(B  $,1(9(B  $,1(:(B  $,1(;(B  :; '"
;;        $,1(7(B  $,1(6(B  $,1(F(B  $,1(2(B  $,1(1(B  $,1(=(B  $,1(<(B  ,$,1r~(B .$,1r|(B /?

(quail-define-rules
 ("#" ?,Lp(B)
 ("&" ?,A'(B)
 ("/#" ?#)
 ("/&" ?&)
 ("/<" ?<)
 ("/>" ?>)
 ("/[" ?\[)
 ("/\\" ?\\)
 ("/]" ?\])
 ("/^" ?^)
 ("/_" ?_)
 ("/`" ?`)
 ("/{" ?{)
 ("/|" ?|)
 ("/}" ?})
 ("/~" ?~)
 ("<" ?$,1r~(B)
 (">" ?$,1r|(B)
 ("A" ?$,1(0(B) ("a" ?$,1(P(B)
 ("B" ?$,1(1(B) ("b" ?$,1(Q(B)
 ("C" ?$,1(F(B) ("c" ?$,1(f(B)
 ("D" ?$,1(4(B) ("d" ?$,1(T(B)
 ("E" ?$,1(5(B) ("e" ?$,1(U(B)
 ("F" ?$,1(D(B) ("f" ?$,1(d(B)
 ("G" ?$,1(3(B) ("g" ?$,1(S(B)
 ("H" ?$,1(E(B) ("h" ?$,1(e(B)
 ("I" ?$,1(8(B) ("i" ?$,1(X(B)
 ("J" ?$,1(9(B) ("j" ?$,1(Y(B)
 ("K" ?$,1(:(B) ("k" ?$,1(Z(B)
 ("L" ?$,1(;(B) ("l" ?$,1([(B)
 ("M" ?$,1(<(B) ("m" ?$,1(\(B)
 ("N" ?$,1(=(B) ("n" ?$,1(](B)
 ("O" ?$,1(>(B) ("o" ?$,1(^(B)
 ("P" ?$,1(?(B) ("p" ?$,1(_(B)
 ("Q" ?$,1(G(B) ("q" ?$,1(g(B)
 ("R" ?$,1(@(B) ("r" ?$,1(`(B)
 ("S" ?$,1(A(B) ("s" ?$,1(a(B)
 ("T" ?$,1(B(B) ("t" ?$,1(b(B)
 ("U" ?$,1(C(B) ("u" ?$,1(c(B)
 ("V" ?$,1(2(B) ("v" ?$,1(R(B)
 ("W" ?$,1(H(B) ("w" ?$,1(h(B)
 ("X" ?$,1(6(B) ("x" ?$,1(V(B)
 ("Y" ?$,1(J(B) ("y" ?$,1(j(B)
 ("Z" ?$,1(7(B) ("z" ?$,1(W(B)
 ("[" ?$,1(o(B)
 ("\\" ?$,1(l(B)
 ("]" ?$,1(i(B)
 ("^" ?$,1tL(B)
 ("_" ?$,1rs(B)
 ("`" ?$,1(n(B)
 ("{" ?$,1(O(B)
 ("|" ?$,1(}(B)
 ("}" ?$,1(I(B)
 ("~" ?$,1(N(B))

;; From `Bulgarian-PHO.kmap for Yudit', Alexander Shopov
;; <al_shopov@web.bg>.

;; Extra commentary and the indicator from an independent
;; (cyrillic-iso8859-5) implementation by Ognyan Kulev
;; <ogi@fmi.uni-sofia.bg> and name changes from Anton Zinoviev
;; <anton@lml.bas.bg>.
(quail-define-package
 "bulgarian-phonetic" "Bulgarian" "$,1(6(1(D(B"
 nil
 "Bulgarian Phonetic keyboard layout, producing Unicode.

The layout is similar to `cyrillic-translit', but all Bulgarian
characters are typed with a single key.

Use /& for ,A'(B (Cyrillic paragraph) and /# for $,1uV(B.

The letters $,1(G(B, $,1(H(B, $,1(I(B and $,1(N(B are not affected by Caps Lock."
 nil t t t t nil nil nil nil nil t)

;;  $,1(G(B
;;      $,1(O(B  $,1(2(B  $,1(5(B  $,1(@(B  $,1(B(B  $,1(J(B  $,1(C(B  $,1(8(B  $,1(>(B  $,1(?(B  $,1(H(B  $,1(I(B
;;       $,1(0(B  $,1(A(B  $,1(4(B  $,1(D(B  $,1(3(B  $,1(E(B  $,1(9(B  $,1(:(B  $,1(;(B        $,1(N(B
;;        $,1(7(B  $,1(L(B  $,1(F(B  $,1(6(B  $,1(1(B  $,1(=(B  $,1(<(B

(quail-define-rules
 ("/&" ?,A'(B)
 ("/#" ?$,1uV(B)
 ("A" ?$,1(0(B)
 ("B" ?$,1(1(B)
 ("W" ?$,1(2(B)
 ("G" ?$,1(3(B)
 ("D" ?$,1(4(B)
 ("E" ?$,1(5(B)
 ("V" ?$,1(6(B)
 ("Z" ?$,1(7(B)
 ("I" ?$,1(8(B)
 ("J" ?$,1(9(B)
 ("K" ?$,1(:(B)
 ("L" ?$,1(;(B)
 ("M" ?$,1(<(B)
 ("N" ?$,1(=(B)
 ("O" ?$,1(>(B)
 ("P" ?$,1(?(B)
 ("R" ?$,1(@(B)
 ("S" ?$,1(A(B)
 ("T" ?$,1(B(B)
 ("U" ?$,1(C(B)
 ("F" ?$,1(D(B)
 ("H" ?$,1(E(B)
 ("C" ?$,1(F(B)
 ("~" ?$,1(G(B)
 ("{" ?$,1(H(B)
 ("}" ?$,1(I(B)
 ("Y" ?$,1(J(B)
 ("X" ?$,1(L(B)
 ("|" ?$,1(N(B)
 ("Q" ?$,1(O(B)
 ("a" ?$,1(P(B)
 ("b" ?$,1(Q(B)
 ("w" ?$,1(R(B)
 ("g" ?$,1(S(B)
 ("d" ?$,1(T(B)
 ("e" ?$,1(U(B)
 ("v" ?$,1(V(B)
 ("z" ?$,1(W(B)
 ("i" ?$,1(X(B)
 ("j" ?$,1(Y(B)
 ("k" ?$,1(Z(B)
 ("l" ?$,1([(B)
 ("m" ?$,1(\(B)
 ("n" ?$,1(](B)
 ("o" ?$,1(^(B)
 ("p" ?$,1(_(B)
 ("r" ?$,1(`(B)
 ("s" ?$,1(a(B)
 ("t" ?$,1(b(B)
 ("u" ?$,1(c(B)
 ("f" ?$,1(d(B)
 ("h" ?$,1(e(B)
 ("c" ?$,1(f(B)
 ("`" ?$,1(g(B)
 ("[" ?$,1(h(B)
 ("]" ?$,1(i(B)
 ("y" ?$,1(j(B)
 ("x" ?$,1(l(B)
 ("\\" ?$,1(n(B)
 ("q" ?$,1(o(B))

;; Based on an implementation by Ognyan Kulev <ogi@fmi.uni-sofia.bg>.
;; This follows XKB bg.

(quail-define-package
 "bulgarian-bds" "Bulgarian" "$,1(1(4(A(B" nil
 "Bulgarian standard keyboard layout (BDS)

This keyboard layout is standard for Bulgarian typewriters.

The letters $,1(F(B, $,1(<(B, $,1(G(B, $,1(@(B, $,1(;(B, $,1(1(B and $,1(K(B are not affected by Caps Lock.

In addition to original Bulgarian typewriter layout, keys \\ and |
are transformed into ' and $,1(K(B respectively.  Some keyboards mark these
keys as being transformed into ( and ) respectively.  For ( and ), use
` and ~ respectively.  This input method follows XKB."
 nil t t t t nil nil nil nil nil t)

;;  () 1! 2? 3+ 4" 5% 6= 7: 8/ 9_ 0$,1uV(B -I .V
;;      ,$,1(k(B $,1(C(B  $,1(5(B  $,1(8(B  $,1(H(B  $,1(I(B  $,1(:(B  $,1(A(B  $,1(4(B  $,1(7(B  $,1(F(B  ;,A'(B
;;       $,1(L(B  $,1(O(B  $,1(0(B  $,1(>(B  $,1(6(B  $,1(3(B  $,1(B(B  $,1(=(B  $,1(2(B  $,1(<(B  $,1(G(B  '$,1(K(B
;;        $,1(N(B  $,1(9(B  $,1(J(B  $,1(M(B  $,1(D(B  $,1(E(B  $,1(?(B  $,1(@(B  $,1(;(B  $,1(1(B

(quail-define-rules

 ("1" ?1) ("!" ?!)
 ("2" ?2) ("@" ??)
 ("3" ?3) ("#" ?+)
 ("4" ?4) ("$" ?\")
 ("5" ?5) ("%" ?%)
 ("6" ?6) ("^" ?=)
 ("7" ?7) ("&" ?:)
 ("8" ?8) ("*" ?/)
 ("9" ?9) ("(" ?_)
 ("0" ?0) (")" ?$,1uV(B)
 ("-" ?-) ("_" ?I)
 ("=" ?.) ("+" ?V)

 ("q" ?,) ("Q" ?$,1(k(B)
 ("w" ?$,1(c(B) ("W" ?$,1(C(B)
 ("e" ?$,1(U(B) ("E" ?$,1(5(B)
 ("r" ?$,1(X(B) ("R" ?$,1(8(B)
 ("t" ?$,1(h(B) ("T" ?$,1(H(B)
 ("y" ?$,1(i(B) ("Y" ?$,1(I(B)
 ("u" ?$,1(Z(B) ("U" ?$,1(:(B)
 ("i" ?$,1(a(B) ("I" ?$,1(A(B)
 ("o" ?$,1(T(B) ("O" ?$,1(4(B)
 ("p" ?$,1(W(B) ("P" ?$,1(7(B)
 ("[" ?$,1(f(B) ("{" ?$,1(F(B)
 ("]" ?\;) ("}" ?,A'(B)

 ("a" ?$,1(l(B) ("A" ?$,1(L(B)
 ("s" ?$,1(o(B) ("S" ?$,1(O(B)
 ("d" ?$,1(P(B) ("D" ?$,1(0(B)
 ("f" ?$,1(^(B) ("F" ?$,1(>(B)
 ("g" ?$,1(V(B) ("G" ?$,1(6(B)
 ("h" ?$,1(S(B) ("H" ?$,1(3(B)
 ("j" ?$,1(b(B) ("J" ?$,1(B(B)
 ("k" ?$,1(](B) ("K" ?$,1(=(B)
 ("l" ?$,1(R(B) ("L" ?$,1(2(B)
 (";" ?$,1(\(B) (":" ?$,1(<(B)
 ("'" ?$,1(g(B) ("\"" ?$,1(G(B)
 ("`" ?\() ("~" ?\))

 ("z" ?$,1(n(B) ("Z" ?$,1(N(B)
 ("x" ?$,1(Y(B) ("X" ?$,1(9(B)
 ("c" ?$,1(j(B) ("C" ?$,1(J(B)
 ("v" ?$,1(m(B) ("V" ?$,1(M(B)
 ("b" ?$,1(d(B) ("B" ?$,1(D(B)
 ("n" ?$,1(e(B) ("N" ?$,1(E(B)
 ("m" ?$,1(_(B) ("M" ?$,1(?(B)
 ("," ?$,1(`(B) ("<" ?$,1(@(B)
 ("." ?$,1([(B) (">" ?$,1(;(B)
 ("/" ?$,1(Q(B) ("?" ?$,1(1(B)
 ("\\" ?') ("|" ?$,1(K(B))

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyrillic.el ends here
