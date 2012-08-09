;; hebrew.el --- Quail package for inputting Hebrew characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Many input methods in this file provided
;; by Yair Friedman <yair.f.lists@gmail.com>

;; Keywords: multilingual, input method, Hebrew

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

(quail-define-package
 "hebrew" "Hebrew" ",Hr(B" nil "Hebrew SI-1452 input method.

Based on SI-1452 keyboard layout.
Only Hebrew-related characters are considered.
 'q' is used to switch levels instead of Alt-Gr.
 Maqaaf ($,1,^(B) is mapped to '/,Ht(B'.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?\;)
 ("w" ?\')
 ("e" ?,Hw(B)  ; Qof
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,H`(B)  ; Alef
 ("y" ?,Hh(B)  ; Tet
 ("u" ?,He(B)  ; Vav
 ("i" ?,Ho(B)  ; Final Nun
 ("o" ?,Hm(B)  ; Final Mem
 ("p" ?,Ht(B)  ; Pe
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("a" ?,Hy(B)  ; Shin
 ("s" ?,Hc(B)  ; Dalet
 ("d" ?,Hb(B)  ; Gimel
 ("f" ?,Hk(B)  ; Kaf
 ("g" ?,Hr(B)  ; Ayin
 ("h" ?,Hi(B)  ; Yod
 ("j" ?,Hg(B)  ; Het
 ("k" ?,Hl(B)  ; Lamed
 ("l" ?,Hj(B)  ; Final Kaf
 (";" ?,Hs(B)  ; Final Pe
 ("'" ?,)
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hq(B)  ; Samekh
 ("c" ?,Ha(B)  ; Bet
 ("v" ?,Hd(B)  ; He
 ("b" ?,Hp(B)  ; Nun
 ("n" ?,Hn(B)  ; Mem
 ("m" ?,Hv(B)  ; Tsadi
 ("," ?,Hz(B)  ; Tav
 ("." ?,Hu(B)  ; Final Tsadi
 ("/" ?.)  ; Stop
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring
 ("q`" ?$,1,P(B)  ; Sheva
 ("q1" ?$,1,Q(B)  ; Hataf Segol
 ("q2" ?$,1,R(B)  ; Hataf Patah
 ("q3" ?$,1,S(B)  ; Hataf Qamats
 ("q4" ?$,1,T(B)  ; Hiriq
 ("q5" ?$,1,U(B)  ; Tsere
 ("q6" ?$,1,V(B)  ; Segol (Point)
 ("q7" ?$,1,W(B)  ; Patah
 ("q8" ?$,1,X(B)  ; Qamats
 ("q9" ?$,1,b(B)  ; Sin dot
 ("q0" ?$,1,a(B)  ; Shin dot
 ("q-" ?$,1,Y(B)  ; Holam
 ("q=" ?$,1,\(B)  ; Dagesh or Mapiq
 ("q\\" ?$,1,[(B)  ; Qubuts
 ("qq" ?/)
 ("qw" ?$,1-3(B)  ; Geresh (Punct.)
 ("qi" ?$,1-0(B)  ; Yiddish Double Vav
 ("qp" ?$,1,^(B)  ; Maqaf
 ("q[" ?$,1,_(B)  ; Rafe
 ("q]" ?$,1,](B)  ; Meteg
 ("qa" ?$,1tJ(B)  ; New Sheqel sign
 ("qh" ?$,1-2(B)  ; Yiddish Double Yod
 ("qj" ?$,1-1(B)  ; Yiddish Vav Yod
 ("q\"" ?$,1-4(B)  ; Gershayim (Punct.)
 ("q," ?\u200E)  ;  LRM
 ("q." ?\u200F)  ;  RLM
)

(quail-define-package
 "hebrew-new" "Hebrew" ",Hr(B" nil "Hebrew SI-1452 new draft input method.

Based on latest draft of SI-1452 keyboard layout.
Only Hebrew-related characters are considered.
 '`' is used to switch levels instead of Alt-Gr.
Geresh is mapped to '`k'.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q" ?/)
 ("w" ?\')
 ("e" ?,Hw(B)  ; Qof
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,H`(B)  ; Alef
 ("y" ?,Hh(B)  ; Tet
 ("u" ?,He(B)  ; Vav
 ("i" ?,Ho(B)  ; Final Nun
 ("o" ?,Hm(B)  ; Final Mem
 ("p" ?,Ht(B)  ; Pe
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("a" ?,Hy(B)  ; Shin
 ("s" ?,Hc(B)  ; Dalet
 ("d" ?,Hb(B)  ; Gimel
 ("f" ?,Hk(B)  ; Kaf
 ("g" ?,Hr(B)  ; Ayin
 ("h" ?,Hi(B)  ; Yod
 ("j" ?,Hg(B)  ; Het
 ("k" ?,Hl(B)  ; Lamed
 ("l" ?,Hj(B)  ; Final Kaf
 (";" ?,Hs(B)  ; Final Pe
 ("'" ?,)
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hq(B)  ; Samekh
 ("c" ?,Ha(B)  ; Bet
 ("v" ?,Hd(B)  ; He
 ("b" ?,Hp(B)  ; Nun
 ("n" ?,Hn(B)  ; Mem
 ("m" ?,Hv(B)  ; Tsadi
 ("," ?,Hz(B)  ; Tav
 ("." ?,Hu(B)  ; Final Tsadi
 ("/" ?.)  ; Stop
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring

 ("``" ?\;)
 ("`1" ?$,1,](B)  ; Meteg
;("`2" ??)  ; Unassigned
 ("`3" ?,F$(B)  ; Euro Sign
 ("`4" ?$,1tJ(B)  ; New Sheqel sign
 ("`5" ?,A0(B)  ; Degree Sign
 ("`6" ?$,1,K(B)  ; Ole
;("`7" ??)  ; Unassigned
 ("`8" ?,AW(B)  ; Multiplication Sign
 ("`9" ?\u200E)  ; LRM
 ("`0" ?\u200F)  ; RLM
 ("`-" ?$,1,^(B)  ; Maqaf
 ("`=" ?$(G!9(B)  ; En Dash
 ("`q" ?$,1,b(B)  ; Sin dot
 ("`w" ?$,1,a(B)  ; Shin dot
 ("`e" ?$,1,X(B)  ; Qamats
 ("`r" ?$,1,S(B)  ; Hataf Qamats
;("`t" ??)  ; Unassigned
 ("`y" ?$,1-0(B)  ; Yiddish Double Vav
 ("`u" ?$,1,Y(B)  ; Holam
;("`i" ??)  ; Unassigned
;("`o" ??)  ; Unassigned
 ("`p" ?$,1,W(B)  ; Patah
 ("`[" ?$,1,R(B)  ; Hataf Patah
 ("`]" ?$,1,_(B)  ; Rafe
 ("`\\" ?$,1,[(B)  ; Qubuts
 ("`a" ?$,1,P(B)  ; Sheva
 ("`s" ?$,1,\(B)  ; Dagesh or Mapiq
;("`d" ??)  ; Unassigned
;("`f" ??)  ; Unassigned
 ("`g" ?$,1-1(B)  ; Yiddish Vav Yod
 ("`h" ?$,1-2(B)  ; Yiddish Double Yod
 ("`j" ?$,1,T(B)  ; Hiriq
 ("`k" ?$,1-3(B)  ; Geresh (Punct.)
 ("`l" ?,Y4(B)  ; Left Double Quotation Mark
 ("`;" ?,Y!(B)  ; Right Double Quotation Mark
 ("`'" ?$,1-4(B)  ; Gershayim (Punct.)
;("`z" ??)  ; Unassigned
 ("`x" ?$,1,V(B)  ; Segol (Point)
 ("`c" ?$,1,Q(B)  ; Hataf Segol
;("`v" ??)  ; Unassigned
;("`b" ??)  ; Unassigned
;("`n" ??)  ; Unassigned
 ("`m" ?$,1,U(B)  ; Tsere
;("`," ??)  ; Unassigned
;("`." ??)  ; Unassigned
 ("`/" ?,Aw(B)  ; Division Sign

 ("``" ?$,1,c(B)  ; Sof Pasuq
 ("`!" ?$,1,1(B)  ; Etnahta
 ("`@" ?$,1,2(B)  ; Segol (Accent)
 ("`#" ?$,1,3(B)  ; Shalshelet
 ("`$" ?$,1,4(B)  ; Zaqef Qatan
 ("`%" ?$,1,5(B)  ; Zaqef Gadol
 ("`^" ?$,1,6(B)  ; Tipeha
 ("`&" ?$,1,7(B)  ; Revia
 ("`*" ?$,1,8(B)  ; Zarqa
 ("`(" ?$,1,9(B)  ; Pashta
 ("`)" ?$,1,:(B)  ; Yetiv
 ("`_" ?$,1,;(B)  ; Tevir
 ("`+" ?$,1,<(B)  ; Geresh (Accent)
 ("`Q" ?$,1,=(B)  ; Geresh Muqdam
 ("`W" ?$,1,>(B)  ; Gershayim (Accent)
 ("`E" ?$,1,g(B)  ; Qamats Qatan
 ("`R" ?$,1,?(B)  ; Qarney Para
 ("`T" ?$,1,@(B)  ; Telisha Gedola
 ("`Y" ?$,1,A(B)  ; Pazer
 ("`U" ?$,1,Z(B)  ; Holam Haser for Vav
 ("`I" ?$,1,B(B)  ; Atnah Hafukh
 ("`O" ?$,1,C(B)  ; Munah
;("`P" ??)  ; Reserved
 ("`{" ?$,1,D(B)  ; Mahapakh
 ("`}" ?$,1,E(B)  ; Merkha
 ("`|" ?$,1,F(B)  ; Merkha Kefula
;("`A" ??)  ; Reserved
;("`S" ??)  ; Reserved
 ("`D" ?$,1,G(B)  ; Darga
 ("`F" ?$,1,H(B)  ; Qadma
 ("`G" ?$,1,I(B)  ; Telisha Qetana
 ("`H" ?$,1,J(B)  ; Yerah Ben Yomo
 ("`J" ?\u200D)  ; ZWJ
 ("`K" ?$,1,L(B)  ; Iluy
 ("`L" ?,Y4(B)  ; Left Double Quotation Mark (2nd)
 ("`:" ?,Y%(B)  ; Double Low-9 Quotation Mark
 ("`\"" ?$,1,M(B)  ; Dehi
 ("`Z" ?$,1,N(B)  ; Zinor
 ("`X" ?$,1,O(B)  ; Masora Circle
 ("`C" ?\u034F)  ; CGJ
 ("`V" ?$,1,`(B)  ; Paseq
 ("`B" ?$,1,f(B)  ; Nun Hafukha
 ("`N" ?\u200C)  ; ZWNJ
;("`M" ??)  ; Unassigned
;("`<" ??)  ; Unassigned
 ("`>" ?$,1,e(B)  ; Lower Dot
 ("`?" ?$,1,d(B)  ; Upper Dot
)

(quail-define-package
 "hebrew-lyx" "Hebrew" ",Hl$,1,T(B" nil "Hebrew LyX input method.

Based on LyX keyboard layout.
Additional mappings for Rafe and Yiddish ligatures.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?\;)
 ("_" ?$,1,^(B)  ; Maqaf
 ("q`" ?$,1,P(B)  ; Sheva
 ("w" ?\')
 ("e" ?,Hw(B)  ; Qof
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,H`(B)  ; Alef
 ("y" ?,Hh(B)  ; Tet
 ("u" ?,He(B)  ; Vav
 ("i" ?,Ho(B)  ; Final Nun
 ("o" ?,Hm(B)  ; Final Mem
 ("p" ?,Ht(B)  ; Pe
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("a" ?,Hy(B)  ; Shin
 ("s" ?,Hc(B)  ; Dalet
 ("d" ?,Hb(B)  ; Gimel
 ("f" ?,Hk(B)  ; Kaf
 ("g" ?,Hr(B)  ; Ayin
 ("h" ?,Hi(B)  ; Yod
 ("j" ?,Hg(B)  ; Het
 ("k" ?,Hl(B)  ; Lamed
 ("l" ?,Hj(B)  ; Final Kaf
 (";" ?,Hs(B)  ; Final Pe
 ("'" ?,)
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hq(B)  ; Samekh
 ("c" ?,Ha(B)  ; Bet
 ("v" ?,Hd(B)  ; He
 ("b" ?,Hp(B)  ; Nun
 ("n" ?,Hn(B)  ; Mem
 ("m" ?,Hv(B)  ; Tsadi
 ("," ?,Hz(B)  ; Tav
 ("." ?,Hu(B)  ; Final Tsadi
 ("/" ?.)  ; Stop
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("W" ?$,1-3(B)  ; Geresh (Punct.)
 ("E" ?$,1,X(B)  ; Qamats
 ("R" ?$,1,_(B)  ; Rafe
 ("T" ?\u200E)  ; LRM
 ("Y" ?\u200F)  ; RLM
 ("U" ?$,1,Y(B)  ; Holam
 ("I" ?$,1-2(B)  ; Yiddish Double Yod
 ("O" ?$,1-0(B)  ; Yiddish Double Vav
 ("P" ?$,1,W(B)  ; Patah
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring
 ("A" ?$,1,P(B)  ; Sheva
 ("S" ?$,1,\(B)  ; Dagesh or Mapiq
 ("F"  ?$,1-4(B)  ; Gershayim (Punct.)
 ("G" ?$,1,b(B)  ; Sin dot
 ("H" ?$,1,a(B)  ; Shin dot
 ("J" ?$,1,T(B)  ; Hiriq
 ("K" ?$,1tJ(B)  ; New Sheqel sign
 ("L" ?$,1-1(B)  ; Yiddish Vav Yod
 ("X" ?$,1,V(B)  ; Segol (Point)
 ("C" ?$,1,[(B)  ; Qubuts
 ("V" ?$,1,Q(B)  ; Hataf Segol
 ("B" ?$,1,R(B)  ; Hataf Patah
 ("N" ?$,1,S(B)  ; Hataf Qamats
 ("M" ?$,1,U(B)  ; Tsere
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring
)


(quail-define-package
 "hebrew-full" "Hebrew" ",Hr$,1,T,K(B" nil "Hebrew Full method.

Provides access to all Hebrew characters suitable to Modern Hebrew.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?\;)
 ("-" ?$,1,^(B)  ; Maqaf
 ("w" ?')
 ("e" ?,Hw(B)  ; Qof
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,H`(B)  ; Alef
 ("y" ?,Hh(B)  ; Tet
 ("u" ?,He(B)  ; Vav
 ("i" ?,Ho(B)  ; Final Nun
 ("o" ?,Hm(B)  ; Final Mem
 ("p" ?,Ht(B)  ; Pe
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("a" ?,Hy(B)  ; Shin
 ("s" ?,Hc(B)  ; Dalet
 ("d" ?,Hb(B)  ; Gimel
 ("f" ?,Hk(B)  ; Kaf
 ("g" ?,Hr(B)  ; Ayin
 ("h" ?,Hi(B)  ; Yod
 ("j" ?,Hg(B)  ; Het
 ("k" ?,Hl(B)  ; Lamed
 ("l" ?,Hj(B)  ; Final Kaf
 (";" ?,Hs(B)  ; Final Pe
 ("'" ?,)
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hq(B)  ; Samekh
 ("c" ?,Ha(B)  ; Bet
 ("v" ?,Hd(B)  ; He
 ("b" ?,Hp(B)  ; Nun
 ("n" ?,Hn(B)  ; Mem
 ("m" ?,Hv(B)  ; Tsadi
 ("," ?,Hz(B)  ; Tav
 ("." ?,Hu(B)  ; Final Tsadi
 ("/" ?.)

 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("Q" ?/)
 ("W" ?$,1-3(B)  ; Geresh (Punct.)
 ("E" ?$,1tJ(B)  ; New Sheqel Sign
 ("R" ?$,1,_(B)  ; Rafe
 ("T" ?$,1,Q(B)  ; Hataf Segol
 ("Y" ?$,1-1(B)  ; Yiddish Vav Yod
 ("U" ?$,1-0(B)  ; Yiddish Double Vav
 ("I" ?$,1,R(B)  ; Hataf Patah
 ("O" ?$,1,S(B)  ; Hataf Qamats
 ("P" ?$,1-4(B)  ; Gershayim (Punct.)
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring
 ("A" ?$,1,P(B)  ; Sheva
 ("S" ?$,1,\(B)  ; Dagesh or Mapiq
 ("D" ?$,1,[(B)  ; Qubuts
 ("F" ?$,1,Y(B)  ; Holam
 ("G" ?$,1,V(B)  ; Segol (Point)
 ("H" ?$,1,U(B)  ; Tsere
 ("J" ?$,1,T(B)  ; Hiriq
 ("K" ?$,1,W(B)  ; Patah
 ("L" ?$,1,X(B)  ; Qamats
 ("Z" ?$,1,b(B)  ; Sin Dot
 ("X" ?$,1,a(B)  ; Shin Dot
 ("C" ?$,1,K(B)  ; Ole
 ("V" ?$,1-2(B)  ; Yiddish Double Yod
 ("B" ?$,1,c(B)  ; Sof Pasuq
 ("N" ?\u200E)  ; LRM
 ("M" ?\u200F)  ; RLM
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring

 ("q`" ?\u202D)  ; LRO
 ("q1" ?\u202E)  ; RLO
 ("q2" ?\u202A)  ; LRE
 ("q3" ?\u202B)  ; RLE
 ("q4" ?\u202C)  ; PDF
 ("q5" ?\u034F)  ; CGJ
 ("q6" ?$,1,L(B)  ; Iluy
 ("q8" ?$,1,M(B)  ; Dehi
 ("q9" ?$,1,g(B)  ; Qamats Qatan
 ("q0" ?$,1,=(B)  ; Geresh Muqdam
 ("q-" ?-)  ; Minus
 ("q=" ?$,1,N(B)  ; Zinor
 ("q|" ?$,1,`(B)  ; Paseq
 ("qw" ?$,1,O(B)  ; Masora Circle
 ("qe" ?$,1,d(B)  ; Upper Dot
 ("qr" ?$,1,e(B)  ; Lower Dot
 ("qy" ?$,1,?(B)  ; Qarney Para
 ("qu" ?$,1,3(B)  ; Shalshelet
 ("qi" ?$,1,>(B)  ; Gershayim (Accent)
 ("qo" ?$,1,<(B)  ; Geresh (Accent)
 ("qp" ?$,1,H(B)  ; Qadma
 ("q[" ?$,1,f(B)  ; Nun Hafukha
 ("qa" ?$,1,Z(B)  ; Holam Haser for Vav
 ("qs" ?$,1,I(B)  ; Telisha Qetana
 ("qd" ?$,1,@(B)  ; Telisha Gedola
 ("qf" ?$,1,A(B)  ; Pazer
 ("qg" ?$,1,5(B)  ; Zaqef Gadol
 ("qh" ?$,1,4(B)  ; Zaqef Qatan
 ("qj" ?$,1,9(B)  ; Pashta
 ("qk" ?$,1,D(B)  ; Mahapakh
 ("ql" ?$,1,7(B)  ; Revia
 ("q;" ?$,1,2(B)  ; Segol (Accent)
 ("q'" ?$,1,8(B)  ; Zarqa
 ("qz" ?$,1,J(B)  ; Yerah Ben Yomo
 ("qx" ?$,1,F(B)  ; Merkha Kefula
 ("qc" ?$,1,:(B)  ; Yetiv
 ("qv" ?$,1,;(B)  ; Tevir
 ("qb" ?$,1,G(B)  ; Darga
 ("qn" ?$,1,1(B)  ; Etnahta
 ("qm" ?$,1,C(B)  ; Munah
 ("q," ?$,1,6(B)  ; Tipeha
 ("q." ?$,1,E(B)  ; Merkha
 ("q/" ?$,1,](B)  ; Meteg
)


(quail-define-package
 "hebrew-biblical-tiro" "Hebrew" ",Hz$,1,T,Hx$,1,Y(B" nil
"Biblical Hebrew Tiro input method.

Based on Society of Biblical Literature's Tiro keyboard layout.
Not suitable for modern Hebrew input.
 'q' is used to switch levels instead of Alt-Gr.
 Combining dot above (Called Masora dot) ($,1%G(B) is mapped to 'q1'.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?$,1,c(B)  ; Sof Pasuq
 ("-" ?$,1,^(B)  ; Maqaf
 ("=" ?$(O#?(B)  ; White Bullet
 ("w" ?$,1-3(B)  ; Geresh (Punct.)
 ("e" ?,Hw(B)  ; Qof
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,H`(B)  ; Alef
 ("y" ?,Hh(B)  ; Tet
 ("u" ?,He(B)  ; Vav
 ("i" ?,Ho(B)  ; Final Nun
 ("o" ?,Hm(B)  ; Final Mem
 ("p" ?,Ht(B)  ; Pe
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("\\" ?$,1,`(B)  ; Paseq
 ("a" ?,Hy(B)  ; Shin
 ("s" ?,Hc(B)  ; Dalet
 ("d" ?,Hb(B)  ; Gimel
 ("f" ?,Hk(B)  ; Kaf
 ("g" ?,Hr(B)  ; Ayin
 ("h" ?,Hi(B)  ; Yod
 ("j" ?,Hg(B)  ; Het
 ("k" ?,Hl(B)  ; Lamed
 ("l" ?,Hj(B)  ; Final Kaf
 (";" ?,Hs(B)  ; Final Pe
 ("'" ?$,1,:(B)  ; Yetiv
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hq(B)  ; Samekh
 ("c" ?,Ha(B)  ; Bet
 ("v" ?,Hd(B)  ; He
 ("b" ?,Hp(B)  ; Nun
 ("n" ?,Hn(B)  ; Mem
 ("m" ?,Hv(B)  ; Tsadi
 ("," ?,Hz(B)  ; Tav
 ("." ?,Hu(B)  ; Final Tsadi
 ("/" ?$,1,M(B)  ; Dehi
 ("~" ?$,1,N(B)  ; Zinor
 ("!" ?$,1,I(B)  ; Telisha Qetana
 ("@" ?$,1,9(B)  ; Pashta
 ("#" ?$,1,2(B)  ; Segol (Accent)
 ("$" ?$,1,O(B)  ; Masora circle
 ("%" ?$,1,Z(B)  ; Holam Haser for Vav
 ("^" ?$,1,Y(B)  ; Holam
 ("&" ?$,1,_(B)  ; Rafe
 ("*" ?$,1,b(B)  ; Sin dot
 ("(" ?$,1,a(B)  ; Shin dot
 (")" ?$,1,=(B)  ; Geresh Muqdam
 ("_" ?$,1,@(B)  ; Telisha Gedola
 ("+" ?$,1,\(B)  ; Dagesh or Mapiq
 ("Q" ?$,1,d(B)  ; Upper dot
 ("W" ?$,1,L(B)  ; Iluy
 ("E" ?$,1,K(B)  ; Ole
 ("R" ?$,1,?(B)  ; Qarney Para
 ("T" ?$,1,3(B)  ; Shalshelet
 ("Y" ?$,1,>(B)  ; Gershayim (Accent)
 ("U" ?$,1,<(B)  ; Geresh (Accent)
 ("I" ?$,1,A(B)  ; Pazer
 ("O" ?$,1,5(B)  ; Zaqef Gadol
 ("P" ?$,1,4(B)  ; Zaqef Qatan
 ("{" ?$,1,7(B)  ; Revia
 ("}" ?$,1,8(B)  ; Zarqa
 ("|" ?$,1,H(B)  ; Qadma
 ("A" ?$,1,](B)  ; Meteg
 ("S" ?$,1,P(B)  ; Sheva
 ("D" ?$,1,[(B)  ; Qubuts
 ("F" ?$,1,T(B)  ; Hiriq
 ("G" ?$,1,Q(B)  ; Hataf Segol
 ("H" ?$,1,V(B)  ; Segol (Point)
 ("J" ?$,1,U(B)  ; Tsere
 ("K" ?$,1,S(B)  ; Hataf Qamats
 ("L" ?$,1,X(B)  ; Qamats
 (":" ?$,1,R(B)  ; Hataf Patah
 ("\"" ?$,1,W(B)  ; Patah
 ("Z" ?$,1,e(B)  ; Lower dot
 ("X" ?$,1,D(B)  ; Mahapakh
 ("C" ?$,1,J(B)  ; Yerah Ben Yomo
 ("V" ?$,1,F(B)  ; Merkha Kefula
 ("B" ?$,1,E(B)  ; Merkha
 ("N" ?$,1,G(B)  ; Darga
 ("M" ?$,1,;(B)  ; Tevir
 ("<" ?$,1,1(B)  ; Etnahta
 (">" ?$,1,6(B)  ; Tipeha
 ("?" ?$,1,C(B)  ; Munah

 ("q`" ?\;)
 ("q1" ?\u0307)  ; Combining dot above
 ("q2" ?\u0336)  ; Combining long stroke overlay
 ("q3" ?\u030A)  ; Combining ring above
 ("q4" ?$,1tJ(B)  ; New Sheqel Sign
 ("q5" ?\u200D)  ; ZWJ
 ("q6" ?\u200C)  ; ZWNJ
 ("q7" ?\u034F)  ; CGJ
 ("q8" ?\u200E)  ; LRM
 ("q9" ?\u200F)  ; RLM
 ("q0" ?$,2",(B)  ; Dotted Circle
 ("q-" ?-)  ; Minus
 ("q=" ?$(O#@(B)  ; Bullet
 ("qq" ?\u0308)  ; Combining Diaeresis
 ("qw" ?$,1-4(B)  ; Gershayim (Punct.)
 ("qe" ?,F$(B)  ; Euro Sign
 ("qu" ?$,1-0(B)  ; Yiddish Double Vav
 ("q\\" ?\\)
 ("qh" ?$,1-2(B)  ; Yiddish Double Yod
 ("qj" ?$,1-1(B)  ; Yiddish Vav Yod
 ("ql" ?$,1,g(B)  ; Qamats Qatan
 ("q'" ?,)
 ("qc" ?$,1,B(B)  ; Atnah Hafukh
 ("qb" ?$,1,f(B)  ; Nun Hafukha
 ("q/" ?.)

 ("q~" ?~)
 ("q!" ?!)
 ("q@" ?@)
 ("q#" ?#)
 ("q$" ?$)
 ("q%" ?%)
 ("q^" ?^)
 ("q&" ?&)
 ("q*" ?*)
 ("q(" ?\))  ; mirroring
 ("q)" ?\()  ; mirroring
 ("q_" ?_)
 ("q+" ?+)
 ("qQ" ?/)
 ("qW" ?')
 ("q{" ?})  ; mirroring
 ("q}" ?{)  ; mirroring
 ("q|" ?|)
 ("q:" ?:)
 ("q\"" ?\")
 ("q<" ?>)
 ("q>" ?<)
 ("q?" ??)
)

(quail-define-package
 "hebrew-biblical-sil" "Hebrew" ",Hq$,1,T,Hl(B" nil
"Biblical Hebrew SIL input method.

Based on Society of Biblical Literature's SIL keyboard layout.
Phonetic and not suitable for modern Hebrew input.
 '`' is used to switch levels instead of Alt-Gr.
 Euro Sign (,F$(B) is mapped to 'Z'.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("-" ?$,1,^(B)  ; Maqaf
 ("=" ?$,1,\(B)  ; Dagesh or Mapiq
 ("q" ?,Hw(B)  ; Qof
 ("w" ?,He(B)  ; Vav
 ("e" ?$,1,V(B)  ; Segol (Point)
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,Hz(B)  ; Tav
 ("y" ?,Hi(B)  ; Yod
 ("u" ?$,1,[(B)  ; Qubuts
 ("i" ?$,1,T(B)  ; Hiriq
 ("o" ?$,1,Y(B)  ; Holam
 ("p" ?,Ht(B)  ; Pe
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("\\" ?$,1,`(B)  ; Paseq
 ("a" ?$,1,W(B)  ; Patah
 ("s" ?,Hq(B)  ; Samekh
 ("d" ?,Hc(B)  ; Dalet
 ("f" [ ",Hy$,1,b(B" ])  ; Shin + Sin dot
 ("g" ?,Hb(B)  ; Gimel
 ("h" ?,Hd(B)  ; He
 ("j" [ ",Hy$,1,a(B" ])  ; Shin + Shin dot
 ("k" ?,Hk(B)  ; Kaf
 ("l" ?,Hl(B)  ; Lamed
 (";" ?$,1,P(B)  ; Sheva
 ("'" ?,F"(B)  ; Right Single Quotation Mark
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hg(B)  ; Het
 ("c" ?,Hv(B)  ; Tsadi
 ("v" ?,Hh(B)  ; Tet
 ("b" ?,Ha(B)  ; Bet
 ("n" ?,Hp(B)  ; Nun
 ("m" ?,Hn(B)  ; Mem

 ("~" ?$,1tJ(B)  ; New Sheqel Sign
 ("@" ?$,1,8(B)  ; Zarqa
 ("#" ?$,1,H(B)  ; Qadma
 ("$" ?$,1,<(B)  ; Geresh (Accent)
 ("%" ?$,1,>(B)  ; Gershayim (Accent)
 ("&" ?$,1,L(B)  ; Iluy
 ("*" ?$,1,=(B)  ; Geresh Muqdam
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("_" ?$(G!9(B)  ; Em Dash
 ("Q" ?$,1,7(B)  ; Revia
 ("E" ?$,1,U(B)  ; Tsere
 ("Y" ?$,1,?(B)  ; Qarney Para
 ("O" ?$,1,Z(B)  ; Holam Haser for Vav
 ("P" ?,Hs(B)  ; Final Pe
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring

 ("A" ?$,1,X(B)  ; Qamats
 ("S" ?,Hy(B)  ; Shin
 ("K" ?,Hj(B)  ; Final Kaf
 (":" ?$,1-4(B)  ; Gershayim (Punct.)
 ("\"" ?,Y!(B)  ; Right Double Quotation Mark
 ("Z" ?,F$(B)  ; Euro Sign
 ("C" ?,Hu(B)  ; Final Tsadi
 ("N" ?,Ho(B)  ; Final Nun
 ("M" ?,Hm(B)  ; Final Mem
 ("<" ?,Hr(B)  ; Ayin
 (">" ?,H`(B)  ; Alef

 ("``" ?$)
 ("`1" ?$,1,](B)  ; Meteg
 ("`2" ?$,1,B(B)  ; Atnah Hafukh
 ("`3" ?$,1,6(B)  ; Tipeha
 ("`4" ?$,1,E(B)  ; Merkha
 ("`5" ?$,1,F(B)  ; Merkha Kefula
 ("`6" ?$,1,M(B)  ; Dehi
 ("`7" ?$,1,C(B)  ; Munah
 ("`8" ?$,1,;(B)  ; Tevir
 ("`9" ?$,1,G(B)  ; Darga
 ("`0" ?$,1,J(B)  ; Yerah Ben Yomo
 ("`-" ?$(G!7(B)  ; Em Dash
 ("`=" ?$,1,1(B)  ; Etnahta
 ("`]" ?$,1,:(B)  ; Accent Yetiv
 ("`\\" ?$,1,D(B)  ; Mahapakh
 ("`a" ?$,1,g(B)  ; Qamats Qatan
 ("`g" ? $(O#?(B)  ; White Bullet
 ("`h" ?\u0336)  ; Combining Long Stroke Overlay
 ("`;" ?\;)
 ("`'" ?\u0323); Combining Dot Below (Lower Point??)
 ("`m" ?\u200C)  ; ZWNJ
 ("`," ?,A;(B)  ; mirroring
 ("`." ?,A+(B)  ; mirroring
 ("`/" ?$,1-3(B)  ; Geresh (Punct.)

 ("`!" ?$,1,7(B)  ; Revia
 ("`@" ?$,1,N(B)  ; Zinor
 ("`#" ?$,1,9(B)  ; Pashta
 ("`$" ?$,1,@(B)  ; Telisha Gedola
 ("`%" ?$,1,I(B)  ; Telisha Qetana
 ("`&" ?$,1,A(B)  ; Pazer
 ("`*" ?$,1,5(B)  ; Zaqef Gadol
 ("`(" ?$,1,3(B)  ; Shalshelet
 ("`)" ?$,1,O(B)  ; Masora Circle
 ("`_" ?$,1,_(B)  ; Rafe
 ("`+" ?$,2",(B)  ; Dotted Circle
 ("`E" ?$,1,Q(B)  ; Hataf Segol
 ("`O" ?$,1,S(B)  ; Hataf Qamats
 ("`P" ?\u034F)  ; CGJ
 ("`{" ?$,1,4(B)  ; Zaqef Qatan
 ("`}" ?$,1,2(B)  ; Segol (Accent)
 ("`|" ?$,1,K(B)  ; Ole
 ("`A" ?$,1,R(B)  ; Hataf Patah
 ("`G" ?$(O#@(B)  ; Bullet
 ("`H" ?\u030A)  ; Combining ring above
 ("`:" ?$,1,c(B)  ; Sof Pasuq
 ("`\"" ?$,1,d(B)  ; Upper Dot
 ("`M" ?\u200D)  ; ZWJ
 ("`<" ?\u0307)  ; Combining dot above
 ("`>" ?\u0308)  ; Combining Diaeresis
)


(quail-define-package
 "yiddish-royal" "Hebrew" "$,1-2,Hx(B" nil "Yiddish Royal input method.

Based on Royal Yiddish typewriter.
Better for yiddish than Hebrew methods.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?~)
 ("q" ?,Hw(B)  ; Qof
 ("w" [ ",H`$,1,X(B" ])  ; Qamats Alef (Komets Alef)
 ("e" ?,Hx(B)  ; Resh
 ("r" ?,H`(B)  ; Alef (Shtumer Alef)
 ("t" ?,Hh(B)  ; Tet
 ("y" ?$,1-0(B)  ; Yiddish Double Vav (Tsvey Vovn)
 ("u" ?,He(B)  ; Vav
 ("i" ?,Ho(B)  ; Final Nun
 ("o" ?,Hm(B)  ; Final Mem
 ("p" [ ",Ht$,1,_(B" ])  ; Rafe Pe (Fey)
 ("[" [ ",Ht$,1,\(B" ])  ; Dagesh Pe (Pey)
 ("]" ?,)
 ("a" ?,Hy(B)  ; Shin
 ("s" ?,Hc(B)  ; Dalet
 ("d" ?,Hb(B)  ; Gimel
 ("f" ?,Hk(B)  ; Kaf
 ("g" ?,Hr(B)  ; Ayin
 ("h" ?$,1-2(B)  ; Yiddish Double Yod (Tsvey Yudn)
 ("j" ?,Hi(B)  ; Yod
 ("k" ?,Hg(B)  ; Het
 ("l" ?,Hl(B)  ; Lamed
 (";" ?,Hj(B)  ; Final Kaf
 ("'" ?,Hs(B)  ; Final Pe
 ("z" ?.)
 ("x" ?,Hf(B)  ; Zayin
 ("c" ?,Hq(B)  ; Samekh
 ("v" ?,Ha(B)  ; Bet
 ("b" ?,Hd(B)  ; He
 ("n" ?,Hp(B)  ; Nun
 ("m" ?,Hn(B)  ; Mem
 ("," ?,Hv(B)  ; Tsadi
 ("." ?,Hz(B)  ; Tav
 ("/" ?,Hu(B)  ; Final Tsadi

 ("~" ?@)
 ("!" ?,Y!(B)  ; Right Double Quotation Mark
 ("@" ?,Y%(B)  ; Double Low-9 Quotation Mark
 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("Q" ?,A=(B)  ; Right Double Quotation Mark
 ("W" ?,A<(B)
 ("E" ?,A>(B)  ; Yiddish Double Yod (x2)
 ("R" [ ",H`$,1,W(B" ])  ; Patah Alef (Pasekh Alef)
; ("T" "")
 ("Y" ?$,1-1(B)  ; Ligature Yiddish Vav Yod (vov yud)
 ("U" [ ",He$,1,\(B" ])  ; Melupm vov
 ("I" ?/)
 ("O" ?\\)
 ("P" ?,Ht(B)  ; Pe
 ("{" ??)
 ("}" ?!)
 ("A" [ ",Hy$,1,b(B" ])  ; Shin + Sin dot
 ("S" [ ",Hy$,1,b(B" ])  ; Shin + Sin dot
; ("D" "")
 ("F" [ ",Hk$,1,\(B" ])  ; Dagesh Kaf (Kof)
; ("G" "")
 ("H" [ "$,1-2,W(B" ])  ; Yiddish Double Yod + Patah (Pasekh Tsvey Yudn)
 ("J" [ ",Hi$,1,T(B" ])  ; Khirik Yud
 ("K" ?})  ; mirroring
 ("L" ?{)  ; mirroring
 ("\"" ?\;)
 ("Z" ??)
 ("X" ?|)
 ("C"  [ ",Ha$,1,\(B" ])  ; Dagesh Bet (Beys)
 ("V" [ ",Ha$,1,_(B" ])  ; Rafe Bet (Veys)
 ("B" ?\])  ; mirroring
 ("N" ?\[)  ; mirroring
 ("M" ?>)  ; mirroring
 ("<" ?<)  ; mirroring
 (">" [ ",Hz$,1,\(B" ])  ; Dagesh Tav (Tof)
 ("?" ?\')
)


(quail-define-package
 "yiddish-keyman" "Hebrew" "$,1-2,Hw(B" nil "Yiddish Keyman input method.

Based on Keyman keyboard layout.
Better for yiddish than Hebrew methods..
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?\;)
 ("q" ?,Y%(B)  ; Double Low-9 Quotation Mark
 ("w" ?,Hy(B)  ; Shin
 ("e" ?,Hr(B)  ; Ayin
 ("r" ?,Hx(B)  ; Resh
 ("t" ?,Hh(B)  ; Tet
 ("y" ?,Hi(B)  ; Yod
 ("u" ?,He(B)  ; Vav
 ("i" ?,Hi(B)  ; Yod (x2)
 ("o" [ ",H`$,1,X(B" ])  ; Qamats Alef (Komets Alef)
 ("p" [ ",Ht$,1,\(B" ])  ; Dagesh Pe (Pey)
 ("[" ?\])  ; mirroring
 ("]" ?\[)  ; mirroring
 ("a"  [ ",H`$,1,W(B" ])  ; Patah Alef (Pasekh Alef)
 ("s" ?,Hq(B)  ; Samekh
 ("d" ?,Hc(B)  ; Dalet
 ("f" [ ",Ht$,1,_(B" ])  ; Rafe Pe (Fey)
 ("g" ?,Hb(B)  ; Gimel
 ("h" ?,Hd(B)  ; He
 ("j" ?$,1-2(B)  ; Yiddish Double Yod (Tsvey Yudn)
 ("k" ?,Hw(B)  ; Qof
 ("l" ?,Hl(B)  ; Lamed
 ("z" ?,Hf(B)  ; Zayin
 ("x" ?,Hk(B)  ; Kaf
 ("c" ?,Hv(B)  ; Tsadi
 ("v" ?$,1-0(B)  ; Yiddish Double Vav (Tsvey Vovn)
 ("b" ?,Ha(B)  ; Bet
 ("n" ?,Hp(B)  ; Nun
 ("m" ?,Hn(B)  ; Mem

 ("(" ?\))  ; mirroring
 (")" ?\()  ; mirroring
 ("Q" ?,Y!(B)  ; Right Double Quotation Mark
 ("W" [ ",Hy$,1,b(B" ])  ; Shin + Sin dot
 ("E" ?$,1-2(B)  ; Yiddish Double Yod (x2)
; ("R" "")  ;
 ("T" [ ",Hz$,1,\(B" ])  ; Dagesh Tav (Tof)
 ("Y" [ "$,1-2,W(B" ])  ; Yiddish Double Yod + Patah (Pasekh Tsvey Yudn)
 ("U" [ ",He$,1,\(B" ])  ; Melupm vov
 ("I" [ ",Hi$,1,T(B" ])  ; Khirik Yud
 ("O" ?$,1-1(B)  ; Ligature Yiddish Vav Yod (vov yud)
; ("P" "")
 ("{" ?})  ; mirroring
 ("}" ?{)  ; mirroring
 ("A" ?,H`(B)  ; Alef (Shtumer Alef)
 ("S" ?,Hz(B)  ; Tav
 ("F"  ?,Hs(B)  ; Final Pe
 ("G" ?$,1-3(B)  ; Geresh (Punct.)
 ("H" ?,Hg(B)  ; Het
 ("J" ?$,1-2(B)  ; Yiddish Double Yod (x2)
 ("K" [ ",Hk$,1,\(B" ])  ; Dagesh Kaf (Kof)
; ("L" "")
; ("Z" "")
 ("X" ?,Hj(B)  ; Final Kaf
 ("C" ?,Hu(B)  ; Final Tsadi
 ("V" [ ",Ha$,1,_(B" ])  ; Rafe Bet (Veys) )  ; Bet
; ("B" "")
 ("N" ?,Ho(B)  ; Final Nun
 ("M" ?,Hm(B)  ; Final Mem
 ("<" ?>)  ; mirroring
 (">" ?<)  ; mirroring
)

;;; hebrew.el ends here
