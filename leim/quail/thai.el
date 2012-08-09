;;; thai.el --- Quail package for inputting Thai characters -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Thai

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

(defmacro thai-generate-quail-map (translation-table)
  (let (map)
     (dotimes (i (length translation-table))
       (let ((trans (aref translation-table i)))
	 (when (not (eq trans 0))
	   (if (> (length trans) 1)
	       (setq trans (vector trans))
	     (setq trans (aref trans 0)))
	   (setq map (cons (list (char-to-string i) trans) map)))))
     `(quail-define-rules ,@map)))

;; Thai Kesmanee keyboard support.

(quail-define-package
 "thai-kesmanee" "Thai" ",T!!(B>" t
 "Thai Kesmanee input method with TIS620 keyboard layout

The difference from the ordinal Thai keyboard:
    ',T_(B' and ',To(B' are assigned to '\\' and '|' respectively,
    ',T#(B' and ',T%(B' are assigned to '`' and '~' respectively,
    Don't know where to assign characters ',Tz(B' and ',T{(B'."
 nil t t t t nil nil nil nil nil t)

(thai-generate-quail-map
 [
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
;; This data is quite old.
;;   0   "#" "." ",Tr(B" ",Ts(B" ",Tt(B" ",TQi(B" ",T'(B"	; SPC .. '
;;   ",Tv(B" ",Tw(B" ",Tu(B" ",Ty(B" ",TA(B" ",T"(B" ",Tc(B" ",T=(B"	; ( .. /
;;   ",T((B" ",TE(B" "/" "_" ",T@(B" ",T6(B" ",TX(B" ",TV(B"	; 0 .. 7
;;   ",T$(B" ",T5(B" ",T+(B" ",TG(B" ",T2(B" ",T*(B" ",TL(B" ",TF(B"	; 8 .. ?
;;   ",Tq(B" ",TD(B" ",TZ(B" ",T)(B" ",T/(B" ",T.(B" ",Tb(B" ",T,(B"	; @ .. G
;;   ",Tg(B" ",T3(B" ",Tk(B" ",TI(B" ",TH(B" ",Tn(B" ",Tl(B" ",TO(B"	; H .. O
;;   ",T-(B" ",Tp(B" ",T1(B" ",T&(B" ",T8(B" ",Tj(B" ",TN(B" "\""	; P .. W
;;   ")" ",Tm(B" "(" ",T:(B" ",T_(B" ",TE(B" ",TY(B" ",Tx(B"	; X .. _
;;   ",T#(B" ",T?(B" ",TT(B" ",Ta(B" ",T!(B" ",TS(B" ",T4(B" ",T`(B"	; ` .. g
;;   ",Ti(B" ",TC(B" ",Th(B" ",TR(B" ",TJ(B" ",T7(B" ",TW(B" ",T9(B"	; h .. o
;;   ",TB(B" ",Tf(B" ",T>(B" ",TK(B" ",TP(B" ",TU(B" ",TM(B" ",Td(B"	; p .. w
;;   ",T;(B" ",TQ(B" ",T<(B" ",T0(B" ",To(B" "," ",T%(B" 0	; x .. DEL
;; This is the correct data nowadays.
  0  "+" "." ",Tr(B" ",Ts(B" ",Tt(B" ",T_(B" ",T'(B"	; SPC .. '
  ",Tv(B" ",Tw(B" ",Tu(B" ",Ty(B" ",TA(B" ",T"(B" ",Tc(B" ",T=(B"	; ( .. /
  ",T((B" ",Te(B" "/" "-" ",T@(B" ",T6(B" ",TX(B" ",TV(B"	; 0 .. 7
  ",T$(B" ",T5(B" ",T+(B" ",TG(B" ",T2(B" ",T*(B" ",TL(B" ",TF(B"	; 8 .. ?
  ",Tq(B" ",TD(B" ",TZ(B" ",T)(B" ",T/(B" ",T.(B" ",Tb(B" ",T,(B"	; @ .. G
  ",Tg(B" ",T3(B" ",Tk(B" ",TI(B" ",TH(B" "?" ",Tl(B" ",TO(B"	; H .. O
  ",T-(B" ",Tp(B" ",T1(B" ",T&(B" ",T8(B" ",Tj(B" ",TN(B" "\""	; P .. W
  "\)" ",Tm(B" "\(" ",T:(B" ",T#(B" ",TE(B" ",TY(B" ",Tx(B"	; X .. _
  "_" ",T?(B" ",TT(B" ",Ta(B" ",T!(B" ",TS(B" ",T4(B" ",T`(B"	; ` .. g
  ",Ti(B" ",TC(B" ",Th(B" ",TR(B" ",TJ(B" ",T7(B" ",TW(B" ",T9(B"	; h .. o
  ",TB(B" ",Tf(B" ",T>(B" ",TK(B" ",TP(B" ",TU(B" ",TM(B" ",Td(B"	; p .. w
  ",T;(B" ",TQ(B" ",T<(B" ",T0(B" ",T%(B" "," "%" 0	; x .. DEL
  ])


;; Thai Pattachote keyboard support.

(quail-define-package
 "thai-pattachote" "Thai" ",T!;(B>" t
 "Thai Pattachote input method with TIS620 keyboard layout"
 nil t t t t nil nil nil nil nil t)

(thai-generate-quail-map
 [
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0	; control codes
  0 "+" ",T1(B" "/" "," "?" "_" ",T"(B"		; SPC .. '
  "(" ")" "." "%" ",TP(B" ",Tq(B" ",T((B" ",T>(B"	; ( .. /
  ",Tp(B" "=" ",Tr(B" ",Ts(B" ",Tt(B" ",Tu(B" ",TY(B" ",Tw(B"	; 0 .. 7
  ",Tx(B" ",Ty(B" ",T&(B" ",Td(B" ",T?(B" ",Tv(B" ",T2(B" ",TL(B"	; 8 .. ?
  "\"" ",Tk(B" ",TQ(B" ",T0(B" ",TS(B" ",Tf(B" ",T3(B" ",Tl(B"	; @ .. G
  ",TW(B" ",T+(B" ",T<(B" ",T*(B" ",Tb(B" ",TN(B" ",TH(B" ",T6(B"	; H .. O
  ",T2(B" ",Tj(B" ",T-(B" ",T8(B" ",TI(B" ",T=(B" ",T@(B" ",TD(B"	; P .. W
  ",T.(B" ",TV(B" ",T.(B" ",Tc(B" ",TZ(B" ",T2(B" ",TX(B" "-"	; X .. _
  ",T#(B" ",Ti(B" ",TT(B" ",TE(B" ",T'(B" ",TB(B" ",T!(B" ",TQ(B"	; ` .. g
  ",TU(B" ",TA(B" ",TR(B" ",T9(B" ",T`(B" ",TJ(B" ",T$(B" ",TG(B"	; h .. o
  ",Ta(B" ",Tg(B" ",TM(B" ",T7(B" ",TC(B" ",T4(B" ",TK(B" ",T5(B"	; p .. w
  ",T;(B" ",Th(B" ",T:(B" ",TO(B" ",Tm(B" ",TF(B" ",T%(B" 0		; x .. DEL
  ])

;;; thai.el ends here
