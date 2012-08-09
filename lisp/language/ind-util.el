;;; ind-util.el --- Transliteration and Misc. Tools for Indian Languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Maintainer:  KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: multilingual, Indian, Devanagari

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

;; This file provides conversion between UCS and various
;; transliteration schemes, such as ITRANS, kyoto-harvard and aiba
;; methods.  It also provides conversion between IS 13194 and UCS.
;; Finally, this program provides the compatibility support with
;; old implementation of Devanagari script.

;;; Code:

;;; Transliteration

;; The followings provide the various transliteration schemes (such as
;; ITRANS, kyoto-harvard, and Aiba) of Indian scripts.  They are also
;; used in quail/indian.el for typing Indian script in Emacs.

(eval-and-compile

(defun indian-regexp-of-hashtbl-keys (hashtbl)
  "Returns the regular expression of hashtable keys."
  (let (keys)
    (maphash (lambda (key val) (push key keys)) hashtbl)
    (regexp-opt keys)))

(defvar indian-dev-base-table
  '(
    (;; VOWELS  (18)
     (?$,15E(B nil) (?$,15F(B ?$,15~(B) (?$,15G(B ?$,15(B) (?$,15H(B ?$,16 (B) (?$,15I(B ?$,16!(B) (?$,15J(B ?$,16"(B)
     (?$,15K(B ?$,16#(B) (?$,15L(B ?$,16B(B) (?$,15M(B ?$,16%(B) (?$,15N(B ?$,16&(B) (?$,15O(B ?$,16'(B) (?$,15P(B ?$,16((B)
     (?$,15Q(B ?$,16)(B) (?$,15R(B ?$,16*(B) (?$,15S(B ?$,16+(B) (?$,15T(B ?$,16,(B) (?$,16@(B ?$,16$(B) (?$,16A(B ?$,16C(B))
    (;; CONSONANTS (currently 42, including special cases)
     ?$,15U(B ?$,15V(B ?$,15W(B ?$,15X(B ?$,15Y(B                  ;; GUTTRULS
     ?$,15Z(B ?$,15[(B ?$,15\(B ?$,15](B ?$,15^(B                  ;; PALATALS
     ?$,15_(B ?$,15`(B ?$,15a(B ?$,15b(B ?$,15c(B                  ;; CEREBRALS
     ?$,15d(B ?$,15e(B ?$,15f(B ?$,15g(B ?$,15h(B ?$,15i(B              ;; DENTALS
     ?$,15j(B ?$,15k(B ?$,15l(B ?$,15m(B ?$,15n(B                  ;; LABIALS
     ?$,15o(B ?$,15p(B ?$,15q(B ?$,15r(B ?$,15s(B ?$,15t(B ?$,15u(B          ;; SEMIVOWELS
     ?$,15v(B ?$,15w(B ?$,15x(B ?$,15y(B                    ;; SIBILANTS
     ?$,168(B ?$,169(B ?$,16:(B ?$,16;(B ?$,16<(B ?$,16=(B ?$,16>(B ?$,16?(B      ;; NUKTAS
     "$,15\6-5^(B" "$,15U6-5w(B")
    (;; Misc Symbols (7)
     ?$,15A(B ?$,15B(B ?$,15C(B ?$,15}(B ?$,16-(B ?$,160(B ?$,16D(B)
    (;; Digits (10)
     ?$,16F(B ?$,16G(B ?$,16H(B ?$,16I(B ?$,16J(B ?$,16K(B ?$,16L(B ?$,16M(B ?$,16N(B ?$,16O(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,16-5p(B" "$,15p6-(B" "$,15d6-5p(B" "$,15v6-5p(B" "$,15|(B")))

;; Punjabi is also known as Gurmukhi.
(defvar indian-pnj-base-table
  '(
    (;; VOWELS
     (?$,18%(B nil) (?$,18&(B ?$,18^(B) (?$,18'(B ?$,18_(B) (?$,18((B ?$,18`(B) (?$,18)(B ?$,18a(B) (?$,18*(B ?$,18b(B)
     nil nil nil nil (?$,18/(B ?$,18g(B) (?$,180(B ?$,18h(B)
     nil nil (?$,183(B ?$,18k(B) (?$,184(B ?$,18l(B) nil nil)
    (;; CONSONANTS
     ?$,185(B ?$,186(B ?$,187(B ?$,188(B ?$,189(B                  ;; GUTTRULS
     ?$,18:(B ?$,18;(B ?$,18<(B ?$,18=(B ?$,18>(B                  ;; PALATALS
     ?$,18?(B ?$,18@(B ?$,18A(B ?$,18B(B ?$,18C(B                  ;; CEREBRALS
     ?$,18D(B ?$,18E(B ?$,18F(B ?$,18G(B ?$,18H(B nil              ;; DENTALS
     ?$,18J(B ?$,18K(B ?$,18L(B ?$,18M(B ?$,18N(B                  ;; LABIALS
     ?$,18O(B ?$,18P(B nil ?$,18R(B ?$,18S(B nil ?$,18U(B          ;; SEMIVOWELS
     ?$,18V(B nil ?$,18X(B ?$,18Y(B                    ;; SIBILANTS
     nil ?$,18y(B ?$,18z(B ?$,18{(B ?$,18|(B nil ?$,18~(B nil      ;; NUKTAS
     "$,18<8m8>(B" nil)
    (;; Misc Symbols (7)
     nil ?$,18"(B nil nil ?$,18m(B nil nil) ;; ek onkar, etc.
    (;; Digits
     ?$,19&(B ?$,19'(B ?$,19((B ?$,19)(B ?$,19*(B ?$,19+(B ?$,19,(B ?$,19-(B ?$,19.(B ?$,19/(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,18m8P(B" "$,18P8m(B" "$,18D8m8P(B" "$,18V8m8P(B" "$,18\(B")))

(defvar indian-gjr-base-table
  '(
    (;; VOWELS
     (?$,19E(B nil) (?$,19F(B ?$,19~(B) (?$,19G(B ?$,19(B) (?$,19H(B ?$,1: (B) (?$,19I(B ?$,1:!(B) (?$,19J(B ?$,1:"(B)
     (?$,19K(B ?$,1:#(B) nil (?$,19M(B ?$,1:%(B) nil (?$,19O(B ?$,1:'(B) (?$,19P(B ?$,1:((B)
     (?$,19Q(B ?$,1:)(B) nil (?$,19S(B ?$,1:+(B) (?$,19T(B ?$,1:,(B) (?$,1:@(B ?$,1:$(B) nil)
    (;; CONSONANTS
     ?$,19U(B ?$,19V(B ?$,19W(B ?$,19X(B ?$,19Y(B                  ;; GUTTRULS
     ?$,19Z(B ?$,19[(B ?$,19\(B ?$,19](B ?$,19^(B                  ;; PALATALS
     ?$,19_(B ?$,19`(B ?$,19a(B ?$,19b(B ?$,19c(B                  ;; CEREBRALS
     ?$,19d(B ?$,19e(B ?$,19f(B ?$,19g(B ?$,19h(B nil              ;; DENTALS
     ?$,19j(B ?$,19k(B ?$,19l(B ?$,19m(B ?$,19n(B                  ;; LABIALS
     ?$,19o(B ?$,19p(B nil ?$,19r(B ?$,19s(B nil ?$,19u(B          ;; SEMIVOWELS
     ?$,19v(B ?$,19w(B ?$,19x(B ?$,19y(B                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "$,19\:-9^(B" "$,19U:-9w(B")
    (;; Misc Symbols (7)
     ?$,19A(B ?$,19B(B ?$,19C(B ?$,19}(B ?$,1:-(B ?$,1:0(B nil)
    (;; Digits
     ?$,1:F(B ?$,1:G(B ?$,1:H(B ?$,1:I(B ?$,1:J(B ?$,1:K(B ?$,1:L(B ?$,1:M(B ?$,1:N(B ?$,1:O(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,1:-9p(B" "$,19p:-(B" "$,19d:-9p(B" "$,19v:-9p(B" "$,19|(B")))

(defvar indian-ori-base-table
  '(
    (;; VOWELS
     (?$,1:e(B nil) (?$,1:f(B ?$,1;>(B) (?$,1:g(B ?$,1;?(B) (?$,1:h(B ?$,1;@(B) (?$,1:i(B ?$,1;A(B) (?$,1:j(B ?$,1;B(B)
     (?$,1:k(B ?$,1;C(B) (?$,1:l(B nil) nil nil (?$,1:o(B ?$,1;G(B) (?$,1:p(B ?$,1;H(B)
     nil nil (?$,1:s(B ?$,1;K(B) (?$,1:t(B ?$,1;L(B) (?$,1;`(B nil) (?$,1;a(B nil))
    (;; CONSONANTS
     ?$,1:u(B ?$,1:v(B ?$,1:w(B ?$,1:x(B ?$,1:y(B                  ;; GUTTRULS
     ?$,1:z(B ?$,1:{(B ?$,1:|(B ?$,1:}(B ?$,1:~(B                  ;; PALATALS
     ?$,1:(B ?$,1; (B ?$,1;!(B ?$,1;"(B ?$,1;#(B                  ;; CEREBRALS
     ?$,1;$(B ?$,1;%(B ?$,1;&(B ?$,1;'(B ?$,1;((B nil              ;; DENTALS
     ?$,1;*(B ?$,1;+(B ?$,1;,(B ?$,1;-(B ?$,1;.(B                  ;; LABIALS
     ?$,1;/(B ?$,1;0(B nil ?$,1;2(B ?$,1;3(B nil nil          ;; SEMIVOWELS
     ?$,1;6(B ?$,1;7(B ?$,1;8(B ?$,1;9(B                    ;; SIBILANTS
     nil nil nil nil ?$,1;\(B ?$,1;](B nil ?$,1;_(B      ;; NUKTAS
     "$,1:|;M:~(B" "$,1:u;M;7(B")
    (;; Misc Symbols
     ?$,1:a(B ?$,1:b(B ?$,1:c(B ?$,1;=(B ?$,1;M(B nil nil)
    (;; Digits
     ?$,1;f(B ?$,1;g(B ?$,1;h(B ?$,1;i(B ?$,1;j(B ?$,1;k(B ?$,1;l(B ?$,1;m(B ?$,1;n(B ?$,1;o(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,1;M;0(B" "$,1;0;M(B" "$,1;$;M;0(B" "$,1;6;M;0(B" "$,1;<(B")))

(defvar indian-bng-base-table
  '(
    (;; VOWELS
     (?$,16e(B nil) (?$,16f(B ?$,17>(B) (?$,16g(B ?$,17?(B) (?$,16h(B ?$,17@(B) (?$,16i(B ?$,17A(B) (?$,16j(B ?$,17B(B)
     (?$,16k(B ?$,17C(B) (?$,16l(B ?$,17b(B) nil nil (?$,16o(B ?$,17G(B) (?$,16p(B ?$,17H(B)
     nil nil (?$,16s(B ?$,17K(B) (?$,16t(B ?$,17L(B) (?$,17`(B ?$,17D(B) (?$,17a(B ?$,17c(B))
    (;; CONSONANTS
     ?$,16u(B ?$,16v(B ?$,16w(B ?$,16x(B ?$,16y(B                  ;; GUTTRULS
     ?$,16z(B ?$,16{(B ?$,16|(B ?$,16}(B ?$,16~(B                  ;; PALATALS
     ?$,16(B ?$,17 (B ?$,17!(B ?$,17"(B ?$,17#(B                  ;; CEREBRALS
     ?$,17$(B ?$,17%(B ?$,17&(B ?$,17'(B ?$,17((B nil              ;; DENTALS
     ?$,17*(B ?$,17+(B ?$,17,(B ?$,17-(B ?$,17.(B                  ;; LABIALS
     ?$,17/(B ?$,170(B nil ?$,172(B nil nil nil          ;; SEMIVOWELS
     ?$,176(B ?$,177(B ?$,178(B ?$,179(B                    ;; SIBILANTS
     nil nil nil nil ?$,17\(B ?$,17](B nil ?$,17_(B      ;; NUKTAS
     "$,16|7M6~(B" "$,16u7M77(B")
    (;; Misc Symbols
     ?$,16a(B ?$,16b(B ?$,16c(B nil ?$,17M(B nil nil)
    (;; Digits
     ?$,17f(B ?$,17g(B ?$,17h(B ?$,17i(B ?$,17j(B ?$,17k(B ?$,17l(B ?$,17m(B ?$,17n(B ?$,17o(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,17M70(B" "$,1707M(B" "$,17$7M70(B" "$,1767M70(B" "$,17<(B")))

(defvar indian-asm-base-table
  '(
    (;; VOWELS
     (?$,16e(B nil) (?$,16f(B ?$,17>(B) (?$,16g(B ?$,17?(B) (?$,16h(B ?$,17@(B) (?$,16i(B ?$,17A(B) (?$,16j(B ?$,17B(B)
     (?$,16k(B ?$,17C(B) (?$,16l(B ?$,17b(B) nil nil (?$,16o(B ?$,17G(B) (?$,16p(B ?$,17H(B)
     nil nil (?$,16s(B ?$,17K(B) (?$,16t(B ?$,17L(B) (?$,17`(B ?$,17D(B) (?$,17a(B ?$,17c(B))
    (;; CONSONANTS
     ?$,16u(B ?$,16v(B ?$,16w(B ?$,16x(B ?$,16y(B                  ;; GUTTRULS
     ?$,16z(B ?$,16{(B ?$,16|(B ?$,16}(B ?$,16~(B                  ;; PALATALS
     ?$,16(B ?$,17 (B ?$,17!(B ?$,17"(B ?$,17#(B                  ;; CEREBRALS
     ?$,17$(B ?$,17%(B ?$,17&(B ?$,17'(B ?$,17((B nil              ;; DENTALS
     ?$,17*(B ?$,17+(B ?$,17,(B ?$,17-(B ?$,17.(B                  ;; LABIALS
     ?$,17/(B ?$,17p(B nil ?$,172(B nil nil ?$,17q(B          ;; SEMIVOWELS
     ?$,176(B ?$,177(B ?$,178(B ?$,179(B                    ;; SIBILANTS
     nil nil nil nil ?$,17\(B ?$,17](B nil ?$,17_(B      ;; NUKTAS
     "$,16|7M6~(B" "$,16u7M77(B")
    (;; Misc Symbols
     ?$,16a(B ?$,16b(B ?$,16c(B nil ?$,17M(B nil nil)
    (;; Digits
     ?$,17f(B ?$,17g(B ?$,17h(B ?$,17i(B ?$,17j(B ?$,17k(B ?$,17l(B ?$,17m(B ?$,17n(B ?$,17o(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,17M7p(B" "$,17p7M(B" "$,17$7M7p(B" "$,1767M7p(B" "$,17<(B")))

(defvar indian-tlg-base-table
  '(
    (;; VOWELS
     (?$,1=E(B nil) (?$,1=F(B ?$,1=~(B) (?$,1=G(B ?$,1=(B) (?$,1=H(B ?$,1> (B) (?$,1=I(B ?$,1>!(B) (?$,1=J(B ?$,1>"(B)
     (?$,1=K(B ?$,1>#(B) (?$,1=L(B nil) nil (?$,1=O(B ?$,1>'(B) (?$,1=N(B ?$,1>&(B) (?$,1=P(B ?$,1>((B)
     nil (?$,1=S(B ?$,1>+(B) (?$,1=R(B ?$,1>*(B) (?$,1=T(B ?$,1>,(B) (?$,1>@(B ?$,1>$(B) (?$,1>A(B nil))
    (;; CONSONANTS
     ?$,1=U(B ?$,1=V(B ?$,1=W(B ?$,1=X(B ?$,1=Y(B                  ;; GUTTRULS
     ?$,1=Z(B ?$,1=[(B ?$,1=\(B ?$,1=](B ?$,1=^(B                  ;; PALATALS
     ?$,1=_(B ?$,1=`(B ?$,1=a(B ?$,1=b(B ?$,1=c(B                  ;; CEREBRALS
     ?$,1=d(B ?$,1=e(B ?$,1=f(B ?$,1=g(B ?$,1=h(B nil              ;; DENTALS
     ?$,1=j(B ?$,1=k(B ?$,1=l(B ?$,1=m(B ?$,1=n(B                  ;; LABIALS
     ?$,1=o(B ?$,1=p(B ?$,1=q(B ?$,1=r(B ?$,1=s(B nil ?$,1=u(B          ;; SEMIVOWELS
     ?$,1=v(B ?$,1=w(B ?$,1=x(B ?$,1=y(B                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "$,1=\>-=^(B" "$,1=U>-=w(B")
    (;; Misc Symbols
     ?$,1=A(B ?$,1=B(B ?$,1=C(B nil ?$,1>-(B nil nil)
    (;; Digits
     ?$,1>F(B ?$,1>G(B ?$,1>H(B ?$,1>I(B ?$,1>J(B ?$,1>K(B ?$,1>L(B ?$,1>M(B ?$,1>N(B ?$,1>O(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,1>-=p(B" "$,1=p>-(B" "$,1=d>-=p(B" "$,1=v>-=p(B" nil)))

(defvar indian-knd-base-table
  '(
    (;; VOWELS
     (?$,1>e(B nil) (?$,1>f(B ?$,1?>(B) (?$,1>g(B ?$,1??(B) (?$,1>h(B ?$,1?@(B) (?$,1>i(B ?$,1?A(B) (?$,1>j(B ?$,1?B(B)
     (?$,1>k(B ?$,1?C(B) (?$,1>l(B nil) nil (?$,1>o(B ?$,1?G(B) (?$,1>n(B ?$,1?F(B) (?$,1>p(B ?$,1?H(B)
     nil (?$,1>s(B ?$,1?K(B) (?$,1>r(B ?$,1?J(B) (?$,1>t(B ?$,1?L(B) (?$,1?`(B ?$,1?D(B) (?$,1?a(B nil))
    (;; CONSONANTS
     ?$,1>u(B ?$,1>v(B ?$,1>w(B ?$,1>x(B ?$,1>y(B                  ;; GUTTRULS
     ?$,1>z(B ?$,1>{(B ?$,1>|(B ?$,1>}(B ?$,1>~(B                  ;; PALATALS
     ?$,1>(B ?$,1? (B ?$,1?!(B ?$,1?"(B ?$,1?#(B                  ;; CEREBRALS
     ?$,1?$(B ?$,1?%(B ?$,1?&(B ?$,1?'(B ?$,1?((B nil              ;; DENTALS
     ?$,1?*(B ?$,1?+(B ?$,1?,(B ?$,1?-(B ?$,1?.(B                  ;; LABIALS
     ?$,1?/(B ?$,1?0(B ?$,1?1(B ?$,1?2(B ?$,1?3(B nil ?$,1?5(B          ;; SEMIVOWELS
     ?$,1?6(B ?$,1?7(B ?$,1?8(B ?$,1?9(B                    ;; SIBILANTS
     nil nil nil nil nil nil ?$,1?^(B nil      ;; NUKTAS
     "$,1>|?M>~(B" "$,1>u?M?7(B")
    (;; Misc Symbols
     nil ?$,1>b(B ?$,1>c(B nil ?$,1?M(B nil nil)
    (;; Digits
     ?$,1?f(B ?$,1?g(B ?$,1?h(B ?$,1?i(B ?$,1?j(B ?$,1?k(B ?$,1?l(B ?$,1?m(B ?$,1?n(B ?$,1?o(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,1?M?0(B" "$,1?0?M(B" "$,1?$?M?0(B" "$,1?6?M?0(B" nil)))

(defvar indian-mlm-base-table
  '(
    (;; VOWELS
     (?$,1@%(B nil) (?$,1@&(B ?$,1@^(B) (?$,1@'(B ?$,1@_(B) (?$,1@((B ?$,1@`(B) (?$,1@)(B ?$,1@a(B) (?$,1@*(B ?$,1@b(B)
     (?$,1@+(B ?$,1@c(B) (?$,1@,(B nil) nil (?$,1@/(B ?$,1@g(B) (?$,1@.(B ?$,1@f(B) (?$,1@0(B ?$,1@h(B)
     nil (?$,1@3(B ?$,1@k(B) (?$,1@2(B ?$,1@j(B) (?$,1@4(B ?$,1@l(B) nil nil)
    (;; CONSONANTS
     ?$,1@5(B ?$,1@6(B ?$,1@7(B ?$,1@8(B ?$,1@9(B                  ;; GUTTRULS
     ?$,1@:(B ?$,1@;(B ?$,1@<(B ?$,1@=(B ?$,1@>(B                  ;; PALATALS
     ?$,1@?(B ?$,1@@(B ?$,1@A(B ?$,1@B(B ?$,1@C(B                  ;; CEREBRALS
     ?$,1@D(B ?$,1@E(B ?$,1@F(B ?$,1@G(B ?$,1@H(B nil              ;; DENTALS
     ?$,1@J(B ?$,1@K(B ?$,1@L(B ?$,1@M(B ?$,1@N(B                  ;; LABIALS
     ?$,1@O(B ?$,1@P(B ?$,1@Q(B ?$,1@R(B ?$,1@S(B ?$,1@T(B ?$,1@U(B          ;; SEMIVOWELS
     ?$,1@V(B ?$,1@W(B ?$,1@X(B ?$,1@Y(B                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "$,1@<@m@>(B" "$,1@5@m@W(B")
    (;; Misc Symbols
     nil ?$,1@"(B ?$,1@#(B nil ?$,1@m(B nil nil)
    (;; Digits
     ?$,1A&(B ?$,1A'(B ?$,1A((B ?$,1A)(B ?$,1A*(B ?$,1A+(B ?$,1A,(B ?$,1A-(B ?$,1A.(B ?$,1A/(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,1@m@P(B" "$,1@P@m(B" "$,1@D@m@P(B" "$,1@V@m@P(B" nil)))

(defvar indian-tml-base-table
  '(
    (;; VOWELS
     (?$,1<%(B nil) (?$,1<&(B ?$,1<^(B) (?$,1<'(B ?$,1<_(B) (?$,1<((B ?$,1<`(B) (?$,1<)(B ?$,1<a(B) (?$,1<*(B ?$,1<b(B)
     nil nil nil (?$,1</(B ?$,1<g(B) (?$,1<.(B ?$,1<f(B) (?$,1<0(B ?$,1<h(B)
     nil (?$,1<3(B ?$,1<k(B) (?$,1<2(B ?$,1<j(B) (?$,1<4(B ?$,1<l(B) nil nil)
    (;; CONSONANTS
     ?$,1<5(B nil nil nil ?$,1<9(B                  ;; GUTTRULS
     ?$,1<:(B nil ?$,1<<(B nil ?$,1<>(B                  ;; PALATALS
     ?$,1<?(B nil nil nil ?$,1<C(B                  ;; CEREBRALS
     ?$,1<D(B nil nil nil ?$,1<H(B ?$,1<I(B              ;; DENTALS
     ?$,1<J(B nil nil nil ?$,1<N(B                  ;; LABIALS
     ?$,1<O(B ?$,1<P(B ?$,1<Q(B ?$,1<R(B ?$,1<S(B ?$,1<T(B ?$,1<U(B          ;; SEMIVOWELS
     nil ?$,1<W(B ?$,1<X(B ?$,1<Y(B                    ;; SIBILANTS
     nil nil nil nil nil nil nil nil      ;; NUKTAS
     "$,1<<<m<>(B" "$,1<5<m<W(B")
    (;; Misc Symbols
     nil ?$,1<"(B ?$,1<#(B nil ?$,1<m(B nil nil)
    (;; Digits
     ?$,1=&(B ?$,1='(B ?$,1=((B ?$,1=)(B ?$,1=*(B ?$,1=+(B ?$,1=,(B ?$,1=-(B ?$,1=.(B ?$,1=/(B)
    (;; Inscript-extra (4)  (#, $, ^, *, ])
     "$,1<m<P(B" "$,1<P<m(B" "$,1<D<m<P(B" nil nil)))

(defvar indian-base-table-to-language-alist
  '((indian-dev-base-table . "Devanagari")
    (indian-pnj-base-table . "Punjabi")
    (indian-ori-base-table . "Oriya")
    (indian-bng-base-table . "Bengali")
    (indian-asm-base-table . "Assamese")
    (indian-tlg-base-table . "Telugu")
    (indian-knd-base-table . "Kannada")
    (indian-mlm-base-table . "Malayalam")
    (indian-tml-base-table . "Tamil")))

(defvar indian-itrans-v5-table
  '(;; for encode/decode
    (;; vowels -- 18
     "a" ("aa" "A") "i" ("ii" "I") "u" ("uu" "U")
     ("RRi" "R^i") ("LLi" "L^i") (".c" "e.c") "E" "e" "ai"
     "o.c"  "O"   "o"   "au"  ("RRI" "R^I") ("LLI" "L^I"))
    (;; consonants -- 40
     "k"   "kh"  "g"   "gh"  ("~N" "N^")
     "ch" ("Ch" "chh") "j" "jh" ("~n" "JN")
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   "nh"
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   "rh"  "l"   ("L" "ld") nil  ("v" "w")
     "sh" ("Sh" "shh") "s" "h"
     "q" "K" "G" ("J" "z") ".D" ".Dh" "f" ("Y" "yh")
     ("GY" "dny") "x")
    (;; misc -- 7
     ".N" (".n" "M") "H" ".a" ".h" ("AUM" "OM") "..")))

(defvar indian-itrans-v5-table-for-tamil
  '(;; for encode/decode
    (;; vowels -- 18
     "a" ("aa" "A") "i" ("ii" "I") "u" ("uu" "U")
     ("RRi" "R^i") ("LLi" "L^i") (".c" "e.c") "E" "e" "ai"
     "o.c"  "O"   "o"   "au"  ("RRI" "R^I") ("LLI" "L^I"))
    (;; consonants -- 40
     "k"   "kh"  "g"   "gh"  ("~N" "N^")
     "ch" ("Ch" "chh") "j" "jh" ("~n" "JN")
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   "nh"
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   "rh"  "l"   ("L" "ld") ("J" "z")  ("v" "w")
     "sh" ("Sh" "shh") "s" "h"
     "q" "K" "G" nil ".D" ".Dh" "f" ("Y" "yh")
     ("GY" "dny") "x")
    (;; misc -- 7
     ".N" (".n" "M") "H" ".a" ".h" ("AUM" "OM") "..")))

(defvar indian-kyoto-harvard-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("q" "RR" "Q")   ("E" "LL" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("z" "Z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-harvard-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("RR" "q" "Q")   ("LL" "E" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("z" "Z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-tokyo-table
  '(;; for encode/decode
    (;; vowel
     "a"   ("A" "aa")  "i"   ("I" "ii")  "u"   ("U" "uu")
     "R"   ("L" "lR")  nil   nil   "e"   "ai"
     nil   nil   "o"   "au"  ("Q" "RR" "q")   ("E" "LL" "lRR"))
    (;; consonant
     "k"   "kh"  "g"   "gh"  "G"
     "c"   "ch"  "j"   "jh"  "J"
     "T"   "Th"  "D"   "Dh"  "N"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   "L"   nil   "v"
     ("Z" "z")   "S"   "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   "M"   "H"   "'"   nil   "." nil)))

(defvar indian-aiba-table
  '(;; for encode/decode
    (;; vowel
     "a"   "aa"  "i"   "ii"  "u"   "uu"
     ".r"  ".l"   nil   nil  "e"   "ai"
     nil   nil   "o"   "au"  "~r"  "~l")
    (;; consonant
     "k"   "kh"  "g"   "gh"  "^n"
     "c"   "ch"  "j"   "jh"  "~n"
     ".t"  ".th" ".d"  ".dh" ".n"
     "t"   "th"  "d"   "dh"  "n"   nil
     "p"   "ph"  "b"   "bh"  "m"
     "y"   "r"   nil   "l"   nil  nil  "v"
     "^s"  ".s"  "s"   "h"
     nil   nil   nil   nil   nil   nil   nil   nil
     nil   nil)
    (;; misc
     nil   ".m"  ".h"  "'"   nil   "." nil)))

(defun combinatorial (head &rest tail)
  (if tail
      (apply 'append
	     (mapcar (lambda (y) (mapcar (lambda (x) (cons x y)) head))
		     (apply 'combinatorial tail)))
    (mapcar 'list head)))

(defun indian--puthash-char (char trans-char hashtbls)
  (let ((encode-hash (car hashtbls))  ;; char -> trans
	(decode-hash (cdr hashtbls))  ;; trans -> char
	)
    ;; char -- nil / char / string (/ list of vowel & matra)
    ;; trans-char -- nil / string / list of strings
    (when (and char trans-char)
      (if (stringp trans-char) (setq trans-char (list trans-char)))
      (if (characterp char) (setq char (char-to-string char)))
      (puthash char (car trans-char) encode-hash)
      (dolist (trans trans-char)
	 (puthash trans char decode-hash)))))

(defun indian--map (f l1 l2)
  (while l1
    (funcall f (pop l1) (pop l2))))

(defun indian--puthash-v (v trans-v hashtbls)
  (indian--map
   (lambda (v trans-v)
     (indian--puthash-char (car v) trans-v hashtbls))
   v trans-v))

(defun indian--puthash-c (c trans-c halant hashtbls)
  (indian--map
   (lambda (c trans-c)
     (if (characterp c) (setq c (char-to-string c)))
     (indian--puthash-char (concat c halant) trans-c hashtbls))
   c trans-c))

(defun indian--puthash-m (m trans-m hashtbls)
  (indian--map
   (lambda (m trans-m)
     (indian--puthash-char m trans-m hashtbls))
   m trans-m))

(defun indian--puthash-cv (c trans-c v trans-v hashtbls)
  (indian--map
   (lambda (c trans-c)
     (indian--map
      (lambda (v trans-v)
	(when (and c trans-c  v trans-v)
	  (if (characterp c) (setq c (char-to-string c)))
	  (setq v (if (characterp (cadr v)) (char-to-string (cadr v)) ""))
	  (if (stringp trans-c) (setq trans-c (list trans-c)))
	  (if (stringp trans-v) (setq trans-v (list trans-v)))
	  (indian--puthash-char
	   (concat c v)
	   (mapcar (lambda (x) (apply 'concat x))
		  (combinatorial trans-c trans-v))
	   hashtbls)))
      v trans-v))
   c trans-c))

(defun indian-make-hash (table trans-table)
  "Indian Transliteration Hash for decode/encode"
  (let* ((encode-hash (make-hash-table :test 'equal))
	 (decode-hash (make-hash-table :test 'equal))
	 (hashtbls (cons encode-hash decode-hash))
	 (vowels     (elt table 0))
	 (consonants (elt table 1))
	 (misc       (elt table 2))
	 (digits     (elt table 3))
	 (halant     (char-to-string (elt misc  4)))
	 (trans-vowels     (elt trans-table 0))
	 (trans-consonants (elt trans-table 1))
	 (trans-misc       (elt trans-table 2))
	 (trans-digits  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (indian--puthash-v vowels trans-vowels hashtbls)
    (indian--puthash-c consonants trans-consonants halant hashtbls)
    (indian--puthash-cv consonants trans-consonants
			      vowels trans-vowels hashtbls)
    (indian--puthash-m misc trans-misc hashtbls)
    (indian--puthash-m digits trans-digits hashtbls)
    hashtbls))

(defvar indian-dev-itrans-v5-hash
  (indian-make-hash indian-dev-base-table
			  indian-itrans-v5-table))
(defvar indian-dev-kyoto-harvard-hash
  (indian-make-hash indian-dev-base-table
			  indian-kyoto-harvard-table))
(defvar indian-dev-aiba-hash
  (indian-make-hash indian-dev-base-table
			  indian-aiba-table))

(defvar indian-pnj-itrans-v5-hash
  (indian-make-hash indian-pnj-base-table
			  indian-itrans-v5-table))

(defvar indian-gjr-itrans-v5-hash
  (indian-make-hash indian-gjr-base-table
			  indian-itrans-v5-table))

(defvar indian-ori-itrans-v5-hash
  (indian-make-hash indian-ori-base-table
			  indian-itrans-v5-table))

(defvar indian-bng-itrans-v5-hash
  (indian-make-hash indian-bng-base-table
			  indian-itrans-v5-table))

(defvar indian-asm-itrans-v5-hash
  (indian-make-hash indian-asm-base-table
			  indian-itrans-v5-table))

(defvar indian-tlg-itrans-v5-hash
  (indian-make-hash indian-tlg-base-table
			  indian-itrans-v5-table))

(defvar indian-knd-itrans-v5-hash
  (indian-make-hash indian-knd-base-table
			  indian-itrans-v5-table))

(defvar indian-mlm-itrans-v5-hash
  (indian-make-hash indian-mlm-base-table
			  indian-itrans-v5-table))

(defvar indian-tml-itrans-v5-hash
  (indian-make-hash indian-tml-base-table
			  indian-itrans-v5-table-for-tamil))
)

(defmacro indian-translate-region (from to hashtable encode-p)
  `(save-excursion
     (save-restriction
       (let ((regexp ,(indian-regexp-of-hashtbl-keys
		       (if encode-p (car (eval hashtable))
			 (cdr (eval hashtable))))))
	 (narrow-to-region from to)
	 (goto-char (point-min))
	 (while (re-search-forward regexp nil t)
	   (let ((matchstr (gethash (match-string 0)
				    (if ,encode-p
					(car ,hashtable)
				      (cdr ,hashtable)))))
	     (if matchstr (replace-match matchstr))))))))

;;;

(defun indian-dev-itrans-v5-encode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-itrans-v5-hash t))

(defun indian-dev-itrans-v5-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-itrans-v5-hash nil))

(defun indian-dev-kyoto-harvard-encode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-kyoto-harvard-hash t))

(defun indian-dev-kyoto-harvard-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-kyoto-harvard-hash nil))

(defun indian-dev-aiba-encode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-aiba-hash t))

(defun indian-dev-aiba-decode-region (from to)
  (interactive "r")
  (indian-translate-region
   from to indian-dev-aiba-hash nil))




;;; IS 13194 utilities

;; The followings provide conversion between IS 13194 (ISCII) and UCS.

(let
    ;;Unicode vs IS13194  ;; only Devanagari is supported now.
    ((ucs-devanagari-to-is13194-alist
      '((?\x0900 . "[U+0900]")
	(?\x0901 . "(5!(B")
	(?\x0902 . "(5"(B")
	(?\x0903 . "(5#(B")
	(?\x0904 . "[U+0904]")
	(?\x0905 . "(5$(B")
	(?\x0906 . "(5%(B")
	(?\x0907 . "(5&(B")
	(?\x0908 . "(5'(B")
	(?\x0909 . "(5((B")
	(?\x090a . "(5)(B")
	(?\x090b . "(5*(B")
	(?\x090c . "(5&i(B")
	(?\x090d . "(5.(B")
	(?\x090e . "(5+(B")
	(?\x090f . "(5,(B")
	(?\x0910 . "(5-(B")
	(?\x0911 . "(52(B")
	(?\x0912 . "(5/(B")
	(?\x0913 . "(50(B")
	(?\x0914 . "(51(B")
	(?\x0915 . "(53(B")
	(?\x0916 . "(54(B")
	(?\x0917 . "(55(B")
	(?\x0918 . "(56(B")
	(?\x0919 . "(57(B")
	(?\x091a . "(58(B")
	(?\x091b . "(59(B")
	(?\x091c . "(5:(B")
	(?\x091d . "(5;(B")
	(?\x091e . "(5<(B")
	(?\x091f . "(5=(B")
	(?\x0920 . "(5>(B")
	(?\x0921 . "(5?(B")
	(?\x0922 . "(5@(B")
	(?\x0923 . "(5A(B")
	(?\x0924 . "(5B(B")
	(?\x0925 . "(5C(B")
	(?\x0926 . "(5D(B")
	(?\x0927 . "(5E(B")
	(?\x0928 . "(5F(B")
	(?\x0929 . "(5G(B")
	(?\x092a . "(5H(B")
	(?\x092b . "(5I(B")
	(?\x092c . "(5J(B")
	(?\x092d . "(5K(B")
	(?\x092e . "(5L(B")
	(?\x092f . "(5M(B")
	(?\x0930 . "(5O(B")
	(?\x0931 . "(5P(B")
	(?\x0932 . "(5Q(B")
	(?\x0933 . "(5R(B")
	(?\x0934 . "(5S(B")
	(?\x0935 . "(5T(B")
	(?\x0936 . "(5U(B")
	(?\x0937 . "(5V(B")
	(?\x0938 . "(5W(B")
	(?\x0939 . "(5X(B")
	(?\x093a . "[U+093a]")
	(?\x093b . "[U+093b]")
	(?\x093c . "(5i(B")
	(?\x093d . "(5ji(B")
	(?\x093e . "(5Z(B")
	(?\x093f . "(5[(B")
	(?\x0940 . "(5\(B")
	(?\x0941 . "(5](B")
	(?\x0942 . "(5^(B")
	(?\x0943 . "(5_(B")
	(?\x0944 . "(5_i(B")
	(?\x0945 . "(5c(B")
	(?\x0946 . "(5`(B")
	(?\x0947 . "(5a(B")
	(?\x0948 . "(5b(B")
	(?\x0949 . "(5g(B")
	(?\x094a . "(5d(B")
	(?\x094b . "(5e(B")
	(?\x094c . "(5f(B")
	(?\x094d . "(5h(B")
	(?\x094e . "[U+094e]")
	(?\x094f . "[U+094f]")
	(?\x0950 . "(5!i(B")
	(?\x0951 . "(5p5(B")
	(?\x0952 . "(5p8(B")
	(?\x0953 . "[DEVANAGARI GRAVE ACCENT]")
	(?\x0954 . "[DEVANAGARI ACUTE ACCENT]")
	(?\x0955 . "[U+0955]")
	(?\x0956 . "[U+0956]")
	(?\x0957 . "[U+0957]")
	(?\x0958 . "(53i(B")
	(?\x0959 . "(54i(B")
	(?\x095a . "(55i(B")
	(?\x095b . "(5:i(B")
	(?\x095c . "(5?i(B")
	(?\x095d . "(5@i(B")
	(?\x095e . "(5Ii(B")
	(?\x095f . "(5N(B")
	(?\x0960 . "(5*i(B")
	(?\x0961 . "(5'i(B")
	(?\x0962 . "(5[i(B")
	(?\x0963 . "(5ei(B")
	(?\x0964 . "(5j(B")
	(?\x0965 . "(5jj(B")
	(?\x0966 . "(5q(B")
	(?\x0967 . "(5r(B")
	(?\x0968 . "(5s(B")
	(?\x0969 . "(5t(B")
	(?\x096a . "(5u(B")
	(?\x096b . "(5v(B")
	(?\x096c . "(5w(B")
	(?\x096d . "(5x(B")
	(?\x096e . "(5y(B")
	(?\x096f . "(5z(B")
	(?\x0970 . "[U+0970]")
	(?\x0971 . "[U+0971]")
	(?\x0972 . "[U+0972]")
	(?\x0973 . "[U+0973]")
	(?\x0974 . "[U+0974]")
	(?\x0975 . "[U+0975]")
	(?\x0976 . "[U+0976]")
	(?\x0977 . "[U+0977]")
	(?\x0978 . "[U+0978]")
	(?\x0979 . "[U+0979]")
	(?\x097a . "[U+097a]")
	(?\x097b . "[U+097b]")
	(?\x097c . "[U+097c]")
	(?\x097d . "[U+097d]")
	(?\x097e . "[U+097e]")
	(?\x097f . "[U+097f]")))
     (ucs-bengali-to-is13194-alist nil)
     (ucs-assamese-to-is13194-alist nil)
     (ucs-gurmukhi-to-is13194-alist nil)
     (ucs-gujarati-to-is13194-alist nil)
     (ucs-oriya-to-is13194-alist nil)
     (ucs-tamil-to-is13194-alist nil)
     (ucs-telugu-to-is13194-alist nil)
     (ucs-malayalam-to-is13194-alist nil)
     (ucs-kannada-to-is13194-alist nil))
  (dolist (script '(devanagari bengali assamese gurmukhi gujarati
		    oriya tamil telugu malayalam kannada))
   (let ((hashtable (intern (concat "is13194-to-ucs-"
				    (symbol-name script) "-hashtbl" )))
	 (regexp    (intern (concat "is13194-to-ucs-"
				    (symbol-name script) "-regexp"))))
     (set hashtable (make-hash-table :test 'equal :size 128))
     (dolist (x (eval (intern (concat "ucs-" (symbol-name script)
				      "-to-is13194-alist"))))
       (put-char-code-property (car x) 'script script)
       (put-char-code-property (car x) 'iscii (cdr x))
       (puthash (cdr x) (char-to-string (car x)) (eval hashtable)))
      (set regexp (indian-regexp-of-hashtbl-keys (eval hashtable))))))

(defvar is13194-default-repertory 'devanagari)

(defvar is13194-repertory-to-ucs-script
  `((DEF ?\x40 ,is13194-default-repertory)
    (RMN ?\x41 ,is13194-default-repertory)
    (DEV ?\x42 devanagari)
    (BNG ?\x43 bengali)
    (TML ?\x44 tamil)
    (TLG ?\x45 telugu)
    (ASM ?\x46 bengali)
    (ORI ?\x47 oriya)
    (KND ?\x48 kannada)
    (MLM ?\x49 malayalam)
    (GJR ?\x4a gujarati)
    (PNJ ?\x4b gurmukhi)))

;; for guiding find-variable function.
(defvar is13194-to-ucs-devanagari-hashtbl nil)
(defvar is13194-to-ucs-devanagari-regexp nil)
(defvar is13194-to-ucs-bengali-hashtbl nil)
(defvar is13194-to-ucs-bengali-regexp nil)
(defvar is13194-to-ucs-assamese-hashtbl nil)
(defvar is13194-to-ucs-assamese-regexp nil)
(defvar is13194-to-ucs-gurmukhi-hashtbl nil)
(defvar is13194-to-ucs-gurmukhi-regexp nil)
(defvar is13194-to-ucs-gujarati-hashtbl nil)
(defvar is13194-to-ucs-gujarati-regexp nil)
(defvar is13194-to-ucs-oriya-hashtbl nil)
(defvar is13194-to-ucs-oriya-regexp nil)
(defvar is13194-to-ucs-tamil-hashtbl nil)
(defvar is13194-to-ucs-tamil-regexp nil)
(defvar is13194-to-ucs-telugu-hashtbl nil)
(defvar is13194-to-ucs-telugu-regexp nil)
(defvar is13194-to-ucs-malayalam-hashtbl nil)
(defvar is13194-to-ucs-malayalam-regexp nil)
(defvar is13194-to-ucs-kannada-hashtbl nil)
(defvar is13194-to-ucs-kannada-regexp nil)

(defvar ucs-to-is13194-regexp
  ;; only Devanagari is supported now.
  (concat "[" (char-to-string #x0900)
          "-" (char-to-string #x097f) "]")
  "Regexp that matches to conversion")

(defun ucs-to-iscii-region (from to)
  "Converts the indian UCS characters in the region to ISCII.
Returns new end position."
  (interactive "r")
  ;; only Devanagari is supported now.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((current-repertory is13194-default-repertory))
	(while (re-search-forward ucs-to-is13194-regexp nil t)
	  (replace-match
	   (get-char-code-property (string-to-char (match-string 0))
				   'iscii))))
      (point-max))))

(defun iscii-to-ucs-region (from to)
  "Converts the ISCII characters in the region to UCS.
Returns new end position."
  (interactive "r")
  ;; only Devanagari is supported now.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((current-repertory is13194-default-repertory)
	     (current-hashtable
	      (intern (concat "is13194-to-ucs-"
			      (symbol-name current-repertory) "-hashtbl")))
	     (current-regexp
	      (intern (concat "is13194-to-ucs-"
			      (symbol-name current-repertory) "-regexp")))
	     (re (eval current-regexp))
	     (hash (eval current-hashtable)))
	(while (re-search-forward re nil t)
	  (replace-match (gethash (match-string 0) hash ""))))
      (point-max))))

;;;###autoload
(defun indian-compose-region (from to)
  "Compose the region according to `composition-function-table'."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((pos from) newpos func (max to))
	(narrow-to-region from to)
	(while (< pos max)
	  (setq func (aref composition-function-table (char-after pos)))
	  (if (fboundp func)
	      (setq newpos (funcall func pos nil)
		    pos (if (and (integerp newpos) (> newpos pos))
			    newpos (1+ pos)))
	    (setq pos (1+ pos))))))))

;;;###autoload
(defun indian-compose-string (string)
  (with-temp-buffer
    (insert string)
    (indian-compose-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun in-is13194-post-read-conversion (len)
  (let ((pos (point)) endpos)
    (setq endpos (iscii-to-ucs-region pos (+ pos len)))
    (- endpos pos)))

;;;###autoload
(defun in-is13194-pre-write-conversion (from to)
  (let ((buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    (ucs-to-iscii-region (point-min) (point-max))
    nil))




;;; Backward Compatibility support programs

;; The following provides the conversion from old-implementation of
;; Emacs Devanagari script to UCS.

(defconst indian-2-colum-to-ucs
  '(
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2120   $(6!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/(B
  ("$(6!!(B" . "$,15A(B")
  ("$(6!"(B" . "$,15B(B")
  ("$(6!#(B" . "$,15C(B")
  ("$(6!$(B" . "$,15E(B")
  ("$(6!%(B" . "$,15F(B")
  ("$(6!&(B" . "$,15G(B")
  ("$(6!'(B" . "$,15H(B")
  ("$(6!((B" . "$,15I(B")
  ("$(6!)(B" . "$,15J(B")
  ("$(6!*(B" . "$,15K(B")
  ("$(6!*"p(B" . "$,15p6#(B")
  ("$(6!+(B" . "$,15N(B")
  ("$(6!,(B" . "$,15O(B")
  ("$(6!-(B" . "$,15P(B")
  ("$(6!.(B" . "$,15M(B")
  ("$(6!/(B" . "$,15R(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2130 $(6!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B
  ("$(6!0(B" . "$,15S(B")
  ("$(6!1(B" . "$,15T(B")
  ("$(6!2(B" . "$,15Q(B")
  ("$(6!3(B" . "$,15U(B")
  ("$(6!4(B" . "$,15V(B")
  ("$(6!5(B" . "$,15W(B")
  ("$(6!6(B" . "$,15X(B")
  ("$(6!7(B" . "$,15Y(B")
  ("$(6!8(B" . "$,15Z(B")
  ("$(6!9(B" . "$,15[(B")
  ("$(6!:(B" . "$,15\(B")
  ("$(6!;(B" . "$,15](B")
  ("$(6!<(B" . "$,15^(B")
  ("$(6!=(B" . "$,15_(B")
  ("$(6!>(B" . "$,15`(B")
  ("$(6!?(B" . "$,15a(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2140 $(6!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O(B
  ("$(6!@(B" . "$,15b(B")
  ("$(6!A(B" . "$,15c(B")
  ("$(6!B(B" . "$,15d(B")
  ("$(6!C(B" . "$,15e(B")
  ("$(6!D(B" . "$,15f(B")
  ("$(6!E(B" . "$,15g(B")
  ("$(6!F(B" . "$,15h(B")
  ("$(6!G(B" . "$,15i(B")
  ("$(6!H(B" . "$,15j(B")
  ("$(6!I(B" . "$,15k(B")
  ("$(6!J(B" . "$,15l(B")
  ("$(6!K(B" . "$,15m(B")
  ("$(6!L(B" . "$,15n(B")
  ("$(6!M(B" . "$,15o(B")
  ("$(6!N(B" . "$,16?(B")
  ("$(6!O(B" . "$,15p(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2150 $(6!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_(B
  ("$(6!P(B" . "$,15q(B")
  ("$(6!Q(B" . "$,15r(B")
  ("$(6!R(B" . "$,15s(B")
  ("$(6!S(B" . "$,15t(B")
  ("$(6!T(B" . "$,15u(B")
  ("$(6!U(B" . "$,15v(B")
  ("$(6!V(B" . "$,15w(B")
  ("$(6!W(B" . "$,15x(B")
  ("$(6!X(B" . "$,15y(B")
  ("$(6!Z(B" . "$,15~(B")
  ("$(6![(B" . "$,15(B")
  ("$(6!\(B" . "$,16 (B")
  ("$(6!](B" . "$,16!(B")
  ("$(6!^(B" . "$,16"(B")
  ("$(6!_(B" . "$,16#(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2160 $(6!`!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o(B
  ("$(6!`(B" . "$,16&(B")
  ("$(6!a(B" . "$,16'(B")
  ("$(6!b(B" . "$,16((B")
  ("$(6!c(B" . "$,16%(B")
  ("$(6!d(B" . "$,16*(B")
  ("$(6!e(B" . "$,16+(B")
  ("$(6!f(B" . "$,16,(B")
  ("$(6!g(B" . "$,16)(B")
  ("$(6!h(B" . "$,16-(B")
  ("$(6!i(B" . "$,15|(B")
  ("$(6!j(B" . "$,16D(B")
  ("$(6!j!j(B" . "$,16E(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2170 $(6!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B
  ("$(6!q(B" . "$,16F(B")
  ("$(6!r(B" . "$,16G(B")
  ("$(6!s(B" . "$,16H(B")
  ("$(6!t(B" . "$,16I(B")
  ("$(6!u(B" . "$,16J(B")
  ("$(6!v(B" . "$,16K(B")
  ("$(6!w(B" . "$,16L(B")
  ("$(6!x(B" . "$,16M(B")
  ("$(6!y(B" . "$,16N(B")
  ("$(6!z(B" . "$,16O(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2220   $(6"!"""#"$"%"&"'"(")"*"+","-"."/(B
  ("$(6"!(B" . "$,16;6-5p(B")
  ("$(6""(B" . "$,16>6-5p(B")
  ("$(6"#(B" . "$,15U6-5p(B")
  ("$(6"$(B" . "$,15W6-5p(B")
  ("$(6"%(B" . "$,15d6-5p(B")
  ("$(6"&(B" . "$,15j6-5p(B")
  ("$(6"'(B" . "$,15k6-5p(B")
  ("$(6")(B" . "$,15v6-5p(B")
  ("$(6",(B" . "$,15p6!(B")
  ("$(6"-(B" . "$,15p6"(B")
  ("$(6".(B" . "$,15q6!(B")
  ("$(6"/(B" . "$,15q6"(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2230 $(6"0"1"2"3"4"5"6"7"8"9":";"<"=">"?(B
  ("$(6"3(B" . "$,15U6-(B")
  ("$(6"4(B" . "$,15V6-(B")
  ("$(6"5(B" . "$,15W6-(B")
  ("$(6"6(B" . "$,15X6-(B")
  ("$(6"8(B" . "$,15Z6-(B")
  ("$(6"8"q(B" . "$,15Z6-5p6-(B")
  ("$(6":(B" . "$,15\6-(B")
  ("$(6";(B" . "$,15]6-(B")
  ("$(6"<(B" . "$,15^6-(B")
  ("$(6"<(B" . "$,15^6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2240 $(6"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O(B
  ("$(6"A(B" . "$,15c6-(B")
  ("$(6"B(B" . "$,15d6-(B")
  ("$(6"C(B" . "$,15e6-(B")
  ("$(6"E(B" . "$,15g6-(B")
  ("$(6"F(B" . "$,15h6-(B")
  ("$(6"G(B" . "$,15i6-(B")
  ("$(6"H(B" . "$,15j6-(B")
  ("$(6"I(B" . "$,15k6-(B")
  ("$(6"J(B" . "$,15l6-(B")
  ("$(6"J(B" . "$,15l6-(B")
  ("$(6"K(B" . "$,15m6-(B")
  ("$(6"L(B" . "$,15n6-(B")
  ("$(6"M(B" . "$,15o6-(B")
  ("$(6"N(B" . "$,16?6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2250 $(6"P"Q"R"S"T"U"V"W"X"Y"Z"["\"]"^"_(B
  ("$(6"Q(B" . "$,15r6-(B")
  ("$(6"R(B" . "$,15s6-(B")
  ("$(6"S(B" . "$,15t6-(B")
  ("$(6"T(B" . "$,15u6-(B")
  ("$(6"U(B" . "$,15v6-(B")
  ("$(6"V(B" . "$,15w6-(B")
  ("$(6"W(B" . "$,15x6-(B")
  ("$(6"](B" . "$,16-5o(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2260 $(6"`"a"b"c"d"e"f"g"h"i"j"k"l"m"n"o(B
  ("$(6"`(B" . "$,15W6-5p6-(B")
  ("$(6"a(B" . "$,15X6-5h6-(B")
  ("$(6"c(B" . "$,15d6-5d6-(B")
  ("$(6"d(B" . "$,15d6-5p6-(B")
  ("$(6"e(B" . "$,15g6-5h6-(B")
  ("$(6"f(B" . "$,15g6-5p6-(B")
  ("$(6"g(B" . "$,15j6-5d6-(B")
  ("$(6"h(B" . "$,15v6-5Z6-(B")
  ("$(6"i(B" . "$,15v6-5p6-(B")
  ("$(6"j(B" . "$,15v6-5u6-(B")
  ("$(6"k(B" . "$,15h6-5h6-(B")
  ("$(6"l(B" . "$,15U6-5w6-(B")
  ("$(6"m(B" . "$,15\6-5^6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2270 $(6"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~(B
  ("$(6"p(B" . "$,15p6-(B")
  ("$(6"q(B" . "$,16-5p(B")
  ("$(6"r(B" . "$,16-5p(B")
  ("$(6"s(B" . "$,1686-(B")
  ("$(6"t(B" . "$,1696-(B")
  ("$(6"u(B" . "$,16:6-(B")
  ("$(6"y(B" . "$,16>6-(B")
  ("$(6"z(B" . "$,16;6-(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2320   $(6#!#"###$#%#&#'#(#)#*#+#,#-#.#/(B
  ("$(6#!(B" . "$,160(B")
  ("$(6#&(B" . "$,15L(B")
  ("$(6#&"p(B" . "$,15p6$(B")
  ("$(6#'(B" . "$,16A(B")
  ("$(6#'"p(B" . "$,15p6C(B")
  ("$(6#*(B" . "$,16@(B")
  ("$(6#*"p(B" . "$,15p6B(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2330 $(6#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?(B
  ("$(6#3(B" . "$,168(B")
  ("$(6#4(B" . "$,169(B")
  ("$(6#5(B" . "$,16:(B")
  ("$(6#:(B" . "$,16;(B")
  ("$(6#?(B" . "$,16<(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2340 $(6#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O(B
  ("$(6#@(B" . "$,16=(B")
  ("$(6#I(B" . "$,16>(B")
  ("$(6#J(B" . "$,15}(B")
  ("$(6#K(B" . "$,16$(B")
  ("$(6#L(B" . "$,16B(B")
  ("$(6#M(B" . "$,16C(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2350 $(6#P#Q#R#S#T#U#V#W#X#Y#Z#[#\#]#^#_(B
  ("$(6#P(B" . "$,15n6-5h(B")
  ("$(6#Q(B" . "$,15n6-5r(B")
  ("$(6#R(B" . "$,15y6#(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2360 $(6#`#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o(B
  ("$(6#`(B" . "$,15r6-5r(B")
  ("$(6#a(B" . "$,15u6-5h(B")
  ("$(6#b(B" . "$,15u6-5u(B")
  ("$(6#c(B" . "$,15v6-5Z(B")
  ("$(6#d(B" . "$,15v6-5h(B")
  ("$(6#e(B" . "$,15v6-5l(B")
  ("$(6#f(B" . "$,15v6-5r(B")
  ("$(6#g(B" . "$,15v6-5u(B")
  ("$(6#h(B" . "$,15w6-5_6-5p6-5o(B")
  ("$(6#i(B" . "$,15w6-5_6-5o(B")
  ("$(6#j(B" . "$,15w6-5_6-5u(B")
  ("$(6#k(B" . "$,15w6-5_(B")
  ("$(6#l(B" . "$,15w6-5`(B")
  ("$(6#m(B" . "$,15x6-5h(B")
  ("$(6#n(B" . "$,15x6-5p(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2370 $(6#p#q#r#s#t#u#v#w#x#y#z#{#|#}#~(B
  ("$(6#p(B" . "$,15y6-5c(B")
  ("$(6#q(B" . "$,15y6-5h(B")
  ("$(6#r(B" . "$,15y6-5n(B")
  ("$(6#s(B" . "$,15y6-5o(B")
  ("$(6#t(B" . "$,15y6-5p(B")
  ("$(6#u(B" . "$,15y6-5r(B")
  ("$(6#v(B" . "$,15y6-5u(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2420   $(6$!$"$#$$$%$&$'$($)$*$+$,$-$.$/(B
  ("$(6$!(B" . "$,15U6-5d6-5p6-5o(B")
  ("$(6$"(B" . "$,15U6-5d6-5u(B")
  ("$(6$#(B" . "$,15U6-5d6-5o(B")
  ("$(6$$(B" . "$,15U6-5h6-5o(B")
  ("$(6$%(B" . "$,15U6-5p6-5o(B")
  ("$(6$&(B" . "$,15U6-5u6-5o(B")
  ("$(6$'(B" . "$,15U6-5U(B")
  ("$(6$((B" . "$,15U6-5d(B")
  ("$(6$)(B" . "$,15U6-5h(B")
  ("$(6$*(B" . "$,15U6-5n(B")
  ("$(6$+(B" . "$,15U6-5o(B")
  ("$(6$,(B" . "$,15U6-5r(B")
  ("$(6$-(B" . "$,15U6-5u(B")
  ("$(6$.(B" . "$,15U6-5w(B")
  ("$(6$/(B" . "$,15X6-5h(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2430 $(6$0$1$2$3$4$5$6$7$8$9$:$;$<$=$>$?(B
  ("$(6$0(B" . "$,15Y6-5U6-5d6-5o(B")
  ("$(6$1(B" . "$,15Y6-5U6-5w6-5u(B")
  ("$(6$2(B" . "$,15Y6-5U6-5d(B")
  ("$(6$3(B" . "$,15Y6-5U6-5w(B")
  ("$(6$4(B" . "$,15Y6-5X6-5p(B")
  ("$(6$5(B" . "$,15Y6-5U6-5o(B")
  ("$(6$6(B" . "$,15Y6-5V6-5o(B")
  ("$(6$7(B" . "$,15Y6-5W6-5o(B")
  ("$(6$8(B" . "$,15Y6-5X6-5o(B")
  ("$(6$9(B" . "$,15Y6-5U(B")
  ("$(6$:(B" . "$,15Y6-5V(B")
  ("$(6$;(B" . "$,15Y6-5W(B")
  ("$(6$<(B" . "$,15Y6-5X(B")
  ("$(6$=(B" . "$,15Y6-5Y(B")
  ("$(6$>(B" . "$,15Y6-5h(B")
  ("$(6$?(B" . "$,15Y6-5n(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2440 $(6$@$A$B$C$D$E$F$G$H$I$J$K$L$M$N$O(B
  ("$(6$@(B" . "$,15Y6-5o(B")
  ("$(6$A(B" . "$,15Z6-5Z(B")
  ("$(6$B(B" . "$,15Z6-5^(B")
  ("$(6$C(B" . "$,15[6-5o(B")
  ("$(6$D(B" . "$,15\6-5p(B")
  ("$(6$E(B" . "$,15\6-5^(B")
  ("$(6$F(B" . "$,15^6-5Z(B")
  ("$(6$G(B" . "$,15^6-5\(B")
  ("$(6$H(B" . "$,15_6-5U(B")
  ("$(6$I(B" . "$,15_6-5_(B")
  ("$(6$J(B" . "$,15_6-5`(B")
  ("$(6$K(B" . "$,15_6-5o(B")
  ("$(6$L(B" . "$,15`6-5o(B")
  ("$(6$M(B" . "$,15a6-5W6-5o(B")
  ("$(6$N(B" . "$,15a6-5X6-5p(B")
  ("$(6$O(B" . "$,15a6-5p6-5o(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2450 $(6$P$Q$R$S$T$U$V$W$X$Y$Z$[$\$]$^$_(B
  ("$(6$P(B" . "$,15a6-5W(B")
  ("$(6$Q(B" . "$,15a6-5X(B")
  ("$(6$R(B" . "$,15a6-5a(B")
  ("$(6$S(B" . "$,15a6-5n(B")
  ("$(6$T(B" . "$,15a6-5o(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2460 $(6$`$a$b$c$d$e$f$g$h$i$j$k$l$m$n$o(B
  ("$(6$`(B" . "$,15b6-5o(B")
  ("$(6$a(B" . "$,15d6-5d(B")
  ("$(6$b(B" . "$,15d6-5h(B")
  ("$(6$c(B" . "$,15f6-5f6-5o(B")
  ("$(6$d(B" . "$,15f6-5g6-5o(B")
  ("$(6$e(B" . "$,15f6-5m6-5o(B")
  ("$(6$f(B" . "$,15f6-5p6-5o(B")
  ("$(6$g(B" . "$,15f6-5u6-5o(B")
  ("$(6$h(B" . "$,15f6-5W6-5p(B")
  ("$(6$i(B" . "$,15f6-5X6-5p(B")
  ("$(6$j(B" . "$,15f6-5f6-5u(B")
  ("$(6$k(B" . "$,15f6-5g6-5u(B")
  ("$(6$l(B" . "$,15f6-5W(B")
  ("$(6$m(B" . "$,15f6-5X(B")
  ("$(6$n(B" . "$,15f6-5f(B")
  ("$(6$o(B" . "$,15f6-5g(B")
  ;;      0 1 2 3 4 5 6 7 8 9 a b c d e f
  ;;2470 $(6$p$q$r$s$t$u$v$w$x$y$z${$|$}$~(B
  ("$(6$p(B" . "$,15f6-5h(B")
  ("$(6$q(B" . "$,15f6-5l(B")
  ("$(6$r(B" . "$,15f6-5m(B")
  ("$(6$s(B" . "$,15f6-5n(B")
  ("$(6$t(B" . "$,15f6-5o(B")
  ("$(6$u(B" . "$,15f6-5u(B")
  ("$(6$v(B" . "$,15g6-5h(B")
  ("$(6$w(B" . "$,15h6-5h(B")
  ("$(6$x(B" . "$,15j6-5d(B")
  ("$(6$y(B" . "$,15j6-5h(B")
  ("$(6$z(B" . "$,15j6-5r(B")
  ("$(6${(B" . "$,15l6-5h(B")
  ("$(6$|(B" . "$,15l6-5l(B")
  ("$(6$}(B" . "$,15l6-5u(B")
  ("$(6$~(B" . "$,15m6-5h(B")))

(defconst indian-2-column-to-ucs-regexp
  "$(6!j!j(B\\|$(6"8"q(B\\|[$(6#&#'!*#*(B]$(6"p(B\\|[$(6!!(B-$(6$~(B]")

(put 'indian-2-column-to-ucs-chartable 'char-table-extra-slots 1)
(defconst indian-2-column-to-ucs-chartable
  (let ((table (make-char-table 'indian-2-column-to-ucs-chartable))
	(alist nil))
    (dolist (elt indian-2-colum-to-ucs)
      (if (= (length (car elt)) 1)
	  (aset table (aref (car elt) 0) (cdr elt))
	(setq alist (cons elt alist))))
    (set-char-table-extra-slot table 0 alist)
    table))

;;;###autoload
(defun indian-2-column-to-ucs-region (from to)
  "Convert old Emacs Devanagari characters to UCS."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((pos from)
	    (alist (char-table-extra-slot indian-2-column-to-ucs-chartable 0)))
	(narrow-to-region from to)
	(decompose-region from to)
	(goto-char (point-min))
	(while (re-search-forward indian-2-column-to-ucs-regexp nil t)
	  (let ((len (- (match-end 0) (match-beginning 0)))
		subst)
	    (if (= len 1)
		(setq subst (aref indian-2-column-to-ucs-chartable
				  (char-after (match-beginning 0))))
	      (setq subst (cdr (assoc (match-string 0) alist))))
	    (replace-match (if subst subst "?"))))
	(indian-compose-region (point-min) (point-max))))))

(provide 'ind-util)

;;; ind-util.el ends here
