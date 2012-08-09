;;; greek.el --- Quail package for inputting Greek -*-coding: iso-2022-7bit-*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Greek

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
 "greek-jis" "Greek" "$B&8(B" nil
 "$B&%&K&K&G&M&I&J&A(B: Greek keyboard layout (JIS X0208.1983)

The layout is same as greek, but uses JIS characters.
Sorry, accents and terminal sigma are not supported in JIS."
 nil t t t t nil nil nil nil nil t)

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
 ("`" ?$B!F(B)
 ("q" ?$B!&(B)
 ("w" ?$B&R(B)
 ("e" ?$B&E(B)
 ("r" ?$B&Q(B)
 ("t" ?$B&S(B)
 ("y" ?$B&T(B)
 ("u" ?$B&H(B)
 ("i" ?$B&I(B)
 ("o" ?$B&O(B)
 ("p" ?$B&P(B)
 ("[" ?\$B!N(B)
 ("]" ?\$B!O(B)
 ("a" ?$B&A(B)
 ("s" ?$B&R(B)
 ("d" ?$B&D(B)
 ("f" ?$B&U(B)
 ("g" ?$B&C(B)
 ("h" ?$B&G(B)
 ("j" ?$B&N(B)
 ("k" ?$B&J(B)
 ("l" ?$B&K(B)
 (";" ?$B!G(B)
 ("'" ?$B!G(B)
 ("\\" ?$B!@(B)
 ("z" ?$B&F(B)
 ("x" ?$B&V(B)
 ("c" ?$B&W(B)
 ("v" ?$B&X(B)
 ("b" ?$B&B(B)
 ("n" ?$B&M(B)
 ("m" ?$B&L(B)
 ("," ?, )
 ("." ?. )
 ("/" ?$B!?(B)

 ("!" ?$B!*(B)
 ("@" ?$B!w(B)
 ("#" ?$B!t(B)
 ("$" ?$B!t(B)
 ("%" ?$B!s(B)
 ("^" ?$B!0(B)
 ("&" ?$B!u(B)
 ("*" ?$B!v(B)
 ("(" ?\$B!J(B)
 (")" ?\$B!K(B)
 ("_" ?$B!2(B)
 ("+" ?$B!\(B)
 ("~" ?$B!1(B)
 ("Q" ?$B!](B)
 ("W" ?$B&2(B)
 ("E" ?$B&%(B)
 ("R" ?$B&1(B)
 ("T" ?$B&3(B)
 ("Y" ?$B&4(B)
 ("U" ?$B&((B)
 ("I" ?$B&)(B)
 ("O" ?$B&/(B)
 ("P" ?$B&1(B)
 ("{" ?\$B!P(B)
 ("}" ?\$B!Q(B)
 ("A" ?$B&!(B)
 ("S" ?$B&2(B)
 ("D" ?$B&$(B)
 ("F" ?$B&5(B)
 ("G" ?$B&#(B)
 ("H" ?$B&'(B)
 ("J" ?$B&.(B)
 ("K" ?$B&*(B)
 ("L" ?$B&+(B)
 (":" ?$B!I(B)
 ("\"" ?$B!I(B)
 ("|" ?$B!C(B)
 ("Z" ?$B&&(B)
 ("X" ?$B&6(B)
 ("C" ?$B&7(B)
 ("V" ?$B&8(B)
 ("B" ?$B&"(B)
 ("N" ?$B&-(B)
 ("M" ?$B&,(B)
 ("<" ?$B!((B)
 (">" ?$B!'(B)
 ("?" ?$B!)(B))

;;

(quail-define-package "greek-mizuochi" "Greek" "CG" t
"The Mizuochi input method for Classical Greek using mule-unicode-0100-24ff.

-------------------------------------
character     capital	      small
-------------------------------------
alpha		A		a
beta		B		b
gamma		G		g
delta		D		d
epsilon		E		e
zeta		Z		z
eta		H		h
theta		Q		q
iota		I		i
kappa		K		k
lambda		L		l
mu		M		m
nu		N		n
xi		X		x
omicron		O		o
pi		P		p
rho		R		r
sigma		S		s
final sigma			j
tau		T		t
upsilon		U		u
phi		F		f
chi		C		c
psi		Y		y
omega		W		w
-------------------------------------
sampi				!
digamma		#
stigma				$
koppa		&		%
-------------------------------------

------------------------
mark		key
------------------------
ypogegrammeni	J
psili		'  or  v
dasia		`  or  V
oxia		/
varia		?
perispomeni	\\  or  ^
dialytika	\"
ano teleia	:
erotimatiko	;
----------------------
"
nil t t nil nil nil nil nil nil nil t)

(quail-define-rules

 ("!" ?$,1'a(B) ; sampi
 ("#" ?$,1'\(B) ; DIGAMMA
 ("$" ?$,1'[(B) ; stigma
 ("%" ?$,1'_(B) ; koppa
 ("&" ?$,1'^(B) ; KOPPA
 ("'" ?$,1q(B) ("v" ?$,1q(B) ; psili
 ("/" ?$,1r](B) ; oxia
 (":" ?$,1&g(B) ; ano teleia
 (";" ?$,1&^(B) ; erotimatiko
 ("\"" ?,A((B) ; dialytika

 ("A" ?$,1&q(B)
 ("B" ?$,1&r(B)
 ("C" ?$,1''(B)
 ("D" ?$,1&t(B)
 ("E" ?$,1&u(B)
 ("F" ?$,1'&(B)
 ("G" ?$,1&s(B)
 ("H" ?$,1&w(B)
 ("I" ?$,1&y(B)
 ("wJ" ?$,1rS(B)
 ("K" ?$,1&z(B)
 ("L" ?$,1&{(B)
 ("M" ?$,1&|(B)
 ("N" ?$,1&}(B)
 ("O" ?$,1&(B)

 ("P" ?$,1' (B)
 ("Q" ?$,1&x(B)
 ("R" ?$,1'!(B)
 ("S" ?$,1'#(B)
 ("T" ?$,1'$(B)
 ("U" ?$,1'%(B)
 ("hJ" ?$,1r#(B)
 ("W" ?$,1')(B)
 ("X" ?$,1&~(B)
 ("Y" ?$,1'((B)
 ("Z" ?$,1&v(B)
 ("?" ?$,1rO(B) ; varia
 ("\\" ?$,1r (B) ("^" ?$,1r (B) ; perispomeni

 ("`" ?$,1r^(B) ("V" ?$,1r^(B) ; dasia
 ("a" ?$,1'1(B)
 ("b" ?$,1'2(B)
 ("c" ?$,1'G(B)
 ("d" ?$,1'4(B)
 ("e" ?$,1'5(B)
 ("f" ?$,1'F(B)
 ("g" ?$,1'3(B)
 ("h" ?$,1'7(B)
 ("i" ?$,1'9(B)
 ("j" ?$,1'B(B)
 ("k" ?$,1':(B)
 ("l" ?$,1';(B)
 ("m" ?$,1'<(B)
 ("n" ?$,1'=(B)
 ("o" ?$,1'?(B)

 ("p" ?$,1'@(B)
 ("q" ?$,1'8(B)
 ("r" ?$,1'A(B)
 ("s" ?$,1'C(B)
 ("t" ?$,1'D(B)
 ("u" ?$,1'E(B)
 ("aJ" ?$,1qs(B)
 ("w" ?$,1'I(B)
 ("x" ?$,1'>(B)
 ("y" ?$,1'H(B)
 ("z" ?$,1'6(B)

 ("i`" ?$,1pQ(B) ("iV" ?$,1pQ(B)
 ("i'" ?$,1pP(B) ("iv" ?$,1pP(B)
 ("i/" ?$,1q7(B)
 ("i`/" ?$,1pU(B) ("iV/" ?$,1pU(B) ("i/`" ?$,1pU(B) ("i/V" ?$,1pU(B)
 ("i'/" ?$,1pT(B) ("iv/" ?$,1pT(B) ("i/'" ?$,1pT(B) ("i/v" ?$,1pT(B)
 ("i?" ?$,1q6(B)
 ("i`?" ?$,1pS(B) ("iV?" ?$,1pS(B) ("i?`" ?$,1pS(B) ("i?V" ?$,1pS(B)
 ("i'?" ?$,1pR(B) ("iv?" ?$,1pR(B) ("i?'" ?$,1pR(B) ("i?v" ?$,1pR(B)
 ("i^"  ?$,1r6(B) ("i\\" ?$,1r6(B)
 ("i`^"  ?$,1pW(B) ("i`\\" ?$,1pW(B) ("iV^"  ?$,1pW(B) ("iV\\" ?$,1pW(B)
 ("i^`"  ?$,1pW(B) ("i\\`" ?$,1pW(B) ("i^V"  ?$,1pW(B) ("i\\V" ?$,1pW(B)
 ("i'^"  ?$,1pV(B) ("i'\\" ?$,1pV(B) ("iv^"  ?$,1pV(B) ("iv\\" ?$,1pV(B)
 ("i^'"  ?$,1pV(B) ("i\\'" ?$,1pV(B) ("i^v"  ?$,1pV(B) ("i\\v" ?$,1pV(B)
 ("i\"" ?$,1'J(B)
 ("i/\"" ?$,1r3(B) ("i\"/" ?$,1r3(B)
 ("i?\"" ?$,1r2(B) ("i\"?" ?$,1r2(B)

 ("^`"  ?$,1r?(B) ("^V"  ?$,1r?(B) ("\\`" ?$,1r?(B) ("\\V" ?$,1r?(B)
 ("`^"  ?$,1r?(B) ("V^"  ?$,1r?(B) ("`\\" ?$,1r?(B) ("V\\" ?$,1r?(B)
 ("^'"  ?$,1r/(B) ("^v"  ?$,1r/(B) ("\\'" ?$,1r/(B) ("\\v" ?$,1r/(B)
 ("'^"  ?$,1r/(B) ("v^"  ?$,1r/(B) ("'\\" ?$,1r/(B) ("v\\" ?$,1r/(B)
 ("/`" ?$,1r>(B) ("/V" ?$,1r>(B) ("`/" ?$,1r>(B) ("V/" ?$,1r>(B)
 ("/'" ?$,1r.(B) ("/v" ?$,1r.(B) ("'/" ?$,1r.(B) ("v/" ?$,1r.(B)
 ("?`" ?$,1r=(B) ("?V" ?$,1r=(B) ("`?" ?$,1r=(B) ("V?" ?$,1r=(B)
 ("?'" ?$,1r-(B) ("?v" ?$,1r-(B) ("'?" ?$,1r-(B) ("v?" ?$,1r-(B)
 ("\"/" ?$,1rN(B) ("/\"" ?$,1rN(B)
 ("\"?" ?$,1rM(B) ("?\"" ?$,1rM(B)

 ("e`" ?$,1p1(B) ("eV" ?$,1p1(B)
 ("e'" ?$,1p0(B) ("ev" ?$,1p0(B)
 ("e/" ?$,1q3(B)
 ("e/`" ?$,1p5(B) ("e/V" ?$,1p5(B) ("e`/" ?$,1p5(B) ("eV/" ?$,1p5(B)
 ("e/'" ?$,1p4(B) ("e/v" ?$,1p4(B) ("e'/" ?$,1p4(B) ("ev/" ?$,1p4(B)
 ("e?" ?$,1q2(B)
 ("e?`" ?$,1p3(B) ("e?V" ?$,1p3(B) ("e`?" ?$,1p3(B) ("eV?" ?$,1p3(B)
 ("e?'" ?$,1p2(B) ("e?v" ?$,1p2(B) ("e'?" ?$,1p2(B) ("ev?" ?$,1p2(B)

 ("a`" ?$,1p!(B) ("aV" ?$,1p!(B)
 ("a'" ?$,1p (B) ("av" ?$,1p (B)
 ("a/" ?$,1q1(B)
 ("a/`" ?$,1p%(B) ("a/V" ?$,1p%(B) ("a`/" ?$,1p%(B) ("aV/" ?$,1p%(B)
 ("a/'" ?$,1p$(B) ("a/v" ?$,1p$(B) ("a'/" ?$,1p$(B) ("av/" ?$,1p$(B)
 ("a?" ?$,1q0(B)
 ("a?`" ?$,1p#(B) ("a?V" ?$,1p#(B) ("a`?" ?$,1p#(B) ("aV?" ?$,1p#(B)
 ("a?'" ?$,1p"(B) ("a?v" ?$,1p"(B) ("a'?" ?$,1p"(B) ("av?" ?$,1p"(B)
 ("a^"  ?$,1qv(B) ("a\\" ?$,1qv(B)
 ("a^`"  ?$,1p'(B) ("a^V"  ?$,1p'(B) ("a\\`" ?$,1p'(B) ("a\\V" ?$,1p'(B)
 ("a`^"  ?$,1p'(B) ("aV^"  ?$,1p'(B) ("a`\\" ?$,1p'(B) ("aV\\" ?$,1p'(B)
 ("a^'"  ?$,1p&(B) ("a^v"  ?$,1p&(B) ("a\\'" ?$,1p&(B) ("a\\v" ?$,1p&(B)
 ("a'^"  ?$,1p&(B) ("av^"  ?$,1p&(B) ("a'\\" ?$,1p&(B) ("av\\" ?$,1p&(B)

 ("aJ`" ?$,1qA(B) ("aJV" ?$,1qA(B)
 ("aJ'" ?$,1q@(B) ("aJv" ?$,1q@(B)
 ("aJ/" ?$,1qt(B)
 ("aJ/`" ?$,1qE(B) ("aJ/V" ?$,1qE(B) ("aJ`/" ?$,1qE(B) ("aJV/" ?$,1qE(B)
 ("aJ/'" ?$,1qD(B) ("aJ/v" ?$,1qD(B) ("aJ'/" ?$,1qD(B) ("aJv/" ?$,1qD(B)
 ("aJ?" ?$,1qr(B)
 ("aJ?`" ?$,1qC(B) ("aJ?V" ?$,1qC(B) ("aJ`?" ?$,1qC(B) ("aJV?" ?$,1qC(B)
 ("aJ?'" ?$,1qB(B) ("aJ?v" ?$,1qB(B) ("aJ'?" ?$,1qB(B) ("aJv?" ?$,1qB(B)
 ("aJ^"  ?$,1qw(B) ("aJ\\" ?$,1qw(B)
 ("aJ^`"  ?$,1qG(B) ("aJ^V"  ?$,1qG(B) ("aJ\\`" ?$,1qG(B) ("aJ\\V" ?$,1qG(B)
 ("aJ`^"  ?$,1qG(B) ("aJV^"  ?$,1qG(B) ("aJ`\\" ?$,1qG(B) ("aJV\\" ?$,1qG(B)
 ("aJ^'"  ?$,1qF(B) ("aJ^v"  ?$,1qF(B) ("aJ\\'" ?$,1qF(B) ("aJ\\v" ?$,1qF(B)
 ("aJ'^"  ?$,1qF(B) ("aJv^"  ?$,1qF(B) ("aJ'\\" ?$,1qF(B) ("aJv\\" ?$,1qF(B)

 ("r`" ?$,1rE(B) ("rV" ?$,1rE(B)
 ("r'" ?$,1rD(B) ("rv" ?$,1rD(B)

 ("h`" ?$,1pA(B) ("hV" ?$,1pA(B)
 ("h'" ?$,1p@(B) ("hv" ?$,1p@(B)
 ("h/" ?$,1q5(B)
 ("h/`" ?$,1pE(B) ("h/V" ?$,1pE(B) ("h`/" ?$,1pE(B) ("hV/" ?$,1pE(B)
 ("h/'" ?$,1pD(B) ("h/v" ?$,1pD(B) ("h'/" ?$,1pD(B) ("hv/" ?$,1pD(B)
 ("h?" ?$,1q4(B)
 ("h?`" ?$,1pC(B) ("h?V" ?$,1pC(B) ("h`?" ?$,1pC(B) ("hV?" ?$,1pC(B)
 ("h?'" ?$,1pB(B) ("h?v" ?$,1pB(B) ("h'?" ?$,1pB(B) ("hv?" ?$,1pB(B)
 ("h^"  ?$,1r&(B) ("h\\" ?$,1r&(B)
 ("h^`"  ?$,1pG(B) ("h^V"  ?$,1pG(B) ("h\\`" ?$,1pG(B) ("h\\V" ?$,1pG(B)
 ("h`^"  ?$,1pG(B) ("h`\\" ?$,1pG(B) ("hV^"  ?$,1pG(B) ("hV\\" ?$,1pG(B)
 ("h^'"  ?$,1pF(B) ("h^v"  ?$,1pF(B) ("h\\'" ?$,1pF(B) ("h\\v" ?$,1pF(B)
 ("h'^"  ?$,1pF(B) ("h'\\" ?$,1pF(B) ("hv^"  ?$,1pF(B) ("hv\\" ?$,1pF(B)

 ("J" ?$,1&Z(B) ; ypogegrammeni

 ("hJ`" ?$,1qQ(B) ("hJV" ?$,1qQ(B)
 ("hJ'" ?$,1qP(B) ("hJv" ?$,1qP(B)
 ("hJ/" ?$,1r$(B)
 ("hJ`/" ?$,1qU(B) ("hJV/" ?$,1qU(B) ("hJ/`" ?$,1qU(B) ("hJ/V" ?$,1qU(B)
 ("hJ'/" ?$,1qT(B) ("hJv/" ?$,1qT(B) ("hJ/'" ?$,1qT(B) ("hJ/v" ?$,1qT(B)
 ("hJ?" ?$,1r"(B)
 ("hJ`?" ?$,1qS(B) ("hJV?" ?$,1qS(B) ("hJ?`" ?$,1qS(B) ("hJ?V" ?$,1qS(B)
 ("hJ'?" ?$,1qR(B) ("hJv?" ?$,1qR(B) ("hJ?'" ?$,1qR(B) ("hJ?v" ?$,1qR(B)
 ("hJ^"  ?$,1r'(B) ("hJ\\" ?$,1r'(B)
 ("hJ`^"  ?$,1qW(B) ("hJ`\\" ?$,1qW(B) ("hJV^"  ?$,1qW(B) ("hJV\\" ?$,1qW(B)
 ("hJ^`"  ?$,1qW(B) ("hJ\\`" ?$,1qW(B) ("hJ^V"  ?$,1qW(B) ("hJ\\V" ?$,1qW(B)
 ("hJ'^"  ?$,1qV(B) ("hJ'\\" ?$,1qV(B) ("hJv^"  ?$,1qV(B) ("hJv\\" ?$,1qV(B)
 ("hJ^'"  ?$,1qV(B) ("hJ\\'" ?$,1qV(B) ("hJ^v"  ?$,1qV(B) ("hJ\\v" ?$,1qV(B)

 ("o`" ?$,1pa(B) ("oV" ?$,1pa(B)
 ("o'" ?$,1p`(B) ("ov" ?$,1p`(B)
 ("o/" ?$,1q9(B)
 ("o/`" ?$,1pe(B) ("o/V" ?$,1pe(B) ("o`/" ?$,1pe(B) ("oV/" ?$,1pe(B)
 ("o/'" ?$,1pd(B) ("o/v" ?$,1pd(B) ("o'/" ?$,1pd(B) ("ov/" ?$,1pd(B)
 ("o?" ?$,1q8(B)
 ("o?`" ?$,1pc(B) ("o?V" ?$,1pc(B) ("o`?" ?$,1pc(B) ("oV?" ?$,1pc(B)
 ("o?'" ?$,1pb(B) ("o?v" ?$,1pb(B) ("o'?" ?$,1pb(B) ("ov?" ?$,1pb(B)

 ("u`" ?$,1pq(B) ("uV" ?$,1pq(B)
 ("u'" ?$,1pp(B) ("uv" ?$,1pp(B)
 ("u/" ?$,1q;(B)
 ("u/`" ?$,1pu(B) ("u/V" ?$,1pu(B) ("u`/" ?$,1pu(B) ("uV/" ?$,1pu(B)
 ("u/'" ?$,1pt(B) ("u/v" ?$,1pt(B) ("u'/" ?$,1pt(B) ("uv/" ?$,1pt(B)
 ("u?" ?$,1q:(B)
 ("u?`" ?$,1ps(B) ("u?V" ?$,1ps(B) ("u`?" ?$,1ps(B) ("uV?" ?$,1ps(B)
 ("u?'" ?$,1pr(B) ("u?v" ?$,1pr(B) ("u'?" ?$,1pr(B) ("uv?" ?$,1pr(B)
 ("u^"  ?$,1rF(B) ("u\\" ?$,1rF(B)
 ("u^`"  ?$,1pw(B) ("u^V"  ?$,1pw(B) ("u\\`" ?$,1pw(B) ("u\\V" ?$,1pw(B)
 ("u`^"  ?$,1pw(B) ("uV^"  ?$,1pw(B) ("u`\\" ?$,1pw(B) ("uV\\" ?$,1pw(B)
 ("u^'"  ?$,1pv(B) ("u^v"  ?$,1pv(B) ("u\\'" ?$,1pv(B) ("u\\v" ?$,1pv(B)
 ("u'^"  ?$,1pv(B) ("uv^"  ?$,1pv(B) ("u'\\" ?$,1pv(B) ("uv\\" ?$,1pv(B)
 ("u\"" ?$,1'K(B)
 ("u\"/" ?$,1rC(B) ("u/\"" ?$,1rC(B)
 ("u\"?" ?$,1rB(B) ("u?\"" ?$,1rB(B)

 ("w`" ?$,1q!(B) ("wV" ?$,1q!(B)
 ("w'" ?$,1q (B) ("wv" ?$,1q (B)
 ("w/" ?$,1q=(B)
 ("w/`" ?$,1q%(B) ("w/V" ?$,1q%(B) ("w`/" ?$,1q%(B) ("wV/" ?$,1q%(B)
 ("w/'" ?$,1q$(B) ("w/v" ?$,1q$(B) ("w'/" ?$,1q$(B) ("wv/" ?$,1q$(B)
 ("w?" ?$,1q<(B)
 ("w?`" ?$,1q#(B) ("w?V" ?$,1q#(B) ("w`?" ?$,1q#(B) ("wV?" ?$,1q#(B)
 ("w?'" ?$,1q"(B) ("w?v" ?$,1q"(B) ("w'?" ?$,1q"(B) ("wv?" ?$,1q"(B)
 ("w^"  ?$,1rV(B) ("w\\" ?$,1rV(B)
 ("w^`"  ?$,1q'(B) ("w^V"  ?$,1q'(B) ("w\\`" ?$,1q'(B) ("w\\V" ?$,1q'(B)
 ("w`^"  ?$,1q'(B) ("wV^"  ?$,1q'(B) ("w`\\" ?$,1q'(B) ("wV\\" ?$,1q'(B)
 ("w^'"  ?$,1q&(B) ("w^v"  ?$,1q&(B) ("w\\'" ?$,1q&(B) ("w\\v" ?$,1q&(B)
 ("w'^"  ?$,1q&(B) ("wv^"  ?$,1q&(B) ("w'\\" ?$,1q&(B) ("wv\\" ?$,1q&(B)

 ("wJ`" ?$,1qa(B) ("wJV" ?$,1qa(B)
 ("wJ'" ?$,1q`(B) ("wJv" ?$,1q`(B)
 ("wJ/" ?$,1rT(B)
 ("wJ/`" ?$,1qe(B) ("wJ/V" ?$,1qe(B) ("wJ`/" ?$,1qe(B) ("wJV/" ?$,1qe(B)
 ("wJ/'" ?$,1qd(B) ("wJ/v" ?$,1qd(B) ("wJ'/" ?$,1qd(B) ("wJv/" ?$,1qd(B)
 ("wJ?" ?$,1rR(B)
 ("wJ?`" ?$,1qc(B) ("wJ?V" ?$,1qc(B) ("wJ`?" ?$,1qc(B) ("wJV?" ?$,1qc(B)
 ("wJ?'" ?$,1qb(B) ("wJ?v" ?$,1qb(B) ("wJ'?" ?$,1qb(B) ("wJv?" ?$,1qb(B)
 ("wJ^"  ?$,1rW(B) ("wJ\\" ?$,1rW(B)
 ("wJ^`"  ?$,1qg(B) ("wJ^V"  ?$,1qg(B) ("wJ\\`" ?$,1qg(B) ("wJ\\V" ?$,1qg(B)
 ("wJ`^"  ?$,1qg(B) ("wJV^"  ?$,1qg(B) ("wJ`\\" ?$,1qg(B) ("wJV\\" ?$,1qg(B)
 ("wJ^'"  ?$,1qf(B) ("wJ^v"  ?$,1qf(B) ("wJ\\'" ?$,1qf(B) ("wJ\\v" ?$,1qf(B)
 ("wJ'^"  ?$,1qf(B) ("wJv^"  ?$,1qf(B) ("wJ'\\" ?$,1qf(B) ("wJv\\" ?$,1qf(B)
 )

;;

(quail-define-package "greek-babel" "Greek" "BG" t
"The TeX Babel input method for Classical Greek using mule-unicode-0100-24ff.

-------------------------------------
character     capital	      small
-------------------------------------
alpha		A		a
beta		B		b
gamma		G		g
delta		D		d
epsilon		E		e
zeta		Z		z
eta		H		h
theta		J		j
iota		I		i
kappa		K		k
lambda		L		l
mu		M		m
nu		N		n
xi		X		x
omicron		O		o
pi		P		p
rho		R		r
sigma		S		s
final sigma			c
tau		T		t
upsilon		U		u
phi		F		f
chi		Q		q
psi		Y		y
omega		W		w
-------------------------------------
sampi				!
digamma		#
stigma				$
koppa		&		%
-------------------------------------

------------------------
mark		key
------------------------
ypogegrammeni	|
psili		>
dasia		<
oxia		'
koronis         ''
varia		`
perispomeni	~
dialytika	\"
ano teleia	;
erotimatiko	?
----------------------
"
nil t t nil nil nil nil nil nil nil t)

(quail-define-rules

 ("!" ?$,1'a(B) ; sampi
 ("#" ?$,1'\(B) ; DIGAMMA
 ("$" ?$,1'[(B) ; stigma
 ("%" ?$,1'_(B) ; koppa
 ("&" ?$,1'^(B) ; KOPPA
 (">" ?$,1q(B) ; psili
 ("'" ?$,1r](B) ; oxia
 (";" ?$,1&g(B) ; ano teleia
 ("?" ?$,1&^(B) ; erotimatiko
 ("\"" ?,A((B) ; dialytika
 ("|" ?$,1&Z(B) ; ypogegrammeni
 ("''" ?$,1q}(B) ; koronis
 ("((" ?,A+(B) ; #x00ab
 ("))" ?,A;(B) ; #x00bb

 ("A" ?$,1&q(B)
 ("A|" ?$,1q|(B)
 ("B" ?$,1&r(B)
 ("D" ?$,1&t(B)
 ("E" ?$,1&u(B)
 ("F" ?$,1'&(B)
 ("G" ?$,1&s(B)
 ("H" ?$,1&w(B)
 ("H|" ?$,1r,(B)
 ("I" ?$,1&y(B)
 ("J" ?$,1&x(B)
 ("K" ?$,1&z(B)
 ("L" ?$,1&{(B)
 ("M" ?$,1&|(B)
 ("N" ?$,1&}(B)
 ("O" ?$,1&(B)
 ("P" ?$,1' (B)
 ("Q" ?$,1''(B)
 ("R" ?$,1'!(B)
 ("S" ?$,1'#(B)
 ("T" ?$,1'$(B)
 ("U" ?$,1'%(B)
 ("W" ?$,1')(B)
 ("W|" ?$,1r\(B)
 ("X" ?$,1&~(B)
 ("Y" ?$,1'((B)
 ("Z" ?$,1&v(B)
 ("`" ?$,1rO(B) ; varia
 ("~" ?$,1r (B) ; perispomeni

 ("<" ?$,1r^(B) ; dasia
 ("a" ?$,1'1(B)
 ("a|" ?$,1qs(B)
 ("b" ?$,1'2(B)
 ("c" ?$,1'B(B)
 ("d" ?$,1'4(B)
 ("e" ?$,1'5(B)
 ("f" ?$,1'F(B)
 ("g" ?$,1'3(B)
 ("h" ?$,1'7(B)
 ("h|" ?$,1r#(B)
 ("i" ?$,1'9(B)
 ("j" ?$,1'8(B)
 ("k" ?$,1':(B)
 ("l" ?$,1';(B)
 ("m" ?$,1'<(B)
 ("n" ?$,1'=(B)
 ("o" ?$,1'?(B)
 ("p" ?$,1'@(B)
 ("q" ?$,1'G(B)
 ("r" ?$,1'A(B)
 ("s" ?$,1'C(B)
 ("t" ?$,1'D(B)
 ("u" ?$,1'E(B)
 ("w" ?$,1'I(B)
 ("w|" ?$,1rS(B)
 ("x" ?$,1'>(B)
 ("y" ?$,1'H(B)
 ("z" ?$,1'6(B)

 ("<i" ?$,1pQ(B)
 (">i" ?$,1pP(B)
 ("'i" ?$,1q7(B)
 ("<'i" ?$,1pU(B)
 (">'i" ?$,1pT(B)
 ("`i" ?$,1q6(B)
 ("<`i" ?$,1pS(B)
 (">`i" ?$,1pR(B)
 ("~i"  ?$,1r6(B)
 ("<~i"  ?$,1pW(B)
 (">~i"  ?$,1pV(B)
 ("\"i" ?$,1'J(B)
 ("\"'i" ?$,1r3(B)
 ("\"`i" ?$,1r2(B)

 ("<I" ?$,1pY(B)
 (">I" ?$,1pX(B)
 ("'I" ?$,1r;(B)
 ("<'I" ?$,1p](B)
 (">'I" ?$,1p\(B)
 ("`I" ?$,1r:(B)
 ("<`I" ?$,1p[(B)
 (">`I" ?$,1pZ(B)
 ("<~I"  ?$,1p_(B)
 (">~I"  ?$,1p^(B)
 ("\"I" ?$,1'*(B)

 ("<~"  ?$,1r?(B)
 (">~"  ?$,1r/(B)
 ("<'" ?$,1r>(B)
 (">'" ?$,1r.(B)
 ("<`" ?$,1r=(B)
 (">`" ?$,1r-(B)
 ("\"'" ?$,1rN(B)
 ("\"`" ?$,1rM(B)

 ("<e" ?$,1p1(B)
 (">e" ?$,1p0(B)
 ("'e" ?$,1q3(B)
 ("<'e" ?$,1p5(B)
 (">'e" ?$,1p4(B)
 ("`e" ?$,1q2(B)
 ("<`e" ?$,1p3(B)
 (">`e" ?$,1p2(B)

 ("<E" ?$,1p9(B)
 (">E" ?$,1p8(B)
 ("'E" ?$,1r)(B)
 ("<'E" ?$,1p=(B)
 (">'E" ?$,1p<(B)
 ("`E" ?$,1r((B)
 ("<`E" ?$,1p;(B)
 (">`E" ?$,1p:(B)

 ("<a" ?$,1p!(B)
 (">a" ?$,1p (B)
 ("'a" ?$,1q1(B)
 ("<'a" ?$,1p%(B)
 (">'a" ?$,1p$(B)
 ("`a" ?$,1q0(B)
 ("<`a" ?$,1p#(B)
 (">`a" ?$,1p"(B)
 ("~a"  ?$,1qv(B)
 ("<~a"  ?$,1p'(B)
 (">~a"  ?$,1p&(B)

 ("<A" ?$,1p)(B)
 (">A" ?$,1p((B)
 ("'A" ?$,1q{(B)
 ("<'A" ?$,1p-(B)
 (">'A" ?$,1p,(B)
 ("`A" ?$,1qz(B)
 ("<`A" ?$,1p+(B)
 (">`A" ?$,1p*(B)
 ("<~A"  ?$,1p/(B)
 (">~A"  ?$,1p.(B)

 ("<a|" ?$,1qA(B)
 (">a|" ?$,1q@(B)
 ("'a|" ?$,1qt(B)
 ("<'a|" ?$,1qE(B)
 (">'a|" ?$,1qD(B)
 ("`a|" ?$,1qr(B)
 ("<`a|" ?$,1qC(B)
 (">`a|" ?$,1qB(B)
 ("~a|"  ?$,1qw(B)
 ("<~a|"  ?$,1qG(B)
 (">~a|"  ?$,1qF(B)

 ("<A|" ?$,1qI(B)
 (">A|" ?$,1qH(B)
 ("<'A|" ?$,1qM(B)
 (">'A|" ?$,1qL(B)
 ("<`A|" ?$,1qK(B)
 (">`A|" ?$,1qJ(B)
 ("<~A|"  ?$,1qO(B)
 (">~A|"  ?$,1qN(B)

 ("<r" ?$,1rE(B)
 (">r" ?$,1rD(B)

 ("<R" ?$,1rL(B)

 ("<h" ?$,1pA(B)
 (">h" ?$,1p@(B)
 ("'h" ?$,1q5(B)
 ("<'h" ?$,1pE(B)
 (">'h" ?$,1pD(B)
 ("`h" ?$,1q4(B)
 ("<`h" ?$,1pC(B)
 (">`h" ?$,1pB(B)
 ("~h"  ?$,1r&(B)
 ("<~h"  ?$,1pG(B)
 (">~h"  ?$,1pF(B)

 ("<H" ?$,1pI(B)
 (">H" ?$,1pH(B)
 ("'H" ?$,1r+(B)
 ("<'H" ?$,1pM(B)
 (">'H" ?$,1pL(B)
 ("`H" ?$,1r*(B)
 ("<`H" ?$,1pK(B)
 (">`H" ?$,1pJ(B)
 ("<~H"  ?$,1pO(B)
 (">~H"  ?$,1pN(B)

 ("|" ?$,1&Z(B) ; ypogegrammeni

 ("<h|" ?$,1qQ(B)
 (">h|" ?$,1qP(B)
 ("'h|" ?$,1r$(B)
 ("<'h|" ?$,1qU(B)
 (">'h|" ?$,1qT(B)
 ("`h|" ?$,1r"(B)
 ("<`h|" ?$,1qS(B)
 (">`h|" ?$,1qR(B)
 ("~h|"  ?$,1r'(B)
 ("<~h|"  ?$,1qW(B)
 (">~h|"  ?$,1qV(B)

 ("<H|" ?$,1qY(B)
 (">H|" ?$,1qX(B)
 ("<'H|" ?$,1q](B)
 (">'H|" ?$,1q\(B)
 ("<`H|" ?$,1q[(B)
 (">`H|" ?$,1qZ(B)
 ("<~H|"  ?$,1q_(B)
 (">~H|"  ?$,1q^(B)

 ("<o" ?$,1pa(B)
 (">o" ?$,1p`(B)
 ("'o" ?$,1q9(B)
 ("<'o" ?$,1pe(B)
 (">'o" ?$,1pd(B)
 ("`o" ?$,1q8(B)
 ("<`o" ?$,1pc(B)
 (">`o" ?$,1pb(B)

 ("<O" ?$,1pi(B)
 (">O" ?$,1ph(B)
 ("'O" ?$,1rY(B)
 ("<'O" ?$,1pm(B)
 (">'O" ?$,1pl(B)
 ("`O" ?$,1rX(B)
 ("<`O" ?$,1pk(B)
 (">`O" ?$,1pj(B)

 ("<u"   ?$,1pq(B)
 (">u"   ?$,1pp(B)
 ("'u"   ?$,1q;(B)
 ("<'u"  ?$,1pu(B)
 (">'u"  ?$,1pt(B)
 ("`u"   ?$,1q:(B)
 ("<`u"  ?$,1ps(B)
 (">`u"  ?$,1pr(B)
 ("~u"   ?$,1rF(B)
 ("<~u"  ?$,1pw(B)
 (">~u"  ?$,1pv(B)
 ("\"u"  ?$,1'K(B)
 ("\"'u" ?$,1rC(B)
 ("`\"u" ?$,1rB(B)

 ("<U"   ?$,1py(B)
 ("'U"   ?$,1rK(B)
 ("<'U"  ?$,1p}(B)
 ("`U"   ?$,1rJ(B)
 ("<`U"  ?$,1p{(B)
 ("<~U"  ?$,1p(B)
 ("\"U"  ?$,1'+(B)

 ("<w"  ?$,1q!(B)
 (">w"  ?$,1q (B)
 ("'w"  ?$,1q=(B)
 ("<'w" ?$,1q%(B)
 (">'w" ?$,1q$(B)
 ("`w"  ?$,1q<(B)
 ("<`w" ?$,1q#(B)
 (">`w" ?$,1q"(B)
 ("~w"  ?$,1rV(B)
 ("<~w" ?$,1q'(B)
 (">~w" ?$,1q&(B)

 ("<W"  ?$,1q)(B)
 (">W"  ?$,1q((B)
 ("'W"  ?$,1r[(B)
 ("<'W" ?$,1q-(B)
 (">'W" ?$,1q,(B)
 ("`W"  ?$,1rZ(B)
 ("<`W" ?$,1q+(B)
 (">`W" ?$,1q*(B)
 ("<~W" ?$,1q/(B)
 (">~W" ?$,1q.(B)

 ("<w|"	 ?$,1qa(B)
 (">w|"	 ?$,1q`(B)
 ("'w|"	 ?$,1rT(B)
 ("<'w|" ?$,1qe(B)
 (">'w|" ?$,1qd(B)
 ("`w|"  ?$,1rR(B)
 ("<`w|" ?$,1qc(B)
 (">`w|" ?$,1qb(B)
 ("~w|"	 ?$,1rW(B)
 ("<~w|" ?$,1qg(B)
 (">~w|" ?$,1qf(B)

 ("<W|"	 ?$,1qi(B)
 (">W|"	 ?$,1qh(B)
 ("'W|"	 ?$,1rT(B)
 ("<'W|" ?$,1qm(B)
 (">'W|" ?$,1ql(B)
 ("<`W|" ?$,1qk(B)
 (">`W|" ?$,1qj(B)
 ("<~W|" ?$,1qo(B)
 (">~W|" ?$,1qn(B)
 )

;;

(quail-define-package "greek-ibycus4" "Greek" "IB" t
"The Ibycus4 input method for Classical Greek using mule-unicode-0100-24ff."
nil t t nil nil nil nil nil nil nil t)

(quail-define-rules

 ("{((}" ?\() ("((" ?\() ; #x0028
 ("{))}" ?\)) ("))" ?\)) ; #x0029
 ("<<" ?,A+(B) ; #x00ab
 (">>" ?,A;(B) ; #x00bb

 ("-" ?$,1rp(B) ; #x2010
 ("---" ?$,1rt(B) ; #x2014
 ("||" ?$,1rv(B) ; #x2016
 ("{`}" ?$,1rx(B) ("`" ?$,1rx(B) ; #x2018
 ("{'}" ?$,1ry(B) ("'" ?$,1ry(B) ; #x2019
 ("{``}" ?$,1r|(B) ("``" ?$,1r|(B) ; #x201c
 ("{''}" ?$,1r}(B) ("''" ?$,1r}(B) ; #x201d
 ("{\\dag}" ?$,1s (B) ("\\dag" ?$,1s (B) ; #x2020
 ("{\\ddag}" ?$,1s!(B) ("\\ddag" ?$,1s!(B) ; #x2021
 ("<" ?$,1s9(B) ; #x2039
 (">" ?$,1s:(B) ; #x203a
 ("$\\leftarrow$" ?$,1vp(B) ; #x2190
 ("$\\rightarrow$" ?$,1vr(B) ; #x2192

 ("?" ?$,1&^(B) ; #x037e ; erotimatiko
 (";" ?$,1&g(B) ; #x0387 ; ano teleia
 ("|" ?$,1&Z(B) ; #x037a ; ypogegrammeni

 ("A" ?$,1&q(B)
 ("B" ?$,1&r(B)
 ("G" ?$,1&s(B)
 ("D" ?$,1&t(B)
 ("E" ?$,1&u(B)
 ("Z" ?$,1&v(B)
 ("H" ?$,1&w(B)
 ("Q" ?$,1&x(B)
 ("I" ?$,1&y(B)
 ("K" ?$,1&z(B)
 ("L" ?$,1&{(B)
 ("M" ?$,1&|(B)
 ("N" ?$,1&}(B)
 ("C" ?$,1&~(B)
 ("O" ?$,1&(B)
 ("P" ?$,1' (B)
 ("R" ?$,1'!(B)
 ("S" ?$,1'#(B)
 ("T" ?$,1'$(B)
 ("U" ?$,1'%(B)
 ("F" ?$,1'&(B)
 ("X" ?$,1''(B)
 ("Y" ?$,1'((B)
 ("W" ?$,1')(B)

 ("a" ?$,1'1(B)
 ("b" ?$,1'2(B)
 ("g" ?$,1'3(B)
 ("d" ?$,1'4(B)
 ("e" ?$,1'5(B)
 ("z" ?$,1'6(B)
 ("h" ?$,1'7(B)
 ("q" ?$,1'8(B)
 ("i" ?$,1'9(B)
 ("k" ?$,1':(B)
 ("l" ?$,1';(B)
 ("m" ?$,1'<(B)
 ("n" ?$,1'=(B)
 ("c" ?$,1'>(B)
 ("o" ?$,1'?(B)
 ("p" ?$,1'@(B)
 ("r" ?$,1'A(B)
 ("j" ?$,1'B(B) ("s " ["$,1'B(B "]) ("s," ["$,1'B(B,"]) ("s." ["$,1'B(B."]) ("s?" ["$,1'B&^(B"]) ("s;" ["$,1'B&g(B"])
 ("s|" ?$,1'C(B) ("s" ?$,1'C(B)
 ("t" ?$,1'D(B)
 ("u" ?$,1'E(B)
 ("f" ?$,1'F(B)
 ("x" ?$,1'G(B)
 ("y" ?$,1'H(B)
 ("w" ?$,1'I(B)

 ("i+" ?$,1'J(B)
 ("u+" ?$,1'K(B)
 ("V" ?$,1'\(B) ; DIGAMMA
 ("v" ?$,1'](B) ; digamma
 ("K+" ?$,1'^(B) ; KOPPA
 ("k+" ?$,1'_(B) ; koppa
 ("S+" ?$,1'`(B) ; SAMPI
 ("s+" ?$,1'a(B) ; sampi
 ("c+" ?$,1'r(B) ; lunate sigma

 ("a)" ?$,1p (B)
 ("a(" ?$,1p!(B)
 ("a)`" ?$,1p"(B)
 ("a(`" ?$,1p#(B)
 ("a)'" ?$,1p$(B)
 ("a('" ?$,1p%(B)
 ("a)=" ?$,1p&(B)
 ("a(=" ?$,1p'(B)

 (")A" ?$,1p((B)
 ("(A" ?$,1p)(B)
 (")`A" ?$,1p*(B)
 ("(`A" ?$,1p+(B)
 (")'A" ?$,1p,(B)
 ("('A" ?$,1p-(B)
 (")=A" ?$,1p.(B)
 ("(=A" ?$,1p/(B)

 ("e)" ?$,1p0(B)
 ("e(" ?$,1p1(B)
 ("e)`" ?$,1p2(B)
 ("e(`" ?$,1p3(B)
 ("e)'" ?$,1p4(B)
 ("e('" ?$,1p5(B)

 (")E" ?$,1p8(B)
 ("(E" ?$,1p9(B)
 (")`E" ?$,1p:(B)
 ("(`E" ?$,1p;(B)
 (")'E" ?$,1p<(B)
 ("('E" ?$,1p=(B)

 ("h)" ?$,1p@(B)
 ("h(" ?$,1pA(B)
 ("h)`" ?$,1pB(B)
 ("h(`" ?$,1pC(B)
 ("h)'" ?$,1pD(B)
 ("h('" ?$,1pE(B)
 ("h)=" ?$,1pF(B)
 ("h(=" ?$,1pG(B)

 (")H" ?$,1pH(B)
 ("(H" ?$,1pI(B)
 (")`H" ?$,1pJ(B)
 ("(`H" ?$,1pK(B)
 (")'H" ?$,1pL(B)
 ("('H" ?$,1pM(B)
 (")=H" ?$,1pN(B)
 ("(=H" ?$,1pO(B)

 ("i)" ?$,1pP(B)
 ("i(" ?$,1pQ(B)
 ("i)`" ?$,1pR(B)
 ("i(`" ?$,1pS(B)
 ("i)'" ?$,1pT(B)
 ("i('" ?$,1pU(B)
 ("i)=" ?$,1pV(B)
 ("i(=" ?$,1pW(B)

 (")I" ?$,1pX(B)
 ("(I" ?$,1pY(B)
 (")`I" ?$,1pZ(B)
 ("(`I" ?$,1p[(B)
 (")'I" ?$,1p\(B)
 ("('I" ?$,1p](B)
 (")=I" ?$,1p^(B)
 ("(=I" ?$,1p_(B)

 ("o)" ?$,1p`(B)
 ("o(" ?$,1pa(B)
 ("o)`" ?$,1pb(B)
 ("o(`" ?$,1pc(B)
 ("o)'" ?$,1pd(B)
 ("o('" ?$,1pe(B)

 (")O" ?$,1ph(B)
 ("(O" ?$,1pi(B)
 (")`O" ?$,1pj(B)
 ("(`O" ?$,1pk(B)
 (")'O" ?$,1pl(B)
 ("('O" ?$,1pm(B)

 ("u)" ?$,1pp(B)
 ("u(" ?$,1pq(B)
 ("u)`" ?$,1pr(B)
 ("u(`" ?$,1ps(B)
 ("u)'" ?$,1pt(B)
 ("u('" ?$,1pu(B)
 ("u)=" ?$,1pv(B)
 ("u(=" ?$,1pw(B)

 ("(U" ?$,1py(B)
 ("(`U" ?$,1p{(B)
 ("('U" ?$,1p}(B)
 ("(=U" ?$,1p(B)

 ("w)" ?$,1q (B)
 ("w(" ?$,1q!(B)
 ("w)`" ?$,1q"(B)
 ("w(`" ?$,1q#(B)
 ("w)'" ?$,1q$(B)
 ("w('" ?$,1q%(B)
 ("w)=" ?$,1q&(B)
 ("w(=" ?$,1q'(B)

 (")W" ?$,1q((B)
 ("(W" ?$,1q)(B)
 (")`W" ?$,1q*(B)
 ("(`W" ?$,1q+(B)
 (")'W" ?$,1q,(B)
 ("('W" ?$,1q-(B)
 (")=W" ?$,1q.(B)
 ("(=W" ?$,1q/(B)

 ("a`" ?$,1q0(B)
 ("a'" ?$,1q1(B)
 ("e`" ?$,1q2(B)
 ("e'" ?$,1q3(B)
 ("h`" ?$,1q4(B)
 ("h'" ?$,1q5(B)
 ("i`" ?$,1q6(B)
 ("i'" ?$,1q7(B)
 ("o`" ?$,1q8(B)
 ("o'" ?$,1q9(B)
 ("u`" ?$,1q:(B)
 ("u'" ?$,1q;(B)
 ("w`" ?$,1q<(B)
 ("w'" ?$,1q=(B)

 ("a)|" ?$,1q@(B)
 ("a(|" ?$,1qA(B)
 ("a)`|" ?$,1qB(B)
 ("a(`|" ?$,1qC(B)
 ("a)'|" ?$,1qD(B)
 ("a('|" ?$,1qE(B)
 ("a)=|" ?$,1qF(B)
 ("a(=|" ?$,1qG(B)

 (")A|" ?$,1qH(B)
 ("(A|" ?$,1qI(B)
 (")`A|" ?$,1qJ(B)
 ("(`A|" ?$,1qK(B)
 (")'A|" ?$,1qL(B)
 ("('A|" ?$,1qM(B)
 (")=A|" ?$,1qN(B)
 ("(=A|" ?$,1qO(B)

 ("h)|" ?$,1qP(B)
 ("h(|" ?$,1qQ(B)
 ("h)`|" ?$,1qR(B)
 ("h(`|" ?$,1qS(B)
 ("h)'|" ?$,1qT(B)
 ("h('|" ?$,1qU(B)
 ("h)=|" ?$,1qV(B)
 ("h(=|" ?$,1qW(B)

 (")H|" ?$,1qX(B)
 ("(H|" ?$,1qY(B)
 (")`H|" ?$,1qZ(B)
 ("(`H|" ?$,1q[(B)
 (")'H|" ?$,1q\(B)
 ("('H|" ?$,1q](B)
 (")=H|" ?$,1q^(B)
 ("(=H|" ?$,1q_(B)

 ("w)|" ?$,1q`(B)
 ("w(|" ?$,1qa(B)
 ("w)`|" ?$,1qb(B)
 ("w(`|" ?$,1qc(B)
 ("w)'|" ?$,1qd(B)
 ("w('|" ?$,1qe(B)
 ("w)=|" ?$,1qf(B)
 ("w(=|" ?$,1qg(B)

 (")W|" ?$,1qh(B)
 ("(W|" ?$,1qi(B)
 (")`W|" ?$,1qj(B)
 ("(`W|" ?$,1qk(B)
 (")'W|" ?$,1ql(B)
 ("('W|" ?$,1qm(B)
 (")=W|" ?$,1qn(B)
 ("(=W|" ?$,1qo(B)

 ("a`|" ?$,1qr(B)
 ("a|" ?$,1qs(B)
 ("a'|" ?$,1qt(B)
 ("a=" ?$,1qv(B)
 ("a=|" ?$,1qw(B)

 ("`A" ?$,1qz(B)
 ("'A" ?$,1q{(B)
 ("A|" ?$,1q|(B)

 (")" ?$,1q(B) ; #x1fbf ; psili
 ("=" ?$,1r (B) ; #x1fc0 ; perispomeni
 ("+=" ?$,1r!(B) ; #x1fc1

 ("h`|" ?$,1r"(B)
 ("h|" ?$,1r#(B)
 ("h'|" ?$,1r$(B)
 ("h=" ?$,1r&(B)
 ("h=|" ?$,1r'(B)

 ("`E" ?$,1r((B)
 ("'E" ?$,1r)(B)

 ("`H" ?$,1r*(B)
 ("'H" ?$,1r+(B)
 ("H|" ?$,1r,(B)

 (")`" ?$,1r-(B) ; #x1fcd
 (")'" ?$,1r.(B) ; #x1fce
 (")=" ?$,1r/(B) ; #x1fcf

 ("i+`" ?$,1r2(B)
 ("i+'" ?$,1r3(B)
 ("i=" ?$,1r6(B)
 ("i+=" ?$,1r7(B)

 ("`I" ?$,1r:(B)
 ("'I" ?$,1r;(B)

 ("(`" ?$,1r=(B) ; #x1fdd
 ("('" ?$,1r>(B) ; #x1fde
 ("(=" ?$,1r?(B) ; #x1fdf

 ("u+`" ?$,1rB(B)
 ("u+'" ?$,1rC(B)

 ("r)" ?$,1rD(B)
 ("r(" ?$,1rE(B)

 ("u=" ?$,1rF(B)
 ("u+=" ?$,1rG(B)

 ("`U" ?$,1rJ(B)
 ("'U" ?$,1rK(B)

 ("`R" ?$,1rL(B)

 ("+`" ?$,1rM(B) ; #x1fed
 ("+'" ?$,1rN(B) ; #x1fee
 ("`" ?$,1rO(B) ; #x1fef ; varia

 ("w`|" ?$,1rR(B)
 ("w|" ?$,1rS(B)
 ("w'|" ?$,1rT(B)
 ("w=" ?$,1rV(B)
 ("w=|" ?$,1rW(B)

 ("`O" ?$,1rX(B)
 ("'O" ?$,1rY(B)

 ("`W" ?$,1rZ(B)
 ("'W" ?$,1r[(B)
 ("W|" ?$,1r\(B)

 ("'" ?$,1r](B) ; #x1ffd ; oxia
 ("(" ?$,1r^(B) ;  #x1ffe ; dasia
)

;;

(quail-define-package
 "greek" "Greek" ",FY(B" nil
 ",FEkkgmij\(B: Greek keyboard layout (ISO 8859-7)
--------------

In the right of ,Fk(B key is a combination key, where
 ,F4(B acute
 ,F((B diaeresis

e.g.
 ,F4(B + ,Fa(B -> ,F\(B
 ,F((B + ,Fi(B -> ,Fz(B
 ,F((B + ,F4(B + ,Fi(B -> ,F@(B"
 nil t t t t nil nil nil nil nil t)

;; 1!  2@  3#  4$  5%  6^  7&  8*  9(  0)  -_  =+  `~
;;  ;:  ,FrS(B  ,FeE(B  ,FqQ(B  ,FtT(B  ,FuU(B  ,FhH(B  ,FiI(B  ,FoO(B  ,FpP(B  [{  ]}
;;   ,FaA(B  ,FsS(B  ,FdD(B  ,FvV(B  ,FcC(B  ,FgG(B  ,FnN(B  ,FjJ(B  ,FkK(B  ,F4((B  '"  \|
;;    ,FfF(B  ,FwW(B  ,FxX(B  ,FyY(B  ,FbB(B  ,FmM(B  ,FlL(B  ,<  .>  /?

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
 ("`" ?`)
 ("q" ?\;)
 ("w" ?,Fr(B)
 ("e" ?,Fe(B)
 ("r" ?,Fq(B)
 ("t" ?,Ft(B)
 ("y" ?,Fu(B)
 ("u" ?,Fh(B)
 ("i" ?,Fi(B)
 ("o" ?,Fo(B)
 ("p" ?,Fp(B)
 ("[" ?\[)
 ("]" ?\])
 ("a" ?,Fa(B)
 ("s" ?,Fs(B)
 ("d" ?,Fd(B)
 ("f" ?,Fv(B)
 ("g" ?,Fc(B)
 ("h" ?,Fg(B)
 ("j" ?,Fn(B)
 ("k" ?,Fj(B)
 ("l" ?,Fk(B)
 (";" ?,F4(B)
 ("'" ?')
 ("\\" ?\\)
 ("z" ?,Ff(B)
 ("x" ?,Fw(B)
 ("c" ?,Fx(B)
 ("v" ?,Fy(B)
 ("b" ?,Fb(B)
 ("n" ?,Fm(B)
 ("m" ?,Fl(B)
 ("," ?,)
 ("." ?.)
 ("/" ?/)

 ("!" ?!)
 ("@" ?@)
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?+)
 ("~" ?~)
 ("Q" ?:)
 ("W" ?,FS(B)
 ("E" ?,FE(B)
 ("R" ?,FQ(B)
 ("T" ?,FT(B)
 ("Y" ?,FU(B)
 ("U" ?,FH(B)
 ("I" ?,FI(B)
 ("O" ?,FO(B)
 ("P" ?,FP(B)
 ("{" ?{)
 ("}" ?})
 ("A" ?,FA(B)
 ("S" ?,FS(B)
 ("D" ?,FD(B)
 ("F" ?,FV(B)
 ("G" ?,FC(B)
 ("H" ?,FG(B)
 ("J" ?,FN(B)
 ("K" ?,FJ(B)
 ("L" ?,FK(B)
 (":" ?,F((B)
 ("\"" ?\")
 ("|" ?|)
 ("Z" ?,FF(B)
 ("X" ?,FW(B)
 ("C" ?,FX(B)
 ("V" ?,FY(B)
 ("B" ?,FB(B)
 ("N" ?,FM(B)
 ("M" ?,FL(B)
 ("<" ?<)
 (">" ?>)
 ("?" ??)

 (";a" ?,F\(B)
 (";e" ?,F](B)
 (";h" ?,F^(B)
 (";i" ?,F_(B)
 (";o" ?,F|(B)
 (";y" ?,F}(B)
 (";v" ?,F~(B)
 (";A" ?,F6(B)
 (";E" ?,F8(B)
 (";H" ?,F9(B)
 (";I" ?,F:(B)
 (";O" ?,F<(B)
 (";Y" ?,F>(B)
 (";V" ?,F?(B)
 (":i" ?,Fz(B)
 (":y" ?,F{(B)
 (":I" ?,FZ(B)
 (":Y" ?,F[(B)
 (";:i" ?,F@(B)
 (":;i" ?,F@(B)
 (";:y" ?,F`(B)
 (":;y" ?,F`(B)
 (";<" ?$(Q)((B)
 (";>" ?$(Q)2(B))

(quail-define-package
 "greek-postfix" "GreekPost" ",FX(B" nil
 ",FEkkgmij\(B: Greek keyboard layout with postfix accents (ISO 8859-7)
--------------

In the right of ,Fk(B key is a combination key, where
 ,F4(B acute
 ,F((B diaeresis

e.g.
 ,Fa(B + ,F4(B -> ,F\(B
 ,Fi(B + ,F((B -> ,Fz(B
 ,Fi(B + ,F((B + ,F4(B -> ,F@(B"
 nil t t t t nil nil nil nil nil t)

;; 1!  2@  3#  4$  5%  6^  7&  8*  9(  0)  -_  =+  `~
;;  ,F7/(B  ,FrS(B  ,FeE(B  ,FqQ(B  ,FtT(B  ,FuU(B  ,FhH(B  ,FiI(B  ,FoO(B  ,FpP(B  [{  ]}
;;   ,FaA(B  ,FsS(B  ,FdD(B  ,FvV(B  ,FcC(B  ,FgG(B  ,FnN(B  ,FjJ(B  ,FkK(B  ,F4((B  '"  \|
;;    ,FfF(B  ,FwW(B  ,FxX(B  ,FyY(B  ,FbB(B  ,FmM(B  ,FlL(B  ,;  .:  /?

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
 ("`" ?`)
 ("q" ?\;)
 ("w" ?,Fr(B)
 ("e" ?,Fe(B)
 ("r" ?,Fq(B)
 ("t" ?,Ft(B)
 ("y" ?,Fu(B)
 ("u" ?,Fh(B)
 ("i" ?,Fi(B)
 ("o" ?,Fo(B)
 ("p" ?,Fp(B)
 ("[" ?\[)
 ("]" ?\])
 ("a" ?,Fa(B)
 ("s" ?,Fs(B)
 ("d" ?,Fd(B)
 ("f" ?,Fv(B)
 ("g" ?,Fc(B)
 ("h" ?,Fg(B)
 ("j" ?,Fn(B)
 ("k" ?,Fj(B)
 ("l" ?,Fk(B)
 (";" ?,F4(B)
 ("'" ?')
 ("\\" ?\\)
 ("z" ?,Ff(B)
 ("x" ?,Fw(B)
 ("c" ?,Fx(B)
 ("v" ?,Fy(B)
 ("b" ?,Fb(B)
 ("n" ?,Fm(B)
 ("m" ?,Fl(B)
 ("," ?,)
 ("." ?.)
 ("/" ?/)

 ("!" ?!)
 ("@" ?@)
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?+)
 ("~" ?~)
 ("Q" ?:)
 ("W" ?,FS(B)
 ("E" ?,FE(B)
 ("R" ?,FQ(B)
 ("T" ?,FT(B)
 ("Y" ?,FU(B)
 ("U" ?,FH(B)
 ("I" ?,FI(B)
 ("O" ?,FO(B)
 ("P" ?,FP(B)
 ("{" ?{)
 ("}" ?})
 ("A" ?,FA(B)
 ("S" ?,FS(B)
 ("D" ?,FD(B)
 ("F" ?,FV(B)
 ("G" ?,FC(B)
 ("H" ?,FG(B)
 ("J" ?,FN(B)
 ("K" ?,FJ(B)
 ("L" ?,FK(B)
 (":" ?,F((B)
 ("\"" ?\")
 ("|" ?|)
 ("Z" ?,FF(B)
 ("X" ?,FW(B)
 ("C" ?,FX(B)
 ("V" ?,FY(B)
 ("B" ?,FB(B)
 ("N" ?,FM(B)
 ("M" ?,FL(B)
 ("<" ?<)
 (">" ?>)
 ("?" ??)

 ("a;" ?,F\(B)
 ("e;" ?,F](B)
 ("h;" ?,F^(B)
 ("i;" ?,F_(B)
 ("o;" ?,F|(B)
 ("y;" ?,F}(B)
 ("v;" ?,F~(B)
 ("A;" ?,F6(B)
 ("E;" ?,F8(B)
 ("H;" ?,F9(B)
 ("I;" ?,F:(B)
 ("O;" ?,F<(B)
 ("Y;" ?,F>(B)
 ("V;" ?,F?(B)
 ("i:" ?,Fz(B)
 ("y:" ?,F{(B)
 ("I:" ?,FZ(B)
 ("Y:" ?,F[(B)
 ("i:;" ?,F@(B)
 ("i;:" ?,F@(B)
 ("y:;" ?,F`(B)
 ("y;:" ?,F`(B)
 ;; These two are asymmetric with ";<" and ";>" in "greek" input
 ;; method.  But, as the other Latin postfix methods adopt "<<" and
 ;; ">>", it may be better to follow them.
 ("<<" ?$(Q)((B)
 (">>" ?$(Q)2(B))


;;; greek.el ends here
