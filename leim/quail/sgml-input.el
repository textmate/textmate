;;; sgml-input.el --- Quail method for Unicode entered as SGML entities -*- coding: utf-8 -*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

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

;; The table was derived from the Unicode consortium file
;; MAPPINGS/VENDORS/MISC/SGML.TXT.

;;; Code:

(require 'quail)

(quail-define-package
 "sgml" "UTF-8" "&" t
 "Unicode characters input method using SGML entities.
Entities are covered from the public sets ISOamsa, ISOamsb, ISOamsc,
ISOamsn, ISOamso, ISOamsr, ISObox, ISOcyr1, ISOcyr2, ISOdia, ISOgrk1,
ISOgrk2, ISOgrk3, ISOgrk4, ISOlat1, ISOlat2, ISOnum, ISOpub, ISOtech,
HTMLspecial and HTMLsymbol.

E.g.: &aacute; -> á"
 '(("\t" . quail-completion))
 t nil nil nil nil nil nil nil nil t)

(quail-define-rules
  ("&Aacgr;" ?\Ά) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
  ("&aacgr;" ?\ά) ;; GREEK SMALL LETTER ALPHA WITH TONOS
  ("&Aacute;" ?\Á) ;; LATIN CAPITAL LETTER A WITH ACUTE
  ("&aacute;" ?\á) ;; LATIN SMALL LETTER A WITH ACUTE
  ("&Abreve;" ?\Ă) ;; LATIN CAPITAL LETTER A WITH BREVE
  ("&abreve;" ?\ă) ;; LATIN SMALL LETTER A WITH BREVE
  ("&Acirc;" ?\Â) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
  ("&acirc;" ?\â) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
  ("&acute;" ?\´) ;; ACUTE ACCENT
  ("&Acy;" ?\А) ;; CYRILLIC CAPITAL LETTER A
  ("&acy;" ?\а) ;; CYRILLIC SMALL LETTER A
  ("&AElig;" ?\Æ) ;; LATIN CAPITAL LETTER AE
  ("&aelig;" ?\æ) ;; LATIN SMALL LETTER AE
  ("&Agr;" ?\Α) ;; GREEK CAPITAL LETTER ALPHA
  ("&agr;" ?\α) ;; GREEK SMALL LETTER ALPHA
  ("&Agrave;" ?\À) ;; LATIN CAPITAL LETTER A WITH GRAVE
  ("&agrave;" ?\à) ;; LATIN SMALL LETTER A WITH GRAVE
  ("&alefsym;" ?\ℵ) ;; ALEF SYMBOL
  ("&aleph;" ?\ℵ) ;; ALEF SYMBOL
  ("&Alpha;" ?\Α) ;; GREEK CAPITAL LETTER ALPHA
  ("&alpha;" ?\α) ;; GREEK SMALL LETTER ALPHA
  ("&Amacr;" ?\Ā) ;; LATIN CAPITAL LETTER A WITH MACRON
  ("&amacr;" ?\ā) ;; LATIN SMALL LETTER A WITH MACRON
  ("&amalg;" ?\∐) ;; N-ARY COPRODUCT
  ("&amp;" ?\&) ;; AMPERSAND
  ("&and;" ?\∧) ;; LOGICAL AND
  ("&ang;" ?\∠) ;; ANGLE
  ("&ang90;" ?\∟) ;; RIGHT ANGLE
  ("&angmsd;" ?\∡) ;; MEASURED ANGLE
  ("&angsph;" ?\∢) ;; SPHERICAL ANGLE
  ("&angst;" ?\Å) ;; ANGSTROM SIGN
  ("&Aogon;" ?\Ą) ;; LATIN CAPITAL LETTER A WITH OGONEK
  ("&aogon;" ?\ą) ;; LATIN SMALL LETTER A WITH OGONEK
  ("&ap;" ?\≈) ;; ALMOST EQUAL TO
  ("&ape;" ?\≊) ;; ALMOST EQUAL OR EQUAL TO
  ("&apos;" ?\ʼ) ;; MODIFIER LETTER APOSTROPHE
  ("&Aring;" ?\Å) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
  ("&aring;" ?\å) ;; LATIN SMALL LETTER A WITH RING ABOVE
  ("&ast;" ?\*) ;; ASTERISK
  ("&asymp;" ?\≈) ;; ALMOST EQUAL TO
  ("&Atilde;" ?\Ã) ;; LATIN CAPITAL LETTER A WITH TILDE
  ("&atilde;" ?\ã) ;; LATIN SMALL LETTER A WITH TILDE
  ("&Auml;" ?\Ä) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
  ("&auml;" ?\ä) ;; LATIN SMALL LETTER A WITH DIAERESIS
  ("&b.alpha;" ?\α) ;; GREEK SMALL LETTER ALPHA
  ("&barwed;" ?\⊼) ;; NAND
  ("&Barwed;" ?\⌆) ;; PERSPECTIVE
  ("&b.beta;" ?\β) ;; GREEK SMALL LETTER BETA
  ("&bchi;" ?\χ) ;; GREEK SMALL LETTER CHI
  ("&bcong;" ?\≌) ;; ALL EQUAL TO
  ("&Bcy;" ?\Б) ;; CYRILLIC CAPITAL LETTER BE
  ("&bcy;" ?\б) ;; CYRILLIC SMALL LETTER BE
  ("&b.Delta;" ?\Δ) ;; GREEK CAPITAL LETTER DELTA
  ("&b.delta;" ?\γ) ;; GREEK SMALL LETTER GAMMA
  ("&bdquo;" ?\„) ;; DOUBLE LOW-9 QUOTATION MARK
  ("&becaus;" ?\∵) ;; BECAUSE
  ("&bepsi;" ?\∍) ;; SMALL CONTAINS AS MEMBER
  ("&b.epsi;" ?\ε) ;; GREEK SMALL LETTER EPSILON
  ("&b.epsis;" ?\ε) ;; GREEK SMALL LETTER EPSILON
  ("&b.epsiv;" ?\ε) ;; GREEK SMALL LETTER EPSILON
  ("&bernou;" ?\ℬ) ;; SCRIPT CAPITAL B
  ("&Beta;" ?\Β) ;; GREEK CAPITAL LETTER BETA
  ("&beta;" ?\β) ;; GREEK SMALL LETTER BETA
  ("&b.eta;" ?\η) ;; GREEK SMALL LETTER ETA
  ("&beth;" ?\ℶ) ;; BET SYMBOL
  ("&b.Gamma;" ?\Γ) ;; GREEK CAPITAL LETTER GAMMA
  ("&b.gamma;" ?\γ) ;; GREEK SMALL LETTER GAMMA
  ("&b.gammagrk4;" ?\Ϝ) ;; GREEK LETTER DIGAMMA
  ("&Bgr;" ?\Β) ;; GREEK CAPITAL LETTER BETA
  ("&bgr;" ?\β) ;; GREEK SMALL LETTER BETA
  ("&b.iota;" ?\ι) ;; GREEK SMALL LETTER IOTA
  ("&b.kappa;" ?\κ) ;; GREEK SMALL LETTER KAPPA
  ("&b.kappagrk4;" ?\ϰ) ;; GREEK KAPPA SYMBOL
  ("&b.Lambdgrk4;" ?\Λ) ;; GREEK CAPITAL LETTER LAMDA
  ("&b.lambdgrk4;" ?\λ) ;; GREEK SMALL LETTER LAMDA
  ("&blank;" ?\␣) ;; OPEN BOX
  ("&blk12;" ?\▒) ;; MEDIUM SHADE
  ("&blk14;" ?\░) ;; LIGHT SHADE
  ("&blk34;" ?\▓) ;; DARK SHADE
  ("&block;" ?\█) ;; FULL BLOCK
  ("&b.mu;" ?\μ) ;; GREEK SMALL LETTER MU
  ("&b.nu;" ?\ν) ;; GREEK SMALL LETTER NU
  ("&b.Omega;" ?\Ω) ;; GREEK CAPITAL LETTER OMEGA
  ("&b.omega;" ?\ώ) ;; GREEK SMALL LETTER OMEGA WITH TONOS
  ("&bottom;" ?\⊥) ;; UP TACK
  ("&bowtie;" ?\⋈) ;; BOWTIE
  ("&boxdl;" ?\┐) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
  ("&boxdL;" ?\╕) ;; BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
  ("&boxDl;" ?\╖) ;; BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
  ("&boxDL;" ?\╗) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
  ("&boxdr;" ?\┌) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
  ("&boxdR;" ?\╒) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
  ("&boxDr;" ?\╓) ;; BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
  ("&boxDR;" ?\╔) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
  ("&boxh;" ?\─) ;; BOX DRAWINGS LIGHT HORIZONTAL
  ("&boxH;" ?\═) ;; BOX DRAWINGS DOUBLE HORIZONTAL
  ("&boxhd;" ?\┬) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
  ("&boxHd;" ?\╤) ;; BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
  ("&boxhD;" ?\╥) ;; BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
  ("&boxHD;" ?\╦) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
  ("&boxhu;" ?\┴) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
  ("&boxHu;" ?\╧) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
  ("&boxhU;" ?\╨) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
  ("&boxHU;" ?\╩) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
  ("&boxul;" ?\┘) ;; BOX DRAWINGS LIGHT UP AND LEFT
  ("&boxuL;" ?\╛) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
  ("&boxUl;" ?\╜) ;; BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
  ("&boxUL;" ?\╝) ;; BOX DRAWINGS DOUBLE UP AND LEFT
  ("&boxur;" ?\└) ;; BOX DRAWINGS LIGHT UP AND RIGHT
  ("&boxuR;" ?\╘) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
  ("&boxUr;" ?\╙) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
  ("&boxUR;" ?\╚) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
  ("&boxv;" ?\│) ;; BOX DRAWINGS LIGHT VERTICAL
  ("&boxV;" ?\║) ;; BOX DRAWINGS DOUBLE VERTICAL
  ("&boxvh;" ?\┼) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
  ("&boxvH;" ?\╪) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
  ("&boxVh;" ?\╫) ;; BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
  ("&boxVH;" ?\╬) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
  ("&boxvl;" ?\┤) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
  ("&boxvL;" ?\╡) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
  ("&boxVl;" ?\╢) ;; BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
  ("&boxVL;" ?\╣) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
  ("&boxvr;" ?\├) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
  ("&boxvR;" ?\╞) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
  ("&boxVr;" ?\╟) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
  ("&boxVR;" ?\╠) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
  ("&b.Phi;" ?\Φ) ;; GREEK CAPITAL LETTER PHI
  ("&b.phis;" ?\φ) ;; GREEK SMALL LETTER PHI
  ("&b.phiv;" ?\ϕ) ;; GREEK PHI SYMBOL
  ("&b.Pi;" ?\Π) ;; GREEK CAPITAL LETTER PI
  ("&b.pi;" ?\π) ;; GREEK SMALL LETTER PI
  ("&b.piv;" ?\ϖ) ;; GREEK PI SYMBOL
  ("&bprime;" ?\‵) ;; REVERSED PRIME
  ("&b.Psi;" ?\Ψ) ;; GREEK CAPITAL LETTER PSI
  ("&b.psi;" ?\ψ) ;; GREEK SMALL LETTER PSI
  ("&breve;" ?\˘) ;; BREVE
  ("&b.rho;" ?\ρ) ;; GREEK SMALL LETTER RHO
  ("&b.rhov;" ?\ϱ) ;; GREEK RHO SYMBOL
  ("&brvbar;" ?\¦) ;; BROKEN BAR
  ("&b.Sigma;" ?\Σ) ;; GREEK CAPITAL LETTER SIGMA
  ("&b.sigma;" ?\σ) ;; GREEK SMALL LETTER SIGMA
  ("&b.sigmagrk4;" ?\ς) ;; GREEK SMALL LETTER FINAL SIGMA
  ("&bsim;" ?\∽) ;; REVERSED TILDE
  ("&bsime;" ?\⋍) ;; REVERSED TILDE EQUALS
  ("&bsol;" ?\\) ;; REVERSE SOLIDUS
  ("&b.tau;" ?\τ) ;; GREEK SMALL LETTER TAU
  ("&b.Theta;" ?\Θ) ;; GREEK CAPITAL LETTER THETA
  ("&b.thetagrk4;" ?\θ) ;; GREEK SMALL LETTER THETA
  ("&b.thetagrk4;" ?\ϑ) ;; GREEK THETA SYMBOL
  ("&bull;" ?\•) ;; BULLET
  ("&bump;" ?\≎) ;; GEOMETRICALLY EQUIVALENT TO
  ("&bumpe;" ?\≏) ;; DIFFERENCE BETWEEN
  ("&b.Upsi;" ?\Υ) ;; GREEK CAPITAL LETTER UPSILON
  ("&b.upsi;" ?\υ) ;; GREEK SMALL LETTER UPSILON
  ("&b.Xi;" ?\Ξ) ;; GREEK CAPITAL LETTER XI
  ("&b.xi;" ?\ξ) ;; GREEK SMALL LETTER XI
  ("&b.zeta;" ?\ζ) ;; GREEK SMALL LETTER ZETA
  ("&Cacute;" ?\Ć) ;; LATIN CAPITAL LETTER C WITH ACUTE
  ("&cacute;" ?\ć) ;; LATIN SMALL LETTER C WITH ACUTE
  ("&Cap;" ?\⋒) ;; DOUBLE INTERSECTION
  ("&cap;" ?\∩) ;; INTERSECTION
  ("&caret;" ?\⁁) ;; CARET INSERTION POINT
  ("&caron;" ?\ˇ) ;; CARON
  ("&Ccaron;" ?\Č) ;; LATIN CAPITAL LETTER C WITH CARON
  ("&ccaron;" ?\č) ;; LATIN SMALL LETTER C WITH CARON
  ("&Ccedil;" ?\Ç) ;; LATIN CAPITAL LETTER C WITH CEDILLA
  ("&ccedil;" ?\ç) ;; LATIN SMALL LETTER C WITH CEDILLA
  ("&Ccirc;" ?\Ĉ) ;; LATIN CAPITAL LETTER C WITH CIRCUMFLEX
  ("&ccirc;" ?\ĉ) ;; LATIN SMALL LETTER C WITH CIRCUMFLEX
  ("&Cdot;" ?\Ċ) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
  ("&cdot;" ?\ċ) ;; LATIN SMALL LETTER C WITH DOT ABOVE
  ("&cedil;" ?\¸) ;; CEDILLA
  ("&cent;" ?\¢) ;; CENT SIGN
  ("&CHcy;" ?\Ч) ;; CYRILLIC CAPITAL LETTER CHE
  ("&chcy;" ?\ч)  ;; CYRILLIC SMALL LETTER CHE
  ("&check;" ?\✓) ;; CHECK MARK
  ("&Chi;" ?\Χ) ;; GREEK CAPITAL LETTER CHI
  ("&chi;" ?\χ) ;; GREEK SMALL LETTER CHI
  ("&cir;" ?\○) ;; WHITE CIRCLE
  ("&circ;" ?\ˆ) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
  ("&cire;" ?\≗) ;; RING EQUAL TO
  ("&clubs;" ?\♣) ;; BLACK CLUB SUIT
  ("&colon;" ?\:) ;; COLON
  ("&colone;" ?\≔) ;; COLON EQUALS
  ("&comma;" ?\,) ;; COMMA
  ("&commat;" ?\@) ;; COMMERCIAL AT
  ("&comp;" ?\∁) ;; COMPLEMENT
  ("&compfn;" ?\∘) ;; RING OPERATOR
  ("&cong;" ?\≅) ;; APPROXIMATELY EQUAL TO
  ("&conint;" ?\∮) ;; CONTOUR INTEGRAL
  ("&coprod;" ?\∐) ;; N-ARY COPRODUCT
  ("&copy;" ?\©) ;; COPYRIGHT SIGN
  ("&copysr;" ?\℗) ;; SOUND RECORDING COPYRIGHT
  ("&crarr;" ?\↵) ;; DOWNWARDS ARROW WITH CORNER LEFTWARDS
  ("&cross;" ?\✗) ;; BALLOT X
  ("&cuepr;" ?\⋞) ;; EQUAL TO OR PRECEDES
  ("&cuesc;" ?\⋟) ;; EQUAL TO OR SUCCEEDS
  ("&cularr;" ?\↶) ;; ANTICLOCKWISE TOP SEMICIRCLE ARROW
  ("&Cup;" ?\⋓) ;; DOUBLE UNION
  ("&cup;" ?\∪) ;; UNION
  ("&cupre;" ?\≼) ;; PRECEDES OR EQUAL TO
  ("&curarr;" ?\↷) ;; CLOCKWISE TOP SEMICIRCLE ARROW
  ("&curren;" ?\¤) ;; CURRENCY SIGN
  ("&cuvee;" ?\⋎) ;; CURLY LOGICAL OR
  ("&cuwed;" ?\⋏) ;; CURLY LOGICAL AND
  ("&dagger;" ?\†) ;; DAGGER
  ("&Dagger;" ?\‡) ;; DOUBLE DAGGER
  ("&daleth;" ?\ℸ) ;; DALET SYMBOL
  ("&dArr;" ?\⇓) ;; DOWNWARDS DOUBLE ARROW
  ("&darr;" ?\↓) ;; DOWNWARDS ARROW
  ("&darr2;" ?\⇊) ;; DOWNWARDS PAIRED ARROWS
  ("&dash;" ?\‐) ;; HYPHEN
  ("&dashv;" ?\⊣) ;; LEFT TACK
  ("&dblac;" ?\˝) ;; DOUBLE ACUTE ACCENT
  ("&Dcaron;" ?\Ď) ;; LATIN CAPITAL LETTER D WITH CARON
  ("&dcaron;" ?\ď) ;; LATIN SMALL LETTER D WITH CARON
  ("&Dcy;" ?\Д) ;; CYRILLIC CAPITAL LETTER DE
  ("&dcy;" ?\д) ;; CYRILLIC SMALL LETTER DE
  ("&deg;" ?\°) ;; DEGREE SIGN
  ("&Delta;" ?\Δ) ;; GREEK CAPITAL LETTER DELTA
  ("&delta;" ?\δ) ;; GREEK SMALL LETTER DELTA
  ("&Dgr;" ?\Δ) ;; GREEK CAPITAL LETTER DELTA
  ("&dgr;" ?\δ) ;; GREEK SMALL LETTER DELTA
  ("&dharl;" ?\⇃) ;; DOWNWARDS HARPOON WITH BARB LEFTWARDS
  ("&dharr;" ?\⇂) ;; DOWNWARDS HARPOON WITH BARB RIGHTWARDS
  ("&diam;" ?\⋄) ;; DIAMOND OPERATOR
  ("&diams;" ?\♦) ;; BLACK DIAMOND SUIT
  ("&die;" ?\¨) ;; DIAERESIS
  ("&divide;" ?\÷) ;; DIVISION SIGN
  ("&divonx;" ?\⋇) ;; DIVISION TIMES
  ("&DJcy;" ?\Ђ) ;; CYRILLIC CAPITAL LETTER DJE
  ("&djcy;" ?\ђ) ;; CYRILLIC SMALL LETTER DJE
  ("&dlarr;" ?\↙) ;; SOUTH WEST ARROW
  ("&dlcorn;" ?\⌞) ;; BOTTOM LEFT CORNER
  ("&dlcrop;" ?\⌍) ;; BOTTOM LEFT CROP
  ("&dollar;" ?\$) ;; DOLLAR SIGN
  ("&dot;" ?\˙) ;; DOT ABOVE
  ("&Dot;" ?\¨) ;; DIAERESIS
  ("&DotDot;" ?\⃜) ;; COMBINING FOUR DOTS ABOVE
  ("&drarr;" ?\↘) ;; SOUTH EAST ARROW
  ("&drcorn;" ?\⌟) ;; BOTTOM RIGHT CORNER
  ("&drcrop;" ?\⌌) ;; BOTTOM RIGHT CROP
  ("&DScy;" ?\Ѕ) ;; CYRILLIC CAPITAL LETTER DZE
  ("&dscy;" ?\ѕ) ;; CYRILLIC SMALL LETTER DZE
  ("&Dstrok;" ?\Đ) ;; LATIN CAPITAL LETTER D WITH STROKE
  ("&dstrok;" ?\đ) ;; LATIN SMALL LETTER D WITH STROKE
  ("&dtri;" ?\▿) ;; WHITE DOWN-POINTING SMALL TRIANGLE
  ("&dtrif;" ?\▾) ;; BLACK DOWN-POINTING SMALL TRIANGLE
  ("&DZcy;" ?\Џ) ;; CYRILLIC CAPITAL LETTER DZHE
  ("&dzcy;" ?\џ) ;; CYRILLIC SMALL LETTER DZHE
  ("&Eacgr;" ?\Έ) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
  ("&eacgr;" ?\έ) ;; GREEK SMALL LETTER EPSILON WITH TONOS
  ("&Eacute;" ?\É) ;; LATIN CAPITAL LETTER E WITH ACUTE
  ("&eacute;" ?\é) ;; LATIN SMALL LETTER E WITH ACUTE
  ("&Ecaron;" ?\Ě) ;; LATIN CAPITAL LETTER E WITH CARON
  ("&ecaron;" ?\ě) ;; LATIN SMALL LETTER E WITH CARON
  ("&ecir;" ?\≖) ;; RING IN EQUAL TO
  ("&Ecirc;" ?\Ê) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
  ("&ecirc;" ?\ê) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
  ("&ecolon;" ?\≕) ;; EQUALS COLON
  ("&Ecy;" ?\Э) ;; CYRILLIC CAPITAL LETTER E
  ("&ecy;" ?\э) ;; CYRILLIC SMALL LETTER E
  ("&eDot;" ?\≑) ;; GEOMETRICALLY EQUAL TO
  ("&Edot;" ?\Ė) ;; LATIN CAPITAL LETTER E WITH DOT ABOVE
  ("&edot;" ?\ė) ;; LATIN SMALL LETTER E WITH DOT ABOVE
  ("&EEacgr;" ?\Ή) ;; GREEK CAPITAL LETTER ETA WITH TONOS
  ("&eeacgr;" ?\ή) ;; GREEK SMALL LETTER ETA WITH TONOS
  ("&EEgr;" ?\Η) ;; GREEK CAPITAL LETTER ETA
  ("&eegr;" ?\η) ;; GREEK SMALL LETTER ETA
  ("&efDot;" ?\≒) ;; APPROXIMATELY EQUAL TO OR THE IMAGE OF
  ("&Egr;" ?\Ε) ;; GREEK CAPITAL LETTER EPSILON
  ("&egr;" ?\ε) ;; GREEK SMALL LETTER EPSILON
  ("&Egrave;" ?\È) ;; LATIN CAPITAL LETTER E WITH GRAVE
  ("&egrave;" ?\è) ;; LATIN SMALL LETTER E WITH GRAVE
  ("&egs;" ?\⋝) ;; EQUAL TO OR GREATER-THAN
  ("&ell;" ?\ℓ) ;; SCRIPT SMALL L
  ("&els;" ?\⋜) ;; EQUAL TO OR LESS-THAN
  ("&Emacr;" ?\Ē) ;; LATIN CAPITAL LETTER E WITH MACRON
  ("&emacr;" ?\ē) ;; LATIN SMALL LETTER E WITH MACRON
  ("&empty;" ?\∅) ;; EMPTY SET
  ("&emsp;" ?\ ) ;; EM SPACE
  ("&emsp13;" ?\ ) ;; THREE-PER-EM SPACE
  ("&emsp14;" ?\ ) ;; FOUR-PER-EM SPACE
  ("&ENG;" ?\Ŋ) ;; LATIN CAPITAL LETTER ENG
  ("&eng;" ?\ŋ) ;; LATIN SMALL LETTER ENG
  ("&ensp;" ?\ ) ;; EN SPACE
  ("&Eogon;" ?\Ę) ;; LATIN CAPITAL LETTER E WITH OGONEK
  ("&eogon;" ?\ę) ;; LATIN SMALL LETTER E WITH OGONEK
  ("&epsi;" ?\ε) ;; GREEK SMALL LETTER EPSILON
  ("&Epsilon;" ?\Ε) ;; GREEK CAPITAL LETTER EPSILON
  ("&epsilon;" ?\ε) ;; GREEK SMALL LETTER EPSILON
  ("&epsis;" ?\∊) ;; SMALL ELEMENT OF
;;;  ("&epsiv;" ?\x????) ;; variant epsilon
  ("&equals;" ?\=) ;; EQUALS SIGN
  ("&equiv;" ?\≡) ;; IDENTICAL TO
  ("&erDot;" ?\≓) ;; IMAGE OF OR APPROXIMATELY EQUAL TO
  ("&esdot;" ?\≐) ;; APPROACHES THE LIMIT
  ("&Eta;" ?\Η) ;; GREEK CAPITAL LETTER ETA
  ("&eta;" ?\η) ;; GREEK SMALL LETTER ETA
  ("&ETH;" ?\Ð) ;; LATIN CAPITAL LETTER ETH
  ("&eth;" ?\ð) ;; LATIN SMALL LETTER ETH
  ("&Euml;" ?\Ë) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
  ("&euml;" ?\ë) ;; LATIN SMALL LETTER E WITH DIAERESIS
  ("&excl;" ?\!) ;; EXCLAMATION MARK
  ("&exist;" ?\∃) ;; THERE EXISTS
  ("&Fcy;" ?\Ф) ;; CYRILLIC CAPITAL LETTER EF
  ("&fcy;" ?\ф) ;; CYRILLIC SMALL LETTER EF
  ("&female;" ?\♀) ;; FEMALE SIGN
  ("&ffilig;" ?\ﬃ) ;; LATIN SMALL LIGATURE FFI
  ("&fflig;" ?\ﬀ) ;; LATIN SMALL LIGATURE FF
  ("&ffllig;" ?\ﬄ) ;; LATIN SMALL LIGATURE FFL
  ("&filig;" ?\ﬁ) ;; LATIN SMALL LIGATURE FI
;;  ("&fjlig;" ?\x????) ;; fj ligature
  ("&flat;" ?\♭) ;; MUSIC FLAT SIGN
  ("&fllig;" ?\ﬂ) ;; LATIN SMALL LIGATURE FL
  ("&fnof;" ?\ƒ) ;; LATIN SMALL LETTER F WITH HOOK
  ("&forall;" ?\∀) ;; FOR ALL
  ("&fork;" ?\⋔) ;; PITCHFORK
  ("&frac12;" ?\½) ;; VULGAR FRACTION ONE HALF
  ("&frac13;" ?\⅓) ;; VULGAR FRACTION ONE THIRD
  ("&frac14;" ?\¼) ;; VULGAR FRACTION ONE QUARTER
  ("&frac15;" ?\⅕) ;; VULGAR FRACTION ONE FIFTH
  ("&frac16;" ?\⅙) ;; VULGAR FRACTION ONE SIXTH
  ("&frac18;" ?\⅛) ;; VULGAR FRACTION ONE EIGHTH
  ("&frac23;" ?\⅔) ;; VULGAR FRACTION TWO THIRDS
  ("&frac25;" ?\⅖) ;; VULGAR FRACTION TWO FIFTHS
  ("&frac34;" ?\¾) ;; VULGAR FRACTION THREE QUARTERS
  ("&frac35;" ?\⅗) ;; VULGAR FRACTION THREE FIFTHS
  ("&frac38;" ?\⅜) ;; VULGAR FRACTION THREE EIGHTHS
  ("&frac45;" ?\⅘) ;; VULGAR FRACTION FOUR FIFTHS
  ("&frac56;" ?\⅚) ;; VULGAR FRACTION FIVE SIXTHS
  ("&frac58;" ?\⅝) ;; VULGAR FRACTION FIVE EIGHTHS
  ("&frac78;" ?\⅞) ;; VULGAR FRACTION SEVEN EIGHTHS
  ("&frasl;" ?\⁄) ;; FRACTION SLASH
  ("&frown;" ?\⌢) ;; FROWN
  ("&gacute;" ?\ǵ) ;; LATIN SMALL LETTER G WITH ACUTE
  ("&Gamma;" ?\Γ) ;; GREEK CAPITAL LETTER GAMMA
  ("&gamma;" ?\γ) ;; GREEK SMALL LETTER GAMMA
  ("&gammad;" ?\Ϝ) ;; GREEK LETTER DIGAMMA
;;;  ("&gap;" 0x????) ;; greater-than, approximately equal to
  ("&Gbreve;" ?\Ğ) ;; LATIN CAPITAL LETTER G WITH BREVE
  ("&gbreve;" ?\ğ) ;; LATIN SMALL LETTER G WITH BREVE
  ("&Gcedil;" ?\Ģ) ;; LATIN CAPITAL LETTER G WITH CEDILLA
  ("&gcedil;" ?\ģ) ;; LATIN SMALL LETTER G WITH CEDILLA
  ("&Gcirc;" ?\Ĝ) ;; LATIN CAPITAL LETTER G WITH CIRCUMFLEX
  ("&gcirc;" ?\ĝ) ;; LATIN SMALL LETTER G WITH CIRCUMFLEX
  ("&Gcy;" ?\Г) ;; CYRILLIC CAPITAL LETTER GHE
  ("&gcy;" ?\г) ;; CYRILLIC SMALL LETTER GHE
  ("&Gdot;" ?\Ġ) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
  ("&gdot;" ?\ġ) ;; LATIN SMALL LETTER G WITH DOT ABOVE
  ("&gE;" ?\≧) ;; GREATER-THAN OVER EQUAL TO
  ("&ge;" ?\≥) ;; GREATER-THAN OR EQUAL TO
;;;  ("&gEl;" ?\x????) ;; greater-than, double equals, less-than
  ("&gel;" ?\⋛) ;; GREATER-THAN EQUAL TO OR LESS-THAN
  ("&ges;" ?\≥) ;; GREATER-THAN OR EQUAL TO
  ("&Gg;" ?\⋙) ;; VERY MUCH GREATER-THAN
  ("&Ggr;" ?\Γ) ;; GREEK CAPITAL LETTER GAMMA
  ("&ggr;" ?\γ) ;; GREEK SMALL LETTER GAMMA
  ("&gimel;" ?\ℷ) ;; GIMEL SYMBOL
  ("&GJcy;" ?\Ѓ) ;; CYRILLIC CAPITAL LETTER GJE
  ("&gjcy;" ?\ѓ) ;; CYRILLIC SMALL LETTER GJE
  ("&gl;" ?\≷) ;; GREATER-THAN OR LESS-THAN
;;;  ("&gnap;" ?\x????) ;; greater-than, not approximately equal to
  ("&gne;" ?\≩) ;; GREATER-THAN BUT NOT EQUAL TO
  ("&gnE;" ?\≩) ;; GREATER-THAN BUT NOT EQUAL TO
  ("&gnsim;" ?\⋧) ;; GREATER-THAN BUT NOT EQUIVALENT TO
  ("&grave;" ?\`) ;; GRAVE ACCENT
  ("&gsdot;" ?\⋗) ;; GREATER-THAN WITH DOT
  ("&gsim;" ?\≳) ;; GREATER-THAN OR EQUIVALENT TO
  ("&Gt;" ?\≫) ;; MUCH GREATER-THAN
  ("&gt;" ?\>) ;; GREATER-THAN SIGN
  ("&gvnE;" ?\≩) ;; GREATER-THAN BUT NOT EQUAL TO
  ("&hairsp;" ?\ ) ;; HAIR SPACE
  ("&half;" ?\½) ;; VULGAR FRACTION ONE HALF
  ("&hamilt;" ?\ℋ) ;; SCRIPT CAPITAL H
  ("&HARDcy;" ?\Ъ) ;; CYRILLIC CAPITAL LETTER HARD SIGN
  ("&hardcy;" ?\ъ) ;; CYRILLIC SMALL LETTER HARD SIGN
  ("&harr;" ?\↔) ;; LEFT RIGHT ARROW
  ("&hArr;" ?\⇔) ;; LEFT RIGHT DOUBLE ARROW
  ("&harrw;" ?\↭) ;; LEFT RIGHT WAVE ARROW
  ("&Hcirc;" ?\Ĥ) ;; LATIN CAPITAL LETTER H WITH CIRCUMFLEX
  ("&hcirc;" ?\ĥ) ;; LATIN SMALL LETTER H WITH CIRCUMFLEX
  ("&hearts;" ?\♥) ;; BLACK HEART SUIT
  ("&hellip;" ?\…) ;; HORIZONTAL ELLIPSIS
  ("&horbar;" ?\―) ;; HORIZONTAL BAR
  ("&Hstrok;" ?\Ħ) ;; LATIN CAPITAL LETTER H WITH STROKE
  ("&hstrok;" ?\ħ) ;; LATIN SMALL LETTER H WITH STROKE
  ("&hybull;" ?\⁃) ;; HYPHEN BULLET
  ("&hyphen;" ?\-) ;; HYPHEN-MINUS
  ("&Iacgr;" ?\Ί) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
  ("&iacgr;" ?\ί) ;; GREEK SMALL LETTER IOTA WITH TONOS
  ("&Iacute;" ?\Í) ;; LATIN CAPITAL LETTER I WITH ACUTE
  ("&iacute;" ?\í) ;; LATIN SMALL LETTER I WITH ACUTE
  ("&Icirc;" ?\Î) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
  ("&icirc;" ?\î) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
  ("&Icy;" ?\И) ;; CYRILLIC CAPITAL LETTER I
  ("&icy;" ?\и) ;; CYRILLIC SMALL LETTER I
  ("&idiagr;" ?\ΐ) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
  ("&Idigr;" ?\Ϊ) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
  ("&idigr;" ?\ϊ) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
  ("&Idot;" ?\İ) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
  ("&IEcy;" ?\Е) ;; CYRILLIC CAPITAL LETTER IE
  ("&iecy;" ?\е) ;; CYRILLIC SMALL LETTER IE
  ("&iexcl;" ?\¡) ;; INVERTED EXCLAMATION MARK
  ("&iff;" ?\⇔) ;; LEFT RIGHT DOUBLE ARROW
  ("&Igr;" ?\Ι) ;; GREEK CAPITAL LETTER IOTA
  ("&igr;" ?\ι) ;; GREEK SMALL LETTER IOTA
  ("&Igrave;" ?\Ì) ;; LATIN CAPITAL LETTER I WITH GRAVE
  ("&igrave;" ?\ì) ;; LATIN SMALL LETTER I WITH GRAVE
  ("&IJlig;" ?\Ĳ) ;; LATIN CAPITAL LIGATURE IJ
  ("&ijlig;" ?\ĳ) ;; LATIN SMALL LIGATURE IJ
  ("&Imacr;" ?\Ī) ;; LATIN CAPITAL LETTER I WITH MACRON
  ("&imacr;" ?\ī) ;; LATIN SMALL LETTER I WITH MACRON
  ("&image;" ?\ℑ) ;; BLACK-LETTER CAPITAL I
  ("&incare;" ?\℅) ;; CARE OF
  ("&infin;" ?\∞) ;; INFINITY
  ("&inodot;" ?\ı) ;; LATIN SMALL LETTER DOTLESS I
  ("&inodot;" ?\ı) ;; LATIN SMALL LETTER DOTLESS I
  ("&int;" ?\∫) ;; INTEGRAL
  ("&intcal;" ?\⊺) ;; INTERCALATE
  ("&IOcy;" ?\Ё) ;; CYRILLIC CAPITAL LETTER IO
  ("&iocy;" ?\ё) ;; CYRILLIC SMALL LETTER IO
  ("&Iogon;" ?\Į) ;; LATIN CAPITAL LETTER I WITH OGONEK
  ("&iogon;" ?\į) ;; LATIN SMALL LETTER I WITH OGONEK
  ("&Iota;" ?\Ι) ;; GREEK CAPITAL LETTER IOTA
  ("&iota;" ?\ι) ;; GREEK SMALL LETTER IOTA
  ("&iquest;" ?\¿) ;; INVERTED QUESTION MARK
  ("&isin;" ?\∈) ;; ELEMENT OF
  ("&Itilde;" ?\Ĩ) ;; LATIN CAPITAL LETTER I WITH TILDE
  ("&itilde;" ?\ĩ) ;; LATIN SMALL LETTER I WITH TILDE
  ("&Iukcy;" ?\І) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
  ("&iukcy;" ?\і) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
  ("&Iuml;" ?\Ï) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
  ("&iuml;" ?\ï) ;; LATIN SMALL LETTER I WITH DIAERESIS
  ("&Jcirc;" ?\Ĵ) ;; LATIN CAPITAL LETTER J WITH CIRCUMFLEX
  ("&jcirc;" ?\ĵ) ;; LATIN SMALL LETTER J WITH CIRCUMFLEX
  ("&Jcy;" ?\Й) ;; CYRILLIC CAPITAL LETTER SHORT I
  ("&jcy;" ?\й) ;; CYRILLIC SMALL LETTER SHORT I
;;;  ("&jnodot;" ?\x????) ;; latin small letter dotless j
  ("&Jsercy;" ?\Ј) ;; CYRILLIC CAPITAL LETTER JE
  ("&jsercy;" ?\ј) ;; CYRILLIC SMALL LETTER JE
  ("&Jukcy;" ?\Є) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
  ("&jukcy;" ?\є) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
  ("&Kappa;" ?\Κ) ;; GREEK CAPITAL LETTER KAPPA
  ("&kappa;" ?\κ) ;; GREEK SMALL LETTER KAPPA
  ("&kappav;" ?\ϰ) ;; GREEK KAPPA SYMBOL
  ("&Kcedil;" ?\Ķ) ;; LATIN CAPITAL LETTER K WITH CEDILLA
  ("&kcedil;" ?\ķ) ;; LATIN SMALL LETTER K WITH CEDILLA
  ("&Kcy;" ?\К) ;; CYRILLIC CAPITAL LETTER KA
  ("&kcy;" ?\к) ;; CYRILLIC SMALL LETTER KA
  ("&Kgr;" ?\Κ) ;; GREEK CAPITAL LETTER KAPPA
  ("&kgr;" ?\κ) ;; GREEK SMALL LETTER KAPPA
  ("&kgreen;" ?\ĸ) ;; LATIN SMALL LETTER KRA
  ("&KHcy;" ?\Х) ;; CYRILLIC CAPITAL LETTER HA
  ("&khcy;" ?\х) ;; CYRILLIC SMALL LETTER HA
  ("&KHgr;" ?\Χ) ;; GREEK CAPITAL LETTER CHI
  ("&khgr;" ?\χ) ;; GREEK SMALL LETTER CHI
  ("&KJcy;" ?\Ќ) ;; CYRILLIC CAPITAL LETTER KJE
  ("&kjcy;" ?\ќ) ;; CYRILLIC SMALL LETTER KJE
  ("&lAarr;" ?\⇚) ;; LEFTWARDS TRIPLE ARROW
  ("&Lacute;" ?\Ĺ) ;; LATIN CAPITAL LETTER L WITH ACUTE
  ("&lacute;" ?\ĺ) ;; LATIN SMALL LETTER L WITH ACUTE
  ("&lagran;" ?\ℒ) ;; SCRIPT CAPITAL L
  ("&Lambda;" ?\Λ) ;; GREEK CAPITAL LETTER LAMDA
  ("&lambda;" ?\λ) ;; GREEK SMALL LETTER LAMDA
  ("&lang;" ?\〈) ;; LEFT-POINTING ANGLE BRACKET
;;;  ("&lap;" ?\x????) ;; less-than, approximately equal to
  ("&laquo;" ?\«) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  ("&Larr;" ?\↞) ;; LEFTWARDS TWO HEADED ARROW
  ("&larr;" ?\←) ;; LEFTWARDS ARROW
  ("&lArr;" ?\⇐) ;; LEFTWARDS DOUBLE ARROW
  ("&larr2;" ?\⇇) ;; LEFTWARDS PAIRED ARROWS
  ("&larrhk;" ?\↩) ;; LEFTWARDS ARROW WITH HOOK
  ("&larrlp;" ?\↫) ;; LEFTWARDS ARROW WITH LOOP
  ("&larrtl;" ?\↢) ;; LEFTWARDS ARROW WITH TAIL
  ("&Lcaron;" ?\Ľ) ;; LATIN CAPITAL LETTER L WITH CARON
  ("&lcaron;" ?\ľ) ;; LATIN SMALL LETTER L WITH CARON
  ("&Lcedil;" ?\Ļ) ;; LATIN CAPITAL LETTER L WITH CEDILLA
  ("&lcedil;" ?\ļ) ;; LATIN SMALL LETTER L WITH CEDILLA
  ("&lceil;" ?\⌈) ;; LEFT CEILING
  ("&lcub;" ?\{) ;; LEFT CURLY BRACKET
  ("&Lcy;" ?\Л) ;; CYRILLIC CAPITAL LETTER EL
  ("&lcy;" ?\л) ;; CYRILLIC SMALL LETTER EL
  ("&ldot;" ?\⋖) ;; LESS-THAN WITH DOT
  ("&ldquo;" ?\“) ;; LEFT DOUBLE QUOTATION MARK
  ("&ldquor;" ?\„) ;; DOUBLE LOW-9 QUOTATION MARK
  ("&lE;" ?\≦) ;; LESS-THAN OVER EQUAL TO
  ("&le;" ?\≤) ;; LESS-THAN OR EQUAL TO
;;;  ("&lEg;" ?\x????) ;; less-than, double equals, greater-than
  ("&leg;" ?\⋚) ;; LESS-THAN EQUAL TO OR GREATER-THAN
  ("&les;" ?\≤) ;; LESS-THAN OR EQUAL TO
  ("&lfloor;" ?\⌊) ;; LEFT FLOOR
  ("&lg;" ?\≶) ;; LESS-THAN OR GREATER-THAN
  ("&Lgr;" ?\Λ) ;; GREEK CAPITAL LETTER LAMDA
  ("&lgr;" ?\λ) ;; GREEK SMALL LETTER LAMDA
  ("&lhard;" ?\↽) ;; LEFTWARDS HARPOON WITH BARB DOWNWARDS
  ("&lharu;" ?\↼) ;; LEFTWARDS HARPOON WITH BARB UPWARDS
  ("&lhblk;" ?\▄) ;; LOWER HALF BLOCK
  ("&LJcy;" ?\Љ) ;; CYRILLIC CAPITAL LETTER LJE
  ("&ljcy;" ?\љ) ;; CYRILLIC SMALL LETTER LJE
  ("&Ll;" ?\⋘) ;; VERY MUCH LESS-THAN
  ("&Lmidot;" ?\Ŀ) ;; LATIN CAPITAL LETTER L WITH MIDDLE DOT
  ("&lmidot;" ?\ŀ) ;; LATIN SMALL LETTER L WITH MIDDLE DOT
;;;  ("&lnap;" 0x????) ;; less-than, not approximately equal to
  ("&lnE;" ?\≨) ;; LESS-THAN BUT NOT EQUAL TO
  ("&lne;" ?\≨) ;; LESS-THAN BUT NOT EQUAL TO
  ("&lnsim;" ?\⋦) ;; LESS-THAN BUT NOT EQUIVALENT TO
  ("&lowast;" ?\∗) ;; ASTERISK OPERATOR
  ("&lowbar;" ?\_) ;; LOW LINE
  ("&loz;" ?\◊) ;; LOZENGE
  ("&loz;" ?\✧) ;; WHITE FOUR POINTED STAR
  ("&lozf;" ?\✦) ;; BLACK FOUR POINTED STAR
  ("&lpar;" ?\() ;; LEFT PARENTHESIS
;;;  ("&lpargt;" ?\x????) ;; left parenthesis, greater-than
  ("&lrarr2;" ?\⇆) ;; LEFTWARDS ARROW OVER RIGHTWARDS ARROW
  ("&lrhar2;" ?\⇋) ;; LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
  ("&lrm;" ?\‎) ;; LEFT-TO-RIGHT MARK
  ("&lsaquo;" ?\‹) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
  ("&lsh;" ?\↰) ;; UPWARDS ARROW WITH TIP LEFTWARDS
  ("&lsim;" ?\≲) ;; LESS-THAN OR EQUIVALENT TO
  ("&lsqb;" ?\[) ;; LEFT SQUARE BRACKET
  ("&lsquo;" ?\‘) ;; LEFT SINGLE QUOTATION MARK
  ("&lsquor;" ?\‚) ;; SINGLE LOW-9 QUOTATION MARK
  ("&Lstrok;" ?\Ł) ;; LATIN CAPITAL LETTER L WITH STROKE
  ("&lstrok;" ?\ł) ;; LATIN SMALL LETTER L WITH STROKE
  ("&Lt;" ?\≪) ;; MUCH LESS-THAN
  ("&lt;" ?\<) ;; LESS-THAN SIGN
  ("&lthree;" ?\⋋) ;; LEFT SEMIDIRECT PRODUCT
  ("&ltimes;" ?\⋉) ;; LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
  ("&ltri;" ?\◃) ;; WHITE LEFT-POINTING SMALL TRIANGLE
  ("&ltrie;" ?\⊴) ;; NORMAL SUBGROUP OF OR EQUAL TO
  ("&ltrif;" ?\◂) ;; BLACK LEFT-POINTING SMALL TRIANGLE
  ("&lvnE;" ?\≨) ;; LESS-THAN BUT NOT EQUAL TO
  ("&macr;" ?\¯) ;; MACRON
  ("&male;" ?\♂) ;; MALE SIGN
  ("&malt;" ?\✠) ;; MALTESE CROSS
  ("&map;" ?\↦) ;; RIGHTWARDS ARROW FROM BAR
  ("&marker;" ?\▮) ;; BLACK VERTICAL RECTANGLE
  ("&Mcy;" ?\М) ;; CYRILLIC CAPITAL LETTER EM
  ("&mcy;" ?\м) ;; CYRILLIC SMALL LETTER EM
  ("&mdash;" ?\—) ;; EM DASH
  ("&Mgr;" ?\Μ) ;; GREEK CAPITAL LETTER MU
  ("&mgr;" ?\μ) ;; GREEK SMALL LETTER MU
  ("&micro;" ?\µ) ;; MICRO SIGN
  ("&mid;" ?\∣) ;; DIVIDES
  ("&middot;" ?\·) ;; MIDDLE DOT
  ("&minus;" ?\−) ;; MINUS SIGN
  ("&minusb;" ?\⊟) ;; SQUARED MINUS
  ("&mldr;" ?\…) ;; HORIZONTAL ELLIPSIS
  ("&mnplus;" ?\∓) ;; MINUS-OR-PLUS SIGN
  ("&models;" ?\⊧) ;; MODELS
  ("&Mu;" ?\Μ) ;; GREEK CAPITAL LETTER MU
  ("&mu;" ?\μ) ;; GREEK SMALL LETTER MU
  ("&mumap;" ?\⊸) ;; MULTIMAP
  ("&nabla;" ?\∇) ;; NABLA
  ("&Nacute;" ?\Ń) ;; LATIN CAPITAL LETTER N WITH ACUTE
  ("&nacute;" ?\ń) ;; LATIN SMALL LETTER N WITH ACUTE
  ("&nap;" ?\≉) ;; NOT ALMOST EQUAL TO
  ("&napos;" ?\ŉ) ;; LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
  ("&natur;" ?\♮) ;; MUSIC NATURAL SIGN
  ("&nbsp;" ?\ ) ;; NO-BREAK SPACE
  ("&Ncaron;" ?\Ň) ;; LATIN CAPITAL LETTER N WITH CARON
  ("&ncaron;" ?\ň) ;; LATIN SMALL LETTER N WITH CARON
  ("&Ncedil;" ?\Ņ) ;; LATIN CAPITAL LETTER N WITH CEDILLA
  ("&ncedil;" ?\ņ) ;; LATIN SMALL LETTER N WITH CEDILLA
  ("&ncong;" ?\≇) ;; NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
  ("&Ncy;" ?\Н) ;; CYRILLIC CAPITAL LETTER EN
  ("&ncy;" ?\н) ;; CYRILLIC SMALL LETTER EN
  ("&ndash;" ?\–) ;; EN DASH
  ("&ne;" ?\≠) ;; NOT EQUAL TO
  ("&nearr;" ?\↗) ;; NORTH EAST ARROW
  ("&nequiv;" ?\≢) ;; NOT IDENTICAL TO
  ("&nexist;" ?\∄) ;; THERE DOES NOT EXIST
;;;  ("&ngE;" ?\x????) ;; not greater-than, double equals
  ("&nge;" ?\≱) ;; NEITHER GREATER-THAN NOR EQUAL TO
  ("&nges;" ?\≱) ;; NEITHER GREATER-THAN NOR EQUAL TO
  ("&Ngr;" ?\Ν) ;; GREEK CAPITAL LETTER NU
  ("&ngr;" ?\ν) ;; GREEK SMALL LETTER NU
  ("&ngt;" ?\≯) ;; NOT GREATER-THAN
  ("&nharr;" ?\↮) ;; LEFT RIGHT ARROW WITH STROKE
  ("&nhArr;" ?\⇎) ;; LEFT RIGHT DOUBLE ARROW WITH STROKE
  ("&ni;" ?\∋) ;; CONTAINS AS MEMBER
  ("&NJcy;" ?\Њ) ;; CYRILLIC CAPITAL LETTER NJE
  ("&njcy;" ?\њ) ;; CYRILLIC SMALL LETTER NJE
  ("&nlarr;" ?\↚) ;; LEFTWARDS ARROW WITH STROKE
  ("&nlArr;" ?\⇍) ;; LEFTWARDS DOUBLE ARROW WITH STROKE
  ("&nldr;" ?\‥) ;; TWO DOT LEADER
;;;  ("&nlE;" ?\x????) ;; not less-than, double equals
  ("&nle;" ?\≰) ;; NEITHER LESS-THAN NOR EQUAL TO
  ("&nles;" ?\≰) ;; NEITHER LESS-THAN NOR EQUAL TO
  ("&nlt;" ?\≮) ;; NOT LESS-THAN
  ("&nltri;" ?\⋪) ;; NOT NORMAL SUBGROUP OF
  ("&nltrie;" ?\⋬) ;; NOT NORMAL SUBGROUP OF OR EQUAL TO
  ("&nmid;" ?\∤) ;; DOES NOT DIVIDE
  ("&not;" ?\¬) ;; NOT SIGN
  ("&notin;" ?\∉) ;; NOT AN ELEMENT OF
  ("&npar;" ?\∦) ;; NOT PARALLEL TO
  ("&npr;" ?\⊀) ;; DOES NOT PRECEDE
  ("&npre;" ?\⋠) ;; DOES NOT PRECEDE OR EQUAL
  ("&nrarr;" ?\↛) ;; RIGHTWARDS ARROW WITH STROKE
  ("&nrArr;" ?\⇏) ;; RIGHTWARDS DOUBLE ARROW WITH STROKE
  ("&nrtri;" ?\⋫) ;; DOES NOT CONTAIN AS NORMAL SUBGROUP
  ("&nrtrie;" ?\⋭) ;; DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
  ("&nsc;" ?\⊁) ;; DOES NOT SUCCEED
  ("&nsce;" ?\⋡) ;; DOES NOT SUCCEED OR EQUAL
  ("&nsim;" ?\≁) ;; NOT TILDE
  ("&nsime;" ?\≄) ;; NOT ASYMPTOTICALLY EQUAL TO
;;;  ("&nsmid;" ?\x????) ;; nshortmid ?\∤
  ("&nspar;" ?\∦) ;; NOT PARALLEL TO
  ("&nsub;" ?\⊄) ;; NOT A SUBSET OF
  ("&nsubE;" ?\⊈) ;; NEITHER A SUBSET OF NOR EQUAL TO
  ("&nsube;" ?\⊈) ;; NEITHER A SUBSET OF NOR EQUAL TO
  ("&nsup;" ?\⊅) ;; NOT A SUPERSET OF
  ("&nsupE;" ?\⊉) ;; NEITHER A SUPERSET OF NOR EQUAL TO
  ("&nsupe;" ?\⊉) ;; NEITHER A SUPERSET OF NOR EQUAL TO
  ("&Ntilde;" ?\Ñ) ;; LATIN CAPITAL LETTER N WITH TILDE
  ("&ntilde;" ?\ñ) ;; LATIN SMALL LETTER N WITH TILDE
  ("&Nu;" ?\Ν) ;; GREEK CAPITAL LETTER NU
  ("&nu;" ?\ν) ;; GREEK SMALL LETTER NU
  ("&num;" ?\#) ;; NUMBER SIGN
  ("&numero;" ?\№) ;; NUMERO SIGN
  ("&numsp;" ?\ ) ;; FIGURE SPACE
  ("&nvdash;" ?\⊬) ;; DOES NOT PROVE
  ("&nvDash;" ?\⊭) ;; NOT TRUE
  ("&nVdash;" ?\⊮) ;; DOES NOT FORCE
  ("&nVDash;" ?\⊯) ;; NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
  ("&nwarr;" ?\↖) ;; NORTH WEST ARROW
  ("&Oacgr;" ?\Ό) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
  ("&oacgr;" ?\ό) ;; GREEK SMALL LETTER OMICRON WITH TONOS
  ("&Oacute;" ?\Ó) ;; LATIN CAPITAL LETTER O WITH ACUTE
  ("&oacute;" ?\ó) ;; LATIN SMALL LETTER O WITH ACUTE
  ("&oast;" ?\⊛) ;; CIRCLED ASTERISK OPERATOR
  ("&ocir;" ?\⊚) ;; CIRCLED RING OPERATOR
  ("&Ocirc;" ?\Ô) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
  ("&ocirc;" ?\ô) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
  ("&Ocy;" ?\О) ;; CYRILLIC CAPITAL LETTER O
  ("&ocy;" ?\о) ;; CYRILLIC SMALL LETTER O
  ("&odash;" ?\⊝) ;; CIRCLED DASH
  ("&Odblac;" ?\Ő) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
  ("&odblac;" ?\ő) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
  ("&odot;" ?\⊙) ;; CIRCLED DOT OPERATOR
  ("&OElig;" ?\Œ) ;; LATIN CAPITAL LIGATURE OE
  ("&oelig;" ?\œ) ;; LATIN SMALL LIGATURE OE
  ("&ogon;" ?\˛) ;; OGONEK
  ("&Ogr;" ?\Ο) ;; GREEK CAPITAL LETTER OMICRON
  ("&ogr;" ?\ο) ;; GREEK SMALL LETTER OMICRON
  ("&Ograve;" ?\Ò) ;; LATIN CAPITAL LETTER O WITH GRAVE
  ("&ograve;" ?\ò) ;; LATIN SMALL LETTER O WITH GRAVE
  ("&OHacgr;" ?\Ώ) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
  ("&ohacgr;" ?\ώ) ;; GREEK SMALL LETTER OMEGA WITH TONOS
  ("&OHgr;" ?\Ω) ;; GREEK CAPITAL LETTER OMEGA
  ("&ohgr;" ?\ω) ;; GREEK SMALL LETTER OMEGA
  ("&ohm;" ?\Ω) ;; OHM SIGN
  ("&olarr;" ?\↺) ;; ANTICLOCKWISE OPEN CIRCLE ARROW
  ("&oline;" ?\‾) ;; OVERLINE
  ("&Omacr;" ?\Ō) ;; LATIN CAPITAL LETTER O WITH MACRON
  ("&omacr;" ?\ō) ;; LATIN SMALL LETTER O WITH MACRON
  ("&Omega;" ?\Ω) ;; GREEK CAPITAL LETTER OMEGA
  ("&omega;" ?\ω) ;; GREEK SMALL LETTER OMEGA
  ("&Omicron;" ?\Ο) ;; GREEK CAPITAL LETTER OMICRON
  ("&omicron;" ?\ο) ;; GREEK SMALL LETTER OMICRON
  ("&ominus;" ?\⊖) ;; CIRCLED MINUS
  ("&oplus;" ?\⊕) ;; CIRCLED PLUS
  ("&or;" ?\∨) ;; LOGICAL OR
  ("&orarr;" ?\↻) ;; CLOCKWISE OPEN CIRCLE ARROW
  ("&order;" ?\ℴ) ;; SCRIPT SMALL O
  ("&ordf;" ?\ª) ;; FEMININE ORDINAL INDICATOR
  ("&ordm;" ?\º) ;; MASCULINE ORDINAL INDICATOR
  ("&oS;" ?\Ⓢ) ;; CIRCLED LATIN CAPITAL LETTER S
  ("&Oslash;" ?\Ø) ;; LATIN CAPITAL LETTER O WITH STROKE
  ("&oslash;" ?\ø) ;; LATIN SMALL LETTER O WITH STROKE
  ("&osol;" ?\⊘) ;; CIRCLED DIVISION SLASH
  ("&Otilde;" ?\Õ) ;; LATIN CAPITAL LETTER O WITH TILDE
  ("&otilde;" ?\õ) ;; LATIN SMALL LETTER O WITH TILDE
  ("&otimes;" ?\⊗) ;; CIRCLED TIMES
  ("&Ouml;" ?\Ö) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
  ("&ouml;" ?\ö) ;; LATIN SMALL LETTER O WITH DIAERESIS
  ("&par;" ?\∥) ;; PARALLEL TO
  ("&para;" ?\¶) ;; PILCROW SIGN
  ("&part;" ?\∂) ;; PARTIAL DIFFERENTIAL
  ("&Pcy;" ?\П) ;; CYRILLIC CAPITAL LETTER PE
  ("&pcy;" ?\п) ;; CYRILLIC SMALL LETTER PE
  ("&percnt;" ?\%) ;; PERCENT SIGN
  ("&period;" ?\.) ;; FULL STOP
  ("&permil;" ?\‰) ;; PER MILLE SIGN
  ("&perp;" ?\⊥) ;; UP TACK
  ("&Pgr;" ?\Π) ;; GREEK CAPITAL LETTER PI
  ("&pgr;" ?\π) ;; GREEK SMALL LETTER PI
  ("&PHgr;" ?\Φ) ;; GREEK CAPITAL LETTER PHI
  ("&phgr;" ?\φ) ;; GREEK SMALL LETTER PHI
  ("&phi;" ?\φ) ;; GREEK SMALL LETTER PHI
  ("&Phi;" ?\Φ) ;; GREEK CAPITAL LETTER PHI
  ("&phis;" ?\φ) ;; GREEK SMALL LETTER PHI
  ("&phiv;" ?\ϕ) ;; GREEK PHI SYMBOL
  ("&phmmat;" ?\ℳ) ;; SCRIPT CAPITAL M
  ("&phone;" ?\☎) ;; BLACK TELEPHONE
  ("&Pi;" ?\Π) ;; GREEK CAPITAL LETTER PI
  ("&pi;" ?\π) ;; GREEK SMALL LETTER PI
  ("&piv;" ?\ϖ) ;; GREEK PI SYMBOL
  ("&planck;" ?\ℏ) ;; PLANCK CONSTANT OVER TWO PI
  ("&plus;" ?\+) ;; PLUS SIGN
  ("&plusb;" ?\⊞) ;; SQUARED PLUS
  ("&plusdo;" ?\∔) ;; DOT PLUS
  ("&plusmn;" ?\±) ;; PLUS-MINUS SIGN
  ("&pound;" ?\£) ;; POUND SIGN
  ("&pr;" ?\≺) ;; PRECEDES
;;;  ("&prap;" ?\x????) ;; precedes, approximately equal to
  ("&pre;" ?\≼) ;; PRECEDES OR EQUAL TO
  ("&prime;" ?\′) ;; PRIME
  ("&Prime;" ?\″) ;; DOUBLE PRIME
;;;  ("&prnap;" 0x????) ;; precedes, not approximately equal to
;;;  ("&prnE;" 0x????) ;; precedes, not double equal
  ("&prnsim;" ?\⋨) ;; PRECEDES BUT NOT EQUIVALENT TO
  ("&prod;" ?\∏) ;; N-ARY PRODUCT
  ("&prop;" ?\∝) ;; PROPORTIONAL TO
  ("&prsim;" ?\≾) ;; PRECEDES OR EQUIVALENT TO
  ("&PSgr;" ?\Ψ) ;; GREEK CAPITAL LETTER PSI
  ("&psgr;" ?\ψ) ;; GREEK SMALL LETTER PSI
  ("&Psi;" ?\Ψ) ;; GREEK CAPITAL LETTER PSI
  ("&psi;" ?\ψ) ;; GREEK SMALL LETTER PSI
  ("&puncsp;" ?\ ) ;; PUNCTUATION SPACE
  ("&quest;" ?\?) ;; QUESTION MARK
  ("&quot;" ?\") ;; QUOTATION MARK
  ("&rAarr;" ?\⇛) ;; RIGHTWARDS TRIPLE ARROW
  ("&Racute;" ?\Ŕ) ;; LATIN CAPITAL LETTER R WITH ACUTE
  ("&racute;" ?\ŕ) ;; LATIN SMALL LETTER R WITH ACUTE
  ("&radic;" ?\√) ;; SQUARE ROOT
  ("&rang;" ?\〉) ;; RIGHT-POINTING ANGLE BRACKET
  ("&raquo;" ?\») ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  ("&Rarr;" ?\↠) ;; RIGHTWARDS TWO HEADED ARROW
  ("&rarr;" ?\→) ;; RIGHTWARDS ARROW
  ("&rArr;" ?\⇒) ;; RIGHTWARDS DOUBLE ARROW
  ("&rarr2;" ?\⇉) ;; RIGHTWARDS PAIRED ARROWS
  ("&rarrhk;" ?\↪) ;; RIGHTWARDS ARROW WITH HOOK
  ("&rarrlp;" ?\↬) ;; RIGHTWARDS ARROW WITH LOOP
  ("&rarrtl;" ?\↣) ;; RIGHTWARDS ARROW WITH TAIL
  ("&rarrw;" ?\↝) ;; RIGHTWARDS WAVE ARROW
  ("&Rcaron;" ?\Ř) ;; LATIN CAPITAL LETTER R WITH CARON
  ("&rcaron;" ?\ř) ;; LATIN SMALL LETTER R WITH CARON
  ("&Rcedil;" ?\Ŗ) ;; LATIN CAPITAL LETTER R WITH CEDILLA
  ("&rcedil;" ?\ŗ) ;; LATIN SMALL LETTER R WITH CEDILLA
  ("&rceil;" ?\⌉) ;; RIGHT CEILING
  ("&rcub;" ?\}) ;; RIGHT CURLY BRACKET
  ("&Rcy;" ?\Р) ;; CYRILLIC CAPITAL LETTER ER
  ("&rcy;" ?\р) ;; CYRILLIC SMALL LETTER ER
  ("&rdquo;" ?\”) ;; RIGHT DOUBLE QUOTATION MARK
  ("&rdquor;" ?\“) ;; LEFT DOUBLE QUOTATION MARK
  ("&real;" ?\ℜ) ;; BLACK-LETTER CAPITAL R
  ("&rect;" ?\▭) ;; WHITE RECTANGLE
  ("&reg;" ?\®) ;; REGISTERED SIGN
  ("&rfloor;" ?\⌋) ;; RIGHT FLOOR
  ("&Rgr;" ?\Ρ) ;; GREEK CAPITAL LETTER RHO
  ("&rgr;" ?\ρ) ;; GREEK SMALL LETTER RHO
  ("&rhard;" ?\⇁) ;; RIGHTWARDS HARPOON WITH BARB DOWNWARDS
  ("&rharu;" ?\⇀) ;; RIGHTWARDS HARPOON WITH BARB UPWARDS
  ("&Rho;" ?\Ρ) ;; GREEK CAPITAL LETTER RHO
  ("&rho;" ?\ρ) ;; GREEK SMALL LETTER RHO
  ("&rhov;" ?\ϱ) ;; GREEK RHO SYMBOL
  ("&ring;" ?\˚) ;; RING ABOVE
  ("&rlarr2;" ?\⇄) ;; RIGHTWARDS ARROW OVER LEFTWARDS ARROW
  ("&rlhar2;" ?\⇌) ;; RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
  ("&rlm;" ?\‏) ;; RIGHT-TO-LEFT MARK
  ("&rpar;" ?\)) ;; RIGHT PARENTHESIS
;;;  ("&rpargt;" ?\x????) ;; right parenthesis, greater-than
  ("&rsaquo;" ?\›) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
  ("&rsh;" ?\↱) ;; UPWARDS ARROW WITH TIP RIGHTWARDS
  ("&rsqb;" ?\]) ;; RIGHT SQUARE BRACKET
  ("&rsquo;" ?\’) ;; RIGHT SINGLE QUOTATION MARK
  ("&rsquor;" ?\‘) ;; LEFT SINGLE QUOTATION MARK
  ("&rthree;" ?\⋌) ;; RIGHT SEMIDIRECT PRODUCT
  ("&rtimes;" ?\⋊) ;; RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
  ("&rtri;" ?\▹) ;; WHITE RIGHT-POINTING SMALL TRIANGLE
  ("&rtrie;" ?\⊵) ;; CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
  ("&rtrif;" ?\▸) ;; BLACK RIGHT-POINTING SMALL TRIANGLE
  ("&rx;" ?\℞) ;; PRESCRIPTION TAKE
  ("&Sacute;" ?\Ś) ;; LATIN CAPITAL LETTER S WITH ACUTE
  ("&sacute;" ?\ś) ;; LATIN SMALL LETTER S WITH ACUTE
  ("&samalg;" ?\∐) ;; N-ARY COPRODUCT
  ("&sbquo;" ?\‚) ;; SINGLE LOW-9 QUOTATION MARK
  ("&sbsol;" ?\\) ;; REVERSE SOLIDUS
  ("&sc;" ?\≻) ;; SUCCEEDS
;;;  ("&scap;" ?\x????) ;; succeeds, approximately equal to
  ("&Scaron;" ?\Š) ;; LATIN CAPITAL LETTER S WITH CARON
  ("&scaron;" ?\š) ;; LATIN SMALL LETTER S WITH CARON
  ("&sccue;" ?\≽) ;; SUCCEEDS OR EQUAL TO
  ("&sce;" ?\≽) ;; SUCCEEDS OR EQUAL TO
  ("&Scedil;" ?\Ş) ;; LATIN CAPITAL LETTER S WITH CEDILLA
  ("&scedil;" ?\ş) ;; LATIN SMALL LETTER S WITH CEDILLA
  ("&Scirc;" ?\Ŝ) ;; LATIN CAPITAL LETTER S WITH CIRCUMFLEX
  ("&scirc;" ?\ŝ) ;; LATIN SMALL LETTER S WITH CIRCUMFLEX
;;;  ("&scnap;" ?\x????) ;; succeeds, not approximately equal to
;;;  ("&scnE;" ?\x????) ;; succeeds, not double equals
  ("&scnsim;" ?\⋩) ;; SUCCEEDS BUT NOT EQUIVALENT TO
  ("&scsim;" ?\≿) ;; SUCCEEDS OR EQUIVALENT TO
  ("&Scy;" ?\С) ;; CYRILLIC CAPITAL LETTER ES
  ("&scy;" ?\с) ;; CYRILLIC SMALL LETTER ES
  ("&sdot;" ?\⋅) ;; DOT OPERATOR
  ("&sdotb;" ?\⊡) ;; SQUARED DOT OPERATOR
  ("&sect;" ?\§) ;; SECTION SIGN
  ("&semi;" ?\;) ;; SEMICOLON
  ("&setmn;" ?\∖) ;; SET MINUS
  ("&sext;" ?\✶) ;; SIX POINTED BLACK STAR
  ("&sfgr;" ?\ς) ;; GREEK SMALL LETTER FINAL SIGMA
  ("&sfrown;" ?\⌢) ;; FROWN
  ("&Sgr;" ?\Σ) ;; GREEK CAPITAL LETTER SIGMA
  ("&sgr;" ?\σ) ;; GREEK SMALL LETTER SIGMA
  ("&sharp;" ?\♯) ;; MUSIC SHARP SIGN
  ("&SHCHcy;" ?\Щ) ;; CYRILLIC CAPITAL LETTER SHCHA
  ("&shchcy;" ?\щ) ;; CYRILLIC SMALL LETTER SHCHA
  ("&SHcy;" ?\Ш) ;; CYRILLIC CAPITAL LETTER SHA
  ("&shcy;" ?\ш) ;; CYRILLIC SMALL LETTER SHA
  ("&shy;" ?\­) ;; SOFT HYPHEN
  ("&Sigma;" ?\Σ) ;; GREEK CAPITAL LETTER SIGMA
  ("&sigma;" ?\σ) ;; GREEK SMALL LETTER SIGMA
  ("&sigmaf;" ?\ς) ;; GREEK SMALL LETTER FINAL SIGMA
  ("&sigmav;" ?\ς) ;; GREEK SMALL LETTER FINAL SIGMA
  ("&sim;" ?\∼) ;; TILDE OPERATOR
  ("&sime;" ?\≃) ;; ASYMPTOTICALLY EQUAL TO
;;;  ("&smid;" ?\x????) ;; shortmid ?\∤
  ("&smile;" ?\⌣) ;; SMILE
  ("&SOFTcy;" ?\Ь) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
  ("&softcy;" ?\ь) ;; CYRILLIC SMALL LETTER SOFT SIGN
  ("&sol;" ?\/) ;; SOLIDUS
  ("&spades;" ?\♠) ;; BLACK SPADE SUIT
  ("&spar;" ?\∥) ;; PARALLEL TO
  ("&sqcap;" ?\⊓) ;; SQUARE CAP
  ("&sqcup;" ?\⊔) ;; SQUARE CUP
  ("&sqsub;" ?\⊏) ;; SQUARE IMAGE OF
  ("&sqsube;" ?\⊑) ;; SQUARE IMAGE OF OR EQUAL TO
  ("&sqsup;" ?\⊐) ;; SQUARE ORIGINAL OF
  ("&sqsupe;" ?\⊒) ;; SQUARE ORIGINAL OF OR EQUAL TO
  ("&squ;" ?\□) ;; WHITE SQUARE
  ("&square;" ?\□) ;; WHITE SQUARE
  ("&squf;" ?\▪) ;; BLACK SMALL SQUARE
  ("&ssetmn;" ?\∖) ;; SET MINUS
  ("&ssmile;" ?\⌣) ;; SMILE
  ("&sstarf;" ?\⋆) ;; STAR OPERATOR
  ("&star;" ?\☆) ;; WHITE STAR
  ("&starf;" ?\★) ;; BLACK STAR
  ("&Sub;" ?\⋐) ;; DOUBLE SUBSET
  ("&sub;" ?\⊂) ;; SUBSET OF
  ("&subE;" ?\⊆) ;; SUBSET OF OR EQUAL TO
  ("&sube;" ?\⊆) ;; SUBSET OF OR EQUAL TO
  ("&subnE;" ?\⊊) ;; SUBSET OF WITH NOT EQUAL TO
  ("&subne;" ?\⊊) ;; SUBSET OF WITH NOT EQUAL TO
  ("&sum;" ?\∑) ;; N-ARY SUMMATION
  ("&sung;" ?\♪) ;; EIGHTH NOTE
  ("&Sup;" ?\⋑) ;; DOUBLE SUPERSET
  ("&sup;" ?\⊃) ;; SUPERSET OF
  ("&sup1;" ?\¹) ;; SUPERSCRIPT ONE
  ("&sup2;" ?\²) ;; SUPERSCRIPT TWO
  ("&sup3;" ?\³) ;; SUPERSCRIPT THREE
  ("&supE;" ?\⊇) ;; SUPERSET OF OR EQUAL TO
  ("&supe;" ?\⊇) ;; SUPERSET OF OR EQUAL TO
  ("&supnE;" ?\⊋) ;; SUPERSET OF WITH NOT EQUAL TO
  ("&supne;" ?\⊋) ;; SUPERSET OF WITH NOT EQUAL TO
  ("&szlig;" ?\ß) ;; LATIN SMALL LETTER SHARP S
  ("&target;" ?\⌖) ;; POSITION INDICATOR
  ("&Tau;" ?\Τ) ;; GREEK CAPITAL LETTER TAU
  ("&tau;" ?\τ) ;; GREEK SMALL LETTER TAU
  ("&Tcaron;" ?\Ť) ;; LATIN CAPITAL LETTER T WITH CARON
  ("&tcaron;" ?\ť) ;; LATIN SMALL LETTER T WITH CARON
  ("&Tcedil;" ?\Ţ) ;; LATIN CAPITAL LETTER T WITH CEDILLA
  ("&tcedil;" ?\ţ) ;; LATIN SMALL LETTER T WITH CEDILLA
  ("&Tcy;" ?\Т) ;; CYRILLIC CAPITAL LETTER TE
  ("&tcy;" ?\т) ;; CYRILLIC SMALL LETTER TE
  ("&tdot;" ?\⃛) ;; COMBINING THREE DOTS ABOVE
  ("&telrec;" ?\⌕) ;; TELEPHONE RECORDER
  ("&Tgr;" ?\Τ) ;; GREEK CAPITAL LETTER TAU
  ("&tgr;" ?\τ) ;; GREEK SMALL LETTER TAU
  ("&there4;" ?\∴) ;; THEREFORE
  ("&theta;" ?\θ) ;; GREEK SMALL LETTER THETA
  ("&Theta;" ?\Θ) ;; GREEK CAPITAL LETTER THETA
  ("&thetas;" ?\θ) ;; GREEK SMALL LETTER THETA
  ("&thetasym;" ?\ϑ) ;; GREEK THETA SYMBOL
  ("&thetav;" ?\ϑ) ;; GREEK THETA SYMBOL
  ("&THgr;" ?\Θ) ;; GREEK CAPITAL LETTER THETA
  ("&thgr;" ?\θ) ;; GREEK SMALL LETTER THETA
  ("&thinsp;" ?\ ) ;; THIN SPACE
  ("&thkap;" ?\≈) ;; ALMOST EQUAL TO
  ("&thksim;" ?\∼) ;; TILDE OPERATOR
  ("&THORN;" ?\Þ) ;; LATIN CAPITAL LETTER THORN
  ("&thorn;" ?\þ) ;; LATIN SMALL LETTER THORN
  ("&tilde;" ?\˜) ;; SMALL TILDE
  ("&times;" ?\×) ;; MULTIPLICATION SIGN
  ("&timesb;" ?\⊠) ;; SQUARED TIMES
  ("&top;" ?\⊤) ;; DOWN TACK
  ("&tprime;" ?\‴) ;; TRIPLE PRIME
  ("&trade;" ?\™) ;; TRADE MARK SIGN
  ("&trie;" ?\≜) ;; DELTA EQUAL TO
  ("&TScy;" ?\Ц) ;; CYRILLIC CAPITAL LETTER TSE
  ("&tscy;" ?\ц) ;; CYRILLIC SMALL LETTER TSE
  ("&TSHcy;" ?\Ћ) ;; CYRILLIC CAPITAL LETTER TSHE
  ("&tshcy;" ?\ћ) ;; CYRILLIC SMALL LETTER TSHE
  ("&Tstrok;" ?\Ŧ) ;; LATIN CAPITAL LETTER T WITH STROKE
  ("&tstrok;" ?\ŧ) ;; LATIN SMALL LETTER T WITH STROKE
  ("&twixt;" ?\≬) ;; BETWEEN
  ("&Uacgr;" ?\Ύ) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
  ("&uacgr;" ?\ύ) ;; GREEK SMALL LETTER UPSILON WITH TONOS
  ("&Uacute;" ?\Ú) ;; LATIN CAPITAL LETTER U WITH ACUTE
  ("&uacute;" ?\ú) ;; LATIN SMALL LETTER U WITH ACUTE
  ("&uArr;" ?\⇑) ;; UPWARDS DOUBLE ARROW
  ("&uarr;" ?\↑) ;; UPWARDS ARROW
  ("&uarr2;" ?\⇈) ;; UPWARDS PAIRED ARROWS
  ("&Ubrcy;" ?\Ў) ;; CYRILLIC CAPITAL LETTER SHORT U
  ("&ubrcy;" ?\ў) ;; CYRILLIC SMALL LETTER SHORT U
  ("&Ubreve;" ?\Ŭ) ;; LATIN CAPITAL LETTER U WITH BREVE
  ("&ubreve;" ?\ŭ) ;; LATIN SMALL LETTER U WITH BREVE
  ("&Ucirc;" ?\Û) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  ("&ucirc;" ?\û) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
  ("&Ucy;" ?\У) ;; CYRILLIC CAPITAL LETTER U
  ("&ucy;" ?\у) ;; CYRILLIC SMALL LETTER U
  ("&Udblac;" ?\Ű) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
  ("&udblac;" ?\ű) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
  ("&udiagr;" ?\ΰ) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
  ("&Udigr;" ?\Ϋ) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
  ("&udigr;" ?\ϋ) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
  ("&Ugr;" ?\Υ) ;; GREEK CAPITAL LETTER UPSILON
  ("&ugr;" ?\υ) ;; GREEK SMALL LETTER UPSILON
  ("&Ugrave;" ?\Ù) ;; LATIN CAPITAL LETTER U WITH GRAVE
  ("&ugrave;" ?\ù) ;; LATIN SMALL LETTER U WITH GRAVE
  ("&uharl;" ?\↿) ;; UPWARDS HARPOON WITH BARB LEFTWARDS
  ("&uharr;" ?\↾) ;; UPWARDS HARPOON WITH BARB RIGHTWARDS
  ("&uhblk;" ?\▀) ;; UPPER HALF BLOCK
  ("&ulcorn;" ?\⌜) ;; TOP LEFT CORNER
  ("&ulcrop;" ?\⌏) ;; TOP LEFT CROP
  ("&Umacr;" ?\Ū) ;; LATIN CAPITAL LETTER U WITH MACRON
  ("&umacr;" ?\ū) ;; LATIN SMALL LETTER U WITH MACRON
  ("&uml;" ?\¨) ;; DIAERESIS
  ("&Uogon;" ?\Ų) ;; LATIN CAPITAL LETTER U WITH OGONEK
  ("&uogon;" ?\ų) ;; LATIN SMALL LETTER U WITH OGONEK
  ("&uplus;" ?\⊎) ;; MULTISET UNION
  ("&Upsi;" ?\Υ) ;; GREEK CAPITAL LETTER UPSILON
  ("&upsi;" ?\υ) ;; GREEK SMALL LETTER UPSILON
  ("&upsih;" ?\ϒ) ;; GREEK UPSILON WITH HOOK SYMBOL
  ("&Upsilon;" ?\Υ) ;; GREEK CAPITAL LETTER UPSILON
  ("&upsilon;" ?\υ) ;; GREEK SMALL LETTER UPSILON
  ("&urcorn;" ?\⌝) ;; TOP RIGHT CORNER
  ("&urcrop;" ?\⌎) ;; TOP RIGHT CROP
  ("&Uring;" ?\Ů) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
  ("&uring;" ?\ů) ;; LATIN SMALL LETTER U WITH RING ABOVE
  ("&Utilde;" ?\Ũ) ;; LATIN CAPITAL LETTER U WITH TILDE
  ("&utilde;" ?\ũ) ;; LATIN SMALL LETTER U WITH TILDE
  ("&utri;" ?\▵) ;; WHITE UP-POINTING SMALL TRIANGLE
  ("&utrif;" ?\▴) ;; BLACK UP-POINTING SMALL TRIANGLE
  ("&Uuml;" ?\Ü) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
  ("&uuml;" ?\ü) ;; LATIN SMALL LETTER U WITH DIAERESIS
  ("&varr;" ?\↕) ;; UP DOWN ARROW
  ("&vArr;" ?\⇕) ;; UP DOWN DOUBLE ARROW
  ("&Vcy;" ?\В) ;; CYRILLIC CAPITAL LETTER VE
  ("&vcy;" ?\в) ;; CYRILLIC SMALL LETTER VE
  ("&vdash;" ?\⊢) ;; RIGHT TACK
  ("&vDash;" ?\⊨) ;; TRUE
  ("&Vdash;" ?\⊩) ;; FORCES
  ("&veebar;" ?\⊻) ;; XOR
  ("&vellip;" ?\⋮) ;; VERTICAL ELLIPSIS
  ("&verbar;" ?\|) ;; VERTICAL LINE
  ("&Verbar;" ?\‖) ;; DOUBLE VERTICAL LINE
  ("&vltri;" ?\⊲) ;; NORMAL SUBGROUP OF
  ("&vprime;" ?\′) ;; PRIME
  ("&vprop;" ?\∝) ;; PROPORTIONAL TO
  ("&vrtri;" ?\⊳) ;; CONTAINS AS NORMAL SUBGROUP
  ("&vsubnE;" ?\⊊) ;; SUBSET OF WITH NOT EQUAL TO
  ("&vsubne;" ?\⊊) ;; SUBSET OF WITH NOT EQUAL TO
  ("&vsupne;" ?\⊋) ;; SUPERSET OF WITH NOT EQUAL TO
  ("&vsupnE;" ?\⊋) ;; SUPERSET OF WITH NOT EQUAL TO
  ("&Vvdash;" ?\⊪) ;; TRIPLE VERTICAL BAR RIGHT TURNSTILE
  ("&Wcirc;" ?\Ŵ) ;; LATIN CAPITAL LETTER W WITH CIRCUMFLEX
  ("&wcirc;" ?\ŵ) ;; LATIN SMALL LETTER W WITH CIRCUMFLEX
  ("&wedgeq;" ?\≙) ;; ESTIMATES
  ("&weierp;" ?\℘) ;; SCRIPT CAPITAL P
  ("&wreath;" ?\≀) ;; WREATH PRODUCT
  ("&xcirc;" ?\○) ;; WHITE CIRCLE
  ("&xdtri;" ?\▽) ;; WHITE DOWN-POINTING TRIANGLE
  ("&Xgr;" ?\Ξ) ;; GREEK CAPITAL LETTER XI
  ("&xgr;" ?\ξ) ;; GREEK SMALL LETTER XI
  ("&xhArr;" ?\↔) ;; LEFT RIGHT ARROW
  ("&xharr;" ?\↔) ;; LEFT RIGHT ARROW
  ("&Xi;" ?\Ξ) ;; GREEK CAPITAL LETTER XI
  ("&xi;" ?\ξ) ;; GREEK SMALL LETTER XI
  ("&xlArr;" ?\⇐) ;; LEFTWARDS DOUBLE ARROW
  ("&xrArr;" ?\⇒) ;; RIGHTWARDS DOUBLE ARROW
  ("&xutri;" ?\△) ;; WHITE UP-POINTING TRIANGLE
  ("&Yacute;" ?\Ý) ;; LATIN CAPITAL LETTER Y WITH ACUTE
  ("&yacute;" ?\ý) ;; LATIN SMALL LETTER Y WITH ACUTE
  ("&YAcy;" ?\Я) ;; CYRILLIC CAPITAL LETTER YA
  ("&yacy;" ?\я) ;; CYRILLIC SMALL LETTER YA
  ("&Ycirc;" ?\Ŷ) ;; LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
  ("&ycirc;" ?\ŷ) ;; LATIN SMALL LETTER Y WITH CIRCUMFLEX
  ("&Ycy;" ?\Ы) ;; CYRILLIC CAPITAL LETTER YERU
  ("&ycy;" ?\ы) ;; CYRILLIC SMALL LETTER YERU
  ("&yen;" ?\¥) ;; YEN SIGN
  ("&YIcy;" ?\Ї) ;; CYRILLIC CAPITAL LETTER YI
  ("&yicy;" ?\ї) ;; CYRILLIC SMALL LETTER YI
  ("&YUcy;" ?\Ю) ;; CYRILLIC CAPITAL LETTER YU
  ("&yucy;" ?\ю) ;; CYRILLIC SMALL LETTER YU
  ("&yuml;" ?\ÿ) ;; LATIN SMALL LETTER Y WITH DIAERESIS
  ("&Yuml;" ?\Ÿ) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
  ("&Zacute;" ?\Ź) ;; LATIN CAPITAL LETTER Z WITH ACUTE
  ("&zacute;" ?\ź) ;; LATIN SMALL LETTER Z WITH ACUTE
  ("&Zcaron;" ?\Ž) ;; LATIN CAPITAL LETTER Z WITH CARON
  ("&zcaron;" ?\ž) ;; LATIN SMALL LETTER Z WITH CARON
  ("&Zcy;" ?\З) ;; CYRILLIC CAPITAL LETTER ZE
  ("&zcy;" ?\з) ;; CYRILLIC SMALL LETTER ZE
  ("&Zdot;" ?\Ż) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
  ("&zdot;" ?\ż) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
  ("&Zeta;" ?\Ζ) ;; GREEK CAPITAL LETTER ZETA
  ("&zeta;" ?\ζ) ;; GREEK SMALL LETTER ZETA
  ("&Zgr;" ?\Ζ) ;; GREEK CAPITAL LETTER ZETA
  ("&zgr;" ?\ζ) ;; GREEK SMALL LETTER ZETA
  ("&ZHcy;" ?\Ж) ;; CYRILLIC CAPITAL LETTER ZHE
  ("&zhcy;" ?\ж) ;; CYRILLIC SMALL LETTER ZHE
  ("&zwj;" ?\‍) ;; ZERO WIDTH JOINER
  ("&zwnj;" ?\‌) ;; ZERO WIDTH NON-JOINER
)

;;; sgml-input.el ends here
