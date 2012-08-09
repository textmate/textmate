;;; symbol-ksc.el --- Quail-package for Korean Symbol (KSC5601) -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Korean, Hangul

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

;;; 94.10.24   Written for Mule Ver.2.0 (koaunghi.un@zdv.uni-tuebingen.de)
;;; 94.11.04   Updated for Mule Ver.2.1 (koaunghi.un@zdv.uni-tuebingen.de)
;;; 96.09.23   Updated for emacs-19.33-beta (koaunghi.un@zdv.uni-tuebingen.de)

;;; Commentary:

;;; Code:

(require 'quail)
(require 'korea-util)

(quail-define-package
 "korean-symbol" "Korean" "$(C=I9z(B" t
 "$(CGQ1[=I9z@T7BG%(B:
  $(C!<(B($(C!=0}H#?-1b!<(Barrow$(C!=H-;l!<(Bsex$(C!=!N!O!<(Bindex$(C!=C7@Z(B  $(C!<(Baccent$(C!=>G<>F.(B
  $(C!<(B)$(C!=0}H#4]1b!<(Bmusic$(C!=@=>G!<(Bdot$(C!=A!(B  $(C!<(Bquote$(C!=5{?HG%!<(Bxtext$(C!=!W!X"R"."/(B
  $(C!<(BUnit$(C!=!I!J!K"5!<(Bmath$(C!=<vGP1bH#!<(Bpic$(C!=;sG|9.@Z!<(Bline$(C!=<19.@Z(B
  $(C!<(Bunit$(C!=4\@'(B    $(C!<(Bfrac$(C!=:P<v(B    $(C!<(Btextline$(C!=!)!*!+!,!-(B
  $(C!<(Bwn$(C!="_!<(Bks$(C!="^!<(BNo$(C!="`!<"a!="a(B $(C!<(Bdag$(C!="S(B $(C!<(Bddag$(C!="T!<(Bpercent$(C!="6(B
  $(C!<(Bam$(C!="c!<(Bpm$(C!="d!<"b!="b!<(BTel$(C!="e!<(Bwon$(C!=#\(B $(C!<(Byen$(C!=!M(B $(C!<(Bpound$(C!=!L(B
  $(C!<(BEng$(C!=#A#B#C!&(B $(C!<(Benum$(C!=#0#1#2!&(B $(C!<(BRuss$(C!=,!,",#!&!<(BGreek$(C!=%A%B%C!&(B
  $(C!<(Beng$(C!=#a#b#c!&(B $(C!<(Beasc$(C!=?5>n(BASCII$(C!<(Bruss$(C!=,Q,R,S!&!<(Bgreek$(C!=%a%b%c!&(B
  $(C!<(BRom$(C!=%0%1%2!&(B $(C!<(BScan$(C!=(!("(#!&(B $(C!<(Bhira$(C!=*!*"*#(B
  $(C!<(Brom$(C!=%!%"%#!&(B $(C!<(Bscan$(C!=)!)")#!&(B $(C!<(Bkata$(C!=+!+"+#(B
  $(C!<(Bojaso$(C!=(1!-(>!<(Bpjaso$(C!=)1!-)>!<(Boeng$(C!=(M!-(f!<(Bpeng$(C!=)M!-)f(B
  $(C!<(Bogana$(C!=(?!-(L!<(Bpgana$(C!=)?!-)L!<(Bonum$(C!=(g!-(u!<(Bpnum$(C!=)g!-)u(B
  $(C!<@Z<R!=(B2$(C9z=D(B + $(C$U(B(S) $(C$o(B(t_) $(C$q(B(D) $(C$p(B(DD) $(C$a(B(aD) $(C$v(B(_d) $(C$u(B(G) $(C$}(B(uk)")

(quail-define-rules
 ("("	"$(C!2!4!6!8!:!<(B")
 (")"	"$(C!3!5!7!9!;!=(B")
 ("math"	"$(C!>!?!@!A!B!C!D!E!P!Q!R!S!T!U!V!k!l!m!n!o!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~"!"""#"$"1"2"3(B")
 ("pic"	"$(C!Y!Z![!\!]!^!_!`!a!b!c!d!e"7"8"9":";"<"=">"?"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O"P"Q"4(B")
 ("arrow"	"$(C!f!g!h!i!j"U"V"W"X"Y(B")
 ("music"	"$(C"Z"["\"](B")
 ("won"	"$(C#\(B")
 ("yen"	"$(C!M(B")
 ("pound"	"$(C!L(B")
 ("xtext"	"$(C!W!X"R"."/(B")
 ("dot"	"$(C!$!%!&!'"0(B")
 ("quote"	"$(C!"!#!(!.!/!0!1!F!G!H"%")(B")
 ("textline"	"$(C!)!*!+!,!-(B")
 ("Unit"	"$(C!I!J!K"5(B")
 ("sex"	"$(C!N!O(B")
 ("accent"	"$(C"&"'"("*"+","-(B")
 ("percent"	"$(C"6(B")
 ("dag"	"$(C"S(B")
 ("ddag"	"$(C"T(B")
 ("wn"	"$(C"_(B")
 ("ks"	"$(C"^(B")
 ("No"	"$(C"`(B")
 ("Co"	"$(C"a(B")
 ("TM"	"$(C"b(B")
 ("am"	"$(C"c(B")
 ("pm"	"$(C"d(B")
 ("Tel"	"$(C"e(B")
 ("easc"	"$(C#!#"###$#%#&#'#(#)#*#+#,#-#.#/#:#;#<#=#>#?#@#[#]#^#_#`#{#|#}#~(B")
 ("enum"	"$(C#0#1#2#3#4#5#6#7#8#9(B")
 ("Eng"	"$(C#A#B#C#D#E#F#G#H#I#J#K#L#M#N#O#P#Q#R#S#T#U#V#W#X#Y#Z(B")
 ("eng"	"$(C#a#b#c#d#e#f#g#h#i#j#k#l#m#n#o#p#q#r#s#t#u#v#w#x#y#z(B")
 ("r"	"$(C$!(B")
 ("R"	"$(C$"(B")
 ("rt"	"$(C$#(B")
 ("s"	"$(C$$(B")
 ("sw"	"$(C$%(B")
 ("sg"	"$(C$&(B")
 ("e"	"$(C$'(B")
 ("E"	"$(C$((B")
 ("f"	"$(C$)(B")
 ("fr"	"$(C$*(B")
 ("fa"	"$(C$+(B")
 ("fq"	"$(C$,(B")
 ("ft"	"$(C$-(B")
 ("fx"	"$(C$.(B")
 ("fv"	"$(C$/(B")
 ("fg"	"$(C$0(B")
 ("a"	"$(C$1(B")
 ("q"	"$(C$2(B")
 ("Q"	"$(C$3(B")
 ("qt"	"$(C$4(B")
 ("t"	"$(C$5(B")
 ("T"	"$(C$6(B")
 ("d"	"$(C$7(B")
 ("w"	"$(C$8(B")
 ("W"	"$(C$9(B")
 ("c"	"$(C$:(B")
 ("z"	"$(C$;(B")
 ("x"	"$(C$<(B")
 ("v"	"$(C$=(B")
 ("g"	"$(C$>(B")
 ("k"	"$(C$?(B")
 ("o"	"$(C$@(B")
 ("i"	"$(C$A(B")
 ("O"	"$(C$B(B")
 ("j"	"$(C$C(B")
 ("p"	"$(C$D(B")
 ("u"	"$(C$E(B")
 ("P"	"$(C$F(B")
 ("h"	"$(C$G(B")
 ("hk"	"$(C$H(B")
 ("ho"	"$(C$I(B")
 ("hl"	"$(C$J(B")
 ("y"	"$(C$K(B")
 ("n"	"$(C$L(B")
 ("nj"	"$(C$M(B")
 ("np"	"$(C$N(B")
 ("nl"	"$(C$O(B")
 ("b"	"$(C$P(B")
 ("m"	"$(C$Q(B")
 ("ml"	"$(C$R(B")
 ("l"	"$(C$S(B")
 ("S"	"$(C$U(B")
 ("se"	"$(C$V(B")
 ("st"	"$(C$W(B")
 ("st_"	"$(C$X(B")
 ("frt"	"$(C$Y(B")
 ("fqt"	"$(C$[(B")
 ("fe"	"$(C$Z(B")
 ("ft_"	"$(C$\(B")
 ("f_d"	"$(C$](B")
 ("aq"	"$(C$^(B")
 ("at"	"$(C$_(B")
 ("at_"	"$(C$`(B")
 ("aD"	"$(C$a(B")
 ("qr"	"$(C$b(B")
 ("qe"	"$(C$c(B")
 ("qtr"	"$(C$d(B")
 ("qte"	"$(C$e(B")
 ("qw"	"$(C$f(B")
 ("qx"	"$(C$g(B")
 ("qD"	"$(C$h(B")
 ("QD"	"$(C$i(B")
 ("tr"	"$(C$j(B")
 ("ts"	"$(C$k(B")
 ("te"	"$(C$l(B")
 ("tq"	"$(C$m(B")
 ("tw"	"$(C$n(B")
 ("t_"	"$(C$o(B")
 ("DD"	"$(C$p(B")
 ("D"	"$(C$q(B")
 ("Dt"	"$(C$r(B")
 ("Dt_"	"$(C$s(B")
 ("vD"	"$(C$t(B")
 ("G"	"$(C$u(B")
 ("_d"	"$(C$v(B")
 ("yi"	"$(C$w(B")
 ("yO"	"$(C$x(B")
 ("yl"	"$(C$y(B")
 ("bu"	"$(C$z(B")
 ("bP"	"$(C${(B")
 ("bl"	"$(C$|(B")
 ("uk"	"$(C$}(B")
 ("ukl"	"$(C$~(B")
 ("Rom"	"$(C%0%1%2%3%4%5%6%7%8%9(B")
 ("rom"	"$(C%!%"%#%$%%%&%'%(%)%*(B")
 ("Greek"	"$(C%A%B%C%D%E%F%G%H%I%J%K%L%M%N%O%P%Q%R%S%T%U%V%W%X(B")
 ("greek"	"$(C%a%b%c%d%e%f%g%h%i%j%k%l%m%n%o%p%q%r%s%t%u%v%w%x(B")
 ("line"	"$(C&!&"&#&$&%&&&'&(&)&*&+&,&-&.&/&0&1&2&3&4&5&6&7&8&9&:&;&<&=&>&?&@&A&B&C&D&E&F&G&H&I&J&K&L&M&N&O&P&Q&R&S&T&U&V&W&X&Y&Z&[&\&]&^&_&`&a&b&c&d(B")
 ("unit"	"$(C'!'"'#'$'%'&'''(')'*'+','-'.'/'0'1'2'3'4'5'6'7'8'9':';'<'='>'?'@'A'B'C'D'E'F'G'H'I'J'K'L'M'N'O'P'Q'R'S'T'U'V'W'X'Y'Z'['\']'^'_'`'a'b'c'd'e'f'g'h'i'j'k'l'm'n'o(B")
 ("Scan"	"$(C(!("(#($(&((()(*(+(,(-(.(/(B")
 ("ojaso"	"$(C(1(2(3(4(5(6(7(8(9(:(;(<(=(>(B")
 ("ogana"	"$(C(?(@(A(B(C(D(E(F(G(H(I(J(K(L(B")
 ("oeng"	"$(C(M(N(O(P(Q(R(S(T(U(V(W(X(Y(Z([(\(](^(_(`(a(b(c(d(e(f(B")
 ("onum"	"$(C(g(h(i(j(k(l(m(n(o(p(q(r(s(t(u(B")
 ("frac"	"$(C(v(w(x(y(z({(|(}(~(B")
 ("scan"	"$(C)!)")#)$)%)&)')()))*)+),)-).)/)0(B")
 ("pjaso"	"$(C)1)2)3)4)5)6)7)8)9):);)<)=)>(B>")
 ("pgana"	"$(C)?)@)A)B)C)D)E)F)G)H)I)J)K)L(B")
 ("peng"	"$(C)M)N)O)P)Q)R)S)T)U)V)W)X)Y)Z)[)\)])^)_)`)a)b)c)d)e)f(B")
 ("pnum"	"$(C)g)h)i)j)k)l)m)n)o)p)q)r)s)t)u(B")
 ("index"	"$(C)v)w)x)y)z){)|)})~(B")
 ("hira"	"$(C*!*"*#*$*%*&*'*(*)***+*,*-*.*/*0*1*2*3*4*5*6*7*8*9*:*;*<*=*>*?*@*A*B*C*D*E*F*G*H*I*J*K*L*M*N*O*P*Q*R*S*T*U*V*W*X*Y*Z*[*\*]*^*_*`*a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*p*q*r*s(B")
 ("kata"	"$(C+!+"+#+$+%+&+'+(+)+*+++,+-+.+/+0+1+2+3+4+5+6+7+8+9+:+;+<+=+>+?+@+A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+[+\+]+^+_+`+a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v(B")
 ("Russ"	"$(C,!,",#,$,%,&,',(,),*,+,,,-,.,/,0,1,2,3,4,5,6,7,8,9,:,;,<,=,>,?,@,A(B")
 ("russ"	"$(C,Q,R,S,T,U,V,W,X,Y,Z,[,\,],^,_,`,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q(B"))

;;; symbol-ksc.el ends here
