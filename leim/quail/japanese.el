;;; japanese.el --- Quail package for inputting Japanese  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Japanese

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
(require 'kkc)

(defvar quail-japanese-use-double-n nil
  "If non-nil, use type \"nn\" to insert $B$s(B.")

;; Update Quail translation region while considering Japanese bizarre
;; translation rules.
(defun quail-japanese-update-translation (control-flag)
  (if (null control-flag)
      (setq quail-current-str
	    (if (/= (aref quail-current-key 0) ?q)
		(or quail-current-str quail-current-key)
	      ""))
    (if (integerp control-flag)
	(let ((keylen (length quail-current-key)))
	  (cond ((= control-flag 0)
		 (setq quail-current-str (aref quail-current-key 0)
		       control-flag t))
		((= (aref quail-current-key 0) ?n)
		 (setq quail-current-str ?$B$s(B)
		 (if (and quail-japanese-use-double-n
			  (> keylen 0)
			  (= (aref quail-current-key 1) ?n))
		     (setq control-flag t)))
		((and (> keylen 1)
		      (= (aref quail-current-key 0) (aref quail-current-key 1)))
		 (setq quail-current-str ?$B$C(B))
		(t
		 (setq quail-current-str (aref quail-current-key 0))))
	  (if (integerp control-flag)
	      (setq unread-command-events
		    (string-to-list
		     (substring quail-current-key control-flag)))))))
  control-flag)

;; Convert Hiragana <-> Katakana in the current translation region.
(defun quail-japanese-toggle-kana ()
  (interactive)
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
	(end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\cH" end t)
	  (japanese-katakana-region start end)
	(japanese-hiragana-region start end)))
    (setq quail-conversion-str
	  (buffer-substring (overlay-start quail-conv-overlay)
			    (overlay-end quail-conv-overlay)))))

;; Convert Hiragana in the current translation region to Kanji by KKC
;; (Kana Kanji Converter) utility.
(defun quail-japanese-kanji-kkc ()
  (interactive)
  (when (= (char-before (overlay-end quail-conv-overlay)) ?n)
    ;; The last char is `n'.  We had better convert it to `$B$s(B'
    ;; before kana-kanji conversion.
    (goto-char (1- (overlay-end quail-conv-overlay)))
    (insert ?$B$s(B)
    (delete-char 1))
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
	 (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
	(let ((result (kkc-region from (+ from len))))
	  (move-overlay quail-conv-overlay from (point))
	  (setq quail-conversion-str (buffer-substring from (point)))
	  (if (= (+ from result) (point))
	      (setq quail-converting nil))
	  (setq quail-translating nil))
      (set-marker from nil))))

(defun quail-japanese-self-insert-and-switch-to-alpha (key idx)
  (quail-delete-region)
  (setq unread-command-events (list (aref key (1- idx))))
  (quail-japanese-switch-package "q" 1))

(defvar quail-japanese-switch-table
  '((?z . "japanese-zenkaku")
    (?k . "japanese-hankaku-kana")
    (?h . "japanese")
    (?q . ("japanese-ascii"))))

(defvar quail-japanese-package-saved nil)
(make-variable-buffer-local 'quail-japanese-package-saved)
(put 'quail-japanese-package-saved 'permanent-local t)

(defun quail-japanese-switch-package (key idx)
  (quail-delete-region)
  (setq quail-current-str nil
	quail-converting nil
	quail-conversion-str "")
  (let ((pkg (cdr (assq (aref key (1- idx)) quail-japanese-switch-table))))
    (if (null pkg)
	(quail-error "No package to be switched")
      (if (stringp pkg)
	  (activate-input-method pkg)
	(if (string= (car pkg) current-input-method)
	    (if quail-japanese-package-saved
		(activate-input-method quail-japanese-package-saved))
	  (setq quail-japanese-package-saved current-input-method)
	  (activate-input-method (car pkg))))))
  (throw 'quail-tag nil))

(defvar quail-japanese-transliteration-rules
  '(( "a" "$B$"(B") ( "i" "$B$$(B") ( "u" "$B$&(B") ( "e" "$B$((B") ( "o" "$B$*(B")
    ("ka" "$B$+(B") ("ki" "$B$-(B") ("ku" "$B$/(B") ("ke" "$B$1(B") ("ko" "$B$3(B")
    ("sa" "$B$5(B") ("si" "$B$7(B") ("su" "$B$9(B") ("se" "$B$;(B") ("so" "$B$=(B")
    ("ta" "$B$?(B") ("ti" "$B$A(B") ("tu" "$B$D(B") ("te" "$B$F(B") ("to" "$B$H(B")
    ("na" "$B$J(B") ("ni" "$B$K(B") ("nu" "$B$L(B") ("ne" "$B$M(B") ("no" "$B$N(B")
    ("ha" "$B$O(B") ("hi" "$B$R(B") ("hu" "$B$U(B") ("he" "$B$X(B") ("ho" "$B$[(B")
    ("ma" "$B$^(B") ("mi" "$B$_(B") ("mu" "$B$`(B") ("me" "$B$a(B") ("mo" "$B$b(B")
    ("ya" "$B$d(B")             ("yu" "$B$f(B")             ("yo" "$B$h(B")
    ("ra" "$B$i(B") ("ri" "$B$j(B") ("ru" "$B$k(B") ("re" "$B$l(B") ("ro" "$B$m(B")
    ("la" "$B$i(B") ("li" "$B$j(B") ("lu" "$B$k(B") ("le" "$B$l(B") ("lo" "$B$m(B")
    ("wa" "$B$o(B") ("wi" "$B$p(B") ("wu" "$B$&(B") ("we" "$B$q(B") ("wo" "$B$r(B")
    ("n'" "$B$s(B")
    ("ga" "$B$,(B") ("gi" "$B$.(B") ("gu" "$B$0(B") ("ge" "$B$2(B") ("go" "$B$4(B")
    ("za" "$B$6(B") ("zi" "$B$8(B") ("zu" "$B$:(B") ("ze" "$B$<(B") ("zo" "$B$>(B")
    ("da" "$B$@(B") ("di" "$B$B(B") ("du" "$B$E(B") ("de" "$B$G(B") ("do" "$B$I(B")
    ("ba" "$B$P(B") ("bi" "$B$S(B") ("bu" "$B$V(B") ("be" "$B$Y(B") ("bo" "$B$\(B")
    ("pa" "$B$Q(B") ("pi" "$B$T(B") ("pu" "$B$W(B") ("pe" "$B$Z(B") ("po" "$B$](B")

    ("kya" ["$B$-$c(B"]) ("kyu" ["$B$-$e(B"]) ("kye" ["$B$-$'(B"]) ("kyo" ["$B$-$g(B"])
    ("sya" ["$B$7$c(B"]) ("syu" ["$B$7$e(B"]) ("sye" ["$B$7$'(B"]) ("syo" ["$B$7$g(B"])
    ("sha" ["$B$7$c(B"]) ("shu" ["$B$7$e(B"]) ("she" ["$B$7$'(B"]) ("sho" ["$B$7$g(B"])
    ("cha" ["$B$A$c(B"]) ("chu" ["$B$A$e(B"]) ("che" ["$B$A$'(B"]) ("cho" ["$B$A$g(B"])
    ("tya" ["$B$A$c(B"]) ("tyu" ["$B$A$e(B"]) ("tye" ["$B$A$'(B"]) ("tyo" ["$B$A$g(B"])
    ("nya" ["$B$K$c(B"]) ("nyu" ["$B$K$e(B"]) ("nye" ["$B$K$'(B"]) ("nyo" ["$B$K$g(B"])
    ("hya" ["$B$R$c(B"]) ("hyu" ["$B$R$e(B"]) ("hye" ["$B$R$'(B"]) ("hyo" ["$B$R$g(B"])
    ("mya" ["$B$_$c(B"]) ("myu" ["$B$_$e(B"]) ("mye" ["$B$_$'(B"]) ("myo" ["$B$_$g(B"])
    ("rya" ["$B$j$c(B"]) ("ryu" ["$B$j$e(B"]) ("rye" ["$B$j$'(B"]) ("ryo" ["$B$j$g(B"])
    ("lya" ["$B$j$c(B"]) ("lyu" ["$B$j$e(B"]) ("lye" ["$B$j$'(B"]) ("lyo" ["$B$j$g(B"])
    ("gya" ["$B$.$c(B"]) ("gyu" ["$B$.$e(B"]) ("gye" ["$B$.$'(B"]) ("gyo" ["$B$.$g(B"])
    ("zya" ["$B$8$c(B"]) ("zyu" ["$B$8$e(B"]) ("zye" ["$B$8$'(B"]) ("zyo" ["$B$8$g(B"])
    ("jya" ["$B$8$c(B"]) ("jyu" ["$B$8$e(B"]) ("jye" ["$B$8$'(B"]) ("jyo" ["$B$8$g(B"])
    ( "ja" ["$B$8$c(B"]) ( "ju" ["$B$8$e(B"]) ( "je" ["$B$8$'(B"]) ( "jo" ["$B$8$g(B"])
    ("bya" ["$B$S$c(B"]) ("byu" ["$B$S$e(B"]) ("bye" ["$B$S$'(B"]) ("byo" ["$B$S$g(B"])
    ("pya" ["$B$T$c(B"]) ("pyu" ["$B$T$e(B"]) ("pye" ["$B$T$'(B"]) ("pyo" ["$B$T$g(B"])

    ("kwa" ["$B$/$n(B"]) ("kwi" ["$B$/$#(B"]) ("kwe" ["$B$/$'(B"]) ("kwo" ["$B$/$)(B"])
    ("tsa" ["$B$D$!(B"]) ("tsi" ["$B$D$#(B"]) ("tse" ["$B$D$'(B"]) ("tso" ["$B$D$)(B"])
    ( "fa" ["$B$U$!(B"]) ( "fi" ["$B$U$#(B"]) ( "fe" ["$B$U$'(B"]) ( "fo" ["$B$U$)(B"])
    ("gwa" ["$B$0$n(B"]) ("gwi" ["$B$0$#(B"]) ("gwe" ["$B$0$'(B"]) ("gwo" ["$B$0$)(B"])

    ("dyi" ["$B$G$#(B"]) ("dyu" ["$B$I$%(B"]) ("dye" ["$B$G$'(B"]) ("dyo" ["$B$I$)(B"])
    ("xwi" ["$B$&$#(B"])                  ("xwe" ["$B$&$'(B"]) ("xwo" ["$B$&$)(B"])

    ("shi" "$B$7(B") ("tyi" ["$B$F$#(B"]) ("chi" "$B$A(B") ("tsu" "$B$D(B") ("ji" "$B$8(B")
    ("fu"  "$B$U(B")
    ("ye" ["$B$$$'(B"])

    ("va" ["$B%t$!(B"]) ("vi" ["$B%t$#(B"]) ("vu" "$B%t(B") ("ve" ["$B%t$'(B"]) ("vo" ["$B%t$)(B"])

    ("xa"  "$B$!(B") ("xi"  "$B$#(B") ("xu"  "$B$%(B") ("xe"  "$B$'(B") ("xo"  "$B$)(B")
    ("xtu" "$B$C(B") ("xya" "$B$c(B") ("xyu" "$B$e(B") ("xyo" "$B$g(B") ("xwa" "$B$n(B")
    ("xka" "$B%u(B") ("xke" "$B%v(B")

    ("1" "$B#1(B") ("2" "$B#2(B") ("3" "$B#3(B") ("4" "$B#4(B") ("5" "$B#5(B")
    ("6" "$B#6(B") ("7" "$B#7(B") ("8" "$B#8(B") ("9" "$B#9(B") ("0" "$B#0(B")

    ("!" "$B!*(B") ("@" "$B!w(B") ("#" "$B!t(B") ("$" "$B!p(B") ("%" "$B!s(B")
    ("^" "$B!0(B") ("&" "$B!u(B") ("*" "$B!v(B") ("(" "$B!J(B") (")" "$B!K(B")
    ("-" "$B!<(B") ("=" "$B!a(B") ("`" "$B!.(B") ("\\" "$B!o(B") ("|" "$B!C(B")
    ("_" "$B!2(B") ("+" "$B!\(B") ("~" "$B!1(B") ("[" "$B!V(B") ("]" "$B!W(B")
    ("{" "$B!P(B") ("}" "$B!Q(B") (":" "$B!'(B") (";" "$B!((B") ("\""  "$B!I(B")
    ("'" "$B!G(B") ("." "$B!#(B") ("," "$B!"(B") ("<" "$B!c(B") (">" "$B!d(B")
    ("?" "$B!)(B") ("/" "$B!?(B")

    ("z1" "$B!{(B") ("z!" "$B!|(B")
    ("z2" "$B"&(B") ("z@" "$B"'(B")
    ("z3" "$B"$(B") ("z#" "$B"%(B")
    ("z4" "$B""(B") ("z$" "$B"#(B")
    ("z5" "$B!~(B") ("z%" "$B"!(B")
    ("z6" "$B!y(B") ("z^" "$B!z(B")
    ("z7" "$B!}(B") ("z&" "$B!r(B")
    ("z8" "$B!q(B") ("z*" "$B!_(B")
    ("z9" "$B!i(B") ("z(" "$B!Z(B")
    ("z0" "$B!j(B") ("z)" "$B![(B")
    ("z-" "$B!A(B") ("z_" "$B!h(B")
    ("z=" "$B!b(B") ("z+" "$B!^(B")
    ("z\\" "$B!@(B") ("z|" "$B!B(B")
    ("z`" "$B!-(B") ("z~" "$B!/(B")

    ("zq" "$B!T(B") ("zQ" "$B!R(B")
    ("zw" "$B!U(B") ("zW" "$B!S(B")
    ("zr" "$B!9(B") ("zR" "$B!8(B")
    ("zt" "$B!:(B") ("zT" "$B!x(B")
    ("zp" "$B")(B") ("zP" "$B",(B")
    ("z[" "$B!X(B") ("z{" "$B!L(B")
    ("z]" "$B!Y(B") ("z}" "$B!M(B")

    ("zs" "$B!3(B") ("zS" "$B!4(B")
    ("zd" "$B!5(B") ("zD" "$B!6(B")
    ("zf" "$B!7(B") ("zF" "$B"*(B")
    ("zg" "$B!>(B") ("zG" "$B!=(B")
    ("zh" "$B"+(B")
    ("zj" "$B"-(B")
    ("zk" "$B",(B")
    ("zl" "$B"*(B")
    ("z;" "$B!+(B") ("z:" "$B!,(B")
    ("z\'" "$B!F(B") ("z\"" "$B!H(B")

    ("zx" [":-"]) ("zX" [":-)"])
    ("zc" "$B!;(B") ("zC" "$B!n(B")
    ("zv" "$B"((B") ("zV" "$B!`(B")
    ("zb" "$B!k(B") ("zB" "$B"+(B")
    ("zn" "$B!l(B") ("zN" "$B"-(B")
    ("zm" "$B!m(B") ("zM" "$B".(B")
    ("z," "$B!E(B") ("z<" "$B!e(B")
    ("z." "$B!D(B") ("z>" "$B!f(B")
    ("z/" "$B!&(B") ("z?" "$B!g(B")

    ("\\\\" quail-japanese-self-insert-and-switch-to-alpha)
    ("{{" quail-japanese-self-insert-and-switch-to-alpha)
    ("}}" quail-japanese-self-insert-and-switch-to-alpha)

    ("qq" quail-japanese-switch-package)
    ("qz" quail-japanese-switch-package)
    ))


;; $B%m!<%^;zF~NO5Z$S2>L>4A;zJQ49$K$h$kF|K\8lF~NO%a%=%C%I(B
;;
;; $B$3$NF~NO%a%=%C%I$G$NF|K\8l$NF~NO$OFs$D$N%9%F!<%8!V%m!<%^;z2>L>JQ49!W(B
;; $B$H!V2>L>4A;zJQ49!W$+$i$J$k!#:G=i$O%m!<%^;z2>L>JQ49$N%9%F!<%8$G!"%9(B
;; $B%Z!<%9%-!<$r2!$9$3$H$K$h$j!"<!$N%9%F!<%8!V2>L>4A;zJQ49!W$X?J$`!#(B
;;
;; $B!V%m!<%^;z2>L>JQ49!W(B
;;
;; $BJ?2>L>$O>.J8;z%-!<!JNs!K$rBG$D$3$H$K$h$jF~NO!#6gFIE@!"3g8LN`$OBP1~(B
;; $B$9$k1Q;z%-!<$rBG$D$3$H$K$h$jF~NO!#$=$NB>$N%7%s%\%k$O(B `z' $B$KB3$1$F2?(B
;; $B$l$+$N%-!<$rBG$D$3$H$K$h$jF~NO!#2<$KA4$F$N2DG=$J%-!<%7!<%1%s%9%j%9(B
;; $B%H%"%C%W$5$l$F$$$k!#F~NO$5$l$?J8;z$O2<@~$G<($5$l$k!#(B
;;
;; $B$5$i$K0J2<$N%-!<$GFCJL$J=hM}$r9T$&!#(B
;;
;; K	$BJ?2>L>$rJR2>L>$K!"$"$k$$$OJR2>L>$rJ?2>L>$KJQ49(B
;; qq	$B$3$NF~NO%a%=%C%I$H(B `japanese-ascii' $BF~NO%a%=%C%I$r%H%0%k@ZBX(B
;; qz	`japanese-zenkaku' $BF~NO%a%=%C%I$K%7%U%H(B
;;	qh $B$HBG$F$P85$KLa$k(B
;; RET	$B8=:_$NF~NOJ8;zNs$r3NDj(B
;; SPC	$B2>L>4A;zJQ49$K?J$`(B
;;
;; `japanese-ascii' $BF~NO%a%=%C%I$O(B ASCII $BJ8;z$rF~NO$9$k$N$K;H$&!#$3$l(B
;; $B$OF~NO%a%=%C%I$r%*%U$K$9$k$N$H$[$H$s$IF1$8$G$"$k!#0[$J$k$N$O(B qq $B$H(B
;; $BBG$D$3$H$K$h$j!"(B`japanese' $BF~NO%a%=%C%I$KLa$l$kE@$G$"$k!#(B
;;
;; `japanese-zenkaku' $BF~NO%a%=%C%I$OA43Q1Q?t;z$rF~NO$9$k$N$K;H$&!#(B
;;
;; $B!V%m!<%^;z2>L>JQ49!W%9%F!<%8$G$N%-!<%7!<%1%s%9$N%j%9%H$O:G8e$KIU$1(B
;; $B$F$"$k!#(B
;;
;; $B!V2>L>4A;zJQ49!W(B
;;
;; $B$3$N%9%F!<%8$G$O!"A0%9%F!<%8$GF~NO$5$l$?J8;zNs$r2>L>4A;zJQ49$9$k!#(B
;; $BJQ49$5$l$?J8;zNs$O!"CmL\J8@a!JH?E>I=<(!K$H;D$j$NF~NO!J2<@~I=<(!K$K(B
;; $BJ,$1$i$l$k!#CmL\J8@a$KBP$7$F$O0J2<$N%3%^%s%I$,;H$($k!#(B
;;
;; SPC, C-n	kkc-next
;;	$B<!$NJQ498uJd$rI=<((B
;;	kkc-show-conversion-list-count $B0J>eB3$1$FBG$F$P!"JQ498uJd%j%9(B
;;	$B%H$r%(%3!<%(%j%"$KI=<((B
;; C-p		kkc-prev
;;	$BA0$NJQ498uJd$rI=<((B
;;	kkc-show-conversion-list-count $B0J>eB3$1$FBG$F$P!"JQ498uJd%j%9(B
;;	$B%H$r%(%3!<%(%j%"$KI=<((B
;; l		kkc-show-conversion-list-or-next-group
;;	$B:G9b#1#08D$^$G$NJQ498uJd$r%(%3!<%(%j%"$KI=<(!#(B
;;	$BB3$1$FBG$?$l$l$P!"<!$N#1#08uJd$rI=<(!#(B
;; L		kkc-show-conversion-list-or-prev-group
;;	$B:G9b#1#08D$^$G$NJQ498uJd$r%(%3!<%(%j%"$KI=<(!#(B
;;	$BB3$1$FBG$?$l$l$P!"A0$N#1#08uJd$rI=<(!#(B
;; 0..9		kkc-select-from-list
;;	$BBG$?$l$??t;z$NJQ498uJd$rA*Br(B
;; H		kkc-hiragana
;;	$BCmL\J8@a$rJ?2>L>$KJQ49(B
;; K		kkc-katakana
;;	$BCmL\J8@a$rJR2>L>$KJQ49(B
;; C-o		kkc-longer
;;	$BCmL\J8@a$r8e$m$K0lJ8;z?-$P$9(B
;; C-i		kkc-shorter
;;	$BCmL\J8@a$r8e$m$+$i0lJ8;z=L$a$k(B
;; C-f		kkc-next-phrase
;;	$BCmL\J8@a$r3NDj$5$;$k!#$b$7;D$j$NF~NO$,$^$@$"$l$P!":G=i$NJ8@a$r(B
;;	$BA*Br$7!"$=$l$rCmL\J8@a$H$7!"$=$N:G=i$NJQ498uJd$rI=<($9$k!#(B
;; DEL, C-c	kkc-cancel
;;	$B2>L>4A;zJQ49$r%-%c%s%;%k$7!"%m!<%^;z2>L>JQ49$N%9%F!<%8$KLa$k!#(B
;; return		kkc-terminate
;;	$BA4J8@a$r3NDj$5$;$k!#(B
;; C-SPC, C-@	kkc-first-char-only
;;	$B:G=i$NJ8;z$r3NDj$5$;!";D$j$O:o=|$9$k!#(B
;; C-h		kkc-help
;;	$B$3$l$i$N%-!<%P%$%s%I$N%j%9%H$rI=<($9$k!#$"(B

(quail-define-package
 "japanese" "Japanese" "A$B$"(B"
 nil
 "Japanese input method by Roman transliteration and Kana-Kanji conversion.

When you use this input method, text entry proceeds in two stages:
Roman-Kana transliteration and Kana-Kanji conversion.  When you start
to enter text, you are in the first stage, Roman-Kana transliteration.
Type SPC to proceed to the next stage, Kana-Kanji conversion.

:: Roman-Kana transliteration ::

You can input any Hiragana character as a sequence of lower-case
letters, Japanese punctuation characters by typing punctuation keys,
Japanese symbols by typing `z' followed by another key.  See below for
a list of all available sequences.  The characters you input are
underlined.

In addition, the following keys provide special effects:

K	Change Hiragana to Katakana or Katakana to Hiragana.
qq	Toggle between this input method and the input method `japanese-ascii'.
qz	Shift to the input method `japanese-zenkaku'.
	Typing \"qh\" will put you back to this input method.
RET	Accept the current character sequence.
SPC	Proceed to the next stage, Kana-Kanji conversion.

The input method `japanese-ascii' is used to enter ASCII characters.
This is almost the same as turning off the input method.  The only
difference is that typing `qq' will put you back into the Japanese
input method.

The input method `japanese-zenkaku' is used to enter full width
JISX0208 characters corresponding to typed ASCII characters.

List of the all key sequences for Roman-Kana transliteration is shown
at the tail.

:: Kana-Kanji conversion ::

You can convert the current Japanese characters (underlined) to
Kana-Kanji mixed text.  In this stage, the converted text is divided
into two parts, the current phrase (highlighted) and the remaining
input (underlined).  The following commands can be used on the
current phrase.

SPC, C-n	kkc-next
	Show the next candidate for the current phrase.
	If successively typed `kkc-show-conversion-list-count' times,
	conversion candidates are shown in the echo area.
C-p		kkc-prev
	Show the previous candidate for the current phrase.
	If successively typed `kkc-show-conversion-list-count' times,
	conversion candidates are shown in the echo area.
l		kkc-show-conversion-list-or-next-group
	Show at most 10 candidates for the current phrase in echo area.
	If typed repeatedly, show the next 10 candidates.
L		kkc-show-conversion-list-or-prev-group
	Show at most 10 candidates for the current phrase in echo area.
	If typed repeatedly, show the previous 10 candidates.
0..9		kkc-select-from-list
	Select a candidate corresponding to the typed number.
H		kkc-hiragana
	Convert the current phrase to Hiragana
K		kkc-katakana
	Convert the current phrase to Katakana
C-o		kkc-longer
	Extend the current phrase; pull in the first character of
	the remaining input.
C-i		kkc-shorter
	Contract the current phrase; drop its last character
	back into the remaining input.
C-f		kkc-next-phrase
	Accept the current phrase.  If there remains input, select
	the first phrase as the current one, and show the first
	candidate for the conversion.
DEL, C-c	kkc-cancel
	Cancel the conversion, shift back to the Roman-Kana
	transliteration.
return		kkc-terminate
	Accept the whole conversion.
C-SPC, C-@	kkc-first-char-only
	Accept the first character of the current conversion,
	delete the remaining input.
C-h		kkc-help
	List these key bindings.
"
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation
 '(("K" . quail-japanese-toggle-kana)
   (" " . quail-japanese-kanji-kkc)
   ("\C-m" . quail-no-conversion)
   ([return] . quail-no-conversion))
 )

(dolist (elt quail-japanese-transliteration-rules)
  (quail-defrule (car elt) (nth 1 elt)))

(quail-define-package
 "japanese-ascii" "Japanese" "Aa"
 nil
 "Temporary ASCII input mode used within the input method `japanese'.
Type \"qq\" to go back to previous input method."
 nil t t)

(quail-define-rules ("qq" quail-japanese-switch-package))

(quail-define-package
 "japanese-zenkaku" "Japanese" "$B#A(B"
 nil
 "Japanese zenkaku alpha numeric character input method.
---- Special key bindings ----
qq:	toggle between this input method and the input method `japanese-ascii'.
qh:	shift to the input method `japanese',
	typing \"qz\" puts you back to this input method.
"
 nil t t)

(quail-define-rules

(" " "$B!!(B") ("!" "$B!*(B") ("\"" "$B!m(B") ("#" "$B!t(B")
("$" "$B!p(B") ("%" "$B!s(B") ("&" "$B!u(B") ("'" "$B!l(B")
("(" "$B!J(B") (")" "$B!K(B") ("*" "$B!v(B") ("+" "$B!\(B")
("," "$B!$(B") ("-" "$B!](B") ("." "$B!%(B") ("/" "$B!?(B")
("0" "$B#0(B") ("1" "$B#1(B") ("2" "$B#2(B") ("3" "$B#3(B")
("4" "$B#4(B") ("5" "$B#5(B") ("6" "$B#6(B") ("7" "$B#7(B")
("8" "$B#8(B") ("9" "$B#9(B") (":" "$B!'(B") (";" "$B!((B")
("<" "$B!c(B") ("=" "$B!a(B") (">" "$B!d(B") ("?" "$B!)(B")
("@" "$B!w(B") ("A" "$B#A(B") ("B" "$B#B(B") ("C" "$B#C(B")
("D" "$B#D(B") ("E" "$B#E(B") ("F" "$B#F(B") ("G" "$B#G(B")
("H" "$B#H(B") ("I" "$B#I(B") ("J" "$B#J(B") ("K" "$B#K(B")
("L" "$B#L(B") ("M" "$B#M(B") ("N" "$B#N(B") ("O" "$B#O(B")
("P" "$B#P(B") ("Q" "$B#Q(B") ("R" "$B#R(B") ("S" "$B#S(B")
("T" "$B#T(B") ("U" "$B#U(B") ("V" "$B#V(B") ("W" "$B#W(B")
("X" "$B#X(B") ("Y" "$B#Y(B") ("Z" "$B#Z(B") ("[" "$B!N(B")
("\\" "$B!o(B") ("]" "$B!O(B") ("^" "$B!0(B") ("_" "$B!2(B")
("`" "$B!F(B") ("a" "$B#a(B") ("b" "$B#b(B") ("c" "$B#c(B")
("d" "$B#d(B") ("e" "$B#e(B") ("f" "$B#f(B") ("g" "$B#g(B")
("h" "$B#h(B") ("i" "$B#i(B") ("j" "$B#j(B") ("k" "$B#k(B")
("l" "$B#l(B") ("m" "$B#m(B") ("n" "$B#n(B") ("o" "$B#o(B")
("p" "$B#p(B") ("q" "$B#q(B") ("r" "$B#r(B") ("s" "$B#s(B")
("t" "$B#t(B") ("u" "$B#u(B") ("v" "$B#v(B") ("w" "$B#w(B")
("x" "$B#x(B") ("y" "$B#y(B") ("z" "$B#z(B") ("{" "$B!P(B")
("|" "$B!C(B") ("}" "$B!Q(B") ("~" "$B!A(B")

("qq" quail-japanese-switch-package)
("qh" quail-japanese-switch-package)
)

(defun quail-japanese-hankaku-update-translation (control-flag)
  (setq control-flag
	(quail-japanese-update-translation control-flag))
  (if (or (and (stringp quail-current-str)
	       (> (length quail-current-str) 0))
	  (integerp quail-current-str))
      (setq quail-current-str (japanese-hankaku quail-current-str)))
  control-flag)

(quail-define-package
 "japanese-hankaku-kana"
 "Japanese" "(I1(B"
 nil
 "Japanese hankaku katakana input method by Roman transliteration.
---- Special key bindings ----
qq:	toggle between this input method and the input method `japanese-ascii'.
"
 nil t t nil nil nil nil nil
 'quail-japanese-hankaku-update-translation)

(dolist (elt quail-japanese-transliteration-rules)
  (quail-defrule (car elt)
		 (let ((trans (nth 1 elt)))
		   (when (or (stringp trans) (vectorp trans))
		     (let ((s (japanese-hankaku (if (stringp trans)
						    trans
						  (aref trans 0)))))
		       ;; If the result of the conversion is a string
		       ;; containing more than one character, make the
		       ;; result a vector, so that quail-defrule
		       ;; recognizes the whole string is the
		       ;; translation, instead of interpreting
		       ;; individual characters as alternative
		       ;; translations.
		       (if (and (stringp s) (> (length s) 1))
			   (setq trans (vector s))
			 (setq trans s))))
		   trans)))

(quail-define-package
 "japanese-hiragana" "Japanese" "$B$"(B"
 nil
 "Japanese hiragana input method by Roman transliteration."
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation)

;; Use the same map as that of `japanese'.
(setcar (cdr (cdr quail-current-package))
	(nth 2 (assoc "japanese" quail-package-alist)))

;; Update Quail translation region while converting Hiragana to Katakana.
(defun quail-japanese-katakana-update-translation (control-flag)
  (setq control-flag
	(quail-japanese-update-translation control-flag))
  (if (or (and (stringp quail-current-str)
	       (> (length quail-current-str) 0))
	  (integerp quail-current-str))
      (setq quail-current-str (japanese-katakana quail-current-str)))
  control-flag)

(quail-define-package
 "japanese-katakana" "Japanese" "$B%"(B"
 nil
 "Japanese katakana input method by Roman transliteration."
 nil t t nil nil nil nil nil
 'quail-japanese-katakana-update-translation)

(dolist (elt quail-japanese-transliteration-rules)
  (quail-defrule (car elt)
		 (let ((trans (nth 1 elt)))
		   (cond ((stringp trans)
			  (japanese-katakana trans))
			 ((vectorp trans)
			  (vector (japanese-katakana (aref trans 0))))
			 (t trans)))))

;;; japanese.el ends here
