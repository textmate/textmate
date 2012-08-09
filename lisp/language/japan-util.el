;;; japan-util.el --- utilities for Japanese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, Japanese

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

;;;###autoload
(defun setup-japanese-environment-internal ()
  ;; By default, we use 'japanese-iso-8bit for file names.  But, the
  ;; following prefer-coding-system will override it.
  (if (memq system-type '(windows-nt ms-dos cygwin))
      (prefer-coding-system 'japanese-shift-jis)
    (prefer-coding-system 'japanese-iso-8bit))
  (use-cjk-char-width-table 'ja_JP))

(defconst japanese-kana-table
  '((?$B$"(B ?$B%"(B ?(I1(B) (?$B$$(B ?$B%$(B ?(I2(B) (?$B$&(B ?$B%&(B ?(I3(B) (?$B$((B ?$B%((B ?(I4(B) (?$B$*(B ?$B%*(B ?(I5(B)
    (?$B$+(B ?$B%+(B ?(I6(B) (?$B$-(B ?$B%-(B ?(I7(B) (?$B$/(B ?$B%/(B ?(I8(B) (?$B$1(B ?$B%1(B ?(I9(B) (?$B$3(B ?$B%3(B ?(I:(B)
    (?$B$5(B ?$B%5(B ?(I;(B) (?$B$7(B ?$B%7(B ?(I<(B) (?$B$9(B ?$B%9(B ?(I=(B) (?$B$;(B ?$B%;(B ?(I>(B) (?$B$=(B ?$B%=(B ?(I?(B)
    (?$B$?(B ?$B%?(B ?(I@(B) (?$B$A(B ?$B%A(B ?(IA(B) (?$B$D(B ?$B%D(B ?(IB(B) (?$B$F(B ?$B%F(B ?(IC(B) (?$B$H(B ?$B%H(B ?(ID(B)
    (?$B$J(B ?$B%J(B ?(IE(B) (?$B$K(B ?$B%K(B ?(IF(B) (?$B$L(B ?$B%L(B ?(IG(B) (?$B$M(B ?$B%M(B ?(IH(B) (?$B$N(B ?$B%N(B ?(II(B)
    (?$B$O(B ?$B%O(B ?(IJ(B) (?$B$R(B ?$B%R(B ?(IK(B) (?$B$U(B ?$B%U(B ?(IL(B) (?$B$X(B ?$B%X(B ?(IM(B) (?$B$[(B ?$B%[(B ?(IN(B)
    (?$B$^(B ?$B%^(B ?(IO(B) (?$B$_(B ?$B%_(B ?(IP(B) (?$B$`(B ?$B%`(B ?(IQ(B) (?$B$a(B ?$B%a(B ?(IR(B) (?$B$b(B ?$B%b(B ?(IS(B)
    (?$B$d(B ?$B%d(B ?(IT(B) (?$B$f(B ?$B%f(B ?(IU(B) (?$B$h(B ?$B%h(B ?(IV(B)
    (?$B$i(B ?$B%i(B ?(IW(B) (?$B$j(B ?$B%j(B ?(IX(B) (?$B$k(B ?$B%k(B ?(IY(B) (?$B$l(B ?$B%l(B ?(IZ(B) (?$B$m(B ?$B%m(B ?(I[(B)
    (?$B$o(B ?$B%o(B ?(I\(B) (?$B$p(B ?$B%p(B "(I2(B") (?$B$q(B ?$B%q(B "(I4(B") (?$B$r(B ?$B%r(B ?(I&(B)
    (?$B$s(B ?$B%s(B ?(I](B)
    (?$B$,(B ?$B%,(B "(I6^(B") (?$B$.(B ?$B%.(B "(I7^(B") (?$B$0(B ?$B%0(B "(I8^(B") (?$B$2(B ?$B%2(B "(I9^(B") (?$B$4(B ?$B%4(B "(I:^(B")
    (?$B$6(B ?$B%6(B "(I;^(B") (?$B$8(B ?$B%8(B "(I<^(B") (?$B$:(B ?$B%:(B "(I=^(B") (?$B$<(B ?$B%<(B "(I>^(B") (?$B$>(B ?$B%>(B "(I?^(B")
    (?$B$@(B ?$B%@(B "(I@^(B") (?$B$B(B ?$B%B(B "(IA^(B") (?$B$E(B ?$B%E(B "(IB^(B") (?$B$G(B ?$B%G(B "(IC^(B") (?$B$I(B ?$B%I(B "(ID^(B")
    (?$B$P(B ?$B%P(B "(IJ^(B") (?$B$S(B ?$B%S(B "(IK^(B") (?$B$V(B ?$B%V(B "(IL^(B") (?$B$Y(B ?$B%Y(B "(IM^(B") (?$B$\(B ?$B%\(B "(IN^(B")
    (?$B$Q(B ?$B%Q(B "(IJ_(B") (?$B$T(B ?$B%T(B "(IK_(B") (?$B$W(B ?$B%W(B "(IL_(B") (?$B$Z(B ?$B%Z(B "(IM_(B") (?$B$](B ?$B%](B "(IN_(B")
    (?$B$!(B ?$B%!(B ?(I'(B) (?$B$#(B ?$B%#(B ?(I((B) (?$B$%(B ?$B%%(B ?(I)(B) (?$B$'(B ?$B%'(B ?(I*(B) (?$B$)(B ?$B%)(B ?(I+(B)
    (?$B$C(B ?$B%C(B ?(I/(B)
    (?$B$c(B ?$B%c(B ?(I,(B) (?$B$e(B ?$B%e(B ?(I-(B) (?$B$g(B ?$B%g(B ?(I.(B)
    (?$B$n(B ?$B%n(B "(I\(B")
    ("$B$&!+(B" ?$B%t(B "(I3^(B") (nil ?$B%u(B "(I6(B") (nil ?$B%v(B "(I9(B"))
  "Japanese JISX0208 Kana character table.
Each element is of the form (HIRAGANA KATAKANA HANKAKU-KATAKANA), where
HIRAGANA and KATAKANA belong to `japanese-jisx0208',
HANKAKU-KATAKANA belongs to `japanese-jisx0201-kana'.")

;; Put properties 'katakana, 'hiragana, and 'jix0201 to each Japanese
;; kana characters for conversion among them.
(let ((l japanese-kana-table)
      slot hiragana katakana jisx0201)
  (while l
    (setq slot (car l)
	  hiragana (car slot) katakana (nth 1 slot) jisx0201 (nth 2 slot)
	  l (cdr l))
    (if hiragana
	(if (stringp hiragana)
	    (if (> (length hiragana) 1)
		(let ((hira (aref hiragana 0)))
		  (put-char-code-property
		   hira 'kana-composition
		   (cons (cons (aref hiragana 1) katakana)
			 (get-char-code-property hira 'kana-composition)))))
	  (put-char-code-property hiragana 'katakana katakana)
	  (put-char-code-property hiragana 'jisx0201 jisx0201)))
    (when (integerp katakana)
      (put-char-code-property katakana 'hiragana hiragana)
      (put-char-code-property katakana 'jisx0201 jisx0201))
    (if jisx0201
	(if (stringp jisx0201)
	    (if (> (length jisx0201) 1)
		(let ((kana (aref jisx0201 0)))
		  (put-char-code-property
		   kana 'kana-composition
		   (cons (cons (aref jisx0201 1) katakana)
			 (get-char-code-property kana 'kana-composition)))))
	  (put-char-code-property jisx0201 'hiragana hiragana)
	  (put-char-code-property jisx0201 'katakana katakana)
	  (put-char-code-property jisx0201 'jisx0208 katakana)))))

(defconst japanese-symbol-table
  '((?\$B!!(B ?\ ) (?$B!$(B ?, ?(I$(B) (?$B!%(B ?. ?(I!(B) (?$B!"(B ?, ?(I$(B) (?$B!#(B ?. ?(I!(B) (?$B!&(B nil ?(I%(B)
    (?$B!'(B ?:) (?$B!((B ?\;) (?$B!)(B ??) (?$B!*(B ?!) (?$B!+(B nil ?(I^(B) (?$B!,(B nil ?(I_(B)
    (?$B!-(B ?') (?$B!.(B ?`) (?$B!0(B ?^) (?$B!2(B ?_) (?$B!<(B ?- ?(I0(B) (?$B!=(B ?-) (?$B!>(B ?-)
    (?$B!?(B ?/) (?$B!@(B ?\\) (?$B!A(B ?~)  (?$B!C(B ?|) (?$B!F(B ?`) (?$B!G(B ?') (?$B!H(B ?\") (?$B!I(B ?\")
    (?\$B!J(B ?\() (?\$B!K(B ?\)) (?\$B!N(B ?[) (?\$B!O(B ?]) (?\$B!P(B ?{) (?\$B!Q(B ?})
    (?$B!R(B ?<) (?$B!S(B ?>) (?\$B!V(B nil ?\(I"(B) (?\$B!W(B nil ?\(I#(B) 
    (?$B!\(B ?+) (?$B!](B ?-) (?$B!a(B ?=) (?$B!c(B ?<) (?$B!d(B ?>)
    (?$B!l(B ?') (?$B!m(B ?\") (?$B!o(B ?\\) (?$B!p(B ?$) (?$B!s(B ?%) (?$B!t(B ?#) (?$B!u(B ?&) (?$B!v(B ?*)
    (?$B!w(B ?@)
    ;; cp932-2-byte
    (#x2015 ?-) (#xFF5E ?~) (#xFF0D ?-))
  "Japanese JISX0208 and CP932 symbol character table.
  Each element is of the form (SYMBOL ASCII HANKAKU), where SYMBOL
belongs to `japanese-jisx0208' or `cp932', ASCII belongs to `ascii',
and HANKAKU belongs to `japanese-jisx0201-kana'.")

;; Put properties 'jisx0208, 'jisx0201, and 'ascii to each Japanese
;; symbol and ASCII characters for conversion among them.
(let ((l japanese-symbol-table)
      slot jisx0208 ascii jisx0201)
  (while l
    (setq slot (car l)
	  jisx0208 (car slot) ascii (nth 1 slot) jisx0201 (nth 2 slot)
	  l (cdr l))
    (if ascii
	(progn
	  (put-char-code-property jisx0208 'ascii ascii)
	  (if (encode-char jisx0208 'japanese-jisx0208)
	      (put-char-code-property ascii 'jisx0208 jisx0208))))
    (if jisx0201
	(progn
	  (put-char-code-property jisx0208 'jisx0201 jisx0201)
	  (if (encode-char jisx0208 'japanese-jisx0208)
	      (put-char-code-property jisx0201 'jisx0208 jisx0208))))))

(defconst japanese-alpha-numeric-table
  '((?$B#0(B . ?0) (?$B#1(B . ?1) (?$B#2(B . ?2) (?$B#3(B . ?3) (?$B#4(B . ?4)
    (?$B#5(B . ?5) (?$B#6(B . ?6) (?$B#7(B . ?7) (?$B#8(B . ?8) (?$B#9(B . ?9)
    (?$B#A(B . ?A) (?$B#B(B . ?B) (?$B#C(B . ?C) (?$B#D(B . ?D) (?$B#E(B . ?E)
    (?$B#F(B . ?F) (?$B#G(B . ?G) (?$B#H(B . ?H) (?$B#I(B . ?I) (?$B#J(B . ?J)
    (?$B#K(B . ?K) (?$B#L(B . ?L) (?$B#M(B . ?M) (?$B#N(B . ?N) (?$B#O(B . ?O)
    (?$B#P(B . ?P) (?$B#Q(B . ?Q) (?$B#R(B . ?R) (?$B#S(B . ?S) (?$B#T(B . ?T)
    (?$B#U(B . ?U) (?$B#V(B . ?V) (?$B#W(B . ?W) (?$B#X(B . ?X) (?$B#Y(B . ?Y) (?$B#Z(B . ?Z)
    (?$B#a(B . ?a) (?$B#b(B . ?b) (?$B#c(B . ?c) (?$B#d(B . ?d) (?$B#e(B . ?e)
    (?$B#f(B . ?f) (?$B#g(B . ?g) (?$B#h(B . ?h) (?$B#i(B . ?i) (?$B#j(B . ?j)
    (?$B#k(B . ?k) (?$B#l(B . ?l) (?$B#m(B . ?m) (?$B#n(B . ?n) (?$B#o(B . ?o)
    (?$B#p(B . ?p) (?$B#q(B . ?q) (?$B#r(B . ?r) (?$B#s(B . ?s) (?$B#t(B . ?t)
    (?$B#u(B . ?u) (?$B#v(B . ?v) (?$B#w(B . ?w) (?$B#x(B . ?x) (?$B#y(B . ?y) (?$B#z(B . ?z))
  "Japanese JISX0208 alpha numeric character table.
Each element is of the form (ALPHA-NUMERIC ASCII), where ALPHA-NUMERIC
belongs to `japanese-jisx0208', ASCII belongs to `ascii'.")

;; Put properties 'jisx0208 and 'ascii to each Japanese alpha numeric
;; and ASCII characters for conversion between them.
(let ((l japanese-alpha-numeric-table)
      slot jisx0208 ascii)
  (while l
    (setq slot (car l)
	  jisx0208 (car slot) ascii (cdr slot)
	  l (cdr l))
    (put-char-code-property jisx0208 'ascii ascii)
    (put-char-code-property ascii 'jisx0208 jisx0208)))

;; Convert string STR by FUNC and return a resulting string.
(defun japanese-string-conversion (str func &rest args)
  (let ((buf (get-buffer-create " *Japanese work*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert str)
      (apply func 1 (point) args)
      (buffer-string))))

;;;###autoload
(defun japanese-katakana (obj &optional hankaku)
  "Convert argument to Katakana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument HANKAKU t means to convert to `hankaku' Katakana
 \(`japanese-jisx0201-kana'), in which case return value
 may be a string even if OBJ is a character if two Katakanas are
 necessary to represent OBJ."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-katakana-region hankaku)
    (or (get-char-code-property obj (if hankaku 'jisx0201 'katakana))
	obj)))

;;;###autoload
(defun japanese-hiragana (obj)
  "Convert argument to Hiragana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-hiragana-region)
    (or (get-char-code-property obj 'hiragana)
	obj)))

;;;###autoload
(defun japanese-hankaku (obj &optional ascii-only)
  "Convert argument to `hankaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument ASCII-ONLY non-nil means to return only ASCII character."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-hankaku-region ascii-only)
    (or (and (not ascii-only)
	     (get-char-code-property obj 'jisx0201))
	(get-char-code-property obj 'ascii)
	obj)))

;;;###autoload
(defun japanese-zenkaku (obj)
  "Convert argument to `zenkaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-zenkaku-region)
    (or (get-char-code-property obj 'jisx0208)
	obj)))

(defun japanese-replace-region (from to string)
  "Replace the region specified by FROM and TO to STRING."
  (goto-char from)
  (insert string)
  (delete-char (- to from)))

;;;###autoload
(defun japanese-katakana-region (from to &optional hankaku)
  "Convert Japanese `hiragana' chars in the region to `katakana' chars.
Optional argument HANKAKU t means to convert to `hankaku katakana' character
of which charset is `japanese-jisx0201-kana'."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cH\\|\\cK" nil t)
	(let* ((kana (preceding-char))
	       (composition
		(and (not hankaku)
		     (get-char-code-property kana 'kana-composition)))
	       next slot)
	  (if (and composition (setq slot (assq (following-char) composition)))
	      (japanese-replace-region (match-beginning 0) (1+ (point))
				       (cdr slot))
	    (let ((kata (get-char-code-property
			 kana (if hankaku 'jisx0201 'katakana))))
	      (if kata
		  (japanese-replace-region (match-beginning 0) (point)
					   kata)))))))))


;;;###autoload
(defun japanese-hiragana-region (from to)
  "Convert Japanese `katakana' chars in the region to `hiragana' chars."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cK\\|\\ck" nil t)
	(let* ((kata (preceding-char))
	       (composition (get-char-code-property kata 'kana-composition))
	       next slot)
	  (if (and composition (setq slot (assq (following-char) composition)))
	      (japanese-replace-region (match-beginning 0) (1+ (point))
				       (get-char-code-property
					(cdr slot) 'hiragana))
	    (let ((hira (get-char-code-property kata 'hiragana)))
	      (if hira
		  (japanese-replace-region (match-beginning 0) (point)
					   hira)))))))))

;;;###autoload
(defun japanese-hankaku-region (from to &optional ascii-only)
  "Convert Japanese `zenkaku' chars in the region to `hankaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument ASCII-ONLY non-nil means to convert only to ASCII char."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cj" nil t)
	(let* ((zenkaku (preceding-char))
	       (hankaku (or (and (not ascii-only)
				 (get-char-code-property zenkaku 'jisx0201))
			    (get-char-code-property zenkaku 'ascii))))
	  (if hankaku
	      (japanese-replace-region (match-beginning 0) (match-end 0)
				       hankaku)))))))

;;;###autoload
(defun japanese-zenkaku-region (from to &optional katakana-only)
  "Convert hankaku' chars in the region to Japanese `zenkaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument KATAKANA-ONLY non-nil means to convert only KATAKANA char."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (or (and katakana-only
		      (re-search-forward "\\ck" nil t))
		 (and (not katakana-only)
		      (re-search-forward "\\ca\\|\\ck" nil t)))
	(let* ((hankaku (preceding-char))
	       (composition (get-char-code-property hankaku 'kana-composition))
	       next slot)
	  (if (and composition (setq slot (assq (following-char) composition)))
	      (japanese-replace-region (match-beginning 0) (1+ (point))
				       (cdr slot))
	    (let ((zenkaku (japanese-zenkaku hankaku)))
	      (if zenkaku
		  (japanese-replace-region (match-beginning 0) (match-end 0)
					   zenkaku)))))))))

;;;###autoload
(defun read-hiragana-string (prompt &optional initial-input)
  "Read a Hiragana string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading."
  (read-multilingual-string prompt initial-input "japanese-hiragana"))

;;
(provide 'japan-util)

;;; japan-util.el ends here
