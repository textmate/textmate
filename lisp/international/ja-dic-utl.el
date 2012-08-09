;;; ja-dic-utl.el --- utilities for handling Japanese dictionary (SKK-JISYO.L)

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: i18n, mule, multilingual, Japanese

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

;; This file provides a generic function to look up a Japanese
;; dictionary of SKK format.
;;
;; SKK is a free Japanese input method running on Mule created by
;; Masahiko Sato <masahiko@sato.riec.tohoku.ac.jp>.  The Emacs Lisp
;; library kkc.el provides a facility to convert a Japanese kana
;; string to a kanji-kana-mixed string by using SKK's dictionary.
;;
;; The original SKK dictionary SKK-JISYO.L is converted to ja-dic.el
;; by ja-dic-cnv.el.  We get entries of the dictionary in four
;; variables (listed below) by loading that file (or byte-compiled
;; version ja-dic.elc).

;;; Code:

;; The following four variables are set by loading ja-dic.el[c].
(defvar skkdic-okuri-ari nil
  "Nested alist for OKURI-ARI entries of SKK dictionary.")

(defvar skkdic-postfix nil
  "Nested alist for SETSUBIJI (postfix) entries of SKK dictionary.")

(defvar skkdic-prefix nil
  "Nested alist SETTOUJI (prefix) entries of SKK dictionary.")

(defvar skkdic-okuri-nasi nil
  "Nested alist for OKURI-NASI entries of SKK dictionary.")

(defconst skkdic-okurigana-table
  '((?$B$!(B . ?a) (?$B$"(B . ?a) (?$B$#(B . ?i) (?$B$$(B . ?i) (?$B$%(B . ?u)
    (?$B$&(B . ?u) (?$B$'(B . ?e) (?$B$((B . ?e) (?$B$)(B . ?o) (?$B$*(B . ?o)
    (?$B$+(B . ?k) (?$B$,(B . ?g) (?$B$-(B . ?k) (?$B$.(B . ?g) (?$B$/(B . ?k)
    (?$B$0(B . ?g) (?$B$1(B . ?k) (?$B$2(B . ?g) (?$B$3(B . ?k) (?$B$4(B . ?g)
    (?$B$5(B . ?s) (?$B$6(B . ?z) (?$B$7(B . ?s) (?$B$8(B . ?j) (?$B$9(B . ?s)
    (?$B$:(B . ?z) (?$B$;(B . ?s) (?$B$<(B . ?z) (?$B$=(B . ?s) (?$B$>(B . ?z)
    (?$B$?(B . ?t) (?$B$@(B . ?d) (?$B$A(B . ?t) (?$B$B(B . ?d) (?$B$C(B . ?t)
    (?$B$D(B . ?t) (?$B$E(B . ?d) (?$B$F(B . ?t) (?$B$G(B . ?d) (?$B$H(B . ?t) (?$B$I(B . ?d)
    (?$B$J(B . ?n) (?$B$K(B . ?n) (?$B$L(B . ?n) (?$B$M(B . ?n) (?$B$N(B . ?n)
    (?$B$O(B . ?h) (?$B$P(B . ?b) (?$B$Q(B . ?p) (?$B$R(B . ?h) (?$B$S(B . ?b)
    (?$B$T(B . ?p) (?$B$U(B . ?h) (?$B$V(B . ?b) (?$B$W(B . ?p) (?$B$X(B . ?h)
    (?$B$Y(B . ?b) (?$B$Z(B . ?p) (?$B$[(B . ?h) (?$B$\(B . ?b) (?$B$](B . ?p)
    (?$B$^(B . ?m) (?$B$_(B . ?m) (?$B$`(B . ?m) (?$B$a(B . ?m) (?$B$b(B . ?m)
    (?$B$c(B . ?y) (?$B$d(B . ?y) (?$B$e(B . ?y) (?$B$f(B . ?y) (?$B$g(B . ?y) (?$B$h(B . ?y)
    (?$B$i(B . ?r) (?$B$j(B . ?r) (?$B$k(B . ?r) (?$B$l(B . ?r) (?$B$m(B . ?r)
    (?$B$o(B . ?w) (?$B$p(B . ?w) (?$B$q(B . ?w) (?$B$r(B . ?w)
    (?$B$s(B . ?n)
    )
  "Alist of Okuriganas vs trailing ASCII letters in OKURI-ARI entry.")

(defun skkdic-merge-head-and-tail (heads tails postfix)
  (let ((min-len 2)
	l)
    (while heads
      (if (or (not postfix)
	      (>= (length (car heads)) min-len))
	  (let ((tail tails))
	    (while tail
	      (if (or postfix
		      (>= (length (car tail)) min-len))
		  (setq l (cons (concat (car heads) (car tail)) l)))
	      (setq tail (cdr tail)))))
      (setq heads (cdr heads)))
    l))

(defconst skkdic-jisx0208-hiragana-block
  (cons (decode-char 'japanese-jisx0208 #x2421)
	(decode-char 'japanese-jisx0208 #x247E)))

(defun skkdic-lookup-key (seq len &optional postfix prefer-noun)
  "Return a list of conversion string for sequence SEQ of length LEN.

SEQ is a vector of Kana characters to be converted by SKK dictionary.
If LEN is shorter than the length of KEYSEQ, the first LEN keys in SEQ
are took into account.

Optional 3rd arg POSTFIX non-nil means SETSUBIJI (postfix) are also
considered to find conversion strings.

Optional 4th arg PREFER-NOUN non-nil means that the conversions
without okurigana are placed at the head of the returned list."
  (or skkdic-okuri-nasi
      (condition-case err
	  (load-library "ja-dic/ja-dic")
	(error (ding)
	       (with-output-to-temp-buffer "*Help*"
		 (princ "The library `ja-dic' can't be loaded.

The most common case is that you have not yet installed the library
included in LEIM (Libraries of Emacs Input Method) which is
distributed separately from Emacs.

LEIM is available from the same ftp directory as Emacs."))
	       (signal (car err) (cdr err)))))

  (let ((vec (make-vector len 0))
	(i 0)
	entry)
    ;; At first, generate vector VEC from SEQ for looking up SKK
    ;; alists.  Nth element in VEC corresponds to Nth element in SEQ.
    ;; The values are decided as follows.
    ;;   If SEQ[N] is `$B!<(B', VEC[N] is 0,
    ;;   else if SEQ[N] is a Hiragana character, VEC[N] is:
    ;;     ((The 2nd position code of SEQ[N]) - 32),
    ;;   else VEC[N] is 128.
    (while (< i len)
      (let ((ch (aref seq i))
	    code)
	(cond ((= ch ?$B!<(B)
	       (aset vec i 0))
	      ((and (>= ch (car skkdic-jisx0208-hiragana-block))
		    (<= ch (cdr skkdic-jisx0208-hiragana-block)))
	       (setq code (encode-char ch 'japanese-jisx0208))
	       (if code
		   (aset vec i (- (logand code #xFF) 32))
		 (aset vec i 128)))
	      (t
	       (aset vec i 128))))
      (setq i (1+ i)))

    ;; Search OKURI-NASI entries.
    (setq entry (lookup-nested-alist vec skkdic-okuri-nasi len 0 t))
    (if (consp (car entry))
	(setq entry (copy-sequence (car entry)))
      (setq entry nil))

    (if postfix
	;; Search OKURI-NASI entries with postfixes.
	(let ((break (max (- len (car skkdic-postfix)) 1))
	      entry-head entry-postfix entry2)
	  (while (< break len)
	    (if (and (setq entry-head
			   (lookup-nested-alist vec skkdic-okuri-nasi
						break 0 t))
		     (consp (car entry-head))
		     (setq entry-postfix
			   (lookup-nested-alist vec skkdic-postfix
						len break t))
		     (consp (car entry-postfix))
		     (setq entry2 (skkdic-merge-head-and-tail
				   (car entry-head) (car entry-postfix) t)))
		(if entry
		    (nconc entry entry2)
		  (setq entry entry2)))
	    (setq break (1+ break)))))

    ;; Search OKURI-NASI entries with prefixes.
    (let ((break (min (car skkdic-prefix) (- len 2)))
	  entry-prefix entry-tail entry2)
      (while (> break 0)
	(if (and (setq entry-prefix
		       (lookup-nested-alist vec skkdic-prefix break 0 t))
		 (consp (car entry-prefix))
		 (setq entry-tail
		       (lookup-nested-alist vec skkdic-okuri-nasi len break t))
		 (consp (car entry-tail))
		 (setq entry2 (skkdic-merge-head-and-tail
			       (car entry-prefix) (car entry-tail) nil)))
	    (progn
	      (if entry
		  (nconc entry entry2)
		(setq entry entry2))))
	(setq break (1- break))))

    ;; Search OKURI-ARI entries.
    (let ((okurigana (assq (aref seq (1- len)) skkdic-okurigana-table))
	  orig-element entry2)
      (if okurigana
	  (progn
	    (setq orig-element (aref vec (1- len)))
	    (aset vec (1- len) (- (cdr okurigana)))
	    (if (and (setq entry2 (lookup-nested-alist vec skkdic-okuri-ari
						       len 0 t))
		     (consp (car entry2)))
		(progn
		  (setq entry2 (copy-sequence (car entry2)))
		  (let ((l entry2)
			(okuri (char-to-string (aref seq (1- len)))))
		    (while l
		      (setcar l (concat (car l) okuri))
		      (setq l (cdr l)))
		    (if entry
			(if prefer-noun
			    (nconc entry entry2)
			  (setq entry2 (nreverse entry2))
			  (nconc entry2 entry)
			  (setq entry entry2))
		      (setq entry (nreverse entry2))))))
	    (aset vec (1- len) orig-element))))

    entry))

;;
(provide 'ja-dic-utl)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; ja-dic-utl.el ends here
