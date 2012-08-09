;;; cyril-util.el --- utilities for Cyrillic scripts

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Keywords: mule, multilingual, Cyrillic

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

;; Fixme: are the next two useful?

;;;###autoload
(defun cyrillic-encode-koi8-r-char (char)
  "Return KOI8-R external character code of CHAR if appropriate."
  (encode-char char 'koi8-r))

;;;###autoload
(defun cyrillic-encode-alternativnyj-char (char)
  "Return ALTERNATIVNYJ external character code of CHAR if appropriate."
  (encode-char char 'alternativnyj))


;; Display

;; Written by Valery Alexeev <valery@math.uga.edu>.

(defvar cyrillic-language-alist
      (list '("Belorussian") '("Bulgarian") '("Macedonian")
	    '("Russian") '("Serbo-Croatian") '("Ukrainian"))
      "List of known cyrillic languages.")

;;;###autoload
(defun standard-display-cyrillic-translit (&optional cyrillic-language)
  "Display a cyrillic buffer using a transliteration.
For readability, the table is slightly
different from the one used for the input method `cyrillic-translit'.

The argument is a string which specifies which language you are using;
that affects the choice of transliterations slightly.
Possible values are listed in `cyrillic-language-alist'.
If the argument is t, we use the default cyrillic transliteration.
If the argument is nil, we return the display table to its standard state."
  (interactive
   (list
    (let* ((completion-ignore-case t))
      (completing-read
       "Cyrillic language (default nil): "
       cyrillic-language-alist nil t nil nil nil))))

  (or standard-display-table
      (setq standard-display-table (make-display-table)))

  (if (equal cyrillic-language "")
      (setq cyrillic-language nil))

  (if (null cyrillic-language)
      (setq standard-display-table (make-display-table))
    (aset standard-display-table ?,LP(B  [?a])
    (aset standard-display-table ?,LQ(B  [?b])
    (aset standard-display-table ?,LR(B  [?v])
    (aset standard-display-table ?,LS(B  [?g])
    (aset standard-display-table ?,LT(B  [?d])
    (aset standard-display-table ?,LU(B  [?e])
    (aset standard-display-table ?,Lq(B  [?y ?o])
    (aset standard-display-table ?,LV(B  [?z ?h])
    (aset standard-display-table ?,LW(B  [?z])
    (aset standard-display-table ?,LX(B  [?i])
    (aset standard-display-table ?,LY(B  [?j])
    (aset standard-display-table ?,LZ(B  [?k])
    (aset standard-display-table ?,L[(B  [?l])
    (aset standard-display-table ?,L\(B  [?m])
    (aset standard-display-table ?,L](B  [?n])
    (aset standard-display-table ?,L^(B  [?o])
    (aset standard-display-table ?,L_(B  [?p])
    (aset standard-display-table ?,L`(B  [?r])
    (aset standard-display-table ?,La(B  [?s])
    (aset standard-display-table ?,Lb(B  [?t])
    (aset standard-display-table ?,Lc(B  [?u])
    (aset standard-display-table ?,Ld(B  [?f])
    (aset standard-display-table ?,Le(B  [?k ?h])
    (aset standard-display-table ?,Lf(B  [?t ?s])
    (aset standard-display-table ?,Lg(B  [?c ?h])
    (aset standard-display-table ?,Lh(B  [?s ?h])
    (aset standard-display-table ?,Li(B  [?s ?c ?h])
    (aset standard-display-table ?,Lj(B  [?~])
    (aset standard-display-table ?,Lk(B  [?y])
    (aset standard-display-table ?,Ll(B  [?'])
    (aset standard-display-table ?,Lm(B  [?e ?'])
    (aset standard-display-table ?,Ln(B  [?y ?u])
    (aset standard-display-table ?,Lo(B  [?y ?a])

    (aset standard-display-table ?,L0(B  [?A])
    (aset standard-display-table ?,L1(B  [?B])
    (aset standard-display-table ?,L2(B  [?V])
    (aset standard-display-table ?,L3(B  [?G])
    (aset standard-display-table ?,L4(B  [?D])
    (aset standard-display-table ?,L5(B  [?E])
    (aset standard-display-table ?,L!(B  [?Y ?o])
    (aset standard-display-table ?,L6(B  [?Z ?h])
    (aset standard-display-table ?,L7(B  [?Z])
    (aset standard-display-table ?,L8(B  [?I])
    (aset standard-display-table ?,L9(B  [?J])
    (aset standard-display-table ?,L:(B  [?K])
    (aset standard-display-table ?,L;(B  [?L])
    (aset standard-display-table ?,L<(B  [?M])
    (aset standard-display-table ?,L=(B  [?N])
    (aset standard-display-table ?,L>(B  [?O])
    (aset standard-display-table ?,L?(B  [?P])
    (aset standard-display-table ?,L@(B  [?R])
    (aset standard-display-table ?,LA(B  [?S])
    (aset standard-display-table ?,LB(B  [?T])
    (aset standard-display-table ?,LC(B  [?U])
    (aset standard-display-table ?,LD(B  [?F])
    (aset standard-display-table ?,LE(B  [?K ?h])
    (aset standard-display-table ?,LF(B  [?T ?s])
    (aset standard-display-table ?,LG(B  [?C ?h])
    (aset standard-display-table ?,LH(B  [?S ?h])
    (aset standard-display-table ?,LI(B  [?S ?c ?h])
    (aset standard-display-table ?,LJ(B  [?~])
    (aset standard-display-table ?,LK(B  [?Y])
    (aset standard-display-table ?,LL(B  [?'])
    (aset standard-display-table ?,LM(B  [?E ?'])
    (aset standard-display-table ?,LN(B  [?Y ?u])
    (aset standard-display-table ?,LO(B  [?Y ?a])

    (aset standard-display-table ?,Lt(B  [?i ?e])
    (aset standard-display-table ?,Lw(B  [?i])
    (aset standard-display-table ?,L~(B  [?u])
    (aset standard-display-table ?,Lr(B  [?d ?j])
    (aset standard-display-table ?,L{(B  [?c ?h ?j])
    (aset standard-display-table ?,Ls(B  [?g ?j])
    (aset standard-display-table ?,Lu(B  [?s])
    (aset standard-display-table ?,L|(B  [?k])
    (aset standard-display-table ?,Lv(B  [?i])
    (aset standard-display-table ?,Lx(B  [?j])
    (aset standard-display-table ?,Ly(B  [?l ?j])
    (aset standard-display-table ?,Lz(B  [?n ?j])
    (aset standard-display-table ?,L(B  [?d ?z])

    (aset standard-display-table ?,L$(B  [?Y ?e])
    (aset standard-display-table ?,L'(B  [?Y ?i])
    (aset standard-display-table ?,L.(B  [?U])
    (aset standard-display-table ?,L"(B  [?D ?j])
    (aset standard-display-table ?,L+(B  [?C ?h ?j])
    (aset standard-display-table ?,L#(B  [?G ?j])
    (aset standard-display-table ?,L%(B  [?S])
    (aset standard-display-table ?,L,(B  [?K])
    (aset standard-display-table ?,L&(B  [?I])
    (aset standard-display-table ?,L((B  [?J])
    (aset standard-display-table ?,L)(B  [?L ?j])
    (aset standard-display-table ?,L*(B  [?N ?j])
    (aset standard-display-table ?,L/(B  [?D ?j])

    (when (equal cyrillic-language "Bulgarian")
      (aset standard-display-table ?,Li(B [?s ?h ?t])
      (aset standard-display-table ?,LI(B [?S ?h ?t])
      (aset standard-display-table ?,Ln(B [?i ?u])
      (aset standard-display-table ?,LN(B [?I ?u])
      (aset standard-display-table ?,Lo(B [?i ?a])
      (aset standard-display-table ?,LO(B [?I ?a]))

    (when (equal cyrillic-language "Ukrainian")	; based on the official
					; transliteration table
      (aset standard-display-table ?,LX(B [?y])
      (aset standard-display-table ?,L8(B [?Y])
      (aset standard-display-table ?,LY(B [?i])
      (aset standard-display-table ?,L9(B [?Y])
      (aset standard-display-table ?,Ln(B [?i ?u])
      (aset standard-display-table ?,Lo(B [?i ?a]))))

;;
(provide 'cyril-util)

;; Local Variables:
;; coding: iso-2022-7bit
;; End:

;;; cyril-util.el ends here
