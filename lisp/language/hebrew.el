;;; hebrew.el --- support for Hebrew -*- coding: utf-8 -*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Hebrew

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

;; For Hebrew, the character set ISO8859-8 is supported.
;; See http://www.ecma.ch/ecma1/STAND/ECMA-121.HTM.
;; Windows-1255 is also supported.

;;; Code:

(define-coding-system 'hebrew-iso-8bit
  "ISO 2022 based 8-bit encoding for Hebrew (MIME:ISO-8859-8)."
  :coding-type 'charset
  :mnemonic ?8
  :charset-list '(iso-8859-8)
  :mime-charset 'iso-8859-8)

(define-coding-system-alias 'iso-8859-8 'hebrew-iso-8bit)

;; These are for Explicit and Implicit directionality information, as
;; defined in RFC 1556.
(define-coding-system-alias 'iso-8859-8-e 'hebrew-iso-8bit)
(define-coding-system-alias 'iso-8859-8-i 'hebrew-iso-8bit)

(set-language-info-alist
 "Hebrew" '((tutorial . "TUTORIAL.he")
	    (charset iso-8859-8)
	    (coding-priority hebrew-iso-8bit)
	    (coding-system hebrew-iso-8bit windows-1255 cp862)
	    (nonascii-translation . iso-8859-8)
	    (input-method . "hebrew")
	    (unibyte-display . hebrew-iso-8bit)
	    (sample-text . "Hebrew	שלום")
	    (documentation . "Bidirectional editing is supported.")))

(set-language-info-alist
 "Windows-1255" '((coding-priority windows-1255)
		  (coding-system windows-1255)
		  (documentation . "\
Support for Windows-1255 encoding, e.g. for Yiddish.
Bidirectional editing is supported.")))

(define-coding-system 'windows-1255
  "windows-1255 (Hebrew) encoding (MIME: WINDOWS-1255)"
  :coding-type 'charset
  :mnemonic ?h
  :charset-list '(windows-1255)
  :mime-charset 'windows-1255)
(define-coding-system-alias 'cp1255 'windows-1255)

(define-coding-system 'cp862
  "DOS codepage 862 (Hebrew)"
  :coding-type 'charset
  :mnemonic ?D
  :charset-list '(cp862)
  :mime-charset 'cp862)
(define-coding-system-alias 'ibm862 'cp862)

;; Return a nested alist of Hebrew character sequences vs the
;; corresponding glyph of FONT-OBJECT.
(defun hebrew-font-get-precomposed (font-object)
  (let ((precomposed (font-get font-object 'hebrew-precomposed))
	;; Vector of Hebrew precomposed characters.
	(chars [#xFB2A #xFB2B #xFB2C #xFB2D #xFB2E #xFB2F #xFB30 #xFB31
		#xFB32 #xFB33 #xFB34 #xFB35 #xFB36 #xFB38 #xFB39 #xFB3A
		#xFB3B #xFB3C #xFB3E #xFB40 #xFB41 #xFB43 #xFB44 #xFB46
		#xFB47 #xFB48 #xFB49 #xFB4A #xFB4B #xFB4C #xFB4D #xFB4E])
	;; Vector of decomposition character sequences corresponding
	;; to the above vector.
	(decomposed
	 [[#x05E9 #x05C1]
	  [#x05E9 #x05C2]
	  [#x05E9 #x05BC #x05C1]
	  [#x05E9 #x05BC #x05C2]
	  [#x05D0 #x05B7]
	  [#x05D0 #x05B8]
	  [#x05D0 #x05BC]
	  [#x05D1 #x05BC]
	  [#x05D2 #x05BC]
	  [#x05D3 #x05BC]
	  [#x05D4 #x05BC]
	  [#x05D5 #x05BC]
	  [#x05D6 #x05BC]
	  [#x05D8 #x05BC]
	  [#x05D9 #x05BC]
	  [#x05DA #x05BC]
	  [#x05DB #x05BC]
	  [#x05DC #x05BC]
	  [#x05DE #x05BC]
	  [#x05E0 #x05BC]
	  [#x05E1 #x05BC]
	  [#x05E3 #x05BC]
	  [#x05E4 #x05BC]
	  [#x05E6 #x05BC]
	  [#x05E7 #x05BC]
	  [#x05E8 #x05BC]
	  [#x05E9 #x05BC]
	  [#x05EA #x05BC]
	  [#x05D5 #x05B9]
	  [#x05D1 #x05BF]
	  [#x05DB #x05BF]
	  [#x05E4 #x05BF]]))
    (unless precomposed
      (setq precomposed (list t))
      (let ((gvec (font-get-glyphs font-object 0 (length chars) chars)))
	(dotimes (i (length chars))
	  (if (aref gvec i)
	      (set-nested-alist (aref decomposed i) (aref gvec i)
				precomposed))))
      ;; Cache the result in FONT-OBJECT's property.
      (font-put font-object 'hebrew-precomposed precomposed))
    precomposed))

;; Composition function for hebrew.  GSTRING is made of a Hebrew base
;; character followed by Hebrew diacritical marks, or is made of
;; single Hebrew diacritical mark.  Adjust GSTRING to display that
;; sequence properly.  The basic strategy is:
;;
;; (1) If there's single diacritical, add padding space to the left
;; and right of the glyph.
;;
;; (2) If the font has OpenType features for Hebrew, ask the OTF
;; driver the whole work.
;;
;; (3) If the font has precomposed glyphs, use them as far as
;; possible.  Adjust the remaining glyphs artificially.

(defun hebrew-shape-gstring (gstring)
  (let* ((font (lgstring-font gstring))
	 (otf (font-get font :otf))
	 (nchars (lgstring-char-len gstring))
	 header nglyphs base-width glyph precomposed val idx)
    (cond
     ((= nchars 1)
      ;; Independent diacritical mark.  Add padding space to left or
      ;; right so that the glyph doesn't overlap with the surrounding
      ;; chars.
      (setq glyph (lgstring-glyph gstring 0))
      (let ((width (lglyph-width glyph))
	    bearing)
	(if (< (setq bearing (lglyph-lbearing glyph)) 0)
	    (lglyph-set-adjustment glyph bearing 0 (- width bearing)))
	(if (> (setq bearing (lglyph-rbearing glyph)) width)
	    (lglyph-set-adjustment glyph 0 0 bearing))))

     ((or (assq 'hebr (car otf)) (assq 'hebr (cdr otf)))
      ;; FONT has OpenType features for Hebrew.
      (font-shape-gstring gstring))

     (t
      ;; FONT doesn't have OpenType features for Hebrew.
      ;; Try a precomposed glyph.
      ;; Now GSTRING is in this form:
      ;;   [[FONT CHAR1 CHAR2 ... CHARn] nil GLYPH1 GLYPH2 ... GLYPHn nil ...]
      (setq precomposed (hebrew-font-get-precomposed font)
	    header (lgstring-header gstring)
	    val (lookup-nested-alist header precomposed nil 1))
      (if (and (consp val) (vectorp (car val)))
	  ;; All characters can be displayed by a single precomposed glyph.
	  ;; Reform GSTRING to [HEADER nil PRECOMPOSED-GLYPH nil ...]
	  (let ((glyph (copy-sequence (car val))))
	    (lglyph-set-from-to glyph 0 (1- nchars))
	    (lgstring-set-glyph gstring 0 glyph)
	    (lgstring-set-glyph gstring 1 nil))
	(if (and (integerp val) (> val 2)
		 (setq glyph (lookup-nested-alist header precomposed val 1))
		 (consp glyph) (vectorp (car glyph)))
	    ;; The first (1- VAL) characters can be displayed by a
	    ;; precomposed glyph.  Provided that VAL is 3, the first
	    ;; two glyphs should be replaced by the precomposed glyph.
	    ;; In that case, reform GSTRING to:
	    ;;   [HEADER nil PRECOMPOSED-GLYPH GLYPH3 ... GLYPHn nil ...]
	    (let* ((ncmp (1- val))	; number of composed glyphs
		   (diff (1- ncmp)))	; number of reduced glyphs
	      (setq glyph (copy-sequence (car glyph)))
	      (lglyph-set-from-to glyph 0 (1- nchars))
	      (lgstring-set-glyph gstring 0 glyph)
	      (setq idx ncmp)
	      (while (< idx nchars)
		(setq glyph (lgstring-glyph gstring idx))
		(lglyph-set-from-to glyph 0 (1- nchars))
		(lgstring-set-glyph gstring (- idx diff) glyph)
		(setq idx (1+ idx)))
	      (lgstring-set-glyph gstring (- idx diff) nil)
	      (setq idx (- ncmp diff)
		    nglyphs (- nchars diff)))
	  (setq glyph (lgstring-glyph gstring 0))
	  (lglyph-set-from-to glyph 0 (1- nchars))
	  (setq idx 1 nglyphs nchars))
	;; Now IDX is an index to the first non-precomposed glyph.
	;; Adjust positions of the remaining glyphs artificially.
	(setq base-width (lglyph-width (lgstring-glyph gstring 0)))
	(while (< idx nglyphs)
	  (setq glyph (lgstring-glyph gstring idx))
	  (lglyph-set-from-to glyph 0 (1- nchars))
	  (if (>= (lglyph-lbearing glyph) (lglyph-width glyph))
	      ;; It seems that this glyph is designed to be rendered
	      ;; before the base glyph.
	      (lglyph-set-adjustment glyph (- base-width) 0 0)
	    (if (>= (lglyph-lbearing glyph) 0)
		;; Align the horizontal center of this glyph to the
		;; horizontal center of the base glyph.
		(let ((width (- (lglyph-rbearing glyph)
				(lglyph-lbearing glyph))))
		  (lglyph-set-adjustment glyph
					 (- (/ (- base-width width) 2)
					    (lglyph-lbearing glyph)
					    base-width) 0 0))))
	  (setq idx (1+ idx))))))
    gstring))

(let* ((base "[\u05D0-\u05F2]")
       (combining "[\u0591-\u05BD\u05BF\u05C1-\u05C2\u05C4-\u05C5\u05C7]+")
       (pattern1 (concat base combining))
       (pattern2 (concat base "\u200D" combining)))
  (set-char-table-range
   composition-function-table '(#x591 . #x5C7)
   (list (vector pattern2 3 'hebrew-shape-gstring)
	 (vector pattern2 2 'hebrew-shape-gstring)
	 (vector pattern1 1 'hebrew-shape-gstring)
	 [nil 0 hebrew-shape-gstring]))
  ;; Exclude non-combining characters.
  (set-char-table-range
   composition-function-table #x5BE nil)
  (set-char-table-range
   composition-function-table #x5C0 nil)
  (set-char-table-range
   composition-function-table #x5C3 nil)
  (set-char-table-range
   composition-function-table #x5C6 nil))

(provide 'hebrew)

;;; hebrew.el ends here
