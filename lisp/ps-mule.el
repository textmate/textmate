;;; ps-mule.el --- provide multi-byte character facility to ps-print

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;;	Kenichi Handa <handa@m17n.org> (multi-byte characters)
;; Maintainer: Kenichi Handa <handa@m17n.org> (multi-byte characters)
;;	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, print, PostScript, multibyte, mule
;; Package: ps-print

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; About ps-mule
;; -------------
;;
;; This package is used for ps-print to print multi-byte buffer.
;;
;; See also ps-print.el.
;;
;;
;; Printing Multi-byte Buffer
;; --------------------------
;;
;; The variable `ps-multibyte-buffer' specifies the ps-print multi-byte buffer
;; handling.
;;
;; Valid values for `ps-multibyte-buffer' are:
;;
;;  nil			    This is the value to use the default settings;
;;			    by default, this only works to print buffers with
;;			    only ASCII and Latin characters.   But this default
;;			    setting can be changed by setting the variable
;;			    `ps-mule-font-info-database-default' differently.
;;			    The initial value of this variable is
;;			    `ps-mule-font-info-database-latin' (see
;;			    documentation).
;;
;;  `non-latin-printer'	    This is the value to use when you have a japanese
;;			    or korean PostScript printer and want to print
;;			    buffer with ASCII, Latin-1, Japanese (JISX0208 and
;;			    JISX0201-Kana) and Korean characters.  At present,
;;			    it was not tested with the Korean characters
;;			    printing.  If you have a korean PostScript printer,
;;			    please, test it.
;;
;;  `bdf-font'		    This is the value to use when you want to print
;;			    buffer with BDF fonts.  BDF fonts include both latin
;;			    and non-latin fonts.  BDF (Bitmap Distribution
;;			    Format) is a format used for distributing X's font
;;			    source file.  BDF fonts are included in
;;			    `intlfonts-1.2' which is a collection of X11 fonts
;;			    for all characters supported by Emacs.  In order to
;;			    use this value, be sure to have installed
;;			    `intlfonts-1.2' and set the variable
;;			    `bdf-directory-list' appropriately (see ps-bdf.el
;;			    for documentation of this variable).
;;
;;  `bdf-font-except-latin' This is like `bdf-font' except that it uses
;;			    PostScript default fonts to print ASCII and Latin-1
;;			    characters.  This is convenient when you want or
;;			    need to use both latin and non-latin characters on
;;			    the same buffer.  See `ps-font-family',
;;			    `ps-header-font-family' and `ps-font-info-database'.
;;
;; Any other value is treated as nil.
;;
;; The default is nil.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'ps-print)


;;;###autoload
(defcustom ps-multibyte-buffer nil
  "Specifies the multi-byte buffer handling.

Valid values are:

  nil			  This is the value to use the default settings;
			  by default, this only works to print buffers with
			  only ASCII and Latin characters.   But this default
			  setting can be changed by setting the variable
			  `ps-mule-font-info-database-default' differently.
			  The initial value of this variable is
			  `ps-mule-font-info-database-latin' (see
			  documentation).

  `non-latin-printer'	  This is the value to use when you have a Japanese
			  or Korean PostScript printer and want to print
			  buffer with ASCII, Latin-1, Japanese (JISX0208 and
			  JISX0201-Kana) and Korean characters.  At present,
			  it was not tested with the Korean characters
			  printing.  If you have a korean PostScript printer,
			  please, test it.

  `bdf-font'		  This is the value to use when you want to print
			  buffer with BDF fonts.  BDF fonts include both latin
			  and non-latin fonts.  BDF (Bitmap Distribution
			  Format) is a format used for distributing X's font
			  source file.  BDF fonts are included in
			  `intlfonts-1.2' which is a collection of X11 fonts
			  for all characters supported by Emacs.  In order to
			  use this value, be sure to have installed
			  `intlfonts-1.2' and set the variable
			  `bdf-directory-list' appropriately (see ps-bdf.el for
			  documentation of this variable).

  `bdf-font-except-latin' This is like `bdf-font' except that it uses
			  PostScript default fonts to print ASCII and Latin-1
			  characters.  This is convenient when you want or
			  need to use both latin and non-latin characters on
			  the same buffer.  See `ps-font-family',
			  `ps-header-font-family' and `ps-font-info-database'.

Any other value is treated as nil."
  :type '(choice (const non-latin-printer)     (const bdf-font)
		 (const bdf-font-except-latin) (const :tag "nil" nil))
  :group 'ps-print-font)

(defvar ps-mule-font-info-database
  nil
  "Alist of charsets with the corresponding font information.
Each element has the form:

	(CHARSET (FONT-TYPE FONT-SRC FONT-NAME ENCODING BYTES) ...)

Where

CHARSET is a charset (symbol) for this font family,

FONT-TYPE is a font type: normal, bold, italic, or bold-italic.

FONT-SRC is a font source: builtin, bdf, vflib, or nil.

  If FONT-SRC is builtin, FONT-NAME is a built-in PostScript font name.

  If FONT-SRC is bdf, FONT-NAME is a BDF font file name, or a list of
  alternative font names.  To use this font, the external library `ps-bdf'
  is required.

  If FONT-SRC is vflib, FONT-NAME is the name of a font that VFlib knows.
  To use this font, the external library `vflib' is required.

  If FONT-SRC is nil, a proper ASCII font in the variable
  `ps-font-info-database' is used.  This is useful for Latin-1 characters.

ENCODING is a coding system to encode a string of characters of CHARSET into a
proper string matching an encoding of the specified font.  ENCODING may be a
function that does this encoding.  In this case, the function is called with
one argument, the string to encode, and it should return an encoded string.

BYTES specifies how many bytes each character has in the encoded byte
sequence; it should be 1 or 2.

All multi-byte characters are printed by fonts specified in this database
regardless of a font family of ASCII characters.  The exception is Latin-1
characters which are printed by the same font as ASCII characters, thus obey
font family.

See also the variable `ps-font-info-database'.")

(defconst ps-mule-font-info-database-latin
  '((iso-8859-1
     (normal nil nil)))
  "Sample setting of `ps-mule-font-info-database' to use latin fonts.")

(defcustom ps-mule-font-info-database-default
  ps-mule-font-info-database-latin
  "The default setting to use when `ps-multibyte-buffer' is nil."
  :type '(symbol :tag "Multi-Byte Buffer Database Font Default")
  :group 'ps-print-font)

(defconst ps-mule-font-info-database-ps
  '((katakana-jisx0201
     (normal builtin "Ryumin-Light.Katakana")
     (bold builtin "GothicBBB-Medium.Katakana")
     (bold-italic builtin "GothicBBB-Medium.Katakana"))
    (latin-jisx0201
     (normal builtin "Ryumin-Light.Hankaku")
     (bold builtin "GothicBBB-Medium.Hankaku"))
    (japanese-jisx0208
     (normal builtin "Ryumin-Light-H")
     (bold builtin "GothicBBB-Medium-H"))
    (korean-ksc5601
     (normal builtin "Munhwa-Regular-KSC-EUC-H")
     (bold builtin "Munhwa-Bold-KSC-EUC-H"))
    )
  "Sample setting of the `ps-mule-font-info-database' to use builtin PS font.

Currently, data for Japanese and Korean PostScript printers are listed.")

(defconst ps-mule-font-info-database-bdf
  '((iso-8859-1
     (normal bdf ("lt1-24-etl.bdf" "etl24-latin1.bdf"))
     (bold bdf ("lt1-16b-etl.bdf" "etl16b-latin1.bdf"))
     (italic bdf ("lt1-16i-etl.bdf" "etl16i-latin1.bdf"))
     (bold-italic bdf ("lt1-16bi-etl.bdf" "etl16bi-latin1.bdf")))
    (iso-8859-2
     (normal bdf ("lt2-24-etl.bdf" "etl24-latin2.bdf")))
    (iso-8859-3
     (normal bdf ("lt3-24-etl.bdf" "etl24-latin3.bdf")))
    (iso-8859-4
     (normal bdf ("lt4-24-etl.bdf" "etl24-latin4.bdf")))
    (thai-tis620
     (normal bdf ("thai24.bdf" "thai-24.bdf") iso-8859-11))
    (greek-iso8859-7
     (normal bdf ("grk24-etl.bdf" "etl24-greek.bdf") iso-8859-7))
    (hebrew-iso8859-8
     (normal bdf ("heb24-etl.bdf" "etl24-hebrew.bdf") iso-8859-8))
    (jisx0201
     (normal bdf "12x24rk.bdf" jisx0201))
    (cyrillic-iso8859-5
     (normal bdf ("cyr24-etl.bdf" "etl24-cyrillic.bdf") iso-8859-5))
    (iso-8859-9
     (normal bdf ("lt5-24-etl.bdf" "etl24-latin5.bdf") iso-8859-9))
    (chinese-gb2312
     (normal bdf "gb24st.bdf"))
    (japanese-jisx0208
     (normal bdf "jiskan24.bdf"))
    (korean-ksc5601
     (normal bdf "hanglm24.bdf"))
    (japanese-jisx0212
     (normal bdf ("jksp40.bdf" "jisksp40.bdf")))
    (chinese-cns11643-1
     (normal bdf ("cns1-40.bdf" "cns-1-40.bdf")))
    (chinese-cns11643-2
     (normal bdf ("cns2-40.bdf" "cns-2-40.bdf")))
    (big5
     (normal bdf "taipei24.bdf"))
    (chinese-sisheng
     (normal bdf ("sish24-etl.bdf" "etl24-sisheng.bdf")))
    (ipa
     (normal bdf ("ipa24-etl.bdf" "etl24-ipa.bdf") ipa))
    (viscii
     (normal bdf ("visc24-etl.bdf" "etl24-viscii.bdf")))
    (arabic-digit
     (normal bdf ("arab24-0-etl.bdf" "etl24-arabic0.bdf")))
    (arabic-1-column
     (normal bdf ("arab24-1-etl.bdf" "etl24-arabic1.bdf")))
    (lao
     (normal bdf ("lao24-mule.bdf" "mule-lao-24.bdf") ps-mule-encode-lao 1))
    (arabic-2-column
     (normal bdf ("arab24-2-etl.bdf" "etl24-arabic2.bdf")))
    (devanagari-cdac
     (normal bdf "dvsr0ntt-32.bdf"))
    (malayalam-cdac
     (normal bdf "mlkr0ntt-32.bdf"))
    (tamil-cdac
     (normal bdf "tmvl0ntt-32.bdf"))
    (indian-is13194
     (normal bdf ("isci24-mule.bdf" "mule-iscii-24.bdf")))
    (indian-1-column
     (normal bdf ("ind1c24-mule.bdf" "mule-indian-1col-24.bdf")))
    (ethiopic
     (normal bdf ("ethio16f-uni.bdf" "ethiomx24f-uni.bdf") unicode-bmp))
    (chinese-cns11643-3
     (normal bdf ("cns3-40.bdf" "cns-3-40.bdf")))
    (chinese-cns11643-4
     (normal bdf ("cns4-40.bdf" "cns-4-40.bdf")))
    (chinese-cns11643-5
     (normal bdf ("cns5-40.bdf" "cns-5-40.bdf")))
    (chinese-cns11643-6
     (normal bdf ("cns6-40.bdf" "cns-6-40.bdf")))
    (chinese-cns11643-7
     (normal bdf ("cns7-40.bdf" "cns-7-40.bdf")))
    (indian-2-column
     (normal bdf ("ind24-mule.bdf" "mule-indian-24.bdf")))
    (tibetan
     (normal bdf ("tib24p-mule.bdf" "tib24-mule.bdf" "mule-tibmdx-24.bdf")))
    (unicode-bmp
     (normal bdf "etl24-unicode.bdf"))
    )
  "Sample setting of the `ps-mule-font-info-database' to use BDF fonts.
BDF (Bitmap Distribution Format) is a format used for distributing X's font
source file.

Current default value list for BDF fonts is included in `intlfonts-1.2'
which is a collection of X11 fonts for all characters supported by Emacs.

Using this list as default value to `ps-mule-font-info-database', all
characters including ASCII and Latin-1 are printed by BDF fonts.

See also `ps-mule-font-info-database-ps-bdf'.")

(defconst ps-mule-font-info-database-ps-bdf
  (cons (car ps-mule-font-info-database-latin)
	(cdr (cdr ps-mule-font-info-database-bdf)))
  "Sample setting of the `ps-mule-font-info-database' to use BDF fonts.

Current default value list for BDF fonts is included in `intlfonts-1.2'
which is a collection of X11 fonts for all characters supported by Emacs.

Using this list as default value to `ps-mule-font-info-database', all
characters except ASCII and Latin-1 characters are printed with BDF fonts.
ASCII and Latin-1 characters are printed with PostScript font specified
by `ps-font-family' and `ps-header-font-family'.

See also `ps-mule-font-info-database-bdf'.")

;; Functions to access each element of FONT-SPEC.
;;
;; FONT-SPEC is a vector of this form:
;; 	[ID CHARSET FONT-ID FONT-SRC FONT-NAME ENCODING BYTES EXTRA-DATA]
;; Where
;;
;; ID is an identification number for this FONT-SPEC and is unique in the list.
;;
;; CHARSET, FONT-SRC, FONT-NAME, ENCODING, and BYTES are the same as those in
;; `ps-mule-font-info-database' (which see).
;;
;; EXTRA-DATA is a data attached by external libraries.

(defsubst ps-mule-font-spec-id (font-spec) (aref font-spec 0))
(defsubst ps-mule-font-spec-charset (font-spec) (aref font-spec 1))
(defsubst ps-mule-font-spec-font-id (font-spec) (aref font-spec 2))
(defsubst ps-mule-font-spec-src (font-spec) (aref font-spec 3))
(defsubst ps-mule-font-spec-name (font-spec) (aref font-spec 4))
(defsubst ps-mule-font-spec-set-name (font-spec name)
  (aset font-spec 4 name))
(defsubst ps-mule-font-spec-encoding (font-spec) (aref font-spec 5))
(defsubst ps-mule-font-spec-bytes (font-spec) (aref font-spec 6))
(defsubst ps-mule-font-spec-extra (font-spec) (aref font-spec 7))
(defsubst ps-mule-font-spec-set-extra (font-spec extra)
  (aset font-spec 7 extra))

;; Functions to encode character into glyph code.
(defun ps-mule-encode-lao (char)
  (- char #x0DE0))

(defun ps-mule-encode-char (char font-spec)
  (let ((encoding (ps-mule-font-spec-encoding font-spec)))
    (cond ((charsetp encoding)
	   (encode-char char encoding))
	  ((fboundp encoding)
	   (funcall encoding char))
	  (t
	   char))))

;; Array of FONT-SPEC-TABLEs; Nth element is for FONT-TYPE N.
;;
;; FONT-TYPE is 0, 1, 2, or 3 representing normal, bold, italic, and
;; bold-italic respectively.
;;
;; FONT-SPEC-TABLE is a char-table of FONT-SPECs.  It records which
;; character is printed by which FONT-SPEC.  It has one extra slot
;; whose value is an alist of the form:
;;	(CHARSET . FONT-SPEC)
;; FONT-SPEC is a vector of the form:
;;	(ID FONT-SRC FONT-NAME ENCODING EXTRA)
(defvar ps-mule-font-spec-tables nil)

;; Array of FONT-TYPEs; Nth element FONT-NUMBER N.
;;
;; FONT-NUMBER is 0, 1, 2, 3, 4, 5, 6 representing fonts f0, f1, f2,
;; f3, h0, h1, and H0.
(defconst ps-mule-font-number-to-type [ 0 1 2 3 1 0 0 ])

(defsubst ps-mule-get-font-spec (char font-spec-table font-spec)
  "Return a font spec for printing CHAR with FONT-SPEC-TABLE.

FONT-SPEC, if non-nil, is a font spec to try at first.

See the documentation of `ps-mule-font-spec-tables' for the
format of font spec."

  (or (aref font-spec-table char)
      (aset font-spec-table char
	    (or (and (< char 256)
		     (cdr (car (char-table-extra-slot font-spec-table 0))))
		(and font-spec
		     (encode-char char (ps-mule-font-spec-charset font-spec))
		     font-spec)
		(catch 'tag
		  (dolist (elt (char-table-extra-slot font-spec-table 0))
		    (and (encode-char char (car elt))
			 (throw 'tag (cdr elt)))))
		;; Record that no font-spec exist for CHAR.
		t))))

(defconst ps-mule-external-libraries
  '((builtin nil nil nil
	     nil nil nil)
    (bdf     ps-bdf nil bdf-generate-prologue
	     bdf-check-font bdf-generate-font bdf-generate-glyph)
    (pcf     nil nil pcf-generate-prologue
	     pcf-check-font pcf-generate-font pcf-generate-glyph)
    (vflib   nil nil vflib-generate-prologue
	     vflib-check-font vflib-generate-font vflib-generate-glyphs))
  "Alist of external libraries information to support PostScript printing.
Each element has the form:

    (FONT-SRC FEATURE INITIALIZED-P PROLOGUE-FUNC
     CHECK-FUNC FONT-FUNC GLYPH-FUNC)

FONT-SRC is the font source: builtin, bdf, pcf, or vflib.

FEATURE is the feature that provide a facility to handle FONT-SRC.  Except for
`builtin' FONT-SRC, this feature is automatically `require'd before handling
FONT-SRC.  Currently, we only have the feature `ps-bdf'.

INITIALIZED-P indicates if this library is initialized or not.

PROLOGUE-FUNC is a function to generate PostScript code which define several
PostScript procedures that will be called by FONT-FUNC and GLYPHS-FUNC.  It is
called with no argument, and should return a list of strings.

CHECK-FUNC is a function to check if a font is available or not.
It is called with one argument FONT-SPEC, and should return non-nil if and
only if the font specified in FONT-SPEC is available.

FONT-FUNC is a function to generate PostScript code which define a new font.
It is called with one argument FONT-SPEC, and should return a list of strings.

GLYPH-FUNC is a function to generate PostScript code which define glyphs of
characters.  It is called with two arguments FONT-SPEC and CODE, and should
return a list of strings.")

(defsubst ps-mule-exlib-feature (exlib) (nth 1 exlib))
(defsubst ps-mule-exlib-initialized-p (exlib) (nth 2 exlib))
(defsubst ps-mule-exlib-set-initialized-p (exlib val)
  (setcar (nthcdr 2 exlib) val))
(defsubst ps-mule-exlib-prologue (exlib) (nth 3 exlib))
(defsubst ps-mule-exlib-check (exlib) (nth 4 exlib))
(defsubst ps-mule-exlib-font (exlib) (nth 5 exlib))
(defsubst ps-mule-exlib-glyph (exlib) (nth 6 exlib))

(defun ps-mule-init-external-library (exlib)
  "Initialize external library specified by EXLIB for PostScript printing.
See the documentation of `ps-mule-external-libraries' for EXLIB's meaning."
  (or (ps-mule-exlib-initialized-p exlib)
      (let ((prologue-func (ps-mule-exlib-prologue exlib)))
	(if prologue-func
	    (let ((feature (ps-mule-exlib-feature exlib)))
	      (if feature
		  (require feature))
	      (ps-output-prologue (funcall prologue-func))))
	(ps-mule-exlib-set-initialized-p exlib t))))

(defvar ps-mule-output-list nil)

(defun ps-mule-check-font (font-spec)
  "Check if a font specified in FONT-SPEC is available or not."
  (let ((font-src (ps-mule-font-spec-src font-spec)))
    (or (not font-src)
	(let ((exlib (assq font-src ps-mule-external-libraries)))
	  (ps-mule-init-external-library exlib)
	  (or (not (ps-mule-exlib-check exlib))
	      (funcall (ps-mule-exlib-check exlib) font-spec))))))

(defun ps-mule-prepare-font (font-spec)
  "Generate PostScript codes defining a new font of FONT-SPEC for charset."
  (let* ((font-src (ps-mule-font-spec-src font-spec))
	 (exlib (assq font-src ps-mule-external-libraries))
	 (id (ps-mule-font-spec-id font-spec))
	 (ftag (format "%02X" id))
	 (font-func (ps-mule-exlib-font exlib))
	 output-list)
    (if font-func
	(setq output-list (funcall font-func font-spec))
      (setq output-list
	    (format "/F%s /%s findfont def\n"
		    ftag (or (ps-mule-font-spec-name font-spec) "Courier")))
      (ps-mule-font-spec-set-extra font-spec t))
    (and output-list
	 (nconc ps-mule-output-list (list output-list)))))

(defun ps-mule-prepare-glyph (char font-spec)
  "Generate PostScript codes to print CHAR by FONT-SPEC.

If CHAR is a cons (FROM TO), generate codes for characters
specified by the character code range FROM and TO.

The generated code is inserted on prologue part."
  (if (vectorp font-spec)
      (progn
	(or (ps-mule-font-spec-extra font-spec)
	    (ps-mule-prepare-font font-spec))
	(let ((glyph-func (ps-mule-exlib-glyph
			   (assq (ps-mule-font-spec-src font-spec)
				 ps-mule-external-libraries))))
	  (if glyph-func
	      (let (from to output-list)
		(if (consp char)
		    (setq from (car char) to (cdr char))
		  (setq from char to char))
		(while (<= from to)
		  (setq output-list
			(funcall glyph-func font-spec from))
		  (and output-list
		       (ps-output-prologue output-list))
		  (setq from (1+ from)))))))))

;; This is a PostScript code inserted in the header of generated PostScript.
(defconst ps-mule-prologue
  "%%%% Start of Mule Section

/Latin1Encoding {	% newname fontname  |  font
    findfont dup length dict begin
	{ 1 index /FID ne { def } { pop pop } ifelse } forall
        /Encoding ISOLatin1Encoding def
	currentdict
    end
    definefont
} bind def

%% Redefine fonts for multiple charsets.
/ReDefFont {		     % fontname encoding fdepvector size  |  -
  20 dict begin
  3 index findfont {
    1 index /FID ne 2 index /UniqueID ne and {def} {pop pop} ifelse
  } forall
  /FontType 0 def
  /FMapType 3 def
  /EscChar 0 def
  % FontMatrix ::= [ size 0 0 size 0 0 ]
  /FontMatrix exch [ exch dup 0 exch 0 exch 0 0 ] def
  /FDepVector exch def
  /Encoding exch def
  currentdict
  end			% fontname dic
  definefont pop
} bind def
"
  "PostScript code for printing multi-byte characters.")

(defvar ps-mule-prologue-generated nil)

;; EscChar used in generated composite fonts.
(defconst ps-mule-esc-char 0)

(defun ps-mule-prologue-generated ()
  (unless ps-mule-prologue-generated
    (ps-output-prologue ps-mule-prologue)
    (ps-output-prologue
     (format "\n/EscChar %d def\n\n%%%% End of Mule Section\n\n"
	     ps-mule-esc-char))
    (setq ps-mule-prologue-generated t)))

(defun ps-mule-encode-region (from to font-spec-table)
  "Generate PostScript code for plotting characters in the region FROM and TO.

FONT-SPEC-TABLE is 0, 1, 2, 3, 4, 5, or 6, each represents font tags f0, f1,
f2, f3, h0, h1, and H0 respectively."
  (let* ((font-spec nil)
	 (font-id 0)
	 (code-list nil))
    (goto-char from)
    (while (< (point) to)
      (let* ((char (following-char))
	     (this-spec (ps-mule-get-font-spec char font-spec-table font-spec))
	     this-id)
	(if (vectorp this-spec)
	    (setq this-id (ps-mule-font-spec-font-id this-spec))
	  ;; Can't print CHAR.   Replace it with '?'.
	  (setq char ??
		this-spec (ps-mule-get-font-spec char font-spec-table nil)
		this-id (ps-mule-font-spec-font-id this-spec)))
	(unless (= font-id this-id)
	  (setq font-id this-id)
	  (push ps-mule-esc-char code-list)
	  (push font-id code-list))
	(setq font-spec this-spec)
	(if (< char 128)
	    (push char code-list)
	  (let* ((code (ps-mule-encode-char char font-spec)))
	    (if (= (ps-mule-font-spec-bytes font-spec) 1)
		(push code code-list)
	      (push (/ code 256) code-list)
	      (push (% code 256) code-list))))
	(forward-char 1)))
    (apply 'unibyte-string (nreverse code-list))))

(defun ps-mule-plot-composition (composition font-spec-table)
  "Generate PostScript code for plotting COMPOSITION with FONT-SPEC-TABLE."
  (ps-output "[")
  (let ((components (copy-sequence (nth 2 composition)))
	(font-spec nil))
    (dotimes (i (length components))
      (let ((elt (aref components i))
	    this-spec)
	(if (consp elt)
	    ;; ELT is a composition rule.
	    (ps-output (format " %d" (encode-composition-rule elt)))
	  ;; ELT is a glyph character.
	  (setq this-spec
		(ps-mule-get-font-spec elt font-spec-table font-spec))
	  (or (vectorp this-spec)
	      ;; Can't print CHAR.   Replace it with '?'.
	      (setq elt ??
		    this-spec
		    (ps-mule-get-font-spec elt font-spec-table font-spec)))
	  (setq font-spec this-spec)
	  (let* ((bytes (ps-mule-font-spec-bytes font-spec))
		 (code (ps-mule-encode-char elt font-spec))
		 (font-id (ps-mule-font-spec-font-id font-spec))
		 (str (make-string (if (= font-id 0) 1 (+ 2 bytes)) 0)))
	    (if (= font-id 0)
		(aset str 0 code)
	      (aset str 0 ps-mule-esc-char)
	      (aset str 1 font-id)
	      (if (= bytes 1)
		  (aset str 2 code)
		(aset str 2 (/ code 256))
		(aset str 3 (% code 256))))
	    (ps-output "[")
	    (ps-output-string str)
	    (ps-output (if (eq (ps-mule-font-spec-src font-spec) 'bdf)
			   (format "/C%02X-%X" (ps-mule-font-spec-id font-spec)
				   elt)
			 "false"))
	    (ps-output "]"))))))
  (ps-output " ] " (if (nth 3 composition) "RLC" "RBC") "\n"))

(defun ps-mule-plot-string (from to &optional _bg-color)
  "Generate PostScript code for plotting characters in the region FROM and TO.

Optional argument BG-COLOR is ignored.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence."
  (let* ((average-width (ps-avg-char-width 'ps-font-for-text))
	 (point (point))
	 (composition (find-composition from to nil t))
	 (stop (if (and composition
			(not (vectorp (aref (nth 2 composition) 0))))
		   (car composition)
		 to))
	 (ascii-or-latin-1 "[\000-\377]+")
	 (run-width 0)
	 (endpos nil)
	 (font-spec-table (aref ps-mule-font-spec-tables
				(aref ps-mule-font-number-to-type
				      ps-current-font)))
	 width)
    (goto-char from)
    (while (not endpos)
      (cond ((>= (point) stop)
	     (if (= stop to)
		 (setq endpos stop)
	       (when (< from stop)
		 (ps-output-string (ps-mule-encode-region from (point)
							  font-spec-table))
		 (ps-output " S\n"))
	       (setq width (* (nth 5 composition) average-width))
	       (if (< ps-width-remaining (+ run-width width))
		   (setq endpos stop)
		 (ps-mule-plot-composition composition font-spec-table)
		 (setq run-width (+ run-width width)
		       from (nth 1 composition))
		 (goto-char from)
		 (setq composition (find-composition (point) to nil t))
		 (setq stop (if composition (car composition) to)))))

	    ((looking-at ascii-or-latin-1)
	     (let ((nchars (- (min (match-end 0) stop) (point))))
	       (setq width (* average-width nchars))
	       (if (< ps-width-remaining (+ run-width width))
		   (setq nchars (truncate (- ps-width-remaining run-width)
					  average-width)
			 run-width (+ run-width (* nchars average-width))
			 endpos (+ (point) nchars))
		 (setq run-width (+ run-width width))
		 (forward-char nchars))))

	    (t
	     (while (and (< (point) stop) (not endpos))
	       (setq width (char-width (following-char)))
	       (if (< ps-width-remaining (+ run-width width))
		   (setq endpos (point))
		 (setq run-width (+ run-width width))
		 (forward-char 1))))))

    (when (< from endpos)
      (ps-output-string (ps-mule-encode-region from endpos font-spec-table))
      (ps-output " S\n"))
    (goto-char point)
    (cons endpos run-width)))

;; Character composition support

(defvar ps-mule-composition-prologue-generated nil)

(defconst ps-mule-composition-prologue
  "%%%% Procedures for character composition.
/RelativeCompositionSkip 0.4 def

%% Get a bounding box (relative to currentpoint) of STR.
/GetPathBox {			% [ str cname ]  |  -
    dup 1 get dup false ne {
	BitmapDict exch get /bmp exch def
	%% bmp ::= [ DWIDTH WIDTH HEIGHT XOFF YOFF BITMAP RELATIVE-COMPOSE]
	/LLY bmp 4 get def
	/URY LLY bmp 2 get add def
	/RelativeCompose bmp 6 get dup false ne {
	    dup LLY le { pop 1 } { URY ge { -1 } { 0 } ifelse } ifelse
	} {
	    pop 0
	} ifelse def
	dup 0 get stringwidth pop dup /WIDTH exch def bmp 0 get div
	dup LLY mul /LLY exch def
	URY mul /URY exch def
    } {
	pop
	dup 0 get stringwidth pop /WIDTH exch def
	gsave 0 0 moveto
	dup 0 get false charpath flattenpath pathbbox
	/URY exch def pop /LLY exch def pop
	grestore
	/RelativeCompose 0 def
    } ifelse
} bind def

%% Apply effects except for shadow and outline to the rectangle
%% specified by TOP BOTTOM LEFT RIGHT.
/SpecialEffect {		% --  |  --
    currentpoint dup TOP add /yy exch def BOTTOM add /YY exch def
    dup LEFT add /xx exch def RIGHT add /XX exch def
    %% Adjust positions for future shadowing.
    Effect 8 and 0 ne {
	/yy yy Yshadow add def
	/XX XX Xshadow add def
    } if
    Effect 1 and 0 ne { UnderlinePosition Hline } if	% underline
    Effect 2 and 0 ne { StrikeoutPosition Hline } if	% strikeout
    Effect 4 and 0 ne { OverlinePosition  Hline } if	% overline
    bg {						% background
	true
	Effect 16 and 0 ne {SpaceBackground doBox} { xx yy XX YY doRect} ifelse
    } if
    Effect 16 and 0 ne { false 0 doBox } if		% box
} def

%% Draw COMPONENTS which has the form [ [str0 xoff0 yoff0] ... ] with
%% effects shadow and outline.
/ShowComponents {		% components  |  -
    gsave
    { 	gsave aload pop rmoveto
	Effect 8 and 0 ne { dup doShadow } if
	Effect 32 and 0 ne { true doOutline } { show } ifelse
	grestore
    } forall
    grestore
    RIGHT 0 rmoveto
} def

%% Show relative composition.
/RLC {	       % [[str0 cname0] [str1 cname1] ... [strN cnameN]]  |  -
    /components exch def
    [ 				% push [str xoff yoff] one by one
    [ components 0 get GetPathBox aload pop pop 0 0 ]
    %% Bounding box of overall glyphs.
    /LEFT 0 def
    /RIGHT WIDTH def
    /TOP URY def
    /BOTTOM LLY def

    1 1 components length 1 sub {
	components exch get
	[ exch
	    GetPathBox
	    aload pop pop				% str
	    0						% xoff
	    RelativeCompose 1 eq {	    % compose on TOP
		TOP LLY sub RelativeCompositionSkip add	% yoff
		/TOP TOP URY LLY sub add RelativeCompositionSkip add def
	    } { RelativeCompose -1 eq {	% compose under BOTTOM
		BOTTOM URY sub RelativeCompositionSkip sub % yoff
		/BOTTOM BOTTOM URY LLY sub sub
		RelativeCompositionSkip sub def
	    } {
		0					% yoff
		URY TOP gt { /TOP URY def } if
		LLY BOTTOM lt { /BOTTOM LLY def } if
	    } ifelse } ifelse
	]
    } for
    ]
    SpecialEffect		% Reflect special effects.
    ShowComponents		% Draw components.
} def

%% Show rule-base composition.
/RBC { % [[str0 cname0] rule1 [str1 cname0] rule2 ... [strN cnameN]]  |  -
    /components exch def
    [ 				% push [str xoff yoff] one by one
    [ components 0 get GetPathBox aload pop pop 0 0 ]
    %% Bounding box of overall glyphs.
    /LEFT 0 def
    /RIGHT WIDTH def
    /TOP URY def
    /BOTTOM LLY def
    1 1 components length 1 sub {
	components exch get /elt exch def
	elt type /integertype eq {			% rule
	    %% Do the same RULE decoding as the macro
	    %% COMPOSITION_DECODE_RULE in emacs/src/composite.h.
	    elt 12 idiv dup 3 mod /grefx exch def 3 idiv /grefy exch def
	    elt 12 mod dup 3 mod /nrefx exch def 3 idiv /nrefy exch def
	} {						% other strings
	    [
	    elt GetPathBox
	    aload pop pop
	    /height URY LLY sub def
	    /left LEFT [ 0 RIGHT LEFT sub dup 2 div exch ] grefx get add
		[ 0 WIDTH 2 div WIDTH ] nrefx get sub def
	    /bottom [ TOP 0 BOTTOM TOP BOTTOM add 2 div ] grefy get
		[ height LLY neg 0 height 2 div ] nrefy get sub def
	    %% Update bounding box
	    left LEFT lt { /LEFT left def } if
	    left WIDTH add RIGHT gt { /RIGHT left WIDTH add def } if
	    bottom BOTTOM lt { /BOTTOM bottom def } if
	    bottom height add TOP gt { /TOP bottom height add def } if
	    left bottom LLY sub ]
	} ifelse
    } for
    ]

    LEFT 0 lt {			% Adjust xoff to the right.
	dup { dup 1 get LEFT sub 1 exch put } forall
	/RIGHT RIGHT LEFT sub def
    } if

    SpecialEffect		% Reflect special effects.
    ShowComponents		% Draw components.
} def

%%%% End of procedures for character composition
"
  "PostScript code for printing character composition.")

(defun ps-mule-composition-prologue-generated ()
  (unless ps-mule-composition-prologue-generated
    (ps-mule-prologue-generated)
    (ps-output-prologue ps-mule-composition-prologue)
    (setq ps-mule-composition-prologue-generated t)))

;; Bitmap font support

(defvar ps-mule-bitmap-prologue-generated nil)

(defconst ps-mule-bitmap-prologue
  "%%%% Procedures for bitmap fonts.

%% Create a base bitmap font.
/NBF { % fontname fontsize relative-compose baseline-offset enc  |  --
    11 dict begin
    /FontType 3 def
    /FontMatrix matrix def
    /FontBBox [ 0 0 0 0 ] def
    /Encoding exch def
    /BaselineOffset exch def
    /RelativeCompose exch def
    /FontSize exch def
    /FontMatrix [ 1 FontSize div 0 0 1 FontSize div 0 0 ] def
    /BuildGlyph {		% fontdict charname  |  -
	BitmapDict exch get /bmp exch def pop
	%% bmp ::= [ DWIDTH WIDTH HEIGHT XOFF YOFF BITMAP RELATIVE-COMPOSE ]
	/llx bmp 3 get def
	/lly bmp 4 get def
	/urx llx bmp 1 get add def
	/ury lly bmp 2 get add def
	bmp 0 get 0 llx lly urx ury setcachedevice
	bmp 5 get length 0 gt {
	    llx ury translate
	    bmp 1 get bmp 2 get
	    true [ 1 0 0 -1 0 0 ] { bmp 5 get } imagemask
	} if
    } bind def
    /BuildChar { 		% fontdict byte  |  -
	1 index /Encoding get exch get
	1 index /BuildGlyph get exec
    } bind def
    dup currentdict end
    definefont def
} bind def

%% Create a parent font of 8/8 mapping.
/NPF {				% fontname encoding fdepvector  |  -
    8 dict begin
	/FontType 0 def
	/FMapType 2 def
	/FontMatrix matrix def
	/FDepVector exch def
	/Encoding exch def
	dup currentdict
    end
    definefont def
} bind def

%%%% End of procedures for bitmap fonts.
")

;; External library support.

(defvar ps-mule-bitmap-dict-list nil)
(defvar ps-mule-bitmap-font-record nil)

;; The following three functions are to be called from external
;; libraries which support bitmap fonts (e.g. `bdf') to get
;; appropriate PostScript code.

(defun ps-mule-generate-bitmap-prologue ()
  (unless ps-mule-bitmap-prologue-generated
    (setq ps-mule-bitmap-prologue-generated t
	  ps-mule-bitmap-dict-list nil
	  ps-mule-bitmap-font-record (make-vector 1024 nil))
    (list ps-mule-bitmap-prologue)))

(defun ps-mule-generate-bitmap-font (font-spec size relative-compose
					       baseline-offset bbx)
  (let* ((id (ps-mule-font-spec-id font-spec))
	 (bytes (ps-mule-font-spec-bytes font-spec))
	 output-list)
    (if (= bytes 1)
	(setq output-list
	      (list (format "/E%02X [ 0 1 255 {pop /.notdef} for ] def\n" id)
		    (format "%%%% %s\n" (ps-mule-font-spec-name font-spec))
		    (format "/F%02X %f %S %d E%02X NBF\n" id size
			    relative-compose baseline-offset id)))
      (setq output-list
	    (list (list (format "/E%02X [ 0 1 255 { pop 0 } for ] def\n" id))
		  (list (format "/V%02X [" id))
		  " ] def\n"
		  (format "%%%% %s\n" (ps-mule-font-spec-name font-spec))
		  (format "/F%02X E%02X V%02X NPF\n" id id id))))
    (aset ps-mule-bitmap-font-record id
	  (vector (= bytes 1) output-list
		  size relative-compose baseline-offset bbx))
    (if ps-mule-bitmap-dict-list
	output-list
      (setq ps-mule-bitmap-dict-list (list "/BitmapDict <<\n" ">> def\n"))
      (cons ps-mule-bitmap-dict-list output-list))))

(defun ps-mule-generate-bitmap-glyph (font-spec char code bitmap)
  (let* ((id (ps-mule-font-spec-id font-spec))
	 ;; FONT-RECORD ::= ([(SUBFONT-OUTPUT-LIST ...) | t]
	 ;;                  BASEFONT-OUTPUT-LIST SIZE REL-COMP B-OFFSET BBX)
	 (font-record (aref ps-mule-bitmap-font-record id))
	 enc-name
	 output-list)
    (if (listp (aref font-record 0))
	;; This is a 2-dimensional font.  Create a subfont for this
	;; glyph if not yet created.
	(let* ((high (/ code 256))
	       (id2 (+ (* id 256) high)))
	  (setq output-list (cdr (assq high (aref font-record 0)))
		code (% code 256))
	  (or output-list
	      ;; We must create a subfont.
	      (let ((enc-list (car (aref font-record 1)))
		    (fdep-list (nth 1 (aref font-record 1))))
		(setq output-list
		      (list
		       (format "/E%04X [ 0 1 255 {pop /.notdef} for ] def\n"
			       id2)
		       (format "/F%04X %f %S %d E%04X NBF\n"
			       id2 (aref font-record 2) (aref font-record 3)
			       (aref font-record 4) id2)
		       (format "E%02X %d %d put\n"
			       id high (1- (length fdep-list)))))
		(nconc enc-list (list output-list))
		(nconc fdep-list (list (format " F%04X" id2)))
		(aset font-record 0
		      (cons (cons high output-list) (aref font-record 0)))))
	  (setq enc-name (format "%04X" id2)))
      (setq output-list (aref font-record 1)
	    enc-name (format "%02X" id)))
    (setcdr ps-mule-bitmap-dict-list
	    (cons (format "/C%02X-%X %s\n" id char bitmap)
		  (cdr ps-mule-bitmap-dict-list)))
    (setcdr output-list
	    (cons (format "E%s %d /C%02X-%X put\n" enc-name code id char)
		  (cdr output-list))))
  nil)

;; Mule specific initializers.

;;;###autoload
(defun ps-mule-initialize ()
  "Initialize global data for printing multi-byte characters."
  (setq ps-mule-prologue-generated nil
	ps-mule-composition-prologue-generated nil
	ps-mule-bitmap-prologue-generated nil)
  (mapcar `(lambda (x) (setcar (nthcdr 2 x) nil))
	  ps-mule-external-libraries))

(defun ps-mule-encode-header-string (string fonttag)
  "Generate PostScript code for plotting STRING by font FONTTAG.
FONTTAG should be a string \"/h0\", \"/h1\", \"/L0\", or \"/H0\".
Any other value is treated as \"/H0\"."
  (with-temp-buffer
    (insert string)
    (list (ps-mule-encode-region (point-min) (point-max)
				 (aref ps-mule-font-spec-tables
				       (aref ps-mule-font-number-to-type
					     (cond ((string= fonttag "/h0") 4)
						   ((string= fonttag "/h1") 5)
						   ((string= fonttag "/L0") 6)
						   (t 0))))))))

;;;###autoload
(defun ps-mule-begin-job (from to)
  "Start printing job for multi-byte chars between FROM and TO.
It checks if all multi-byte characters in the region are printable or not."
  (if (and (not (find-composition from to))
	   (save-excursion
	     (goto-char from)
	     (= (skip-chars-forward "\x00-\x7F" to) to)))
      ;; All characters can be printed by normal PostScript fonts.
      (setq ps-basic-plot-string-function 'ps-basic-plot-string
	    ps-encode-header-string-function 'identity)
    (setq ps-basic-plot-string-function 'ps-mule-plot-string
	  ps-encode-header-string-function 'ps-mule-encode-header-string
	  ps-mule-font-info-database
	  (cond ((eq ps-multibyte-buffer 'non-latin-printer)
		 ps-mule-font-info-database-ps)
		((eq ps-multibyte-buffer 'bdf-font)
		 ps-mule-font-info-database-bdf)
		((eq ps-multibyte-buffer 'bdf-font-except-latin)
		 ps-mule-font-info-database-ps-bdf)
		(t
		 ps-mule-font-info-database-default)))

    ;; Be sure to have font information for Latin-1.
    (or (assq 'iso-8859-1 ps-mule-font-info-database)
	(setq ps-mule-font-info-database
	      (cons '(iso-8859-1 (normal nil nil))
		    ps-mule-font-info-database)))

    ;; Generate ps-mule-font-spec-tables.
    (let ((font-spec-alist (make-vector 4 nil))
	  (id-max 0)
	  (font-id 0)
	  font-info-list)
      ;; Generate properly ordered font-info-list from
      ;; ps-mule-font-info-database.
      (let ((charset-list
	     (copy-sequence (get-language-info current-language-environment
					       'charset))))
	(setq charset-list (cons 'iso-8859-1 (delq 'iso-8859-1 charset-list)))
	(dolist (charset charset-list)
	  (let ((font-info (assq charset ps-mule-font-info-database)))
	    (and font-info
		 (setq font-info-list (cons font-info font-info-list)))))
	(dolist (font-info ps-mule-font-info-database)
	  (or (memq (car font-info) charset-list)
	      (setq font-info-list (cons font-info font-info-list))))
	(setq font-info-list (nreverse font-info-list)))

      ;; Now font-info-list is an alist ordered by charset priority.
      ;; Store FONT-SPECs in each element of font-spec-alist.
      (dolist (font-info font-info-list)
	(let ((font-spec-vec (make-vector 4 nil))
	      (charset (car font-info))
	      encoding bytes font-spec)
	  (dolist (e (cdr font-info))
	    (setq encoding (nth 3 e) bytes (nth 4 e))
	    (unless encoding
	      (setq encoding charset bytes (charset-dimension charset)))
	    (setq font-spec (vector id-max charset font-id
				    (nth 1 e) (nth 2 e) encoding
				    (or bytes 1) nil)
		  id-max (1+ id-max))
	    (if (ps-mule-check-font font-spec)
		(aset font-spec-vec
		      (cond ((eq (car e) 'normal) 0)
			    ((eq (car e) 'bold) 1)
			    ((eq (car e) 'italic) 2)
			    (t 3)) font-spec)))
	  (when (aref font-spec-vec 0)
	    (or (aref font-spec-vec 3)
		(aset font-spec-vec 3 (or (aref font-spec-vec 1)
					  (aref font-spec-vec 2)
					  (aref font-spec-vec 0))))
	    (or (aref font-spec-vec 1)
		(aset font-spec-vec 1 (aref font-spec-vec 0)))
	    (or (aref font-spec-vec 2)
		(aset font-spec-vec 2 (aref font-spec-vec 1)))
	    (dotimes (i 4)
	      (aset font-spec-alist i
		    (nconc (aref font-spec-alist i)
			   (list (cons charset (aref font-spec-vec i))))))
	    (setq font-id (1+ font-id)))))

      ;; Make four FONT-SPEC-TABLEs and set them in
      ;; ps-mule-font-spec-tables.  Each char table has one extra slot
      ;; whose value is an element of font-spec-alist.
      (setq ps-mule-font-spec-tables (make-vector 4 nil))
      (put 'font-spec-table 'char-table-extra-slots 1)
      (dotimes (i 4)
	(let ((table (make-char-table 'font-spec-table)))
	  (aset ps-mule-font-spec-tables i table)
	  (set-char-table-extra-slot table 0 (aref font-spec-alist i))
	  ;; Be sure to have glyphs for "0123456789/" in advance for
	  ;; page numbering.
	  (let ((str " 0123456789/"))
	    (dotimes (i (length str))
	      (or (vectorp (ps-mule-get-font-spec (aref str i) table nil))
		  (error "ASCII font not available")))))))

    (ps-mule-prologue-generated)
    (if (find-composition from to)
	(ps-mule-composition-prologue-generated))))

(defun ps-mule-restruct-output-list (list tail)
  (dolist (elt list)
    (if (listp elt)
	(setq tail (ps-mule-restruct-output-list elt tail))
      (setcdr tail (cons elt (cdr tail)))
      (setq tail (cdr tail))))
  tail)

(defun ps-mule-redefine-font (font-number fonttag size ps-font)
  (let* ((font-type (aref ps-mule-font-number-to-type font-number))
	 (font-spec-alist (char-table-extra-slot
			   (aref ps-mule-font-spec-tables font-type) 0)))
    (ps-output-prologue
     (list (if (ps-mule-font-spec-src (cdr (car font-spec-alist)))
	       ;; We ignore a font specified in ps-font-info-database.
	       (format "/V%s VTOP%d def\n" fonttag font-type)
	     (format "/V%s [ VTOP%d aload pop ] def\n
V%s 0 /%s-latin1 /%s Latin1Encoding put\n"
		     fonttag font-type fonttag ps-font ps-font))
	   (format "/%s ETOP%d V%s %f ReDefFont\n"
		   fonttag font-type fonttag size)))))


;;;###autoload
(defun ps-mule-end-job ()
  "Finish printing job for multi-byte chars."

  ;; Prepare root and sub fonts while generating glyphs if necessary.
  (let ((output-head (list t))
	(ps-mule-output-list (list t)))
    (dotimes (i 4)
      (map-char-table 'ps-mule-prepare-glyph
		      (aref ps-mule-font-spec-tables i)))
    (ps-mule-restruct-output-list (cdr ps-mule-output-list) output-head)
    (ps-output-prologue (cdr output-head)))

  ;; Prepare top Encoding and templates of FDepVector.
  (dotimes (i 4)
    (let ((font-spec-alist (char-table-extra-slot
			    (aref ps-mule-font-spec-tables i) 0))
	  font-list font-spec)
      (dolist (elt font-spec-alist)
	(setq font-spec (cdr elt))
	(if (ps-mule-font-spec-extra font-spec)
	    (push (cons (ps-mule-font-spec-font-id font-spec)
			(ps-mule-font-spec-id font-spec))
		  font-list)))
      (setq font-list (nreverse font-list))
      (ps-output-prologue
       (list (format "/ETOP%d 256 array def\n" i)
	     (format "0 1 255 { ETOP%d exch 0 put } for\n" i)))
      (let ((index 0))
	(dolist (font font-list)
	  (ps-output-prologue (format "ETOP%d %d %d put\n" i (car font) index))
	  (setq index (1+ index))))
      (ps-output-prologue (format "/VTOP%d [%s] def\n" i
				  (mapconcat #'(lambda (x)
						 (format "F%02X" (cdr x)))
					     font-list " ")))))

  ;; Redefine fonts f0, f1, f2, f3, h0, h1, H0.
  (ps-mule-redefine-font 4 "h0" ps-header-title-font-size-internal
			 (ps-font 'ps-font-for-header 'bold))
  (ps-mule-redefine-font 5 "h1" ps-header-font-size-internal
			 (ps-font 'ps-font-for-header 'normal))
  (ps-mule-redefine-font 6 "H0" ps-footer-font-size-internal
			 (ps-font 'ps-font-for-footer 'normal))
  (let ((font (ps-font-alist 'ps-font-for-text))
	(i 0))
    (while font
      (ps-mule-redefine-font i (format "f%d" i)
			     ps-font-size-internal
			     (ps-font 'ps-font-for-text (car (car font))))
      (setq font (cdr font)
	    i (1+ i)))))

(provide 'ps-mule)

;; Local Variables:
;; generated-autoload-file: "ps-print.el"
;; End:

;;; ps-mule.el ends here
