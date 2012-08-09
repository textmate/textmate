;;; nxml-uchnm.el --- support for Unicode standard cha names in nxml-mode

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML

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

;; This enables the use of the character names defined in the Unicode
;; Standard.  The use of the names can be controlled on a per-block
;; basis, so as both to reduce memory usage and loading time,
;; and to make completion work better.

;;; Code:

(require 'nxml-mode)

(defconst nxml-unicode-blocks
  '(("Basic Latin" #x0000 #x007F)
    ("Latin-1 Supplement" #x0080 #x00FF)
    ("Latin Extended-A" #x0100 #x017F)
    ("Latin Extended-B" #x0180 #x024F)
    ("IPA Extensions" #x0250 #x02AF)
    ("Spacing Modifier Letters" #x02B0 #x02FF)
    ("Combining Diacritical Marks" #x0300 #x036F)
    ("Greek and Coptic" #x0370 #x03FF)
    ("Cyrillic" #x0400 #x04FF)
    ("Cyrillic Supplementary" #x0500 #x052F)
    ("Armenian" #x0530 #x058F)
    ("Hebrew" #x0590 #x05FF)
    ("Arabic" #x0600 #x06FF)
    ("Syriac" #x0700 #x074F)
    ("Thaana" #x0780 #x07BF)
    ("Devanagari" #x0900 #x097F)
    ("Bengali" #x0980 #x09FF)
    ("Gurmukhi" #x0A00 #x0A7F)
    ("Gujarati" #x0A80 #x0AFF)
    ("Oriya" #x0B00 #x0B7F)
    ("Tamil" #x0B80 #x0BFF)
    ("Telugu" #x0C00 #x0C7F)
    ("Kannada" #x0C80 #x0CFF)
    ("Malayalam" #x0D00 #x0D7F)
    ("Sinhala" #x0D80 #x0DFF)
    ("Thai" #x0E00 #x0E7F)
    ("Lao" #x0E80 #x0EFF)
    ("Tibetan" #x0F00 #x0FFF)
    ("Myanmar" #x1000 #x109F)
    ("Georgian" #x10A0 #x10FF)
    ("Hangul Jamo" #x1100 #x11FF)
    ("Ethiopic" #x1200 #x137F)
    ("Cherokee" #x13A0 #x13FF)
    ("Unified Canadian Aboriginal Syllabics" #x1400 #x167F)
    ("Ogham" #x1680 #x169F)
    ("Runic" #x16A0 #x16FF)
    ("Tagalog" #x1700 #x171F)
    ("Hanunoo" #x1720 #x173F)
    ("Buhid" #x1740 #x175F)
    ("Tagbanwa" #x1760 #x177F)
    ("Khmer" #x1780 #x17FF)
    ("Mongolian" #x1800 #x18AF)
    ("Latin Extended Additional" #x1E00 #x1EFF)
    ("Greek Extended" #x1F00 #x1FFF)
    ("General Punctuation" #x2000 #x206F)
    ("Superscripts and Subscripts" #x2070 #x209F)
    ("Currency Symbols" #x20A0 #x20CF)
    ("Combining Diacritical Marks for Symbols" #x20D0 #x20FF)
    ("Letterlike Symbols" #x2100 #x214F)
    ("Number Forms" #x2150 #x218F)
    ("Arrows" #x2190 #x21FF)
    ("Mathematical Operators" #x2200 #x22FF)
    ("Miscellaneous Technical" #x2300 #x23FF)
    ("Control Pictures" #x2400 #x243F)
    ("Optical Character Recognition" #x2440 #x245F)
    ("Enclosed Alphanumerics" #x2460 #x24FF)
    ("Box Drawing" #x2500 #x257F)
    ("Block Elements" #x2580 #x259F)
    ("Geometric Shapes" #x25A0 #x25FF)
    ("Miscellaneous Symbols" #x2600 #x26FF)
    ("Dingbats" #x2700 #x27BF)
    ("Miscellaneous Mathematical Symbols-A" #x27C0 #x27EF)
    ("Supplemental Arrows-A" #x27F0 #x27FF)
    ("Braille Patterns" #x2800 #x28FF)
    ("Supplemental Arrows-B" #x2900 #x297F)
    ("Miscellaneous Mathematical Symbols-B" #x2980 #x29FF)
    ("Supplemental Mathematical Operators" #x2A00 #x2AFF)
    ("CJK Radicals Supplement" #x2E80 #x2EFF)
    ("Kangxi Radicals" #x2F00 #x2FDF)
    ("Ideographic Description Characters" #x2FF0 #x2FFF)
    ("CJK Symbols and Punctuation" #x3000 #x303F)
    ("Hiragana" #x3040 #x309F)
    ("Katakana" #x30A0 #x30FF)
    ("Bopomofo" #x3100 #x312F)
    ("Hangul Compatibility Jamo" #x3130 #x318F)
    ("Kanbun" #x3190 #x319F)
    ("Bopomofo Extended" #x31A0 #x31BF)
    ("Katakana Phonetic Extensions" #x31F0 #x31FF)
    ("Enclosed CJK Letters and Months" #x3200 #x32FF)
    ("CJK Compatibility" #x3300 #x33FF)
    ("CJK Unified Ideographs Extension A" #x3400 #x4DBF)
    ;;("CJK Unified Ideographs" #x4E00 #x9FFF)
    ("Yi Syllables" #xA000 #xA48F)
    ("Yi Radicals" #xA490 #xA4CF)
    ;;("Hangul Syllables" #xAC00 #xD7AF)
    ;;("High Surrogates" #xD800 #xDB7F)
    ;;("High Private Use Surrogates" #xDB80 #xDBFF)
    ;;("Low Surrogates" #xDC00 #xDFFF)
    ;;("Private Use Area" #xE000 #xF8FF)
    ;;("CJK Compatibility Ideographs" #xF900 #xFAFF)
    ("Alphabetic Presentation Forms" #xFB00 #xFB4F)
    ("Arabic Presentation Forms-A" #xFB50 #xFDFF)
    ("Variation Selectors" #xFE00 #xFE0F)
    ("Combining Half Marks" #xFE20 #xFE2F)
    ("CJK Compatibility Forms" #xFE30 #xFE4F)
    ("Small Form Variants" #xFE50 #xFE6F)
    ("Arabic Presentation Forms-B" #xFE70 #xFEFF)
    ("Halfwidth and Fullwidth Forms" #xFF00 #xFFEF)
    ("Specials" #xFFF0 #xFFFF)
    ("Old Italic" #x10300 #x1032F)
    ("Gothic" #x10330 #x1034F)
    ("Deseret" #x10400 #x1044F)
    ("Byzantine Musical Symbols" #x1D000 #x1D0FF)
    ("Musical Symbols" #x1D100 #x1D1FF)
    ("Mathematical Alphanumeric Symbols" #x1D400 #x1D7FF)
    ;;("CJK Unified Ideographs Extension B" #x20000 #x2A6DF)
    ;;("CJK Compatibility Ideographs Supplement" #x2F800 #x2FA1F)
    ("Tags" #xE0000 #xE007F)
    ;;("Supplementary Private Use Area-A" #xF0000 #xFFFFF)
    ;;("Supplementary Private Use Area-B" #x100000 #x10FFFF)
    )
  "List of Unicode blocks.
For each block there is a list (NAME FIRST LAST), where
NAME is a string giving the official name of the block,
FIRST is the first code-point and LAST is the last code-point.
Blocks containing only characters with algorithmic names or no names
are omitted.")

(defun nxml-unicode-block-char-name-set (name)
  "Return a symbol for a block whose official Unicode name is NAME.
The symbol is generated by downcasing and replacing each space
by a hyphen."
  (intern (replace-regexp-in-string " " "-" (downcase name))))

;; This is intended to be a superset of the coverage
;; of existing standard entity sets.
(defvar nxml-enabled-unicode-blocks-default
  '(basic-latin
    latin-1-supplement
    latin-extended-a
    latin-extended-b
    ipa-extensions
    spacing-modifier-letters
    combining-diacritical-marks
    greek-and-coptic
    cyrillic
    general-punctuation
    superscripts-and-subscripts
    currency-symbols
    combining-diacritical-marks-for-symbols
    letterlike-symbols
    number-forms
    arrows
    mathematical-operators
    miscellaneous-technical
    control-pictures
    optical-character-recognition
    enclosed-alphanumerics
    box-drawing
    block-elements
    geometric-shapes
    miscellaneous-symbols
    dingbats
    miscellaneous-mathematical-symbols-a
    supplemental-arrows-a
    supplemental-arrows-b
    miscellaneous-mathematical-symbols-b
    supplemental-mathematical-operators
    cjk-symbols-and-punctuation
    alphabetic-presentation-forms
    variation-selectors
    small-form-variants
    specials
    mathematical-alphanumeric-symbols)
  "Default value for `nxml-enabled-unicode-blocks'.")

(mapc (lambda (block)
        (nxml-autoload-char-name-set
         (nxml-unicode-block-char-name-set (car block))
         (expand-file-name
          (format "nxml/%05X-%05X"
                  (nth 1 block)
                  (nth 2 block))
          data-directory)))
      nxml-unicode-blocks)

;; Internal flag to control whether customize reloads the character tables.
;; Should be set the first time the
(defvar nxml-internal-unicode-char-name-sets-enabled nil)

(defcustom nxml-enabled-unicode-blocks nxml-enabled-unicode-blocks-default
  "List of Unicode blocks for which Unicode character names are enabled.
Each block is identified by a symbol derived from the name
of the block by downcasing and replacing each space by a hyphen."
  :group 'nxml
  :set (lambda (sym value)
	 (set-default 'nxml-enabled-unicode-blocks value)
	 (when nxml-internal-unicode-char-name-sets-enabled
	   (nxml-enable-unicode-char-name-sets)))
  :type (cons 'set
	      (mapcar (lambda (block)
			`(const :tag ,(format "%s (%04X-%04X)"
					      (nth 0 block)
					      (nth 1 block)
					      (nth 2 block))
				,(nxml-unicode-block-char-name-set
				  (nth 0 block))))
		      nxml-unicode-blocks)))

;;;###autoload
(defun nxml-enable-unicode-char-name-sets ()
  "Enable the use of Unicode standard names for characters.
The Unicode blocks for which names are enabled is controlled by
the variable `nxml-enabled-unicode-blocks'."
  (interactive)
  (setq nxml-internal-unicode-char-name-sets-enabled t)
  (mapc (lambda (block)
          (nxml-disable-char-name-set
           (nxml-unicode-block-char-name-set (car block))))
        nxml-unicode-blocks)
  (mapc (lambda (nameset)
          (nxml-enable-char-name-set nameset))
        nxml-enabled-unicode-blocks))

(provide 'nxml-uchnm)

;;; nxml-uchnm.el ends here
