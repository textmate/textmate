;;; viet-util.el --- utilities for Vietnamese  -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021
;; Copyright (C) 2003
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: mule, multilingual, Vietnamese

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

;; Vietnamese uses ASCII characters and additional 134 unique
;; characters (these are Latin alphabets with various diacritical and
;; tone marks).  As far as I know, Vietnamese now has 5 different ways
;; for representing these characters: VISCII, TCVN-5712, VPS, VIQR,
;; and Unicode.  VISCII, TCVN-5712 and VPS are simple 1-byte code
;; which assigns 134 unique characters in control-code area
;; (0x00..0x1F) and right half area (0x80..0xFF).  VIQR is a mnemonic
;; encoding specification representing diacritical marks by following
;; ASCII characters.

;;; Code:

(defvar viet-viscii-nonascii-translation-table)

;;;###autoload
(defun viet-encode-viscii-char (char)
  "Return VISCII character code of CHAR if appropriate."
  (encode-char char 'viscii))

;; VIQR is a mnemonic encoding specification for Vietnamese.
;; It represents diacritical marks by ASCII characters as follows:
;; ------------+----------+--------
;;     mark    | mnemonic | example
;; ------------+----------+---------
;;    breve    |    (     | a( -> ,1e(B
;;  circumflex |    ^     | a^ -> ,1b(B
;;    horn     |    +     | o+ -> ,1=(B
;; ------------+----------+---------
;;    acute    |    '     | a' -> ,1a(B
;;    grave    |    `     | a` -> ,1`(B
;;  hook above |    ?     | a? -> ,1d(B
;;    tilde    |    ~     | a~ -> ,1c(B
;;   dot below |    .     | a. -> ,1U(B
;; ------------+----------+---------
;;    d bar    |   dd     | dd -> ,1p(B
;; ------------+----------+---------

(defvar viet-viqr-alist
  '(;; lowercase
    (?,1!(B . "a('")			; 161
    (?,1"(B . "a(`")			; 162
    (?,1#(B . "a(.")			; 163
    (?,1$(B . "a^'")			; 164
    (?,1%(B . "a^`")			; 165
    (?,1&(B . "a^?")			; 166
    (?,1'(B . "a^.")			; 167
    (?,1((B . "e~")				; 168
    (?,1)(B . "e.")				; 169
    (?,1*(B . "e^'")			; 170
    (?,1+(B . "e^`")			; 171
    (?,1,(B . "e^?")			; 172
    (?,1-(B . "e^~")			; 173
    (?,1.(B . "e^.")			; 174
    (?,1/(B . "o^'")			; 175
    (?,10(B . "o^`")			; 176
    (?,11(B . "o^?")			; 177
    (?,12(B . "o^~")			; 178
    (?,15(B . "o^.")			; 181
    (?,16(B . "o+`")			; 182
    (?,17(B . "o+?")			; 183
    (?,18(B . "i.")				; 184
    (?,1=(B . "o+")				; 189
    (?,1>(B . "o+'")			; 190
    (?,1F(B . "a(?")			; 198
    (?,1G(B . "a(~")			; 199
    (?,1O(B . "y`")				; 207
    (?,1Q(B . "u+'")			; 209
    (?,1U(B . "a.")				; 213
    (?,1V(B . "y?")				; 214
    (?,1W(B . "u+`")			; 215
    (?,1X(B . "u+?")			; 216
    (?,1[(B . "y~")				; 219
    (?,1\(B . "y.")				; 220
    (?,1^(B . "o+~")			; 222
    (?,1_(B . "u+")				; 223
    (?,1`(B . "a`")				; 224
    (?,1a(B . "a'")				; 225
    (?,1b(B . "a^")				; 226
    (?,1c(B . "a~")				; 227
    (?,1d(B . "a?")				; 228
    (?,1e(B . "a(")				; 229
    (?,1f(B . "u+~")			; 230
    (?,1g(B . "a^~")			; 231
    (?,1h(B . "e`")				; 232
    (?,1i(B . "e'")				; 233
    (?,1j(B . "e^")				; 234
    (?,1k(B . "e?")				; 235
    (?,1l(B . "i`")				; 236
    (?,1m(B . "i'")				; 237
    (?,1n(B . "i~")				; 238
    (?,1o(B . "i?")				; 239
    (?,1p(B . "dd")				; 240
    (?,1q(B . "u+.")			; 241
    (?,1r(B . "o`")				; 242
    (?,1s(B . "o'")				; 243
    (?,1t(B . "o^")				; 244
    (?,1u(B . "o~")				; 245
    (?,1v(B . "o?")				; 246
    (?,1w(B . "o.")				; 247
    (?,1x(B . "u.")				; 248
    (?,1y(B . "u`")				; 249
    (?,1z(B . "u'")				; 250
    (?,1{(B . "u~")				; 251
    (?,1|(B . "u?")				; 252
    (?,1}(B . "y'")				; 253
    (?,1~(B . "o+.")			; 254

    ;; upper case
    (?,2!(B . "A('")			; 161
    (?,2"(B . "A(`")			; 162
    (?,2#(B . "A(.")			; 163
    (?,2$(B . "A^'")			; 164
    (?,2%(B . "A^`")			; 165
    (?,2&(B . "A^?")			; 166
    (?,2'(B . "A^.")			; 167
    (?,2((B . "E~")				; 168
    (?,2)(B . "E.")				; 169
    (?,2*(B . "E^'")			; 170
    (?,2+(B . "E^`")			; 171
    (?,2,(B . "E^?")			; 172
    (?,2-(B . "E^~")			; 173
    (?,2.(B . "E^.")			; 174
    (?,2/(B . "O^'")			; 175
    (?,20(B . "O^`")			; 176
    (?,21(B . "O^?")			; 177
    (?,22(B . "O^~")			; 178
    (?,25(B . "O^.")			; 181
    (?,26(B . "O+`")			; 182
    (?,27(B . "O+?")			; 183
    (?,28(B . "I.")				; 184
    (?,2=(B . "O+")				; 189
    (?,2>(B . "O+'")			; 190
    (?,2F(B . "A(?")			; 198
    (?,2G(B . "A(~")			; 199
    (?,2O(B . "Y`")				; 207
    (?,2Q(B . "U+'")			; 209
    (?,2U(B . "A.")				; 213
    (?,2V(B . "Y?")				; 214
    (?,2W(B . "U+`")			; 215
    (?,2X(B . "U+?")			; 216
    (?,2[(B . "Y~")				; 219
    (?,2\(B . "Y.")				; 220
    (?,2^(B . "O+~")			; 222
    (?,2_(B . "U+")				; 223
    (?,2`(B . "A`")				; 224
    (?,2a(B . "A'")				; 225
    (?,2b(B . "A^")				; 226
    (?,2c(B . "A~")				; 227
    (?,2d(B . "A?")				; 228
    (?,2e(B . "A(")				; 229
    (?,2f(B . "U+~")			; 230
    (?,2g(B . "A^~")			; 231
    (?,2h(B . "E`")				; 232
    (?,2i(B . "E'")				; 233
    (?,2j(B . "E^")				; 234
    (?,2k(B . "E?")				; 235
    (?,2l(B . "I`")				; 236
    (?,2m(B . "I'")				; 237
    (?,2n(B . "I~")				; 238
    (?,2o(B . "I?")				; 239
    (?,2p(B . "DD")				; 240
    (?,2p(B . "dD")				; 240
    (?,2p(B . "Dd")				; 240
    (?,2q(B . "U+.")			; 241
    (?,2r(B . "O`")				; 242
    (?,2s(B . "O'")				; 243
    (?,2t(B . "O^")				; 244
    (?,2u(B . "O~")				; 245
    (?,2v(B . "O?")				; 246
    (?,2w(B . "O.")				; 247
    (?,2x(B . "U.")				; 248
    (?,2y(B . "U`")				; 249
    (?,2z(B . "U'")				; 250
    (?,2{(B . "U~")				; 251
    (?,2|(B . "U?")				; 252
    (?,2}(B . "Y'")				; 253
    (?,2~(B . "O+.")			; 254

    ;; escape from composition
    (?\( . "\\(")			; breve (left parenthesis)
    (?^ . "\\^")			; circumflex (caret)
    (?+ . "\\+")			; horn (plus sign)
    (?' . "\\'")			; acute (apostrophe)
    (?` . "\\`")			; grave (backquote)
    (?? . "\\?")			; hook above (question mark)
    (?~ . "\\~")			; tilde (tilde)
    (?. . "\\.")			; dot below (period)
    (?d . "\\d")			; d-bar (d)
    (?\\ . "\\\\")			; literal backslash
    )
  "Alist of Vietnamese characters vs corresponding `VIQR' string.")

;; Regular expression matching single Vietnamese character represented
;; by VIQR.
(defconst viqr-regexp
  "[aeiouyAEIOUY]\\([(^+]?['`?~.]\\|[(^+]\\)\\|[Dd][Dd]")

;;;###autoload
(defun viet-decode-viqr-region (from to)
  "Convert `VIQR' mnemonics of the current region to Vietnamese characters.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch of the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward viqr-regexp nil t)
      (let* ((viqr (buffer-substring (match-beginning 0) (match-end 0)))
	     (ch (car (rassoc viqr viet-viqr-alist))))
	(if ch
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert ch)))))))

;;;###autoload
(defun viet-decode-viqr-buffer ()
  "Convert `VIQR' mnemonics of the current buffer to Vietnamese characters."
  (interactive)
  (viet-decode-viqr-region (point-min) (point-max)))

;;;###autoload
(defun viet-encode-viqr-region (from to)
  "Convert Vietnamese characters of the current region to `VIQR' mnemonics.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch of the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\cv" nil t)
      (let* ((ch (preceding-char))
	     (viqr (cdr (assq ch viet-viqr-alist))))
	(if viqr
	    (progn
	      (delete-char -1)
	      (insert viqr)))))))

;;;###autoload
(defun viet-encode-viqr-buffer ()
  "Convert Vietnamese characters of the current buffer to `VIQR' mnemonics."
  (interactive)
  (viet-encode-viqr-region (point-min) (point-max)))

;;;###autoload
(defun viqr-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (+ (point) len))
      (let ((buffer-modified-p (buffer-modified-p)))
	(viet-decode-viqr-region (point-min) (point-max))
	(set-buffer-modified-p buffer-modified-p)
	(- (point-max) (point-min))))))

;;;###autoload
(defun viqr-pre-write-conversion (from to)
  (let ((old-buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring old-buf from to))
    (viet-encode-viqr-region (point-min) (point-max))
    ;; Should return nil as annotations.
    nil))

;;;
(provide 'viet-util)

;;; viet-util.el ends here
