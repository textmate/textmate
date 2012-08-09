;;; iso-ascii.el --- set up char tables for ISO 8859/1 on ASCII terminals

;; Copyright (C) 1987, 1995, 1998, 2001-2012 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
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

;; Written by Howard Gayle.  See display-table.el for details.

;; This code sets up to display ISO 8859/1 characters on plain
;; ASCII terminals.  The display strings for the characters are
;; more-or-less based on TeX.

;;; Code:

(require 'disp-table)
(eval-when-compile (require 'cl))

(defgroup iso-ascii nil
  "Set up char tables for ISO 8859/1 on ASCII terminals."
  :prefix "iso-ascii-"
  :group 'i18n)

(defcustom iso-ascii-convenient nil
  "Non-nil means `iso-ascii' should aim for convenience, not precision."
  :type 'boolean
  :group 'iso-ascii)

(defvar iso-ascii-display-table (make-display-table)
  "Display table used for ISO-ASCII mode.")

(defvar iso-ascii-standard-display-table nil
  "Display table used when not in ISO-ASCII mode.")
;; Don't alter iso-ascii-standard-display-table if this file is loaded again,
;; or even by using C-M-x on any of the expressions.
(unless iso-ascii-standard-display-table
  (setq iso-ascii-standard-display-table
	standard-display-table))

(defun iso-ascii-display (code string &optional convenient-string)
  (if iso-ascii-convenient
      (setq string (or convenient-string string))
    (setq string (concat "{" string "}")))
  ;; unibyte
  (aset iso-ascii-display-table code string)
  ;; multibyte
  (aset iso-ascii-display-table (make-char 'latin-iso8859-1 (- code 128))
	string))

(iso-ascii-display 160 "_" " ")   ; NBSP (no-break space)
(iso-ascii-display 161 "!")   ; inverted exclamation mark
(iso-ascii-display 162 "c")   ; cent sign
(iso-ascii-display 163 "GBP") ; pound sign
(iso-ascii-display 164 "$")   ; general currency sign
(iso-ascii-display 165 "JPY") ; yen sign
(iso-ascii-display 166 "|")   ; broken vertical line
(iso-ascii-display 167 "S" "(S)")   ; section sign
(iso-ascii-display 168 "\"")  ; diaeresis
(iso-ascii-display 169 "C" "(C)")   ; copyright sign
(iso-ascii-display 170 "_a")  ; ordinal indicator, feminine
(iso-ascii-display 171 "<<")  ; left angle quotation mark
(iso-ascii-display 172 "~")   ; not sign
(iso-ascii-display 173 "-")   ; soft hyphen
(iso-ascii-display 174 "R" "(R)")   ; registered sign
(iso-ascii-display 175 "=")   ; macron
(iso-ascii-display 176 "o")   ; degree sign
(iso-ascii-display 177 "+-")  ; plus or minus sign
(iso-ascii-display 178 "2")   ; superscript two
(iso-ascii-display 179 "3")   ; superscript three
(iso-ascii-display 180 "'")   ; acute accent
(iso-ascii-display 181 "u")   ; micro sign
(iso-ascii-display 182 "P" "(P)")   ; pilcrow
(iso-ascii-display 183 ".")   ; middle dot
(iso-ascii-display 184 ",")   ; cedilla
(iso-ascii-display 185 "1")   ; superscript one
(iso-ascii-display 186 "_o")  ; ordinal indicator, masculine
(iso-ascii-display 187 ">>")  ; right angle quotation mark
(iso-ascii-display 188 "1/4") ; fraction one-quarter
(iso-ascii-display 189 "1/2") ; fraction one-half
(iso-ascii-display 190 "3/4") ; fraction three-quarters
(iso-ascii-display 191 "?")   ; inverted question mark
(iso-ascii-display 192 "`A")  ; A with grave accent
(iso-ascii-display 193 "'A")  ; A with acute accent
(iso-ascii-display 194 "^A")  ; A with circumflex accent
(iso-ascii-display 195 "~A")  ; A with tilde
(iso-ascii-display 196 "\"A") ; A with diaeresis or umlaut mark
(iso-ascii-display 197 "AA")  ; A with ring
(iso-ascii-display 198 "AE")  ; AE diphthong
(iso-ascii-display 199 ",C")  ; C with cedilla
(iso-ascii-display 200 "`E")  ; E with grave accent
(iso-ascii-display 201 "'E")  ; E with acute accent
(iso-ascii-display 202 "^E")  ; E with circumflex accent
(iso-ascii-display 203 "\"E") ; E with diaeresis or umlaut mark
(iso-ascii-display 204 "`I")  ; I with grave accent
(iso-ascii-display 205 "'I")  ; I with acute accent
(iso-ascii-display 206 "^I")  ; I with circumflex accent
(iso-ascii-display 207 "\"I") ; I with diaeresis or umlaut mark
(iso-ascii-display 208 "-D")  ; D with stroke, Icelandic eth
(iso-ascii-display 209 "~N")  ; N with tilde
(iso-ascii-display 210 "`O")  ; O with grave accent
(iso-ascii-display 211 "'O")  ; O with acute accent
(iso-ascii-display 212 "^O")  ; O with circumflex accent
(iso-ascii-display 213 "~O")  ; O with tilde
(iso-ascii-display 214 "\"O") ; O with diaeresis or umlaut mark
(iso-ascii-display 215 "x")   ; multiplication sign
(iso-ascii-display 216 "/O")  ; O with slash
(iso-ascii-display 217 "`U")  ; U with grave accent
(iso-ascii-display 218 "'U")  ; U with acute accent
(iso-ascii-display 219 "^U")  ; U with circumflex accent
(iso-ascii-display 220 "\"U") ; U with diaeresis or umlaut mark
(iso-ascii-display 221 "'Y")  ; Y with acute accent
(iso-ascii-display 222 "TH")  ; capital thorn, Icelandic
(iso-ascii-display 223 "ss")  ; small sharp s, German
(iso-ascii-display 224 "`a")  ; a with grave accent
(iso-ascii-display 225 "'a")  ; a with acute accent
(iso-ascii-display 226 "^a")  ; a with circumflex accent
(iso-ascii-display 227 "~a")  ; a with tilde
(iso-ascii-display 228 "\"a") ; a with diaeresis or umlaut mark
(iso-ascii-display 229 "aa")  ; a with ring
(iso-ascii-display 230 "ae")  ; ae diphthong
(iso-ascii-display 231 ",c")  ; c with cedilla
(iso-ascii-display 232 "`e")  ; e with grave accent
(iso-ascii-display 233 "'e")  ; e with acute accent
(iso-ascii-display 234 "^e")  ; e with circumflex accent
(iso-ascii-display 235 "\"e") ; e with diaeresis or umlaut mark
(iso-ascii-display 236 "`i")  ; i with grave accent
(iso-ascii-display 237 "'i")  ; i with acute accent
(iso-ascii-display 238 "^i")  ; i with circumflex accent
(iso-ascii-display 239 "\"i") ; i with diaeresis or umlaut mark
(iso-ascii-display 240 "-d")  ; d with stroke, Icelandic eth
(iso-ascii-display 241 "~n")  ; n with tilde
(iso-ascii-display 242 "`o")  ; o with grave accent
(iso-ascii-display 243 "'o")  ; o with acute accent
(iso-ascii-display 244 "^o")  ; o with circumflex accent
(iso-ascii-display 245 "~o")  ; o with tilde
(iso-ascii-display 246 "\"o") ; o with diaeresis or umlaut mark
(iso-ascii-display 247 "/")   ; division sign
(iso-ascii-display 248 "/o")  ; o with slash
(iso-ascii-display 249 "`u")  ; u with grave accent
(iso-ascii-display 250 "'u")  ; u with acute accent
(iso-ascii-display 251 "^u")  ; u with circumflex accent
(iso-ascii-display 252 "\"u") ; u with diaeresis or umlaut mark
(iso-ascii-display 253 "'y")  ; y with acute accent
(iso-ascii-display 254 "th")  ; small thorn, Icelandic
(iso-ascii-display 255 "\"y") ; small y with diaeresis or umlaut mark

(define-minor-mode iso-ascii-mode
  "Toggle ISO-ASCII mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :variable (eq standard-display-table iso-ascii-display-table)
  (unless standard-display-table
    (setq standard-display-table iso-ascii-standard-display-table)))

(provide 'iso-ascii)

;;; iso-ascii.el ends here
