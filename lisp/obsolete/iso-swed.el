;;; iso-swed.el --- set up char tables for ISO 8859/1 for Swedish/Finnish ttys

;; Copyright (C) 1987, 2001-2012 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n
;; Obsolete-since: 22.1

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

;; Written by Howard Gayle.  See case-table.el for details.

;;; Code:

;; This code sets up to display ISO 8859/1 characters on
;; terminals that have ASCII in the G0 set and a Swedish/Finnish
;; version of ISO 646 in the G1 set.  The G1 set differs from
;; ASCII as follows:
;;
;; ASCII G1
;;     $ general currency sign
;;     @ capital E with acute accent
;;     [ capital A with diaeresis or umlaut mark
;;     \ capital O with diaeresis or umlaut mark
;;     ] capital A with ring
;;     ^ capital U with diaeresis or umlaut mark
;;     ` small e with acute accent
;;     { small a with diaeresis or umlaut mark
;;     | small o with diaeresis or umlaut mark
;;     } small a with ring
;;     ~ small u with diaeresis or umlaut mark

(require 'disp-table)

(standard-display-ascii 160 "{_}")   ; NBSP (no-break space)
(standard-display-ascii 161 "{!}")   ; inverted exclamation mark
(standard-display-ascii 162 "{c}")   ; cent sign
(standard-display-ascii 163 "{GBP}") ; pound sign
(standard-display-g1    164 ?$)      ; general currency sign
(standard-display-ascii 165 "{JPY}") ; yen sign
(standard-display-ascii 166 "{|}")   ; broken vertical line
(standard-display-ascii 167 "{S}")   ; section sign
(standard-display-ascii 168 "{\"}")  ; diaeresis
(standard-display-ascii 169 "{C}")   ; copyright sign
(standard-display-ascii 170 "{_a}")  ; ordinal indicator, feminine
(standard-display-ascii 171 "{<<}")  ; left angle quotation mark
(standard-display-ascii 172 "{~}")   ; not sign
(standard-display-ascii 173 "{-}")   ; soft hyphen
(standard-display-ascii 174 "{R}")   ; registered sign
(standard-display-ascii 175 "{=}")   ; macron
(standard-display-ascii 176 "{o}")   ; degree sign
(standard-display-ascii 177 "{+-}")  ; plus or minus sign
(standard-display-ascii 178 "{2}")   ; superscript two
(standard-display-ascii 179 "{3}")   ; superscript three
(standard-display-ascii 180 "{'}")   ; acute accent
(standard-display-ascii 181 "{u}")   ; micro sign
(standard-display-ascii 182 "{P}")   ; pilcrow
(standard-display-ascii 183 "{.}")   ; middle dot
(standard-display-ascii 184 "{,}")   ; cedilla
(standard-display-ascii 185 "{1}")   ; superscript one
(standard-display-ascii 186 "{_o}")  ; ordinal indicator, masculine
(standard-display-ascii 187 "{>>}")  ; right angle quotation mark
(standard-display-ascii 188 "{1/4}") ; fraction one-quarter
(standard-display-ascii 189 "{1/2}") ; fraction one-half
(standard-display-ascii 190 "{3/4}") ; fraction three-quarters
(standard-display-ascii 191 "{?}")   ; inverted question mark
(standard-display-ascii 192 "{`A}")  ; A with grave accent
(standard-display-ascii 193 "{'A}")  ; A with acute accent
(standard-display-ascii 194 "{^A}")  ; A with circumflex accent
(standard-display-ascii 195 "{~A}")  ; A with tilde
(standard-display-g1    196 ?[)      ; A with diaeresis or umlaut mark
(standard-display-g1    197 ?])      ; A with ring
(standard-display-ascii 198 "{AE}")  ; AE diphthong
(standard-display-ascii 199 "{,C}")  ; C with cedilla
(standard-display-ascii 200 "{`E}")  ; E with grave accent
(standard-display-g1    201 ?@)      ; E with acute accent
(standard-display-ascii 202 "{^E}")  ; E with circumflex accent
(standard-display-ascii 203 "{\"E}") ; E with diaeresis or umlaut mark
(standard-display-ascii 204 "{`I}")  ; I with grave accent
(standard-display-ascii 205 "{'I}")  ; I with acute accent
(standard-display-ascii 206 "{^I}")  ; I with circumflex accent
(standard-display-ascii 207 "{\"I}") ; I with diaeresis or umlaut mark
(standard-display-ascii 208 "{-D}")  ; D with stroke, Icelandic eth
(standard-display-ascii 209 "{~N}")  ; N with tilde
(standard-display-ascii 210 "{`O}")  ; O with grave accent
(standard-display-ascii 211 "{'O}")  ; O with acute accent
(standard-display-ascii 212 "{^O}")  ; O with circumflex accent
(standard-display-ascii 213 "{~O}")  ; O with tilde
(standard-display-g1    214 ?\\)     ; O with diaeresis or umlaut mark
(standard-display-ascii 215 "{x}")   ; multiplication sign
(standard-display-ascii 216 "{/O}")  ; O with slash
(standard-display-ascii 217 "{`U}")  ; U with grave accent
(standard-display-ascii 218 "{'U}")  ; U with acute accent
(standard-display-ascii 219 "{^U}")  ; U with circumflex accent
(standard-display-g1    220 ?^)      ; U with diaeresis or umlaut mark
(standard-display-ascii 221 "{'Y}")  ; Y with acute accent
(standard-display-ascii 222 "{TH}")  ; capital thorn, Icelandic
(standard-display-ascii 223 "{ss}")  ; small sharp s, German
(standard-display-ascii 224 "{`a}")  ; a with grave accent
(standard-display-ascii 225 "{'a}")  ; a with acute accent
(standard-display-ascii 226 "{^a}")  ; a with circumflex accent
(standard-display-ascii 227 "{~a}")  ; a with tilde
(standard-display-g1    228 ?{)      ; a with diaeresis or umlaut mark
(standard-display-g1    229 ?})      ; a with ring
(standard-display-ascii 230 "{ae}")  ; ae diphthong
(standard-display-ascii 231 "{,c}")  ; c with cedilla
(standard-display-ascii 232 "{`e}")  ; e with grave accent
(standard-display-g1    233 ?`)      ; e with acute accent
(standard-display-ascii 234 "{^e}")  ; e with circumflex accent
(standard-display-ascii 235 "{\"e}") ; e with diaeresis or umlaut mark
(standard-display-ascii 236 "{`i}")  ; i with grave accent
(standard-display-ascii 237 "{'i}")  ; i with acute accent
(standard-display-ascii 238 "{^i}")  ; i with circumflex accent
(standard-display-ascii 239 "{\"i}") ; i with diaeresis or umlaut mark
(standard-display-ascii 240 "{-d}")  ; d with stroke, Icelandic eth
(standard-display-ascii 241 "{~n}")  ; n with tilde
(standard-display-ascii 242 "{`o}")  ; o with grave accent
(standard-display-ascii 243 "{'o}")  ; o with acute accent
(standard-display-ascii 244 "{^o}")  ; o with circumflex accent
(standard-display-ascii 245 "{~o}")  ; o with tilde
(standard-display-g1    246 ?|)      ; o with diaeresis or umlaut mark
(standard-display-ascii 247 "{/}")   ; division sign
(standard-display-ascii 248 "{/o}")  ; o with slash
(standard-display-ascii 249 "{`u}")  ; u with grave accent
(standard-display-ascii 250 "{'u}")  ; u with acute accent
(standard-display-ascii 251 "{^u}")  ; u with circumflex accent
(standard-display-g1    252 ?~)      ; u with diaeresis or umlaut mark
(standard-display-ascii 253 "{'y}")  ; y with acute accent
(standard-display-ascii 254 "{th}")  ; small thorn, Icelandic
(standard-display-ascii 255 "{\"y}") ; small y with diaeresis or umlaut mark

(provide 'iso-swed)

;;; iso-swed.el ends here
