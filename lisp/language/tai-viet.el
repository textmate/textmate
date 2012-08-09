;;; tai-viet.el --- support for Tai Viet -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.
;; Copyright (C) 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Tai Viet, i18n

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

;; Tai Viet is being included in the Unicode at the range U+AA80..U+AADF.

;;; Code:

(set-char-table-range composition-function-table
		      '(#xAA80 . #xAADF)
		      'tai-viet-composition-function)

(set-language-info-alist
 "TaiViet" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "tai-sonla")
	      (sample-text . "TaiViet (ꪁꪫꪱꪣ ꪼꪕ)\t\tꪅꪰꪙꫂ ꪨꪮꫂ ꪁꪫꪱ / ꪅꪽ ꪨꪷ ꪁꪫꪱ")
	      (documentation . "\
TaiViet refers to the Tai language used by Tai people in
Vietnam, and also refers to the script used for this language.
Both the script and language have the same origin as that of Thai
language/script used in Thailand, but now they differ from each
other in a significant way (especially the scripts are).

The language name is spelled as \"ꪁꪫꪱꪣ ꪼꪕ\", and the script name is
spelled as \"ꪎ ꪼꪕ\" in the modern form, \"ꪎꪳ ꪼꪕ\" in the traditional
form.

As the proposal for TaiViet script to the Unicode is still on
the progress, we use the Private Use Area for TaiViet
characters (U+F000..U+F07E).  A TaiViet font encoded accordingly
is available at this web page:
    http://www.m17n.org/viettai/
")))

(provide 'tai-viet)
