;;; cham.el --- support for Cham -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2008, 2009, 2010, 2011, 2012
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Cham, i18n

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
		      '(#xAA00 . #xAA5F)
		      (list (vector "[\xAA00-\xAA5F]+" 0 'font-shape-gstring)))

(set-language-info-alist
 "Cham" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)))

(provide 'cham)
