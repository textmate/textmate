;;; sinhala.el --- support for Sinhala -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Sinhala, i18n

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

;;; Code:

(set-language-info-alist
 "Sinhala" '((charset unicode)
	     (coding-system utf-8)
	     (coding-priority utf-8)
	     (sample-text . "Sinhala (සිංහල)	ආයුබෝවන්")
	     (documentation . t)))

(set-char-table-range 
 composition-function-table
 '(#xD80 . #xDFF)
 (list (vector
	;; C:consonant, H:HALANT, J:ZWJ, v:vowel sign,
	;; V:independent vowel, a:ANUSVARA .. VISARGA
	(concat
	 ;; C(HJC)*v*H?a?, or
	 "[\u0D9A-\u0DC6]\\(?:\u0DCA\u200D[\u0D9A-\u0DC6]\\)*[\u0DCF-\u0DDF\u0DF2-\u0DF3]*\u0DCA?[\u0D82-\u0D83]?\\|"
	 ;; Va?, or
	 "[\u0D85-\u0D96][\u0D82-\u0D83]?\\|"
	 ;; any other singleton characters
	 "[\u0D80-\u0DFF]")
	0 'font-shape-gstring)))

;; sinhala.el ends here
