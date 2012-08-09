;;; georgian.el --- language support for Georgian -*- no-byte-compile: t -*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
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

;;; Code:

(define-coding-system 'georgian-ps
  "Georgian PS encoding"
  :coding-type 'charset
  :mnemonic ?G
  :charset-list '(georgian-ps))

(define-coding-system 'georgian-academy
  "Georgian Academy encoding"
  :coding-type 'charset
  :mnemonic ?G
  :charset-list '(georgian-academy))

(set-language-info-alist
 "Georgian" `((coding-system georgian-ps)
	      (coding-priority georgian-ps)
	      (input-method . "georgian")
	      (nonascii-translation . georgian-ps)
	      (documentation . "Support for georgian-ps character set."))
 '("European"))				; fixme: is this appropriate for
					; a non-Latin script?

(provide 'georgian)

;;; georgian.el ends here
