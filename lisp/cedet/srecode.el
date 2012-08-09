;;; srecode.el --- Semantic buffer evaluator.

;;; Copyright (C) 2005, 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: codegeneration
;; Version: 1.0pre7

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
;;
;; Semantic does the job of converting source code into useful tag
;; information.  The set of `semantic-format-tag' functions has one
;; function that will create a prototype of a tag, which has severe
;; issues of complexity (in the format tag file itself) and inaccuracy
;; (for the purpose of C++ code.)
;;
;; Contemplation of the simplistic problem within the scope of
;; semantic showed that the solution was more complex than could
;; possibly be handled in semantic/format.el.   Semantic Recode, or
;; srecode is a rich API for generating code out of semantic tags, or
;; recoding the tags.
;;
;; See the srecode manual for specific details.

(require 'eieio)
(require 'mode-local)
(load "srecode/loaddefs" nil 'nomessage)

(defvar srecode-version "1.0"
  "Current version of the Semantic Recoder.")

;;; Code:
(defgroup srecode nil
  "Semantic Recoder."
  :group 'extensions
  :group 'tools)

(provide 'srecode)

;;; srecode.el ends here
