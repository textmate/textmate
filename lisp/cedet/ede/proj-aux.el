;;; ede/proj-aux.el --- EDE Generic Project auxiliary file support

;; Copyright (C) 1998-2000, 2007, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make

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
;; Handle auxiliary files (README, FAQ, etc) in and EDE Project file.

(require 'ede/proj)
(require 'ede/pmake)

;;; Code:
(defclass ede-proj-target-aux (ede-proj-target)
  ((sourcetype :initform '(ede-aux-source)))
  "This target consists of aux files such as READMEs and COPYING.")

(defvar ede-aux-source
  (ede-sourcecode "ede-aux-source-txt"
		  :name "Auxiliary Text"
		  :sourcepattern "^[A-Z]+$\\|\\.txt$")
  "Miscellaneous fields definition.")

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-aux))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_AUX"))

(provide 'ede/proj-aux)

;;; ede/proj-aux.el ends here
