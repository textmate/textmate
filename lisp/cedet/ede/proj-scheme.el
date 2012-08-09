;;; ede/proj-scheme.el --- EDE Generic Project scheme (guile) support

;; Copyright (C) 1998-2000, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make, scheme

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
;; Handle scheme (Guile) in and EDE Project file.
;; This is a specialized do nothing class.

(require 'ede/proj)
(require 'ede/autoconf-edit)

;;; Code:
(defclass ede-proj-target-scheme (ede-proj-target)
  ((menu :initform nil)
   (keybindings :initform nil)
   (interpreter :initarg :interpreter
		:initform "guile"
		:type string
		:custom string
		:documentation "The preferred interpreter for this code.")
   )
  "This target consists of scheme files.")

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target-scheme))
  "Tweak the configure file (current buffer) to accommodate THIS."
  (autoconf-insert-new-macro "AM_INIT_GUILE_MODULE"))

(provide 'ede/proj-scheme)

;;; ede/proj-scheme.el ends here
