;;; ede/proj-archive.el --- EDE Generic Project archive support

;;  Copyright (C) 1998-2001, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make

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
;; Handle object code archives in and EDE Project file.

(require 'ede/pmake)
(require 'ede/proj-obj)

;;; Code:

(defclass ede-proj-target-makefile-archive
  (ede-proj-target-makefile-objectcode)
  ((availablelinkers :initform '(ede-archive-linker)))
  "This target generates an object code archive.")

(defvar ede-archive-linker
  (ede-linker
   "ede-archive-linker"
   :name "ar"
   :variables  '(("AR" . "ar")
		 ("AR_CMD" . "$(AR) cr"))
   :commands '("$(AR_CMD) lib$@.a $^")
   :autoconf '(("AC_CHECK_PROGS" . "RANLIB, ranlib"))
   :objectextention "")
  "Linker object for creating an archive.")

(defmethod ede-proj-makefile-insert-source-variables :BEFORE
  ((this ede-proj-target-makefile-archive) &optional moresource)
  "Insert bin_PROGRAMS variables needed by target THIS.
We aren't actually inserting SOURCE details, but this is used by the
Makefile.am generator, so use it to add this important bin program."
  (ede-pmake-insert-variable-shared
      (concat "lib" (ede-name this) "_a_LIBRARIES")
    (insert (concat "lib" (ede-name this) ".a"))))

(defmethod ede-proj-makefile-garbage-patterns
  ((this ede-proj-target-makefile-archive))
  "Add archive name to the garbage patterns.
This makes sure that the archive is removed with 'make clean'."
  (let ((garb (call-next-method)))
    (append garb (list (concat "lib" (ede-name this) ".a")))))

(provide 'ede/proj-archive)

;;; ede/proj-archive.el ends here
