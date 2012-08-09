;;; mail-prsvr.el --- Interface variables for parsing mail

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

(defvar mail-parse-charset nil
  "Default charset used by low-level libraries.
This variable should never be set.  Instead, it should be bound by
functions that wish to call mail-parse functions and let them know
what the desired charset is to be.")

(defvar mail-parse-mule-charset nil
  "Default MULE charset used by low-level libraries.
This variable should never be set.")

(defvar mail-parse-ignored-charsets nil
  "Ignored charsets used by low-level libraries.
This variable should never be set.  Instead, it should be bound by
functions that wish to call mail-parse functions and let them know
what the desired charsets is to be ignored.")

(provide 'mail-prsvr)

;;; mail-prsvr.el ends here
