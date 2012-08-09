;;; trampver.el --- Transparent Remote Access, Multiple Protocol
;;; lisp/trampver.el.  Generated from trampver.el.in by configure.

;; Copyright (C) 2003-2012 Free Software Foundation, Inc.

;; Author: Kai Großjohann <kai.grossjohann@gmx.net>
;; Keywords: comm, processes
;; Package: tramp

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

;; In the Tramp CVS repository, the version number and the bug report
;; address are auto-frobbed from configure.ac, so you should edit that
;; file and run "autoconf && ./configure" to change them.  (X)Emacs
;; version check is defined in macro AC_EMACS_INFO of aclocal.m4;
;; should be changed only there.

;;;###tramp-autoload
(defconst tramp-version "2.2.3-24.1"
  "This version of Tramp.")

;;;###tramp-autoload
(defconst tramp-bug-report-address "tramp-devel@gnu.org"
  "Email address to send bug reports to.")

;; Check for (X)Emacs version.
(let ((x (if (or (>= emacs-major-version 22)
		 (and (featurep 'xemacs)
		      (= emacs-major-version 21)
		      (>= emacs-minor-version 4)))
	     "ok"
	   (format "Tramp 2.2.3-24.1 is not fit for %s"
		   (when (string-match "^.*$" (emacs-version))
		     (match-string 0 (emacs-version)))))))
  (unless (string-match "\\`ok\\'" x) (error "%s" x)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'trampver 'force)))

(provide 'trampver)

;;; trampver.el ends here

;; Local Variables:
;; mode: Emacs-Lisp
;; coding: utf-8
;; End:
