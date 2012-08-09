;;; blessmail.el --- decide whether movemail needs special privileges -*- no-byte-compile: t -*-

;; Copyright (C) 1994, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

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

;; This is loaded into a bare Emacs to create the blessmail script,
;; which (on systems that need it) is used during installation
;; to give appropriate permissions to movemail.
;;
;; It has to be done from lisp in order to be sure of getting the
;; correct value of rmail-spool-directory.

;;; Code:

;; These are no longer needed because we run this in emacs instead of temacs.
;; (message "Using load-path %s" load-path)
;; (load "paths.el")
;; It is not safe to load site-init.el here, because it might have things in it
;; that won't load properly unless all the rest of Emacs is loaded.

(let ((dirname (directory-file-name rmail-spool-directory))
      linkname attr modes)
  ;; Check for symbolic link
  (while (setq linkname (file-symlink-p dirname))
    (setq dirname (if (file-name-absolute-p linkname)
		      linkname
		    (concat (file-name-directory dirname) linkname))))
  (insert "#!/bin/sh\n")
  (setq attr (file-attributes dirname))
  (if (not (eq t (car attr)))
      (insert (format "echo %s is not a directory\n" rmail-spool-directory))
    (setq modes (nth 8 attr))
    (cond ((= ?w (aref modes 8))
	   ;; Nothing needs to be done.
	   )
	  ((= ?w (aref modes 5))
	   (insert "chgrp " (number-to-string (nth 3 attr))
		   " $* && chmod g+s $*\n"))
	  ((= ?w (aref modes 2))
	   (insert "chown " (number-to-string (nth 2 attr))
		   " $* && chmod u+s $*\n"))
	  (t
	   (insert "chown root $* && chmod u+s $*\n"))))
  (insert "echo mail directory = " dirname "\n"))
(write-region (point-min) (point-max) "blessmail")
(kill-emacs)

;;; blessmail.el ends here
