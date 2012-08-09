;;; cedet.el --- Setup CEDET environment

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: Eric M. Ludlam  <zappo@gnu.org>
;; Version: 1.0pre7
;; Keywords: OO, lisp

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
;;
;; This file depends on the major components of CEDET, so that you can
;; load them all by doing (require 'cedet).  This is mostly for
;; compatibility with the upstream, stand-alone CEDET distribution.

(eval-when-compile
  (require 'cl))

(declare-function inversion-find-version "inversion")

(defconst cedet-version "1.0"
  "Current version of CEDET.")

(defconst cedet-packages
  `(
    ;;PACKAGE   MIN-VERSION
    (cedet         ,cedet-version)
    (eieio         "1.3")
    (semantic      "2.0")
    (srecode       "1.0")
    (ede           "1.0")
    (speedbar      "1.0"))
  "Table of CEDET packages installed.")

(defvar cedet-menu-map ;(make-sparse-keymap "CEDET menu")
  (let ((map (make-sparse-keymap "CEDET menu")))
    (define-key map [semantic-force-refresh]     'undefined)
    (define-key map [semantic-edit-menu]         'undefined)
    (define-key map [navigate-menu]              'undefined)
    (define-key map [semantic-options-separator] 'undefined)
    (define-key map [global-semantic-highlight-func-mode]   'undefined)
    (define-key map [global-semantic-highlight-func-mode]   'undefined)
    (define-key map [global-semantic-decoration-mode]       'undefined)
    (define-key map [global-semantic-idle-completions-mode] 'undefined)
    (define-key map [global-semantic-idle-summary-mode]     'undefined)
    (define-key map [global-semantic-idle-scheduler-mode]   'undefined)
    (define-key map [global-semanticdb-minor-mode]          'undefined)
    (define-key map [cedet-menu-separator] 'undefined)
    (define-key map [ede-find-file]        'undefined)
    (define-key map [ede-speedbar]         'undefined)
    (define-key map [ede] 'undefined)
    (define-key map [ede-new]              'undefined)
    (define-key map [ede-target-options]   'undefined)
    (define-key map [ede-project-options]  'undefined)
    (define-key map [ede-build-forms-menu] 'undefined)
    map)
  "Menu keymap for the CEDET package.
This is used by `semantic-mode' and `global-ede-mode'.")

(defun cedet-version ()
  "Display all active versions of CEDET and dependent packages.

The PACKAGE column is the name of a given package from CEDET.

REQUESTED VERSION is the version requested by the CEDET load script.
See `cedet-packages' for details.

FILE VERSION is the version number found in the source file
for the specified PACKAGE.

LOADED VERSION is the version of PACKAGE currently loaded in Emacs
memory and (presumably) running in this Emacs instance.  Value is X
if the package has not been loaded."
  (interactive)
  (require 'inversion)
  (with-output-to-temp-buffer "*CEDET*"
    (princ "CEDET Version:\t") (princ cedet-version)
    (princ "\n  \t\t\tRequested\tFile\t\tLoaded")
    (princ "\n  Package\t\tVersion\t\tVersion\t\tVersion")
    (princ "\n  ----------------------------------------------------------")
    (let ((p cedet-packages))
      (while p
	(let ((sym (symbol-name (car (car p)))))
	  (princ "\n  ")
	  (princ sym)
	  (princ ":\t")
	  (if (< (length sym) 5)
	      (princ "\t"))
	  (if (< (length sym) 13)
	      (princ "\t"))
	  (let ((reqver (nth 1 (car p)))
		(filever (car (inversion-find-version sym)))
		(loadver (when (featurep (car (car p)))
			   (symbol-value (intern-soft (concat sym "-version"))))))
	    (princ reqver)
	    (if (< (length reqver) 8) (princ "\t"))
	    (princ "\t")
	    (if (string= filever reqver)
		;; I tried the words "check" and "match", but that
		;; just looked lame.
		(princ "ok\t")
	      (princ filever)
	      (if (< (length filever) 8) (princ "\t")))
	    (princ "\t")
	    (if loadver
		(if (string= loadver reqver)
		    (princ "ok")
		  (princ loadver))
	      (princ "Not Loaded"))
	    ))
	(setq p (cdr p))))
    (princ "\n\n\nC-h f cedet-version RET\n  for details on output format.")
    ))

(provide 'cedet)

;;; cedet.el ends here
