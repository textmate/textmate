;;; ede/srecode.el --- EDE utilities on top of SRecoder

;; Copyright (C) 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; EDE utilities for using SRecode to generate project files, such as
;; Makefiles.

(require 'srecode)

(declare-function srecode-create-dictionary "srecode/dictionary")
(declare-function srecode-dictionary-set-value "srecode/dictionary")
(declare-function srecode-load-tables-for-mode "srecode/find")
(declare-function srecode-table "srecode/find")
(declare-function srecode-template-get-table "srecode/find")
(declare-function srecode-insert-fcn "srecode/insert")
(declare-function srecode-resolve-arguments "srecode/insert")
(declare-function srecode-map-update-map "srecode/map")

;;; Code:
(defun ede-srecode-setup ()
  "Initialize Srecode for EDE."
  (require 'srecode/map)
  (require 'srecode/find)
  (srecode-map-update-map t)
  ;; We don't call this unless we need it.  Load in the templates.
  (srecode-load-tables-for-mode 'makefile-mode)
  (srecode-load-tables-for-mode 'makefile-mode 'ede)
  (srecode-load-tables-for-mode 'autoconf-mode)
  (srecode-load-tables-for-mode 'autoconf-mode 'ede))

(defmacro ede-srecode-insert-with-dictionary (template &rest forms)
  "Insert TEMPLATE after executing FORMS with a dictionary.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Locally binds the variable DICT to a dictionary which can be
updated in FORMS."
  `(let* ((dict (srecode-create-dictionary))
	  (temp (srecode-template-get-table (srecode-table)
					    ,template
					    nil
					    'ede))
	  )
     (when (not temp)
       (error "EDE template %s for %s not found!"
	      ,template major-mode))
     (srecode-resolve-arguments temp dict)

     ;; Now execute forms for updating DICT.
     (progn ,@forms)

     (srecode-insert-fcn temp dict)
     ))

(defun ede-srecode-insert (template &rest dictionary-entries)
  "Insert at the current point TEMPLATE.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Add DICTIONARY-ENTRIES into the dictionary before insertion.
Note: Just like `srecode-insert', but templates found in 'ede app."
  (require 'srecode/insert)
  (ede-srecode-insert-with-dictionary template

    ;; Add in optional dictionary entries.
    (while dictionary-entries
      (srecode-dictionary-set-value dict
				    (car dictionary-entries)
				    (car (cdr dictionary-entries)))
      (setq dictionary-entries
	    (cdr (cdr dictionary-entries))))

    ))

(provide 'ede/srecode)

;;; ede/srecode.el ends here
