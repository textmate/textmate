;;; semantic/db-debug.el --- Extra level debugging routines for Semantic

;;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Various routines for debugging SemanticDB issues, or viewing
;; semanticdb state.

(require 'data-debug)
(require 'semantic/db)
(require 'semantic/format)

;;; Code:
;;
(defun semanticdb-dump-all-table-summary ()
  "Dump a list of all databases in Emacs memory."
  (interactive)
  (require 'data-debug)
  (let ((db semanticdb-database-list))
    (data-debug-new-buffer "*SEMANTICDB*")
    (data-debug-insert-stuff-list db "*")))

(defalias 'semanticdb-adebug-database-list 'semanticdb-dump-all-table-summary)

(defun semanticdb-adebug-current-database ()
  "Run ADEBUG on the current database."
  (interactive)
  (require 'data-debug)
  (let ((p semanticdb-current-database)
	)
    (data-debug-new-buffer "*SEMANTICDB ADEBUG*")
    (data-debug-insert-stuff-list p "*")))

(defun semanticdb-adebug-current-table ()
  "Run ADEBUG on the current database."
  (interactive)
  (require 'data-debug)
  (let ((p semanticdb-current-table))
    (data-debug-new-buffer "*SEMANTICDB ADEBUG*")
    (data-debug-insert-stuff-list p "*")))


(defun semanticdb-adebug-project-database-list ()
  "Run ADEBUG on the current database."
  (interactive)
  (require 'data-debug)
  (let ((p (semanticdb-current-database-list)))
    (data-debug-new-buffer "*SEMANTICDB ADEBUG*")
    (data-debug-insert-stuff-list p "*")))



;;; Sanity Checks
;;

(defun semanticdb-table-oob-sanity-check (cache)
  "Validate that CACHE tags do not have any overlays in them."
  (while cache
    (when (semantic-overlay-p (semantic-tag-overlay cache))
      (message "Tag %s has an erroneous overlay!"
	       (semantic-format-tag-summarize (car cache))))
    (semanticdb-table-oob-sanity-check
     (semantic-tag-components-with-overlays (car cache)))
    (setq cache (cdr cache))))

(defun semanticdb-table-sanity-check (&optional table)
  "Validate the current semanticdb TABLE."
  (interactive)
  (if (not table) (setq table semanticdb-current-table))
  (let* ((full-filename (semanticdb-full-filename table))
	 (buff (find-buffer-visiting full-filename)))
    (if buff
	(with-current-buffer buff
	  (semantic-sanity-check))
      ;; We can't use the usual semantic validity check, so hack our own.
      (semanticdb-table-oob-sanity-check (semanticdb-get-tags table)))))

(defun semanticdb-database-sanity-check ()
  "Validate the current semantic database."
  (interactive)
  (let ((tables (semanticdb-get-database-tables
		 semanticdb-current-database)))
    (while tables
      (semanticdb-table-sanity-check (car tables))
      (setq tables (cdr tables)))
    ))



(provide 'semantic/db-debug)

;;; semantic/db-debug.el ends here
