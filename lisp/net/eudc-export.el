;;; eudc-export.el --- functions to export EUDC query results

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;; Maintainer: Pavel Janík <Pavel@Janik.cz>
;; Keywords: comm
;; Package: eudc

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

;;; Usage:
;;    See the corresponding info file

;;; Code:

(require 'eudc)

(if (not (featurep 'bbdb))
    (load-library "bbdb"))
(if (not (featurep 'bbdb-com))
    (load-library "bbdb-com"))

(defun eudc-create-bbdb-record (record &optional silent)
  "Create a BBDB record using the RECORD alist.
RECORD is an alist of (KEY . VALUE) where KEY is a directory attribute name
symbol and VALUE is the corresponding value for the record.
If SILENT is non-nil then the created BBDB record is not displayed."
  ;; This function runs in a special context where lisp symbols corresponding
  ;; to field names in record are bound to the corresponding values
  (eval
   `(let* (,@(mapcar (lambda (c)
			(list (car c) (if (listp (cdr c))
					  (list 'quote (cdr c))
					(cdr c))))
		     record)
	     bbdb-name
	     bbdb-company
	     bbdb-net
	     bbdb-address
	     bbdb-phones
	     bbdb-notes
	     spec
	     bbdb-record
	     value
	     (conversion-alist (symbol-value eudc-bbdb-conversion-alist)))

      ;; BBDB standard fields
      (setq bbdb-name (eudc-parse-spec (cdr (assq 'name conversion-alist)) record nil)
	    bbdb-company (eudc-parse-spec (cdr (assq 'company conversion-alist)) record nil)
	    bbdb-net (eudc-parse-spec (cdr (assq 'net conversion-alist)) record nil)
	    bbdb-notes (eudc-parse-spec (cdr (assq 'notes conversion-alist)) record nil))
      (setq spec (cdr (assq 'address conversion-alist)))
      (setq bbdb-address (delq nil (eudc-parse-spec (if (listp (car spec))
						      spec
						    (list spec))
						  record t)))
      (setq spec (cdr (assq 'phone conversion-alist)))
      (setq bbdb-phones (delq nil (eudc-parse-spec (if (listp (car spec))
						     spec
						   (list spec))
						 record t)))
      ;; BBDB custom fields
      (setq bbdb-notes (append (list (and bbdb-notes (cons 'notes bbdb-notes)))
			       (mapcar (function
					(lambda (mapping)
					  (if (and (not (memq (car mapping)
							      '(name company net address phone notes)))
						   (setq value (eudc-parse-spec (cdr mapping) record nil)))
					      (cons (car mapping) value))))
				       conversion-alist)))
      (setq bbdb-notes (delq nil bbdb-notes))
      (setq bbdb-record (bbdb-create-internal bbdb-name
					      bbdb-company
					      bbdb-net
					      bbdb-address
					      bbdb-phones
					      bbdb-notes))
      (or silent
	  (bbdb-display-records (list bbdb-record))))))

(defun eudc-parse-spec (spec record recurse)
  "Parse the conversion SPEC using RECORD.
If RECURSE is non-nil then SPEC may be a list of atomic specs."
  (cond
   ((or (stringp spec)
	(symbolp spec)
	(and (listp spec)
	     (symbolp (car spec))
	     (fboundp (car spec))))
    (condition-case nil
	(eval spec)
      (void-variable nil)))
   ((and recurse
	 (listp spec))
    (mapcar (lambda (spec-elem)
	       (eudc-parse-spec spec-elem record nil))
	    spec))
   (t
    (error "Invalid specification for `%s' in `eudc-bbdb-conversion-alist'" spec))))

(defun eudc-bbdbify-address (addr location)
  "Parse ADDR into a vector compatible with BBDB.
ADDR should be an address string of no more than four lines or a
list of lines.
The last two lines are searched for the zip code, city and state name.
LOCATION is used as the address location for bbdb."
  (let* ((addr-components (if (listp addr)
			      (reverse addr)
			    (reverse (split-string addr "\n"))))
	 (last1 (pop addr-components))
	 (last2 (pop addr-components))
	 zip city state)
    (setq addr-components (nreverse addr-components))
    ;; If not containing the zip code the last line is supposed to contain a
    ;; country name and the address is supposed to be in european style
    (if (not (string-match "[0-9][0-9][0-9]" last1))
	(progn
	  (setq state last1)
	  (if (string-match "\\([0-9]+\\)[ \t]+\\(.*\\)" last2)
	      (setq city (match-string 2 last2)
		    zip (string-to-number (match-string 1 last2)))
	    (error "Cannot parse the address")))
      (cond
       ;; American style
       ((string-match "\\(\\w+\\)\\W*\\([A-Z][A-Z]\\)\\W*\\([0-9]+\\)" last1)
	(setq city (match-string 1 last1)
	      state (match-string 2 last1)
	      zip (string-to-number (match-string 3 last1))))
       ;; European style
       ((string-match "\\([0-9]+\\)[ \t]+\\(.*\\)" last1)
	(setq city (match-string 2 last1)
	      zip (string-to-number (match-string 1 last1))))
       (t
	(error "Cannot parse the address"))))
    (vector location
	    (or (nth 0 addr-components) "")
	    (or (nth 1 addr-components) "")
	    (or (nth 2 addr-components) "")
	    (or city "")
	    (or state "")
	    zip)))

;; External.
(declare-function bbdb-parse-phone-number "ext:bbdb-com"
                  (string &optional number-type))
(declare-function bbdb-string-trim "ext:bbdb" (string))

(defun eudc-bbdbify-phone (phone location)
  "Parse PHONE into a vector compatible with BBDB.
PHONE is either a string supposedly containing a phone number or
a list of such strings which are concatenated.
LOCATION is used as the phone location for BBDB."
  (cond
   ((stringp phone)
    (let (phone-list)
      (condition-case err
	  (setq phone-list (bbdb-parse-phone-number phone))
	(error
	 (if (string= "phone number unparsable." (eudc-cadr err))
	     (if (not (y-or-n-p (format "BBDB claims %S to be unparsable--insert anyway? " phone)))
		 (error "Phone number unparsable")
	       (setq phone-list (list (bbdb-string-trim phone))))
	   (signal (car err) (cdr err)))))
      (if (= 3 (length phone-list))
	  (setq phone-list (append phone-list '(nil))))
      (apply 'vector location phone-list)))
   ((listp phone)
    (vector location (mapconcat 'identity phone ", ")))
   (t
    (error "Invalid phone specification"))))

(defun eudc-batch-export-records-to-bbdb ()
  "Insert all the records returned by a directory query into BBDB."
  (interactive)
  (goto-char (point-min))
  (let ((nbrec 0)
	record)
    (while (eudc-move-to-next-record)
      (and (overlays-at (point))
	   (setq record (overlay-get (car (overlays-at (point))) 'eudc-record))
	   (1+ nbrec)
	   (eudc-create-bbdb-record record t)))
    (message "%d records imported into BBDB" nbrec)))

;;;###autoload
(defun eudc-insert-record-at-point-into-bbdb ()
  "Insert record at point into the BBDB database.
This function can only be called from a directory query result buffer."
  (interactive)
  (let ((record (and (overlays-at (point))
		     (overlay-get (car (overlays-at (point))) 'eudc-record))))
    (if (null record)
	(error "Point is not over a record")
      (eudc-create-bbdb-record record))))

;;;###autoload
(defun eudc-try-bbdb-insert ()
  "Call `eudc-insert-record-at-point-into-bbdb' if on a record."
  (interactive)
  (and (or (featurep 'bbdb)
	   (prog1 (locate-library "bbdb") (message "")))
       (overlays-at (point))
       (overlay-get (car (overlays-at (point))) 'eudc-record)
       (eudc-insert-record-at-point-into-bbdb)))

;;; eudc-export.el ends here
