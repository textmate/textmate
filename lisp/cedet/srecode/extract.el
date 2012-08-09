;;; srecode/extract.el --- Extract content from previously inserted macro.

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
;; Extract content from a previously inserted macro.
;;
;; The extraction routines can be handy if you want to extract users
;; added text from the middle of a template inserted block of text.
;; This code will not work for all templates.  It will only work for
;; templates with unique static text between all the different insert
;; macros.
;;
;; That said, it will handle include and section templates, so complex
;; or deep template calls can be extracted.
;;
;; This code was specifically written for srecode-document, which
;; wants to extract user written text, and re-use it in a reformatted
;; comment.

(require 'srecode)
(require 'srecode/compile)
(require 'srecode/insert)

;;; Code:

(defclass srecode-extract-state ()
  ((anchor :initform nil
	   :documentation
	   "The last known plain-text end location.")
   (lastinserter :initform nil
		 :documentation
		 "The last inserter with 'later extraction type.")
   (lastdict :initform nil
	     :documentation
	     "The dictionary associated with lastinserter.")
   )
  "The current extraction state.")

(defmethod srecode-extract-state-set ((st srecode-extract-state) ins dict)
  "Set onto the extract state ST a new inserter INS and dictionary DICT."
  (oset st lastinserter ins)
  (oset st lastdict dict))

(defmethod srecode-extract-state-set-anchor ((st srecode-extract-state))
  "Reset the anchor point on extract state ST."
  (oset st anchor (point)))

(defmethod srecode-extract-state-extract ((st srecode-extract-state)
					  endpoint)
  "Perform an extraction on the extract state ST with ENDPOINT.
If there was no waiting inserter, do nothing."
  (when (oref st lastinserter)
    (save-match-data
      (srecode-inserter-extract (oref st lastinserter)
				(oref st anchor)
				endpoint
				(oref st lastdict)
				st))
    ;; Clear state.
    (srecode-extract-state-set st nil nil)))

;;; Extraction
;l
(defun srecode-extract (template start end)
  "Extract TEMPLATE from between START and END in the current buffer.
Uses TEMPLATE's constant strings to break up the text and guess what
the dictionary entries were for that block of text."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((dict (srecode-create-dictionary t))
	    (state (srecode-extract-state "state"))
	    )
	(goto-char start)
	(srecode-extract-method template dict state)
	dict))))

(defmethod srecode-extract-method ((st srecode-template) dictionary
				   state)
  "Extract template ST and store extracted text in DICTIONARY.
Optional STARTRETURN is a symbol in which the start of the first
plain-text match occurred."
  (srecode-extract-code-stream (oref st code) dictionary state))

(defun srecode-extract-code-stream (code dictionary state)
  "Extract CODE from buffer text into DICTIONARY.
Uses string constants in CODE to split up the buffer.
Uses STATE to maintain the current extraction state."
  (while code
    (cond

     ;; constant strings need mark the end of old inserters that
     ;; need to extract values, or are just there.
     ((stringp (car code))
      (srecode-extract-state-set-anchor state)
      ;; When we have a string, find it in the collection, then extract
      ;; that start point as the end point of the inserter
      (unless (re-search-forward (regexp-quote (car code))
				 (point-max) t)
	(error "Unable to extract all dictionary entries"))

      (srecode-extract-state-extract state (match-beginning 0))
      (goto-char (match-end 0))
      )

     ;; Some inserters are simple, and need to be extracted after
     ;; we find our next block of static text.
     ((eq (srecode-inserter-do-extract-p (car code)) 'later)
      (srecode-extract-state-set state (car code) dictionary)
      )

     ;; Some inserter want to start extraction now, such as sections.
     ;; We can't predict the end point till we parse out the middle.
     ((eq (srecode-inserter-do-extract-p (car code)) 'now)
      (srecode-extract-state-set-anchor state)
      (srecode-inserter-extract (car code) (point) nil dictionary state))
     )
    (setq code (cdr code))
    ))

;;; Inserter Base Extractors
;;
(defmethod srecode-inserter-do-extract-p ((ins srecode-template-inserter))
  "Return non-nil if this inserter can extract values."
  nil)

(defmethod srecode-inserter-extract ((ins srecode-template-inserter)
				     start end dict state)
  "Extract text from START/END and store in DICT.
Return nil as this inserter will extract nothing."
  nil)

;;; Variable extractor is simple and can extract later.
;;
(defmethod srecode-inserter-do-extract-p ((ins srecode-template-inserter-variable))
  "Return non-nil if this inserter can extract values."
  'later)

(defmethod srecode-inserter-extract ((ins srecode-template-inserter-variable)
				     start end vdict state)
  "Extract text from START/END and store in VDICT.
Return t if something was extracted.
Return nil if this inserter doesn't need to extract anything."
  (srecode-dictionary-set-value vdict
				(oref ins :object-name)
				(buffer-substring-no-properties
				 start end)
				)
  t)

;;; Section Inserter
;;
(defmethod srecode-inserter-do-extract-p ((ins srecode-template-inserter-section-start))
  "Return non-nil if this inserter can extract values."
  'now)

(defmethod srecode-inserter-extract ((ins srecode-template-inserter-section-start)
				     start end indict state)
  "Extract text from START/END and store in INDICT.
Return the starting location of the first plain-text match.
Return nil if nothing was extracted."
  (let ((name (oref ins :object-name))
	(subdict (srecode-create-dictionary indict))
	(allsubdict nil)
	)

    ;; Keep extracting till we can extract no more.
    (while (condition-case nil
	       (progn
		 (srecode-extract-method
		  (oref ins template) subdict state)
		 t)
	     (error nil))

      ;; Success means keep this subdict, and also make a new one for
      ;; the next iteration.
      (setq allsubdict (cons subdict allsubdict))
      (setq subdict (srecode-create-dictionary indict))
      )

    (srecode-dictionary-set-value indict name (nreverse allsubdict))

    nil))

;;; Include Extractor must extract now.
;;
(defmethod srecode-inserter-do-extract-p ((ins srecode-template-inserter-include))
  "Return non-nil if this inserter can extract values."
  'now)

(defmethod srecode-inserter-extract ((ins srecode-template-inserter-include)
				     start end dict state)
  "Extract text from START/END and store in DICT.
Return the starting location of the first plain-text match.
Return nil if nothing was extracted."
  (goto-char start)
  (srecode-insert-include-lookup ins dict)
  ;; There are two modes for includes.  One is with no dict,
  ;; so it is inserted straight.  If the dict has a name, then
  ;; we need to run once per dictionary occurrence.
  (if (not (string= (oref ins :object-name) ""))
      ;; With a name, do the insertion.
      (let ((subdict (srecode-dictionary-add-section-dictionary
		      dict (oref ins :object-name))))
	(error "Need to implement include w/ name extractor")
	;; Recurse into the new template while no errors.
	(while (condition-case nil
		   (progn
		     (srecode-extract-method
		      (oref ins includedtemplate) subdict
		      state)
		     t)
		 (error nil))))

    ;; No stream, do the extraction into the current dictionary.
    (srecode-extract-method (oref ins includedtemplate) dict
			    state))
  )


(provide 'srecode/extract)

;;; srecode/extract.el ends here
