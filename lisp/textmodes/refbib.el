;;; refbib.el --- convert refer-style references to ones usable by Latex bib

;; Copyright (C) 1989, 2001-2012 Free Software Foundation, Inc.

;; Author: Henry Kautz <kautz@research.att.com>
;; Maintainer: FSF
;; Keywords: bib, tex

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

;; Use: from a buffer containing the refer-style bibliography,
;;   M-x r2b-convert-buffer
;; Program will prompt for an output buffer name, and will log
;; warnings during the conversion process in the buffer *Log*.

;;; Change Log:

;; HISTORY
;; 9/88, created H.Kautz
;; modified 1/19/89, allow books with editor but no author;
;;                   added %O ordering field;
;;                   appended invalid multiple fields, instead of
;;                     discarding;
;;                   added rule, a tech report whose %R number
;;                     contains "ISBN" is really a book
;;                   added rule, anything with an editor is a book
;;                     or a proceedings
;;                   added 'manual type, for items with institution
;;                     but no author or editor
;;                   fixed bug so trailing blanks are trimmed
;;                   added 'proceedings type
;;                   used "organization" field for proceedings
;; modified 2/16/89, updated help messages
;; modified 2/23/89, include capitalize stop words in r2b stop words,
;;                   fixed problems with contractions (e.g. it's),
;;                   caught multiple stop words in a row
;; modified 3/1/89,  fixed capitalize-title for first words all caps
;; modified 3/15/89, allow use of " to delimit fields
;; modified 4/18/89, properly "quote" special characters on output

;;; Code:

;**********************************************************
; User Parameters

(defgroup refbib nil
  "Convert refer-style references to ones usable by Latex bib."
  :prefix "r2b-"
  :group 'wp)

(defcustom r2b-trace-on nil
  "*Non-nil means trace conversion."
  :type 'boolean
  :group 'refbib)

(defcustom r2b-journal-abbrevs
  '(
    )
  "Abbreviation list for journal names.
If the car of an element matches a journal name exactly, it is replaced by
the cadr when output.  Braces must be included if replacement is a
{string}, but not if replacement is a bibtex abbreviation.  The cadr
may be eliminated if is exactly the same as the car.
  Because titles are capitalized before matching, the abbreviation
for the journal name should be listed as beginning with a capital
letter, even if it really doesn't.
  For example, a value of '((\"Aij\" \"{Artificial Intelligence}\")
\(\"Ijcai81\" \"ijcai7\")) would expand Aij to the text string
\"Artificial Intelligence\", but would replace Ijcai81 with the
BibTeX macro \"ijcai7\"."
  :type '(repeat (list string string))
  :group 'refbib)

(defcustom r2b-booktitle-abbrevs
  '(
    )
  "Abbreviation list for book and proceedings names.
If the car of an element matches a title or booktitle exactly, it is
replaced by the cadr when output.  Braces must be included if
replacement is a {string}, but not if replacement is a bibtex
abbreviation.  The cadr may be eliminated if is exactly the same as
the car.
  Because titles are capitalized before matching, the abbreviated title
should be listed as beginning with a capital letter, even if it doesn't.
  For example, a value of '((\"Aij\" \"{Artificial Intelligence}\")
\(\"Ijcai81\" \"ijcai7\")) would expand Aij to the text string
\"Artificial Intelligence\", but would replace Ijcai81 with the
BibTeX macro \"ijcai7\"."
  :type '(repeat (list string string))
  :group 'refbib)

(defcustom r2b-proceedings-list
  '()
  "Assoc list of books or journals which are really conference proceedings,
but whose name and whose abbrev expansion (as defined in `r2b-journal-abbrevs'
and `r2b-booktitle-abbrevs') does not contain the words \"conference\" or
\"proceedings\".  (Those cases are handled automatically.)
The entry must match the given data exactly.
  Because titles are capitalized before matching, the items in this list
should begin with a capital letter.
  For example, suppose the title \"Ijcai81\" is used for the proceedings of
a conference, and its expansion is the BibTeX macro \"ijcai7\".  Then
`r2b-proceedings-list' should be '((\"Ijcai81\") ...).  If instead its
expansion were \"Proceedings of the Seventh International Conference
on Artificial Intelligence\", then you would NOT need to include Ijcai81
in `r2b-proceedings-list' (although it wouldn't cause an error)."
  :type '(repeat (list string string))
  :group 'refbib)

(defvar r2b-additional-stop-words
  "Some\\|What"
  "Words not to be used to build the citation key.
This is in addition to the `r2b-capitalize-title-stop-words'.")

(defcustom r2b-delimit-with-quote t
  "*If true, then use \" to delimit fields, otherwise use braces."
  :type 'boolean
  :group 'refbib)

;**********************************************************
; Utility Functions

(defvar r2b-capitalize-title-stop-words
   (concat
      "the\\|and\\|of\\|is\\|a\\|an\\|of\\|for\\|in\\|to\\|in\\|on\\|at\\|"
      "by\\|with\\|that\\|its")
   "Words not to be capitalized in a title (unless the first word).")

(defvar r2b-capitalize-title-stop-regexp
   (concat "\\(" r2b-capitalize-title-stop-words "\\)\\(\\b\\|'\\)"))

(defun r2b-capitalize-title-region (begin end)
   "Like `capitalize-region', but don't capitalize stop words, except the first."
   (interactive "r")
   (let ((case-fold-search nil) (orig-syntax-table (syntax-table)))
      (unwind-protect
	 (save-restriction
	    (set-syntax-table text-mode-syntax-table)
	    (narrow-to-region begin end)
	    (goto-char (point-min))
	    (if (looking-at "[A-Z][a-z]*[A-Z]")
	       (forward-word 1)
	       (capitalize-word 1))
	    (while (re-search-forward "\\<" nil t)
	       (if (looking-at "[A-Z][a-z]*[A-Z]")
		  (forward-word 1)
		  (if (let ((case-fold-search t))
			 (looking-at r2b-capitalize-title-stop-regexp))
		     (downcase-word 1)
		     (capitalize-word 1)))
	       ))
	 (set-syntax-table orig-syntax-table))))


(defun r2b-capitalize-title (s)
   "Like `capitalize', but don't capitalize stop words, except the first."
   (with-current-buffer (get-buffer-create "$$$Scratch$$$")
     (erase-buffer)
     (insert s)
     (r2b-capitalize-title-region (point-min) (point-max))
     (buffer-string)))

;*********************************************************
(defun r2b-reset ()
   "Unbind defvars, for debugging."
   (interactive)
   (makunbound 'r2b-journal-abbrevs)
   (makunbound 'r2b-booktitle-abbrevs)
   (makunbound 'r2b-proceedings-list)
   (makunbound 'r2b-capitalize-title-stop-words)
   (makunbound 'r2b-capitalize-title-stop-regexp)
   (makunbound 'r2b-additional-stop-words)
   (makunbound 'r2b-stop-regexp))

(defvar r2b-stop-regexp
   (concat "\\`\\(\\("
      r2b-additional-stop-words "\\|" r2b-capitalize-title-stop-words
      "\\)\\('\\w*\\)?\\W+\\)*\\([A-Z0-9]+\\)"))


(defun r2b-trace (&rest args)
   (if r2b-trace-on
      (progn
	 (apply (function message) args)
	 (sit-for 0))))

(defun r2b-match (exp)
   "Returns string matched in current buffer."
   (buffer-substring (match-beginning exp) (match-end exp)))

(defcustom r2b-out-buf-name "*Out*"
  "*Name of buffer for output from refer-to-bibtex."
  :type 'string
  :group 'refbib)

(defcustom r2b-log-name "*Log*"
  "*Name of buffer for logs errors from refer-to-bibtex."
  :type 'string
  :group 'refbib)

(defvar r2b-in-buf nil)
(defvar r2b-out-buf nil)
(defvar r2b-log nil)

(defvar r2b-error-found nil)

(defvar r2b-variables) (defvar r2bv-address)    (defvar r2bv-annote)
(defvar r2bv-author)   (defvar r2bv-booktitle)  (defvar r2bv-date)
(defvar r2bv-decade)   (defvar r2bv-editor)     (defvar r2bv-entry-kind)
(defvar r2bv-institution) (defvar r2bv-journal) (defvar r2bv-keywords)
(defvar r2bv-kn)       (defvar r2bv-month)      (defvar r2bv-note)
(defvar r2bv-number)   (defvar r2bv-ordering)   (defvar r2bv-organization)
(defvar r2bv-pages)    (defvar r2bv-primary-author) (defvar r2bv-publisher)
(defvar r2bv-school)   (defvar r2bv-title)      (defvar r2bv-title-first-word)
(defvar r2bv-tr)       (defvar r2bv-type)       (defvar r2bv-volume)
(defvar r2bv-where)    (defvar r2bv-year)

(setq r2b-variables '(
                      r2b-error-found
                      r2bv-author
                      r2bv-primary-author
                      r2bv-date
                      r2bv-year
                      r2bv-decade
                      r2bv-month
                      r2bv-title
                      r2bv-title-first-word
                      r2bv-editor
                      r2bv-annote
                      r2bv-tr
                      r2bv-address
                      r2bv-institution
                      r2bv-keywords
                      r2bv-booktitle
                      r2bv-journal
                      r2bv-volume
                      r2bv-number
                      r2bv-pages
                      r2bv-booktitle
                      r2bv-kn
                      r2bv-publisher
                      r2bv-organization
                      r2bv-school
                      r2bv-type
                      r2bv-where
                      r2bv-note
                      r2bv-ordering
                      ))

(defun r2b-clear-variables ()
   "Set all global vars used by r2b to nil."
   (let ((vars r2b-variables))
      (while vars
	 (set (car vars) nil)
	 (setq vars (cdr vars)))))

(defun r2b-warning (&rest args)
   (setq r2b-error-found t)
   (princ (apply (function format) args) r2b-log)
   (princ "\n" r2b-log)
   (princ "\n" r2b-out-buf)
   (princ "% " r2b-out-buf)
   (princ (apply (function format) args) r2b-out-buf))

(defun r2b-get-field (var field &optional unique required capitalize)
   "Set VAR to string value of FIELD, if any.  If none, VAR is set to
nil.  If multiple fields appear, then separate values with the
'\\nand\\t\\t', unless UNIQUE is non-nil, in which case log a warning
and just concatenate the values.  Trim off leading blanks and tabs on
first line, and trailing blanks and tabs of every line.  Log a warning
and set VAR to the empty string if REQUIRED is true.  Capitalize as a
title if CAPITALIZE is true.  Returns value of VAR."
   (let (item val (not-past-end t))
      (r2b-trace "snarfing %s" field)
      (goto-char (point-min))
      (while (and not-past-end
		(re-search-forward
		   (concat "^" field "\\b[ \t]*\\(.*[^ \t\n]\\)[ \t]*") nil t))
	 (setq item (r2b-match 1))
	 (while (and (setq not-past-end (zerop (forward-line 1)))
		   (not (looking-at "[ \t]*$\\|%")))
	       (looking-at "\\(.*[^ \t\n]\\)[ \t]*$")
	       (setq item (concat item "\n" (r2b-match 1)))
	    )
	 (if (null val)
	    (setq val item)
	    (if unique
	       (progn
		  (r2b-warning "*Invalid multiple field %s %s" field item)
		  (setq val (concat val "\n" item))
		  )
	       (setq val (concat val "\n\t\tand " item))
	       )
	    )
	 )
      (if (and val capitalize)
	 (setq val (r2b-capitalize-title val)))
      (set var val)
      (if (and (null val) required)
	 (r2b-require var))
      ))

(defun r2b-set-match (var n regexp string )
   "Set VAR to the Nth subpattern in REGEXP matched by STRING, or nil if none."
   (set var
      (if (and (stringp string) (string-match regexp string))
	 (substring string (match-beginning n) (match-end n))
	 nil)
      )
   )

(defvar r2b-month-abbrevs
   '(("jan") ("feb") ("mar") ("apr") ("may") ("jun") ("jul") ("aug")
       ("sep") ("oct") ("nov") ("dec")))

(defun r2b-convert-month ()
   "Try to convert `r2bv-month' to a standard 3 letter name."
   (if r2bv-month
      (let ((months r2b-month-abbrevs))
	 (if (string-match "[^0-9]" r2bv-month)
	    (progn
	       (while (and months (not (string-match (car (car months))
					  r2bv-month)))
		  (setq months (cdr months)))
	       (if months
		  (setq r2bv-month (car (car months)))))
	    (progn
	       (setq months (car (read-from-string r2bv-month)))
	       (if (and (numberp months)
		      (> months 0)
		      (< months 13))
		  (setq r2bv-month (car (nth months r2b-month-abbrevs)))
		  (progn
		     (r2b-warning "* Ridiculous month")
		     (setq r2bv-month nil))
		  ))
	    ))
      )
   )

(defun r2b-snarf-input ()
   "Parse buffer into global variables."
   (let ((case-fold-search t))
      (r2b-trace "snarfing...")
      (sit-for 0)
      (set-buffer r2b-in-buf)
      (goto-char (point-min))
      (princ "    " r2b-log)
      (princ (buffer-substring (point) (progn (end-of-line) (point))) r2b-log)
      (terpri r2b-log)

      (r2b-get-field 'r2bv-author "%A")
      (r2b-get-field 'r2bv-editor "%E")
      (cond
	 (r2bv-author
	    (r2b-set-match 'r2bv-primary-author 1
	       "\\b\\(\\w+\\)[ \t]*\\($\\|,\\)" r2bv-author)
	    )
	 (r2bv-editor
	    (r2b-set-match 'r2bv-primary-author 1
	       "\\b\\(\\w+\\)[ \t]*\\($\\|,\\)" r2bv-editor)
	    )
	 (t
	    (setq r2bv-primary-author "")
	    )
	 )

      (r2b-get-field 'r2bv-date "%D" t t)
      (r2b-set-match 'r2bv-year 0 "[12][0-9][0-9][0-9]" r2bv-date)
      (and (null r2bv-year)
	 (r2b-set-match 'r2bv-year 1 "[^0-9]\\([0-9][0-9]\\)$" r2bv-date)
	 (setq r2bv-year (concat "19" r2bv-year)))
      (r2b-set-match 'r2bv-decade 1 "..\\(..\\)" r2bv-year)
      (r2b-set-match 'r2bv-month 0
	 "[0-9]+/\\|[a-zA-Z]+" r2bv-date)
      (if (and (stringp r2bv-month) (string-match "\\(.*\\)/$" r2bv-month))
	 (setq r2bv-month (substring r2bv-month 0 (match-end 1))))
      (r2b-convert-month)

      (r2b-get-field 'r2bv-title "%T" t t t)
      (r2b-set-match 'r2bv-title-first-word 4
	 r2b-stop-regexp
	 r2bv-title)

      (r2b-get-field 'r2bv-annote "%X" t )
      (r2b-get-field 'r2bv-tr "%R" t)
      (r2b-get-field 'r2bv-address "%C" t)
      (r2b-get-field 'r2bv-institution "%I" t)
      (r2b-get-field 'r2bv-keywords "%K")
      (r2b-get-field 'r2bv-booktitle "%B" t nil t)
      (r2b-get-field 'r2bv-journal "%J" t nil t)
      (r2b-get-field 'r2bv-volume "%V" t)
      (r2b-get-field 'r2bv-number "%N" t)
      (r2b-get-field 'r2bv-pages "%P" t)
      (r2b-get-field 'r2bv-where "%W" t)
      (r2b-get-field 'r2bv-ordering "%O" t)
      )
   )


(defun r2b-put-field (field data &optional abbrevs)
  "Print bibtex FIELD = {DATA} if DATA not null; precede
with a comma and newline; if ABBREVS list is given, then
try to replace the {DATA} with an abbreviation."
  (if data
    (let (match nodelim multi-line index)
      (cond
	((and abbrevs (setq match (assoc data abbrevs)))
	  (if (null (cdr match))
	    (setq data (car match))
	    (setq data (car (cdr match))))
	  (setq nodelim t))
	((and (not (equal data ""))
		(not (string-match "[^0-9]" data)))
	  (setq nodelim t))
	(t
	  (setq index 0)
	  (while (string-match "[\\~^]" data index)
	    (setq data (concat (substring data 0 (match-beginning 0))
			 "\\verb+"
			 (substring data (match-beginning 0) (match-end 0))
			 "+"
			 (substring data (match-end 0))))
	    (setq index (+ (match-end 0) 7)))
	  (setq index 0)
	  (while (string-match "[$&%#_{}]" data index)
	    (setq data (concat (substring data 0 (match-beginning 0))
			 "\\"
			 (substring data (match-beginning 0))))
	    (setq index (+ (match-end 0) 1)))
	  (setq index 0)
	  (if r2b-delimit-with-quote
	    (while (string-match "\"" data index)
	      (setq data (concat (substring data 0 (match-beginning 0))
			   "{\"}"
			   (substring data (match-end 0))))
	      (setq index (+ (match-end 0) 2))))
	    ))
      (princ ", \n  ")
      (princ field)
      (princ " =\t")
      (if (not nodelim)
	(if r2b-delimit-with-quote
	  (princ "\"")
	  (princ "{")))
      (string-match ".*" data)
      (if (> (match-end 0) 59)
	(princ "\n"))
      (princ data)
      (if (not nodelim)
	(if r2b-delimit-with-quote
	  (princ "\"")
	  (princ "}")))
      )
    ))


(defun r2b-require (vars)
   "If any of VARS is null, set to empty string and log error."
   (cond
      ((null vars))
      ((listp vars) (r2b-require (car vars)) (r2b-require (cdr vars)))
      (t
	 (if (null (symbol-value vars))
	    (progn
	       (r2b-warning "*Missing value for field %s" vars)
	       (set vars "")
	       )))
      )
   )


(defmacro r2b-moveq (new old)
   "Set NEW to OLD and set OLD to nil."
   (list 'progn (list 'setq new old) (list 'setq old 'nil)))

(defun r2b-isa-proceedings (name)
   "Return t if NAME is the name of proceedings."
   (and
      name
      (or
	 (string-match "proceedings\\|conference" name)
	 (assoc name r2b-proceedings-list)
	 (let ((match (assoc name r2b-booktitle-abbrevs)))
	    (and match
	       (string-match "proceedings\\|conference" (car (cdr match)))))
      )))

(defun r2b-isa-university (name)
   "Return t if NAME is a university or similar organization,
but not a publisher."
   (and
      name
      (string-match "university" name)
      (not (string-match "press" name))

   ))

(defun r2b-barf-output ()
   "Generate bibtex based on global variables."
   (let ((standard-output r2b-out-buf) (case-fold-search t) match)

      (r2b-trace "...barfing")
      (sit-for 0)
      (set-buffer r2b-out-buf)

      (setq r2bv-kn (concat r2bv-primary-author r2bv-decade
			r2bv-title-first-word))

      (setq r2bv-entry-kind
	 (cond
	    ((r2b-isa-proceedings r2bv-journal)
	       (r2b-moveq r2bv-booktitle r2bv-journal)
	       (if (r2b-isa-university r2bv-institution)
		  (r2b-moveq r2bv-organization r2bv-institution)
		  (r2b-moveq r2bv-publisher r2bv-institution))
	       (r2b-moveq r2bv-note r2bv-tr)
	       (r2b-require 'r2bv-author)
	       'inproceedings)
	    ((r2b-isa-proceedings r2bv-booktitle)
	       (if (r2b-isa-university r2bv-institution)
		  (r2b-moveq r2bv-organization r2bv-institution)
		  (r2b-moveq r2bv-publisher r2bv-institution))
	       (r2b-moveq r2bv-note r2bv-tr)
	       (r2b-require 'r2bv-author)
	       'inproceedings)
	    ((and r2bv-tr (string-match "phd" r2bv-tr))
	       (r2b-moveq r2bv-school r2bv-institution)
	       (r2b-require 'r2bv-school )
	       (r2b-require 'r2bv-author)
	       'phdthesis)
	    ((and r2bv-tr (string-match "master" r2bv-tr))
	       (r2b-moveq r2bv-school r2bv-institution)
	       (r2b-require 'r2bv-school )
	       (r2b-require 'r2bv-author)
	       'mastersthesis)
	    ((and r2bv-tr (string-match "draft\\|unpublish" r2bv-tr))
	       (r2b-moveq r2bv-note r2bv-institution)
	       (r2b-require 'r2bv-author)
	       'unpublished)
	    (r2bv-journal
	       (r2b-require 'r2bv-author)
	       'article)
	    (r2bv-booktitle
	       (r2b-moveq r2bv-publisher r2bv-institution)
	       (r2b-moveq r2bv-note r2bv-tr)
	       (r2b-require 'r2bv-publisher)
	       (r2b-require 'r2bv-author)
	       'incollection)
	    ((and r2bv-author
		(null r2bv-editor)
		(string-match "\\`personal communication\\'" r2bv-title))
	       'misc)
	    ((r2b-isa-proceedings r2bv-title)
	       (if (r2b-isa-university r2bv-institution)
		  (r2b-moveq r2bv-organization r2bv-institution)
		  (r2b-moveq r2bv-publisher r2bv-institution))
	       (r2b-moveq r2bv-note r2bv-tr)
	       'proceedings)
	    ((or r2bv-editor
		(and r2bv-author
		   (or
		      (null r2bv-tr)
		      (string-match "\\bisbn\\b" r2bv-tr))))
	       (r2b-moveq r2bv-publisher r2bv-institution)
	       (r2b-moveq r2bv-note r2bv-tr)
	       (r2b-require 'r2bv-publisher)
	       (if (null r2bv-editor)
		  (r2b-require 'r2bv-author))
	       'book)
	    (r2bv-tr
	       (r2b-require 'r2bv-institution)
	       (if (string-match
		      "\\`\\(\\(.\\|\n\\)+\\)[ \t\n]+\\([^ \t\n]\\)+\\'"
		      r2bv-tr)
		  (progn
		     (setq r2bv-type (substring r2bv-tr 0 (match-end 1)))
		     (setq r2bv-number (substring r2bv-tr
					  (match-beginning 3)))
		     (setq r2bv-tr nil))
		  (r2b-moveq r2bv-number r2bv-tr))
	       (r2b-require 'r2bv-author)
	       'techreport)
	    (r2bv-institution
	       (r2b-moveq r2bv-organization r2bv-institution)
	       'manual)
	    (t
	       'misc)
	    ))

      (r2b-require '( r2bv-year))

      (if r2b-error-found
	 (princ "\n% Warning -- Errors During Conversion Next Entry\n"))

      (princ "\n@")
      (princ r2bv-entry-kind)
      (princ "( ")
      (princ r2bv-kn)

      (r2b-put-field "author" r2bv-author )
      (r2b-put-field "title" r2bv-title r2b-booktitle-abbrevs)
      (r2b-put-field "year" r2bv-year )

      (r2b-put-field "month" r2bv-month r2b-month-abbrevs)
      (r2b-put-field "journal" r2bv-journal r2b-journal-abbrevs)
      (r2b-put-field "volume" r2bv-volume)
      (r2b-put-field "type" r2bv-type)
      (r2b-put-field "number" r2bv-number)
      (r2b-put-field "booktitle" r2bv-booktitle r2b-booktitle-abbrevs)
      (r2b-put-field "editor" r2bv-editor)
      (r2b-put-field "publisher" r2bv-publisher)
      (r2b-put-field "institution" r2bv-institution)
      (r2b-put-field "organization" r2bv-organization)
      (r2b-put-field "school" r2bv-school)
      (r2b-put-field "pages" r2bv-pages)
      (r2b-put-field "address" r2bv-address)
      (r2b-put-field "note" r2bv-note)
      (r2b-put-field "keywords" r2bv-keywords)
      (r2b-put-field "where" r2bv-where)
      (r2b-put-field "ordering" r2bv-ordering)
      (r2b-put-field "annote" r2bv-annote)

      (princ " )\n")
      )
   )


(defun r2b-convert-record (output)
   "Transform current bib entry and append to buffer OUTPUT.
Do `\\[r2b-help]' for more info."
   (interactive
      (list (read-string "Output to buffer: " r2b-out-buf-name)))
   (let (rec-end rec-begin not-done)
      (setq r2b-out-buf-name output)
      (setq r2b-out-buf (get-buffer-create output))
      (setq r2b-in-buf (current-buffer))
      (set-buffer r2b-out-buf)
      (goto-char (point-max))
      (setq r2b-log (get-buffer-create r2b-log-name))
      (set-buffer r2b-log)
      (goto-char (point-max))
      (set-buffer r2b-in-buf)
      (setq not-done (re-search-forward "[^ \t\n]" nil t))
      (if not-done
	 (progn
	    (re-search-backward "^[ \t]*$" nil 2)
	    (re-search-forward "^%")
	    (beginning-of-line nil)
	    (setq rec-begin (point))
	    (re-search-forward "^[ \t]*$" nil 2)
	    (setq rec-end (point))
	    (narrow-to-region rec-begin rec-end)
	    (r2b-clear-variables)
	    (r2b-snarf-input)
	    (r2b-barf-output)
	    (set-buffer r2b-in-buf)
	    (widen)
	    (goto-char rec-end)
	    t)
	 nil
	 )
      ))


(defun r2b-convert-buffer (output)
  "Transform current buffer and append to buffer OUTPUT.
Do `\\[r2b-help]' for more info."
  (interactive
   (list (read-string "Output to buffer: " r2b-out-buf-name)))
  (with-current-buffer (setq r2b-log (get-buffer-create r2b-log-name))
    (erase-buffer))
  (widen)
  (goto-char (point-min))
  (message "Working, please be patient...")
  (sit-for 0)
  (while (r2b-convert-record output) t)
  (message "Done, results in %s, errors in %s"
           r2b-out-buf-name r2b-log-name))

(defvar r2b-help-message
"                   Refer to Bibtex Bibliography Conversion

A refer-style database is of the form:

%A Joe Blow
%T Great Thoughts I've Thought
%D 1977
etc.

This utility converts these kind of databases to bibtex form, for
users of TeX and LaTex.  Instructions:
1.  Visit the file containing the refer-style database.
2.  The command
	M-x r2b-convert-buffer
    converts the entire buffer, appending its output by default in a
    buffer named *Out*, and logging progress and errors in a buffer
    named *Log*.  The original file is never modified.
	Note that results are appended to *Out*, so if that buffer
	buffer already exists and contains material you don't want to
 	save, you should kill it first.
3.  Switch to the buffer *Out* and save it as a named file.
4.  To convert a single refer-style entry, simply position the cursor
    at the entry and enter
	M-x r2b-convert-record
    Again output is appended to *Out* and errors are logged in *Log*.

This utility is very robust and pretty smart about determining the
type of the entry.  It includes facilities for expanding refer macros
to text, or substituting bibtex macros.  Do M-x describe-variable on
     r2b-journal-abbrevs
     r2b-booktitle-abbrevs
     r2b-proceedings-list
for information on these features.

Please send bug reports and suggestions to
	Henry Kautz
        kautz@research.att.com
	allegra!kautz")


(defun r2b-help ()
  "Print help describing the `refbib' package."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ r2b-help-message)
    (with-current-buffer standard-output
      (help-mode))))

(provide 'refbib)
(provide 'refer-to-bibtex)

;;; refbib.el ends here
