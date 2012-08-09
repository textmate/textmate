;;; srecode/dictionary.el --- Dictionary code for the semantic recoder.

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

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
;; Dictionaries contain lists of names and their associated values.
;; These dictionaries are used to fill in macros from recoder templates.

;;; Code:

;;; CLASSES

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'srecode)
(require 'srecode/table)
(eval-when-compile (require 'semantic))

(declare-function srecode-compile-parse-inserter "srecode/compile")
(declare-function srecode-dump-code-list "srecode/compile")
(declare-function srecode-load-tables-for-mode "srecode/find")
(declare-function srecode-template-table-in-project-p "srecode/find")
(declare-function srecode-insert-code-stream "srecode/insert")
(declare-function data-debug-new-buffer "data-debug")
(declare-function data-debug-insert-object-slots "eieio-datadebug")
(declare-function srecode-field "srecode/fields")

(defclass srecode-dictionary ()
  ((namehash :initarg :namehash
	     :documentation
	     "Hash table containing the names of all the templates.")
   (buffer :initarg :buffer
	   :documentation
	   "The buffer this dictionary was initialized with.")
   (parent :initarg :parent
	   :type (or null srecode-dictionary)
	   :documentation
	   "The parent dictionary.
Symbols not appearing in this dictionary will be checked against the
parent dictionary.")
   (origin :initarg :origin
	   :type string
	   :documentation
	   "A string representing the origin of this dictionary.
Useful only while debugging.")
   )
  "Dictionary of symbols and what they mean.
Dictionaries are used to look up named symbols from
templates to decide what to do with those symbols.")

(defclass srecode-dictionary-compound-value ()
  ()
  "A compound dictionary value.
Values stored in a dictionary must be a STRING,
a dictionary for showing sections, or an instance of a subclass
of this class.

Compound dictionary values derive from this class, and must
provide a sequence of method implementations to convert into
a string."
  :abstract t)

(defclass srecode-dictionary-compound-variable
  (srecode-dictionary-compound-value)
  ((value :initarg :value
	  :documentation
	  "The value of this template variable.
Variables in template files are usually a single string
which can be inserted into a dictionary directly.

Some variables may be more complex and involve dictionary
lookups, strings, concatenation, or the like.

The format of VALUE is determined by current template
formatting rules.")
   (compiled :initarg :compiled
	     :type list
	     :documentation
	     "The compiled version of VALUE.")
   )
  "A compound dictionary value for template file variables.
You can declare a variable in a template like this:

set NAME \"str\" macro \"OTHERNAME\"

with appending various parts together in a list.")

(defmethod initialize-instance ((this srecode-dictionary-compound-variable)
				&optional fields)
  "Initialize the compound variable THIS.
Makes sure that :value is compiled."
  (let ((newfields nil)
	(state nil))
    (while fields
      ;; Strip out :state
      (if (eq (car fields) :state)
	  (setq state (car (cdr fields)))
	(setq newfields (cons (car (cdr fields))
			      (cons (car fields) newfields))))
      (setq fields (cdr (cdr fields))))

    (when (not state)
      (error "Cannot create compound variable without :state"))

    (call-next-method this (nreverse newfields))
    (when (not (slot-boundp this 'compiled))
      (let ((val (oref this :value))
	    (comp nil))
	(while val
	  (let ((nval (car val))
		)
	    (cond ((stringp nval)
		   (setq comp (cons nval comp)))
		  ((and (listp nval)
			(equal (car nval) 'macro))
		   (require 'srecode/compile)
		   (setq comp (cons
			       (srecode-compile-parse-inserter
				(cdr nval)
				state)
			       comp)))
		  (t
		   (error "Don't know how to handle variable value %S" nval)))
	    )
	  (setq val (cdr val)))
	(oset this :compiled (nreverse comp))))))

;;; DICTIONARY METHODS
;;

(defun srecode-create-dictionary (&optional buffer-or-parent)
  "Create a dictionary for BUFFER.
If BUFFER-OR-PARENT is not specified, assume a buffer, and
use the current buffer.
If BUFFER-OR-PARENT is another dictionary, then remember the
parent within the new dictionary, and assume that BUFFER
is the same as belongs to the parent dictionary.
The dictionary is initialized with variables setup for that
buffer's table.
If BUFFER-OR-PARENT is t, then this dictionary should not be
associated with a buffer or parent."
  (save-excursion
    ;; Handle the parent
    (let ((parent nil)
	  (buffer nil)
	  (origin nil)
	  (initfrombuff nil))
      (cond
       ;; Parent is a buffer
       ((bufferp buffer-or-parent)
	(set-buffer buffer-or-parent)
	(setq buffer buffer-or-parent
	      origin (buffer-name buffer-or-parent)
	      initfrombuff t))

       ;; Parent is another dictionary
       ((srecode-dictionary-child-p buffer-or-parent)
	(setq parent buffer-or-parent
	      buffer (oref buffer-or-parent buffer)
	      origin (concat (object-name buffer-or-parent) " in "
			     (if buffer (buffer-name buffer)
			       "no buffer")))
	(when buffer
	  (set-buffer buffer)))

       ;; No parent
       ((eq buffer-or-parent t)
	(setq buffer nil
	      origin "Unspecified Origin"))

       ;; Default to unspecified parent
       (t
	(setq buffer (current-buffer)
	      origin (concat "Unspecified.  Assume "
			     (buffer-name buffer))
	      initfrombuff t)))

      ;; Create the new dictionary object.
      (let ((dict (srecode-dictionary
		   major-mode
		   :buffer   buffer
		   :parent   parent
		   :namehash (make-hash-table :test 'equal
					      :size 20)
		   :origin   origin)))
	;; Only set up the default variables if we are being built
	;; directly for a particular buffer.
	(when initfrombuff
	  ;; Variables from the table we are inserting from.
	  ;; @todo - get a better tree of tables.
	  (let ((mt (srecode-get-mode-table major-mode))
		(def (srecode-get-mode-table 'default)))
	    ;; Each table has multiple template tables.
	    ;; Do DEF first so that MT can override any values.
	    (srecode-dictionary-add-template-table dict def)
	    (srecode-dictionary-add-template-table dict mt)
	    ))
	dict))))

(defmethod srecode-dictionary-add-template-table ((dict srecode-dictionary)
						  tpl)
  "Insert into DICT the variables found in table TPL.
TPL is an object representing a compiled template file."
  (when tpl
    (let ((tabs (oref tpl :tables)))
      (require 'srecode/find) ; For srecode-template-table-in-project-p
      (while tabs
	(when (srecode-template-table-in-project-p (car tabs))
	  (let ((vars (oref (car tabs) variables)))
	    (while vars
	      (srecode-dictionary-set-value
	       dict (car (car vars)) (cdr (car vars)))
	      (setq vars (cdr vars)))))
  	(setq tabs (cdr tabs))))))


(defmethod srecode-dictionary-set-value ((dict srecode-dictionary)
					 name value)
  "In dictionary DICT, set NAME to have VALUE."
  ;; Validate inputs
  (unless (stringp name)
    (signal 'wrong-type-argument (list name 'stringp)))

  ;; Add the value.
  (with-slots (namehash) dict
    (puthash name value namehash))
  )

(defmethod srecode-dictionary-add-section-dictionary ((dict srecode-dictionary)
						      name &optional show-only force)
  "In dictionary DICT, add a section dictionary for section macro NAME.
Return the new dictionary.

You can add several dictionaries to the same section entry.
For each dictionary added to a variable, the block of codes in
the template will be repeated.

If optional argument SHOW-ONLY is non-nil, then don't add a new dictionary
if there is already one in place.  Also, don't add FIRST/LAST entries.
These entries are not needed when we are just showing a section.

Each dictionary added will automatically get values for positional macros
which will enable SECTIONS to be enabled.

 * FIRST - The first entry in the table.
 * NOTFIRST - Not the first entry in the table.
 * LAST - The last entry in the table
 * NOTLAST - Not the last entry in the table.

Adding a new dictionary will alter these values in previously
inserted dictionaries."
  ;; Validate inputs
  (unless (stringp name)
    (signal 'wrong-type-argument (list name 'stringp)))

  (let ((new (srecode-create-dictionary dict))
	(ov  (srecode-dictionary-lookup-name dict name t)))

    (when (not show-only)
      ;; Setup the FIRST/NOTFIRST and LAST/NOTLAST entries.
      (if (null ov)
	  (progn
	    (srecode-dictionary-show-section new "FIRST")
	    (srecode-dictionary-show-section new "LAST"))
	;; Not the very first one.  Let's clean up CAR.
	(let ((tail (car (last ov))))
	  (srecode-dictionary-hide-section tail "LAST")
	  (srecode-dictionary-show-section tail "NOTLAST")
	  )
	(srecode-dictionary-show-section new "NOTFIRST")
	(srecode-dictionary-show-section new "LAST"))
      )

    (when (or force
	      (not show-only)
	      (null ov))
      (srecode-dictionary-set-value dict name (append ov (list new))))
    ;; Return the new sub-dictionary.
    new))

(defmethod srecode-dictionary-show-section ((dict srecode-dictionary) name)
  "In dictionary DICT, indicate that the section NAME should be exposed."
  ;; Validate inputs
  (unless (stringp name)
    (signal 'wrong-type-argument (list name 'stringp)))

  ;; Showing a section is just like making a section dictionary, but
  ;; with no dictionary values to add.
  (srecode-dictionary-add-section-dictionary dict name t)
  nil)

(defmethod srecode-dictionary-hide-section ((dict srecode-dictionary) name)
  "In dictionary DICT, indicate that the section NAME should be hidden."
  ;; We need to find the has value, and then delete it.
  ;; Validate inputs
  (unless (stringp name)
    (signal 'wrong-type-argument (list name 'stringp)))

  ;; Add the value.
  (with-slots (namehash) dict
    (remhash name namehash))
  nil)

(defmethod srecode-dictionary-add-entries ((dict srecode-dictionary)
					   entries &optional state)
  "Add ENTRIES to DICT.

ENTRIES is a list of even length of dictionary entries to
add. ENTRIES looks like this:

  (NAME_1 VALUE_1 NAME_2 VALUE_2 ...)

The following rules apply:
 * NAME_N is a string
and for values
 * If VALUE_N is t, the section NAME_N is shown.
 * If VALUE_N is a string, an ordinary value is inserted.
 * If VALUE_N is a dictionary, it is inserted as entry NAME_N.
 * Otherwise, a compound variable is created for VALUE_N.

The optional argument STATE has to non-nil when compound values
are inserted. An error is signaled if ENTRIES contains compound
values but STATE is nil."
  (while entries
    (let ((name  (nth 0 entries))
	  (value (nth 1 entries)))
      (cond
       ;; Value is t; show a section.
       ((eq value t)
	(srecode-dictionary-show-section dict name))

       ;; Value is a simple string; create an ordinary dictionary
       ;; entry
       ((stringp value)
	(srecode-dictionary-set-value dict name value))

       ;; Value is a dictionary; insert as child dictionary.
       ((srecode-dictionary-child-p value)
	(srecode-dictionary-merge
	 (srecode-dictionary-add-section-dictionary dict name)
	 value t))

       ;; Value is some other object; create a compound value.
       (t
	(unless state
	  (error "Cannot insert compound values without state."))

	(srecode-dictionary-set-value
	 dict name
	 (srecode-dictionary-compound-variable
	  name :value value :state state)))))
    (setq entries (nthcdr 2 entries)))
  dict)

(defmethod srecode-dictionary-merge ((dict srecode-dictionary) otherdict
				     &optional force)
  "Merge into DICT the dictionary entries from OTHERDICT.
Unless the optional argument FORCE is non-nil, values in DICT are
not modified, even if there are values of the same names in
OTHERDICT."
  (when otherdict
    (maphash
     (lambda (key entry)
       ;; The new values is only merged in if there was no old value
       ;; or FORCE is non-nil.
       ;;
       ;; This protects applications from being whacked, and basically
       ;; makes these new section dictionary entries act like
       ;; "defaults" instead of overrides.
       (when (or force
		 (not (srecode-dictionary-lookup-name dict key t)))
	 (cond
	  ;; A list of section dictionaries. We need to merge them in.
	  ((and (listp entry)
		(srecode-dictionary-p (car entry)))
	   (dolist (sub-dict entry)
	     (srecode-dictionary-merge
	      (srecode-dictionary-add-section-dictionary
	       dict key t t)
	      sub-dict force)))

	  ;; Other values can be set directly.
	  (t
	   (srecode-dictionary-set-value dict key entry)))))
     (oref otherdict namehash))))

(defmethod srecode-dictionary-lookup-name ((dict srecode-dictionary)
					   name &optional non-recursive)
  "Return information about DICT's value for NAME.
DICT is a dictionary, and NAME is a string that is treated as the
name of an entry in the dictionary. If such an entry exists, its
value is returned. Otherwise, nil is returned. Normally, the
lookup is recursive in the sense that the parent of DICT is
searched for NAME if it is not found in DICT.  This recursive
lookup can be disabled by the optional argument NON-RECURSIVE.

This function derives values for some special NAMEs, such as
'FIRST' and 'LAST'."
  (if (not (slot-boundp dict 'namehash))
      nil
    ;; Get the value of this name from the dictionary or its parent
    ;; unless the lookup should be non-recursive.
    (with-slots (namehash parent) dict
      (or (gethash name namehash)
	  (and (not non-recursive)
	       (not (member name '("FIRST" "LAST" "NOTFIRST" "NOTLAST")))
	       parent
	       (srecode-dictionary-lookup-name parent name)))))
  )

(defmethod srecode-root-dictionary ((dict srecode-dictionary))
  "For dictionary DICT, return the root dictionary.
The root dictionary is usually for a current or active insertion."
  (let ((ans dict))
    (while (oref ans parent)
      (setq ans (oref ans parent)))
    ans))

;;; COMPOUND VALUE METHODS
;;
;; Compound values must provide at least the toString method
;; for use in converting the compound value into something insertable.

(defmethod srecode-compound-toString ((cp srecode-dictionary-compound-value)
				      function
				      dictionary)
  "Convert the compound dictionary value CP to a string.
If FUNCTION is non-nil, then FUNCTION is somehow applied to an aspect
of the compound value.  The FUNCTION could be a fraction
of some function symbol with a logical prefix excluded.

If you subclass `srecode-dictionary-compound-value' then this
method could return nil, but if it does that, it must insert
the value itself using `princ', or by detecting if the current
standard out is a buffer, and using `insert'."
  (object-name cp))

(defmethod srecode-dump ((cp srecode-dictionary-compound-value)
			 &optional indent)
  "Display information about this compound value."
  (princ (object-name cp))
  )

(defmethod srecode-compound-toString ((cp srecode-dictionary-compound-variable)
				      function
				      dictionary)
  "Convert the compound dictionary variable value CP into a string.
FUNCTION and DICTIONARY are as for the baseclass."
  (require 'srecode/insert)
  (srecode-insert-code-stream (oref cp compiled) dictionary))


(defmethod srecode-dump ((cp srecode-dictionary-compound-variable)
			 &optional indent)
  "Display information about this compound value."
  (require 'srecode/compile)
  (princ "# Compound Variable #\n")
  (let ((indent (+ 4 (or indent 0)))
	(cmp (oref cp compiled))
	)
    (srecode-dump-code-list cmp (make-string indent ? ))
    ))

;;; FIELD EDITING COMPOUND VALUE
;;
;; This is an interface to using field-editing objects
;; instead of asking questions.  This provides the basics
;; behind this compound value.

(defclass srecode-field-value (srecode-dictionary-compound-value)
  ((firstinserter :initarg :firstinserter
		  :documentation
		  "The inserter object for the first occurrence of this field.")
   (defaultvalue :initarg :defaultvalue
     :documentation
     "The default value for this inserter.")
   )
  "When inserting values with editable field mode, a dictionary value.
Compound values allow a field to be stored in the dictionary for when
it is referenced a second time.  This compound value can then be
inserted with a new editable field.")

(defmethod srecode-compound-toString((cp srecode-field-value)
				     function
				     dictionary)
  "Convert this field into an insertable string."
  (require 'srecode/fields)
  ;; If we are not in a buffer, then this is not supported.
  (when (not (bufferp standard-output))
    (error "FIELDS invoked while inserting template to non-buffer"))

  (if function
      (error "@todo: Cannot mix field insertion with functions")

    ;; No function.  Perform a plain field insertion.
    ;; We know we are in a buffer, so we can perform the insertion.
    (let* ((dv (oref cp defaultvalue))
	   (sti (oref cp firstinserter))
	   (start (point))
	   (name (oref sti :object-name)))

      (cond
       ;; No default value.
       ((not dv) (insert name))
       ;; A compound value as the default?  Recurse.
       ((srecode-dictionary-compound-value-child-p dv)
	(srecode-compound-toString dv function dictionary))
       ;; A string that is empty?  Use the name.
       ((and (stringp dv) (string= dv ""))
	(insert name))
       ;; Insert strings
       ((stringp dv) (insert dv))
       ;; Some other issue
       (t
	(error "Unknown default value for value %S" name)))

      ;; Create a field from the inserter.
      (srecode-field name :name name
		     :start start
		     :end (point)
		     :prompt (oref sti prompt)
		     :read-fcn (oref sti read-fcn)
		     )
      ))
  ;; Returning nil is a signal that we have done the insertion ourselves.
  nil)


;;; Higher level dictionary functions
;;
(defun srecode-create-section-dictionary (sectiondicts STATE)
  "Create a dictionary with section entries for a template.
The format for SECTIONDICTS is what is emitted from the template parsers.
STATE is the current compiler state."
  (when sectiondicts
    (let ((new (srecode-create-dictionary t)))
      ;; Loop over each section.  The section is a macro w/in the
      ;; template.
      (while sectiondicts
	(let* ((sect (car (car sectiondicts)))
	       (entries (cdr (car sectiondicts)))
	       (subdict (srecode-dictionary-add-section-dictionary new sect))
	       )
	  ;; Loop over each entry.  This is one variable in the
	  ;; section dictionary.
	  (while entries
	    (let ((tname (semantic-tag-name (car entries)))
		  (val (semantic-tag-variable-default (car entries))))
	      (if (eq val t)
		  (srecode-dictionary-show-section subdict tname)
		(cond
		 ((and (stringp (car val))
		       (= (length val) 1))
		  (setq val (car val)))
		 (t
		  (setq val (srecode-dictionary-compound-variable
			     tname :value val :state STATE))))
		(srecode-dictionary-set-value
		 subdict tname val))
	      (setq entries (cdr entries))))
	  )
	(setq sectiondicts (cdr sectiondicts)))
      new)))

(defun srecode-create-dictionaries-from-tags (tags state)
  "Create a dictionary with entries according to TAGS.

TAGS should be in the format produced by the template file
grammar. That is

TAGS = (ENTRY_1 ENTRY_2 ...)

where

ENTRY_N = (NAME ENTRY_N_1 ENTRY_N_2 ...) | TAG

where TAG is a semantic tag of class 'variable. The (NAME ... )
form creates a child dictionary which is stored under the name
NAME. The TAG form creates a value entry or section dictionary
entry whose name is the name of the tag.

STATE is the current compiler state."
  (let ((dict    (srecode-create-dictionary t))
	(entries (apply #'append
			(mapcar
			 (lambda (entry)
			   (cond
			    ;; Entry is a tag
			    ((semantic-tag-p entry)
			     (let ((name  (semantic-tag-name entry))
				   (value (semantic-tag-variable-default entry)))
			       (list name
				     (if (and (listp value)
					      (= (length value) 1)
					      (stringp (car value)))
					 (car value)
				       value))))

			    ;; Entry is a nested dictionary
			    (t
			     (let ((name    (car entry))
				   (entries (cdr entry)))
			       (list name
				     (srecode-create-dictionaries-from-tags
				      entries state))))))
			 tags))))
    (srecode-dictionary-add-entries
     dict entries state)
    dict)
  )

;;; DUMP DICTIONARY
;;
;; Make a dictionary, and dump it's contents.

(defun srecode-adebug-dictionary ()
  "Run data-debug on this mode's dictionary."
  (interactive)
  (require 'eieio-datadebug)
  (require 'semantic)
  (require 'srecode/find)
  (let* ((modesym major-mode)
	 (start (current-time))
	 (junk (or (progn (srecode-load-tables-for-mode modesym)
			  (srecode-get-mode-table modesym))
		   (error "No table found for mode %S" modesym)))
	 (dict (srecode-create-dictionary (current-buffer)))
	 (end (current-time))
	 )
    (message "Creating a dictionary took %.2f seconds."
	     (semantic-elapsed-time start end))
    (data-debug-new-buffer "*SRECODE ADEBUG*")
    (data-debug-insert-object-slots dict "*")))

(defun srecode-dictionary-dump ()
  "Dump a typical fabricated dictionary."
  (interactive)
  (require 'srecode/find)
  (let ((modesym major-mode))
    ;; This load allows the dictionary access to inherited
    ;; and stacked dictionary entries.
    (srecode-load-tables-for-mode modesym)
    (let ((tmp (srecode-get-mode-table modesym))
	  )
      (if (not tmp)
	  (error "No table found for mode %S" modesym))
      ;; Now make the dictionary.
      (let ((dict (srecode-create-dictionary (current-buffer))))
	(with-output-to-temp-buffer "*SRECODE DUMP*"
	  (princ "DICTIONARY FOR ")
	  (princ major-mode)
	  (princ "\n--------------------------------------------\n")
	  (srecode-dump dict))
	))))

(defmethod srecode-dump ((dict srecode-dictionary) &optional indent)
  "Dump a dictionary."
  (if (not indent) (setq indent 0))
  (maphash (lambda (key entry)
	     (princ (make-string indent ? ))
	     (princ " ")
	     (princ key)
	     (princ " ")
	     (cond ((and (listp entry)
			 (srecode-dictionary-p (car entry)))
		    (let ((newindent (if indent
					 (+ indent 4)
				       4)))
		      (while entry
			(princ " --> SUBDICTIONARY ")
			(princ (object-name dict))
			(princ "\n")
			(srecode-dump (car entry) newindent)
			(setq entry (cdr entry))
			))
		    (princ "\n")
		    )
		   ((srecode-dictionary-compound-value-child-p entry)
		    (srecode-dump entry indent)
		    (princ "\n")
		    )
		   (t
		    (prin1 entry)
		    ;(princ "\n")
		    ))
	     (terpri)
	     )
	   (oref dict namehash))
  )

(provide 'srecode/dictionary)

;;; srecode/dictionary.el ends here
