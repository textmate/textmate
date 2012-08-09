;;; srecode/insert.el --- Insert srecode templates to an output stream.

;; Copyright (C) 2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Define and implements specific inserter objects.
;;
;; Manage the insertion process for a template.
;;

(eval-when-compile
  (require 'cl)) ;; for `lexical-let'

(require 'srecode/compile)
(require 'srecode/find)
(require 'srecode/dictionary)
(require 'srecode/args)
(require 'srecode/filters)

(defvar srecode-template-inserter-point)
(declare-function srecode-overlaid-activate "srecode/fields")
(declare-function srecode-template-inserted-region "srecode/fields")

;;; Code:

(defcustom srecode-insert-ask-variable-method 'ask
  "Determine how to ask for a dictionary value when inserting a template.
Only the ASK style inserter will query the user for a value.
Dictionary value references that ask begin with the ? character.
Possible values are:
  'ask   - Prompt in the minibuffer as the value is inserted.
  'field - Use the dictionary macro name as the inserted value,
           and place a field there.  Matched fields change together.

NOTE: The field feature does not yet work with XEmacs."
  :group 'srecode
  :type '(choice (const :tag "Ask" ask)
		 (const :tag "Field" field)))

(defvar srecode-insert-with-fields-in-progress nil
  "Non-nil means that we are actively inserting a template with fields.")

;;; INSERTION COMMANDS
;;
;; User level commands for inserting stuff.
(defvar srecode-insertion-start-context nil
  "The context that was at point at the beginning of the template insertion.")

(defun srecode-insert-again ()
  "Insert the previously inserted template (by name) again."
  (interactive)
  (let ((prev (car srecode-read-template-name-history)))
    (if prev
	(srecode-insert prev)
      (call-interactively 'srecode-insert))))

;;;###autoload
(defun srecode-insert (template-name &rest dict-entries)
  "Insert the template TEMPLATE-NAME into the current buffer at point.
DICT-ENTRIES are additional dictionary values to add."
  (interactive (list (srecode-read-template-name "Template Name: ")))
  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
  (let ((newdict (srecode-create-dictionary))
	(temp (srecode-template-get-table (srecode-table) template-name))
	(srecode-insertion-start-context (srecode-calculate-context))
	)
    (if (not temp)
	(error "No Template named %s" template-name))
    (while dict-entries
      (srecode-dictionary-set-value newdict
				    (car dict-entries)
				    (car (cdr dict-entries)))
      (setq dict-entries (cdr (cdr dict-entries))))
    (srecode-insert-fcn temp newdict)
    ;; Don't put code here.  We need to return the end-mark
    ;; for this insertion step.
    ))

(defun srecode-insert-fcn (template dictionary &optional stream skipresolver)
  "Insert TEMPLATE using DICTIONARY into STREAM.
Optional SKIPRESOLVER means to avoid refreshing the tag list,
or resolving any template arguments.  It is assumed the caller
has set everything up already."
  ;; Perform the insertion.
  (let ((standard-output (or stream (current-buffer)))
	(end-mark nil))
    ;; Merge any template entries into the input dictionary.
    (when (slot-boundp template 'dictionary)
      (srecode-dictionary-merge dictionary (oref template dictionary)))

    (unless skipresolver
      ;; Make sure the semantic tags are up to date.
      (semantic-fetch-tags)
      ;; Resolve the arguments
      (srecode-resolve-arguments template dictionary))
    ;; Insert
    (if (bufferp standard-output)
	;; If there is a buffer, turn off various hooks.  This will cause
	;; the mod hooks to be buffered up during the insert, but
	;; prevent tools like font-lock from fontifying mid-template.
	;; Especially important during insertion of complex comments that
	;; cause the new font-lock to comment-color stuff after the inserted
	;; comment.
	;;
	;; I'm not sure about the motion hooks.  It seems like a good
	;; idea though.
	;;
	;; Borrowed these concepts out of font-lock.
	;;
	;; I tried `combine-after-change-calls', but it did not have
	;; the effect I wanted.
	(let ((start (point)))
	  (let ((inhibit-point-motion-hooks t)
		(inhibit-modification-hooks t)
		)
	    (srecode--insert-into-buffer template dictionary)
	    )
	  ;; Now call those after change functions.
	  (run-hook-with-args 'after-change-functions
			      start (point) 0)
	  )
      (srecode-insert-method template dictionary))
    ;; Handle specialization of the POINT inserter.
    (when (and (bufferp standard-output)
	       (slot-boundp 'srecode-template-inserter-point 'point)
	       )
      (set-buffer standard-output)
      (setq end-mark (point-marker))
      (goto-char  (oref srecode-template-inserter-point point)))
    (oset-default 'srecode-template-inserter-point point eieio-unbound)

    ;; Return the end-mark.
    (or end-mark (point)))
  )

(defun srecode--insert-into-buffer (template dictionary)
  "Insert a TEMPLATE with DICTIONARY into a buffer.
Do not call this function yourself.  Instead use:
  `srecode-insert' - Inserts by name.
  `srecode-insert-fcn' - Insert with objects.
This function handles the case from one of the above functions when
the template is inserted into a buffer.  It looks
at `srecode-insert-ask-variable-method' to decide if unbound dictionary
entries ask questions or insert editable fields.

Buffer based features related to change hooks is handled one level up."
  ;; This line prevents the field archive from being let bound
  ;; while the field insert tool is loaded via autoloads during
  ;; the insert.
  (when (eq srecode-insert-ask-variable-method 'field)
    (require 'srecode/fields))

  (let ((srecode-field-archive nil) ; Prevent field leaks during insert
	(start (point)) ; Beginning of the region.
	)
    ;; This sub-let scopes the 'in-progress' piece so we know
    ;; when to setup the end-template.
    (let ((srecode-insert-with-fields-in-progress
	   (if (eq srecode-insert-ask-variable-method 'field) t nil))
	  )
      (srecode-insert-method template dictionary)
      )
    ;; If we are not in-progress, and we insert fields, then
    ;; create the end-template with fields editable area.
    (when (and (not srecode-insert-with-fields-in-progress)
	       (eq srecode-insert-ask-variable-method 'field) ; Only if user asked
	       srecode-field-archive ; Only if there were fields created
	       )
      (let ((reg
	     ;; Create the field-driven editable area.
	     (srecode-template-inserted-region
	      "TEMPLATE" :start start :end (point))))
	(srecode-overlaid-activate reg))
      )
    ;; We return with 'point being the end of the template insertion
    ;; area.  Return value is not important.
    ))

;;; TEMPLATE ARGUMENTS
;;
;; Some templates have arguments.  Each argument is associated with
;; a function that can resolve the inputs needed.
(defun srecode-resolve-arguments (temp dict)
  "Resolve all the arguments needed by the template TEMP.
Apply anything learned to the dictionary DICT."
  (srecode-resolve-argument-list (oref temp args) dict temp))

(defun srecode-resolve-argument-list (args dict &optional temp)
  "Resolve arguments in the argument list ARGS.
ARGS is a list of symbols, such as :blank, or :file.
Apply values to DICT.
Optional argument TEMP is the template that is getting its arguments resolved."
  (let ((fcn nil))
    (while args
      (setq fcn (intern-soft (concat "srecode-semantic-handle-"
				     (symbol-name (car args)))))
      (if (not fcn)
	  (error "Error resolving template argument %S" (car args)))
      (if temp
	  (condition-case nil
	      ;; Allow some to accept a 2nd argument optionally.
	      ;; They throw an error if not available, so try again.
	      (funcall fcn dict temp)
	    (wrong-number-of-arguments (funcall fcn dict)))
	(funcall fcn dict))
      (setq args (cdr args)))
    ))

;;; INSERTION STACK & METHOD
;;
;; Code managing the top-level insert method and the current
;; insertion stack.
;;
(defmethod srecode-push ((st srecode-template))
  "Push the srecoder template ST onto the active stack."
  (oset st active (cons st (oref st active))))

(defmethod srecode-pop :STATIC ((st srecode-template))
  "Pop the srecoder template ST onto the active stack.
ST can be a class, or an object."
  (oset st active (cdr (oref st active))))

(defmethod srecode-peek :STATIC ((st srecode-template))
  "Fetch the topmost active template record.  ST can be a class."
  (car (oref st active)))

(defmethod srecode-insert-method ((st srecode-template) dictionary)
  "Insert the srecoder template ST."
  ;; Merge any template entries into the input dictionary.
  ;; This may happen twice since some templates arguments need
  ;; these dictionary values earlier, but these values always
  ;; need merging for template inserting in other templates.
  (when (slot-boundp st 'dictionary)
    (srecode-dictionary-merge dictionary (oref st dictionary)))
  ;; Do an insertion.
  (unwind-protect
      (let ((c (oref st code)))
	(srecode-push st)
	(srecode-insert-code-stream c dictionary))
    ;; Popping the stack is protected.
    (srecode-pop st)))

(defun srecode-insert-code-stream (code dictionary)
  "Insert the CODE from a template into `standard-output'.
Use DICTIONARY to resolve any macros."
  (while code
    (cond ((stringp (car code))
	   (princ (car code)))
	  (t
	   (srecode-insert-method (car code) dictionary)))
    (setq code (cdr code))))

;;; INSERTERS
;;
;; Specific srecode inserters.
;; The base class is from srecode-compile.
;;
;; Each inserter handles various macro codes from the template.
;; The `code' slot specifies a character used to identify which
;; inserter is to be created.
;;
(defclass srecode-template-inserter-newline (srecode-template-inserter)
  ((key :initform "\n"
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (hard :initform nil
	 :initarg :hard
	 :documentation
	 "Is this a hard newline (always inserted) or optional?
Optional newlines don't insert themselves if they are on a blank line
by themselves.")
   )
  "Insert a newline, and possibly do indenting.
Specify the :indent argument to enable automatic indentation when newlines
occur in your template.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-newline)
				  dictionary)
  "Insert the STI inserter."
  ;; To be safe, indent the previous line since the template will
  ;; change what is there to indent
  (let ((i (srecode-dictionary-lookup-name dictionary "INDENT"))
	(inbuff (bufferp standard-output))
	(doit t)
	(pm (point-marker)))
    (when (and inbuff (not (oref sti hard)))
      ;; If this is not a hard newline, we need do the calculation
      ;; and set "doit" to nil.
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) pm)
	(when (looking-at "\\s-*$")
	  (setq doit nil)))
      (goto-char pm)
      )
    ;; Do indentation regardless of the newline.
    (when (and (eq i t) inbuff)
      (indent-according-to-mode)
      (goto-char pm))

    (when doit
      (princ "\n")
      ;; Indent after the newline, particularly for numeric indents.
      (cond ((and (eq i t) (bufferp standard-output))
	     ;; WARNING - indent according to mode requires that standard-output
	     ;;           is a buffer!
	     ;; @todo - how to indent in a string???
	     (setq pm (point-marker))
	     (indent-according-to-mode)
	     (goto-char pm))
	    ((numberp i)
	     (princ (make-string i " ")))
	    ((stringp i)
	     (princ i))))))

(defmethod srecode-dump ((ins srecode-template-inserter-newline) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (when (oref ins hard)
    (princ " : hard")
    ))

(defclass srecode-template-inserter-blank (srecode-template-inserter)
   ((key :initform "\r"
	 :allocation :class
	 :documentation
	 "The character representing this inserter style.
Can't be blank, or it might be used by regular variable insertion.")
    (where :initform 'begin
	   :initarg :where
	   :documentation
	   "This should be 'begin or 'end, indicating where to insert a CR.
When set to 'begin, it will insert a CR if we are not at 'bol'.
When set to 'end it will insert a CR if we are not at 'eol'.")
    ;; @TODO - Add slot and control for the number of blank
    ;;         lines before and after point.
   )
   "Insert a newline before and after a template, and possibly do indenting.
Specify the :blank argument to enable this inserter.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-blank)
				  dictionary)
  "Make sure there is no text before or after point."
  (let ((i (srecode-dictionary-lookup-name dictionary "INDENT"))
	(inbuff (bufferp standard-output))
	(pm (point-marker)))
    (when (and inbuff
	       ;; Don't do this if we are not the active template.
	       (= (length (oref srecode-template active)) 1))

      (when (and (eq i t) inbuff (not (eq (oref sti where) 'begin)))
	(indent-according-to-mode)
	(goto-char pm))

      (cond ((and (eq (oref sti where) 'begin) (not (bolp)))
	     (princ "\n"))
	    ((eq (oref sti where) 'end)
	     ;; If there is whitespace after pnt, then clear it out.
	     (when (looking-at "\\s-*$")
	       (delete-region (point) (point-at-eol)))
	     (when (not (eolp))
	       (princ "\n")))
	    )
      (setq pm (point-marker))
      (when (and (eq i t) inbuff (not (eq (oref sti where) 'end)))
	(indent-according-to-mode)
	(goto-char pm))
      )))

(defclass srecode-template-inserter-comment (srecode-template-inserter)
  ((key :initform ?!
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "Allow comments within template coding.  This inserts nothing.")

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter-comment)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (princ "   ")
  (princ escape-start)
  (princ "! Miscellaneous text commenting in your template. ")
  (princ escape-end)
  (terpri)
  )

(defmethod srecode-insert-method ((sti srecode-template-inserter-comment)
				  dictionary)
  "Don't insert anything for comment macros in STI."
  nil)


(defclass srecode-template-inserter-variable (srecode-template-inserter)
  ((key :initform nil
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style."))
  "Insert the value of a dictionary entry.
If there is no entry, insert nothing.")

(defvar srecode-inserter-variable-current-dictionary nil
  "The active dictionary when calling a variable filter.")

(defmethod srecode-insert-variable-secondname-handler
  ((sti srecode-template-inserter-variable) dictionary value secondname)
  "For VALUE handle SECONDNAME behaviors for this variable inserter.
Return the result as a string.
By default, treat as a function name.
If SECONDNAME is nil, return VALUE."
  (if secondname
      (let ((fcnpart (read secondname)))
	(if (fboundp fcnpart)
	    (let ((srecode-inserter-variable-current-dictionary dictionary))
	      (funcall fcnpart value))
	  ;; Else, warn.
	  (error "Variable insertion second arg %s is not a function"
		 secondname)))
    value))

(defmethod srecode-insert-method ((sti srecode-template-inserter-variable)
				  dictionary)
  "Insert the STI inserter."
  ;; Convert the name into a name/fcn pair
  (let* ((name (oref sti :object-name))
	 (fcnpart (oref sti :secondname))
	 (val (srecode-dictionary-lookup-name
	       dictionary name))
	 (do-princ t)
	 )
    ;; Alert if a macro wasn't found.
    (when (not val)
      (message "Warning: macro %S was not found in the dictionary." name)
      (setq val ""))
    ;; If there was a functional part, call that function.
    (cond ;; Strings
       ((stringp val)
	(setq val (srecode-insert-variable-secondname-handler
		   sti dictionary val fcnpart)))
       ;; Compound data value
       ((srecode-dictionary-compound-value-child-p val)
	;; Force FCN to be a symbol
	(when fcnpart (setq fcnpart (read fcnpart)))
	;; Convert compound value to a string with the fcn.
	(setq val (srecode-compound-toString val fcnpart dictionary))
	;; If the value returned is nil, then it may be a special
	;; field inserter that requires us to set do-princ to nil.
	(when (not val)
	  (setq do-princ nil)
	  )
	)
       ;; Dictionaries... not allowed in this style
       ((srecode-dictionary-child-p val)
	(error "Macro %s cannot insert a dictionary - use section macros instead"
	       name))
       ;; Other stuff... convert
       (t
	(error "Macro %s cannot insert arbitrary data" name)
	;;(if (and val (not (stringp val)))
	;;    (setq val (format "%S" val))))
	))
    ;; Output the dumb thing unless the type of thing specifically
    ;; did the inserting for us.
    (when do-princ
      (princ val))))

(defclass srecode-template-inserter-ask (srecode-template-inserter-variable)
  ((key :initform ??
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (prompt :initarg :prompt
	   :initform nil
	   :documentation
	   "The prompt used to query for this dictionary value.")
   (defaultfcn :initarg :defaultfcn
	       :initform nil
	       :documentation
	       "The function which can calculate a default value.")
   (read-fcn :initarg :read-fcn
	     :initform 'read-string
	     :documentation
	     "The function used to read in the text for this prompt.")
   )
  "Insert the value of a dictionary entry.
If there is no entry, prompt the user for the value to use.
The prompt text used is derived from the previous PROMPT command in the
template file.")

(defmethod srecode-inserter-apply-state
  ((ins srecode-template-inserter-ask) STATE)
  "For the template inserter INS, apply information from STATE.
Loop over the prompts to see if we have a match."
  (let ((prompts (oref STATE prompts))
	)
    (while prompts
      (when (string= (semantic-tag-name (car prompts))
		     (oref ins :object-name))
	(oset ins :prompt
	      (semantic-tag-get-attribute (car prompts) :text))
	(oset ins :defaultfcn
	      (semantic-tag-get-attribute (car prompts) :default))
	(oset ins :read-fcn
	      (or (semantic-tag-get-attribute (car prompts) :read)
		  'read-string))
	)
      (setq prompts (cdr prompts)))
    ))

(defmethod srecode-insert-method ((sti srecode-template-inserter-ask)
				  dictionary)
  "Insert the STI inserter."
  (let ((val (srecode-dictionary-lookup-name
	      dictionary (oref sti :object-name))))
    (if val
	;; Does some extra work.  Oh well.
	(call-next-method)

      ;; How is our -ask value determined?
      (if srecode-insert-with-fields-in-progress
	  ;; Setup editable fields.
	  (setq val (srecode-insert-method-field sti dictionary))
	;; Ask the question...
	(setq val (srecode-insert-method-ask sti dictionary)))

      ;; After asking, save in the dictionary so that
      ;; the user can use the same name again later.
      (srecode-dictionary-set-value
       (srecode-root-dictionary dictionary)
       (oref sti :object-name) val)

      ;; Now that this value is safely stowed in the dictionary,
      ;; we can do what regular inserters do.
      (call-next-method))))

(defmethod srecode-insert-ask-default ((sti srecode-template-inserter-ask)
				       dictionary)
  "Derive the default value for an askable inserter STI.
DICTIONARY is used to derive some values."
  (let ((defaultfcn (oref sti :defaultfcn)))
    (cond ((stringp defaultfcn)
	   defaultfcn)
	  ((functionp defaultfcn)
	   (funcall defaultfcn))
	  ((and (listp defaultfcn)
		(eq (car defaultfcn) 'macro))
	   (srecode-dictionary-lookup-name
	    dictionary (cdr defaultfcn)))
	  ((null defaultfcn)
	   "")
	  (t
	   (error "Unknown default for prompt: %S"
		  defaultfcn)))))

(defmethod srecode-insert-method-ask ((sti srecode-template-inserter-ask)
				      dictionary)
  "Do the \"asking\" for the template inserter STI.
Use DICTIONARY to resolve values."
  (let* ((prompt (oref sti prompt))
	 (default (srecode-insert-ask-default sti dictionary))
	 (reader (oref sti :read-fcn))
	 (val nil)
	 )
    (cond ((eq reader 'y-or-n-p)
	   (if (y-or-n-p (or prompt
			     (format "%s? "
				     (oref sti :object-name))))
	       (setq val default)
	     (setq val "")))
	  ((eq reader 'read-char)
	   (setq val (format
		      "%c"
		      (read-char (or prompt
				     (format "Char for %s: "
					     (oref sti :object-name))))))
	   )
	  (t
	   (save-excursion
	     (setq val (funcall reader
				(or prompt
				    (format "Specify %s: "
					    (oref sti :object-name)))
				default
				)))))
    ;; Return our derived value.
    val)
  )

(defmethod srecode-insert-method-field ((sti srecode-template-inserter-ask)
					dictionary)
  "Create an editable field for the template inserter STI.
Use DICTIONARY to resolve values."
  (let* ((default (srecode-insert-ask-default sti dictionary))
	 (compound-value
	  (srecode-field-value (oref sti :object-name)
			       :firstinserter sti
			       :defaultvalue default))
	 )
    ;; Return this special compound value as the thing to insert.
    ;; This special compound value will repeat our asked question
    ;; across multiple locations.
    compound-value))

(defmethod srecode-dump ((ins srecode-template-inserter-ask) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (princ " : \"")
  (princ (oref ins prompt))
  (princ "\"")
  )

(defclass srecode-template-inserter-width (srecode-template-inserter-variable)
  ((key :initform ?|
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "Inserts the value of a dictionary variable with a specific width.
The second argument specifies the width, and a pad, separated by a colon.
Thus a specification of `10:left' will insert the value of A
to 10 characters, with spaces added to the left.  Use `right' for adding
spaces to the right.")

(defmethod srecode-insert-variable-secondname-handler
  ((sti srecode-template-inserter-width) dictionary value width)
  "For VALUE handle WIDTH behaviors for this variable inserter.
Return the result as a string.
By default, treat as a function name."
  (if width
      ;; Trim or pad to new length
      (let* ((split (split-string width ":"))
	     (width (string-to-number (nth 0 split)))
	     (second (nth 1 split))
	     (pad (cond ((or (null second) (string= "right" second))
			 'right)
			((string= "left" second)
			 'left)
			(t
			 (error "Unknown pad type %s" second)))))
	(if (>= (length value) width)
	    ;; Simple case - too long.
	    (substring value 0 width)
	  ;; We need to pad on one side or the other.
	  (let ((padchars (make-string (- width (length value)) ? )))
	    (if (eq pad 'left)
		(concat padchars value)
	      (concat value padchars)))))
    (error "Width not specified for variable/width inserter")))

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter-width)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (princ "   ")
  (princ escape-start)
  (princ "|A:10:right")
  (princ escape-end)
  (terpri)
  )

(defvar srecode-template-inserter-point-override nil
  "Point-positioning method for the SRecode template inserter.
When nil, perform normal point-positioning behavior.
When the value is a cons cell (DEPTH . FUNCTION), call FUNCTION
instead, unless the template nesting depth, measured
by (length (oref srecode-template active)), is greater than
DEPTH.")


(defclass srecode-template-inserter-point (srecode-template-inserter)
  ((key :initform ?^
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (point :type (or null marker)
	  :allocation :class
	  :documentation
	  "Record the value of (point) in this class slot.
It is the responsibility of the inserter algorithm to clear this
after a successful insertion."))
  "Record the value of (point) when inserted.
The cursor is placed at the ^ macro after insertion.
Some inserter macros, such as `srecode-template-inserter-include-wrap'
will place text at the ^ macro from the included macro.")

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter-point)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (princ "   ")
  (princ escape-start)
  (princ "^")
  (princ escape-end)
  (terpri)
  )

(defmethod srecode-insert-method ((sti srecode-template-inserter-point)
				  dictionary)
  "Insert the STI inserter.
Save point in the class allocated 'point' slot.
If `srecode-template-inserter-point-override' non-nil then this
generalized marker will do something else.  See
`srecode-template-inserter-include-wrap' as an example."
  ;; If `srecode-template-inserter-point-override' is non-nil, its car
  ;; is the maximum template nesting depth for which the override is
  ;; valid. Compare this to the actual template nesting depth and
  ;; maybe use the override function which is stored in the cdr.
  (if (and srecode-template-inserter-point-override
	   (<= (length (oref srecode-template active))
	       (car srecode-template-inserter-point-override)))
      ;; Disable the old override while we do this.
      (let ((over (cdr srecode-template-inserter-point-override))
	    (srecode-template-inserter-point-override nil))
	(funcall over dictionary))
    (oset sti point (point-marker))
    ))

(defclass srecode-template-inserter-subtemplate (srecode-template-inserter)
  ()
  "Wrap a section of a template under the control of a macro."
  :abstract t)

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter-subtemplate)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (call-next-method)
  (princ "     Template Text to control")
  (terpri)
  (princ "   ")
  (princ escape-start)
  (princ "/VARNAME")
  (princ escape-end)
  (terpri)
  )

(defmethod srecode-insert-subtemplate ((sti srecode-template-inserter-subtemplate)
				       dict slot)
  "Insert a subtemplate for the inserter STI with dictionary DICT."
  ;; make sure that only dictionaries are used.
  (when (not (srecode-dictionary-child-p dict))
    (error "Only section dictionaries allowed for %s"
	   (object-name-string sti)))
  ;; Output the code from the sub-template.
  (srecode-insert-method (slot-value sti slot) dict)
  )

(defmethod srecode-insert-method-helper ((sti srecode-template-inserter-subtemplate)
					 dictionary slot)
  "Do the work for inserting the STI inserter.
Loops over the embedded CODE which was saved here during compilation.
The template to insert is stored in SLOT."
  (let ((dicts (srecode-dictionary-lookup-name
		dictionary (oref sti :object-name))))
    (when (not (listp dicts))
      (error "Cannot insert section %S from non-section variable."
	     (oref sti :object-name)))
    ;; If there is no section dictionary, then don't output anything
    ;; from this section.
    (while dicts
      (when (not (srecode-dictionary-p (car dicts)))
	(error "Cannot insert section %S from non-section variable."
	       (oref sti :object-name)))
      (srecode-insert-subtemplate sti (car dicts) slot)
      (setq dicts (cdr dicts)))))

(defmethod srecode-insert-method ((sti srecode-template-inserter-subtemplate)
				  dictionary)
  "Insert the STI inserter.
Calls back to `srecode-insert-method-helper' for this class."
  (srecode-insert-method-helper sti dictionary 'template))


(defclass srecode-template-inserter-section-start (srecode-template-inserter-subtemplate)
  ((key :initform ?#
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (template :initarg :template
	     :documentation
	     "A template used to frame the codes from this inserter.")
   )
  "Apply values from a sub-dictionary to a template section.
The dictionary saved at the named dictionary entry will be
applied to the text between the section start and the
`srecode-template-inserter-section-end' macro.")

(defmethod srecode-parse-input ((ins srecode-template-inserter-section-start)
				tag input STATE)
  "For the section inserter INS, parse INPUT.
Shorten input until the END token is found.
Return the remains of INPUT."
  (let* ((out (srecode-compile-split-code tag input STATE
					  (oref ins :object-name))))
    (oset ins template (srecode-template
			(object-name-string ins)
			:context nil
			:args nil
			:code (cdr out)))
    (car out)))

(defmethod srecode-dump ((ins srecode-template-inserter-section-start) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (princ "\n")
  (srecode-dump-code-list (oref (oref ins template) code)
			  (concat indent "    "))
  )

(defclass srecode-template-inserter-section-end (srecode-template-inserter)
  ((key :initform ?/
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "All template segments between the section-start and section-end
are treated specially.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-section-end)
				  dictionary)
  "Insert the STI inserter."
  )

(defmethod srecode-match-end ((ins srecode-template-inserter-section-end) name)

  "For the template inserter INS, do I end a section called NAME?"
  (string= name (oref ins :object-name)))

(defclass srecode-template-inserter-include (srecode-template-inserter-subtemplate)
  ((key :initform ?>
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (includedtemplate
    :initarg :includedtemplate
    :documentation
    "The template included for this inserter."))
   "Include a different template into this one.
The included template will have additional dictionary entries from the subdictionary
stored specified by this macro.")

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter-include)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (princ "   ")
  (princ escape-start)
  (princ ">DICTNAME:contextname:templatename")
  (princ escape-end)
  (terpri)
  )

(defmethod srecode-insert-include-lookup ((sti srecode-template-inserter-include)
					  dictionary)
  "For the template inserter STI, lookup the template to include.
Finds the template with this macro function part and stores it in
this template instance."
  (let* ((templatenamepart (oref sti :secondname))
	 )
    ;; If there was no template name, throw an error
    (if (not templatenamepart)
	(error "Include macro %s needs a template name" (oref sti :object-name)))

    ;; NOTE: We used to cache the template and not look it up a second time,
    ;; but changes in the template tables can change which template is
    ;; eventually discovered, so now we always lookup that template.

    ;; Calculate and store the discovered template
    (let ((tmpl (srecode-template-get-table (srecode-table)
					    templatenamepart))
	  (active (oref srecode-template active))
	  ctxt)
      (when (not tmpl)
	;; If it isn't just available, scan back through
	;; the active template stack, searching for a matching
	;; context.
	(while (and (not tmpl) active)
	  (setq ctxt (oref (car active) context))
	  (setq tmpl (srecode-template-get-table (srecode-table)
						 templatenamepart
						 ctxt))
	  (when (not tmpl)
	    (when (slot-boundp (car active) 'table)
	      (let ((app (oref (oref (car active) table) application)))
		(when app
		  (setq tmpl (srecode-template-get-table
			      (srecode-table)
			      templatenamepart
			      ctxt app)))
		)))
	  (setq active (cdr active)))
	(when (not tmpl)
	  ;; If it wasn't in this context, look to see if it
	  ;; defines its own context
	  (setq tmpl (srecode-template-get-table (srecode-table)
						 templatenamepart)))
	)

      ;; Store the found template into this object for later use.
      (oset sti :includedtemplate tmpl))

    (if (not (oref sti includedtemplate))
	;; @todo - Call into a debugger to help find the template in question.
	(error "No template \"%s\" found for include macro `%s'"
	       templatenamepart (oref sti :object-name)))
    ))

(defmethod srecode-insert-method ((sti srecode-template-inserter-include)
				  dictionary)
  "Insert the STI inserter.
Finds the template with this macro function part, and inserts it
with the dictionaries found in the dictionary."
  (srecode-insert-include-lookup sti dictionary)
  ;; Insert the template.
  ;; Our baseclass has a simple way to do this.
  (if (srecode-dictionary-lookup-name dictionary (oref sti :object-name))
      ;; If we have a value, then call the next method
      (srecode-insert-method-helper sti dictionary 'includedtemplate)
    ;; If we don't have a special dictionary, then just insert with the
    ;; current dictionary.
    (srecode-insert-subtemplate sti dictionary 'includedtemplate))
  )

;;
;; This template combines the include template and the sectional template.
;; It will first insert the included template, then insert the embedded
;; template wherever the $^$ in the included template was.
;;
;; Since it uses dual inheritance, it will magically get the end-matching
;; behavior of #, with the including feature of >.
;;
(defclass srecode-template-inserter-include-wrap (srecode-template-inserter-include srecode-template-inserter-section-start)
   ((key :initform ?<
	 :allocation :class
	 :documentation
	 "The character code used to identify inserters of this style.")
    )
   "Include a different template into this one, and add text at the ^ macro.
The included template will have additional dictionary entries from the subdictionary
stored specified by this macro.  If the included macro includes a ^ macro,
then the text between this macro and the end macro will be inserted at
the ^ macro.")

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter-include-wrap)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (princ "   ")
  (princ escape-start)
  (princ "<DICTNAME:contextname:templatename")
  (princ escape-end)
  (terpri)
  (princ "     Template Text to insert at ^ macro")
  (terpri)
  (princ "   ")
  (princ escape-start)
  (princ "/DICTNAME")
  (princ escape-end)
  (terpri)
  )

(defmethod srecode-insert-method ((sti srecode-template-inserter-include-wrap)
				  dictionary)
  "Insert the template STI.
This will first insert the include part via inheritance, then
insert the section it wraps into the location in the included
template where a ^ inserter occurs."
  ;; Step 1: Look up the included inserter
  (srecode-insert-include-lookup sti dictionary)
  ;; Step 2: Temporarily override the point inserter.
  ;; We bind `srecode-template-inserter-point-override' to a cons cell
  ;; (DEPTH . FUNCTION) that has the maximum template nesting depth,
  ;; for which the override is valid, in DEPTH and a lambda function
  ;; which implements the wrap insertion behavior in FUNCTION. The
  ;; maximum valid nesting depth is just the current depth + 1.
  (let ((srecode-template-inserter-point-override
	 (lexical-let ((inserter1 sti))
	   (cons
	    ;; DEPTH
	    (+ (length (oref srecode-template active)) 1)
	    ;; FUNCTION
	    (lambda (dict)
	      (let ((srecode-template-inserter-point-override nil))
		(if (srecode-dictionary-lookup-name
		     dict (oref inserter1 :object-name))
		    ;; Insert our sectional part with looping.
		    (srecode-insert-method-helper
		     inserter1 dict 'template)
		  ;; Insert our sectional part just once.
		  (srecode-insert-subtemplate
		   inserter1 dict 'template))))))))
    ;; Do a regular insertion for an include, but with our override in
    ;; place.
    (call-next-method)))

(provide 'srecode/insert)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/insert"
;; End:

;;; srecode/insert.el ends here
