;;; srecode/compile --- Compilation of srecode template files.

;; Copyright (C) 2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: codegeneration

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
;; Compile a Semantic Recoder template file.
;;
;; Template files are parsed using a Semantic/Wisent parser into
;; a tag table.  The code therein is then further parsed down using
;; a regular expression parser.
;;
;; The output are a series of EIEIO objects which represent the
;; templates in a way that could be inserted later.

(eval-when-compile (require 'cl))
(require 'semantic)
(require 'eieio)
(require 'eieio-base)
(require 'srecode/table)
(require 'srecode/dictionary)

(declare-function srecode-template-inserter-newline-child-p "srecode/insert"
		  t t)

;;; Code:

;;; Template Class
;;
;; Templates describe a pattern of text that can be inserted into a
;; buffer.
;;
(defclass srecode-template (eieio-named)
  ((context :initarg :context
	    :initform nil
	    :documentation
	    "Context this template belongs to.")
   (args :initarg :args
	 :documentation
	 "List of arguments that this template requires.")
   (code :initarg :code
	 :documentation
	 "Compiled text from the template.")
   (dictionary :initarg :dictionary
	       :type (or null srecode-dictionary)
	       :documentation
	       "List of section dictionaries.
The compiled template can contain lists of section dictionaries,
or values that are expected to be passed down into different
section macros.  The template section dictionaries are merged in with
any incoming dictionaries values.")
   (binding :initarg :binding
	    :documentation
	    "Preferred keybinding for this template in `srecode-minor-mode-map'.")
   (active :allocation :class
	   :initform nil
	   :documentation
	   "During template insertion, this is the stack of active templates.
The top-most template is the 'active' template.  Use the accessor methods
for push, pop, and peek for the active template.")
   (table :initarg :table
	  :documentation
	  "The table this template lives in.")
   )
  "Class defines storage for semantic recoder templates.")

(defun srecode-flush-active-templates ()
  "Flush the active template storage.
Useful if something goes wrong in SRecode, and the active template
stack is broken."
  (interactive)
  (if (oref srecode-template active)
      (when (y-or-n-p (format "%d active templates.  Flush? "
			      (length (oref srecode-template active))))
	(oset-default srecode-template active nil))
    (message "No active templates to flush."))
  )

;;; Inserters
;;
;; Each inserter object manages a different thing that
;; might be inserted into a template output stream.
;;
;; The 'srecode-insert-method' on each inserter does the actual
;; work, and the smaller, simple inserter object is saved in
;; the compiled templates.
;;
;; See srecode/insert.el for the specialized classes.
;;
(defclass srecode-template-inserter (eieio-named)
  ((secondname :initarg :secondname
	       :type (or null string)
	       :documentation
	       "If there is a colon in the inserter's name, it represents
additional static argument data."))
  "This represents an item to be inserted via a template macro.
Plain text strings are not handled via this baseclass."
  :abstract t)

(defmethod srecode-parse-input ((ins srecode-template-inserter)
				tag input STATE)
  "For the template inserter INS, parse INPUT.
Shorten input only by the amount needed.
Return the remains of INPUT.
STATE is the current compilation state."
  input)

(defmethod srecode-match-end ((ins srecode-template-inserter) name)
  "For the template inserter INS, do I end a section called NAME?"
  nil)

(defmethod srecode-inserter-apply-state ((ins srecode-template-inserter) STATE)
  "For the template inserter INS, apply information from STATE."
  nil)

(defmethod srecode-inserter-prin-example :STATIC ((ins srecode-template-inserter)
						  escape-start escape-end)
  "Insert an example using inserter INS.
Arguments ESCAPE-START and ESCAPE-END are the current escape sequences in use."
  (princ "   ")
  (princ escape-start)
  (when (and (slot-exists-p ins 'key) (oref ins key))
    (princ (format "%c" (oref ins key))))
  (princ "VARNAME")
  (princ escape-end)
  (terpri)
  )


;;; Compile State
(defclass srecode-compile-state ()
  ((context :initform "declaration"
	    :documentation "The active context.")
   (prompts :initform nil
	    :documentation "The active prompts.")
   (escape_start :initform "{{"
		 :documentation "The starting escape sequence.")
   (escape_end :initform "}}"
	       :documentation "The ending escape sequence.")
   )
  "Current state of the compile.")

(defmethod srecode-compile-add-prompt ((state srecode-compile-state)
				       prompttag)
  "Add PROMPTTAG to the current list of prompts."
  (with-slots (prompts) state
      (let ((match (assoc (semantic-tag-name prompttag) prompts))
	    (newprompts prompts))
	(when match
	  (let ((tmp prompts))
	    (setq newprompts nil)
	    (while tmp
	      (when (not (string= (car (car tmp))
				  (car prompttag)))
		(setq newprompts (cons (car tmp)
				       newprompts)))
	      (setq tmp (cdr tmp)))))
	(setq prompts (cons prompttag newprompts)))
      ))

;;;  TEMPLATE COMPILER
;;
(defun srecode-compile-file (fname)
  "Compile the templates from the file FNAME."
  (let ((peb (get-file-buffer fname)))
    (save-excursion
      ;; Make whatever it is local.
      (if (not peb)
	  (set-buffer (semantic-find-file-noselect fname))
	(set-buffer peb))
      ;; Do the compile.
      (unless (semantic-active-p)
	(semantic-new-buffer-fcn))
      (srecode-compile-templates)
      ;; Trash the buffer if we had to read it in.
      (if (not peb)
	  (kill-buffer (current-buffer)))
      )))

;;;###autoload
(defun srecode-compile-templates ()
  "Compile a semantic recode template file into a mode-local variable."
  (interactive)
  (require 'srecode/insert)
  (message "Compiling template %s..."
	   (file-name-nondirectory (buffer-file-name)))
  (let ((tags (semantic-fetch-tags))
	(tag nil)
	(class nil)
	(table nil)
	(STATE (srecode-compile-state (file-name-nondirectory
				       (buffer-file-name))))
	(mode nil)
	(application nil)
	(priority nil)
	(project nil)
	(vars nil)
	)

    ;;
    ;; COMPILE
    ;;
    (while tags
      (setq tag (car tags)
	    class (semantic-tag-class tag))
      ;; What type of item is it?
      (cond
       ;; CONTEXT tags specify the context all future tags
       ;; belong to.
       ((eq class 'context)
	(oset STATE context (semantic-tag-name tag))
	)

       ;; PROMPT tags specify prompts for dictionary ? inserters
       ;; which appear in the following templates
       ((eq class 'prompt)
	(srecode-compile-add-prompt STATE tag)
	)

       ;; VARIABLE tags can specify operational control
       ((eq class 'variable)
	(let* ((name (semantic-tag-name tag))
	       (value (semantic-tag-variable-default tag))
	       (firstvalue (car value)))
	  ;; If it is a single string, and one value, then
	  ;; look to see if it is one of our special variables.
	  (if (and (= (length value) 1) (stringp firstvalue))
	      (cond ((string= name "mode")
		     (setq mode (intern firstvalue)))
		    ((string= name "escape_start")
		     (oset STATE escape_start firstvalue)
		     )
		    ((string= name "escape_end")
		     (oset STATE escape_end firstvalue)
		     )
		    ((string= name "application")
		     (setq application (read firstvalue)))
		    ((string= name "priority")
		     (setq priority (read firstvalue)))
		    ((string= name "project")
		     (setq project firstvalue))
		    (t
		     ;; Assign this into some table of variables.
		     (setq vars (cons (cons name firstvalue) vars))
		     ))
	    ;; If it isn't a single string, then the value of the
	    ;; variable belongs to a compound dictionary value.
	    ;;
	    ;; Create a compound dictionary value from "value".
	    (require 'srecode/dictionary)
	    (let ((cv (srecode-dictionary-compound-variable
		       name :value value)))
	      (setq vars (cons (cons name cv) vars)))
	    ))
	)

       ;; FUNCTION tags are really templates.
       ((eq class 'function)
	(setq table (cons (srecode-compile-one-template-tag tag STATE)
			  table))
	)

       ;; Ooops
       (t (error "Unknown TAG class %s" class))
       )
      ;; Continue
      (setq tags (cdr tags)))

    ;; MSG - Before install since nreverse whacks our list.
    (message "%d templates compiled for %s"
	     (length table) mode)

    ;;
    ;; APPLY TO MODE
    ;;
    (if (not mode)
	(error "You must specify a MODE for your templates"))

    ;;
    ;; Calculate priority
    ;;
    (if (not priority)
	(let ((d (expand-file-name (file-name-directory (buffer-file-name))))
	      (sd (expand-file-name (file-name-directory (locate-library "srecode"))))
	      (defaultdelta (if (eq mode 'default) 0 10)))
	  ;; @TODO :   WHEN INTEGRATING INTO EMACS
	  ;;   The location of Emacs default templates needs to be specified
	  ;;   here to also have a lower priority.
	  (if (string-match (concat "^" sd) d)
	      (setq priority (+ 30 defaultdelta))
	    ;; If the user created template is for a project, then
	    ;; don't add as much as if it is unique to just some user.
	    (if (stringp project)
		(setq priority (+ 50 defaultdelta))
	      (setq priority (+ 80 defaultdelta))))
	  (message "Templates %s has estimated priority of %d"
		   (file-name-nondirectory (buffer-file-name))
		   priority))
      (message "Compiling templates %s priority %d... done!"
	       (file-name-nondirectory (buffer-file-name))
	       priority))

    ;; Save it up!
    (srecode-compile-template-table table mode priority application project vars)
    )
)

(defun srecode-compile-one-template-tag (tag state)
  "Compile a template tag TAG into a srecode template object.
STATE is the current compile state as an object of class
`srecode-compile-state'."
  (let* ((context   (oref state context))
	 (code      (cdr (srecode-compile-split-code
			  tag (semantic-tag-get-attribute tag :code)
			  state)))
	 (args      (semantic-tag-function-arguments tag))
	 (binding   (semantic-tag-get-attribute tag :binding))
	 (dict-tags (semantic-tag-get-attribute tag :dictionaries))
	 (root-dict (when dict-tags
		      (srecode-create-dictionaries-from-tags
		       dict-tags state)))
	 (addargs))
    ;; Examine arguments.
    (dolist (arg args)
      (let ((symbol (intern arg)))
	(push symbol addargs)

	;; If we have a wrap, then put wrap inserters on both ends of
	;; the code.
	(when (eq symbol :blank)
	  (setq code (append
		      (list (srecode-compile-inserter
			     "BLANK"
			     "\r"
			     state
			     :secondname nil
			     :where 'begin))
		      code
		      (list (srecode-compile-inserter
			     "BLANK"
			     "\r"
			     state
			     :secondname nil
			     :where 'end)))))))

    ;; Construct and return the template object.
    (srecode-template (semantic-tag-name tag)
		      :context    context
		      :args       (nreverse addargs)
		      :dictionary root-dict
		      :binding    binding
		      :code       code))
  )

(defun srecode-compile-do-hard-newline-p (comp)
  "Examine COMP to decide if the upcoming newline should be hard.
It is hard if the previous inserter is a newline object."
  (while (and comp (stringp (car comp)))
    (setq comp (cdr comp)))
  (or (not comp)
      (require 'srecode/insert)
      (srecode-template-inserter-newline-child-p (car comp))))

(defun srecode-compile-split-code (tag str STATE
				       &optional end-name)
  "Split the code for TAG into something templatable.
STR is the string of code from TAG to split.
STATE is the current compile state.
ESCAPE_START and ESCAPE_END are regexps that indicate the beginning
escape character, and end escape character pattern for expandable
macro names.
Optional argument END-NAME specifies the name of a token upon which
parsing should stop.
If END-NAME is specified, and the input string"
  (let* ((what str)
	 (end-token nil)
	 (comp nil)
	 (regex (concat "\n\\|" (regexp-quote (oref STATE escape_start))))
	 (regexend (regexp-quote (oref STATE escape_end)))
	 )
    (while (and what (not end-token))
      (cond
       ((string-match regex what)
	(let* ((prefix (substring what 0 (match-beginning 0)))
	       (match (substring what
				 (match-beginning 0)
				 (match-end 0)))
	       (namestart (match-end 0))
	       (junk (string-match regexend what namestart))
	       end tail name key)
	  ;; Add string to compiled output
	  (when (> (length prefix) 0)
	    (setq comp (cons prefix comp)))
	  (if (string= match "\n")
	      ;; Do newline thingy.
	      (let ((new-inserter
		     (srecode-compile-inserter
		      "INDENT"
		      "\n"
		      STATE
		      :secondname nil
		      ;; This newline is "hard" meaning ALWAYS do it
		      ;; if the previous entry is also a newline.
		      ;; Without it, user entered blank lines will be
		      ;; ignored.
		      :hard (srecode-compile-do-hard-newline-p comp)
		      )))
		;; Trim WHAT back.
		(setq what (substring what namestart))
		(when (> (length what) 0)
		  ;; make the new inserter, but only if we aren't last.
		  (setq comp (cons new-inserter comp))
		  ))
	    ;; Regular inserter thingy.
	    (setq end (if junk
			  (match-beginning 0)
			(error "Could not find end escape for %s"
			       (semantic-tag-name tag)))
		  tail (match-end 0))
	    (cond ((not end)
		   (error "No matching escape end for %s"
			  (semantic-tag-name tag)))
		  ((<= end namestart)
		   (error "Stray end escape for %s"
			  (semantic-tag-name tag)))
		  )
	    ;; Add string to compiled output
	    (setq name (substring what namestart end)
		  key nil)
	    ;; Trim WHAT back.
	    (setq what (substring what tail))
	    ;; Get the inserter
	    (let ((new-inserter
		   (srecode-compile-parse-inserter name STATE))
		  )
	      ;; If this is an end inserter, then assign into
	      ;; the end-token.
	      (if (srecode-match-end new-inserter end-name)
		  (setq end-token new-inserter))
	      ;; Add the inserter to our compilation stream.
	      (setq comp (cons new-inserter comp))
	      ;; Allow the inserter an opportunity to modify
	      ;; the input stream.
	      (setq what (srecode-parse-input new-inserter tag what
					      STATE))
	      )
	    )))
       (t
	(if end-name
	    (error "Unmatched section end %s" end-name))
	(setq comp (cons what comp)
	      what nil))))
    (cons what (nreverse comp))))

(defun srecode-compile-parse-inserter (txt STATE)
  "Parse the inserter TXT with the current STATE.
Return an inserter object."
  (let ((key (aref txt 0))
	name
	)
    (if (and (or (< key ?A) (> key ?Z))
	     (or (< key ?a) (> key ?z)) )
	(setq name (substring txt 1))
      (setq name txt
	    key nil))
    (let* ((junk (string-match ":" name))
	   (namepart (if junk
			 (substring name 0 (match-beginning 0))
		       name))
	   (secondname (if junk
			   (substring name (match-end 0))
			 nil))
	   (new-inserter (srecode-compile-inserter
			  namepart key STATE
			  :secondname secondname
			  )))
      ;; Return the new inserter
      new-inserter)))

(defun srecode-compile-inserter (name key STATE &rest props)
  "Create an srecode inserter object for some macro NAME.
KEY indicates a single character key representing a type
of inserter to create.
STATE is the current compile state.
PROPS are additional properties that might need to be passed
to the inserter constructor."
  ;;(message "Compile: %s %S" name props)
  (if (not key)
      (apply 'srecode-template-inserter-variable name props)
    (let ((classes (class-children srecode-template-inserter))
	  (new nil))
      ;; Loop over the various subclasses and
      ;; create the correct inserter.
      (while (and (not new) classes)
	(setq classes (append classes (class-children (car classes))))
	;; Do we have a match?
	(when (and (not (class-abstract-p (car classes)))
		   (equal (oref (car classes) key) key))
	  ;; Create the new class, and apply state.
	  (setq new (apply (car classes) name props))
	  (srecode-inserter-apply-state new STATE)
	  )
	(setq classes (cdr classes)))
      (if (not new) (error "SRECODE: Unknown macro code %S" key))
      new)))

(defun srecode-compile-template-table (templates mode priority application project vars)
  "Compile a list of TEMPLATES into an semantic recode table.
The table being compiled is for MODE, or the string \"default\".
PRIORITY is a numerical value that indicates this tables location
in an ordered search.
APPLICATION is the name of the application these templates belong to.
PROJECT is a directory name which these templates scope to.
A list of defined variables VARS provides a variable table."
  (let ((namehash (make-hash-table :test 'equal
				   :size (length templates)))
	(contexthash (make-hash-table :test 'equal :size 10))
	(lp templates)
	)

    (while lp

      (let* ((objname (oref (car lp) :object-name))
	     (context (oref (car lp) :context))
	     (globalname (concat context ":" objname))
	     )

	;; Place this template object into the global name hash.
	(puthash globalname (car lp) namehash)

	;; Place this template into the specific context name hash.
	(let ((hs (gethash context contexthash)))
	  ;; Make a new context if none was available.
	  (when (not hs)
	    (setq hs (make-hash-table :test 'equal :size 20))
	    (puthash context hs contexthash))
	  ;; Put into that context's hash.
	  (puthash objname (car lp) hs)
	  )

	(setq lp (cdr lp))))

    (when (stringp project)
      (setq project (expand-file-name project)))

    (let* ((table (srecode-mode-table-new mode (buffer-file-name)
		   :templates (nreverse templates)
		   :namehash namehash
		   :contexthash contexthash
		   :variables vars
		   :major-mode mode
		   :priority priority
		   :application application
		   :project project))
	   (tmpl (oref table templates)))
      ;; Loop over all the templates, and xref.
      (while tmpl
	(oset (car tmpl) :table table)
	(setq tmpl (cdr tmpl))))
    ))



;;; DEBUG
;;
;; Dump out information about the current srecoder compiled templates.
;;

(defmethod srecode-dump ((tmp srecode-template))
  "Dump the contents of the SRecode template tmp."
  (princ "== Template \"")
  (princ (object-name-string tmp))
  (princ "\" in context ")
  (princ (oref tmp context))
  (princ "\n")
  (when (oref tmp args)
    (princ "   Arguments: ")
    (prin1 (oref tmp args))
    (princ "\n"))
  (when (oref tmp dictionary)
    (princ "   Section Dictionaries:\n")
    (srecode-dump (oref tmp dictionary) 4)
    ;(princ "\n")
    )
  (when (and (slot-boundp tmp 'binding) (oref tmp binding))
    (princ "   Binding: ")
    (prin1 (oref tmp binding))
    (princ "\n"))
  (princ "   Compiled Codes:\n")
  (srecode-dump-code-list (oref tmp code) "    ")
  (princ "\n\n")
  )

(defun srecode-dump-code-list (code indent)
  "Dump the CODE from a template code list to standard output.
Argument INDENT specifies the indentation level for the list."
  (let ((i 1))
    (while code
      (princ indent)
      (prin1 i)
      (princ ") ")
      (cond ((stringp (car code))
	     (prin1 (car code)))
	    ((srecode-template-inserter-child-p (car code))
	     (srecode-dump (car code) indent))
	    (t
	     (princ "Unknown Code: ")
	     (prin1 (car code))))
      (setq code (cdr code)
	    i (1+ i))
      (when code
	(princ "\n"))))
  )

(defmethod srecode-dump ((ins srecode-template-inserter) indent)
  "Dump the state of the SRecode template inserter INS."
  (princ "INS: \"")
  (princ (object-name-string ins))
  (when (oref ins :secondname)
    (princ "\" : \"")
    (princ (oref ins :secondname)))
  (princ "\" type \"")
  (let* ((oc (symbol-name (object-class ins)))
	 (junk (string-match "srecode-template-inserter-" oc))
	 (on (if junk
		 (substring oc (match-end 0))
	       oc)))
    (princ on))
  (princ "\"")
  )

(provide 'srecode/compile)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/compile"
;; End:

;;; srecode/compile.el ends here
