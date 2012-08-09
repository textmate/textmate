;;; semantic/symref.el --- Symbol Reference API

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
;; Semantic Symbol Reference API.
;;
;; Semantic's native parsing tools do not handle symbol references.
;; Tracking such information is a task that requires a huge amount of
;; space and processing not appropriate for an Emacs Lisp program.
;;
;; Many desired tools used in refactoring, however, need to have
;; such references available to them.  This API aims to provide a
;; range of functions that can be used to identify references.  The
;; API is backed by an OO system that is used to allow multiple
;; external tools to provide the information.
;;
;; The default implementation uses a find/grep combination to do a
;; search.  This works ok in small projects.  For larger projects, it
;; is important to find an alternate tool to use as a back-end to
;; symref.
;;
;; See the command: `semantic-symref' for an example app using this api.
;;
;; TO USE THIS TOOL
;;
;; The following functions can be used to find different kinds of
;; references.
;;
;;  `semantic-symref-find-references-by-name'
;;  `semantic-symref-find-file-references-by-name'
;;  `semantic-symref-find-text'
;;
;; All the search routines return a class of type
;; `semantic-symref-result'.  You can reference the various slots, but
;; you will need the following methods to get extended information.
;;
;;  `semantic-symref-result-get-files'
;;  `semantic-symref-result-get-tags'
;;
;; ADD A NEW EXTERNAL TOOL
;;
;; To support a new external tool, subclass `semantic-symref-tool-baseclass'
;; and implement the methods.  The baseclass provides support for
;; managing external processes that produce parsable output.
;;
;; Your tool should then create an instance of `semantic-symref-result'.

(require 'semantic)

(defvar ede-minor-mode)
(declare-function data-debug-new-buffer "data-debug")
(declare-function data-debug-insert-object-slots "eieio-datadebug")
(declare-function ede-toplevel "ede/base")
(declare-function ede-project-root-directory "ede/files")
(declare-function ede-up-directory "ede/files")

;;; Code:
(defvar semantic-symref-tool 'detect
  "*The active symbol reference tool name.
The tool symbol can be 'detect, or a symbol that is the name of
a tool that can be used for symbol referencing.")
(make-variable-buffer-local 'semantic-symref-tool)

;;; TOOL SETUP
;;
(defvar semantic-symref-tool-alist
  '( ( (lambda (rootdir) (file-exists-p (expand-file-name "GPATH" rootdir))) .
       global)
     ( (lambda (rootdir) (file-exists-p (expand-file-name "ID" rootdir))) .
       idutils)
     ( (lambda (rootdir) (file-exists-p (expand-file-name "cscope.out" rootdir))) .
       cscope )
    )
  "Alist of tools usable by `semantic-symref'.
Each entry is of the form:
   ( PREDICATE . KEY )
Where PREDICATE is a function that takes a directory name for the
root of a project, and returns non-nil if the tool represented by KEY
is supported.

If no tools are supported, then 'grep is assumed.")

(defun semantic-symref-calculate-rootdir ()
  "Calculate the root directory for a symref search.
Start with and EDE project, or use the default directory."
  (let* ((rootproj (when (and (featurep 'ede) ede-minor-mode)
		     (ede-toplevel)))
	 (rootdirbase (if rootproj
			  (ede-project-root-directory rootproj)
			default-directory)))
    (if (and rootproj (condition-case nil
			  ;; Hack for subprojects.
			  (oref rootproj :metasubproject)
			(error nil)))
	(ede-up-directory rootdirbase)
      rootdirbase)))

(defun semantic-symref-detect-symref-tool ()
  "Detect the symref tool to use for the current buffer."
  (if (not (eq semantic-symref-tool 'detect))
      semantic-symref-tool
    ;; We are to perform a detection for the right tool to use.
    (let* ((rootdir (semantic-symref-calculate-rootdir))
	   (tools semantic-symref-tool-alist))
      (while (and tools (eq semantic-symref-tool 'detect))
	(when (funcall (car (car tools)) rootdir)
	  (setq semantic-symref-tool (cdr (car tools))))
	(setq tools (cdr tools)))

      (when (eq semantic-symref-tool 'detect)
	(setq semantic-symref-tool 'grep))

      semantic-symref-tool)))

(defun semantic-symref-instantiate (&rest args)
  "Instantiate a new symref search object.
ARGS are the initialization arguments to pass to the created class."
  (let* ((srt (symbol-name (semantic-symref-detect-symref-tool)))
	 (class (intern-soft (concat "semantic-symref-tool-" srt)))
	 (inst nil)
	 )
    (when (not (class-p class))
      (error "Unknown symref tool %s" semantic-symref-tool))
    (setq inst (apply 'make-instance class args))
    inst))

(defvar semantic-symref-last-result nil
  "The last calculated symref result.")

(defun semantic-symref-data-debug-last-result ()
  "Run the last symref data result in Data Debug."
  (interactive)
  (require 'eieio-datadebug)
  (if semantic-symref-last-result
      (progn
	(data-debug-new-buffer "*Symbol Reference ADEBUG*")
	(data-debug-insert-object-slots semantic-symref-last-result "]"))
    (message "Empty results.")))

;;; EXTERNAL API
;;

;;;###autoload
(defun semantic-symref-find-references-by-name (name &optional scope tool-return)
  "Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'.
TOOL-RETURN is an optional symbol, which will be assigned the tool used
to perform the search.  This was added for use by a test harness."
  (interactive "sName: ")
  (let* ((inst (semantic-symref-instantiate
		:searchfor name
		:searchtype 'symbol
		:searchscope (or scope 'project)
		:resulttype 'line))
	 (result (semantic-symref-get-result inst)))
    (when tool-return
      (set tool-return inst))
    (prog1
	(setq semantic-symref-last-result result)
      (when (called-interactively-p 'interactive)
	(semantic-symref-data-debug-last-result))))
  )

;;;###autoload
(defun semantic-symref-find-tags-by-name (name &optional scope)
  "Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'."
  (interactive "sName: ")
  (let* ((inst (semantic-symref-instantiate
		:searchfor name
		:searchtype 'tagname
		:searchscope (or scope 'project)
		:resulttype 'line))
	 (result (semantic-symref-get-result inst)))
    (prog1
	(setq semantic-symref-last-result result)
      (when (called-interactively-p 'interactive)
	(semantic-symref-data-debug-last-result))))
  )

;;;###autoload
(defun semantic-symref-find-tags-by-regexp (name &optional scope)
  "Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'."
  (interactive "sName: ")
  (let* ((inst (semantic-symref-instantiate
		:searchfor name
		:searchtype 'tagregexp
		:searchscope (or scope 'project)
		:resulttype 'line))
	 (result (semantic-symref-get-result inst)))
    (prog1
	(setq semantic-symref-last-result result)
      (when (called-interactively-p 'interactive)
	(semantic-symref-data-debug-last-result))))
  )

;;;###autoload
(defun semantic-symref-find-tags-by-completion (name &optional scope)
  "Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'."
  (interactive "sName: ")
  (let* ((inst (semantic-symref-instantiate
		:searchfor name
		:searchtype 'tagcompletions
		:searchscope (or scope 'project)
		:resulttype 'line))
	 (result (semantic-symref-get-result inst)))
    (prog1
	(setq semantic-symref-last-result result)
      (when (called-interactively-p 'interactive)
	(semantic-symref-data-debug-last-result))))
  )

;;;###autoload
(defun semantic-symref-find-file-references-by-name (name &optional scope)
  "Find a list of references to NAME in the current project.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'."
  (interactive "sName: ")
  (let* ((inst (semantic-symref-instantiate
		:searchfor name
		:searchtype 'regexp
		:searchscope (or scope 'project)
		:resulttype 'file))
	 (result (semantic-symref-get-result inst)))
    (prog1
	(setq semantic-symref-last-result result)
      (when (called-interactively-p 'interactive)
	(semantic-symref-data-debug-last-result))))
  )

;;;###autoload
(defun semantic-symref-find-text (text &optional scope)
  "Find a list of occurrences of TEXT in the current project.
TEXT is a regexp formatted for use with egrep.
Optional SCOPE specifies which file set to search.  Defaults to 'project.
Refers to `semantic-symref-tool', to determine the reference tool to use
for the current buffer.
Returns an object of class `semantic-symref-result'."
  (interactive "sEgrep style Regexp: ")
  (let* ((inst (semantic-symref-instantiate
		:searchfor text
		:searchtype 'regexp
		:searchscope (or scope 'project)
		:resulttype 'line))
	 (result (semantic-symref-get-result inst)))
    (prog1
	(setq semantic-symref-last-result result)
      (when (called-interactively-p 'interactive)
	(semantic-symref-data-debug-last-result))))
  )

;;; RESULTS
;;
;; The results class and methods provide features for accessing hits.
(defclass semantic-symref-result ()
  ((created-by :initarg :created-by
	       :type semantic-symref-tool-baseclass
	       :documentation
	       "Back-pointer to the symref tool creating these results.")
   (hit-files :initarg :hit-files
	      :type list
	      :documentation
	      "The list of files hit.")
   (hit-text :initarg :hit-text
	     :type list
	     :documentation
	     "If the result doesn't provide full lines, then fill in hit-text.
GNU Global does completion search this way.")
   (hit-lines :initarg :hit-lines
	      :type list
	      :documentation
	      "The list of line hits.
Each element is a cons cell of the form (LINE . FILENAME).")
   (hit-tags :initarg :hit-tags
	     :type list
	     :documentation
	     "The list of tags with hits in them.
Use the  `semantic-symref-hit-tags' method to get this list.")
   )
  "The results from a symbol reference search.")

(defmethod semantic-symref-result-get-files ((result semantic-symref-result))
  "Get the list of files from the symref result RESULT."
  (if (slot-boundp result :hit-files)
      (oref result hit-files)
    (let* ((lines  (oref result :hit-lines))
	   (files (mapcar (lambda (a) (cdr a)) lines))
	   (ans nil))
      (setq ans (list (car files))
	    files (cdr files))
      (dolist (F files)
	;; This algorithm for uniquifying the file list depends on the
	;; tool in question providing all the hits in the same file
	;; grouped together.
	(when (not (string= F (car ans)))
	  (setq ans (cons F ans))))
      (oset result hit-files (nreverse ans))
      )
    ))

(defmethod semantic-symref-result-get-tags ((result semantic-symref-result)
					    &optional open-buffers)
  "Get the list of tags from the symref result RESULT.
Optional OPEN-BUFFERS indicates that the buffers that the hits are
in should remain open after scanning.
Note: This can be quite slow if most of the hits are not in buffers
already."
  (if (and (slot-boundp result :hit-tags) (oref result hit-tags))
      (oref result hit-tags)
    ;; Calculate the tags.
    (let ((lines (oref result :hit-lines))
	  (txt (oref (oref result :created-by) :searchfor))
	  (searchtype (oref (oref result :created-by) :searchtype))
	  (ans nil)
	  (out nil)
	  (buffs-to-kill nil))
      (save-excursion
	(setq
	 ans
	 (mapcar
	  (lambda (hit)
	    (let* ((line (car hit))
		   (file (cdr hit))
		   (buff (get-file-buffer file))
		   (tag nil)
		   )
	      (cond
	       ;; We have a buffer already.  Check it out.
	       (buff
		(set-buffer buff))

	       ;; We have a table, but it needs a refresh.
	       ;; This means we should load in that buffer.
	       (t
		(let ((kbuff
		       (if open-buffers
			   ;; Even if we keep the buffers open, don't
			   ;; let EDE ask lots of questions.
			   (let ((ede-auto-add-method 'never))
			     (find-file-noselect file t))
			 ;; When not keeping the buffers open, then
			 ;; don't setup all the fancy froo-froo features
			 ;; either.
			 (semantic-find-file-noselect file t))))
		  (set-buffer kbuff)
		  (setq buffs-to-kill (cons kbuff buffs-to-kill))
		  (semantic-fetch-tags)
		  ))
	       )

	      ;; Too much baggage in goto-line
	      ;; (goto-line line)
	      (goto-char (point-min))
	      (forward-line (1- line))

	      ;; Search forward for the matching text
	      (re-search-forward (regexp-quote txt)
				 (point-at-eol)
				 t)

	      (setq tag (semantic-current-tag))

	      ;; If we are searching for a tag, but bound the tag we are looking
	      ;; for, see if it resides in some other parent tag.
	      ;;
	      ;; If there is no parent tag, then we still need to hang the originator
	      ;; in our list.
	      (when (and (eq searchtype 'symbol)
			 (string= (semantic-tag-name tag) txt))
		(setq tag (or (semantic-current-tag-parent) tag)))

	      ;; Copy the tag, which adds a :filename property.
	      (when tag
		(setq tag (semantic-tag-copy tag nil t))
		;; Ad this hit to the tag.
		(semantic--tag-put-property tag :hit (list line)))
	      tag))
	  lines)))
      ;; Kill off dead buffers, unless we were requested to leave them open.
      (when (not open-buffers)
	(mapc 'kill-buffer buffs-to-kill))
      ;; Strip out duplicates.
      (dolist (T ans)
	(if (and T (not (semantic-equivalent-tag-p (car out) T)))
	    (setq out (cons T out))
	  (when T
	    ;; Else, add this line into the existing list of lines.
	    (let ((lines (append (semantic--tag-get-property (car out) :hit)
				 (semantic--tag-get-property T :hit))))
	      (semantic--tag-put-property (car out) :hit lines)))
	  ))
      ;; Out is reversed... twice
      (oset result :hit-tags (nreverse out)))))

;;; SYMREF TOOLS
;;
;; The base symref tool provides something to hang new tools off of
;; for finding symbol references.
(defclass semantic-symref-tool-baseclass ()
  ((searchfor :initarg :searchfor
	      :type string
	      :documentation "The thing to search for.")
   (searchtype :initarg :searchtype
		:type symbol
		:documentation "The type of search to do.
Values could be `symbol, `regexp, 'tagname, or 'completion.")
   (searchscope :initarg :searchscope
		:type symbol
		:documentation
		"The scope to search for.
Can be 'project, 'target, or 'file.")
   (resulttype :initarg :resulttype
	       :type symbol
	       :documentation
	       "The kind of search results desired.
Can be 'line, 'file, or 'tag.
The type of result can be converted from 'line to 'file, or 'line to 'tag,
but not from 'file to 'line or 'tag.")
   )
  "Baseclass for all symbol references tools.
A symbol reference tool supplies functionality to identify the locations of
where different symbols are used.

Subclasses should be named `semantic-symref-tool-NAME', where
NAME is the name of the tool used in the configuration variable
`semantic-symref-tool'"
  :abstract t)

(defmethod semantic-symref-get-result ((tool semantic-symref-tool-baseclass))
  "Calculate the results of a search based on TOOL.
The symref TOOL should already contain the search criteria."
  (let ((answer (semantic-symref-perform-search tool))
	)
    (when answer
      (let ((answersym (if (eq (oref tool :resulttype) 'file)
			   :hit-files
			 (if (stringp (car answer))
			     :hit-text
			   :hit-lines))))
	(semantic-symref-result (oref tool searchfor)
				answersym
				answer
				:created-by tool))
      )
    ))

(defmethod semantic-symref-perform-search ((tool semantic-symref-tool-baseclass))
  "Base search for symref tools should throw an error."
  (error "Symref tool objects must implement `semantic-symref-perform-search'"))

(defmethod semantic-symref-parse-tool-output ((tool semantic-symref-tool-baseclass)
					      outputbuffer)
  "Parse the entire OUTPUTBUFFER of a symref tool.
Calls the method `semantic-symref-parse-tool-output-one-line' over and
over until it returns nil."
  (with-current-buffer outputbuffer
    (goto-char (point-min))
    (let ((result nil)
	  (hit nil))
      (while (setq hit (semantic-symref-parse-tool-output-one-line tool))
	(setq result (cons hit result)))
      (nreverse result)))
  )

(defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-baseclass))
  "Base tool output parser is not implemented."
  (error "Symref tool objects must implement `semantic-symref-parse-tool-output-one-line'"))

(provide 'semantic/symref)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/symref"
;; End:

;;; semantic/symref.el ends here
