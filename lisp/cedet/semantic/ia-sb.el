;;; semantic/ia-sb.el --- Speedbar analysis display interactor

;;; Copyright (C) 2002-2004, 2006, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;; Speedbar node for displaying derived context information.
;;

(require 'semantic/analyze)
(require 'speedbar)

;;; Code:
(defvar semantic-ia-sb-key-map nil
  "Keymap used when in semantic analysis display mode.")

(if semantic-ia-sb-key-map
    nil
  (setq semantic-ia-sb-key-map (speedbar-make-specialized-keymap))

  ;; Basic features.
  (define-key semantic-ia-sb-key-map "\C-m" 'speedbar-edit-line)
  (define-key semantic-ia-sb-key-map "I" 'semantic-ia-sb-show-tag-info)
  )

(defvar semantic-ia-sb-easymenu-definition
  '( "---"
;     [ "Expand" speedbar-expand-line nil ]
;     [ "Contract" speedbar-contract-line nil ]
     [ "Tag Information" semantic-ia-sb-show-tag-info t ]
     [ "Jump to Tag" speedbar-edit-line t ]
     [ "Complete" speedbar-edit-line t ]
     )
  "Extra menu items Analysis mode.")

;; Make sure our special speedbar major mode is loaded
(speedbar-add-expansion-list '("Analyze"
			       semantic-ia-sb-easymenu-definition
			       semantic-ia-sb-key-map
			       semantic-ia-speedbar))

(speedbar-add-mode-functions-list
 (list "Analyze"
       ;;'(speedbar-item-info . eieio-speedbar-item-info)
       '(speedbar-line-directory . semantic-ia-sb-line-path)))

;;;###autoload
(defun semantic-speedbar-analysis ()
  "Start Speedbar in semantic analysis mode.
The analyzer displays information about the current context, plus a smart
list of possible completions."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Analyze  mode on speedbar.
  (speedbar-change-initial-expansion-list "Analyze")
  )

(defun semantic-ia-speedbar (directory zero)
  "Create buttons in speedbar which define the current analysis at POINT.
DIRECTORY is the current directory, which is ignored, and ZERO is 0."
  (let ((analysis nil)
	(scope nil)
	(buffer nil)
	(completions nil)
	(cf (selected-frame))
	(cnt nil)
	(mode-local-active-mode nil)
	)
    ;; Try and get some sort of analysis
    (condition-case nil
	(progn
	  (speedbar-select-attached-frame)
	  (setq buffer (current-buffer))
	  (setq mode-local-active-mode major-mode)
	  (save-excursion
	    ;; Get the current scope
	    (setq scope (semantic-calculate-scope (point)))
	    ;; Get the analysis
	    (setq analysis (semantic-analyze-current-context (point)))
	    (setq cnt (semantic-find-tag-by-overlay))
	    (when analysis
	      (setq completions (semantic-analyze-possible-completions analysis))
	      )
	    ))
      (error nil))
    (select-frame cf)
    (with-current-buffer speedbar-buffer
      ;; If we have something, do something spiff with it.
      (erase-buffer)
      (speedbar-insert-separator "Buffer/Function")
      ;; Note to self: Turn this into an expandable file name.
      (speedbar-make-tag-line 'bracket ?  nil nil
			      (buffer-name buffer)
			      nil nil 'speedbar-file-face 0)

      (when cnt
	(semantic-ia-sb-string-list cnt
				    'speedbar-tag-face
				    'semantic-sb-token-jump))
      (when analysis
	;; If this analyzer happens to point at a complete symbol, then
	;; see if we can dig up some documentation for it.
	(semantic-ia-sb-show-doc analysis))

      (when analysis
	;; Let different classes draw more buttons.
	(semantic-ia-sb-more-buttons analysis)
	(when completions
	  (speedbar-insert-separator "Completions")
	  (semantic-ia-sb-completion-list completions
					  'speedbar-tag-face
					  'semantic-ia-sb-complete))
	)

      ;; Show local variables
      (when scope
	(semantic-ia-sb-show-scope scope))

      )))

(defmethod semantic-ia-sb-show-doc ((context semantic-analyze-context))
  "Show documentation about CONTEXT iff CONTEXT points at a complete symbol."
  (let ((sym (car (reverse (oref context prefix))))
	(doc nil))
    (when (semantic-tag-p sym)
      (setq doc (semantic-documentation-for-tag sym))
      (when doc
	(speedbar-insert-separator "Documentation")
	(insert doc)
	(insert "\n")
	))
    ))

(defun semantic-ia-sb-show-scope (scope)
  "Show SCOPE information."
  (let ((localvars (when scope
		     (oref scope localvar)))
	)
    (when localvars
      (speedbar-insert-separator "Local Variables")
      (semantic-ia-sb-string-list localvars
				  'speedbar-tag-face
				  ;; This is from semantic-sb
				  'semantic-sb-token-jump))))

(defmethod semantic-ia-sb-more-buttons ((context semantic-analyze-context))
  "Show a set of speedbar buttons specific to CONTEXT."
  (let ((prefix (oref context prefix)))
    (when prefix
      (speedbar-insert-separator "Prefix")
      (semantic-ia-sb-string-list prefix
				  'speedbar-tag-face
				  'semantic-sb-token-jump))
    ))

(defmethod semantic-ia-sb-more-buttons ((context semantic-analyze-context-assignment))
  "Show a set of speedbar buttons specific to CONTEXT."
  (call-next-method)
  (let ((assignee (oref context assignee)))
    (when assignee
      (speedbar-insert-separator "Assignee")
      (semantic-ia-sb-string-list assignee
				  'speedbar-tag-face
				  'semantic-sb-token-jump))))

(defmethod semantic-ia-sb-more-buttons ((context semantic-analyze-context-functionarg))
  "Show a set of speedbar buttons specific to CONTEXT."
  (call-next-method)
  (let ((func (oref context function)))
    (when func
      (speedbar-insert-separator "Function")
      (semantic-ia-sb-string-list func
				  'speedbar-tag-face
				  'semantic-sb-token-jump)
      ;; An index for the argument the prefix is in:
      (let ((arg (oref context argument))
	    (args (semantic-tag-function-arguments (car func)))
	    (idx 0)
	    )
	(speedbar-insert-separator
	 (format "Argument #%d" (oref context index)))
	(if args
	    (semantic-ia-sb-string-list args
					'speedbar-tag-face
					'semantic-sb-token-jump
					(oref context index)
					'speedbar-selected-face)
	  ;; Else, no args list, so use what the context had.
	  (semantic-ia-sb-string-list arg
				      'speedbar-tag-face
				      'semantic-sb-token-jump))
	))))

(defun semantic-ia-sb-string-list (list face function &optional idx idxface)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION.
Optional IDX is an index into LIST to apply IDXFACE instead."
  (let ((count 1))
    (while list
      (let* ((usefn nil)
	     (string (cond ((stringp (car list))
			    (car list))
			   ((semantic-tag-p (car list))
			    (setq usefn (semantic-tag-with-position-p (car list)))
			    (semantic-format-tag-uml-concise-prototype (car list)))
			   (t "<No Tag>")))
	     (localface (if (or (not idx) (/= idx count))
			    face
			  idxface))
	     )
	(if (semantic-tag-p (car list))
	    (speedbar-make-tag-line 'angle ?i
				    'semantic-ia-sb-tag-info (car list)
				    string (if usefn function) (car list) localface
				    0)
	  (speedbar-make-tag-line 'statictag ??
				  nil nil
				  string (if usefn function) (car list) localface
				  0))
	(setq list (cdr list)
	      count (1+ count)))
      )))

(defun semantic-ia-sb-completion-list (list face function)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION."
  (while list
    (let* ((documentable nil)
	   (string (cond ((stringp (car list))
			  (car list))
			 ((semantic-tag-p (car list))
			  (setq documentable t)
			  (semantic-format-tag-uml-concise-prototype (car list)))
			(t "foo"))))
      (if documentable
	  (speedbar-make-tag-line 'angle ?i
				  'semantic-ia-sb-tag-info
				  (car list)
				  string function (car list) face
				  0)
	(speedbar-make-tag-line 'statictag ?  nil nil
				string function (car list) face
				0))
      (setq list (cdr list)))))

(defun semantic-ia-sb-show-tag-info ()
  "Display information about the tag on the current line.
Same as clicking on the <i> button.
See `semantic-ia-sb-tag-info' for more."
  (interactive)
  (let ((tok nil))
    (save-excursion
      (end-of-line)
      (forward-char -1)
      (setq tok (get-text-property (point) 'speedbar-token)))
    (semantic-ia-sb-tag-info nil tok 0)))

(defun semantic-ia-sb-tag-info (text tag indent)
  "Display as much information as we can about tag.
Show the information in a shrunk split-buffer and expand
out as many details as possible.
TEXT, TAG, and INDENT are speedbar function arguments."
  (when (semantic-tag-p tag)
    (unwind-protect
	(let ((ob nil))
	  (speedbar-select-attached-frame)
	  (setq ob (current-buffer))
	  (with-output-to-temp-buffer "*Tag Information*"
	    ;; Output something about this tag:
	    (with-current-buffer "*Tag Information*"
	      (goto-char (point-max))
	      (insert
	       (semantic-format-tag-prototype tag nil t)
	       "\n")
	      (let ((typetok
		     (condition-case nil
			 (with-current-buffer ob
			   ;; @todo - We need a context to derive a scope from.
			   (semantic-analyze-tag-type tag nil))
		       (error nil))))
		(if typetok
		    (insert (semantic-format-tag-prototype
			     typetok nil t))
		  ;; No type found by the analyzer
		  ;; The below used to try and select the buffer from the last
		  ;; analysis, but since we are already in the correct buffer, I
		  ;; don't think that is needed.
		  (let ((type (semantic-tag-type tag)))
		    (cond ((semantic-tag-p type)
			   (setq type (semantic-tag-name type)))
			  ((listp type)
			   (setq type (car type))))
		    (if (semantic-lex-keyword-p type)
			(setq typetok
			      (semantic-lex-keyword-get type 'summary))))
		  (if typetok
		      (insert typetok))
		  ))
	      ))
	  ;; Make it small
	  (shrink-window-if-larger-than-buffer
	   (get-buffer-window "*Tag Information*")))
      (select-frame speedbar-frame))))

(defun semantic-ia-sb-line-path (&optional depth)
  "Return the file name associated with DEPTH."
  (save-match-data
    (let* ((tok (speedbar-line-token))
	   (buff (if (semantic-tag-buffer tok)
		     (semantic-tag-buffer tok)
		   (current-buffer))))
      (buffer-file-name buff))))

(defun semantic-ia-sb-complete (text tag indent)
  "At point in the attached buffer, complete the symbol clicked on.
TEXT TAG and INDENT are the details."
  ;; Find the specified bounds from the current analysis.
  (speedbar-select-attached-frame)
  (unwind-protect
      (let* ((a (semantic-analyze-current-context (point)))
	     (bounds (oref a bounds))
	     (movepoint nil)
	     )
	(save-excursion
	  (if (and (<= (point) (cdr bounds)) (>= (point) (car bounds)))
	      (setq movepoint t))
	  (goto-char (car bounds))
	  (delete-region (car bounds) (cdr bounds))
	  (insert (semantic-tag-name tag))
	  (if movepoint (setq movepoint (point)))
	  ;; I'd like to use this to add fancy () or what not at the end
	  ;; but we need the parent file which requires an upgrade to the
	  ;; analysis tool.
	  ;;(semantic-insert-foreign-tag tag ??))
	  )
	(if movepoint
	    (let ((cf (selected-frame)))
	      (speedbar-select-attached-frame)
	      (goto-char movepoint)
	      (select-frame cf))))
    (select-frame speedbar-frame)))

(provide 'semantic/ia-sb)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/ia-sb"
;; End:

;;; semantic/ia-sb.el ends here
