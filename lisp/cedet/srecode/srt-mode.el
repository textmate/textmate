;;; srecode/srt-mode.el --- Major mode for writing screcode macros

;; Copyright (C) 2005, 2007-2012 Free Software Foundation, Inc.

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

;; Originally named srecode-template-mode.el in the CEDET repository.

(require 'srecode/compile)
(require 'srecode/ctxt)
(require 'srecode/template)

(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/wisent)
(eval-when-compile
  (require 'semantic/find))

(declare-function srecode-create-dictionary "srecode/dictionary")
(declare-function srecode-resolve-argument-list "srecode/insert")

;;; Code:
(defvar srecode-template-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\; ". 12"  table) ;; SEMI, Comment start ;;
    (modify-syntax-entry ?\n ">"     table) ;; Comment end
    (modify-syntax-entry ?$  "."     table) ;; Punctuation
    (modify-syntax-entry ?:  "."     table) ;; Punctuation
    (modify-syntax-entry ?<  "."     table) ;; Punctuation
    (modify-syntax-entry ?>  "."     table) ;; Punctuation
    (modify-syntax-entry ?#  "."     table) ;; Punctuation
    (modify-syntax-entry ?!  "."     table) ;; Punctuation
    (modify-syntax-entry ??  "."     table) ;; Punctuation
    (modify-syntax-entry ?\" "\""    table) ;; String
    (modify-syntax-entry ?\- "_"     table) ;; Symbol
    (modify-syntax-entry ?\\ "\\"    table) ;; Quote
    (modify-syntax-entry ?\` "'"     table) ;; Prefix ` (backquote)
    (modify-syntax-entry ?\' "'"     table) ;; Prefix ' (quote)
    (modify-syntax-entry ?\, "'"     table) ;; Prefix , (comma)

    table)
  "Syntax table used in semantic recoder macro buffers.")

(defface srecode-separator-face
  '((t (:weight bold :strike-through t)))
  "Face used for decorating separators in srecode template mode."
  :group 'srecode)

(defvar srecode-font-lock-keywords
  '(
    ;; Template
    ("^\\(template\\)\\s-+\\(\\w*\\)\\(\\( \\(:\\w+\\)\\|\\)+\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-builtin-face ))
    ("^\\(sectiondictionary\\)\\s-+\""
     (1 font-lock-keyword-face))
    ("^\\(bind\\)\\s-+\""
     (1 font-lock-keyword-face))
    ;; Variable type setting
    ("^\\(set\\)\\s-+\\(\\w+\\)\\s-+"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("^\\(show\\)\\s-+\\(\\w+\\)\\s-*$"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\<\\(macro\\)\\s-+\""
     (1 font-lock-keyword-face))
    ;; Context type setting
    ("^\\(context\\)\\s-+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face))
    ;; Prompting setting
    ("^\\(prompt\\)\\s-+\\(\\w+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\(default\\(macro\\)?\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     (1 font-lock-keyword-face)
     (3 font-lock-type-face))
    ("\\<\\(default\\(macro\\)?\\)\\>" (1 font-lock-keyword-face))
    ("\\<\\(read\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))

    ;; Macro separators
    ("^----\n" 0 'srecode-separator-face)

    ;; Macro Matching
    (srecode-template-mode-macro-escape-match 1 font-lock-string-face)
    ((lambda (limit)
       (srecode-template-mode-font-lock-macro-helper
	limit "\\(\\??\\w+\\)[^ \t\n{}$#@&*()]*"))
     1 font-lock-variable-name-face)
    ((lambda (limit)
       (srecode-template-mode-font-lock-macro-helper
	limit "\\([#/]\\w+\\)[^ \t\n{}$#@&*()]*"))
     1 font-lock-keyword-face)
    ((lambda (limit)
       (srecode-template-mode-font-lock-macro-helper
	limit "\\([<>]\\w*\\):\\(\\w+\\):\\(\\w+\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face)
     (3 font-lock-type-face))
    ((lambda (limit)
       (srecode-template-mode-font-lock-macro-helper
	limit "\\([<>?]?\\w*\\):\\(\\w+\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    ((lambda (limit)
       (srecode-template-mode-font-lock-macro-helper
	limit "!\\([^{}$]*\\)"))
     1 font-lock-comment-face)

    )
  "Keywords for use with srecode macros and font-lock.")

(defun srecode-template-mode-font-lock-macro-helper (limit expression)
  "Match against escape characters.
Don't scan past LIMIT.  Match with EXPRESSION."
  (let* ((done nil)
	 (md nil)
	 (es (regexp-quote (srecode-template-get-escape-start)))
	 (ee (regexp-quote (srecode-template-get-escape-end)))
	 (regex (concat es expression ee))
	 )
    (while (not done)
      (save-match-data
	(if (re-search-forward regex limit t)
	    (when (equal (car (srecode-calculate-context)) "code")
	      (setq md (match-data)
		    done t))
	  (setq done t))))
    (set-match-data md)
    ;; (when md (message "Found a match!"))
    (when md t)))

(defun srecode-template-mode-macro-escape-match (limit)
  "Match against escape characters.
Don't scan past LIMIT."
  (let* ((done nil)
	 (md nil)
	 (es (regexp-quote (srecode-template-get-escape-start)))
	 (ee (regexp-quote (srecode-template-get-escape-end)))
	 (regex (concat "\\(" es "\\|" ee "\\)"))
	 )
    (while (not done)
      (save-match-data
	(if (re-search-forward regex limit t)
	    (when (equal (car (srecode-calculate-context)) "code")
	      (setq md (match-data)
		    done t))
	  (setq done t))))
    (set-match-data md)
    ;;(when md (message "Found a match!"))
    (when md t)))

(defvar srecode-font-lock-macro-keywords nil
  "Dynamically generated `font-lock' keywords for srecode templates.
Once the escape_start, and escape_end sequences are known, then
we can tell font lock about them.")

(defvar srecode-template-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-c\C-c" 'srecode-compile-templates)
    (define-key km "\C-c\C-m" 'srecode-macro-help)
    (define-key km "/" 'srecode-self-insert-complete-end-macro)
    km)
  "Keymap used in srecode mode.")

;;;###autoload
(define-derived-mode srecode-template-mode fundamental-mode "SRecorder"
  "Major-mode for writing SRecode macros."
  (setq comment-start ";;"
	comment-end "")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set (make-local-variable 'font-lock-defaults)
       '(srecode-font-lock-keywords
         nil  ;; perform string/comment fontification
         nil  ;; keywords are case sensitive.
         ;; This puts _ & - as a word constituent,
         ;; simplifying our keywords significantly
         ((?_ . "w") (?- . "w")))))

;;;###autoload
(defalias 'srt-mode 'srecode-template-mode)

;;; Template Commands
;;
(defun srecode-self-insert-complete-end-macro ()
  "Self insert the current key, then autocomplete the end macro."
  (interactive)
  (call-interactively 'self-insert-command)
  (when (and (semantic-current-tag)
	     (semantic-tag-of-class-p (semantic-current-tag) 'function)
	     )
    (let* ((es (srecode-template-get-escape-start))
	   (ee (srecode-template-get-escape-end))
	   (name (save-excursion
		   (forward-char (- (length es)))
		   (forward-char -1)
		   (if (looking-at (regexp-quote es))
		       (srecode-up-context-get-name (point) t))))
	   )
      (when name
	(insert name)
	(insert ee))))
  )


(defun srecode-macro-help ()
  "Provide help for working with macros in a template."
  (interactive)
  (let* ((root 'srecode-template-inserter)
	 (chl (aref (class-v root) class-children))
	 (ess (srecode-template-get-escape-start))
	 (ees (srecode-template-get-escape-end))
	 )
    (with-output-to-temp-buffer "*SRecode Macros*"
      (princ "Description of known SRecode Template Macros.")
      (terpri)
      (terpri)
      (while chl
	(let* ((C (car chl))
	       (name (symbol-name C))
	       (key (when (slot-exists-p C 'key)
		      (oref C key)))
	       (showexample t)
	       )
	  (setq chl (cdr chl))
	  (setq chl (append (aref (class-v C) class-children) chl))

	  (catch 'skip
	    (when (eq C 'srecode-template-inserter-section-end)
	      (throw 'skip nil))

	    (when (class-abstract-p C)
	      (throw 'skip nil))

	    (princ "`")
	    (princ name)
	    (princ "'")
	    (when (slot-exists-p C 'key)
	      (when key
		(princ " - Character Key: ")
		(if (stringp key)
		    (progn
		      (setq showexample nil)
		      (cond ((string= key "\n")
			     (princ "\"\\n\"")
			     )
			    (t
			     (prin1 key)
			     )))
		  (prin1 (format "%c" key))
		  )))
	    (terpri)
	    (princ (documentation-property C 'variable-documentation))
	    (terpri)
	    (when showexample
	      (princ "Example:")
	      (terpri)
	      (srecode-inserter-prin-example C ess ees)
	      )

	    (terpri)

	    ) ;; catch
	  );; let*
	))))


;;; Misc Language Overrides
;;
(define-mode-local-override semantic-ia-insert-tag
  srecode-template-mode (tag)
  "Insert the SRecode TAG into the current buffer."
  (insert (semantic-tag-name tag)))


;;; Local Context Parsing.

(defun srecode-in-macro-p (&optional point)
  "Non-nil if POINT is inside a macro bounds.
If the ESCAPE_START and END are different sequences,
a simple search is used.  If ESCAPE_START and END are the same
characters, start at the beginning of the line, and find out
how many occur."
  (let ((tag (semantic-current-tag))
	(es (regexp-quote (srecode-template-get-escape-start)))
	(ee (regexp-quote (srecode-template-get-escape-end)))
	(start (or point (point)))
	)
    (when (and tag (semantic-tag-of-class-p tag 'function))
      (if (string= es ee)
	  (save-excursion
	    (beginning-of-line)
	    (while (re-search-forward es start t 2))
	    (if (re-search-forward es start t)
		;; If there is a single, the answer is yes.
		t
	      ;; If there wasn't another, then the answer is no.
	      nil)
	    )
	;; ES And EE are not the same.
	(save-excursion
	  (and (re-search-backward es (semantic-tag-start tag) t)
	       (>= (or (re-search-forward ee (semantic-tag-end tag) t)
		       ;; No end match means an incomplete macro.
		       start)
		  start)))
	))))

(defun srecode-up-context-get-name (&optional point find-unmatched)
  "Move up one context as for `semantic-up-context', and return the name.
Moves point to the opening characters of the section macro text.
If there is no upper context, return nil.
Starts at POINT if provided.
If FIND-UNMATCHED is specified as non-nil, then we are looking for an unmatched
section."
  (when point (goto-char (point)))
  (let* ((tag (semantic-current-tag))
	 (es (regexp-quote (srecode-template-get-escape-start)))
	 (start (concat es "[#<]\\(\\w+\\)"))
	 (orig (point))
	 (name nil)
	 (res nil))
    (when (semantic-tag-of-class-p tag 'function)
      (while (and (not res)
		  (re-search-backward start (semantic-tag-start tag) t))
	(when (save-excursion
		(setq name (match-string 1))
		(let ((endr (concat es "/" name)))
		  (if (re-search-forward endr (semantic-tag-end tag) t)
		      (< orig (point))
		    (if (not find-unmatched)
			(error "Unmatched Section Template")
		      ;; We found what we want.
		      t))))
	  (setq res (point)))
	)
      ;; Restore in no result found.
      (goto-char (or res orig))
      name)))

(define-mode-local-override semantic-up-context
  srecode-template-mode (&optional point)
  "Move up one context in the current code.
Moves out one named section."
  (not (srecode-up-context-get-name point)))

(define-mode-local-override semantic-beginning-of-context
  srecode-template-mode (&optional point)
  "Move to the beginning of the current context.
Moves to the beginning of one named section."
  (if (semantic-up-context point)
      t
    (let ((es (regexp-quote (srecode-template-get-escape-start)))
	  (ee (regexp-quote (srecode-template-get-escape-end))))
      (re-search-forward es) ;; move over the start chars.
      (re-search-forward ee) ;; Move after the end chars.
      nil)))

(define-mode-local-override semantic-end-of-context
  srecode-template-mode (&optional point)
  "Move to the end of the current context.
Moves to the end of one named section."
  (let ((name (srecode-up-context-get-name point))
	(tag (semantic-current-tag))
	(es  (regexp-quote (srecode-template-get-escape-start))))
  (if (not name)
      t
    (unless (re-search-forward (concat es "/" name) (semantic-tag-end tag) t)
      (error "Section %s has no end" name))
    (goto-char (match-beginning 0))
    nil)))

(define-mode-local-override semantic-get-local-variables
  srecode-template-mode (&optional point)
  "Get local variables from an SRecode template."
  (save-excursion
    (when point (goto-char (point)))
    (let* ((tag (semantic-current-tag))
	   (name (save-excursion
		   (srecode-up-context-get-name (point))))
	   (subdicts (semantic-tag-get-attribute tag :dictionaries))
	   (global nil)
	   )
      (dolist (D subdicts)
	(setq global (cons (semantic-tag-new-variable (car D) nil)
			   global)))
      (if name
	  ;; Lookup any subdictionaries in TAG.
	  (let ((res nil))

	    (while (and (not res) subdicts)
	      ;; Find the subdictionary with the same name.  Those variables
	      ;; are now local to this section.
	      (when (string= (car (car subdicts)) name)
		(setq res (cdr (car subdicts))))
	      (setq subdicts (cdr subdicts)))
	    ;; Pre-pend our global vars.
	    (append global res))
	;; If we aren't in a subsection, just do the global variables
	global
	))))

(define-mode-local-override semantic-get-local-arguments
  srecode-template-mode (&optional point)
  "Get local arguments from an SRecode template."
  (require 'srecode/insert)
  (save-excursion
    (when point (goto-char (point)))
    (let* ((tag (semantic-current-tag))
	   (args (semantic-tag-function-arguments tag))
	   (argsym (mapcar 'intern args))
	   (argvars nil)
	   ;; Create a temporary dictionary in which the
	   ;; arguments can be resolved so we can extract
	   ;; the results.
	   (dict (srecode-create-dictionary t))
	   )
      ;; Resolve args into our temp dictionary
      (srecode-resolve-argument-list argsym dict)

      (maphash
       (lambda (key entry)
	 (setq argvars
	       (cons (semantic-tag-new-variable key nil entry)
		     argvars)))
       (oref dict namehash))

      argvars)))

(define-mode-local-override semantic-ctxt-current-symbol
  srecode-template-mode (&optional point)
  "Return the current symbol under POINT.
Return nil if point is not on/in a template macro."
  (let ((macro (srecode-parse-this-macro point)))
    (cdr macro))
  )

(defun srecode-parse-this-macro (&optional point)
  "Return the current symbol under POINT.
Return nil if point is not on/in a template macro.
The first element is the key for the current macro, such as # for a
section or ? for an ask variable."
  (save-excursion
    (if point (goto-char point))
    (let ((tag (semantic-current-tag))
	  (es (regexp-quote (srecode-template-get-escape-start)))
	  (ee (regexp-quote (srecode-template-get-escape-end)))
	  (start (point))
	  (macrostart nil)
	  (raw nil)
	  )
      (when (and tag (semantic-tag-of-class-p tag 'function)
		 (srecode-in-macro-p point)
		 (re-search-backward es (semantic-tag-start tag) t))
	(setq macrostart (match-end 0))
	(goto-char macrostart)
	;; We have a match
	(when (not (re-search-forward ee (semantic-tag-end tag) t))
	  (goto-char start) ;; Pretend we are ok for completion
	  (set-match-data (list start start))
	  )

	(if (> start (point))
	    ;; If our starting point is after the found point, that
	    ;; means we are not inside the macro.  Return nil.
	    nil
	  ;; We are inside the macro, extract the text so far.
	  (let* ((macroend (match-beginning 0))
		 (raw (buffer-substring-no-properties
		       macrostart macroend))
		 (STATE (srecode-compile-state "TMP"))
		 (inserter (condition-case nil
			       (srecode-compile-parse-inserter
				raw STATE)
			     (error nil)))
		 )
	    (when inserter
	      (let ((base
		     (cons (oref inserter :object-name)
			   (if (and (slot-boundp inserter :secondname)
				    (oref inserter :secondname))
			       (split-string (oref inserter :secondname)
					     ":")
			     nil)))
		    (key (oref inserter key)))
		(cond ((null key)
		       ;; A plain variable
		       (cons nil base))
		      (t
		       ;; A complex variable thingy.
		       (cons (format "%c" key)
			     base)))))
	    )
	  )))
    ))

(define-mode-local-override semantic-analyze-current-context
  srecode-template-mode (point)
  "Provide a Semantic analysis in SRecode template mode."
    (let* ((context-return nil)
	   (prefixandbounds (semantic-ctxt-current-symbol-and-bounds))
	   (prefix (car prefixandbounds))
	   (bounds (nth 2 prefixandbounds))
	   (key (car (srecode-parse-this-macro (point))))
	   (prefixsym nil)
	   (prefix-var nil)
	   (prefix-context nil)
	   (prefix-function nil)
	   (prefixclass (semantic-ctxt-current-class-list))
	   (globalvar (semantic-find-tags-by-class 'variable (current-buffer)))
	   (argtype 'macro)
	   (scope (semantic-calculate-scope point))
	   )

      (oset scope fullscope (append (oref scope localvar) globalvar))

      (when prefix
	;; First, try to find the variable for the first
	;; entry in the prefix list.
	(setq prefix-var (semantic-find-first-tag-by-name
			  (car prefix) (oref scope fullscope)))

	(cond
	 ((and (or (not key) (string= key "?"))
	       (> (length prefix) 1))
	  ;; Variables can have lisp function names.
	  (with-mode-local emacs-lisp-mode
	    (let ((fcns (semanticdb-find-tags-by-name (car (last prefix)))))
	      (setq prefix-function (car (semanticdb-find-result-nth fcns 0)))
	      (setq argtype 'elispfcn)))
	  )
	 ((or (string= key "<") (string= key ">"))
	  ;; Includes have second args that is the template name.
	  (if (= (length prefix) 3)
	      (let ((contexts (semantic-find-tags-by-class
			       'context (current-buffer))))
		(setq prefix-context
		      (or (semantic-find-first-tag-by-name
			   (nth 1 prefix) contexts)
			  ;; Calculate from location
			  (semantic-tag
			   (symbol-name
			    (srecode-template-current-context))
			   'context)))
		(setq argtype 'template))
	    (setq prefix-context
		  ;; Calculate from location
		  (semantic-tag
		   (symbol-name (srecode-template-current-context))
		   'context))
	    (setq argtype 'template)
	    )
	  ;; The last one?
	  (when (> (length prefix) 1)
	    (let ((toc (srecode-template-find-templates-of-context
			(read (semantic-tag-name prefix-context))))
		  )
	      (setq prefix-function
		    (or (semantic-find-first-tag-by-name
			(car (last prefix)) toc)
			;; Not in this buffer?  Search the master
			;; templates list.
			nil))
	      ))
	  )
	 )

	(setq prefixsym
	      (cond ((= (length prefix) 3)
		     (list (or prefix-var (nth 0 prefix))
			   (or prefix-context (nth 1 prefix))
			   (or prefix-function (nth 2 prefix))))
		    ((= (length prefix) 2)
		     (list (or prefix-var (nth 0 prefix))
			   (or prefix-function (nth 1 prefix))))
		    ((= (length prefix) 1)
		     (list (or prefix-var (nth 0 prefix)))
		     )))

	(setq context-return
	      (semantic-analyze-context-functionarg
	       "context-for-srecode"
	       :buffer (current-buffer)
	       :scope scope
	       :bounds bounds
	       :prefix (or prefixsym
			   prefix)
	       :prefixtypes nil
	       :prefixclass prefixclass
	       :errors nil
	       ;; Use the functionarg analyzer class so we
	       ;; can save the current key, and the index
	       ;; into the macro part we are completing on.
	       :function (list key)
	       :index (length prefix)
	       :argument (list argtype)
	       ))

	context-return)))

(define-mode-local-override semantic-analyze-possible-completions
  srecode-template-mode (context)
  "Return a list of possible completions based on NONTEXT."
  (with-current-buffer (oref context buffer)
    (let* ((prefix (car (last (oref context :prefix))))
	   (prefixstr (cond ((stringp prefix)
			     prefix)
			    ((semantic-tag-p prefix)
			     (semantic-tag-name prefix))))
;	   (completetext (cond ((semantic-tag-p prefix)
;				(semantic-tag-name prefix))
;			       ((stringp prefix)
;				prefix)
;			       ((stringp (car prefix))
;				(car prefix))))
	   (argtype (car (oref context :argument)))
	   (matches nil))

      ;; Depending on what the analyzer is, we have different ways
      ;; of creating completions.
      (cond ((eq argtype 'template)
	     (setq matches (semantic-find-tags-for-completion
			    prefixstr (current-buffer)))
	     (setq matches (semantic-find-tags-by-class
			    'function matches))
	     )
	    ((eq argtype 'elispfcn)
	     (with-mode-local emacs-lisp-mode
	       (setq matches (semanticdb-find-tags-for-completion
			      prefixstr))
	       (setq matches (semantic-find-tags-by-class
			      'function matches))
	       )
	     )
	    ((eq argtype 'macro)
	     (let ((scope (oref context scope)))
	       (setq matches
		     (semantic-find-tags-for-completion
		      prefixstr (oref scope fullscope))))
	     )
	    )

      matches)))



;;; Utils
;;
(defun srecode-template-get-mode ()
  "Get the supported major mode for this template file."
  (let ((m (semantic-find-first-tag-by-name "mode" (current-buffer))))
    (when m (read (semantic-tag-variable-default m)))))

(defun srecode-template-get-escape-start ()
  "Get the current escape_start characters."
  (let ((es (semantic-find-first-tag-by-name "escape_start" (current-buffer)))
	)
     (if es (car (semantic-tag-get-attribute es :default-value))
       "{{")))

(defun srecode-template-get-escape-end ()
  "Get the current escape_end characters."
  (let ((ee (semantic-find-first-tag-by-name "escape_end" (current-buffer)))
	)
    (if ee (car (semantic-tag-get-attribute ee :default-value))
      "}}")))

(defun srecode-template-current-context (&optional point)
  "Calculate the context encompassing POINT."
  (save-excursion
    (when point (goto-char (point)))
    (let ((ct (semantic-current-tag)))
      (when (not ct)
	(setq ct (semantic-find-tag-by-overlay-prev)))

      ;; Loop till we find the context.
      (while (and ct (not (semantic-tag-of-class-p ct 'context)))
	(setq ct (semantic-find-tag-by-overlay-prev
		  (semantic-tag-start ct))))

      (if ct
	  (read (semantic-tag-name ct))
	'declaration))))

(defun srecode-template-find-templates-of-context (context &optional buffer)
  "Find all the templates belonging to a particular CONTEXT.
When optional BUFFER is provided, search that buffer."
  (save-excursion
    (when buffer (set-buffer buffer))
    (let ((tags (semantic-fetch-available-tags))
	  (cc 'declaration)
	  (scan nil)
	  (ans nil))

      (when (eq cc context)
	(setq scan t))

      (dolist (T tags)
	;; Handle contexts
	(when (semantic-tag-of-class-p T 'context)
	  (setq cc (read (semantic-tag-name T)))
	  (when (eq cc context)
	    (setq scan t)))

	;; Scan
	(when (and scan (semantic-tag-of-class-p T 'function))
	  (setq ans (cons T ans)))
	)

      (nreverse ans))))

(provide 'srecode/srt-mode)

;; The autoloads in this file must go into the global loaddefs.el, not
;; the srecode one, so that srecode-template-mode can be called from
;; auto-mode-alist.

;; Local variables:
;; generated-autoload-load-name: "srecode/srt-mode"
;; End:

;;; srecode/srt-mode.el ends here
