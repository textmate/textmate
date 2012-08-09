;;; rng-cmpct.el --- parsing of RELAX NG Compact Syntax schemas

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

;; This parses a RELAX NG Compact Syntax schema into the form
;; specified in rng-pttrn.el.
;;
;; RELAX NG Compact Syntax is specified by
;;    http://relaxng.org/compact.html
;;
;; This file uses the prefix "rng-c-".

;;; Code:

(require 'nxml-util)
(require 'rng-util)
(require 'rng-uri)
(require 'rng-pttrn)

;;;###autoload
(defun rng-c-load-schema (filename)
  "Load a schema in RELAX NG compact syntax from FILENAME.
Return a pattern."
  (rng-c-parse-file filename))

;;; Error handling

(put 'rng-c-incorrect-schema
     'error-conditions
     '(error rng-error nxml-file-parse-error rng-c-incorrect-schema))

(put 'rng-c-incorrect-schema
     'error-message
     "Incorrect schema")

(defun rng-c-signal-incorrect-schema (filename pos message)
  (nxml-signal-file-parse-error filename
				pos
				message
				'rng-c-incorrect-schema))

;;; Lexing

(defconst rng-c-keywords
  '("attribute"
    "default"
    "datatypes"
    "div"
    "element"
    "empty"
    "external"
    "grammar"
    "include"
    "inherit"
    "list"
    "mixed"
    "namespace"
    "notAllowed"
    "parent"
    "start"
    "string"
    "text"
    "token")
  "List of strings that are keywords in the compact syntax.")

(defconst rng-c-anchored-keyword-re
  (concat "\\`\\(" (regexp-opt rng-c-keywords) "\\)\\'")
  "Regular expression to match a keyword in the compact syntax.")

(defvar rng-c-syntax-table nil
  "Syntax table for parsing the compact syntax.")

(if rng-c-syntax-table
    ()
  (setq rng-c-syntax-table (make-syntax-table))
  (modify-syntax-entry ?# "<" rng-c-syntax-table)
  (modify-syntax-entry ?\n ">" rng-c-syntax-table)
  (modify-syntax-entry ?- "w" rng-c-syntax-table)
  (modify-syntax-entry ?. "w" rng-c-syntax-table)
  (modify-syntax-entry ?_ "w" rng-c-syntax-table)
  (modify-syntax-entry ?: "_" rng-c-syntax-table))

(defconst rng-c-literal-1-re
  "'\\(''\\([^']\\|'[^']\\|''[^']\\)*''\\|[^'\n]*\\)'"
  "Regular expression to match a single-quoted literal.")

(defconst rng-c-literal-2-re
  (replace-regexp-in-string "'" "\"" rng-c-literal-1-re)
  "Regular expression to match a double-quoted literal.")

(defconst rng-c-ncname-re "\\w+")

(defconst rng-c-anchored-ncname-re
  (concat "\\`" rng-c-ncname-re "\\'"))

(defconst rng-c-token-re
  (concat "[&|]=" "\\|"
	  "[][()|&,*+?{}~=-]" "\\|"
	  rng-c-literal-1-re "\\|"
	  rng-c-literal-2-re "\\|"
	  rng-c-ncname-re "\\(:\\(\\*\\|" rng-c-ncname-re "\\)\\)?" "\\|"
	  "\\\\" rng-c-ncname-re "\\|"
	  ">>")
  "Regular expression to match a token in the compact syntax.")

(defun rng-c-init-buffer ()
  (setq case-fold-search nil) ; automatically becomes buffer-local when set
  (set-buffer-multibyte t)
  (set-syntax-table rng-c-syntax-table))

(defvar rng-c-current-token nil)
(make-variable-buffer-local 'rng-c-current-token)

(defun rng-c-advance ()
  (cond ((looking-at rng-c-token-re)
	 (setq rng-c-current-token (match-string 0))
	 (goto-char (match-end 0))
	 (forward-comment (point-max)))
	((= (point) (point-max))
	 (setq rng-c-current-token ""))
	(t (rng-c-error "Invalid token"))))

(defconst rng-c-anchored-datatype-name-re
  (concat "\\`" rng-c-ncname-re ":"  rng-c-ncname-re "\\'"))

(defsubst rng-c-current-token-keyword-p ()
  (string-match rng-c-anchored-keyword-re rng-c-current-token))

(defsubst rng-c-current-token-prefixed-name-p ()
  (string-match rng-c-anchored-datatype-name-re rng-c-current-token))

(defsubst rng-c-current-token-literal-p ()
  (string-match "\\`['\"]" rng-c-current-token))

(defsubst rng-c-current-token-quoted-identifier-p ()
  (string-match "\\`\\\\" rng-c-current-token))

(defsubst rng-c-current-token-ncname-p ()
  (string-match rng-c-anchored-ncname-re rng-c-current-token))

(defsubst rng-c-current-token-ns-name-p ()
  (let ((len (length rng-c-current-token)))
    (and (> len 0)
	 (= (aref rng-c-current-token (- len 1)) ?*))))

;;; Namespaces

(defvar rng-c-inherit-namespace nil)

(defvar rng-c-default-namespace nil)

(defvar rng-c-default-namespace-declared nil)

(defvar rng-c-namespace-decls nil
  "Alist of namespace declarations.")

(defconst rng-c-no-namespace nil)

(defun rng-c-declare-standard-namespaces ()
  (setq rng-c-namespace-decls
	(cons (cons "xml" nxml-xml-namespace-uri)
	      rng-c-namespace-decls))
  (when (and (not rng-c-default-namespace-declared)
	     rng-c-inherit-namespace)
    (setq rng-c-default-namespace rng-c-inherit-namespace)))

(defun rng-c-expand-name (prefixed-name)
  (let ((i (string-match ":" prefixed-name)))
    (rng-make-name (rng-c-lookup-prefix (substring prefixed-name
						   0
						   i))
		   (substring prefixed-name (+ i 1)))))

(defun rng-c-lookup-prefix (prefix)
  (let ((binding (assoc prefix rng-c-namespace-decls)))
    (or binding (rng-c-error "Undefined prefix %s" prefix))
    (cdr binding)))

(defun rng-c-unqualified-namespace (attribute)
  (if attribute
      rng-c-no-namespace
    rng-c-default-namespace))

(defun rng-c-make-context ()
  (cons rng-c-default-namespace rng-c-namespace-decls))

;;; Datatypes

(defconst rng-string-datatype
  (rng-make-datatype rng-builtin-datatypes-uri "string"))

(defconst rng-token-datatype
  (rng-make-datatype rng-builtin-datatypes-uri "token"))

(defvar rng-c-datatype-decls nil
  "Alist of datatype declarations.
Contains a list of pairs (PREFIX . URI) where PREFIX is a string
and URI is a symbol.")

(defun rng-c-declare-standard-datatypes ()
  (setq rng-c-datatype-decls
	(cons (cons "xsd" rng-xsd-datatypes-uri)
	      rng-c-datatype-decls)))

(defun rng-c-lookup-datatype-prefix (prefix)
  (let ((binding (assoc prefix rng-c-datatype-decls)))
    (or binding (rng-c-error "Undefined prefix %s" prefix))
    (cdr binding)))

(defun rng-c-expand-datatype (prefixed-name)
  (let ((i (string-match ":" prefixed-name)))
    (rng-make-datatype
     (rng-c-lookup-datatype-prefix (substring prefixed-name 0 i))
     (substring prefixed-name (+ i 1)))))

;;; Grammars

(defvar rng-c-current-grammar nil)
(defvar rng-c-parent-grammar nil)

(defun rng-c-make-grammar ()
  (make-hash-table :test 'equal))

(defconst rng-c-about-override-slot 0)
(defconst rng-c-about-combine-slot 1)

(defun rng-c-lookup-create (name grammar)
  "Return a def object for NAME.
A def object is a pair \(ABOUT . REF) where REF is returned by
`rng-make-ref'.
ABOUT is a two-element vector [OVERRIDE COMBINE].
COMBINE is either nil, choice or interleave.
OVERRIDE is either nil, require or t."
  (let ((def (gethash name grammar)))
    (if def
	def
      (progn
	(setq def (cons (vector nil nil) (rng-make-ref name)))
	(puthash name def grammar)
	def))))

(defun rng-c-make-ref (name)
  (or rng-c-current-grammar
      (rng-c-error "Reference not in a grammar"))
  (cdr (rng-c-lookup-create name rng-c-current-grammar)))

(defun rng-c-make-parent-ref (name)
  (or rng-c-parent-grammar
      (rng-c-error "Reference to non-existent parent grammar"))
  (cdr (rng-c-lookup-create name rng-c-parent-grammar)))

(defvar rng-c-overrides nil
  "Contains a list of (NAME . DEF) pairs.")

(defun rng-c-merge-combine (def combine name)
  (let* ((about (car def))
	 (current-combine (aref about rng-c-about-combine-slot)))
    (if combine
	(if current-combine
	    (or (eq combine current-combine)
		(rng-c-error "Inconsistent combine for %s" name))
	  (aset about rng-c-about-combine-slot combine))
      current-combine)))

(defun rng-c-prepare-define (name combine in-include)
  (let* ((def (rng-c-lookup-create name rng-c-current-grammar))
	 (about (car def))
	 (overridden (aref about rng-c-about-override-slot)))
    (and in-include
	 (setq rng-c-overrides (cons (cons name def) rng-c-overrides)))
    (cond (overridden (and (eq overridden 'require)
			   (aset about rng-c-about-override-slot t))
		      nil)
	  (t (setq combine (rng-c-merge-combine def combine name))
	     (and (rng-ref-get (cdr def))
		  (not combine)
		  (rng-c-error "Duplicate definition of %s" name))
	     def))))

(defun rng-c-start-include (overrides)
  (mapcar (lambda (name-def)
	    (let* ((def (cdr name-def))
		   (about (car def))
		   (save (aref about rng-c-about-override-slot)))
	      (aset about rng-c-about-override-slot 'require)
	      (cons save name-def)))
	  overrides))

(defun rng-c-end-include (overrides)
  (mapcar (lambda (o)
	    (let* ((saved (car o))
		   (name-def (cdr o))
		   (name (car name-def))
		   (def (cdr name-def))
		   (about (car def)))
	      (and (eq (aref about rng-c-about-override-slot) 'require)
		   (rng-c-error "Definition of %s in include did not override definition in included file" name))
	      (aset about rng-c-about-override-slot saved)))
	  overrides))

(defun rng-c-define (def value)
  (and def
       (let ((current-value (rng-ref-get (cdr def))))
	 (rng-ref-set (cdr def)
		      (if current-value
			  (if (eq (aref (car def) rng-c-about-combine-slot)
				  'choice)
			      (rng-make-choice (list current-value value))
			    (rng-make-interleave (list current-value value)))
			value)))))

(defun rng-c-finish-grammar ()
  (maphash (lambda (key def)
	     (or (rng-ref-get (cdr def))
		 (rng-c-error "Reference to undefined pattern %s" key)))
	   rng-c-current-grammar)
  (rng-ref-get (cdr (or (gethash 'start rng-c-current-grammar)
			(rng-c-error "No definition of start")))))

;;; Parsing

(defvar rng-c-escape-positions nil)
(make-variable-buffer-local 'rng-c-escape-positions)

(defvar rng-c-file-name nil)
(make-variable-buffer-local 'rng-c-file-name)

(defvar rng-c-file-index nil)

(defun rng-c-parse-file (filename &optional context)
  (with-current-buffer (get-buffer-create (rng-c-buffer-name context))
    (erase-buffer)
    (rng-c-init-buffer)
    (setq rng-c-file-name
	  (car (insert-file-contents filename)))
    (setq rng-c-escape-positions nil)
    (rng-c-process-escapes)
    (rng-c-parse-top-level context)))

(defun rng-c-buffer-name (context)
  (concat " *RNC Input"
	  (if context
	      (concat "<"
		      (number-to-string (setq rng-c-file-index
					      (1+ rng-c-file-index)))
		      ">*")
	    (setq rng-c-file-index 1)
	    "*")))

(defun rng-c-process-escapes ()
  ;; Check for any nuls, since we will use nul chars
  ;; for internal purposes.
  (let ((pos (search-forward "\C-@" nil t)))
    (and pos
	 (rng-c-error "Nul character found (binary file?)")))
  (let ((offset 0))
    (while (re-search-forward "\\\\x+{\\([0-9a-fA-F]+\\)}"
			      (point-max)
			      t)
      (let* ((ch (decode-char 'ucs (string-to-number (match-string 1) 16))))
	(if (and ch (> ch 0))
	    (let ((begin (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region begin end)
	      ;; Represent an escaped newline by nul, so
	      ;; that we can distinguish it from a literal newline.
	      ;; We will translate it back into a real newline later.
	      (insert (if (eq ch ?\n) 0 ch))
	      (setq offset (+ offset (- end begin 1)))
	      (setq rng-c-escape-positions
		    (cons (cons (point) offset)
			  rng-c-escape-positions)))
	  (rng-c-error "Invalid character escape")))))
  (goto-char 1))

(defun rng-c-translate-position (pos)
  (let ((tem rng-c-escape-positions))
    (while (and tem
		(> (caar tem) pos))
      (setq tem (cdr tem)))
    (if tem
	(+ pos (cdar tem))
      pos)))

(defun rng-c-error (&rest args)
  (rng-c-signal-incorrect-schema rng-c-file-name
				 (rng-c-translate-position (point))
				 (apply 'format args)))

(defun rng-c-parse-top-level (context)
  (let ((rng-c-namespace-decls nil)
	(rng-c-default-namespace nil)
	(rng-c-datatype-decls nil))
    (goto-char (point-min))
    (forward-comment (point-max))
    (rng-c-advance)
    (rng-c-parse-decls)
    (let ((p (if (eq context 'include)
		 (if (rng-c-implicit-grammar-p)
		     (rng-c-parse-grammar-body "")
		   (rng-c-parse-included-grammar))
	       (if (rng-c-implicit-grammar-p)
		   (rng-c-parse-implicit-grammar)
		 (rng-c-parse-pattern)))))
      (or (string-equal rng-c-current-token "")
	  (rng-c-error "Unexpected characters after pattern"))
      p)))

(defun rng-c-parse-included-grammar ()
  (or (string-equal rng-c-current-token "grammar")
      (rng-c-error "Included schema is not a grammar"))
  (rng-c-advance)
  (rng-c-expect "{")
  (rng-c-parse-grammar-body "}"))

(defun rng-c-implicit-grammar-p ()
  (or (and (or (rng-c-current-token-prefixed-name-p)
	       (rng-c-current-token-quoted-identifier-p)
	       (and (rng-c-current-token-ncname-p)
		    (not (rng-c-current-token-keyword-p))))
	   (looking-at "\\["))
      (and (string-equal rng-c-current-token "[")
	   (rng-c-parse-lead-annotation)
	   nil)
      (member rng-c-current-token '("div" "include" ""))
      (looking-at "[|&]?=")))

(defun rng-c-parse-decls ()
  (setq rng-c-default-namespace-declared nil)
  (while (progn
	   (let ((binding
		  (assoc rng-c-current-token
			 '(("namespace" . rng-c-parse-namespace)
			   ("datatypes" . rng-c-parse-datatypes)
			   ("default" . rng-c-parse-default)))))
	     (if binding
		 (progn
		   (rng-c-advance)
		   (funcall (cdr binding))
		   t)
	       nil))))
  (rng-c-declare-standard-datatypes)
  (rng-c-declare-standard-namespaces))

(defun rng-c-parse-datatypes ()
  (let ((prefix (rng-c-parse-identifier-or-keyword)))
    (or (not (assoc prefix rng-c-datatype-decls))
	(rng-c-error "Duplicate datatypes declaration for prefix %s" prefix))
    (rng-c-expect "=")
    (setq rng-c-datatype-decls
	  (cons (cons prefix
		      (rng-make-datatypes-uri (rng-c-parse-literal)))
		rng-c-datatype-decls))))

(defun rng-c-parse-namespace ()
  (rng-c-declare-namespace nil
			   (rng-c-parse-identifier-or-keyword)))

(defun rng-c-parse-default ()
  (rng-c-expect "namespace")
  (rng-c-declare-namespace t
			   (if (string-equal rng-c-current-token "=")
			       nil
			     (rng-c-parse-identifier-or-keyword))))

(defun rng-c-declare-namespace (declare-default prefix)
  (rng-c-expect "=")
  (let ((ns (cond ((string-equal rng-c-current-token "inherit")
		   (rng-c-advance)
		   rng-c-inherit-namespace)
		  (t
		   (nxml-make-namespace (rng-c-parse-literal))))))
    (and prefix
	 (or (not (assoc prefix rng-c-namespace-decls))
	     (rng-c-error "Duplicate namespace declaration for prefix %s"
			  prefix))
	 (setq rng-c-namespace-decls
	       (cons (cons prefix ns) rng-c-namespace-decls)))
    (and declare-default
	 (or (not rng-c-default-namespace-declared)
	     (rng-c-error "Duplicate default namespace declaration"))
	 (setq rng-c-default-namespace-declared t)
	 (setq rng-c-default-namespace ns))))

(defun rng-c-parse-implicit-grammar ()
  (let* ((rng-c-parent-grammar rng-c-current-grammar)
	 (rng-c-current-grammar (rng-c-make-grammar)))
    (rng-c-parse-grammar-body "")
    (rng-c-finish-grammar)))

(defun rng-c-parse-grammar-body (close-token &optional in-include)
  (while (not (string-equal rng-c-current-token close-token))
    (cond ((rng-c-current-token-keyword-p)
	   (let ((kw (intern rng-c-current-token)))
	     (cond ((eq kw 'start)
		    (rng-c-parse-define 'start in-include))
		   ((eq kw 'div)
		    (rng-c-advance)
		    (rng-c-parse-div in-include))
		   ((eq kw 'include)
		    (and in-include
			 (rng-c-error "Nested include"))
		    (rng-c-advance)
		    (rng-c-parse-include))
		   (t (rng-c-error "Invalid grammar keyword")))))
	  ((rng-c-current-token-ncname-p)
	   (if (looking-at "\\[")
	       (rng-c-parse-annotation-element)
	     (rng-c-parse-define rng-c-current-token
				 in-include)))
	  ((rng-c-current-token-quoted-identifier-p)
	   (if (looking-at "\\[")
	       (rng-c-parse-annotation-element)
	     (rng-c-parse-define (substring rng-c-current-token 1)
				 in-include)))
	  ((rng-c-current-token-prefixed-name-p)
	   (rng-c-parse-annotation-element))
	  ((string-equal rng-c-current-token "[")
	   (rng-c-parse-lead-annotation)
	   (and (string-equal rng-c-current-token close-token)
		(rng-c-error "Missing annotation subject"))
	   (and (looking-at "\\[")
		(rng-c-error "Leading annotation applied to annotation")))
	  (t (rng-c-error "Invalid grammar content"))))
  (or (string-equal rng-c-current-token "")
      (rng-c-advance)))

(defun rng-c-parse-div (in-include)
  (rng-c-expect "{")
  (rng-c-parse-grammar-body "}" in-include))

(defun rng-c-parse-include ()
  (let* ((filename (rng-c-expand-file (rng-c-parse-literal)))
	 (rng-c-inherit-namespace (rng-c-parse-opt-inherit))
	 overrides)
    (cond ((string-equal rng-c-current-token "{")
	   (rng-c-advance)
	   (let ((rng-c-overrides nil))
	     (rng-c-parse-grammar-body "}" t)
	     (setq overrides rng-c-overrides))
	   (setq overrides (rng-c-start-include overrides))
	   (rng-c-parse-file filename 'include)
	   (rng-c-end-include overrides))
	  (t (rng-c-parse-file filename 'include)))))

(defun rng-c-parse-define (name in-include)
  (rng-c-advance)
  (let ((assign (assoc rng-c-current-token
		       '(("=" . nil)
			 ("|=" . choice)
			 ("&=" . interleave)))))
    (or assign
	(rng-c-error "Expected assignment operator"))
    (rng-c-advance)
    (let ((ref (rng-c-prepare-define name (cdr assign) in-include)))
      (rng-c-define ref (rng-c-parse-pattern)))))

(defvar rng-c-had-except nil)

(defun rng-c-parse-pattern ()
  (let* ((rng-c-had-except nil)
	 (p (rng-c-parse-repeated))
	 (op (assoc rng-c-current-token
		    '(("|" . rng-make-choice)
		      ("," . rng-make-group)
		      ("&" . rng-make-interleave)))))
    (if op
	(if rng-c-had-except
	    (rng-c-error "Parentheses required around pattern using -")
	  (let* ((patterns (cons p nil))
		 (tail patterns)
		 (connector rng-c-current-token))
	    (while (progn
		     (rng-c-advance)
		     (let ((newcdr (cons (rng-c-parse-repeated) nil)))
		       (setcdr tail newcdr)
		       (setq tail newcdr))
		     (string-equal rng-c-current-token connector)))
	    (funcall (cdr op) patterns)))
      p)))

(defun rng-c-parse-repeated ()
  (let ((p (rng-c-parse-follow-annotations
	    (rng-c-parse-primary)))
	(op (assoc rng-c-current-token
		   '(("*" . rng-make-zero-or-more)
		     ("+" . rng-make-one-or-more)
		     ("?" . rng-make-optional)))))
    (if op
	(if rng-c-had-except
	    (rng-c-error "Parentheses required around pattern using -")
	  (rng-c-parse-follow-annotations
	   (progn
	     (rng-c-advance)
	     (funcall (cdr op) p))))
      p)))

(defun rng-c-parse-primary ()
  "Parse a primary expression.
The current token must be the first token of the expression.
After parsing the current token should be the token following
the primary expression."
  (cond ((rng-c-current-token-keyword-p)
	 (let ((parse-function (get (intern rng-c-current-token)
				    'rng-c-pattern)))
	   (or parse-function
	       (rng-c-error "Keyword %s does not introduce a pattern"
			    rng-c-current-token))
	   (rng-c-advance)
	   (funcall parse-function)))
	((rng-c-current-token-ncname-p)
	 (rng-c-advance-with (rng-c-make-ref rng-c-current-token)))
	((string-equal rng-c-current-token "(")
 	 (rng-c-advance)
	 (let ((p (rng-c-parse-pattern)))
	   (rng-c-expect ")")
	   p))
	((rng-c-current-token-prefixed-name-p)
	 (let ((name (rng-c-expand-datatype rng-c-current-token)))
	   (rng-c-advance)
	   (rng-c-parse-data name)))
	((rng-c-current-token-literal-p)
	 (rng-make-value rng-token-datatype (rng-c-parse-literal) nil))
	((rng-c-current-token-quoted-identifier-p)
	 (rng-c-advance-with
	  (rng-c-make-ref (substring rng-c-current-token 1))))
	((string-equal rng-c-current-token "[")
	 (rng-c-parse-lead-annotation)
	 (rng-c-parse-primary))
	(t (rng-c-error "Invalid pattern"))))

(defun rng-c-parse-parent ()
  (and (rng-c-current-token-keyword-p)
       (rng-c-error "Keyword following parent was not quoted"
		    rng-c-current-token))
  (rng-c-make-parent-ref (rng-c-parse-identifier-or-keyword)))

(defun rng-c-parse-literal ()
  (rng-c-fix-escaped-newlines
   (apply 'concat (rng-c-parse-literal-segments))))

(defun rng-c-parse-literal-segments ()
  (let ((str (rng-c-parse-literal-segment)))
    (cons str
	  (cond ((string-equal rng-c-current-token "~")
		 (rng-c-advance)
		 (rng-c-parse-literal-segments))
		(t nil)))))

(defun rng-c-parse-literal-segment ()
  (or (rng-c-current-token-literal-p)
      (rng-c-error "Expected a literal"))
  (rng-c-advance-with
   (let ((n (if (and (>= (length rng-c-current-token) 6)
		     (eq (aref rng-c-current-token 0)
			 (aref rng-c-current-token 1)))
		3
	      1)))
     (substring rng-c-current-token n (- n)))))

(defun rng-c-fix-escaped-newlines (str)
  (let ((pos 0))
    (while (progn
	     (let ((n (string-match "\C-@" str pos)))
	       (and n
		    (aset str n ?\n)
		    (setq pos (1+ n)))))))
  str)

(defun rng-c-parse-identifier-or-keyword ()
  (cond ((rng-c-current-token-ncname-p)
	 (rng-c-advance-with rng-c-current-token))
	((rng-c-current-token-quoted-identifier-p)
	 (rng-c-advance-with (substring rng-c-current-token 1)))
	(t (rng-c-error "Expected identifier or keyword"))))

(put 'string 'rng-c-pattern 'rng-c-parse-string)
(put 'token 'rng-c-pattern 'rng-c-parse-token)
(put 'element 'rng-c-pattern 'rng-c-parse-element)
(put 'attribute 'rng-c-pattern 'rng-c-parse-attribute)
(put 'list 'rng-c-pattern 'rng-c-parse-list)
(put 'mixed 'rng-c-pattern 'rng-c-parse-mixed)
(put 'text 'rng-c-pattern 'rng-c-parse-text)
(put 'empty 'rng-c-pattern 'rng-c-parse-empty)
(put 'notAllowed 'rng-c-pattern 'rng-c-parse-not-allowed)
(put 'grammar 'rng-c-pattern 'rng-c-parse-grammar)
(put 'parent 'rng-c-pattern 'rng-c-parse-parent)
(put 'external 'rng-c-pattern 'rng-c-parse-external)

(defun rng-c-parse-element ()
  (let ((name-class (rng-c-parse-name-class nil)))
    (rng-c-expect "{")
    (let ((pattern (rng-c-parse-pattern)))
      (rng-c-expect "}")
      (rng-make-element name-class pattern))))

(defun rng-c-parse-attribute ()
  (let ((name-class (rng-c-parse-name-class 'attribute)))
    (rng-c-expect "{")
    (let ((pattern (rng-c-parse-pattern)))
      (rng-c-expect "}")
      (rng-make-attribute name-class pattern))))

(defun rng-c-parse-name-class (attribute)
  (let* ((rng-c-had-except nil)
	 (name-class
	  (rng-c-parse-follow-annotations
	   (rng-c-parse-primary-name-class attribute))))
    (if (string-equal rng-c-current-token "|")
	(let* ((name-classes (cons name-class nil))
	       (tail name-classes))
	  (or (not rng-c-had-except)
	      (rng-c-error "Parentheses required around name-class using - operator"))
	  (while (progn
		   (rng-c-advance)
		   (let ((newcdr
			  (cons (rng-c-parse-follow-annotations
				 (rng-c-parse-primary-name-class attribute))
				nil)))
		     (setcdr tail newcdr)
		     (setq tail newcdr))
		   (string-equal rng-c-current-token "|")))
	  (rng-make-choice-name-class name-classes))
      name-class)))

(defun rng-c-parse-primary-name-class (attribute)
  (cond ((rng-c-current-token-ncname-p)
	 (rng-c-advance-with
	  (rng-make-name-name-class
	   (rng-make-name (rng-c-unqualified-namespace attribute)
			  rng-c-current-token))))
	((rng-c-current-token-prefixed-name-p)
	 (rng-c-advance-with
	  (rng-make-name-name-class
	   (rng-c-expand-name rng-c-current-token))))
	((string-equal rng-c-current-token "*")
	 (let ((except (rng-c-parse-opt-except-name-class attribute)))
	   (if except
	       (rng-make-any-name-except-name-class except)
	     (rng-make-any-name-name-class))))
	((rng-c-current-token-ns-name-p)
	 (let* ((ns
		 (rng-c-lookup-prefix (substring rng-c-current-token
						 0
						 -2)))
		(except (rng-c-parse-opt-except-name-class attribute)))
	   (if except
	       (rng-make-ns-name-except-name-class ns except)
	     (rng-make-ns-name-name-class ns))))
	((string-equal rng-c-current-token "(")
	 (rng-c-advance)
	 (let ((name-class (rng-c-parse-name-class attribute)))
	   (rng-c-expect ")")
	   name-class))
	((rng-c-current-token-quoted-identifier-p)
	 (rng-c-advance-with
	  (rng-make-name-name-class
	   (rng-make-name (rng-c-unqualified-namespace attribute)
			  (substring rng-c-current-token 1)))))
	((string-equal rng-c-current-token "[")
	 (rng-c-parse-lead-annotation)
	 (rng-c-parse-primary-name-class attribute))
	(t (rng-c-error "Bad name class"))))

(defun rng-c-parse-opt-except-name-class (attribute)
  (rng-c-advance)
  (and (string-equal rng-c-current-token "-")
       (or (not rng-c-had-except)
	   (rng-c-error "Parentheses required around name-class using - operator"))
       (setq rng-c-had-except t)
       (progn
	 (rng-c-advance)
	 (rng-c-parse-primary-name-class attribute))))

(defun rng-c-parse-mixed ()
  (rng-c-expect "{")
  (let ((pattern (rng-make-mixed (rng-c-parse-pattern))))
    (rng-c-expect "}")
    pattern))

(defun rng-c-parse-list ()
  (rng-c-expect "{")
  (let ((pattern (rng-make-list (rng-c-parse-pattern))))
    (rng-c-expect "}")
    pattern))

(defun rng-c-parse-text ()
  (rng-make-text))

(defun rng-c-parse-empty ()
  (rng-make-empty))

(defun rng-c-parse-not-allowed ()
  (rng-make-not-allowed))

(defun rng-c-parse-string ()
  (rng-c-parse-data rng-string-datatype))

(defun rng-c-parse-token ()
  (rng-c-parse-data rng-token-datatype))

(defun rng-c-parse-data (name)
  (if (rng-c-current-token-literal-p)
      (rng-make-value name
		      (rng-c-parse-literal)
		      (and (car name)
			   (rng-c-make-context)))
    (let ((params (rng-c-parse-optional-params)))
      (if (string-equal rng-c-current-token "-")
	  (progn
	    (if rng-c-had-except
		(rng-c-error "Parentheses required around pattern using -")
	      (setq rng-c-had-except t))
	    (rng-c-advance)
	    (rng-make-data-except name
				  params
				  (rng-c-parse-primary)))
	(rng-make-data name params)))))

(defun rng-c-parse-optional-params ()
  (and (string-equal rng-c-current-token "{")
       (let* ((head (cons nil nil))
	      (tail head))
	 (rng-c-advance)
	 (while (not (string-equal rng-c-current-token "}"))
	   (and (string-equal rng-c-current-token "[")
		(rng-c-parse-lead-annotation))
	   (let ((name (rng-c-parse-identifier-or-keyword)))
	     (rng-c-expect "=")
	     (let ((newcdr (cons (cons (intern name)
				       (rng-c-parse-literal))
				 nil)))
	       (setcdr tail newcdr)
	       (setq tail newcdr))))
	 (rng-c-advance)
	 (cdr head))))

(defun rng-c-parse-external ()
  (let* ((filename (rng-c-expand-file (rng-c-parse-literal)))
	 (rng-c-inherit-namespace (rng-c-parse-opt-inherit)))
    (rng-c-parse-file filename 'external)))

(defun rng-c-expand-file (uri)
  (condition-case err
      (rng-uri-file-name (rng-uri-resolve uri
					  (rng-file-name-uri rng-c-file-name)))
    (rng-uri-error
     (rng-c-error (cadr err)))))

(defun rng-c-parse-opt-inherit ()
  (cond ((string-equal rng-c-current-token "inherit")
	 (rng-c-advance)
	 (rng-c-expect "=")
	 (rng-c-lookup-prefix (rng-c-parse-identifier-or-keyword)))
	(t rng-c-default-namespace)))

(defun rng-c-parse-grammar ()
  (rng-c-expect "{")
  (let* ((rng-c-parent-grammar rng-c-current-grammar)
	 (rng-c-current-grammar (rng-c-make-grammar)))
    (rng-c-parse-grammar-body "}")
    (rng-c-finish-grammar)))

(defun rng-c-parse-lead-annotation ()
  (rng-c-parse-annotation-body)
  (and (string-equal rng-c-current-token "[")
       (rng-c-error "Multiple leading annotations")))

(defun rng-c-parse-follow-annotations (obj)
  (while (string-equal rng-c-current-token ">>")
    (rng-c-advance)
    (if (rng-c-current-token-prefixed-name-p)
	(rng-c-advance)
      (rng-c-parse-identifier-or-keyword))
    (rng-c-parse-annotation-body t))
  obj)

(defun rng-c-parse-annotation-element ()
  (rng-c-advance)
  (rng-c-parse-annotation-body t))

;; XXX need stricter checking of attribute names
;; XXX don't allow attributes after text

(defun rng-c-parse-annotation-body (&optional allow-text)
  "Current token is [.  Parse up to matching ].
Current token after parse is token following ]."
  (or (string-equal rng-c-current-token "[")
      (rng-c-error "Expected ["))
  (rng-c-advance)
  (while (not (string-equal rng-c-current-token "]"))
    (cond ((rng-c-current-token-literal-p)
	   (or allow-text
	       (rng-c-error "Out of place text within annotation"))
	   (rng-c-parse-literal))
	  (t
	   (if (rng-c-current-token-prefixed-name-p)
	       (rng-c-advance)
	     (rng-c-parse-identifier-or-keyword))
	   (cond ((string-equal rng-c-current-token "[")
		  (rng-c-parse-annotation-body t))
		 ((string-equal rng-c-current-token "=")
		  (rng-c-advance)
		  (rng-c-parse-literal))
		 (t (rng-c-error "Expected = or ["))))))
  (rng-c-advance))

(defun rng-c-advance-with (pattern)
  (rng-c-advance)
  pattern)

(defun rng-c-expect (str)
  (or (string-equal rng-c-current-token str)
      (rng-c-error "Expected `%s' but got `%s'" str rng-c-current-token))
  (rng-c-advance))

(provide 'rng-cmpct)

;;; rng-cmpct.el

