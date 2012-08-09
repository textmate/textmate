;;; semantic/lex-spp.el --- Semantic Lexical Pre-processor

;; Copyright (C) 2006-2012  Free Software Foundation, Inc.

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
;; The Semantic Preprocessor works with semantic-lex to provide a phase
;; during lexical analysis to do the work of a pre-processor.
;;
;; A pre-processor identifies lexical syntax mixed in with another language
;; and replaces some keyword tokens with streams of alternate tokens.
;;
;; If you use SPP in your language, be sure to specify this in your
;; semantic language setup function:
;;
;; (add-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook nil t)
;;
;;
;; Special Lexical Tokens:
;;
;; There are several special lexical tokens that are used by the
;; Semantic PreProcessor lexer.  They are:
;;
;; Declarations:
;;   spp-macro-def - A definition of a lexical macro.
;;   spp-macro-undef - A removal of a definition of a lexical macro.
;;   spp-system-include - A system level include file
;;   spp-include - An include file
;;   spp-concat - A lexical token representing textual concatenation
;;           of symbol parts.
;;
;; Operational tokens:
;;   spp-arg-list - Represents an argument list to a macro.
;;   spp-symbol-merge - A request for multiple symbols to be textually merged.
;;
;;; TODO:
;;
;; Use `semantic-push-parser-warning' for situations where there are likely
;; macros that are undefined unexpectedly, or other problem.
;;
;; TODO:
;;
;; Try to handle the case of:
;;
;; #define NN namespace nn {
;; #define NN_END }
;;
;; NN
;;   int mydecl() {}
;; NN_END
;;

(require 'semantic)
(require 'semantic/lex)

;;; Code:
(defvar semantic-lex-spp-macro-symbol-obarray nil
  "Table of macro keywords used by the Semantic Preprocessor.
These symbols will be used in addition to those in
`semantic-lex-spp-dynamic-macro-symbol-obarray'.")
(make-variable-buffer-local 'semantic-lex-spp-macro-symbol-obarray)

(defvar semantic-lex-spp-project-macro-symbol-obarray nil
  "Table of macro keywords for this project.
These symbols will be used in addition to those in
`semantic-lex-spp-dynamic-macro-symbol-obarray'.")
(make-variable-buffer-local 'semantic-lex-spp-project-macro-symbol-obarray)

(defvar semantic-lex-spp-dynamic-macro-symbol-obarray nil
  "Table of macro keywords used during lexical analysis.
Macros are lexical symbols which are replaced by other lexical
tokens during lexical analysis.  During analysis symbols can be
added and removed from this symbol table.")
(make-variable-buffer-local 'semantic-lex-spp-dynamic-macro-symbol-obarray)

(defvar semantic-lex-spp-dynamic-macro-symbol-obarray-stack nil
  "A stack of obarrays for temporarily scoped macro values.")
(make-variable-buffer-local 'semantic-lex-spp-dynamic-macro-symbol-obarray-stack)

(defvar semantic-lex-spp-expanded-macro-stack nil
  "The stack of lexical SPP macros we have expanded.")
;; The above is not buffer local.  Some macro expansions need to be
;; dumped into a secondary buffer for re-lexing.

;;; NON-RECURSIVE MACRO STACK
;; C Pre-processor does not allow recursive macros.  Here are some utils
;; for managing the symbol stack of where we've been.

(defmacro semantic-lex-with-macro-used (name &rest body)
  "With the macro NAME currently being expanded, execute BODY.
Pushes NAME into the macro stack.  The above stack is checked
by `semantic-lex-spp-symbol' to not return true for any symbol
currently being expanded."
  `(unwind-protect
       (progn
	 (push ,name semantic-lex-spp-expanded-macro-stack)
	 ,@body)
     (pop semantic-lex-spp-expanded-macro-stack)))
(put 'semantic-lex-with-macro-used 'lisp-indent-function 1)

(add-hook
 'edebug-setup-hook
 #'(lambda ()

     (def-edebug-spec semantic-lex-with-macro-used
       (symbolp def-body)
       )

     ))

;;; MACRO TABLE UTILS
;;
;; The dynamic macro table is a buffer local variable that is modified
;; during the analysis.  OBARRAYs are used, so the language must
;; have symbols that are compatible with Emacs Lisp symbols.
;;
(defsubst semantic-lex-spp-symbol (name)
  "Return spp symbol with NAME or nil if not found.
The search priority is:
  1. DYNAMIC symbols
  2. PROJECT specified symbols.
  3. SYSTEM specified symbols."
  (and
   ;; Only strings...
   (stringp name)
   ;; Make sure we don't recurse.
   (not (member name semantic-lex-spp-expanded-macro-stack))
   ;; Do the check of the various tables.
   (or
    ;; DYNAMIC
    (and (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
	 (intern-soft name semantic-lex-spp-dynamic-macro-symbol-obarray))
    ;; PROJECT
    (and (arrayp semantic-lex-spp-project-macro-symbol-obarray)
	 (intern-soft name semantic-lex-spp-project-macro-symbol-obarray))
    ;; SYSTEM
    (and (arrayp semantic-lex-spp-macro-symbol-obarray)
	 (intern-soft name semantic-lex-spp-macro-symbol-obarray))
    ;; ...
    )))

(defsubst semantic-lex-spp-symbol-p (name)
  "Return non-nil if a keyword with NAME exists in any keyword table."
  (if (semantic-lex-spp-symbol name)
      t))

(defsubst semantic-lex-spp-dynamic-map ()
  "Return the dynamic macro map for the current buffer."
  (or semantic-lex-spp-dynamic-macro-symbol-obarray
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray
	    (make-vector 13 0))))

(defsubst semantic-lex-spp-dynamic-map-stack ()
  "Return the dynamic macro map for the current buffer."
  (or semantic-lex-spp-dynamic-macro-symbol-obarray-stack
      (setq semantic-lex-spp-dynamic-macro-symbol-obarray-stack
	    (make-vector 13 0))))

(defun semantic-lex-spp-value-valid-p (value)
  "Return non-nil if VALUE is valid."
  (or (null value)
      (stringp value)
      (and (consp value)
	   (or (semantic-lex-token-p (car value))
	       (eq (car (car value)) 'spp-arg-list)))))

(defvar semantic-lex-spp-debug-symbol nil
  "A symbol to break on if it is being set somewhere.")

(defun semantic-lex-spp-enable-debug-symbol (sym)
  "Enable debugging for symbol SYM.
Disable debugging by entering nothing."
  (interactive "sSymbol: ")
  (if (string= sym "")
      (setq semantic-lex-spp-debug-symbol nil)
    (setq semantic-lex-spp-debug-symbol sym)))

(defmacro semantic-lex-spp-validate-value (name value)
  "Validate the NAME and VALUE of a macro before it is set."
;  `(progn
;     (when (not (semantic-lex-spp-value-valid-p ,value))
;       (error "Symbol \"%s\" with bogus value %S" ,name ,value))
;     (when (and semantic-lex-spp-debug-symbol
;		(string= semantic-lex-spp-debug-symbol name))
;       (debug))
;     )
  nil
  )

(defun semantic-lex-spp-symbol-set (name value &optional obarray-in)
  "Set value of spp symbol with NAME to VALUE and return VALUE.
If optional OBARRAY-IN is non-nil, then use that obarray instead of
the dynamic map."
  (semantic-lex-spp-validate-value name value)
  (if (and (stringp value) (string= value "")) (setq value nil))
  (set (intern name (or obarray-in
			(semantic-lex-spp-dynamic-map)))
       value))

(defsubst semantic-lex-spp-symbol-remove (name &optional obarray)
  "Remove the spp symbol with NAME.
If optional OBARRAY is non-nil, then use that obarray instead of
the dynamic map."
  (unintern name (or obarray
		     (semantic-lex-spp-dynamic-map))))

(defun semantic-lex-spp-symbol-push (name value)
  "Push macro NAME with VALUE into the map.
Reverse with `semantic-lex-spp-symbol-pop'."
  (semantic-lex-spp-validate-value name value)
  (let* ((map (semantic-lex-spp-dynamic-map))
	 (stack (semantic-lex-spp-dynamic-map-stack))
	 (mapsym (intern name map))
	 (stacksym (intern name stack))
	 (mapvalue (when (boundp mapsym) (symbol-value mapsym)))
	 )
    (when (boundp mapsym)
      ;; Make sure there is a stack
      (if (not (boundp stacksym)) (set stacksym nil))
      ;; If there is a value to push, then push it.
      (set stacksym (cons mapvalue (symbol-value stacksym)))
      )
    ;; Set our new value here.
    (set mapsym value)
    ))

(defun semantic-lex-spp-symbol-pop (name)
  "Pop macro NAME from the stackmap into the orig map.
Reverse with `semantic-lex-spp-symbol-pop'."
  (let* ((map (semantic-lex-spp-dynamic-map))
	 (stack (semantic-lex-spp-dynamic-map-stack))
	 (mapsym (intern name map))
	 (stacksym (intern name stack))
	 (oldvalue nil)
	 )
    (if (or (not (boundp stacksym) )
	    (= (length (symbol-value stacksym)) 0))
	;; Nothing to pop, remove it.
	(unintern name map)
      ;; If there is a value to pop, then add it to the map.
      (set mapsym (car (symbol-value stacksym)))
      (set stacksym (cdr (symbol-value stacksym)))
      )))

(defsubst semantic-lex-spp-symbol-stream (name)
  "Return replacement stream of macro with NAME."
  (let ((spp (semantic-lex-spp-symbol name)))
    (if spp
        (symbol-value spp))))

(defun semantic-lex-make-spp-table (specs)
  "Convert spp macro list SPECS into an obarray and return it.
SPECS must be a list of (NAME . REPLACEMENT) elements, where:

NAME is the name of the spp macro symbol to define.
REPLACEMENT a string that would be substituted in for NAME."

  ;; Create the symbol hash table
  (let ((semantic-lex-spp-macro-symbol-obarray (make-vector 13 0))
        spec)
    ;; fill it with stuff
    (while specs
      (setq spec  (car specs)
            specs (cdr specs))
      (semantic-lex-spp-symbol-set
       (car spec)
       (cdr spec)
       semantic-lex-spp-macro-symbol-obarray))
    semantic-lex-spp-macro-symbol-obarray))

(defun semantic-lex-spp-save-table ()
  "Return a list of spp macros and values.
The return list is meant to be saved in a semanticdb table."
  (let (macros)
    (when (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons (cons (symbol-name symbol)
				    (symbol-value symbol))
			      macros)))
       semantic-lex-spp-dynamic-macro-symbol-obarray))
    macros))

(defun semantic-lex-spp-macros ()
  "Return a list of spp macros as Lisp symbols.
The value of each symbol is the replacement stream."
  (let (macros)
    (when (arrayp semantic-lex-spp-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-macro-symbol-obarray))
    (when (arrayp semantic-lex-spp-project-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-project-macro-symbol-obarray))
    (when (arrayp semantic-lex-spp-dynamic-macro-symbol-obarray)
      (mapatoms
       #'(lambda (symbol)
	   (setq macros (cons symbol macros)))
       semantic-lex-spp-dynamic-macro-symbol-obarray))
    macros))

(defun semantic-lex-spp-set-dynamic-table (new-entries)
  "Set the dynamic symbol table to NEW-ENTRIES.
For use with semanticdb restoration of state."
  (dolist (e new-entries)
    ;; Default obarray for below is the dynamic map.
    (semantic-lex-spp-symbol-set (car e) (cdr e))))

(defun semantic-lex-spp-reset-hook (start end)
  "Reset anything needed by SPP for parsing.
In this case, reset the dynamic macro symbol table if
START is (point-min).
END is not used."
  (when (= start (point-min))
    (setq semantic-lex-spp-dynamic-macro-symbol-obarray nil
	  semantic-lex-spp-dynamic-macro-symbol-obarray-stack nil
	  ;; This shouldn't not be nil, but reset just in case.
	  semantic-lex-spp-expanded-macro-stack nil)
    ))

;;; MACRO EXPANSION: Simple cases
;;
;; If a user fills in the table with simple strings, we can
;; support that by converting them into tokens with the
;; various analyzers that are available.

(defun semantic-lex-spp-extract-regex-and-compare (analyzer value)
  "Extract a regexp from an ANALYZER and use to match VALUE.
Return non-nil if it matches"
  (let* ((condition (car analyzer))
	 (regex (cond ((eq (car condition) 'looking-at)
		       (nth 1 condition))
		      (t
		       nil))))
    (when regex
      (string-match regex value))
    ))

(defun semantic-lex-spp-simple-macro-to-macro-stream (val beg end argvalues)
  "Convert lexical macro contents VAL into a macro expansion stream.
These are for simple macro expansions that a user may have typed in directly.
As such, we need to analyze the input text, to figure out what kind of real
lexical token we should be inserting in its place.

Argument VAL is the value of some macro to be converted into a stream.
BEG and END are the token bounds of the macro to be expanded
that will somehow gain a much longer token stream.
ARGVALUES are values for any arg list, or nil."
  (cond
   ;; We perform a replacement.  Technically, this should
   ;; be a full lexical step over the "val" string, but take
   ;; a guess that its just a keyword or existing symbol.
   ;;
   ;; Probably a really bad idea.  See how it goes.
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-symbol-or-keyword val)
    (semantic-lex-push-token
     (semantic-lex-token (or (semantic-lex-keyword-p val) 'symbol)
			 beg end
			 val)))

   ;; Ok, the rest of these are various types of syntax.
   ;; Conveniences for users that type in their symbol table.
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-punctuation val)
    (semantic-lex-token 'punctuation beg end val))
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-number val)
    (semantic-lex-token 'number beg end val))
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-paren-or-list val)
    (semantic-lex-token 'semantic-list beg end val))
   ((semantic-lex-spp-extract-regex-and-compare
     semantic-lex-string val)
    (semantic-lex-token 'string beg end val))
   (t nil)
   ))

;;; MACRO EXPANSION : Lexical token replacement
;;
;; When substituting in a macro from a token stream of formatted
;; semantic lex tokens, things can be much more complicated.
;;
;; Some macros have arguments that get set into the dynamic macro
;; table during replacement.
;;
;; In general, the macro tokens are substituted into the regular
;; token stream, but placed under the characters of the original
;; macro symbol.
;;
;; Argument lists are saved as a lexical token at the beginning
;; of a replacement value.

(defun semantic-lex-spp-one-token-to-txt (tok &optional blocktok)
  "Convert the token TOK into a string.
If TOK is made of multiple tokens, convert those to text.  This
conversion is needed if a macro has a merge symbol in it that
combines the text of two previously distinct symbols.  For
example, in c:

#define (a,b) a ## b;

If optional string BLOCKTOK matches the expanded value, then do not
continue processing recursively."
  (let ((txt (semantic-lex-token-text tok))
	(sym nil)
	)
    (cond
     ;; Recursion prevention
     ((and (stringp blocktok) (string= txt blocktok))
      blocktok)
     ;; A complex symbol
     ((and (eq (car tok) 'symbol)
	   (setq sym (semantic-lex-spp-symbol txt))
	   (not (semantic-lex-spp-macro-with-args (symbol-value sym)))
	   )
      ;; Now that we have a symbol,
      (let ((val (symbol-value sym)))
	(cond
	 ;; This is another lexical token.
	 ((and (consp val)
	       (symbolp (car val)))
	  (semantic-lex-spp-one-token-to-txt val txt))
	 ;; This is a list of tokens.
	 ((and (consp val)
	       (consp (car val))
	       (symbolp (car (car val))))
	  (mapconcat (lambda (subtok)
		       (semantic-lex-spp-one-token-to-txt subtok))
		     val
		     ""))
	 ;; If val is nil, that's probably wrong.
	 ;; Found a system header case where this was true.
	 ((null val) "")
	 ;; Debug weird stuff.
	 (t (debug)))
	))
     ((stringp txt)
      txt)
     (t nil))
    ))

(defun semantic-lex-spp-macro-with-args (val)
  "If the macro value VAL has an argument list, return the arglist."
  (when (and val (consp val) (consp (car val))
	     (eq 'spp-arg-list (car (car val))))
    (car (cdr (car val)))))

(defun semantic-lex-spp-token-macro-to-macro-stream (val beg end argvalues)
  "Convert lexical macro contents VAL into a macro expansion stream.
Argument VAL is the value of some macro to be converted into a stream.
BEG and END are the token bounds of the macro to be expanded
that will somehow gain a much longer token stream.
ARGVALUES are values for any arg list, or nil.
See comments in code for information about how token streams are processed
and what valid VAL values are."

  ;; A typical VAL value might be either a stream of tokens.
  ;; Tokens saved into a macro stream always includes the text from the
  ;; buffer, since the locations specified probably don't represent
  ;; that text anymore, or even the same buffer.
  ;;
  ;; CASE 1: Simple token stream
  ;;
  ;; #define SUPER mysuper::
  ;;  ==>
  ;;((symbol "mysuper" 480 . 487)
  ;; (punctuation ":" 487 . 488)
  ;; (punctuation ":" 488 . 489))
  ;;
  ;; CASE 2: Token stream with argument list
  ;;
  ;; #define INT_FCN(name) int name (int in)
  ;;  ==>
  ;; ((spp-arg-list ("name") 558 . 564)
  ;;  (INT "int" 565 . 568)
  ;;  (symbol "name" 569 . 573)
  ;;  (semantic-list "(int in)" 574 . 582))
  ;;
  ;; In the second case, a macro with an argument list as the a rgs as the
  ;; first entry.
  ;;
  ;; CASE 3: Symbol text merge
  ;;
  ;; #define TMP(a) foo_ ## a
  ;;   ==>
  ;; ((spp-arg-list ("a") 20 . 23)
  ;;  (spp-symbol-merge ((symbol "foo_" 24 . 28) (symbol "a" 32 . 33))
  ;; 		          24 . 33))
  ;;
  ;; Usually in conjunction with a macro with an argument, merging symbol
  ;; parts is a way of fabricating new symbols from pieces inside the macro.
  ;; These macros use `spp-symbol-merge' tokens whose TEXT part is another
  ;; token stream.  This sub-stream ought to consist of only 2 SYMBOL pieces,
  ;; though I suppose keywords might be ok.  The end result of this example
  ;; merge symbol would be (symbol "foo_A" 24 . 33) where A is the symbol
  ;; passed in from the arg list "a".
  ;;
  ;; CASE 4: Nested token streams
  ;;
  ;; #define FOO(f) f
  ;; #define BLA bla FOO(foo)
  ;;  ==>
  ;; ((INT "int" 82 . 85)
  ;;  (symbol "FOO" 86 . 89)
  ;;  (semantic-list "(foo)" 89 . 94))
  ;;
  ;; Nested token FOO shows up in the table of macros, and gets replace
  ;; inline.  This is the same as case 2.

  (let ((arglist (semantic-lex-spp-macro-with-args val))
	(argalist nil)
	(val-tmp nil)
	(v nil)
	)
    ;; CASE 2: Dealing with the arg list.
    (when arglist
      ;;  Skip the arg list.
      (setq val (cdr val))

      ;; Push args into the replacement list.
      (let ((AV argvalues))
	(dolist (A arglist)
	  (let* ((argval (car AV)))

	    (semantic-lex-spp-symbol-push A argval)
	    (setq argalist (cons (cons A argval) argalist))
	    (setq AV (cdr AV)))))
      )

    ;; Set val-tmp after stripping arguments.
    (setq val-tmp val)

    ;; CASE 1: Push everything else onto the list.
    ;;   Once the arg list is stripped off, CASE 2 is the same
    ;;   as CASE 1.
    (while val-tmp
      (setq v (car val-tmp))
      (setq val-tmp (cdr val-tmp))

      (let* (;; The text of the current lexical token.
	     (txt (car (cdr v)))
	     ;; Try to convert txt into a macro declaration.  If it is
	     ;; not a macro, use nil.
	     (txt-macro-or-nil (semantic-lex-spp-symbol txt))
	     ;; If our current token is a macro, then pull off the argument
	     ;; list.
	     (macro-and-args
	      (when txt-macro-or-nil
		(semantic-lex-spp-macro-with-args (symbol-value txt-macro-or-nil)))
	      )
	     ;; We need to peek at the next token when testing for
	     ;; used macros with arg lists.
	     (next-tok-class (semantic-lex-token-class (car val-tmp)))
	     )

	(cond
	 ;; CASE 3: Merge symbols together.
	 ((eq (semantic-lex-token-class v) 'spp-symbol-merge)
	  ;; We need to merge the tokens in the 'text segment together,
	  ;; and produce a single symbol from it.
	  (let ((newsym
		 (mapconcat (lambda (tok)
			      (semantic-lex-spp-one-token-to-txt tok))
			    txt
			    "")))
	    (semantic-lex-push-token
	     (semantic-lex-token 'symbol beg end newsym))
	    ))

	 ;; CASE 2: Argument replacement.   If a discovered symbol is in
	 ;;    the active list of arguments, then we need to substitute
	 ;;    in the new value.
	 ((and (eq (semantic-lex-token-class v) 'symbol) txt-macro-or-nil
	       (or (and macro-and-args (eq next-tok-class 'semantic-list))
		   (not macro-and-args))
	       )
	  (let ((AV nil))
	    (when macro-and-args
	      (setq AV
		    (semantic-lex-spp-stream-for-arglist (car val-tmp)))
	      ;; We used up these args.  Pull from the stream.
	      (setq val-tmp (cdr val-tmp))
	      )

	    (semantic-lex-with-macro-used txt
	      ;; Don't recurse directly into this same fcn, because it is
	      ;; convenient to have plain string replacements too.
	      (semantic-lex-spp-macro-to-macro-stream
	       (symbol-value txt-macro-or-nil)
	       beg end AV))
	    ))

	 ;; This is a HACK for the C parser.  The 'macros text
	 ;; property is some storage so that the parser can do
	 ;; some C specific text manipulations.
	 ((eq (semantic-lex-token-class v) 'semantic-list)
	  ;; Push our arg list onto the semantic list.
	  (when argalist
	    (setq txt (concat txt)) ; Copy the text.
	    (put-text-property 0 1 'macros argalist txt))
	  (semantic-lex-push-token
	   (semantic-lex-token (semantic-lex-token-class v) beg end txt))
	  )

	 ;; CASE 1: Just another token in the stream.
	 (t
	  ;; Nothing new.
	  (semantic-lex-push-token
	   (semantic-lex-token (semantic-lex-token-class v) beg end txt))
	  )
	 )))

    ;; CASE 2: The arg list we pushed onto the symbol table
    ;;         must now be removed.
    (dolist (A arglist)
      (semantic-lex-spp-symbol-pop A))
    ))

;;; Macro Merging
;;
;; Used when token streams from different macros include each other.
;; Merged macro streams perform in place replacements.

(defun semantic-lex-spp-merge-streams (raw-stream)
  "Merge elements from the RAW-STREAM together.
Handle spp-concat symbol concatenation.
Handle Nested macro replacements.
Return the cooked stream."
  (let ((cooked-stream nil))
    ;; Merge the stream
    (while raw-stream
      (cond ((eq (semantic-lex-token-class (car raw-stream)) 'spp-concat)
	     ;; handle hashhash, by skipping it.
	     (setq raw-stream (cdr raw-stream))
	     ;; Now merge the symbols.
	     (let ((prev-tok (car cooked-stream))
		   (next-tok (car raw-stream)))
	       (setq cooked-stream (cdr cooked-stream))
	       (push (semantic-lex-token
		      'spp-symbol-merge
		      (semantic-lex-token-start prev-tok)
		      (semantic-lex-token-end next-tok)
		      (list prev-tok next-tok))
		     cooked-stream)
	       ))
	    (t
	     (push (car raw-stream) cooked-stream))
	    )
      (setq raw-stream (cdr raw-stream))
      )

    (nreverse cooked-stream))
  )

;;; MACRO EXPANSION
;;
;; There are two types of expansion.
;;
;; 1. Expansion using a value made up of lexical tokens.
;; 2. User input replacement from a plain string.

(defun semantic-lex-spp-macro-to-macro-stream (val beg end argvalues)
  "Convert lexical macro contents VAL into a macro expansion stream.
Argument VAL is the value of some macro to be converted into a stream.
BEG and END are the token bounds of the macro to be expanded
that will somehow gain a much longer token stream.
ARGVALUES are values for any arg list, or nil."
  (cond
   ;; If val is nil, then just skip it.
   ((null val) t)
   ;; If it is a token, then return that token rebuilt.
   ((and (consp val) (car val) (symbolp (car val)))
    (semantic-lex-push-token
     (semantic-lex-token (car val) beg end (semantic-lex-token-text val))))
   ;; Test for a token list.
   ((and (consp val) (consp (car val)) (car (car val))
	 (symbolp (car (car val))))
    (semantic-lex-spp-token-macro-to-macro-stream val beg end argvalues))
   ;; Test for miscellaneous strings.
   ((stringp val)
    (semantic-lex-spp-simple-macro-to-macro-stream val beg end argvalues))
   ))

;;; --------------------------------------------------------
;;;
;;; ANALYZERS:
;;;

;;; Symbol Is Macro
;;
;; An analyzer that will push tokens from a macro in place
;; of the macro symbol.
;;
(defun semantic-lex-spp-anlyzer-do-replace (sym val beg end)
  "Do the lexical replacement for SYM with VAL.
Argument BEG and END specify the bounds of SYM in the buffer."
  (if (not val)
      (setq semantic-lex-end-point end)
    (let ((arg-in nil)
	  (arg-parsed nil)
	  (arg-split nil)
	  )

      ;; Check for arguments.
      (setq arg-in (semantic-lex-spp-macro-with-args val))

      (when arg-in
	(save-excursion
	  (goto-char end)
	  (setq arg-parsed
		(semantic-lex-spp-one-token-and-move-for-macro
		 ;; NOTE: This used to be (point-at-eol), but
		 ;;       that was too close for multi-line arguments
		 ;;       to a macro.  Point max may be too far if there
		 ;;       is a typo in the buffer.
		 ;;
		 ;; Look here for performance issues while a user is typing
		 ;; incomplete code.
		 (point-max)))
	  (setq end (semantic-lex-token-end arg-parsed))

	  (when (and (listp arg-parsed) (eq (car arg-parsed) 'semantic-list))
	    (setq arg-split
		  ;; Use lex to split up the contents of the argument list.
		  (semantic-lex-spp-stream-for-arglist arg-parsed)
		  ))
	  ))

      ;; if we have something to sub in, then do it.
      (semantic-lex-spp-macro-to-macro-stream val beg end arg-split)
      (setq semantic-lex-end-point end)
      )
    ))

(defvar semantic-lex-spp-replacements-enabled t
  "Non-nil means do replacements when finding keywords.
Disable this only to prevent recursive expansion issues.")

(defun semantic-lex-spp-analyzer-push-tokens-for-symbol (str beg end)
  "Push lexical tokens for the symbol or keyword STR.
STR occurs in the current buffer between BEG and END."
  (let (sym val count)
    (cond
     ;;
     ;; It is a macro.  Prepare for a replacement.
     ((and semantic-lex-spp-replacements-enabled
	   (semantic-lex-spp-symbol-p str))
      (setq sym (semantic-lex-spp-symbol str)
	    val (symbol-value sym)
	    count 0)

      (let ((semantic-lex-spp-expanded-macro-stack
	     semantic-lex-spp-expanded-macro-stack))

	(semantic-lex-with-macro-used str
	  ;; Do direct replacements of single value macros of macros.
	  ;; This solves issues with a macro containing one symbol that
	  ;; is another macro, and get arg lists passed around.
	  (while (and val (consp val)
		      (semantic-lex-token-p (car val))
		      (eq (length val) 1)
		      (eq (semantic-lex-token-class (car val)) 'symbol)
		      (semantic-lex-spp-symbol-p (semantic-lex-token-text (car val)))
		      (< count 10)
		      )
	    (setq str (semantic-lex-token-text (car val)))
	    (setq sym (semantic-lex-spp-symbol str)
		  val (symbol-value sym))
	    ;; Prevent recursion
	    (setq count (1+ count))
	    ;; This prevents a different kind of recursion.
	    (push str semantic-lex-spp-expanded-macro-stack)
	    )

	  (semantic-lex-spp-anlyzer-do-replace sym val beg end))

	))
     ;; Anything else.
     (t
      ;; A regular keyword.
      (semantic-lex-push-token
       (semantic-lex-token (or (semantic-lex-keyword-p str) 'symbol)
			   beg end))))
    ))

(define-lex-regex-analyzer semantic-lex-spp-replace-or-symbol-or-keyword
  "Like 'semantic-lex-symbol-or-keyword' plus preprocessor macro replacement."
  "\\(\\sw\\|\\s_\\)+"
  (let ((str (match-string 0))
	(beg (match-beginning 0))
	(end (match-end 0)))
    (semantic-lex-spp-analyzer-push-tokens-for-symbol str beg end)))

;;; ANALYZERS FOR NEW MACROS
;;
;; These utilities and analyzer declaration function are for
;; creating an analyzer which produces new macros in the macro table.
;;
;; There are two analyzers.  One for new macros, and one for removing
;; a macro.

(defun semantic-lex-spp-first-token-arg-list (token)
  "If TOKEN is a semantic-list, turn it into an SPP ARG LIST."
  (when (and (consp token)
	     (symbolp (car token))
	     (eq 'semantic-list (car token)))
    ;; Convert TOKEN in place.
    (let ((argsplit (split-string (semantic-lex-token-text token)
				  "[(), ]" t)))
      (setcar token 'spp-arg-list)
      (setcar (nthcdr 1 token) argsplit))
    ))

(defun semantic-lex-spp-one-token-and-move-for-macro (max)
  "Lex up one token, and move to end of that token.
Don't go past MAX."
  (let ((ans (semantic-lex (point) max 0 0)))
    (if (not ans)
	(progn (goto-char max)
	       nil)
      (when (> (semantic-lex-token-end (car ans)) max)
	(let ((bounds (semantic-lex-token-bounds (car ans))))
	  (setcdr bounds max)))
      (goto-char (semantic-lex-token-end (car ans)))
      (car ans))
    ))

(defun semantic-lex-spp-stream-for-arglist (token)
  "Lex up the contents of the arglist TOKEN.
Parsing starts inside the parens, and ends at the end of TOKEN."
  (let ((end (semantic-lex-token-end token))
	(fresh-toks nil)
	(toks nil))
    (save-excursion

      (if (stringp (nth 1 token))
	  ;; If the 2nd part of the token is a string, then we have
	  ;; a token specifically extracted from a buffer.  Possibly
	  ;; a different buffer.  This means we need to do something
	  ;; nice to parse its contents.
	  (let ((txt (semantic-lex-token-text token)))
	    (semantic-lex-spp-lex-text-string
	     (substring txt 1 (1- (length txt)))))

	;; This part is like the original
	(goto-char (semantic-lex-token-start token))
	;; A cheat for going into the semantic list.
	(forward-char 1)
	(setq fresh-toks (semantic-lex-spp-stream-for-macro (1- end)))
	(dolist (tok fresh-toks)
	  (when (memq (semantic-lex-token-class tok) '(symbol semantic-list))
	    (setq toks (cons tok toks))))

	(nreverse toks)))))

(defvar semantic-lex-spp-hack-depth 0
  "Current depth of recursive calls to `semantic-lex-spp-lex-text-string'.")

(defun semantic-lex-spp-lex-text-string (text)
  "Lex the text string TEXT using the current buffer's state.
Use this to parse text extracted from a macro as if it came from
the current buffer.  Since the lexer is designed to only work in
a buffer, we need to create a new buffer, and populate it with rules
and variable state from the current buffer."
  (let* ((semantic-lex-spp-hack-depth (1+ semantic-lex-spp-hack-depth))
	 (buf (get-buffer-create (format " *SPP parse hack %d*"
					 semantic-lex-spp-hack-depth)))
	 (mode major-mode)
	 (fresh-toks nil)
	 (toks nil)
	 (origbuff (current-buffer))
	 (important-vars '(semantic-lex-spp-macro-symbol-obarray
			   semantic-lex-spp-project-macro-symbol-obarray
			   semantic-lex-spp-dynamic-macro-symbol-obarray
			   semantic-lex-spp-dynamic-macro-symbol-obarray-stack
			   semantic-lex-spp-expanded-macro-stack
			   ))
	 )
    (if (> semantic-lex-spp-hack-depth 5)
	nil
      (with-current-buffer buf
	(erase-buffer)
	;; Below is a painful hack to make sure everything is setup correctly.
	(when (not (eq major-mode mode))
	  (save-match-data

	    ;; Protect against user-hooks that throw errors.
	    (condition-case nil
		(funcall mode)
	      (error nil))

	    ;; Hack in mode-local
	    (activate-mode-local-bindings)

	    ;; CHEATER!  The following 3 lines are from
	    ;; `semantic-new-buffer-fcn', but we don't want to turn
	    ;; on all the other annoying modes for this little task.
	    (setq semantic-new-buffer-fcn-was-run t)
	    (semantic-lex-init)
	    (semantic-clear-toplevel-cache)
	    (remove-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook
			 t)
	    ))

	;; Second Cheat: copy key variables regarding macro state from the
	;; the originating buffer we are parsing.  We need to do this every time
	;; since the state changes.
	(dolist (V important-vars)
	  (set V (semantic-buffer-local-value V origbuff)))
	(insert text)
	(goto-char (point-min))

	(setq fresh-toks (semantic-lex-spp-stream-for-macro (point-max))))

      (dolist (tok fresh-toks)
	(when (memq (semantic-lex-token-class tok) '(symbol semantic-list))
	  (setq toks (cons tok toks)))))

    (nreverse toks)))

;;;; FIRST DRAFT
;; This is the fist version of semantic-lex-spp-stream-for-arglist
;; that worked pretty well.  It doesn't work if the TOKEN was derived
;; from some other buffer, in which case it can get the wrong answer
;; or throw an error if the token location in the originating buffer is
;; larger than the current buffer.
;;(defun semantic-lex-spp-stream-for-arglist-orig (token)
;;  "Lex up the contents of the arglist TOKEN.
;; Parsing starts inside the parens, and ends at the end of TOKEN."
;;  (save-excursion
;;    (let ((end (semantic-lex-token-end token))
;;	  (fresh-toks nil)
;;	  (toks nil))
;;      (goto-char (semantic-lex-token-start token))
;;      ;; A cheat for going into the semantic list.
;;      (forward-char 1)
;;      (setq fresh-toks (semantic-lex-spp-stream-for-macro (1- end)))
;;      (dolist (tok fresh-toks)
;;	(when (memq (semantic-lex-token-class tok) '(symbol semantic-list))
;;	  (setq toks (cons tok toks))))
;;      (nreverse toks))
;;    ))

;;;; USING SPLIT
;; This doesn't work, because some arguments passed into a macro
;; might contain non-simple symbol words, which this doesn't handle.
;;
;; Thus, you need a full lex to occur.
;; (defun semantic-lex-spp-stream-for-arglist-split (token)
;;   "Lex up the contents of the arglist TOKEN.
;; Parsing starts inside the parens, and ends at the end of TOKEN."
;;   (let* ((txt (semantic-lex-token-text token))
;; 	 (split (split-string (substring txt 1 (1- (length txt)))
;; 			      "(), " t))
;; 	 ;; Hack for lexing.
;; 	 (semantic-lex-spp-analyzer-push-tokens-for-symbol nil))
;;     (dolist (S split)
;;       (semantic-lex-spp-analyzer-push-tokens-for-symbol S 0 1))
;;     (reverse semantic-lex-spp-analyzer-push-tokens-for-symbol)))


(defun semantic-lex-spp-stream-for-macro (eos)
  "Lex up a stream of tokens for a #define statement.
Parsing starts at the current point location.
EOS is the end of the stream to lex for this macro."
  (let ((stream nil))
    (while (< (point) eos)
      (let* ((tok (semantic-lex-spp-one-token-and-move-for-macro eos))
	     (str (when tok
		    (semantic-lex-token-text tok)))
	     )
	(if str
	    (push (semantic-lex-token (semantic-lex-token-class tok)
				      (semantic-lex-token-start tok)
				      (semantic-lex-token-end tok)
				      str)
		  stream)
	  ;; Nothing to push.
	  nil)))
    (goto-char eos)
    ;; Fix the order
    (nreverse stream)
    ))

(defmacro define-lex-spp-macro-declaration-analyzer (name doc regexp tokidx
							  &rest valform)
  "Define a lexical analyzer for defining new MACROS.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-def' is to be created.
VALFORM are forms that return the value to be saved for this macro, or nil.
When implementing a macro, you can use `semantic-lex-spp-stream-for-macro'
to convert text into a lexical stream for storage in the macro."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end"))
	(val (make-symbol "val"))
	(startpnt (make-symbol "startpnt"))
	(endpnt (make-symbol "endpnt")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     (,startpnt semantic-lex-end-point)
	     (,val (save-match-data ,@valform))
	     (,endpnt semantic-lex-end-point))
	 (semantic-lex-spp-symbol-set
	  (buffer-substring-no-properties ,start ,end)
	  ,val)
	 (semantic-lex-push-token
	  (semantic-lex-token 'spp-macro-def
			      ,start ,end))
	 ;; Preserve setting of the end point from the calling macro.
	 (when (and (/= ,startpnt ,endpnt)
		    (/= ,endpnt semantic-lex-end-point))
	   (setq semantic-lex-end-point ,endpnt))
	 ))))

(defmacro define-lex-spp-macro-undeclaration-analyzer (name doc regexp tokidx)
  "Undefine a lexical analyzer for defining new MACROS.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-undef' is to be created."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     )
	 (semantic-lex-spp-symbol-remove
	  (buffer-substring-no-properties ,start ,end))
	 (semantic-lex-push-token
	  (semantic-lex-token 'spp-macro-undef
			      ,start ,end))
	 ))))

;;; INCLUDES
;;
;; These analyzers help a language define how include files
;; are identified.  These are ONLY for languages that perform
;; an actual textual inclusion, and not for imports.
;;
;; This section is supposed to allow the macros from the headers to be
;; added to the local dynamic macro table, but that hasn't been
;; written yet.
;;
(defcustom semantic-lex-spp-use-headers-flag nil
  "*Non-nil means to pre-parse headers as we go.
For languages that use the Semantic pre-processor, this can
improve the accuracy of parsed files where include files
can change the state of what's parsed in the current file.

Note: Note implemented yet"
  :group 'semantic
  :type 'boolean)

(defun semantic-lex-spp-merge-header (name)
  "Extract and merge any macros from the header with NAME.
Finds the header file belonging to NAME, gets the macros
from that file, and then merge the macros with our current
symbol table."
  (when semantic-lex-spp-use-headers-flag
    ;; @todo - do this someday, ok?
    ))

(defmacro define-lex-spp-include-analyzer (name doc regexp tokidx
						&rest valform)
  "Define a lexical analyzer for defining a new INCLUDE lexical token.
Macros defined in the found include will be added to our running table
at the time the include statement is found.
NAME is the name of the analyzer.
DOC is the documentation for the analyzer.
REGEXP is a regular expression for the analyzer to match.
See `define-lex-regex-analyzer' for more on regexp.
TOKIDX is an index into REGEXP for which a new lexical token
of type `spp-macro-include' is to be created.
VALFORM are forms that return the name of the thing being included, and the
type of include.  The return value should be of the form:
  (NAME . TYPE)
where NAME is the name of the include, and TYPE is the type of the include,
where a valid symbol is 'system, or nil."
  (let ((start (make-symbol "start"))
	(end (make-symbol "end"))
	(val (make-symbol "val"))
	(startpnt (make-symbol "startpnt"))
	(endpnt (make-symbol "endpnt")))
    `(define-lex-regex-analyzer ,name
       ,doc
       ,regexp
       (let ((,start (match-beginning ,tokidx))
	     (,end (match-end ,tokidx))
	     (,startpnt semantic-lex-end-point)
	     (,val (save-match-data ,@valform))
	     (,endpnt semantic-lex-end-point))
	 ;;(message "(car ,val) -> %S" (car ,val))
	 (semantic-lex-spp-merge-header (car ,val))
	 (semantic-lex-push-token
	  (semantic-lex-token (if (eq (cdr ,val) 'system)
				  'spp-system-include
				'spp-include)
			      ,start ,end
			      (car ,val)))
	 ;; Preserve setting of the end point from the calling macro.
	 (when (and (/= ,startpnt ,endpnt)
		    (/= ,endpnt semantic-lex-end-point))
	   (setq semantic-lex-end-point ,endpnt))
	 ))))

;;; EIEIO USAGE
;;
;; Semanticdb can save off macro tables for quick lookup later.
;;
;; These routines are for saving macro lists into an EIEIO persistent
;; file.
(defvar semantic-lex-spp-macro-max-length-to-save 200
  "*Maximum length of an SPP macro before we opt to not save it.")

;;;###autoload
(defun semantic-lex-spp-table-write-slot-value (value)
  "Write out the VALUE of a slot for EIEIO.
The VALUE is a spp lexical table."
  (if (not value)
      (princ "nil")
    (princ "\n        '(")
    ;(princ value)
    (dolist (sym value)
      (princ "(")
      (prin1 (car sym))
      (let* ((first (car (cdr sym)))
	     (rest (cdr sym)))
	(if (not (listp first))
	    (insert "nil ;; bogus macro found.\n")
	  (when (eq (car first) 'spp-arg-list)
	    (princ " ")
	    (prin1 first)
	    (setq rest (cdr rest)))

	  (when rest
	    (princ " . ")
	    (let ((len (length (cdr rest))))
	      (cond ((< len 2)
		     (condition-case nil
			 (prin1 rest)
		       (error
			(princ "nil ;; Error writing macro\n"))))
		    ((< len semantic-lex-spp-macro-max-length-to-save)
		     (princ "\n              ")
		     (condition-case nil
			 (prin1 rest)
		       (error
			(princ "nil ;; Error writing macro\n          "))))
		    (t ;; Too Long!
		     (princ "nil ;; Too Long!\n          ")))))))
      (princ ")\n          "))
    (princ ")\n")))

;;; MACRO TABLE DEBUG
;;
(defun semantic-lex-spp-describe (&optional buffer)
  "Describe the current list of spp macros for BUFFER.
If BUFFER is not provided, use the current buffer."
  (interactive)
  (let ((syms (save-excursion
		(if buffer (set-buffer buffer))
		(semantic-lex-spp-macros)))
	(sym nil))
    (with-output-to-temp-buffer "*SPP MACROS*"
      (princ "Macro\t\tValue\n")
      (while syms
	(setq sym (car syms)
	      syms (cdr syms))
	(princ (symbol-name sym))
	(princ "\t")
	(if (< (length (symbol-name sym)) 8)
	    (princ "\t"))
	(prin1 (symbol-value sym))
	(princ "\n")
	))))

;;; EDEBUG Handlers
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()

     (def-edebug-spec define-lex-spp-macro-declaration-analyzer
       (&define name stringp stringp form def-body)
       )

     (def-edebug-spec define-lex-spp-macro-undeclaration-analyzer
       (&define name stringp stringp form)
       )

     (def-edebug-spec define-lex-spp-include-analyzer
       (&define name stringp stringp form def-body))))

(provide 'semantic/lex-spp)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/lex-spp"
;; End:

;;; semantic/lex-spp.el ends here
