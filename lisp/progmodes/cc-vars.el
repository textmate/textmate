;;; cc-vars.el --- user customization variables for CC Mode

;; Copyright (C) 1985, 1987, 1992-2012  Free Software Foundation, Inc.

;; Authors:    2002- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
;;             1987 Dave Detlefs
;;             1987 Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Keywords:   c languages
;; Package:    cc-mode

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

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)

;; Silence the compiler.
(cc-bytecomp-defun get-char-table)	; XEmacs

(cc-eval-when-compile
  (require 'custom)
  (require 'widget))

(cc-eval-when-compile
  ;; Need the function form of `backquote', which isn't standardized
  ;; between Emacsen.  It's called `bq-process' in XEmacs, and
  ;; `backquote-process' in Emacs.  `backquote-process' returns a
  ;; slightly more convoluted form, so let `bq-process' be the norm.
  (if (fboundp 'backquote-process)
      (cc-bytecomp-defmacro bq-process (form)
	`(cdr (backquote-process ,form)))))


;;; Helpers

;; This widget exists in newer versions of the Custom library
(or (get 'other 'widget-type)
    (define-widget 'other 'sexp
      "Matches everything, but doesn't let the user edit the value.
Useful as last item in a `choice' widget."
      :tag "Other"
      :format "%t%n"
      :value 'other))

;; The next defun will supersede c-const-symbol.
(eval-and-compile
  (defun c-constant-symbol (sym len)
  "Create an uneditable symbol for customization buffers.
SYM is the name of the symbol, LEN the length of the field (in
characters) the symbol will be displayed in.  LEN must be big
enough.

This returns a (const ....) structure, suitable for embedding
within a customization type."
  (or (symbolp sym) (error "c-constant-symbol: %s is not a symbol" sym))
  (let* ((name (symbol-name sym))
	 (l (length name))
	 (disp (concat name ":" (make-string (- len l 1) ?\ ))))
    `(const
      :size ,len
      :format ,disp
      :value ,sym))))

(define-widget 'c-const-symbol 'item
  "An uneditable lisp symbol.  This is obsolete -
use c-constant-symbol instead."
  :value nil
  :tag "Symbol"
  :format "%t: %v\n%d"
  :match (lambda (widget value) (symbolp value))
  :value-to-internal
  (lambda (widget value)
    (let ((s (if (symbolp value)
		 (symbol-name value)
	       value))
	  (l (widget-get widget :size)))
      (if l
	  (setq s (concat s (make-string (- l (length s)) ?\ ))))
      s))
  :value-to-external
  (lambda (widget value)
    (if (stringp value)
	(intern (progn
		  (string-match "\\`[^ ]*" value)
		  (match-string 0 value)))
      value)))

(define-widget 'c-integer-or-nil 'sexp
  "An integer or the value nil."
  :value nil
  :tag "Optional integer"
  :match (lambda (widget value) (or (integerp value) (null value))))

(define-widget 'c-symbol-list 'sexp
  "A single symbol or a list of symbols."
  :tag "Symbols separated by spaces"
  :validate 'widget-field-validate
  :match
  (lambda (widget value)
    (or (symbolp value)
	(catch 'ok
	  (while (listp value)
	    (unless (symbolp (car value))
	      (throw 'ok nil))
	    (setq value (cdr value)))
	  (null value))))
  :value-to-internal
  (lambda (widget value)
    (cond ((null value)
	   "")
	  ((symbolp value)
	   (symbol-name value))
	  ((consp value)
	   (mapconcat (lambda (symbol)
			(symbol-name symbol))
		      value
		      " "))
	  (t
	   value)))
  :value-to-external
  (lambda (widget value)
    (if (stringp value)
	(let (list end)
	  (while (string-match "\\S +" value end)
	    (setq list (cons (intern (match-string 0 value)) list)
		  end (match-end 0)))
	  (if (and list (not (cdr list)))
	      (car list)
	    (nreverse list)))
      value)))

(defvar c-style-variables
  '(c-basic-offset c-comment-only-line-offset c-indent-comment-alist
    c-indent-comments-syntactically-p c-block-comment-prefix
    c-comment-prefix-regexp c-doc-comment-style c-cleanup-list
    c-hanging-braces-alist c-hanging-colons-alist
    c-hanging-semi&comma-criteria c-backslash-column c-backslash-max-column
    c-special-indent-hook c-label-minimum-indentation c-offsets-alist)
  "List of the style variables.")

(defvar c-fallback-style nil)

(defsubst c-set-stylevar-fallback (name val)
  (put name 'c-stylevar-fallback val)
  (setq c-fallback-style (cons (cons name val) c-fallback-style)))

(defmacro defcustom-c-stylevar (name val doc &rest args)
  "Define a style variable NAME with VAL and DOC.
More precisely, convert the given `:type FOO', mined out of ARGS,
to an aggregate `:type (radio STYLE (PREAMBLE FOO))', append some
some boilerplate documentation to DOC, arrange for the fallback
value of NAME to be VAL, and call `custom-declare-variable' to
do the rest of the work.

STYLE stands for the choice where the value is taken from some
style setting.  PREAMBLE is optionally prepended to FOO; that is,
if FOO contains :tag or :value, the respective two-element list
component is ignored."
  (declare (debug (symbolp form stringp &rest)))
  (let* ((expanded-doc (concat doc "

This is a style variable.  Apart from the valid values described
above, it can be set to the symbol `set-from-style'.  In that case,
it takes its value from the style system (see `c-default-style' and
`c-style-alist') when a CC Mode buffer is initialized.  Otherwise,
the value set here overrides the style system (there is a variable
`c-old-style-variable-behavior' that changes this, though)."))
         (typ (eval (plist-get args :type)))
         (type (if (consp typ) typ (list typ)))
         (head (car type))
         (tail (cdr type))
         (newt (append (unless (plist-get tail :tag)
                         '(:tag "Override style settings"))
                       (unless (plist-get tail :value)
                         `(:value ,(eval val)))
                       tail))
         (aggregate `'(radio
                       (const :tag "Use style settings" set-from-style)
                       ,(cons head newt))))
    `(progn
       (c-set-stylevar-fallback ',name ,val)
       (custom-declare-variable
        ',name ''set-from-style
        ,expanded-doc
        ,@(plist-put args :type aggregate)))))

(defun c-valid-offset (offset)
  "Return non-nil if OFFSET is a valid offset for a syntactic symbol.
See `c-offsets-alist'."
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (eq offset '*)
      (eq offset '/)
      (integerp offset)
      (functionp offset)
      (and (symbolp offset) (boundp offset))
      (and (vectorp offset)
	   (= (length offset) 1)
	   (integerp (elt offset 0)))
      (and (consp offset)
	   (not (eq (car offset) 'quote)) ; Detect misquoted lists.
	   (progn
	     (when (memq (car offset) '(first min max add))
	       (setq offset (cdr offset)))
	     (while (and (consp offset)
			 (c-valid-offset (car offset)))
	       (setq offset (cdr offset)))
	     (null offset)))))



;;; User variables

(defcustom c-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `c-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, or if no non-nil offset value can be determined
for a symbol, an error is generated, otherwise no error is reported
and the syntactic symbol is ignored.

This variable is considered obsolete; it doesn't work well with lineup
functions that return nil to support the feature of using lists on
syntactic symbols in `c-offsets-alist'.  Please keep it set to nil."
  :type 'boolean
  :group 'c)

(defcustom c-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented."
  :type 'boolean
  :group 'c)

(defcustom c-report-syntactic-errors nil
  "*If non-nil, certain syntactic errors are reported with a ding
and a message, for example when an \"else\" is indented for which
there's no corresponding \"if\".

Note however that CC Mode doesn't make any special effort to check for
syntactic errors; that's the job of the compiler.  The reason it can
report cases like the one above is that it can't find the correct
anchoring position to indent the line in that case."
  :type 'boolean
  :group 'c)

(defcustom-c-stylevar c-basic-offset 4
  "*Amount of basic offset used by + and - symbols in `c-offsets-alist'.
Also used as the indentation step when `c-syntactic-indentation' is
nil."
  :type 'integer
  :group 'c)
;;;###autoload(put 'c-basic-offset 'safe-local-variable 'integerp)

(defcustom c-tab-always-indent t
  "*Controls the operation of the TAB key.
If t, hitting TAB always just indents the current line.  If nil, hitting
TAB indents the current line if point is at the left margin or in the
line's indentation, otherwise it calls `c-insert-tab-function' to
insert a `real' tab character.  If some other value (neither nil nor t),
then inserts a tab only within literals (comments and strings), but
always reindents the line.

Note: the variable `c-comment-only-line-offset' also controls the
indentation of lines containing only comments."
  :type '(radio
	  (const :tag "TAB key always indents, never inserts TAB" t)
	  (const :tag "TAB key indents in left margin, otherwise inserts TAB" nil)
	  (other :tag "TAB key inserts TAB in literals, otherwise indents" other))
  :group 'c)

(defcustom c-insert-tab-function 'insert-tab
  "*Function used when inserting a tab for \\[c-indent-command].
Only used when `c-tab-always-indent' indicates a `real' tab character
should be inserted.  Value must be a function taking no arguments.
The default, `insert-tab', inserts either a tab or the equivalent
number of spaces depending on the value of `indent-tabs-mode'."
  :type 'function
  :group 'c)

(defcustom c-syntactic-indentation t
  "*Whether the indentation should be controlled by the syntactic context.

If t, the indentation functions indent according to the syntactic
context, using the style settings specified by `c-offsets-alist'.

If nil, every line is just indented to the same level as the previous
one, and the \\[c-indent-command] command adjusts the indentation in
steps specified by `c-basic-offset'.  The indentation style has no
effect in this mode, nor any of the indentation associated variables,
e.g. `c-special-indent-hook'."
  :type 'boolean
  :group 'c)
(make-variable-buffer-local 'c-syntactic-indentation)
(put 'c-syntactic-indentation 'safe-local-variable 'booleanp)

(defcustom c-syntactic-indentation-in-macros t
  "*Enable syntactic analysis inside macros.
If this is nil, all lines inside macro definitions are analyzed as
`cpp-macro-cont'.  Otherwise they are analyzed syntactically, just
like normal code, and `cpp-define-intro' is used to create the
additional indentation of the bodies of \"#define\" macros.

Having this enabled simplifies editing of large multiline macros, but
it might complicate editing if CC Mode doesn't recognize the context
of the macro content.  The default context inside the macro is the
same as the top level, so if it contains \"bare\" statements they
might be indented wrongly, although there are special cases that
handle this in most cases.  If this problem occurs, it's usually
countered easily by surrounding the statements by a block \(or even
better with the \"do { ... } while \(0)\" trick)."
  :type 'boolean
  :group 'c)
(put 'c-syntactic-indentation-in-macros 'safe-local-variable 'booleanp)

(defcustom c-defun-tactic 'go-outward
  "*Whether functions are recognized inside, e.g., a class.
This is used by `c-beginning-of-defun' and like functions.

Its value is one of:
 t           -- Functions are recognized only at the top level.
 go-outward  -- Nested functions are also recognized.  Should a function
                command hit the beginning/end of a nested scope, it will
                carry on at the less nested level."
  :version "24.1"
  :type '(radio
	  (const :tag "Functions are at the top-level" t)
	  (const :tag "Functions are also recognized inside declaration scopes" go-outward))
  :group 'c)

(defcustom-c-stylevar c-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . -1000).

Note that this variable only has effect when the `c-lineup-comment'
lineup function is used on the `comment-intro' syntactic symbol (the
default)."
  :type '(choice (integer :tag "Non-anchored offset" 0)
		 (cons :tag "Non-anchored & anchored offset"
		       :value (0 . 0)
		       (integer :tag "Non-anchored offset")
		       (integer :tag "Anchored offset")))
  :group 'c)

(defcustom-c-stylevar c-indent-comment-alist
  '((anchored-comment . (column . 0))
    (end-block . (space . 1))
    (cpp-end-block . (space . 2)))
  "*Specifies how \\[indent-for-comment] calculates the comment start column.
This is an association list that contains entries of the form:

 (LINE-TYPE . INDENT-SPEC)

LINE-TYPE specifies a type of line as described below, and INDENT-SPEC
says what \\[indent-for-comment] should do when used on that type of line.

The recognized values for LINE-TYPE are:

 empty-line        -- The line is empty.
 anchored-comment  -- The line contains a comment that starts in column 0.
 end-block         -- The line contains a solitary block closing brace.
 cpp-end-block     -- The line contains a preprocessor directive that
                      closes a block, i.e. either \"#endif\" or \"#else\".
 other             -- The line does not match any other entry
                      currently on the list.

An INDENT-SPEC is a cons cell of the form:

 (ACTION . VALUE)

ACTION says how \\[indent-for-comment] should align the comment, and
VALUE is interpreted depending on ACTION.  ACTION can be any of the
following:

 space   -- Put VALUE spaces between the end of the line and the start
            of the comment.
 column  -- Start the comment at the column VALUE.  If the line is
            longer than that, the comment is preceded by a single
            space.  If VALUE is nil, `comment-column' is used.
 align   -- Align the comment with one on the previous line, if there
            is any.  If the line is too long, the comment is preceded
            by a single space.  If there isn't a comment start on the
            previous line, the behavior is specified by VALUE, which
            in turn is interpreted as an INDENT-SPEC.

If a LINE-TYPE is missing, then \\[indent-for-comment] indents the comment
according to `comment-column'.

Note that a non-nil value on `c-indent-comments-syntactically-p'
overrides this variable, so empty lines are indented syntactically
in that case, i.e. as if \\[c-indent-command] was used instead."
  :type
  (let ((space '(cons :tag "space"
		      :format "%v"
		      :value (space . 1)
		      (const :format "space  " space)
		      (integer :format "%v")))
	(column '(cons :tag "column"
		       :format "%v"
		       (const :format "column " column)
		       (c-integer-or-nil :format "%v"))))
    `(set ,@(mapcar
	     (lambda (elt)
	       `(cons :format "%v"
		      ,(c-constant-symbol elt 20)
		      (choice
		       :format "%[Choice%] %v"
		       :value (column . nil)
		       ,space
		       ,column
		       (cons :tag "align"
			     :format "%v"
			     (const :format "align  " align)
			     (choice
			      :format "%[Choice%] %v"
			      :value (column . nil)
			      ,space
			      ,column)))))
	     '(empty-line anchored-comment end-block cpp-end-block other))))
  :group 'c)

(defcustom-c-stylevar c-indent-comments-syntactically-p nil
  "*Specifies how \\[indent-for-comment] should handle comment-only lines.
When this variable is non-nil, comment-only lines are indented
according to syntactic analysis via `c-offsets-alist'.  Otherwise, the
comment is indented as if it was preceded by code.  Note that this
variable does not affect how the normal line indentation treats
comment-only lines."
  :type 'boolean
  :group 'c)

(make-obsolete-variable 'c-comment-continuation-stars
			'c-block-comment-prefix "21.1")

;; Although c-comment-continuation-stars is obsolete, we look at it in
;; some places in CC Mode anyway, so make the compiler ignore it
;; during our compilation.
;; [This is unclean; better to use `symbol-value'. --ttn]
;;(cc-bytecomp-obsolete-var c-comment-continuation-stars)
;;(cc-bytecomp-defvar c-comment-continuation-stars)

(defcustom-c-stylevar c-block-comment-prefix
  (if (boundp 'c-comment-continuation-stars)
      (symbol-value 'c-comment-continuation-stars)
    "* ")
  "*Specifies the line prefix of continued C-style block comments.
You should set this variable to the literal string that gets inserted
at the front of continued block style comment lines.  This should
either be the empty string, or some characters without preceding
spaces.  To adjust the alignment under the comment starter, put an
appropriate value on the `c' syntactic symbol (see the
`c-offsets-alist' variable).

It's only used when a one-line block comment is broken into two or
more lines for the first time; otherwise the appropriate prefix is
adapted from the comment.  This variable is not used for C++ line
style comments."
  :type 'string
  :group 'c)

(defcustom-c-stylevar c-comment-prefix-regexp
  '((pike-mode . "//+!?\\|\\**")
    (awk-mode . "#+")
    (other . "//+\\|\\**"))
  "*Regexp to match the line prefix inside comments.
This regexp is used to recognize the fill prefix inside comments for
correct paragraph filling and other things.

If this variable is a string, it will be used in all CC Mode major
modes.  It can also be an association list, to associate specific
regexps to specific major modes.  The symbol for the major mode is
looked up in the association list, and its value is used as the line
prefix regexp.  If it's not found, then the symbol `other' is looked
up and its value is used instead.

The regexp should match the prefix used in both C++ style line
comments and C style block comments, but it does not need to match a
block comment starter.  In other words, it should at least match
\"//\" for line comments and the string in `c-block-comment-prefix',
which is sometimes inserted by CC Mode inside block comments.  It
should not match any surrounding whitespace.

Note that CC Mode uses this variable to set many other variables that
handle the paragraph filling.  That's done at mode initialization or
when you switch to a style which sets this variable.  Thus, if you
change it in some other way, e.g. interactively in a CC Mode buffer,
you will need to do \\[c-setup-paragraph-variables] afterwards so that
the other variables are updated with the new value.

Note also that when CC Mode starts up, all variables are initialized
before the mode hooks are run.  It's therefore necessary to make a
call to `c-setup-paragraph-variables' explicitly if you change this
variable in a mode hook."
  :type '(radio
	  (regexp :tag "Regexp for all modes")
	  (list
	   :tag "Mode-specific regexps"
	   (set
	    :inline t :format "%v"
	    (cons :format "%v"
		  (const :format "C     " c-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "C++   " c++-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "ObjC  " objc-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "Java  " java-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "IDL   " idl-mode) (regexp :format "%v"))
	    (cons :format "%v"
		  (const :format "Pike  " pike-mode) (regexp :format "%v"))
            (cons :format "%v"
		  (const :format "AWK   " awk-mode) (regexp :format "%v")))
	   (cons :format "    %v"
		 (const :format "Other " other) (regexp :format "%v"))))
  :group 'c)

(defcustom-c-stylevar c-doc-comment-style
  '((java-mode . javadoc)
    (pike-mode . autodoc)
    (c-mode    . gtkdoc))
  "*Specifies documentation comment style(s) to recognize.
This is primarily used to fontify doc comments and the markup within
them, e.g. Javadoc comments.

The value can be any of the following symbols for various known doc
comment styles:

 javadoc -- Javadoc style for \"/** ... */\" comments (default in Java mode).
 autodoc -- Pike autodoc style for \"//! ...\" comments (default in Pike mode).
 gtkdoc  -- GtkDoc style for \"/** ... **/\" comments (default in C mode).

The value may also be a list of doc comment styles, in which case all
of them are recognized simultaneously (presumably with markup cues
that don't conflict).

The value may also be an association list to specify different doc
comment styles for different languages.  The symbol for the major mode
is then looked up in the alist, and the value of that element is
interpreted as above if found.  If it isn't found then the symbol
`other' is looked up and its value is used instead.

Note that CC Mode uses this variable to set other variables that
handle fontification etc.  That's done at mode initialization or when
you switch to a style which sets this variable.  Thus, if you change
it in some other way, e.g. interactively in a CC Mode buffer, you will
need to do \\[java-mode] (or whatever mode you're currently using) to
reinitialize.

Note also that when CC Mode starts up, the other variables are
modified before the mode hooks are run.  If you change this variable
in a mode hook, you have to call `c-setup-doc-comment-style'
afterwards to redo that work."
  ;; Symbols other than those documented above may be used on this
  ;; variable.  If a variable exists that has that name with
  ;; "-font-lock-keywords" appended, its value is prepended to the
  ;; font lock keywords list.  If it's a function then it's called and
  ;; the result is prepended.
  :type '(radio
	  (c-symbol-list :tag "Doc style(s) in all modes")
	  (list
	   :tag "Mode-specific doc styles"
	   (set
	    :inline t :format "%v"
	    (cons :format "%v"
		  (const :format "C     " c-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "C++   " c++-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "ObjC  " objc-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "Java  " java-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "IDL   " idl-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "Pike  " pike-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "AWK   " awk-mode)
		  (c-symbol-list :format "%v"))
	    (cons :format "%v"
		  (const :format "Other " other)
		  (c-symbol-list :format "%v")))))
  :group 'c)

(defcustom c-ignore-auto-fill '(string cpp code)
  "*List of contexts in which automatic filling never occurs.
If Auto Fill mode is active, it will be temporarily disabled if point
is in any context on this list.  It's e.g. useful to enable Auto Fill
in comments only, but not in strings or normal code.  The valid
contexts are:

 string  -- inside a string or character literal
 c       -- inside a C style block comment
 c++     -- inside a C++ style line comment
 cpp     -- inside a preprocessor directive
 code    -- anywhere else, i.e. in normal code"
  :type '(set
	  (const :tag "String literals" string)
	  (const :tag "C style block comments" c)
	  (const :tag "C++ style line comments" c++)
	  (const :tag "Preprocessor directives" cpp)
	  (const :tag "Normal code" code))
  :group 'c)

(defcustom-c-stylevar c-cleanup-list '(scope-operator)
  "*List of various C/C++/ObjC constructs to \"clean up\".
The following clean ups only take place when the auto-newline feature
is turned on, as evidenced by the `/la' appearing next to the mode
name:

 brace-else-brace    -- Clean up \"} else {\" constructs by placing
                        entire construct on a single line.  This clean
                        up only takes place when there is nothing but
                        white space between the braces and the `else'.
                        Clean up occurs when the open brace after the
                        `else' is typed.
 brace-elseif-brace  -- Similar to brace-else-brace, but clean up
                        \"} else if (...) {\" constructs.  Clean up
                        occurs after the open parenthesis and the open
                        brace.
 brace-catch-brace   -- Similar to brace-elseif-brace, but clean up
                        \"} catch (...) {\" constructs.
 empty-defun-braces  -- Clean up empty defun braces by placing the
                        braces on the same line.  Clean up occurs when
                        the defun closing brace is typed.
 one-liner-defun     -- If the code inside a function body can fit in
                        a single line, then remove any newlines
                        between that line and the defun braces so that
                        the whole body becomes a single line.
                        `c-max-one-liner-length' gives the maximum
                        length allowed for the resulting line.  Clean
                        up occurs when the closing brace is typed.
 defun-close-semi    -- Clean up the terminating semi-colon on defuns
                        by placing the semi-colon on the same line as
                        the closing brace.  Clean up occurs when the
                        semi-colon is typed.
 list-close-comma    -- Clean up commas following braces in array
                        and aggregate initializers.  Clean up occurs
                        when the comma is typed.
 scope-operator      -- Clean up double colons which may designate
                        a C++ scope operator split across multiple
                        lines.  Note that certain C++ constructs can
                        generate ambiguous situations.  This clean up
                        only takes place when there is nothing but
                        whitespace between colons.  Clean up occurs
                        when the second colon is typed.

The following clean ups always take place when they are on this list,
regardless of the auto-newline feature, since they typically don't
involve auto-newline inserted newlines:

 space-before-funcall -- Insert exactly one space before the opening
                        parenthesis of a function call.  Clean up
                        occurs when the opening parenthesis is typed.
 compact-empty-funcall -- Clean up any space before the function call
                        opening parenthesis if and only if the
                        argument list is empty.  This is typically
                        useful together with `space-before-funcall' to
                        get the style \"foo (bar)\" and \"foo()\".
                        Clean up occurs when the closing parenthesis
                        is typed.
 comment-close-slash -- When a slash is typed after the comment prefix
                        on a bare line in a c-style comment, the comment
                        is closed by cleaning up preceding space and
                        inserting a star if needed."
  :type '(set
	  (const :tag "Put \"} else {\" on one line (brace-else-brace)"
		 brace-else-brace)
	  (const :tag "Put \"} else if (...) {\" on one line (brace-elseif-brace)"
		 brace-elseif-brace)
	  (const :tag "Put \"} catch (...) {\" on one line (brace-catch-brace)"
		 brace-catch-brace)
	  (const :tag "Put empty defun braces on one line (empty-defun-braces)"
		 empty-defun-braces)
	  (const :tag "Put short function bodies on one line (one-liner-defun)"
		 one-liner-defun)
	  (const :tag "Put \"};\" ending defuns on one line (defun-close-semi)"
		 defun-close-semi)
	  (const :tag "Put \"},\" in aggregates on one line (list-close-comma)"
		 list-close-comma)
	  (const :tag "Put C++ style \"::\" on one line (scope-operator)"
		 scope-operator)
	  (const :tag "Put a space before funcall parens, e.g. \"foo (bar)\" (space-before-funcall)"
		 space-before-funcall)
	  (const :tag "Remove space before empty funcalls, e.g. \"foo()\" (compact-empty-funcall)"
		 compact-empty-funcall)
	  (const :tag "Make / on a bare line of a C-style comment close it (comment-close-slash)"
		 comment-close-slash))
  :group 'c)

(defcustom-c-stylevar c-hanging-braces-alist '((brace-list-open)
					       (brace-entry-open)
					       (statement-cont)
					       (substatement-open after)
					       (block-close . c-snug-do-while)
					       (extern-lang-open after)
					       (namespace-open after)
					       (module-open after)
					       (composition-open after)
					       (inexpr-class-open after)
					       (inexpr-class-close before)
					       (arglist-cont-nonempty))
  "*Controls the insertion of newlines before and after braces
when the auto-newline feature is active.  This variable contains an
association list with elements of the following form:
\(SYNTACTIC-SYMBOL . ACTION).

When a brace (either opening or closing) is inserted, the syntactic
context it defines is looked up in this list, and if found, the
associated ACTION is used to determine where newlines are inserted.
If the context is not found, the default is to insert a newline both
before and after the brace.

SYNTACTIC-SYMBOL can be statement-cont, brace-list-intro,
inexpr-class-open, inexpr-class-close, and any of the *-open and
*-close symbols.  See `c-offsets-alist' for details, except for
inexpr-class-open and inexpr-class-close, which doesn't have any
corresponding symbols there.  Those two symbols are used for the
opening and closing braces, respectively, of anonymous inner classes
in Java.

ACTION can be either a function symbol or a list containing any
combination of the symbols `before' or `after'.  If the list is empty,
no newlines are inserted either before or after the brace.

When ACTION is a function symbol, the function is called with a two
arguments: the syntactic symbol for the brace and the buffer position
at which the brace was inserted.  The function must return a list as
described in the preceding paragraph.  Note that during the call to
the function, the variable `c-syntactic-context' is set to the entire
syntactic context for the brace line."
  :type
  `(set ,@(mapcar
	   (lambda (elt)
	     `(cons :format "%v"
		    ,(c-constant-symbol elt 24)
		    (choice :format "%[Choice%] %v"
			    :value (before after)
			    (set :menu-tag "Before/after"
				 :format "Newline  %v brace\n"
				 (const :format "%v,  " before)
				 (const :format "%v " after))
			    (function :menu-tag "Function"
				      :format "Run function: %v"))))
	   '(defun-open defun-close
	      class-open class-close
	      inline-open inline-close
	      block-open block-close
	      statement-cont substatement-open statement-case-open
	      brace-list-open brace-list-close
	      brace-list-intro brace-entry-open
	      extern-lang-open extern-lang-close
	      namespace-open namespace-close
	      module-open module-close
	      composition-open composition-close
	      inexpr-class-open inexpr-class-close
	      arglist-cont-nonempty)))
    :group 'c)

(defcustom c-max-one-liner-length 80
  "Maximum length of line that clean-up \"one-liner-defun\" will compact to.
Zero or nil means no limit."
  :type 'integer
  :group 'c)

(defcustom-c-stylevar c-hanging-colons-alist nil
  "*Controls the insertion of newlines before and after certain colons.
This variable contains an association list with elements of the
following form: (SYNTACTIC-SYMBOL . ACTION).

SYNTACTIC-SYMBOL can be any of: case-label, label, access-label,
member-init-intro, or inher-intro.

See the variable `c-hanging-braces-alist' for the semantics of this
variable.  Note however that making ACTION a function symbol is
currently not supported for this variable."
  :type
  `(set ,@(mapcar
	   (lambda (elt)
	     `(cons :format "%v"
		    ,(c-constant-symbol elt 20)
		    (set :format "Newline  %v  colon\n"
			 (const :format "%v,  " before)
			 (const :format "%v" after))))
	   '(case-label label access-label member-init-intro inher-intro)))
  :group 'c)

(defcustom-c-stylevar c-hanging-semi&comma-criteria
  '(c-semi&comma-inside-parenlist)
  "*List of functions that decide whether to insert a newline or not.
The functions in this list are called, in order, whenever the
auto-newline minor mode is activated (as evidenced by a `/a' or `/ah'
string in the mode line), and a semicolon or comma is typed (see
`c-electric-semi&comma').  Each function in this list is called with
no arguments, and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not insert a newline, and stop checking
  (anything else) -- insert a newline, and stop checking

If every function in the list is called with no determination made,
then no newline is inserted."
  :type '(repeat function)
  :group 'c)

(defcustom-c-stylevar c-backslash-column 48
  "*Minimum alignment column for line continuation backslashes.
This is used by the functions that automatically insert or align the
line continuation backslashes in multiline macros.  If any line in the
macro exceeds this column then the next tab stop from that line is
used as alignment column instead.  See also `c-backslash-max-column'."
  :type 'integer
  :group 'c)
;;;###autoload(put 'c-backslash-column 'safe-local-variable 'integerp)

(defcustom-c-stylevar c-backslash-max-column 72
  "*Maximum alignment column for line continuation backslashes.
This is used by the functions that automatically insert or align the
line continuation backslashes in multiline macros.  If any line in the
macro exceeds this column then the backslashes for the other lines
will be aligned at this column."
  :type 'integer
  :group 'c)

(defcustom c-auto-align-backslashes t
  "*Align automatically inserted line continuation backslashes.
When line continuation backslashes are inserted automatically for line
breaks in multiline macros, e.g. by \\[c-context-line-break], they are
aligned with the other backslashes in the same macro if this flag is
set.  Otherwise the inserted backslashes are preceded by a single
space."
  :type 'boolean
  :group 'c)

(defcustom c-backspace-function 'backward-delete-char-untabify
  "*Function called by `c-electric-backspace' when deleting backwards."
  :type 'function
  :group 'c)

(defcustom c-delete-function 'delete-char
  "*Function called by `c-electric-delete-forward' when deleting forwards."
  :type 'function
  :group 'c)

(defcustom c-require-final-newline
  ;; C and C++ mandate that all nonempty files should end with a
  ;; newline.  Objective-C refers to C for all things it doesn't
  ;; specify, so the same holds there.  The other languages do not
  ;; require it (at least not explicitly in a normative text).
  '((c-mode    . t)
    (c++-mode  . t)
    (objc-mode . t))
  "*Controls whether a final newline is ensured when the file is saved.
The value is an association list that for each language mode specifies
the value to give to `require-final-newline' at mode initialization;
see that variable for details about the value.  If a language isn't
present on the association list, CC Mode won't touch
`require-final-newline' in buffers for that language."
  :type `(set (cons :format "%v"
		    (const :format "C     " c-mode)
		    (symbol :format "%v" :value ,require-final-newline))
	      (cons :format "%v"
		    (const :format "C++   " c++-mode)
		    (symbol :format "%v" :value ,require-final-newline))
	      (cons :format "%v"
		    (const :format "ObjC  " objc-mode)
		    (symbol :format "%v" :value ,require-final-newline))
	      (cons :format "%v"
		    (const :format "Java  " java-mode)
		    (symbol :format "%v" :value ,require-final-newline))
	      (cons :format "%v"
		    (const :format "IDL   " idl-mode)
		    (symbol :format "%v" :value ,require-final-newline))
	      (cons :format "%v"
		    (const :format "Pike  " pike-mode)
		    (symbol :format "%v" :value ,require-final-newline))
	      (cons :format "%v"
		    (const :format "AWK   " awk-mode)
		    (symbol :format "%v" :value ,require-final-newline)))
  :group 'c)

(defcustom c-electric-pound-behavior nil
  "*List of behaviors for electric pound insertion.
Only currently supported behavior is `alignleft'."
  :type '(set (const alignleft))
  :group 'c)

(defcustom c-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after each line is indented by the mode.  It is only
called if `c-syntactic-indentation' is non-nil."
  :type 'hook
  :group 'c)

(defcustom-c-stylevar c-label-minimum-indentation 1
  "*Minimum indentation for lines inside code blocks.
This variable typically only affects code using the `gnu' style, which
mandates a minimum of one space in front of every line inside code
blocks.  Specifically, the function `c-gnu-impose-minimum' on your
`c-special-indent-hook' is what enforces this."
  :type 'integer
  :group 'c)

(defcustom c-progress-interval 5
  "*Interval used to update progress status during long re-indentation.
If a number, percentage complete gets updated after each interval of
that many seconds.  To inhibit all messages during indentation, set
this variable to nil."
  :type 'integer
  :group 'c)

(defcustom c-objc-method-arg-min-delta-to-bracket 2
  "*Minimum number of chars to the opening bracket.

Consider this ObjC snippet:

	[foo blahBlah: fred
	|<-x->|barBaz: barney

If `x' is less than this number then `c-lineup-ObjC-method-call-colons'
will defer the indentation decision to the next function.  By default
this is `c-lineup-ObjC-method-call', which would align it like:

	[foo blahBlahBlah: fred
	     thisIsTooDamnLong: barney

This behavior can be overridden by customizing the indentation of
`objc-method-call-cont' in the \"objc\" style."
  :type 'integer
  :group 'c)

(defcustom c-objc-method-arg-unfinished-offset 4
  "*Offset relative to bracket if first selector is on a new line.

    [aaaaaaaaa
    |<-x->|bbbbbbb:  cccccc
             ddddd: eeee];"
  :type 'integer
  :group 'c)

(defcustom c-objc-method-parameter-offset 4
  "*Offset for selector parameter on a new line (relative to first selector.

    [aaaaaaa bbbbbbbbbb:
	     |<-x->|cccccccc
                    ddd: eeee
                   ffff: ggg];"
  :type 'integer
  :group 'c)

(defcustom c-default-style '((java-mode . "java") (awk-mode . "awk")
			     (other . "gnu"))
  "*Style which gets installed by default when a file is visited.

The value of this variable can be any style defined in
`c-style-alist', including styles you add.  The value can also be an
association list of major mode symbols to style names.

When the value is a string, all CC Mode major modes will install this
style by default.

When the value is an alist, the major mode symbol is looked up in it
and the associated style is installed.  If the major mode is not
listed in the alist, then the symbol `other' is looked up in it, and
if found, the style in that entry is used.  If `other' is not found in
the alist, then \"gnu\" style is used.

The default style gets installed before your mode hooks run, so you
can always override the use of `c-default-style' by making calls to
`c-set-style' in the appropriate mode hook."
  :type '(radio
	  (string :tag "Style in all modes")
	  (set :tag "Mode-specific styles"
	    (cons :format "%v"
		  (const :format "C     " c-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "C++   " c++-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "ObjC  " objc-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "Java  " java-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "IDL   " idl-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "Pike  " pike-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "AWK   " awk-mode) (string :format "%v"))
	    (cons :format "%v"
		  (const :format "Other " other) (string :format "%v"))))
  :group 'c)

;; *) At the start of a statement or declaration means in more detail:
;; At the closest preceding statement/declaration that starts at boi
;; and doesn't have a label or comment at that position.  If there's
;; no such statement within the same block, then back up to the
;; surrounding block or statement, add the appropriate
;; statement-block-intro, defun-block-intro or substatement syntax
;; symbol and continue searching.
(c-set-stylevar-fallback 'c-offsets-alist
     '((string                . c-lineup-dont-change)
       ;; Anchor pos: Beg of previous line.
       (c                     . c-lineup-C-comments)
       ;; Anchor pos: Beg of the comment.
       (defun-open            . 0)
       ;; Anchor pos: When inside a class: Boi at the func decl start.
       ;; When at top level: Bol at the func decl start.  When inside
       ;; a code block (only possible in Pike): At the func decl
       ;; start(*).
       (defun-close           . 0)
       ;; Anchor pos: At the defun block open if it's at boi,
       ;; otherwise boi at the func decl start.
       (defun-block-intro     . +)
       ;; Anchor pos: At the block open(*).
       (class-open            . 0)
       ;; Anchor pos: Boi at the class decl start.
       (class-close           . 0)
       ;; Anchor pos: Boi at the class decl start.
       (inline-open           . +)
       ;; Anchor pos: None for functions (inclass got the relpos
       ;; then), boi at the lambda start for lambdas.
       (inline-close          . 0)
       ;; Anchor pos: Inexpr functions: At the lambda block open if
       ;; it's at boi, else at the statement(*) at boi of the start of
       ;; the lambda construct.  Otherwise: At the inline block open
       ;; if it's at boi, otherwise boi at the func decl start.
       (func-decl-cont        . +)
       ;; Anchor pos: Boi at the func decl start.
       (knr-argdecl-intro     . +)
       ;; Anchor pos: Boi at the topmost intro line.
       (knr-argdecl           . 0)
       ;; Anchor pos: At the beginning of the first K&R argdecl.
       (topmost-intro	      . 0)
       ;; Anchor pos: Bol at the last line of previous construct.
       (topmost-intro-cont    . c-lineup-topmost-intro-cont)
       ;;Anchor pos: Bol at the topmost annotation line
       (annotation-top-cont   .   0)
       ;;Anchor pos: Bol at the topmost annotation line
       (annotation-var-cont   .   +)
       ;; Anchor pos: Boi at the topmost intro line.
       (member-init-intro     . +)
       ;; Anchor pos: Boi at the func decl arglist open.
       (member-init-cont      . c-lineup-multi-inher)
       ;; Anchor pos: Beg of the first member init.
       (inher-intro           . +)
       ;; Anchor pos: Boi at the class decl start.
       (inher-cont            . c-lineup-multi-inher)
       ;; Anchor pos: Java: At the implements/extends keyword start.
       ;; Otherwise: At the inher start colon, or boi at the class
       ;; decl start if the first inherit clause hangs and it's not a
       ;; func-local inherit clause (when does that occur?).
       (block-open            . 0)
       ;; Anchor pos: Inexpr statement: At the statement(*) at boi of
       ;; the start of the inexpr construct.  Otherwise: None.
       (block-close           . 0)
       ;; Anchor pos: Inexpr statement: At the inexpr block open if
       ;; it's at boi, else at the statement(*) at boi of the start of
       ;; the inexpr construct.  Block hanging on a case/default
       ;; label: At the closest preceding label that starts at boi.
       ;; Otherwise: At the block open(*).
       (brace-list-open       . 0)
       ;; Anchor pos: Boi at the brace list decl start, but a starting
       ;; "typedef" token is ignored.
       (brace-list-close      . 0)
       ;; Anchor pos: At the brace list decl start(*).
       (brace-list-intro      . +)
       ;; Anchor pos: At the brace list decl start(*).
       (brace-list-entry      . 0)
       ;; Anchor pos: At the first non-ws char after the open paren if
       ;; the first token is on the same line, otherwise boi at that
       ;; token.
       (brace-entry-open      . 0)
       ;; Anchor pos: Same as brace-list-entry.
       (statement             . 0)
       ;; Anchor pos: After a `;' in the condition clause of a for
       ;; statement: At the first token after the starting paren.
       ;; Otherwise: At the preceding statement(*).
       (statement-cont        . +)
       ;; Anchor pos: After the first token in the condition clause of
       ;; a for statement: At the first token after the starting
       ;; paren.  Otherwise: At the containing statement(*).
       (statement-block-intro . +)
       ;; Anchor pos: In inexpr statement block: At the inexpr block
       ;; open if it's at boi, else at the statement(*) at boi of the
       ;; start of the inexpr construct.  In a block hanging on a
       ;; case/default label: At the closest preceding label that
       ;; starts at boi.  Otherwise: At the start of the containing
       ;; block(*).
       (statement-case-intro  . +)
       ;; Anchor pos: At the case/default label(*).
       (statement-case-open   . 0)
       ;; Anchor pos: At the case/default label(*).
       (substatement          . +)
       ;; Anchor pos: At the containing statement(*).
       (substatement-open     . +)
       ;; Anchor pos: At the containing statement(*).
       (substatement-label    . 2)
       ;; Anchor pos: At the containing statement(*).
       (case-label            . 0)
       ;; Anchor pos: At the start of the switch block(*).
       (access-label          . -)
       ;; Anchor pos: Same as inclass.
       (label                 . 2)
       ;; Anchor pos: At the start of the containing block(*).
       (do-while-closure      . 0)
       ;; Anchor pos: At the corresponding while statement(*).
       (else-clause           . 0)
       ;; Anchor pos: At the corresponding if statement(*).
       (catch-clause          . 0)
       ;; Anchor pos: At the previous try or catch statement clause(*).
       (comment-intro         . (c-lineup-knr-region-comment c-lineup-comment))
       ;; Anchor pos: None.
       (arglist-intro         . +)
       ;; Anchor pos: At the containing statement(*).
       ;; 2nd pos: At the open paren.
       (arglist-cont          . (c-lineup-gcc-asm-reg 0))
       ;; Anchor pos: At the first token after the open paren.
       (arglist-cont-nonempty . (c-lineup-gcc-asm-reg c-lineup-arglist))
       ;; Anchor pos: At the containing statement(*).
       ;; 2nd pos: At the open paren.
       (arglist-close         . +)
       ;; Anchor pos: At the containing statement(*).
       ;; 2nd pos: At the open paren.
       (stream-op             . c-lineup-streamop)
       ;; Anchor pos: Boi at the first stream op in the statement.
       (inclass               . +)
       ;; Anchor pos: At the class open brace if it's at boi,
       ;; otherwise boi at the class decl start.
       (cpp-macro             . [0])
       ;; Anchor pos: None.
       (cpp-macro-cont        . +)
       ;; Anchor pos: At the macro start (always at boi).
       (cpp-define-intro      . (c-lineup-cpp-define +))
       ;; Anchor pos: None.
       (friend                . 0)
       ;; Anchor pos: None.
       (objc-method-intro     . [0])
       ;; Anchor pos: Boi.
       (objc-method-args-cont . c-lineup-ObjC-method-args)
       ;; Anchor pos: At the method start (always at boi).
       (objc-method-call-cont . (c-lineup-ObjC-method-call-colons
			        c-lineup-ObjC-method-call +))
       ;; Anchor pos: At the open bracket.
       (extern-lang-open      . 0)
       (namespace-open        . 0)
       (module-open           . 0)
       (composition-open      . 0)
       ;; Anchor pos: Boi at the extern/namespace/etc keyword.
       (extern-lang-close     . 0)
       (namespace-close       . 0)
       (module-close          . 0)
       (composition-close     . 0)
       ;; Anchor pos: Boi at the corresponding extern/namespace/etc keyword.
       (inextern-lang         . +)
       (innamespace           . +)
       (inmodule              . +)
       (incomposition         . +)
       ;; Anchor pos: At the extern/namespace/etc block open brace if
       ;; it's at boi, otherwise boi at the keyword.
       (template-args-cont    . (c-lineup-template-args +))
       ;; Anchor pos: Boi at the decl start.  This might be changed;
       ;; the logical position is clearly the opening '<'.
       (inlambda              . c-lineup-inexpr-block)
       ;; Anchor pos: None.
       (lambda-intro-cont     . +)
       ;; Anchor pos: Boi at the lambda start.
       (inexpr-statement      . +)
       ;; Anchor pos: None.
       (inexpr-class          . +)
       ;; Anchor pos: None.
       ))
(defcustom c-offsets-alist nil
  "Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, CC Mode first determines the syntactic
context of it by generating a list of symbols called syntactic
elements.  The global variable `c-syntactic-context' is bound to the
that list.  Each element in the list is in turn a list where the first
element is a syntactic symbol which tells what kind of construct the
indentation point is located within.  More elements in the syntactic
element lists are optional.  If there is one more and it isn't nil,
then it's the anchor position for that construct.

After generating the syntactic context for the line, CC Mode
calculates the absolute indentation: First the base indentation is
found by using the anchor position for the first syntactic element
that provides one.  If none does, zero is used as base indentation.
Then CC Mode looks at each syntactic element in the context in turn.
It compares the car of the syntactic element against the
SYNTACTIC-SYMBOL's in `c-offsets-alist'.  When it finds a match, it
adds OFFSET to the base indentation.  The sum of this calculation is
the absolute offset for line being indented.

If the syntactic element does not match any in the `c-offsets-alist',
the element is ignored.

OFFSET can specify an offset in several different ways:

  If OFFSET is nil then it's ignored.

  If OFFSET is an integer then it's used as relative offset, i.e. it's
  added to the base indentation.

  If OFFSET is one of the symbols `+', `-', `++', `--', `*', or `/'
  then a positive or negative multiple of `c-basic-offset' is added to
  the base indentation; 1, -1, 2, -2, 0.5, and -0.5, respectively.

  If OFFSET is a symbol with a value binding then that value, which
  must be an integer, is used as relative offset.

  If OFFSET is a vector then its first element, which must be an
  integer, is used as an absolute indentation column.  This overrides
  the previous base indentation and the relative offsets applied to
  it, and it becomes the new base indentation.

  If OFFSET is a function or a lambda expression then it's called with
  a single argument containing the cons of the syntactic symbol and
  the anchor position (or nil if there is none).  The return value
  from the function is then reinterpreted as an offset specification.

  If OFFSET is a list then its elements are evaluated recursively as
  offset specifications.  If the first element is any of the symbols
  below then it isn't evaluated but instead specifies how the
  remaining offsets in the list should be combined.  If it's something
  else then the list is combined according the method `first'.  The
  valid combination methods are:

  `first' -- Use the first offset (that doesn't evaluate to nil).
  `min'   -- Use the minimum of all the offsets.  All must be either
             relative or absolute - they can't be mixed.
  `max'   -- Use the maximum of all the offsets.  All must be either
             relative or absolute - they can't be mixed.
  `add'   -- Add all the evaluated offsets together.  Exactly one of
             them may be absolute, in which case the result is
             absolute.  Any relative offsets that preceded the
             absolute one in the list will be ignored in that case.

`c-offsets-alist' is a style variable.  This means that the offsets on
this variable are normally taken from the style system in CC Mode
\(see `c-default-style' and `c-style-alist').  However, any offsets
put explicitly on this list will override the style system when a CC
Mode buffer is initialized \(there is a variable
`c-old-style-variable-behavior' that changes this, though).

Here is the current list of valid syntactic element symbols:

 string                 -- Inside multi-line string.
 c                      -- Inside a multi-line C style block comment.
 defun-open             -- Brace that opens a function definition.
 defun-close            -- Brace that closes a function definition.
 defun-block-intro      -- The first line in a top-level defun.
 class-open             -- Brace that opens a class definition.
 class-close            -- Brace that closes a class definition.
 inline-open            -- Brace that opens an in-class inline method.
 inline-close           -- Brace that closes an in-class inline method.
 func-decl-cont         -- The region between a function definition's
                           argument list and the function opening brace
                           (excluding K&R argument declarations).  In C, you
                           cannot put anything but whitespace and comments
                           between them; in C++ and Java, throws declarations
                           and other things can appear in this context.
 knr-argdecl-intro      -- First line of a K&R C argument declaration.
 knr-argdecl		-- Subsequent lines in a K&R C argument declaration.
 topmost-intro		-- The first line in a topmost construct definition.
 topmost-intro-cont	-- Topmost definition continuation lines.
 annotation-top-cont    -- Topmost definition continuation line where only
 			   annotations are on previous lines.
 annotation-var-cont    -- A continuation of a C (or like) statement where
 			   only annotations are on previous lines.
 member-init-intro	-- First line in a member initialization list.
 member-init-cont	-- Subsequent member initialization list lines.
 inher-intro		-- First line of a multiple inheritance list.
 inher-cont             -- Subsequent multiple inheritance lines.
 block-open             -- Statement block open brace.
 block-close            -- Statement block close brace.
 brace-list-open        -- Open brace of an enum or static array list.
 brace-list-close       -- Close brace of an enum or static array list.
 brace-list-intro       -- First line in an enum or static array list.
 brace-list-entry       -- Subsequent lines in an enum or static array list.
 brace-entry-open       -- Subsequent lines in an enum or static array
                           list that start with an open brace.
 statement              -- A C (or like) statement.
 statement-cont         -- A continuation of a C (or like) statement.
 statement-block-intro  -- The first line in a new statement block.
 statement-case-intro   -- The first line in a case \"block\".
 statement-case-open    -- The first line in a case block starting with brace.
 substatement           -- The first line after an if/while/for/do/else.
 substatement-open      -- The brace that opens a substatement block.
 substatement-label     -- Labeled line after an if/while/for/do/else.
 case-label             -- A \"case\" or \"default\" label.
 access-label           -- C++ private/protected/public access label.
 label                  -- Any ordinary label.
 do-while-closure       -- The \"while\" that ends a do/while construct.
 else-clause            -- The \"else\" of an if/else construct.
 catch-clause           -- The \"catch\" or \"finally\" of a try/catch construct.
 comment-intro          -- A line containing only a comment introduction.
 arglist-intro          -- The first line in an argument list.
 arglist-cont           -- Subsequent argument list lines when no
                           arguments follow on the same line as the
                           arglist opening paren.
 arglist-cont-nonempty  -- Subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren.
 arglist-close          -- The solo close paren of an argument list.
 stream-op              -- Lines continuing a stream operator construct.
 inclass                -- The construct is nested inside a class definition.
                           Used together with e.g. `topmost-intro'.
 cpp-macro              -- The start of a C preprocessor macro definition.
 cpp-macro-cont         -- Inside a multi-line C preprocessor macro definition.
 friend                 -- A C++ friend declaration.
 objc-method-intro      -- The first line of an Objective-C method definition.
 objc-method-args-cont  -- Lines continuing an Objective-C method definition.
 objc-method-call-cont  -- Lines continuing an Objective-C method call.
 extern-lang-open       -- Brace that opens an \"extern\" block.
 extern-lang-close      -- Brace that closes an \"extern\" block.
 inextern-lang          -- Analogous to the `inclass' syntactic symbol,
                           but used inside \"extern\" blocks.
 namespace-open, namespace-close, innamespace
                        -- Similar to the three `extern-lang' symbols, but for
                           C++ \"namespace\" blocks.
 module-open, module-close, inmodule
                        -- Similar to the three `extern-lang' symbols, but for
                           CORBA IDL \"module\" blocks.
 composition-open, composition-close, incomposition
                        -- Similar to the three `extern-lang' symbols, but for
                           CORBA CIDL \"composition\" blocks.
 template-args-cont     -- C++ template argument list continuations.
 inlambda               -- In the header or body of a lambda function.
 lambda-intro-cont      -- Continuation of the header of a lambda function.
 inexpr-statement       -- The statement is inside an expression.
 inexpr-class           -- The class is inside an expression.  Used e.g. for
                           Java anonymous classes."
  :type
  `(set :format "%{%t%}:
 Override style setting
 |  Syntax                     Offset
%v"
	,@(mapcar
	   (lambda (elt)
	     `(cons :format "%v"
		    :value ,elt
		    ,(c-constant-symbol (car elt) 25)
		    (sexp :format "%v"
			  :validate
			  (lambda (widget)
			    (unless (c-valid-offset (widget-value widget))
			      (widget-put widget :error "Invalid offset")
			      widget)))))
	   (get 'c-offsets-alist 'c-stylevar-fallback)))
  :group 'c)

;; The syntactic symbols that can occur inside code blocks. Used by
;; `c-gnu-impose-minimum'.
(defconst c-inside-block-syms
  '(defun-block-intro block-open block-close statement statement-cont
    statement-block-intro statement-case-intro statement-case-open
    substatement substatement-open substatement-label case-label label
    do-while-closure else-clause catch-clause inlambda annotation-var-cont))

(defcustom c-style-variables-are-local-p t
  "*Whether style variables should be buffer local by default.
If non-nil, then all indentation style related variables will be made
buffer local by default.  If nil, they will remain global.  Variables
are made buffer local when this file is loaded, and once buffer
localized, they cannot be made global again.

This variable must be set appropriately before CC Mode is loaded.

The list of variables to buffer localize are:
    c-basic-offset
    c-comment-only-line-offset
    c-indent-comment-alist
    c-indent-comments-syntactically-p
    c-block-comment-prefix
    c-comment-prefix-regexp
    c-doc-comment-style
    c-cleanup-list
    c-hanging-braces-alist
    c-hanging-colons-alist
    c-hanging-semi&comma-criteria
    c-backslash-column
    c-backslash-max-column
    c-label-minimum-indentation
    c-offsets-alist
    c-special-indent-hook
    c-indentation-style"
  :type 'boolean
  :safe 'booleanp
  :group 'c)

(defcustom c-mode-hook nil
  "*Hook called by `c-mode'."
  :type 'hook
  :group 'c)

(defcustom c++-mode-hook nil
  "*Hook called by `c++-mode'."
  :type 'hook
  :group 'c)

(defcustom objc-mode-hook nil
  "*Hook called by `objc-mode'."
  :type 'hook
  :group 'c)

(defcustom java-mode-hook nil
  "*Hook called by `java-mode'."
  :type 'hook
  :group 'c)

(defcustom idl-mode-hook nil
  "*Hook called by `idl-mode'."
  :type 'hook
  :group 'c)

(defcustom pike-mode-hook nil
  "*Hook called by `pike-mode'."
  :type 'hook
  :group 'c)

(defcustom awk-mode-hook nil
  "*Hook called by `awk-mode'."
  :type 'hook
  :group 'c)

(defcustom c-mode-common-hook nil
  "*Hook called by all CC Mode modes for common initializations."
  :type 'hook
  :group 'c)

(defcustom c-initialization-hook nil
  "*Hook called when the CC Mode package gets initialized.
This hook is only run once per Emacs session and can be used as a
`load-hook' or in place of using `eval-after-load'."
  :type 'hook
  :group 'c)

(defcustom c-enable-xemacs-performance-kludge-p nil
  "*Enables a XEmacs only hack that may improve speed for some coding styles.
For styles that hang top-level opening braces (as is common with JDK
Java coding styles) this can improve performance between 3 and 60
times for core indentation functions (e.g. `c-parse-state').  For
styles that conform to the Emacs recommendation of putting these
braces in column zero, this can degrade performance about as much.
This variable only has effect in XEmacs."
  :type 'boolean
  :group 'c)

(defvar c-old-style-variable-behavior nil
  "*Enables the old style variable behavior when non-nil.

Normally the values of the style variables will override the style
settings specified by the variables `c-default-style' and
`c-style-alist'.  However, in CC Mode 5.25 and earlier, it was the
other way around, meaning that changes made to the style variables
from e.g. Customize would not take effect unless special precautions
were taken.  That was confusing, especially for novice users.

It's believed that despite this change, the new behavior will still
produce the same results for most old CC Mode configurations, since
all style variables are per default set in a special non-override
state.  Set this variable only if your configuration has stopped
working due to this change.")

(define-widget 'c-extra-types-widget 'radio
  "Internal CC Mode widget for the `*-font-lock-extra-types' variables."
  :args '((const :tag "none" nil)
	  (repeat :tag "types" regexp)))

(defun c-make-font-lock-extra-types-blurb (mode1 mode2 example)
  (concat "\
*List of extra types (aside from the type keywords) to recognize in "
mode1 " mode.
Each list item should be a regexp matching a single identifier.
" example "

Note that items on this list that don't include any regexp special
characters are automatically optimized using `regexp-opt', so you
should not use `regexp-opt' explicitly to build regexps here.

On decoration level 3 (and higher, where applicable), a method is used
that finds most types and declarations by syntax alone.  This variable
is still used as a first step, but other types are recognized
correctly anyway in most cases.  Therefore this variable should be
fairly restrictive and not contain patterns that are uncertain.

Note that this variable is only consulted when the major mode is
initialized.  If you change it later you have to reinitialize CC Mode
by doing \\[" mode2 "].

Despite the name, this variable is not only used for font locking but
also elsewhere in CC Mode to tell types from other identifiers."))

;; Note: Most of the variables below are also defined in font-lock.el
;; in older versions of Emacs, so depending on the load order we might
;; not install the values below.  There's no kludge to cope with this
;; (as opposed to the *-font-lock-keywords-* variables) since the old
;; values work fairly well anyway.

(defcustom c-font-lock-extra-types
  '("\\sw+_t"
    ;; Defined in C99:
    "bool" "complex" "imaginary"
    ;; Standard library types (except those matched by the _t pattern):
    "FILE" "lconv" "tm" "va_list" "jmp_buf"
    ;; I do not appreciate the following very Emacs-specific luggage
    ;; in the default value, but otoh it can hardly get in the way for
    ;; other users, and removing it would cause unnecessary grief for
    ;; the old timers that are used to it. /mast
    "Lisp_Object")
  (c-make-font-lock-extra-types-blurb "C" "c-mode"
"For example, a value of (\"FILE\" \"\\\\sw+_t\") means the word \"FILE\"
and words ending in \"_t\" are treated as type names.")
  :type 'c-extra-types-widget
  :group 'c)

(defcustom c++-font-lock-extra-types
  '("\\sw+_t"
    ;; C library types (except those matched by the _t pattern):
    "FILE" "lconv" "tm" "va_list" "jmp_buf"
    ;; Some standard C++ types that came from font-lock.el.
    ;; Experienced C++ users says there's no clear benefit in
    ;; extending this to all the types in the standard library, at
    ;; least not when they'll be recognized without "std::" too.
    "istream" "istreambuf"
    "ostream" "ostreambuf"
    "ifstream" "ofstream" "fstream"
    "strstream" "strstreambuf" "istrstream" "ostrstream"
    "ios"
    "string" "rope"
    "list" "slist"
    "deque" "vector" "bit_vector"
    "set" "multiset"
    "map" "multimap"
    "hash"
    "hash_set" "hash_multiset"
    "hash_map" "hash_multimap"
    "stack" "queue" "priority_queue"
    "type_info"
    "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator"
    "reference" "const_reference")
  (c-make-font-lock-extra-types-blurb "C++" "c++-mode"
"For example, a value of (\"string\") means the word \"string\" is treated
as a type name.")
  :type 'c-extra-types-widget
  :group 'c)

(defcustom objc-font-lock-extra-types
  (list (concat "[" c-upper "]\\sw*[" c-lower "]\\sw*"))
  (c-make-font-lock-extra-types-blurb "ObjC" "objc-mode" (concat
"For example, a value of (\"[" c-upper "]\\\\sw*[" c-lower "]\\\\sw*\") means
capitalized words are treated as type names (the requirement for a
lower case char is to avoid recognizing all-caps macro and constant
names)."))
  :type 'c-extra-types-widget
  :group 'c)

(defcustom java-font-lock-extra-types
  (list (concat "[" c-upper "]\\sw*[" c-lower "]\\sw"))
  (c-make-font-lock-extra-types-blurb "Java" "java-mode" (concat
"For example, a value of (\"[" c-upper "]\\\\sw*[" c-lower "]\\\\sw*\") means
capitalized words are treated as type names (the requirement for a
lower case char is to avoid recognizing all-caps constant names)."))
  :type 'c-extra-types-widget
  :group 'c)

(defcustom idl-font-lock-extra-types nil
  (c-make-font-lock-extra-types-blurb "IDL" "idl-mode" "")
  :type 'c-extra-types-widget
  :group 'c)

(defcustom pike-font-lock-extra-types
  (list (concat "[" c-upper "]\\sw*[" c-lower "]\\sw*"))
  (c-make-font-lock-extra-types-blurb "Pike" "pike-mode" (concat
"For example, a value of (\"[" c-upper "]\\\\sw*[" c-lower "]\\\\sw*\") means
capitalized words are treated as type names (the requirement for a
lower case char is to avoid recognizing all-caps macro and constant
names)."))
  :type 'c-extra-types-widget
  :group 'c)


;; Non-customizable variables, still part of the interface to CC Mode
(defvar c-macro-with-semi-re nil
  ;; Regular expression which matches a (#define'd) symbol whose expansion
  ;; ends with a semicolon.
  ;;
  ;; This variable should be set by `c-make-macros-with-semi-re' rather than
  ;; directly.
)
(make-variable-buffer-local 'c-macro-with-semi-re)

(defun c-make-macro-with-semi-re ()
  ;; Convert `c-macro-names-with-semicolon' into the regexp
  ;; `c-macro-with-semi-re' (or just copy it if it's already a re).
  (setq c-macro-with-semi-re
	(and
	 c-opt-cpp-macro-define
	 (cond
	  ((stringp c-macro-names-with-semicolon)
	   (copy-sequence c-macro-names-with-semicolon))
	  ((consp c-macro-names-with-semicolon)
	   (concat
	    "\\<"
	    (regexp-opt c-macro-names-with-semicolon)
	    "\\>"))   ; N.B. the PAREN param of regexp-opt isn't supported by
		      ; all XEmacsen.
	  ((null c-macro-names-with-semicolon)
	   nil)
	  (t (error "c-make-macro-with-semi-re: invalid \
c-macro-names-with-semicolon: %s"
		    c-macro-names-with-semicolon))))))

(defvar c-macro-names-with-semicolon
  '("Q_OBJECT" "Q_PROPERTY" "Q_DECLARE" "Q_ENUMS")
  "List of #defined symbols whose expansion ends with a semicolon.
Alternatively it can be a string, a regular expression which
matches all such symbols.

The \"symbols\" must be syntactically valid identifiers in the
target language \(C, C++, Objective C), or \(as the case may be)
the regular expression must match only valid identifiers.

If you change this variable's value, call the function
`c-make-macros-with-semi-re' to set the necessary internal
variables.

Note that currently \(2008-11-04) this variable is a prototype,
and is likely to disappear or change its form soon.")
(make-variable-buffer-local 'c-macro-names-with-semicolon)

(defvar c-file-style nil
  "Variable interface for setting style via File Local Variables.
In a file's Local Variable section, you can set this variable to a
string suitable for `c-set-style'.  When the file is visited, CC Mode
will set the style of the file to this value automatically.

Note that file style settings are applied before file offset settings
as designated in the variable `c-file-offsets'.")
(make-variable-buffer-local 'c-file-style)
;;;###autoload(put 'c-file-style 'safe-local-variable 'string-or-null-p)

(defvar c-file-offsets nil
  "Variable interface for setting offsets via File Local Variables.
In a file's Local Variable section, you can set this variable to an
association list similar to the values allowed in `c-offsets-alist'.
When the file is visited, CC Mode will institute these offset settings
automatically.

Note that file offset settings are applied after file style settings
as designated in the variable `c-file-style'.")
(make-variable-buffer-local 'c-file-offsets)

;; It isn't possible to specify a doc-string without specifying an
;; initial value with `defvar', so the following two variables have been
;; given doc-strings by setting the property `variable-documentation'
;; directly.  It's really good not to have an initial value for
;; variables like these that always should be dynamically bound, so it's
;; worth the inconvenience.

(cc-bytecomp-defvar c-syntactic-context)
(defvar c-syntactic-context)
(put 'c-syntactic-context 'variable-documentation
  "Variable containing the syntactic analysis list for a line of code.

It is a list with one element for each syntactic symbol pertinent to the
line, for example \"((defun-block-intro 1) (comment-intro))\".

It is dynamically bound when calling \(i) a brace hanging \"action
function\"; \(ii) a semicolon/comma hanging \"criteria function\"; \(iii) a
\"line-up function\"; \(iv) a c-special-indent-hook function.  It is also
used internally by CC Mode.

c-syntactic-context is always bound dynamically.  It must NEVER be set
statically (e.g. with `setq').")


(cc-bytecomp-defvar c-syntactic-element)
(defvar c-syntactic-element)
(put 'c-syntactic-element 'variable-documentation
     "Variable containing the current syntactic element during calls to
the lineup functions.  The value is one of the elements in the list in
`c-syntactic-context' and is a list with the symbol name in the first
position, followed by zero or more elements containing any additional
info associated with the syntactic symbol.  There are accessor functions
`c-langelem-sym', `c-langelem-pos', `c-langelem-col', and
`c-langelem-2nd-pos' to access the list.

Specifically, the element returned by `c-langelem-pos' is the anchor
position, or nil if there isn't any.  See the comments in the
`c-offsets-alist' variable and the CC Mode manual for more detailed info
about the data each syntactic symbol provides.

This is always bound dynamically.  It should never be set
statically (e.g. with `setq').")

(defvar c-indentation-style nil
  "Name of the currently installed style.
Don't change this directly; call `c-set-style' instead, or set the variable
`c-file-style' in the file's Local Variable list.")

(defvar c-current-comment-prefix nil
  "The current comment prefix regexp.
Set from `c-comment-prefix-regexp' at mode initialization.")
(make-variable-buffer-local 'c-current-comment-prefix)

;; N.B. The next three variables are initialized in
;; c-setup-paragraph-variables.  Their initializations here are "just in
;; case".  ACM, 2004/2/15.  They are NOT buffer local (yet?).
(defvar c-string-par-start
;;   (concat "\\(" (default-value 'paragraph-start) "\\)\\|[ \t]*\\\\$")
  "\f\\|[ \t]*\\\\?$"
  "Value of paragraph-start used when scanning strings.
It treats escaped EOLs as whitespace.")

(defvar c-string-par-separate
  ;; (concat "\\(" (default-value 'paragraph-separate) "\\)\\|[ \t]*\\\\$")
  "[ \t\f]*\\\\?$"
  "Value of paragraph-separate used when scanning strings.
It treats escaped EOLs as whitespace.")

(defvar c-sentence-end-with-esc-eol
  (concat "\\(\\(" (c-default-value-sentence-end) "\\)"
		;; N.B.:  "$" would be illegal when not enclosed like "\\($\\)".
		"\\|" "[.?!][]\"')}]* ?\\\\\\($\\)[ \t\n]*"
		"\\)")
  "Value used like sentence-end used when scanning strings.
It treats escaped EOLs as whitespace.")


(cc-provide 'cc-vars)

;;; cc-vars.el ends here
