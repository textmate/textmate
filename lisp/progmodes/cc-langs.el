;;; cc-langs.el --- language specific settings for CC Mode

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

;; HACKERS NOTE: There's heavy macro magic here.  If you need to make
;; changes in this or other files containing `c-lang-defconst' but
;; don't want to read through the longer discussion below then read
;; this:
;;
;; o  A change in a `c-lang-defconst' or `c-lang-defvar' will not take
;;    effect if the file containing the mode init function (typically
;;    cc-mode.el) is byte compiled.
;; o  To make changes show in font locking you need to reevaluate the
;;    `*-font-lock-keywords-*' constants, which normally is easiest to
;;    do with M-x eval-buffer in cc-fonts.el.
;; o  In either case it's necessary to reinitialize the mode to make
;;    the changes show in an existing buffer.

;;; Introduction to the language dependent variable system:
;;
;; This file contains all the language dependent variables, except
;; those specific for font locking which reside in cc-fonts.el.  As
;; far as possible, all the differences between the languages that CC
;; Mode supports are described with these variables only, so that the
;; code can be shared.
;;
;; The language constant system (see cc-defs.el) is used to specify
;; various language dependent info at a high level, such as lists of
;; keywords, and then from them generate - at compile time - the
;; various regexps and other low-level structures actually employed in
;; the code at runtime.
;;
;; This system is also designed to make it easy for developers of
;; derived modes to customize the source constants for new language
;; variants, without having to keep up with the exact regexps etc that
;; are used in each CC Mode version.  It's possible from an external
;; package to add a new language by inheriting an existing one, and
;; then change specific constants as necessary for the new language.
;; The old values for those constants (and the values of all the other
;; high-level constants) may be used to build the new ones, and those
;; new values will in turn be used by the low-level definitions here
;; to build the runtime constants appropriately for the new language
;; in the current version of CC Mode.
;;
;; Like elsewhere in CC Mode, the existence of a doc string signifies
;; that a language constant is part of the external API, and that it
;; therefore can be used with a high confidence that it will continue
;; to work with future versions of CC Mode.  Even so, it's not
;; unlikely that such constants will change meaning slightly as this
;; system is refined further; a certain degree of dependence on the CC
;; Mode version is unavoidable when hooking in at this level.  Also
;; note that there's still work to be done to actually use these
;; constants everywhere inside CC Mode; there are still hardcoded
;; values in many places in the code.
;;
;; Separate packages will also benefit from the compile time
;; evaluation; the byte compiled file(s) for them will contain the
;; compiled runtime constants ready for use by (the byte compiled) CC
;; Mode, and the source definitions in this file don't have to be
;; loaded then.  However, if a byte compiled package is loaded that
;; has been compiled with a different version of CC Mode than the one
;; currently loaded, then the compiled-in values will be discarded and
;; new ones will be built when the mode is initialized.  That will
;; automatically trig a load of the file(s) containing the source
;; definitions (i.e. this file and/or cc-fonts.el) if necessary.
;;
;; A small example of a derived mode is available at
;; <http://cc-mode.sourceforge.net/derived-mode-ex.el>.  It also
;; contains some useful hints for derived mode developers.

;;; Using language variables:
;;
;; The `c-lang-defvar' forms in this file comprise the language
;; variables that CC Mode uses.  It does not work to use
;; `c-lang-defvar' anywhere else (which isn't much of a limitation
;; since these variables sole purpose is to interface with the CC Mode
;; core functions).  The values in these `c-lang-defvar's are not
;; evaluated right away but instead collected to a single large `setq'
;; that can be inserted for a particular language with the
;; `c-init-language-vars' macro.

;; This file is only required at compile time, or when not running
;; from byte compiled files, or when the source definitions for the
;; language constants are requested.

;;; Code:

;; For Emacs < 22.2.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)


;; This file is not always loaded.  See note above.
(cc-external-require 'cl)


;;; Setup for the `c-lang-defvar' system.

(eval-and-compile
  ;; These are used to collect the init forms from the subsequent
  ;; `c-lang-defvar' and `c-lang-setvar'.  They are used to build the
  ;;  lambda in `c-make-init-lang-vars-fun' below, and to build `defvar's
  ;;  and `make-variable-buffer-local's in cc-engine and
  ;;  `make-local-variable's in `c-init-language-vars-for'.
  (defvar c-lang-variable-inits nil)
  (defvar c-lang-variable-inits-tail nil)
  (setq c-lang-variable-inits (list nil)
	c-lang-variable-inits-tail c-lang-variable-inits)
  (defvar c-emacs-variable-inits nil)
  (defvar c-emacs-variable-inits-tail nil)
  (setq c-emacs-variable-inits (list nil)
	c-emacs-variable-inits-tail c-emacs-variable-inits))

(defmacro c-lang-defvar (var val &optional doc)
  "Declares the buffer local variable VAR to get the value VAL.  VAL is
evaluated and assigned at mode initialization.  More precisely, VAL is
evaluated and bound to VAR when the result from the macro
`c-init-language-vars' is evaluated.

`c-lang-const' is typically used in VAL to get the right value for the
language being initialized, and such calls will be macro expanded to
the evaluated constant value at compile time."

  (when (and (not doc)
	     (eq (car-safe val) 'c-lang-const)
	     (eq (nth 1 val) var)
	     (not (nth 2 val)))
    ;; Special case: If there's no docstring and the value is a
    ;; simple (c-lang-const foo) where foo is the same name as VAR
    ;; then take the docstring from the language constant foo.
    (setq doc (get (intern (symbol-name (nth 1 val)) c-lang-constants)
		   'variable-documentation)))
  (or (stringp doc)
      (setq doc nil))

  (let ((elem (assq var (cdr c-lang-variable-inits))))
    (if elem
	(setcdr elem (list val doc))
      (setcdr c-lang-variable-inits-tail (list (list var val doc)))
      (setq c-lang-variable-inits-tail (cdr c-lang-variable-inits-tail))))

  ;; Return the symbol, like the other def* forms.
  `',var)

(defmacro c-lang-setvar (var val)
  "Causes the variable VAR to be made buffer local and to get set to the
value VAL.  VAL is evaluated and assigned at mode initialization.  More
precisely, VAL is evaluated and bound to VAR when the result from the
macro `c-init-language-vars' is evaluated.  VAR is typically a standard
Emacs variable like `comment-start'.

`c-lang-const' is typically used in VAL to get the right value for the
language being initialized, and such calls will be macro expanded to
the evaluated constant value at compile time."
  (let ((elem (assq var (cdr c-emacs-variable-inits))))
    (if elem
	(setcdr elem (list val)) ; Maybe remove "list", sometime. 2006-07-19
      (setcdr c-emacs-variable-inits-tail (list (list var val)))
      (setq c-emacs-variable-inits-tail (cdr c-emacs-variable-inits-tail))))

  ;; Return the symbol, like the other def* forms.
  `',var)

(put 'c-lang-defvar 'lisp-indent-function 'defun)
; (eval-after-load "edebug" ; 2006-07-09: def-edebug-spec is now in subr.el.
;  '
(def-edebug-spec c-lang-defvar
  (&define name def-form &optional stringp)) ;)

;; Suppress "might not be defined at runtime" warning.
;; This file is only used when compiling other cc files.
(declare-function delete-duplicates "cl-seq" (cl-seq &rest cl-keys))
(declare-function mapcan "cl-extra" (cl-func cl-seq &rest cl-rest))
(declare-function cl-macroexpand-all "cl-extra" (form &optional env))

(eval-and-compile
  ;; Some helper functions used when building the language constants.

  (defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
    ;; Extract a subset of the operators in the list OPS in a DWIM:ey
    ;; way.  The return value is a plain list of operators:
    ;;
    ;; OPS either has the structure of `c-operators', is a single
    ;; group in `c-operators', or is a plain list of operators.
    ;;
    ;; OPGROUP-FILTER specifies how to select the operator groups.  It
    ;; can be t to choose all groups, a list of group type symbols
    ;; (such as 'prefix) to accept, or a function which will be called
    ;; with the group symbol for each group and should return non-nil
    ;; if that group is to be included.
    ;;
    ;; If XLATE is given, it's a function which is called for each
    ;; matching operator and its return value is collected instead.
    ;; If it returns a list, the elements are spliced directly into
    ;; the final result, which is returned as a list with duplicates
    ;; removed using `equal'.
    ;;
    ;; `c-mode-syntax-table' for the current mode is in effect during
    ;; the whole procedure.
    (unless (listp (car-safe ops))
      (setq ops (list ops)))
    (cond ((eq opgroup-filter t)
	   (setq opgroup-filter (lambda (opgroup) t)))
	  ((not (functionp opgroup-filter))
	   (setq opgroup-filter `(lambda (opgroup)
				   (memq opgroup ',opgroup-filter)))))
    (cond ((eq op-filter t)
	   (setq op-filter (lambda (op) t)))
	  ((stringp op-filter)
	   (setq op-filter `(lambda (op)
			      (string-match ,op-filter op)))))
    (unless xlate
      (setq xlate 'identity))
    (c-with-syntax-table (c-lang-const c-mode-syntax-table)
      (delete-duplicates
       (mapcan (lambda (opgroup)
		 (when (if (symbolp (car opgroup))
			   (when (funcall opgroup-filter (car opgroup))
			     (setq opgroup (cdr opgroup))
			     t)
			 t)
		   (mapcan (lambda (op)
			     (when (funcall op-filter op)
			       (let ((res (funcall xlate op)))
				 (if (listp res) res (list res)))))
			   opgroup)))
	       ops)
       :test 'equal))))


;;; Various mode specific values that aren't language related.

(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.  The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  t `(["Comment Out Region"     comment-region
       (c-fn-region-is-active-p)]
      ["Uncomment Region"       (comment-region (region-beginning)
						(region-end) '(4))
       (c-fn-region-is-active-p)]
      ["Indent Expression"      c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"  c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"     c-beginning-of-statement t]
      ["Forward Statement"      c-end-of-statement t]
      ,@(when (c-lang-const c-opt-cpp-prefix)
	  ;; Only applicable if there's a cpp preprocessor.
	  `(["Up Conditional"         c-up-conditional t]
	    ["Backward Conditional"   c-backward-conditional t]
	    ["Forward Conditional"    c-forward-conditional t]
	    "----"
	    ["Macro Expand Region"    c-macro-expand
	     (c-fn-region-is-active-p)]
	    ["Backslashify"           c-backslash-region
	     (c-fn-region-is-active-p)]))
      "----"
      ("Style..."
       ["Set Style..."                   c-set-style t]
       ["Show Current Style Name"        (message
					  "Style Name: %s"
					  c-indentation-style) t]
       ["Guess Style from this Buffer"   c-guess-buffer-no-install t]
       ["Install the Last Guessed Style..." c-guess-install
	(and c-guess-guessed-offsets-alist
	     c-guess-guessed-basic-offset) ]
       ["View the Last Guessed Style"    c-guess-view
	(and c-guess-guessed-offsets-alist
	     c-guess-guessed-basic-offset) ])
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation
	:style toggle :selected c-syntactic-indentation]
       ["Electric mode"         c-toggle-electric-state
	:style toggle :selected c-electric-flag]
       ["Auto newline"          c-toggle-auto-newline
	:style toggle :selected c-auto-newline]
       ["Hungry delete"         c-toggle-hungry-state
	:style toggle :selected c-hungry-delete-key]
       ["Subword mode"          subword-mode
	:style toggle :selected (and (boundp 'subword-mode)
                                     subword-mode)])))


;;; Syntax tables.

(defun c-populate-syntax-table (table)
  "Populate the given syntax table as necessary for a C-like language.
This includes setting ' and \" as string delimiters, and setting up
the comment syntax to handle both line style \"//\" and block style
\"/*\" \"*/\" comments."

  (modify-syntax-entry ?_  "_"     table)
  (modify-syntax-entry ?\\ "\\"    table)
  (modify-syntax-entry ?+  "."     table)
  (modify-syntax-entry ?-  "."     table)
  (modify-syntax-entry ?=  "."     table)
  (modify-syntax-entry ?%  "."     table)
  (modify-syntax-entry ?<  "."     table)
  (modify-syntax-entry ?>  "."     table)
  (modify-syntax-entry ?&  "."     table)
  (modify-syntax-entry ?|  "."     table)
  (modify-syntax-entry ?\' "\""    table)
  (modify-syntax-entry ?\240 "."   table)

  ;; Set up block and line oriented comments.  The new C
  ;; standard mandates both comment styles even in C, so since
  ;; all languages now require dual comments, we make this the
  ;; default.
  (cond
   ;; XEmacs
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table))
   ;; Emacs
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table))
   ;; incompatible
   (t (error "CC Mode is incompatible with this version of Emacs")))

  (modify-syntax-entry ?\n "> b"  table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" table))

(c-lang-defconst c-make-mode-syntax-table
  "Functions that generates the mode specific syntax tables.
The syntax tables aren't stored directly since they're quite large."
  t `(lambda ()
       (let ((table (make-syntax-table)))
	 (c-populate-syntax-table table)
	 ;; Mode specific syntaxes.
	 ,(cond ((or (c-major-mode-is 'objc-mode) (c-major-mode-is 'java-mode))
		 ;; Let '@' be part of symbols in ObjC to cope with
		 ;; its compiler directives as single keyword tokens.
		 ;; This is then necessary since it's assumed that
		 ;; every keyword is a single symbol.
		 `(modify-syntax-entry ?@ "_" table))
		((c-major-mode-is 'pike-mode)
		 `(modify-syntax-entry ?@ "." table)))
	 table)))

(c-lang-defconst c-mode-syntax-table
  ;; The syntax tables in evaluated form.  Only used temporarily when
  ;; the constants in this file are evaluated.
  t (funcall (c-lang-const c-make-mode-syntax-table)))

(c-lang-defconst c++-make-template-syntax-table
  ;; A variant of `c++-mode-syntax-table' that defines `<' and `>' as
  ;; parenthesis characters.  Used temporarily when template argument
  ;; lists are parsed.  Note that this encourages incorrect parsing of
  ;; templates since they might contain normal operators that uses the
  ;; '<' and '>' characters.  Therefore this syntax table might go
  ;; away when CC Mode handles templates correctly everywhere.
  t   nil
  (java c++) `(lambda ()
	 (let ((table (funcall ,(c-lang-const c-make-mode-syntax-table))))
	   (modify-syntax-entry ?< "(>" table)
	   (modify-syntax-entry ?> ")<" table)
	   table)))
(c-lang-defvar c++-template-syntax-table
  (and (c-lang-const c++-make-template-syntax-table)
       (funcall (c-lang-const c++-make-template-syntax-table))))

(c-lang-defconst c-no-parens-syntax-table
  ;; A variant of the standard syntax table which is used to find matching
  ;; "<"s and ">"s which have been marked as parens using syntax table
  ;; properties.  The other paren characters (e.g. "{", ")" "]") are given a
  ;; non-paren syntax here. so that the list commands will work on "< ... >"
  ;; even when there's unbalanced other parens inside them.
  ;;
  ;; This variable is nil for languages which don't have template stuff.
  t  `(lambda ()
	(if (c-lang-const c-recognize-<>-arglists)
	    (let ((table (funcall ,(c-lang-const c-make-mode-syntax-table))))
	      (modify-syntax-entry ?\( "." table)
	      (modify-syntax-entry ?\) "." table)
	      (modify-syntax-entry ?\[ "." table)
	      (modify-syntax-entry ?\] "." table)
	      (modify-syntax-entry ?\{ "." table)
	      (modify-syntax-entry ?\} "." table)
	      table))))
(c-lang-defvar c-no-parens-syntax-table
	       (funcall (c-lang-const c-no-parens-syntax-table)))

(c-lang-defconst c-identifier-syntax-modifications
  "A list that describes the modifications that should be done to the
mode syntax table to get a syntax table that matches all identifiers
and keywords as words.

The list is just like the one used in `font-lock-defaults': Each
element is a cons where the car is the character to modify and the cdr
the new syntax, as accepted by `modify-syntax-entry'."
  ;; The $ character is not allowed in most languages (one exception
  ;; is Java which allows it for legacy reasons) but we still classify
  ;; it as an identifier character since it's often used in various
  ;; machine generated identifiers.
  t    '((?_ . "w") (?$ . "w"))
  (objc java) (append '((?@ . "w"))
	       (c-lang-const c-identifier-syntax-modifications))
  awk  '((?_ . "w")))
(c-lang-defvar c-identifier-syntax-modifications
  (c-lang-const c-identifier-syntax-modifications))

(c-lang-defvar c-identifier-syntax-table
  (let ((table (copy-syntax-table (c-mode-var "mode-syntax-table")))
	(mods c-identifier-syntax-modifications)
	mod)
    (while mods
      (setq mod (car mods)
	    mods (cdr mods))
      (modify-syntax-entry (car mod) (cdr mod) table))
    table)
  "Syntax table built on the mode syntax table but additionally
classifies symbol constituents like '_' and '$' as word constituents,
so that all identifiers are recognized as words.")

(c-lang-defconst c-get-state-before-change-functions
  ;; For documentation see the following c-lang-defvar of the same name.
  ;; The value here may be a list of functions or a single function.
  t nil
  c++ '(c-extend-region-for-CPP
	c-before-change-check-<>-operators
	c-invalidate-macro-cache)
  (c objc) '(c-extend-region-for-CPP c-invalidate-macro-cache)
  ;; java 'c-before-change-check-<>-operators
  awk 'c-awk-record-region-clear-NL)
(c-lang-defvar c-get-state-before-change-functions
	       (let ((fs (c-lang-const c-get-state-before-change-functions)))
		  (if (listp fs)
		      fs
		    (list fs)))
  "If non-nil, a list of functions called from c-before-change-hook.
Typically these will record enough state to allow
`c-before-font-lock-function' to extend the region to fontify,
and may do such things as removing text-properties which must be
recalculated.

These functions will be run in the order given.  Each of them
takes 2 parameters, the BEG and END supplied to every
before-change function; on entry, the buffer will have been
widened and match-data will have been saved; point is undefined
on both entry and exit; the return value is ignored.

The functions are called even when font locking isn't enabled.

When the mode is initialized, the functions are called with
parameters \(point-min) and \(point-max).")

(c-lang-defconst c-before-font-lock-functions
  ;; For documentation see the following c-lang-defvar of the same name.
  ;; The value here may be a list of functions or a single function.
  t 'c-change-set-fl-decl-start
  (c c++ objc) '(c-neutralize-syntax-in-and-mark-CPP
		 c-change-set-fl-decl-start)
  awk 'c-awk-extend-and-syntax-tablify-region)
(c-lang-defvar c-before-font-lock-functions
	       (let ((fs (c-lang-const c-before-font-lock-functions)))
		 (if (listp fs)
		     fs
		   (list fs)))
  "If non-nil, a list of functions called just before font locking.
Typically they will extend the region about to be fontified \(see
below) and will set `syntax-table' text properties on the region.

These functions will be run in the order given.  Each of them
takes 3 parameters, the BEG, END, and OLD-LEN supplied to every
after-change function; point is undefined on both entry and exit;
on entry, the buffer will have been widened and match-data will
have been saved; the return value is ignored.

The functions may extend the region to be fontified by setting the
buffer local variables c-new-BEG and c-new-END.

The functions are called even when font locking is disabled.

When the mode is initialized, these functions are called with
parameters \(point-min), \(point-max) and <buffer size>.")

(c-lang-defconst c-before-context-fontification-functions
  awk nil
  t 'c-context-set-fl-decl-start)
  ;; For documentation see the following c-lang-defvar of the same name.
  ;; The value here may be a list of functions or a single function.
(c-lang-defvar c-before-context-fontification-functions
  (let ((fs (c-lang-const c-before-context-fontification-functions)))
    (if (listp fs)
	fs
      (list fs)))
  "If non-nil, a list of functions called just before context (or
other non-change) fontification is done.  Typically they will
extend the region.

These functions will be run in the order given.  Each of them
takes 2 parameters, the BEG and END of the region to be
fontified.  Point is undefined on both entry and exit.  On entry,
the buffer will have been widened and match-data will have been
saved; the return value is a cons of the adjusted
region, (NEW-BEG . NEW-END).")


;;; Syntactic analysis ("virtual semicolons") for line-oriented languages (AWK).
(c-lang-defconst c-at-vsemi-p-fn
  "Contains a function \"Is there a virtual semicolon at POS or point?\".
Such a function takes one optional parameter, a buffer position (defaults to
point), and returns nil or t.  This variable contains nil for languages which
don't have EOL terminated statements. "
  t nil
  (c c++ objc) 'c-at-macro-vsemi-p
  awk 'c-awk-at-vsemi-p)
(c-lang-defvar c-at-vsemi-p-fn (c-lang-const c-at-vsemi-p-fn))

(c-lang-defconst c-vsemi-status-unknown-p-fn
  "Contains a function \"are we unsure whether there is a virtual semicolon on this line?\".
The (admittedly kludgy) purpose of such a function is to prevent an infinite
recursion in c-beginning-of-statement-1 when point starts at a `while' token.
The function MUST NOT UNDER ANY CIRCUMSTANCES call c-beginning-of-statement-1,
even indirectly.  This variable contains nil for languages which don't have
EOL terminated statements."
  t nil
  (c c++ objc) 'c-macro-vsemi-status-unknown-p
  awk 'c-awk-vsemi-status-unknown-p)
(c-lang-defvar c-vsemi-status-unknown-p-fn
  (c-lang-const c-vsemi-status-unknown-p-fn))


;;; Lexer-level syntax (identifiers, tokens etc).

(c-lang-defconst c-has-bitfields
  "Whether the language has bitfield declarations."
  t nil
  (c c++ objc) t)
(c-lang-defvar c-has-bitfields (c-lang-const c-has-bitfields))

(c-lang-defconst c-symbol-start
  "Regexp that matches the start of a symbol, i.e. any identifier or
keyword.  It's unspecified how far it matches.	Does not contain a \\|
operator at the top level."
  t    (concat "[" c-alpha "_]")
  java (concat "[" c-alpha "_@]")
  objc (concat "[" c-alpha "@]")
  pike (concat "[" c-alpha "_`]"))
(c-lang-defvar c-symbol-start (c-lang-const c-symbol-start))

(c-lang-defconst c-symbol-chars
  "Set of characters that can be part of a symbol.
This is of the form that fits inside [ ] in a regexp."
  ;; Pike note: With the backquote identifiers this would include most
  ;; operator chars too, but they are handled with other means instead.
  t    (concat c-alnum "_$")
  objc (concat c-alnum "_$@"))
(c-lang-defvar c-symbol-chars (c-lang-const c-symbol-chars))

(c-lang-defconst c-symbol-key
  "Regexp matching identifiers and keywords (with submatch 0).  Assumed
to match if `c-symbol-start' matches on the same position."
  t    (concat (c-lang-const c-symbol-start)
	       "[" (c-lang-const c-symbol-chars) "]*")
  pike (concat
	;; Use the value from C here since the operator backquote is
	;; covered by the other alternative.
	(c-lang-const c-symbol-key c)
	"\\|"
	(c-make-keywords-re nil
	  (c-lang-const c-overloadable-operators))))
(c-lang-defvar c-symbol-key (c-lang-const c-symbol-key))

(c-lang-defconst c-symbol-key-depth
  ;; Number of regexp grouping parens in `c-symbol-key'.
  t (regexp-opt-depth (c-lang-const c-symbol-key)))

(c-lang-defconst c-nonsymbol-chars
  "This is the set of chars that can't be part of a symbol, i.e. the
negation of `c-symbol-chars'."
  t (concat "^" (c-lang-const c-symbol-chars)))
(c-lang-defvar c-nonsymbol-chars (c-lang-const c-nonsymbol-chars))

(c-lang-defconst c-nonsymbol-key
  "Regexp that matches any character that can't be part of a symbol.
It's usually appended to other regexps to avoid matching a prefix.
It's assumed to not contain any submatchers."
  ;; The same thing regarding Unicode identifiers applies here as to
  ;; `c-symbol-key'.
  t (concat "[" (c-lang-const c-nonsymbol-chars) "]"))

(c-lang-defconst c-identifier-ops
  "The operators that make up fully qualified identifiers.  nil in
languages that don't have such things.  See `c-operators' for a
description of the format.  Binary operators can concatenate symbols,
e.g. \"::\" in \"A::B::C\".  Prefix operators can precede identifiers,
e.g. \"~\" in \"~A::B\".  Other types of operators aren't supported.

This value is by default merged into `c-operators'."
  t    nil
  c++  '((prefix "~" "??-" "compl")
	 (right-assoc "::")
	 (prefix "::"))
  ;; Java has "." to concatenate identifiers but it's also used for
  ;; normal indexing.  There's special code in the Java font lock
  ;; rules to fontify qualified identifiers based on the standard
  ;; naming conventions.  We still define "." here to make
  ;; `c-forward-name' move over as long names as possible which is
  ;; necessary to e.g. handle throws clauses correctly.
  java '((left-assoc "."))
  idl  '((left-assoc "::")
	 (prefix "::"))
  pike '((left-assoc "::")
	 (prefix "::")
	 (left-assoc ".")))

(c-lang-defconst c-opt-identifier-concat-key
  ;; Appendable adorned regexp matching the operators that join
  ;; symbols to fully qualified identifiers, or nil in languages that
  ;; don't have such things.
  ;;
  ;; This was a docstring constant in 5.30.  It still works but is now
  ;; considered internal - change `c-identifier-ops' instead.
  t (let ((ops (c-filter-ops (c-lang-const c-identifier-ops)
			     '(left-assoc right-assoc)
			     t)))
      (when ops
	(c-make-keywords-re 'appendable ops))))
(c-lang-defvar c-opt-identifier-concat-key
  (c-lang-const c-opt-identifier-concat-key)
  'dont-doc)

(c-lang-defconst c-opt-identifier-concat-key-depth
  ;; Number of regexp grouping parens in `c-opt-identifier-concat-key'.
  t (regexp-opt-depth (c-lang-const c-opt-identifier-concat-key)))

(c-lang-defconst c-opt-identifier-prefix-key
  ;; Appendable adorned regexp matching operators that might precede
  ;; an identifier and that are part of the identifier in that case.
  ;; nil in languages without such things.
  t (let ((ops (c-filter-ops (c-lang-const c-identifier-ops)
			     '(prefix)
			     t)))
      (when ops
	(c-make-keywords-re 'appendable ops))))

(c-lang-defconst c-after-id-concat-ops
  "Operators that can occur after a binary operator on `c-identifier-ops'
in identifiers.  nil in languages that don't have such things.

Operators here should also have appropriate entries in `c-operators' -
it's not taken care of by default."
  t    nil
  ;; '~' for destructors in C++, '*' for member pointers.
  c++  '("~" "*")
  ;; In Java we recognize '*' to deal with "foo.bar.*" that can occur
  ;; in import declarations.  (This will also match bogus things like
  ;; "foo.*bar" but we don't bother.)
  java '("*"))

(c-lang-defconst c-opt-after-id-concat-key
  ;; Regexp that must match the token after
  ;; `c-opt-identifier-concat-key' for it to be considered an
  ;; identifier concatenation operator (which e.g. causes the
  ;; preceding identifier to be fontified as a reference).  Assumed to
  ;; be a string if `c-opt-identifier-concat-key' is.
  ;;
  ;; This was a docstring constant in 5.30.  It still works but is now
  ;; considered internal - change `c-after-id-concat-ops' instead.
  t (concat (c-lang-const c-symbol-start)
	    (if (c-lang-const c-after-id-concat-ops)
		(concat "\\|" (c-make-keywords-re 'appendable
				(c-lang-const c-after-id-concat-ops)))
	      "")))

(c-lang-defconst c-identifier-start
  "Regexp that matches the start of an (optionally qualified) identifier.
It should also match all keywords.  It's unspecified how far it
matches."
  t (concat (c-lang-const c-symbol-start)
	    (if (c-lang-const c-opt-identifier-prefix-key)
		(concat "\\|"
			(c-lang-const c-opt-identifier-prefix-key))
	      "")))
(c-lang-defvar c-identifier-start (c-lang-const c-identifier-start))

(c-lang-defconst c-identifier-key
  "Regexp matching a fully qualified identifier, like \"A::B::c\" in
C++.  It does not recognize the full range of syntactic whitespace
between the tokens; `c-forward-name' has to be used for that.  It
should also not match identifiers containing parenthesis groupings,
e.g. identifiers with template arguments such as \"A<X,Y>\" in C++."
  ;; This regexp is more complex than strictly necessary to ensure
  ;; that it can be matched with a minimum of backtracking.
  t (concat (if (c-lang-const c-opt-identifier-prefix-key)
		(concat
		 "\\("
		 (c-lang-const c-opt-identifier-prefix-key)
		 (c-lang-const c-simple-ws) "*"
		 "\\)?")
	      "")
	    "\\(" (c-lang-const c-symbol-key) "\\)"
	    (if (c-lang-const c-opt-identifier-concat-key)
		(concat
		 "\\("
		 (c-lang-const c-simple-ws) "*"
		 (c-lang-const c-opt-identifier-concat-key)
		 (c-lang-const c-simple-ws) "*"
		 (if (c-lang-const c-after-id-concat-ops)
		     (concat
		      "\\("
		       (c-make-keywords-re 'appendable
			 (c-lang-const c-after-id-concat-ops))
		      (concat
		       ;; For flexibility, consider the symbol match
		       ;; optional if we've hit a
		       ;; `c-after-id-concat-ops' operator.  This is
		       ;; also necessary to handle the "*" that can
		       ;; end import declaration identifiers in Java.
		       "\\("
		       (c-lang-const c-simple-ws) "*"
		       "\\(" (c-lang-const c-symbol-key) "\\)"
		       "\\)?")
		      "\\|"
		      "\\(" (c-lang-const c-symbol-key) "\\)"
		      "\\)")
		   (concat "\\(" (c-lang-const c-symbol-key) "\\)"))
		 "\\)*")
	      "")))
(c-lang-defvar c-identifier-key (c-lang-const c-identifier-key))

(c-lang-defconst c-identifier-last-sym-match
  ;; This was a docstring constant in 5.30 but it's no longer used.
  ;; It's only kept to avoid breaking third party code.
  ;;
  ;; Used to identify the submatch in `c-identifier-key' that
  ;; surrounds the last symbol in the qualified identifier.  It's a
  ;; list of submatch numbers, of which the first that has a match is
  ;; taken.  It's assumed that at least one does when the regexp has
  ;; matched.
  t nil)

(c-lang-defconst c-string-escaped-newlines
  "Set if the language support backslash escaped newlines inside string
literals."
  t nil
  (c c++ objc pike) t)
(c-lang-defvar c-string-escaped-newlines
  (c-lang-const c-string-escaped-newlines))

(c-lang-defconst c-multiline-string-start-char
  "Set if the language supports multiline string literals without escaped
newlines.  If t, all string literals are multiline.  If a character,
only literals where the open quote is immediately preceded by that
literal are multiline."
  t    nil
  pike ?#)
(c-lang-defvar c-multiline-string-start-char
  (c-lang-const c-multiline-string-start-char))

(c-lang-defconst c-opt-cpp-symbol
  "The symbol which starts preprocessor constructs when in the margin."
  t "#"
  (java awk) nil)
(c-lang-defvar c-opt-cpp-symbol (c-lang-const c-opt-cpp-symbol))

(c-lang-defconst c-opt-cpp-prefix
  "Regexp matching the prefix of a cpp directive in the languages that
normally use that macro preprocessor.  Tested at bol or at boi.
Assumed to not contain any submatches or \\| operators."
  ;; TODO (ACM, 2005-04-01).  Amend the following to recognize escaped NLs;
  ;; amend all uses of c-opt-cpp-prefix which count regexp-depth.
  t "\\s *#\\s *"
  (java awk) nil)
(c-lang-defvar c-opt-cpp-prefix (c-lang-const c-opt-cpp-prefix))

(c-lang-defconst c-anchored-cpp-prefix
  "Regexp matching the prefix of a cpp directive anchored to BOL,
in the languages that have a macro preprocessor."
  t (if (c-lang-const c-opt-cpp-prefix)
	(concat "^" (c-lang-const c-opt-cpp-prefix))))
(c-lang-defvar c-anchored-cpp-prefix (c-lang-const c-anchored-cpp-prefix))

(c-lang-defconst c-opt-cpp-start
  "Regexp matching the prefix of a cpp directive including the directive
name, or nil in languages without preprocessor support.  The first
submatch surrounds the directive name."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   (concat (c-lang-const c-opt-cpp-prefix)
		   "\\([" c-alnum "]+\\)"))
  ;; Pike, being a scripting language, recognizes hash-bangs too.
  pike (concat (c-lang-const c-opt-cpp-prefix)
	       "\\([" c-alnum "]+\\|!\\)"))
(c-lang-defvar c-opt-cpp-start (c-lang-const c-opt-cpp-start))

(c-lang-defconst c-cpp-message-directives
  "List of cpp directives (without the prefix) that are followed by a
string message."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   '("error"))
  (c c++ objc pike) '("error" "warning"))

(c-lang-defconst c-cpp-include-directives
  "List of cpp directives (without the prefix) that are followed by a
file name in angle brackets or quotes."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   '("include"))
  objc '("include" "import"))

(c-lang-defconst c-opt-cpp-macro-define
  "Cpp directive (without the prefix) that is followed by a macro
definition, or nil if the language doesn't have any."
  t (if (c-lang-const c-opt-cpp-prefix)
	"define"))
(c-lang-defvar c-opt-cpp-macro-define
  (c-lang-const c-opt-cpp-macro-define))

(c-lang-defconst c-opt-cpp-macro-define-start
  ;; Regexp matching everything up to the macro body of a cpp define, or the
  ;; end of the logical line if there is none.  Submatch 1 is the name of the
  ;; macro.  Set if c-opt-cpp-macro-define is.
  t (if (c-lang-const c-opt-cpp-macro-define)
	(concat (c-lang-const c-opt-cpp-prefix)
		(c-lang-const c-opt-cpp-macro-define)
		"[ \t]+\\(\\(\\sw\\|_\\)+\\)\\(\([^\)]*\)\\)?"
		;;       ^                 ^ #defined name
		"\\([ \t]\\|\\\\\n\\)*")))
(c-lang-defvar c-opt-cpp-macro-define-start
  (c-lang-const c-opt-cpp-macro-define-start))

(c-lang-defconst c-opt-cpp-macro-define-id
  ;; Regexp matching everything up to the end of the identifier defined
  ;; by a cpp define.
  t (if (c-lang-const c-opt-cpp-macro-define)
	(concat (c-lang-const c-opt-cpp-prefix)	; #
		(c-lang-const c-opt-cpp-macro-define) ; define
		"[ \t]+\\(\\sw\\|_\\)+")))
(c-lang-defvar c-opt-cpp-macro-define-id
  (c-lang-const c-opt-cpp-macro-define-id))

(c-lang-defconst c-cpp-expr-directives
  "List of cpp directives (without the prefix) that are followed by an
expression."
  t (if (c-lang-const c-opt-cpp-prefix)
	'("if" "elif")))

(c-lang-defconst c-cpp-expr-intro-re
  "Regexp which matches the start of a CPP directive which contains an
expression, or nil if there aren't any in the language."
  t (if (c-lang-const c-cpp-expr-directives)
	(concat
	 (c-lang-const c-opt-cpp-prefix)
	 (c-make-keywords-re t (c-lang-const c-cpp-expr-directives)))))
(c-lang-defvar c-cpp-expr-intro-re
  (c-lang-const c-cpp-expr-intro-re))

(c-lang-defconst c-cpp-expr-functions
  "List of functions in cpp expressions."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   '("defined"))
  pike '("defined" "efun" "constant"))

(c-lang-defconst c-assignment-operators
  "List of all assignment operators."
  t    '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=")
  java (append (c-lang-const c-assignment-operators)
	       '(">>>="))
  c++  (append (c-lang-const c-assignment-operators)
	       '("and_eq" "or_eq" "xor_eq" "??!=" "??'="))
  idl  nil)

(c-lang-defconst c-operators
  "List describing all operators, along with their precedence and
associativity.  The order in the list corresponds to the precedence of
the operators: The operators in each element are a group with the same
precedence, and the group has higher precedence than the groups in all
following elements.  The car of each element describes the type of the
operator group, and the cdr is a list of the operator tokens in it.
The operator group types are:

'prefix         Unary prefix operators.
'postfix        Unary postfix operators.
'postfix-if-paren
		Unary postfix operators if and only if the chars have
		parenthesis syntax.
'left-assoc     Binary left associative operators (i.e. a+b+c means (a+b)+c).
'right-assoc    Binary right associative operators (i.e. a=b=c means a=(b=c)).
'right-assoc-sequence
                Right associative operator that constitutes of a
                sequence of tokens that separate expressions.  All the
                tokens in the group are in this case taken as
                describing the sequence in one such operator, and the
                order between them is therefore significant.

Operators containing a character with paren syntax are taken to match
with a corresponding open/close paren somewhere else.  A postfix
operator with close paren syntax is taken to end a postfix expression
started somewhere earlier, rather than start a new one at point.  Vice
versa for prefix operators with open paren syntax.

Note that operators like \".\" and \"->\" which in language references
often are described as postfix operators are considered binary here,
since CC Mode treats every identifier as an expression."

  ;; There's currently no code in CC Mode that exploit all the info
  ;; in this variable; precedence, associativity etc are present as a
  ;; preparation for future work.

  t `(;; Preprocessor.
      ,@(when (c-lang-const c-opt-cpp-prefix)
	  `((prefix "#"
		    ,@(when (c-major-mode-is '(c-mode c++-mode))
			'("%:" "??=")))
	    (left-assoc "##"
			,@(when (c-major-mode-is '(c-mode c++-mode))
			    '("%:%:" "??=??=")))))

      ;; Primary.
      ,@(c-lang-const c-identifier-ops)
      ,@(cond ((or (c-major-mode-is 'c++-mode) (c-major-mode-is 'java-mode))
	       `((postfix-if-paren "<" ">"))) ; Templates.
	      ((c-major-mode-is 'pike-mode)
	       `((prefix "global" "predef")))
	      ((c-major-mode-is 'java-mode)
	       `((prefix "super"))))

      ;; Postfix.
      ,@(when (c-major-mode-is 'c++-mode)
	  ;; The following need special treatment.
	  `((prefix "dynamic_cast" "static_cast"
		    "reinterpret_cast" "const_cast" "typeid")))
      (left-assoc "."
		  ,@(unless (c-major-mode-is 'java-mode)
		      '("->")))
      (postfix "++" "--" "[" "]" "(" ")"
	       ,@(when (c-major-mode-is '(c-mode c++-mode))
		   '("<:" ":>" "??(" "??)")))

      ;; Unary.
      (prefix "++" "--" "+" "-" "!" "~"
	      ,@(when (c-major-mode-is 'c++-mode) '("not" "compl"))
	      ,@(when (c-major-mode-is '(c-mode c++-mode))
		  '("*" "&" "sizeof" "??-"))
	      ,@(when (c-major-mode-is 'objc-mode)
		  '("@selector" "@protocol" "@encode"))
	      ;; The following need special treatment.
	      ,@(cond ((c-major-mode-is 'c++-mode)
		       '("new" "delete"))
		      ((c-major-mode-is 'java-mode)
		       '("new"))
		      ((c-major-mode-is 'pike-mode)
		       '("class" "lambda" "catch" "throw" "gauge")))
	      "(" ")"			; Cast.
	      ,@(when (c-major-mode-is 'pike-mode)
		  '("[" "]")))		; Type cast.

      ;; Member selection.
      ,@(when (c-major-mode-is 'c++-mode)
	  `((left-assoc ".*" "->*")))

      ;; Multiplicative.
      (left-assoc "*" "/" "%")

      ;; Additive.
      (left-assoc "+" "-")

      ;; Shift.
      (left-assoc "<<" ">>"
		  ,@(when (c-major-mode-is 'java-mode)
		      '(">>>")))

      ;; Relational.
      (left-assoc "<" ">" "<=" ">="
		  ,@(when (c-major-mode-is 'java-mode)
		      '("instanceof")))

      ;; Equality.
      (left-assoc "==" "!="
		  ,@(when (c-major-mode-is 'c++-mode) '("not_eq")))

      ;; Bitwise and.
      (left-assoc "&"
		  ,@(when (c-major-mode-is 'c++-mode) '("bitand")))

      ;; Bitwise exclusive or.
      (left-assoc "^"
		  ,@(when (c-major-mode-is '(c-mode c++-mode))
		      '("??'"))
		  ,@(when (c-major-mode-is 'c++-mode) '("xor")))

      ;; Bitwise or.
      (left-assoc "|"
		  ,@(when (c-major-mode-is '(c-mode c++-mode))
		      '("??!"))
		  ,@(when (c-major-mode-is 'c++-mode) '("bitor")))

      ;; Logical and.
      (left-assoc "&&"
		  ,@(when (c-major-mode-is 'c++-mode) '("and")))

      ;; Logical or.
      (left-assoc "||"
		  ,@(when (c-major-mode-is '(c-mode c++-mode))
		      '("??!??!"))
		  ,@(when (c-major-mode-is 'c++-mode) '("or")))

      ;; Conditional.
      (right-assoc-sequence "?" ":")

      ;; Assignment.
      (right-assoc ,@(c-lang-const c-assignment-operators))

      ;; Exception.
      ,@(when (c-major-mode-is 'c++-mode)
	  '((prefix "throw")))

      ;; Sequence.
      (left-assoc ","))

  ;; IDL got its own definition since it has a much smaller operator
  ;; set than the other languages.
  idl `(;; Preprocessor.
	(prefix "#")
	(left-assoc "##")
	;; Primary.
	,@(c-lang-const c-identifier-ops)
	;; Unary.
	(prefix  "+" "-" "~")
	;; Multiplicative.
	(left-assoc "*" "/" "%")
	;; Additive.
	(left-assoc "+" "-")
	;; Shift.
	(left-assoc "<<" ">>")
	;; And.
	(left-assoc "&")
	;; Xor.
	(left-assoc "^")
	;; Or.
	(left-assoc "|")))

(c-lang-defconst c-operator-list
  ;; The operators as a flat list (without duplicates).
  t (c-filter-ops (c-lang-const c-operators) t t))

(c-lang-defconst c-overloadable-operators
  "List of the operators that are overloadable, in their \"identifier
form\".  See also `c-op-identifier-prefix'."
  t    nil
  c++  '("new" "delete" ;; Can be followed by "[]" but we ignore that.
	 "+" "-" "*" "/" "%"
	 "^" "??'" "xor" "&" "bitand" "|" "??!" "bitor" "~" "??-" "compl"
	 "!" "=" "<" ">" "+=" "-=" "*=" "/=" "%=" "^="
	 "??'=" "xor_eq" "&=" "and_eq" "|=" "??!=" "or_eq"
	 "<<" ">>" ">>=" "<<=" "==" "!=" "not_eq" "<=" ">="
	 "&&" "and" "||" "??!??!" "or" "++" "--" "," "->*" "->"
	 "()" "[]" "<::>" "??(??)")
  ;; These work like identifiers in Pike.
  pike '("`+" "`-" "`&" "`|" "`^" "`<<" "`>>" "`*" "`/" "`%" "`~"
	 "`==" "`<" "`>" "`!" "`[]" "`[]=" "`->" "`->=" "`()" "``+"
	 "``-" "``&" "``|" "``^" "``<<" "``>>" "``*" "``/" "``%"
	 "`+="))

(c-lang-defconst c-overloadable-operators-regexp
  ;; Regexp tested after an "operator" token in C++.
  t   nil
  c++ (c-make-keywords-re nil (c-lang-const c-overloadable-operators)))
(c-lang-defvar c-overloadable-operators-regexp
  (c-lang-const c-overloadable-operators-regexp))

(c-lang-defconst c-opt-op-identifier-prefix
  "Regexp matching the token before the ones in
`c-overloadable-operators' when operators are specified in their
\"identifier form\".  This typically matches \"operator\" in C++ where
operator functions are specified as e.g. \"operator +\".  It's nil in
languages without operator functions or where the complete operator
identifier is listed in `c-overloadable-operators'.

This regexp is assumed to not match any non-operator identifier."
  t   nil
  c++ (c-make-keywords-re t '("operator")))
(c-lang-defvar c-opt-op-identifier-prefix
  (c-lang-const c-opt-op-identifier-prefix))

;; Note: the following alias is an old name which was a mis-spelling.  It has
;; been corrected above and throughout cc-engine.el.  It will be removed at
;; some release very shortly in the future.  ACM, 2006-04-14.
(defvaralias 'c-opt-op-identitier-prefix 'c-opt-op-identifier-prefix)
(make-obsolete-variable 'c-opt-op-identitier-prefix 'c-opt-op-identifier-prefix
			"CC Mode 5.31.4, 2006-04-14")

(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
  t '("{" "}" "(" ")" "[" "]" ";" ":" "," "=" "/*" "*/" "//")
  (c c++ pike) (append '("#" "##"	; Used by cpp.
			 "::" "...")
		       (c-lang-const c-other-op-syntax-tokens))
  (c c++) (append '("*") (c-lang-const c-other-op-syntax-tokens))
  c++  (append '("&" "<%" "%>" "<:" ":>" "%:" "%:%:")
	       (c-lang-const c-other-op-syntax-tokens))
  objc (append '("#" "##"		; Used by cpp.
		 "+" "-") (c-lang-const c-other-op-syntax-tokens))
  idl  (append '("#" "##")		; Used by cpp.
	       (c-lang-const c-other-op-syntax-tokens))
  pike (append '("..")
	       (c-lang-const c-other-op-syntax-tokens)
	       (c-lang-const c-overloadable-operators))
  awk '("{" "}" "(" ")" "[" "]" ";" "," "=" "/"))

(c-lang-defconst c-all-op-syntax-tokens
  ;; List of all tokens in the punctuation and parenthesis syntax
  ;; classes.
  t (delete-duplicates (append (c-lang-const c-other-op-syntax-tokens)
			       (c-lang-const c-operator-list))
		       :test 'string-equal))

(c-lang-defconst c-nonsymbol-token-char-list
  ;; List containing all chars not in the word, symbol or
  ;; syntactically irrelevant syntax classes, i.e. all punctuation,
  ;; parenthesis and string delimiter chars.
  t (c-with-syntax-table (c-lang-const c-mode-syntax-table)
      ;; Only go through the chars in the printable ASCII range.  No
      ;; language so far has 8-bit or widestring operators.
      (let (list (char 32))
	(while (< char 127)
	  (or (memq (char-syntax char) '(?w ?_ ?< ?> ?\ ))
	      (setq list (cons (c-int-to-char char) list)))
	  (setq char (1+ char)))
	list)))

(c-lang-defconst c-nonsymbol-token-regexp
  ;; Regexp matching all tokens in the punctuation and parenthesis
  ;; syntax classes.  Note that this also matches ".", which can start
  ;; a float.
  t (c-make-keywords-re nil
      (c-filter-ops (c-lang-const c-all-op-syntax-tokens)
		    t
		    "\\`\\(\\s.\\|\\s\(\\|\\s\)\\)+\\'")))
(c-lang-defvar c-nonsymbol-token-regexp
  (c-lang-const c-nonsymbol-token-regexp))

(c-lang-defconst c-assignment-op-regexp
  ;; Regexp matching all assignment operators and only them.  The
  ;; beginning of the first submatch is used to detect the end of the
  ;; token, along with the end of the whole match.
  t (if (c-lang-const c-assignment-operators)
	(concat
	 ;; Need special case for "=" since it's a prefix of "==".
	 "=\\([^=]\\|$\\)"
	 "\\|"
	 (c-make-keywords-re nil
	   (set-difference (c-lang-const c-assignment-operators)
			   '("=")
			   :test 'string-equal)))
      "\\<\\>"))
(c-lang-defvar c-assignment-op-regexp
  (c-lang-const c-assignment-op-regexp))

(c-lang-defconst c-<>-multichar-token-regexp
  ;; Regexp matching all tokens containing "<" or ">" which are longer
  ;; than one char.
  t (c-make-keywords-re nil
      (c-filter-ops (c-lang-const c-all-op-syntax-tokens)
		    t
		    ".[<>]\\|[<>].")))
(c-lang-defvar c-<>-multichar-token-regexp
  (c-lang-const c-<>-multichar-token-regexp))

(c-lang-defconst c-<-op-cont-regexp
  ;; Regexp matching the second and subsequent characters of all
  ;; multicharacter tokens that begin with "<".
  t (c-make-keywords-re nil
      (c-filter-ops (c-lang-const c-all-op-syntax-tokens)
		    t
		    "\\`<."
		    (lambda (op) (substring op 1)))))

(c-lang-defvar c-<-op-cont-regexp (c-lang-const c-<-op-cont-regexp))

(c-lang-defconst c->-op-cont-regexp
  ;; Regexp matching the second and subsequent characters of all
  ;; multicharacter tokens that begin with ">".
  t (c-make-keywords-re nil
      (c-filter-ops (c-lang-const c-all-op-syntax-tokens)
		    t
		    "\\`>."
		    (lambda (op) (substring op 1))))
  java (c-make-keywords-re nil
	 (c-filter-ops (c-lang-const c-all-op-syntax-tokens)
		       t
		       "\\`>[^>]\\|\\`>>[^>]"
		       (lambda (op) (substring op 1)))))

(c-lang-defvar c->-op-cont-regexp (c-lang-const c->-op-cont-regexp))

(c-lang-defconst c-stmt-delim-chars
  ;; The characters that should be considered to bound statements.  To
  ;; optimize `c-crosses-statement-barrier-p' somewhat, it's assumed to
  ;; begin with "^" to negate the set.  If ? : operators should be
  ;; detected then the string must end with "?:".
  t "^;{}?:")
(c-lang-defvar c-stmt-delim-chars (c-lang-const c-stmt-delim-chars))

(c-lang-defconst c-stmt-delim-chars-with-comma
  ;; Variant of `c-stmt-delim-chars' that additionally contains ','.
  t    "^;,{}?:")
(c-lang-defvar c-stmt-delim-chars-with-comma
  (c-lang-const c-stmt-delim-chars-with-comma))


;;; Syntactic whitespace.

(c-lang-defconst c-simple-ws
  "Regexp matching an ordinary whitespace character.
Does not contain a \\| operator at the top level."
  ;; "\\s " is not enough since it doesn't match line breaks.
  t "\\(\\s \\|[\n\r]\\)")

(c-lang-defconst c-simple-ws-depth
  ;; Number of regexp grouping parens in `c-simple-ws'.
  t (regexp-opt-depth (c-lang-const c-simple-ws)))

(c-lang-defconst c-line-comment-starter
  "String that starts line comments, or nil if such don't exist.
Line comments are always terminated by newlines.  At least one of
`c-block-comment-starter' and this one is assumed to be set.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  t    "//"
  awk  "#")
(c-lang-defvar c-line-comment-starter (c-lang-const c-line-comment-starter))

(c-lang-defconst c-block-comment-starter
  "String that starts block comments, or nil if such don't exist.
Block comments are ended by `c-block-comment-ender', which is assumed
to be set if this is.  At least one of `c-line-comment-starter' and
this one is assumed to be set.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  t    "/*"
  awk  nil)

(c-lang-defconst c-block-comment-ender
  "String that ends block comments, or nil if such don't exist.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  t    "*/"
  awk  nil)

(c-lang-defconst c-comment-start-regexp
  ;; Regexp to match the start of any type of comment.
  t (let ((re (c-make-keywords-re nil
		(list (c-lang-const c-line-comment-starter)
		      (c-lang-const c-block-comment-starter)))))
      (if (memq 'gen-comment-delim c-emacs-features)
	  (concat re "\\|\\s!")
	re)))
(c-lang-defvar c-comment-start-regexp (c-lang-const c-comment-start-regexp))

(c-lang-defconst c-block-comment-start-regexp
  ;; Regexp which matches the start of a block comment (if such exists in the
  ;; language)
  t (if (c-lang-const c-block-comment-starter)
	(regexp-quote (c-lang-const c-block-comment-starter))
      "\\<\\>"))
(c-lang-defvar c-block-comment-start-regexp
  (c-lang-const c-block-comment-start-regexp))

(c-lang-defconst c-line-comment-start-regexp
  ;; Regexp which matches the start of a line comment (if such exists in the
  ;; language; it does in all 7 CC Mode languages).
  t (if (c-lang-const c-line-comment-starter)
	(regexp-quote (c-lang-const c-line-comment-starter))
      "\\<\\>"))
(c-lang-defvar c-line-comment-start-regexp
	       (c-lang-const c-line-comment-start-regexp))

(c-lang-defconst c-literal-start-regexp
  ;; Regexp to match the start of comments and string literals.
  t (concat (c-lang-const c-comment-start-regexp)
	    "\\|"
	    (if (memq 'gen-string-delim c-emacs-features)
		"\"|"
	      "\"")))
(c-lang-defvar c-literal-start-regexp (c-lang-const c-literal-start-regexp))

(c-lang-defconst c-doc-comment-start-regexp
  "Regexp to match the start of documentation comments."
  t    "\\<\\>"
  ;; From font-lock.el: `doxygen' uses /*! while others use /**.
  (c c++ objc) "/\\*[*!]"
  java "/\\*\\*"
  pike "/[/*]!")
(c-lang-defvar c-doc-comment-start-regexp
  (c-lang-const c-doc-comment-start-regexp))

(c-lang-defconst comment-start
  "String that starts comments inserted with M-; etc.
`comment-start' is initialized from this."
  ;; Default: Prefer line comments to block comments, and pad with a space.
  t (concat (or (c-lang-const c-line-comment-starter)
		(c-lang-const c-block-comment-starter))
	    " ")
  ;; In C we still default to the block comment style since line
  ;; comments aren't entirely portable.
  c "/* ")
(c-lang-setvar comment-start (c-lang-const comment-start))

(c-lang-defconst comment-end
  "String that ends comments inserted with M-; etc.
`comment-end' is initialized from this."
  ;; Default: Use block comment style if comment-start uses block
  ;; comments, and pad with a space in that case.
  t (if (string-match (concat "\\`\\("
			      (c-lang-const c-block-comment-start-regexp)
			      "\\)")
		      (c-lang-const comment-start))
	(concat " " (c-lang-const c-block-comment-ender))
      ""))
(c-lang-setvar comment-end (c-lang-const comment-end))

(c-lang-defconst comment-start-skip
  "Regexp to match the start of a comment plus everything up to its body.
`comment-start-skip' is initialized from this."
  ;; Default: Allow the last char of the comment starter(s) to be
  ;; repeated, then allow any amount of horizontal whitespace.
  t (concat "\\("
	    (c-concat-separated
	     (mapcar (lambda (cs)
		       (when cs
			 (concat (regexp-quote cs) "+")))
		     (list (c-lang-const c-line-comment-starter)
			   (c-lang-const c-block-comment-starter)))
	     "\\|")
	    "\\)\\s *"))
(c-lang-setvar comment-start-skip (c-lang-const comment-start-skip))

(c-lang-defconst c-syntactic-ws-start
  ;; Regexp matching any sequence that can start syntactic whitespace.
  ;; The only uncertain case is '#' when there are cpp directives.
  t (concat "\\s \\|"
	    (c-make-keywords-re nil
	      (append (list (c-lang-const c-line-comment-starter)
			    (c-lang-const c-block-comment-starter)
			    (when (c-lang-const c-opt-cpp-prefix)
			      "#"))
		      '("\n" "\r")))
	    "\\|\\\\[\n\r]"
	    (when (memq 'gen-comment-delim c-emacs-features)
	      "\\|\\s!")))
(c-lang-defvar c-syntactic-ws-start (c-lang-const c-syntactic-ws-start))

(c-lang-defconst c-syntactic-ws-end
  ;; Regexp matching any single character that might end syntactic whitespace.
  t (concat "\\s \\|"
	    (c-make-keywords-re nil
	      (append (when (c-lang-const c-block-comment-ender)
			(list
			 (string
			  (elt (c-lang-const c-block-comment-ender)
			       (1- (length
				    (c-lang-const c-block-comment-ender)))))))
		      '("\n" "\r")))
	    (when (memq 'gen-comment-delim c-emacs-features)
	      "\\|\\s!")))
(c-lang-defvar c-syntactic-ws-end (c-lang-const c-syntactic-ws-end))

(c-lang-defconst c-unterminated-block-comment-regexp
  ;; Regexp matching an unterminated block comment that doesn't
  ;; contain line breaks, or nil in languages without block comments.
  ;; Does not contain a \| operator at the top level.
  t (when (c-lang-const c-block-comment-starter)
      (concat
       (regexp-quote (c-lang-const c-block-comment-starter))
       ;; It's messy to cook together a regexp that matches anything
       ;; but c-block-comment-ender.
       (let ((end (c-lang-const c-block-comment-ender)))
	 (cond ((= (length end) 1)
		(concat "[^" end "\n\r]*"))
	       ((= (length end) 2)
		(concat "[^" (substring end 0 1) "\n\r]*"
			"\\("
			(regexp-quote (substring end 0 1)) "+"
			"[^"
			;; The quoting rules inside char classes are silly. :P
			(cond ((= (elt end 0) (elt end 1))
			       (concat (substring end 0 1) "\n\r"))
			      ((= (elt end 1) ?\])
			       (concat (substring end 1 2) "\n\r"
				       (substring end 0 1)))
			      (t
			       (concat (substring end 0 1) "\n\r"
				       (substring end 1 2))))
			"]"
			"[^" (substring end 0 1) "\n\r]*"
			"\\)*"))
	       (t
		(error "Can't handle a block comment ender of length %s"
		       (length end))))))))

(c-lang-defconst c-block-comment-regexp
  ;; Regexp matching a block comment that doesn't contain line breaks,
  ;; or nil in languages without block comments.  The reason we don't
  ;; allow line breaks is to avoid going very far and risk running out
  ;; of regexp stack; this regexp is intended to handle only short
  ;; comments that might be put in the middle of limited constructs
  ;; like declarations.  Does not contain a \| operator at the top
  ;; level.
  t (when (c-lang-const c-unterminated-block-comment-regexp)
      (concat
       (c-lang-const c-unterminated-block-comment-regexp)
       (let ((end (c-lang-const c-block-comment-ender)))
	 (cond ((= (length end) 1)
		(regexp-quote end))
	       ((= (length end) 2)
		(concat (regexp-quote (substring end 0 1)) "+"
			(regexp-quote (substring end 1 2))))
	       (t
		(error "Can't handle a block comment ender of length %s"
		       (length end))))))))

(c-lang-defconst c-nonwhite-syntactic-ws
  ;; Regexp matching a piece of syntactic whitespace that isn't a
  ;; sequence of simple whitespace characters.  As opposed to
  ;; `c-(forward|backward)-syntactic-ws', this doesn't regard cpp
  ;; directives as syntactic whitespace.
  t (c-concat-separated
     (list (when (c-lang-const c-line-comment-starter)
	     (concat (regexp-quote (c-lang-const c-line-comment-starter))
		     "[^\n\r]*[\n\r]"))
	   (c-lang-const c-block-comment-regexp)
	   "\\\\[\n\r]"
	   (when (memq 'gen-comment-delim c-emacs-features)
	     "\\s!\\S!*\\s!"))
     "\\|"))

(c-lang-defconst c-syntactic-ws
  ;; Regexp matching syntactic whitespace, including possibly the
  ;; empty string.  As opposed to `c-(forward|backward)-syntactic-ws',
  ;; this doesn't regard cpp directives as syntactic whitespace.  Does
  ;; not contain a \| operator at the top level.
  t (concat (c-lang-const c-simple-ws) "*"
	    "\\("
	    (concat "\\(" (c-lang-const c-nonwhite-syntactic-ws) "\\)"
		    (c-lang-const c-simple-ws) "*")
	    "\\)*"))

(c-lang-defconst c-syntactic-ws-depth
  ;; Number of regexp grouping parens in `c-syntactic-ws'.
  t (regexp-opt-depth (c-lang-const c-syntactic-ws)))

(c-lang-defconst c-nonempty-syntactic-ws
  ;; Regexp matching syntactic whitespace, which is at least one
  ;; character long.  As opposed to `c-(forward|backward)-syntactic-ws',
  ;; this doesn't regard cpp directives as syntactic whitespace.  Does
  ;; not contain a \| operator at the top level.
  t (concat "\\("
	    (c-lang-const c-simple-ws)
	    "\\|"
	    (c-lang-const c-nonwhite-syntactic-ws)
	    "\\)+"))

(c-lang-defconst c-nonempty-syntactic-ws-depth
  ;; Number of regexp grouping parens in `c-nonempty-syntactic-ws'.
  t (regexp-opt-depth (c-lang-const c-nonempty-syntactic-ws)))

(c-lang-defconst c-single-line-syntactic-ws
  ;; Regexp matching syntactic whitespace without any line breaks.  As
  ;; opposed to `c-(forward|backward)-syntactic-ws', this doesn't
  ;; regard cpp directives as syntactic whitespace.  Does not contain
  ;; a \| operator at the top level.
  t (if (c-lang-const c-block-comment-regexp)
	(concat "\\s *\\("
		(c-lang-const c-block-comment-regexp)
		"\\s *\\)*")
      "\\s *"))

(c-lang-defconst c-single-line-syntactic-ws-depth
  ;; Number of regexp grouping parens in `c-single-line-syntactic-ws'.
  t (regexp-opt-depth (c-lang-const c-single-line-syntactic-ws)))

(c-lang-defconst c-syntactic-eol
  ;; Regexp that matches when there is no syntactically significant
  ;; text before eol.  Macros are regarded as syntactically
  ;; significant text here.
  t (concat (c-lang-const c-single-line-syntactic-ws)
	    ;; Match eol (possibly inside a block comment or preceded
	    ;; by a line continuation backslash), or the beginning of a
	    ;; line comment.  Note: This has to be modified for awk
	    ;; where line comments start with '#'.
	    "\\("
	    (c-concat-separated
	     (list (when (c-lang-const c-line-comment-starter)
		     (regexp-quote (c-lang-const c-line-comment-starter)))
		   (when (c-lang-const c-unterminated-block-comment-regexp)
		     (concat (c-lang-const c-unterminated-block-comment-regexp)
			     "$"))
		   "\\\\$"
		   "$")
	     "\\|")
	    "\\)"))
(c-lang-defvar c-syntactic-eol (c-lang-const c-syntactic-eol))


;;; Defun functions

;; The Emacs variables beginning-of-defun-function and
;; end-of-defun-function will be set so that commands like
;; `mark-defun' and `narrow-to-defun' work right.  The key sequences
;; C-M-a and C-M-e are, however, bound directly to the CC Mode
;; functions, allowing optimization for large n.
(c-lang-defconst beginning-of-defun-function
  "Function to which beginning-of-defun-function will be set."
  t 'c-beginning-of-defun
  awk 'c-awk-beginning-of-defun)
(c-lang-setvar beginning-of-defun-function
	       (c-lang-const beginning-of-defun-function))

(c-lang-defconst end-of-defun-function
  "Function to which end-of-defun-function will be set."
  t 'c-end-of-defun
  awk 'c-awk-end-of-defun)
(c-lang-setvar end-of-defun-function (c-lang-const end-of-defun-function))

;;; In-comment text handling.

(c-lang-defconst c-paragraph-start
  "Regexp to append to `paragraph-start'."
  t    "$"
  java "\\(@[a-zA-Z]+\\>\\|$\\)"	; For Javadoc.
  pike "\\(@[a-zA-Z_-]+\\>\\([^{]\\|$\\)\\|$\\)") ; For Pike refdoc.
(c-lang-defvar c-paragraph-start (c-lang-const c-paragraph-start))

(c-lang-defconst c-paragraph-separate
  "Regexp to append to `paragraph-separate'."
  t    "$"
  pike (c-lang-const c-paragraph-start))
(c-lang-defvar c-paragraph-separate (c-lang-const c-paragraph-separate))


;;; Keyword lists.

;; Note: All and only all language constants containing keyword lists
;; should end with "-kwds"; they're automatically collected into the
;; `c-kwds-lang-consts' list below and used to build `c-keywords' etc.

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  t    '("char" "double" "float" "int" "long" "short" "signed"
	 "unsigned" "void")
  c    (append
	'("_Bool" "_Complex" "_Imaginary") ; Conditionally defined in C99.
	(c-lang-const c-primitive-type-kwds))
  c++  (append
	'("bool" "wchar_t")
	(c-lang-const c-primitive-type-kwds))
  ;; Objective-C extends C, but probably not the new stuff in C99.
  objc (append
	'("id" "Class" "SEL" "IMP" "BOOL")
	(c-lang-const c-primitive-type-kwds))
  java '("boolean" "byte" "char" "double" "float" "int" "long" "short" "void")
  idl  '("Object" "ValueBase" "any" "boolean" "char" "double" "fixed" "float"
	 "long" "octet" "sequence" "short" "string" "void" "wchar" "wstring"
	 ;; In CORBA PSDL:
	 "ref"
	 ;; The following can't really end a type, but we have to specify them
	 ;; here due to the assumption in `c-primitive-type-prefix-kwds'.  It
	 ;; doesn't matter that much.
	 "unsigned" "strong")
  pike '(;; this_program isn't really a keyword, but it's practically
	 ;; used as a builtin type.
	 "array" "float" "function" "int" "mapping" "mixed" "multiset"
	 "object" "program" "string" "this_program" "void"))

(c-lang-defconst c-primitive-type-key
  ;; An adorned regexp that matches `c-primitive-type-kwds'.
  t (c-make-keywords-re t (c-lang-const c-primitive-type-kwds)))
(c-lang-defvar c-primitive-type-key (c-lang-const c-primitive-type-key))

(c-lang-defconst c-primitive-type-prefix-kwds
  "Keywords that might act as prefixes for primitive types.  Assumed to
be a subset of `c-primitive-type-kwds'."
  t       nil
  (c c++) '("long" "short" "signed" "unsigned")
  idl     '("long" "unsigned"
	    ;; In CORBA PSDL:
	    "strong"))

(c-lang-defconst c-typedef-kwds
  "Prefix keyword\(s\) like \"typedef\" which make a type declaration out
of a variable declaration."
  t        '("typedef")
  (awk idl java) nil)

(c-lang-defconst c-typedef-key
  ;; Adorned regexp matching `c-typedef-kwds'.
  t (c-make-keywords-re t (c-lang-const c-typedef-kwds)))
(c-lang-defvar c-typedef-key (c-lang-const c-typedef-key))

(c-lang-defconst c-type-prefix-kwds
  "Keywords where the following name - if any - is a type name, and
where the keyword together with the symbol works as a type in
declarations.

Note that an alternative if the second part doesn't hold is
`c-type-list-kwds'.  Keywords on this list are typically also present
on one of the `*-decl-kwds' lists."
  t    nil
  c    '("struct" "union" "enum")
  c++  (append '("class" "typename")
	       (c-lang-const c-type-prefix-kwds c)))

(c-lang-defconst c-type-prefix-key
  ;; Adorned regexp matching `c-type-prefix-kwds'.
  t (c-make-keywords-re t (c-lang-const c-type-prefix-kwds)))
(c-lang-defvar c-type-prefix-key (c-lang-const c-type-prefix-key))

(c-lang-defconst c-type-modifier-kwds
  "Type modifier keywords.  These can occur almost anywhere in types
but they don't build a type of themselves.  Unlike the keywords on
`c-primitive-type-kwds', they are fontified with the keyword face and
not the type face."
  t    nil
  c    '("const" "restrict" "volatile")
  c++  '("const" "volatile" "throw")
  objc '("const" "volatile"))

(c-lang-defconst c-opt-type-modifier-key
  ;; Adorned regexp matching `c-type-modifier-kwds', or nil in
  ;; languages without such keywords.
  t (and (c-lang-const c-type-modifier-kwds)
	 (c-make-keywords-re t (c-lang-const c-type-modifier-kwds))))
(c-lang-defvar c-opt-type-modifier-key (c-lang-const c-opt-type-modifier-key))

(c-lang-defconst c-opt-type-component-key
  ;; An adorned regexp that matches `c-primitive-type-prefix-kwds' and
  ;; `c-type-modifier-kwds', or nil in languages without any of them.
  t (and (or (c-lang-const c-primitive-type-prefix-kwds)
	     (c-lang-const c-type-modifier-kwds))
	 (c-make-keywords-re t
	   (append (c-lang-const c-primitive-type-prefix-kwds)
		   (c-lang-const c-type-modifier-kwds)))))
(c-lang-defvar c-opt-type-component-key
  (c-lang-const c-opt-type-component-key))

(c-lang-defconst c-type-start-kwds
  ;; All keywords that can start a type (i.e. are either a type prefix
  ;; or a complete type).
  t (delete-duplicates (append (c-lang-const c-primitive-type-kwds)
			       (c-lang-const c-type-prefix-kwds)
			       (c-lang-const c-type-modifier-kwds))
		       :test 'string-equal))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Note that presence on this list does not automatically treat the
following identifier as a type; the keyword must also be present on
`c-type-prefix-kwds' or `c-type-list-kwds' to accomplish that."
  t    nil
  c    '("struct" "union")
  c++  '("class" "struct" "union")
  objc '("struct" "union"
	 "@interface" "@implementation" "@protocol")
  java '("class" "@interface" "interface")
  idl  '("component" "eventtype" "exception" "home" "interface" "struct"
	 "union" "valuetype"
	 ;; In CORBA PSDL:
	 "storagehome" "storagetype"
	 ;; In CORBA CIDL:
	 "catalog" "executor" "manages" "segment")
  pike '("class"))

(c-lang-defconst c-class-key
  ;; Regexp matching the start of a class.
  t (c-make-keywords-re t (c-lang-const c-class-decl-kwds)))
(c-lang-defvar c-class-key (c-lang-const c-class-key))

(c-lang-defconst c-brace-list-decl-kwds
  "Keywords introducing declarations where the following block (if
any) is a brace list.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t    '("enum")
  (awk) nil)

(c-lang-defconst c-brace-list-key
  ;; Regexp matching the start of declarations where the following
  ;; block is a brace list.
  t (c-make-keywords-re t (c-lang-const c-brace-list-decl-kwds)))
(c-lang-defvar c-brace-list-key (c-lang-const c-brace-list-key))

(c-lang-defconst c-other-block-decl-kwds
  "Keywords where the following block (if any) contains another
declaration level that should not be considered a class.  For every
keyword here, CC Mode will add a set of special syntactic symbols for
those blocks.  E.g. if the keyword is \"foo\" then there will be
`foo-open', `foo-close', and `infoo' symbols.

The intention is that this category should be used for block
constructs that aren't related to object orientation concepts like
classes (which thus also include e.g. interfaces, templates,
contracts, structs, etc).  The more pragmatic distinction is that
while most want some indentation inside classes, it's fairly common
that they don't want it in some of these constructs, so it should be
simple to configure that differently from classes.  See also
`c-class-decl-kwds'.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t   nil
  (c objc) '("extern")
  c++ '("namespace" "extern")
  idl '("module"
	;; In CORBA CIDL:
	"composition"))

(c-lang-defconst c-other-decl-block-key
  ;; Regexp matching the start of blocks besides classes that contain
  ;; another declaration level.
  t (c-make-keywords-re t (c-lang-const c-other-block-decl-kwds)))
(c-lang-defvar c-other-decl-block-key (c-lang-const c-other-decl-block-key))

(c-lang-defvar c-other-decl-block-key-in-symbols-alist
  (mapcar
   (lambda (elt)
     (cons elt
	   (if (string= elt "extern")
	       'inextern-lang
	     (intern (concat "in" elt)))))
   (c-lang-const c-other-block-decl-kwds))
  "Alist associating keywords in c-other-decl-block-decl-kwds with
their matching \"in\" syntactic symbols.")

(c-lang-defconst c-typedef-decl-kwds
  "Keywords introducing declarations where the identifier(s) being
declared are types.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is a type that's being defined in "class Foo
  ;; {...}").
  t    (append (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds))
  ;; Languages that have a "typedef" construct.
  (c c++ objc idl pike) (append (c-lang-const c-typedef-decl-kwds)
				'("typedef"))
  ;; Unlike most other languages, exception names are not handled as
  ;; types in IDL since they only can occur in "raises" specs.
  idl  (delete "exception" (append (c-lang-const c-typedef-decl-kwds) nil)))

(c-lang-defconst c-typedef-decl-key
  t  (c-make-keywords-re t (c-lang-const c-typedef-decl-kwds)))
(c-lang-defvar c-typedef-decl-key (c-lang-const c-typedef-decl-key))

(c-lang-defconst c-typeless-decl-kwds
  "Keywords introducing declarations where the \(first) identifier
\(declarator) follows directly after the keyword, without any type.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is the identifier being defined in "class Foo
  ;; {...}").
  t    (append (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds))
  ;; Note: "manages" for CORBA CIDL clashes with its presence on
  ;; `c-type-list-kwds' for IDL.
  idl  (append (c-lang-const c-typeless-decl-kwds)
	       '("factory" "finder" "native"
		 ;; In CORBA PSDL:
		 "key" "stores"
		 ;; In CORBA CIDL:
		 "facet"))
  pike (append (c-lang-const c-class-decl-kwds)
	       '("constant")))

(c-lang-defconst c-modifier-kwds
  "Keywords that can prefix normal declarations of identifiers
\(and typically act as flags).  Things like argument declarations
inside function headers are also considered declarations in this
sense.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t    nil
  (c c++) '("auto" "extern" "inline" "register" "static")
  c++  (append '("explicit" "friend" "mutable" "template" "using" "virtual")
	       (c-lang-const c-modifier-kwds))
  objc '("auto" "bycopy" "byref" "extern" "in" "inout" "oneway" "out" "static")
  ;; FIXME: Some of those below ought to be on `c-other-decl-kwds' instead.
  idl  '("abstract" "attribute" "const" "consumes" "custom" "emits" "import"
	 "in" "inout" "local" "multiple" "oneway" "out" "private" "provides"
	 "public" "publishes" "readonly" "typeid" "typeprefix" "uses"
	 ;; In CORBA PSDL:
	 "primary" "state"
	 ;; In CORBA CIDL:
	 "bindsTo" "delegatesTo" "implements" "proxy" "storedOn")
  ;; Note: "const" is not used in Java, but it's still a reserved keyword.
  java '("abstract" "const" "final" "native" "private" "protected" "public"
	 "static" "strictfp" "synchronized" "transient" "volatile")
  pike '("final" "inline" "local" "nomask" "optional" "private" "protected"
	 "public" "static" "variant"))

(c-lang-defconst c-other-decl-kwds
  "Keywords that can start or prefix any declaration level construct,
besides those on `c-class-decl-kwds', `c-brace-list-decl-kwds',
`c-other-block-decl-kwds', `c-typedef-decl-kwds',
`c-typeless-decl-kwds' and `c-modifier-kwds'.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t       nil
  objc    '("@class" "@end" "@defs")
  java    '("import" "package")
  pike    '("import" "inherit"))

(c-lang-defconst c-decl-start-kwds
  "Keywords that always start declarations, wherever they occur.
This can be used for declarations that aren't recognized by the normal
combination of `c-decl-prefix-re' and `c-decl-start-re'."
  t    nil
  ;; Classes can be declared anywhere in a Pike expression.
  pike '("class"))

(c-lang-defconst c-decl-hangon-kwds
  "Keywords that can occur anywhere in a declaration level construct.
This is used for self-contained things that can be tacked on anywhere
on a declaration and that should be ignored to be able to recognize it
correctly.  Typical cases are compiler extensions like
\"__attribute__\" or \"__declspec\":

    __declspec(noreturn) void foo();
    class __declspec(dllexport) classname {...};
    void foo() __attribute__((noreturn));

Note that unrecognized plain symbols are skipped anyway if they occur
before the type, so such things are not necessary to mention here.
Mentioning them here is necessary only if they can occur in other
places, or if they are followed by a construct that must be skipped
over \(like the parens in the \"__attribute__\" and \"__declspec\"
examples above).  In the last case, they alse need to be present on
one of `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds'."
  ;; NB: These are currently not recognized in all parts of a
  ;; declaration.  Specifically, they aren't recognized in the middle
  ;; of multi-token types, inside declarators, and between the
  ;; identifier and the arglist paren of a function declaration.
  ;;
  ;; FIXME: This ought to be user customizable since compiler stuff
  ;; like this usually is wrapped in project specific macros.  (It'd
  ;; of course be even better if we could cope without knowing this.)
  t nil
  (c c++) '(;; GCC extension.
	    "__attribute__"
	    ;; MSVC extension.
	    "__declspec"))

(c-lang-defconst c-decl-hangon-key
  ;; Adorned regexp matching `c-decl-hangon-kwds'.
  t (c-make-keywords-re t (c-lang-const c-decl-hangon-kwds)))
(c-lang-defvar c-decl-hangon-key (c-lang-const c-decl-hangon-key))

(c-lang-defconst c-prefix-spec-kwds
  ;; All keywords that can occur in the preamble of a declaration.
  ;; They typically occur before the type, but they are also matched
  ;; after presumptive types since we often can't be sure that
  ;; something is a type or just some sort of macro in front of the
  ;; declaration.  They might be ambiguous with types or type
  ;; prefixes.
  t (delete-duplicates (append (c-lang-const c-class-decl-kwds)
			       (c-lang-const c-brace-list-decl-kwds)
			       (c-lang-const c-other-block-decl-kwds)
			       (c-lang-const c-typedef-decl-kwds)
			       (c-lang-const c-typeless-decl-kwds)
			       (c-lang-const c-modifier-kwds)
			       (c-lang-const c-other-decl-kwds)
			       (c-lang-const c-decl-start-kwds)
			       (c-lang-const c-decl-hangon-kwds))
		       :test 'string-equal))

(c-lang-defconst c-prefix-spec-kwds-re
  ;; Adorned regexp of `c-prefix-spec-kwds'.
  t (c-make-keywords-re t (c-lang-const c-prefix-spec-kwds)))

(c-lang-defvar c-prefix-spec-kwds-re (c-lang-const c-prefix-spec-kwds-re))

(c-lang-defconst c-specifier-key
  ;; Adorned regexp of the keywords in `c-prefix-spec-kwds' that aren't
  ;; ambiguous with types or type prefixes.  These are the keywords (like
  ;; extern, namespace, but NOT template) that can modify a declaration.
  t (c-make-keywords-re t
      (set-difference (c-lang-const c-prefix-spec-kwds)
		      (append (c-lang-const c-type-start-kwds)
			      (c-lang-const c-<>-arglist-kwds))
		      :test 'string-equal)))
(c-lang-defvar c-specifier-key (c-lang-const c-specifier-key))

(c-lang-defconst c-postfix-spec-kwds
  ;; Keywords that can occur after argument list of a function header
  ;; declaration, i.e. in the "K&R region".
  t (append (c-lang-const c-postfix-decl-spec-kwds)
	    (c-lang-const c-decl-hangon-kwds)))

(c-lang-defconst c-not-decl-init-keywords
  ;; Adorned regexp matching all keywords that can't appear at the
  ;; start of a declaration.
  t (c-make-keywords-re t
      (set-difference (c-lang-const c-keywords)
		      (append (c-lang-const c-type-start-kwds)
			      (c-lang-const c-prefix-spec-kwds))
		      :test 'string-equal)))
(c-lang-defvar c-not-decl-init-keywords
  (c-lang-const c-not-decl-init-keywords))

(c-lang-defconst c-not-primitive-type-keywords
  "List of all keywords apart from primitive types (like \"int\")."
  t (set-difference (c-lang-const c-keywords)
		    (c-lang-const c-primitive-type-kwds)
		    :test 'string-equal)
  ;; The "more" for C++ is the QT keyword (as in "more slots:").
  ;; This variable is intended for use in c-beginning-of-statement-1.
  c++ (append (c-lang-const c-not-primitive-type-keywords) '("more")))

(c-lang-defconst c-not-primitive-type-keywords-regexp
  t (c-make-keywords-re t
      (c-lang-const c-not-primitive-type-keywords)))
(c-lang-defvar c-not-primitive-type-keywords-regexp
  (c-lang-const c-not-primitive-type-keywords-regexp))

(c-lang-defconst c-protection-kwds
  "Access protection label keywords in classes."
  t    nil
  c++  '("private" "protected" "public")
  objc '("@private" "@protected" "@public"))

(c-lang-defconst c-block-decls-with-vars
  "Keywords introducing declarations that can contain a block which
might be followed by variable declarations, e.g. like \"foo\" in
\"class Foo { ... } foo;\".  So if there is a block in a declaration
like that, it ends with the following ';' and not right away.

The keywords on list are assumed to also be present on one of the
`*-decl-kwds' lists."
  t        nil
  (c objc) '("struct" "union" "enum" "typedef")
  c++      '("class" "struct" "union" "enum" "typedef"))

(c-lang-defconst c-opt-block-decls-with-vars-key
  ;; Regexp matching the `c-block-decls-with-vars' keywords, or nil in
  ;; languages without such constructs.
  t (and (c-lang-const c-block-decls-with-vars)
	 (c-make-keywords-re t (c-lang-const c-block-decls-with-vars))))
(c-lang-defvar c-opt-block-decls-with-vars-key
  (c-lang-const c-opt-block-decls-with-vars-key))

(c-lang-defconst c-postfix-decl-spec-kwds
  "Keywords introducing extra declaration specifiers in the region
between the header and the body \(i.e. the \"K&R-region\") in
declarations."
  t    nil
  java '("extends" "implements" "throws")
  idl  '("context" "getraises" "manages" "primarykey" "raises" "setraises"
	 "supports"
	 ;; In CORBA PSDL:
	 "as" "const" "implements" "of" "ref"))

(c-lang-defconst c-nonsymbol-sexp-kwds
  "Keywords that may be followed by a nonsymbol sexp before whatever
construct it's part of continues."
  t    nil
  (c c++ objc) '("extern"))

(c-lang-defconst c-type-list-kwds
  "Keywords that may be followed by a comma separated list of type
identifiers, where each optionally can be prefixed by keywords.  (Can
also be used for the special case when the list can contain only one
element.)

Assumed to be mutually exclusive with `c-ref-list-kwds'.  There's no
reason to put keywords on this list if they are on `c-type-prefix-kwds'.
There's also no reason to add keywords that prefixes a normal
declaration consisting of a type followed by a declarator (list), so
the keywords on `c-modifier-kwds' should normally not be listed here
either.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  t    nil
  c++  '("operator")
  objc '("@class")
  java '("import" "new" "extends" "super" "implements" "throws")
  idl  '("manages" "native" "primarykey" "supports"
	 ;; In CORBA PSDL:
	 "as" "implements" "of" "scope")
  pike '("inherit"))

(c-lang-defconst c-ref-list-kwds
  "Keywords that may be followed by a comma separated list of
reference (i.e. namespace/scope/module) identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)  Assumed to
be mutually exclusive with `c-type-list-kwds'.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  t    nil
  c++  '("namespace")
  java '("package")
  idl  '("import" "module"
	 ;; In CORBA CIDL:
	 "composition")
  pike '("import"))

(c-lang-defconst c-colon-type-list-kwds
  "Keywords that may be followed (not necessarily directly) by a colon
and then a comma separated list of type identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)"
  t    nil
  c++  '("class" "struct")
  idl  '("component" "eventtype" "home" "interface" "valuetype"
	 ;; In CORBA PSDL:
	 "storagehome" "storagetype"))

(c-lang-defconst c-colon-type-list-re
  "Regexp matched after the keywords in `c-colon-type-list-kwds' to skip
forward to the colon.  The end of the match is assumed to be directly
after the colon, so the regexp should end with \":\".  Must be a
regexp if `c-colon-type-list-kwds' isn't nil."
  t (if (c-lang-const c-colon-type-list-kwds)
	;; Disallow various common punctuation chars that can't come
	;; before the ":" that starts the inherit list after "class"
	;; or "struct" in C++.  (Also used as default for other
	;; languages.)
	"[^\]\[{}();,/#=:]*:"))
(c-lang-defvar c-colon-type-list-re (c-lang-const c-colon-type-list-re))

(c-lang-defconst c-paren-nontype-kwds
  "Keywords that may be followed by a parenthesis expression that doesn't
contain type identifiers."
  t       nil
  (c c++) '(;; GCC extension.
	    "__attribute__"
	    ;; MSVC extension.
	    "__declspec"))

(c-lang-defconst c-paren-type-kwds
  "Keywords that may be followed by a parenthesis expression containing
type identifiers separated by arbitrary tokens."
  t    nil
  c++  '("throw")
  objc '("@defs")
  idl  '("switch")
  pike '("array" "function" "int" "mapping" "multiset" "object" "program"))

(c-lang-defconst c-paren-any-kwds
  t (delete-duplicates (append (c-lang-const c-paren-nontype-kwds)
			       (c-lang-const c-paren-type-kwds))
		       :test 'string-equal))

(c-lang-defconst c-<>-type-kwds
  "Keywords that may be followed by an angle bracket expression
containing type identifiers separated by \",\".  The difference from
`c-<>-arglist-kwds' is that unknown names are taken to be types and
not other identifiers.  `c-recognize-<>-arglists' is assumed to be set
if this isn't nil."
  t    nil
  objc '("id")
  idl  '("sequence"
	 ;; In CORBA PSDL:
	 "ref"))

(c-lang-defconst c-<>-arglist-kwds
  "Keywords that can be followed by a C++ style template arglist; see
`c-recognize-<>-arglists' for details.  That language constant is
assumed to be set if this isn't nil."
  t    nil
  c++  '("template")
  idl  '("fixed" "string" "wstring"))

(c-lang-defconst c-<>-sexp-kwds
  ;; All keywords that can be followed by an angle bracket sexp.
  t (delete-duplicates (append (c-lang-const c-<>-type-kwds)
			       (c-lang-const c-<>-arglist-kwds))
		       :test 'string-equal))

(c-lang-defconst c-opt-<>-sexp-key
  ;; Adorned regexp matching keywords that can be followed by an angle
  ;; bracket sexp.  Always set when `c-recognize-<>-arglists' is.
  t (if (c-lang-const c-recognize-<>-arglists)
	(c-make-keywords-re t (c-lang-const c-<>-sexp-kwds))))
(c-lang-defvar c-opt-<>-sexp-key (c-lang-const c-opt-<>-sexp-key))

(c-lang-defconst c-brace-id-list-kwds
  "Keywords that may be followed by a brace block containing a comma
separated list of identifier definitions, i.e. like the list of
identifiers that follows the type in a normal declaration."
  t (c-lang-const c-brace-list-decl-kwds))

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  t    '("do" "else")
  c++  '("do" "else" "try")
  objc '("do" "else" "@finally" "@try")
  java '("do" "else" "finally" "try")
  idl  nil)

(c-lang-defconst c-block-stmt-1-key
  ;; Regexp matching the start of any statement followed directly by a
  ;; substatement (doesn't match a bare block, however).
  t (c-make-keywords-re t (c-lang-const c-block-stmt-1-kwds)))
(c-lang-defvar c-block-stmt-1-key (c-lang-const c-block-stmt-1-key))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  t    '("for" "if" "switch" "while")
  c++  '("for" "if" "switch" "while" "catch")
  objc '("for" "if" "switch" "while" "@catch" "@synchronized")
  java '("for" "if" "switch" "while" "catch" "synchronized")
  idl  nil
  pike '("for" "if" "switch" "while" "foreach")
  awk  '("for" "if" "while"))

(c-lang-defconst c-block-stmt-2-key
  ;; Regexp matching the start of any statement followed by a paren sexp
  ;; and then by a substatement.
  t (c-make-keywords-re t (c-lang-const c-block-stmt-2-kwds)))
(c-lang-defvar c-block-stmt-2-key (c-lang-const c-block-stmt-2-key))

(c-lang-defconst c-block-stmt-kwds
  ;; Union of `c-block-stmt-1-kwds' and `c-block-stmt-2-kwds'.
  t (delete-duplicates (append (c-lang-const c-block-stmt-1-kwds)
			       (c-lang-const c-block-stmt-2-kwds))
		       :test 'string-equal))

(c-lang-defconst c-opt-block-stmt-key
  ;; Regexp matching the start of any statement that has a
  ;; substatement (except a bare block).  Nil in languages that
  ;; don't have such constructs.
  t (if (or (c-lang-const c-block-stmt-1-kwds)
	    (c-lang-const c-block-stmt-2-kwds))
	(c-make-keywords-re t
	  (append (c-lang-const c-block-stmt-1-kwds)
		  (c-lang-const c-block-stmt-2-kwds)))))
(c-lang-defvar c-opt-block-stmt-key (c-lang-const c-opt-block-stmt-key))

(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  t    '("break" "continue" "goto" "return")
  objc '("break" "continue" "goto" "return" "@throw")
  ;; Note: `goto' is not valid in Java, but the keyword is still reserved.
  java '("break" "continue" "goto" "return" "throw")
  idl  nil
  pike '("break" "continue" "return")
  awk  '(;; Not sure about "delete", "exit", "getline", etc. ; ACM 2002/5/30
	 "break" "continue" "return" "delete" "exit" "getline" "next"
	 "nextfile" "print" "printf"))

(c-lang-defconst c-simple-stmt-key
  ;; Adorned regexp matching `c-simple-stmt-kwds'.
  t (c-make-keywords-re t (c-lang-const c-simple-stmt-kwds)))
(c-lang-defvar c-simple-stmt-key (c-lang-const c-simple-stmt-key))

(c-lang-defconst c-paren-stmt-kwds
  "Statement keywords followed by a parenthesis expression that
nevertheless contains a list separated with ';' and not ','."
  t    '("for")
  idl  nil)

(c-lang-defconst c-paren-stmt-key
  ;; Adorned regexp matching `c-paren-stmt-kwds'.
  t (c-make-keywords-re t (c-lang-const c-paren-stmt-kwds)))
(c-lang-defvar c-paren-stmt-key (c-lang-const c-paren-stmt-key))

(c-lang-defconst c-asm-stmt-kwds
  "Statement keywords followed by an assembler expression."
  t nil
  (c c++) '("asm" "__asm__")) ;; Not standard, but common.

(c-lang-defconst c-opt-asm-stmt-key
  ;; Regexp matching the start of an assembler statement.  Nil in
  ;; languages that don't support that.
  t (if (c-lang-const c-asm-stmt-kwds)
	(c-make-keywords-re t (c-lang-const c-asm-stmt-kwds))))
(c-lang-defvar c-opt-asm-stmt-key (c-lang-const c-opt-asm-stmt-key))

(c-lang-defconst c-case-kwds
  "The keyword\(s) which introduce a \"case\" like construct.
This construct is \"<keyword> <expression> :\"."
  t '("case")
  awk nil)

(c-lang-defconst c-case-kwds-regexp
  ;; Adorned regexp matching any "case"-like keyword.
  t (c-make-keywords-re t (c-lang-const c-case-kwds)))
(c-lang-defvar c-case-kwds-regexp (c-lang-const c-case-kwds-regexp))

(c-lang-defconst c-label-kwds
  "Keywords introducing colon terminated labels in blocks."
  t '("case" "default"))

(c-lang-defconst c-label-kwds-regexp
  ;; Adorned regexp matching any keyword that introduces a label.
  t (c-make-keywords-re t (c-lang-const c-label-kwds)))
(c-lang-defvar c-label-kwds-regexp (c-lang-const c-label-kwds-regexp))

(c-lang-defconst c-before-label-kwds
  "Keywords that might be followed by a label identifier."
  t    '("goto")
  (java pike) (append '("break" "continue")
		      (c-lang-const c-before-label-kwds))
  idl  nil
  awk  nil)

(c-lang-defconst c-constant-kwds
  "Keywords for constants."
  t       nil
  (c c++) '("NULL" ;; Not a keyword, but practically works as one.
	    "false" "true")		; Defined in C99.
  objc    '("nil" "Nil" "YES" "NO" "NS_DURING" "NS_HANDLER" "NS_ENDHANDLER")
  idl     '("TRUE" "FALSE")
  java    '("true" "false" "null") ; technically "literals", not keywords
  pike    '("UNDEFINED")) ;; Not a keyword, but practically works as one.

(c-lang-defconst c-primary-expr-kwds
  "Keywords besides constants and operators that start primary expressions."
  t    nil
  c++  '("operator" "this")
  objc '("super" "self")
  java '("this")
  pike '("this")) ;; Not really a keyword, but practically works as one.

(c-lang-defconst c-expr-kwds
  ;; Keywords that can occur anywhere in expressions.  Built from
  ;; `c-primary-expr-kwds' and all keyword operators in `c-operators'.
  t (delete-duplicates
     (append (c-lang-const c-primary-expr-kwds)
	     (c-filter-ops (c-lang-const c-operator-list)
			   t
			   "\\`\\(\\w\\|\\s_\\)+\\'"))
     :test 'string-equal))

(c-lang-defconst c-lambda-kwds
  "Keywords that start lambda constructs, i.e. function definitions in
expressions."
  t    nil
  pike '("lambda"))

(c-lang-defconst c-inexpr-block-kwds
  "Keywords that start constructs followed by statement blocks which can
be used in expressions \(the gcc extension for this in C and C++ is
handled separately by `c-recognize-paren-inexpr-blocks')."
  t    nil
  pike '("catch" "gauge"))

(c-lang-defconst c-inexpr-class-kwds
  "Keywords that can start classes inside expressions."
  t    nil
  java '("new")
  pike '("class"))

(c-lang-defconst c-inexpr-brace-list-kwds
  "Keywords that can start brace list blocks inside expressions.
Note that Java specific rules are currently applied to tell this from
`c-inexpr-class-kwds'."
  t    nil
  java '("new"))

(c-lang-defconst c-opt-inexpr-brace-list-key
  ;; Regexp matching the start of a brace list in an expression, or
  ;; nil in languages that don't have such things.  This should not
  ;; match brace lists recognized through `c-special-brace-lists'.
  t (and (c-lang-const c-inexpr-brace-list-kwds)
	 (c-make-keywords-re t (c-lang-const c-inexpr-brace-list-kwds))))
(c-lang-defvar c-opt-inexpr-brace-list-key
  (c-lang-const c-opt-inexpr-brace-list-key))

(c-lang-defconst c-decl-block-key
  ;; Regexp matching keywords in any construct that contain another
  ;; declaration level, i.e. that isn't followed by a function block
  ;; or brace list.  When the first submatch matches, it's an
  ;; unambiguous construct, otherwise it's an ambiguous match that
  ;; might also be the return type of a function declaration.
  t (let* ((decl-kwds (append (c-lang-const c-class-decl-kwds)
			      (c-lang-const c-other-block-decl-kwds)
			      (c-lang-const c-inexpr-class-kwds)))
	   (unambiguous (set-difference decl-kwds
					(c-lang-const c-type-start-kwds)
					:test 'string-equal))
	   (ambiguous (intersection decl-kwds
				    (c-lang-const c-type-start-kwds)
				    :test 'string-equal)))
      (if ambiguous
	  (concat (c-make-keywords-re t unambiguous)
		  "\\|"
		  (c-make-keywords-re t ambiguous))
	(c-make-keywords-re t unambiguous))))
(c-lang-defvar c-decl-block-key (c-lang-const c-decl-block-key))

(c-lang-defconst c-bitfield-kwds
  "Keywords that can introduce bitfields."
  t nil
  (c c++ objc) '("char" "int" "long" "signed" "unsigned"))

(c-lang-defconst c-opt-bitfield-key
  ;; Regexp matching the start of a bitfield (not uniquely), or nil in
  ;; languages without bitfield support.
  t       nil
  (c c++) (c-make-keywords-re t (c-lang-const c-bitfield-kwds)))
(c-lang-defvar c-opt-bitfield-key (c-lang-const c-opt-bitfield-key))

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  t    nil
  idl  '("truncatable"
	 ;; In CORBA CIDL: (These are declaration keywords that never
	 ;; can start a declaration.)
	 "entity" "process" "service" "session" "storage"))


;;; Constants built from keywords.

;; Note: No `*-kwds' language constants may be defined below this point.

(eval-and-compile
  (defconst c-kwds-lang-consts
    ;; List of all the language constants that contain keyword lists.
    (let (list)
      (mapatoms (lambda (sym)
		  (when (and (boundp sym)
			     (string-match "-kwds\\'" (symbol-name sym)))
		    ;; Make the list of globally interned symbols
		    ;; instead of ones interned in `c-lang-constants'.
		    (setq list (cons (intern (symbol-name sym)) list))))
		c-lang-constants)
      list)))

(c-lang-defconst c-keywords
  ;; All keywords as a list.
  t (delete-duplicates
     (c-lang-defconst-eval-immediately
      `(append ,@(mapcar (lambda (kwds-lang-const)
			   `(c-lang-const ,kwds-lang-const))
			 c-kwds-lang-consts)
	       nil))
     :test 'string-equal))

(c-lang-defconst c-keywords-regexp
  ;; All keywords as an adorned regexp.
  t (c-make-keywords-re t (c-lang-const c-keywords)))
(c-lang-defvar c-keywords-regexp (c-lang-const c-keywords-regexp))

(c-lang-defconst c-keyword-member-alist
  ;; An alist with all the keywords in the cars.  The cdr for each
  ;; keyword is a list of the symbols for the `*-kwds' lists that
  ;; contains it.
  t (let ((kwd-list-alist
	   (c-lang-defconst-eval-immediately
	    `(list ,@(mapcar (lambda (kwds-lang-const)
			       `(cons ',kwds-lang-const
				      (c-lang-const ,kwds-lang-const)))
			     c-kwds-lang-consts))))
	  lang-const kwd-list kwd
	  result-alist elem)
      (while kwd-list-alist
	(setq lang-const (caar kwd-list-alist)
	      kwd-list (cdar kwd-list-alist)
	      kwd-list-alist (cdr kwd-list-alist))
	(while kwd-list
	  (setq kwd (car kwd-list)
		kwd-list (cdr kwd-list))
	  (unless (setq elem (assoc kwd result-alist))
	    (setq result-alist (cons (setq elem (list kwd)) result-alist)))
	  (unless (memq lang-const (cdr elem))
	    (setcdr elem (cons lang-const (cdr elem))))))
      result-alist))

(c-lang-defvar c-keywords-obarray
  ;; An obarray containing all keywords as symbols.  The property list
  ;; of each symbol has a non-nil entry for the specific `*-kwds'
  ;; lists it's a member of.
  ;;
  ;; E.g. to see whether the string str contains a keyword on
  ;; `c-class-decl-kwds', one can do like this:
  ;;     (get (intern-soft str c-keyword-obarray) 'c-class-decl-kwds)
  ;; Which preferably is written using the associated functions in
  ;; cc-engine:
  ;;     (c-keyword-member (c-keyword-sym str) 'c-class-decl-kwds)

  ;; The obarray is not stored directly as a language constant since
  ;; the printed representation for obarrays used in .elc files isn't
  ;; complete.

  (let* ((alist (c-lang-const c-keyword-member-alist))
	 kwd lang-const-list
	 (obarray (make-vector (* (length alist) 2) 0)))
    (while alist
      (setq kwd (caar alist)
	    lang-const-list (cdar alist)
	    alist (cdr alist))
      (setplist (intern kwd obarray)
		;; Emacs has an odd bug that causes `mapcan' to fail
		;; with unintelligible errors.  (XEmacs works.)
		;;(mapcan (lambda (lang-const)
		;;	      (list lang-const t))
		;;	    lang-const-list)
		(apply 'nconc (mapcar (lambda (lang-const)
					(list lang-const t))
				      lang-const-list))))
    obarray))

(c-lang-defconst c-regular-keywords-regexp
  ;; Adorned regexp matching all keywords that should be fontified
  ;; with the keywords face.  I.e. that aren't types or constants.
  t (c-make-keywords-re t
      (set-difference (c-lang-const c-keywords)
		      (append (c-lang-const c-primitive-type-kwds)
			      (c-lang-const c-constant-kwds))
		      :test 'string-equal)))
(c-lang-defvar c-regular-keywords-regexp
  (c-lang-const c-regular-keywords-regexp))

(c-lang-defconst c-primary-expr-regexp
  ;; Regexp matching the start of any primary expression, i.e. any
  ;; literal, symbol, prefix operator, and '('.  It doesn't need to
  ;; exclude keywords; they are excluded afterwards unless the second
  ;; submatch matches. If the first but not the second submatch
  ;; matches then it is an ambiguous primary expression; it could also
  ;; be a match of e.g. an infix operator. (The case with ambiguous
  ;; keyword operators isn't handled.)

  t (let* ((prefix-ops
	    (c-filter-ops (c-lang-const c-operators)
			  '(prefix)
			  (lambda (op)
			    ;; Filter out the special case prefix
			    ;; operators that are close parens.
			    (not (string-match "\\s)" op)))))

	   (nonkeyword-prefix-ops
	    (c-filter-ops prefix-ops
			  t
			  "\\`\\(\\s.\\|\\s(\\|\\s)\\)+\\'"))

	   (in-or-postfix-ops
	    (c-filter-ops (c-lang-const c-operators)
			  '(postfix
			    postfix-if-paren
			    left-assoc
			    right-assoc
			    right-assoc-sequence)
			  t))

	   (unambiguous-prefix-ops (set-difference nonkeyword-prefix-ops
						   in-or-postfix-ops
						   :test 'string-equal))
	   (ambiguous-prefix-ops (intersection nonkeyword-prefix-ops
					       in-or-postfix-ops
					       :test 'string-equal)))

      (concat
       "\\("
       ;; Take out all symbol class operators from `prefix-ops' and make the
       ;; first submatch from them together with `c-primary-expr-kwds'.
       (c-make-keywords-re t
	 (append (c-lang-const c-primary-expr-kwds)
		 (set-difference prefix-ops nonkeyword-prefix-ops
				 :test 'string-equal)))

       "\\|"
       ;; Match all ambiguous operators.
       (c-make-keywords-re nil
	 (intersection nonkeyword-prefix-ops in-or-postfix-ops
		       :test 'string-equal))
       "\\)"

       "\\|"
       ;; Now match all other symbols.
       (c-lang-const c-symbol-start)

       "\\|"
       ;; The chars that can start integer and floating point
       ;; constants.
       "\\.?[0-9]"

       "\\|"
       ;; The unambiguous operators from `prefix-ops'.
       (c-make-keywords-re nil
	 (set-difference nonkeyword-prefix-ops in-or-postfix-ops
			 :test 'string-equal))

       "\\|"
       ;; Match string and character literals.
       "\\s\""
       (if (memq 'gen-string-delim c-emacs-features)
	   "\\|\\s|"
	 ""))))
(c-lang-defvar c-primary-expr-regexp (c-lang-const c-primary-expr-regexp))


;;; Additional constants for parser-level constructs.

(c-lang-defconst c-decl-prefix-re
  "Regexp matching something that might precede a declaration, cast or
label, such as the last token of a preceding statement or declaration.
This is used in the common situation where a declaration or cast
doesn't start with any specific token that can be searched for.

The regexp should not match bob; that is done implicitly.  It can't
require a match longer than one token.  The end of the token is taken
to be at the end of the first submatch, which is assumed to always
match.  It's undefined whether identifier syntax (see
`c-identifier-syntax-table') is in effect or not.  This regexp is
assumed to be a superset of `c-label-prefix-re' if
`c-recognize-colon-labels' is set.

Besides this, `c-decl-start-kwds' is used to find declarations.

Note: This variable together with `c-decl-start-re' and
`c-decl-start-kwds' is only used to detect \"likely\"
declaration/cast/label starts.  I.e. they might produce more matches
but should not miss anything (or else it's necessary to use text
properties - see the next note).  Wherever they match, the following
construct is analyzed to see if it indeed is a declaration, cast or
label.  That analysis is not cheap, so it's important that not too
many false matches are triggered.

Note: If a declaration/cast/label start can't be detected with this
variable, it's necessary to use the `c-type' text property with the
value `c-decl-end' on the last char of the last token preceding the
declaration.  See the comment blurb at the start of cc-engine.el for
more info."

  ;; We match a sequence of characters to skip over things like \"};\"
  ;; more quickly.  We match ")" in C for K&R region declarations, and
  ;; in all languages except Java for when a cpp macro definition
  ;; begins with a declaration.
  t "\\([\{\}\(\);,]+\\)"
  java "\\([\{\}\(;,<]+\\)"
  ;; Match "<" in C++ to get the first argument in a template arglist.
  ;; In that case there's an additional check in `c-find-decl-spots'
  ;; that it got open paren syntax.
  c++ "\\([\{\}\(\);,<]+\\)"
  ;; Additionally match the protection directives in Objective-C.
  ;; Note that this doesn't cope with the longer directives, which we
  ;; would have to match from start to end since they don't end with
  ;; any easily recognized characters.
  objc (concat "\\([\{\}\(\);,]+\\|"
	       (c-make-keywords-re nil (c-lang-const c-protection-kwds))
	       "\\)")
  ;; Pike is like C but we also match "[" for multiple value
  ;; assignments and type casts.
  pike "\\([\{\}\(\)\[;,]+\\)")
(c-lang-defvar c-decl-prefix-re (c-lang-const c-decl-prefix-re)
  'dont-doc)

(c-lang-defconst c-decl-start-re
  "Regexp matching the start of any declaration, cast or label.
It's used on the token after the one `c-decl-prefix-re' matched.  This
regexp should not try to match those constructs accurately as it's
only used as a sieve to avoid spending more time checking other
constructs."
  t (c-lang-const c-identifier-start))
(c-lang-defvar c-decl-start-re (c-lang-const c-decl-start-re))

(c-lang-defconst c-decl-prefix-or-start-re
  ;; Regexp matching something that might precede or start a
  ;; declaration, cast or label.
  ;;
  ;; If the first submatch matches, it's taken to match the end of a
  ;; token that might precede such a construct, e.g. ';', '}' or '{'.
  ;; It's built from `c-decl-prefix-re'.
  ;;
  ;; If the first submatch did not match, the match of the whole
  ;; regexp is taken to be at the first token in the declaration.
  ;; `c-decl-start-re' is not checked in this case.
  ;;
  ;; Design note: The reason the same regexp is used to match both
  ;; tokens that precede declarations and start them is to avoid an
  ;; extra regexp search from the previous declaration spot in
  ;; `c-find-decl-spots'.  Users of `c-find-decl-spots' also count on
  ;; that it finds all declaration/cast/label starts in approximately
  ;; linear order, so we can't do the searches in two separate passes.
  t (if (c-lang-const c-decl-start-kwds)
	(concat (c-lang-const c-decl-prefix-re)
		"\\|"
		(c-make-keywords-re t (c-lang-const c-decl-start-kwds)))
      (c-lang-const c-decl-prefix-re)))
(c-lang-defvar c-decl-prefix-or-start-re
  (c-lang-const c-decl-prefix-or-start-re)
  'dont-doc)

(c-lang-defconst c-cast-parens
  ;; List containing the paren characters that can open a cast, or nil in
  ;; languages without casts.
  t (c-filter-ops (c-lang-const c-operators)
		  '(prefix)
		  "\\`\\s\(\\'"
		  (lambda (op) (elt op 0))))
(c-lang-defvar c-cast-parens (c-lang-const c-cast-parens))

(c-lang-defconst c-block-prefix-disallowed-chars
  "List of syntactically relevant characters that never can occur before
the open brace in any construct that contains a brace block, e.g. in
the \"class Foo: public Bar\" part of:

    class Foo: public Bar {int x();} a, *b;

If parens can occur, the chars inside those aren't filtered with this
list.

'<' and '>' should be disallowed even if angle bracket arglists can
occur.  That since the search function needs to stop at them anyway to
ensure they are given paren syntax.

This is used to skip backward from the open brace to find the region
in which to look for a construct like \"class\", \"enum\",
\"namespace\" or whatever.  That skipping should be as tight as
possible for good performance."

  ;; Default to all chars that only occurs in nonsymbol tokens outside
  ;; identifiers.
  t (set-difference
     (c-lang-const c-nonsymbol-token-char-list)
     (c-filter-ops (append (c-lang-const c-identifier-ops)
			   (list (cons nil
				       (c-lang-const c-after-id-concat-ops))))
		   t
		   t
		   (lambda (op)
		     (let ((pos 0) res)
		       (while (string-match "\\(\\s.\\|\\s(\\|\\s)\\)"
					    op pos)
			 (setq res (cons (aref op (match-beginning 1)) res)
			       pos (match-end 0)))
		       res))))

  ;; Allow cpp operations (where applicable).
  t (if (c-lang-const c-opt-cpp-prefix)
	(set-difference (c-lang-const c-block-prefix-disallowed-chars)
			'(?#))
      (c-lang-const c-block-prefix-disallowed-chars))

  ;; Allow ':' for inherit list starters.
  (c++ objc idl) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
				 '(?:))

  ;; Allow ',' for multiple inherits.
  (c++ java) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
			     '(?,))

  ;; Allow parentheses for anonymous inner classes in Java and class
  ;; initializer lists in Pike.
  (java pike) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
			      '(?\( ?\)))

  ;; Allow '"' for extern clauses (e.g. extern "C" {...}).
  (c c++ objc) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
			       '(?\" ?')))

(c-lang-defconst c-block-prefix-charset
  ;; `c-block-prefix-disallowed-chars' as an inverted charset suitable
  ;; for `c-syntactic-skip-backward'.
  t (c-make-bare-char-alt (c-lang-const c-block-prefix-disallowed-chars) t))
(c-lang-defvar c-block-prefix-charset (c-lang-const c-block-prefix-charset))

(c-lang-defconst c-type-decl-prefix-key
  "Regexp matching the declarator operators that might precede the
identifier in a declaration, e.g. the \"*\" in \"char *argv\".  This
regexp should match \"(\" if parentheses are valid in declarators.
The end of the first submatch is taken as the end of the operator.
Identifier syntax is in effect when this is matched \(see
`c-identifier-syntax-table')."
  t (if (c-lang-const c-type-modifier-kwds)
	(concat (regexp-opt (c-lang-const c-type-modifier-kwds) t) "\\>")
      ;; Default to a regexp that never matches.
      "\\<\\>")
  ;; Check that there's no "=" afterwards to avoid matching tokens
  ;; like "*=".
  (c objc) (concat "\\("
		   "[*\(]"
		   "\\|"
		   (c-lang-const c-type-decl-prefix-key)
		   "\\)"
		   "\\([^=]\\|$\\)")
  c++  (concat "\\("
	       "[*\(&]"
	       "\\|"
	       (c-lang-const c-type-decl-prefix-key)
	       "\\|"
	       (concat "\\("   ; 3
		       ;; If this matches there's special treatment in
		       ;; `c-font-lock-declarators' and
		       ;; `c-font-lock-declarations' that check for a
		       ;; complete name followed by ":: *".
		       (c-lang-const c-identifier-start)
		       "\\)")
	       "\\)"
	       "\\([^=]\\|$\\)")
  pike "\\(\\*\\)\\([^=]\\|$\\)")
(c-lang-defvar c-type-decl-prefix-key (c-lang-const c-type-decl-prefix-key)
  'dont-doc)

(c-lang-defconst c-type-decl-suffix-key
  "Regexp matching the declarator operators that might follow after the
identifier in a declaration, e.g. the \"[\" in \"char argv[]\".  This
regexp should match \")\" if parentheses are valid in declarators.  If
it matches an open paren of some kind, the type declaration check
continues at the corresponding close paren, otherwise the end of the
first submatch is taken as the end of the operator.  Identifier syntax
is in effect when this is matched (see `c-identifier-syntax-table')."
  ;; Default to a regexp that matches `c-type-modifier-kwds' and a
  ;; function argument list parenthesis.
  t    (if (c-lang-const c-type-modifier-kwds)
	   (concat "\\(\(\\|"
		   (regexp-opt (c-lang-const c-type-modifier-kwds) t) "\\>"
		   "\\)")
	 "\\(\(\\)")
  (c c++ objc) (concat
		"\\("
		"[\)\[\(]"
		(if (c-lang-const c-type-modifier-kwds)
		    (concat
		     "\\|"
		     ;; "throw" in `c-type-modifier-kwds' is followed
		     ;; by a parenthesis list, but no extra measures
		     ;; are necessary to handle that.
		     (regexp-opt (c-lang-const c-type-modifier-kwds) t)
		     "\\>")
		  "")
		"\\)")
  (java idl) "\\([\[\(]\\)")
(c-lang-defvar c-type-decl-suffix-key (c-lang-const c-type-decl-suffix-key)
  'dont-doc)

(c-lang-defconst c-after-suffixed-type-decl-key
  "This regexp is matched after a declarator expression where
`c-type-decl-suffix-key' has matched.  If it matches then the
construct is taken as a declaration.  It's typically used to match the
beginning of a function body or whatever might occur after the
function header in a function declaration or definition.  It's
undefined whether identifier syntax (see `c-identifier-syntax-table')
is in effect or not.

Note that it's used in cases like after \"foo (bar)\" so it should
only match when it's certain that it's a declaration, e.g \"{\" but
not \",\" or \";\"."
  t "{"
  ;; If K&R style declarations should be recognized then one could
  ;; consider to match the start of any symbol since we want to match
  ;; the start of the first declaration in the "K&R region".  That
  ;; could however produce false matches on code like "FOO(bar) x"
  ;; where FOO is a cpp macro, so it's better to leave it out and rely
  ;; on the other heuristics in that case.
  t (if (c-lang-const c-postfix-spec-kwds)
	;; Add on the keywords in `c-postfix-spec-kwds'.
	(concat (c-lang-const c-after-suffixed-type-decl-key)
		"\\|"
		(c-make-keywords-re t (c-lang-const c-postfix-spec-kwds)))
      (c-lang-const c-after-suffixed-type-decl-key))
  ;; Also match the colon that starts a base class initializer list in
  ;; C++.  That can be confused with a function call before the colon
  ;; in a ? : operator, but we count on that `c-decl-prefix-re' won't
  ;; match before such a thing (as a declaration-level construct;
  ;; matches inside arglist contexts are already excluded).
  c++ "[{:]")
(c-lang-defvar c-after-suffixed-type-decl-key
  (c-lang-const c-after-suffixed-type-decl-key)
  'dont-doc)

(c-lang-defconst c-after-suffixed-type-maybe-decl-key
  ;; Regexp that in addition to `c-after-suffixed-type-decl-key'
  ;; matches ";" and ",".
  t (concat "\\(" (c-lang-const c-after-suffixed-type-decl-key) "\\)"
	    "\\|[;,]"))
(c-lang-defvar c-after-suffixed-type-maybe-decl-key
  (c-lang-const c-after-suffixed-type-maybe-decl-key))

(c-lang-defconst c-opt-type-concat-key
  "Regexp matching operators that concatenate types, e.g. the \"|\" in
\"int|string\" in Pike.  The end of the first submatch is taken as the
end of the operator.  nil in languages without such operators.  It's
undefined whether identifier syntax (see `c-identifier-syntax-table')
is in effect or not."
  t nil
  pike "\\([|.&]\\)\\($\\|[^|.&]\\)")
(c-lang-defvar c-opt-type-concat-key (c-lang-const c-opt-type-concat-key)
  'dont-doc)

(c-lang-defconst c-opt-type-suffix-key
  "Regexp matching operators that might follow after a type, or nil in
languages that don't have such operators.  The end of the first
submatch is taken as the end of the operator.  This should not match
things like C++ template arglists if `c-recognize-<>-arglists' is set.
It's undefined whether identifier syntax (see `c-identifier-syntax-table')
is in effect or not."
  t nil
  (c c++ objc pike) "\\(\\.\\.\\.\\)"
  java (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\.\\.\\.\\)"))
(c-lang-defvar c-opt-type-suffix-key (c-lang-const c-opt-type-suffix-key))

(c-lang-defvar c-known-type-key
  ;; Regexp matching the known type identifiers.  This is initialized
  ;; from the type keywords and `*-font-lock-extra-types'.  The first
  ;; submatch is the one that matches the type.  Note that this regexp
  ;; assumes that symbol constituents like '_' and '$' have word
  ;; syntax.
  (let* ((extra-types
	  (when (boundp (c-mode-symbol "font-lock-extra-types"))
	    (c-mode-var "font-lock-extra-types")))
	 (regexp-strings
	  (apply 'nconc
		 (mapcar (lambda (re)
		    (when (string-match "[][.*+?^$\\]" re)
		      (list re)))
		  extra-types)))
	 (plain-strings
	  (apply 'nconc
		 (mapcar (lambda (re)
		    (unless (string-match "[][.*+?^$\\]" re)
		      (list re)))
		  extra-types))))
    (concat "\\<\\("
	    (c-concat-separated
	     (append (list (c-make-keywords-re nil
			     (append (c-lang-const c-primitive-type-kwds)
				     plain-strings)))
		     regexp-strings)
	     "\\|")
	    "\\)\\>")))

(c-lang-defconst c-special-brace-lists
"List of open- and close-chars that makes up a pike-style brace list,
i.e. for a ([ ]) list there should be a cons (?\\[ . ?\\]) in this
list."
  t    nil
  pike '((?{ . ?}) (?\[ . ?\]) (?< . ?>)))
(c-lang-defvar c-special-brace-lists (c-lang-const c-special-brace-lists))

(c-lang-defconst c-recognize-knr-p
  "Non-nil means K&R style argument declarations are valid."
  t nil
  c t)
(c-lang-defvar c-recognize-knr-p (c-lang-const c-recognize-knr-p))

(c-lang-defconst c-recognize-typeless-decls
  "Non-nil means function declarations without return type should be
recognized.  That can introduce an ambiguity with parenthesized macro
calls before a brace block.  This setting does not affect declarations
that are preceded by a declaration starting keyword, so
e.g. `c-typeless-decl-kwds' may still be used when it's set to nil."
  t nil
  (c c++ objc) t)
(c-lang-defvar c-recognize-typeless-decls
  (c-lang-const c-recognize-typeless-decls))

(c-lang-defconst c-recognize-<>-arglists
  "Non-nil means C++ style template arglists should be handled.  More
specifically, this means a comma separated list of types or
expressions surrounded by \"<\" and \">\".  It's always preceded by an
identifier or one of the keywords on `c-<>-type-kwds' or
`c-<>-arglist-kwds'.  If there's an identifier before then the whole
expression is considered to be a type."
  t (or (consp (c-lang-const c-<>-type-kwds))
	(consp (c-lang-const c-<>-arglist-kwds))))
(c-lang-defvar c-recognize-<>-arglists (c-lang-const c-recognize-<>-arglists))

(c-lang-defconst c-enums-contain-decls
  "Non-nil means that an enum structure can contain declarations."
  t nil
  java t)
(c-lang-defvar c-enums-contain-decls (c-lang-const c-enums-contain-decls))

(c-lang-defconst c-recognize-paren-inits
  "Non-nil means that parenthesis style initializers exist,
i.e. constructs like

Foo bar (gnu);

in addition to the more classic

Foo bar = gnu;"
  t nil
  c++ t)
(c-lang-defvar c-recognize-paren-inits (c-lang-const c-recognize-paren-inits))

(c-lang-defconst c-recognize-paren-inexpr-blocks
  "Non-nil to recognize gcc style in-expression blocks,
i.e. compound statements surrounded by parentheses inside expressions."
  t nil
  (c c++) t)
(c-lang-defvar c-recognize-paren-inexpr-blocks
  (c-lang-const c-recognize-paren-inexpr-blocks))

(c-lang-defconst c-opt-<>-arglist-start
  ;; Regexp matching the start of angle bracket arglists in languages
  ;; where `c-recognize-<>-arglists' is set.  Does not exclude
  ;; keywords outside `c-<>-arglist-kwds'.  The first submatch is
  ;; assumed to surround the preceding symbol.  The whole match is
  ;; assumed to end directly after the opening "<".
  t (if (c-lang-const c-recognize-<>-arglists)
	(concat "\\("
		(c-lang-const c-symbol-key)
		"\\)"
		(c-lang-const c-syntactic-ws)
		"<")))
(c-lang-defvar c-opt-<>-arglist-start (c-lang-const c-opt-<>-arglist-start))

(c-lang-defconst c-opt-<>-arglist-start-in-paren
  ;; Regexp that in addition to `c-opt-<>-arglist-start' matches close
  ;; parens.  The first submatch is assumed to surround
  ;; `c-opt-<>-arglist-start'.
  t (if (c-lang-const c-opt-<>-arglist-start)
	(concat "\\("
		(c-lang-const c-opt-<>-arglist-start)
		"\\)\\|\\s\)")))
(c-lang-defvar c-opt-<>-arglist-start-in-paren
  (c-lang-const c-opt-<>-arglist-start-in-paren))

(c-lang-defconst c-opt-postfix-decl-spec-key
  ;; Regexp matching the beginning of a declaration specifier in the
  ;; region between the header and the body of a declaration.
  ;;
  ;; TODO: This is currently not used uniformly; c++-mode and
  ;; java-mode each have their own ways of using it.
  t    nil
  c++  (concat ":?"
	       (c-lang-const c-simple-ws) "*"
	       "\\(virtual" (c-lang-const c-simple-ws) "+\\)?\\("
	       (c-make-keywords-re nil (c-lang-const c-protection-kwds))
	       "\\)" (c-lang-const c-simple-ws) "+"
	       "\\(" (c-lang-const c-symbol-key) "\\)")
  java (c-make-keywords-re t (c-lang-const c-postfix-spec-kwds)))
(c-lang-defvar c-opt-postfix-decl-spec-key
  (c-lang-const c-opt-postfix-decl-spec-key))

(c-lang-defconst c-recognize-colon-labels
  "Non-nil if generic labels ending with \":\" should be recognized.
That includes labels in code and access keys in classes.  This does
not apply to labels recognized by `c-label-kwds' and
`c-opt-extra-label-key'."
  t nil
  (c c++ objc java pike) t)
(c-lang-defvar c-recognize-colon-labels
  (c-lang-const c-recognize-colon-labels))

(c-lang-defconst c-label-prefix-re
  "Regexp like `c-decl-prefix-re' that matches any token that can precede
a generic colon label.  Not used if `c-recognize-colon-labels' is
nil."
  t "\\([{};]+\\)")
(c-lang-defvar c-label-prefix-re
  (c-lang-const c-label-prefix-re))

(c-lang-defconst c-nonlabel-token-key
  "Regexp matching things that can't occur in generic colon labels,
neither in a statement nor in a declaration context.  The regexp is
tested at the beginning of every sexp in a suspected label,
i.e. before \":\".  Only used if `c-recognize-colon-labels' is set."
  t (concat
     ;; All keywords except `c-label-kwds' and `c-protection-kwds'.
     (c-make-keywords-re t
       (set-difference (c-lang-const c-keywords)
		       (append (c-lang-const c-label-kwds)
			       (c-lang-const c-protection-kwds))
		       :test 'string-equal)))
  ;; Don't allow string literals, except in AWK.  Character constants are OK.
  (c objc java pike idl) (concat "\"\\|"
				 (c-lang-const c-nonlabel-token-key))
  ;; Also check for open parens in C++, to catch member init lists in
  ;; constructors.  We normally allow it so that macros with arguments
  ;; work in labels.
  c++ (concat "\\s\(\\|\"\\|" (c-lang-const c-nonlabel-token-key)))
(c-lang-defvar c-nonlabel-token-key (c-lang-const c-nonlabel-token-key))

(c-lang-defconst c-nonlabel-token-2-key
  "Regexp matching things that can't occur two symbols before a colon in
a label construct.  This catches C++'s inheritance construct \"class foo
: bar\".  Only used if `c-recognize-colon-labels' is set."
  t "\\<\\>"				; matches nothing
  c++ (c-make-keywords-re t '("class")))
(c-lang-defvar c-nonlabel-token-2-key (c-lang-const c-nonlabel-token-2-key))

(c-lang-defconst c-opt-extra-label-key
  "Optional regexp matching labels.
Normally, labels are detected according to `c-nonlabel-token-key',
`c-decl-prefix-re' and `c-nonlabel-decl-prefix-re'.  This regexp can
be used if there are additional labels that aren't recognized that
way."
  t    nil
  objc (c-make-keywords-re t (c-lang-const c-protection-kwds)))
(c-lang-defvar c-opt-extra-label-key (c-lang-const c-opt-extra-label-key))

(c-lang-defconst c-opt-friend-key
  ;; Regexp describing friend declarations classes, or nil in
  ;; languages that don't have such things.
  ;;
  ;; TODO: Ought to use `c-prefix-spec-kwds-re' or similar, and the
  ;; template skipping isn't done properly.  This will disappear soon.
  t    nil
  c++  (concat "friend" (c-lang-const c-simple-ws) "+"
	       "\\|"
	       (concat "template"
		       (c-lang-const c-simple-ws) "*"
		       "<.+>"
		       (c-lang-const c-simple-ws) "*"
		       "friend"
		       (c-lang-const c-simple-ws) "+")))
(c-lang-defvar c-opt-friend-key (c-lang-const c-opt-friend-key))

(c-lang-defconst c-opt-method-key
  ;; Special regexp to match the start of Objective-C methods.  The
  ;; first submatch is assumed to end after the + or - key.
  t    nil
  objc (concat
	;; TODO: Ought to use a better method than anchoring on bol.
	"^\\s *"
	"\\([+-]\\)"
	(c-lang-const c-simple-ws) "*"
	(concat "\\("			; Return type.
		"([^\)]*)"
		(c-lang-const c-simple-ws) "*"
		"\\)?")
	"\\(" (c-lang-const c-symbol-key) "\\)"))
(c-lang-defvar c-opt-method-key (c-lang-const c-opt-method-key))

(c-lang-defconst c-type-decl-end-used
  ;; Must be set in buffers where the `c-type' text property might be
  ;; used with the value `c-decl-end'.
  ;;
  ;; `c-decl-end' is used to mark the ends of labels and access keys
  ;; to make interactive refontification work better.
  t (or (c-lang-const c-recognize-colon-labels)
	(and (c-lang-const c-label-kwds) t))
  ;; `c-decl-end' is used to mark the end of the @-style directives in
  ;; Objective-C.
  objc t)
(c-lang-defvar c-type-decl-end-used (c-lang-const c-type-decl-end-used))


;;; Wrap up the `c-lang-defvar' system.

;; Compile in the list of language variables that has been collected
;; with the `c-lang-defvar' and `c-lang-setvar' macros.  Note that the
;; first element of each is nil.
(defconst c-lang-variable-inits (cc-eval-when-compile c-lang-variable-inits))
(defconst c-emacs-variable-inits (cc-eval-when-compile c-emacs-variable-inits))

;; Make the `c-lang-setvar' variables buffer local in the current buffer.
;; These are typically standard emacs variables such as `comment-start'.
(defmacro c-make-emacs-variables-local ()
  `(progn
     ,@(mapcar (lambda (init)
		 `(make-local-variable ',(car init)))
	       (cdr c-emacs-variable-inits))))

(defun c-make-init-lang-vars-fun (mode)
  "Create a function that initializes all the language dependent variables
for the given mode.

This function should be evaluated at compile time, so that the
function it returns is byte compiled with all the evaluated results
from the language constants.  Use the `c-init-language-vars' macro to
accomplish that conveniently."

  (if (and (not load-in-progress)
	   (boundp 'byte-compile-dest-file)
	   (stringp byte-compile-dest-file))

      ;; No need to byte compile this lambda since the byte compiler is
      ;; smart enough to detect the `funcall' construct in the
      ;; `c-init-language-vars' macro below and compile it all straight
      ;; into the function that contains `c-init-language-vars'.
      `(lambda ()

	 ;; This let sets up the context for `c-mode-var' and similar
	 ;; that could be in the result from `cl-macroexpand-all'.
	 (let ((c-buffer-is-cc-mode ',mode)
	       current-var source-eval)
	   (c-make-emacs-variables-local)
	   (condition-case err

	       (if (eq c-version-sym ',c-version-sym)
		   (setq ,@(let ((c-buffer-is-cc-mode mode)
				 (c-lang-const-expansion 'immediate))
			     ;; `c-lang-const' will expand to the evaluated
			     ;; constant immediately in `cl-macroexpand-all'
			     ;; below.
			      (mapcan
			       (lambda (init)
				 `(current-var ',(car init)
				   ,(car init) ,(cl-macroexpand-all
						 (elt init 1))))
			       ;; Note: The following `append' copies the
			       ;; first argument.  That list is small, so
			       ;; this doesn't matter too much.
			      (append (cdr c-emacs-variable-inits)
				      (cdr c-lang-variable-inits)))))

		 ;; This diagnostic message isn't useful for end
		 ;; users, so it's disabled.
		 ;;(unless (get ',mode 'c-has-warned-lang-consts)
		 ;;  (message ,(concat "%s compiled with CC Mode %s "
		 ;;		       "but loaded with %s - evaluating "
		 ;;		       "language constants from source")
		 ;;	      ',mode ,c-version c-version)
		 ;;  (put ',mode 'c-has-warned-lang-consts t))

		 (setq source-eval t)
		 (let ((init ',(append (cdr c-emacs-variable-inits)
				       (cdr c-lang-variable-inits))))
		   (while init
		     (setq current-var (caar init))
		     (set (caar init) (eval (cadar init)))
		     (setq init (cdr init)))))

	     (error
	      (if current-var
		  (message "Eval error in the `c-lang-defvar' or `c-lang-setvar' for `%s'%s: %S"
			   current-var
			   (if source-eval
			       (format "\
 (fallback source eval - %s compiled with CC Mode %s but loaded with %s)"
				       ',mode ,c-version c-version)
			     "")
			   err)
		(signal (car err) (cdr err)))))))

    ;; Being evaluated from source.  Always use the dynamic method to
    ;; work well when `c-lang-defvar's in this file are reevaluated
    ;; interactively.
    `(lambda ()
       (require 'cc-langs)
       (let ((c-buffer-is-cc-mode ',mode)
	     (init (append (cdr c-emacs-variable-inits)
			   (cdr c-lang-variable-inits)))
	     current-var)
	 (c-make-emacs-variables-local)
	 (condition-case err

	     (while init
	       (setq current-var (caar init))
	       (set (caar init) (eval (cadar init)))
	       (setq init (cdr init)))

	   (error
	    (if current-var
		(message
		 "Eval error in the `c-lang-defvar' or `c-lang-setver' for `%s' (source eval): %S"
		 current-var err)
	      (signal (car err) (cdr err)))))))
    ))

(defmacro c-init-language-vars (mode)
  "Initialize all the language dependent variables for the given mode.
This macro is expanded at compile time to a form tailored for the mode
in question, so MODE must be a constant.  Therefore MODE is not
evaluated and should not be quoted."
  `(funcall ,(c-make-init-lang-vars-fun mode)))


(cc-provide 'cc-langs)

;;; cc-langs.el ends here
