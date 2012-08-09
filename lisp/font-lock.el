;;; font-lock.el --- Electric font lock mode

;; Copyright (C) 1992-2012 Free Software Foundation, Inc.

;; Author: Jamie Zawinski
;;	Richard Stallman
;;	Stefan Monnier
;; Maintainer: FSF
;; Keywords: languages, faces
;; Package: emacs

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

;; Font Lock mode is a minor mode that causes your comments to be displayed in
;; one face, strings in another, reserved words in another, and so on.
;;
;; Comments will be displayed in `font-lock-comment-face'.
;; Strings will be displayed in `font-lock-string-face'.
;; Regexps are used to display selected patterns in other faces.
;;
;; To make the text you type be fontified, use M-x font-lock-mode RET.
;; When this minor mode is on, the faces of the current line are updated with
;; every insertion or deletion.
;;
;; To turn Font Lock mode on automatically, add this to your ~/.emacs file:
;;
;;  (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
;;
;; Or if you want to turn Font Lock mode on in many modes:
;;
;;  (global-font-lock-mode t)
;;
;; Fontification for a particular mode may be available in a number of levels
;; of decoration.  The higher the level, the more decoration, but the more time
;; it takes to fontify.  See the variable `font-lock-maximum-decoration', and
;; also the variable `font-lock-maximum-size'.  Support modes for Font Lock
;; mode can be used to speed up Font Lock mode.  See `font-lock-support-mode'.

;;; How Font Lock mode fontifies:

;; When Font Lock mode is turned on in a buffer, it (a) fontifies the entire
;; buffer and (b) installs one of its fontification functions on one of the
;; hook variables that are run by Emacs after every buffer change (i.e., an
;; insertion or deletion).  Fontification means the replacement of `face' text
;; properties in a given region; Emacs displays text with these `face' text
;; properties appropriately.
;;
;; Fontification normally involves syntactic (i.e., strings and comments) and
;; regexp (i.e., keywords and everything else) passes.  There are actually
;; three passes; (a) the syntactic keyword pass, (b) the syntactic pass and (c)
;; the keyword pass.  Confused?
;;
;; The syntactic keyword pass places `syntax-table' text properties in the
;; buffer according to the variable `font-lock-syntactic-keywords'.  It is
;; necessary because Emacs's syntax table is not powerful enough to describe all
;; the different syntactic constructs required by the sort of people who decide
;; that a single quote can be syntactic or not depending on the time of day.
;; (What sort of person could decide to overload the meaning of a quote?)
;; Obviously the syntactic keyword pass must occur before the syntactic pass.
;;
;; The syntactic pass places `face' text properties in the buffer according to
;; syntactic context, i.e., according to the buffer's syntax table and buffer
;; text's `syntax-table' text properties.  It involves using a syntax parsing
;; function to determine the context of different parts of a region of text.  A
;; syntax parsing function is necessary because generally strings and/or
;; comments can span lines, and so the context of a given region is not
;; necessarily apparent from the content of that region.  Because the keyword
;; pass only works within a given region, it is not generally appropriate for
;; syntactic fontification.  This is the first fontification pass that makes
;; changes visible to the user; it fontifies strings and comments.
;;
;; The keyword pass places `face' text properties in the buffer according to
;; the variable `font-lock-keywords'.  It involves searching for given regexps
;; (or calling given search functions) within the given region.  This is the
;; second fontification pass that makes changes visible to the user; it
;; fontifies language reserved words, etc.
;;
;; Oh, and the answer is, "Yes, obviously just about everything should be done
;; in a single syntactic pass, but the only syntactic parser available
;; understands only strings and comments."  Perhaps one day someone will write
;; some syntactic parsers for common languages and a son-of-font-lock.el could
;; use them rather then relying so heavily on the keyword (regexp) pass.

;;; How Font Lock mode supports modes or is supported by modes:

;; Modes that support Font Lock mode do so by defining one or more variables
;; whose values specify the fontification.  Font Lock mode knows of these
;; variable names from the buffer local variable `font-lock-defaults'.
;; (Font Lock mode is set up via (a) where a mode's patterns are
;; distributed with the mode's package library, and (b) where a mode's
;; patterns are distributed with font-lock.el itself.  An example of (a)
;; is Pascal mode, an example of (b) is Lisp mode.  Normally, the mechanism is
;; (a); (b) is used where it is not clear which package library should contain
;; the pattern definitions.)  Font Lock mode chooses which variable to use for
;; fontification based on `font-lock-maximum-decoration'.
;;
;; Font Lock mode fontification behavior can be modified in a number of ways.
;; See the below comments and the comments distributed throughout this file.

;;; Constructing patterns:

;; See the documentation for the variable `font-lock-keywords'.
;;
;; Efficient regexps for use as MATCHERs for `font-lock-keywords' and
;; `font-lock-syntactic-keywords' can be generated via the function
;; `regexp-opt'.

;;; Adding patterns for modes that already support Font Lock:

;; Though Font Lock highlighting patterns already exist for many modes, it's
;; likely there's something that you want fontified that currently isn't, even
;; at the maximum fontification level.  You can add highlighting patterns via
;; `font-lock-add-keywords'.  For example, say in some C
;; header file you #define the token `and' to expand to `&&', etc., to make
;; your C code almost readable.  In your ~/.emacs there could be:
;;
;;  (font-lock-add-keywords 'c-mode '("\\<\\(and\\|or\\|not\\)\\>"))
;;
;; Some modes provide specific ways to modify patterns based on the values of
;; other variables.  For example, additional C types can be specified via the
;; variable `c-font-lock-extra-types'.

;;; Adding patterns for modes that do not support Font Lock:

;; Not all modes support Font Lock mode.  If you (as a user of the mode) add
;; patterns for a new mode, you must define in your ~/.emacs a variable or
;; variables that specify regexp fontification.  Then, you should indicate to
;; Font Lock mode, via the mode hook setting `font-lock-defaults', exactly what
;; support is required.  For example, say Foo mode should have the following
;; regexps fontified case-sensitively, and comments and strings should not be
;; fontified automagically.  In your ~/.emacs there could be:
;;
;;  (defvar foo-font-lock-keywords
;;    '(("\\<\\(one\\|two\\|three\\)\\>" . font-lock-keyword-face)
;;      ("\\<\\(four\\|five\\|six\\)\\>" . font-lock-type-face))
;;    "Default expressions to highlight in Foo mode.")
;;
;;  (add-hook 'foo-mode-hook
;;   (lambda ()
;;     (set (make-local-variable 'font-lock-defaults)
;;          '(foo-font-lock-keywords t))))

;;; Adding Font Lock support for modes:

;; Of course, it would be better that the mode already supports Font Lock mode.
;; The package author would do something similar to above.  The mode must
;; define at the top-level a variable or variables that specify regexp
;; fontification.  Then, the mode command should indicate to Font Lock mode,
;; via `font-lock-defaults', exactly what support is required.  For example,
;; say Bar mode should have the following regexps fontified case-insensitively,
;; and comments and strings should be fontified automagically.  In bar.el there
;; could be:
;;
;;  (defvar bar-font-lock-keywords
;;    '(("\\<\\(uno\\|due\\|tre\\)\\>" . font-lock-keyword-face)
;;      ("\\<\\(quattro\\|cinque\\|sei\\)\\>" . font-lock-type-face))
;;    "Default expressions to highlight in Bar mode.")
;;
;; and within `bar-mode' there could be:
;;
;;  (set (make-local-variable 'font-lock-defaults)
;;       '(bar-font-lock-keywords nil t))

;; What is fontification for?  You might say, "It's to make my code look nice."
;; I think it should be for adding information in the form of cues.  These cues
;; should provide you with enough information to both (a) distinguish between
;; different items, and (b) identify the item meanings, without having to read
;; the items and think about it.  Therefore, fontification allows you to think
;; less about, say, the structure of code, and more about, say, why the code
;; doesn't work.  Or maybe it allows you to think less and drift off to sleep.
;;
;; So, here are my opinions/advice/guidelines:
;;
;; - Highlight conceptual objects, such as function and variable names, and
;;   different objects types differently, i.e., (a) and (b) above, highlight
;;   function names differently to variable names.
;; - Keep the faces distinct from each other as far as possible.
;;   i.e., (a) above.
;; - Use the same face for the same conceptual object, across all modes.
;;   i.e., (b) above, all modes that have items that can be thought of as, say,
;;   keywords, should be highlighted with the same face, etc.
;; - Make the face attributes fit the concept as far as possible.
;;   i.e., function names might be a bold color such as blue, comments might
;;   be a bright color such as red, character strings might be brown, because,
;;   err, strings are brown (that was not the reason, please believe me).
;; - Don't use a non-nil OVERRIDE unless you have a good reason.
;;   Only use OVERRIDE for special things that are easy to define, such as the
;;   way `...' quotes are treated in strings and comments in Emacs Lisp mode.
;;   Don't use it to, say, highlight keywords in commented out code or strings.
;; - Err, that's it.

;;; Code:

(require 'syntax)
(eval-when-compile (require 'cl))

;; Define core `font-lock' group.
(defgroup font-lock '((jit-lock custom-group))
  "Font Lock mode text highlighting package."
  :link '(custom-manual :tag "Emacs Manual" "(emacs)Font Lock")
  :link '(custom-manual :tag "Elisp Manual" "(elisp)Font Lock Mode")
  :group 'faces)

(defgroup font-lock-faces nil
  "Faces for highlighting text."
  :prefix "font-lock-"
  :group 'font-lock)

(defgroup font-lock-extra-types nil
  "Extra mode-specific type names for highlighting declarations."
  :group 'font-lock)

;; User variables.

(defcustom font-lock-maximum-size 256000
  "Maximum buffer size for unsupported buffer fontification.
When `font-lock-support-mode' is nil, only buffers smaller than
this are fontified.  This variable has no effect if a Font Lock
support mode (usually `jit-lock-mode') is enabled.

If nil, means size is irrelevant.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 256000) (c++-mode . 256000) (rmail-mode . 1048576))
means that the maximum size is 250K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise."
  :type '(choice (const :tag "none" nil)
		 (integer :tag "size")
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . nil))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Size"
				      (const :tag "none" nil)
				      (integer :tag "size")))))
  :group 'font-lock)
(make-obsolete-variable 'font-lock-maximum-size nil "24.1")

(defcustom font-lock-maximum-decoration t
  "Maximum decoration level for fontification.
If nil, use the default decoration (typically the minimum available).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
The higher the number, the more decoration is done.
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . t) (c++-mode . 2) (t . 1))
means use the maximum decoration available for buffers in C mode, level 2
decoration for buffers in C++ mode, and level 1 decoration otherwise."
  :type '(choice (const :tag "default" nil)
		 (const :tag "maximum" t)
		 (integer :tag "level" 1)
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . t))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Decoration"
				      (const :tag "default" nil)
				      (const :tag "maximum" t)
				      (integer :tag "level" 1)))))
  :group 'font-lock)

(defcustom font-lock-verbose nil
  "If non-nil, means show status messages for buffer fontification.
If a number, only buffers greater than this size have fontification messages."
  :type '(choice (const :tag "never" nil)
		 (other :tag "always" t)
		 (integer :tag "size"))
  :group 'font-lock
  :version "24.1")


;; Originally these variable values were face names such as `bold' etc.
;; Now we create our own faces, but we keep these variables for compatibility
;; and they give users another mechanism for changing face appearance.
;; We now allow a FACENAME in `font-lock-keywords' to be any expression that
;; returns a face.  So the easiest thing is to continue using these variables,
;; rather than sometimes evalling FACENAME and sometimes not.  sm.

;; Note that in new code, in the vast majority of cases there is no
;; need to create variables that specify face names.  Simply using
;; faces directly is enough.  Font-lock is not a template to be
;; followed in this area.
(defvar font-lock-comment-face		'font-lock-comment-face
  "Face name to use for comments.")

(defvar font-lock-comment-delimiter-face 'font-lock-comment-delimiter-face
  "Face name to use for comment delimiters.")

(defvar font-lock-string-face		'font-lock-string-face
  "Face name to use for strings.")

(defvar font-lock-doc-face		'font-lock-doc-face
  "Face name to use for documentation.")

(defvar font-lock-keyword-face		'font-lock-keyword-face
  "Face name to use for keywords.")

(defvar font-lock-builtin-face		'font-lock-builtin-face
  "Face name to use for builtins.")

(defvar font-lock-function-name-face	'font-lock-function-name-face
  "Face name to use for function names.")

(defvar font-lock-variable-name-face	'font-lock-variable-name-face
  "Face name to use for variable names.")

(defvar font-lock-type-face		'font-lock-type-face
  "Face name to use for type and class names.")

(defvar font-lock-constant-face		'font-lock-constant-face
  "Face name to use for constant and label names.")

(defvar font-lock-warning-face		'font-lock-warning-face
  "Face name to use for things that should stand out.")

(defvar font-lock-negation-char-face	'font-lock-negation-char-face
  "Face name to use for easy to overlook negation.
This can be an \"!\" or the \"n\" in \"ifndef\".")

(defvar font-lock-preprocessor-face	'font-lock-preprocessor-face
  "Face name to use for preprocessor directives.")

(defvar font-lock-reference-face	'font-lock-constant-face)
(make-obsolete-variable 'font-lock-reference-face 'font-lock-constant-face "20.3")

;; Fontification variables:

(defvar font-lock-keywords nil
  "A list of the keywords to highlight.
There are two kinds of values: user-level, and compiled.

A user-level keywords list is what a major mode or the user would
set up.  Normally the list would come from `font-lock-defaults'.
through selection of a fontification level and evaluation of any
contained expressions.  You can also alter it by calling
`font-lock-add-keywords' or `font-lock-remove-keywords' with MODE = nil.

Each element in a user-level keywords list should have one of these forms:

 MATCHER
 (MATCHER . SUBEXP)
 (MATCHER . FACENAME)
 (MATCHER . HIGHLIGHT)
 (MATCHER HIGHLIGHT ...)
 (eval . FORM)

where MATCHER can be either the regexp to search for, or the function name to
call to make the search (called with one argument, the limit of the search;
it should return non-nil, move point, and set `match-data' appropriately if
it succeeds; like `re-search-forward' would).
MATCHER regexps can be generated via the function `regexp-opt'.

FORM is an expression, whose value should be a keyword element, evaluated when
the keyword is (first) used in a buffer.  This feature can be used to provide a
keyword that can only be generated when Font Lock mode is actually turned on.

HIGHLIGHT should be either MATCH-HIGHLIGHT or MATCH-ANCHORED.

For highlighting single items, for example each instance of the word \"foo\",
typically only MATCH-HIGHLIGHT is required.
However, if an item or (typically) items are to be highlighted following the
instance of another item (the anchor), for example each instance of the
word \"bar\" following the word \"anchor\" then MATCH-ANCHORED may be required.

MATCH-HIGHLIGHT should be of the form:

 (SUBEXP FACENAME [OVERRIDE [LAXMATCH]])

SUBEXP is the number of the subexpression of MATCHER to be highlighted.

FACENAME is an expression whose value is the face name to use.
Instead of a face, FACENAME can evaluate to a property list
of the form (face FACE PROP1 VAL1 PROP2 VAL2 ...)
in which case all the listed text-properties will be set rather than
just FACE.  In such a case, you will most likely want to put those
properties in `font-lock-extra-managed-props' or to override
`font-lock-unfontify-region-function'.

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing fontification can
be overwritten.  If `keep', only parts not already fontified are highlighted.
If `prepend' or `append', existing fontification is merged with the new, in
which the new or existing fontification, respectively, takes precedence.
If LAXMATCH is non-nil, that means don't signal an error if there is
no match for SUBEXP in MATCHER.

For example, an element of the form highlights (if not already highlighted):

 \"\\\\\\=<foo\\\\\\=>\"		discrete occurrences of \"foo\" in the value of the
			variable `font-lock-keyword-face'.
 (\"fu\\\\(bar\\\\)\" . 1)	substring \"bar\" within all occurrences of \"fubar\" in
			the value of `font-lock-keyword-face'.
 (\"fubar\" . fubar-face)	Occurrences of \"fubar\" in the value of `fubar-face'.
 (\"foo\\\\|bar\" 0 foo-bar-face t)
			occurrences of either \"foo\" or \"bar\" in the value
			of `foo-bar-face', even if already highlighted.
 (fubar-match 1 fubar-face)
			the first subexpression within all occurrences of
			whatever the function `fubar-match' finds and matches
			in the value of `fubar-face'.

MATCH-ANCHORED should be of the form:

 (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

where MATCHER is a regexp to search for or the function name to call to make
the search, as for MATCH-HIGHLIGHT above, but with one exception; see below.
PRE-MATCH-FORM and POST-MATCH-FORM are evaluated before the first, and after
the last, instance MATCH-ANCHORED's MATCHER is used.  Therefore they can be
used to initialize before, and cleanup after, MATCHER is used.  Typically,
PRE-MATCH-FORM is used to move to some position relative to the original
MATCHER, before starting with MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might
be used to move back, before resuming with MATCH-ANCHORED's parent's MATCHER.

For example, an element of the form highlights (if not already highlighted):

 (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-face) (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-face)))

 discrete occurrences of \"anchor\" in the value of `anchor-face', and subsequent
 discrete occurrences of \"item\" (on the same line) in the value of `item-face'.
 (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore \"item\" is
 initially searched for starting from the end of the match of \"anchor\", and
 searching for subsequent instances of \"anchor\" resumes from where searching
 for \"item\" concluded.)

The above-mentioned exception is as follows.  The limit of the MATCHER search
defaults to the end of the line after PRE-MATCH-FORM is evaluated.
However, if PRE-MATCH-FORM returns a position greater than the position after
PRE-MATCH-FORM is evaluated, that position is used as the limit of the search.
It is generally a bad idea to return a position greater than the end of the
line, i.e., cause the MATCHER search to span lines.

These regular expressions can match text which spans lines, although
it is better to avoid it if possible since updating them while editing
text is slower, and it is not guaranteed to be always correct when using
support modes like jit-lock or lazy-lock.

This variable is set by major modes via the variable `font-lock-defaults'.
Be careful when composing regexps for this list; a poorly written pattern can
dramatically slow things down!

A compiled keywords list starts with t.  It is produced internal
by `font-lock-compile-keywords' from a user-level keywords list.
Its second element is the user-level keywords list that was
compiled.  The remaining elements have the same form as
user-level keywords, but normally their values have been
optimized.")

(defvar font-lock-keywords-alist nil
  "Alist of additional `font-lock-keywords' elements for major modes.

Each element has the form (MODE KEYWORDS . HOW).
`font-lock-set-defaults' adds the elements in the list KEYWORDS to
`font-lock-keywords' when Font Lock is turned on in major mode MODE.

If HOW is nil, KEYWORDS are added at the beginning of
`font-lock-keywords'.  If it is `set', they are used to replace the
value of `font-lock-keywords'.  If HOW is any other non-nil value,
they are added at the end.

This is normally set via `font-lock-add-keywords' and
`font-lock-remove-keywords'.")
(put 'font-lock-keywords-alist 'risky-local-variable t)

(defvar font-lock-removed-keywords-alist nil
  "Alist of `font-lock-keywords' elements to be removed for major modes.

Each element has the form (MODE . KEYWORDS).  `font-lock-set-defaults'
removes the elements in the list KEYWORDS from `font-lock-keywords'
when Font Lock is turned on in major mode MODE.

This is normally set via `font-lock-add-keywords' and
`font-lock-remove-keywords'.")

(defvar font-lock-keywords-only nil
  "*Non-nil means Font Lock should not fontify comments or strings.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-keywords-case-fold-search nil
  "*Non-nil means the patterns in `font-lock-keywords' are case-insensitive.
This is set via the function `font-lock-set-defaults', based on
the CASE-FOLD argument of `font-lock-defaults'.")
(make-variable-buffer-local 'font-lock-keywords-case-fold-search)

(defvar font-lock-syntactically-fontified 0
  "Point up to which `font-lock-syntactic-keywords' has been applied.
If nil, this is ignored, in which case the syntactic fontification may
sometimes be slightly incorrect.")
(make-variable-buffer-local 'font-lock-syntactically-fontified)

(defvar font-lock-syntactic-face-function
  (lambda (state)
    (if (nth 3 state) font-lock-string-face font-lock-comment-face))
  "Function to determine which face to use when fontifying syntactically.
The function is called with a single parameter (the state as returned by
`parse-partial-sexp' at the beginning of the region to highlight) and
should return a face.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-syntactic-keywords nil
  "A list of the syntactic keywords to put syntax properties on.
The value can be the list itself, or the name of a function or variable
whose value is the list.

See `font-lock-keywords' for a description of the form of this list;
only the differences are stated here.  MATCH-HIGHLIGHT should be of the form:

 (SUBEXP SYNTAX OVERRIDE LAXMATCH)

where SYNTAX can be a string (as taken by `modify-syntax-entry'), a syntax
table, a cons cell (as returned by `string-to-syntax') or an expression whose
value is such a form.  OVERRIDE cannot be `prepend' or `append'.

Here are two examples of elements of `font-lock-syntactic-keywords'
and what they do:

 (\"\\\\$\\\\(#\\\\)\" 1 \".\")

 gives a hash character punctuation syntax (\".\") when following a
 dollar-sign character.  Hash characters in other contexts will still
 follow whatever the syntax table says about the hash character.

 (\"\\\\('\\\\).\\\\('\\\\)\"
  (1 \"\\\"\")
  (2 \"\\\"\"))

 gives a pair single-quotes, which surround a single character, a SYNTAX of
 \"\\\"\" (meaning string quote syntax).  Single-quote characters in other
 contexts will not be affected.

This is normally set via `font-lock-defaults'.")
(make-obsolete-variable 'font-lock-syntactic-keywords
                        'syntax-propertize-function "24.1")

(defvar font-lock-syntax-table nil
  "Non-nil means use this syntax table for fontifying.
If this is nil, the major mode's syntax table is used.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-beginning-of-syntax-function nil
  "*Non-nil means use this function to move back outside all constructs.
When called with no args it should move point backward to a place which
is not in a string or comment and not within any bracket-pairs (or else,
a place such that any bracket-pairs outside it can be ignored for Emacs
syntax analysis and fontification).

If this is nil, Font Lock uses `syntax-begin-function' to move back
outside of any comment, string, or sexp.  This variable is semi-obsolete;
we recommend setting `syntax-begin-function' instead.

This is normally set via `font-lock-defaults'.")
(make-obsolete-variable 'font-lock-beginning-of-syntax-function
                        'syntax-begin-function "23.3" 'set)

(defvar font-lock-mark-block-function nil
  "*Non-nil means use this function to mark a block of text.
When called with no args it should leave point at the beginning of any
enclosing textual block and mark at the end.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-fontify-buffer-function 'font-lock-default-fontify-buffer
  "Function to use for fontifying the buffer.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-unfontify-buffer-function 'font-lock-default-unfontify-buffer
  "Function to use for unfontifying the buffer.
This is used when turning off Font Lock mode.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-fontify-region-function 'font-lock-default-fontify-region
  "Function to use for fontifying a region.
It should take two args, the beginning and end of the region, and an optional
third arg VERBOSE.  If VERBOSE is non-nil, the function should print status
messages.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-unfontify-region-function 'font-lock-default-unfontify-region
  "Function to use for unfontifying a region.
It should take two args, the beginning and end of the region.
This is normally set via `font-lock-defaults'.")

(defvar font-lock-inhibit-thing-lock nil
  "List of Font Lock mode related modes that should not be turned on.
Currently, valid mode names are `fast-lock-mode', `jit-lock-mode' and
`lazy-lock-mode'.  This is normally set via `font-lock-defaults'.")

(defvar font-lock-multiline nil
  "Whether font-lock should cater to multiline keywords.
If nil, don't try to handle multiline patterns.
If t, always handle multiline patterns.
If `undecided', don't try to handle multiline patterns until you see one.
Major/minor modes can set this variable if they know which option applies.")

(defvar font-lock-fontified nil)	; Whether we have fontified the buffer.

;; Font Lock mode.

(eval-when-compile
  ;;
  ;; We don't do this at the top-level as we only use non-autoloaded macros.
  (require 'cl)
  ;;
  ;; Borrowed from lazy-lock.el.
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro save-buffer-state (&rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state."
    (declare (indent 0) (debug t))
    `(let ((inhibit-point-motion-hooks t))
       (with-silent-modifications
         ,@body)))
  ;;
  ;; Shut up the byte compiler.
  (defvar font-lock-face-attributes))	; Obsolete but respected if set.

(defun font-lock-specified-p (mode)
  "Return non-nil if the current buffer is ready for fontification.
The MODE argument, if non-nil, means Font Lock mode is about to
be enabled."
  (or font-lock-defaults
      (and (boundp 'font-lock-keywords)
	   font-lock-keywords)
      (and mode
	   (boundp 'font-lock-set-defaults)
	   font-lock-set-defaults
	   font-lock-major-mode
	   (not (eq font-lock-major-mode major-mode)))))

(defun font-lock-initial-fontify ()
  ;; The first fontification after turning the mode on.  This must
  ;;  only be called after the mode hooks have been run.
  (when (and font-lock-mode
	     (font-lock-specified-p t))
    (let ((max-size (font-lock-value-in-major-mode font-lock-maximum-size)))
      (cond (font-lock-fontified
	     nil)
	    ((or (null max-size) (> max-size (buffer-size)))
	     (font-lock-fontify-buffer))
	    (font-lock-verbose
	     (message "Fontifying %s...buffer size greater than font-lock-maximum-size"
		      (buffer-name)))))))

(defun font-lock-mode-internal (arg)
  ;; Turn on Font Lock mode.
  (when arg
    (add-hook 'after-change-functions 'font-lock-after-change-function t t)
    (font-lock-set-defaults)
    (font-lock-turn-on-thing-lock))
  ;; Turn off Font Lock mode.
  (unless font-lock-mode
    (remove-hook 'after-change-functions 'font-lock-after-change-function t)
    (font-lock-unfontify-buffer)
    (font-lock-turn-off-thing-lock)))

(defun font-lock-add-keywords (mode keywords &optional how)
  "Add highlighting KEYWORDS for MODE.

MODE should be a symbol, the major mode command name, such as `c-mode'
or nil.  If nil, highlighting keywords are added for the current buffer.
KEYWORDS should be a list; see the variable `font-lock-keywords'.
By default they are added at the beginning of the current highlighting list.
If optional argument HOW is `set', they are used to replace the current
highlighting list.  If HOW is any other non-nil value, they are added at the
end of the current highlighting list.

For example:

 (font-lock-add-keywords 'c-mode
  '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . font-lock-keyword-face)))

adds two fontification patterns for C mode, to fontify `FIXME:' words, even in
comments, and to fontify `and', `or' and `not' words as keywords.

The above procedure will only add the keywords for C mode, not
for modes derived from C mode.  To add them for derived modes too,
pass nil for MODE and add the call to c-mode-hook.

For example:

 (add-hook 'c-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
      (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" .
       font-lock-keyword-face)))))

The above procedure may fail to add keywords to derived modes if
some involved major mode does not follow the standard conventions.
File a bug report if this happens, so the major mode can be corrected.

Note that some modes have specialized support for additional patterns, e.g.,
see the variables `c-font-lock-extra-types', `c++-font-lock-extra-types',
`objc-font-lock-extra-types' and `java-font-lock-extra-types'."
  (cond (mode
	 ;; If MODE is non-nil, add the KEYWORDS and HOW spec to
	 ;; `font-lock-keywords-alist' so `font-lock-set-defaults' uses them.
	 (let ((spec (cons keywords how)) cell)
	   (if (setq cell (assq mode font-lock-keywords-alist))
	       (if (eq how 'set)
		   (setcdr cell (list spec))
		 (setcdr cell (append (cdr cell) (list spec))))
	     (push (list mode spec) font-lock-keywords-alist)))
	 ;; Make sure that `font-lock-removed-keywords-alist' does not
	 ;; contain the new keywords.
	 (font-lock-update-removed-keyword-alist mode keywords how))
	(t
         (when (and font-lock-mode
                    (not (or font-lock-keywords font-lock-defaults)))
           ;; The major mode has not set any keywords, so when we enabled
           ;; font-lock-mode it only enabled the font-core.el part, not the
           ;; font-lock-mode-internal.  Try again.
           (font-lock-mode -1)
           (set (make-local-variable 'font-lock-defaults) '(nil t))
           (font-lock-mode 1))
	 ;; Otherwise set or add the keywords now.
	 ;; This is a no-op if it has been done already in this buffer
	 ;; for the correct major mode.
	 (font-lock-set-defaults)
	 (let ((was-compiled (eq (car font-lock-keywords) t)))
	   ;; Bring back the user-level (uncompiled) keywords.
	   (if was-compiled
	       (setq font-lock-keywords (cadr font-lock-keywords)))
	   ;; Now modify or replace them.
	   (if (eq how 'set)
	       (setq font-lock-keywords keywords)
	     (font-lock-remove-keywords nil keywords) ;to avoid duplicates
	     (let ((old (if (eq (car-safe font-lock-keywords) t)
			    (cdr font-lock-keywords)
			  font-lock-keywords)))
	       (setq font-lock-keywords (if how
					    (append old keywords)
					  (append keywords old)))))
	   ;; If the keywords were compiled before, compile them again.
	   (if was-compiled
	       (setq font-lock-keywords
                     (font-lock-compile-keywords font-lock-keywords)))))))

(defun font-lock-update-removed-keyword-alist (mode keywords how)
  "Update `font-lock-removed-keywords-alist' when adding new KEYWORDS to MODE."
  ;; When font-lock is enabled first all keywords in the list
  ;; `font-lock-keywords-alist' are added, then all keywords in the
  ;; list `font-lock-removed-keywords-alist' are removed.  If a
  ;; keyword was once added, removed, and then added again it must be
  ;; removed from the removed-keywords list.  Otherwise the second add
  ;; will not take effect.
  (let ((cell (assq mode font-lock-removed-keywords-alist)))
    (if cell
	(if (eq how 'set)
	    ;; A new set of keywords is defined.  Forget all about
	    ;; our old keywords that should be removed.
	    (setq font-lock-removed-keywords-alist
		  (delq cell font-lock-removed-keywords-alist))
	  ;; Delete all previously removed keywords.
	  (dolist (kword keywords)
	    (setcdr cell (delete kword (cdr cell))))
	  ;; Delete the mode cell if empty.
	  (if (null (cdr cell))
	      (setq font-lock-removed-keywords-alist
		    (delq cell font-lock-removed-keywords-alist)))))))

;; Written by Anders Lindgren <andersl@andersl.com>.
;;
;; Case study:
;; (I)  The keywords are removed from a major mode.
;;      In this case the keyword could be local (i.e. added earlier by
;;      `font-lock-add-keywords'), global, or both.
;;
;;      (a) In the local case we remove the keywords from the variable
;;          `font-lock-keywords-alist'.
;;
;;      (b) The actual global keywords are not known at this time.
;;          All keywords are added to `font-lock-removed-keywords-alist',
;;          when font-lock is enabled those keywords are removed.
;;
;;      Note that added keywords are taken out of the list of removed
;;      keywords.  This ensure correct operation when the same keyword
;;      is added and removed several times.
;;
;; (II) The keywords are removed from the current buffer.
(defun font-lock-remove-keywords (mode keywords)
  "Remove highlighting KEYWORDS for MODE.

MODE should be a symbol, the major mode command name, such as `c-mode'
or nil.  If nil, highlighting keywords are removed for the current buffer.

To make the removal apply to modes derived from MODE as well,
pass nil for MODE and add the call to MODE-hook.  This may fail
for some derived modes if some involved major mode does not
follow the standard conventions.  File a bug report if this
happens, so the major mode can be corrected."
  (cond (mode
	 ;; Remove one keyword at the time.
	 (dolist (keyword keywords)
	   (let ((top-cell (assq mode font-lock-keywords-alist)))
	     ;; If MODE is non-nil, remove the KEYWORD from
	     ;; `font-lock-keywords-alist'.
	     (when top-cell
	       (dolist (keyword-list-how-pair (cdr top-cell))
		 ;; `keywords-list-how-pair' is a cons with a list of
		 ;; keywords in the car top-cell and the original how
		 ;; argument in the cdr top-cell.
		 (setcar keyword-list-how-pair
			 (delete keyword (car keyword-list-how-pair))))
	       ;; Remove keyword list/how pair when the keyword list
	       ;; is empty and how doesn't specify `set'.  (If it
	       ;; should be deleted then previously deleted keywords
	       ;; would appear again.)
	       (let ((cell top-cell))
		 (while (cdr cell)
		   (if (and (null (car (car (cdr cell))))
			    (not (eq (cdr (car (cdr cell))) 'set)))
		       (setcdr cell (cdr (cdr cell)))
		     (setq cell (cdr cell)))))
	       ;; Final cleanup, remove major mode cell if last keyword
	       ;; was deleted.
	       (if (null (cdr top-cell))
		   (setq font-lock-keywords-alist
			 (delq top-cell font-lock-keywords-alist))))
	     ;; Remember the keyword in case it is not local.
	     (let ((cell (assq mode font-lock-removed-keywords-alist)))
	       (if cell
		   (unless (member keyword (cdr cell))
		     (nconc cell (list keyword)))
		 (push (cons mode (list keyword))
		       font-lock-removed-keywords-alist))))))
	(t
	 ;; Otherwise remove it immediately.
	 (font-lock-set-defaults)
	 (let ((was-compiled (eq (car font-lock-keywords) t)))
	   ;; Bring back the user-level (uncompiled) keywords.
	   (if was-compiled
	       (setq font-lock-keywords (cadr font-lock-keywords)))

	   ;; Edit them.
	   (setq font-lock-keywords (copy-sequence font-lock-keywords))
	   (dolist (keyword keywords)
	     (setq font-lock-keywords
		   (delete keyword font-lock-keywords)))

	   ;; If the keywords were compiled before, compile them again.
	   (if was-compiled
	       (setq font-lock-keywords
                     (font-lock-compile-keywords font-lock-keywords)))))))

;;; Font Lock Support mode.

;; This is the code used to interface font-lock.el with any of its add-on
;; packages, and provide the user interface.  Packages that have their own
;; local buffer fontification functions (see below) may have to call
;; `font-lock-after-fontify-buffer' and/or `font-lock-after-unfontify-buffer'
;; themselves.

(defcustom font-lock-support-mode 'jit-lock-mode
  "Support mode for Font Lock mode.
Support modes speed up Font Lock mode by being choosy about when fontification
occurs.  The default support mode, Just-in-time Lock mode (symbol
`jit-lock-mode'), is recommended.

Other, older support modes are Fast Lock mode (symbol `fast-lock-mode') and
Lazy Lock mode (symbol `lazy-lock-mode').  See those modes for more info.
However, they are no longer recommended, as Just-in-time Lock mode is better.

If nil, means support for Font Lock mode is never performed.
If a symbol, use that support mode.
If a list, each element should be of the form (MAJOR-MODE . SUPPORT-MODE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . fast-lock-mode) (c++-mode . fast-lock-mode) (t . lazy-lock-mode))
means that Fast Lock mode is used to support Font Lock mode for buffers in C or
C++ modes, and Lazy Lock mode is used to support Font Lock mode otherwise.

The value of this variable is used when Font Lock mode is turned on."
  :type '(choice (const :tag "none" nil)
		 (const :tag "fast lock" fast-lock-mode)
		 (const :tag "lazy lock" lazy-lock-mode)
		 (const :tag "jit lock" jit-lock-mode)
		 (repeat :menu-tag "mode specific" :tag "mode specific"
			 :value ((t . jit-lock-mode))
			 (cons :tag "Instance"
			       (radio :tag "Mode"
				      (const :tag "all" t)
				      (symbol :tag "name"))
			       (radio :tag "Support"
				      (const :tag "none" nil)
				      (const :tag "fast lock" fast-lock-mode)
				      (const :tag "lazy lock" lazy-lock-mode)
				      (const :tag "JIT lock" jit-lock-mode)))
			 ))
  :version "21.1"
  :group 'font-lock)

(defvar fast-lock-mode)
(defvar lazy-lock-mode)
(defvar jit-lock-mode)

(declare-function fast-lock-after-fontify-buffer "fast-lock")
(declare-function fast-lock-after-unfontify-buffer "fast-lock")
(declare-function fast-lock-mode "fast-lock")
(declare-function lazy-lock-after-fontify-buffer "lazy-lock")
(declare-function lazy-lock-after-unfontify-buffer "lazy-lock")
(declare-function lazy-lock-mode "lazy-lock")

(defun font-lock-turn-on-thing-lock ()
  (case (font-lock-value-in-major-mode font-lock-support-mode)
    (fast-lock-mode (fast-lock-mode t))
    (lazy-lock-mode (lazy-lock-mode t))
    (jit-lock-mode
     ;; Prepare for jit-lock
     (remove-hook 'after-change-functions
                  'font-lock-after-change-function t)
     (set (make-local-variable 'font-lock-fontify-buffer-function)
          'jit-lock-refontify)
     ;; Don't fontify eagerly (and don't abort if the buffer is large).
     (set (make-local-variable 'font-lock-fontified) t)
     ;; Use jit-lock.
     (jit-lock-register 'font-lock-fontify-region
                        (not font-lock-keywords-only))
     ;; Tell jit-lock how we extend the region to refontify.
     (add-hook 'jit-lock-after-change-extend-region-functions
               'font-lock-extend-jit-lock-region-after-change
               nil t))))

(defun font-lock-turn-off-thing-lock ()
  (cond ((bound-and-true-p fast-lock-mode)
	 (fast-lock-mode -1))
	((bound-and-true-p jit-lock-mode)
	 (jit-lock-unregister 'font-lock-fontify-region)
	 ;; Reset local vars to the non-jit-lock case.
	 (kill-local-variable 'font-lock-fontify-buffer-function))
	((bound-and-true-p lazy-lock-mode)
	 (lazy-lock-mode -1))))

(defun font-lock-after-fontify-buffer ()
  (cond ((bound-and-true-p fast-lock-mode)
	 (fast-lock-after-fontify-buffer))
	;; Useless now that jit-lock intercepts font-lock-fontify-buffer.  -sm
	;; (jit-lock-mode
	;;  (jit-lock-after-fontify-buffer))
	((bound-and-true-p lazy-lock-mode)
	 (lazy-lock-after-fontify-buffer))))

(defun font-lock-after-unfontify-buffer ()
  (cond ((bound-and-true-p fast-lock-mode)
	 (fast-lock-after-unfontify-buffer))
	;; Useless as well.  It's only called when:
	;; - turning off font-lock: it does not matter if we leave spurious
	;;   `fontified' text props around since jit-lock-mode is also off.
	;; - font-lock-default-fontify-buffer fails: this is not run
	;;   any more anyway.   -sm
	;;
	;; (jit-lock-mode
	;;  (jit-lock-after-unfontify-buffer))
	((bound-and-true-p lazy-lock-mode)
	 (lazy-lock-after-unfontify-buffer))))

;;; End of Font Lock Support mode.

;;; Fontification functions.

;; Rather than the function, e.g., `font-lock-fontify-region' containing the
;; code to fontify a region, the function runs the function whose name is the
;; value of the variable, e.g., `font-lock-fontify-region-function'.  Normally,
;; the value of this variable is, e.g., `font-lock-default-fontify-region'
;; which does contain the code to fontify a region.  However, the value of the
;; variable could be anything and thus, e.g., `font-lock-fontify-region' could
;; do anything.  The indirection of the fontification functions gives major
;; modes the capability of modifying the way font-lock.el fontifies.  Major
;; modes can modify the values of, e.g., `font-lock-fontify-region-function',
;; via the variable `font-lock-defaults'.
;;
;; For example, Rmail mode sets the variable `font-lock-defaults' so that
;; font-lock.el uses its own function for buffer fontification.  This function
;; makes fontification be on a message-by-message basis and so visiting an
;; RMAIL file is much faster.  A clever implementation of the function might
;; fontify the headers differently than the message body.  (It should, and
;; correspondingly for Mail mode, but I can't be bothered to do the work.  Can
;; you?)  This hints at a more interesting use...
;;
;; Languages that contain text normally contained in different major modes
;; could define their own fontification functions that treat text differently
;; depending on its context.  For example, Perl mode could arrange that here
;; docs are fontified differently than Perl code.  Or Yacc mode could fontify
;; rules one way and C code another.  Neat!
;;
;; A further reason to use the fontification indirection feature is when the
;; default syntactic fontification, or the default fontification in general,
;; is not flexible enough for a particular major mode.  For example, perhaps
;; comments are just too hairy for `font-lock-fontify-syntactically-region' to
;; cope with.  You need to write your own version of that function, e.g.,
;; `hairy-fontify-syntactically-region', and make your own version of
;; `hairy-fontify-region' call that function before calling
;; `font-lock-fontify-keywords-region' for the normal regexp fontification
;; pass.  And Hairy mode would set `font-lock-defaults' so that font-lock.el
;; would call your region fontification function instead of its own.  For
;; example, TeX modes could fontify {\foo ...} and \bar{...}  etc. multi-line
;; directives correctly and cleanly.  (It is the same problem as fontifying
;; multi-line strings and comments; regexps are not appropriate for the job.)

(defvar font-lock-extend-after-change-region-function nil
  "A function that determines the region to refontify after a change.

This variable is either nil, or is a function that determines the
region to refontify after a change.
It is usually set by the major mode via `font-lock-defaults'.
Font-lock calls this function after each buffer change.

The function is given three parameters, the standard BEG, END, and OLD-LEN
from `after-change-functions'.  It should return either a cons of the beginning
and end buffer positions \(in that order) of the region to refontify, or nil
\(which directs the caller to fontify a default region).
This function should preserve the match-data.
The region it returns may start or end in the middle of a line.")
(make-variable-buffer-local 'font-lock-extend-after-change-region-function)

(defun font-lock-fontify-buffer ()
  "Fontify the current buffer the way the function `font-lock-mode' would."
  (interactive)
  (font-lock-set-defaults)
  (let ((font-lock-verbose (or font-lock-verbose
			       (called-interactively-p 'interactive))))
    (funcall font-lock-fontify-buffer-function)))

(defun font-lock-unfontify-buffer ()
  (funcall font-lock-unfontify-buffer-function))

(defun font-lock-fontify-region (beg end &optional loudly)
  "Fontify the text between BEG and END.
If LOUDLY is non-nil, print status messages while fontifying.
This works by calling `font-lock-fontify-region-function'."
  (font-lock-set-defaults)
  (funcall font-lock-fontify-region-function beg end loudly))

(defun font-lock-unfontify-region (beg end)
  "Unfontify the text between BEG and END.
This works by calling `font-lock-unfontify-region-function'."
  (save-buffer-state
    (funcall font-lock-unfontify-region-function beg end)))

(defun font-lock-default-fontify-buffer ()
  "Fontify the whole buffer using `font-lock-fontify-region-function'."
  (let ((verbose (if (numberp font-lock-verbose)
		     (> (buffer-size) font-lock-verbose)
		   font-lock-verbose)))
    (with-temp-message
	(when verbose
	  (format "Fontifying %s..." (buffer-name)))
      ;; Make sure we fontify etc. in the whole buffer.
      (save-restriction
	(widen)
	(condition-case nil
	    (save-excursion
	      (save-match-data
		(font-lock-fontify-region (point-min) (point-max) verbose)
		(font-lock-after-fontify-buffer)
		(setq font-lock-fontified t)))
	  ;; We don't restore the old fontification, so it's best to unfontify.
	  (quit (font-lock-unfontify-buffer)))))))

(defun font-lock-default-unfontify-buffer ()
  "Unfontify the whole buffer using `font-lock-unfontify-region-function'."
  ;; Make sure we unfontify etc. in the whole buffer.
  (save-restriction
    (widen)
    (font-lock-unfontify-region (point-min) (point-max))
    (font-lock-after-unfontify-buffer)
    (setq font-lock-fontified nil)))

(defvar font-lock-dont-widen nil
  "If non-nil, font-lock will work on the non-widened buffer.
Useful for things like RMAIL and Info where the whole buffer is not
a very meaningful entity to highlight.")


(defvar font-lock-beg) (defvar font-lock-end)
(defvar font-lock-extend-region-functions
  '(font-lock-extend-region-wholelines
    ;; This use of font-lock-multiline property is unreliable but is just
    ;; a handy heuristic: in case you don't have a function that does
    ;; /identification/ of multiline elements, you may still occasionally
    ;; discover them by accident (or you may /identify/ them but not in all
    ;; cases), in which case the font-lock-multiline property can help make
    ;; sure you will properly *re*identify them during refontification.
    font-lock-extend-region-multiline)
  "Special hook run just before proceeding to fontify a region.
This is used to allow major modes to help font-lock find safe buffer positions
as beginning and end of the fontified region.  Its most common use is to solve
the problem of /identification/ of multiline elements by providing a function
that tries to find such elements and move the boundaries such that they do
not fall in the middle of one.
Each function is called with no argument; it is expected to adjust the
dynamically bound variables `font-lock-beg' and `font-lock-end'; and return
non-nil if it did make such an adjustment.
These functions are run in turn repeatedly until they all return nil.
Put first the functions more likely to cause a change and cheaper to compute.")
;; Mark it as a special hook which doesn't use any global setting
;; (i.e. doesn't obey the element t in the buffer-local value).
(make-variable-buffer-local 'font-lock-extend-region-functions)

(defun font-lock-extend-region-multiline ()
  "Move fontification boundaries away from any `font-lock-multiline' property."
  (let ((changed nil))
    (when (and (> font-lock-beg (point-min))
               (get-text-property (1- font-lock-beg) 'font-lock-multiline))
      (setq changed t)
      (setq font-lock-beg (or (previous-single-property-change
                               font-lock-beg 'font-lock-multiline)
                              (point-min))))
    ;;
    (when (get-text-property font-lock-end 'font-lock-multiline)
      (setq changed t)
      (setq font-lock-end (or (text-property-any font-lock-end (point-max)
                                                 'font-lock-multiline nil)
                              (point-max))))
    changed))

(defun font-lock-extend-region-wholelines ()
  "Move fontification boundaries to beginning of lines."
  (let ((changed nil))
    (goto-char font-lock-beg)
    (unless (bolp)
      (setq changed t font-lock-beg (line-beginning-position)))
    (goto-char font-lock-end)
    (unless (bolp)
      (unless (eq font-lock-end
                  (setq font-lock-end (line-beginning-position 2)))
        (setq changed t)))
    changed))

(defun font-lock-default-fontify-region (beg end loudly)
  "Fontify the text between BEG and END.
If LOUDLY is non-nil, print status messages while fontifying.
This function is the default `font-lock-fontify-region-function'."
  (save-buffer-state
    ;; Use the fontification syntax table, if any.
    (with-syntax-table (or font-lock-syntax-table (syntax-table))
      (save-restriction
        (unless font-lock-dont-widen (widen))
        ;; Extend the region to fontify so that it starts and ends at
        ;; safe places.
        (let ((funs font-lock-extend-region-functions)
              (font-lock-beg beg)
              (font-lock-end end))
          (while funs
            (setq funs (if (or (not (funcall (car funs)))
                               (eq funs font-lock-extend-region-functions))
                           (cdr funs)
                         ;; If there's been a change, we should go through
                         ;; the list again since this new position may
                         ;; warrant a different answer from one of the fun
                         ;; we've already seen.
                         font-lock-extend-region-functions)))
          (setq beg font-lock-beg end font-lock-end))
        ;; Now do the fontification.
        (font-lock-unfontify-region beg end)
        (when (and font-lock-syntactic-keywords
                   (null syntax-propertize-function))
          ;; Ensure the beginning of the file is properly syntactic-fontified.
          (let ((start beg))
            (when (< font-lock-syntactically-fontified start)
              (setq start (max font-lock-syntactically-fontified (point-min)))
              (setq font-lock-syntactically-fontified end))
            (font-lock-fontify-syntactic-keywords-region start end)))
        (unless font-lock-keywords-only
          (font-lock-fontify-syntactically-region beg end loudly))
        (font-lock-fontify-keywords-region beg end loudly)))))

;; The following must be rethought, since keywords can override fontification.
;;    ;; Now scan for keywords, but not if we are inside a comment now.
;;    (or (and (not font-lock-keywords-only)
;;             (let ((state (parse-partial-sexp beg end nil nil
;;                                              font-lock-cache-state)))
;;               (or (nth 4 state) (nth 7 state))))
;;        (font-lock-fontify-keywords-region beg end))

(defvar font-lock-extra-managed-props nil
  "Additional text properties managed by font-lock.
This is used by `font-lock-default-unfontify-region' to decide
what properties to clear before refontifying a region.")

(defun font-lock-default-unfontify-region (beg end)
  "Unfontify the text between BEG and END.
This function is the default `font-lock-unfontify-region-function'."
  (remove-list-of-text-properties
   beg end (append
	    font-lock-extra-managed-props
	    (if font-lock-syntactic-keywords
		'(syntax-table face font-lock-multiline)
	      '(face font-lock-multiline)))))

;; Called when any modification is made to buffer text.
(defun font-lock-after-change-function (beg end old-len)
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
          (inhibit-quit t)
          (region (if font-lock-extend-after-change-region-function
                      (funcall font-lock-extend-after-change-region-function
                               beg end old-len))))
      (save-match-data
	(if region
	    ;; Fontify the region the major mode has specified.
	    (setq beg (car region) end (cdr region))
	  ;; Fontify the whole lines which enclose the region.
          ;; Actually, this is not needed because
          ;; font-lock-default-fontify-region already rounds up to a whole
          ;; number of lines.
	  ;; (setq beg (progn (goto-char beg) (line-beginning-position))
	  ;;       end (progn (goto-char end) (line-beginning-position 2)))
	  (unless (eq end (point-max))
	    ;; Rounding up to a whole number of lines should include the
	    ;; line right after `end'.  Typical case: the first char of
	    ;; the line was deleted.  Or a \n was inserted in the middle
	    ;; of a line.
	    (setq end (1+ end))))
	(font-lock-fontify-region beg end)))))

(defvar jit-lock-start) (defvar jit-lock-end)
(defun font-lock-extend-jit-lock-region-after-change (beg end old-len)
  "Function meant for `jit-lock-after-change-extend-region-functions'.
This function does 2 things:
- extend the region so that it not only includes the part that was modified
  but also the surrounding text whose highlighting may change as a consequence.
- anticipate (part of) the region extension that will happen later in
  `font-lock-default-fontify-region', in order to avoid the need for
  double-redisplay in `jit-lock-fontify-now'."
  (save-excursion
    ;; First extend the region as font-lock-after-change-function would.
    (let ((region (if font-lock-extend-after-change-region-function
                      (funcall font-lock-extend-after-change-region-function
                               beg end old-len))))
      (if region
          (setq beg (min jit-lock-start (car region))
                end (max jit-lock-end (cdr region))))
      ;; Then extend the region obeying font-lock-multiline properties,
      ;; indicating which part of the buffer needs to be refontified.
      ;; !!! This is the *main* user of font-lock-multiline property !!!
      ;; font-lock-after-change-function could/should also do that, but it
      ;; doesn't need to because font-lock-default-fontify-region does
      ;; it anyway.  Here OTOH we have no guarantee that
      ;; font-lock-default-fontify-region will be executed on this region
      ;; any time soon.
      ;; Note: contrary to font-lock-default-fontify-region, we do not do
      ;; any loop here because we are not looking for a safe spot: we just
      ;; mark the text whose appearance may need to change as a result of
      ;; the buffer modification.
      (when (and (> beg (point-min))
                 (get-text-property (1- beg) 'font-lock-multiline))
        (setq beg (or (previous-single-property-change
                       beg 'font-lock-multiline)
                      (point-min))))
      (when (< end (point-max))
        (setq end
              (if (get-text-property end 'font-lock-multiline)
                  (or (text-property-any end (point-max)
                                         'font-lock-multiline nil)
                      (point-max))
                ;; Rounding up to a whole number of lines should include the
                ;; line right after `end'.  Typical case: the first char of
                ;; the line was deleted.  Or a \n was inserted in the middle
                ;; of a line.
                (1+ end))))
      ;; Finally, pre-enlarge the region to a whole number of lines, to try
      ;; and anticipate what font-lock-default-fontify-region will do, so as to
      ;; avoid double-redisplay.
      ;; We could just run `font-lock-extend-region-functions', but since
      ;; the only purpose is to avoid the double-redisplay, we prefer to
      ;; do here only the part that is cheap and most likely to be useful.
      (when (memq 'font-lock-extend-region-wholelines
                  font-lock-extend-region-functions)
        (goto-char beg)
        (setq jit-lock-start (min jit-lock-start (line-beginning-position)))
        (goto-char end)
        (setq jit-lock-end
              (max jit-lock-end
                   (if (bolp) (point) (line-beginning-position 2))))))))

(defun font-lock-fontify-block (&optional arg)
  "Fontify some lines the way `font-lock-fontify-buffer' would.
The lines could be a function or paragraph, or a specified number of lines.
If ARG is given, fontify that many lines before and after point, or 16 lines if
no ARG is given and `font-lock-mark-block-function' is nil.
If `font-lock-mark-block-function' non-nil and no ARG is given, it is used to
delimit the region to fontify."
  (interactive "P")
  (let ((inhibit-point-motion-hooks t) font-lock-beginning-of-syntax-function
	deactivate-mark)
    ;; Make sure we have the right `font-lock-keywords' etc.
    (if (not font-lock-mode) (font-lock-set-defaults))
    (save-excursion
      (save-match-data
	(condition-case error-data
	    (if (or arg (not font-lock-mark-block-function))
		(let ((lines (if arg (prefix-numeric-value arg) 16)))
		  (font-lock-fontify-region
		   (save-excursion (forward-line (- lines)) (point))
		   (save-excursion (forward-line lines) (point))))
	      (funcall font-lock-mark-block-function)
	      (font-lock-fontify-region (point) (mark)))
	  ((error quit) (message "Fontifying block...%s" error-data)))))))

;;; End of Fontification functions.

;;; Additional text property functions.

;; The following text property functions should be builtins.  This means they
;; should be written in C and put with all the other text property functions.
;; In the meantime, those that are used by font-lock.el are defined in Lisp
;; below and given a `font-lock-' prefix.  Those that are not used are defined
;; in Lisp below and commented out.  sm.

(defun font-lock-prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      ;; Canonicalize old forms of face property.
      (and (memq prop '(face font-lock-face))
	   (listp prev)
	   (or (keywordp (car prev))
	       (memq (car prev) '(foreground-color background-color)))
	   (setq prev (list prev)))
      (put-text-property start next prop
			 (append val (if (listp prev) prev (list prev)))
			 object)
      (setq start next))))

(defun font-lock-append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      ;; Canonicalize old forms of face property.
      (and (memq prop '(face font-lock-face))
	   (listp prev)
	   (or (keywordp (car prev))
	       (memq (car prev) '(foreground-color background-color)))
	   (setq prev (list prev)))
      (put-text-property start next prop
			 (append (if (listp prev) prev (list prev)) val)
			 object)
      (setq start next))))

(defun font-lock-fillin-text-property (start end prop value &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end prop nil object)) next)
    (while start
      (setq next (next-single-property-change start prop object end))
      (put-text-property start next prop value object)
      (setq start (text-property-any next end prop nil object)))))

;; For completeness: this is to `remove-text-properties' as `put-text-property'
;; is to `add-text-properties', etc.
;;(defun remove-text-property (start end property &optional object)
;;  "Remove a property from text from START to END.
;;Argument PROPERTY is the property to remove.
;;Optional argument OBJECT is the string or buffer containing the text.
;;Return t if the property was actually removed, nil otherwise."
;;  (remove-text-properties start end (list property) object))

;; For consistency: maybe this should be called `remove-single-property' like
;; `next-single-property-change' (not `next-single-text-property-change'), etc.
;;(defun remove-single-text-property (start end prop value &optional object)
;;  "Remove a specific property value from text from START to END.
;;Arguments PROP and VALUE specify the property and value to remove.  The
;;resulting property values are not equal to VALUE nor lists containing VALUE.
;;Optional argument OBJECT is the string or buffer containing the text."
;;  (let ((start (text-property-not-all start end prop nil object)) next prev)
;;    (while start
;;      (setq next (next-single-property-change start prop object end)
;;	    prev (get-text-property start prop object))
;;      (cond ((and (symbolp prev) (eq value prev))
;;	     (remove-text-property start next prop object))
;;	    ((and (listp prev) (memq value prev))
;;	     (let ((new (delq value prev)))
;;	       (cond ((null new)
;;		      (remove-text-property start next prop object))
;;		     ((= (length new) 1)
;;		      (put-text-property start next prop (car new) object))
;;		     (t
;;		      (put-text-property start next prop new object))))))
;;      (setq start (text-property-not-all next end prop nil object)))))

;;; End of Additional text property functions.

;;; Syntactic regexp fontification functions.

;; These syntactic keyword pass functions are identical to those keyword pass
;; functions below, with the following exceptions; (a) they operate on
;; `font-lock-syntactic-keywords' of course, (b) they are all `defun' as speed
;; is less of an issue, (c) eval of property value does not occur JIT as speed
;; is less of an issue, (d) OVERRIDE cannot be `prepend' or `append' as it
;; makes no sense for `syntax-table' property values, (e) they do not do it
;; LOUDLY as it is not likely to be intensive.

(defun font-lock-apply-syntactic-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT,
see `font-lock-syntactic-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (value (nth 1 highlight))
	 (override (nth 2 highlight)))
    (if (not start)
	;; No match but we might not signal an error.
	(or (nth 3 highlight)
	    (error "No match %d in highlight %S" match highlight))
      (when (and (consp value) (not (numberp (car value))))
	(setq value (eval value)))
      (when (stringp value) (setq value (string-to-syntax value)))
      ;; Flush the syntax-cache.  I believe this is not necessary for
      ;; font-lock's use of syntax-ppss, but I'm not 100% sure and it can
      ;; still be necessary for other users of syntax-ppss anyway.
      (syntax-ppss-after-change-function start)
      (cond
       ((not override)
	;; Cannot override existing fontification.
	(or (text-property-not-all start end 'syntax-table nil)
	    (put-text-property start end 'syntax-table value)))
       ((eq override t)
	;; Override existing fontification.
	(put-text-property start end 'syntax-table value))
       ((eq override 'keep)
	;; Keep existing fontification.
	(font-lock-fillin-text-property start end 'syntax-table value))))))

(defun font-lock-fontify-syntactic-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords))))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (and (numberp pre-match-value) (> pre-match-value (point)))
	(setq limit pre-match-value)
      (setq limit (line-end-position)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (if (stringp matcher)
		 (re-search-forward matcher limit t)
	       (funcall matcher limit))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-syntactic-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))

(defun font-lock-fontify-syntactic-keywords-region (start end)
  "Fontify according to `font-lock-syntactic-keywords' between START and END.
START should be at the beginning of a line."
  (unless parse-sexp-lookup-properties
    ;; We wouldn't go through so much trouble if we didn't intend to use those
    ;; properties, would we?
    (set (make-local-variable 'parse-sexp-lookup-properties) t))
  ;; If `font-lock-syntactic-keywords' is a symbol, get the real keywords.
  (when (symbolp font-lock-syntactic-keywords)
    (setq font-lock-syntactic-keywords (font-lock-eval-keywords
					font-lock-syntactic-keywords)))
  ;; If `font-lock-syntactic-keywords' is not compiled, compile it.
  (unless (eq (car font-lock-syntactic-keywords) t)
    (setq font-lock-syntactic-keywords (font-lock-compile-keywords
					font-lock-syntactic-keywords
					t)))
  ;; Get down to business.
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-syntactic-keywords))
	keyword matcher highlights)
    (while keywords
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (and (< (point) end)
                  (if (stringp matcher)
                      (re-search-forward matcher end t)
                    (funcall matcher end)))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-syntactic-highlight (car highlights))
	    (font-lock-fontify-syntactic-anchored-keywords (car highlights)
							   end))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))))

;;; End of Syntactic regexp fontification functions.

;;; Syntactic fontification functions.

(defvar font-lock-comment-start-skip nil
  "If non-nil, Font Lock mode uses this instead of `comment-start-skip'.")

(defvar font-lock-comment-end-skip nil
  "If non-nil, Font Lock mode uses this instead of `comment-end'.")

(defun font-lock-fontify-syntactically-region (start end &optional loudly)
  "Put proper face on each string and comment between START and END.
START should be at the beginning of a line."
  (syntax-propertize end)  ; Apply any needed syntax-table properties.
  (let ((comment-end-regexp
	 (or font-lock-comment-end-skip
	     (regexp-quote
	      (replace-regexp-in-string "^ *" "" comment-end))))
        ;; Find the `start' state.
        (state (syntax-ppss start))
        face beg)
    (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
    ;;
    ;; Find each interesting place between here and `end'.
    (while
	(progn
	  (when (or (nth 3 state) (nth 4 state))
	    (setq face (funcall font-lock-syntactic-face-function state))
	    (setq beg (max (nth 8 state) start))
	    (setq state (parse-partial-sexp (point) end nil nil state
					    'syntax-table))
	    (when face (put-text-property beg (point) 'face face))
	    (when (and (eq face 'font-lock-comment-face)
                       (or font-lock-comment-start-skip
			   comment-start-skip))
	      ;; Find the comment delimiters
	      ;; and use font-lock-comment-delimiter-face for them.
	      (save-excursion
		(goto-char beg)
		(if (looking-at (or font-lock-comment-start-skip
				    comment-start-skip))
		    (put-text-property beg (match-end 0) 'face
				       font-lock-comment-delimiter-face)))
	      (if (looking-back comment-end-regexp (point-at-bol) t)
		  (put-text-property (match-beginning 0) (point) 'face
				     font-lock-comment-delimiter-face))))
	  (< (point) end))
      (setq state (parse-partial-sexp (point) end nil nil state
				      'syntax-table)))))

;;; End of Syntactic fontification functions.

;;; Keyword regexp fontification functions.

(defsubst font-lock-apply-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT, see `font-lock-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (override (nth 2 highlight)))
    (if (not start)
	;; No match but we might not signal an error.
	(or (nth 3 highlight)
	    (error "No match %d in highlight %S" match highlight))
      (let ((val (eval (nth 1 highlight))))
	(when (eq (car-safe val) 'face)
	  (add-text-properties start end (cddr val))
	  (setq val (cadr val)))
	(cond
	 ((not (or val (eq override t)))
	  ;; If `val' is nil, don't do anything.  It is important to do it
	  ;; explicitly, because when adding nil via things like
	  ;; font-lock-append-text-property, the property is actually
	  ;; changed from <face> to (<face>) which is undesirable.  --Stef
	  nil)
	 ((not override)
	  ;; Cannot override existing fontification.
	  (or (text-property-not-all start end 'face nil)
	      (put-text-property start end 'face val)))
	 ((eq override t)
	  ;; Override existing fontification.
	  (put-text-property start end 'face val))
	 ((eq override 'prepend)
	  ;; Prepend to existing fontification.
	  (font-lock-prepend-text-property start end 'face val))
	 ((eq override 'append)
	  ;; Append to existing fontification.
	  (font-lock-append-text-property start end 'face val))
	 ((eq override 'keep)
	  ;; Keep existing fontification.
	  (font-lock-fillin-text-property start end 'face val)))))))

(defsubst font-lock-fontify-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
	(lead-start (match-beginning 0))
	;; Evaluate PRE-MATCH-FORM.
	(pre-match-value (eval (nth 1 keywords))))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (not (and (numberp pre-match-value) (> pre-match-value (point))))
	(setq limit (line-end-position))
      (setq limit pre-match-value)
      (when (and font-lock-multiline (>= limit (line-beginning-position 2)))
	;; this is a multiline anchored match
	;; (setq font-lock-multiline t)
	(put-text-property (if (= limit (line-beginning-position 2))
			       (1- limit)
			     (min lead-start (point)))
			   limit
			   'font-lock-multiline t)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (and (< (point) limit)
		  (if (stringp matcher)
		      (re-search-forward matcher limit t)
		    (funcall matcher limit)))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (font-lock-apply-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))

(defun font-lock-fontify-keywords-region (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END.
START should be at the beginning of a line.
LOUDLY, if non-nil, allows progress-meter bar."
  (unless (eq (car font-lock-keywords) t)
    (setq font-lock-keywords
	  (font-lock-compile-keywords font-lock-keywords)))
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(keywords (cddr font-lock-keywords))
	(bufname (buffer-name)) (count 0)
        (pos (make-marker))
	keyword matcher highlights)
    ;;
    ;; Fontify each item in `font-lock-keywords' from `start' to `end'.
    (while keywords
      (if loudly (message "Fontifying %s... (regexps..%s)" bufname
			  (make-string (incf count) ?.)))
      ;;
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (and (< (point) end)
		  (if (stringp matcher)
		      (re-search-forward matcher end t)
		    (funcall matcher end))
                  ;; Beware empty string matches since they will
                  ;; loop indefinitely.
                  (or (> (point) (match-beginning 0))
                      (progn (forward-char 1) t)))
	(when (and font-lock-multiline
		   (>= (point)
		       (save-excursion (goto-char (match-beginning 0))
				       (forward-line 1) (point))))
	  ;; this is a multiline regexp match
	  ;; (setq font-lock-multiline t)
	  (put-text-property (if (= (point)
				    (save-excursion
				      (goto-char (match-beginning 0))
				      (forward-line 1) (point)))
				 (1- (point))
			       (match-beginning 0))
			     (point)
			     'font-lock-multiline t))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (font-lock-apply-highlight (car highlights))
	    (set-marker pos (point))
            (font-lock-fontify-anchored-keywords (car highlights) end)
            ;; Ensure forward progress.  `pos' is a marker because anchored
            ;; keyword may add/delete text (this happens e.g. in grep.el).
            (if (< (point) pos) (goto-char pos)))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))
    (set-marker pos nil)))

;;; End of Keyword regexp fontification functions.

;; Various functions.

(defun font-lock-compile-keywords (keywords &optional syntactic-keywords)
  "Compile KEYWORDS into the form (t KEYWORDS COMPILED...)
Here each COMPILED is of the form (MATCHER HIGHLIGHT ...) as shown in the
`font-lock-keywords' doc string.
If SYNTACTIC-KEYWORDS is non-nil, it means these keywords are used for
`font-lock-syntactic-keywords' rather than for `font-lock-keywords'."
  (if (not font-lock-set-defaults)
      ;; This should never happen.  But some external packages sometimes
      ;; call font-lock in unexpected and incorrect ways.  It's important to
      ;; stop processing at this point, otherwise we may end up changing the
      ;; global value of font-lock-keywords and break highlighting in many
      ;; other buffers.
      (error "Font-lock trying to use keywords before setting them up"))
  (if (eq (car-safe keywords) t)
      keywords
    (setq keywords
	  (cons t (cons keywords
			(mapcar 'font-lock-compile-keyword keywords))))
    (if (and (not syntactic-keywords)
	     (let ((beg-function
		    (or font-lock-beginning-of-syntax-function
			syntax-begin-function)))
	       (or (eq beg-function 'beginning-of-defun)
		   (get beg-function 'font-lock-syntax-paren-check)))
	     (not beginning-of-defun-function))
	;; Try to detect when a string or comment contains something that
	;; looks like a defun and would thus confuse font-lock.
	(nconc keywords
	       `((,(if defun-prompt-regexp
		       (concat "^\\(?:" defun-prompt-regexp "\\)?\\s(")
		     "^\\s(")
		  (0
		   (if (memq (get-text-property (match-beginning 0) 'face)
			     '(font-lock-string-face font-lock-doc-face
			       font-lock-comment-face))
		       (list 'face font-lock-warning-face
                             'help-echo "Looks like a toplevel defun: escape the parenthesis"))
		   prepend)))))
    keywords))

(defun font-lock-compile-keyword (keyword)
  (cond ((nlistp keyword)			; MATCHER
	 (list keyword '(0 font-lock-keyword-face)))
	((eq (car keyword) 'eval)		; (eval . FORM)
	 (font-lock-compile-keyword (eval (cdr keyword))))
	((eq (car-safe (cdr keyword)) 'quote)	; (MATCHER . 'FORM)
	 ;; If FORM is a FACENAME then quote it.  Otherwise ignore the quote.
	 (if (symbolp (nth 2 keyword))
	     (list (car keyword) (list 0 (cdr keyword)))
	   (font-lock-compile-keyword (cons (car keyword) (nth 2 keyword)))))
	((numberp (cdr keyword))		; (MATCHER . MATCH)
	 (list (car keyword) (list (cdr keyword) 'font-lock-keyword-face)))
	((symbolp (cdr keyword))		; (MATCHER . FACENAME)
	 (list (car keyword) (list 0 (cdr keyword))))
	((nlistp (nth 1 keyword))		; (MATCHER . HIGHLIGHT)
	 (list (car keyword) (cdr keyword)))
	(t					; (MATCHER HIGHLIGHT ...)
	 keyword)))

(defun font-lock-eval-keywords (keywords)
  "Evaluate KEYWORDS if a function (funcall) or variable (eval) name."
  (if (listp keywords)
      keywords
    (font-lock-eval-keywords (if (fboundp keywords)
				 (funcall keywords)
			       (eval keywords)))))

(defun font-lock-value-in-major-mode (alist)
  "Return value in ALIST for `major-mode', or ALIST if it is not an alist.
Structure is ((MAJOR-MODE . VALUE) ...) where MAJOR-MODE may be t."
  (if (consp alist)
      (cdr (or (assq major-mode alist) (assq t alist)))
    alist))

(defun font-lock-choose-keywords (keywords level)
  "Return LEVELth element of KEYWORDS.
A LEVEL of nil is equal to a LEVEL of 0, a LEVEL of t is equal to
\(1- (length KEYWORDS))."
  (cond ((not (and (listp keywords) (symbolp (car keywords))))
	 keywords)
	((numberp level)
	 (or (nth level keywords) (car (last keywords))))
	((eq level t)
	 (car (last keywords)))
	(t
	 (car keywords))))

(defvar font-lock-set-defaults nil)	; Whether we have set up defaults.

(defun font-lock-refresh-defaults ()
  "Restart fontification in current buffer after recomputing from defaults.
Recompute fontification variables using `font-lock-defaults' and
`font-lock-maximum-decoration'.  Then restart fontification.

Use this function when you have changed any of the above
variables directly.

Note: This function will erase modifications done by
`font-lock-add-keywords' or `font-lock-remove-keywords', but will
preserve `hi-lock-mode' highlighting patterns."
  (font-lock-mode -1)
  (kill-local-variable 'font-lock-set-defaults)
  (font-lock-mode 1))

(defvar font-lock-major-mode nil
  "Major mode for which the font-lock settings have been setup.")
(make-variable-buffer-local 'font-lock-major-mode)

(defun font-lock-set-defaults ()
  "Set fontification defaults appropriately for this mode.
Sets various variables using `font-lock-defaults' and
`font-lock-maximum-decoration'."
  ;; Set fontification defaults if not previously set for correct major mode.
  (unless (and font-lock-set-defaults
	       (eq font-lock-major-mode major-mode))
    (setq font-lock-major-mode major-mode)
    (set (make-local-variable 'font-lock-set-defaults) t)
    (make-local-variable 'font-lock-fontified)
    (make-local-variable 'font-lock-multiline)
    (let* ((defaults font-lock-defaults)
	   (keywords
	    (font-lock-choose-keywords (nth 0 defaults)
				       (font-lock-value-in-major-mode font-lock-maximum-decoration)))
	   (local (cdr (assq major-mode font-lock-keywords-alist)))
	   (removed-keywords
	    (cdr-safe (assq major-mode font-lock-removed-keywords-alist))))
      (set (make-local-variable 'font-lock-defaults) defaults)
      ;; Syntactic fontification?
      (if (nth 1 defaults)
          (set (make-local-variable 'font-lock-keywords-only) t)
        (kill-local-variable 'font-lock-keywords-only))
      ;; Case fold during regexp fontification?
      (if (nth 2 defaults)
          (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
        (kill-local-variable 'font-lock-keywords-case-fold-search))
      ;; Syntax table for regexp and syntactic fontification?
      (if (null (nth 3 defaults))
          (kill-local-variable 'font-lock-syntax-table)
	(set (make-local-variable 'font-lock-syntax-table)
	     (copy-syntax-table (syntax-table)))
	(dolist (selem (nth 3 defaults))
	  ;; The character to modify may be a single CHAR or a STRING.
	  (let ((syntax (cdr selem)))
	    (dolist (char (if (numberp (car selem))
			      (list (car selem))
			    (mapcar 'identity (car selem))))
	      (modify-syntax-entry char syntax font-lock-syntax-table)))))
      ;; Syntax function for syntactic fontification?
      (if (nth 4 defaults)
	(set (make-local-variable 'font-lock-beginning-of-syntax-function)
               (nth 4 defaults))
        (kill-local-variable 'font-lock-beginning-of-syntax-function))
      ;; Variable alist?
      (dolist (x (nthcdr 5 defaults))
	(set (make-local-variable (car x)) (cdr x)))
      ;; Set up `font-lock-keywords' last because its value might depend
      ;; on other settings (e.g. font-lock-compile-keywords uses
      ;; font-lock-beginning-of-syntax-function).
      (set (make-local-variable 'font-lock-keywords)
	   (font-lock-eval-keywords keywords))
      ;; Local fontification?
      (while local
	(font-lock-add-keywords nil (car (car local)) (cdr (car local)))
	(setq local (cdr local)))
      (when removed-keywords
	(font-lock-remove-keywords nil removed-keywords))
      ;; Now compile the keywords.
      (unless (eq (car font-lock-keywords) t)
	(setq font-lock-keywords
              (font-lock-compile-keywords font-lock-keywords))))))

;;; Color etc. support.

;; Note that `defface' will not overwrite any faces declared above via
;; `custom-declare-face'.
(defface font-lock-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow"))
    (t (:weight bold :slant italic)))
  "Font Lock mode face used to highlight comments."
  :group 'font-lock-faces)

(defface font-lock-comment-delimiter-face
  '((default :inherit font-lock-comment-face))
  "Font Lock mode face used to highlight comment delimiters."
  :group 'font-lock-faces)

(defface font-lock-string-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark)) (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Font Lock mode face used to highlight strings."
  :group 'font-lock-faces)

(defface font-lock-doc-face
  '((t :inherit font-lock-string-face))
  "Font Lock mode face used to highlight documentation."
  :group 'font-lock-faces)

(defface font-lock-keyword-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords."
  :group 'font-lock-faces)

(defface font-lock-builtin-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:weight bold)))
  "Font Lock mode face used to highlight builtins."
  :group 'font-lock-faces)

(defface font-lock-function-name-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:foreground "blue" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-faces)

(defface font-lock-variable-name-face
  '((((class grayscale) (background light))
     (:foreground "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light)) (:foreground "sienna"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight light))
    (t (:weight bold :slant italic)))
  "Font Lock mode face used to highlight variable names."
  :group 'font-lock-faces)

(defface font-lock-type-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Font Lock mode face used to highlight type and classes."
  :group 'font-lock-faces)

(defface font-lock-constant-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light)) (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Font Lock mode face used to highlight constants and labels."
  :group 'font-lock-faces)

(defface font-lock-warning-face
  '((t :inherit error))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-faces)

(defface font-lock-negation-char-face
  '((t nil))
  "Font Lock mode face used to highlight easy to overlook negation."
  :group 'font-lock-faces)

(defface font-lock-preprocessor-face
  '((t :inherit font-lock-builtin-face))
  "Font Lock mode face used to highlight preprocessor directives."
  :group 'font-lock-faces)

(defface font-lock-regexp-grouping-backslash
  '((t :inherit bold))
  "Font Lock mode face for backslashes in Lisp regexp grouping constructs."
  :group 'font-lock-faces)

(defface font-lock-regexp-grouping-construct
  '((t :inherit bold))
  "Font Lock mode face used to highlight grouping constructs in Lisp regexps."
  :group 'font-lock-faces)

;;; End of Color etc. support.

;;; Menu support.

;; This section of code is commented out because Emacs does not have real menu
;; buttons.  (We can mimic them by putting "( ) " or "(X) " at the beginning of
;; the menu entry text, but with Xt it looks both ugly and embarrassingly
;; amateur.)  If/When Emacs gets real menus buttons, put in menu-bar.el after
;; the entry for "Text Properties" something like:
;;
;; (define-key menu-bar-edit-menu [font-lock]
;;   (cons "Syntax Highlighting" font-lock-menu))
;;
;; and remove a single ";" from the beginning of each line in the rest of this
;; section.  Probably the mechanism for telling the menu code what are menu
;; buttons and when they are on or off needs tweaking.  I have assumed that the
;; mechanism is via `menu-toggle' and `menu-selected' symbol properties.  sm.

;;;;;###autoload
;;(progn
;;  ;; Make the Font Lock menu.
;;  (defvar font-lock-menu (make-sparse-keymap "Syntax Highlighting"))
;;  ;; Add the menu items in reverse order.
;;  (define-key font-lock-menu [fontify-less]
;;    '("Less In Current Buffer" . font-lock-fontify-less))
;;  (define-key font-lock-menu [fontify-more]
;;    '("More In Current Buffer" . font-lock-fontify-more))
;;  (define-key font-lock-menu [font-lock-sep]
;;    '("--"))
;;  (define-key font-lock-menu [font-lock-mode]
;;    '("In Current Buffer" . font-lock-mode))
;;  (define-key font-lock-menu [global-font-lock-mode]
;;    '("In All Buffers" . global-font-lock-mode)))
;;
;;;;;###autoload
;;(progn
;;  ;; We put the appropriate `menu-enable' etc. symbol property values on when
;;  ;; font-lock.el is loaded, so we don't need to autoload the three variables.
;;  (put 'global-font-lock-mode 'menu-toggle t)
;;  (put 'font-lock-mode 'menu-toggle t)
;;  (put 'font-lock-fontify-more 'menu-enable '(identity))
;;  (put 'font-lock-fontify-less 'menu-enable '(identity)))
;;
;; ;; Put the appropriate symbol property values on now.  See above.
;;(put 'global-font-lock-mode 'menu-selected 'global-font-lock-mode)
;;(put 'font-lock-mode 'menu-selected 'font-lock-mode)
;;(put 'font-lock-fontify-more 'menu-enable '(nth 2 font-lock-fontify-level))
;;(put 'font-lock-fontify-less 'menu-enable '(nth 1 font-lock-fontify-level))
;;
;;(defvar font-lock-fontify-level nil)	; For less/more fontification.
;;
;;(defun font-lock-fontify-level (level)
;;  (let ((font-lock-maximum-decoration level))
;;    (when font-lock-mode
;;      (font-lock-mode))
;;    (font-lock-mode)
;;    (when font-lock-verbose
;;      (message "Fontifying %s... level %d" (buffer-name) level))))
;;
;;(defun font-lock-fontify-less ()
;;  "Fontify the current buffer with less decoration.
;;See `font-lock-maximum-decoration'."
;;  (interactive)
;;  ;; Check in case we get called interactively.
;;  (if (nth 1 font-lock-fontify-level)
;;      (font-lock-fontify-level (1- (car font-lock-fontify-level)))
;;    (error "No less decoration")))
;;
;;(defun font-lock-fontify-more ()
;;  "Fontify the current buffer with more decoration.
;;See `font-lock-maximum-decoration'."
;;  (interactive)
;;  ;; Check in case we get called interactively.
;;  (if (nth 2 font-lock-fontify-level)
;;      (font-lock-fontify-level (1+ (car font-lock-fontify-level)))
;;    (error "No more decoration")))
;;
;; ;; This should be called by `font-lock-set-defaults'.
;;(defun font-lock-set-menu ()
;;  ;; Activate less/more fontification entries if there are multiple levels for
;;  ;; the current buffer.  Sets `font-lock-fontify-level' to be of the form
;;  ;; (CURRENT-LEVEL IS-LOWER-LEVEL-P IS-HIGHER-LEVEL-P) for menu activation.
;;  (let ((keywords (nth 0 font-lock-defaults))
;;	(level (font-lock-value-in-major-mode font-lock-maximum-decoration)))
;;    (make-local-variable 'font-lock-fontify-level)
;;    (if (or (symbolp keywords) (= (length keywords) 1))
;;	(font-lock-unset-menu)
;;      (cond ((eq level t)
;;	     (setq level (1- (length keywords))))
;;	    ((or (null level) (zerop level))
;;	     ;; The default level is usually, but not necessarily, level 1.
;;	     (setq level (- (length keywords)
;;			    (length (member (eval (car keywords))
;;					    (mapcar 'eval (cdr keywords))))))))
;;      (setq font-lock-fontify-level (list level (> level 1)
;;					  (< level (1- (length keywords))))))))
;;
;; ;; This should be called by `font-lock-unset-defaults'.
;;(defun font-lock-unset-menu ()
;;  ;; Deactivate less/more fontification entries.
;;  (setq font-lock-fontify-level nil))

;;; End of Menu support.

;;; Various regexp information shared by several modes.
;; ;; Information specific to a single mode should go in its load library.

;; Font Lock support for C, C++, Objective-C and Java modes is now in
;; cc-fonts.el (and required by cc-mode.el).  However, the below function
;; should stay in font-lock.el, since it is used by other libraries.  sm.

(defun font-lock-match-c-style-declaration-item-and-skip-to-next (limit)
  "Match, and move over, any declaration/definition item after point.
Matches after point, but ignores leading whitespace and `*' characters.
Does not move further than LIMIT.

The expected syntax of a declaration/definition item is `word' (preceded by
optional whitespace and `*' characters and proceeded by optional whitespace)
optionally followed by a `('.  Everything following the item (but belonging to
it) is expected to be skip-able by `scan-sexps', and items are expected to be
separated with a `,' and to be terminated with a `;'.

Thus the regexp matches after point:	word (
					^^^^ ^
Where the match subexpressions are:	  1  2

The item is delimited by (match-beginning 1) and (match-end 1).
If (match-beginning 2) is non-nil, the item is followed by a `('.

This function could be MATCHER in a MATCH-ANCHORED `font-lock-keywords' item."
  (when (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?")
    (when (and (match-end 2) (> (- (match-end 2) (match-beginning 2)) 1))
      ;; If `word' is followed by a double open-paren, it's probably
      ;; a macro used for "int myfun P_ ((int arg1))".  Let's go back one
      ;; word to try and match `myfun' rather than `P_'.
      (let ((pos (point)))
	(skip-chars-backward " \t\n")
	(skip-syntax-backward "w")
	(unless (looking-at "\\(\\sw+\\)[ \t\n]*\\sw+[ \t\n]*\\(((?\\)?")
	  ;; Looks like it was something else, so go back to where we
	  ;; were and reset the match data by rematching.
	  (goto-char pos)
	  (looking-at "[ \n\t*]*\\(\\sw+\\)[ \t\n]*\\(((?\\)?"))))
    (save-match-data
      (condition-case nil
	  (save-restriction
	    ;; Restrict to the LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    ;; Move over any item value, etc., to the next item.
	    (while (not (looking-at "[ \t\n]*\\(\\(,\\)\\|;\\|\\'\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
	    (if (match-end 2)
		(goto-char (match-end 2))))
	(error t)))))

;; C preprocessor(cpp) is used outside of C, C++ and Objective-C source file.
;; e.g. assembler code and GNU linker script in Linux kernel.
;; `cpp-font-lock-keywords' is handy for modes for the files.
;;
;; Here we cannot use `regexp-opt' because because regex-opt is not preloaded
;; while font-lock.el is preloaded to emacs. So values pre-calculated with
;; regexp-opt are used here.

;; `cpp-font-lock-keywords-source-directives' is calculated from:
;;
;;	    (regexp-opt
;;	     '("define"  "elif" "else" "endif" "error" "file" "if" "ifdef"
;;	       "ifndef" "import" "include" "line" "pragma" "undef" "warning"))
;;
(defconst cpp-font-lock-keywords-source-directives
  "define\\|e\\(?:l\\(?:if\\|se\\)\\|ndif\\|rror\\)\\|file\\|i\\(?:f\\(?:n?def\\)?\\|mport\\|nclude\\)\\|line\\|pragma\\|undef\\|warning"
  "Regular expression used in `cpp-font-lock-keywords'.")

;; `cpp-font-lock-keywords-source-depth' is calculated from:
;;
;;          (regexp-opt-depth (regexp-opt
;;		       '("define"  "elif" "else" "endif" "error" "file" "if" "ifdef"
;;			 "ifndef" "import" "include" "line" "pragma" "undef" "warning")))
;;
(defconst cpp-font-lock-keywords-source-depth 0
  "An integer representing regular expression depth of `cpp-font-lock-keywords-source-directives'.
Used in `cpp-font-lock-keywords'.")

(defconst cpp-font-lock-keywords
  (let* ((directives cpp-font-lock-keywords-source-directives)
	 (directives-depth cpp-font-lock-keywords-source-depth))
    (list
     ;;
     ;; Fontify error directives.
     '("^#[ \t]*\\(?:error\\|warning\\)[ \t]+\\(.+\\)" 1 font-lock-warning-face prepend)
     ;;
     ;; Fontify filenames in #include <...> preprocessor directives as strings.
     '("^#[ \t]*\\(?:import\\|include\\)[ \t]*\\(<[^>\"\n]*>?\\)"
       1 font-lock-string-face prepend)
     ;;
     ;; Fontify function macro names.
     '("^#[ \t]*define[ \t]+\\([[:alpha:]_][[:alnum:]_$]*\\)("
       (1 font-lock-function-name-face prepend)
       ;;
       ;; Macro arguments.
       ((lambda (limit)
	  (re-search-forward
	   "\\(?:\\([[:alpha:]_][[:alnum:]_]*\\)[,]?\\)"
	   (or (save-excursion (re-search-forward ")" limit t))
	       limit)
	   t))
	nil nil (1 font-lock-variable-name-face prepend)))
     ;;
     ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
     '("^#[ \t]*\\(?:elif\\|if\\)\\>"
       ("\\<\\(defined\\)\\>[ \t]*(?\\([[:alpha:]_][[:alnum:]_]*\\)?" nil nil
	(1 font-lock-builtin-face prepend) (2 font-lock-variable-name-face prepend t)))
     ;;
     ;; Fontify otherwise as symbol names, and the preprocessor directive names.
     (list
      (concat "^\\(#[ \t]*\\(?:" directives
	      "\\)\\)\\>[ \t!]*\\([[:alpha:]_][[:alnum:]_]*\\)?")
      '(1 font-lock-preprocessor-face prepend)
      (list (+ 2 directives-depth)
	    'font-lock-variable-name-face nil t))))
  "Font lock keywords for C preprocessor directives.
`c-mode', `c++-mode' and `objc-mode' have their own font lock keywords
for C preprocessor directives.  This definition is for the other modes
in which C preprocessor directives are used. e.g. `asm-mode' and
`ld-script-mode'.")


;; Lisp.

(defconst lisp-font-lock-keywords-1
  (eval-when-compile
    `(;; Definitions.
      (,(concat "(\\(def\\("
		;; Function declarations.
		"\\(advice\\|alias\\|generic\\|macro\\*?\\|method\\|"
		"setf\\|subst\\*?\\|un\\*?\\|"
		"ine-\\(condition\\|"
		"\\(?:derived\\|\\(?:global\\(?:ized\\)?-\\)?minor\\|generic\\)-mode\\|"
		"method-combination\\|setf-expander\\|skeleton\\|widget\\|"
		"function\\|\\(compiler\\|modify\\|symbol\\)-macro\\)\\)\\|"
		;; Variable declarations.
		"\\(const\\(ant\\)?\\|custom\\|varalias\\|face\\|parameter\\|var\\)\\|"
		;; Structure declarations.
		"\\(class\\|group\\|theme\\|package\\|struct\\|type\\)"
		"\\)\\)\\>"
		;; Any whitespace and defined object.
		"[ \t'\(]*"
		"\\(setf[ \t]+\\sw+\\|\\sw+\\)?")
       (1 font-lock-keyword-face)
       (9 (cond ((match-beginning 3) font-lock-function-name-face)
		((match-beginning 6) font-lock-variable-name-face)
		(t font-lock-type-face))
	  nil t))
      ;; Emacs Lisp autoload cookies.  Supports the slightly different
      ;; forms used by mh-e, calendar, etc.
      ("^;;;###\\([-a-z]*autoload\\)" 1 font-lock-warning-face prepend)
      ;; Regexp negated char group.
      ("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)))
  "Subdued level highlighting for Lisp modes.")

(defconst lisp-font-lock-keywords-2
  (append lisp-font-lock-keywords-1
   (eval-when-compile
     `(;; Control structures.  Emacs Lisp forms.
       (,(concat
	  "(" (regexp-opt
	       '("cond" "if" "while" "while-no-input" "let" "let*" "letrec"
		 "prog" "progn" "progv" "prog1" "prog2" "prog*"
		 "inline" "lambda" "save-restriction" "save-excursion"
		 "save-selected-window" "save-window-excursion"
		 "save-match-data" "save-current-buffer"
		 "combine-after-change-calls" "unwind-protect"
		 "condition-case" "condition-case-unless-debug"
		 "track-mouse" "eval-after-load" "eval-and-compile"
		 "eval-when-compile" "eval-when" "eval-next-after-load"
		 "with-case-table" "with-category-table"
		 "with-current-buffer" "with-demoted-errors"
		 "with-electric-help"
		 "with-local-quit" "with-no-warnings"
		 "with-output-to-string" "with-output-to-temp-buffer"
		 "with-selected-window" "with-selected-frame"
		 "with-silent-modifications" "with-syntax-table"
		 "with-temp-buffer" "with-temp-file" "with-temp-message"
		 "with-timeout" "with-timeout-handler" "with-wrapper-hook") t)
	  "\\>")
	  .  1)
       ;; Control structures.  Common Lisp forms.
       (,(concat
	  "(" (regexp-opt
	       '("when" "unless" "case" "ecase" "typecase" "etypecase"
		 "ccase" "ctypecase" "handler-case" "handler-bind"
		 "restart-bind" "restart-case" "in-package"
		 "break" "ignore-errors"
		 "loop" "do" "do*" "dotimes" "dolist" "the" "locally"
		 "proclaim" "declaim" "declare" "symbol-macrolet" "letf"
		 "lexical-let" "lexical-let*" "flet" "labels" "compiler-let"
		 "destructuring-bind" "macrolet" "tagbody" "block" "go"
		 "multiple-value-bind" "multiple-value-prog1"
		 "return" "return-from"
		 "with-accessors" "with-compilation-unit"
		 "with-condition-restarts" "with-hash-table-iterator"
		 "with-input-from-string" "with-open-file"
		 "with-open-stream" "with-output-to-string"
		 "with-package-iterator" "with-simple-restart"
		 "with-slots" "with-standard-io-syntax") t)
	  "\\>")
	  . 1)
       ;; Exit/Feature symbols as constants.
       (,(concat "(\\(catch\\|throw\\|featurep\\|provide\\|require\\)\\>"
		 "[ \t']*\\(\\sw+\\)?")
	(1 font-lock-keyword-face)
	(2 font-lock-constant-face nil t))
       ;; Erroneous structures.
       ("(\\(abort\\|assert\\|warn\\|check-type\\|cerror\\|error\\|signal\\)\\>" 1 font-lock-warning-face)
       ;; Words inside \\[] tend to be for `substitute-command-keys'.
       ("\\\\\\\\\\[\\(\\sw+\\)\\]" 1 font-lock-constant-face prepend)
       ;; Words inside `' tend to be symbol names.
       ("`\\(\\sw\\sw+\\)'" 1 font-lock-constant-face prepend)
       ;; Constant values.
       ("\\<:\\sw+\\>" 0 font-lock-builtin-face)
       ;; ELisp and CLisp `&' keywords as types.
       ("\\<\\&\\sw+\\>" . font-lock-type-face)
       ;; ELisp regexp grouping constructs
       ((lambda (bound)
          (catch 'found
            ;; The following loop is needed to continue searching after matches
            ;; that do not occur in strings.  The associated regexp matches one
            ;; of `\\\\' `\\(' `\\(?:' `\\|' `\\)'.  `\\\\' has been included to
            ;; avoid highlighting, for example, `\\(' in `\\\\('.
            (while (re-search-forward "\\(\\\\\\\\\\)\\(?:\\(\\\\\\\\\\)\\|\\((\\(?:\\?[0-9]*:\\)?\\|[|)]\\)\\)" bound t)
              (unless (match-beginning 2)
                (let ((face (get-text-property (1- (point)) 'face)))
                  (when (or (and (listp face)
                                 (memq 'font-lock-string-face face))
                            (eq 'font-lock-string-face face))
                    (throw 'found t)))))))
        (1 'font-lock-regexp-grouping-backslash prepend)
        (3 'font-lock-regexp-grouping-construct prepend))
;;;  This is too general -- rms.
;;;  A user complained that he has functions whose names start with `do'
;;;  and that they get the wrong color.
;;;      ;; CL `with-' and `do-' constructs
;;;      ("(\\(\\(do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
      )))
  "Gaudy level highlighting for Lisp modes.")

(defvar lisp-font-lock-keywords lisp-font-lock-keywords-1
  "Default expressions to highlight in Lisp modes.")

(provide 'font-lock)

;;; font-lock.el ends here
