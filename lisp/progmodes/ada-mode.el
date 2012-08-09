;;; ada-mode.el --- major-mode for editing Ada sources

;; Copyright (C) 1994-1995, 1997-2012  Free Software Foundation, Inc.

;; Author: Rolf Ebert      <ebert@inf.enst.fr>
;;      Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;      Emmanuel Briot  <briot@gnat.com>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada

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
;; This mode is a major mode for editing Ada code.  This is a major
;; rewrite of the file packaged with Emacs-20.  The Ada mode is
;; composed of four Lisp files: ada-mode.el, ada-xref.el, ada-prj.el
;; and ada-stmt.el.  Only this file (ada-mode.el) is completely
;; independent from the GNU Ada compiler GNAT, distributed by Ada
;; Core Technologies.  All the other files rely heavily on features
;; provided only by GNAT.
;;
;; Note: this mode will not work with Emacs 19. If you are on a VMS
;; system, where the latest version of Emacs is 19.28, you will need
;; another file, called ada-vms.el, that provides some required
;; functions.

;;; Usage:
;; Emacs should enter Ada mode automatically when you load an Ada file.
;; By default, the valid extensions for Ada files are .ads, .adb or .ada
;; If the ada-mode does not start automatically, then simply type the
;; following command :
;;     M-x ada-mode
;;
;; By default, ada-mode is configured to take full advantage of the GNAT
;; compiler (the menus will include the cross-referencing features,...).
;; If you are using another compiler, you might want to set the following
;; variable in your .emacs (Note: do not set this in the ada-mode-hook, it
;; won't work) :
;;    (setq ada-which-compiler 'generic)
;;
;; This mode requires find-file.el to be present on your system.

;;; History:
;; The first Ada mode for GNU Emacs was written by V. Broman in
;; 1985. He based his work on the already existing Modula-2 mode.
;; This was distributed as ada.el in versions of Emacs prior to 19.29.
;;
;; Lynn Slater wrote an extensive Ada mode in 1989. It consisted of
;; several files with support for dired commands and other nice
;; things. It is currently available from the PAL
;; (wuarchive.wustl.edu:/languages/ada) as ada-mode-1.06a.tar.Z.
;;
;; The probably very first Ada mode (called electric-ada.el) was
;; written by Steven D. Litvintchouk and Steven M. Rosen for the
;; Gosling Emacs. L. Slater based his development on ada.el and
;; electric-ada.el.
;;
;; A complete rewrite by M. Heritsch and R. Ebert has been done.
;; Some ideas from the Ada mode mailing list have been
;; added.  Some of the functionality of L. Slater's mode has not
;; (yet) been recoded in this new mode.  Perhaps you prefer sticking
;; to his version.
;;
;; A complete rewrite for Emacs-20 / GNAT-3.11 has been done by Ada Core
;; Technologies.

;;; Credits:
;;   Many thanks to John McCabe <john@assen.demon.co.uk> for sending so
;;     many patches included in this package.
;;   Christian Egli <Christian.Egli@hcsd.hac.com>:
;;     ada-imenu-generic-expression
;;   Many thanks also to the following persons that have contributed
;;   to the ada-mode
;;     Philippe Waroquiers (PW) <philippe@cfmu.eurocontrol.be> in particular,
;;     woodruff@stc.llnl.gov (John Woodruff)
;;     jj@ddci.dk (Jesper Joergensen)
;;     gse@ocsystems.com (Scott Evans)
;;     comar@gnat.com (Cyrille Comar)
;;     stephen.leake@gsfc.nasa.gov (Stephen Leake)
;;     robin-reply@reagans.org
;;    and others for their valuable hints.

;;; Code:
;; Note: Every function in this package is compiler-independent.
;; The names start with  ada-
;; The variables that the user can edit can all be modified through
;;   the customize mode. They are sorted in alphabetical order in this
;;   file.

;; Supported packages.
;; This package supports a number of other Emacs modes. These other modes
;; should be loaded before the ada-mode, which will then setup some variables
;; to improve the support for Ada code.
;; Here is the list of these modes:
;;   `which-function-mode': Display in the modeline the name of the subprogram
;;      the cursor is in.
;;   `outline-mode': Provides the capability to collapse or expand the code
;;      for specific language constructs, for instance if you want to hide the
;;      code corresponding to a subprogram
;;   `align': This mode is now provided with Emacs 21, but can also be
;;      installed manually for older versions of Emacs. It provides the
;;      capability to automatically realign the selected region (for instance
;;      all ':=', ':' and '--' will be aligned on top of each other.
;;   `imenu': Provides a menu with the list of entities defined in the current
;;      buffer, and an easy way to jump to any of them
;;   `speedbar': Provides a separate file browser, and the capability for each
;;      file to see the list of entities defined in it and to jump to them
;;      easily
;;   `abbrev-mode': Provides the capability to define abbreviations, which
;;      are automatically expanded when you type them. See the Emacs manual.

(require 'find-file nil t)
(require 'align nil t)
(require 'which-func nil t)
(require 'compile nil t)

(defvar ispell-check-comments)
(defvar skeleton-further-elements)

(defun ada-mode-version ()
  "Return Ada mode version."
  (interactive)
  (let ((version-string "4.00"))
    (if (called-interactively-p 'interactive)
	(message version-string)
      version-string)))

(defvar ada-mode-hook nil
  "*List of functions to call when Ada mode is invoked.
This hook is automatically executed after the `ada-mode' is
fully loaded.
This is a good place to add Ada environment specific bindings.")

(defgroup ada nil
  "Major mode for editing and compiling Ada source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom ada-auto-case t
  "*Non-nil means automatically change case of preceding word while typing.
Casing is done according to `ada-case-keyword', `ada-case-identifier'
and `ada-case-attribute'."
  :type 'boolean :group 'ada)

(defcustom ada-broken-decl-indent 0
  "*Number of columns to indent a broken declaration.

An example is :
  declare
     A,
     >>>>>B : Integer;"
  :type 'integer :group 'ada)

(defcustom ada-broken-indent 2
  "*Number of columns to indent the continuation of a broken line.

An example is :
   My_Var : My_Type := (Field1 =>
			>>>>>>>>>Value);"
  :type 'integer :group 'ada)

(defcustom ada-continuation-indent ada-broken-indent
  "*Number of columns to indent the continuation of broken lines in parenthesis.

An example is :
   Func (Param1,
	 >>>>>Param2);"
  :type 'integer :group 'ada)

(defcustom ada-case-attribute 'ada-capitalize-word
  "*Function to call to adjust the case of Ada attributes.
It may be `downcase-word', `upcase-word', `ada-loose-case-word',
`ada-capitalize-word' or `ada-no-auto-case'."
  :type '(choice (const downcase-word)
		 (const upcase-word)
		 (const ada-capitalize-word)
		 (const ada-loose-case-word)
		 (const ada-no-auto-case))
  :group 'ada)

(defcustom ada-case-exception-file
  (list (convert-standard-filename' "~/.emacs_case_exceptions"))
  "*List of special casing exceptions dictionaries for identifiers.
The first file is the one where new exceptions will be saved by Emacs
when you call `ada-create-case-exception'.

These files should contain one word per line, that gives the casing
to be used for that word in Ada files.  If the line starts with the
character *, then the exception will be used for substrings that either
start at the beginning of a word or after a _ character, and end either
at the end of the word or at a _ character.  Each line can be terminated
by a comment."
  :type '(repeat (file))
  :group 'ada)

(defcustom ada-case-keyword 'downcase-word
  "*Function to call to adjust the case of an Ada keywords.
It may be `downcase-word', `upcase-word', `ada-loose-case-word' or
`ada-capitalize-word'."
  :type '(choice (const downcase-word)
		 (const upcase-word)
		 (const ada-capitalize-word)
		 (const ada-loose-case-word)
		 (const ada-no-auto-case))
  :group 'ada)

(defcustom ada-case-identifier 'ada-loose-case-word
  "*Function to call to adjust the case of an Ada identifier.
It may be `downcase-word', `upcase-word', `ada-loose-case-word' or
`ada-capitalize-word'."
  :type '(choice (const downcase-word)
		 (const upcase-word)
		 (const ada-capitalize-word)
		 (const ada-loose-case-word)
		 (const ada-no-auto-case))
  :group 'ada)

(defcustom ada-clean-buffer-before-saving t
  "*Non-nil means remove trailing spaces and untabify the buffer before saving."
  :type 'boolean :group 'ada)
(make-obsolete-variable 'ada-clean-buffer-before-saving
			"use the `write-file-functions' hook."
			"23.2")


(defcustom ada-indent 3
  "*Size of Ada indentation.

An example is :
procedure Foo is
begin
>>>>>>>>>>null;"
  :type 'integer  :group 'ada)

(defcustom ada-indent-after-return t
  "*Non-nil means automatically indent after RET or LFD."
  :type 'boolean :group 'ada)

(defcustom ada-indent-align-comments t
  "*Non-nil means align comments on previous line comments, if any.
If nil, indentation is calculated as usual.
Note that indentation is calculated only if `ada-indent-comment-as-code' is t.

For instance:
    A := 1;   --  A multi-line comment
	      --  aligned if `ada-indent-align-comments' is t"
  :type 'boolean :group 'ada)

(defcustom ada-indent-comment-as-code t
  "*Non-nil means indent comment lines as code.
A nil value means do not auto-indent comments."
  :type 'boolean :group 'ada)

(defcustom ada-indent-handle-comment-special nil
  "*Non-nil if comment lines should be handled specially inside parenthesis.
By default, if the line that contains the open parenthesis has some
text following it, then the following lines will be indented in the
same column as this text.  This will not be true if the first line is
a comment and `ada-indent-handle-comment-special' is t.

type A is
  (   Value_1,    --  common behavior, when not a comment
      Value_2);

type A is
  (   --  `ada-indent-handle-comment-special' is nil
      Value_1,
      Value_2);

type A is
  (   --  `ada-indent-handle-comment-special' is non-nil
   Value_1,
   Value_2);"
  :type 'boolean :group 'ada)

(defcustom ada-indent-is-separate t
  "*Non-nil means indent 'is separate' or 'is abstract' if on a single line."
  :type 'boolean :group 'ada)

(defcustom ada-indent-record-rel-type 3
  "*Indentation for 'record' relative to 'type' or 'use'.

An example is:
   type A is
   >>>>>>>>>>>record"
  :type 'integer :group 'ada)

(defcustom ada-indent-renames ada-broken-indent
  "*Indentation for renames relative to the matching function statement.
If `ada-indent-return' is null or negative, the indentation is done relative to
the open parenthesis (if there is no parenthesis, `ada-broken-indent' is used).

An example is:
   function A (B : Integer)
       return C;
   >>>renames Foo;"
  :type 'integer :group 'ada)

(defcustom ada-indent-return 0
  "*Indentation for 'return' relative to the matching 'function' statement.
If `ada-indent-return' is null or negative, the indentation is done relative to
the open parenthesis (if there is no parenthesis, `ada-broken-indent' is used).

An example is:
   function A (B : Integer)
   >>>>>return C;"
  :type 'integer :group 'ada)

(defcustom ada-indent-to-open-paren t
  "*Non-nil means indent according to the innermost open parenthesis."
  :type 'boolean :group 'ada)

(defcustom ada-fill-comment-prefix "--  "
  "*Text inserted in the first columns when filling a comment paragraph.
Note: if you modify this variable, you will have to invoke `ada-mode'
again to take account of the new value."
  :type 'string :group 'ada)

(defcustom ada-fill-comment-postfix " --"
  "*Text inserted at the end of each line when filling a comment paragraph.
Used by `ada-fill-comment-paragraph-postfix'."
  :type 'string :group 'ada)

(defcustom ada-label-indent -4
  "*Number of columns to indent a label.

An example is:
procedure Foo is
begin
>>>>Label:

This is also used for <<..>> labels"
  :type 'integer :group 'ada)

(defcustom ada-language-version 'ada95
  "*Ada language version; one of `ada83', `ada95', `ada2005'."
  :type '(choice (const ada83) (const ada95) (const ada2005)) :group 'ada)

(defcustom ada-move-to-declaration nil
  "*Non-nil means `ada-move-to-start' moves to the subprogram declaration, not to 'begin'."
  :type 'boolean :group 'ada)

(defcustom ada-popup-key '[down-mouse-3]
  "*Key used for binding the contextual menu.
If nil, no contextual menu is available."
  :type '(restricted-sexp :match-alternatives (stringp vectorp))
  :group 'ada)

(defcustom ada-search-directories
  (append '(".")
	  (split-string (or (getenv "ADA_INCLUDE_PATH") "") ":")
	  '("/usr/adainclude" "/usr/local/adainclude"
	    "/opt/gnu/adainclude"))
  "*Default list of directories to search for Ada files.
See the description for the `ff-search-directories' variable.  This variable
is the initial value of `ada-search-directories-internal'."
  :type '(repeat (choice :tag "Directory"
			 (const :tag "default" nil)
			 (directory :format "%v")))
  :group 'ada)

(defvar ada-search-directories-internal ada-search-directories
  "Internal version of `ada-search-directories'.
Its value is the concatenation of the search path as read in the project file
and the standard runtime location, and the value of the user-defined
`ada-search-directories'.")

(defcustom ada-stmt-end-indent 0
  "*Number of columns to indent the end of a statement on a separate line.

An example is:
   if A = B
   >>>>then"
  :type 'integer :group 'ada)

(defcustom ada-tab-policy 'indent-auto
  "*Control the behavior of the TAB key.
Must be one of :
`indent-rigidly' : always adds `ada-indent' blanks at the beginning of the line.
`indent-auto'    : use indentation functions in this file.
`always-tab'     : do `indent-relative'."
  :type '(choice (const indent-auto)
		 (const indent-rigidly)
		 (const always-tab))
  :group 'ada)

(defcustom ada-use-indent ada-broken-indent
  "*Indentation for the lines in a 'use' statement.

An example is:
   use Ada.Text_IO,
   >>>>Ada.Numerics;"
  :type 'integer :group 'ada)

(defcustom ada-when-indent 3
  "*Indentation for 'when' relative to 'exception' or 'case'.

An example is:
   case A is
   >>>>when B =>"
  :type 'integer :group 'ada)

(defcustom ada-with-indent ada-broken-indent
  "*Indentation for the lines in a 'with' statement.

An example is:
   with Ada.Text_IO,
   >>>>Ada.Numerics;"
  :type 'integer :group 'ada)

(defcustom ada-which-compiler 'gnat
  "*Name of the compiler to use.
This will determine what features are made available through the Ada mode.
The possible choices are:
`gnat': Use Ada Core Technologies' GNAT compiler.  Add some cross-referencing
    features.
`generic': Use a generic compiler."
  :type '(choice (const gnat)
		 (const generic))
  :group 'ada)


;;; ---- end of user configurable variables


(defvar ada-body-suffixes '(".adb")
  "List of possible suffixes for Ada body files.
The extensions should include a `.' if needed.")

(defvar ada-spec-suffixes '(".ads")
  "List of possible suffixes for Ada spec files.
The extensions should include a `.' if needed.")

(defvar ada-mode-menu (make-sparse-keymap "Ada")
  "Menu for Ada mode.")

(defvar ada-mode-map (make-sparse-keymap)
  "Local keymap used for Ada mode.")

(defvar ada-mode-extra-map (make-sparse-keymap)
  "Keymap used for non-standard keybindings.")

;; default is C-c C-q because it's free in ada-mode-map
(defvar ada-mode-extra-prefix "\C-c\C-q"
  "Prefix key to access `ada-mode-extra-map' functions.")

(defvar ada-mode-abbrev-table nil
  "Local abbrev table for Ada mode.")
(define-abbrev-table 'ada-mode-abbrev-table ())

(defvar ada-mode-syntax-table nil
  "Syntax table to be used for editing Ada source code.")

(defvar ada-mode-symbol-syntax-table nil
  "Syntax table for Ada, where `_' is a word constituent.")

(eval-when-compile
  ;; These values are used in eval-when-compile expressions.
  (defconst ada-83-string-keywords
    '("abort" "abs" "accept" "access" "all" "and" "array" "at" "begin"
      "body" "case" "constant" "declare" "delay" "delta" "digits" "do"
      "else" "elsif" "end" "entry" "exception" "exit" "for" "function"
      "generic" "goto" "if" "in" "is" "limited" "loop" "mod" "new"
      "not" "null" "of" "or" "others" "out" "package" "pragma" "private"
      "procedure" "raise" "range" "record" "rem" "renames" "return"
      "reverse" "select" "separate" "subtype" "task" "terminate" "then"
      "type" "use" "when" "while" "with" "xor")
    "List of Ada 83 keywords.
Used to define `ada-*-keywords'.")

  (defconst ada-95-string-keywords
    '("abstract" "aliased" "protected" "requeue" "tagged" "until")
    "List of keywords new in Ada 95.
Used to define `ada-*-keywords'.")

  (defconst ada-2005-string-keywords
    '("interface" "overriding" "synchronized")
    "List of keywords new in Ada 2005.
Used to define `ada-*-keywords.'"))

(defvar ada-ret-binding nil
  "Variable to save key binding of RET when casing is activated.")

(defvar ada-case-exception '()
  "Alist of words (entities) that have special casing.")

(defvar ada-case-exception-substring '()
  "Alist of substrings (entities) that have special casing.
The substrings are detected for word constituent when the word
is not itself in `ada-case-exception', and only for substrings that
either are at the beginning or end of the word, or start after '_'.")

(defvar ada-lfd-binding nil
  "Variable to save key binding of LFD when casing is activated.")

(defvar ada-other-file-alist nil
  "Variable used by `find-file' to find the name of the other package.
See `ff-other-file-alist'.")

(defvar ada-align-list
    '(("[^:]\\(\\s-*\\):[^:]" 1 t)
      ("[^=]\\(\\s-+\\)=[^=]" 1 t)
      ("\\(\\s-*\\)use\\s-" 1)
      ("\\(\\s-*\\)--" 1))
    "Ada support for align.el <= 2.2.
This variable provides regular expressions on which to align different lines.
See `align-mode-alist' for more information.")

(defvar ada-align-modes
  '((ada-declaration
     (regexp  . "[^:]\\(\\s-*\\):[^:]")
     (valid   . (lambda() (not (ada-in-comment-p))))
     (modes   . '(ada-mode)))
    (ada-assignment
     (regexp  . "[^=]\\(\\s-+\\)=[^=]")
     (valid   . (lambda() (not (ada-in-comment-p))))
     (modes   . '(ada-mode)))
    (ada-comment
     (regexp  . "\\(\\s-*\\)--")
     (modes   . '(ada-mode)))
    (ada-use
     (regexp  . "\\(\\s-*\\)use\\s-")
     (valid   . (lambda() (not (ada-in-comment-p))))
     (modes   . '(ada-mode)))
    )
  "Ada support for align.el >= 2.8.
This variable defines several rules to use to align different lines.")

(defconst ada-align-region-separate
  (eval-when-compile
    (concat
     "^\\s-*\\($\\|\\("
     "begin\\|"
     "declare\\|"
     "else\\|"
     "end\\|"
     "exception\\|"
     "for\\|"
     "function\\|"
     "generic\\|"
     "if\\|"
     "is\\|"
     "procedure\\|"
     "record\\|"
     "return\\|"
     "type\\|"
     "when"
     "\\)\\>\\)"))
  "See the variable `align-region-separate' for more information.")

;;; ---- Below are the regexp used in this package for parsing

(defconst ada-83-keywords
  (eval-when-compile
    (concat "\\<" (regexp-opt ada-83-string-keywords t) "\\>"))
  "Regular expression matching Ada83 keywords.")

(defconst ada-95-keywords
  (eval-when-compile
    (concat "\\<" (regexp-opt
		   (append
		    ada-95-string-keywords
		    ada-83-string-keywords) t) "\\>"))
  "Regular expression matching Ada95 keywords.")

(defconst ada-2005-keywords
  (eval-when-compile
    (concat "\\<" (regexp-opt
		   (append
		    ada-2005-string-keywords
		    ada-83-string-keywords
		    ada-95-string-keywords) t) "\\>"))
  "Regular expression matching Ada2005 keywords.")

(defvar ada-keywords ada-2005-keywords
  "Regular expression matching Ada keywords.")
;; FIXME: make this customizable

(defconst ada-ident-re
  "[[:alpha:]]\\(?:[_[:alnum:]]\\)*"
  ;; [:alnum:] matches any multibyte word constituent, as well as
  ;; Latin-1 letters and numbers. This allows __ and trailing _;
  ;; someone (emacs bug#1919) proposed [^\W_] to fix that, but \W does
  ;; _not_ mean "not word constituent" inside a character alternative.
  "Regexp matching an Ada identifier.")

(defconst ada-goto-label-re
  (concat "<<" ada-ident-re ">>")
  "Regexp matching a goto label.")

(defconst ada-block-label-re
  (concat ada-ident-re "[ \t\n]*:[^=]")
  "Regexp matching a block label.
Note that this also matches a variable declaration.")

(defconst ada-label-re
  (concat "\\(?:" ada-block-label-re "\\)\\|\\(?:" ada-goto-label-re "\\)")
  "Regexp matching a goto or block label.")

;;  "with" needs to be included in the regexp, to match generic subprogram parameters
;;  Similarly, we put '[not] overriding' on the same line with 'procedure' etc.
(defvar ada-procedure-start-regexp
  (concat
   "^[ \t]*\\(with[ \t]+\\)?\\(\\(not[ \t]+\\)?overriding[ \t]+\\)?\\(procedure\\|function\\|task\\)[ \t\n]+"

   ;;  subprogram name: operator ("[+/=*]")
   "\\("
   "\\(\"[^\"]+\"\\)"

   ;;  subprogram name: name
   "\\|"
   "\\(\\(\\sw\\|[_.]\\)+\\)"
   "\\)")
  "Regexp matching Ada subprogram start.
The actual start is at (match-beginning 4).  The name is in (match-string 5).")

(defconst ada-name-regexp
  "\\([a-zA-Z][a-zA-Z0-9_.']*[a-zA-Z0-9]\\)"
  "Regexp matching a fully qualified name (including attribute).")

(defconst ada-package-start-regexp
  (concat "^[ \t]*\\(private[ \t]+\\)?\\(package\\)[ \t\n]+\\(body[ \t]*\\)?" ada-name-regexp)
  "Regexp matching start of package.
The package name is in (match-string 4).")

(defconst ada-compile-goto-error-file-linenr-re
  "\\([-_.a-zA-Z0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?"
  "Regexp matching filename:linenr[:column].")


;;; ---- regexps for indentation functions

(defvar ada-block-start-re
  (eval-when-compile
    (concat "\\<\\(" (regexp-opt '("begin" "declare" "else"
				   "exception" "generic" "loop" "or"
				   "private" "select" ))
	    "\\|\\(\\(limited\\|abstract\\|tagged\\)[ \t\n]+\\)*record\\)\\>"))
  "Regexp for keywords starting Ada blocks.")

(defvar ada-end-stmt-re
  (eval-when-compile
    (concat "\\("
	    ";"                                        "\\|"
	    "=>[ \t]*$"                                "\\|"
	    "=>[ \t]*--.*$"                            "\\|"
	    "^[ \t]*separate[ \t]*(\\(\\sw\\|[_.]\\)+)"  "\\|"
	    "\\<" (regexp-opt '("begin" "declare" "is" "do" "else" "generic"
				"loop" "private" "record" "select"
				"then abort" "then") t) "\\>"  "\\|"
	    "^[ \t]*" (regexp-opt '("function" "package" "procedure")
				  t) "\\>\\(\\sw\\|[ \t_.]\\)+\\<is\\>"        "\\|"
	    "^[ \t]*exception\\>"
	    "\\)")                      )
  "Regexp of possible ends for a non-broken statement.
A new statement starts after these.")

(defvar ada-matching-start-re
  (eval-when-compile
    (concat "\\<"
	    (regexp-opt
	     '("end" "loop" "select" "begin" "case" "do" "declare"
	       "if" "task" "package" "procedure" "function" "record" "protected") t)
	    "\\>"))
  "Regexp used in `ada-goto-matching-start'.")

(defvar ada-loop-start-re
  "\\<\\(for\\|while\\|loop\\)\\>"
  "Regexp for the start of a loop.")

(defvar ada-subprog-start-re
  (eval-when-compile
    (concat "\\<" (regexp-opt '("accept" "entry" "function" "overriding" "package" "procedure"
				"protected" "task") t) "\\>"))
  "Regexp for the start of a subprogram.")

(defvar ada-contextual-menu-on-identifier nil
  "Set to true when the right mouse button was clicked on an identifier.")

(defvar ada-contextual-menu-last-point nil
  "Position of point just before displaying the menu.
This is a list (point buffer).
Since `ada-popup-menu' moves the point where the user clicked, the region
is modified.  Therefore no command from the menu knows what the user selected
before displaying the contextual menu.
To get the original region, restore the point to this position before
calling `region-end' and `region-beginning'.
Modify this variable if you want to restore the point to another position.")

(easy-menu-define ada-contextual-menu nil
  "Menu to use when the user presses the right mouse button.
The variable `ada-contextual-menu-on-identifier' will be set to t before
displaying the menu if point was on an identifier."
  '("Ada"
    ["Goto Declaration/Body" ada-point-and-xref
     :included ada-contextual-menu-on-identifier]
    ["Goto Body" ada-point-and-xref-body
     :included ada-contextual-menu-on-identifier]
    ["Goto Previous Reference" ada-xref-goto-previous-reference]
    ["List References" ada-find-references
     :included ada-contextual-menu-on-identifier]
    ["List Local References" ada-find-local-references
      :included ada-contextual-menu-on-identifier]
    ["-"                nil nil]
    ["Other File"       ff-find-other-file]
    ["Goto Parent Unit" ada-goto-parent]))


;;------------------------------------------------------------------
;; Support for imenu  (see imenu.el)
;;------------------------------------------------------------------

(defconst ada-imenu-comment-re "\\([ \t]*--.*\\)?")

(defconst ada-imenu-subprogram-menu-re
  (concat "^[ \t]*\\(overriding[ \t]*\\)?\\(procedure\\|function\\)[ \t\n]+"
	  "\\(\\(\\sw\\|_\\)+\\)[ \t\n]*\\([ \t\n]\\|([^)]+)"
	  ada-imenu-comment-re
	  "\\)[ \t\n]*"
	  "\\(return[ \t\n]+\\(\\sw\\|[_.]\\)+[ \t\n]*\\)?is[ \t\n]"))

(defvar ada-imenu-generic-expression
  (list
   (list nil ada-imenu-subprogram-menu-re 3)
   (list "*Specs*"
	 (concat
	  "^[ \t]*\\(procedure\\|function\\)[ \t\n]+\\(\\(\\sw\\|_\\)+\\)"
	  "\\("
	  "\\(" ada-imenu-comment-re "[ \t\n]+\\|[ \t\n]*([^)]+)"
	  ada-imenu-comment-re "\\)";; parameter list or simple space
	  "\\([ \t\n]*return[ \t\n]+\\(\\sw\\|[_.]\\)+[ \t\n]*\\)?"
	  "\\)?;") 2)
   '("*Tasks*" "^[ \t]*task[ \t]+\\(type[ \t]+\\)?\\(\\(body[ \t]+\\)?\\(\\sw\\|_\\)+\\)" 2)
   '("*Type Defs*" "^[ \t]*\\(sub\\)?type[ \t]+\\(\\(\\sw\\|_\\)+\\)" 2)
   '("*Protected*"
     "^[ \t]*protected[ \t]+\\(type[ \t]+\\)?\\(\\(body[ \t]+\\)?\\(\\sw\\|_\\)+\\)" 2)
   '("*Packages*" "^[ \t]*package[ \t]+\\(\\(body[ \t]+\\)?\\(\\sw\\|[_.]\\)+\\)" 1))
  "Imenu generic expression for Ada mode.
See `imenu-generic-expression'.  This variable will create several submenus for
each type of entity that can be found in an Ada file.")


;;------------------------------------------------------------
;;  Support for compile.el
;;------------------------------------------------------------

(defun ada-compile-mouse-goto-error ()
  "Mouse interface for `ada-compile-goto-error'."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-compile-goto-error (point))
  )

(defun ada-compile-goto-error (pos)
  "Replace `compile-goto-error' from compile.el.
If POS is on a file and line location, go to this position.  It adds
to compile.el the capacity to go to a reference in an error message.
For instance, on these lines:
  foo.adb:61:11:  [...] in call to size declared at foo.ads:11
  foo.adb:61:11:  [...] in call to local declared at line 20
the 4 file locations can be clicked on and jumped to."
  (interactive "d")
  (goto-char pos)

  (skip-chars-backward "-a-zA-Z0-9_:./\\")
  (cond
   ;;  special case: looking at a filename:line not at the beginning of a line
   ;;  or a simple line reference "at line ..."
   ((and (not (bolp))
	 (or (looking-at ada-compile-goto-error-file-linenr-re)
	     (and
	      (save-excursion
		(beginning-of-line)
		(looking-at ada-compile-goto-error-file-linenr-re))
	      (save-excursion
		(if (looking-at "\\([0-9]+\\)") (backward-word 1))
		(looking-at "line \\([0-9]+\\)"))))
	     )
    (let ((line (if (match-beginning 2) (match-string 2) (match-string 1)))
	  (file (if (match-beginning 2) (match-string 1)
		  (save-excursion (beginning-of-line)
				  (looking-at ada-compile-goto-error-file-linenr-re)
				  (match-string 1))))
	  (error-pos (point-marker))
	  source)

      ;; set source marker
      (save-excursion
	(compilation-find-file (point-marker) (match-string 1) "./")
	(set-buffer file)

	(when (stringp line)
	  (goto-char (point-min))
	  (forward-line (1- (string-to-number line))))

	(setq source (point-marker)))

      (compilation-goto-locus error-pos source nil)

      ))

   ;; otherwise, default behavior
   (t
    (compile-goto-error))
   )
  (recenter))


;;-------------------------------------------------------------------------
;; Grammar related function
;; The functions below work with the syntax class of the characters in an Ada
;; buffer. Two syntax tables are created, depending on whether we want '_'
;; to be considered as part of a word or not.
;; Some characters may have multiple meanings depending on the context:
;;  - ' is either the beginning of a constant character or an attribute
;;  - # is either part of a based literal or a gnatprep statement.
;;  - " starts a string, but not if inside a constant character.
;;  - ( and ) should be ignored if inside a constant character.
;; Thus their syntax property is changed automatically, and we can still use
;; the standard Emacs functions for sexp (see `ada-in-string-p')
;;
;; On Emacs, this is done through the `syntax-table' text property.  The
;; corresponding action is applied automatically each time the buffer
;; changes via syntax-propertize-function.
;;
;; on XEmacs, the `syntax-table' property does not exist and we have to use a
;; slow advice to `parse-partial-sexp' to do the same thing.
;; When executing parse-partial-sexp, we simply modify the strings before and
;; after, so that the special constants '"', '(' and ')' do not interact
;; with parse-partial-sexp.
;; Note: this code is slow and needs to be rewritten as soon as something
;; better is available on XEmacs.
;;-------------------------------------------------------------------------

(defun ada-create-syntax-table ()
  "Create the two syntax tables use in the Ada mode.
The standard table declares `_' as a symbol constituent, the second one
declares it as a word constituent."
  (interactive)
  (setq ada-mode-syntax-table (make-syntax-table))

  ;; define string brackets (`%' is alternative string bracket, but
  ;; almost never used as such and throws font-lock and indentation
  ;; off the track.)
  (modify-syntax-entry ?%  "$" ada-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" ada-mode-syntax-table)

  (modify-syntax-entry ?:  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\; "." ada-mode-syntax-table)
  (modify-syntax-entry ?&  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\|  "." ada-mode-syntax-table)
  (modify-syntax-entry ?+  "." ada-mode-syntax-table)
  (modify-syntax-entry ?*  "." ada-mode-syntax-table)
  (modify-syntax-entry ?/  "." ada-mode-syntax-table)
  (modify-syntax-entry ?=  "." ada-mode-syntax-table)
  (modify-syntax-entry ?<  "." ada-mode-syntax-table)
  (modify-syntax-entry ?>  "." ada-mode-syntax-table)
  (modify-syntax-entry ?$ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\[ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\] "." ada-mode-syntax-table)
  (modify-syntax-entry ?\{ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\} "." ada-mode-syntax-table)
  (modify-syntax-entry ?. "." ada-mode-syntax-table)
  (modify-syntax-entry ?\\ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\' "." ada-mode-syntax-table)

  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" ada-mode-syntax-table)

  ;; See the comment above on grammar related function for the special
  ;; setup for '#'.
  (if (featurep 'xemacs)
      (modify-syntax-entry ?#  "<" ada-mode-syntax-table)
    (modify-syntax-entry ?#  "$" ada-mode-syntax-table))

  ;; and \f and \n end a comment
  (modify-syntax-entry ?\f  ">   " ada-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " ada-mode-syntax-table)

  ;; define what belongs in Ada symbols
  (modify-syntax-entry ?_ "_" ada-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" ada-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ada-mode-syntax-table)

  (setq ada-mode-symbol-syntax-table (copy-syntax-table ada-mode-syntax-table))
  (modify-syntax-entry ?_ "w" ada-mode-symbol-syntax-table)
  )

;;  Support of special characters in XEmacs (see the comments at the beginning
;;  of the section on Grammar related functions).

(if (featurep 'xemacs)
    (defadvice parse-partial-sexp (around parse-partial-sexp-protect-constants)
      "Handles special character constants and gnatprep statements."
      (let (change)
	(if (< to from)
	    (let ((tmp from))
	      (setq from to  to tmp)))
	(save-excursion
	  (goto-char from)
	  (while (re-search-forward "'\\([(\")#]\\)'" to t)
	    (setq change (cons (list (match-beginning 1)
				     1
				     (match-string 1))
			       change))
	    (replace-match "'A'"))
	  (goto-char from)
	  (while (re-search-forward "\\(#[0-9a-fA-F]*#\\)" to t)
	    (setq change (cons (list (match-beginning 1)
				     (length (match-string 1))
				     (match-string 1))
			       change))
	    (replace-match (make-string (length (match-string 1)) ?@))))
	ad-do-it
	(save-excursion
	  (while change
	    (goto-char (caar change))
	    (delete-char (cadar change))
	    (insert (caddar change))
	    (setq change (cdr change)))))))

(unless (eval-when-compile (fboundp 'syntax-propertize-via-font-lock))
  ;; Before `syntax-propertize', we had to use font-lock to apply syntax-table
  ;; properties, and in some cases we even had to do it manually (in
  ;; `ada-after-change-function').  `ada-handle-syntax-table-properties'
  ;; decides which method to use.

(defun ada-set-syntax-table-properties ()
  "Assign `syntax-table' properties in accessible part of buffer.
In particular, character constants are said to be strings, #...#
are treated as numbers instead of gnatprep comments."
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t))
    (remove-text-properties (point-min) (point-max) '(syntax-table nil))
    (goto-char (point-min))
    (while (re-search-forward
	    ;; The following regexp was adapted from
	    ;; `ada-font-lock-syntactic-keywords'.
	    "^[ \t]*\\(#\\(?:if\\|else\\|elsif\\|end\\)\\)\\|[^a-zA-Z0-9)]\\('\\)[^'\n]\\('\\)"
	    nil t)
      (if (match-beginning 1)
	  (put-text-property
	       (match-beginning 1) (match-end 1) 'syntax-table '(11 . ?\n))
	(put-text-property
	     (match-beginning 2) (match-end 2) 'syntax-table '(7 . ?'))
	(put-text-property
	     (match-beginning 3) (match-end 3) 'syntax-table '(7 . ?'))))
    (unless modified
      (restore-buffer-modified-p nil))))

(defun ada-after-change-function (beg end _old-len)
  "Called when the region between BEG and END was changed in the buffer.
OLD-LEN indicates what the length of the replaced text was."
  (save-excursion
    (save-restriction
      (let ((from (progn (goto-char beg) (line-beginning-position)))
	    (to (progn (goto-char end) (line-end-position))))
	(narrow-to-region from to)
	(save-match-data
	  (ada-set-syntax-table-properties))))))

(defun ada-initialize-syntax-table-properties ()
  "Assign `syntax-table' properties in current buffer."
    (save-excursion
      (save-restriction
	(widen)
	(save-match-data
	  (ada-set-syntax-table-properties))))
    (add-hook 'after-change-functions 'ada-after-change-function nil t))

(defun ada-handle-syntax-table-properties ()
  "Handle `syntax-table' properties."
  (if font-lock-mode
      ;; `font-lock-mode' will take care of `syntax-table' properties.
      (remove-hook 'after-change-functions 'ada-after-change-function t)
    ;; Take care of `syntax-table' properties manually.
    (ada-initialize-syntax-table-properties)))

) ;;(not (fboundp 'syntax-propertize))

;;------------------------------------------------------------------
;;  Testing the grammatical context
;;------------------------------------------------------------------

(defsubst ada-in-comment-p (&optional parse-result)
  "Return t if inside a comment.
If PARSE-RESULT is non-nil, use it instead of calling `parse-partial-sexp'."
  (nth 4 (or parse-result
	     (parse-partial-sexp
	      (line-beginning-position) (point)))))

(defsubst ada-in-string-p (&optional parse-result)
  "Return t if point is inside a string.
If PARSE-RESULT is non-nil, use it instead of calling `parse-partial-sexp'."
  (nth 3 (or parse-result
	     (parse-partial-sexp
	      (line-beginning-position) (point)))))

(defsubst ada-in-string-or-comment-p (&optional parse-result)
  "Return t if inside a comment or string.
If PARSE-RESULT is non-nil, use it instead of calling `parse-partial-sexp'."
  (setq parse-result (or parse-result
			 (parse-partial-sexp
			  (line-beginning-position) (point))))
  (or (ada-in-string-p parse-result) (ada-in-comment-p parse-result)))

(defsubst ada-in-numeric-literal-p ()
  "Return t if point is after a prefix of a numeric literal."
  (looking-back "\\([0-9]+#[0-9a-fA-F_]+\\)"))

;;------------------------------------------------------------------
;; Contextual menus
;; The Ada mode comes with contextual menus, bound by default to the right
;; mouse button.
;; Add items to this menu by modifying `ada-contextual-menu'. Note that the
;; variable `ada-contextual-menu-on-identifier' is set automatically to t
;; if the mouse button was pressed on an identifier.
;;------------------------------------------------------------------

(defun ada-call-from-contextual-menu (function)
  "Execute FUNCTION when called from the contextual menu.
It forces Emacs to change the cursor position."
  (interactive)
  (funcall function)
  (setq ada-contextual-menu-last-point
	(list (point) (current-buffer))))

(defun ada-popup-menu (position)
  "Pops up a contextual menu, depending on where the user clicked.
POSITION is the location the mouse was clicked on.
Sets `ada-contextual-menu-last-point' to the current position before
displaying the menu.  When a function from the menu is called, the
point is where the mouse button was clicked."
  (interactive "e")

  ;;  declare this as a local variable, so that the function called
  ;;  in the contextual menu does not hide the region in
  ;;  transient-mark-mode.
  (let ((deactivate-mark nil))
    (setq ada-contextual-menu-last-point
	 (list (point) (current-buffer)))
    (mouse-set-point last-input-event)

    (setq ada-contextual-menu-on-identifier
	  (and (char-after)
	       (or (= (char-syntax (char-after)) ?w)
		   (= (char-after) ?_))
	       (not (ada-in-string-or-comment-p))
	       (save-excursion (skip-syntax-forward "w")
			       (not (ada-after-keyword-p)))
	       ))
    (if (fboundp 'popup-menu)
	(funcall (symbol-function 'popup-menu) ada-contextual-menu)
      (let (choice)
	(setq choice (x-popup-menu position ada-contextual-menu))
	(if choice
	    (funcall (lookup-key ada-contextual-menu (vector (car choice)))))))

    (set-buffer (cadr ada-contextual-menu-last-point))
    (goto-char (car ada-contextual-menu-last-point))
    ))


;;------------------------------------------------------------------
;; Misc functions
;;------------------------------------------------------------------

;;;###autoload
(defun ada-add-extensions (spec body)
  "Define SPEC and BODY as being valid extensions for Ada files.
Going from body to spec with `ff-find-other-file' used these
extensions.
SPEC and BODY are two regular expressions that must match against
the file name."
  (let* ((reg (concat (regexp-quote body) "$"))
	 (tmp (assoc reg ada-other-file-alist)))
    (if tmp
	(setcdr tmp (list (cons spec (cadr tmp))))
      (add-to-list 'ada-other-file-alist (list reg (list spec)))))

  (let* ((reg (concat (regexp-quote spec) "$"))
	 (tmp (assoc reg ada-other-file-alist)))
    (if tmp
	(setcdr tmp (list (cons body (cadr tmp))))
      (add-to-list 'ada-other-file-alist (list reg (list body)))))

  (add-to-list 'auto-mode-alist
	       (cons (concat (regexp-quote spec) "\\'") 'ada-mode))
  (add-to-list 'auto-mode-alist
	       (cons (concat (regexp-quote body) "\\'") 'ada-mode))

  (add-to-list 'ada-spec-suffixes spec)
  (add-to-list 'ada-body-suffixes body)

  ;; Support for speedbar (Specifies that we want to see these files in
  ;; speedbar)
  (if (fboundp 'speedbar-add-supported-extension)
      (progn
	(funcall (symbol-function 'speedbar-add-supported-extension)
		 spec)
	(funcall (symbol-function 'speedbar-add-supported-extension)
		 body))))

(defvar ada-font-lock-syntactic-keywords) ; defined below

;;;###autoload
(define-derived-mode ada-mode prog-mode "Ada"
  "Ada mode is the major mode for editing Ada code."

  ;;  Set the paragraph delimiters so that one can select a whole block
  ;;  simply with M-h
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\n\f]*$")

  ;; comment end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before. RE
  (set (make-local-variable 'comment-end) "")

  ;; used by autofill and indent-new-comment-line
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")

  ;; used by autofill to break a comment line and continue it on another line.
  ;; The reason we need this one is that the default behavior does not work
  ;; correctly with the definition of paragraph-start above when the comment
  ;; is right after a multi-line subprogram declaration (the comments are
  ;; aligned under the latest parameter, not under the declaration start).
  (set (make-local-variable 'comment-line-break-function)
       (lambda (&optional soft) (let ((fill-prefix nil))
				  (indent-new-comment-line soft))))

  (set (make-local-variable 'indent-line-function)
       'ada-indent-current-function)

  (set (make-local-variable 'comment-column) 40)

  ;;  Emacs 20.3 defines a comment-padding to insert spaces between
  ;;  the comment and the text. We do not want any, this is already
  ;;  included in comment-start
  (unless (featurep 'xemacs)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    (set (make-local-variable 'comment-padding) 0)
    (set (make-local-variable 'parse-sexp-lookup-properties) t))

  (setq case-fold-search t)
  (if (boundp 'imenu-case-fold-search)
      (setq imenu-case-fold-search t))

  (set (make-local-variable 'fill-paragraph-function)
       'ada-fill-comment-paragraph)

  ;;  Support for compile.el
  ;;  We just substitute our own functions to go to the error.
  (add-hook 'compilation-mode-hook
	    (lambda()
	      ;; FIXME: This has global impact!  -stef
	      (define-key compilation-minor-mode-map [mouse-2]
		'ada-compile-mouse-goto-error)
	      (define-key compilation-minor-mode-map "\C-c\C-c"
		'ada-compile-goto-error)
	      (define-key compilation-minor-mode-map "\C-m"
		'ada-compile-goto-error)))

  ;;  font-lock support :

  (set (make-local-variable 'font-lock-defaults)
       '(ada-font-lock-keywords
	 nil t
	 ((?\_ . "w") (?# . "."))
	 beginning-of-line))

  (if (eval-when-compile (fboundp 'syntax-propertize-via-font-lock))
      (set (make-local-variable 'syntax-propertize-function)
           (syntax-propertize-via-font-lock ada-font-lock-syntactic-keywords))
    (set (make-local-variable 'font-lock-syntactic-keywords)
         ada-font-lock-syntactic-keywords))

  ;; Set up support for find-file.el.
  (set (make-local-variable 'ff-other-file-alist)
       'ada-other-file-alist)
  (set (make-local-variable 'ff-search-directories)
       'ada-search-directories-internal)
  (setq ff-post-load-hook    'ada-set-point-accordingly
	ff-file-created-hook 'ada-make-body)
  (add-hook 'ff-pre-load-hook 'ada-which-function-are-we-in)

  (make-local-variable 'ff-special-constructs)
  (mapc (lambda (pair) (add-to-list 'ff-special-constructs pair))
	(list
	 ;; Top level child package declaration; go to the parent package.
	 (cons (eval-when-compile
		 (concat "^\\(private[ \t]\\)?[ \t]*package[ \t]+"
			 "\\(body[ \t]+\\)?"
			 "\\(\\(\\sw\\|[_.]\\)+\\)\\.\\(\\sw\\|_\\)+[ \t\n]+is"))
	       (lambda ()
		 (ff-get-file
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 3))
		  ada-spec-suffixes)))

	 ;; A "separate" clause.
	 (cons "^separate[ \t\n]*(\\(\\(\\sw\\|[_.]\\)+\\))"
	       (lambda ()
		 (ff-get-file
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 1))
		  ada-spec-suffixes)))

	 ;; A "with" clause.
	 (cons "^with[ \t]+\\([a-zA-Z0-9_\\.]+\\)"
	       (lambda ()
		 (ff-get-file
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 1))
		  ada-spec-suffixes)))
	 ))

  ;;  Support for outline-minor-mode
  (set (make-local-variable 'outline-regexp)
       "\\([ \t]*\\(procedure\\|function\\|package\\|if\\|while\\|for\\|declare\\|case\\|end\\|begin\\|loop\\)\\|--\\)")
  (set (make-local-variable 'outline-level) 'ada-outline-level)

  ;;  Support for imenu : We want a sorted index
  (setq imenu-generic-expression ada-imenu-generic-expression)

  (setq imenu-sort-function 'imenu--sort-by-name)

  ;;  Support for ispell : Check only comments
  (set (make-local-variable 'ispell-check-comments) 'exclusive)

  ;;  Support for align
  (add-to-list 'align-dq-string-modes 'ada-mode)
  (add-to-list 'align-open-comment-modes 'ada-mode)
  (set (make-local-variable 'align-region-separate) ada-align-region-separate)

  ;; Exclude comments alone on line from alignment.
  (add-to-list 'align-exclude-rules-list
	       '(ada-solo-comment
		 (regexp  . "^\\(\\s-*\\)--")
		 (modes   . '(ada-mode))))
  (add-to-list 'align-exclude-rules-list
	       '(ada-solo-use
		 (regexp  . "^\\(\\s-*\\)\\<use\\>")
		 (modes   . '(ada-mode))))

  (setq ada-align-modes nil)

  (add-to-list 'ada-align-modes
	       '(ada-declaration-assign
		 (regexp  . "[^:]\\(\\s-*\\):[^:]")
		 (valid   . (lambda() (not (ada-in-comment-p))))
		 (repeat . t)
		 (modes   . '(ada-mode))))
  (add-to-list 'ada-align-modes
	       '(ada-associate
		 (regexp  . "[^=]\\(\\s-*\\)=>")
		 (valid   . (lambda() (not (ada-in-comment-p))))
		 (modes   . '(ada-mode))))
  (add-to-list 'ada-align-modes
	       '(ada-comment
		 (regexp  . "\\(\\s-*\\)--")
		 (modes   . '(ada-mode))))
  (add-to-list 'ada-align-modes
	       '(ada-use
		 (regexp  . "\\(\\s-*\\)\\<use\\s-")
		 (valid   . (lambda() (not (ada-in-comment-p))))
		 (modes   . '(ada-mode))))
  (add-to-list 'ada-align-modes
	       '(ada-at
		 (regexp . "\\(\\s-+\\)at\\>")
		 (modes . '(ada-mode))))

  (setq align-mode-rules-list ada-align-modes)

  ;;  Set up the contextual menu
  (if ada-popup-key
      (define-key ada-mode-map ada-popup-key 'ada-popup-menu))

  ;;  Support for Abbreviations (the user still need to "M-x abbrev-mode"
  (setq local-abbrev-table ada-mode-abbrev-table)

  ;;  Support for which-function mode
  (set (make-local-variable 'which-func-functions) '(ada-which-function))

  ;;  Support for indent-new-comment-line (Especially for XEmacs)
  (set (make-local-variable 'comment-multi-line) nil)

  ;;  Support for add-log
  (set (make-local-variable 'add-log-current-defun-function)
       'ada-which-function)

  (easy-menu-add ada-mode-menu ada-mode-map)

  (set (make-local-variable 'skeleton-further-elements)
       '((< '(backward-delete-char-untabify
	      (min ada-indent (current-column))))))
  (add-hook 'skeleton-end-hook  'ada-adjust-case-skeleton nil t)

  ;;  To be run after the hook, in case the user modified
  ;;  ada-fill-comment-prefix
  (add-hook 'hack-local-variables-hook
            (lambda ()
              (set (make-local-variable 'comment-start)
                   (or ada-fill-comment-prefix "-- "))

              ;; Run this after the hook to give the users a chance
              ;; to activate font-lock-mode.

              (unless (or (eval-when-compile (fboundp 'syntax-propertize-via-font-lock))
                          (featurep 'xemacs))
                (ada-initialize-syntax-table-properties)
                (add-hook 'font-lock-mode-hook
                          'ada-handle-syntax-table-properties nil t))

              ;; FIXME: ada-language-version might be set in the mode
              ;; hook or it might even be set later on via file-local
              ;; vars, so ada-keywords should be set lazily.
              (cond ((eq ada-language-version 'ada83)
                     (setq ada-keywords ada-83-keywords))
                    ((eq ada-language-version 'ada95)
                     (setq ada-keywords ada-95-keywords))
                    ((eq ada-language-version 'ada2005)
                     (setq ada-keywords ada-2005-keywords)))

              (if ada-auto-case
                  (ada-activate-keys-for-case)))
            nil 'local))

(defun ada-adjust-case-skeleton ()
  "Adjust the case of the text inserted by a skeleton."
  (save-excursion
    (let ((aa-end (point)))
      (ada-adjust-case-region
       (progn (goto-char (symbol-value 'beg)) (forward-word -1) (point))
       (goto-char aa-end)))))

(defun ada-region-selected ()
  "Should we operate on an active region?"
  (if (fboundp 'use-region-p)
      (use-region-p)
    (region-active-p)))

;;-----------------------------------------------------------------
;;                      auto-casing
;; Since Ada is case-insensitive, the Ada mode provides an extensive set of
;; functions to auto-case identifiers, keywords, ...
;; The basic rules for autocasing are defined through the variables
;; `ada-case-attribute', `ada-case-keyword' and `ada-case-identifier'. These
;; are references to the functions that will do the actual casing.
;;
;; However, in most cases, the user will want to define some exceptions to
;; these casing rules. This is done through a list of files, that contain
;; one word per line. These files are stored in `ada-case-exception-file'.
;; For backward compatibility, this variable can also be a string.
;;-----------------------------------------------------------------

(defun ada-save-exceptions-to-file (file-name)
  "Save the casing exception lists to the file FILE-NAME.
Casing exception lists are `ada-case-exception' and `ada-case-exception-substring'."
  (find-file (expand-file-name file-name))
  (erase-buffer)
  (mapc (lambda (x) (insert (car x) "\n"))
	(sort (copy-sequence ada-case-exception)
	      (lambda(a b) (string< (car a) (car b)))))
  (mapc (lambda (x) (insert "*" (car x) "\n"))
	(sort (copy-sequence ada-case-exception-substring)
	      (lambda(a b) (string< (car a) (car b)))))
  (save-buffer)
  (kill-buffer nil)
  )

(defun ada-create-case-exception (&optional word)
  "Define WORD as an exception for the casing system.
If WORD is not given, then the current word in the buffer is used instead.
The new word is added to the first file in `ada-case-exception-file'.
The standard casing rules will no longer apply to this word."
  (interactive)
  (let ((file-name
         (cond ((stringp ada-case-exception-file)
                ada-case-exception-file)
               ((listp ada-case-exception-file)
                (car ada-case-exception-file))
               (t
                (error (concat "No exception file specified.  "
                               "See variable ada-case-exception-file"))))))

    (unless word
      (with-syntax-table ada-mode-symbol-syntax-table
        (save-excursion
          (skip-syntax-backward "w")
          (setq word (buffer-substring-no-properties
                      (point) (save-excursion (forward-word 1) (point)))))))

    ;;  Reread the exceptions file, in case it was modified by some other,
    (ada-case-read-exceptions-from-file file-name)

    ;;  If the word is already in the list, even with a different casing
    ;;  we simply want to replace it.
    (if (and (not (equal ada-case-exception '()))
	     (assoc-string word ada-case-exception t))
	(setcar (assoc-string word ada-case-exception t) word)
      (add-to-list 'ada-case-exception (cons word t)))

    (ada-save-exceptions-to-file file-name)))

(defun ada-create-case-exception-substring (&optional word)
  "Define the substring WORD as an exception for the casing system.
If WORD is not given, then the current word in the buffer is used instead,
or the selected region if any is active.
The new word is added to the first file in `ada-case-exception-file'.
When auto-casing a word, this substring will be special-cased, unless the
word itself has a special casing."
  (interactive)
  (let ((file-name
	 (cond ((stringp ada-case-exception-file)
		ada-case-exception-file)
	       ((listp ada-case-exception-file)
		(car ada-case-exception-file))
	       (t
		(error (concat "No exception file specified.  "
			       "See variable ada-case-exception-file"))))))

    ;;  Find the substring to define as an exception. Order is: the parameter,
    ;;  if any, or the selected region, or the word under the cursor
    (cond
     (word   nil)

     ((ada-region-selected)
      (setq word (buffer-substring-no-properties
		  (region-beginning) (region-end))))

     (t
      (let ((underscore-syntax (char-syntax ?_)))
	(unwind-protect
	    (progn
	      (modify-syntax-entry ?_ "." (syntax-table))
	      (save-excursion
		(skip-syntax-backward "w")
		(setq word (buffer-substring-no-properties
			    (point)
			    (save-excursion (forward-word 1) (point))))))
	  (modify-syntax-entry ?_ (make-string 1 underscore-syntax)
			       (syntax-table))))))

    ;;  Reread the exceptions file, in case it was modified by some other,
    (ada-case-read-exceptions-from-file file-name)

    ;;  If the word is already in the list, even with a different casing
    ;;  we simply want to replace it.
    (if (and (not (equal ada-case-exception-substring '()))
	     (assoc-string word ada-case-exception-substring t))
	(setcar (assoc-string word ada-case-exception-substring t) word)
      (add-to-list 'ada-case-exception-substring (cons word t))
      )

    (ada-save-exceptions-to-file file-name)

    (message "%s" (concat "Defining " word " as a casing exception"))))

(defun ada-case-read-exceptions-from-file (file-name)
  "Read the content of the casing exception file FILE-NAME."
  (if (file-readable-p (expand-file-name file-name))
      (let ((buffer (current-buffer)))
	(find-file (expand-file-name file-name))
	(set-syntax-table ada-mode-symbol-syntax-table)
	(widen)
	(goto-char (point-min))
	(while (not (eobp))

	  ;; If the item is already in the list, even with an other casing,
	  ;; do not add it again. This way, the user can easily decide which
	  ;; priority should be applied to each casing exception
	  (let ((word (buffer-substring-no-properties
		       (point) (save-excursion (forward-word 1) (point)))))

	    ;;  Handling a substring ?
	    (if (char-equal (string-to-char word) ?*)
		(progn
		  (setq word (substring word 1))
		  (unless (assoc-string word ada-case-exception-substring t)
		    (add-to-list 'ada-case-exception-substring (cons word t))))
	      (unless (assoc-string word ada-case-exception t)
		(add-to-list 'ada-case-exception (cons word t)))))

	  (forward-line 1))
	(kill-buffer nil)
	(set-buffer buffer)))
  )

(defun ada-case-read-exceptions ()
  "Read all the casing exception files from `ada-case-exception-file'."
  (interactive)

  ;;  Reinitialize the casing exception list
  (setq ada-case-exception '()
	ada-case-exception-substring '())

  (cond ((stringp ada-case-exception-file)
	 (ada-case-read-exceptions-from-file ada-case-exception-file))

	((listp ada-case-exception-file)
	 (mapcar 'ada-case-read-exceptions-from-file
		 ada-case-exception-file))))

(defun ada-adjust-case-substring ()
  "Adjust case of substrings in the previous word."
  (interactive)
  (let ((substrings            ada-case-exception-substring)
	(max                   (point))
	(case-fold-search      t)
	(underscore-syntax     (char-syntax ?_))
	re)

    (save-excursion
       (forward-word -1)

       (unwind-protect
	  (progn
	    (modify-syntax-entry ?_ "." (syntax-table))

	    (while substrings
	      (setq re (concat "\\b" (regexp-quote (caar substrings)) "\\b"))

	      (save-excursion
		 (while (re-search-forward re max t)
		   (replace-match (caar substrings) t)))
	      (setq substrings (cdr substrings))
	      )
	    )
	 (modify-syntax-entry ?_ (make-string 1 underscore-syntax) (syntax-table)))
       )))

(defun ada-adjust-case-identifier ()
  "Adjust case of the previous identifier.
The auto-casing is done according to the value of `ada-case-identifier'
and the exceptions defined in `ada-case-exception-file'."
  (interactive)
  (if (or (equal ada-case-exception '())
	  (equal (char-after) ?_))
      (progn
	(funcall ada-case-identifier -1)
	(ada-adjust-case-substring))

    (progn
      (let ((end   (point))
	    (start (save-excursion (skip-syntax-backward "w")
				   (point)))
	    match)
	;;  If we have an exception, replace the word by the correct casing
	(if (setq match (assoc-string (buffer-substring start end)
				      ada-case-exception t))

	    (progn
	      (delete-region start end)
	      (insert (car match)))

	  ;;  Else simply re-case the word
	  (funcall ada-case-identifier -1)
	  (ada-adjust-case-substring))))))

(defun ada-after-keyword-p ()
  "Return t if cursor is after a keyword that is not an attribute."
  (save-excursion
    (forward-word -1)
    (and (not (and (char-before)
		   (or (= (char-before) ?_)
		       (= (char-before) ?'))));; unless we have a _ or '
	 (looking-at (concat ada-keywords "[^_]")))))

(defun ada-adjust-case (&optional force-identifier)
  "Adjust the case of the word before the character just typed.
If FORCE-IDENTIFIER is non-nil then also adjust keyword as identifier."
  (if (not (bobp))
      (progn
	(forward-char -1)
	(if (and (not (bobp))
		 ;;  or if at the end of a character constant
		 (not (and (eq (following-char) ?')
			   (eq (char-before (1- (point))) ?')))
		 ;;  or if the previous character was not part of a word
		 (eq (char-syntax (char-before)) ?w)
		 ;;  if in a string or a comment
		 (not (ada-in-string-or-comment-p))
		 ;;  if in a numeric literal
		 (not (ada-in-numeric-literal-p))
		 )
	    (if (save-excursion
		  (forward-word -1)
		  (or (= (point) (point-min))
		      (backward-char 1))
		  (= (following-char) ?'))
		(funcall ada-case-attribute -1)
	      (if (and
		   (not force-identifier)     ; (MH)
		   (ada-after-keyword-p))
		  (funcall ada-case-keyword -1)
		(ada-adjust-case-identifier))))
	(forward-char 1)
	))
  )

(defun ada-adjust-case-interactive (arg)
  "Adjust the case of the previous word, and process the character just typed.
ARG is the prefix the user entered with \\[universal-argument]."
  (interactive "P")

  (if ada-auto-case
      (let ((lastk last-command-event))

        (with-syntax-table ada-mode-symbol-syntax-table
          (cond ((or (eq lastk ?\n)
                     (eq lastk ?\r))
                 ;; horrible kludge
                 (insert " ")
                 (ada-adjust-case)
                 ;; horrible dekludge
                 (delete-char -1)
                 ;; some special keys and their bindings
                 (cond
                  ((eq lastk ?\n)
                   (funcall ada-lfd-binding))
                  ((eq lastk ?\r)
                   (funcall ada-ret-binding))))
                ((eq lastk ?\C-i) (ada-tab))
                ;; Else just insert the character
                ((self-insert-command (prefix-numeric-value arg))))
          ;; if there is a keyword in front of the underscore
          ;; then it should be part of an identifier (MH)
          (if (eq lastk ?_)
              (ada-adjust-case t)
            (ada-adjust-case))))

    ;; Else, no auto-casing
    (cond
     ((eq last-command-event ?\n)
      (funcall ada-lfd-binding))
     ((eq last-command-event ?\r)
      (funcall ada-ret-binding))
     (t
      (self-insert-command (prefix-numeric-value arg))))))

(defun ada-activate-keys-for-case ()
  ;; FIXME: Use post-self-insert-hook instead of changing key bindings.
  "Modify the key bindings for all the keys that should readjust the casing."
  (interactive)
  ;; Save original key-bindings to allow swapping ret/lfd
  ;; when casing is activated.
  ;; The 'or ...' is there to be sure that the value will not
  ;; be changed again when Ada mode is called more than once
  (or ada-ret-binding    (setq ada-ret-binding (key-binding "\C-M")))
  (or ada-lfd-binding    (setq ada-lfd-binding (key-binding "\C-j")))

  ;; Call case modifying function after certain keys.
  (mapcar (function (lambda(key) (define-key
				   ada-mode-map
				   (char-to-string key)
				   'ada-adjust-case-interactive)))
	  '( ?` ?_ ?# ?% ?& ?* ?( ?) ?- ?= ?+
		?| ?\; ?: ?' ?\" ?< ?, ?. ?> ?/ ?\n 32 ?\r )))

(defun ada-loose-case-word (&optional _arg)
  "Upcase first letter and letters following `_' in the following word.
No other letter is modified.
ARG is ignored, and is there for compatibility with `capitalize-word' only."
  (interactive)
  (save-excursion
    (let ((end   (save-excursion (skip-syntax-forward  "w") (point)))
	  (first t))
      (skip-syntax-backward "w")
      (while (and (or first (search-forward "_" end t))
		  (< (point) end))
	(and first
	     (setq first nil))
	(insert-char (upcase (following-char)) 1)
	(delete-char 1)))))

(defun ada-no-auto-case (&optional _arg)
  "Do nothing.  ARG is ignored.
This function can be used for the auto-casing variables in Ada mode, to
adapt to unusual auto-casing schemes.  Since it does nothing, you can for
instance use it for `ada-case-identifier' if you don't want any special
auto-casing for identifiers, whereas keywords have to be lower-cased.
See also `ada-auto-case' to disable auto casing altogether."
  nil)

(defun ada-capitalize-word (&optional _arg)
  "Upcase first letter and letters following '_', lower case other letters.
ARG is ignored, and is there for compatibility with `capitalize-word' only."
  (interactive)
  (let ((end   (save-excursion (skip-syntax-forward  "w") (point)))
	(begin (save-excursion (skip-syntax-backward "w") (point))))
    (modify-syntax-entry ?_ "_")
    (capitalize-region begin end)
    (modify-syntax-entry ?_ "w")))

(defun ada-adjust-case-region (from to)
  "Adjust the case of all words in the region between FROM and TO.
Attention: This function might take very long for big regions!"
  (interactive "*r")
  (let ((begin nil)
	(end nil)
	(keywordp nil)
	(attribp nil))
    (message "Adjusting case ...")
    (with-syntax-table ada-mode-symbol-syntax-table
      (save-excursion
        (goto-char to)
        ;;
        ;; loop: look for all identifiers, keywords, and attributes
        ;;
        (while (re-search-backward "\\<\\(\\sw+\\)\\>" from t)
          (setq end (match-end 1))
          (setq attribp
                (and (> (point) from)
                     (save-excursion
                       (forward-char -1)
                       (setq attribp (looking-at "'.[^']")))))
          (or
           ;; do nothing if it is a string or comment
           (ada-in-string-or-comment-p)
           (progn
             ;;
             ;; get the identifier or keyword or attribute
             ;;
             (setq begin (point))
             (setq keywordp (looking-at ada-keywords))
             (goto-char end)
             ;;
             ;; casing according to user-option
             ;;
             (if attribp
                 (funcall ada-case-attribute -1)
               (if keywordp
                   (funcall ada-case-keyword -1)
                 (ada-adjust-case-identifier)))
             (goto-char begin))))
        (message "Adjusting case ... Done")))))

(defun ada-adjust-case-buffer ()
  "Adjust the case of all words in the whole buffer.
ATTENTION: This function might take very long for big buffers!"
  (interactive "*")
  (ada-adjust-case-region (point-min) (point-max)))


;;--------------------------------------------------------------
;; Format Parameter Lists
;; Some special algorithms are provided to indent the parameter lists in
;; subprogram declarations. This is done in two steps:
;;  - First parses the parameter list. The returned list has the following
;;    format:
;;     ( (<Param_Name> in? out? access? <Type_Name> <Default_Expression>)
;;       ... )
;;    This is done in `ada-scan-paramlist'.
;;  - Delete and recreate the parameter list in function
;;    `ada-insert-paramlist'.
;; Both steps are called from `ada-format-paramlist'.
;; Note: Comments inside the parameter list are lost.
;;       The syntax has to be correct, or the reformatting will fail.
;;--------------------------------------------------------------

(defun ada-format-paramlist ()
  "Reformat the parameter list point is in."
  (interactive)
  (let ((begin nil)
	(end nil)
	(delend nil)
	(paramlist nil))
    (with-syntax-table ada-mode-symbol-syntax-table

      ;; check if really inside parameter list
      (or (ada-in-paramlist-p)
          (error "Not in parameter list"))

      ;; find start of current parameter-list
      (ada-search-ignore-string-comment
       (concat ada-subprog-start-re "\\|\\<body\\>" ) t nil)
      (down-list 1)
      (backward-char 1)
      (setq begin (point))

      ;; find end of parameter-list
      (forward-sexp 1)
      (setq delend (point))
      (delete-char -1)
      (insert "\n")

      ;; find end of last parameter-declaration
      (forward-comment -1000)
      (setq end (point))

      ;; build a list of all elements of the parameter-list
      (setq paramlist (ada-scan-paramlist (1+ begin) end))

      ;; delete the original parameter-list
      (delete-region begin  delend)

      ;; insert the new parameter-list
      (goto-char begin)
      (ada-insert-paramlist paramlist))))

(defun ada-scan-paramlist (begin end)
  "Scan the parameter list found in between BEGIN and END.
Return the equivalent internal parameter list."
  (let ((paramlist (list))
	(param (list))
	(notend t)
	(apos nil)
	(epos nil)
	(semipos nil)
	(match-cons nil))

    (goto-char begin)

    ;; loop until end of last parameter
    (while notend

      ;; find first character of parameter-declaration
      (ada-goto-next-non-ws)
      (setq apos (point))

      ;; find last character of parameter-declaration
      (if (setq match-cons
	       (ada-search-ignore-string-comment "[ \t\n]*;" nil end t))
	  (progn
	    (setq epos (car match-cons))
	    (setq semipos (cdr match-cons)))
	(setq epos end))

      ;; read name(s) of parameter(s)
      (goto-char apos)
      (looking-at "\\(\\(\\sw\\|[_, \t\n]\\)*\\(\\sw\\|_\\)\\)[ \t\n]*:[^=]")

      (setq param (list (match-string 1)))
      (ada-search-ignore-string-comment ":" nil epos t 'search-forward)

      ;; look for 'in'
      (setq apos (point))
      (setq param
	   (append param
		   (list
		    (consp
		     (ada-search-ignore-string-comment
		      "in" nil epos t 'word-search-forward)))))

      ;; look for 'out'
      (goto-char apos)
      (setq param
	   (append param
		   (list
		    (consp
		     (ada-search-ignore-string-comment
		      "out" nil epos t 'word-search-forward)))))

      ;; look for 'access'
      (goto-char apos)
      (setq param
	   (append param
		   (list
		    (consp
		     (ada-search-ignore-string-comment
		      "access" nil epos t 'word-search-forward)))))

      ;; skip 'in'/'out'/'access'
      (goto-char apos)
      (ada-goto-next-non-ws)
      (while (looking-at "\\<\\(in\\|out\\|access\\)\\>")
	(forward-word 1)
	(ada-goto-next-non-ws))

      ;; read type of parameter
      ;; We accept spaces in the name, since some software like Rose
      ;; generates something like: "A : B 'Class"
      (looking-at "\\<\\(\\sw\\|[_.' \t]\\)+\\>")
      (setq param
	   (append param
		   (list (match-string 0))))

      ;; read default-expression, if there is one
      (goto-char (setq apos (match-end 0)))
      (setq param
	   (append param
		   (list
		    (if (setq match-cons
			     (ada-search-ignore-string-comment
			      ":=" nil epos t 'search-forward))
			(buffer-substring (car match-cons) epos)
		      nil))))

      ;; add this parameter-declaration to the list
      (setq paramlist (append paramlist (list param)))

      ;; check if it was the last parameter
      (if (eq epos end)
	  (setq notend nil)
	(goto-char semipos))
      )
    (reverse paramlist)))

(defun ada-insert-paramlist (paramlist)
  "Insert a formatted PARAMLIST in the buffer."
  (let ((i (length paramlist))
	(parlen 0)
	(typlen 0)
	(inp nil)
	(outp nil)
	(accessp nil)
	(column nil)
	(firstcol nil))

    ;; loop until last parameter
    (while (not (zerop i))
      (setq i (1- i))

      ;; get max length of parameter-name
      (setq parlen (max parlen (length (nth 0 (nth i paramlist)))))

      ;; get max length of type-name
      (setq typlen (max typlen (length (nth 4 (nth i paramlist)))))

      ;; is there any 'in' ?
      (setq inp (or inp (nth 1 (nth i paramlist))))

      ;; is there any 'out' ?
      (setq outp (or outp (nth 2 (nth i paramlist))))

      ;; is there any 'access' ?
      (setq accessp (or accessp (nth 3 (nth i paramlist))))
      )

    ;; does paramlist already start on a separate line ?
    (if (save-excursion
	  (re-search-backward "^.\\|[^ \t]" nil t)
	  (looking-at "^."))
	;; yes => re-indent it
	(progn
	  (ada-indent-current)
	  (save-excursion
	    (if (looking-at "\\(is\\|return\\)")
		(replace-match " \\1"))))

      ;; no => insert it where we are after removing any whitespace
      (fixup-whitespace)
      (save-excursion
	(cond
	 ((looking-at "[ \t]*\\(\n\\|;\\)")
	  (replace-match "\\1"))
	 ((looking-at "[ \t]*\\(is\\|return\\)")
	  (replace-match " \\1"))))
      (insert " "))

    (insert "(")
    (ada-indent-current)

    (setq firstcol (current-column))
    (setq i (length paramlist))

    ;; loop until last parameter
    (while (not (zerop i))
      (setq i (1- i))
      (setq column firstcol)

      ;; insert parameter-name, space and colon
      (insert (nth 0 (nth i paramlist)))
      (indent-to (+ column parlen 1))
      (insert ": ")
      (setq column (current-column))

      ;; insert 'in' or space
      (if (nth 1 (nth i paramlist))
	  (insert "in ")
	(if (and
	     (or inp
		 accessp)
	     (not (nth 3 (nth i paramlist))))
	    (insert "   ")))

      ;; insert 'out' or space
      (if (nth 2 (nth i paramlist))
	  (insert "out ")
	(if (and
	     (or outp
		 accessp)
	     (not (nth 3 (nth i paramlist))))
	    (insert "    ")))

      ;; insert 'access'
      (if (nth 3 (nth i paramlist))
	  (insert "access "))

      (setq column (current-column))

      ;; insert type-name and, if necessary, space and default-expression
      (insert (nth 4 (nth i paramlist)))
      (if (nth 5 (nth i paramlist))
	  (progn
	    (indent-to (+ column typlen 1))
	    (insert (nth 5 (nth i paramlist)))))

      ;; check if it was the last parameter
      (if (zerop i)
	  (insert ")")
	;; no => insert ';' and newline and indent
	(insert ";")
	(newline)
	(indent-to firstcol))
      )

    ;; if anything follows, except semicolon, newline, is or return
    ;; put it in a new line and indent it
    (unless (looking-at "[ \t]*\\(;\\|\n\\|is\\|return\\)")
      (ada-indent-newline-indent))
    ))



;;;----------------------------------------------------------------
;;  Indentation Engine
;;  All indentations are indicated as a two-element string:
;;     - position of reference in the buffer
;;     - offset to indent from this position (can also be a symbol or a list
;;       that are evaluated)
;;  Thus the total indentation for a line is the column number of the reference
;;  position plus whatever value the evaluation of the second element provides.
;;  This mechanism is used so that the Ada mode can "explain" how the
;;  indentation was calculated, by showing which variables were used.
;;
;;  The indentation itself is done in only one pass: first we try to guess in
;;  what context we are by looking at the following keyword or punctuation
;;  sign. If nothing remarkable is found, just try to guess the indentation
;;  based on previous lines.
;;
;;  The relevant functions for indentation are:
;;  - `ada-indent-region': Re-indent a region of text
;;  - `ada-justified-indent-current': Re-indent the current line and shows the
;;    calculation that were done
;;  - `ada-indent-current': Re-indent the current line
;;  - `ada-get-current-indent': Calculate the indentation for the current line,
;;    based on the context (see above).
;;  - `ada-get-indent-*': Calculate the indentation in a specific context.
;;    For efficiency, these functions do not check they are in the correct
;;    context.
;;;----------------------------------------------------------------

(defun ada-indent-region (beg end)
  "Indent the region between BEG end END."
  (interactive "*r")
  (goto-char beg)
  (let ((block-done 0)
	(lines-remaining (count-lines beg end))
	(msg (format "%%4d out of %4d lines remaining ..."
		     (count-lines beg end)))
	(endmark (copy-marker end)))
    ;; catch errors while indenting
    (while (< (point) endmark)
      (if (> block-done 39)
	  (progn
	    (setq lines-remaining (- lines-remaining block-done)
		  block-done     0)
	    (message msg lines-remaining)))
      (if (= (char-after) ?\n) nil
	(ada-indent-current))
      (forward-line 1)
      (setq block-done      (1+ block-done)))
    (message "Indenting ... done")))

(defun ada-indent-newline-indent ()
  "Indent the current line, insert a newline and then indent the new line."
  (interactive "*")
  (ada-indent-current)
  (newline)
  (ada-indent-current))

(defun ada-indent-newline-indent-conditional ()
  "Insert a newline and indent it.
The original line is re-indented if `ada-indent-after-return' is non-nil."
  (interactive "*")
  ;; If at end of buffer (entering brand new code), some indentation
  ;; fails.  For example, a block label requires whitespace following
  ;; the : to be recognized.  So we do the newline first, then
  ;; go back and indent the original line.
  (newline)
  (if ada-indent-after-return
      (progn
        (forward-char -1)
        (ada-indent-current)
        (forward-char 1)))
  (ada-indent-current))

(defun ada-justified-indent-current ()
  "Indent the current line and explain how the calculation was done."
  (interactive)

  (let ((cur-indent (ada-indent-current)))

    (let ((line (save-excursion
		  (goto-char (car cur-indent))
		  (count-lines 1 (point)))))

      (if (equal (cdr cur-indent) '(0))
	  (message (concat "same indentation as line " (number-to-string line)))
	(message "%s" (mapconcat (lambda(x)
				   (cond
				    ((symbolp x)
				     (symbol-name x))
				    ((numberp x)
				     (number-to-string x))
				    ((listp x)
				     (concat "- " (symbol-name (cadr x))))
				    ))
				 (cdr cur-indent)
				 " + "))))
    (save-excursion
      (goto-char (car cur-indent))
      (sit-for 1))))

(defun ada-batch-reformat ()
  "Re-indent and re-case all the files found on the command line.
This function should be used from the command line, with a
command like:
  emacs -batch -l ada-mode -f ada-batch-reformat file1 file2 ..."

  (while command-line-args-left
    (let ((source (car command-line-args-left)))
      (message "Formatting %s" source)
      (find-file source)
      (ada-indent-region (point-min) (point-max))
      (ada-adjust-case-buffer)
      (write-file source))
    (setq command-line-args-left (cdr command-line-args-left)))
  (message "Done")
  (kill-emacs 0))

(defsubst ada-goto-previous-word ()
  "Move point to the beginning of the previous word of Ada code.
Return the new position of point or nil if not found."
  (ada-goto-next-word t))

(defun ada-indent-current ()
  "Indent current line as Ada code.
Return the calculation that was done, including the reference point
and the offset."
  (interactive)
  (let ((orgpoint (point-marker))
	cur-indent tmp-indent
	prev-indent)

    (unwind-protect
	(with-syntax-table ada-mode-symbol-syntax-table

	  ;;  This need to be done here so that the advice is not always
	  ;;  activated (this might interact badly with other modes)
	  (if (featurep 'xemacs)
	      (ad-activate 'parse-partial-sexp t))

	  (save-excursion
	    (setq cur-indent

                  ;; Not First line in the buffer ?
                  (if (save-excursion (zerop (forward-line -1)))
                      (progn
                        (back-to-indentation)
                        (ada-get-current-indent))

                    ;; first line in the buffer
                    (list (point-min) 0))))

	  ;; Evaluate the list to get the column to indent to
	  ;; prev-indent contains the column to indent to
	  (if cur-indent
	      (setq prev-indent (save-excursion (goto-char (car cur-indent))
						(current-column))
		    tmp-indent (cdr cur-indent))
	    (setq prev-indent 0  tmp-indent '()))

	  (while (not (null tmp-indent))
	    (cond
	     ((numberp (car tmp-indent))
	      (setq prev-indent (+ prev-indent (car tmp-indent))))
	     (t
	      (setq prev-indent (+ prev-indent (eval (car tmp-indent)))))
	     )
	    (setq tmp-indent (cdr tmp-indent)))

	  ;; only re-indent if indentation is different then the current
	  (if (= (save-excursion (back-to-indentation) (current-column)) prev-indent)
	      nil
	    (beginning-of-line)
	    (delete-horizontal-space)
	    (indent-to prev-indent))
	  ;;
	  ;; restore position of point
	  ;;
	  (goto-char orgpoint)
	  (if (< (current-column) (current-indentation))
	      (back-to-indentation)))

      (if (featurep 'xemacs)
	  (ad-deactivate 'parse-partial-sexp)))

    cur-indent))

(defun ada-get-current-indent ()
  "Return the indentation to use for the current line."
  (let (column
	pos
	match-cons
	result
	(orgpoint (save-excursion
		    (beginning-of-line)
		    (forward-comment -10000)
		    (forward-line 1)
		    (point))))

    (setq result
    (cond

     ;;-----------------------------
     ;; in open parenthesis, but not in parameter-list
     ;;-----------------------------

     ((and ada-indent-to-open-paren
	   (not (ada-in-paramlist-p))
	   (setq column (ada-in-open-paren-p)))

      ;; check if we have something like this  (Table_Component_Type =>
      ;;                                          Source_File_Record)
      (save-excursion

	;;  Align the closing parenthesis on the opening one
	(if (= (following-char) ?\))
	    (save-excursion
	      (goto-char column)
	      (skip-chars-backward " \t")
	      (list (1- (point)) 0))

	  (if (and (skip-chars-backward " \t")
		   (= (char-before) ?\n)
		   (not (forward-comment -10000))
		   (= (char-before) ?>))
	      ;; ??? Could use a different variable
	      (list column 'ada-broken-indent)

	    ;;  We want all continuation lines to be indented the same
	    ;;  (ada-broken-line from the opening parenthesis. However, in
	    ;;  parameter list, each new parameter should be indented at the
	    ;;  column as the opening parenthesis.

	    ;;  A special case to handle nested boolean expressions, as in
	    ;;    ((B
	    ;;        and then C) --  indented by ada-broken-indent
	    ;;     or else D)     --  indenting this line.
	    ;;  ??? This is really a hack, we should have a proper way to go to
	    ;;  ??? the beginning of the statement

	    (if (= (char-before) ?\))
		(backward-sexp))

	    (if (memq (char-before) '(?, ?\; ?\( ?\)))
		(list column 0)
	      (list column 'ada-continuation-indent)
	      )))))

     ;;---------------------------
     ;;   at end of buffer
     ;;---------------------------

     ((not (char-after))
      (ada-indent-on-previous-lines nil orgpoint orgpoint))

     ;;---------------------------
     ;;  starting with e
     ;;---------------------------

     ((= (downcase (char-after)) ?e)
      (cond

       ;; -------  end  ------

       ((looking-at "end\\>")
	(let ((label 0)
	      limit)
	  (save-excursion
	    (ada-goto-matching-start 1)

	    ;;
	    ;; found 'loop' => skip back to 'while' or 'for'
	    ;;                 if 'loop' is not on a separate line
	    ;; Stop the search for 'while' and 'for' when a ';' is encountered.
	    ;;
	    (if (save-excursion
		  (beginning-of-line)
		  (looking-at ".+\\<loop\\>"))
		(progn
		  (save-excursion
		    (setq limit (car (ada-search-ignore-string-comment ";" t))))
		  (if (save-excursion
			(and
			 (setq match-cons
			      (ada-search-ignore-string-comment ada-loop-start-re t limit))
			 (not (looking-at "\\<loop\\>"))))
		      (progn
			(goto-char (car match-cons))
			(save-excursion
			  (back-to-indentation)
			  (if (looking-at ada-block-label-re)
			      (setq label (- ada-label-indent))))))))

	    ;; found 'record' =>
	    ;;  if the keyword is found at the beginning of a line (or just
	    ;;  after limited, we indent on it, otherwise we indent on the
	    ;;  beginning of the type declaration)
	    ;;      type A is (B : Integer;
	    ;;                 C : Integer) is record
	    ;;          end record;   --  This is badly indented otherwise
	    (if (looking-at "record")
		(if (save-excursion
		      (beginning-of-line)
		      (looking-at "^[ \t]*\\(record\\|limited record\\)"))
		    (list (save-excursion (back-to-indentation) (point)) 0)
		  (list (save-excursion
			  (car (ada-search-ignore-string-comment "\\<type\\>" t)))
			0))

	      ;;  Else keep the same indentation as the beginning statement
	      (list (+ (save-excursion (back-to-indentation) (point)) label) 0)))))

       ;; ------  exception  ----

       ((looking-at "exception\\>")
	(save-excursion
	  (ada-goto-matching-start 1)
	  (list (save-excursion (back-to-indentation) (point)) 0)))

       ;; else

       ((looking-at "else\\>")
	(if (save-excursion (ada-goto-previous-word)
			    (looking-at "\\<or\\>"))
	    (ada-indent-on-previous-lines nil orgpoint orgpoint)
	  (save-excursion
	    (ada-goto-matching-start 1 nil t)
	    (list (progn (back-to-indentation) (point)) 0))))

       ;; elsif

       ((looking-at "elsif\\>")
	(save-excursion
	  (ada-goto-matching-start 1 nil t)
	  (list (progn (back-to-indentation) (point)) 0)))

       ))

     ;;---------------------------
     ;;  starting with w (when)
     ;;---------------------------

     ((and (= (downcase (char-after)) ?w)
	   (looking-at "when\\>"))
      (save-excursion
	(ada-goto-matching-start 1)
	(list (save-excursion (back-to-indentation) (point))
	      'ada-when-indent)))

     ;;---------------------------
     ;;   starting with t (then)
     ;;---------------------------

     ((and (= (downcase (char-after)) ?t)
	   (looking-at "then\\>"))
      (if (save-excursion (ada-goto-previous-word)
			  (looking-at "and\\>"))
	  (ada-indent-on-previous-lines nil orgpoint orgpoint)
	(save-excursion
	  ;;  Select has been added for the statement: "select ... then abort"
	  (ada-search-ignore-string-comment
	   "\\<\\(elsif\\|if\\|select\\)\\>" t nil)
	  (list (progn (back-to-indentation) (point))
		'ada-stmt-end-indent))))

     ;;---------------------------
     ;;   starting with l (loop)
     ;;---------------------------

     ((and (= (downcase (char-after)) ?l)
	   (looking-at "loop\\>"))
      (setq pos (point))
      (save-excursion
	(goto-char (match-end 0))
	(ada-goto-stmt-start)
	(if (looking-at "\\<\\(loop\\|if\\)\\>")
	    (ada-indent-on-previous-lines nil orgpoint orgpoint)
	  (unless (looking-at ada-loop-start-re)
	    (ada-search-ignore-string-comment ada-loop-start-re
					      nil pos))
	  (if (looking-at "\\<loop\\>")
	      (ada-indent-on-previous-lines nil orgpoint orgpoint)
	    (list (progn (back-to-indentation) (point)) 'ada-stmt-end-indent)))))

     ;;----------------------------
     ;;    starting with l (limited) or r (record)
     ;;----------------------------

     ((or (and (= (downcase (char-after)) ?l)
	       (looking-at "limited\\>"))
	  (and (= (downcase (char-after)) ?r)
	       (looking-at "record\\>")))

      (save-excursion
	(ada-search-ignore-string-comment
	 "\\<\\(type\\|use\\)\\>" t nil)
	(if (looking-at "\\<use\\>")
	    (ada-search-ignore-string-comment "for" t nil nil
					      'word-search-backward))
	(list (progn (back-to-indentation) (point))
	      'ada-indent-record-rel-type)))

     ;;---------------------------
     ;;   starting with b (begin)
     ;;---------------------------

     ((and (= (downcase (char-after)) ?b)
	   (looking-at "begin\\>"))
      (save-excursion
	(if (ada-goto-decl-start t)
	    (list (progn (back-to-indentation) (point)) 0)
	  (ada-indent-on-previous-lines nil orgpoint orgpoint))))

     ;;---------------------------
     ;;   starting with i (is)
     ;;---------------------------

     ((and (= (downcase (char-after)) ?i)
	   (looking-at "is\\>"))

      (if (and ada-indent-is-separate
	       (save-excursion
		 (goto-char (match-end 0))
		 (ada-goto-next-non-ws (point-at-eol))
		 (looking-at "\\<abstract\\>\\|\\<separate\\>")))
	  (save-excursion
	    (ada-goto-stmt-start)
	    (list (progn (back-to-indentation) (point)) 'ada-indent))
	(save-excursion
	  (ada-goto-stmt-start)
	  (if (looking-at "\\<overriding\\|package\\|procedure\\|function\\>")
	      (list (progn (back-to-indentation) (point)) 0)
	    (list (progn (back-to-indentation) (point)) 'ada-indent)))))

     ;;---------------------------
     ;;  starting with r (return, renames)
     ;;---------------------------

     ((and (= (downcase (char-after)) ?r)
	   (looking-at "re\\(turn\\|names\\)\\>"))

      (save-excursion
	(let ((var 'ada-indent-return))
	  ;;  If looking at a renames, skip the 'return' statement too
	  (if (looking-at "renames")
	      (let (pos)
		(save-excursion
		  (setq pos (ada-search-ignore-string-comment ";\\|return\\>" t)))
		(if (and pos
			 (= (downcase (char-after (car pos))) ?r))
		    (goto-char (car pos)))
		(setq var 'ada-indent-renames)))

	  (forward-comment -1000)
	  (if (= (char-before) ?\))
	      (forward-sexp -1)
	    (forward-word -1))

	  ;; If there is a parameter list, and we have a function declaration
	  ;; or a access to subprogram declaration
	  (let ((num-back 1))
	    (if (and (= (following-char) ?\()
		     (save-excursion
		       (or (progn
			     (backward-word 1)
			     (looking-at "\\(function\\|procedure\\)\\>"))
			   (progn
			     (backward-word 1)
			     (setq num-back 2)
			     (looking-at "\\(function\\|procedure\\)\\>")))))

		;; The indentation depends of the value of ada-indent-return
		(if (<= (eval var) 0)
		    (list (point) (list '- var))
		  (list (progn (backward-word num-back) (point))
			var))

	      ;; Else there is no parameter list, but we have a function
	      ;; Only do something special if the user want to indent
	      ;; relative to the "function" keyword
	      (if (and (> (eval var) 0)
		       (save-excursion (forward-word -1)
				       (looking-at "function\\>")))
		  (list (progn (forward-word -1) (point)) var)

		;; Else...
		(ada-indent-on-previous-lines nil orgpoint orgpoint)))))))

     ;;--------------------------------
     ;;   starting with 'o' or 'p'
     ;;   'or'      as statement-start
     ;;   'private' as statement-start
     ;;--------------------------------

     ((and (or (= (downcase (char-after)) ?o)
	       (= (downcase (char-after)) ?p))
	   (or (ada-looking-at-semi-or)
	       (ada-looking-at-semi-private)))
      (save-excursion
	;;  ??? Wasn't this done already in ada-looking-at-semi-or ?
	(ada-goto-matching-start 1)
	(list (progn (back-to-indentation) (point)) 0)))

     ;;--------------------------------
     ;;   starting with 'd'  (do)
     ;;--------------------------------

     ((and (= (downcase (char-after)) ?d)
	   (looking-at "do\\>"))
      (save-excursion
	(ada-goto-stmt-start)
	(list (progn (back-to-indentation) (point)) 'ada-stmt-end-indent)))

     ;;--------------------------------
     ;;   starting with '-'  (comment)
     ;;--------------------------------

     ((= (char-after) ?-)
      (if ada-indent-comment-as-code

	  ;;  Indent comments on previous line comments if required
	  ;;  We must use a search-forward (even if the code is more complex),
	  ;;  since we want to find the beginning of the comment.
	  (let (pos)

	    (if (and ada-indent-align-comments
		     (save-excursion
		       (forward-line -1)
		       (beginning-of-line)
		       (while (and (not pos)
				   (search-forward "--" (point-at-eol) t))
			 (unless (ada-in-string-p)
			   (setq pos (point))))
		       pos))
		(list (- pos 2) 0)

	    ;;  Else always on previous line
	    (ada-indent-on-previous-lines nil orgpoint orgpoint)))

	;; Else same indentation as the previous line
	(list (save-excursion (back-to-indentation) (point)) 0)))

     ;;--------------------------------
     ;;   starting with '#'  (preprocessor line)
     ;;--------------------------------

     ((and (= (char-after) ?#)
	   (equal ada-which-compiler 'gnat)
	   (looking-at "#[ \t]*\\(if\\|els\\(e\\|if\\)\\|end[ \t]*if\\)"))
      (list (point-at-bol) 0))

     ;;--------------------------------
     ;;   starting with ')' (end of a parameter list)
     ;;--------------------------------

     ((and (not (eobp)) (= (char-after) ?\)))
      (save-excursion
	(forward-char 1)
	(backward-sexp 1)
	(list (point) 0)))

     ;;---------------------------------
     ;; new/abstract/separate
     ;;---------------------------------

     ((looking-at "\\(new\\|abstract\\|separate\\)\\>")
      (ada-indent-on-previous-lines nil orgpoint orgpoint))

     ;;---------------------------------
     ;; package/function/procedure
     ;;---------------------------------

     ((and (or (= (downcase (char-after)) ?p) (= (downcase (char-after)) ?f))
	   (looking-at "\\<\\(package\\|function\\|procedure\\)\\>"))
      (save-excursion
	;;  Go up until we find either a generic section, or the end of the
	;;  previous subprogram/package, or 'overriding' for this function/procedure
	(let (found)
	  (while (and (not found)
		      (ada-search-ignore-string-comment
	     "\\<\\(generic\\|end\\|begin\\|overriding\\|package\\|procedure\\|function\\)\\>" t))

	    ;;  avoid "with procedure"... in generic parts
	    (save-excursion
	      (forward-word -1)
	      (setq found (not (looking-at "with"))))))

	(cond
	 ((looking-at "\\<generic\\|overriding\\>")
	  (list (progn (back-to-indentation) (point)) 0))

	 (t
	  (ada-indent-on-previous-lines nil orgpoint orgpoint)))))

     ;;---------------------------------
     ;; label
     ;;---------------------------------

     ((looking-at ada-label-re)
      (if (ada-in-decl-p)
          ;; ada-block-label-re matches variable declarations
	  (ada-indent-on-previous-lines nil orgpoint orgpoint)
	(append (ada-indent-on-previous-lines nil orgpoint orgpoint)
		'(ada-label-indent))))

     ))

    ;;---------------------------------
    ;; Other syntaxes
    ;;---------------------------------
    (or	result (ada-indent-on-previous-lines nil orgpoint orgpoint))))

(defun ada-indent-on-previous-lines (&optional nomove orgpoint initial-pos)
  "Calculate the indentation for the new line after ORGPOINT.
The result list is based on the previous lines in the buffer.
If NOMOVE is nil, moves point to the beginning of the current statement.
if INITIAL-POS is non-nil, moves point to INITIAL-POS before calculation."
  (if initial-pos
      (goto-char initial-pos))
  (let ((oldpoint (point)))

    ;; Is inside a parameter-list ?
    (if (ada-in-paramlist-p)
	(ada-get-indent-paramlist)

      ;; Move to beginning of current statement. If already at a
      ;; statement start, move to beginning of enclosing statement.
      (unless nomove
	(ada-goto-stmt-start t))

      ;; no beginning found => don't change indentation
      (if (and (eq oldpoint (point))
	       (not nomove))
	  (ada-get-indent-nochange)

	(cond
	 ;;
	 ((and
	   ada-indent-to-open-paren
	   (ada-in-open-paren-p))
	  (ada-get-indent-open-paren))
	 ;;
	 ((looking-at "end\\>")
	  (ada-get-indent-end orgpoint))
	 ;;
	 ((looking-at ada-loop-start-re)
	  (ada-get-indent-loop orgpoint))
	 ;;
	 ((looking-at ada-subprog-start-re)
	  (ada-get-indent-subprog orgpoint))
	 ;;
	 ((looking-at ada-block-start-re)
	  (ada-get-indent-block-start orgpoint))
	 ;;
	 ((looking-at ada-block-label-re) ; also variable declaration
	  (ada-get-indent-block-label orgpoint))
	 ;;
	 ((looking-at ada-goto-label-re)
	  (ada-get-indent-goto-label orgpoint))
	 ;;
	 ((looking-at "\\(sub\\)?type\\>")
	  (ada-get-indent-type orgpoint))
	 ;;
	 ;; "then" has to be included in the case of "select...then abort"
	 ;; statements, since (goto-stmt-start) at the beginning of
	 ;; the current function would leave the cursor on that position
	 ((looking-at "\\(\\(els\\)?if\\>\\)\\|then abort\\\>")
	  (ada-get-indent-if orgpoint))
	 ;;
	 ((looking-at "case\\>")
	  (ada-get-indent-case orgpoint))
	 ;;
	 ((looking-at "when\\>")
	  (ada-get-indent-when orgpoint))
	 ;;
	 ((looking-at "separate\\>")
	  (ada-get-indent-nochange))
	 ;;
	 ((looking-at "with\\>\\|use\\>")
	  ;;  Are we still in that statement, or are we in fact looking at
	  ;;  the previous one ?
	  (if (save-excursion (search-forward ";" oldpoint t))
	      (list (progn (back-to-indentation) (point)) 0)
	    (list (point) (if (looking-at "with")
			      'ada-with-indent
			    'ada-use-indent))))
	 ;;
	 (t
	  (ada-get-indent-noindent orgpoint)))))
    ))

(defun ada-get-indent-open-paren ()
  "Calculate the indentation when point is behind an unclosed parenthesis."
  (list (ada-in-open-paren-p) 0))

(defun ada-get-indent-nochange ()
  "Return the current indentation of the previous line."
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (list (point) 0)))

(defun ada-get-indent-paramlist ()
  "Calculate the indentation when point is inside a parameter list."
  (save-excursion
    (ada-search-ignore-string-comment "[^ \t\n]" t nil t)
    (cond
     ;; in front of the first parameter
     ((= (char-after) ?\()
      (goto-char (match-end 0))
      (list (point) 0))

     ;; in front of another parameter
     ((= (char-after) ?\;)
      (goto-char (cdr (ada-search-ignore-string-comment "(\\|;" t nil t)))
      (ada-goto-next-non-ws)
      (list (point) 0))

     ;;  After an affectation (default parameter value in subprogram
     ;;  declaration)
     ((and (= (following-char) ?=) (= (preceding-char) ?:))
      (back-to-indentation)
      (list (point) 'ada-broken-indent))

     ;; inside a parameter declaration
     (t
      (goto-char (cdr (ada-search-ignore-string-comment "(\\|;" t nil t)))
      (ada-goto-next-non-ws)
      (list (point) 'ada-broken-indent)))))

(defun ada-get-indent-end (orgpoint)
  "Calculate the indentation when point is just before an end statement.
ORGPOINT is the limit position used in the calculation."
  (let ((defun-name nil)
	(indent nil))

    ;; is the line already terminated by ';' ?
    (if (save-excursion
	  (ada-search-ignore-string-comment ";" nil orgpoint nil
					    'search-forward))

	;; yes, look what's following 'end'
	(progn
	  (forward-word 1)
	  (ada-goto-next-non-ws)
	  (cond
	   ;;
	   ;; loop/select/if/case/return
	   ;;
	   ((looking-at "\\<\\(loop\\|select\\|if\\|case\\|return\\)\\>")
	    (save-excursion (ada-check-matching-start (match-string 0)))
	    (list (save-excursion (back-to-indentation) (point)) 0))

	   ;;
	   ;; record
	   ;;
	   ((looking-at "\\<record\\>")
	    (save-excursion
	      (ada-check-matching-start (match-string 0))
	      ;;  we are now looking at the matching "record" statement
	      (forward-word 1)
	      (ada-goto-stmt-start)
	      ;;  now on the matching type declaration, or use clause
	      (unless (looking-at "\\(for\\|type\\)\\>")
		(ada-search-ignore-string-comment "\\<type\\>" t))
	      (list (progn (back-to-indentation) (point)) 0)))
	   ;;
	   ;; a named block end
	   ;;
	   ((looking-at ada-ident-re)
	    (setq defun-name (match-string 0))
	    (save-excursion
	      (ada-goto-matching-start 0)
	      (ada-check-defun-name defun-name))
	    (list (progn (back-to-indentation) (point)) 0))
	   ;;
	   ;; a block-end without name
	   ;;
	   ((= (char-after) ?\;)
	    (save-excursion
	      (ada-goto-matching-start 0)
	      (if (looking-at "\\<begin\\>")
		  (progn
		    (setq indent (list (point) 0))
		    (if (ada-goto-decl-start t)
			(list (progn (back-to-indentation) (point)) 0)
		      indent))
		(list (progn (back-to-indentation) (point)) 0)
		)))
	   ;;
	   ;; anything else - should maybe signal an error ?
	   ;;
	   (t
	    (list (save-excursion (back-to-indentation) (point))
		  'ada-broken-indent))))

      (list (save-excursion (back-to-indentation) (point))
	    'ada-broken-indent))))

(defun ada-get-indent-case (orgpoint)
  "Calculate the indentation when point is just before a case statement.
ORGPOINT is the limit position used in the calculation."
  (let ((match-cons nil)
	(opos (point)))
    (cond
     ;;
     ;; case..is..when..=>
     ;;
     ((save-excursion
	(setq match-cons (and
			  ;; the `=>' must be after the keyword `is'.
			  (ada-search-ignore-string-comment
			   "is" nil orgpoint nil 'word-search-forward)
			  (ada-search-ignore-string-comment
			   "[ \t\n]+=>" nil orgpoint))))
      (save-excursion
	(goto-char (car match-cons))
	(unless (ada-search-ignore-string-comment "when" t opos)
	  (error "Missing 'when' between 'case' and '=>'"))
	(list (save-excursion (back-to-indentation) (point)) 'ada-indent)))
     ;;
     ;; case..is..when
     ;;
     ((save-excursion
	(setq match-cons (ada-search-ignore-string-comment
			  "when" nil orgpoint nil 'word-search-forward)))
      (goto-char (cdr match-cons))
      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))
     ;;
     ;; case..is
     ;;
     ((save-excursion
	(setq match-cons (ada-search-ignore-string-comment
			  "is" nil orgpoint nil 'word-search-forward)))
      (list (save-excursion (back-to-indentation) (point)) 'ada-when-indent))
     ;;
     ;; incomplete case
     ;;
     (t
      (list (save-excursion (back-to-indentation) (point))
	    'ada-broken-indent)))))

(defun ada-get-indent-when (orgpoint)
  "Calculate the indentation when point is just before a when statement.
ORGPOINT is the limit position used in the calculation."
  (let ((cur-indent (save-excursion (back-to-indentation) (point))))
    (if (ada-search-ignore-string-comment "[ \t\n]*=>" nil orgpoint)
	(list cur-indent 'ada-indent)
      (list cur-indent 'ada-broken-indent))))

(defun ada-get-indent-if (orgpoint)
  "Calculate the indentation when point is just before an if statement.
ORGPOINT is the limit position used in the calculation."
  (let ((cur-indent (save-excursion (back-to-indentation) (point)))
	(match-cons nil))
    ;;
    ;; Move to the correct then (ignore all "and then")
    ;;
    (while (and (setq match-cons (ada-search-ignore-string-comment
				  "\\<\\(then\\|and[ \t]*then\\)\\>"
				  nil orgpoint))
		(= (downcase (char-after (car match-cons))) ?a)))
    ;; If "then" was found (we are looking at it)
    (if match-cons
	(progn
	  ;;
	  ;; 'then' first in separate line ?
	  ;; => indent according to 'then',
	  ;; => else indent according to 'if'
	  ;;
	  (if (save-excursion
		(back-to-indentation)
		(looking-at "\\<then\\>"))
	      (setq cur-indent (save-excursion (back-to-indentation) (point))))
	  ;; skip 'then'
	  (forward-word 1)
	  (list cur-indent 'ada-indent))

      (list cur-indent 'ada-broken-indent))))

(defun ada-get-indent-block-start (orgpoint)
  "Calculate the indentation when point is at the start of a block.
ORGPOINT is the limit position used in the calculation."
  (let ((pos nil))
    (cond
     ((save-excursion
	(forward-word 1)
	(setq pos (ada-goto-next-non-ws orgpoint)))
      (goto-char pos)
      (save-excursion
	(ada-indent-on-previous-lines t orgpoint)))

     ;;  Special case for record types, for instance for:
     ;;     type A is (B : Integer;
     ;;                C : Integer) is record
     ;;         null;   --  This is badly indented otherwise
     ((looking-at "record")

      ;;  If record is at the beginning of the line, indent from there
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "^[ \t]*\\(record\\|limited record\\)"))
	  (list (save-excursion (back-to-indentation) (point)) 'ada-indent)

	;;  else indent relative to the type command
	(list (save-excursion
		(car (ada-search-ignore-string-comment "\\<type\\>" t)))
	      'ada-indent)))

     ;; Special case for label:
     ((looking-at ada-block-label-re)
      (list (- (save-excursion (back-to-indentation) (point)) ada-label-indent) 'ada-indent))

     ;; nothing follows the block-start
     (t
      (list (save-excursion (back-to-indentation) (point)) 'ada-indent)))))

(defun ada-get-indent-subprog (orgpoint)
  "Calculate the indentation when point is just before a subprogram.
ORGPOINT is the limit position used in the calculation."
  (let ((match-cons nil)
	(cur-indent (save-excursion (back-to-indentation) (point)))
	(foundis nil))
    ;;
    ;; is there an 'is' in front of point ?
    ;;
    (if (save-excursion
	  (setq match-cons
	       (ada-search-ignore-string-comment
		"\\<\\(is\\|do\\)\\>" nil orgpoint)))
	;;
	;; yes, then skip to its end
	;;
	(progn
	  (setq foundis t)
	  (goto-char (cdr match-cons)))
      ;;
      ;; no, then goto next non-ws, if there is one in front of point
      ;;
      (progn
	(unless (ada-goto-next-non-ws orgpoint)
	  (goto-char orgpoint))))

    (cond
     ;;
     ;; nothing follows 'is'
     ;;
     ((and
       foundis
       (save-excursion
	 (not (ada-search-ignore-string-comment
	       "[^ \t\n]" nil orgpoint t))))
      (list cur-indent 'ada-indent))
     ;;
     ;; is abstract/separate/new ...
     ;;
     ((and
       foundis
       (save-excursion
	 (setq match-cons
	      (ada-search-ignore-string-comment
	       "\\<\\(separate\\|new\\|abstract\\)\\>"
	       nil orgpoint))))
      (goto-char (car match-cons))
      (ada-search-ignore-string-comment ada-subprog-start-re t)
      (ada-get-indent-noindent orgpoint))
     ;;
     ;; something follows 'is'
     ;;
     ((and
       foundis
       (save-excursion (setq match-cons (ada-goto-next-non-ws orgpoint)))
       (goto-char match-cons)
       (ada-indent-on-previous-lines t orgpoint)))
     ;;
     ;; no 'is' but ';'
     ;;
     ((save-excursion
	(ada-search-ignore-string-comment ";" nil orgpoint nil 'search-forward))
      (list cur-indent 0))
     ;;
     ;; no 'is' or ';'
     ;;
     (t
      (list cur-indent 'ada-broken-indent)))))

(defun ada-get-indent-noindent (orgpoint)
  "Calculate the indentation when point is just before a 'noindent stmt'.
ORGPOINT is the limit position used in the calculation."
  (let ((label 0))
    (save-excursion
      (beginning-of-line)

      (cond

       ;;  This one is called when indenting a line preceded by a multi-line
       ;;  subprogram declaration (in that case, we are at this point inside
       ;;  the parameter declaration list)
       ((ada-in-paramlist-p)
	(ada-previous-procedure)
	(list (save-excursion (back-to-indentation) (point)) 0))

       ;;  This one is called when indenting the second line of a multi-line
       ;;  declaration section, in a declare block or a record declaration
       ((looking-at "[ \t]*\\(\\sw\\|_\\)*[ \t]*,[ \t]*$")
	(list (save-excursion (back-to-indentation) (point))
	      'ada-broken-decl-indent))

       ;;  This one is called in every other case when indenting a line at the
       ;;  top level
       (t
	(if (looking-at (concat "[ \t]*" ada-block-label-re))
	    (setq label (- ada-label-indent))

	  (let (p)

	    ;;  "with private" or "null record" cases
	    (if (or (save-excursion
		      (and (ada-search-ignore-string-comment "\\<private\\>" nil orgpoint)
			   (setq p (point))
			   (save-excursion (forward-char -7);; skip back "private"
					   (ada-goto-previous-word)
					   (looking-at "with"))))
		    (save-excursion
		      (and (ada-search-ignore-string-comment "\\<record\\>" nil orgpoint)
			   (setq p (point))
			   (save-excursion (forward-char -6);; skip back "record"
					   (ada-goto-previous-word)
					   (looking-at "null")))))
		(progn
		  (goto-char p)
		  (re-search-backward "\\<\\(type\\|subtype\\)\\>" nil t)
		  (list (save-excursion (back-to-indentation) (point)) 0)))))
	(if (save-excursion
	      (ada-search-ignore-string-comment ";" nil orgpoint nil
						'search-forward))
	    (list (+ (save-excursion (back-to-indentation) (point)) label) 0)
	  (list (+ (save-excursion (back-to-indentation) (point)) label)
		'ada-broken-indent)))))))

(defun ada-get-indent-block-label (orgpoint)
  "Calculate the indentation when before a label or variable declaration.
ORGPOINT is the limit position used in the calculation."
  (let ((match-cons nil)
	(cur-indent (save-excursion (back-to-indentation) (point))))
    (ada-search-ignore-string-comment ":" nil)
    (cond
     ;; loop label
     ((save-excursion
	(setq match-cons (ada-search-ignore-string-comment
			  ada-loop-start-re nil orgpoint)))
      (goto-char (car match-cons))
      (ada-get-indent-loop orgpoint))

     ;; declare label
     ((save-excursion
	(setq match-cons (ada-search-ignore-string-comment
			  "\\<declare\\|begin\\>" nil orgpoint)))
      (goto-char (car match-cons))
      (list (save-excursion (back-to-indentation) (point)) 'ada-indent))

     ;; variable declaration
     ((ada-in-decl-p)
      (if (save-excursion
	    (ada-search-ignore-string-comment ";" nil orgpoint))
	  (list cur-indent 0)
	(list cur-indent 'ada-broken-indent)))

     ;; nothing follows colon
     (t
      (list cur-indent '(- ada-label-indent))))))

(defun ada-get-indent-goto-label (orgpoint)
  "Calculate the indentation when at a goto label."
  (search-forward ">>")
  (ada-goto-next-non-ws)
  (if (>= (point) orgpoint)
      ;; labeled statement is the one we need to indent
      (list (- (point) ada-label-indent))
    ;; else indentation is indent for labeled statement
    (ada-indent-on-previous-lines t orgpoint)))

(defun ada-get-indent-loop (orgpoint)
  "Calculate the indentation when just before a loop or a for ... use.
ORGPOINT is the limit position used in the calculation."
  (let ((match-cons nil)
	(pos (point))

	;; If looking at a named block, skip the label
	(label (save-excursion
		 (back-to-indentation)
		 (if (looking-at ada-block-label-re)
		     (- ada-label-indent)
		   0))))

    (cond

     ;;
     ;; statement complete
     ;;
     ((save-excursion
	(ada-search-ignore-string-comment ";" nil orgpoint nil
					  'search-forward))
      (list (+ (save-excursion (back-to-indentation) (point)) label) 0))
     ;;
     ;; simple loop
     ;;
     ((looking-at "loop\\>")
      (setq pos (ada-get-indent-block-start orgpoint))
      (if (equal label 0)
	  pos
	(list (+ (car pos) label) (cadr pos))))

     ;;
     ;; 'for'- loop (or also a for ... use statement)
     ;;
     ((looking-at "for\\>")
      (cond
       ;;
       ;; for ... use
       ;;
       ((save-excursion
	  (and
	   (goto-char (match-end 0))
	   (ada-goto-next-non-ws orgpoint)
	   (forward-word 1)
	   (if (= (char-after) ?') (forward-word 1) t)
	   (ada-goto-next-non-ws orgpoint)
	   (looking-at "\\<use\\>")
	   ;;
	   ;; check if there is a 'record' before point
	   ;;
	   (progn
	     (setq match-cons (ada-search-ignore-string-comment
			       "record" nil orgpoint nil 'word-search-forward))
	     t)))
	(if match-cons
	    (progn
	      (goto-char (car match-cons))
	      (list (save-excursion (back-to-indentation) (point)) 'ada-indent))
	  (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))
	)

       ;;
       ;; for..loop
       ;;
       ((save-excursion
	  (setq match-cons (ada-search-ignore-string-comment
			    "loop" nil orgpoint nil 'word-search-forward)))
	(goto-char (car match-cons))
	;;
	;; indent according to 'loop', if it's first in the line;
	;; otherwise to 'for'
	;;
	(unless (save-excursion
		  (back-to-indentation)
		  (looking-at "\\<loop\\>"))
	  (goto-char pos))
	(list (+ (save-excursion (back-to-indentation) (point)) label)
	      'ada-indent))
       ;;
       ;; for-statement is broken
       ;;
       (t
	(list (+ (save-excursion (back-to-indentation) (point)) label)
	      'ada-broken-indent))))

     ;;
     ;; 'while'-loop
     ;;
     ((looking-at "while\\>")
      ;;
      ;; while..loop ?
      ;;
      (if (save-excursion
	    (setq match-cons (ada-search-ignore-string-comment
			      "loop" nil orgpoint nil 'word-search-forward)))

	  (progn
	    (goto-char (car match-cons))
	    ;;
	    ;; indent according to 'loop', if it's first in the line;
	    ;; otherwise to 'while'.
	    ;;
	    (unless (save-excursion
		      (back-to-indentation)
		      (looking-at "\\<loop\\>"))
	      (goto-char pos))
	    (list (+ (save-excursion (back-to-indentation) (point)) label)
		  'ada-indent))

	(list (+ (save-excursion (back-to-indentation) (point)) label)
	      'ada-broken-indent))))))

(defun ada-get-indent-type (orgpoint)
  "Calculate the indentation when before a type statement.
ORGPOINT is the limit position used in the calculation."
  (let ((match-dat nil))
    (cond
     ;;
     ;; complete record declaration
     ;;
     ((save-excursion
	(and
	 (setq match-dat (ada-search-ignore-string-comment
			  "end" nil orgpoint nil 'word-search-forward))
	 (ada-goto-next-non-ws)
	 (looking-at "\\<record\\>")
	 (forward-word 1)
	 (ada-goto-next-non-ws)
	 (= (char-after) ?\;)))
      (goto-char (car match-dat))
      (list (save-excursion (back-to-indentation) (point)) 0))
     ;;
     ;; record type
     ;;
     ((save-excursion
	(setq match-dat (ada-search-ignore-string-comment
			 "record" nil orgpoint nil 'word-search-forward)))
      (goto-char (car match-dat))
      (list (save-excursion (back-to-indentation) (point)) 'ada-indent))
     ;;
     ;; complete type declaration
     ;;
     ((save-excursion
	(ada-search-ignore-string-comment ";" nil orgpoint nil
					  'search-forward))
      (list (save-excursion (back-to-indentation) (point)) 0))
     ;;
     ;; "type ... is", but not "type ... is ...", which is broken
     ;;
     ((save-excursion
	(and
	 (ada-search-ignore-string-comment "is" nil orgpoint nil
					   'word-search-forward)
	 (not (ada-goto-next-non-ws orgpoint))))
      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))
     ;;
     ;; broken statement
     ;;
     (t
      (list (save-excursion (back-to-indentation) (point))
	    'ada-broken-indent)))))


;; -----------------------------------------------------------
;; -- searching and matching
;; -----------------------------------------------------------

(defun ada-goto-stmt-start (&optional ignore-goto-label)
  "Move point to the beginning of the statement that point is in or after.
Return the new position of point.
As a special case, if we are looking at a closing parenthesis, skip to the
open parenthesis."
  (let ((match-dat nil)
	(orgpoint (point)))

    (setq match-dat (ada-search-prev-end-stmt))
    (if match-dat

	;;
	;; found a previous end-statement => check if anything follows
	;;
	(unless (looking-at "declare")
	  (progn
	    (unless (save-excursion
		      (goto-char (cdr match-dat))
		      (ada-goto-next-non-ws orgpoint ignore-goto-label))
	      ;;
	      ;; nothing follows => it's the end-statement directly in
	      ;;                    front of point => search again
	      ;;
	      (setq match-dat (ada-search-prev-end-stmt)))
	    ;;
	    ;; if found the correct end-statement => goto next non-ws
	    ;;
	    (if match-dat
		(goto-char (cdr match-dat)))
	    (ada-goto-next-non-ws)
	    ))

      ;;
      ;; no previous end-statement => we are at the beginning of the
      ;;                              accessible part of the buffer
      ;;
      (progn
	(goto-char (point-min))
	;;
	;; skip to the very first statement, if there is one
	;;
	(unless (ada-goto-next-non-ws orgpoint)
	  (goto-char orgpoint))))
    (point)))


(defun ada-search-prev-end-stmt ()
  "Move point to previous end statement.
Return a cons cell whose car is the beginning and whose cdr
is the end of the match."
  (let ((match-dat nil)
	(found nil))

    ;; search until found or beginning-of-buffer
    (while
	(and
	 (not found)
	 (setq match-dat (ada-search-ignore-string-comment
			  ada-end-stmt-re t)))

      (goto-char (car match-dat))
      (unless (ada-in-open-paren-p)
	(cond

	 ((and (looking-at
		"\\<\\(record\\|loop\\|select\\|else\\|then\\)\\>")
	       (save-excursion
		 (ada-goto-previous-word)
		 (looking-at "\\<\\(end\\|or\\|and\\)\\>[ \t]*[^;]")))
	  (forward-word -1))

	 ((looking-at "is")
	  (setq found
		(and (save-excursion (ada-goto-previous-word)
				     (ada-goto-previous-word)
				     (not (looking-at "subtype")))

		    (save-excursion (goto-char (cdr match-dat))
				    (ada-goto-next-non-ws)
				    ;;  words that can go after an 'is'
				    (not (looking-at
				     (eval-when-compile
				       (concat "\\<"
					       (regexp-opt
						'("separate" "access" "array"
						  "private" "abstract" "new") t)
					       "\\>\\|("))))))))

	 ((looking-at "private")
	  (save-excursion
	    (backward-word 1)
	    (setq found (not (looking-at "is")))))

	 (t
	  (setq found t))
	)))

    (if found
	match-dat
      nil)))

(defun ada-goto-next-non-ws (&optional limit skip-goto-label)
  "Skip to next non-whitespace character.
Skips spaces, newlines and comments, and possibly goto labels.
Return `point' if moved, nil if not.
Stop the search at LIMIT.
Do not call this function from within a string."
  (unless limit
    (setq limit (point-max)))
  (while (and (<= (point) limit)
	      (or (progn (forward-comment 10000)
                         (if (and (not (eobp))
                                  (save-excursion (forward-char 1)
                                                  (ada-in-string-p)))
                             (progn (forward-sexp 1) t)))
                  (and skip-goto-label
                       (looking-at ada-goto-label-re)
                       (progn
                         (goto-char (match-end 0))
                         t)))))
  (if (< (point) limit)
      (point)
    nil)
  )


(defun ada-goto-stmt-end (&optional limit)
  "Move point to the end of the statement that point is in or before.
Return the new position of point or nil if not found.
Stop the search at LIMIT."
  (if (ada-search-ignore-string-comment ada-end-stmt-re nil limit)
      (point)
    nil))


(defun ada-goto-next-word (&optional backward)
  "Move point to the beginning of the next word of Ada code.
If BACKWARD is non-nil, jump to the beginning of the previous word.
Return the new position of point or nil if not found."
  (let ((match-cons nil)
	(orgpoint (point))
	(old-syntax (char-to-string (char-syntax ?_))))
    (modify-syntax-entry ?_ "w")
    (unless backward
      (skip-syntax-forward "w"))
    (if (setq match-cons
              (ada-search-ignore-string-comment "\\w" backward nil t))
	;;
	;; move to the beginning of the word found
	;;
	(progn
	  (goto-char (car match-cons))
	  (skip-syntax-backward "w")
	  (point))
      ;;
      ;; if not found, restore old position of point
      ;;
      (goto-char orgpoint)
      'nil)
    (modify-syntax-entry ?_ old-syntax))
  )


(defun ada-check-matching-start (keyword)
  "Signal an error if matching block start is not KEYWORD.
Moves point to the matching block start."
  (ada-goto-matching-start 0)
  (unless (looking-at (concat "\\<" keyword "\\>"))
    (error "Matching start is not '%s'" keyword)))


(defun ada-check-defun-name (defun-name)
  "Check if the name of the matching defun really is DEFUN-NAME.
Assumes point to be already positioned by `ada-goto-matching-start'.
Moves point to the beginning of the declaration."

  ;; named block without a `declare'; ada-goto-matching-start leaves
  ;; point at start of 'begin' for a block.
  (if (save-excursion
	(ada-goto-previous-word)
	(looking-at (concat "\\<" defun-name "\\> *:")))
      t                                 ; name matches
    ;; else
    ;;
    ;; 'accept' or 'package' ?
    ;;
    (unless (looking-at ada-subprog-start-re)
      (ada-goto-decl-start))
    ;;
    ;; 'begin' of 'procedure'/'function'/'task' or 'declare'
    ;;
    (save-excursion
      ;;
      ;; a named 'declare'-block ? => jump to the label
      ;;
      (if (looking-at "\\<declare\\>")
	  (progn
	    (forward-comment -1)
	    (backward-word 1))
	;;
	;; no, => 'procedure'/'function'/'task'/'protected'
	;;
	(progn
	  (forward-word 2)
	  (backward-word 1)
	  ;;
	  ;; skip 'body' 'type'
	  ;;
	  (if (looking-at "\\<\\(body\\|type\\)\\>")
	      (forward-word 1))
	  (forward-sexp 1)
	  (backward-sexp 1)))
      ;;
      ;; should be looking-at the correct name
      ;;
      (unless (looking-at (concat "\\<" defun-name "\\>"))
	(error "Matching defun has different name: %s"
	       (buffer-substring (point)
				 (progn (forward-sexp 1) (point))))))))

(defun ada-goto-decl-start (&optional noerror)
  "Move point to the declaration start of the current construct.
If NOERROR is non-nil, return nil if no match was found;
otherwise throw error."
  (let ((nest-count 1)
        (regexp (eval-when-compile
                  (concat "\\<"
                          (regexp-opt
                           '("is" "separate" "end" "declare" "if" "new" "begin" "generic" "when") t)
                          "\\>")))

	;;  first should be set to t if we should stop at the first
	;;  "begin" we encounter.
	(first t)
	(count-generic nil)
	(stop-at-when nil)
	)

    ;;  Ignore "when" most of the time, except if we are looking at the
    ;;  beginning of a block (structure:  case .. is
    ;;                                    when ... =>
    ;;                                       begin ...
    ;;                                       exception ... )
    (if (looking-at "begin")
	(setq stop-at-when t))

    (if (or
	 (looking-at "\\<\\(package\\|procedure\\|function\\)\\>")
	 (save-excursion
	   (ada-search-ignore-string-comment
	    "\\<\\(package\\|procedure\\|function\\|generic\\)\\>" t)
	   (looking-at "generic")))
	(setq count-generic t))

    ;; search backward for interesting keywords
    (while (and
	    (not (zerop nest-count))
	    (ada-search-ignore-string-comment regexp t))
      ;;
      ;; calculate nest-depth
      ;;
      (cond
       ;;
       ((looking-at "end")
	(ada-goto-matching-start 1 noerror)

	;;  In some case, two begin..end block can follow each other closely,
	;;  which we have to detect, as in
	;;     procedure P is
	;;        procedure Q is
	;;        begin
	;;        end;
	;;     begin    --  here we should go to procedure, not begin
	;;     end

	(if (looking-at "begin")
	    (let ((loop-again t))
	      (save-excursion
		(while loop-again
		  ;;  If begin was just there as the beginning of a block
		  ;;  (with no declare) then do nothing, otherwise just
		  ;;  register that we have to find the statement that
		  ;;  required the begin

		  (ada-search-ignore-string-comment
		   "\\<\\(declare\\|begin\\|end\\|procedure\\|function\\|task\\|package\\)\\>"
		   t)

		  (if (looking-at "end")
		      (ada-goto-matching-start 1 noerror t)

		    (setq loop-again nil)
		    (unless (looking-at "begin")
		      (setq nest-count (1+ nest-count))))
		  ))
	      )))
       ;;
       ((looking-at "generic")
	(if count-generic
	    (progn
	      (setq first nil)
	      (setq nest-count (1- nest-count)))))
       ;;
       ((looking-at "if")
	(save-excursion
	  (forward-word -1)
	  (unless (looking-at "\\<end[ \t\n]*if\\>")
	    (progn
	      (setq nest-count (1- nest-count))
	      (setq first nil)))))

       ;;
       ((looking-at "declare\\|generic")
	(setq nest-count (1- nest-count))
	(setq first t))
       ;;
       ((looking-at "is")
        ;; look for things to ignore
        (if
            (or
             ;; generic formal parameter
             (looking-at "is[ t]+<>")

             ;; A type definition, or a case statement.  Note that the
             ;; goto-matching-start above on 'end record' leaves us at
             ;; 'record', not at 'type'.
             ;;
             ;; We get to a case statement here by calling
             ;; 'ada-move-to-end' from inside a case statement; then
             ;; we are not ignoring 'when'.
             (save-excursion
               ;; Skip type discriminants or case argument function call param list
               (forward-comment -10000)
               (forward-char -1)
               (if (= (char-after) ?\))
                   (progn
                     (forward-char 1)
                     (backward-sexp 1)
                     (forward-comment -10000)
                     ))
               ;; skip type or case argument name
               (skip-chars-backward "a-zA-Z0-9_.'")
               (ada-goto-previous-word)
               (and
                ;; if it's a protected type, it's the decl start we
                ;; are looking for; since we didn't see the 'end'
                ;; above, we are inside it.
                (looking-at "\\<\\(sub\\)?type\\|case\\>")
		   (save-match-data
		     (ada-goto-previous-word)
		     (not (looking-at "\\<protected\\>"))))
               )                    ; end of type definition p

             ;; null procedure declaration
             (save-excursion (ada-goto-next-word) (looking-at "\\<null\\>"))
             );; end or
            ;; skip this construct
            nil
          ;; this is the right "is"
          (setq nest-count (1- nest-count))
          (setq first nil)))

       ;;
       ((looking-at "new")
	(if (save-excursion
	      (ada-goto-previous-word)
	      (looking-at "is"))
	    (goto-char (match-beginning 0))))
       ;;
       ((and first
	     (looking-at "begin"))
	(setq nest-count 0))
       ;;
       ((looking-at "when")
	(save-excursion
	   (forward-word -1)
	   (unless (looking-at "\\<exit[ \t\n]*when\\>")
	     (progn
	       (if stop-at-when
		   (setq nest-count (1- nest-count)))
	       ))))
       ;;
       ((looking-at "begin")
	(setq first nil))
       ;;
       (t
	(setq nest-count (1+ nest-count))
	(setq first nil)))

      );; end of loop

    ;; check if declaration-start is really found
    (if (and
	 (zerop nest-count)
	 (if (looking-at "is")
	     (ada-search-ignore-string-comment ada-subprog-start-re t)
	   (looking-at "declare\\|generic")))
	t
      (if noerror nil
	(error "No matching proc/func/task/declare/package/protected")))
    ))

(defun ada-goto-matching-start (&optional nest-level noerror gotothen)
  "Move point to the beginning of a block-start.
Which block depends on the value of NEST-LEVEL, which defaults to zero.
If NOERROR is non-nil, it only returns nil if no matching start was found.
If GOTOTHEN is non-nil, point moves to the 'then' following 'if'."
  (let ((nest-count (if nest-level nest-level 0))
	(found nil)

	(last-was-begin '())
	;;  List all keywords encountered while traversing
	;;  something like '("end" "end" "begin")
	;;  This is removed from the list when "package", "procedure",...
	;;  are seen. The goal is to find whether a package has an elaboration
	;;  part

	(pos nil))

    ;; search backward for interesting keywords
    (while (and
	    (not found)
	    (ada-search-ignore-string-comment ada-matching-start-re t))

      (unless (and (looking-at "\\<record\\>")
		   (save-excursion
		     (forward-word -1)
		     (looking-at "\\<null\\>")))
	(progn
	  ;; calculate nest-depth
	  (cond
	   ;; found block end => increase nest depth
	   ((looking-at "end")
	    (push nil last-was-begin)
	    (setq nest-count (1+ nest-count)))

	   ;; found loop/select/record/case/if => check if it starts or
	   ;; ends a block
	   ((looking-at "loop\\|select\\|record\\|case\\|if")
	    (setq pos (point))
	    (save-excursion
	      ;; check if keyword follows 'end'
	      (ada-goto-previous-word)
	      (if (looking-at "\\<end\\>[ \t]*[^;]")
		  (progn
		    ;; it ends a block => increase nest depth
		    (setq nest-count (1+ nest-count)
			  pos        (point))
		    (push nil last-was-begin))

		;; it starts a block => decrease nest depth
		(setq nest-count (1- nest-count))

		;; Some nested  "begin .. end" blocks with no "declare"?
		;;  => remove those entries
		(while (car last-was-begin)
		  (setq last-was-begin (cdr (cdr last-was-begin))))

		(setq last-was-begin (cdr last-was-begin))
		))
	    (goto-char pos)
	    )

	   ;; found package start => check if it really is a block
	   ((looking-at "package")
	    (save-excursion
	      ;; ignore if this is just a renames statement
	      (let ((current (point))
		    (pos (ada-search-ignore-string-comment
			  "\\<\\(is\\|renames\\|;\\)\\>" nil)))
		(if pos
		    (goto-char (car pos))
		  (error (concat
			  "No matching 'is' or 'renames' for 'package' at"
			  " line "
			  (number-to-string (count-lines 1 (1+ current)))))))
	      (unless (looking-at "renames")
		(progn
		  (forward-word 1)
		  (ada-goto-next-non-ws)
		  ;; ignore it if it is only a declaration with 'new'
		  ;; We could have  package Foo is new ....
		  ;;  or            package Foo is separate;
		  ;;  or            package Foo is begin null; end Foo
		  ;;                     for elaboration code (elaboration)
		  (if (and (not (looking-at "\\<\\(new\\|separate\\|begin\\)\\>"))
			   (not (car last-was-begin)))
		      (setq nest-count (1- nest-count))))))

	    (setq last-was-begin (cdr last-was-begin))
	    )
	   ;; found task start => check if it has a body
	   ((looking-at "task")
	    (save-excursion
	      (forward-word 1)
	      (ada-goto-next-non-ws)
	      (cond
	       ((looking-at "\\<body\\>"))
	       ((looking-at "\\<type\\>")
		;;  In that case, do nothing if there is a "is"
		(forward-word 2);; skip "type"
		(ada-goto-next-non-ws);; skip type name

		;; Do nothing if we are simply looking at a simple
		;; "task type name;" statement with no block
		(unless (looking-at ";")
		  (progn
		    ;; Skip the parameters
		    (if (looking-at "(")
			(ada-search-ignore-string-comment ")" nil))
		    (let ((tmp (ada-search-ignore-string-comment
				"\\<\\(is\\|;\\)\\>" nil)))
		      (if tmp
			  (progn
			    (goto-char (car tmp))
			    (if (looking-at "is")
				(setq nest-count (1- nest-count)))))))))
	       (t
		;; Check if that task declaration had a block attached to
		;; it (i.e do nothing if we have just "task name;")
		(unless (progn (forward-word 1)
			       (looking-at "[ \t]*;"))
		  (setq nest-count (1- nest-count))))))
	    (setq last-was-begin (cdr last-was-begin))
	    )

	   ((looking-at "declare")
	    ;;  remove entry for begin and end (include nested begin..end
	    ;;  groups)
	    (setq last-was-begin (cdr last-was-begin))
	    (let ((count 1))
	      (while (and (> count 0))
		(if (equal (car last-was-begin) t)
		    (setq count (1+ count))
		  (setq count (1- count)))
		(setq last-was-begin (cdr last-was-begin))
		)))

	   ((looking-at "protected")
	    ;; Ignore if this is just a declaration
	    (save-excursion
	      (let ((pos (ada-search-ignore-string-comment
			  "\\(\\<is\\>\\|\\<renames\\>\\|;\\)" nil)))
		(if pos
		    (goto-char (car pos)))
		(if (looking-at "is")
		    ;;  remove entry for end
		    (setq last-was-begin (cdr last-was-begin)))))
	    (setq nest-count     (1- nest-count)))

	   ((or (looking-at "procedure")
		(looking-at "function"))
	    ;; Ignore if this is just a declaration
	    (save-excursion
	      (let ((pos (ada-search-ignore-string-comment
			  "\\(\\<is\\>\\|\\<renames\\>\\|)[ \t]*;\\)" nil)))
		(if pos
		    (goto-char (car pos)))
		(if (looking-at "is")
		    ;;  remove entry for begin and end
		    (setq last-was-begin (cdr (cdr last-was-begin))))))
	    )

	   ;; all the other block starts
	   (t
	    (push (looking-at "begin") last-was-begin)
	    (setq nest-count (1- nest-count)))

	   )

	  ;; match is found, if nest-depth is zero
	  (setq found (zerop nest-count))))) ; end of loop

    (if (bobp)
	(point)
      (if found
	  ;;
	  ;; match found => is there anything else to do ?
	  ;;
	  (progn
	    (cond
	     ;;
	     ;; found 'if' => skip to 'then', if it's on a separate line
	     ;;                               and GOTOTHEN is non-nil
	     ;;
	     ((and
	       gotothen
	       (looking-at "if")
	       (save-excursion
		 (ada-search-ignore-string-comment "then" nil nil nil
						   'word-search-forward)
		 (back-to-indentation)
		 (looking-at "\\<then\\>")))
	      (goto-char (match-beginning 0)))

	     ;;
	     ;; found 'do' => skip back to 'accept' or 'return'
	     ;;
	     ((looking-at "do")
	      (unless (ada-search-ignore-string-comment
		       "\\<accept\\|return\\>" t)
		(error "Missing 'accept' or 'return' in front of 'do'"))))
	    (point))

	(if noerror
	    nil
	  (error "No matching start"))))))


(defun ada-goto-matching-end (&optional nest-level noerror)
  "Move point to the end of a block.
Which block depends on the value of NEST-LEVEL, which defaults to zero.
If NOERROR is non-nil, it only returns nil if no matching start found."
  (let ((nest-count (or nest-level 0))
	(regex (eval-when-compile
		 (concat "\\<"
			 (regexp-opt '("end" "loop" "select" "begin" "case"
				       "if" "task" "package" "record" "do"
				       "procedure" "function") t)
			 "\\>")))
	found
	pos

	;;  First is used for subprograms: they are generally handled
	;;  recursively, but of course we do not want to do that the
	;;  first time (see comment below about subprograms)
	(first (not (looking-at "declare"))))

    ;;  If we are already looking at one of the keywords, this shouldn't count
    ;;  in the nesting loop below, so we just make sure we don't count it.
    ;;  "declare" is a special case because we need to look after the "begin"
    ;;  keyword
    (if (looking-at "\\<if\\|loop\\|case\\|begin\\>")
	(forward-char 1))

    ;;
    ;; search forward for interesting keywords
    ;;
    (while (and
	    (not found)
	    (ada-search-ignore-string-comment regex nil))

      ;;
      ;; calculate nest-depth
      ;;
      (backward-word 1)
      (cond
       ;; procedures and functions need to be processed recursively, in
       ;; case they are defined in a declare/begin block, as in:
       ;;    declare  --  NL 0   (nested level)
       ;;      A : Boolean;
       ;;      procedure B (C : D) is
       ;;      begin --  NL 1
       ;;         null;
       ;;      end B;   --  NL 0, and we would exit
       ;;    begin
       ;;    end; --  we should exit here
       ;; processing them recursively avoids the need for any special
       ;; handling.
       ;; Nothing should be done if we have only the specs or a
       ;; generic instantiation.

       ((and (looking-at "\\<procedure\\|function\\>"))
	(if first
	    (forward-word 1)

	  (setq pos (point))
	  (ada-search-ignore-string-comment "is\\|;")
	  (if (= (char-before) ?s)
	      (progn
		(ada-goto-next-non-ws)
		(unless (looking-at "\\<new\\>")
		  (progn
		    (goto-char pos)
		    (ada-goto-matching-end 0 t)))))))

       ;; found block end => decrease nest depth
       ((looking-at "\\<end\\>")
	(setq nest-count (1- nest-count)
	      found (<= nest-count 0))
	 ;; skip the following keyword
	(if (progn
	      (skip-chars-forward "end")
	      (ada-goto-next-non-ws)
	      (looking-at "\\<\\(loop\\|select\\|record\\|case\\|if\\)\\>"))
	    (forward-word 1)))

       ;; found package start => check if it really starts a block, and is not
       ;; in fact a generic instantiation for instance
       ((looking-at "\\<package\\>")
	(ada-search-ignore-string-comment "is" nil nil nil
					  'word-search-forward)
	(ada-goto-next-non-ws)
	;; ignore and skip it if it is only a 'new' package
	(if (looking-at "\\<new\\>")
	    (goto-char (match-end 0))
	  (setq nest-count (1+ nest-count)
		found      (<= nest-count 0))))

       ;; all the other block starts
       (t
	(if (not first)
	    (setq nest-count (1+ nest-count)))
	(setq found      (<= nest-count 0))
	(forward-word 1)))              ; end of 'cond'

      (setq first nil))

    (if found
	t
      (if noerror
	  nil
	(error "No matching end")))
    ))


(defun ada-search-ignore-string-comment
  (search-re &optional backward limit paramlists search-func)
  "Regexp-search for SEARCH-RE, ignoring comments, strings.
Returns a cons cell of begin and end of match data or nil, if not found.
If BACKWARD is non-nil, search backward; search forward otherwise.
The search stops at pos LIMIT.
If PARAMLISTS is nil, ignore parameter lists.
The search is done using SEARCH-FUNC.  SEARCH-FUNC can be optimized
in case we are searching for a constant string.
Point is moved at the beginning of the SEARCH-RE."
  (let (found
	begin
	end
	parse-result)

    ;; FIXME: need to pass BACKWARD to search-func!
    (unless search-func
      (setq search-func (if backward 're-search-backward 're-search-forward)))

    ;;
    ;; search until found or end-of-buffer
    ;; We have to test that we do not look further than limit
    ;;
    (with-syntax-table ada-mode-symbol-syntax-table
      (while (and (not found)
                  (or (not limit)
                      (or (and backward (<= limit (point)))
                          (>= limit (point))))
                  (funcall search-func search-re limit 1))
        (setq begin (match-beginning 0))
        (setq end (match-end 0))
        (setq parse-result (parse-partial-sexp (point-at-bol) (point)))
        (cond
         ;;
         ;; If inside a string, skip it (and the following comments)
         ;;
         ((ada-in-string-p parse-result)
          (if (featurep 'xemacs)
              (search-backward "\"" nil t)
            (goto-char (nth 8 parse-result)))
          (unless backward (forward-sexp 1)))
         ;;
         ;; If inside a comment, skip it (and the following comments)
         ;; There is a special code for comments at the end of the file
         ;;
         ((ada-in-comment-p parse-result)
          (if (featurep 'xemacs)
              (progn
                (forward-line 1)
                (beginning-of-line)
                (forward-comment -1))
            (goto-char (nth 8 parse-result)))
          (unless backward
            ;;  at the end of the file, it is not possible to skip a comment
            ;;  so we just go at the end of the line
            (if (forward-comment 1)
                (progn
                  (forward-comment 1000)
                  (beginning-of-line))
              (end-of-line))))
         ;;
         ;; directly in front of a comment => skip it, if searching forward
         ;;
         ((and (= (char-after begin) ?-) (= (char-after (1+ begin)) ?-))
          (unless backward (progn (forward-char -1) (forward-comment 1000))))

         ;;
         ;; found a parameter-list but should ignore it => skip it
         ;;
         ((and (not paramlists) (ada-in-paramlist-p))
          (if backward
              (search-backward "(" nil t)
            (search-forward ")" nil t)))
         ;;
         ;; found what we were looking for
         ;;
         (t
          (setq found t)))))            ; end of loop

    (if found
	(cons begin end)
      nil)))

;; -------------------------------------------------------
;; --  Testing the position of the cursor
;; -------------------------------------------------------

(defun ada-in-decl-p ()
  "Return t if point is inside a declarative part.
Assumes point to be at the end of a statement."
  (or (ada-in-paramlist-p)
      (save-excursion
	(ada-goto-decl-start t))))


(defun ada-looking-at-semi-or ()
  "Return t if looking at an 'or' following a semicolon."
  (save-excursion
    (and (looking-at "\\<or\\>")
	 (progn
	   (forward-word 1)
	   (ada-goto-stmt-start)
	   (looking-at "\\<or\\>")))))


(defun ada-looking-at-semi-private ()
  "Return t if looking at the start of a private section in a package.
Return nil if the private is part of the package name, as in
'private package A is...' (this can only happen at top level)."
  (save-excursion
    (and (looking-at "\\<private\\>")
	 (not (looking-at "\\<private[ \t]*\\(package\\|generic\\)"))

	 ;;  Make sure this is the start of a private section (ie after
	 ;;  a semicolon or just after the package declaration, but not
	 ;;  after a 'type ... is private' or 'is new ... with private'
	 ;;
	 ;;  Note that a 'private' statement at the beginning of the buffer
	 ;;  does not indicate a private section, since this is instead a
	 ;;  'private procedure ...'
	 (progn (forward-comment -1000)
		(and (not (bobp))
		     (or (= (char-before) ?\;)
			 (and (forward-word -3)
			      (looking-at "\\<package\\>"))))))))


(defun ada-in-paramlist-p ()
  "Return t if point is inside the parameter-list of a declaration, but not a subprogram call or aggregate."
  (save-excursion
    (and
     (ada-search-ignore-string-comment "(\\|)" t nil t)
     ;; inside parentheses ?
     (= (char-after) ?\()

     ;; We could be looking at two things here:
     ;;  operator definition:   function "." (
     ;;  subprogram definition: procedure .... (
     ;; Let's skip back over the first one
     (progn
       (skip-chars-backward " \t\n")
       (if (= (char-before) ?\")
	   (backward-char 3)
	 (backward-word 1))
       t)

     ;; and now over the second one
     (backward-word 1)

     ;; We should ignore the case when the reserved keyword is in a
     ;; comment (for instance, when we have:
     ;;    -- .... package
     ;;    Test (A)
     ;; we should return nil

     (not (ada-in-string-or-comment-p))

     ;; right keyword two words before parenthesis ?
     ;; Type is in this list because of discriminants
     ;; pragma is not, because the syntax is that of a subprogram call.
     (looking-at (eval-when-compile
		   (concat "\\<\\("
			   "procedure\\|function\\|body\\|"
			   "task\\|entry\\|accept\\|"
			   "access[ \t]+procedure\\|"
			   "access[ \t]+function\\|"
			   "type\\)\\>"))))))

(defun ada-search-ignore-complex-boolean (regexp backwardp)
  "Search for REGEXP, ignoring comments, strings, 'and then', 'or else'.
If BACKWARDP is non-nil, search backward; search forward otherwise."
  (let (result)
  (while (and (setq result (ada-search-ignore-string-comment regexp backwardp))
	      (save-excursion (forward-word -1)
			      (looking-at "and then\\|or else"))))
  result))

(defun ada-in-open-paren-p ()
  "Non-nil if in an open parenthesis.
Return value is the position of the first non-ws behind the last unclosed
parenthesis, or nil."
  (save-excursion
    (let ((parse (parse-partial-sexp
		  (point)
		  (or (car (ada-search-ignore-complex-boolean
			    "\\<\\(;\\|is\\|then\\|loop\\|begin\\|else\\)\\>"
			    t))
		      (point-min)))))

      (if (nth 1 parse)
	  (progn
	    (goto-char (1+ (nth 1 parse)))

	    ;;  Skip blanks, if they are not followed by a comment
	    ;;  See:
	    ;;  type A is (   Value_0,
	    ;;                Value_1);
	    ;;  type B is (   --  comment
	    ;;             Value_2);

	    (if (or (not ada-indent-handle-comment-special)
		    (not (looking-at "[ \t]+--")))
		(skip-chars-forward " \t"))

	    (point))))))


;; -----------------------------------------------------------
;; --  Behavior Of TAB Key
;; -----------------------------------------------------------

(defun ada-tab ()
  "Do indenting or tabbing according to `ada-tab-policy'.
In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate only on the current line."
  (interactive)
  (cond ((eq ada-tab-policy 'indent-rigidly) (ada-tab-hard))
	((eq ada-tab-policy 'indent-auto)
	 (if (ada-region-selected)
	     (ada-indent-region (region-beginning) (region-end))
	   (ada-indent-current)))
	((eq ada-tab-policy 'always-tab) (error "Not implemented"))
	))

(defun ada-untab (_arg)
  "Delete leading indenting according to `ada-tab-policy'."
  ;; FIXME: ARG is ignored
  (interactive "P")
  (cond ((eq ada-tab-policy 'indent-rigidly) (ada-untab-hard))
	((eq ada-tab-policy 'indent-auto) (error "Not implemented"))
	((eq ada-tab-policy 'always-tab) (error "Not implemented"))
	))

(defun ada-indent-current-function ()
  "Ada mode version of the `indent-line-function'."
  (interactive "*")
  (let ((starting-point (point-marker)))
    (beginning-of-line)
    (ada-tab)
    (if (< (point) starting-point)
	(goto-char starting-point))
    (set-marker starting-point nil)
    ))

(defun ada-tab-hard ()
  "Indent current line to next tab stop."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert-char ?  ada-indent))
  (if (bolp) (forward-char ada-indent)))

(defun ada-untab-hard ()
  "Indent current line to previous tab stop."
  (interactive)
  (indent-rigidly (point-at-bol) (point-at-eol) (- 0 ada-indent)))


;; ------------------------------------------------------------
;; --  Miscellaneous
;; ------------------------------------------------------------

;;  Not needed any more for Emacs 21.2, but still needed for backward
;;  compatibility
(defun ada-remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "[ \t]+$" (point-max) t)
	  (replace-match "" nil nil))))))

(defun ada-gnat-style ()
  "Clean up comments, `(' and `,' for GNAT style checking switch."
  (interactive)
  (save-excursion

    ;;  The \n is required, or the line after an empty comment line is
    ;;  simply ignored.
    (goto-char (point-min))
    (while (re-search-forward "--[ \t]*\\([^-\n]\\)" nil t)
      (replace-match "--  \\1")
      (forward-line 1)
      (beginning-of-line))

    (goto-char (point-min))
    (while (re-search-forward "\\>(" nil t)
      (if (not (ada-in-string-or-comment-p))
	  (replace-match " (")))
    (goto-char (point-min))
    (while (re-search-forward ";--" nil t)
      (forward-char -1)
      (if (not (ada-in-string-or-comment-p))
	  (replace-match "; --")))
    (goto-char (point-min))
    (while (re-search-forward "([ \t]+" nil t)
      (if (not (ada-in-string-or-comment-p))
	  (replace-match "(")))
    (goto-char (point-min))
    (while (re-search-forward ")[ \t]+)" nil t)
      (if (not (ada-in-string-or-comment-p))
	  (replace-match "))")))
    (goto-char (point-min))
    (while (re-search-forward "\\>:" nil t)
      (if (not (ada-in-string-or-comment-p))
	  (replace-match " :")))

    ;;  Make sure there is a space after a ','.
    ;;  Always go back to the beginning of the match, since otherwise
    ;;  a statement like  ('F','D','E') is incorrectly modified.
    (goto-char (point-min))
    (while (re-search-forward ",[ \t]*\\(.\\)" nil t)
      (if (not (save-excursion
		 (goto-char (match-beginning 0))
		 (ada-in-string-or-comment-p)))
	  (replace-match ", \\1")))

    ;;  Operators should be surrounded by spaces.
    (goto-char (point-min))
    (while (re-search-forward
	    "[ \t]*\\(/=\\|\\*\\*\\|:=\\|\\.\\.\\|[-:+*/]\\)[ \t]*"
	    nil t)
      (goto-char (match-beginning 1))
      (if (or (looking-at "--")
	      (ada-in-string-or-comment-p))
	  (progn
	    (forward-line 1)
	    (beginning-of-line))
	(cond
	 ((string= (match-string 1) "/=")
	  (replace-match " /= "))
	 ((string= (match-string 1) "..")
	  (replace-match " .. "))
	 ((string= (match-string 1) "**")
	  (replace-match " ** "))
	 ((string= (match-string 1) ":=")
	  (replace-match " := "))
	 (t
	  (replace-match " \\1 ")))
	(forward-char 1)))
    ))



;; -------------------------------------------------------------
;; --  Moving To Procedures/Packages/Statements
;; -------------------------------------------------------------

(defun ada-move-to-start ()
  "Move point to the matching start of the current Ada structure."
  (interactive)
  (let ((pos (point)))
    (with-syntax-table ada-mode-symbol-syntax-table

      (save-excursion
        ;;
        ;; do nothing if in string or comment or not on 'end ...;'
        ;;            or if an error occurs during processing
        ;;
        (or
         (ada-in-string-or-comment-p)
         (and (progn
                (or (looking-at "[ \t]*\\<end\\>")
                    (backward-word 1))
                (or (looking-at "[ \t]*\\<end\\>")
                    (backward-word 1))
                (or (looking-at "[ \t]*\\<end\\>")
                    (error "Not on end ...;")))
              (ada-goto-matching-start 1)
              (setq pos (point))

              ;;
              ;; on 'begin' => go on, according to user option
              ;;
              ada-move-to-declaration
              (looking-at "\\<begin\\>")
              (ada-goto-decl-start)
              (setq pos (point))))

        )                               ; end of save-excursion

      ;; now really move to the found position
      (goto-char pos))))

(defun ada-move-to-end ()
  "Move point to the end of the block around point.
Moves to 'begin' if in a declarative part."
  (interactive)
  (let ((pos (point))
	decl-start)
    (with-syntax-table ada-mode-symbol-syntax-table

      (save-excursion

        (cond
         ;; Go to the beginning of the current word, and check if we are
         ;; directly on 'begin'
         ((save-excursion
            (skip-syntax-backward "w")
            (looking-at "\\<begin\\>"))
          (ada-goto-matching-end 1))

         ;; on first line of subprogram body
         ;; Do nothing for specs or generic instantiation, since these are
         ;; handled as the general case (find the enclosing block)
         ;; We also need to make sure that we ignore nested subprograms
         ((save-excursion
            (and (skip-syntax-backward "w")
                 (looking-at "\\<function\\>\\|\\<procedure\\>" )
                 (ada-search-ignore-string-comment "is\\|;")
                 (not (= (char-before) ?\;))
                 ))
          (skip-syntax-backward "w")
          (ada-goto-matching-end 0 t))

         ;; on first line of task declaration
         ((save-excursion
            (and (ada-goto-stmt-start)
                 (looking-at "\\<task\\>" )
                 (forward-word 1)
                 (ada-goto-next-non-ws)
                 (looking-at "\\<body\\>")))
          (ada-search-ignore-string-comment "begin" nil nil nil
                                            'word-search-forward))
         ;; accept block start
         ((save-excursion
            (and (ada-goto-stmt-start)
                 (looking-at "\\<accept\\>" )))
          (ada-goto-matching-end 0))
         ;; package start
         ((save-excursion
            (setq decl-start (and (ada-goto-decl-start t) (point)))
            (and decl-start (looking-at "\\<package\\>")))
          (ada-goto-matching-end 1))

         ;;  On a "declare" keyword
         ((save-excursion
            (skip-syntax-backward "w")
            (looking-at "\\<declare\\>"))
          (ada-goto-matching-end 0 t))

         ;; inside a 'begin' ... 'end' block
         (decl-start
          (goto-char decl-start)
          (ada-goto-matching-end 0 t))

         ;; (hopefully ;-) everything else
         (t
          (ada-goto-matching-end 1)))
        (setq pos (point))
        )

      ;; now really move to the position found
      (goto-char pos))))

(defun ada-next-procedure ()
  "Move point to next procedure."
  (interactive)
  (end-of-line)
  (if (re-search-forward ada-procedure-start-regexp nil t)
      (goto-char (match-beginning 4))
    (error "No more functions/procedures/tasks")))

(defun ada-previous-procedure ()
  "Move point to previous procedure."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward ada-procedure-start-regexp nil t)
      (goto-char (match-beginning 4))
    (error "No more functions/procedures/tasks")))

(defun ada-next-package ()
  "Move point to next package."
  (interactive)
  (end-of-line)
  (if (re-search-forward ada-package-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more packages")))

(defun ada-previous-package ()
  "Move point to previous package."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward ada-package-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more packages")))


;; ------------------------------------------------------------
;; --  Define keymap and menus for Ada
;; -------------------------------------------------------------

(defun ada-create-keymap ()
  "Create the keymap associated with the Ada mode."

  ;; All non-standard keys go into ada-mode-extra-map
  (define-key ada-mode-map ada-mode-extra-prefix ada-mode-extra-map)

  ;; Indentation and Formatting
  (define-key ada-mode-map "\C-j"     'ada-indent-newline-indent-conditional)
  (define-key ada-mode-map "\C-m"     'ada-indent-newline-indent-conditional)
  (define-key ada-mode-map "\t"       'ada-tab)
  (define-key ada-mode-map "\C-c\t"   'ada-justified-indent-current)
  (define-key ada-mode-map "\C-c\C-l" 'ada-indent-region)
  (define-key ada-mode-map [(shift tab)]    'ada-untab)
  (define-key ada-mode-map "\C-c\C-f" 'ada-format-paramlist)
  ;; We don't want to make meta-characters case-specific.

  ;; Movement
  (define-key ada-mode-map "\M-\C-e"  'ada-next-procedure)
  (define-key ada-mode-map "\M-\C-a"  'ada-previous-procedure)
  (define-key ada-mode-map "\C-c\C-a" 'ada-move-to-start)
  (define-key ada-mode-map "\C-c\C-e" 'ada-move-to-end)

  ;; Compilation
  (unless (lookup-key ada-mode-map "\C-c\C-c")
    (define-key ada-mode-map "\C-c\C-c" 'compile))

  ;; Casing
  (define-key ada-mode-map "\C-c\C-b" 'ada-adjust-case-buffer)
  (define-key ada-mode-map "\C-c\C-t" 'ada-case-read-exceptions)
  (define-key ada-mode-map "\C-c\C-y" 'ada-create-case-exception)
  (define-key ada-mode-map "\C-c\C-\M-y" 'ada-create-case-exception-substring)

  ;; On XEmacs, you can easily specify whether DEL should deletes
  ;; one character forward or one character backward. Take this into
  ;; account
  (define-key ada-mode-map
    (if (boundp 'delete-key-deletes-forward) [backspace] "\177")
    'backward-delete-char-untabify)

  ;; Make body
  (define-key ada-mode-map "\C-c\C-n" 'ada-make-subprogram-body)

  ;; Use predefined function of Emacs19 for comments (RE)
  (define-key ada-mode-map "\C-c;"    'comment-region)
  (define-key ada-mode-map "\C-c:"    'ada-uncomment-region)

  ;; The following keys are bound to functions defined in ada-xref.el or
  ;; ada-prj,el., However, RMS rightly thinks that the code should be shared,
  ;; and activated only if the right compiler is used

  (define-key ada-mode-map (if (featurep 'xemacs) '(shift button3) [S-mouse-3])
    'ada-point-and-xref)
  (define-key ada-mode-map [(control tab)] 'ada-complete-identifier)

  (define-key ada-mode-extra-map "o"     'ff-find-other-file)
  (define-key ada-mode-map "\C-c5\C-d" 'ada-goto-declaration-other-frame)
  (define-key ada-mode-map "\C-c\C-d"  'ada-goto-declaration)
  (define-key ada-mode-map "\C-c\C-s"  'ada-xref-goto-previous-reference)
  (define-key ada-mode-map "\C-c\C-c"  'ada-compile-application)
  (define-key ada-mode-extra-map "c"     'ada-change-prj)
  (define-key ada-mode-extra-map "d"     'ada-set-default-project-file)
  (define-key ada-mode-extra-map "g"     'ada-gdb-application)
  (define-key ada-mode-map "\C-c\C-m"  'ada-set-main-compile-application)
  (define-key ada-mode-extra-map "r"     'ada-run-application)
  (define-key ada-mode-map "\C-c\C-o"  'ada-goto-parent)
  (define-key ada-mode-map "\C-c\C-r"  'ada-find-references)
  (define-key ada-mode-extra-map "l"     'ada-find-local-references)
  (define-key ada-mode-map "\C-c\C-v"  'ada-check-current)
  (define-key ada-mode-extra-map "f"     'ada-find-file)

  (define-key ada-mode-extra-map "u"  'ada-prj-edit)

  (define-key ada-mode-map "\C-xnd" 'ada-narrow-to-defun); override narrow-to-defun

  ;;  The templates, defined in ada-stmt.el

  (let ((map (make-sparse-keymap)))
    (define-key map "h"    'ada-header)
    (define-key map "\C-a" 'ada-array)
    (define-key map "b"    'ada-exception-block)
    (define-key map "d"    'ada-declare-block)
    (define-key map "c"    'ada-case)
    (define-key map "\C-e" 'ada-elsif)
    (define-key map "e"    'ada-else)
    (define-key map "\C-k" 'ada-package-spec)
    (define-key map "k"    'ada-package-body)
    (define-key map "\C-p" 'ada-procedure-spec)
    (define-key map "p"    'ada-subprogram-body)
    (define-key map "\C-f" 'ada-function-spec)
    (define-key map "f"    'ada-for-loop)
    (define-key map "i"    'ada-if)
    (define-key map "l"    'ada-loop)
    (define-key map "\C-r" 'ada-record)
    (define-key map "\C-s" 'ada-subtype)
    (define-key map "S"    'ada-tabsize)
    (define-key map "\C-t" 'ada-task-spec)
    (define-key map "t"    'ada-task-body)
    (define-key map "\C-y" 'ada-type)
    (define-key map "\C-v" 'ada-private)
    (define-key map "u"    'ada-use)
    (define-key map "\C-u" 'ada-with)
    (define-key map "\C-w" 'ada-when)
    (define-key map "w"    'ada-while-loop)
    (define-key map "\C-x" 'ada-exception)
    (define-key map "x"    'ada-exit)
    (define-key ada-mode-extra-map "t" map))
  )


(defun ada-create-menu ()
  "Create the Ada menu as shown in the menu bar."
  (let ((m '("Ada"
	     ("Help"
	      ["Ada Mode"               (info "ada-mode") t]
	      ["GNAT User's Guide"      (info "gnat_ugn")
	       (eq ada-which-compiler 'gnat)]
	      ["GNAT Reference Manual"  (info "gnat_rm")
	       (eq ada-which-compiler 'gnat)]
	      ["Gcc Documentation"      (info "gcc")
	       (eq ada-which-compiler 'gnat)]
	      ["Gdb Documentation"      (info "gdb")
	       (eq ada-which-compiler 'gnat)]
	      ["Ada95 Reference Manual" (info "arm95") t])
	     ("Options"  :included (derived-mode-p 'ada-mode)
	      ["Auto Casing" (setq ada-auto-case (not ada-auto-case))
	       :style toggle :selected ada-auto-case]
	      ["Auto Indent After Return"
	       (setq ada-indent-after-return (not ada-indent-after-return))
	       :style toggle :selected ada-indent-after-return]
	      ["Automatically Recompile For Cross-references"
	       (setq ada-xref-create-ali (not ada-xref-create-ali))
	       :style toggle :selected ada-xref-create-ali
	       :included (eq ada-which-compiler 'gnat)]
	      ["Confirm Commands"
	       (setq ada-xref-confirm-compile (not ada-xref-confirm-compile))
	       :style toggle :selected ada-xref-confirm-compile
	       :included (eq ada-which-compiler 'gnat)]
	      ["Show Cross-references In Other Buffer"
	       (setq ada-xref-other-buffer (not ada-xref-other-buffer))
	       :style toggle :selected ada-xref-other-buffer
	       :included (eq ada-which-compiler 'gnat)]
	      ["Tight Integration With GNU Visual Debugger"
	       (setq ada-tight-gvd-integration (not ada-tight-gvd-integration))
	       :style toggle :selected ada-tight-gvd-integration
	       :included (string-match "gvd" ada-prj-default-debugger)])
	     ["Customize"     (customize-group 'ada)
	      :included (fboundp 'customize-group)]
	     ["Check file"    ada-check-current   t]
	     ["Compile file"  ada-compile-current t]
	     ["Set main and Build" ada-set-main-compile-application t]
	     ["Show main" ada-show-current-main t]
	     ["Build"         ada-compile-application t]
	     ["Run"           ada-run-application     t]
	     ["Debug"         ada-gdb-application (eq ada-which-compiler 'gnat)]
	     ["------"        nil nil]
	     ("Project"
	      ["Show project" ada-show-current-project t]
	      ["Load..."      ada-set-default-project-file t]
	      ["New..."       ada-prj-new                  t]
	      ["Edit..."      ada-prj-edit                 t])
	     ("Goto"   :included (derived-mode-p 'ada-mode)
	      ["Goto Declaration/Body"   ada-goto-declaration
	       (eq ada-which-compiler 'gnat)]
	      ["Goto Body"               ada-goto-body
	       (eq ada-which-compiler 'gnat)]
	      ["Goto Declaration Other Frame"
	       ada-goto-declaration-other-frame
	       (eq ada-which-compiler 'gnat)]
	      ["Goto Previous Reference" ada-xref-goto-previous-reference
	       (eq ada-which-compiler 'gnat)]
	      ["List Local References"   ada-find-local-references
	       (eq ada-which-compiler 'gnat)]
	      ["List References"         ada-find-references
	       (eq ada-which-compiler 'gnat)]
	      ["Goto Reference To Any Entity" ada-find-any-references
	       (eq ada-which-compiler 'gnat)]
	      ["Goto Parent Unit"        ada-goto-parent
	       (eq ada-which-compiler 'gnat)]
	      ["--"                      nil                              nil]
	      ["Next compilation error"  next-error             t]
	      ["Previous Package"        ada-previous-package   t]
	      ["Next Package"            ada-next-package       t]
	      ["Previous Procedure"      ada-previous-procedure t]
	      ["Next Procedure"          ada-next-procedure     t]
	      ["Goto Start Of Statement" ada-move-to-start      t]
	      ["Goto End Of Statement"   ada-move-to-end        t]
	      ["-"                       nil                    nil]
	      ["Other File"              ff-find-other-file     t]
	      ["Other File Other Window" ada-ff-other-window    t])
	     ("Edit"   :included (derived-mode-p 'ada-mode)
	      ["Search File On Source Path"  ada-find-file                t]
	      ["------"                      nil                          nil]
	      ["Complete Identifier"         ada-complete-identifier      t]
	      ["-----"                       nil                          nil]
	      ["Indent Line"                 ada-indent-current-function  t]
	      ["Justify Current Indentation" ada-justified-indent-current t]
	      ["Indent Lines in Selection"   ada-indent-region            t]
	      ["Indent Lines in File"
	       (ada-indent-region (point-min) (point-max))                t]
	      ["Format Parameter List"       ada-format-paramlist         t]
	      ["-"                           nil                          nil]
	      ["Comment Selection"           comment-region               t]
	      ["Uncomment Selection"         ada-uncomment-region         t]
	      ["--"                          nil                          nil]
	      ["Fill Comment Paragraph"      fill-paragraph               t]
	      ["Fill Comment Paragraph Justify"
	       ada-fill-comment-paragraph-justify                         t]
	      ["Fill Comment Paragraph Postfix"
	       ada-fill-comment-paragraph-postfix                         t]
	      ["---"                         nil                          nil]
	      ["Adjust Case Selection"       ada-adjust-case-region       t]
	      ["Adjust Case in File"         ada-adjust-case-buffer       t]
	      ["Create Case Exception"       ada-create-case-exception    t]
	      ["Create Case Exception Substring"
	       ada-create-case-exception-substring                        t]
	      ["Reload Case Exceptions"      ada-case-read-exceptions     t]
	      ["----"                        nil                          nil]
	      ["Make body for subprogram"    ada-make-subprogram-body     t]
	      ["-----"                       nil                          nil]
	      ["Narrow to subprogram"        ada-narrow-to-defun          t])
	     ("Templates"
	      :included  (derived-mode-p 'ada-mode)
	      ["Header"          ada-header          t]
	      ["-"               nil                 nil]
	      ["Package Body"    ada-package-body    t]
	      ["Package Spec"    ada-package-spec    t]
	      ["Function Spec"   ada-function-spec   t]
	      ["Procedure Spec"  ada-procedure-spec  t]
	      ["Proc/func Body"  ada-subprogram-body t]
	      ["Task Body"       ada-task-body       t]
	      ["Task Spec"       ada-task-spec       t]
	      ["Declare Block"   ada-declare-block   t]
	      ["Exception Block" ada-exception-block t]
	      ["--"              nil                 nil]
	      ["Entry"           ada-entry           t]
	      ["Entry family"    ada-entry-family    t]
	      ["Select"          ada-select          t]
	      ["Accept"          ada-accept          t]
	      ["Or accept"       ada-or-accept       t]
	      ["Or delay"        ada-or-delay        t]
	      ["Or terminate"    ada-or-terminate    t]
	      ["---"             nil                 nil]
	      ["Type"            ada-type            t]
	      ["Private"         ada-private         t]
	      ["Subtype"         ada-subtype         t]
	      ["Record"          ada-record          t]
	      ["Array"           ada-array           t]
	      ["----"            nil                 nil]
	      ["If"              ada-if              t]
	      ["Else"            ada-else            t]
	      ["Elsif"           ada-elsif           t]
	      ["Case"            ada-case            t]
	      ["-----"           nil                 nil]
	      ["While Loop"      ada-while-loop      t]
	      ["For Loop"        ada-for-loop        t]
	      ["Loop"            ada-loop            t]
	      ["------"          nil                 nil]
	      ["Exception"       ada-exception       t]
	      ["Exit"            ada-exit            t]
	      ["When"            ada-when            t])
	     )))

    (easy-menu-define ada-mode-menu ada-mode-map "Menu keymap for Ada mode" m)
    (if (featurep 'xemacs)
	(progn
	  (define-key ada-mode-map [menu-bar] ada-mode-menu)
	  (setq mode-popup-menu (cons "Ada mode" ada-mode-menu))))))


;; -------------------------------------------------------
;;     Commenting/Uncommenting code
;;  The following two calls are provided to enhance the standard
;;  comment-region function, which only allows uncommenting if the
;;  comment is at the beginning of a line. If the line have been re-indented,
;;  we are unable to use comment-region, which makes no sense.
;;
;;  In addition, we provide an interface to the standard comment handling
;;  function for justifying the comments.
;; -------------------------------------------------------

(defadvice comment-region (before ada-uncomment-anywhere disable)
  (if (and (consp arg)  ;;  a prefix with \C-u is of the form '(4), whereas
		       ;;  \C-u 2  sets arg to '2'  (fixed by S.Leake)
	   (derived-mode-p 'ada-mode))
      (save-excursion
	(let ((cs (concat "^[ \t]*" (regexp-quote comment-start))))
	  (goto-char beg)
	  (while (re-search-forward cs end t)
	    (replace-match comment-start))
	  ))))

(defun ada-uncomment-region (beg end &optional arg)
  "Uncomment region BEG .. END.
ARG gives number of comment characters."
  (interactive "r\nP")

  ;;  This advice is not needed anymore with Emacs21. However, for older
  ;;  versions, as well as for XEmacs, we still need to enable it.
  (if (or (<= emacs-major-version 20) (featurep 'xemacs))
      (progn
	(ad-activate 'comment-region)
	(comment-region beg end (- (or arg 2)))
	(ad-deactivate 'comment-region))
    (comment-region beg end (list (- (or arg 2))))
    (ada-indent-region beg end)))

(defun ada-fill-comment-paragraph-justify ()
  "Fill current comment paragraph and justify each line as well."
  (interactive)
  (ada-fill-comment-paragraph 'full))

(defun ada-fill-comment-paragraph-postfix ()
  "Fill current comment paragraph and justify each line as well.
Adds `ada-fill-comment-postfix' at the end of each line."
  (interactive)
  (ada-fill-comment-paragraph 'full t))

(defun ada-fill-comment-paragraph (&optional justify postfix)
  "Fill the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are non-nil, `ada-fill-comment-postfix' is appended
to each line filled and justified.
The paragraph is indented on the first line."
  (interactive "P")

  ;; check if inside comment or just in front a comment
  (if (and (not (ada-in-comment-p))
	   (not (looking-at "[ \t]*--")))
      (error "Not inside comment"))

  (let* (indent from to
	 (opos (point-marker))

	 ;; Sets this variable to nil, otherwise it prevents
	 ;; fill-region-as-paragraph to work on Emacs <= 20.2
	 (parse-sexp-lookup-properties nil)

	 fill-prefix
	 (fill-column (current-fill-column)))

    ;;  Find end of paragraph
    (back-to-indentation)
    (while (and (not (eobp)) (looking-at ".*--[ \t]*[^ \t\n]"))
      (forward-line 1)

      ;;  If we were at the last line in the buffer, create a dummy empty
      ;;  line at the end of the buffer.
      (if (eobp)
	  (insert "\n")
	(back-to-indentation)))
    (beginning-of-line)
    (setq to (point-marker))
    (goto-char opos)

    ;;  Find beginning of paragraph
    (back-to-indentation)
    (while (and (not (bobp)) (looking-at ".*--[ \t]*[^ \t\n]"))
      (forward-line -1)
      (back-to-indentation))

    ;;  We want one line above the first one, unless we are at the beginning
    ;;  of the buffer
    (unless (bobp)
      (forward-line 1))
    (beginning-of-line)
    (setq from (point-marker))

    ;;  Calculate the indentation we will need for the paragraph
    (back-to-indentation)
    (setq indent (current-column))
    ;;  unindent the first line of the paragraph
    (delete-region from (point))

    ;;  Remove the old postfixes
    (goto-char from)
    (while (re-search-forward "--\n" to t)
      (replace-match "\n"))

    (goto-char (1- to))
    (setq to (point-marker))

    ;;  Indent and justify the paragraph
    (setq fill-prefix ada-fill-comment-prefix)
    (set-left-margin from to indent)
    (if postfix
	(setq fill-column (- fill-column (length ada-fill-comment-postfix))))

    (fill-region-as-paragraph from to justify)

    ;;  Add the postfixes if required
    (if postfix
	(save-restriction
	  (goto-char from)
	  (narrow-to-region from to)
	  (while (not (eobp))
	    (end-of-line)
	    (insert-char ?  (- fill-column (current-column)))
	    (insert ada-fill-comment-postfix)
	    (forward-line))
	  ))

    ;;  In Emacs <= 20.2 and XEmacs <=20.4, there is a bug, and a newline is
    ;;  inserted at the end. Delete it
    (if (or (featurep 'xemacs)
	    (<= emacs-major-version 19)
	    (and (= emacs-major-version 20)
		 (<= emacs-minor-version 2)))
	(progn
	  (goto-char to)
	  (end-of-line)
	  (delete-char 1)))

    (goto-char opos)))


;; ---------------------------------------------------
;;    support for find-file.el
;; These functions are used by find-file to guess the file names from
;; unit names, and to find the other file (spec or body) from the current
;; file (body or spec).
;; It is also used to find in which function we are, so as to put the
;; cursor at the correct position.
;; Standard Ada does not force any relation between unit names and file names,
;; so some of these functions can only be a good approximation. However, they
;; are also overridden in `ada-xref'.el when we know that the user is using
;; GNAT.
;; ---------------------------------------------------

;; Overridden when we work with GNAT, to use gnatkrunch
(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename in which ADANAME is found.
This matches the GNAT default naming convention, except for
pre-defined units."
  (while (string-match "\\." adaname)
    (setq adaname (replace-match "-" t t adaname)))
  (downcase adaname)
  )

(defun ada-other-file-name ()
  "Return the name of the other file.
The name returned is the body if `current-buffer' is the spec,
or the spec otherwise."

  (let ((is-spec nil)
	(is-body nil)
	(suffixes ada-spec-suffixes)
	(name (buffer-file-name)))

    ;;  Guess whether we have a spec or a body, and get the basename of the
    ;;  file. Since the extension may not start with '.', we can not use
    ;;  file-name-extension
    (while (and (not is-spec)
		suffixes)
      (if (string-match (concat "\\(.*\\)" (car suffixes) "$") name)
	  (setq is-spec t
		name    (match-string 1 name)))
      (setq suffixes (cdr suffixes)))

    (if (not is-spec)
	(progn
	  (setq suffixes ada-body-suffixes)
	  (while (and (not is-body)
		      suffixes)
	    (if (string-match (concat "\\(.*\\)" (car suffixes) "$") name)
		(setq is-body t
		      name    (match-string 1 name)))
	    (setq suffixes (cdr suffixes)))))

    ;;  If this wasn't in either list, return name itself
    (if (not (or is-spec is-body))
	name

      ;;  Else find the other possible names
      (if is-spec
	  (setq suffixes ada-body-suffixes)
	(setq suffixes ada-spec-suffixes))
      (setq is-spec name)

      (while suffixes

	;;  If we are using project file, search for the other file in all
	;;  the possible src directories.

	(if (fboundp 'ada-find-src-file-in-dir)
	    (let ((other
		   (ada-find-src-file-in-dir
		    (file-name-nondirectory (concat name (car suffixes))))))
	      (if other
		  (setq is-spec other)))

	  ;;  Else search in the current directory
	  (if (file-exists-p (concat name (car suffixes)))
	      (setq is-spec (concat name (car suffixes)))))
	(setq suffixes (cdr suffixes)))

      is-spec)))

(defun ada-which-function-are-we-in ()
  "Return the name of the function whose definition/declaration point is in.
Used in `ff-pre-load-hook'."
  (setq ff-function-name nil)
  (save-excursion
    (end-of-line);;  make sure we get the complete name
    (or (if (re-search-backward ada-procedure-start-regexp nil t)
	    (setq ff-function-name (match-string 5)))
	(if (re-search-backward ada-package-start-regexp nil t)
	    (setq ff-function-name (match-string 4))))
    ))


(defvar ada-last-which-function-line -1
  "Last line on which `ada-which-function' was called.")
(defvar ada-last-which-function-subprog 0
  "Last subprogram name returned by `ada-which-function'.")
(make-variable-buffer-local 'ada-last-which-function-subprog)
(make-variable-buffer-local 'ada-last-which-function-line)


(defun ada-which-function ()
  "Return the name of the function whose body the point is in.
This function works even in the case of nested subprograms, whereas the
standard Emacs function `which-function' does not.
Since the search can be long, the results are cached."

  (let ((line (count-lines 1 (point)))
	(pos (point))
	end-pos
	func-name indent
	found)

    ;;  If this is the same line as before, simply return the same result
    (if (= line ada-last-which-function-line)
	ada-last-which-function-subprog

      (save-excursion
	;; In case the current line is also the beginning of the body
	(end-of-line)

	;;  Are we looking at "function Foo\n    (paramlist)"
	(skip-chars-forward " \t\n(")

	(condition-case nil
	    (up-list 1)
	  (error nil))

	(skip-chars-forward " \t\n")
	(if (looking-at "return")
	    (progn
	      (forward-word 1)
	      (skip-chars-forward " \t\n")
	      (skip-chars-forward "a-zA-Z0-9_'")))

	;; Can't simply do forward-word, in case the "is" is not on the
	;; same line as the closing parenthesis
	(skip-chars-forward "is \t\n")

	;; No look for the closest subprogram body that has not ended yet.
	;; Not that we expect all the bodies to be finished by "end <name>",
	;; or a simple "end;" indented in the same column as the start of
	;; the subprogram. The goal is to be as efficient as possible.

	(while (and (not found)
		    (re-search-backward ada-imenu-subprogram-menu-re nil t))

	  ;; Get the function name, but not the properties, or this changes
	  ;; the face in the modeline on Emacs 21
	  (setq func-name (match-string-no-properties 3))
	  (if (and (not (ada-in-comment-p))
		   (not (save-excursion
			  (goto-char (match-end 0))
			  (looking-at "[ \t\n]*new"))))
	      (save-excursion
		(back-to-indentation)
		(setq indent (current-column))
		(if (ada-search-ignore-string-comment
		     (concat "end[ \t]+" func-name "[ \t]*;\\|^"
			     (make-string indent ? ) "end;"))
		    (setq end-pos (point))
		  (setq end-pos (point-max)))
		(if (>= end-pos pos)
		    (setq found func-name))))
	  )
	(setq ada-last-which-function-line line
	      ada-last-which-function-subprog found)
	found))))

(defun ada-ff-other-window ()
  "Find other file in other window using `ff-find-other-file'."
  (interactive)
  (and (fboundp 'ff-find-other-file)
       (ff-find-other-file t)))

(defun ada-set-point-accordingly ()
  "Move to the function declaration that was set by `ff-which-function-are-we-in'."
  (if ff-function-name
      (progn
	(goto-char (point-min))
	(unless (ada-search-ignore-string-comment
		 (concat ff-function-name "\\b") nil)
	  (goto-char (point-min))))))

(defun ada-get-body-name (&optional spec-name)
  "Return the file name for the body of SPEC-NAME.
If SPEC-NAME is nil, return the body for the current package.
Return nil if no body was found."
  (interactive)

  (unless spec-name (setq spec-name (buffer-file-name)))

  ;; Remove the spec extension. We can not simply remove the file extension,
  ;; but we need to take into account the specific non-GNAT extensions that the
  ;; user might have specified.

  (let ((suffixes ada-spec-suffixes)
	end)
    (while suffixes
      (setq end (- (length spec-name) (length (car suffixes))))
      (if (string-equal (car suffixes) (substring spec-name end))
	  (setq spec-name (substring spec-name 0 end)))
      (setq suffixes (cdr suffixes))))

  ;; If find-file.el was available, use its functions
  (if (fboundp 'ff-get-file-name)
      (ff-get-file-name ada-search-directories-internal
			(ada-make-filename-from-adaname
			 (file-name-nondirectory
			  (file-name-sans-extension spec-name)))
			ada-body-suffixes)
    ;; Else emulate it very simply
    (concat (ada-make-filename-from-adaname
	     (file-name-nondirectory
	      (file-name-sans-extension spec-name)))
	    ".adb")))


;; ---------------------------------------------------
;;    support for font-lock.el
;; Strings are a real pain in Ada because a single quote character is
;; overloaded as a string quote and type/instance delimiter.  By default, a
;; single quote is given punctuation syntax in `ada-mode-syntax-table'.
;; So, for Font Lock mode purposes, we mark single quotes as having string
;; syntax when the gods that created Ada determine them to be.
;;
;; This only works in Emacs. See the comments before the grammar functions
;; at the beginning of this file for how this is done with XEmacs.
;; ----------------------------------------------------

(defconst ada-font-lock-syntactic-keywords
  ;; Mark single quotes as having string quote syntax in 'c' instances.
  ;; We used to explicitly avoid ''' as a special case for fear the buffer
  ;; be highlighted as a string, but it seems this fear is unfounded.
  ;;
  ;; This sets the properties of the characters, so that ada-in-string-p
  ;; correctly handles '"' too...
  '(("[^a-zA-Z0-9)]\\('\\)[^\n]\\('\\)" (1 (7 . ?')) (2 (7 . ?')))
    ("^[ \t]*\\(#\\(if\\|else\\|elsif\\|end\\)\\)" (1 (11 . ?\n)))))

(defvar ada-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; handle "type T is access function return S;"
     (list "\\<\\(function[ \t]+return\\)\\>" '(1 font-lock-keyword-face) )

     ;;  preprocessor line
     (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))

     ;;
     ;; accept, entry, function, package (body), protected (body|type),
     ;; pragma, procedure, task (body) plus name.
     (list (concat
	    "\\<\\("
	    "accept\\|"
	    "entry\\|"
	    "function\\|"
	    "package[ \t]+body\\|"
	    "package\\|"
	    "pragma\\|"
	    "procedure\\|"
	    "protected[ \t]+body\\|"
	    "protected[ \t]+type\\|"
	    "protected\\|"
	    "task[ \t]+body\\|"
	    "task[ \t]+type\\|"
	    "task"
	    "\\)\\>[ \t]*"
	    "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
	   '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))
     ;;
     ;; Optional keywords followed by a type name.
     (list (concat                      ; ":[ \t]*"
	    "\\<\\(access[ \t]+all\\|access[ \t]+constant\\|access\\|constant\\|in[ \t]+reverse\\|\\|in[ \t]+out\\|in\\|out\\)\\>"
	    "[ \t]*"
	    "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
	   '(1 font-lock-keyword-face nil t) '(2 font-lock-type-face nil t))

     ;;
     ;; Main keywords, except those treated specially below.
     (concat "\\<"
	     (regexp-opt
	      '("abort" "abs" "abstract" "accept" "access" "aliased" "all"
		"and" "array" "at" "begin" "case" "declare" "delay" "delta"
		"digits" "do" "else" "elsif" "entry" "exception" "exit" "for"
		"generic" "if" "in" "interface" "is" "limited" "loop" "mod" "not"
		"null" "or" "others" "overriding" "private" "protected" "raise"
		"range" "record" "rem" "renames" "requeue" "return" "reverse"
		"select" "separate" "synchronized" "tagged" "task" "terminate"
		"then" "until" "when" "while" "with" "xor") t)
	     "\\>")
     ;;
     ;; Anything following end and not already fontified is a body name.
     '("\\<\\(end\\)\\>\\([ \t]+\\)?\\(\\(\\sw\\|[_.]\\)+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ;; Keywords followed by a type or function name.
     (list (concat "\\<\\("
		   "new\\|of\\|subtype\\|type"
		   "\\)\\>[ \t]*\\(\\sw+\\(\\.\\sw*\\)*\\)?[ \t]*\\((\\)?")
	   '(1 font-lock-keyword-face)
	   '(2 (if (match-beginning 4)
		   font-lock-function-name-face
		 font-lock-type-face) nil t))
     ;;
     ;; Keywords followed by a (comma separated list of) reference.
     ;; Note that font-lock only works on single lines, thus we can not
     ;; correctly highlight a with_clause that spans multiple lines.
     (list (concat "\\<\\(goto\\|raise\\|use\\|with\\)"
		   "[ \t]+\\([a-zA-Z0-9_., \t]+\\)\\W")
	   '(1 font-lock-keyword-face) '(2 font-lock-reference-face nil t))

     ;;
     ;; Goto tags.
     '("<<\\(\\sw+\\)>>" 1 font-lock-reference-face)

     ;; Highlight based-numbers (R. Reagan <robin-reply@reagans.org>)
     (list "\\([0-9]+#[0-9a-fA-F_]+#\\)" '(1 font-lock-constant-face t))

     ;; Ada unnamed numerical constants
     (list "\\W\\([-+]?[0-9._]+\\)\\>" '(1 font-lock-constant-face))

     ))
  "Default expressions to highlight in Ada mode.")


;; ---------------------------------------------------------
;;  Support for outline.el
;; ---------------------------------------------------------

(defun ada-outline-level ()
  "This is so that `current-column' DTRT in otherwise-hidden text."
  ;; patch from Dave Love <fx@gnu.org>
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column))))

;; ---------------------------------------------------------
;;  Support for narrow-to-region
;; ---------------------------------------------------------

(defun ada-narrow-to-defun (&optional _arg)
  "Make text outside current subprogram invisible.
The subprogram visible is the one that contains or follow point.
Optional ARG is ignored.
Use \\[widen] to go back to the full visibility for the buffer."

  (interactive)
  (save-excursion
    (let (end)
      (widen)
      (forward-line 1)
      (ada-previous-procedure)
      (setq end (point-at-bol))
      (ada-move-to-end)
      (end-of-line)
      (narrow-to-region end (point))
      (message
       "Use M-x widen to get back to full visibility in the buffer"))))

;; ---------------------------------------------------------
;;    Automatic generation of code
;; The Ada mode has a set of function to automatically generate a subprogram
;; or package body from its spec.
;; These function only use a primary and basic algorithm, this could use a
;; lot of improvement.
;; When the user is using GNAT, we rather use gnatstub to generate an accurate
;; body.
;; ----------------------------------------------------------

(defun ada-gen-treat-proc (match)
  "Make dummy body of a procedure/function specification.
MATCH is a cons cell containing the start and end locations of the last search
for `ada-procedure-start-regexp'."
  (goto-char (car match))
  (let (func-found procname functype)
    (cond
     ((or (looking-at "^[ \t]*procedure")
	  (setq func-found (looking-at "^[ \t]*function")))
      ;; treat it as a proc/func
      (forward-word 2)
      (forward-word -1)
      (setq procname (buffer-substring (point) (cdr match))) ; store  proc name

      ;; goto end of procname
      (goto-char (cdr match))

      ;; skip over parameterlist
      (unless (looking-at "[ \t\n]*\\(;\\|return\\)")
	(forward-sexp))

      ;; if function, skip over 'return' and result type.
      (if func-found
	  (progn
	    (forward-word 1)
	    (skip-chars-forward " \t\n")
	    (setq functype (buffer-substring (point)
					     (progn
					       (skip-chars-forward
						"a-zA-Z0-9_\.")
					       (point))))))
      ;; look for next non WS
      (cond
       ((looking-at "[ \t]*;")
	(delete-region (match-beginning 0) (match-end 0));; delete the ';'
	(ada-indent-newline-indent)
	(insert "is")
	(ada-indent-newline-indent)
	(if func-found
	    (progn
	      (insert "Result : " functype ";")
	      (ada-indent-newline-indent)))
	(insert "begin")
	(ada-indent-newline-indent)
	(if func-found
	    (insert "return Result;")
	  (insert "null;"))
	(ada-indent-newline-indent)
	(insert "end " procname ";")
	(ada-indent-newline-indent)
	)

       ((looking-at "[ \t\n]*is")
	;; do nothing
	)

       ((looking-at "[ \t\n]*rename")
	;; do nothing
	)

       (t
	(message "unknown syntax"))))
     (t
      (if (looking-at "^[ \t]*task")
	  (progn
	    (message "Task conversion is not yet implemented")
	    (forward-word 2)
	    (if (looking-at "[ \t]*;")
		(forward-line)
	      (ada-move-to-end))
	    ))))))

(defun ada-make-body ()
  "Create an Ada package body in the current buffer.
The spec must be the previously visited buffer.
This function typically is to be hooked into `ff-file-created-hook'."
  (delete-region (point-min) (point-max))
  (insert-buffer-substring (car (cdr (buffer-list))))
  (goto-char (point-min))
  (ada-mode)

  (let (found ada-procedure-or-package-start-regexp)
    (if (setq found
	     (ada-search-ignore-string-comment ada-package-start-regexp nil))
	(progn (goto-char (cdr found))
	       (insert " body")
	       )
      (error "No package"))

    (setq ada-procedure-or-package-start-regexp
	 (concat ada-procedure-start-regexp
		 "\\|"
		 ada-package-start-regexp))

    (while (setq found
		(ada-search-ignore-string-comment
		 ada-procedure-or-package-start-regexp nil))
      (progn
	(goto-char (car found))
	(if (looking-at ada-package-start-regexp)
	    (progn (goto-char (cdr found))
		   (insert " body"))
	  (ada-gen-treat-proc found))))))


(defun ada-make-subprogram-body ()
  "Create a dummy subprogram body in package body file from spec surrounding point."
  (interactive)
  (let* ((found (re-search-backward ada-procedure-start-regexp nil t))
	 (spec  (match-beginning 0))
	 body-file)
    (if found
	(progn
	  (goto-char spec)
	  (if (and (re-search-forward "(\\|;" nil t)
		   (= (char-before) ?\())
	      (progn
		(ada-search-ignore-string-comment ")" nil)
		(ada-search-ignore-string-comment ";" nil)))
	  (setq spec (buffer-substring spec (point)))

	  ;; If find-file.el was available, use its functions
	  (setq body-file (ada-get-body-name))
	  (if body-file
	      (find-file body-file)
	    (error "No body found for the package.  Create it first"))

	  (save-restriction
	    (widen)
	    (goto-char (point-max))
	    (forward-comment -10000)
	    (re-search-backward "\\<end\\>" nil t)
	    ;;  Move to the beginning of the elaboration part, if any
	    (re-search-backward "^begin" nil t)
	    (newline)
	    (forward-char -1)
	    (insert spec)
	    (re-search-backward ada-procedure-start-regexp nil t)
	    (ada-gen-treat-proc (cons (match-beginning 0) (match-end 0)))
	    ))
      (error "Not in subprogram spec"))))

;; --------------------------------------------------------
;; Global initializations
;; --------------------------------------------------------

;;  Create the keymap once and for all. If we do that in ada-mode,
;;  the keys changed in the user's .emacs have to be modified
;;  every time
(ada-create-keymap)
(ada-create-menu)

;;  Create the syntax tables, but do not activate them
(ada-create-syntax-table)

;;  Add the default extensions (and set up speedbar)
(ada-add-extensions ".ads" ".adb")
;; This two files are generated by GNAT when running with -gnatD
(if (equal ada-which-compiler 'gnat)
    (ada-add-extensions ".ads.dg" ".adb.dg"))

;;  Read the special cases for exceptions
(ada-case-read-exceptions)

;;  Setup auto-loading of the other Ada mode files.
(autoload 'ada-change-prj                   "ada-xref" nil t)
(autoload 'ada-check-current                "ada-xref" nil t)
(autoload 'ada-compile-application          "ada-xref" nil t)
(autoload 'ada-compile-current              "ada-xref" nil t)
(autoload 'ada-complete-identifier          "ada-xref" nil t)
(autoload 'ada-find-file                    "ada-xref" nil t)
(autoload 'ada-find-any-references          "ada-xref" nil t)
(autoload 'ada-find-src-file-in-dir         "ada-xref" nil t)
(autoload 'ada-find-local-references        "ada-xref" nil t)
(autoload 'ada-find-references              "ada-xref" nil t)
(autoload 'ada-gdb-application              "ada-xref" nil t)
(autoload 'ada-goto-declaration             "ada-xref" nil t)
(autoload 'ada-goto-declaration-other-frame "ada-xref" nil t)
(autoload 'ada-goto-parent                  "ada-xref" nil t)
(autoload 'ada-make-body-gnatstub           "ada-xref" nil t)
(autoload 'ada-point-and-xref               "ada-xref" nil t)
(autoload 'ada-reread-prj-file              "ada-xref" nil t)
(autoload 'ada-run-application              "ada-xref" nil t)
(autoload 'ada-set-default-project-file     "ada-xref" nil t)
(autoload 'ada-xref-goto-previous-reference "ada-xref" nil t)
(autoload 'ada-set-main-compile-application "ada-xref" nil t)
(autoload 'ada-show-current-main            "ada-xref" nil t)

(autoload 'ada-customize                    "ada-prj"  nil t)
(autoload 'ada-prj-edit                     "ada-prj"  nil t)
(autoload 'ada-prj-new                      "ada-prj"  nil t)
(autoload 'ada-prj-save                     "ada-prj"  nil t)

(autoload 'ada-array           "ada-stmt" nil t)
(autoload 'ada-case            "ada-stmt" nil t)
(autoload 'ada-declare-block   "ada-stmt" nil t)
(autoload 'ada-else            "ada-stmt" nil t)
(autoload 'ada-elsif           "ada-stmt" nil t)
(autoload 'ada-exception       "ada-stmt" nil t)
(autoload 'ada-exception-block "ada-stmt" nil t)
(autoload 'ada-exit            "ada-stmt" nil t)
(autoload 'ada-for-loop        "ada-stmt" nil t)
(autoload 'ada-function-spec   "ada-stmt" nil t)
(autoload 'ada-header          "ada-stmt" nil t)
(autoload 'ada-if              "ada-stmt" nil t)
(autoload 'ada-loop            "ada-stmt" nil t)
(autoload 'ada-package-body    "ada-stmt" nil t)
(autoload 'ada-package-spec    "ada-stmt" nil t)
(autoload 'ada-private         "ada-stmt" nil t)
(autoload 'ada-procedure-spec  "ada-stmt" nil t)
(autoload 'ada-record          "ada-stmt" nil t)
(autoload 'ada-subprogram-body "ada-stmt" nil t)
(autoload 'ada-subtype         "ada-stmt" nil t)
(autoload 'ada-tabsize         "ada-stmt" nil t)
(autoload 'ada-task-body       "ada-stmt" nil t)
(autoload 'ada-task-spec       "ada-stmt" nil t)
(autoload 'ada-type            "ada-stmt" nil t)
(autoload 'ada-use             "ada-stmt" nil t)
(autoload 'ada-when            "ada-stmt" nil t)
(autoload 'ada-while-loop      "ada-stmt" nil t)
(autoload 'ada-with            "ada-stmt" nil t)

;;; provide ourselves
(provide 'ada-mode)

;;; ada-mode.el ends here
